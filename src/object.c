/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
 * Copyright (C) 2007, 2008 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "dfsch/object.h"

#include <dfsch/hash.h>
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include <dfsch/serdes.h>
#include <dfsch/generic.h>
#include <assert.h>
#include "util.h"
#include "internal.h"

dfsch_type_t dfsch_metaclass_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name= "metaclass",
  .size = sizeof(dfsch_metaclass_t),
};

typedef dfsch_standard_class_t class_t;

dfsch_object_t* standard_allocate_instance(class_t* c){
  return dfsch_make_object((dfsch_type_t*)c);
}

static dfsch_slot_t* make_slots(dfsch_object_t* slot_desc,
                                dfsch_slot_type_t* default_type){
  dfsch_object_t* i = slot_desc;
  size_t slot_count = dfsch_list_length_check(slot_desc);
  dfsch_slot_t* slots = GC_MALLOC((slot_count + 1) * sizeof(dfsch_slot_t));
  dfsch_slot_t* j = slots;

  while (slot_count && DFSCH_PAIR_P(i)){
    dfsch_object_t* name;
    dfsch_object_t* spec;
    dfsch_object_t* type = default_type;
    dfsch_object_t* options;
    if (DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
      spec = DFSCH_FAST_CAR(i);
      DFSCH_OBJECT_ARG(spec, name);
    } else {
      name = DFSCH_FAST_CAR(i);
      spec = NULL;
    }
    options = spec;
    DFSCH_KEYWORD_PARSER_BEGIN(spec);
    DFSCH_KEYWORD("type", type);
    DFSCH_KEYWORD_PARSER_END_ALLOW_OTHER(spec);
    
    type = DFSCH_ASSERT_INSTANCE(type, DFSCH_SLOT_TYPE_TYPE);

    j->type = type;
    j->name = dfsch_symbol(name);
    j->documentation = NULL;
    j->access = DFSCH_SLOT_ACCESS_RW;
    j->options = options;

    j++;
    slot_count--;
    i = DFSCH_FAST_CDR(i);
  }

  j->type = NULL;
  j->name = NULL;
  j->access = DFSCH_SLOT_ACCESS_RW;
  j->documentation = NULL;

  return slots;
}

static size_t adjust_sizes(dfsch_slot_t* slots, size_t parent_size){
  if (parent_size < sizeof(dfsch_object_t)){
    parent_size = sizeof(dfsch_object_t);
  }
  while (slots->type){
    while (parent_size % slots->type->alignment != 0){
      parent_size++;
    }
    slots->offset = parent_size;
    parent_size += slots->type->size;
    slots++;
  }
  return parent_size;
}

static void init_class_slots(dfsch_type_t* klass){
  dfsch_slot_t* j = klass->slots;

  while (j->type){
    if (j->type->type_init){
      j->type->type_init(klass, j);
    }
    j++;
  }
}

void dfsch_make_class_slots(dfsch_slot_type_t* default_slot_type,
                            dfsch_type_t* klass,
                            dfsch_object_t* defs){
  size_t parent_size = sizeof(dfsch_object_t);
  
  if (klass->superclass){
    parent_size = klass->superclass->size;
  }

  klass->slots = make_slots(defs, default_slot_type);
  assert(klass->slots);
  klass->size = adjust_sizes(klass->slots, parent_size);
  init_class_slots(klass);
}

static void instance_write(dfsch_object_t*obj, dfsch_writer_state_t* state){
  class_t* i = DFSCH_TYPE_OF(obj);

  while (DFSCH_INSTANCE_P(i, DFSCH_STANDARD_CLASS_TYPE)){
    if (i->write_instance){
      dfsch_apply(i->write_instance, dfsch_list(2, obj, state));
      return;
    }
    i = i->standard_type.superclass;
  }

  dfsch_write_unreadable_with_slots(state, obj);
}

static void instance_serialize(dfsch_object_t* obj, dfsch_serializer_t* s){
  dfsch_type_t* klass = DFSCH_TYPE_OF(obj);
  dfsch_serialize_stream_symbol(s, "class-instance");
  dfsch_serialize_object(s, klass);
  while (klass){
    dfsch_slot_t* i = klass->slots;
    while (i->type){
      dfsch_serialize_stream_symbol(s, i->name);
      dfsch_serialize_object(s, dfsch_slot_ref(obj, i, 1));
      i++;
    }
    klass = klass->superclass;
  }
  dfsch_serialize_stream_symbol(s, NULL);
}

DFSCH_DEFINE_DESERIALIZATION_HANDLER("class-instance", class_instance){
  dfsch_type_t* klass;
  dfsch_object_t* ins;
  dfsch_object_t* obj;
  dfsch_object_t** place = dfsch_deserializer__skip_object(ds);
  char* sym;
  obj = dfsch_deserialize_object(ds);
  klass = DFSCH_ASSERT_INSTANCE(obj, DFSCH_STANDARD_CLASS_TYPE);
  *place = ins = dfsch_make_object(klass);

  for(;;){
    sym = dfsch_deserialize_stream_symbol(ds);
    if (!sym){
      break;
    }
    dfsch_slot_set_by_name(ins, sym, dfsch_deserialize_object(ds), 1);
  }
  return ins;
}

void dfsch_standard_class_prepare_slots(dfsch_standard_class_t* klass){
  dfsch_slot_t* j = klass->standard_type.slots;

  while (j->type){
    dfsch_slot_t* slot = j;
    dfsch_object_t* slot_def = j->options;
    while (DFSCH_PAIR_P((slot_def))){                                 
      dfsch_object_t* keyword;                                
      dfsch_object_t* value;                                  
      keyword = DFSCH_FAST_CAR(slot_def);                       
      slot_def = DFSCH_FAST_CDR(slot_def);                                
      if (!DFSCH_PAIR_P(slot_def)){                                     
        dfsch_error("Value expected for slot option", keyword);
      }                                                               
      value = DFSCH_FAST_CAR(slot_def);                         
      slot_def = DFSCH_FAST_CDR(slot_def);
      
      if(dfsch_compare_keyword(keyword, "initfunc")){
        klass->initfuncs = dfsch_cons(dfsch_list(2, value, slot),
                                      klass->initfuncs);
      } else if(dfsch_compare_keyword(keyword, "initarg")){
        klass->initargs = dfsch_cons(dfsch_list(2, value, slot),
                                     klass->initargs);
      } 
    }

    j++;
  }
}


dfsch_object_t* standard_make_class(dfsch_metaclass_t* metaclass,
                                    class_t* super,
                                    char* name,
                                    dfsch_object_t* slots,
                                    dfsch_object_t* options,
                                    dfsch_object_t* roles){
  class_t* klass = (class_t*)dfsch_make_object(DFSCH_STANDARD_CLASS_TYPE);

  klass->standard_type.flags = DFSCH_TYPEF_USER_EXTENSIBLE;
  klass->standard_type.name = name;
  klass->standard_type.roles = dfsch_list_copy_immutable(roles);

  if (super){

    klass->standard_type.superclass = (dfsch_type_t*)super;
    klass->initfuncs = super->initfuncs;
    klass->initargs = super->initargs;

  } else {
    klass->standard_type.superclass = NULL;
  }

  dfsch_make_class_slots(DFSCH_OBJECT_SLOT_TYPE, klass, slots);

  assert(klass->standard_type.slots);

  klass->standard_type.write = instance_write;
  klass->standard_type.serialize = instance_serialize;

  /* klass->standard_type.equal_p = class_equal_p;
  klass->standard_type.apply = class_apply;
  klass->standard_type.hash = class_hash;*/

  dfsch_standard_class_prepare_slots(klass);

  return (dfsch_object_t*)klass;
}

dfsch_metaclass_t dfsch_standard_class_type = {
  .type = {
    DFSCH_METACLASS_TYPE,
    DFSCH_STANDARD_TYPE,
    sizeof(class_t),
    "standard-class",
    
    NULL,
    NULL,
    NULL,
    NULL,
    
    NULL,
    "Metaclass for normal user-defined classes"
  },
  .allocate_instance = standard_allocate_instance,
  .make_class = standard_make_class
};


dfsch_object_t* dfsch_make_class(dfsch_object_t* superclass,
                                 dfsch_object_t* metaclass,
                                 char* name,
                                 dfsch_object_t* slots,
                                 dfsch_object_t* options,
                                 dfsch_object_t* roles){
  dfsch_metaclass_t* mc;
  dfsch_object_t* super = NULL;

  if (superclass){
    super = DFSCH_ASSERT_METACLASS_INSTANCE(superclass,
                                            DFSCH_METACLASS_TYPE);
  }

  if (metaclass){
    mc = DFSCH_ASSERT_INSTANCE(metaclass, DFSCH_METACLASS_TYPE);
  } else if (super) {
    mc = DFSCH_TYPE_OF(super);
  } else {
    mc = DFSCH_STANDARD_CLASS_TYPE;
  }

  if (super && !dfsch_superclass_p(mc, DFSCH_TYPE_OF(super))){
    dfsch_error("Metaclass is not subclass of superclass metaclass",
                mc);
  }

  return mc->make_class(mc, super, name, slots, options, roles);
}

typedef struct role_t role_t;

struct role_t {
  dfsch_type_t* type;
  char* name;
  dfsch_object_t* superroles;
  dfsch_object_t* slots;
  dfsch_object_t* options;
};

static dfsch_slot_t role_slots[] = {
  DFSCH_STRING_SLOT(role_t, name,
                    DFSCH_SLOT_ACCESS_RO,
                    "Name of this role"),
  DFSCH_OBJECT_SLOT(role_t, superroles, 
                    DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "List of parent roles"),
  DFSCH_OBJECT_SLOT(role_t, slots, 
                    DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Slots added by this role"),
  DFSCH_OBJECT_SLOT(role_t, options, 
                    DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Class options added by this role"),
  DFSCH_SLOT_TERMINATOR
};

static int role_inherited_p(role_t* sub, role_t* super){
  dfsch_object_t* i;
  if (sub == super){
    return 1;
  }
  
  i = sub->superroles;

  while (DFSCH_PAIR_P(i)){
    if (role_inherited_p(DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(i),
                                               DFSCH_ROLE_TYPE),
                         super)){
      return 1;
    }
    i = DFSCH_FAST_CDR(i);
  }

  return 0;
}

int dfsch_role_inherited_p(dfsch_object_t* sub, 
                           dfsch_object_t* super){
  return role_inherited_p(DFSCH_ASSERT_INSTANCE(sub, DFSCH_ROLE_TYPE),
                          DFSCH_ASSERT_INSTANCE(super, DFSCH_ROLE_TYPE));
}


static int role_matches_p(role_t* role,
                          dfsch_object_t* type){
  if (DFSCH_INSTANCE_P(type, DFSCH_STANDARD_TYPE)){
    dfsch_type_t* t = ((dfsch_type_t*)type);
    while (t){
      dfsch_object_t* i = t->roles;
      
      while (DFSCH_PAIR_P(i)){
        if (role_inherited_p(DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(i),
                                                   DFSCH_ROLE_TYPE),
                             role)){
          return 1;
        }
        i = DFSCH_FAST_CDR(i);
      }
      t = t->superclass;
    }
    return 0;    
  } else if (DFSCH_INSTANCE_P(type, DFSCH_ROLE_TYPE)){
    return role_inherited_p((role_t*)type, role);
  } else {
    return 0;
  }
}

dfsch_type_specializer_type_t dfsch_role_type = {
  .type = {
    .type = DFSCH_TYPE_SPECIALIZER_METATYPE,
    .superclass = DFSCH_TYPE_SPECIALIZER_TYPE,
    .name = "role",
    .size = sizeof(role_t),
    .slots = role_slots,
  },
  .matches_p = role_matches_p,
};

dfsch_object_t* dfsch_make_role(char* name,
                                dfsch_object_t* superroles,
                                dfsch_object_t* slots,
                                dfsch_object_t* options){
  role_t* role = dfsch_make_object(DFSCH_ROLE_TYPE);
  dfsch_object_t* slot_lists = dfsch_cons(slots, NULL);
  dfsch_object_t* options_lists = dfsch_cons(options, NULL);

  role->name = stracpy(name);
  role->superroles = dfsch_list_copy_immutable(superroles);
  role->slots = dfsch_list_copy_immutable(dfsch_append(slot_lists));
  role->options = dfsch_list_copy_immutable(dfsch_append(options_lists));

  return role;
}




DFSCH_DEFINE_PRIMITIVE(default_initialize_instance,
                       "Default implementation of initialize-instance"){
  dfsch_object_t* obj;
  DFSCH_OBJECT_ARG(args, obj);
  class_t* klass = DFSCH_ASSERT_INSTANCE(DFSCH_TYPE_OF(obj), 
                                         DFSCH_STANDARD_CLASS_TYPE);
  dfsch_object_t* i = klass->initfuncs;
  dfsch_object_t* initialized = NULL;

  while (DFSCH_PAIR_P(args)){                                 
    dfsch_object_t* keyword;                                
    dfsch_object_t* value;                                  
    dfsch_object_t* slot;
    keyword = DFSCH_FAST_CAR(args);                       
    args = DFSCH_FAST_CDR(args);                                
    if (!DFSCH_PAIR_P(args)){                                     
      dfsch_error("Value expected for keyword", keyword);
    }                                                               
    value = DFSCH_FAST_CAR(args);                         
    args = DFSCH_FAST_CDR(args);
    
    slot = dfsch_assq(keyword, klass->initargs);

    if (!slot){
      dfsch_error("Unknown keyword", keyword);      
    }

    slot = dfsch_list_item(slot, 1);
    
    dfsch_slot_set(obj, slot, value, 1);
    initialized = dfsch_cons(slot, initialized);
  }

  i = klass->initfuncs;
  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* j = DFSCH_FAST_CAR(i);
    dfsch_object_t* value;
    dfsch_object_t* slot;

    DFSCH_OBJECT_ARG(j, value);
    DFSCH_OBJECT_ARG(j, slot);
    
    if (!dfsch_memq(slot, initialized)){
      dfsch_slot_set(obj, slot, dfsch_apply(value, NULL), 1);
    }

    i = DFSCH_FAST_CDR(i);
  }
}
static void initialize_instance_slots(dfsch_object_t* inst, 
                                      dfsch_type_t* type){
  while (type){
    dfsch_slot_t* j = type->slots;
    while (j->type){
      if (j->type->instance_init){
        j->type->instance_init(((char*) inst)+j->offset, inst, j);
      }
      j++;
    }
    type = type->superclass;
  }
}

dfsch_object_t* dfsch_allocate_instance(dfsch_object_t* klass){
  dfsch_object_t* obj;
  class_t* c = DFSCH_ASSERT_METACLASS_INSTANCE(klass, 
                                               DFSCH_METACLASS_TYPE);

  obj = ((dfsch_metaclass_t*)DFSCH_TYPE_OF(c))->allocate_instance(c);
  initialize_instance_slots(obj, c);
  return obj;
}

DFSCH_DEFINE_PRIMITIVE(allocate_instance, "Allocate new instance of "
                       "user-defined class"){
  dfsch_object_t* klass;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_ARG_END(args);

  return dfsch_allocate_instance(klass);
}

DFSCH_DEFINE_PRIMITIVE(make_class, "Create new class object"){
  dfsch_object_t* name;
  dfsch_object_t* superclass;
  dfsch_object_t* slots;
  dfsch_object_t* metaclass = DFSCH_STANDARD_CLASS_TYPE;
  dfsch_object_t* roles = NULL;
  dfsch_object_t* options;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, superclass);
  DFSCH_OBJECT_ARG(args, slots);
  options = args;
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("metaclass", metaclass);
  DFSCH_KEYWORD("roles", roles);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  return dfsch_make_class(superclass, metaclass, dfsch_symbol_2_typename(name),
                          slots, options, roles);  
}

DFSCH_DEFINE_PRIMITIVE(make_role, "Create new role object"){
  dfsch_object_t* name;
  dfsch_object_t* superroles;
  dfsch_object_t* slots;
  dfsch_object_t* options;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, superroles);
  DFSCH_OBJECT_ARG(args, slots);
  DFSCH_OBJECT_ARG(args, options);
  DFSCH_ARG_END(args);

  return dfsch_make_role(dfsch_symbol_2_typename(name),
                         superroles, slots, options);  
}


static void write_instance_add_method(dfsch_object_t* function,
                                           dfsch_method_t* method){
  class_t* klass;

  if (dfsch_list_length_check(method->specializers) != 1){
    dfsch_error("write-instance methods can only specialize on first"
                " argument", method);
  }
  if (method->qualifiers != NULL){
    dfsch_error("write-instance cannot have non-primary methods",
                method);
  }

  klass = DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(method->specializers),
                                DFSCH_STANDARD_CLASS_TYPE);

  klass->write_instance = method->function;
}
static void write_instance_remove_method(dfsch_object_t* function,
                                              dfsch_method_t* method){
  class_t* klass;

  if (dfsch_list_length_check(method->specializers) != 1){
    dfsch_error("write-instance methods can only specialize on first"
                " argument", method);
  }
  if (method->qualifiers != NULL){
    dfsch_error("write-instance cannot have non-primary methods",
                method);
  }

  klass = DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(method->specializers),
                                DFSCH_STANDARD_CLASS_TYPE);

  klass->write_instance = NULL;
}
static dfsch_object_t* write_instance_apply(dfsch_object_t* ignore,
                                          dfsch_object_t* args,
                                          dfsch_tail_escape_t* esc,
                                          dfsch_object_t* context){
  dfsch_object_t* state;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, state);
  DFSCH_ARG_END(args);

  dfsch_write_object(DFSCH_ASSERT_TYPE(state, DFSCH_WRITER_STATE_TYPE),
                     object);
  return NULL;
}

static dfsch_singleton_generic_function_t write_instance = {
  .type = DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE,
  .add_method = write_instance_add_method,  
  .remove_method = write_instance_remove_method,  
  .apply = write_instance_apply,
};


void dfsch__object_native_register(dfsch_object_t *ctx){
  dfsch_defcanon_cstr(ctx, "<standard-class>", DFSCH_STANDARD_CLASS_TYPE);
  dfsch_defcanon_cstr(ctx, "<role>", DFSCH_ROLE_TYPE);

  dfsch_defcanon_cstr(ctx, "allocate-instance", 
                      DFSCH_PRIMITIVE_REF(allocate_instance));
  dfsch_defcanon_cstr(ctx, "make-class", DFSCH_PRIMITIVE_REF(make_class));
  dfsch_defcanon_cstr(ctx, "make-role", DFSCH_PRIMITIVE_REF(make_role));

  dfsch_define_method_pkgcstr(ctx, DFSCH_DFSCH_PACKAGE, "initialize-instance",
                              NULL, NULL, 
                              DFSCH_PRIMITIVE_REF(default_initialize_instance));

  dfsch_defcanon_cstr(ctx, "write-instance", &write_instance);
}
