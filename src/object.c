/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
 * Copyright (C) 2007 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, 
 * USA
 */

/*
 * Instances of standard-class created here must be separate for each 
 * top-level environment and thus cannot use any kind of caching 
 * (DFSCH_OBJECT_CACHE and friends). When they are not distinct, method
 * (re)definitions are shared too, which clearly is not desirable behavior.
 */

#include "dfsch/object.h"

#include <dfsch/hash.h>
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include "util.h"

typedef struct instance_t {
  dfsch_type_t* klass;
  dfsch_object_t* inst_vars;  // Hashmap
} instance_t;


typedef struct class_t class_t;

struct class_t {
  dfsch_type_t type;
};

static char* class_write(class_t* klass, int depth, 
                         int readable){
  str_list_t *sl = sl_create();
  char buf[sizeof(void*)*2+4];
  
  sl_append(sl, "#<standard-class ");
  snprintf(buf, sizeof(void*)*2+4, "0x%x ", klass);
  sl_append(sl, buf);
  sl_append(sl, klass->type.name);
  sl_append(sl, ">");
  
  return sl_value(sl);
}


static const dfsch_type_t class_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(class_t),
  "class",
  NULL,
  (dfsch_type_write_t)class_write,
  NULL,
  NULL
};

static const class_t standard_object = {
  {
    &class_type,
    NULL,
    sizeof(instance_t),
    "object",
    NULL,
    NULL,
    NULL,
    NULL
  }
};

static class_t* alloc_class(class_t* superclass, char* name){
  class_t* klass = (class_t*)dfsch_make_object(&class_type);

  klass->type.superclass = superclass;
  klass->type.size = sizeof(instance_t);
  klass->type.name = name;
  klass->type.equal_p = NULL;
  klass->type.write = NULL;
  klass->type.apply = NULL;
  klass->type.hash = NULL;

  return klass;
}

dfsch_object_t* dfsch_object_make_class(dfsch_object_t* superclass, 
                                        char* name){
  if (!superclass || superclass->type != &class_type)
    dfsch_error("exception:not-a-class", superclass);    

  return (dfsch_object_t*)alloc_class((class_t*) superclass, name);
}

dfsch_object_t* dfsch_object_make_instance(dfsch_object_t* klass){
  if (klass && klass->type != &class_type)
    dfsch_error("exception:not-a-class", klass);    

  instance_t* ins = dfsch_make_object(klass);
  ins->inst_vars = dfsch_hash_make(DFSCH_HASH_EQ);

  return (dfsch_object_t*)ins;
}

dfsch_object_t* dfsch_object_slot_set(dfsch_object_t* object,
                                      dfsch_object_t* name,
                                      dfsch_object_t* value){

  if (!object || !object->type || object->type->type != &class_type) // XXX
    dfsch_error("exception:not-a-class-instance", object);

  return dfsch_hash_set(((instance_t*)object)->inst_vars, name, value);  
}
int dfsch_object_slot_unset(dfsch_object_t* object,
                            dfsch_object_t* name){

  if (!object || !object->type || object->type->type != &class_type) // XXX
    dfsch_error("exception:not-a-class-instance", object);

  return dfsch_hash_unset(((instance_t*)object)->inst_vars, name);
}
dfsch_object_t* dfsch_object_slot_ref(dfsch_object_t* object,
                                      dfsch_object_t* name){
  dfsch_object_t* ret;
  if (!object || !object->type || object->type->type != &class_type) // XXX
    dfsch_error("exception:not-a-class-instance", object);

  ret = dfsch_hash_ref(((instance_t*)object)->inst_vars, name);
  if (!ret){
    dfsch_error("exception:slot-undefined", name);
  } else {
    return dfsch_car(ret);
  }
}
dfsch_object_t* dfsch_object_slots_2_alist(dfsch_object_t* object){
  if (!object || !object->type || object->type->type != &class_type) // XXX
    dfsch_error("exception:not-a-class-instance", object);

  return dfsch_hash_2_alist(((instance_t*)object)->inst_vars);
}


// Scheme binding

static dfsch_object_t* native_make_class(void* baton,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* superclass;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, superclass);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  return dfsch_object_make_class(superclass, dfsch_symbol(name));
}
static dfsch_object_t* native_make_instance(void* baton,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* klass;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_ARG_END(args);
  
  return dfsch_object_make_instance(klass);
}


static dfsch_object_t* native_form_define_class(void* baton,
                                                dfsch_object_t* args,
                                                dfsch_tail_escape_t* esc){
  dfsch_object_t* env;
  dfsch_object_t* name;
  dfsch_object_t* superclass;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, superclass);
  DFSCH_ARG_END(args);

  superclass = dfsch_eval(superclass, env);
  
  return dfsch_define(name, 
                      dfsch_object_make_class(superclass, 
                                              dfsch_symbol(name)),
                      env);
}

static dfsch_object_t* native_slot_set(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* name;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);
  
  return dfsch_object_slot_set(object, name, value);
}
static dfsch_object_t* native_slot_unset(void* baton,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  return dfsch_bool(dfsch_object_slot_unset(object, name));
}
static dfsch_object_t* native_slot_ref(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  return dfsch_object_slot_ref(object, name);
}
static dfsch_object_t* native_slots_2_alist(void* baton,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);
  
  return dfsch_object_slots_2_alist(object);
}
static dfsch_object_t* native_form_with_slots(void *baton, 
                                              dfsch_object_t* args, 
                                              dfsch_tail_escape_t* esc){
  dfsch_object_t *env;
  dfsch_object_t *object;
  dfsch_object_t *code;

  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_REST(args, code);

  object = dfsch_eval(object, env);

  if (!object || !object->type || object->type->type != &class_type){
    dfsch_error("exception:not-a-class-instance", object);
  }


  return 
    dfsch_eval_proc_tr(code, 
                       dfsch_new_frame_from_hash(env,
                                                 ((instance_t*)
                                                  object)->inst_vars),
                       NULL, esc);
}

void dfsch__object_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<class>", &class_type);
  dfsch_define_cstr(ctx, "<object>", &standard_object);

  dfsch_define_cstr(ctx, "make-class",
                    dfsch_make_primitive(native_make_class,
                                         NULL));
  dfsch_define_cstr(ctx, "make-instance",
                    dfsch_make_primitive(native_make_instance,
                                         NULL));

  dfsch_define_cstr(ctx, "define-class",
                    dfsch_make_form(dfsch_make_primitive(native_form_define_class,
                                                         NULL)));

  dfsch_define_cstr(ctx, "slot-set!",
                    dfsch_make_primitive(native_slot_set,
                                         NULL));
  dfsch_define_cstr(ctx, "slot-unset!",
                    dfsch_make_primitive(native_slot_unset,
                                         NULL));
  dfsch_define_cstr(ctx, "slot-ref",
                    dfsch_make_primitive(native_slot_ref,
                                         NULL));
  dfsch_define_cstr(ctx, "slots->alist",
                    dfsch_make_primitive(native_slots_2_alist,
                                         NULL));
  dfsch_define_cstr(ctx, "with-slots",
                    dfsch_make_form(dfsch_make_primitive(native_form_with_slots,
                                                         NULL)));
  
}
