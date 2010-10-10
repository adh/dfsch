/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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
 *
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>
#include <dfsch/number.h>
#include <dfsch/parse.h>
#include <dfsch/strings.h>
#include <dfsch/magic.h>
#include <dfsch/conditions.h>
#include <dfsch/introspect.h>
#include <dfsch/weak.h>
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include "types.h"

static uint32_t hash_combine(uint32_t a, uint32_t b){
  a ^= b >> 2;
  b ^= a >> 3;
  a ^= b << 5;
  b ^= a << 7;
  a ^= b >> 11;
  b ^= a >> 13;
  a ^= b << 17;
  b ^= a << 23;
  
  return b ^ a;
}

static dfsch_slot_t slot_slots[] = {
  DFSCH_STRING_SLOT(dfsch_slot_t, name, DFSCH_SLOT_ACCESS_RO,
                    "Slot name"),
  DFSCH_SIZE_T_SLOT(dfsch_slot_t, offset, DFSCH_SLOT_ACCESS_RO,
                    "Offset of data item in object"),
  DFSCH_INT_SLOT(dfsch_slot_t, access, DFSCH_SLOT_ACCESS_RO,
                 "Access mode of slot"),
  DFSCH_STRING_SLOT(dfsch_slot_t, documentation, DFSCH_SLOT_ACCESS_RO,
                    "Slot documentation string"),
  DFSCH_SLOT_TERMINATOR
};

static void slot_write(dfsch_slot_t* slot, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)slot, "%s", slot->name);
}

dfsch_type_t dfsch_slot_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  sizeof(dfsch_slot_t),
  "slot",

  NULL,
  (dfsch_type_write_t)slot_write,
  NULL,
  NULL,

  slot_slots,
  "Common superclass of all slot types"
};

static dfsch_slot_t slot_type_slots[] = {
  DFSCH_SIZE_T_SLOT(dfsch_slot_type_t, size, DFSCH_SLOT_ACCESS_RO,
                    "Size of slot"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_slot_type_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_slot_type_t),
  "slot-type",
  NULL,
  NULL,
  NULL,
  NULL,
  slot_type_slots,
  "Slot metaclass - describes methods for reading and writing"
};

static dfsch_object_t* object_accessor_ref(void* ptr){
  return *((dfsch_object_t**)ptr);
}
static void object_accessor_set(void* ptr, dfsch_object_t* obj){
  *((dfsch_object_t**)ptr) = obj;
}
dfsch_slot_type_t dfsch_object_slot_type = {
  DFSCH_SLOT_TYPE_HEAD("object-slot", "Generic object-pointer slot"),
  object_accessor_ref,
  object_accessor_set,
  sizeof(dfsch_object_t*)
};

static dfsch_object_t* boolean_accessor_ref(void* ptr){
  return dfsch_bool(*((int*)ptr));
}
static void boolean_accessor_set(void* ptr, dfsch_object_t* obj){
  *((int*)ptr) = (obj != NULL);
}
dfsch_slot_type_t dfsch_boolean_slot_type = {
  DFSCH_SLOT_TYPE_HEAD("boolean-slot", "Slot holding boolean value as C int"),
  boolean_accessor_ref,
  boolean_accessor_set,
  sizeof(int)
};

static dfsch_object_t* string_accessor_ref(void* ptr){
  return dfsch_make_string_cstr(*((char**)ptr));
}
static void string_accessor_set(void* ptr, dfsch_object_t* obj){
  *((char**)ptr) = dfsch_string_to_cstr(obj);
}
dfsch_slot_type_t dfsch_string_slot_type = {
  DFSCH_SLOT_TYPE_HEAD("string-slot", "Slot holding string as C char*"),
  string_accessor_ref,
  string_accessor_set,
  sizeof(char*)
};

#define INT_ACCESSOR(type, name) \
  static dfsch_object_t* name ## _accessor_ref(void* ptr){              \
    return dfsch_make_number_from_long(*((type*)ptr));                  \
  }                                                                     \
  static void name ## _accessor_set(void* ptr, dfsch_object_t* obj){    \
    *((type*)ptr) = dfsch_number_to_long(obj);                          \
  }                                                                     \
  dfsch_slot_type_t dfsch_ ## name ## _slot_type = {                    \
    DFSCH_SLOT_TYPE_HEAD(#name "-slot",                                 \
                         "Slot containing C " #name "value"),      \
    name ## _accessor_ref,                                              \
    name ## _accessor_set,                                              \
    sizeof(type)                                                        \
  };                                                                    \

INT_ACCESSOR(int, int)
INT_ACCESSOR(long, long)
INT_ACCESSOR(size_t, size_t)

dfsch_object_t* dfsch_get_slots(dfsch_type_t* type){
  dfsch_slot_t* i;
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;
  dfsch_object_t* tmp;
  while(type){
    if (type->slots){
      i = type->slots;
      while (i->type){
        tmp = dfsch_cons((dfsch_object_t*)i, NULL);
        if (!head) {
          head = tail = tmp;
        } else {
          DFSCH_FAST_CDR_MUT(tail) = tmp;
          tail = tmp;
        }
        i++;
      }
    }
    type = type->superclass;
  }
  return head;
}

dfsch_slot_t* dfsch_find_slot(dfsch_type_t* type, 
                              char* name){
  dfsch_slot_t* i;
  while(type){
    if (type->slots){
    i = type->slots;
    while (i->type){
      if (strcmp(i->name, name)==0){
        return i;
      }
      i++;
    }
    }
    type = type->superclass;
  }
  dfsch_error("No such slot", dfsch_make_symbol(name));
}

dfsch_object_t* dfsch_slot_ref(dfsch_object_t* obj, 
                               dfsch_slot_t* slot,
                               int debug){
  if (!debug && slot->access == DFSCH_SLOT_ACCESS_DEBUG_READ) {
    dfsch_error("Slot not accesible", (dfsch_object_t*)slot);
  }

  return slot->type->ref(((char*) obj)+slot->offset);
}
void dfsch_slot_set(dfsch_object_t* obj, 
                    dfsch_slot_t* slot, 
                    dfsch_object_t* value, 
                    int debug){
  if (!(slot->access == DFSCH_SLOT_ACCESS_RW || 
        (debug && slot->access == DFSCH_SLOT_ACCESS_DEBUG_WRITE))) {
    dfsch_error("Slot not accesible", (dfsch_object_t*)slot);
  }
  
  slot->type->set(((char*) obj)+slot->offset, value);
}
dfsch_object_t* dfsch_slot_ref_by_name(dfsch_object_t* obj, 
                                       char* slot,
                                       int debug){
  return dfsch_slot_ref(obj, dfsch_find_slot(DFSCH_TYPE_OF(obj),
                                             slot),
                        debug);
}
void dfsch_slot_set_by_name(dfsch_object_t* obj, 
                            char* slot, 
                            dfsch_object_t* value,
                            int debug){
  dfsch_slot_set(obj, dfsch_find_slot(DFSCH_TYPE_OF(obj),
                                      slot), 
                 value,
                 debug);
}

typedef struct slot_accessor_t {
  dfsch_type_t* type;
  dfsch_type_t* instance_class;
  dfsch_slot_t* slot;
} slot_accessor_t;

static dfsch_slot_t slot_accessor_slots[]={
  DFSCH_OBJECT_SLOT(slot_accessor_t, instance_class, DFSCH_SLOT_ACCESS_RO,
                    "Class containing applicable slot"),
  DFSCH_OBJECT_SLOT(slot_accessor_t, slot, DFSCH_SLOT_ACCESS_RO,
                    "Accessed slot"),
  DFSCH_SLOT_TERMINATOR
};

static dfsch_object_t* slot_accessor_apply(slot_accessor_t* sa,
                                           dfsch_object_t* args,
                                           dfsch_tail_escape_t* esc,
                                           dfsch_object_t* context){
  dfsch_object_t* instance;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, instance);
  DFSCH_OBJECT_ARG_OPT(args, value, DFSCH_INVALID_OBJECT);
  DFSCH_ARG_END(args);

  instance = DFSCH_ASSERT_INSTANCE(instance, sa->instance_class);

  if (value == DFSCH_INVALID_OBJECT){
    return dfsch_slot_ref(instance, sa->slot, 0);
  } else {
    dfsch_slot_set(instance, sa->slot, value, 0);
    return value;
  }
}

static void slot_accessor_write(slot_accessor_t* sa, 
                                dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)sa, 
                         "%s @ %s", sa->slot->name, sa->instance_class->name);
}

dfsch_type_t dfsch_slot_accessor_type = {
  .type          = DFSCH_STANDARD_TYPE,
  .superclass    = DFSCH_FUNCTION_TYPE,
  .size          = sizeof(slot_accessor_t),
  .name          = "slot-accessor",
  .apply         = (dfsch_type_apply_t)slot_accessor_apply,
  .write         = (dfsch_type_write_t)slot_accessor_write,
  .slots         = slot_accessor_slots,
  .documentation = "Slot accessor allows direct access to slots (runtime-only)"
};

dfsch_object_t* dfsch__make_slot_accessor_for_slot(dfsch_type_t* type,
                                                   dfsch_slot_t* slot){
  slot_accessor_t* sa = 
    (slot_accessor_t*) dfsch_make_object(DFSCH_SLOT_ACCESSOR_TYPE);

  sa->instance_class = type;
  sa->slot = slot;
  return (dfsch_object_t*) sa;
}
dfsch_object_t* dfsch_make_slot_accessor(dfsch_type_t* type,
                                         char* slot){
  return dfsch__make_slot_accessor_for_slot(type,
                                            dfsch_find_slot(type, slot));
}

static dfsch_object_t* slot_reader_apply(slot_accessor_t* sa,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc,
                                         dfsch_object_t* context){
  dfsch_object_t* instance;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, instance);
  DFSCH_ARG_END(args);

  instance = DFSCH_ASSERT_INSTANCE(instance, sa->instance_class);

  return dfsch_slot_ref(instance, sa->slot, 0);
}
static void slot_reader_write(slot_accessor_t* sa, 
                              dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)sa, 
                         "%s @ %s", sa->slot->name, sa->instance_class->name);
}

dfsch_type_t dfsch_slot_reader_type = {
  .type          = DFSCH_STANDARD_TYPE,
  .superclass    = DFSCH_FUNCTION_TYPE,
  .size          = sizeof(slot_accessor_t),
  .name          = "slot-reader",
  .apply         = (dfsch_type_apply_t)slot_accessor_apply,
  .write         = (dfsch_type_write_t)slot_accessor_write,
  .slots         = slot_accessor_slots,
  .documentation = "Slot reader allows direct reading of slot value"
};

dfsch_object_t* dfsch__make_slot_reader_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot){
  slot_accessor_t* sa = 
    (slot_accessor_t*) dfsch_make_object(DFSCH_SLOT_READER_TYPE);
  
  sa->instance_class = type;
  sa->slot = slot;
  return (dfsch_object_t*) sa;
}
dfsch_object_t* dfsch_make_slot_reader(dfsch_type_t* type,
                                         char* slot){
  return dfsch__make_slot_reader_for_slot(type,
                                          dfsch_find_slot(type, slot));
}
static dfsch_object_t* slot_writer_apply(slot_accessor_t* sa,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc,
                                         dfsch_object_t* context){
  dfsch_object_t* instance;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, instance);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  instance = DFSCH_ASSERT_INSTANCE(instance, sa->instance_class);

  dfsch_slot_set(instance, sa->slot, value, 0);
  return value;
}

static void slot_writer_write(slot_accessor_t* sa, 
                                dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)sa, 
                         "%s @ %s", sa->slot->name, sa->instance_class->name);
}

dfsch_type_t dfsch_slot_writer_type = {
  .type          = DFSCH_STANDARD_TYPE,
  .superclass    = DFSCH_FUNCTION_TYPE,
  .size          = sizeof(slot_accessor_t),
  .name          = "slot-writer",
  .apply         = (dfsch_type_apply_t)slot_accessor_apply,
  .write         = (dfsch_type_write_t)slot_accessor_write,
  .slots         = slot_accessor_slots,
  .documentation = "Slot writer allows direct modification of slot value"
};

dfsch_object_t* dfsch__make_slot_writer_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot){
  slot_accessor_t* sa = 
    (slot_accessor_t*) dfsch_make_object(DFSCH_SLOT_WRITER_TYPE);

  sa->instance_class = type;
  sa->slot = slot;
  return (dfsch_object_t*) sa;
}
dfsch_object_t* dfsch_make_slot_writer(dfsch_type_t* type,
                                       char* slot){
  return dfsch__make_slot_writer_for_slot(type,
                                          dfsch_find_slot(type, slot));
}



dfsch_type_t dfsch_abstract_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_type_t),
  "abstract-type",
  NULL,
  NULL,
  NULL
};

dfsch_type_t dfsch_meta_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_type_t),
  "meta-type",
  NULL,
  NULL,
  NULL
};

dfsch_type_t dfsch_iterator_type_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_type_t),
  "iterator-type",
  NULL,
  NULL,
  NULL
};

static dfsch_object_t* iterator_get_iterator(dfsch_object_t* i){
  return i;
}

dfsch_collection_methods_t dfsch_iterator_collection_methods = {
  .get_iterator = iterator_get_iterator,
};

dfsch_type_t dfsch_iterator_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "iterator",
  NULL,
  NULL,
  NULL,

  .collection = &dfsch_iterator_collection_methods,
};


static void type_write(dfsch_type_t* t, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)t, 
                         "%s instance-size: %d", t->name, t->size);
}

static dfsch_slot_t type_slots[] = {
  DFSCH_STRING_SLOT(dfsch_type_t, name, DFSCH_SLOT_ACCESS_RO,
                    "Type name"),
  DFSCH_STRING_SLOT(dfsch_type_t, documentation, DFSCH_SLOT_ACCESS_RO,
                    "Documentation string"),
  DFSCH_OBJECT_SLOT(dfsch_type_t, superclass, DFSCH_SLOT_ACCESS_RO,
                    "Superclass"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_standard_type = {
  DFSCH_META_TYPE,
  NULL,
  sizeof(dfsch_type_t),
  "standard-type",
  NULL,
  (dfsch_type_write_t)type_write,
  NULL,
  NULL,
  type_slots,
  "Base metaclass representing common methods of all objects"
};

dfsch_type_t dfsch_special_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_type_t),
  "special-type",
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "Metaclass of types with special in-memory representation"
};

static dfsch_object_t* list_get_iterator(dfsch_object_t* l){
  return l;
}

static dfsch_collection_methods_t list_collection = {
  .get_iterator = list_get_iterator,
};

static dfsch_sequence_methods_t list_sequence = {
  .ref = dfsch_list_item,
  .set = dfsch_set_list_item,
  .length = dfsch_list_length,
};

dfsch_type_t dfsch_list_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "list",
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "Abstract superclass of list-like objects"

};

dfsch_type_t dfsch_function_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "function",
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "Abstract superclass for functions"
  /* N.B.: anything could work as function, this is only notational 
   *       convention */
};


dfsch_type_t dfsch_empty_list_type = {
  DFSCH_SPECIAL_TYPE,
  DFSCH_LIST_TYPE,
  0,
  "empty-list",
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "Class with only one instance - ()",

  .collection = &list_collection,
  .sequence = &list_sequence,  
};

static int pair_equal_p(dfsch_object_t*a, dfsch_object_t*b){
  return dfsch_equal_p(DFSCH_FAST_CAR(a), DFSCH_FAST_CAR(b)) 
    && dfsch_equal_p(DFSCH_FAST_CDR(a), DFSCH_FAST_CDR(b));
}
static size_t pair_hash(dfsch_object_t* p){
  return hash_combine(dfsch_hash(DFSCH_FAST_CAR(p)), 
                      dfsch_hash(DFSCH_FAST_CDR(p)));
}
static void pair_write(dfsch_object_t*p, dfsch_writer_state_t* state){
  object_t* i=p;
  object_t* j=p;
  int c = 0;
    
  dfsch_write_string(state, "(");
  while (DFSCH_PAIR_P(i)){
    dfsch_write_object(state, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
    if (i == j){
      dfsch_write_string(state, "... #<infinite-list>)");
      return;
    }

    c++;
    if (c == 2){
      c = 0;
      j = DFSCH_FAST_CDR(j);
    }
    if (i) {
      dfsch_write_string(state, " ");  
    }
  }
  
  if (i){  
    dfsch_write_string(state, ". ");  
    dfsch_write_object(state, i);
  }

  dfsch_write_string(state, ")");  
}
dfsch_type_t dfsch_pair_type = {
  DFSCH_ABSTRACT_TYPE,
  DFSCH_LIST_TYPE,
  sizeof(dfsch_pair_t), 
  "pair",
  (dfsch_type_equal_p_t)pair_equal_p,
  (dfsch_type_write_t)pair_write,
  NULL,
  (dfsch_type_hash_t)pair_hash,
  NULL,
  "Abstract superclass for all pair representations"
};
#define PAIR (&dfsch_pair_type)

static void symbol_write(object_t* o, dfsch_writer_state_t* state){
  symbol_t* s;
  s = DFSCH_TAG_REF(o);
  if (s->name){
    if (!s->package){
      dfsch_write_string(state, dfsch_saprintf("#<uninterned-symbol %p %s>", 
                                               o, 
                                               s->name)); 
    } else {
      if (!dfsch_in_current_package(o)) {
        if (s->package != DFSCH_KEYWORD_PACKAGE) {
          dfsch_write_string(state, dfsch_package_name(s->package));
        }
        dfsch_write_string(state, ":");      
      }
      dfsch_write_string(state, s->name);
    }
  } else {
    dfsch_write_string(state, dfsch_saprintf("#<gensym %p>", o)); 
  }
}


dfsch_type_t dfsch_tagged_types[4] = {
  {
    DFSCH_SPECIAL_TYPE,
    DFSCH_IMMUTABLE_PAIR_TYPE,
    sizeof(dfsch_pair_t), 
    "compact-list",
    (dfsch_type_equal_p_t)pair_equal_p,
    (dfsch_type_write_t)pair_write,
    NULL,
    (dfsch_type_hash_t)pair_hash,
    NULL,
    "Immutable list stored as array",

    .collection = &list_collection,
    .sequence = &list_sequence,  
  },
  {
    DFSCH_SPECIAL_TYPE,
    DFSCH_PAIR_TYPE,
    sizeof(dfsch_pair_t), 
    "mutable-pair",
    (dfsch_type_equal_p_t)pair_equal_p,
    (dfsch_type_write_t)pair_write,
    NULL,
    (dfsch_type_hash_t)pair_hash,
    NULL,
    "Normal mutable cons cell",

    .collection = &list_collection,
    .sequence = &list_sequence,  
  },
  {
    DFSCH_STANDARD_TYPE,
    NULL,
    sizeof(symbol_t), 
    "symbol",
    NULL,
    (dfsch_type_write_t)symbol_write,
    NULL,
    NULL,
    NULL,
    "Symbol - equal? instances are always eq?",
    DFSCH_TYPEF_NO_WEAK_REFERENCES
  },
  {
    DFSCH_SPECIAL_TYPE,
    DFSCH_PAIR_TYPE,
    sizeof(dfsch_pair_t), 
    "immutable-pair",
    (dfsch_type_equal_p_t)pair_equal_p,
    (dfsch_type_write_t)pair_write,
    NULL,
    (dfsch_type_hash_t)pair_hash,
    NULL,
    "Immutable cons cell",

    .collection = &list_collection,
    .sequence = &list_sequence,  
  },
};



static void primitive_write(dfsch_primitive_t* p, 
                            dfsch_writer_state_t* state){
  char* name = p->name ? p->name : "";
  dfsch_write_unreadable(state, (dfsch_object_t*)p, "%s", name);
}

static dfsch_slot_t primitive_slots[] = {
  DFSCH_STRING_SLOT(dfsch_primitive_t, name, DFSCH_SLOT_ACCESS_RO,
                    "Primitive's internal name"),
  DFSCH_STRING_SLOT(dfsch_primitive_t, documentation, DFSCH_SLOT_ACCESS_RO,
                    "Documentation string"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_primitive_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_STANDARD_FUNCTION_TYPE,
  sizeof(primitive_t),
  "primitive",
  NULL,
  (dfsch_type_write_t)primitive_write,
  NULL,
  NULL,
  primitive_slots,
  "Function implemented in C code"
};
#define PRIMITIVE (&dfsch_primitive_type)

static void print_lambda_list(lambda_list_t* ll, dfsch_writer_state_t* ws){
  int i;
  for (i = 0; i < ll->positional_count; i++){
    dfsch_write_object(ws, ll->arg_list[i]);
    dfsch_write_string(ws, " ");
  }
  if (ll->optional_count > 0){
    dfsch_write_string(ws, "&optional ");
    for (i = 0; i < ll->optional_count; i++){
      dfsch_write_object(ws, ll->arg_list[i + ll->positional_count]);
      dfsch_write_string(ws, " ");
    }
  }
  if (ll->rest){
    if (ll->flags & LL_FLAG_REST_IS_BODY){
      dfsch_write_string(ws, "&body ");
    } else {
      dfsch_write_string(ws, "&rest ");
    }
    dfsch_write_object(ws, ll->rest);    
    dfsch_write_string(ws, " ");
  }
  if (ll->keyword_count > 0){
    dfsch_write_string(ws, "&key ");
    for (i = 0; i < ll->keyword_count; i++){
      dfsch_write_object(ws, ll->arg_list[i + ll->positional_count 
                                          + ll->optional_count]);
      dfsch_write_string(ws, " ");
    }
  }
}

static void function_write(closure_t* c, dfsch_writer_state_t* state){
  dfsch_write_unreadable_start(state, (dfsch_object_t*)c);

  if (c->name){
    dfsch_write_object(state, c->name);
  }
  dfsch_write_string(state, " (");
  print_lambda_list(c->args, state);
  dfsch_write_string(state, ")");

  dfsch_write_unreadable_end(state);
}

static dfsch_slot_t closure_slots[] = {
  DFSCH_OBJECT_SLOT(closure_t, args, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Arguments of function (compiled lambda-list)"),
  DFSCH_OBJECT_SLOT(closure_t, code, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Actual implementation code"),
  DFSCH_OBJECT_SLOT(closure_t, env, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Lexically enclosing environment"),
  DFSCH_OBJECT_SLOT(closure_t, name, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Primitive name"),
  DFSCH_OBJECT_SLOT(closure_t, orig_code, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Original expression of closure"),
  DFSCH_OBJECT_SLOT(closure_t, documentation, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Documentation string"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_standard_function_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_FUNCTION_TYPE,
  sizeof(closure_t),
  "function",
  NULL,
  (dfsch_type_write_t)function_write,
  NULL,
  NULL,
  closure_slots,
  "User defined function"
};
#define FUNCTION DFSCH_STANDARD_FUNCTION_TYPE

static dfsch_slot_t macro_slots[] = {
  DFSCH_OBJECT_SLOT(macro_t, proc, DFSCH_SLOT_ACCESS_RO,
                    "Procedure implementing macro"),
};

static void macro_write(macro_t* m, dfsch_writer_state_t* state){
  if (dfsch_primitive_p(m->proc)){
    dfsch_write_unreadable(state, (dfsch_object_t*)m,
                           "%%%s", 
                           ((dfsch_primitive_t*)m->proc)->name);
  } else {
    dfsch_write_unreadable_start(state, (dfsch_object_t*)m);
    dfsch_write_object(state, m->proc);
    dfsch_write_unreadable_end(state);
  }
}

dfsch_type_t dfsch_macro_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(macro_t),
  "macro",
  NULL,
  (dfsch_type_write_t)macro_write,
  NULL,
  NULL,
  macro_slots,
  "Macro implemented by arbitrary function"
};
#define MACRO DFSCH_MACRO_TYPE

static void form_write(dfsch_form_t* f, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)f,
                         "%s", f->name);
}
static dfsch_slot_t form_slots[] = {
  DFSCH_STRING_SLOT(dfsch_form_t, name, DFSCH_SLOT_ACCESS_RO,
                    "Internal name of special form"),
  DFSCH_STRING_SLOT(dfsch_form_t, documentation, DFSCH_SLOT_ACCESS_RO,
                    "Documentation string"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_form_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_form_t),
  "form",
  NULL,
  (dfsch_type_write_t)form_write,
  NULL,
  NULL,
  form_slots,
  "C-implemented special form"
};

static int vector_equal_p(vector_t* a, vector_t* b){
  size_t i;
  if (a->length != b->length)
    return 0;

  for (i=0; i<a->length; i++){
    if (!dfsch_equal_p(a->data[i], b->data[i]))
      return 0;
  }

  return 1;
}

static size_t vector_hash(vector_t* v){
  size_t i;
  size_t ret = 0;

  for (i=0; i<v->length; i++){
    ret = hash_combine(ret, dfsch_hash(v->data[i]));
  }

  return ret;
}


static void vector_write(vector_t* v, dfsch_writer_state_t* state){
  size_t i;
  dfsch_write_string(state, "#(");
  
  if (v->length > 0){
    for(i = 0; i < v->length-1; ++i){
      dfsch_write_object(state, v->data[i]);
      dfsch_write_string(state, " ");
    }
    dfsch_write_object(state, v->data[v->length - 1]);
  }
  dfsch_write_string(state, ")");
}

static dfsch_collection_methods_t vector_collection = {
  .get_iterator = dfsch_vector_2_list // TODO
};

static dfsch_sequence_methods_t vector_sequence = {
  .ref = dfsch_vector_ref,
  .set = dfsch_vector_set,
  .length = dfsch_vector_length
};
static dfsch_object_t* vector_describe(vector_t* v){
  dfsch_list_collector_t *lc = dfsch_make_list_collector();
  int i;

  for(i = 0; i < v->length; ++i){
    dfsch_list_collect(lc, dfsch_list(2, NULL, v->data[i]));
  }

  return dfsch_cons(dfsch_make_string_cstr("vector"),
                    dfsch_collected_list(lc));
}

dfsch_type_t dfsch_vector_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(vector_t),
  "vector",
  (dfsch_type_equal_p_t)vector_equal_p,
  (dfsch_type_write_t)vector_write,
  NULL,
  (dfsch_type_hash_t)vector_hash,
  .describe = (dfsch_type_describe_t)vector_describe,
  .collection = &vector_collection,
  .sequence = &vector_sequence,
};
#define VECTOR DFSCH_VECTOR_TYPE

static dfsch_object_t* environment_describe(environment_t* env){
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  dfsch_object_t* list = dfsch_eqhash_2_alist(&(env->values));
  
  list = dfsch_cons(dfsch_list(2,
                               dfsch_make_string_cstr("*context*"),
                               env->context),
                    list);
  list = dfsch_cons(dfsch_list(2,
                               dfsch_make_string_cstr("*declarations*"),
                               env->decls),
                    list);
  list = dfsch_cons(dfsch_list(2,
                               dfsch_make_string_cstr("*parent*"),
                               env->parent),
                    list);

  return dfsch_cons(dfsch_make_string_cstr("environment-frame"), list);
}

static dfsch_slot_t environment_slots[] = {
  DFSCH_OBJECT_SLOT(environment_t, parent, DFSCH_SLOT_ACCESS_DEBUG_WRITE,
                    "Enclosing environment or NIL for top-level"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_environment_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(environment_t),
  "environment",

  NULL,
  NULL,
  NULL,
  NULL,
  
  environment_slots,
  "Lexical environment frame",
  .describe = environment_describe,
};

static void lambda_list_write(lambda_list_t* ll, dfsch_writer_state_t* ws){
  dfsch_write_unreadable_start(ws, (dfsch_object_t*)ll);
  print_lambda_list(ll, ws);
  dfsch_write_unreadable_end(ws);
}
dfsch_type_t dfsch_lambda_list_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(lambda_list_t),
  "lambda-list",
  NULL,
  (dfsch_type_write_t)lambda_list_write,
  NULL,
  NULL,
  NULL,
  "Compiled lambda-list for effective destructuring"
};

/* ***** Object predicates ***** */

int dfsch_null_p(dfsch_object_t* obj){
  return !obj;
}
int dfsch_empty_p(dfsch_object_t* list){
  return !DFSCH_ASSERT_INSTANCE(list, DFSCH_LIST_TYPE);
}

int dfsch_pair_p(dfsch_object_t* obj){
  return DFSCH_PAIR_P(obj);
}
int dfsch_list_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_LIST_TYPE);
}
int dfsch_atom_p(dfsch_object_t* obj){
  return !DFSCH_PAIR_P(obj);
}
int dfsch_primitive_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == PRIMITIVE;

}
int dfsch_function_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == FUNCTION;
}
int dfsch_procedure_p(dfsch_object_t* obj){
  return (DFSCH_TYPE_OF(obj) == PRIMITIVE || DFSCH_TYPE_OF(obj) == FUNCTION) || 
    (DFSCH_TYPE_OF(obj)->apply);
}
int dfsch_macro_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == MACRO;
}
int dfsch_form_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == DFSCH_FORM_TYPE;
}

int dfsch_vector_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == VECTOR;
}


dfsch_object_t* dfsch_nil(){
  return NULL;
}

// Pairs

dfsch_object_t* dfsch_cons(dfsch_object_t* car, dfsch_object_t* cdr){
  dfsch_pair_t* p = GC_NEW(dfsch_pair_t);

  p->car = car;
  p->cdr = cdr;

  return DFSCH_TAG_ENCODE(p, 1);
}

dfsch_object_t* dfsch_cons_immutable(dfsch_object_t* car, dfsch_object_t* cdr){
  dfsch_pair_t* p = GC_NEW(dfsch_pair_t);

  p->car = car;
  p->cdr = cdr;

  return DFSCH_TAG_ENCODE(p, 3);
}



dfsch_object_t* dfsch_multicons(size_t n){
  size_t i;
  dfsch_pair_t* p;

  if (n == 0){
    return NULL;
  }

  p = GC_MALLOC(sizeof(dfsch_pair_t)*n);

  for (i = 0; i < (n-1); i++){
    p[i].cdr = DFSCH_TAG_ENCODE(&(p[i+1]), 1);
  }

  return DFSCH_TAG_ENCODE(&(p[0]), 1);
}

dfsch_object_t* dfsch_car(dfsch_object_t* pair){
  dfsch_object_t* p = DFSCH_ASSERT_PAIR(pair);
  return DFSCH_FAST_CAR(p);
}
dfsch_object_t* dfsch_cdr(dfsch_object_t* pair){
  dfsch_object_t* p = DFSCH_ASSERT_PAIR(pair);
  return DFSCH_FAST_CDR(p);
}

dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
			      dfsch_object_t* car){
  dfsch_object_t* p = DFSCH_ASSERT_TYPE(pair, DFSCH_MUTABLE_PAIR_TYPE);

  DFSCH_FAST_CAR(p) = car;
  
  return p;
}
dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
			      dfsch_object_t* cdr){
  dfsch_object_t* p = DFSCH_ASSERT_TYPE(pair, DFSCH_MUTABLE_PAIR_TYPE);

  DFSCH_FAST_CDR_MUT(p) = cdr;
  
  return p;
}
long dfsch_list_length_fast(object_t* list){
  dfsch_object_t *i;
  long count;

  if (!list)
    return 0;

  if (!DFSCH_PAIR_P(list))
    return -1;

  i = list;
  count = 0;

  while (DFSCH_PAIR_P(i)){
    i = DFSCH_FAST_CDR(i);
    ++count;
  }

  return count;
}

long dfsch_list_length_fast_bounded(object_t* list){
  dfsch_object_t *i;
  long count;

  if (!list)
    return 0;

  if (!DFSCH_PAIR_P(list))
    return -1;

  i = list;
  count = 0;

  while (DFSCH_PAIR_P(i)){
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (count > 65535){
      dfsch_error("Internal error: List too long", list);
    }
  }

  return count;
}

long dfsch_list_length(object_t* list, int *proper){
  dfsch_object_t *i;
  dfsch_object_t *j; 
  long count;

  if (!list)
    return 0;

  if (!DFSCH_PAIR_P(list))
    return -1;

  i = j = list;
  count = 0;

  while (DFSCH_PAIR_P(i)){
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (i == j) {
      return -1;
    }
    j = DFSCH_FAST_CDR(j);
    if (!DFSCH_PAIR_P(i)){
      break;
    }
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (i == j) {
      return -1;
    }
  }

  if (proper){
    *proper = (i == NULL);
  } else { 
    if (i) {
      return -1;
    }
  } 

  return count;
}

int dfsch_list_mutable_p(object_t* list){
  dfsch_object_t *i;
  dfsch_object_t *j; 
  long count;

  if (!list)
    return 1;

  if (DFSCH_TYPE_OF(list) != DFSCH_MUTABLE_PAIR_TYPE){
    return 0;
  }

  i = j = list;
  count = 0;

  while (DFSCH_TYPE_OF(i) == DFSCH_MUTABLE_PAIR_TYPE){
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (i == j) {
      return 0;
    }
    j = DFSCH_FAST_CDR(j);
    if (DFSCH_TYPE_OF(i) != DFSCH_MUTABLE_PAIR_TYPE){
      break;
    }
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (i == j) {
      return 0;
    }
  }

  if (i)
    return 0;

  return 1;
}

dfsch_object_t* dfsch_ensure_mutable_list(dfsch_object_t* list){
  if (dfsch_list_mutable_p(list)){
    return list;
  }
  dfsch_list_length_check(list);
  return dfsch_list_copy(list);
}

long dfsch_list_length_check(object_t* list){
  long len;
  len = dfsch_list_length(list, NULL);
  if (len < 0)
    dfsch_type_error(list, DFSCH_LIST_TYPE, 1);
  return len;
}

struct dfsch_list_collector_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;
};
dfsch_list_collector_t* dfsch_make_list_collector(){
  dfsch_list_collector_t* col = GC_NEW(dfsch_list_collector_t);

  col->head = col->tail = NULL;

  return col;
}
void dfsch_list_collect(dfsch_list_collector_t* col,
                        dfsch_object_t* item){
  dfsch_object_t* tp = dfsch_cons(item, NULL);

  if (col->head){
    DFSCH_FAST_CDR_MUT(col->tail) = tp;
  } else {
    col->head = tp;
  }
  col->tail = tp;
}
dfsch_object_t* dfsch_collected_list(dfsch_list_collector_t* col){
  return col->head;
}


dfsch_object_t* dfsch_list_item(dfsch_object_t* list, size_t index){
  dfsch_object_t* it = list;
  int i;
  for (i=0; i<index; ++i){
    if (DFSCH_PAIR_P(it)){
      it = DFSCH_FAST_CDR(it);
    }else{
      dfsch_error("No such item of list", dfsch_make_number_from_long(index));
    }
  }
  return dfsch_car(it);
}

void dfsch_set_list_item(dfsch_object_t* list, 
                         size_t index,
                         dfsch_object_t* value){
  dfsch_object_t* it = list;
  int i;
  for (i=0; i<index; ++i){
    if (DFSCH_PAIR_P(it)){
      it = DFSCH_FAST_CDR(it);
    }else{
      dfsch_error("No such item of list", dfsch_make_number_from_long(index));
    }
  }
  dfsch_set_car(it, value);
}


dfsch_object_t* dfsch_list_from_array(dfsch_object_t** array, size_t length){
  dfsch_object_t *head; 
  dfsch_object_t *cur;
  size_t i;

  if (length == 0)
    return NULL;

  head = cur = dfsch_multicons(length);

  for(i = 0; i < length; ++i){
    DFSCH_FAST_CAR(cur) = array[i];
    cur = DFSCH_FAST_CDR(cur);
  }

  return (object_t*)head;
}
dfsch_object_t** dfsch_list_as_array(dfsch_object_t* list, size_t* length){
  dfsch_object_t* j = list;
  size_t i=0;
  size_t len;
  object_t** data;

  len = dfsch_list_length_check(list);
  data = GC_MALLOC(sizeof(object_t*)*len);
  
  while (DFSCH_PAIR_P(j)){
    if (i >= len){
      break; /* Can happen due to race condition in user code */
    }
    data[i] = DFSCH_FAST_CAR(j);
    j = DFSCH_FAST_CDR(j);
    i++;
  }

  if (length){
    *length = len;
  }

  return data;
}

dfsch_object_t* dfsch_list_copy_immutable(dfsch_object_t* list){
  dfsch_object_t* j = list;
  size_t i=0;
  size_t len;
  object_t** data;
  int proper;

  if (list == NULL){
    return NULL;
  }

  len = dfsch_list_length(list, &proper);

  if (len == 1){
    return dfsch_cons_immutable(dfsch_car(list),
                                dfsch_cdr(list));
  }

  data = GC_MALLOC(sizeof(object_t*)*(len+4));
  
  while (DFSCH_PAIR_P(j)){
    if (i >= len){
      break; /* Can happen due to race condition in user code */
    }
    data[i] = DFSCH_FAST_CAR(j);
    j = DFSCH_FAST_CDR(j);
    i++;
  }

  data[i] = DFSCH_INVALID_OBJECT;
  i++;
  data[i] = j;
  data[i+1] = NULL;
  data[i+2] = NULL;

  return DFSCH_MAKE_CLIST(data);
}

dfsch_object_t* dfsch_null_immutable_list(size_t l){
  dfsch_object_t** data;

  if (l == 0){
    return NULL;
  }

  data = GC_MALLOC(sizeof(object_t*)*(l+4));
  
  memset(data, 0, l*sizeof(object_t*));

  data[l] = DFSCH_INVALID_OBJECT;
  data[l+1] = NULL;
  data[l+2] = NULL;
  data[l+3] = NULL;

  return DFSCH_MAKE_CLIST(data);
}


dfsch_object_t* dfsch_list_annotate(dfsch_object_t* list, 
                                    dfsch_object_t* source,
                                    dfsch_object_t* location){
  dfsch_object_t* j = list;
  size_t i=0;
  size_t len;
  object_t** data;
  int proper;

  if (list == NULL){
    return NULL;
  }

  len = dfsch_list_length(list, &proper);

  data = GC_MALLOC(sizeof(object_t*)*(len+4));
  
  while (DFSCH_PAIR_P(j)){
    if (i >= len){
      break; /* Can happen due to race condition in user code */
    }
    data[i] = DFSCH_FAST_CAR(j);
    j = DFSCH_FAST_CDR(j);
    i++;
  }

  data[i] = DFSCH_INVALID_OBJECT;
  data[i+1] = j;
  data[i+2] = source;
  data[i+3] = location;

  return DFSCH_MAKE_CLIST(data);
}

dfsch_object_t* dfsch_get_list_annotation(dfsch_object_t* list){
  dfsch_object_t** i;
  if (!DFSCH_PAIR_P(list) || !DFSCH__FAST_CDR_CODED_P(list)){
    return NULL;
  }

  i = (dfsch_object_t**) DFSCH_PAIR_REF(list);

  while (*i != DFSCH_INVALID_OBJECT){
    i++;
  }

  /* i[1] is last CDR */

  if (i[2] || i[3]){
    return dfsch_cons(i[2], i[3]);
  } else {
    return NULL;
  }
}


dfsch_object_t* dfsch_zip(dfsch_object_t* llist){
  size_t len;
  object_t** args = dfsch_list_as_array(llist, &len);

  dfsch_object_t* shead;
  dfsch_object_t* stail;

  dfsch_object_t *head = NULL; 
  dfsch_object_t *tail;

  dfsch_object_t* tmp;
  size_t i;

  if (len == 0){
    return NULL;
  }

  while(1){
    shead = NULL;

    for (i = 0; i<len; i++){
      if (!args[i]){
        return head;
      }
      if (!DFSCH_PAIR_P(args[i])){
	dfsch_type_error(args[i], DFSCH_PAIR_TYPE, 0);
      }

      tmp = dfsch_cons(DFSCH_FAST_CAR(args[i]), NULL);
      if (shead){
        DFSCH_FAST_CDR_MUT(stail) = tmp;
      } else {
        shead = tmp;
      }
      stail = tmp;

      args[i] = DFSCH_FAST_CDR(args[i]);
    }


    tmp = dfsch_cons(shead, NULL);
    if (head){
      DFSCH_FAST_CDR_MUT(tail) = tmp;
    } else {
      head = tmp;
    }
    tail = tmp;

  }
}


dfsch_object_t* dfsch_append(dfsch_object_t* llist){
  dfsch_object_t* head=NULL;
  dfsch_object_t* tail=NULL;
  dfsch_object_t* i = llist;
  dfsch_object_t* j;

  if (!llist)
    return NULL;

  while(DFSCH_PAIR_P(i) && DFSCH_PAIR_P(DFSCH_FAST_CDR(i))){
    
    j = DFSCH_FAST_CAR(i);
    while(DFSCH_PAIR_P(j)){
      if (head){
        object_t* tmp = dfsch_cons(DFSCH_FAST_CAR(j),NULL);
        DFSCH_FAST_CDR_MUT(tail) = tmp;
        tail = tmp;
      }else{
        head = tail = dfsch_cons(DFSCH_FAST_CAR(j),NULL);
      }
      j = DFSCH_FAST_CDR(j);
    }
    if (j && !DFSCH_PAIR_P(j)) {
      dfsch_type_error(j, PAIR, 0);
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (!DFSCH_PAIR_P(i)){
    dfsch_type_error(i, PAIR, 0);
  }

  if (tail){
    DFSCH_FAST_CDR_MUT(tail) = DFSCH_FAST_CAR(i);
  }else{
    head = DFSCH_FAST_CAR(i);
  }

  return head;
}

dfsch_object_t* dfsch_list(size_t count, ...){
  dfsch_object_t *head; 
  dfsch_object_t *cur;
  size_t i;
  va_list al;

  va_start(al,count);

  if (count == 0)
    return NULL;

  head = cur = dfsch_multicons(count);

  for(i = 0; i < count; ++i){
    DFSCH_FAST_CAR(cur) = va_arg(al, dfsch_object_t*);
    cur = DFSCH_FAST_CDR(cur);
  }

  va_end(al);
  return head;
}

dfsch_object_t* dfsch_immutable_list(size_t count, ...){
  size_t i;
  object_t** data;
  va_list al;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();

  va_start(al, count);

  if (count == 0){
    return NULL;
  }

  data = GC_MALLOC(sizeof(object_t*)*(count+4));
  
  for(i = 0; i < count; i++){
    data[i] = va_arg(al, dfsch_object_t*);
  }

  data[i] = DFSCH_INVALID_OBJECT;
  i++;
  data[i] = NULL; // CDR
  if (ti->macroexpanded_expr){
    data[i+1] = DFSCH_SYM_MACRO_EXPANDED_FROM;
    data[i+2] = ti->macroexpanded_expr;
  } else {
    data[i+1] = NULL;
    data[i+2] = NULL;
  }

  va_end(al);
  return DFSCH_MAKE_CLIST(data);
}

dfsch_object_t* dfsch_immutable_list_cdr(dfsch_object_t* cdr,
                                         size_t count, ...){
  size_t i;
  object_t** data;
  va_list al;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();

  va_start(al, count);

  if (count == 0){
    return NULL;
  }

  data = GC_MALLOC(sizeof(object_t*)*(count+4));
  
  for(i = 0; i < count; i++){
    data[i] = va_arg(al, dfsch_object_t*);
  }

  data[i] = DFSCH_INVALID_OBJECT;
  i++;
  data[i] = cdr; 
  if (ti->macroexpanded_expr){
    data[i+1] = DFSCH_SYM_MACRO_EXPANDED_FROM;
    data[i+2] = ti->macroexpanded_expr;
  } else {
    data[i+1] = NULL;
    data[i+2] = NULL;
  }

  va_end(al);
  return DFSCH_MAKE_CLIST(data);
}



dfsch_object_t* dfsch_list_copy(dfsch_object_t* list){
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  dfsch_object_t *i =  list;

  head = tail = NULL;

  while(DFSCH_PAIR_P(i)){
    if (head){
      object_t* tmp = dfsch_cons(DFSCH_FAST_CAR(i),NULL);
      DFSCH_FAST_CDR_MUT(tail) = tmp;
      tail = tmp;
    }else{
      head = tail = dfsch_cons(DFSCH_FAST_CAR(i),NULL);
    }
    i = DFSCH_FAST_CDR(i);
  }
  if (i && !DFSCH_PAIR_P(i)){
    dfsch_type_error(i, DFSCH_LIST_TYPE, 1);
  }

  return (object_t*)head;
}

dfsch_object_t* dfsch_reverse(dfsch_object_t* list){
  object_t *head; 
  dfsch_object_t *i =  list;

  head = NULL;

  while(DFSCH_PAIR_P(i)){
    head = dfsch_cons(DFSCH_FAST_CAR(i), head);
    i = DFSCH_FAST_CDR(i);
  }
  if (i){
    dfsch_type_error(i, DFSCH_LIST_TYPE, 1);
  }

  return (object_t*)head;
}

dfsch_object_t* dfsch_collection_2_list(dfsch_object_t* list){
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  dfsch_object_t *i = dfsch_collection_get_iterator(list);

  if (DFSCH_PAIR_P(i)){
    return i;
  }

  head = tail = NULL;

  while (i){
    if (head){
      object_t* tmp = dfsch_cons(dfsch_iterator_this(i),NULL);
      DFSCH_FAST_CDR_MUT(tail) = tmp;
      tail = tmp;
    }else{
      head = tail = dfsch_cons(dfsch_iterator_this(i),NULL);
    }
    i = dfsch_iterator_next(i);
  }

  return (object_t*)head;
}
dfsch_object_t* dfsch_collection_2_reversed_list(dfsch_object_t* list){
  dfsch_object_t *head; 
  dfsch_object_t *i = dfsch_collection_get_iterator(list);

  head = NULL;

  while (i){
    head = dfsch_cons(dfsch_iterator_this(i), head);
    i = dfsch_iterator_next(i);
  }

  return (object_t*)head;
}


dfsch_object_t* dfsch_member(dfsch_object_t *key,
                             dfsch_object_t *list){
  dfsch_object_t* i;
  i=list;
  
  while (DFSCH_PAIR_P(i)){
    if (dfsch_equal_p(key, DFSCH_FAST_CAR(i))){
      return (object_t*)i;
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_type_error(i, DFSCH_LIST_TYPE, 1);
  }

  return NULL;
}

dfsch_object_t* dfsch_memv(dfsch_object_t *key,
                           dfsch_object_t *list){
  dfsch_object_t* i;
  i=list;
  
  while (DFSCH_PAIR_P(i)){
    if (dfsch_eqv_p(key, DFSCH_FAST_CAR(i))){
      return (object_t*)i;
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_type_error(i, DFSCH_LIST_TYPE, 1);
  }

  return NULL;
}

dfsch_object_t* dfsch_memq(dfsch_object_t *key,
                           dfsch_object_t *list){
  dfsch_object_t* i;
  i=list;
  
  while (DFSCH_PAIR_P(i)){
    if (key == DFSCH_FAST_CAR(i)){
      return (object_t*)i;
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_type_error(i, DFSCH_LIST_TYPE, 1);
  }

  return NULL;
}

/*
 * Linked list mergesort as described by Simon Tatham at
 * http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
 */
dfsch_object_t* dfsch_sort_list(dfsch_object_t* list,
                                dfsch_object_t* comp){
  size_t k = 1;
  dfsch_object_t* p = list;
  size_t p_s;
  dfsch_object_t* q;
  size_t q_s;
  dfsch_object_t* l;
  dfsch_object_t* lt;
  int nmerges;
  size_t i;
  dfsch_object_t* e;

  list = dfsch_list_copy(list);
  l = list;

  do {
    nmerges = 0;
    p = l;
    l = NULL;
    lt = NULL;
    while (DFSCH_PAIR_P(p)){
      nmerges++;
      q = p;
      p_s = 0;
      for (i = 0; DFSCH_PAIR_P(q) && i < k; i++){
        q = DFSCH_FAST_CDR(q);
        p_s++;
      }
      q_s = k;
      while (p_s > 0 || (q_s >0 && DFSCH_PAIR_P(q))){
        if (p_s == 0){
          q_s--;
          e = q;
          q = DFSCH_FAST_CDR(q);
        } else if (q_s == 0 || !DFSCH_PAIR_P(q)){
          p_s--;
          e = p;
          p = DFSCH_FAST_CDR(p);
        } else if (dfsch_apply(comp, dfsch_list(2, DFSCH_FAST_CAR(q), 
                                                DFSCH_FAST_CAR(p))) != NULL){
          q_s--;
          e = q;
          q = DFSCH_FAST_CDR(q);          
        } else {
          p_s--;
          e = p;
          p = DFSCH_FAST_CDR(p);
        }
        DFSCH_FAST_CDR_MUT(e) = NULL;
        if (l) {
          DFSCH_FAST_CDR_MUT(lt) = e;
          lt = e;
        } else {
          l = lt = e;
        }
      }
      p = q;
    }
    k *= 2;
  } while(nmerges > 1);

  return l;
}


// Alists 

dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
			    dfsch_object_t *alist){
  dfsch_object_t* i;
  

  i=alist;
  
  while (DFSCH_PAIR_P(i)){
    if (!DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
      dfsch_error("Not a alist",(object_t*)alist);
    }

    if (dfsch_equal_p(key, DFSCH_FAST_CAR(DFSCH_FAST_CAR(i)))){
      return DFSCH_FAST_CAR(i);
    }
    
    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Not a alist", (object_t*)i);
  }

  return NULL;

}
dfsch_object_t* dfsch_assq(dfsch_object_t *key,
                           dfsch_object_t *alist){
  dfsch_object_t* i;
  

  i=alist;
  
  while (DFSCH_PAIR_P(i)){
    if (!DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
      dfsch_error("Not a alist",(object_t*)alist);
    }

    if (key == DFSCH_FAST_CAR(DFSCH_FAST_CAR(i))){
      return DFSCH_FAST_CAR(i);
    }
    
    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Not a alist",(object_t*)i);
  }

  return NULL;
}
dfsch_object_t* dfsch_assv(dfsch_object_t *key,
                           dfsch_object_t *alist){
  dfsch_object_t* i;
  

  i=alist;
  
  while (DFSCH_PAIR_P(i)){
    if (!DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
      dfsch_error("Not a alist",(object_t*)alist);
    }

    if (dfsch_eqv_p(key, DFSCH_FAST_CAR(DFSCH_FAST_CAR(i)))){
      return DFSCH_FAST_CAR(i);
    }
    
    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Not a alist",(object_t*)i);
  }

  return NULL;
}




// closures

dfsch_object_t* dfsch_named_lambda(dfsch_object_t* env,
                                   dfsch_object_t* args,
                                   dfsch_object_t* code,
                                   dfsch_object_t* name){
  closure_t *c = (closure_t*)dfsch_make_object(DFSCH_STANDARD_FUNCTION_TYPE);
  
  c->env = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  c->args = (lambda_list_t*)dfsch_compile_lambda_list(args);
  c->orig_code = code;

  if (DFSCH_PAIR_P(code) && dfsch_string_p(DFSCH_FAST_CAR(code))){
    c->code = DFSCH_FAST_CDR(code);
    c->documentation = DFSCH_FAST_CAR(code);
  } else {
    c->code = code;
    c->documentation = NULL;
  }

  c->name = name;

  return (object_t*)c;
}

dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
                             dfsch_object_t* args,
                             dfsch_object_t* code){

  return dfsch_named_lambda(env, args, code, NULL); 
}

// native code
object_t* dfsch_make_primitive(dfsch_primitive_impl_t prim, void *baton){
  return dfsch_make_primitive_flags(prim, baton, 0);
}

object_t* dfsch_make_primitive_flags(dfsch_primitive_impl_t prim, 
                                     void *baton, 
                                     int flags){
  primitive_t* p = (primitive_t*)dfsch_make_object(PRIMITIVE);
  if (!p)
    return NULL;

  p->proc = prim;
  p->baton = baton;
  p->flags = 0;
  
  return (object_t*)p;
}

// macros

object_t* dfsch_make_macro(object_t *proc){
  macro_t *m = (macro_t*)dfsch_make_object(MACRO);
  
  if (!m)
    return NULL;

  m->proc = proc;

  return (object_t*)m;
}
dfsch_object_t* dfsch_make_form(dfsch_form_impl_t impl,
                                void* baton,
                                char* name){
  dfsch_form_t *f = (dfsch_form_t*)dfsch_make_object(DFSCH_FORM_TYPE);
  
  f->impl = impl;
  f->baton = baton;
  f->name = name;

  return (object_t*)f;
}

// Vectors

dfsch_object_t* dfsch_make_vector(size_t length, dfsch_object_t* fill){
  vector_t* v;
  size_t i;

  v = (vector_t*)dfsch_make_object_var(VECTOR, 
                                       length * sizeof(dfsch_object_t*));
  v->length = length;

  for(i = 0; i<length; ++i){
    v->data[i] = fill;
  }

  return (object_t*)v;
}
dfsch_object_t* dfsch_vector(size_t count, ...){
  size_t i;
  vector_t* v = 
    (vector_t*)dfsch_make_object_var(VECTOR, count * sizeof(dfsch_object_t*));
  va_list al;

  va_start(al,count);

  v->length = count;

  for(i = 0; i < count; ++i){
    v->data[i] = va_arg(al, dfsch_object_t*);
  }

  va_end(al);
  return (object_t*)v;

}

size_t dfsch_vector_length(dfsch_object_t *vector){
  return ((vector_t*)DFSCH_ASSERT_TYPE(vector, VECTOR))->length;  
}

dfsch_object_t** dfsch_vector_as_array(dfsch_object_t *vector, size_t *length){
  vector_t* v = DFSCH_ASSERT_TYPE(vector, VECTOR);

  if (length){
    *length = v->length;
  }

  return v->data;
}

dfsch_object_t* dfsch_vector_from_array(dfsch_object_t **array, 
                                        size_t length){
  vector_t* v = 
    (vector_t*)dfsch_make_object_var(VECTOR, 
                                     sizeof(dfsch_object_t*) * length);

  v->length = length;
  memcpy(v->data, array, sizeof(object_t*) * length);

  return (object_t*)v;
}

dfsch_object_t* dfsch_vector_ref(dfsch_object_t *vector, size_t k){
  vector_t* v = DFSCH_ASSERT_TYPE(vector, VECTOR);

  k = DFSCH_ASSERT_SEQUENCE_INDEX(vector, k, v->length);
  
  return v->data[k];
}

dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                 dfsch_object_t* obj){
  vector_t* v = DFSCH_ASSERT_TYPE(vector, VECTOR);

  k = DFSCH_ASSERT_SEQUENCE_INDEX(vector, k, v->length);
  
  v->data[k] = obj;

  return vector;
}

dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector){
  vector_t* v = DFSCH_ASSERT_TYPE(vector, VECTOR);

  return dfsch_list_from_array(v->data, v->length);
}

dfsch_object_t* dfsch_list_2_vector(dfsch_object_t* list){
  vector_t* vector;
  size_t length;
  dfsch_object_t** array;
  array = dfsch_list_as_array(list, &length);
  vector = (vector_t*)dfsch_make_object_var(VECTOR, 
                                            length * sizeof(dfsch_object_t*));
  vector->length = length;
  memcpy(vector->data, array, sizeof(dfsch_object_t*) * length);
  return (object_t*)vector;
}


