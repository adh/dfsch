/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2008 Ales Hakl
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/** @file dfsch.c This is implementation of dfsch interpreter. */

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
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include "types.h"

//#define ALLOC_DEBUG

#ifdef ALLOC_DEBUG
static int obj_count = 0;
static int obj_size = 0;
#endif

dfsch_object_t* dfsch_make_object_var(const dfsch_type_t* type, size_t size){
  object_t* o = GC_MALLOC(type->size + size);
  if (!o)
    return NULL;

  o->type = (dfsch_type_t*)type;

#ifdef ALLOC_DEBUG
  obj_count ++;
  obj_size += type->size;
  printf(";; Alloc'd: #<%s 0x%x> serial %d arena %d\n", type->name, o, 
         obj_count, obj_size);
#endif

  return o;
}

object_t* dfsch_make_object(const dfsch_type_t* type){
  return dfsch_make_object_var(type, 0);
}


int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b){
  return (a==b);
}

int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a || !b)
    return 0;

  if ((DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b)) && dfsch_number_p(a))
    return dfsch_number_equal_p(a,b);

  return 0;
}

int dfsch_equal_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a || !b)
    return 0;

  if (DFSCH_TYPE_OF(a) != DFSCH_TYPE_OF(b))
    return 0;

  if (!DFSCH_TYPE_OF(a))
    return 0;
  if (!DFSCH_TYPE_OF(a)->equal_p)
    return 0;

  return DFSCH_TYPE_OF(a)->equal_p(a,b);
}

static size_t ptr_hash(dfsch_object_t* ptr){
  size_t a = (size_t)ptr;        
  size_t b = (size_t)ptr >> 16 | (size_t)ptr << 16;

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

static size_t hash_combine(size_t a, size_t b){
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


uint32_t dfsch_hash(dfsch_object_t* obj){
  if (!obj){
    return 0;
  }
  if (!DFSCH_TYPE_OF(obj) || !DFSCH_TYPE_OF(obj)->hash){
    return ptr_hash(obj);
  }

  return DFSCH_TYPE_OF(obj)->hash(obj);
}

dfsch_type_t* dfsch_type_of(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj);
}

dfsch_type_t* dfsch_object_as_type(dfsch_object_t* obj){
  return dfsch_assert_instance(obj, DFSCH_STANDARD_TYPE);
}

dfsch_object_t* dfsch_superclass(dfsch_object_t* obj){  
  return (dfsch_object_t*)dfsch_object_as_type(obj)->superclass;
}

int dfsch_superclass_p(dfsch_type_t* sub, dfsch_type_t* super){
  if (sub == super)
    return 1;

  while (sub){
    sub = sub->superclass;
    if (sub==super){
      return 1;
    }
  }

  return 0;
}
int dfsch_instance_p(dfsch_object_t* obj, dfsch_type_t* type){
  return dfsch_superclass_p(DFSCH_TYPE_OF(obj), type);
}

void* dfsch_assert_type(dfsch_object_t* obj, dfsch_type_t* type){
  dfsch_object_t* o = obj;
  while (DFSCH_TYPE_OF(o) != type){
    DFSCH_WITH_RETRY_WITH_RESTART(dfsch_make_symbol("retry-with"), 
                                  "Retry with alternate value") {
      dfsch_type_error(o, type, 0);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}
dfsch_object_t* dfsch_assert_instance(dfsch_object_t* obj, 
                                      dfsch_type_t* type){
  dfsch_object_t* o = obj;
  while (!DFSCH_INSTANCE_P(o, type)){
    DFSCH_WITH_RETRY_WITH_RESTART(dfsch_make_symbol("retry-with"), 
                                  "Retry with alternate value") {
      dfsch_type_error(o, type, 1);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}


static dfsch_slot_t slot_slots[] = {
  DFSCH_STRING_SLOT(dfsch_slot_t, name, DFSCH_SLOT_ACCESS_RO),
  DFSCH_SIZE_T_SLOT(dfsch_slot_t, offset, DFSCH_SLOT_ACCESS_RO),
  DFSCH_INT_SLOT(dfsch_slot_t, access, DFSCH_SLOT_ACCESS_RO),
  DFSCH_SLOT_TERMINATOR
};

static void slot_write(dfsch_slot_t* slot, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, slot, "%s", slot->name);
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

  &slot_slots,
};

static dfsch_slot_t slot_type_slots[] = {
  DFSCH_SIZE_T_SLOT(dfsch_slot_type_t, size, DFSCH_SLOT_ACCESS_RO),
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
  &slot_type_slots
};

static dfsch_object_t* object_accessor_ref(void* ptr){
  return *((dfsch_object_t**)ptr);
}
static void object_accessor_set(void* ptr, dfsch_object_t* obj){
  *((dfsch_object_t**)ptr) = obj;
}
dfsch_slot_type_t dfsch_object_slot_type = {
  DFSCH_SLOT_TYPE_HEAD("object-slot"),
  object_accessor_ref,
  object_accessor_set,
  sizeof(dfsch_object_t*)
};

static dfsch_object_t* boolean_accessor_ref(void* ptr){
  return dfsch_bool(*((int*)ptr));
}
static void boolean_accessor_set(void* ptr, dfsch_object_t* obj){
  *((int**)ptr) = obj != NULL;
}
dfsch_slot_type_t dfsch_boolean_slot_type = {
  DFSCH_SLOT_TYPE_HEAD("boolean-slot"),
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
  DFSCH_SLOT_TYPE_HEAD("string-slot"),
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
    DFSCH_SLOT_TYPE_HEAD(#name "-slot"),                                \
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
        tmp = dfsch_cons(i, NULL);
        if (!head) {
          head = tail = tmp;
        } else {
          DFSCH_FAST_CDR(tail) = tmp;
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
    dfsch_error("Slot not accesible", slot);
  }

  return slot->type->ref(((char*) obj)+slot->offset);
}
void dfsch_slot_set(dfsch_object_t* obj, 
                    dfsch_slot_t* slot, 
                    dfsch_object_t* value, 
                    int debug){
  if (!debug && slot->access != DFSCH_SLOT_ACCESS_RW) {
    dfsch_error("Slot not accesible", slot);
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


static void type_write(dfsch_type_t* t, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, t, 
                         "%s instance-size: %d", t->name, t->size);
}

static dfsch_slot_t type_slots[] = {
  DFSCH_STRING_SLOT(dfsch_type_t, name, DFSCH_SLOT_ACCESS_RO),
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
  &type_slots
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
  &type_slots
};

dfsch_type_t dfsch_list_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "list",
  NULL,
  NULL,
  NULL,
  NULL
};

dfsch_type_t dfsch_function_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "function",
  NULL,
  NULL,
  NULL,
  NULL
};


dfsch_type_t dfsch_empty_list_type = {
  DFSCH_SPECIAL_TYPE,
  DFSCH_LIST_TYPE,
  0,
  "empty-list",
  NULL,
  NULL,
  NULL,
  NULL
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
  while (DFSCH_TYPE_OF(i) == DFSCH_PAIR_TYPE){
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
  DFSCH_SPECIAL_TYPE,
  DFSCH_LIST_TYPE,
  sizeof(dfsch_pair_t), 
  "pair",
  (dfsch_type_equal_p_t)pair_equal_p,
  (dfsch_type_write_t)pair_write,
  NULL,
  (dfsch_type_hash_t)pair_hash,
  NULL
};
#define PAIR (&dfsch_pair_type)


static dfsch_slot_t symbol_slots[] = {
  DFSCH_STRING_SLOT(symbol_t, data, DFSCH_SLOT_ACCESS_RO),
  DFSCH_SLOT_TERMINATOR
};

static void symbol_write(symbol_t* s, dfsch_writer_state_t* state){
  if (s->data){
    dfsch_write_string(state, s->data);
  } else {
    dfsch_write_unreadable(state, s, ""); 
  }
}
dfsch_type_t dfsch_symbol_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(symbol_t), 
  "symbol",
  NULL,
  (dfsch_type_write_t)symbol_write,
  NULL,
  NULL,
  &symbol_slots
};
#define SYMBOL DFSCH_SYMBOL_TYPE 

static void primitive_write(dfsch_primitive_t* p, 
                            dfsch_writer_state_t* state){
  char* name = p->name ? p->name : "()";
  dfsch_write_unreadable(state, p, "%s", name);
}

static dfsch_slot_t primitive_slots[] = {
  DFSCH_STRING_SLOT(dfsch_primitive_t, name, DFSCH_SLOT_ACCESS_RO),
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
  &primitive_slots
};
#define PRIMITIVE (&dfsch_primitive_type)

static void function_write(closure_t* c, dfsch_writer_state_t* state){
  dfsch_write_unreadable_start(state, c);

  dfsch_write_object(state, c->name);
  dfsch_write_string(state, " ");
  dfsch_write_object(state, c->args);

  dfsch_write_unreadable_end(state);
}

static dfsch_slot_t closure_slots[] = {
  DFSCH_OBJECT_SLOT(closure_t, args, DFSCH_SLOT_ACCESS_DEBUG_WRITE),
  DFSCH_OBJECT_SLOT(closure_t, code, DFSCH_SLOT_ACCESS_DEBUG_WRITE),
  DFSCH_OBJECT_SLOT(closure_t, env, DFSCH_SLOT_ACCESS_DEBUG_WRITE),
  DFSCH_OBJECT_SLOT(closure_t, name, DFSCH_SLOT_ACCESS_DEBUG_WRITE),
  DFSCH_OBJECT_SLOT(closure_t, orig_code, DFSCH_SLOT_ACCESS_DEBUG_WRITE),
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
  &closure_slots
};
#define FUNCTION DFSCH_STANDARD_FUNCTION_TYPE

dfsch_type_t dfsch_macro_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(macro_t),
  "macro",
  NULL,
  NULL,
  NULL
};
#define MACRO DFSCH_MACRO_TYPE

dfsch_type_t dfsch_form_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_form_t),
  "form",
  NULL,
  NULL,
  NULL
};
#define FORM (&dfsch_form_type)

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

dfsch_type_t dfsch_vector_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(vector_t),
  "vector",
  (dfsch_type_equal_p_t)vector_equal_p,
  (dfsch_type_write_t)vector_write,
  NULL,
  (dfsch_type_hash_t)vector_hash
};
#define VECTOR DFSCH_VECTOR_TYPE


dfsch_type_t dfsch_environment_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(environment_t),
  "environment"
};



int dfsch_null_p(dfsch_object_t* obj){
  return !obj;
}
int dfsch_pair_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == PAIR;
}
int dfsch_list_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_LIST_TYPE);
}
int dfsch_atom_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) != PAIR;
}
int dfsch_symbol_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == SYMBOL;
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
  return DFSCH_TYPE_OF(obj) == FORM;
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

  return DFSCH_PAIR_ENCODE(p);
}

dfsch_object_t* dfsch_multicons(size_t n){
  size_t i;
  dfsch_pair_t* p;

  if (n == 0){
    return NULL;
  }

  p = GC_MALLOC(sizeof(dfsch_pair_t)*n);

  for (i = 0; i < (n-1); i++){
    p[i].cdr = DFSCH_PAIR_ENCODE(&(p[i+1]));
  }

  return DFSCH_PAIR_ENCODE(&(p[0]));
}

dfsch_object_t* dfsch_car(dfsch_object_t* pair){
  if (DFSCH_TYPE_OF(pair) != PAIR)
    dfsch_error("Not a pair",pair);

  return DFSCH_FAST_CAR(pair);
}
dfsch_object_t* dfsch_cdr(dfsch_object_t* pair){
  if (DFSCH_TYPE_OF(pair) != PAIR)
    dfsch_error("Not a pair",pair);

  return DFSCH_FAST_CDR(pair);
}

dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
			      dfsch_object_t* car){
  if (DFSCH_TYPE_OF(pair) != PAIR)
    dfsch_error("Not a pair",pair);

  DFSCH_FAST_CAR(pair) = car;
  
  return pair;

}
dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
			      dfsch_object_t* cdr){
  if (DFSCH_TYPE_OF(pair) != PAIR)
    dfsch_error("Not a pair",pair);
  
  DFSCH_FAST_CDR(pair) = cdr;
  
  return pair;

}
long dfsch_list_length_fast(object_t* list){
  dfsch_object_t *i;
  long count;

  if (!list)
    return 0;

  if (DFSCH_TYPE_OF(list) != PAIR)
    return -1;

  i = list;
  count = 0;

  while (DFSCH_TYPE_OF(i) == PAIR ){
    i = DFSCH_FAST_CDR(i);
    ++count;
  }

  return count;
}
long dfsch_list_length(object_t* list){
  dfsch_object_t *i;
  dfsch_object_t *j; 
  long count;

  if (!list)
    return 0;

  if (DFSCH_TYPE_OF(list) != PAIR)
    return -1;

  i = j = list;
  count = 0;

  while (DFSCH_TYPE_OF(i) == PAIR){
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (i == j)
      return -1;
    j = DFSCH_FAST_CDR(j);
    if (!(DFSCH_TYPE_OF(i) == PAIR))
      break;
    i = DFSCH_FAST_CDR(i);
    ++count;
    if (i == j)
      return -1;
  }

  if (i)
    return -1;

  return count;
}

long dfsch_list_length_check(object_t* list){
  long len;
  len = dfsch_list_length(list);
  if (len < 0)
    dfsch_error("Not a list", list);
  return len;
}

dfsch_object_t* dfsch_list_item(dfsch_object_t* list, int index){
  dfsch_object_t* it = list;
  int i;
  for (i=0; i<index; ++i){
    if (DFSCH_TYPE_OF(it) == PAIR){
      it = DFSCH_FAST_CDR(it);
    }else{
      dfsch_error("No such item of list",dfsch_make_number_from_long(index));
    }
  }
  return dfsch_car(it);
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
  
  while (DFSCH_TYPE_OF(j) == PAIR){
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
	if (i != 0){
          dfsch_error("Not a list of same length lists", llist);
	}
	goto out;
      }
      if (DFSCH_TYPE_OF(args[i]) != PAIR){
	dfsch_error("Not a pair", args[i]);
      }

      tmp = dfsch_cons(DFSCH_FAST_CAR(args[i]), NULL);
      if (shead){
        DFSCH_FAST_CDR(stail) = tmp;
      } else {
        shead = tmp;
      }
      stail = tmp;

      args[i] = DFSCH_FAST_CDR(args[i]);
    }


    tmp = dfsch_cons(shead, NULL);
    if (head){
      DFSCH_FAST_CDR(tail) = tmp;
    } else {
      head = tmp;
    }
    tail = tmp;

  }
  

 out:
  for (i = 0; i<len; i++){
    if (args[i]){
      dfsch_error("Not a list of same length lists", llist);
    }
  }
  
  return head;
}


dfsch_object_t* dfsch_append(dfsch_object_t* llist){
  dfsch_object_t* head=NULL;
  dfsch_object_t* tail=NULL;
  dfsch_object_t* i = llist;
  dfsch_object_t* j;

  if (!llist)
    return NULL;

  while(DFSCH_TYPE_OF(i) == PAIR &&  
        DFSCH_TYPE_OF(DFSCH_FAST_CDR(i)) == PAIR){
    
    j = DFSCH_FAST_CAR(i);
    while(DFSCH_TYPE_OF(j) == PAIR){
      if (head){
        object_t* tmp = dfsch_cons(DFSCH_FAST_CAR(j),NULL);
        DFSCH_FAST_CDR(tail) = tmp;
        tail = tmp;
      }else{
        head = tail = dfsch_cons(DFSCH_FAST_CAR(j),NULL);
      }
      j = DFSCH_FAST_CDR(j);
    }
    if (j && DFSCH_TYPE_OF(j) != PAIR)
      dfsch_error("Not a pair", (object_t*)j);

    i = DFSCH_FAST_CDR(i);
  }

  if (DFSCH_TYPE_OF(i) != PAIR)
    dfsch_error("Not a pair", (object_t*)i);

  if (tail){
    DFSCH_FAST_CDR(tail) = DFSCH_FAST_CAR(i);
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

dfsch_object_t* dfsch_list_copy(dfsch_object_t* list){
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  dfsch_object_t *i =  list;

  head = tail = NULL;

  while(DFSCH_TYPE_OF(i) == PAIR){
    if (head){
      object_t* tmp = dfsch_cons(DFSCH_FAST_CAR(i),NULL);
      DFSCH_FAST_CDR(tail) = tmp;
      tail = tmp;
    }else{
      head = tail = dfsch_cons(DFSCH_FAST_CAR(i),NULL);
    }
    i = DFSCH_FAST_CDR(i);
  }
  if (i && DFSCH_TYPE_OF(i) != PAIR)
    dfsch_error("Not a list", (object_t*)i);


  return (object_t*)head;

}

dfsch_object_t* dfsch_reverse(dfsch_object_t* list){
  object_t *head; 
  dfsch_object_t *i =  list;

  head = NULL;

  while(DFSCH_TYPE_OF(i) == PAIR){
    head = dfsch_cons(DFSCH_FAST_CAR(i), head);
    i = DFSCH_FAST_CDR(i);
  }
  if (i)
    dfsch_error("Not a list", (object_t*)i);


  return (object_t*)head;

}

dfsch_object_t* dfsch_member(dfsch_object_t *key,
                             dfsch_object_t *list){
  dfsch_object_t* i;
  i=list;
  
  while (DFSCH_TYPE_OF(i) == PAIR){
    if (dfsch_equal_p(key, DFSCH_FAST_CAR(i))){
      return (object_t*)i;
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i)
    dfsch_error("Not a list", (object_t*)i);

  return NULL;
}

dfsch_object_t* dfsch_memv(dfsch_object_t *key,
                           dfsch_object_t *list){
  dfsch_object_t* i;
  i=list;
  
  while (DFSCH_TYPE_OF(i) == PAIR){
    if (dfsch_eqv_p(key, DFSCH_FAST_CAR(i))){
      return (object_t*)i;
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i)
    dfsch_error("Not a list", (object_t*)i);

  return NULL;
}

dfsch_object_t* dfsch_memq(dfsch_object_t *key,
                           dfsch_object_t *list){
  dfsch_object_t* i;
  i=list;
  
  while (DFSCH_TYPE_OF(i) == PAIR){
    if (key == DFSCH_FAST_CAR(i)){
      return (object_t*)i;
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i)
    dfsch_error("Not a list", (object_t*)i);

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

  l = list;
  dfsch_list_length(list);

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
          p_s --;
          e = p;
          p = DFSCH_FAST_CDR(p);
        }
        DFSCH_FAST_CDR(e) = NULL;
        if (l) {
          DFSCH_FAST_CDR(lt) = e;
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
  
  while (DFSCH_TYPE_OF(i) == PAIR){
    if (DFSCH_TYPE_OF(DFSCH_FAST_CAR(i)) !=PAIR){
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
  
  while (DFSCH_TYPE_OF(i) == PAIR){
    if (DFSCH_TYPE_OF(DFSCH_FAST_CAR(i)) !=PAIR){
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
  
  while (DFSCH_TYPE_OF(i) == PAIR){
    if (DFSCH_TYPE_OF(DFSCH_FAST_CAR(i)) !=PAIR){
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



// Symbols

#define HASH_BITS 10
#define HASH_SIZE (1 << HASH_BITS)

typedef struct hash_entry_t hash_entry_t;
struct hash_entry_t {
  symbol_t* entry;
  size_t hash;
  hash_entry_t* next;
};


/*
 * ugly case-insensitive string hash used for symbols
 */
static size_t string_hash(char* string){
  size_t tmp=0;

  while (*string){
    char c = *string; 
    tmp ^= c ^ (tmp << 7); 
    tmp ^= ((size_t)c << 17) ^ (tmp >> 11); 
    ++string;
  }

  return tmp & (HASH_SIZE - 1); 
}

static hash_entry_t*  global_symbol_hash[HASH_SIZE];
static unsigned int gsh_init = 0;
static pthread_mutex_t symbol_lock = PTHREAD_MUTEX_INITIALIZER;

/*
 * It's possible to use rwlock here (althought such solution is not so 
 * straightforward), but it seem unnecessary - most symbol creations are 
 * done when reading source and in such case there will be probably only
 * one thread doing such things.
 */


static void gsh_check_init(){
  if (gsh_init)
    return;

  memset(global_symbol_hash, 0, sizeof(hash_entry_t*)*HASH_SIZE);
  gsh_init = 1;
}

static symbol_t* lookup_symbol(char *symbol){

  size_t hash = string_hash(symbol);
  hash_entry_t *i = global_symbol_hash[hash];

  while (i){
    if (i->hash == hash && strcmp(i->entry->data, symbol)==0){
      return i->entry;
    }
    i = i->next;
  }

  return NULL;
}

static void free_symbol(symbol_t* s){
  hash_entry_t *i;
  hash_entry_t *j;

  pthread_mutex_lock(&symbol_lock);

  i = global_symbol_hash[string_hash(s->data)];
  j = NULL;
  
  while (i){
    if (i->entry == s){
      if (j){
        j->next = i->next;
      } else {
        global_symbol_hash[string_hash(s->data)] = i->next;
      }
      free(i);
      break;
    }
    j = i;
    i = i->next;
  }

  pthread_mutex_unlock(&symbol_lock);

  s->data = NULL;
}

static void symbol_finalizer(symbol_t* symbol, void* cd){
  free_symbol(symbol);
}

static symbol_t* make_symbol(char *symbol){
  symbol_t *s;
  symbol_t *f;

  s = (symbol_t*)dfsch_make_object(SYMBOL); /* !!! free_symbol could be called by this */
  s->data = stracpy(symbol);

  pthread_mutex_lock(&symbol_lock);

  f = lookup_symbol(symbol); 
  if (f){ 
    GC_FREE(s->data);
    GC_FREE(s);
    pthread_mutex_unlock(&symbol_lock);
    return f;
  }


  GC_REGISTER_FINALIZER(s, 
                        (GC_finalization_proc)symbol_finalizer, NULL, 
                        NULL, NULL);
    
  hash_entry_t *e = malloc(sizeof(hash_entry_t));

  e->entry = s;
  e->hash = string_hash(symbol);

  e->next = global_symbol_hash[e->hash];
  global_symbol_hash[e->hash] = e;

  pthread_mutex_unlock(&symbol_lock);
  
  return s;
}


void dfsch_unintern(dfsch_object_t* symbol){
  if (DFSCH_TYPE_OF(symbol) != SYMBOL)
    dfsch_error("Not a symbol", symbol);

  free_symbol((symbol_t*)symbol);
}

dfsch_object_t* dfsch_gensym(){
  symbol_t *s = (symbol_t*)dfsch_make_object(SYMBOL);

  s->data = NULL;

  return (object_t*)s;
}

dfsch_object_t* dfsch_make_symbol(char* symbol){

  symbol_t *s;

  if (!symbol){
    return dfsch_gensym();
  }

  pthread_mutex_lock(&symbol_lock);

  gsh_check_init(); 
  // This code is slow already, so this check does not matter (too much)

  s = lookup_symbol(symbol);

  pthread_mutex_unlock(&symbol_lock);

  if (!s)
    s = make_symbol(symbol);

  return (object_t*)s;

}
char* dfsch_symbol(dfsch_object_t* symbol){
  if (DFSCH_TYPE_OF(symbol) != SYMBOL)
    dfsch_error("Not a symbol", symbol);

  return ((symbol_t*)symbol)->data;
}

char* dfsch_symbol_2_typename(dfsch_object_t* symbol){
  char *name;
  char *flt;

  name = dfsch_symbol(symbol);

  if (name[strlen(name)-1] == '>'){
    flt = strchr(name, '<');
    if (flt){
      if (flt == name){
        return strancpy(name+1, strlen(name) - 2);
      } else {
        return strancat(name, flt - name, flt+1, strlen(flt+1)-1);
      }
    }
  }

  return name;
}



int dfsch_compare_symbol(dfsch_object_t* symbol,
                         char* string){
  return (ascii_strcasecmp(string, dfsch_symbol(symbol)) == 0);
}


dfsch_object_t* dfsch_sym_true(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("true"); 
  // TODO: shouldn't this be something other? #t ? T ?
  return cache;
}
dfsch_object_t* dfsch_sym_quote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("quote");
  return cache;
}
dfsch_object_t* dfsch_sym_quasiquote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("quasiquote");
  return cache;
}
dfsch_object_t* dfsch_sym_unquote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("unquote");
  return cache;
}
dfsch_object_t* dfsch_sym_unquote_splicing(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("unquote-splicing");
  return cache;
}
dfsch_object_t* dfsch_sym_else(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("else");
  return cache;
}
dfsch_object_t* dfsch_sym_bold_right_arrow(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("=>");
  return cache;
}
dfsch_object_t* dfsch_sym_tail_recursive(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("tail-recursive");
  return cache;
}
dfsch_object_t* dfsch_bool(int bool){
  return bool?dfsch_sym_true():NULL;
}

struct dfsch_symbol_iter_t{
  hash_entry_t* item;
  size_t bucket;
};

char* dfsch_get_next_symbol(dfsch_symbol_iter_t **iter){ // deep magic
  if (*iter == NULL){
    *iter = GC_MALLOC(sizeof(dfsch_symbol_iter_t));
    (*iter)->bucket = 0;
    (*iter)->item = global_symbol_hash[(*iter)->bucket];
  }
  while ((*iter)->bucket < HASH_SIZE){
    if (!(*iter)->item){
      (*iter)->bucket ++;
      (*iter)->item = global_symbol_hash[(*iter)->bucket];
    }else{
      hash_entry_t *i = (*iter)->item;
      (*iter)->item = (*iter)->item->next;
      return i->entry->data;
    }
  }  
  return NULL;
}


// closures

extern dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
				    dfsch_object_t* args,
				    dfsch_object_t* code){
  closure_t *c = (closure_t*)dfsch_make_object(DFSCH_STANDARD_FUNCTION_TYPE);
  if (!c)
    return NULL;
  
  c->env = env;
  c->args = args;
  c->code = code;
  c->orig_code = code;
  c->name = NULL;

  return (object_t*)c;
  
}
extern dfsch_object_t* dfsch_named_lambda(dfsch_object_t* env,
                                          dfsch_object_t* args,
                                          dfsch_object_t* code,
                                          dfsch_object_t* name){
  closure_t *c = (closure_t*)dfsch_make_object(DFSCH_STANDARD_FUNCTION_TYPE);
  if (!c)
    return NULL;
  
  c->env = env;
  c->args = args;
  c->code = code;
  c->orig_code = code;
  c->name = name;

  return (object_t*)c;
  
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
  dfsch_form_t *f = (dfsch_form_t*)dfsch_make_object(FORM);
  
  f->impl = impl;
  f->baton = baton;
  f->name = name;

  return (object_t*)f;
}


static pthread_key_t thread_key;
static pthread_once_t thread_once = PTHREAD_ONCE_INIT;


/*
 * It seems so volatile really isn't needed around this magic, only automatic
 * variables that have been _CHANGED_ since call to setjmp(3) are indeterminate
 * and there are none such. Actually only thing that happen in these functions
 * between setjmp(3) and opportunity to call longjmp(3) (from corresponding 
 * wrapper function) is function call. This is only case of undefined behavior
 * related to "proper" use of setjmp(3)/longjmp(3) in IEEE 1003.1.
 */

static void thread_info_destroy(void* ptr){
  if (ptr)
    GC_FREE(ptr);
}
static void thread_key_alloc(){
  pthread_key_create(&thread_key, thread_info_destroy);
}

#if defined(__GNUC__) && !defined(__arm__) && !defined(__CYGWIN__)
#define USE_TLS
#endif

dfsch__thread_info_t* dfsch__get_thread_info(){
#ifdef USE_TLS
  static __thread dfsch__thread_info_t* ei;
#else
  dfsch__thread_info_t *ei;
  pthread_once(&thread_once, thread_key_alloc);
  ei = pthread_getspecific(thread_key);
#endif
  if (!ei){
    ei = GC_MALLOC_UNCOLLECTABLE(sizeof(dfsch__thread_info_t)); 
    ei->throw_ret = NULL;
    ei->stack_frame = GC_NEW(dfsch__stack_frame_t);
    ei->stack_frame->arguments = NULL;
    ei->stack_frame->procedure = dfsch_make_symbol("toplevel");
    ei->stack_frame->next = NULL;
    ei->break_type = NULL;
    pthread_setspecific(thread_key, ei);
  }
  return ei;
}

void dfsch__continue_unwind(dfsch__thread_info_t* ti){
  if (!ti->throw_ret){
    fputs("No unwind target!!!\n", stderr);
    abort();
  }
  longjmp(*ti->throw_ret, 1);
}
void dfsch__finalize_unwind(dfsch__thread_info_t* ti){
  ti->throw_tag = NULL;
  ti->throw_value = NULL;
}

void dfsch_throw(dfsch_object_t* tag,
                 dfsch_object_t* value){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  dfsch__catch_list_t* i = ti->catch_list;
  while (i){
    if (i->tag == tag){
      ti->throw_tag = tag;
      ti->throw_value = value;
      dfsch__continue_unwind(ti);
    }
    i = i->next;
  }
  dfsch_error("Invalid catch tag", tag);
}

dfsch_object_t* dfsch_error(char* name, 
                            dfsch_object_t* detail){
  dfsch_signal(dfsch_condition(DFSCH_ERROR_TYPE, 
                               "message", dfsch_make_string_cstr(name),
                               "object", detail,
                               NULL));
}
dfsch_object_t* dfsch_break(char* type){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  ti->break_type = type;
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
  if (DFSCH_TYPE_OF(vector) != VECTOR)
    return 0;

  return ((vector_t*)vector)->length;  
}

dfsch_object_t** dfsch_vector_as_array(dfsch_object_t *vector, size_t *length){
  if (DFSCH_TYPE_OF(vector) != VECTOR)
    dfsch_error("Not a vector",vector);

  if (length){
    *length = ((vector_t*)vector)->length;
  }

  return ((vector_t*)vector)->data;
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
  if (DFSCH_TYPE_OF(vector) != VECTOR)
    dfsch_error("Not a vector",vector);

  if (((vector_t*)vector)->length <= k)
    dfsch_error("Invalid index",dfsch_make_number_from_long(k));
  
  return ((vector_t*)vector)->data[k];
}

dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                 dfsch_object_t* obj){
  if (DFSCH_TYPE_OF(vector) != VECTOR)
    dfsch_error("Not a vector",vector);

  if (((vector_t*)vector)->length <= k)
    dfsch_error("Invalid index",dfsch_make_number_from_long(k));
  
  ((vector_t*)vector)->data[k] = obj;

  return vector;
}

dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector){

  if (DFSCH_TYPE_OF(vector) != VECTOR)
    dfsch_error("Not a vector",vector);

  return dfsch_list_from_array(((vector_t*)vector)->data, 
                               ((vector_t*)vector)->length);
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

char* dfsch_obj_write(dfsch_object_t* obj, 
                      int max_depth, int readable){
  str_list_t* sl = sl_create();
  dfsch_writer_state_t* state = dfsch_make_writer_state(max_depth,
                                                        readable?
                                                        DFSCH_WRITE:
                                                        DFSCH_PRINT,
                                                        sl_append,
                                                        sl);
  dfsch_write_object(state, obj);
  return sl_value(sl);
}


struct dfsch_writer_state_t {
  dfsch_object_t object_head;
  dfsch_output_proc_t output_proc;
  void* output_baton;
  int depth;
  int readability;
};
dfsch_type_t dfsch_writer_state_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_writer_state_t),
  "writer-state"
};

dfsch_writer_state_t* dfsch_make_writer_state(int max_depth,
                                              int readability,
                                              dfsch_output_proc_t proc,
                                              void* baton){
  dfsch_writer_state_t* state = 
    (dfsch_writer_state_t*)dfsch_make_object(DFSCH_WRITER_STATE_TYPE);

  state->output_proc = proc;
  state->output_baton = baton;
  state->depth = max_depth;
  state->readability = readability;

  return state;
}
void dfsch_invalidate_writer_state(dfsch_writer_state_t* state){
  state->output_proc = NULL;
  state->output_baton = NULL;
}
int dfsch_writer_state_print_p(dfsch_writer_state_t* state){
  return state->readability == DFSCH_PRINT;
}

void dfsch_write_object(dfsch_writer_state_t* state,
                        dfsch_object_t* object){
  dfsch_type_t* type;
  char* ret;

  if (!object){
    dfsch_write_string(state, "()");
    return;
  }

  if (state->depth==0){
    dfsch_write_string(state, "...");
  }

  type = DFSCH_TYPE_OF(object);

  while (type){
    if (type->write){
      state->depth--;
      type->write(object, state);
      state->depth++;
      return;
    }
    type = type->superclass;
  }

  dfsch_write_unreadable(state, object, "");
}


void dfsch_write_string(dfsch_writer_state_t* state,
                        char* str){
  dfsch_write_strbuf(state, str, strlen(str));
}
void dfsch_write_strbuf(dfsch_writer_state_t* state,
                        char* str, size_t len){
  if (state->output_proc){
    state->output_proc(state->output_baton, str, len);
  } else {
    dfsch_error("Stale writer-state", state);
  }
}

void dfsch_write_unreadable(dfsch_writer_state_t* state,
                            dfsch_object_t* obj, char* format, ...){
  str_list_t* sl = sl_create();
  va_list args;
  char *ret;
  va_start(args, format);

  dfsch_write_unreadable_start(state, obj);
  dfsch_write_string(state, vsaprintf(format, args)); 
  dfsch_write_unreadable_end(state);
}
void dfsch_write_unreadable_start(dfsch_writer_state_t* state,
                                  dfsch_object_t* obj){
  if (state->readability == DFSCH_STRICT_WRITE){
    dfsch_error("Object has no readable representation", obj);
  }
  dfsch_write_string(state, 
                     saprintf("#<%s %p ", DFSCH_TYPE_OF(obj)->name, obj));
}
void dfsch_write_unreadable_end(dfsch_writer_state_t* state){
  dfsch_write_string(state, ">");
}



typedef struct read_ctx_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;  
} read_ctx_t;

static read_callback(dfsch_object_t *obj, void* ctx){
  dfsch_object_t* new_tail = dfsch_cons(obj, NULL);

  if (!((read_ctx_t*)ctx)->head){
    ((read_ctx_t*)ctx)->head = new_tail;
  }else{
    dfsch_set_cdr(((read_ctx_t*)ctx)->tail, new_tail);
  }

  ((read_ctx_t*)ctx)->tail = new_tail;

  return 1;
}

dfsch_object_t* dfsch_list_read(char* str){
  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  read_ctx_t ctx;
  int err;
  dfsch_parser_callback(parser, read_callback, &ctx);

  ctx.head = ctx.tail = NULL;
  
  err = dfsch_parser_feed(parser, str);
  if (!err)
    dfsch_parser_feed(parser, " ");

  if ((err && err != DFSCH_PARSER_STOPPED)
      || dfsch_parser_get_level(parser)!=0){
    dfsch_error("Syntax error",NULL);
  }  
  
  return ctx.head;
}
dfsch_object_t* dfsch_obj_read(char* str){
  object_t* list = dfsch_list_read(str);
  if (!list)
    return NULL;
  return dfsch_car(list);
}

static dfsch_object_t* global_property_hash;
static pthread_mutex_t global_property_hash_mutex;

static dfsch_object_t* get_global_property_hash(){
  if (!global_property_hash){
    global_property_hash = dfsch_make_weak_key_hash();
  }
  return global_property_hash;
}

static dfsch_object_t* get_phash(dfsch_object_t* o){
  if (!global_property_hash){
    return NULL;
  } else {
    dfsch_object_t* p = NULL;
    dfsch_hash_ref_fast(global_property_hash, o, &p);
    return p;
  }
}
static dfsch_object_t* make_phash(dfsch_object_t* o){
  dfsch_object_t* p = NULL;
  dfsch_object_t* gp;
  pthread_mutex_lock(&global_property_hash_mutex);
  gp = get_global_property_hash();
  dfsch_hash_ref_fast(gp, o, &p);
  if (!p){
    p = dfsch_hash_make(DFSCH_HASH_EQ);
    dfsch_hash_set(gp, o, p);
  }
  pthread_mutex_unlock(&global_property_hash_mutex);
  return p;
}


dfsch_object_t* dfsch_get_object_properties(dfsch_object_t* o){
  dfsch_object_t* ph = get_phash(o);
  if (!ph){
    return NULL;
  }
  return dfsch_hash_2_alist(ph);
}
dfsch_object_t* dfsch_get_object_property(dfsch_object_t* o,
                                          dfsch_object_t* name){
  dfsch_object_t* ph = get_phash(o);
  dfsch_object_t* res = NULL;

  if (!ph){
    return NULL;
  }
  dfsch_hash_ref_fast(ph, name, &res);
  return res;
}
void dfsch_set_object_property(dfsch_object_t* o,
                               dfsch_object_t* name,
                               dfsch_object_t* value){
  dfsch_hash_set(make_phash(o), name, value);
}
void dfsch_unset_object_property(dfsch_object_t* o,
                                 dfsch_object_t* name){
  dfsch_hash_unset(make_phash(o), name);
}

dfsch_object_t* dfsch_get_property(dfsch_object_t* o,
                                   char* name){
  return dfsch_get_object_property(o, dfsch_make_symbol(name));
}
void dfsch_set_property(dfsch_object_t* o,
                        char* name,
                        dfsch_object_t* value){
  dfsch_set_object_property(o, dfsch_make_symbol(name), value);
}
void dfsch_unset_property(dfsch_object_t* o,
                          char* name){
  dfsch_unset_object_property(o, dfsch_make_symbol(name));
}



dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent){
  return dfsch_new_frame_from_hash(parent, dfsch_hash_make(DFSCH_HASH_EQ));
}
dfsch_object_t* dfsch_new_frame_from_hash(dfsch_object_t* parent, 
                                          dfsch_object_t* hash){
  environment_t* e = (environment_t*)dfsch_make_object(DFSCH_ENVIRONMENT_TYPE);

  e->values = hash;
  e->decls = NULL;

  e->proc = NULL;
  e->args = NULL;
  
  if (parent && DFSCH_TYPE_OF(parent) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", parent);
  }

  e->parent = (environment_t*)parent;
    
  return (dfsch_object_t*)e;
}
void dfsch_set_frame_context(dfsch_object_t* env, 
                             dfsch_object_t* proc, dfsch_object_t* args){
  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }
  
  ((environment_t*)env)->proc = proc;
  ((environment_t*)env)->args = args;
}

object_t* dfsch_lookup(object_t* name, object_t* env){
  environment_t *i;
  object_t* ret;

  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }

  i = (environment_t*)env;
  while (i){
    if (dfsch_hash_ref_fast(i->values, name, &ret)){
      return ret;
    }
    
    i = i->parent;
  }

  dfsch_error("Unbound variable", dfsch_cons(name, env));
}
object_t* dfsch_env_get(object_t* name, object_t* env){
  environment_t *i;

  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }

  i = (environment_t*)env;

  while (i){
    object_t* ret = dfsch_hash_ref(i->values, name);
    if (ret){
      return ret;
    }

    i = i->parent;
  }
  
  return NULL;
}


object_t* dfsch_set(object_t* name, object_t* value, object_t* env){
  environment_t *i;

  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }

  i = (environment_t*)env;

  while (i){
    if(dfsch_hash_set_if_exists(i->values, name, value))
      return value;

    i = i->parent;
  }

  dfsch_error("Unbound variable",name);
}
void dfsch_unset(object_t* name, object_t* env){
  environment_t *i;

  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }

  i = (environment_t*)env;
  while (i){
    if (i->decls){
      dfsch_hash_unset(i->decls, name);
    }
    if(dfsch_hash_unset(i->values, name))
      return;

    i = i->parent;
  }
  
  dfsch_error("Unbound variable",name);
}


object_t* dfsch_define(object_t* name, object_t* value, object_t* env){
  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }

  dfsch_hash_set(((environment_t*)env)->values, name, value);  

  return value;

}

void dfsch_declare(dfsch_object_t* variable, dfsch_object_t* declaration,
                   dfsch_object_t* env){
  dfsch_object_t* old = NULL;

  if (env && DFSCH_TYPE_OF(env) != DFSCH_ENVIRONMENT_TYPE){
    dfsch_error("Not an environment", env);
  }

  if (!((environment_t*)env)->decls){
    ((environment_t*)env)->decls = dfsch_hash_make(DFSCH_HASH_EQ);
  } else {
    dfsch_hash_ref_fast(((environment_t*)env)->decls, variable, &old);
  }
  
  dfsch_hash_set(((environment_t*)env)->decls, variable, 
                 dfsch_cons(declaration, old));  
}

dfsch_object_t* dfsch_macro_expand(dfsch_object_t* macro,
                                   dfsch_object_t* args){
  if (!DFSCH_INSTANCE_P(macro, MACRO)){
    dfsch_error("Not a macro", macro);
  }

  return dfsch_apply(((macro_t*)macro)->proc, args);
}

// Evaluator

/*
 * There are some kinds of structures that are passed extensively inside the 
 * evaluator, but are not exposed to public API much. Idea there is that 
 * dfsch__get_thread_info() can be somewhat slow (on linux, it isn't 
 * noticeably slow, but who knows) and thus it is not exactly bad idea to 
 * cache it's result, but passing this value to user code causes marginal 
 * speedup at cost of having one additional argument that is not useful in 
 * any meaningful way to user code.
 *
 * dfsch_tail_escape_t is used to implement tail recursion, it is used only 
 * in dfsch_eval_proc_impl() but it has to be passed through much of evaluator 
 * and some native functions. General idea is that dfsch_eval_proc_impl stores
 * jmp_buf to it's start here and passes it to dfsch_eval_impl() during 
 * evaluation of last form of procedure body. Native functions have to 
 * implement same mechanism (pass dfsch_tail_escape_t argument only to 
 * functions whose return value is return value of native function itself). 
 * When dfsch_eval_proc_impl() is called with non-NULL tail_escape it simply
 * jumps back to previous activation record. This actually causes slow-down,
 * but enables us to do tail-recursion in simple and consistent way, that 
 * works even through C-code.
 */

/* TODO: finish new stack traces
 * general idea is that frames are construed by apply and then filed in 
 * relevant functions. Tail recursion handling should be part of apply. */

typedef dfsch_tail_escape_t tail_escape_t;


static dfsch_object_t* dfsch_eval_proc_impl(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            tail_escape_t* esc,
                                            dfsch__thread_info_t* ti);
static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       dfsch_object_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti);
static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti);


static object_t* eval_list(object_t *list, object_t* env, 
                           dfsch__thread_info_t* ti){
  dfsch_object_t *i;
  object_t *f=NULL;
  dfsch_object_t *t, *p;
  object_t *r; 

  if (!list)
    return NULL;

  i = list;
  while (DFSCH_TYPE_OF(i) ==PAIR){
    r = dfsch_eval_impl(DFSCH_FAST_CAR(i), env, NULL, ti);

    t = dfsch_cons(r,NULL);
    if (f){
      DFSCH_FAST_CDR(p) = (object_t*)t;
      p = t;
    }else{
      f = (object_t*)(p = t);
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Not a proper list", list);    
  }

  return f;
}

dfsch_object_t* dfsch_eval_list(dfsch_object_t* list, dfsch_object_t* env){
  return eval_list(list, env, dfsch__get_thread_info());
}

static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       dfsch_object_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti){
  dfsch_object_t* args;
 start:
  
  if (!exp) 
    return NULL;

  if(DFSCH_TYPE_OF(exp) == SYMBOL){
    ti->stack_frame->env = env;
    ti->stack_frame->expr = exp;
    return dfsch_lookup(exp,env);
  }

  if(DFSCH_TYPE_OF(exp) == PAIR){
    
    object_t *f = dfsch_eval_impl(DFSCH_FAST_CAR(exp), env, NULL, ti);

    
    if (DFSCH_TYPE_OF(f) == FORM){
      ti->stack_frame->env = env;
      ti->stack_frame->expr = exp;
      return ((dfsch_form_t*)f)->impl(((dfsch_form_t*)f), 
                                      env, 
                                      DFSCH_FAST_CDR(exp), 
                                      esc);
    }

    if (DFSCH_TYPE_OF(f) == MACRO){
      ti->stack_frame->env = env;
      ti->stack_frame->expr = exp;
      return dfsch_eval_impl(dfsch_macro_expand(f, DFSCH_FAST_CDR(exp)),
			     env,
 			     esc,
			     ti);
    }

    args = eval_list(DFSCH_FAST_CDR(exp), env, ti);
    ti->stack_frame->env = env;
    ti->stack_frame->expr = exp;

    return dfsch_apply_impl(f, 
                            args,
                            esc,
                            ti);
    
    
  }  
  
  return exp;
}

dfsch_object_t* dfsch_eval_tr(dfsch_object_t* exp, 
                              dfsch_object_t* env,
                              dfsch_tail_escape_t* esc){
  return dfsch_eval_impl(exp, env, esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env){
  return dfsch_eval_impl(exp, env, NULL, dfsch__get_thread_info());
}

static void destructure_impl(dfsch_object_t* llist,
                             dfsch_object_t* list,
                             dfsch_object_t* hash){
  while ((DFSCH_TYPE_OF(llist) == PAIR) &&
	 (DFSCH_TYPE_OF(list) == PAIR)){

    if (DFSCH_TYPE_OF(DFSCH_FAST_CAR(llist)) == PAIR){
      destructure_impl(DFSCH_FAST_CAR(llist), 
                       DFSCH_FAST_CAR(list), 
                       hash);
    } else {
      dfsch_hash_set(hash, DFSCH_FAST_CAR(llist), DFSCH_FAST_CAR(list));
    }

    llist = DFSCH_FAST_CDR(llist);
    list = DFSCH_FAST_CDR(list);
    
  }

  if (DFSCH_TYPE_OF(llist) != PAIR){
    dfsch_hash_set(hash, (object_t*)llist, (object_t*)list);
    return;
  }

  if (!list  && llist){
    dfsch_error("Too few arguments", dfsch_list(2, 
						llist, 
						list));
  }
  if (!llist && list) {
    dfsch_error("Too many arguments", dfsch_list(2,
						 llist, 
						 list));
  }
}

dfsch_object_t* dfsch_destructure(dfsch_object_t* arglist,
                                  dfsch_object_t* list){
  object_t* hash = dfsch_hash_make(DFSCH_HASH_EQ);

  destructure_impl(arglist, list, hash);

  return hash;
}

dfsch_object_t* dfsch_destructuring_bind(dfsch_object_t* arglist, 
                                         dfsch_object_t* list, 
                                         dfsch_object_t* env){
  return dfsch_new_frame_from_hash(env,
                                   dfsch_destructure(arglist,
                                                     list));
}

static dfsch_object_t* dfsch_eval_proc_impl(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            tail_escape_t* esc,
                                            dfsch__thread_info_t* ti){
  dfsch_object_t *i;
  object_t *r=NULL;
  dfsch_object_t *old_frame;
  dfsch_object_t *my_frame;

  if (!env)
    return NULL;
  if (!code)
    return NULL;

  if (ti->break_type){
    char* type = ti->break_type;
    ti->break_type = NULL;
    dfsch_error("Break", dfsch_make_symbol(type));
  }

  ti->stack_frame->code = code;

  i = code;

  while (DFSCH_TYPE_OF(i) == PAIR ){
    object_t* exp = DFSCH_FAST_CAR(i); 

    if (DFSCH_FAST_CDR(i))
      r = dfsch_eval_impl(exp, env, NULL, ti);
    else
      r = dfsch_eval_impl(exp, env, esc, ti);
   
    i = DFSCH_FAST_CDR(i);
  }

  return r;
}

dfsch_object_t* dfsch_eval_proc_tr(dfsch_object_t* code, 
                                   dfsch_object_t* env,
                                   tail_escape_t* esc){
  return dfsch_eval_proc_impl(code, env, esc, 
                              dfsch__get_thread_info());
}
dfsch_object_t* dfsch_eval_proc(dfsch_object_t* code, dfsch_object_t* env){
  return dfsch_eval_proc_impl(code, env, NULL, 
                              dfsch__get_thread_info());
}

struct dfsch_tail_escape_t {
  jmp_buf ret;
  object_t *proc;
  object_t *args; 
};

/* it might be interesting to optionally disable tail-calls for slight 
 * performance boost (~5%) */

static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti){
  dfsch__stack_frame_t f;
  dfsch_object_t* r;
  tail_escape_t myesc;

  if (esc){
    esc->proc = proc;
    esc->args = args;
    longjmp(esc->ret,1);
  }

  f.next = ti->stack_frame;
  f.env = NULL;
  f.expr = NULL;
  f.code = NULL;

  if (setjmp(myesc.ret)){  
    proc = myesc.proc;
    args = myesc.args;
    f.tail_recursive = 1;
  } else {
    f.tail_recursive = 0;
  }


  f.procedure = proc;
  f.arguments = args;
  ti->stack_frame = &f;

  /*
   * Two most common cases are written here explicitly (for historical
   * and performance reasons)
   */

  if (DFSCH_TYPE_OF(proc) == PRIMITIVE){
    r = ((primitive_t*)proc)->proc(((primitive_t*)proc)->baton,args,
                                   &myesc);
    goto out;

  }

  if (DFSCH_TYPE_OF(proc) == DFSCH_STANDARD_FUNCTION_TYPE){
    dfsch_object_t* env = dfsch_destructuring_bind(((closure_t*)proc)->args,
                                                   args,
                                                   ((closure_t*)proc)->env);
    dfsch_set_frame_context(env, proc, args);
    r = 
      dfsch_eval_proc_impl(((closure_t*)proc)->code,
                           env,
                           &myesc,
                           ti);
    goto out;
  }

  if (DFSCH_TYPE_OF(proc)->apply){
    r = DFSCH_TYPE_OF(proc)->apply(proc, args, &myesc);
    goto out;
  }

  dfsch_error("Not a procedure", proc);

 out:
  ti->stack_frame = f.next;
  return r;
   
}

dfsch_object_t* dfsch_apply_tr(dfsch_object_t* proc, 
                               dfsch_object_t* args,
                               tail_escape_t* esc){
  return dfsch_apply_impl(proc, args, esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args){
  return dfsch_apply_impl(proc, args, NULL, dfsch__get_thread_info());
}

dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, dfsch_object_t* arg){
  if (dfsch_pair_p(arg)){
    object_t* car = dfsch_car(arg);
    object_t* cdr = dfsch_cdr(arg);

    if (car == dfsch_sym_unquote() && dfsch_pair_p(cdr)){
      return dfsch_eval(dfsch_car(cdr), env);
    }else if (dfsch_pair_p(car)){
      if (dfsch_car(car) == dfsch_sym_unquote_splicing()){
        return dfsch_append(dfsch_list(2,
                                       dfsch_eval(dfsch_car(dfsch_cdr(car)), 
                                                  env),
                                       dfsch_quasiquote(env, cdr)));
      }
    }

    return dfsch_cons(dfsch_quasiquote(env,car), dfsch_quasiquote(env,cdr));
  }else{
    return arg;
  }
}

static object_t* native_top_level_environment(void *baton, object_t* args,
                                              dfsch_tail_escape_t* esc){
  return baton;
}

DFSCH_DEFINE_FORM_IMPL(current_environment){
  return env;
}

dfsch_object_t* dfsch_make_context(){
  dfsch_object_t* ctx;

  ctx = dfsch_new_frame(NULL);

  dfsch_define_cstr(ctx, "<standard-type>", DFSCH_STANDARD_TYPE);
  dfsch_define_cstr(ctx, "<abstract-type>", DFSCH_ABSTRACT_TYPE);
  dfsch_define_cstr(ctx, "<meta-type>", DFSCH_META_TYPE);
  dfsch_define_cstr(ctx, "<special-type>", DFSCH_SPECIAL_TYPE);
  dfsch_define_cstr(ctx, "<standard-function>", DFSCH_STANDARD_FUNCTION_TYPE);

  dfsch_define_cstr(ctx, "<slot-type>", DFSCH_SLOT_TYPE_TYPE);
  dfsch_define_cstr(ctx, "<slot>", DFSCH_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<object-slot>", DFSCH_OBJECT_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<boolean-slot>", DFSCH_BOOLEAN_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<string-slot>", DFSCH_STRING_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<size_t-slot>", DFSCH_SIZE_T_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<int-slot>", DFSCH_INT_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<long-slot>", DFSCH_LONG_SLOT_TYPE);

  dfsch_define_cstr(ctx, "<list>", DFSCH_LIST_TYPE);
  dfsch_define_cstr(ctx, "<pair>", DFSCH_PAIR_TYPE);
  dfsch_define_cstr(ctx, "<empty-list>", DFSCH_EMPTY_LIST_TYPE);
  dfsch_define_cstr(ctx, "<symbol>", DFSCH_SYMBOL_TYPE);
  dfsch_define_cstr(ctx, "<primitive>", DFSCH_PRIMITIVE_TYPE);
  dfsch_define_cstr(ctx, "<function>", DFSCH_FUNCTION_TYPE);
  dfsch_define_cstr(ctx, "<macro>", DFSCH_MACRO_TYPE);
  dfsch_define_cstr(ctx, "<form>", DFSCH_FORM_TYPE);
  dfsch_define_cstr(ctx, "<vector>", DFSCH_VECTOR_TYPE);

  dfsch_define_cstr(ctx, "top-level-environment", 
                    dfsch_make_primitive(&native_top_level_environment, ctx));
  dfsch_define_cstr(ctx, "current-environment", 
                    DFSCH_FORM_REF(current_environment));
  dfsch_define_cstr(ctx,"*dfsch-version*",dfsch_make_string_cstr(PACKAGE_VERSION));
  dfsch_define_cstr(ctx,"*dfsch-platform*",dfsch_make_string_cstr(HOST_TRIPLET));

  dfsch__native_register(ctx);

  return ctx;
}


dfsch_object_t* dfsch_define_cstr(dfsch_object_t *ctx, 
                                  char *name, 
                                  dfsch_object_t *obj){
  
  return dfsch_define(dfsch_make_symbol(name),
                      obj,
                      ctx);
  
}
dfsch_object_t* dfsch_set_cstr(dfsch_object_t *ctx, 
			       char *name, 
			       dfsch_object_t *obj){
  
  return dfsch_set(dfsch_make_symbol(name),
                   obj,
                   ctx);
  
}
dfsch_object_t* dfsch_lookup_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_lookup(dfsch_make_symbol(name), ctx);
}
dfsch_object_t* dfsch_env_get_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_env_get(dfsch_make_symbol(name), ctx);
}
