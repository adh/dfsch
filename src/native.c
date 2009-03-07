/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Basic native functions
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


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "internal.h"
#include <dfsch/promise.h>
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <dfsch/number.h>
#include <dfsch/strings.h>

typedef dfsch_object_t object_t;

#define NEED_ARGS(args,count) \
  if (dfsch_list_length_check(args)!=(count)) \
    dfsch_error("exception:wrong-number-of-arguments",(args));
#define MIN_ARGS(args,count) \
  if (dfsch_list_length_check(args)<(count)) \
    dfsch_error("exception:too-few-arguments", (args));

// TODO: document all native functions somewhere

// Native procedures:

DFSCH_DEFINE_PRIMITIVE(gensym, 0){
  if (args)
    dfsch_error("exception:too-many-arguments", args);

  return dfsch_gensym();
}

DFSCH_DEFINE_PRIMITIVE(unintern, 0){
  object_t* symbol;
  DFSCH_OBJECT_ARG(args, symbol);
  DFSCH_ARG_END(args);

  dfsch_unintern(symbol);
  return symbol;
}

DFSCH_DEFINE_PRIMITIVE(id, 0){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long((long)object);
}
DFSCH_DEFINE_PRIMITIVE(hash, 0){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long((long)dfsch_hash(object));
}
DFSCH_DEFINE_PRIMITIVE(type_of, 0){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return (object_t*)DFSCH_TYPE_OF(object);
}
DFSCH_DEFINE_PRIMITIVE(type_name, 0){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  if (!DFSCH_INSTANCE_P(object, DFSCH_STANDARD_TYPE)){
    dfsch_error("exception:not-a-type", object);
  }

  return dfsch_make_string_cstr(((dfsch_type_t*)object)->name);
}

DFSCH_DEFINE_PRIMITIVE(superclass, 0){
  object_t* type;
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_ARG_END(args);

  return dfsch_superclass(type);
}
DFSCH_DEFINE_PRIMITIVE(superclass_p, 0){
  dfsch_object_t* sub;
  dfsch_object_t* super;
  DFSCH_OBJECT_ARG(args, sub);
  DFSCH_OBJECT_ARG(args, super);
  DFSCH_ARG_END(args);

  if (!DFSCH_INSTANCE_P(sub, DFSCH_STANDARD_TYPE)){
    dfsch_error("exception:not-a-type", sub);
  }

  if (super && !DFSCH_INSTANCE_P(super, DFSCH_STANDARD_TYPE)){
    dfsch_error("exception:not-a-type", super);
  }

  return dfsch_bool(dfsch_superclass_p((dfsch_type_t*)sub, 
                                       (dfsch_type_t*)super));
}
DFSCH_DEFINE_PRIMITIVE(instance_p, 0){
  dfsch_object_t* object;
  dfsch_object_t* type;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_ARG_END(args);

  if (type && !DFSCH_INSTANCE_P(type, DFSCH_STANDARD_TYPE)){
    dfsch_error("exception:not-a-standard-type", type);
  }

  return dfsch_bool(dfsch_instance_p(object, (dfsch_type_t*)type));
}

DFSCH_DEFINE_PRIMITIVE(slot_set, 0){
  dfsch_object_t* object;
  dfsch_object_t* value;
  char* name;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  dfsch_slot_set_by_name(object, name, value, 0);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(slot_ref, 0){
  dfsch_object_t* object;
  dfsch_object_t* value;
  char* name;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_slot_ref_by_name(object, name, 0);  
}
DFSCH_DEFINE_PRIMITIVE(find_slot, 0){
  dfsch_type_t* type;
  char* name;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_find_slot(type, name);  
}
DFSCH_DEFINE_PRIMITIVE(get_slots, 0){
  dfsch_type_t* type;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_ARG_END(args);

  return dfsch_get_slots(type);  
}


/////////////////////////////////////////////////////////////////////////////
//
// Basic special forms
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_FORM_IMPL(lambda){
  MIN_ARGS(args,1);
  return dfsch_lambda(env,
		      dfsch_car(args),
		      dfsch_cdr(args));
}

DFSCH_DEFINE_FORM_IMPL(define){

  MIN_ARGS(args,1);  

  object_t* name = dfsch_car(args);

  if (DFSCH_PAIR_P(name)){
    object_t* lambda = dfsch_named_lambda(env,dfsch_cdr(name),
                                          dfsch_cdr(args),
                                          dfsch_car(name));
    name = DFSCH_FAST_CAR(name);
    return dfsch_define(name, lambda ,env);
  }else{
    object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(args)),env);
    return dfsch_define(name,value,env);
  }
}

DFSCH_DEFINE_FORM_IMPL(define_variable){
  dfsch_object_t* name;
  dfsch_object_t* value;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, value, NULL);
  DFSCH_ARG_END(args);
  
  if (!dfsch_env_get(name, env)){
    value = dfsch_eval(value, env);
    dfsch_define(name, value, env);
    return value;
  } else {
    return NULL;
  }
}
DFSCH_DEFINE_FORM_IMPL(define_constant){
  dfsch_object_t* name;
  dfsch_object_t* value;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, value, NULL);
  DFSCH_ARG_END(args);
  
  if (!dfsch_env_get(name, env)){
    value = dfsch_eval(value, env);
    dfsch_define(name, value, env);
    dfsch_declare(name, dfsch_make_symbol("constant"), env);
    return value;
  } else {
    return NULL;
  }
}

DFSCH_DEFINE_FORM_IMPL(declare){
  dfsch_object_t* name;
  dfsch_object_t* decls;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_REST(args, decls);

  while (DFSCH_PAIR_P(decls)){
    dfsch_declare(name, DFSCH_FAST_CDR(decls), env);
    decls = DFSCH_FAST_CDR(decls);
  }

  return NULL;
}

DFSCH_DEFINE_FORM_IMPL(set){
  NEED_ARGS(args,2);  

  object_t* name = dfsch_car(args);
  object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(args)),env);

  return dfsch_set(name, value, env);

}
DFSCH_DEFINE_FORM_IMPL(unset){
  object_t* name;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);

  dfsch_unset(name, env);

  return NULL;
}
DFSCH_DEFINE_FORM_IMPL(defined_p){
  NEED_ARGS(args,1);
  object_t* name = dfsch_car(args);

  return dfsch_env_get(name, env);
}

DFSCH_DEFINE_PRIMITIVE(make_macro, 0){
  NEED_ARGS(args,1);  
  return dfsch_make_macro(dfsch_car(args));
}
DFSCH_DEFINE_FORM_IMPL(define_macro){
  dfsch_object_t* name;
  dfsch_object_t* arglist;

  DFSCH_OBJECT_ARG(args, arglist);
  DFSCH_OBJECT_ARG(arglist, name);

  dfsch_define(name, 
               dfsch_make_macro(dfsch_named_lambda(env,
                                                   arglist,
                                                   args,
                                                   name)), 
               env);
}


/////////////////////////////////////////////////////////////////////////////
//
// Pairs and lists
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(car, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_car(dfsch_car(args));
}
DFSCH_DEFINE_PRIMITIVE(cdr, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_cdr(dfsch_car(args));
}
DFSCH_DEFINE_PRIMITIVE(cons, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,2);  
  return dfsch_cons(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
DFSCH_DEFINE_PRIMITIVE(cons_immutable, DFSCH_PRIMITIVE_CACHED){
  dfsch_object_t* car;
  dfsch_object_t* cdr;
  DFSCH_OBJECT_ARG(args, car);
  DFSCH_OBJECT_ARG(args, cdr);
  DFSCH_ARG_END(args);
  return dfsch_cons_immutable(car, cdr);
}

DFSCH_DEFINE_PRIMITIVE(list, DFSCH_PRIMITIVE_CACHED){
  return dfsch_list_copy(args);
}
DFSCH_DEFINE_PRIMITIVE(length, DFSCH_PRIMITIVE_CACHED){
  long len;
  NEED_ARGS(args,1);  

  len = dfsch_list_length_check(dfsch_car(args));

  return dfsch_make_number_from_long(len);
}
DFSCH_DEFINE_PRIMITIVE(set_car, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,2);  
  return dfsch_set_car(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
DFSCH_DEFINE_PRIMITIVE(set_cdr, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,2);  
  return dfsch_set_cdr(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
DFSCH_DEFINE_PRIMITIVE(zip, 0){
  return dfsch_zip(args);
}
DFSCH_DEFINE_PRIMITIVE(append, 0){
  return dfsch_append(args);
}
DFSCH_DEFINE_PRIMITIVE(list_ref, 0){
  int k;
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_LONG_ARG(args, k);
  DFSCH_ARG_END(args);

  return dfsch_list_item(list, k);
}
DFSCH_DEFINE_PRIMITIVE(reverse, 0){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_reverse(list);
}
DFSCH_DEFINE_PRIMITIVE(member, 0){
  object_t* list;
  object_t* key;

  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_member(key, list);
}
DFSCH_DEFINE_PRIMITIVE(memv, 0){
  object_t* list;
  object_t* key;

  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_memv(key, list);
}
DFSCH_DEFINE_PRIMITIVE(memq, 0){
  object_t* list;
  object_t* key;

  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_memq(key, list);
}
DFSCH_DEFINE_PRIMITIVE(sort_list, 0){
  object_t* list;
  object_t* comp;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_OBJECT_ARG(args, comp);
  DFSCH_ARG_END(args);

  return dfsch_sort_list(list, comp);
}

DFSCH_DEFINE_PRIMITIVE(assoc, 0){
  object_t* alist;
  object_t* key;

  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_ARG_END(args);

  return dfsch_assoc(key, alist);
}
DFSCH_DEFINE_PRIMITIVE(assv, 0){
  object_t* alist;
  object_t* key;

  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_ARG_END(args);

  return dfsch_assv(key, alist);
}
DFSCH_DEFINE_PRIMITIVE(assq, 0){
  object_t* alist;
  object_t* key;

  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_ARG_END(args);

  return dfsch_assq(key, alist);
}
DFSCH_DEFINE_PRIMITIVE(for_each, 0){
  object_t* func;
  object_t* list;
  size_t len;
  size_t i;

  DFSCH_OBJECT_ARG(args, func);
  list = dfsch_zip(args);

  if (!list){
    return NULL;
  }

  while (dfsch_pair_p(list)){
    dfsch_apply(func, dfsch_car(list));
    list = dfsch_cdr(list);
  }
  
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(map, 0){
  object_t* func;
  object_t* list;
  object_t* head = NULL;
  object_t* tail;

  DFSCH_OBJECT_ARG(args, func);
  list = dfsch_zip(args);

  if (!list){
    return NULL;
  }

  while (dfsch_pair_p(list)){
    object_t *t = dfsch_cons(dfsch_apply(func, dfsch_car(list)), NULL);
    if (!head){
      head = tail = t;
    }else{
      dfsch_set_cdr(tail, t);
      tail = t;
    }
    list = dfsch_cdr(list);
  }
  
  return head;
}
DFSCH_DEFINE_PRIMITIVE(filter, 0){
  object_t* func;
  object_t* list;
  object_t* head = NULL;
  object_t* tail;

  DFSCH_OBJECT_ARG(args, func);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  while (dfsch_pair_p(list)){
    object_t* item =  dfsch_car(list);
    object_t* t;

    if (dfsch_apply(func, 
                    dfsch_list(1, item))){
      t = dfsch_cons(item, NULL);

      if (!head){
        head = tail = t;
      }else{
        dfsch_set_cdr(tail, t);
        tail = t;
      }
    }
    list = dfsch_cdr(list);
  }
  
  return head;
}
DFSCH_DEFINE_PRIMITIVE(reduce, 0){
  object_t* func;
  object_t* list;
  object_t* tally;

  DFSCH_OBJECT_ARG(args, func);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  if (!dfsch_pair_p(list)){
    return NULL;
  }

  tally = dfsch_car(list);
  list = dfsch_cdr(list);

  while (dfsch_pair_p(list)) {
    tally = dfsch_apply(func, dfsch_list(2, tally, dfsch_car(list)));
    list = dfsch_cdr(list);
  }
  
  return tally;
}



/////////////////////////////////////////////////////////////////////////////
//
// Type predicates
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(null_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_null_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(pair_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_pair_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(list_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_list_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(atom_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_atom_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(symbol_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_symbol_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(string_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_string_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(primitive_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_primitive_p(dfsch_car(args))); 
}
DFSCH_DEFINE_PRIMITIVE(function_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_function_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(procedure_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_procedure_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(vector_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_vector_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(macro_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_macro_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(form_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_form_p(dfsch_car(args)));  
}

/////////////////////////////////////////////////////////////////////////////
//
// Equality predicates
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(eq_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_eq_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}
DFSCH_DEFINE_PRIMITIVE(eqv_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_eqv_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}
DFSCH_DEFINE_PRIMITIVE(equal_p, DFSCH_PRIMITIVE_CACHED){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_equal_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}

/////////////////////////////////////////////////////////////////////////////
//
// Logic
//
/////////////////////////////////////////////////////////////////////////////


DFSCH_DEFINE_FORM_IMPL(or){
  object_t* i;
  object_t* r = NULL;
  i = args;
 
  while(i){
    r = dfsch_eval(dfsch_car(i), env);
    if (r)
      return r;
    i = dfsch_cdr(i);
  }

  return r;
}
DFSCH_DEFINE_FORM_IMPL(and){
  object_t* i;
  object_t* r = DFSCH_SYM_TRUE;
  i = args;
 
  while(i){
    r = dfsch_eval(dfsch_car(i), env);
    if (!r)
      return r;

    i = dfsch_cdr(i);
  }

  return r;
}
DFSCH_DEFINE_PRIMITIVE(not, DFSCH_PRIMITIVE_CACHED){
  dfsch_object_t* val;
  DFSCH_OBJECT_ARG(args, val);
  DFSCH_ARG_END(args);
  return dfsch_bool(val == NULL);
}

/////////////////////////////////////////////////////////////////////////////
//
// Vectors
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(make_vector, DFSCH_PRIMITIVE_CACHED){
  size_t length;
  object_t* fill;

  DFSCH_LONG_ARG(args, length);
  DFSCH_OBJECT_ARG_OPT(args, fill, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_vector(length,fill);
}

DFSCH_DEFINE_PRIMITIVE(vector, DFSCH_PRIMITIVE_CACHED){
  return dfsch_list_2_vector(args);
}
DFSCH_DEFINE_PRIMITIVE(vector_length, DFSCH_PRIMITIVE_CACHED){
  object_t* vector;
  
  DFSCH_OBJECT_ARG(args,vector);
  DFSCH_ARG_END(args);

  if (!dfsch_vector_p(vector))
    dfsch_error("exception:not-a-vector",vector);

  return dfsch_make_number_from_long(dfsch_vector_length(vector));

}
DFSCH_DEFINE_PRIMITIVE(vector_ref, DFSCH_PRIMITIVE_CACHED){
  object_t* vector;
  size_t k;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_ARG_END(args);

  return dfsch_vector_ref(vector, k);
}

DFSCH_DEFINE_PRIMITIVE(vector_set, DFSCH_PRIMITIVE_CACHED){
  object_t* vector;
  size_t k;
  object_t* obj;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_vector_set(vector, k, obj);
}

DFSCH_DEFINE_PRIMITIVE(vector_2_list, DFSCH_PRIMITIVE_CACHED){
  object_t* vector;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_ARG_END(args);

  return dfsch_vector_2_list(vector);
}

DFSCH_DEFINE_PRIMITIVE(list_2_vector, DFSCH_PRIMITIVE_CACHED){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_vector(list);
}

/////////////////////////////////////////////////////////////////////////////
//
// Reading and writing strings
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(object_2_string, 0){
  object_t* object;
  object_t* readable;
  long depth;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_LONG_ARG_OPT(args, depth, 256);
  DFSCH_OBJECT_ARG_OPT(args, readable, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_object_2_string(object, depth, 
                                                      readable != NULL));
}
DFSCH_DEFINE_PRIMITIVE(string_2_object, 0){
  char* string;

  DFSCH_STRING_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_object(string);
}
DFSCH_DEFINE_PRIMITIVE(string_2_object_list, 0){
  char* string;

  DFSCH_STRING_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_object_list(string);
}
DFSCH_DEFINE_PRIMITIVE(write__object, 0){
  dfsch_object_t* state;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, state);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  dfsch_write_object(DFSCH_ASSERT_TYPE(state, DFSCH_WRITER_STATE_TYPE),
                     object);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(write__string, 0){
  dfsch_object_t* state;
  dfsch_strbuf_t* string;
  DFSCH_OBJECT_ARG(args, state);
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  dfsch_write_strbuf(DFSCH_ASSERT_TYPE(state, DFSCH_WRITER_STATE_TYPE),
                     string->ptr, string->len);
  return NULL;
}

/////////////////////////////////////////////////////////////////////////////
//
// Symbols
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(symbol_2_string, 0){
  object_t* object;
  char* str;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  str = dfsch_symbol(object);
  if (str)
    return dfsch_make_string_cstr(str);
  else
    dfsch_error("exception:not-a-symbol", object);
}
DFSCH_DEFINE_PRIMITIVE(string_2_symbol, 0){
  char* string;

  DFSCH_STRING_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_symbol(string);
}

DFSCH_DEFINE_PRIMITIVE(macro_expand, 0){
  dfsch_object_t* macro;
  dfsch_object_t* arguments;

  DFSCH_OBJECT_ARG(args, macro);
  DFSCH_OBJECT_ARG(args, arguments);
  DFSCH_ARG_END(args);

  return dfsch_macro_expand(macro, arguments);
}

/*
 * Properties
 */

DFSCH_DEFINE_PRIMITIVE(get_properties, 0){
  dfsch_object_t* obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);
  
  return dfsch_get_object_properties(obj);
}
DFSCH_DEFINE_PRIMITIVE(get_property, 0){
  dfsch_object_t* obj;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  return dfsch_get_object_property(obj, name);
}
DFSCH_DEFINE_PRIMITIVE(set_property, 0){
  dfsch_object_t* obj;
  dfsch_object_t* name;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);
  
  dfsch_set_object_property(obj, name, value);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(unset_property, 0){
  dfsch_object_t* obj;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  dfsch_unset_object_property(obj, name);
  return NULL;
}

/////////////////////////////////////////////////////////////////////////////
//
// Registering function
//
/////////////////////////////////////////////////////////////////////////////

void dfsch__native_register(dfsch_object_t *ctx){ 
  dfsch_define_cstr(ctx, "gensym", DFSCH_PRIMITIVE_REF(gensym));
  dfsch_define_cstr(ctx, "unintern", DFSCH_PRIMITIVE_REF(unintern));
  dfsch_define_cstr(ctx, "id", DFSCH_PRIMITIVE_REF(id));
  dfsch_define_cstr(ctx, "hash", DFSCH_PRIMITIVE_REF(hash));
  dfsch_define_cstr(ctx, "type-of", DFSCH_PRIMITIVE_REF(type_of));
  dfsch_define_cstr(ctx, "type-name", DFSCH_PRIMITIVE_REF(type_name));
  dfsch_define_cstr(ctx, "superclass?", DFSCH_PRIMITIVE_REF(superclass_p));
  dfsch_define_cstr(ctx, "instance?", DFSCH_PRIMITIVE_REF(instance_p));
  dfsch_define_cstr(ctx, "superclass", DFSCH_PRIMITIVE_REF(superclass));

  dfsch_define_cstr(ctx, "eq?", DFSCH_PRIMITIVE_REF(eq_p));
  dfsch_define_cstr(ctx, "eqv?", DFSCH_PRIMITIVE_REF(eqv_p));
  dfsch_define_cstr(ctx, "equal?", DFSCH_PRIMITIVE_REF(equal_p));

  dfsch_define_cstr(ctx, "and", DFSCH_FORM_REF(and));
  dfsch_define_cstr(ctx, "or",DFSCH_FORM_REF(or));
  dfsch_define_cstr(ctx, "not", DFSCH_PRIMITIVE_REF(not));

  dfsch_define_cstr(ctx, "lambda", DFSCH_FORM_REF(lambda));
  dfsch_define_cstr(ctx, "define", DFSCH_FORM_REF(define));
  dfsch_define_cstr(ctx, "define-variable", DFSCH_FORM_REF(define_variable));
  dfsch_define_cstr(ctx, "define-constant", DFSCH_FORM_REF(define_constant));
  dfsch_define_cstr(ctx, "declare", DFSCH_FORM_REF(declare));
  dfsch_define_cstr(ctx, "defined?", DFSCH_FORM_REF(defined_p));
  dfsch_define_cstr(ctx, "set!", DFSCH_FORM_REF(set));
  dfsch_define_cstr(ctx, "unset!", DFSCH_FORM_REF(unset));

  dfsch_define_cstr(ctx, "make-macro", DFSCH_PRIMITIVE_REF(make_macro));
  dfsch_define_cstr(ctx, "define-macro", DFSCH_FORM_REF(define_macro));
  dfsch_define_cstr(ctx, "cons", DFSCH_PRIMITIVE_REF(cons));
  dfsch_define_cstr(ctx, "cons-immutable", DFSCH_PRIMITIVE_REF(cons_immutable));
  dfsch_define_cstr(ctx, "list", DFSCH_PRIMITIVE_REF(list));
  dfsch_define_cstr(ctx, "car", DFSCH_PRIMITIVE_REF(car));
  dfsch_define_cstr(ctx, "cdr", DFSCH_PRIMITIVE_REF(cdr));
  dfsch_define_cstr(ctx, "set-car!", DFSCH_PRIMITIVE_REF(set_car));
  dfsch_define_cstr(ctx, "set-cdr!", DFSCH_PRIMITIVE_REF(set_cdr));

  dfsch_define_cstr(ctx, "length", DFSCH_PRIMITIVE_REF(length));
  dfsch_define_cstr(ctx, "zip", DFSCH_PRIMITIVE_REF(zip));
  dfsch_define_cstr(ctx, "append", DFSCH_PRIMITIVE_REF(append));
  dfsch_define_cstr(ctx, "for-each", DFSCH_PRIMITIVE_REF(for_each));
  dfsch_define_cstr(ctx, "map", DFSCH_PRIMITIVE_REF(map));
  dfsch_define_cstr(ctx, "filter", DFSCH_PRIMITIVE_REF(filter));
  dfsch_define_cstr(ctx, "reduce", DFSCH_PRIMITIVE_REF(reduce));
  dfsch_define_cstr(ctx, "list-ref", DFSCH_PRIMITIVE_REF(list_ref));
  dfsch_define_cstr(ctx, "reverse", DFSCH_PRIMITIVE_REF(reverse));
  dfsch_define_cstr(ctx, "member", DFSCH_PRIMITIVE_REF(member));
  dfsch_define_cstr(ctx, "memq", DFSCH_PRIMITIVE_REF(memq));
  dfsch_define_cstr(ctx, "memv", DFSCH_PRIMITIVE_REF(memv));
  dfsch_define_cstr(ctx, "sort-list!", DFSCH_PRIMITIVE_REF(sort_list));
  dfsch_define_cstr(ctx, "assoc", DFSCH_PRIMITIVE_REF(assoc));
  dfsch_define_cstr(ctx, "assq", DFSCH_PRIMITIVE_REF(assq));
  dfsch_define_cstr(ctx, "assv", DFSCH_PRIMITIVE_REF(assv));

  dfsch_define_cstr(ctx, "null?", DFSCH_PRIMITIVE_REF(null_p));
  dfsch_define_cstr(ctx, "atom?", DFSCH_PRIMITIVE_REF(atom_p));
  dfsch_define_cstr(ctx, "pair?", DFSCH_PRIMITIVE_REF(pair_p));
  dfsch_define_cstr(ctx, "list?", DFSCH_PRIMITIVE_REF(list_p));
  dfsch_define_cstr(ctx, "symbol?", DFSCH_PRIMITIVE_REF(symbol_p));
  dfsch_define_cstr(ctx, "string?", DFSCH_PRIMITIVE_REF(string_p));
  dfsch_define_cstr(ctx, "primitive?", 
		   DFSCH_PRIMITIVE_REF(primitive_p));
  dfsch_define_cstr(ctx, "function?", DFSCH_PRIMITIVE_REF(function_p));
  dfsch_define_cstr(ctx, "procedure?", 
		   DFSCH_PRIMITIVE_REF(procedure_p));
  dfsch_define_cstr(ctx, "macro?", DFSCH_PRIMITIVE_REF(macro_p));
  dfsch_define_cstr(ctx, "form?", DFSCH_PRIMITIVE_REF(form_p));
  dfsch_define_cstr(ctx, "vector?", DFSCH_PRIMITIVE_REF(vector_p));


  dfsch_define_cstr(ctx, "true", DFSCH_SYM_TRUE);
  dfsch_define_cstr(ctx, "nil", NULL);
  dfsch_define_cstr(ctx, "else", DFSCH_SYM_TRUE);
  dfsch_define_cstr(ctx, "T", DFSCH_SYM_TRUE);


  dfsch_define_cstr(ctx, "make-vector", 
                   DFSCH_PRIMITIVE_REF(make_vector));
  dfsch_define_cstr(ctx, "vector", 
                   DFSCH_PRIMITIVE_REF(vector));
  dfsch_define_cstr(ctx, "vector-length", 
                   DFSCH_PRIMITIVE_REF(vector_length));
  dfsch_define_cstr(ctx, "vector-set!", 
                   DFSCH_PRIMITIVE_REF(vector_set));
  dfsch_define_cstr(ctx, "vector-ref", 
                   DFSCH_PRIMITIVE_REF(vector_ref));
  dfsch_define_cstr(ctx, "vector->list", 
                   DFSCH_PRIMITIVE_REF(vector_2_list));
  dfsch_define_cstr(ctx, "list->vector", 
                   DFSCH_PRIMITIVE_REF(list_2_vector));

  dfsch_define_cstr(ctx, "object->string", 
                   DFSCH_PRIMITIVE_REF(object_2_string));
  dfsch_define_cstr(ctx, "string->object", 
                   DFSCH_PRIMITIVE_REF(string_2_object));
  dfsch_define_cstr(ctx, "string->object-list", 
                   DFSCH_PRIMITIVE_REF(string_2_object_list));
  dfsch_define_cstr(ctx, "dfsch%write-object", 
                   DFSCH_PRIMITIVE_REF(write__object));
  dfsch_define_cstr(ctx, "dfsch%write-string", 
                   DFSCH_PRIMITIVE_REF(write__string));

  dfsch_define_cstr(ctx, "symbol->string", 
                   DFSCH_PRIMITIVE_REF(symbol_2_string));
  dfsch_define_cstr(ctx, "string->symbol", 
                   DFSCH_PRIMITIVE_REF(string_2_symbol));

  dfsch_define_cstr(ctx, "macro-expand", 
                   DFSCH_PRIMITIVE_REF(macro_expand));

  dfsch_define_cstr(ctx, "get-properties", DFSCH_PRIMITIVE_REF(get_properties));
  dfsch_define_cstr(ctx, "get-property", DFSCH_PRIMITIVE_REF(get_property));
  dfsch_define_cstr(ctx, "set-property!", DFSCH_PRIMITIVE_REF(set_property));
  dfsch_define_cstr(ctx, "unset-property!", 
                    DFSCH_PRIMITIVE_REF(unset_property));

  dfsch_define_cstr(ctx, "slot-ref", DFSCH_PRIMITIVE_REF(slot_ref));
  dfsch_define_cstr(ctx, "slot-set!", DFSCH_PRIMITIVE_REF(slot_set));
  dfsch_define_cstr(ctx, "get-slots", DFSCH_PRIMITIVE_REF(get_slots));
  dfsch_define_cstr(ctx, "find-slot", DFSCH_PRIMITIVE_REF(find_slot));


  dfsch__native_cxr_register(ctx);
  dfsch__control_register(ctx);
  dfsch__system_register(ctx);
  dfsch__hash_native_register(ctx);
  dfsch__promise_native_register(ctx);
  dfsch__number_native_register(ctx);
  dfsch__string_native_register(ctx);
  dfsch__wrapper_native_register(ctx);
  dfsch__object_native_register(ctx);
  dfsch__weak_native_register(ctx);
  dfsch__format_native_register(ctx);
  dfsch__port_native_register(ctx);
  dfsch__bignum_register(ctx);
  dfsch__conditions_register(ctx);
  dfsch__random_register(ctx);
}
