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
#include <dfsch/generate.h>
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <dfsch/number.h>
#include <dfsch/strings.h>

typedef dfsch_object_t object_t;

#define NEED_ARGS(args,count)                                   \
  if (dfsch_list_length_check(args)!=(count))                   \
    dfsch_error("exception:wrong-number-of-arguments",(args));
#define MIN_ARGS(args,count)                            \
  if (dfsch_list_length_check(args)<(count))            \
    dfsch_error("exception:too-few-arguments", (args));

// TODO: document all native functions somewhere

// Native procedures:

DFSCH_DEFINE_PRIMITIVE(gensym, "Allocate new unnamed symbol"){
  if (args)
    dfsch_error("exception:too-many-arguments", args);

  return dfsch_gensym();
}

DFSCH_DEFINE_PRIMITIVE(id, "Return object pointer as fixnum"){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long((long)object);
}
DFSCH_DEFINE_PRIMITIVE(hash, "Calculate object's hash value"){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long((long)dfsch_hash(object));
}
DFSCH_DEFINE_PRIMITIVE(type_of, "Get type of object"){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return (object_t*)DFSCH_TYPE_OF(object);
}
DFSCH_DEFINE_PRIMITIVE(type_name, "Get name of given type"){
  object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  if (!DFSCH_INSTANCE_P(object, DFSCH_STANDARD_TYPE)){
    dfsch_error("exception:not-a-type", object);
  }

  return dfsch_make_string_cstr(((dfsch_type_t*)object)->name);
}

DFSCH_DEFINE_PRIMITIVE(superclass, "Get class's superclass"){
  object_t* type;
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_ARG_END(args);

  return dfsch_superclass(type);
}
DFSCH_DEFINE_PRIMITIVE(superclass_p, 
		       "Is class in list of superclasses of second class?"){
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
DFSCH_DEFINE_PRIMITIVE(instance_p, "Is object instance of given type?"){
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
dfsch_object_t* dfsch_generate_instance_p(dfsch_object_t* obj,
                                          dfsch_object_t* klass){
  return dfsch_immutable_list(3, 
                              DFSCH_PRIMITIVE_REF(instance_p), 
                              obj, klass);
}
DFSCH_DEFINE_PRIMITIVE(slot_set, "Store value into object's slot"){
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
DFSCH_DEFINE_PRIMITIVE(slot_ref, "Get value of object's slot"){
  dfsch_object_t* object;
  dfsch_object_t* value;
  char* name;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_slot_ref_by_name(object, name, 0);  
}
DFSCH_DEFINE_PRIMITIVE(find_slot, "Find slot-descriptor by it's name"){
  dfsch_type_t* type;
  char* name;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return (dfsch_object_t*)dfsch_find_slot(type, name);  
}
DFSCH_DEFINE_PRIMITIVE(get_slots, 
		       "Get all slot-descriptors usable for given type"){
  dfsch_type_t* type;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_ARG_END(args);

  return dfsch_get_slots(type);  
}
DFSCH_DEFINE_PRIMITIVE(make_slot_accessor,
                       "Create specialized slot accessor function"){
  dfsch_type_t* type;
  char* name;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_make_slot_accessor(type, name);    
}
DFSCH_DEFINE_PRIMITIVE(make_slot_reader,
                       "Create specialized slot reader function"){
  dfsch_type_t* type;
  char* name;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_make_slot_reader(type, name);    
}
DFSCH_DEFINE_PRIMITIVE(make_slot_writer,
                       "Create specialized slot writer function"){
  dfsch_type_t* type;
  char* name;
  DFSCH_TYPE_ARG(args, type);
  DFSCH_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_make_slot_writer(type, name);    
}


DFSCH_DEFINE_PRIMITIVE(get_list_annotation, 
		       "Return load position of list or NIL if it is "
		       "not avaiable"){
  dfsch_object_t* list;
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_get_list_annotation(list);
}


/////////////////////////////////////////////////////////////////////////////
//
// Pairs and lists
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(car, NULL){
  NEED_ARGS(args,1);  
  return dfsch_car(dfsch_car(args));
}
DFSCH_DEFINE_PRIMITIVE(cdr, NULL){
  NEED_ARGS(args,1);  
  return dfsch_cdr(dfsch_car(args));
}
DFSCH_DEFINE_PRIMITIVE(cons, NULL){
  NEED_ARGS(args,2);  
  return dfsch_cons(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
DFSCH_DEFINE_PRIMITIVE(cons_immutable, "Cons new immutable pair"){
  dfsch_object_t* car;
  dfsch_object_t* cdr;
  DFSCH_OBJECT_ARG(args, car);
  DFSCH_OBJECT_ARG(args, cdr);
  DFSCH_ARG_END(args);
  return dfsch_cons_immutable(car, cdr);
}

DFSCH_DEFINE_PRIMITIVE(list, NULL){
  return dfsch_list_copy(args);
}
dfsch_object_t* dfsch_generate_eval_list(dfsch_object_t* exps){
  return dfsch_cons(DFSCH_PRIMITIVE_REF(list), 
                    exps);
}


DFSCH_DEFINE_PRIMITIVE(length, NULL){
  long len;
  NEED_ARGS(args,1);  

  len = dfsch_list_length_check(dfsch_car(args));

  return dfsch_make_number_from_long(len);
}
DFSCH_DEFINE_PRIMITIVE(set_car, NULL){
  NEED_ARGS(args,2);  
  return dfsch_set_car(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
DFSCH_DEFINE_PRIMITIVE(set_cdr, NULL){
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

DFSCH_DEFINE_PRIMITIVE(null_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_null_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(pair_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_pair_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(list_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_list_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(atom_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_atom_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(symbol_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_symbol_p(dfsch_car(args)));
}
DFSCH_DEFINE_PRIMITIVE(string_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_string_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(primitive_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_primitive_p(dfsch_car(args))); 
}
DFSCH_DEFINE_PRIMITIVE(function_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_function_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(procedure_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_procedure_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(vector_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_vector_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(macro_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_macro_p(dfsch_car(args)));  
}
DFSCH_DEFINE_PRIMITIVE(form_p, NULL){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_form_p(dfsch_car(args)));  
}

/////////////////////////////////////////////////////////////////////////////
//
// Equality predicates
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(eq_p, NULL){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_eq_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}
DFSCH_DEFINE_PRIMITIVE(eqv_p, NULL){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_eqv_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}
DFSCH_DEFINE_PRIMITIVE(equal_p, NULL){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_equal_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}

DFSCH_DEFINE_PRIMITIVE(not, "Logical not - equivalent to null?"){
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

DFSCH_DEFINE_PRIMITIVE(make_vector, NULL){
  size_t length;
  object_t* fill;

  DFSCH_LONG_ARG(args, length);
  DFSCH_OBJECT_ARG_OPT(args, fill, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_vector(length,fill);
}

DFSCH_DEFINE_PRIMITIVE(vector, NULL){
  return dfsch_list_2_vector(args);
}
DFSCH_DEFINE_PRIMITIVE(vector_length, NULL){
  object_t* vector;
  
  DFSCH_OBJECT_ARG(args,vector);
  DFSCH_ARG_END(args);

  if (!dfsch_vector_p(vector))
    dfsch_error("exception:not-a-vector",vector);

  return dfsch_make_number_from_long(dfsch_vector_length(vector));

}
DFSCH_DEFINE_PRIMITIVE(vector_ref, NULL){
  object_t* vector;
  size_t k;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_ARG_END(args);

  return dfsch_vector_ref(vector, k);
}

DFSCH_DEFINE_PRIMITIVE(vector_set, NULL){
  object_t* vector;
  size_t k;
  object_t* obj;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_vector_set(vector, k, obj);
}

DFSCH_DEFINE_PRIMITIVE(vector_2_list, NULL){
  object_t* vector;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_ARG_END(args);

  return dfsch_vector_2_list(vector);
}

DFSCH_DEFINE_PRIMITIVE(list_2_vector, NULL){
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

DFSCH_DEFINE_PRIMITIVE(object_2_string, 
                       "Convert object to it's string representation"){
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
DFSCH_DEFINE_PRIMITIVE(string_2_object, 
                       "Read object from string"){
  char* string;

  DFSCH_STRING_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_object(string);
}
DFSCH_DEFINE_PRIMITIVE(string_2_object_list, 
                       "Read multiple objects from string"){
  char* string;

  DFSCH_STRING_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_object_list(string);
}
DFSCH_DEFINE_PRIMITIVE(write__object, 
                       "Recursively print object "
                       "- used by implementation of write methods"){
  dfsch_object_t* state;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, state);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  dfsch_write_object(DFSCH_ASSERT_TYPE(state, DFSCH_WRITER_STATE_TYPE),
                     object);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(write__string, 
                       "Print string in write method implementation"){
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

DFSCH_DEFINE_PRIMITIVE(symbol_2_string, 
                       "Return symbol's name as string"){
  object_t* object;
  char* str;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  str = dfsch_symbol_qualified_name(object);
  if (str)
    return dfsch_make_string_cstr(str);
  else
    dfsch_error("exception:not-a-symbol", object);
}
DFSCH_DEFINE_PRIMITIVE(string_2_symbol, 
                       "Intern symbol in current package"){
  char* string;

  DFSCH_STRING_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_symbol(string);
}
DFSCH_DEFINE_PRIMITIVE(intern_symbol, 
                       "Intern symbol in current package"){
  char* string;
  dfsch_package_t* package;

  DFSCH_STRING_ARG(args, string);
  DFSCH_PACKAGE_ARG_OPT(args, package, NULL);
  DFSCH_ARG_END(args);

  return dfsch_intern_symbol(package, string);
}

DFSCH_DEFINE_PRIMITIVE(macro_expand, 0){
  dfsch_object_t* macro;
  dfsch_object_t* arguments;

  DFSCH_OBJECT_ARG(args, macro);
  DFSCH_OBJECT_ARG(args, arguments);
  DFSCH_ARG_END(args);

  return dfsch_macro_expand(macro, arguments);
}

DFSCH_DEFINE_PRIMITIVE(eval, "Evaluate expression in lexical environment"){
  object_t* expr;
  object_t* env;

  DFSCH_OBJECT_ARG(args, expr);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);

  return dfsch_eval_tr(expr, env, esc);
}
DFSCH_DEFINE_PRIMITIVE(eval_proc, 
                       "Evaluate function body in lexical environment"){
  object_t* proc;
  object_t* env;

  DFSCH_OBJECT_ARG(args, proc);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);

  return dfsch_eval_proc_tr(proc, env, esc);
}
DFSCH_DEFINE_PRIMITIVE(apply, "Call function with given arguments"){
  /* TODO: free arguments */
  
  object_t* func;
  object_t* arglist;

  DFSCH_OBJECT_ARG(args, func);
  DFSCH_OBJECT_ARG(args, arglist);
  DFSCH_ARG_END(args);

  return dfsch_apply_tr(func, arglist, esc);
}

DFSCH_DEFINE_PRIMITIVE(throw, 0){
  dfsch_object_t* tag;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, tag);
  DFSCH_OBJECT_ARG(args, value);
  
  dfsch_throw(tag, value);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(make_macro, 
		       "Allocate new macro object implemented by function"){
  NEED_ARGS(args,1);  
  return dfsch_make_macro(dfsch_car(args));
}
dfsch_object_t* dfsch_generate_make_macro(dfsch_object_t* proc_exp){
  return dfsch_list(2, DFSCH_PRIMITIVE_REF(make_macro), proc_exp);
}

/////////////////////////////////////////////////////////////////////////////
//
// Registering function
//
/////////////////////////////////////////////////////////////////////////////

void dfsch__primitives_register(dfsch_object_t *ctx){ 
  dfsch_defconst_cstr(ctx, "gensym", DFSCH_PRIMITIVE_REF(gensym));
  dfsch_defconst_cstr(ctx, "id", DFSCH_PRIMITIVE_REF(id));
  dfsch_defconst_cstr(ctx, "hash", DFSCH_PRIMITIVE_REF(hash));
  dfsch_defconst_cstr(ctx, "type-of", DFSCH_PRIMITIVE_REF(type_of));
  dfsch_defconst_cstr(ctx, "type-name", DFSCH_PRIMITIVE_REF(type_name));
  dfsch_defconst_cstr(ctx, "superclass?", DFSCH_PRIMITIVE_REF(superclass_p));
  dfsch_defconst_cstr(ctx, "instance?", DFSCH_PRIMITIVE_REF(instance_p));
  dfsch_defconst_cstr(ctx, "superclass", DFSCH_PRIMITIVE_REF(superclass));
  dfsch_defconst_cstr(ctx, "get-list-annotation", 
                      DFSCH_PRIMITIVE_REF(get_list_annotation));

  dfsch_defconst_cstr(ctx, "eq?", DFSCH_PRIMITIVE_REF(eq_p));
  dfsch_defconst_cstr(ctx, "eqv?", DFSCH_PRIMITIVE_REF(eqv_p));
  dfsch_defconst_cstr(ctx, "equal?", DFSCH_PRIMITIVE_REF(equal_p));

  dfsch_defconst_cstr(ctx, "not", DFSCH_PRIMITIVE_REF(not));


  dfsch_defconst_cstr(ctx, "make-macro", DFSCH_PRIMITIVE_REF(make_macro));
  dfsch_defconst_cstr(ctx, "cons", DFSCH_PRIMITIVE_REF(cons));
  dfsch_defconst_cstr(ctx, "cons-immutable", DFSCH_PRIMITIVE_REF(cons_immutable));
  dfsch_defconst_cstr(ctx, "list", DFSCH_PRIMITIVE_REF(list));
  dfsch_defconst_cstr(ctx, "car", DFSCH_PRIMITIVE_REF(car));
  dfsch_defconst_cstr(ctx, "cdr", DFSCH_PRIMITIVE_REF(cdr));
  dfsch_defconst_cstr(ctx, "set-car!", DFSCH_PRIMITIVE_REF(set_car));
  dfsch_defconst_cstr(ctx, "set-cdr!", DFSCH_PRIMITIVE_REF(set_cdr));

  dfsch_defconst_cstr(ctx, "length", DFSCH_PRIMITIVE_REF(length));
  dfsch_defconst_cstr(ctx, "zip", DFSCH_PRIMITIVE_REF(zip));
  dfsch_defconst_cstr(ctx, "append", DFSCH_PRIMITIVE_REF(append));
  dfsch_defconst_cstr(ctx, "for-each", DFSCH_PRIMITIVE_REF(for_each));
  dfsch_defconst_cstr(ctx, "map", DFSCH_PRIMITIVE_REF(map));
  dfsch_defconst_cstr(ctx, "filter", DFSCH_PRIMITIVE_REF(filter));
  dfsch_defconst_cstr(ctx, "reduce", DFSCH_PRIMITIVE_REF(reduce));
  dfsch_defconst_cstr(ctx, "list-ref", DFSCH_PRIMITIVE_REF(list_ref));
  dfsch_defconst_cstr(ctx, "reverse", DFSCH_PRIMITIVE_REF(reverse));
  dfsch_defconst_cstr(ctx, "member", DFSCH_PRIMITIVE_REF(member));
  dfsch_defconst_cstr(ctx, "memq", DFSCH_PRIMITIVE_REF(memq));
  dfsch_defconst_cstr(ctx, "memv", DFSCH_PRIMITIVE_REF(memv));
  dfsch_defconst_cstr(ctx, "sort-list!", DFSCH_PRIMITIVE_REF(sort_list));
  dfsch_defconst_cstr(ctx, "assoc", DFSCH_PRIMITIVE_REF(assoc));
  dfsch_defconst_cstr(ctx, "assq", DFSCH_PRIMITIVE_REF(assq));
  dfsch_defconst_cstr(ctx, "assv", DFSCH_PRIMITIVE_REF(assv));

  dfsch_defconst_cstr(ctx, "null?", DFSCH_PRIMITIVE_REF(null_p));
  dfsch_defconst_cstr(ctx, "atom?", DFSCH_PRIMITIVE_REF(atom_p));
  dfsch_defconst_cstr(ctx, "pair?", DFSCH_PRIMITIVE_REF(pair_p));
  dfsch_defconst_cstr(ctx, "list?", DFSCH_PRIMITIVE_REF(list_p));
  dfsch_defconst_cstr(ctx, "symbol?", DFSCH_PRIMITIVE_REF(symbol_p));
  dfsch_defconst_cstr(ctx, "string?", DFSCH_PRIMITIVE_REF(string_p));
  dfsch_defconst_cstr(ctx, "primitive?", 
                      DFSCH_PRIMITIVE_REF(primitive_p));
  dfsch_defconst_cstr(ctx, "function?", DFSCH_PRIMITIVE_REF(function_p));
  dfsch_defconst_cstr(ctx, "procedure?", 
                      DFSCH_PRIMITIVE_REF(procedure_p));
  dfsch_defconst_cstr(ctx, "macro?", DFSCH_PRIMITIVE_REF(macro_p));
  dfsch_defconst_cstr(ctx, "form?", DFSCH_PRIMITIVE_REF(form_p));
  dfsch_defconst_cstr(ctx, "vector?", DFSCH_PRIMITIVE_REF(vector_p));


  dfsch_defconst_cstr(ctx, "true", DFSCH_SYM_TRUE);
  dfsch_defconst_cstr(ctx, "nil", NULL);
  dfsch_defconst_cstr(ctx, "else", DFSCH_SYM_TRUE);
  dfsch_defconst_cstr(ctx, "t", DFSCH_SYM_TRUE);  
  dfsch_defconst_cstr(ctx, "T", DFSCH_SYM_TRUE);


  dfsch_defconst_cstr(ctx, "make-vector", 
                      DFSCH_PRIMITIVE_REF(make_vector));
  dfsch_defconst_cstr(ctx, "vector", 
                      DFSCH_PRIMITIVE_REF(vector));
  dfsch_defconst_cstr(ctx, "vector-length", 
                      DFSCH_PRIMITIVE_REF(vector_length));
  dfsch_defconst_cstr(ctx, "vector-set!", 
                      DFSCH_PRIMITIVE_REF(vector_set));
  dfsch_defconst_cstr(ctx, "vector-ref", 
                      DFSCH_PRIMITIVE_REF(vector_ref));
  dfsch_defconst_cstr(ctx, "vector->list", 
                      DFSCH_PRIMITIVE_REF(vector_2_list));
  dfsch_defconst_cstr(ctx, "list->vector", 
                      DFSCH_PRIMITIVE_REF(list_2_vector));

  dfsch_defconst_cstr(ctx, "object->string", 
                      DFSCH_PRIMITIVE_REF(object_2_string));
  dfsch_defconst_cstr(ctx, "string->object", 
                      DFSCH_PRIMITIVE_REF(string_2_object));
  dfsch_defconst_cstr(ctx, "string->object-list", 
                      DFSCH_PRIMITIVE_REF(string_2_object_list));
  dfsch_defconst_cstr(ctx, "dfsch%write-object", 
                      DFSCH_PRIMITIVE_REF(write__object));
  dfsch_defconst_cstr(ctx, "dfsch%write-string", 
                      DFSCH_PRIMITIVE_REF(write__string));

  dfsch_defconst_cstr(ctx, "symbol->string", 
                      DFSCH_PRIMITIVE_REF(symbol_2_string));
  dfsch_defconst_cstr(ctx, "string->symbol", 
                      DFSCH_PRIMITIVE_REF(string_2_symbol));
  dfsch_defconst_cstr(ctx, "intern-symbol", 
                      DFSCH_PRIMITIVE_REF(intern_symbol));

  dfsch_defconst_cstr(ctx, "macro-expand", 
                      DFSCH_PRIMITIVE_REF(macro_expand));

  dfsch_defconst_cstr(ctx, "slot-ref", DFSCH_PRIMITIVE_REF(slot_ref));
  dfsch_defconst_cstr(ctx, "slot-set!", DFSCH_PRIMITIVE_REF(slot_set));
  dfsch_defconst_cstr(ctx, "get-slots", DFSCH_PRIMITIVE_REF(get_slots));
  dfsch_defconst_cstr(ctx, "find-slot", DFSCH_PRIMITIVE_REF(find_slot));
  dfsch_defconst_cstr(ctx, "make-slot-accessor", 
                      DFSCH_PRIMITIVE_REF(make_slot_accessor));
  dfsch_defconst_cstr(ctx, "make-slot-reader", 
                      DFSCH_PRIMITIVE_REF(make_slot_reader));
  dfsch_defconst_cstr(ctx, "make-slot-writer", 
                      DFSCH_PRIMITIVE_REF(make_slot_writer));

  dfsch_defconst_cstr(ctx, "throw", DFSCH_PRIMITIVE_REF(throw));

  dfsch_defconst_cstr(ctx, "eval", DFSCH_PRIMITIVE_REF(eval));
  dfsch_defconst_cstr(ctx, "eval-proc", DFSCH_PRIMITIVE_REF(eval_proc));
  dfsch_defconst_cstr(ctx, "apply", DFSCH_PRIMITIVE_REF(apply));


}
