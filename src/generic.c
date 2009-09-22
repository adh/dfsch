/*
 * dfsch - Scheme-like Lisp dialect
 *   Generic functions
 * Copyright (C) 2005-2009 Ales Hakl
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

#include <dfsch/generic.h>

typedef struct standard_generic_function_t {
} standard_generic_function_t;

typedef struct singleton_generic_function_t {
  dfsch_type_t* type;
  dfsch_type_apply_t apply;
  dfsch_generic_function_add_method_t add_method;
  dfsch_generic_function_remove_method_t remove_method;
  dfsch_generic_function_methods_t methods;
} singleton_generic_function_t;

typedef struct method_t {
  dfsch_type_t* type;
  dfsch_object_t* name;
  dfsch_object_t* qualifiers;
  dfsch_object_t* specializers;
  dfsch_object_t* function;
} method_t;


dfsch_type_t dfsch_generic_function_type_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name = "generic-function-type",
  .size = sizeof(dfsch_generic_function_type_t),
};

dfsch_type_t dfsch_generic_function_type = {
  .type = DFSCH_ABSTRACT_TYPE,
  .superclass = DFSCH_FUNCTION_TYPE,
  .name = "generic-function",
  .documentation = ""
};

static dfsch_object_t* 
apply_standard_generic_function(standard_generic_function_t* function,
                                dfsch_object_t* arguments,
                                dfsch_tail_escape_t* esc,
                                dfsch_object_t* context){
  
}
static void 
standard_generic_function_add_method(standard_generic_function_t* function,
                                     dfsch_object_t* method){
  
}
static void 
standard_generic_function_remove_method(standard_generic_function_t* function,
                                        dfsch_object_t* method){
  
}
static dfsch_object_t* 
standard_generic_function_methods(standard_generic_function_t* function){
  
}

dfsch_generic_function_type_t dfsch_standard_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "standard-generic-function",
    .size = sizeof(standard_generic_function_t),
    .documentation = "Normal class of generic functions",
    .apply = apply_standard_generic_function,
  },

  .add_method = standard_generic_function_add_method,
  .remove_method = standard_generic_function_remove_method,
  .methods = standard_generic_function_methods
};



static dfsch_object_t* 
apply_singleton_generic_function(singleton_generic_function_t* function,
                                dfsch_object_t* arguments,
                                dfsch_tail_escape_t* esc,
                                dfsch_object_t* context){
  return function->apply(function, arguments, esc, context);
}
static void 
singleton_generic_function_add_method(singleton_generic_function_t* function,
                                      dfsch_object_t* method){
  function->add_method(function, method);
}
static void 
singleton_generic_function_remove_method(singleton_generic_function_t* function,
                                         dfsch_object_t* method){
  function->remove_method(function, method);
}
static dfsch_object_t* 
singleton_generic_function_methods(singleton_generic_function_t* function){
  return function->methods(function);
}

dfsch_generic_function_type_t dfsch_singleton_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "singleton-generic-function",
    .size = 0,
    .documentation = "Class of generic functions whose behavior is unique."
    "Used to implement internal special cases in interpreter.",
    .apply = apply_singleton_generic_function
  },

  .add_method = singleton_generic_function_add_method,
  .remove_method = singleton_generic_function_remove_method,
  .methods = singleton_generic_function_methods
};





dfsch_type_t dfsch_method_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "method",
  .size = sizeof(method_t)
};

dfsch_object_t* dfsch_make_method(dfsch_object_t* name,
                                  dfsch_object_t* qualifiers,
                                  dfsch_object_t* specializers,
                                  dfsch_object_t* function){
  method_t* m = (method_t*)dfsch_make_object(DFSCH_METHOD_TYPE);
  m->name = name;
  m->qualifiers = qualifiers;
  m->specializers = specializers;
  m->function = m->function;
  return (dfsch_object_t*)m;
}

dfsch_object_t* dfsch_parse_specialized_lambda_list(dfsch_object_t* s_l_l,
                                                    dfsch_object_t** l_l,
                                                    dfsch_object_t** spec){
  dfsch_object_t* specializers_head;
  dfsch_object_t* specializers_tail;
  dfsch_object_t* lambda_list_head;
  dfsch_object_t* lambda_list_tail;
  dfsch_object_t* i = s_l_l;

  while (DFSCH_PAIR_P(i) && DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
    dfsch_object_t* j = DFSCH_FAST_CAR(i);
    dfsch_object_t* tmp;
    dfsch_object_t* var;
    dfsch_object_t* specializer;

    DFSCH_OBJECT_ARG(j, var);
    DFSCH_OBJECT_ARG(j, specializer);
    DFSCH_ARG_END(j);

    tmp = dfsch_cons(var, NULL);
    if (lambda_list_tail){
      DFSCH_FAST_CDR_MUT(lambda_list_tail) = tmp;
      lambda_list_tail = tmp;
    } else {
      lambda_list_tail = lambda_list_head = tmp;
    }

    tmp = dfsch_cons(specializer, NULL);
    if (specializers_tail){
      DFSCH_FAST_CDR_MUT(specializers_tail) = tmp;
      specializers_tail = tmp;
    } else {
      specializers_tail = specializers_head = tmp;
    }
    i = DFSCH_FAST_CDR(i);
  }
  
  if (lambda_list_tail){
    DFSCH_FAST_CDR_MUT(lambda_list_tail) = i;
  } else {
    lambda_list_head = i;
  }

  *l_l = lambda_list_head;
  *spec = specializers_head;
}
