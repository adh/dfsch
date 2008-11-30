/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Generic functions
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

#include "dfsch/generic.h"
#include <dfsch/hash.h>

typedef struct generic_t {
  dfsch_type_t* type;
  dfsch_object_t* cache;
  dfsch_object_t* methods;
  char* name;
} generic_t;

static dfsch_object_t* generic_find_method(generic_t* gen,
                                           dfsch_type_t* type){
  dfsch_object_t* method;
  dfsch_type_t* i = type;

  while (i){
    method = dfsch_hash_ref(gen->methods, (dfsch_object_t*)i);
    if (method){
      return dfsch_car(method);
    }
    i = i->superclass;
  }

  method = dfsch_hash_ref(gen->methods, NULL);
  if (!method){
    dfsch_error("No matching method", dfsch_list(2,
						 (dfsch_object_t*)gen,
						 (dfsch_object_t*)type));
  }
  return dfsch_car(method);
}

static dfsch_object_t* generic_apply(generic_t* gen,
                                     dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  dfsch_object_t* method;
  dfsch_type_t* type;

  if (!dfsch_pair_p(args)){
    dfsch_error("Discriminating argument missing", 
                (dfsch_object_t*)gen);
  }

  type = dfsch_type_of(dfsch_car(args));

  method = dfsch_hash_ref(gen->cache, type);

  if (method){
    method = dfsch_car(method);
  } else {
    method = generic_find_method(gen, type);
    dfsch_hash_set(gen->cache, type, method);
  }

  return dfsch_apply_tr(method, args, esc);
}

static dfsch_type_t generic_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_FUNCTION_TYPE,
  sizeof(generic_t),
  "generic-function",
  NULL,
  NULL,
  (dfsch_type_apply_t)generic_apply,
  NULL
};

dfsch_object_t* dfsch_make_generic(char* name){
  generic_t* gen = dfsch_make_object(&generic_type);

  gen->cache = dfsch_hash_make(DFSCH_HASH_EQ);
  gen->methods = dfsch_hash_make(DFSCH_HASH_EQ);
  gen->name = name;

  return (dfsch_object_t*) gen;
}

void dfsch_method_set(dfsch_object_t* generic,
                      dfsch_object_t* type,
                      dfsch_object_t* method){
  if (!DFSCH_INSTANCE_P(generic, &generic_type)){
    dfsch_error("Not a generic function", generic);
  }

  dfsch_hash_set(((generic_t*)generic)->methods, type, method);
  ((generic_t*)generic)->cache = dfsch_hash_make(DFSCH_HASH_EQ);
}
void dfsch_method_unset(dfsch_object_t* generic,
                        dfsch_object_t* type){
  if (!DFSCH_INSTANCE_P(generic, &generic_type)){
    dfsch_error("Not a generic function", generic);
  }

  dfsch_hash_unset(((generic_t*)generic)->methods, type);
  ((generic_t*)generic)->cache = dfsch_hash_make(DFSCH_HASH_EQ);

}
dfsch_object_t* dfsch_method_ref(dfsch_object_t* generic,
                      dfsch_object_t* type){
  if (!DFSCH_INSTANCE_P(generic, &generic_type)){
    dfsch_error("Not a generic function", generic);
  }

  return generic_find_method((generic_t*)generic, type);
}
dfsch_object_t* dfsch_methods_2_alist(dfsch_object_t* generic){
  if (!DFSCH_INSTANCE_P(generic, &generic_type)){
    dfsch_error("Not a generic function", generic);
  }

  return dfsch_hash_2_alist(((generic_t*)generic)->methods);
}

dfsch_object_t* dfsch_define_generic(dfsch_object_t* name, dfsch_object_t* env){
  dfsch_object_t* generic;

  if (!dfsch_symbol_p(name)){
    dfsch_error("Not a symbol", name);
  }
    
  generic = dfsch_env_get(name, env);
  
  if (generic){
    generic = dfsch_car(generic);
    if (!DFSCH_INSTANCE_P(generic, &generic_type)){
      dfsch_error("Not a generic function", generic);
    }
    return generic;
  }

  generic = dfsch_make_generic(dfsch_symbol(name));
  dfsch_define(name, generic, env);
  return generic;
}

dfsch_object_t* dfsch_define_generic_cstr(char* name, dfsch_object_t* env){
  return dfsch_define_generic(dfsch_make_symbol(name), env);
}
void dfsch_define_method_cstr(char* name, 
                              dfsch_object_t* type,
                              dfsch_object_t* method,
                              dfsch_object_t* env){
  dfsch_method_set(dfsch_define_generic_cstr(name, env),
                   type,
                   method);
}


/* scheme binding */
DFSCH_DEFINE_FORM_IMPL(define_generic){
  dfsch_object_t* name;

  DFSCH_OBJECT_ARG(args, name);
  
  return dfsch_define_generic(name, env);
}

DFSCH_DEFINE_FORM_IMPL(define_method){
  dfsch_object_t* name;
  dfsch_object_t* type;
  dfsch_object_t* method;
  dfsch_object_t* generic;
  dfsch_object_t* lambda_list;

  DFSCH_OBJECT_ARG(args, name);
  
  if (dfsch_pair_p(name)){
    lambda_list = dfsch_cdr(name);
    name = dfsch_car(name);
    type = dfsch_car(lambda_list);

    generic = dfsch_define_generic(name, env);

    if (dfsch_pair_p(type)){
      lambda_list = dfsch_cons(dfsch_car(type), dfsch_cdr(lambda_list));
      type = dfsch_eval(dfsch_car(dfsch_cdr(type)), env);
      method = dfsch_named_lambda(env, 
                                  lambda_list,
                                  args,
                                  dfsch_list(2,
                                             generic,
                                             type));
    } else {
      type = NULL;
      method = dfsch_named_lambda(env, 
                                  lambda_list,
                                  args,
                                  dfsch_list(2,
                                             generic,
                                             type));
    }
    
  } else {
    DFSCH_OBJECT_ARG(args, type);
    DFSCH_OBJECT_ARG(args, method);
    method = dfsch_eval(method, env);
    generic = dfsch_define_generic(name, env);
    type = dfsch_eval(type, env);
  }


  dfsch_method_set(generic, type, method);
  return method;
}

static dfsch_object_t* make_generic(void* baton,
                                    dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* name;
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_make_generic(name);
}
static dfsch_object_t* method_set(void* baton,
                                  dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  dfsch_object_t* generic;
  dfsch_object_t* type;
  dfsch_object_t* method;
  
  DFSCH_OBJECT_ARG(args, generic);
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_OBJECT_ARG(args, method);
  DFSCH_ARG_END(args);

  dfsch_method_set(generic, type, method);

  return NULL;
}
static dfsch_object_t* method_unset(void* baton,
                                    dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  dfsch_object_t* generic;
  dfsch_object_t* type;
  
  DFSCH_OBJECT_ARG(args, generic);
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_ARG_END(args);

  dfsch_method_unset(generic, type);

  return NULL;
}
static dfsch_object_t* method_ref(void* baton,
                                  dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  dfsch_object_t* generic;
  dfsch_object_t* type;
  
  DFSCH_OBJECT_ARG(args, generic);
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_ARG_END(args);

  return dfsch_method_ref(generic, type);
}

static dfsch_object_t* methods_2_alist(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* generic;
  
  DFSCH_OBJECT_ARG(args, generic);
  DFSCH_ARG_END(args);

  return dfsch_methods_2_alist(generic);
}




void dfsch__generic_register(dfsch_object_t* env){
  dfsch_define_cstr(env, "<generic-function>", &generic_type);
  dfsch_define_cstr(env, "define-generic", DFSCH_FORM_REF(define_generic));
  dfsch_define_cstr(env, "define-generic-method", 
                    DFSCH_FORM_REF(define_method));

  dfsch_define_cstr(env, "make-generic", 
                    dfsch_make_primitive(make_generic, NULL));
  dfsch_define_cstr(env, "method-set!", 
                    dfsch_make_primitive(method_set, NULL));
  dfsch_define_cstr(env, "method-unset!", 
                    dfsch_make_primitive(method_unset, NULL));
  dfsch_define_cstr(env, "method-ref", 
                    dfsch_make_primitive(method_ref, NULL));
  dfsch_define_cstr(env, "methods->alist", 
                    dfsch_make_primitive(methods_2_alist, NULL));
}
