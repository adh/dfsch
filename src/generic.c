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
#include <dfsch/magic.h>
#include <dfsch/mkhash.h>

typedef struct standard_generic_function_t {
  dfsch_type_t* type;
  dfsch_mkhash_t* dispatch_cache;
  dfsch_object_t* methods;
  size_t longest_spec_list;
  dfsch_object_t* name;
  dfsch_object_t* method_combination;
} standard_generic_function_t;

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

/*
 * Normal generic functions
 */

static int more_specific_method_p(dfsch_method_t* a,
                                  dfsch_method_t* b){
  dfsch_object_t* ai = a->specializers;
  dfsch_object_t* bi = b->specializers;

  while (DFSCH_PAIR_P(ai) && DFSCH_PAIR_P(bi)){
    dfsch_object_t* as = DFSCH_FAST_CAR(ai);
    dfsch_object_t* bs = DFSCH_FAST_CAR(bi);
    
    if (as != bs){
      if (dfsch_superclass_p(as, bs)){
        return 0;
      } else {
        return 1;
      }
    }
    
    ai = DFSCH_FAST_CDR(ai);
    bi = DFSCH_FAST_CDR(bi);
  }

  if (bi) {
    return 1;
  } else {
    return 0;
  }
}

/*
 * Linked list mergesort as described by Simon Tatham at
 * http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
 */
static dfsch_object_t* sort_methods(dfsch_object_t* list){
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
        } else if (!more_specific_method_p(DFSCH_FAST_CAR(q), 
                                           DFSCH_FAST_CAR(p))){
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


static int method_applicable_p(dfsch_method_t* method,
                               dfsch_object_t* arguments){
  dfsch_object_t* ai = arguments;
  dfsch_object_t* si = method->specializers;

  while (DFSCH_PAIR_P(ai) && DFSCH_PAIR_P(si)){
    if (!DFSCH_INSTANCE_P(DFSCH_FAST_CAR(ai),
                          DFSCH_FAST_CAR(si))){
      return 0;
    }

    ai = DFSCH_FAST_CDR(ai);
    si = DFSCH_FAST_CDR(si);
  }

  if (si){
    return 0;
  }

  return 1;
}

static dfsch_object_t* compute_applicable_methods(standard_generic_function_t* gf,
                                                  dfsch_object_t* arguments){
  dfsch_object_t* i = gf->methods;
  dfsch_object_t* applicable = NULL;

  while (DFSCH_PAIR_P(i)){
    if (method_applicable_p(DFSCH_FAST_CAR(i), arguments)){
      applicable = dfsch_cons(DFSCH_FAST_CAR(i), applicable);
    }
    i = DFSCH_FAST_CDR(i);
  }
  
  return sort_methods(applicable);
}


typedef struct method_context_t {
  dfsch_type_t* type;

  dfsch_object_t* next_methods;
  dfsch_object_t* args;
} method_context_t;

static dfsch_object_t* cons_method_context(dfsch_object_t* next_methods,
                                           dfsch_object_t* args){
  method_context_t* mc = dfsch_make_object(DFSCH_STANDARD_METHOD_CONTEXT_TYPE);
  
  mc->next_methods = next_methods;
  mc->args = args;

  return mc;
}

dfsch_object_t* dfsch_call_method(dfsch_method_t* method,
                                  dfsch_object_t* next_methods,
                                  dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  return dfsch_apply_with_context(method->function, args,
                                  cons_method_context(next_methods, args), 
                                  esc);
}
dfsch_object_t* dfsch_call_one_method(dfsch_method_t* method,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  return dfsch_apply_tr(method->function, args, esc);
}

static void call_methods_sequentially(dfsch_object_t* methods,
                                      dfsch_object_t* args){
  while (DFSCH_PAIR_P(methods)){
    dfsch_call_one_method(DFSCH_FAST_CAR(methods), args, NULL);
    methods = DFSCH_FAST_CDR(methods);
  }
}


static dfsch_object_t* call_next_method(method_context_t* ctx,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc){
  if (!args){
    args = ctx->args;
  }

  if (!ctx->next_methods){
    dfsch_error("No next method", NULL);
  }

  return dfsch_call_method(DFSCH_FAST_CAR(ctx->next_methods), 
                           DFSCH_FAST_CDR(ctx->next_methods), 
                           args, esc);
}

dfsch_method_context_type_t dfsch_standard_method_context_type = {
  .super = {
    .type = DFSCH_METHOD_CONTEXT_TYPE_TYPE,
    .superclass = DFSCH_METHOD_CONTEXT_TYPE,
    .name = "standard-method-context",
    .size = sizeof(method_context_t)
  },
  .call_next_method = call_next_method,
};

typedef struct effective_method_t {
  dfsch_type_t* type;

  dfsch_object_t* primary_methods;
  dfsch_object_t* before_methods;
  dfsch_object_t* after_methods;
  dfsch_object_t* around_methods;

  dfsch_object_t* genfun;
} effective_method_t;

static dfsch_object_t* standard_mc_core(effective_method_t* em,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc){
  if (em->before_methods){
    call_methods_sequentially(em->before_methods, args);
  }
  
  if (!em->after_methods) {
    if (em->primary_methods){
      return dfsch_call_method(DFSCH_FAST_CAR(em->primary_methods), 
                               DFSCH_FAST_CDR(em->primary_methods), 
                               args, esc);
    }
  } else {
    dfsch_object_t* r; 
    if (em->primary_methods) {
      r = dfsch_call_method(DFSCH_FAST_CAR(em->primary_methods), 
                            DFSCH_FAST_CDR(em->primary_methods), 
                            args, NULL);
    }
    call_methods_sequentially(em->after_methods, args);
    return r;
  }
}

DFSCH_PRIMITIVE_HEAD(around_method_stub_proc){
  effective_method_t* em = baton;
  return standard_mc_core(em, args, esc);
}

static dfsch_object_t* apply_effective_method(effective_method_t* em,
                                              dfsch_object_t* args,
                                              dfsch_tail_escape_t* esc,
                                              dfsch_object_t* ctx){
  if (DFSCH_LIKELY(!em->around_methods)){
    return standard_mc_core(em, args, esc);
  } else {
    return dfsch_call_method(DFSCH_FAST_CAR(em->around_methods), 
                             DFSCH_FAST_CDR(em->around_methods), 
                             args, esc);
  }
}

dfsch_type_t dfsch_standard_effective_method_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = DFSCH_FUNCTION_TYPE,
  .size = sizeof(effective_method_t),
  .name = "standard-effective-method",
  
  .apply = apply_effective_method
};

static dfsch_object_t* get_primary_methods(dfsch_object_t* methods){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;

  while (DFSCH_PAIR_P(methods)){
    dfsch_method_t* method = DFSCH_FAST_CAR(methods);

    if (!method->qualifiers){
      dfsch_object_t* tmp = dfsch_cons(method, NULL);
      if (head){
        DFSCH_FAST_CDR_MUT(tail) = tmp;
        tail = tmp;
      } else {
        tail = head = tmp;
      }
    }
    methods = DFSCH_FAST_CDR(methods);
  }

  return head;
}
static dfsch_object_t* get_qualified_methods(dfsch_object_t* methods,
                                             dfsch_object_t* qualifier){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;

  while (DFSCH_PAIR_P(methods)){
    dfsch_method_t* method = DFSCH_FAST_CAR(methods);

    if (DFSCH_PAIR_P(method->qualifiers) &&
        DFSCH_FAST_CAR(method->qualifiers) == qualifier &&
        DFSCH_FAST_CDR(method->qualifiers) == NULL){
      dfsch_object_t* tmp = dfsch_cons(method, NULL);
      if (head){
        DFSCH_FAST_CDR_MUT(tail) = tmp;
        tail = tmp;
      } else {
        tail = head = tmp;
      }
    }
    methods = DFSCH_FAST_CDR(methods);
  }

  return head;
}

static effective_method_t* make_effective_method(dfsch_object_t* methods,
                                                 dfsch_object_t* genfun){
  effective_method_t* em = 
    dfsch_make_object(DFSCH_STANDARD_EFFECTIVE_METHOD_TYPE);

  em->primary_methods = get_primary_methods(methods);
  em->after_methods = dfsch_reverse(get_qualified_methods(methods, 
                                                          DFSCH_QUAL_AFTER));
  em->before_methods = get_qualified_methods(methods, DFSCH_QUAL_BEFORE);
  em->around_methods = get_qualified_methods(methods, DFSCH_QUAL_AROUND);

  if (em->around_methods){
    dfsch_object_t* i;
    dfsch_method_t* stub_method = 
      dfsch_make_method(NULL,
                        NULL,
                        NULL,
                        DFSCH_PRIMITIVE_REF_MAKE(around_method_stub_proc,
                                                 em));
    i = em->around_methods;
    while (DFSCH_PAIR_P(DFSCH_FAST_CDR(i))){
      i = DFSCH_FAST_CDR(i);
    }
    DFSCH_FAST_CDR_MUT(i) = dfsch_cons(stub_method, NULL);
  }

  em->genfun = genfun;

  return em;
}



static dfsch_object_t* 
apply_standard_generic_function(standard_generic_function_t* function,
                                dfsch_object_t* arguments,
                                dfsch_tail_escape_t* esc,
                                dfsch_object_t* context){
  dfsch_object_t* meths;
  effective_method_t* em;
  dfsch_object_t* cache_keys[function->longest_spec_list];
  size_t i = 0;
  dfsch_object_t* j = arguments;

  while (i < function->longest_spec_list && DFSCH_PAIR_P(j)){
    cache_keys[i] = DFSCH_TYPE_OF(DFSCH_FAST_CAR(j));
    //    printf(";; [%d] = %s\n", i, dfsch_object_2_string(cache_keys[i], 100, 100));
    i++;
    j = DFSCH_FAST_CDR(j);
  }
  while (i < function->longest_spec_list){
    cache_keys[i] = DFSCH_INVALID_OBJECT;
    i++;
  }

  if (!dfsch_mkhash_ref(function->dispatch_cache, cache_keys, &em)){
    printf(";; cache miss\n");
    meths = compute_applicable_methods(function, arguments);
      
    if (!meths){
      dfsch_error("No applicable methods", dfsch_list(2, function, arguments));
    }
    
    em = make_effective_method(meths, function);
    dfsch_mkhash_set(function->dispatch_cache, cache_keys, em);
  }
  return apply_effective_method(em, arguments, esc, context);
  
}
static void write_standard_generic_function(standard_generic_function_t* gf,
                                            dfsch_writer_state_t* ws){
  dfsch_write_unreadable_start(ws, (dfsch_object_t*) gf);
  dfsch_write_object(ws, gf->name);    
  dfsch_write_unreadable_end(ws);
}

static void 
standard_generic_function_add_method(standard_generic_function_t* function,
                                     dfsch_method_t* method){
  size_t spec_length = dfsch_list_length_check(method->specializers);
  dfsch_object_t* i = function->methods;

  if (spec_length > function->longest_spec_list){
    function->longest_spec_list = spec_length;
  }
  
  while (i){
    dfsch_method_t* m = DFSCH_FAST_CAR(i);
    if (dfsch_equal_p(method->specializers, m->specializers) &&
        dfsch_equal_p(method->qualifiers, m->qualifiers)){
      DFSCH_FAST_CAR(i) = method;
      return;
    }

    i = DFSCH_FAST_CDR(i);
  }

  function->methods = dfsch_cons(method, function->methods);
  dfsch_mkhash_reset(function->dispatch_cache, function->longest_spec_list, 0);
}

static void 
standard_generic_function_remove_method(standard_generic_function_t* function,
                                        dfsch_method_t* method){
  dfsch_object_t* i;
  dfsch_object_t* j;

  if (!function->methods){
    dfsch_error("No such method for generic function", 
                dfsch_list(2, function, method));
  }
  
  if (DFSCH_FAST_CAR(function->methods) == method){
    function->methods = DFSCH_FAST_CDR(function->methods);
    dfsch_mkhash_reset(function->dispatch_cache, function->longest_spec_list, 0);
    return;
  }

  j = function->methods;
  i = DFSCH_FAST_CDR(function->methods);
  while (i){
    if (DFSCH_FAST_CAR(i) == method){
      DFSCH_FAST_CDR_MUT(j) = DFSCH_FAST_CDR(i);
      dfsch_mkhash_reset(function->dispatch_cache, function->longest_spec_list, 0);
      return;
    }
    j = i;
    i = DFSCH_FAST_CDR(i);
  }

  dfsch_error("No such method for generic function", 
              dfsch_list(2, function, method));
}

static dfsch_object_t* 
standard_generic_function_methods(standard_generic_function_t* function){
  return dfsch_list_copy(function->methods);
}

dfsch_generic_function_type_t dfsch_standard_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "standard-generic-function",
    .size = sizeof(standard_generic_function_t),
    .documentation = "Normal class of generic functions",
    .apply = apply_standard_generic_function,
    .write = write_standard_generic_function
  },

  .add_method = standard_generic_function_add_method,
  .remove_method = standard_generic_function_remove_method,
  .methods = standard_generic_function_methods
};

/*
 * Singleton generic functions
 *
 * This is intended as thin glue layer that presents various hardcoded methods
 * of internal supporting object structures (C methods of standard-type, 
 * ports, or even generic-function-type) to user as generic functions that can 
 * be extended by means of define-method.
 */

typedef dfsch_singleton_generic_function_t singleton_gf_t;

static dfsch_object_t* 
apply_singleton_generic_function(singleton_gf_t* function,
                                 dfsch_object_t* arguments,
                                 dfsch_tail_escape_t* esc,
                                 dfsch_object_t* context){
  return function->apply(function, arguments, esc, context);
}
static void 
singleton_generic_function_add_method(singleton_gf_t* function,
                                      dfsch_object_t* method){
  if (function->add_method){
    dfsch_error("Methods cannot be added to this generic function", 
                function);
  }
  function->add_method(function, method);
}
static void 
singleton_generic_function_remove_method(singleton_gf_t* function,
                                         dfsch_object_t* method){
  if (function->remove_method){
    dfsch_error("Methods cannot be removed from this generic function", 
                function);
  }
  function->remove_method(function, method);
}
static dfsch_object_t* 
singleton_generic_function_methods(singleton_gf_t* function){
  if (!function->methods){
    return NULL;
  }
  return function->methods(function);
}

dfsch_generic_function_type_t dfsch_singleton_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "singleton-generic-function",
    .size = 0,
    .documentation = "Class of generic functions whose behavior is unique. "
    "Used to implement internal special cases in interpreter.",
    .apply = apply_singleton_generic_function
  },

  .add_method = singleton_generic_function_add_method,
  .remove_method = singleton_generic_function_remove_method,
  .methods = singleton_generic_function_methods
};

dfsch_generic_function_t* dfsch_assert_generic_function(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!DFSCH_INSTANCE_P(DFSCH_TYPE_OF(o), DFSCH_GENERIC_FUNCTION_TYPE_TYPE)){
    DFSCH_WITH_RETRY_WITH_RESTART(dfsch_make_symbol("use-value"), 
                                  "Retry with alternate value") {
      dfsch_error("Not a generic funtion object", obj);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return (dfsch_generic_function_t*)o;
}


dfsch_object_t* dfsch_make_generic_function(dfsch_object_t* name){
  standard_generic_function_t* gf = (standard_generic_function_t*)
    dfsch_make_object(DFSCH_STANDARD_GENERIC_FUNCTION_TYPE);

  gf->name = name;
  gf->methods = NULL;
  gf->dispatch_cache = dfsch_make_mkhash(0, 0);

  return (dfsch_object_t*)gf;
}

void dfsch_generic_function_add_method(dfsch_object_t* function,
                                       dfsch_object_t* method){
  dfsch_generic_function_t* f = dfsch_assert_generic_function(function);
  f->type->add_method(f, method);
}
void dfsch_generic_function_remove_method(dfsch_object_t* function,
                                          dfsch_object_t* method){
  dfsch_generic_function_t* f = dfsch_assert_generic_function(function);
  f->type->remove_method(f, method);
}
dfsch_object_t* dfsch_generic_function_methods(dfsch_object_t* function){
  dfsch_generic_function_t* f = dfsch_assert_generic_function(function);
  return f->type->methods(f);
}


/*
 * Methods
 * 
 * Method objects are only simple containers without any complex associated 
 * logic. Most reasons why one would want to extend method metaobjects in CLOS
 * are handled by other means or simply do not make sense in dfsch.
 */

static void method_write(dfsch_method_t* m,
                         dfsch_writer_state_t* ws){
  dfsch_write_unreadable_start(ws, (dfsch_object_t*) m);
  dfsch_write_object(ws, m->name);    
  dfsch_write_unreadable_end(ws);
}

dfsch_slot_t method_slots[] = {
  DFSCH_OBJECT_SLOT(dfsch_method_t, name, DFSCH_SLOT_ACCESS_RO,
                    "Method name (i.e. first argument of define-method)"),
  DFSCH_OBJECT_SLOT(dfsch_method_t, qualifiers, DFSCH_SLOT_ACCESS_RO,
                    "Method qualifier list"),
  DFSCH_OBJECT_SLOT(dfsch_method_t, specializers, DFSCH_SLOT_ACCESS_RO,
                    "Specializers of method"),
  DFSCH_OBJECT_SLOT(dfsch_method_t, function, DFSCH_SLOT_ACCESS_RO,
                    "Implementing function"),  
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_method_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "method",
  .size = sizeof(dfsch_method_t),
  .write = method_write,
  .slots = method_slots
};


dfsch_method_t* dfsch_make_method(dfsch_object_t* name,
                                  dfsch_object_t* qualifiers,
                                  dfsch_object_t* specializers,
                                  dfsch_object_t* function){
  dfsch_method_t* m = (dfsch_method_t*)dfsch_make_object(DFSCH_METHOD_TYPE);
  m->name = name;
  m->qualifiers = qualifiers;
  m->specializers = specializers;
  m->function = function;
  return (dfsch_object_t*)m;
}

/*
 * Specialized arguments must be mandatory and consecutive from first argument.
 * This simplifies parsing of specialized lambda-lists significantly, because
 * anything that is not list (because it is symbol) marks end of interesting
 * part of lambda list and is simply passed through (probably to 
 * dfsch_compile_lambda_list())
 */

void dfsch_parse_specialized_lambda_list(dfsch_object_t* s_l_l,
                                         dfsch_object_t* env,
                                         dfsch_object_t** l_l,
                                         dfsch_object_t** spec){
  dfsch_object_t* specializers_head = NULL;
  dfsch_object_t* specializers_tail = NULL;
  dfsch_object_t* lambda_list_head = NULL;
  dfsch_object_t* lambda_list_tail = NULL;
  dfsch_object_t* i = s_l_l;

  while (DFSCH_PAIR_P(i) && DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
    dfsch_object_t* j = DFSCH_FAST_CAR(i);
    dfsch_object_t* tmp;
    dfsch_object_t* var;
    dfsch_object_t* specializer;

    DFSCH_OBJECT_ARG(j, var);
    DFSCH_OBJECT_ARG(j, specializer);
    DFSCH_ARG_END(j);

    specializer = dfsch_eval(specializer, env);
    specializer = DFSCH_ASSERT_INSTANCE(specializer, 
                                        DFSCH_STANDARD_TYPE);



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

dfsch_type_t dfsch_method_context_type_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .size = 0,
  .name = "method-context-type"
};
dfsch_type_t dfsch_method_context_type = {
  .type = DFSCH_ABSTRACT_TYPE,
  .superclass = NULL,
  .size = 0,
  .name = "method-context"  
};



dfsch_object_t* dfsch_call_next_method(dfsch_object_t* context,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_method_context_type_t* t = DFSCH_TYPE_OF(context);
  return t->call_next_method(context, args, esc);
}


DFSCH_DEFINE_PRIMITIVE(make_generic_function, ""){
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_make_generic_function(name);
}
DFSCH_DEFINE_PRIMITIVE(make_method, ""){
  dfsch_object_t* name;
  dfsch_object_t* qualifiers;
  dfsch_object_t* specializers;
  dfsch_object_t* function;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, qualifiers);
  DFSCH_OBJECT_ARG(args, specializers);
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_ARG_END(args);

  return dfsch_make_method(name, qualifiers, specializers, function);
}

static dfsch_object_t* add_method_apply(dfsch_object_t* f,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc,
                                        dfsch_object_t* context){
  dfsch_object_t* function;
  dfsch_object_t* method;
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_OBJECT_ARG(args, method);
  DFSCH_ARG_END(args);

  dfsch_generic_function_add_method(function, 
                                    DFSCH_ASSERT_INSTANCE(method, DFSCH_METHOD_TYPE));
  return NULL;
}

static dfsch_singleton_generic_function_t add_method = {
  .type = DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE,
  .apply = add_method_apply,
  
};
static dfsch_object_t* remove_method_apply(dfsch_object_t* f,
                                           dfsch_object_t* args,
                                           dfsch_tail_escape_t* esc,
                                           dfsch_object_t* context){
  dfsch_object_t* function;
  dfsch_object_t* method;
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_OBJECT_ARG(args, method);
  DFSCH_ARG_END(args);

  dfsch_generic_function_remove_method(function, 
                                       DFSCH_ASSERT_INSTANCE(method, 
                                                             DFSCH_METHOD_TYPE));
  return NULL;
}

static dfsch_singleton_generic_function_t remove_method = {
  .type = DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE,
  .apply = remove_method_apply,
  
};
static dfsch_object_t* generic_function_methods_apply(dfsch_object_t* f,
                                                      dfsch_object_t* args,
                                                      dfsch_tail_escape_t* esc,
                                                      dfsch_object_t* context){
  dfsch_object_t* function;
  dfsch_object_t* method;
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_ARG_END(args);

  return dfsch_generic_function_methods(function);
}

static dfsch_singleton_generic_function_t generic_function_methods = {
  .type = DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE,
  .apply = generic_function_methods_apply,
  
};

DFSCH_DEFINE_FORM_IMPL(call_next_method, "Call next less specialized method"){
  dfsch_object_t* ctx;
  args = dfsch_eval_list(args, env);

  ctx = dfsch_find_lexical_context(env, DFSCH_STANDARD_METHOD_CONTEXT_TYPE);

  if (!ctx){
    dfsch_error("call-next-method called outside allowed scope", NULL);
  }

  return dfsch_call_next_method(ctx, args, esc);
}

static dfsch_object_t* ensure_generic_function(dfsch_object_t* env,
                                               dfsch_object_t* name){
  dfsch_object_t* fun;
  fun = dfsch_env_get(name, env);
  if (fun != DFSCH_INVALID_OBJECT){
    if (!DFSCH_INSTANCE_P(fun, DFSCH_GENERIC_FUNCTION_TYPE)){
      dfsch_cerror("Generic function name already defined as different type",
                   name);
    } else {
      return fun;
    }
  }

  fun = dfsch_make_generic_function(name);
  dfsch_define(name, fun, env, DFSCH_VAR_CONSTANT);

  return fun;
}

DFSCH_DEFINE_FORM_IMPL(define_generic_function, "Define new generic function"){
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  

  return ensure_generic_function(env, name);
}

DFSCH_DEFINE_FORM_IMPL(define_method, "Define new generic function"){
  dfsch_object_t* header; 
  dfsch_object_t* body;
  dfsch_object_t* name;
  dfsch_object_t* qualifiers = NULL;
  dfsch_object_t* specializers;
  dfsch_object_t* lambda_list;
  dfsch_object_t* function;
  dfsch_object_t* method;

  DFSCH_OBJECT_ARG(args, header);
  DFSCH_ARG_REST(args, body);
    
  if (!DFSCH_PAIR_P(header)){
    dfsch_error("Invalid method header", header);
  }     

  name = DFSCH_FAST_CAR(header);
  lambda_list = DFSCH_FAST_CDR(header);
  
  if (DFSCH_PAIR_P(name)){
    qualifiers = DFSCH_FAST_CDR(name);
    name = DFSCH_FAST_CAR(name);
  }

  dfsch_parse_specialized_lambda_list(lambda_list, env, 
                                      &lambda_list, &specializers);
 
  
  method = dfsch_make_method(header, qualifiers, specializers, 
                             dfsch_named_lambda(env, lambda_list, 
                                                body, header));

  function = ensure_generic_function(env, name);

  dfsch_generic_function_add_method(function, method);

  return method;
}


void dfsch__generic_register(dfsch_object_t* env){
  dfsch_define_cstr(env, "make-generic-function",
                    DFSCH_PRIMITIVE_REF(make_generic_function));
  dfsch_define_cstr(env, "make-method",
                    DFSCH_PRIMITIVE_REF(make_method));

  dfsch_define_cstr(env, "add-method!", (dfsch_object_t*)&add_method);
  dfsch_define_cstr(env, "remove-method!", (dfsch_object_t*)&remove_method);
  dfsch_define_cstr(env, "generic-function-methods", 
                    (dfsch_object_t*)&generic_function_methods);

  dfsch_define_cstr(env, "call-next-method",
                    DFSCH_FORM_REF(call_next_method));

  dfsch_define_cstr(env, "define-generic-function",
                    DFSCH_FORM_REF(define_generic_function));
  dfsch_define_cstr(env, "define-method",
                    DFSCH_FORM_REF(define_method));

}
