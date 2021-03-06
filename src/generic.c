/*
 * dfsch - Scheme-like Lisp dialect
 *   Generic functions
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

#include <dfsch/generic.h>
#include <dfsch/magic.h>
#include <dfsch/mkhash.h>
#include <dfsch/generate.h>
#include <dfsch/specializers.h>
#include <dfsch/object.h>
#include "types.h"

#include <stdio.h>

//#define GENERIC_PRINT_STATS

typedef struct standard_generic_function_t {
  dfsch_type_t* type;
  dfsch_mkhash_t* dispatch_cache;
  dfsch_object_t* methods;
  size_t longest_spec_list;
  dfsch_object_t* name;
  dfsch_object_t* method_combination;
  char* documentation;
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
      if (dfsch_specializer_matches_type_p(bs, as)){
        return 0;
      } else if (DFSCH_INSTANCE_P(as, DFSCH_ROLE_TYPE) &&
                 DFSCH_INSTANCE_P(bs, DFSCH_ROLE_TYPE) &&
                 dfsch_role_inherited_p(as, bs)) {
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
    if (!dfsch_specializer_matches_type_p(DFSCH_FAST_CAR(si),
                                          DFSCH_TYPE_OF(DFSCH_FAST_CAR(ai)))){
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
  if (!esc){
    args = dfsch_list_copy_immutable(args);
  }

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

dfsch_object_t* dfsch_get_primary_methods(dfsch_object_t* methods){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;

  while (DFSCH_PAIR_P(methods)){
    dfsch_method_t* method = DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(methods),
                                                   DFSCH_METHOD_TYPE);

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
dfsch_object_t* dfsch_get_qualified_methods(dfsch_object_t* methods,
                                            dfsch_object_t* qualifier){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;

  while (DFSCH_PAIR_P(methods)){
    dfsch_method_t* method = DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(methods),
                                                   DFSCH_METHOD_TYPE);

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

  em->primary_methods = dfsch_get_primary_methods(methods);
  em->after_methods = dfsch_reverse(dfsch_get_qualified_methods(methods, 
                                                                DFSCH_QUAL_AFTER));
  em->before_methods = dfsch_get_qualified_methods(methods, DFSCH_QUAL_BEFORE);
  em->around_methods = dfsch_get_qualified_methods(methods, DFSCH_QUAL_AROUND);

  if (em->around_methods){
    dfsch_object_t* i;
    dfsch_method_t* stub_method = 
      dfsch_make_method(NULL,
                        NULL,
                        NULL,
                        DFSCH_PRIMITIVE_REF_MAKE(around_method_stub_proc,
                                                 em,
                                                 "Stub method for :around methods"));
    i = em->around_methods;
    while (DFSCH_PAIR_P(DFSCH_FAST_CDR(i))){
      i = DFSCH_FAST_CDR(i);
    }
    DFSCH_FAST_CDR_MUT(i) = dfsch_cons(stub_method, NULL);
  }

  em->genfun = genfun;

  return em;
}

//#define GENERIC_PRINT_STATS

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

#ifdef GENERIC_PRINT_STATS
  size_t gc_start = GC_get_total_bytes();
#endif

  while (i < function->longest_spec_list && DFSCH_PAIR_P(j)){
    cache_keys[i] = DFSCH_TYPE_OF(DFSCH_FAST_CAR(j));
    i++;
    j = DFSCH_FAST_CDR(j);
  }
  while (i < function->longest_spec_list){
    cache_keys[i] = DFSCH_INVALID_OBJECT;
    i++;
  }

  if (DFSCH_UNLIKELY(!dfsch_mkhash_ref(function->dispatch_cache, 
                                       cache_keys, &em))){
#ifdef GENERIC_PRINT_STATS
    fprintf(stderr, ";; Cache miss\n");
#endif

    arguments = dfsch_list_copy_immutable(arguments);
    /* Both compute-applicable-methods and method-combination logic
     * can potentially call arbitrary lisp code and thus clobber global
     * scratchpad by means of tail-call or multiple-values. So copy 
     * arguments into heap before entering slow-path dispatch code. */

    meths = compute_applicable_methods(function, arguments);
      
    if (!meths){
      dfsch_error("No applicable methods", 
                  dfsch_list(2, function, arguments));
    }
    if (function->method_combination){
      em = dfsch_apply(function->method_combination,
                       dfsch_list(2, meths, function));
    } else {
      em = make_effective_method(meths, function);
    }
    dfsch_mkhash_set(function->dispatch_cache, cache_keys, em);
  }
#ifdef GENERIC_PRINT_STATS
  fprintf(stderr, ";; dispatch finish heap_delta=%d\n", GC_get_total_bytes() - gc_start);
#endif
  return dfsch_apply_with_context(em, arguments, context, esc);
  
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
      dfsch_mkhash_reset(function->dispatch_cache, 
                         function->longest_spec_list, 0);
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

static dfsch_slot_t standard_generic_function_slots[] = {
  DFSCH_STRING_SLOT(standard_generic_function_t, documentation, 
                    DFSCH_SLOT_ACCESS_RO,
                    "Documentation string"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_generic_function_type_t dfsch_standard_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "standard-generic-function",
    .size = sizeof(standard_generic_function_t),
    .documentation = "Normal class of generic functions",
    .apply = apply_standard_generic_function,
    .write = write_standard_generic_function,
    .slots = standard_generic_function_slots,
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
  if (!function->add_method){
    dfsch_error("Methods cannot be added to this generic function", 
                function);
  }
  function->add_method(function, method);
}
static void 
singleton_generic_function_remove_method(singleton_gf_t* function,
                                         dfsch_object_t* method){
  if (!function->remove_method){
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

static dfsch_slot_t singleton_generic_function_slots[] = {
  DFSCH_STRING_SLOT(dfsch_singleton_generic_function_t, documentation, 
                    DFSCH_SLOT_ACCESS_RO,
                    "Documentation string"),
  DFSCH_SLOT_TERMINATOR
};


dfsch_generic_function_type_t dfsch_singleton_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "singleton-generic-function",
    .size = 0,
    .documentation = "Class of generic functions whose behavior is unique. "
    "Used to implement internal special cases in interpreter.",
    .apply = apply_singleton_generic_function,
    .slots = singleton_generic_function_slots,
  },

  .add_method = singleton_generic_function_add_method,
  .remove_method = singleton_generic_function_remove_method,
  .methods = singleton_generic_function_methods
};

dfsch_generic_function_t* dfsch_assert_generic_function(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!DFSCH_INSTANCE_P(DFSCH_TYPE_OF(o), DFSCH_GENERIC_FUNCTION_TYPE_TYPE)){
    DFSCH_WITH_RETRY_WITH_RESTART(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                      "use-value"), 
                                  "Retry with alternate value") {
      dfsch_error("Not a generic funtion object", obj);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return (dfsch_generic_function_t*)o;
}


dfsch_object_t* dfsch_make_generic_function(dfsch_object_t* name,
                                            dfsch_object_t* method_combination,
                                            char* documentation){
  standard_generic_function_t* gf = (standard_generic_function_t*)
    dfsch_make_object(DFSCH_STANDARD_GENERIC_FUNCTION_TYPE);

  gf->name = name;
  gf->methods = NULL;
  gf->method_combination = method_combination;
  gf->dispatch_cache = dfsch_make_mkhash(0, 0);
  gf->documentation = documentation;

  return (dfsch_object_t*)gf;
}

void dfsch_generic_function_add_method(dfsch_object_t* function,
                                       dfsch_method_t* method){
  dfsch_generic_function_t* f = dfsch_assert_generic_function(function);
  f->type->add_method(f, method);
}
void dfsch_generic_function_remove_method(dfsch_object_t* function,
                                          dfsch_method_t* method){
  dfsch_generic_function_t* f = dfsch_assert_generic_function(function);
  f->type->remove_method(f, method);
}
dfsch_object_t* dfsch_generic_function_methods(dfsch_object_t* function){
  dfsch_generic_function_t* f = dfsch_assert_generic_function(function);
  return f->type->methods(f);
}

static dfsch_object_t* ensure_generic_function(dfsch_object_t* env,
                                               dfsch_object_t* name,
                                               char* documentation){
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

  fun = dfsch_make_generic_function(name, NULL, documentation);
  dfsch_define(name, fun, env, DFSCH_VAR_CANONICAL);

  return fun;
}

void dfsch_define_method(dfsch_object_t* env,
                         dfsch_object_t* name,
                         dfsch_object_t* qualifiers,
                         dfsch_object_t* specializers,
                         dfsch_object_t* proc){
  char* documentation = NULL;
  dfsch_object_t* function;

  if (dfsch_primitive_p(proc)){
    documentation = ((dfsch_primitive_t*)proc)->documentation;
  }

  function = ensure_generic_function(env, name, documentation);
  dfsch_add_method_proc(function, qualifiers, specializers, proc);
}
void dfsch_define_method_pkgcstr(dfsch_object_t* env,
                                 dfsch_package_t* pkg,
                                 char* name,
                                 dfsch_object_t* qualifiers,
                                 dfsch_object_t* specializers,
                                 dfsch_object_t* function){
  dfsch_define_method(env,
                      dfsch_intern_symbol(pkg, name),
                      qualifiers, specializers, function);
}
void dfsch_define_method_pkgcstr_1(dfsch_object_t* env,
                                   dfsch_package_t* pkg,
                                   char* name,
                                   dfsch_object_t* specializer,
                                   dfsch_object_t* function){
  dfsch_define_method(env,
                      dfsch_intern_symbol(pkg, name),
                      NULL, 
                      dfsch_list(1, specializer), 
                      function);
}

void dfsch_add_method_proc(dfsch_object_t* gfunc,
                           dfsch_object_t* qualifiers,
                           dfsch_object_t* specializers,
                           dfsch_object_t* proc){
  dfsch_object_t* name = proc;

  dfsch_generic_function_add_method(gfunc,
                                    dfsch_make_method(name,
                                                      qualifiers,
                                                      specializers,
                                                      proc));
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

typedef struct simple_method_context_t {
  dfsch_type_t* type;
  dfsch_simple_method_callback_t cb;
  dfsch_type_t* klass;
  dfsch_object_t* args;
} simple_method_context_t;

static dfsch_object_t* simple_call_next_method(simple_method_context_t* ctx,
                                               dfsch_object_t* args,
                                               dfsch_tail_escape_t* esc){
  if (!args){
    args = ctx->args;
  }

  return ctx->cb(ctx->klass, args, esc);
}

dfsch_method_context_type_t dfsch_simple_method_context_type = {
  .super = {
    .type = DFSCH_METHOD_CONTEXT_TYPE_TYPE,
    .superclass = DFSCH_METHOD_CONTEXT_TYPE,
    .name = "simple-method-context",
    .size = sizeof(simple_method_context_t)
  },
  .call_next_method = simple_call_next_method,
};

dfsch_object_t* dfsch_make_simple_method_context(dfsch_simple_method_callback_t cb,
                                                 dfsch_type_t* klass,
                                                 dfsch_object_t* args){
  simple_method_context_t* ctx = dfsch_make_object(DFSCH_SIMPLE_METHOD_CONTEXT_TYPE);
  
  ctx->cb = cb;
  ctx->klass = klass;
  ctx->args = dfsch_list_copy_immutable(args);

  return ctx;
}



DFSCH_DEFINE_PRIMITIVE(make_generic_function, 
                       "Create new standard generic function"
                       DFSCH_DOC_SYNOPSIS("(name &key "
                                          "method-combination documentation")){
  dfsch_object_t* name;
  dfsch_object_t* method_combination = NULL;
  char* documentation = NULL;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("method-combination", method_combination);
  DFSCH_KEYWORD_GENERIC("documentation", documentation,
                        dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  return dfsch_make_generic_function(name, method_combination, documentation);
}
DFSCH_DEFINE_PRIMITIVE(make_method, "Create new method object"
                       DFSCH_DOC_SYNOPSIS("(name qualifiers "
                                          "specializers function)")){
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
  return method;
}

static dfsch_singleton_generic_function_t add_method = {
  .type = DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE,
  .apply = add_method_apply,
  .documentation = "Add method to passed generic function"  
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
  .documentation = "Remove method from passed generic function"
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
  .documentation = "Return list of all methods of this generic function"
};

DFSCH_DEFINE_PRIMITIVE(call_next_method, NULL){
  dfsch_object_t* env;
  dfsch_object_t* ctx;

  DFSCH_OBJECT_ARG(args, env);

  ctx = dfsch_find_lexical_context(env, DFSCH_METHOD_CONTEXT_TYPE);

  if (!ctx){
    dfsch_error("call-next-method called outside allowed scope", NULL);
  }

  return dfsch_call_next_method(ctx, args, esc);  
}

DFSCH_DEFINE_MACRO(call_next_method, 
                   "Call next less specialized method optionally passing "
                   "different arguments"
                   DFSCH_DOC_SYNOPSIS("(&rest args)")){
  return dfsch_immutable_list_cdr(args, 2,
                                  DFSCH_PRIMITIVE_REF(call_next_method),
                                  dfsch_generate_current_environment());
}


DFSCH_DEFINE_MACRO(define_generic_function, 
                   "Define new generic function. When generic function "
                   "already exists and no optional arguments are passed "
                   "it is not modified in any way."
                   DFSCH_DOC_SYNOPSIS("(name)\n"
                                      "(name &key method-combination documentation)")){
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, name);
  
  if (args){
    /* XXX: When arguments are present, redefine the function */

    return dfsch_generate_define_canonical_constant(name,
                                                    dfsch_immutable_list_cdr
                                              (args, 2,
                                               DFSCH_PRIMITIVE_REF(make_generic_function),
                                               dfsch_generate_quote(name)));
  }

  return dfsch_generate_if
    (dfsch_generate_defined_p(name),
     dfsch_generate_if(dfsch_generate_instance_p(name, 
                                                 DFSCH_GENERIC_FUNCTION_TYPE),
                       name,
                       dfsch_generate_error("Generic function name already "
                                            " defined as different type", 
                                            name)),
     dfsch_generate_define_canonical_constant(name,
                                              dfsch_immutable_list_cdr
                                              (args, 2,
                                               DFSCH_PRIMITIVE_REF(make_generic_function),
                                               dfsch_generate_quote(name))));
}


DFSCH_DEFINE_MACRO(define_method, "Define new method on generic function"
                   DFSCH_DOC_SYNOPSIS("((function &rest arguments) &body body)\n"
                                      "((function (argument specilizer)... &rest arguments) &body body)\n"
                                      "(((function qualifier) ...) ...)")){
  dfsch_object_t* header; 
  dfsch_object_t* body;
  dfsch_object_t* name;
  dfsch_object_t* qualifiers = NULL;
  dfsch_object_t* specializers;
  dfsch_object_t* lambda_list;
  dfsch_object_t* function;
  dfsch_object_t* method;
  dfsch_object_t* name_tag;

  DFSCH_OBJECT_ARG(args, header);
  DFSCH_ARG_REST(args, body);
    
  if (!DFSCH_PAIR_P(header)){
    dfsch_error("Invalid method header", header);
  }     

  name = DFSCH_FAST_CAR(header);
  lambda_list = DFSCH_FAST_CDR(header);
  
  name_tag = name;

  if (DFSCH_PAIR_P(name)){
    qualifiers = DFSCH_FAST_CDR(name);
    name = DFSCH_FAST_CAR(name);
  }

  dfsch_parse_specialized_lambda_list(lambda_list,
                                      &lambda_list, &specializers);

  name_tag = dfsch_cons(name_tag, specializers);

  return dfsch_immutable_list
    (3,
     (dfsch_object_t*)&add_method,
     dfsch_immutable_list(2,
                          DFSCH_MACRO_REF(define_generic_function),
                          name),
     dfsch_immutable_list(5,
                          DFSCH_PRIMITIVE_REF(make_method),
                          dfsch_generate_quote(name_tag),
                          dfsch_generate_quote(qualifiers),
                          dfsch_generate_eval_list(specializers),
                          dfsch_generate_lambda(name_tag, lambda_list, body)));
}


DFSCH_DEFINE_PRIMITIVE(call_method, "Calls method with list of next-methods"){
  dfsch_object_t* method;
  dfsch_object_t* next_methods;
  dfsch_object_t* arg_list;

  DFSCH_OBJECT_ARG(args, method);
  DFSCH_OBJECT_ARG(args, next_methods);
  DFSCH_OBJECT_ARG(args, arg_list);
  DFSCH_ARG_END(args);
  return dfsch_call_method(DFSCH_ASSERT_INSTANCE(method, DFSCH_METHOD_TYPE),
                           next_methods,
                           arg_list,
                           esc);
}

DFSCH_DEFINE_PRIMITIVE(get_primary_methods, 
                       "Filter primary methods from method list"){
  dfsch_object_t* methods;
  DFSCH_OBJECT_ARG(args, methods);
  DFSCH_ARG_END(args);
  return dfsch_get_primary_methods(methods);
}
DFSCH_DEFINE_PRIMITIVE(get_qualified_methods, 
                       "Filter qualified methods from method list"){
  dfsch_object_t* methods;
  dfsch_object_t* qualifier;
  DFSCH_OBJECT_ARG(args, methods);
  DFSCH_OBJECT_ARG(args, qualifier);
  DFSCH_ARG_END(args);
  return dfsch_get_qualified_methods(methods, qualifier);
}

DFSCH_DEFINE_PRIMITIVE(parse_specialized_lambda_list,
                       "Parse list-represented lambda-list into "
                       "non-specialized lambda-list and list of "
                       "specializers"){
  dfsch_object_t* specialized_lambda_list;
  dfsch_object_t* lambda_list;
  dfsch_object_t* specializers;
  DFSCH_OBJECT_ARG(args, specialized_lambda_list);
  DFSCH_ARG_END(args);
  
  dfsch_parse_specialized_lambda_list(specialized_lambda_list,
                                      &lambda_list,
                                      &specializers);

  return dfsch_values(2, lambda_list, specializers);
}

void dfsch__generic_register(dfsch_object_t* env){
  dfsch_defcanon_cstr(env, "make-generic-function",
                      DFSCH_PRIMITIVE_REF(make_generic_function));
  dfsch_defcanon_cstr(env, "make-method",
                      DFSCH_PRIMITIVE_REF(make_method));

  dfsch_defcanon_cstr(env, "add-method!", (dfsch_object_t*)&add_method);
  dfsch_defcanon_cstr(env, "remove-method!", (dfsch_object_t*)&remove_method);
  dfsch_defcanon_cstr(env, "generic-function-methods", 
                      (dfsch_object_t*)&generic_function_methods);

  dfsch_defcanon_cstr(env, "call-next-method",
                      DFSCH_MACRO_REF(call_next_method));

  dfsch_defcanon_cstr(env, "define-generic-function",
                      DFSCH_MACRO_REF(define_generic_function));
  dfsch_defcanon_cstr(env, "define-method",
                      DFSCH_MACRO_REF(define_method));

  dfsch_defcanon_cstr(env, "call-method",
                      DFSCH_PRIMITIVE_REF(call_method));


  dfsch_defcanon_cstr(env, "<generic-function-type>", 
                      DFSCH_GENERIC_FUNCTION_TYPE_TYPE);
  dfsch_defcanon_cstr(env, "<generic-function>", 
                      DFSCH_GENERIC_FUNCTION_TYPE);
  dfsch_defcanon_cstr(env, "<standard-generic-function>", 
                      DFSCH_STANDARD_GENERIC_FUNCTION_TYPE);
  dfsch_defcanon_cstr(env, "<singleton-generic-function>", 
                      DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE);

  dfsch_defcanon_cstr(env, "<method-context-type>", 
                      DFSCH_METHOD_CONTEXT_TYPE_TYPE);
  dfsch_defcanon_cstr(env, "<method-context>", 
                      DFSCH_METHOD_CONTEXT_TYPE);
  dfsch_defcanon_cstr(env, "<standard-method-context>", 
                      DFSCH_STANDARD_METHOD_CONTEXT_TYPE);
  dfsch_defcanon_cstr(env, "<simple-method-context>", 
                      DFSCH_SIMPLE_METHOD_CONTEXT_TYPE);

  dfsch_defcanon_cstr(env, "<standard-effective-method>", 
                      DFSCH_STANDARD_EFFECTIVE_METHOD_TYPE);
  dfsch_defcanon_cstr(env, "<method>", 
                      DFSCH_METHOD_TYPE);

  dfsch_defcanon_pkgcstr(env, DFSCH_DFSCH_LANG_PACKAGE,
                         "parse-specialized-lambda-list",
                         DFSCH_PRIMITIVE_REF(parse_specialized_lambda_list));

  dfsch_defcanon_pkgcstr(env, DFSCH_DFSCH_LANG_PACKAGE,
                         "get-primary-methods",
                         DFSCH_PRIMITIVE_REF(get_primary_methods));

  dfsch_defcanon_pkgcstr(env, DFSCH_DFSCH_LANG_PACKAGE,
                         "get-qualified-methods",
                         DFSCH_PRIMITIVE_REF(get_qualified_methods));
}
