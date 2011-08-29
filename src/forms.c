/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Special forms
 * Copyright (C) 2005-2008 Ales Hakl
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

#include "internal.h"
#include <dfsch/magic.h>
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <dfsch/number.h>
#include <dfsch/generate.h>
#include <dfsch/compiler.h>

DFSCH_FORM_METHOD_COMPILE(if){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);
  object_t* test;
  object_t* consequent;
  object_t* alternate;

  DFSCH_OBJECT_ARG(args,test);
  DFSCH_OBJECT_ARG(args,consequent);
  DFSCH_OBJECT_ARG_OPT(args,alternate, NULL);
  DFSCH_ARG_END(args);
  
  test = dfsch_compile_expression(test, env);
  consequent = dfsch_compile_expression(consequent, env);
  alternate = dfsch_compile_expression(alternate, env);

  return dfsch_cons_ast_node(form,
                             expr,
                             3,
                             test,
                             consequent,
                             alternate);
}

DFSCH_DEFINE_FORM(if, 
                  {DFSCH_FORM_COMPILE(if)},
                  "Conditional operator"){
  object_t* test;
  object_t* consequent;
  object_t* alternate;

  DFSCH_OBJECT_ARG(args,test);
  DFSCH_OBJECT_ARG(args,consequent);
  DFSCH_OBJECT_ARG_OPT(args,alternate, NULL);
  DFSCH_ARG_END(args);

  test = dfsch_eval(test, env);

  return dfsch_eval_tr((test?consequent:alternate), env, esc);
}

dfsch_object_t* dfsch_generate_if(dfsch_object_t* cond,
                                  dfsch_object_t* cons,
                                  dfsch_object_t* alt){
  return dfsch_immutable_list(4, DFSCH_FORM_REF(if), cond, cons, alt);
}


DFSCH_DEFINE_FORM(quote, {}, NULL){
  object_t* value;
  
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  return value;
}
dfsch_object_t* dfsch_generate_quote(dfsch_object_t* value){
  return dfsch_immutable_list(2, 
                              DFSCH_FORM_REF(quote), 
                              value);
}
int dfsch_quote_expression_p(dfsch_object_t* expr){
  return DFSCH_PAIR_P(expr) && DFSCH_FAST_CAR(expr) == DFSCH_FORM_REF(quote);
}

DFSCH_FORM_METHOD_COMPILE(begin_like){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);
  return dfsch_cons_ast_node_cdr(form,
                                 expr,
                                 dfsch_compile_expression_list(args, env),
                                 0);
}



DFSCH_DEFINE_FORM(begin, 
                  {DFSCH_FORM_COMPILE(begin_like)},
                  "Evaluate list of expressions and return last result"){
  return dfsch_eval_proc_tr(args, env, esc);
}
dfsch_object_t* dfsch_generate_begin(dfsch_object_t* exps){
  return dfsch_immutable_list_cdr(exps, 1, DFSCH_FORM_REF(begin));
}

DFSCH_DEFINE_FORM(internal_loop, 
                  {DFSCH_FORM_COMPILE(begin_like)},
                  "Inifinite loop"){
  dfsch_object_t* res;
  DFSCH_CATCH_BEGIN(DFSCH_SYM_BREAK) {
    for(;;){
      dfsch_eval_proc(args, env);
    }
  } DFSCH_CATCH {
    res = DFSCH_CATCH_VALUE;
  } DFSCH_CATCH_END;

  return res;
}
dfsch_object_t* dfsch_generate_loop(dfsch_object_t* exps){
  return dfsch_cons(DFSCH_FORM_REF(internal_loop), 
                    exps);
}

DFSCH_FORM_METHOD_COMPILE(internal_let){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);
  object_t *vars;
  object_t* o_vars; 
  object_t *code;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();

  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  o_vars = vars;
  while (DFSCH_PAIR_P(vars)){
    dfsch_object_t* clause = DFSCH_FAST_CAR(vars);
    object_t* var;
    object_t* val;

    DFSCH_OBJECT_ARG(clause, var);
    DFSCH_OBJECT_ARG(clause, val);
    DFSCH_ARG_END(clause);

    dfsch_list_collect(lc,
                       dfsch_cons_ast_node(var,
                                           clause,
                                           1,
                                           dfsch_compile_expression(val,
                                                                    env)));
    vars = DFSCH_FAST_CDR(vars);
  }

  return dfsch_cons_ast_node_cdr(form,
                                 expr,
                                 dfsch_compile_expression_list(code, env),
                                 1,
                                 dfsch_list_annotate(dfsch_collected_list(lc),
                                                     DFSCH_SYM_COMPILED_FROM,
                                                     o_vars));
}

DFSCH_DEFINE_FORM(internal_let, {DFSCH_FORM_COMPILE(internal_let)},
                  NULL){
  object_t *vars;
  object_t *code;

  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  object_t* ext_env = dfsch_new_frame(env);

  while (DFSCH_PAIR_P(vars)){
    dfsch_object_t* clause = DFSCH_FAST_CAR(vars);
    object_t* var;
    object_t* val;

    DFSCH_OBJECT_ARG(clause, var);
    DFSCH_OBJECT_ARG(clause, val);
    DFSCH_ARG_END(clause);

    val = dfsch_eval(val, env);

    dfsch_define(var, val, ext_env, 0);
    
    vars = DFSCH_FAST_CDR(vars);
  }

  return dfsch_eval_proc_tr_free_env(code, ext_env, esc);
}
dfsch_object_t* dfsch_generate_let1(dfsch_object_t* bind,
                                    dfsch_object_t* exp){
  return dfsch_immutable_list(3, DFSCH_FORM_REF(internal_let), bind, exp);
}
dfsch_object_t* dfsch_generate_let(dfsch_object_t* bind,
                                    dfsch_object_t* exp){
  return dfsch_immutable_list_cdr(exp, 
                                  2, 
                                  DFSCH_FORM_REF(internal_let), 
                                  bind);
}

/////////////////////////////////////////////////////////////////////////////
//
// Stack unwinding
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_FORM(unwind_protect, {DFSCH_FORM_COMPILE(begin_like)},
                  NULL){
  object_t* protect;
  object_t* ret;
  DFSCH_OBJECT_ARG(args, protect);
 
  DFSCH_UNWIND {
    dfsch_eval(protect, env);
  } DFSCH_PROTECT {
    ret = dfsch_eval_proc(args, env);
  } DFSCH_PROTECT_END;

  return ret;
}


DFSCH_DEFINE_FORM(catch, {DFSCH_FORM_COMPILE(begin_like)},
                  NULL){
  object_t* tag;
  object_t* ret;
  DFSCH_OBJECT_ARG(args, tag);
  
  tag = dfsch_eval(tag, env);

  DFSCH_CATCH_BEGIN(tag) {
    ret = dfsch_eval_proc(args, env);
  } DFSCH_CATCH {
    ret = DFSCH_CATCH_VALUE;
    DFSCH_CATCH_RESTORE_VALUES;
  } DFSCH_CATCH_END;

  return ret;
}

DFSCH_DEFINE_FORM(throw, {DFSCH_FORM_COMPILE(begin_like)},
                  NULL){
  dfsch_object_t* tag;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, tag);
  DFSCH_OBJECT_ARG(args, value);
  
  tag = dfsch_eval(tag, env);
  value = dfsch_eval(value, env);

  dfsch_throw(tag, value);
  return NULL;
}
dfsch_object_t* dfsch_generate_throw(dfsch_object_t* tag,
                                     dfsch_object_t* value){
  return dfsch_immutable_list(3, DFSCH_FORM_REF(throw), tag, value);
}


DFSCH_FORM_METHOD_COMPILE(destructuring_bind){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);
  dfsch_object_t* lambda_list;
  DFSCH_OBJECT_ARG(args, lambda_list);
  return dfsch_cons_ast_node_cdr(form,
                                 expr,
                                 dfsch_compile_expression_list(args, env),
                                 1,
                                 lambda_list);
}

DFSCH_DEFINE_FORM(destructuring_bind, 
                  {DFSCH_FORM_COMPILE(destructuring_bind)},
                  "Bind variables as if function with formal parameters "
                  "given by first argument was called with arguments "
                  "resulting from evaluation of second argument"){
  dfsch_object_t *arglist;
  dfsch_object_t *list;
  dfsch_object_t *code;

  DFSCH_OBJECT_ARG(args, arglist);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_REST(args, code);

  list = dfsch_eval(list, env);

  return dfsch_eval_proc_tr(code, 
			    dfsch_destructuring_bind(arglist, 
						     list, 
						     env), 
			    esc);
}

DFSCH_DEFINE_FORM(internal_get_values, 
                  {DFSCH_FORM_COMPILE(begin_like)},
                  "Evaluate list of expressions and return all values of "
                  "last form"){
  return dfsch_get_values_list(dfsch_eval_proc_tr(args, env, NULL));
}

/////////////////////////////////////////////////////////////////////////////
//
// Basic special forms
//
/////////////////////////////////////////////////////////////////////////////


DFSCH_DEFINE_FORM(internal_reclose_closure,
                  {},
                  NULL){
  dfsch_object_t* closure;
  DFSCH_OBJECT_ARG(args, closure);
  DFSCH_ARG_END(args);
  return dfsch__reclose_closure(closure, dfsch_reify_environment(env));
}

DFSCH_FORM_METHOD_COMPILE(lambda){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);

  dfsch_object_t* name;
  dfsch_object_t* lambda_list;
  dfsch_object_t* body;
  dfsch_object_t* c;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, lambda_list);
  DFSCH_ARG_REST(args, body);

  c = dfsch_named_lambda(env, lambda_list, body, name);
  dfsch_precompile_function(c);
  
  return dfsch_cons_ast_node(DFSCH_FORM_REF(internal_reclose_closure),
                             expr,
                             1, c);
}

DFSCH_DEFINE_FORM(internal_lambda,
                  {DFSCH_FORM_COMPILE(lambda)},
                  "Create new function"){
  dfsch_object_t* name;
  dfsch_object_t* lambda_list;
  dfsch_object_t* body;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, lambda_list);
  DFSCH_ARG_REST(args, body);

  return dfsch_named_lambda(dfsch_reify_environment(env), 
                            lambda_list, body, name);
}

dfsch_object_t* dfsch_generate_lambda(dfsch_object_t* name,
                                      dfsch_object_t* lambda_list,
                                      dfsch_object_t* body){
  return dfsch_immutable_list_cdr(body, 
                                  3, 
                                  DFSCH_FORM_REF(internal_lambda),
                                  name,
                                  lambda_list);
}

DFSCH_FORM_METHOD_COMPILE(define){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);
  object_t* name;
  object_t* value;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);
  
  value = dfsch_compile_expression(value, env);

  return dfsch_cons_ast_node(form,
                             expr,
                             2,
                             name,
                             value);
}


DFSCH_DEFINE_FORM(internal_define_variable, 
                  {DFSCH_FORM_COMPILE(define)},
                  "Define variable"){

  object_t* name;
  object_t* value;
    
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  value = dfsch_eval(value, env);
  dfsch_define(name, value, env, 0);
  return value;
}
dfsch_object_t* dfsch_generate_define_variable(dfsch_object_t* name,
                                               dfsch_object_t* value){
  return dfsch_list(3, DFSCH_FORM_REF(internal_define_variable), name, value);
}
DFSCH_DEFINE_FORM(internal_define_constant, 
                  {DFSCH_FORM_COMPILE(define)},
                   "Define constant"){

  object_t* name;
  object_t* value;
    
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  value = dfsch_eval(value, env);
  dfsch_define(name, value, env, DFSCH_VAR_CONSTANT);
  return value;
}
dfsch_object_t* dfsch_generate_define_constant(dfsch_object_t* name,
                                               dfsch_object_t* value){
  return dfsch_immutable_list(3, 
                              DFSCH_FORM_REF(internal_define_constant), 
                              name, value);
}

DFSCH_DEFINE_FORM(internal_define_canonical_constant, 
                  {DFSCH_FORM_COMPILE(define)},
                  "Define canonical constant (seen by serializer)"){

  object_t* name;
  object_t* value;
    
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  value = dfsch_eval(value, env);
  dfsch_define(name, value, env, DFSCH_VAR_CONSTANT | DFSCH_VAR_CANONICAL);
  return value;
}
dfsch_object_t* dfsch_generate_define_canonical_constant(dfsch_object_t* name,
                                                         dfsch_object_t* value){
  return dfsch_immutable_list(3, 
                              DFSCH_FORM_REF(internal_define_canonical_constant), 
                              name, value);
}


DFSCH_DEFINE_FORM(declare, {},
                  "Add declaration specifier to given symbol"){
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

DFSCH_DEFINE_FORM(set, 
                  {DFSCH_FORM_COMPILE(define)},
                  "Change value of variable"){
  object_t* name;
  object_t* value;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  value = dfsch_eval(value, env);

  dfsch_set(name, value, env);
  return value;

}
DFSCH_DEFINE_FORM(unset, {},
                  "Delete variable binding"){
  object_t* name;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);

  dfsch_unset(name, env);

  return NULL;
}
DFSCH_DEFINE_FORM(defined_p, {},
                  "Check if given variable is defined in "
                  "lexically-enclosing environment"){
  object_t* name;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_env_get(name, env) != DFSCH_INVALID_OBJECT);
}
dfsch_object_t* dfsch_generate_defined_p(dfsch_object_t* name){
  return dfsch_immutable_list(2,
                              DFSCH_FORM_REF(defined_p), 
                              name);
}

DFSCH_DEFINE_FORM(case, {}, 
                  NULL){
  object_t* val;
  DFSCH_OBJECT_ARG(args, val);

  val = dfsch_eval(val, env);

  while (dfsch_pair_p(args)){
    object_t* c = dfsch_car(args);
    object_t* i = dfsch_car(c);
    if (i == DFSCH_SYM_ELSE)
      return dfsch_eval_proc_tr(dfsch_cdr(c), env, esc);
      
    while (dfsch_pair_p(i)){
      if (dfsch_eqv_p(dfsch_car(i), val))
        return dfsch_eval_proc_tr(dfsch_cdr(c), env, esc);
      i = dfsch_cdr(i);
    }
    args = dfsch_cdr(args);
  }
  
  return NULL;
  
}

DFSCH_DEFINE_FORM(current_environment, {},
                  "Return lexically-enclosing environment"){
  return dfsch_reify_environment(env);
}
dfsch_object_t* dfsch_generate_current_environment(){
  return dfsch_cons(DFSCH_FORM_REF(current_environment), NULL);
}

DFSCH_FORM_METHOD_COMPILE(condition_system_bind){
  dfsch_object_t* args = DFSCH_FAST_CDR(expr);
  object_t *bindings;
  object_t* o_bindings; 
  object_t *code;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();

  DFSCH_OBJECT_ARG(args, bindings);
  DFSCH_ARG_REST(args, code);

  o_bindings = bindings;
  while (DFSCH_PAIR_P(bindings)){
    dfsch_object_t* clause = DFSCH_FAST_CAR(bindings);

    dfsch_list_collect(lc,
                       dfsch_compile_expression_list(clause,
                                                     env));
    bindings = DFSCH_FAST_CDR(bindings);
  }

  return dfsch_cons_ast_node_cdr(form,
                                 expr,
                                 dfsch_compile_expression_list(code, env),
                                 1,
                                 dfsch_list_annotate(dfsch_collected_list(lc),
                                                     DFSCH_SYM_COMPILED_FROM,
                                                     o_bindings));
}

DFSCH_DEFINE_FORM(handler_bind, 
                  {DFSCH_FORM_COMPILE(condition_system_bind)},
                  NULL){
  dfsch_object_t* ret;
  dfsch_object_t* bindings;
  dfsch_object_t* code;
  DFSCH_OBJECT_ARG(args, bindings);
  DFSCH_ARG_REST(args, code);

  DFSCH_SAVE_HANDLERS;

  while (DFSCH_PAIR_P(bindings)){
    dfsch_object_t* type;
    dfsch_object_t* handler;
    dfsch_object_t* b = DFSCH_FAST_CAR(bindings); 
    DFSCH_OBJECT_ARG(b, type);
    DFSCH_OBJECT_ARG(b, handler);
    DFSCH_ARG_END(b);

    type = dfsch_eval(type, env);
    handler = dfsch_eval(handler, env);

    dfsch_handler_bind(type, handler);
    bindings = DFSCH_FAST_CDR(bindings);
  }

  ret = dfsch_eval_proc(code, env);

  DFSCH_RESTORE_HANDLERS;
  return ret;
}

DFSCH_DEFINE_FORM(restart_bind, 
                  {DFSCH_FORM_COMPILE(condition_system_bind)},
                  NULL){
  dfsch_object_t* ret;
  dfsch_object_t* bindings;
  dfsch_object_t* code;
  DFSCH_OBJECT_ARG(args, bindings);
  DFSCH_ARG_REST(args, code);

  DFSCH_SAVE_RESTARTS;

  while (DFSCH_PAIR_P(bindings)){
    dfsch_object_t* name;
    dfsch_object_t* proc;
    dfsch_object_t* desc;
    dfsch_object_t* b = DFSCH_FAST_CAR(bindings); 
    DFSCH_OBJECT_ARG(b, name);
    DFSCH_OBJECT_ARG(b, proc);
    DFSCH_OBJECT_ARG_OPT(b, desc, NULL);
    DFSCH_ARG_END(b);

    
    name = dfsch_eval(name, env);
    proc = dfsch_eval(proc, env);
    desc = dfsch_eval(desc, env);

    dfsch_restart_bind(dfsch_make_restart(name, proc, 
                                          dfsch_string_to_cstr(desc), 
                                          NULL));
    bindings = DFSCH_FAST_CDR(bindings);
  }

  ret = dfsch_eval_proc(code, env);

  DFSCH_RESTORE_RESTARTS;
  return ret;
}


/////////////////////////////////////////////////////////////////////////////

void dfsch__forms_register(dfsch_object_t *ctx){ 
  dfsch_defcanon_cstr(ctx, "begin", DFSCH_FORM_REF(begin));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE,
                         "%loop", DFSCH_FORM_REF(internal_loop));

  dfsch_defcanon_cstr(ctx, "quote", DFSCH_FORM_REF(quote));
  dfsch_defcanon_cstr(ctx, "if", DFSCH_FORM_REF(if));

  dfsch_defcanon_cstr(ctx, "unwind-protect", DFSCH_FORM_REF(unwind_protect));
  dfsch_defcanon_cstr(ctx, "catch", DFSCH_FORM_REF(catch));
  dfsch_defcanon_cstr(ctx, "throw", DFSCH_FORM_REF(throw));


  dfsch_defcanon_cstr(ctx, "destructuring-bind", 
                      DFSCH_FORM_REF(destructuring_bind));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE,
                         "%get-values", DFSCH_FORM_REF(internal_get_values));

  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, "%lambda", 
                         DFSCH_FORM_REF(internal_lambda));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, "%reclose-closure", 
                         DFSCH_FORM_REF(internal_reclose_closure));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, "%let", 
                         DFSCH_FORM_REF(internal_let));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, "%define-variable", 
                         DFSCH_FORM_REF(internal_define_variable));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, "%define-constant", 
                         DFSCH_FORM_REF(internal_define_constant));
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, 
                         "%define-canonical-constant", 
                         DFSCH_FORM_REF(internal_define_canonical_constant));

  dfsch_defcanon_cstr(ctx, "current-environment", 
                    DFSCH_FORM_REF(current_environment));

  dfsch_defcanon_cstr(ctx, "declare", DFSCH_FORM_REF(declare));
  dfsch_defcanon_cstr(ctx, "defined?", DFSCH_FORM_REF(defined_p));
  dfsch_defcanon_cstr(ctx, "set!", DFSCH_FORM_REF(set));
  dfsch_defcanon_cstr(ctx, "unset!", DFSCH_FORM_REF(unset));

  dfsch_defcanon_cstr(ctx, "case", DFSCH_FORM_REF(case));

  dfsch_defcanon_cstr(ctx, "handler-bind",
                    DFSCH_FORM_REF(handler_bind));
  dfsch_defcanon_cstr(ctx, "restart-bind",
                    DFSCH_FORM_REF(restart_bind));

}
