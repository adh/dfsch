/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Basic native functions
 * Copyright (C) 2005 Ales Hakl
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
#include <dfsch/magic.h>
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <dfsch/number.h>

typedef dfsch_object_t object_t;

// TODO: document all native functions somewhere

static object_t* native_macro_if(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* test;
  object_t* consequent;
  object_t* alternate;

  DFSCH_OBJECT_ARG(args,env);
  DFSCH_OBJECT_ARG(args,test);
  DFSCH_OBJECT_ARG(args,consequent);
  DFSCH_OBJECT_ARG_OPT(args,alternate, NULL);

  test = dfsch_eval(test, env);

  return dfsch_list(1, test?consequent:alternate);

}

static object_t* native_macro_when(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* test;

  DFSCH_OBJECT_ARG(args,env);
  DFSCH_OBJECT_ARG(args,test);

  test = dfsch_eval(test, env);

  return test?args:NULL;
}

static object_t* native_macro_unless(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* test;

  DFSCH_OBJECT_ARG(args,env);
  DFSCH_OBJECT_ARG(args,test);

  test = dfsch_eval(test, env);

  return test?NULL:args;
}


static object_t* native_macro_cond(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env = dfsch_car(args);
  object_t* i = dfsch_cdr(args);

  while (dfsch_pair_p(i)){
    object_t *o = dfsch_eval(dfsch_car(dfsch_car(i)), env);
    if (o){
      object_t* exp = dfsch_cdr(dfsch_car(i));
      if (dfsch_car(exp) == dfsch_sym_bold_right_arrow()){
        object_t* proc = dfsch_eval(dfsch_list_item(exp, 1), env);

        return dfsch_cons(dfsch_list(2,
                                     dfsch_sym_quote(),
                                     dfsch_apply(proc,
                                                 dfsch_list(1,
                                                            o))),
                          NULL);

      }else{
        return exp;
      }
    }
    
    i = dfsch_cdr(i); 
  }

  return NULL;
}
static object_t* native_macro_case(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* val;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, val);

  val = dfsch_eval(val, env);

  while (dfsch_pair_p(args)){
    object_t* c = dfsch_car(args);
    object_t* i = dfsch_car(c);
    if (i == dfsch_sym_else())
        return dfsch_cdr(c);
      
    while (dfsch_pair_p(i)){
      if (dfsch_eqv_p(dfsch_car(i), val))
        return dfsch_cdr(c);
      i = dfsch_cdr(i);
    }
    args = dfsch_cdr(args);
  }
  
  return NULL;
  
}

static object_t* native_form_quote(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* value;
  
  DFSCH_DISCARD_ARG(args, env);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  return value;
}

static object_t* native_form_quasiquote(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* arg;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, arg);
  DFSCH_ARG_END(args);

  return dfsch_quasiquote(env,arg);
}

static object_t* native_macro_begin(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  return dfsch_cdr(args);
}
static object_t* native_form_let(void *baton, object_t* args, dfsch_tail_escape_t* esc){

  object_t *env;
  object_t *vars;
  object_t *code;

  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  object_t* ext_env = dfsch_new_frame(env);

  if (dfsch_symbol_p(vars)){ // named-let
    object_t* name = vars;

    object_t* ll_head = NULL;
    object_t* ll_tail = NULL;
    object_t* vl_head = NULL;
    object_t* vl_tail = NULL;

    object_t* lambda;

    DFSCH_OBJECT_ARG(code, vars);

    while (dfsch_pair_p(vars)){
      object_t* var = dfsch_list_item(dfsch_car(vars),0);
      object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), env);
      
      if (ll_head && vl_head){
        object_t* tmp;
        
        tmp = dfsch_cons(var, NULL);
        dfsch_set_cdr(ll_tail, tmp);
        ll_tail = tmp;

        tmp = dfsch_cons(val, NULL);
        dfsch_set_cdr(vl_tail, tmp);
        vl_tail = tmp;
      }else{
        ll_head = ll_tail = dfsch_cons(var, NULL);
        vl_head = vl_tail = dfsch_cons(val, NULL);
      }

      vars = dfsch_cdr(vars);
    }

    lambda = dfsch_named_lambda(ext_env, ll_head, code, name);
    dfsch_define(name, lambda, ext_env);

    return dfsch_apply_tr(lambda, vl_head, esc);
  }

  while (dfsch_pair_p(vars)){
    object_t* var = dfsch_list_item(dfsch_car(vars),0);
    object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), env);

    dfsch_define(var, val, ext_env);
    
    vars = dfsch_cdr(vars);
  }

  return dfsch_eval_proc_tr(code,ext_env,NULL,esc);
}
static object_t* native_form_letrec(void *baton, object_t* args, dfsch_tail_escape_t* esc){

  object_t *env;
  object_t *vars;
  object_t *code;

  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  object_t* ext_env = dfsch_new_frame(env);

  while (dfsch_pair_p(vars)){
    object_t* var = dfsch_list_item(dfsch_car(vars),0);
    object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), ext_env);

    dfsch_define(var, val, ext_env);
    
    vars = dfsch_cdr(vars);
  }

  return dfsch_eval_proc_tr(code,ext_env,NULL,esc);
}
static object_t* native_form_let_seq(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t *env;
  object_t *vars;
  object_t *code;

  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  object_t* ext_env = env;

  while (dfsch_pair_p(vars)){
    object_t* var = dfsch_list_item(dfsch_car(vars),0);
    object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), ext_env);

    ext_env = dfsch_new_frame(ext_env);
    dfsch_define(var, val, ext_env);
    
    vars = dfsch_cdr(vars);
  }

  return dfsch_eval_proc_tr(code, ext_env, NULL, esc);
}


/////////////////////////////////////////////////////////////////////////////
//
// EVAL + APPLY
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_eval(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* expr;
  object_t* env;

  DFSCH_OBJECT_ARG(args, expr);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);

  return dfsch_eval_tr(expr, env, esc);
}
static object_t* native_eval_proc(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* proc;
  object_t* env;

  DFSCH_OBJECT_ARG(args, proc);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);

  return dfsch_eval_proc_tr(proc, env, NULL, esc);
}
static object_t* native_apply(void *baton, object_t* args, 
                              dfsch_tail_escape_t* esc){

  /* TODO: free arguments */
  
  object_t* func;
  object_t* arglist;

  DFSCH_OBJECT_ARG(args, func);
  DFSCH_OBJECT_ARG(args, arglist);
  DFSCH_ARG_END(args);

  return dfsch_apply_tr(func, arglist, esc);
}
/////////////////////////////////////////////////////////////////////////////
//
// Exception handling
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_make_exception(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* type;
  object_t* data;
  object_t* stack_trace;

  DFSCH_OBJECT_ARG(args, type);
  DFSCH_OBJECT_ARG_OPT(args, data, NULL);
  DFSCH_OBJECT_ARG_OPT(args, stack_trace, dfsch_get_stack_trace());
  DFSCH_ARG_END(args);

  return dfsch_make_exception(type, data, stack_trace);
}
static object_t* native_raise(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* exception;

  DFSCH_OBJECT_ARG(args, exception);
  DFSCH_ARG_END(args);

  dfsch_raise(exception);
}
static object_t* native_throw(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* type;
  object_t* data;

  DFSCH_OBJECT_ARG(args, type);
  DFSCH_OBJECT_ARG_OPT(args, data, NULL);
  DFSCH_ARG_END(args);

  dfsch_raise(dfsch_make_exception(type,
                                   data,
                                   dfsch_get_stack_trace()));
}
static object_t* native_error(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  dfsch_throw("user:error",args);
}
static object_t* native_abort(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  dfsch_throw("user:abort",NULL);
}

static object_t* native_try(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* handler;
  object_t* thunk;
  object_t* finally;
  DFSCH_OBJECT_ARG(args, thunk);
  DFSCH_OBJECT_ARG(args, handler);
  DFSCH_OBJECT_ARG_OPT(args, finally, NULL);
  DFSCH_ARG_END(args);

  return dfsch_try(handler, finally, thunk);
 
}

/////////////////////////////////////////////////////////////////////////////
//
// Continuations
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_form_unwind_protect(void *baton, object_t* args, 
                                            dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* protect;
  object_t* ret;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, protect);
 
  DFSCH_UNWIND {
    dfsch_eval(protect, env);
  } DFSCH_PROTECT {
    ret = dfsch_eval_proc(args, env);
  } DFSCH_END_UNWIND;

  return ret;
}


/////////////////////////////////////////////////////////////////////////////
//
// do
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_form_do(void *baton, object_t* args, 
                                dfsch_tail_escape_t* esc){

  object_t* env;
  object_t* vars;
  object_t* test;
  object_t* exprs;
  object_t* commands;
  object_t* lenv;
  object_t* i;

  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_OBJECT_ARG(args, test);
  
  commands = args;
  exprs = dfsch_cdr(test);
  test = dfsch_car(test);

  lenv = dfsch_new_frame(env);

  i = vars;
  while (dfsch_pair_p(i)){
    object_t* j = dfsch_car(i);
    object_t* name;
    object_t* init;
    object_t* step;

    DFSCH_OBJECT_ARG(j, name);
    DFSCH_OBJECT_ARG(j, init);

    dfsch_define(name, dfsch_eval(init, env), lenv);

    i = dfsch_cdr(i);
  }

  while (dfsch_eval(test, lenv) == NULL){
    object_t* nenv;
    dfsch_eval_proc(commands, lenv);

    nenv = dfsch_new_frame(env);
    i = vars;
    while (dfsch_pair_p(i)){
      object_t* j = dfsch_car(i);
      object_t* name;
      object_t* init;
      object_t* step;
      
      DFSCH_OBJECT_ARG(j, name);
      DFSCH_OBJECT_ARG(j, init);
      DFSCH_OBJECT_ARG_OPT(j, step, name);

      dfsch_define(name, dfsch_eval(step, lenv), nenv);
      
      i = dfsch_cdr(i);
    }
    lenv = nenv;
  }

  return dfsch_eval_proc_tr(exprs, lenv, NULL, esc);
}

static dfsch_object_t* native_form_destructuring_bind(void *baton, 
						      dfsch_object_t* args, 
						      dfsch_tail_escape_t* esc){
  dfsch_object_t *env;
  dfsch_object_t *arglist;
  dfsch_object_t *list;
  dfsch_object_t *code;

  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, arglist);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_REST(args, code);

  list = dfsch_eval(list, env);

  return dfsch_eval_proc_tr(code, 
			    dfsch_destructuring_bind(arglist, 
						     list, 
						     env), 
                            NULL, 
			    esc);
}



/////////////////////////////////////////////////////////////////////////////

void dfsch__control_register(dfsch_object_t *ctx){ 
  dfsch_define_cstr(ctx, "begin", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_begin,
							 NULL)));
  dfsch_define_cstr(ctx, "let", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_let,
							 NULL)));
  dfsch_define_cstr(ctx, "let*", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_let_seq,
							 NULL)));
  dfsch_define_cstr(ctx, "letrec", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_letrec,
							 NULL)));

  dfsch_define_cstr(ctx, "quasiquote", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_quasiquote,
							 NULL)));
  dfsch_define_cstr(ctx, "quote", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_quote,
							 NULL)));
  dfsch_define_cstr(ctx, "if", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_if,
                                                         NULL)));
  dfsch_define_cstr(ctx, "when", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_when,
                                                         NULL)));
  dfsch_define_cstr(ctx, "unless", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_unless,
                                                         NULL)));
  dfsch_define_cstr(ctx, "cond", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_cond,
                                                         NULL)));
  dfsch_define_cstr(ctx, "case", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_case,
                                                         NULL)));

  dfsch_define_cstr(ctx, "raise", 
		   dfsch_make_primitive(&native_raise,NULL));
  dfsch_define_cstr(ctx, "make-exception", 
		   dfsch_make_primitive(&native_make_exception,NULL));
  dfsch_define_cstr(ctx, "throw", 
		   dfsch_make_primitive(&native_throw,NULL));
  dfsch_define_cstr(ctx, "error", 
		   dfsch_make_primitive(&native_error,NULL));
  dfsch_define_cstr(ctx, "abort", 
		   dfsch_make_primitive(&native_abort,NULL));
  dfsch_define_cstr(ctx, "try", 
		   dfsch_make_primitive(&native_try,NULL));

  dfsch_define_cstr(ctx, "unwind-protect", 
                    dfsch_make_form(dfsch_make_primitive(&native_form_unwind_protect,
							 NULL)));



  dfsch_define_cstr(ctx, "eval", dfsch_make_primitive(&native_eval,NULL));
  dfsch_define_cstr(ctx, "eval-proc", dfsch_make_primitive(&native_eval_proc,
                                                          NULL));
  dfsch_define_cstr(ctx, "apply", dfsch_make_primitive(&native_apply,NULL));

  dfsch_define_cstr(ctx, "do", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_do,
							 NULL)));
  dfsch_define_cstr(ctx, "destructuring-bind", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_destructuring_bind,
							 NULL)));



}
