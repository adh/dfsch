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

DFSCH_DEFINE_FORM_IMPL(if){
  object_t* test;
  object_t* consequent;
  object_t* alternate;

  DFSCH_OBJECT_ARG(args,test);
  DFSCH_OBJECT_ARG(args,consequent);
  DFSCH_OBJECT_ARG_OPT(args,alternate, NULL);

  test = dfsch_eval(test, env);

  return dfsch_eval_tr((test?consequent:alternate), env, esc);
}

DFSCH_DEFINE_FORM_IMPL(when){
  object_t* test;

  DFSCH_OBJECT_ARG(args,test);

  if (dfsch_eval(test, env)){
    return dfsch_eval_proc_tr(args, env, esc);
  }

  return NULL;
}

DFSCH_DEFINE_FORM_IMPL(unless){
  object_t* test;

  DFSCH_OBJECT_ARG(args,test);

  if (!dfsch_eval(test, env)){
    return dfsch_eval_proc_tr(args, env, esc);
  }

  return NULL;
}


DFSCH_DEFINE_FORM_IMPL(cond){
  object_t* i = args;

  while (dfsch_pair_p(i)){
    object_t *o = dfsch_eval(dfsch_car(dfsch_car(i)), env);
    if (o){
      object_t* exp = dfsch_cdr(dfsch_car(i));
      if (dfsch_car(exp) == dfsch_sym_bold_right_arrow()){
        object_t* proc = dfsch_eval(dfsch_list_item(exp, 1), env);

        return dfsch_apply(proc, dfsch_list(1, o));
      }else{
        return dfsch_eval_proc_tr(exp, env, esc);
      }
    }
    
    i = dfsch_cdr(i); 
  }

  return NULL;
}
DFSCH_DEFINE_FORM_IMPL(case){
  object_t* val;
  DFSCH_OBJECT_ARG(args, val);

  val = dfsch_eval(val, env);

  while (dfsch_pair_p(args)){
    object_t* c = dfsch_car(args);
    object_t* i = dfsch_car(c);
    if (i == dfsch_sym_else())
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

DFSCH_DEFINE_FORM_IMPL(quote){
  object_t* value;
  
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  return value;
}

DFSCH_DEFINE_FORM_IMPL(quasiquote){ 
  /* This is non-trivial to compile right */
  object_t* arg;
  DFSCH_OBJECT_ARG(args, arg);
  DFSCH_ARG_END(args);

  return dfsch_quasiquote(env,arg);
}

DFSCH_DEFINE_FORM_IMPL(begin){
  return dfsch_eval_proc_tr(args, env, esc);
}
DFSCH_DEFINE_FORM_IMPL(let){
  object_t *vars;
  object_t *code;

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

  return dfsch_eval_proc_tr(code,ext_env, esc);
}
DFSCH_DEFINE_FORM_IMPL(letrec){

  object_t *vars;
  object_t *code;

  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  object_t* ext_env = dfsch_new_frame(env);

  while (dfsch_pair_p(vars)){
    object_t* var = dfsch_list_item(dfsch_car(vars),0);
    object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), ext_env);

    dfsch_define(var, val, ext_env);
    
    vars = dfsch_cdr(vars);
  }

  return dfsch_eval_proc_tr(code,ext_env, esc);
}
DFSCH_DEFINE_FORM_IMPL(let_seq){
  object_t *vars;
  object_t *code;

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

  return dfsch_eval_proc_tr(code, ext_env, esc);
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

  return dfsch_eval_proc_tr(proc, env, esc);
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
// Stack unwinding
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_FORM_IMPL(unwind_protect){
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

DFSCH_DEFINE_FORM_IMPL(catch){
  object_t* tag;
  object_t* ret;
  DFSCH_OBJECT_ARG(args, tag);
  
  tag = dfsch_eval(tag, env);

  DFSCH_CATCH_BEGIN(tag) {
    ret = dfsch_eval_proc(args, env);
  } DFSCH_CATCH {
    ret = DFSCH_CATCH_VALUE;
  } DFSCH_CATCH_END;

  return ret;
}

DFSCH_DEFINE_PRIMITIVE(throw, 0){
  dfsch_object_t* tag;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, tag);
  DFSCH_OBJECT_ARG(args, value);
  
  dfsch_throw(tag, value);
  return NULL;
}


/////////////////////////////////////////////////////////////////////////////
//
// do
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_FORM_IMPL(do){
  object_t* vars;
  object_t* test;
  object_t* exprs;
  object_t* commands;
  object_t* lenv;
  object_t* i;

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

  return dfsch_eval_proc_tr(exprs, lenv, esc);
}

DFSCH_DEFINE_FORM_IMPL(destructuring_bind){
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



/////////////////////////////////////////////////////////////////////////////

void dfsch__control_register(dfsch_object_t *ctx){ 
  dfsch_define_cstr(ctx, "begin", DFSCH_FORM_REF(begin));
  dfsch_define_cstr(ctx, "let", DFSCH_FORM_REF(let));
  dfsch_define_cstr(ctx, "let*", DFSCH_FORM_REF(let_seq));
  dfsch_define_cstr(ctx, "letrec", DFSCH_FORM_REF(letrec));

  dfsch_define_cstr(ctx, "quasiquote", DFSCH_FORM_REF(quasiquote));
  dfsch_define_cstr(ctx, "quote", DFSCH_FORM_REF(quote));
  dfsch_define_cstr(ctx, "if", DFSCH_FORM_REF(if));
  dfsch_define_cstr(ctx, "when", DFSCH_FORM_REF(when));
  dfsch_define_cstr(ctx, "unless", DFSCH_FORM_REF(unless));
  dfsch_define_cstr(ctx, "cond", DFSCH_FORM_REF(cond));
  dfsch_define_cstr(ctx, "case", DFSCH_FORM_REF(case));


  dfsch_define_cstr(ctx, "unwind-protect", DFSCH_FORM_REF(unwind_protect));
  dfsch_define_cstr(ctx, "catch", DFSCH_FORM_REF(catch));
  dfsch_define_cstr(ctx, "throw", DFSCH_PRIMITIVE_REF(throw));
  

  dfsch_define_cstr(ctx, "eval", dfsch_make_primitive(&native_eval,NULL));
  dfsch_define_cstr(ctx, "eval-proc", dfsch_make_primitive(&native_eval_proc,
                                                          NULL));
  dfsch_define_cstr(ctx, "apply", dfsch_make_primitive(&native_apply,NULL));

  dfsch_define_cstr(ctx, "do", DFSCH_FORM_REF(do));

  dfsch_define_cstr(ctx, "destructuring-bind", 
                    DFSCH_FORM_REF(destructuring_bind));

}
