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
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <setjmp.h>

typedef dfsch_object_t object_t;

#define NEED_ARGS(args,count) \
  if (dfsch_list_length(args)!=(count)) \
    DFSCH_THROW("exception:wrong-number-of-arguments",(args));
#define MIN_ARGS(args,count) \
  if (dfsch_list_length(args)<(count)) \
    DFSCH_THROW("exception:too-few-arguments", (args));


// Native procedures:

static object_t* native_gensym(void*baton, object_t* args, dfsch_tail_escape_t* esc){
  if (args)
    dfsch_throw("exception:too-many-arguments", args);

  return dfsch_gensym();
}

static object_t* native_unintern(void*baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* symbol;
  DFSCH_OBJECT_ARG(args, symbol);
  DFSCH_ARG_END(args);

  dfsch_unintern(symbol);
  return symbol;
}


/////////////////////////////////////////////////////////////////////////////
//
// Basic special forms
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_form_lambda(void *baton, object_t* args, dfsch_tail_escape_t* esc){

  MIN_ARGS(dfsch_cdr(args),1);

  return dfsch_lambda(dfsch_car(args),
		      dfsch_car(dfsch_cdr(args)),
		      dfsch_cdr(dfsch_cdr(args)));

}

static object_t* native_form_define(void *baton, object_t* args, dfsch_tail_escape_t* esc){

  MIN_ARGS(dfsch_cdr(args),1);  

  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));

  if (dfsch_pair_p(name)){
    object_t* lambda = dfsch_named_lambda(env,dfsch_cdr(name),
                                          dfsch_cdr(dfsch_cdr(args)),
                                          dfsch_car(name));
    return dfsch_define(dfsch_car(name), lambda ,env);
  }else{
    object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);
    return dfsch_define(name,value,env);
  }

}
static object_t* native_form_set(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  
  NEED_ARGS(dfsch_cdr(args),2);  

  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));
  object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);

  return dfsch_set(name, value, env);

}
static object_t* native_form_defined_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(dfsch_cdr(args),1);
  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));

  return dfsch_bool(dfsch_exception_p(dfsch_lookup(name,env)));
}

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
    object_t* i;
    if (!dfsch_pair_p(c)){
      DFSCH_THROW("exception:not-a-pair",c);
    }
    
    i = dfsch_car(c);
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
  NEED_ARGS(dfsch_cdr(args),1);  
  return dfsch_car(dfsch_cdr(args));
}

static object_t* native_form_quasiquote(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* arg;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, arg);

  return dfsch_quasiquote(env,arg);
}

static object_t* native_macro_begin(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  return dfsch_cdr(args);
}
static object_t* native_form_let(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  MIN_ARGS(args,2);// TODO: named let

  object_t *env = dfsch_car(args);
  object_t *vars = dfsch_car(dfsch_cdr(args));
  object_t *code = dfsch_cdr(dfsch_cdr(args));

  object_t* ext_env = dfsch_new_frame(env);

  while (dfsch_pair_p(vars)){
    object_t* var = dfsch_list_item(dfsch_car(vars),0);
    object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), env);

    dfsch_define(var, val, ext_env);
    
    vars = dfsch_cdr(vars);
  }

  return dfsch_eval_proc_tr(code,ext_env,esc);
}
static object_t* native_form_letrec(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  MIN_ARGS(args,2);

  object_t *env = dfsch_car(args);
  object_t *vars = dfsch_car(dfsch_cdr(args));
  object_t *code = dfsch_cdr(dfsch_cdr(args));

  object_t* ext_env = dfsch_new_frame(env);

  while (dfsch_pair_p(vars)){
    object_t* var = dfsch_list_item(dfsch_car(vars),0);
    object_t* val = dfsch_eval(dfsch_list_item(dfsch_car(vars),1), ext_env);

    dfsch_define(var, val, ext_env);
    
    vars = dfsch_cdr(vars);
  }

  return dfsch_eval_proc_tr(code,ext_env,esc);
}
static object_t* native_form_let_seq(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  MIN_ARGS(args,2);

  object_t *env = dfsch_car(args);
  object_t *vars = dfsch_car(dfsch_cdr(args));
  object_t *code = dfsch_cdr(dfsch_cdr(args));

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



static object_t* native_make_form(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_make_form(dfsch_car(args));
}
static object_t* native_make_macro(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_make_macro(dfsch_car(args));
}

/////////////////////////////////////////////////////////////////////////////
//
// EVAL + APPLY
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_eval(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_eval_tr(dfsch_car(args),dfsch_car(dfsch_cdr(args)), esc);
}
static object_t* native_eval_proc(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_eval_proc_tr(dfsch_car(args),dfsch_car(dfsch_cdr(args)), esc);
}
static object_t* native_apply(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_apply_tr(dfsch_car(args),dfsch_car(dfsch_cdr(args)), esc);
}

/////////////////////////////////////////////////////////////////////////////
//
// Pairs and lists
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_car(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_car(dfsch_car(args));
}
static object_t* native_cdr(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
return dfsch_cdr(dfsch_car(args));
}
static object_t* native_cons(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_cons(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
static object_t* native_list(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  return dfsch_list_copy(args);
}
static object_t* native_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  

  return dfsch_make_number_from_long(dfsch_list_length(dfsch_car(args)));
}
static object_t* native_set_car(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_set_car(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
static object_t* native_set_cdr(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_set_cdr(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
static object_t* native_append(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  return dfsch_append(args);
}
static object_t* native_list_ref(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  int k;
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_NUMBER_ARG(args, k, int);
  DFSCH_ARG_END(args);

  return dfsch_list_item(list, k);
}
static object_t* native_assoc(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* alist;
  object_t* key;

  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_assoc(alist, key);
}
static object_t* native_assv(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* alist;
  object_t* key;

  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_assv(alist, key);
}
static object_t* native_assq(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* alist;
  object_t* key;

  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_assq(alist, key);
}

/////////////////////////////////////////////////////////////////////////////
//
// Type predicates
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_null_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_null_p(dfsch_car(args)));
}
static object_t* native_pair_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_pair_p(dfsch_car(args)));
}
static object_t* native_atom_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_atom_p(dfsch_car(args)));
}
static object_t* native_symbol_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_symbol_p(dfsch_car(args)));
}
static object_t* native_string_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_string_p(dfsch_car(args)));  
}
static object_t* native_primitive_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_primitive_p(dfsch_car(args))); 
}
static object_t* native_closure_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_closure_p(dfsch_car(args)));  
}
static object_t* native_procedure_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_procedure_p(dfsch_car(args)));  
}
static object_t* native_vector_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_vector_p(dfsch_car(args)));  
}
static object_t* native_macro_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_macro_p(dfsch_car(args)));  
}

/////////////////////////////////////////////////////////////////////////////
//
// Equality predicates
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_eq(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_eq_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}
static object_t* native_eqv(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_eqv_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}
static object_t* native_equal(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_equal_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}

/////////////////////////////////////////////////////////////////////////////
//
// Logic
//
/////////////////////////////////////////////////////////////////////////////


static object_t* native_form_or(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* i;
  object_t* r = NULL;
  MIN_ARGS(args, 1);
  env = dfsch_car(args); 
  i = dfsch_cdr(args);
 
  while(i){
    r = dfsch_eval(dfsch_car(i), env);
    if (r)
      return r;
    i = dfsch_cdr(i);
  }

  return r;
}
static object_t* native_form_and(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* env;
  object_t* i;
  object_t* r = dfsch_sym_true();
  MIN_ARGS(args, 1);
  env = dfsch_car(args); 
  i = dfsch_cdr(args);
 
  while(i){
    r = dfsch_eval(dfsch_car(i), env);
    if (!r)
      return r;

    i = dfsch_cdr(i);
  }

  return r;
}
static object_t* native_not(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  object_t *a = dfsch_car(args);
  return dfsch_bool(!a);
}

/////////////////////////////////////////////////////////////////////////////
//
// Exception handling
//
/////////////////////////////////////////////////////////////////////////////

static object_t* native_make_exception(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_make_exception(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
static object_t* native_raise(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  dfsch_raise(dfsch_car(args));
}
static object_t* native_throw(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  dfsch_raise(dfsch_make_exception(dfsch_car(args),
                                   dfsch_car(dfsch_cdr(args))));
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
  DFSCH_OBJECT_ARG(args, handler);
  DFSCH_OBJECT_ARG(args, thunk);
  DFSCH_ARG_END(args);

  return dfsch_try(handler, thunk);
 
}

/////////////////////////////////////////////////////////////////////////////
//
// Continuations
//
/////////////////////////////////////////////////////////////////////////////

typedef struct continuation_t {
  jmp_buf ret;
  object_t* value;
  int active;
} continuation_t;

static object_t* continuation_closure(continuation_t *cont, object_t* args, dfsch_tail_escape_t* esc){
  object_t* value;
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);
  
  if (!cont->active)
    DFSCH_THROW("exception:already-returned",NULL);

  cont->value = value;
  longjmp(cont->ret,1);
}

static object_t* native_call_ec(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t *proc, *value;
  DFSCH_OBJECT_ARG(args, proc);
  DFSCH_ARG_END(args);

  continuation_t *cont = GC_NEW(continuation_t);

  cont->active = 1;
  if(setjmp(cont->ret) == 1){
    value = cont->value;
  }else{
    value = dfsch_apply(proc,
                        dfsch_list(1,
                                   dfsch_make_primitive((dfsch_primitive_t)
                                                        continuation_closure,
                                                        cont)));
  }
  
  cont->active = 0;
  return value;

}

/////////////////////////////////////////////////////////////////////////////
//
// Strings
//
/////////////////////////////////////////////////////////////////////////////


static object_t* native_string_append(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  str_list_t* s = sl_create();

  while(dfsch_pair_p(args)){
    dfsch_object_t* i = dfsch_car(args);
    
    if (!dfsch_string_p(i))
      DFSCH_THROW("exception:not-a-string", i);
      
    sl_append(s, dfsch_string(i));

    args = dfsch_cdr(args);
  }

  object_t* o = dfsch_make_string(sl_value(s)); 
  return o;
}
static object_t* native_string_ref(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);
  object_t* a = dfsch_car(args);
  object_t* b = dfsch_car(dfsch_cdr(args));

  if (!dfsch_string_p(a))
    DFSCH_THROW("exception:not-a-string", a);

  char *s = dfsch_string(a);
  size_t len = strlen(s);
  size_t index = (size_t)(dfsch_number(b));
  
  if (index < 0)
    index = index + len;
  if (index>=len)
    DFSCH_THROW("exception:index-too-large", b);



  return dfsch_make_number((double)s[index]);
}
static object_t* native_string_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);

  object_t* a = dfsch_car(args);
  if (!dfsch_string_p(a))
    DFSCH_THROW("exception:not-a-string", a);

  return dfsch_make_number((double)strlen(dfsch_string(a)));
}

/////////////////////////////////////////////////////////////////////////////
//
// Vectors
//
/////////////////////////////////////////////////////////////////////////////


static object_t* native_make_vector(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t length;
  object_t* fill;

  DFSCH_NUMBER_ARG(args, length, size_t);
  DFSCH_OBJECT_ARG_OPT(args, fill, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_vector(length,fill);
}

static object_t* native_vector(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  return dfsch_list_2_vector(args);
}
static object_t* native_vector_length(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* vector;
  
  DFSCH_OBJECT_ARG(args,vector);
  DFSCH_ARG_END(args);

  if (!dfsch_vector_p(vector))
    DFSCH_THROW("exception:not-a-vector",vector);

  return dfsch_make_number(dfsch_vector_length(vector));

}
static object_t* native_vector_ref(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* vector;
  size_t k;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_NUMBER_ARG(args, k, size_t);
  DFSCH_ARG_END(args);

  return dfsch_vector_ref(vector, k);
}

static object_t* native_vector_set(void* baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* vector;
  size_t k;
  object_t* obj;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_NUMBER_ARG(args, k, size_t);
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_vector_set(vector, k, obj);
}

static object_t* native_vector_2_list(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* vector;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_ARG_END(args);

  return dfsch_vector_2_list(vector);
}

static object_t* native_list_2_vector(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_vector(list);
}


/////////////////////////////////////////////////////////////////////////////
//
// Registering function
//
/////////////////////////////////////////////////////////////////////////////

dfsch_object_t* dfsch_native_register(dfsch_ctx_t *ctx){ 
  dfsch_ctx_define(ctx, "gensym", dfsch_make_primitive(&native_gensym,NULL));
  dfsch_ctx_define(ctx, "unintern", dfsch_make_primitive(&native_unintern,NULL));

  dfsch_ctx_define(ctx, "eq?", dfsch_make_primitive(&native_eq,NULL));
  dfsch_ctx_define(ctx, "eqv?", dfsch_make_primitive(&native_eqv,NULL));
  dfsch_ctx_define(ctx, "equal?", dfsch_make_primitive(&native_equal,NULL));

  dfsch_ctx_define(ctx, "and", 
                   dfsch_make_form(dfsch_make_primitive(&native_form_and,
                                                        NULL)));
  dfsch_ctx_define(ctx, "or", 
                   dfsch_make_form(dfsch_make_primitive(&native_form_or,
                                                        NULL)));
  dfsch_ctx_define(ctx, "not", dfsch_make_primitive(&native_not,NULL));

  dfsch_ctx_define(ctx, "lambda", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_lambda,
							 NULL)));
  dfsch_ctx_define(ctx, "define", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_define,
							 NULL)));
  dfsch_ctx_define(ctx, "defined?", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_defined_p,
							 NULL)));
  dfsch_ctx_define(ctx, "begin", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_begin,
							 NULL)));
  dfsch_ctx_define(ctx, "let", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_let,
							 NULL)));
  dfsch_ctx_define(ctx, "let*", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_let_seq,
							 NULL)));
  dfsch_ctx_define(ctx, "letrec", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_letrec,
							 NULL)));

  dfsch_ctx_define(ctx, "set!", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_set,
							 NULL)));
  dfsch_ctx_define(ctx, "quasiquote", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_quasiquote,
							 NULL)));
  dfsch_ctx_define(ctx, "quote", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_quote,
							 NULL)));
  dfsch_ctx_define(ctx, "if", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_if,
                                                         NULL)));
  dfsch_ctx_define(ctx, "cond", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_cond,
                                                         NULL)));
  dfsch_ctx_define(ctx, "case", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_case,
                                                         NULL)));

  dfsch_ctx_define(ctx, "make-form", 
		   dfsch_make_primitive(&native_make_form,NULL));
  dfsch_ctx_define(ctx, "make-macro", 
		   dfsch_make_primitive(&native_make_macro,NULL));
  dfsch_ctx_define(ctx, "cons", dfsch_make_primitive(&native_cons,NULL));
  dfsch_ctx_define(ctx, "list", dfsch_make_primitive(&native_list,NULL));
  dfsch_ctx_define(ctx, "car", dfsch_make_primitive(&native_car,NULL));
  dfsch_ctx_define(ctx, "cdr", dfsch_make_primitive(&native_cdr,NULL));
  dfsch_ctx_define(ctx, "set-car!", dfsch_make_primitive(&native_set_car,
							 NULL));
  dfsch_ctx_define(ctx, "set-cdr!", dfsch_make_primitive(&native_set_cdr,
							 NULL));

  dfsch_ctx_define(ctx, "length", dfsch_make_primitive(&native_length,NULL));
  dfsch_ctx_define(ctx, "append", dfsch_make_primitive(&native_append,NULL));
  dfsch_ctx_define(ctx, "list-ref", dfsch_make_primitive(&native_list_ref,
                                                         NULL));
  dfsch_ctx_define(ctx, "assoc", dfsch_make_primitive(&native_assoc,NULL));
  dfsch_ctx_define(ctx, "assq", dfsch_make_primitive(&native_assq,NULL));
  dfsch_ctx_define(ctx, "assv", dfsch_make_primitive(&native_assv,NULL));

  dfsch_ctx_define(ctx, "null?", dfsch_make_primitive(&native_null_p,NULL));
  dfsch_ctx_define(ctx, "atom?", dfsch_make_primitive(&native_atom_p,NULL));
  dfsch_ctx_define(ctx, "pair?", dfsch_make_primitive(&native_pair_p,NULL));
  dfsch_ctx_define(ctx, "symbol?", dfsch_make_primitive(&native_symbol_p,
							NULL));
  dfsch_ctx_define(ctx, "string?", dfsch_make_primitive(&native_string_p,
							NULL));
  dfsch_ctx_define(ctx, "primitive?", 
		   dfsch_make_primitive(&native_primitive_p,NULL));
  dfsch_ctx_define(ctx, "closure?", dfsch_make_primitive(&native_closure_p,
							 NULL));
  dfsch_ctx_define(ctx, "procedure?", 
		   dfsch_make_primitive(&native_procedure_p,NULL));
  dfsch_ctx_define(ctx, "macro?", dfsch_make_primitive(&native_macro_p,NULL));
  dfsch_ctx_define(ctx, "vector?", dfsch_make_primitive(&native_vector_p,
                                                        NULL));


  dfsch_ctx_define(ctx, "raise", 
		   dfsch_make_primitive(&native_raise,NULL));
  dfsch_ctx_define(ctx, "make-exception", 
		   dfsch_make_primitive(&native_make_exception,NULL));
  dfsch_ctx_define(ctx, "throw", 
		   dfsch_make_primitive(&native_throw,NULL));
  dfsch_ctx_define(ctx, "error", 
		   dfsch_make_primitive(&native_error,NULL));
  dfsch_ctx_define(ctx, "abort", 
		   dfsch_make_primitive(&native_abort,NULL));
  dfsch_ctx_define(ctx, "try", 
		   dfsch_make_primitive(&native_try,NULL));

  dfsch_ctx_define(ctx, "call-with-escape-continuation",
                   dfsch_ctx_define(ctx, "call/ec", 
                                    dfsch_make_primitive(&native_call_ec,
                                                         NULL)));


  dfsch_ctx_define(ctx, "string-append", 
		   dfsch_make_primitive(&native_string_append,NULL));
  dfsch_ctx_define(ctx, "string-ref", 
		   dfsch_make_primitive(&native_string_ref,NULL));
  dfsch_ctx_define(ctx, "string-length", 
		   dfsch_make_primitive(&native_string_length,NULL));

  dfsch_ctx_define(ctx, "true", dfsch_sym_true());
  dfsch_ctx_define(ctx, "nil", NULL);
  dfsch_ctx_define(ctx, "else", dfsch_sym_true());
  dfsch_ctx_define(ctx, "T", dfsch_sym_true());

  dfsch_ctx_define(ctx, "eval", dfsch_make_primitive(&native_eval,NULL));
  dfsch_ctx_define(ctx, "eval-proc", dfsch_make_primitive(&native_eval_proc,
                                                          NULL));
  dfsch_ctx_define(ctx, "apply", dfsch_make_primitive(&native_apply,NULL));

  dfsch_ctx_define(ctx, "make-vector", 
                   dfsch_make_primitive(&native_make_vector,NULL));
  dfsch_ctx_define(ctx, "vector", 
                   dfsch_make_primitive(&native_vector,NULL));
  dfsch_ctx_define(ctx, "vector-length", 
                   dfsch_make_primitive(&native_vector_length,NULL));
  dfsch_ctx_define(ctx, "vector-set!", 
                   dfsch_make_primitive(&native_vector_set,NULL));
  dfsch_ctx_define(ctx, "vector-ref", 
                   dfsch_make_primitive(&native_vector_ref,NULL));
  dfsch_ctx_define(ctx, "vector->list", 
                   dfsch_make_primitive(&native_vector_2_list,NULL));
  dfsch_ctx_define(ctx, "list->vector", 
                   dfsch_make_primitive(&native_list_2_vector,NULL));


  dfsch__hash_native_register(ctx);
  dfsch__promise_native_register(ctx);
  dfsch__number_native_register(ctx);

  return NULL;
}
