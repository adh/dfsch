#include <dfsch/dfsch.h>
#include <dfsch/generate.h>
#include "internal.h"

DFSCH_DEFINE_MACRO(or, "Short-circuiting logical or"){
  dfsch_object_t* tmp_name = dfsch_gensym();

  if (!args){
    return NULL;
  }

  return dfsch_generate_let1(dfsch_cons(dfsch_list(2, 
                                                   tmp_name, 
                                                   dfsch_car(args)), 
                                        NULL),
                             dfsch_generate_if(tmp_name,
                                               tmp_name,
                                               dfsch_cons(DFSCH_MACRO_REF(or),
                                                          dfsch_cdr(args))));
}


DFSCH_DEFINE_MACRO(and, "Short-circuiting logical and"){
  dfsch_object_t* tmp_name = dfsch_gensym();
  dfsch_object_t* rest;

  if (!args){
    return DFSCH_SYM_TRUE;
  }

  rest = dfsch_cdr(args);
  if (rest) {
    return dfsch_generate_let1(dfsch_cons(dfsch_list(2, 
                                                     tmp_name, 
                                                     dfsch_car(args)), 
                                          NULL),
                               dfsch_generate_if(tmp_name,
                                                 dfsch_cons(DFSCH_MACRO_REF(and),
                                                            dfsch_cdr(args)),
                                                 tmp_name));
  } else {
    return dfsch_car(args);
  }
}

DFSCH_DEFINE_MACRO(when, "Execute body only when condition is true"){
  dfsch_object_t* test;

  DFSCH_OBJECT_ARG(args,test);

  return dfsch_generate_if(test,
                           dfsch_generate_begin(args),
                           NULL);
}

DFSCH_DEFINE_MACRO(unless, "Execute body only when condition is not true"){
  dfsch_object_t* test;

  DFSCH_OBJECT_ARG(args,test);

  return dfsch_generate_if(test,
                           NULL,
                           dfsch_generate_begin(args));
}


DFSCH_DEFINE_MACRO(cond, NULL){
  dfsch_object_t* clause;
  dfsch_object_t* condition;
  dfsch_object_t* consequent;

  if (!args){
    return NULL;
  }

  DFSCH_OBJECT_ARG(args, clause);
  DFSCH_OBJECT_ARG(clause, condition);
  DFSCH_ARG_REST(clause, consequent);

  if (condition == DFSCH_SYM_ELSE){
    return dfsch_generate_begin(consequent);
  } else if (!consequent){
    return dfsch_generate_if(condition,
                             NULL,
                             dfsch_cons(DFSCH_MACRO_REF(cond), args));
  } else if (dfsch_car(consequent) == DFSCH_SYM_BOLD_RIGHT_ARROW){
    dfsch_object_t* tmp_name = dfsch_gensym();
    return dfsch_generate_let1
      (dfsch_cons(dfsch_list(2, tmp_name, condition), 
                  NULL),
       dfsch_generate_if(tmp_name,
                         dfsch_list(2, 
                                    dfsch_car(dfsch_cdr(consequent)), 
                                    tmp_name),
                         dfsch_cons(DFSCH_MACRO_REF(cond), args)));
  } else {
    return dfsch_generate_if(condition,
                             dfsch_generate_begin(consequent),
                             dfsch_cons(DFSCH_MACRO_REF(cond), args));

  }
}

DFSCH_DEFINE_MACRO(lambda, "Create new annonymous function"){
  dfsch_object_t* lambda_list;
  dfsch_object_t* body;

  DFSCH_OBJECT_ARG(args, lambda_list);
  DFSCH_ARG_REST(args, body);

  return dfsch_generate_lambda(NULL, lambda_list, body);
}

DFSCH_DEFINE_MACRO(define_macro,
                   "Define new macro implemented by standard-function"){
  dfsch_object_t* name;
  dfsch_object_t* arglist;

  DFSCH_OBJECT_ARG(args, arglist);
  DFSCH_OBJECT_ARG(arglist, name);

  return dfsch_generate_define_constant
    (name,
     dfsch_generate_make_macro(dfsch_generate_lambda(name, 
                                                     arglist, 
                                                     args)));
}

DFSCH_DEFINE_MACRO(define, "Define variable or procedure"){

  dfsch_object_t* name;

  DFSCH_OBJECT_ARG(args, name);

  if (DFSCH_PAIR_P(name)){
    return dfsch_generate_define_constant
      (DFSCH_FAST_CAR(name),
       dfsch_generate_lambda(DFSCH_FAST_CAR(name),
                             DFSCH_FAST_CDR(name),
                             args));
  } else{
    dfsch_object_t* value;
    DFSCH_OBJECT_ARG(args, value);
    DFSCH_ARG_END(args);

    return dfsch_generate_define_variable(name,
                                          value);
  }
}

DFSCH_DEFINE_MACRO(define_variable, 
                   "Define variable only if it is not already defined"){
  dfsch_object_t* name;
  dfsch_object_t* value;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, value, NULL);
  DFSCH_ARG_END(args);
  
  return dfsch_list(3,
                    DFSCH_MACRO_REF(unless),
                    dfsch_generate_defined_p(name),
                    dfsch_generate_define_variable(name, value));
}
DFSCH_DEFINE_MACRO(define_constant,
                   "Define constant variable"){
  dfsch_object_t* name;
  dfsch_object_t* value;

  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, value, NULL);
  DFSCH_ARG_END(args);
  
  return dfsch_list(3,
                    DFSCH_MACRO_REF(unless),
                    dfsch_generate_defined_p(name),
                    dfsch_generate_define_constant(name, value));
}
DFSCH_DEFINE_MACRO(let, NULL){
  dfsch_object_t *vars;
  dfsch_object_t *code;

  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  if (dfsch_symbol_p(vars)){ // named-let
    dfsch_object_t* name = vars;

    dfsch_object_t* ll_head = NULL;
    dfsch_object_t* ll_tail = NULL;
    dfsch_object_t* vl_head = NULL;
    dfsch_object_t* vl_tail = NULL;

    dfsch_object_t* lambda;

    DFSCH_OBJECT_ARG(code, vars);

    while (dfsch_pair_p(vars)){
      dfsch_object_t* var = dfsch_list_item(dfsch_car(vars),0);
      dfsch_object_t* val = dfsch_list_item(dfsch_car(vars),1);
      
      if (ll_head && vl_head){
        dfsch_object_t* tmp;
        
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

    lambda = dfsch_generate_lambda(name, ll_head, code);
    //    dfsch_define(name, lambda, ext_env, 0);

    return dfsch_generate_let(NULL,
                              dfsch_list(2,
                                         dfsch_generate_define_constant(name,
                                                                        lambda),
                                         dfsch_cons(name, vl_head)));
  }

  return dfsch_generate_let(vars, code);
}
DFSCH_DEFINE_MACRO(letrec, NULL){

  dfsch_object_t *vars;
  dfsch_object_t *code;

  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  dfsch_object_t* def_head = NULL;
  dfsch_object_t* def_tail;

  while (dfsch_pair_p(vars)){
    dfsch_object_t* var = dfsch_list_item(dfsch_car(vars),0);
    dfsch_object_t* val = dfsch_list_item(dfsch_car(vars),1);
    dfsch_object_t* def = dfsch_cons(dfsch_generate_define_variable(var, val),
                                     NULL);

    if (def_head){
      DFSCH_FAST_CDR_MUT(def_tail) = def;
      def_tail = def;
    } else {
      def_head = def_tail = def;
    }
    
    vars = dfsch_cdr(vars);
  }

  if (def_head){
    DFSCH_FAST_CDR_MUT(def_tail) = code;
    def_tail = code;
  } else {
    def_head = def_tail = code;
  }
  

  return dfsch_generate_let(NULL, def_head);
}


DFSCH_DEFINE_MACRO(let_seq, NULL){
  dfsch_object_t *vars;
  dfsch_object_t *code;
  dfsch_object_t *place;

  DFSCH_OBJECT_ARG(args, vars);
  DFSCH_ARG_REST(args, code);

  if (!dfsch_pair_p(vars)){
    return dfsch_generate_let(NULL, code);
  }

  vars = dfsch_reverse(vars);
  code = dfsch_generate_let(dfsch_cons(dfsch_car(vars), NULL),
                            code);
  vars = dfsch_cdr(vars);

  while (dfsch_pair_p(vars)){
    code = dfsch_generate_let1(dfsch_cons(dfsch_car(vars), NULL),
                               code);
    vars = dfsch_cdr(vars);
    
  }

  return code;
}

void dfsch__macros_register(dfsch_object_t *ctx){ 
  dfsch_defconst_cstr(ctx, "and", DFSCH_MACRO_REF(and));
  dfsch_defconst_cstr(ctx, "or",DFSCH_MACRO_REF(or));
  dfsch_defconst_cstr(ctx, "when", DFSCH_MACRO_REF(when));
  dfsch_defconst_cstr(ctx, "unless", DFSCH_MACRO_REF(unless));
  dfsch_defconst_cstr(ctx, "cond", DFSCH_MACRO_REF(cond));

  dfsch_defconst_cstr(ctx, "lambda", DFSCH_MACRO_REF(lambda));
  dfsch_defconst_cstr(ctx, "define", DFSCH_MACRO_REF(define));
  dfsch_defconst_cstr(ctx, "define-variable", DFSCH_MACRO_REF(define_variable));
  dfsch_defconst_cstr(ctx, "define-constant", DFSCH_MACRO_REF(define_constant));
  dfsch_defconst_cstr(ctx, "define-macro", DFSCH_MACRO_REF(define_macro));

  dfsch_defconst_cstr(ctx, "let", DFSCH_MACRO_REF(let));
  dfsch_defconst_cstr(ctx, "let*", DFSCH_MACRO_REF(let_seq));
  dfsch_defconst_cstr(ctx, "letrec", DFSCH_MACRO_REF(letrec));

}
