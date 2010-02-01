#include <dfsch/compile.h>
#include <dfsch/magic.h>
#include "types.h"

dfsch_object_t* dfsch_cons_ast_node(dfsch_object_t* head,
                                    dfsch_object_t* orig_expr,
                                    size_t count,
                                    ...){
  size_t i = 0;
  dfsch_object_t** data;
  va_list al;

  va_start(al, count);


  data = GC_MALLOC(sizeof(dfsch_object_t*)*(count+5));
  data[i] = head;
  i++;
  
  for(i = 1; i < count+1; i++){
    data[i] = va_arg(al, dfsch_object_t*);
  }

  data[i] = DFSCH_INVALID_OBJECT;
  i++;
  data[i] = NULL; // CDR
  if (orig_expr){
    data[i+1] = DFSCH_SYM_COMPILED_FROM;
    data[i+2] = orig_expr;
  } else {
    data[i+1] = NULL;
    data[i+2] = NULL;
  }

  va_end(al);
  return DFSCH_MAKE_CLIST(data);  
}

dfsch_object_t* dfsch_cons_ast_node_cdr(dfsch_object_t* head,
                                        dfsch_object_t* orig_expr,
                                        dfsch_object_t* cdr,
                                        size_t count,
                                        ...){
  size_t i = 0;
  dfsch_object_t** data;
  va_list al;

  va_start(al, count);

  data = GC_MALLOC(sizeof(dfsch_object_t*)*(count+5));
  data[i] = head;
  i++;
  
  for(i = 1; i < count+1; i++){
    data[i] = va_arg(al, dfsch_object_t*);
  }

  data[i] = DFSCH_INVALID_OBJECT;
  i++;
  data[i] = cdr;
  if (orig_expr){
    data[i+1] = DFSCH_SYM_COMPILED_FROM;
    data[i+2] = orig_expr;
  } else {
    data[i+1] = NULL;
    data[i+2] = NULL;
  }

  va_end(al);
  return DFSCH_MAKE_CLIST(data);
  
}

dfsch_object_t* dfsch_constant_expression_value(dfsch_object_t* expression,
                                                dfsch_object_t* env){
  if (DFSCH_SYMBOL_P(expression)){
    return dfsch_variable_constant_value(expression,env);
  } else if (dfsch_quote_expression_p(expression)){
    return DFSCH_FAST_CAR(expression);
  } else if (DFSCH_PAIR_P(expression)){
    return DFSCH_INVALID_OBJECT;
  } else {
    return expression;
  }
}

dfsch_object_t* dfsch_make_constant_ast_node(dfsch_object_t* value){
  if (DFSCH_SYMBOL_P(value) || DFSCH_PAIR_P(value)){
    return dfsch_generate_quote(value);
  } else {
    return value;
  }
}

dfsch_object_t* dfsch_constant_fold_expression_list(dfsch_object_t* list,
                                                    dfsch_object_t* env){
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  dfsch_object_t *i =  list;

  head = tail = NULL;

  while(DFSCH_PAIR_P(i)){
    dfsch_object_t* tmp = 
      dfsch_cons(dfsch_constant_fold_expression(DFSCH_FAST_CAR(i), env),
                 NULL);
    if (head){
      DFSCH_FAST_CDR_MUT(tail) = tmp;
      tail = tmp;
    }else{
      head = tail = tmp;
    }
    i = DFSCH_FAST_CDR(i);
  }
  if (i && !DFSCH_PAIR_P(i)){
    dfsch_type_error(i, DFSCH_LIST_TYPE, 1);
  }

  return dfsch_list_annotate((dfsch_object_t*)head, 
                             DFSCH_SYM_COMPILED_FROM, list);  
  
}

dfsch_object_t* dfsch_constant_fold_expression(dfsch_object_t* expression,
                                               dfsch_object_t* env){
  if (DFSCH_PAIR_P(expression)){
    dfsch_object_t* operator = DFSCH_FAST_CAR(expression);
    dfsch_object_t* operator_value;
    dfsch_object_t* args = DFSCH_FAST_CDR(expression);

    operator = dfsch_constant_fold_expression(operator, env);
    operator_value = dfsch_constant_expression_value(operator, env);
    
    if (operator_value != DFSCH_INVALID_OBJECT){
      if (DFSCH_TYPE_OF(operator_value) == DFSCH_FORM_TYPE){
        dfsch_form_t* form = ((dfsch_form_t*)operator_value);
        if (form->methods.constant_fold){
          return form->methods.constant_fold(operator_value, expression, env);
        }
      }
      if (DFSCH_TYPE_OF(operator_value) == DFSCH_MACRO_TYPE){
        return dfsch_constant_fold_expression(dfsch_macro_expand(operator_value,
                                                                 args),
                                              env);
      }

      return dfsch_cons_ast_node_cdr(dfsch_make_constant_ast_node(operator_value),
                                     expression, 
                                     dfsch_constant_fold_expression_list(args,
                                                                         env),
                                     0);
    }
    return dfsch_cons_ast_node_cdr(operator, expression, 
                                   dfsch_constant_fold_expression_list(args,
                                                                       env),
                                   0);
    

  } else if (DFSCH_SYMBOL_P(expression)){
    return expression; // TODO
  } else {
    return expression;
  }
}


void dfsch_compile_function(dfsch_object_t* function){
  closure_t* func = DFSCH_ASSERT_INSTANCE(function, 
                                          DFSCH_STANDARD_FUNCTION_TYPE);

  func->code = dfsch_constant_fold_expression_list(func->orig_code,
                                                   func->env);
}

DFSCH_DEFINE_PRIMITIVE(constant_fold_expression, NULL){
  dfsch_object_t* expr;
  dfsch_object_t* env;

  DFSCH_OBJECT_ARG(args, expr);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);

  return dfsch_constant_fold_expression(expr, env);
}

DFSCH_DEFINE_PRIMITIVE(compile_function, NULL){
  dfsch_object_t* function;

  DFSCH_OBJECT_ARG(args, function);
  DFSCH_ARG_END(args);

  dfsch_compile_function(function);

  return function;
}

void dfsch__compile_register(dfsch_object_t *ctx){ 
  dfsch_defconst_pkgcstr(ctx, DFSCH_DFSCH_INTERNAL_PACKAGE, 
                         "constant-fold-expression", 
                         DFSCH_PRIMITIVE_REF(constant_fold_expression));  
  dfsch_defconst_cstr(ctx, "compile-function!",
                      DFSCH_PRIMITIVE_REF(compile_function));
}
