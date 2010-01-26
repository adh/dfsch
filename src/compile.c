#include <dfsch/compile.h>
#include <dfsch/magic.h>

dfsch_object_t* dfsch_cons_ast_node(dfsch_object_t* head,
                                    dfsch_object_t* orig_expr,
                                    size_t count,
                                    ...){
  size_t i;
  dfsch_object_t** data;
  va_list al;

  va_start(al, count);


  data = GC_MALLOC(sizeof(dfsch_object_t*)*(count+4));
  data[i] = head;
  i++;
  
  for(i = 0; i < count; i++){
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
  size_t i;
  dfsch_object_t** data;
  va_list al;

  va_start(al, count);

  data = GC_MALLOC(sizeof(dfsch_object_t*)*(count+4));
  data[i] = head;
  i++;
  
  for(i = 0; i < count; i++){
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

  return (dfsch_object_t*)head;  
  
}

dfsch_object_t* dfsch_constant_fold_expression(dfsch_object_t* expression,
                                               dfsch_object_t* env){
  if (DFSCH_PAIR_P(expression)){
    dfsch_object_t* operator = DFSCH_FAST_CAR(expression);
    dfsch_object_t* args = DFSCH_FAST_CDR(expression);

    operator = dfsch_constant_fold_expression(operator, env);
    
    
  } else if (DFSCH_SYMBOL_P(expression)){
    return expression; // TODO
  } else {
    return expression;
  }
}


void dfsch_compile_function(dfsch_object_t* function){
  
}
