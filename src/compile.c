#include "dfsch/compile.h"
#include "types.h"

dfsch_object_t* dfsch_compile(dfsch_object_t* expr,
                              dfsch_object_t* env,
                              int depth){
  dfsch_object_t* first;

  if (depth <= 0){
    return expr;
  }

  if (!expr || !dfsch_list_p(expr)){
    return expr;
  }
  
  first = dfsch_car(expr);

  if (!dfsch_symbol_p(first)){
    return expr;
  }

  first = dfsch_env_get(first, env);
  if (!first){
    return expr;
  } else {
    first = dfsch_car(first);
  }

  if (dfsch_macro_p(first)){
    return dfsch_compile(dfsch_macro_expand(first,
                                            dfsch_cdr(expr)),
                         env,
                         depth - 1);
  } else if (dfsch_form_p(first)){
    if (((dfsch_form_t*)first)->compile){
      return ((dfsch_form_t*)first)->compile(((dfsch_form_t*)first),
                                             env,
                                             dfsch_cdr(expr),
                                             depth - 1);
    } else {
      return dfsch_cons(first, dfsch_cdr(expr));
    }
  } else if (dfsch_primitive_p(first)){
    if (((dfsch_primitive_t*)first)->flags & DFSCH_PRIMITIVE_CACHED){
      return dfsch_cons(first, 
                        dfsch_compile_list(dfsch_cdr(expr),
                                           env,
                                           depth - 1));
    } else {
      return dfsch_cons(dfsch_car(expr), 
                        dfsch_compile_list(dfsch_cdr(expr),
                                           env,
                                           depth - 1));
    }
  } else if (dfsch_procedure_p(first)){
    return dfsch_cons(dfsch_car(expr), 
                      dfsch_compile_list(dfsch_cdr(expr),
                                         env,
                                         depth - 1));
    
  } else {
    return expr;
  }  
}

dfsch_object_t* dfsch_compile_list(dfsch_object_t* list,
                                   dfsch_object_t* env,
                                   int depth){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail = NULL;
  dfsch_object_t* tmp;

  if (depth <= 0){
    return list;
  }

  while (dfsch_pair_p(list)){
    tmp = dfsch_cons(dfsch_compile(dfsch_car(list),
                                   env,
                                   depth), NULL);

    if (head){
      dfsch_set_cdr(tail, tmp);
      tail = tmp;
    } else {
      head = tail = tmp;
    }

    list = dfsch_cdr(list);
  }
  if (tail){
    dfsch_set_cdr(tail, list); /* When there is something non null? */
  } else {
    return list;
  }

  return head;
}

dfsch_object_t* dfsch_compile_llist(dfsch_object_t* llist,
                                    dfsch_object_t* env,
                                    int depth){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail = NULL;
  dfsch_object_t* tmp;
  dfsch_object_t* list;

  if (depth <= 0){
    return llist;
  }

  while (dfsch_pair_p(llist)){
    list = dfsch_car(llist);
    
    if (dfsch_pair_p(list)){
      tmp = dfsch_cons(dfsch_compile_list(list,
                                          env,
                                          depth), NULL);
    } else {
      tmp = dfsch_cons(list, NULL);
      
    }

    if (head){
      dfsch_set_cdr(tail, tmp);
      tail = tmp;
    } else {
      head = tail = tmp;
    }

    llist = dfsch_cdr(llist);
  }

  if (tail){
    dfsch_set_cdr(tail, llist); /* When there is something non null? */
  } else {
    return llist;
  }

  return head;
}


dfsch_object_t* dfsch_compile_alist(dfsch_object_t* alist,
                                   dfsch_object_t* env,
                                   int depth){
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail = NULL;
  dfsch_object_t* tmp;
  dfsch_object_t* list;

  if (depth <= 0){
    return alist;
  }

  while (dfsch_pair_p(alist)){
    list = dfsch_car(alist);
    
    if (dfsch_pair_p(list)){
      tmp = dfsch_cons(dfsch_cons(dfsch_car(list),
                                  dfsch_compile_list(dfsch_cdr(list),
                                                     env,
                                                     depth)), NULL);
    } else {
      tmp = dfsch_cons(list, NULL);
      
    }

    if (head){
      dfsch_set_cdr(tail, tmp);
      tail = tmp;
    } else {
      head = tail = tmp;
    }

    alist = dfsch_cdr(alist);
  }

  if (tail){
    dfsch_set_cdr(tail, alist); /* When there is something non null? */
  } else {
    return alist;
  }

  return head;
}


DFSCH_DEFINE_FORM_IMPL(compiler_internal_progn, dfsch_form_compiler_eval_all){
  return dfsch_eval_proc_tr(args, env, form, esc);
}

dfsch_object_t* dfsch_compile_progn(dfsch_object_t* list,
                                    dfsch_object_t* env,
                                    int depth){
  return dfsch_cons(DFSCH_FORM_REF(compiler_internal_progn),
                    dfsch_compile_list(list, env, depth - 1));
}

void dfsch_compile_function(dfsch_object_t* function){
  if (!dfsch_closure_p(function)){
    dfsch_error("exception:not-a-function", function);
  }

  ((closure_t*)function)->code = 
    dfsch_compile_list(((closure_t*)function)->orig_code,
                       ((closure_t*)function)->env, 1024);
}



dfsch_object_t* dfsch_form_compiler_eval_all(dfsch_form_t* form,
                                             dfsch_object_t* env,
                                             dfsch_object_t* args,
                                             int depth){
  return dfsch_cons((dfsch_object_t*) form,
                    dfsch_compile_list(args, env, depth - 1));
}

dfsch_object_t* dfsch_form_compiler_eval_but_first(dfsch_form_t* form,
                                                  dfsch_object_t* env,
                                                  dfsch_object_t* args,
                                                  int depth){
  if (!dfsch_pair_p(args)){
    return dfsch_cons(form, args);
  }

  return dfsch_cons((dfsch_object_t*) form,
                    dfsch_cons(dfsch_car(args),
                               dfsch_compile_list(dfsch_cdr(args), 
                                                  env, 
                                                  depth - 1)));
}

dfsch_object_t* dfsch_form_compiler_let(dfsch_form_t* form,
                                        dfsch_object_t* env,
                                        dfsch_object_t* args,
                                        int depth){

  if (!dfsch_pair_p(args)){
    return dfsch_cons(form, args);
  }

  return dfsch_cons((dfsch_object_t*) form,
                    dfsch_cons(dfsch_compile_alist(dfsch_car(args),
                                                   env,
                                                   depth),
                               dfsch_compile_list(dfsch_cdr(args), 
                                                  env, 
                                                  depth - 1)));
}

dfsch_object_t* dfsch_form_compiler_cond(dfsch_form_t* form,
                                         dfsch_object_t* env,
                                         dfsch_object_t* args,
                                         int depth){
  return dfsch_cons((dfsch_object_t*) form,
                    dfsch_compile_llist(args, env, depth - 1));
}
dfsch_object_t* dfsch_form_compiler_case(dfsch_form_t* form,
                                         dfsch_object_t* env,
                                         dfsch_object_t* args,
                                         int depth){
  return dfsch_cons((dfsch_object_t*) form,
                    dfsch_cons(dfsch_compile(dfsch_car(args),
                                             env,
                                             depth - 1),
                               dfsch_compile_alist(dfsch_cdr(args), 
                                                   env, 
                                                   depth - 1)));
}
