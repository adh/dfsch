#ifndef H__dfsch__compile__
#define H__dfsch__compile__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_compile(dfsch_object_t* expr,
                              dfsch_object_t* env,
                              int depth);
void dfsch_compile_function(dfsch_object_t* function);


dfsch_object_t* dfsch_compile_list(dfsch_object_t* list,
                                   dfsch_object_t* env,
                                   int depth);
dfsch_object_t* dfsch_compile_llist(dfsch_object_t* llist,
                                    dfsch_object_t* env,
                                    int depth);
dfsch_object_t* dfsch_compile_alist(dfsch_object_t* alist,
                                    dfsch_object_t* env,
                                    int depth);
dfsch_object_t* dfsch_compile_progn(dfsch_object_t* list,
                                    dfsch_object_t* env,
                                    int depth);

dfsch_object_t* dfsch_form_compiler_eval_all(dfsch_form_t* form,
                                             dfsch_object_t* env,
                                             dfsch_object_t* args,
                                             int depth);

dfsch_object_t* dfsch_form_compiler_eval_but_first(dfsch_form_t* form,
                                                   dfsch_object_t* env,
                                                   dfsch_object_t* args,
                                                   int depth);
dfsch_object_t* dfsch_form_compiler_let(dfsch_form_t* form,
                                        dfsch_object_t* env,
                                        dfsch_object_t* args,
                                        int depth);
dfsch_object_t* dfsch_form_compiler_cond(dfsch_form_t* form,
                                         dfsch_object_t* env,
                                         dfsch_object_t* args,
                                         int depth);
dfsch_object_t* dfsch_form_compiler_case(dfsch_form_t* form,
                                         dfsch_object_t* env,
                                         dfsch_object_t* args,
                                         int depth);

#endif
