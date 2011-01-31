#ifndef H__dfsch__compile__
#define H__dfsch__compile__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_cons_ast_node(dfsch_object_t* head,
                                    dfsch_object_t* orig_expr,
                                    size_t count,
                                    ...);
dfsch_object_t* dfsch_cons_ast_node_cdr(dfsch_object_t* head,
                                        dfsch_object_t* orig_expr,
                                        dfsch_object_t* cdr,
                                        size_t count,
                                        ...);
dfsch_object_t* dfsch_compile_expression_list(dfsch_object_t* list,
                                              dfsch_object_t* env);
dfsch_object_t* dfsch_compile_expression(dfsch_object_t* expression,
                                         dfsch_object_t* env);


void dfsch_compile_function(dfsch_object_t* function);

#endif
