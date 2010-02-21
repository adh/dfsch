#ifndef H__dfsch__generate__
#define H__dfsch__generate__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_generate_make_macro(dfsch_object_t* proc_exp);
dfsch_object_t* dfsch_generate_lambda(dfsch_object_t* name,
                                      dfsch_object_t* lambda_list,
                                      dfsch_object_t* body);
dfsch_object_t* dfsch_generate_if(dfsch_object_t* cond,
                                  dfsch_object_t* cons,
                                  dfsch_object_t* alt);
dfsch_object_t* dfsch_generate_begin(dfsch_object_t* exps);
dfsch_object_t* dfsch_generate_let1(dfsch_object_t* bind,
                                    dfsch_object_t* exp);
dfsch_object_t* dfsch_generate_let(dfsch_object_t* bind,
                                   dfsch_object_t* exp);
dfsch_object_t* dfsch_generate_define_variable(dfsch_object_t* name,
                                               dfsch_object_t* value);
dfsch_object_t* dfsch_generate_define_constant(dfsch_object_t* name,
                                               dfsch_object_t* value);
dfsch_object_t* dfsch_generate_defined_p(dfsch_object_t* name);

dfsch_object_t* dfsch_generate_instance_p(dfsch_object_t* obj,
                                          dfsch_object_t* klass);
dfsch_object_t* dfsch_generate_error(char* message,
                                      dfsch_object_t* obj);

dfsch_object_t* dfsch_generate_cons(dfsch_object_t* car, dfsch_object_t* cdr);
dfsch_object_t* dfsch_generate_quote(dfsch_object_t* value);
dfsch_object_t* dfsch_get_append_primitive();


#endif
