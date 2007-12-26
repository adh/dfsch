#ifndef H__dfsch__generic__
#define H__dfsch__generic__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_make_generic(char* name);

void dfsch_method_set(dfsch_object_t* generic,
                      dfsch_object_t* type,
                      dfsch_object_t* method);
void dfsch_method_unset(dfsch_object_t* generic,
                        dfsch_object_t* type);
dfsch_object_t* dfsch_method_ref(dfsch_object_t* generic,
                                 dfsch_object_t* type);
dfsch_object_t* dfsch_methods_2_alist(dfsch_object_t* generic);


#endif
