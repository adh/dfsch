#ifndef H__dfsch__wrapper__
#define H__dfsch__wrapper__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_make_wrapper_type(char* name,
                                                 dfsch_object_t* write,
                                                 dfsch_object_t* equal_p,
                                                 dfsch_object_t* apply);
  
  extern dfsch_object_t* dfsch_wrap(dfsch_object_t* type,
                                    dfsch_object_t* object);
  extern dfsch_object_t* dfsch_unwrap(dfsch_object_t* type,
                                      dfsch_object_t* wrapper);

#ifdef __cplusplus
}
#endif

#endif
