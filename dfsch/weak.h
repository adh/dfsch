#ifndef H__dfsch__weak__
#define H__dfsch__weak__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_make_weak_reference(dfsch_object_t* refered);
  
  extern int dfsch_weak_reference_live_p(dfsch_object_t* reference);
  extern dfsch_object_t* dfsch_weak_reference_dereference(dfsch_object_t* reference);

  extern dfsch_object_t* dfsch_make_weak_vector(size_t length, 
                                                dfsch_object_t* fill);
  extern size_t dfsch_weak_vector_length(dfsch_object_t *vector);
  extern dfsch_object_t** dfsch_weak_vector_as_array(dfsch_object_t *vector, 
                                                     size_t *length);
  extern dfsch_object_t* dfsch_weak_vector_from_array(dfsch_object_t **array, 
                                                      size_t length);
  extern dfsch_object_t* dfsch_weak_vector_ref(dfsch_object_t *vector, 
                                               size_t k);
  extern dfsch_object_t* dfsch_weak_vector_set(dfsch_object_t* vector, 
                                               size_t k, 
                                               dfsch_object_t* obj);

#ifdef __cplusplus
}
#endif

#endif
