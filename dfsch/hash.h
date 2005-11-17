#ifndef H__dfsch__hash__
#define H__dfsch__hash__

#include <dfsch/dfsch.h>

extern dfsch_object_t* dfsch_hash_make();
extern int dfsch_hash_p(dfsch_object_t* obj);
extern dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash, 
                                      dfsch_object_t* key);
extern dfsch_object_t* dfsch_hash_set(dfsch_object_t* hash,
                                      dfsch_object_t* key,
                                      dfsch_object_t* value);
extern dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash,
                                        dfsch_object_t* key);
extern dfsch_object_t* dfsch_hash_set_if_exists(dfsch_object_t* hash,
                                                dfsch_object_t* key,
                                                dfsch_object_t* value);


extern dfsch_object_t* dfsch_hash_2_alist(dfsch_object_t* hash_obj);

#endif
