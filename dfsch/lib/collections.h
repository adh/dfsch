#ifndef H__dfsch_lib__collections__
#define H__dfsch_lib__collections__

#include <dfsch/dfsch.h>

extern dfsch_type_t dfsch_collections_priority_queue_type;
#define DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE \
  (&dfsch_collections_priority_queue_type)

dfsch_object_t* dfsch_collections_make_priority_queue(dfsch_object_t* lt);
void dfsch_collections_priority_queue_push(dfsch_object_t* q,
                                           dfsch_object_t* o);
dfsch_object_t* dfsch_collections_priority_queue_pop(dfsch_object_t* q);
int dfsch_collections_priority_queue_empty_p(dfsch_object_t* q);


#endif
