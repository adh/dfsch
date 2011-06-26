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

extern dfsch_type_t dfsch_collections_bitvector_type;
#define DFSCH_COLLECTIONS_BITVECTOR_TYPE (&dfsch_collections_bitvector_type)

dfsch_object_t* dfsch_collections_make_bitvector(size_t length);
dfsch_object_t* dfsch_collections_list_2_bitvector(dfsch_object_t* values);

dfsch_object_t* dfsch_collections_bitvector_not(dfsch_object_t* bv);
dfsch_object_t* dfsch_collections_bitvector_or(dfsch_object_t* bva,
                                               dfsch_object_t* bvb);
dfsch_object_t* dfsch_collections_bitvector_and(dfsch_object_t* bva,
                                               dfsch_object_t* bvb);
dfsch_object_t* dfsch_collections_bitvector_xor(dfsch_object_t* bva,
                                                dfsch_object_t* bvb);
dfsch_object_t* dfsch_collections_bitvector_2_integer(dfsch_object_t* bv);
dfsch_object_t* dfsch_collections_integer_2_bitvector(dfsch_object_t* bv);
dfsch_object_t* dfsch_collections_bitvector_increment(dfsch_object_t* bv);

int dfsch_collections_bitvector_all_zeros_p(dfsch_object_t* bv);
int dfsch_collections_bitvector_all_ones_p(dfsch_object_t* bv);

#endif
