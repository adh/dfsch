#ifndef H__dfsch__mchash__
#define H__dfsch__mchash__

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>

#define DFSCH_MKHASH_TYPE ((dfsch_type_t*)&dfsch_mkhash_type)
extern dfsch_type_t dfsch_mkhash_type;

typedef struct dfsch_mkhash_t dfsch_mkhash_t;

dfsch_mkhash_t* dfsch_make_mkhash(size_t num_keys,
                                  int eqp);
int dfsch_mkhash_ref(dfsch_mkhash_t* hash,
                     dfsch_object_t** keys,
                     dfsch_object_t** result);
void dfsch_mkhash_set(dfsch_mkhash_t* hash,
                      dfsch_object_t** keys,
                      dfsch_object_t* value);
void dfsch_mkhash_unset(dfsch_mkhash_t* hash,
                        dfsch_object_t** keys);
dfsch_object_t* dfsch_mkhash_2_alist(dfsch_mkhash_t* hash);

#endif 
