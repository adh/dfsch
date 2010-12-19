#ifndef H__dfsch__strhash__
#define H__dfsch__strhash__

#include <stdlib.h>

typedef struct dfsch_strhash_t dfsch_strhash_t;
typedef struct dfsch_strhash__entry_t dfsch_strhash__entry_t;

struct dfsch_strhash_t {
  dfsch_strhash__entry_t** vector;
  size_t mask;
  size_t count;
};

struct dfsch_strhash__entry_t {
  size_t hash;
  char* name;
  char* value;
  dfsch_strhash__entry_t* next;
};

void dfsch_strhash_init(dfsch_strhash_t* h);
void dfsch_strhash_set(dfsch_strhash_t* sh,
                       char* key, void* value);
void* dfsch_strhash_ref(dfsch_strhash_t* sh,
                        char* key);

void dfsch_strhash_init_sa(dfsch_strhash_t* h);
void dfsch_strhash_set_sa(dfsch_strhash_t* sh,
                          char* key, void* value);


#endif
