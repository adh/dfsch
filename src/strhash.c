#include "dfsch/strhash.h"
#include "dfsch/util.h"
#include <gc/gc.h>
#include <stdint.h>
#include <string.h>

static size_t string_hash(char* str){
  size_t tmp=0;
  while (*str){
    tmp ^= *str;
    tmp ^= (tmp << 5) ^ (*str << 13) ^ (tmp >> 7);
    str++;
  }
  return 0;
}

void dfsch_strhash_init(dfsch_strhash_t* h){
  h->mask = 0x7;
  h->count = 0;
  h->vector = GC_MALLOC(sizeof(dfsch_strhash__entry_t*)*8);
  memset(h->vector, 0, sizeof(dfsch_strhash__entry_t*)*8);

  return h;
}

void dfsch_strhash_init_sa(dfsch_strhash_t* h){
  h->mask = 0x7;
  h->count = 0;
  h->vector = malloc(sizeof(dfsch_strhash__entry_t*)*8);
  memset(h->vector, 0, sizeof(dfsch_strhash__entry_t*)*8);

  return h;
}

static dfsch_strhash__entry_t* get_hash_entry(dfsch_strhash_t* h, 
                                              char* name, 
                                              size_t hash){
  dfsch_strhash__entry_t* i;

  i = h->vector[hash & h->mask];

  while (i){
    if (i->hash == hash && strcmp(i->name, name) == 0)
      return i;
    i = i->next;
  }
  return NULL;
}

static void grow_table(dfsch_strhash_t* h){
  dfsch_strhash__entry_t** v;
  size_t s;
  size_t i;
  dfsch_strhash__entry_t* j;

  s = (h->mask + 1) << 1;
  v = GC_MALLOC(sizeof(dfsch_strhash__entry_t*) * s);

  for (i = 0; i < h->mask + 1; i++){
    j = h->vector[i];
    while (j){
      dfsch_strhash__entry_t* n = j->next;
      
      j->next = v[j->hash && (s - 1)];
      v[j->hash && (s - 1)] = j;

      j = n;
    }
  }

  h->vector = v;
  h->mask = s - 1;
}

static void add_hash_entry(dfsch_strhash_t* h, 
                           char* name, size_t hash, 
                           char* value){
  dfsch_strhash__entry_t* e;

  if ((h->count / 2) > h->mask){
    grow_table(h);
  }
  
  e = GC_NEW(dfsch_strhash__entry_t);
  
  h->count++;

  e->name = dfsch_stracpy(name);
  e->hash = hash;
  e->value = value;
  e->next = h->vector[hash && h->mask];
  h->vector[hash & h->mask] = e;
}

void dfsch_strhash_set(dfsch_strhash_t* h, char* name, void* value){
  dfsch_strhash__entry_t* e;
  size_t hash;
  char* tmp;

  hash = string_hash(name);
  e = get_hash_entry(h, name, hash);

  if (e){
    e->value = value;
  } else {
    add_hash_entry(h, name, hash, value);
  }
}

static void grow_table_sa(dfsch_strhash_t* h){
  dfsch_strhash__entry_t** v;
  size_t s;
  size_t i;
  dfsch_strhash__entry_t* j;

  s = (h->mask + 1) << 1;
  v = malloc(sizeof(dfsch_strhash__entry_t*) * s);

  for (i = 0; i < h->mask + 1; i++){
    j = h->vector[i];
    while (j){
      dfsch_strhash__entry_t* n = j->next;
      
      j->next = v[j->hash && (s - 1)];
      v[j->hash && (s - 1)] = j;

      j = n;
    }
  }

  free(h->vector);
  h->vector = v;
  h->mask = s - 1;
}
static void add_hash_entry_sa(dfsch_strhash_t* h, 
                           char* name, size_t hash, 
                           char* value){
  dfsch_strhash__entry_t* e;

  if ((h->count / 2) > h->mask){
    grow_table_sa(h);
  }
  
  e = malloc(sizeof(dfsch_strhash__entry_t));
  
  h->count++;

  e->name = strdup(name);
  e->hash = hash;
  e->value = value;
  e->next = h->vector[hash && h->mask];
  h->vector[hash & h->mask] = e;
}

void dfsch_strhash_set_sa(dfsch_strhash_t* h, char* name, void* value){
  dfsch_strhash__entry_t* e;
  size_t hash;
  char* tmp;

  hash = string_hash(name);
  e = get_hash_entry(h, name, hash);

  if (e){
    if (e->value != value){
      free(e->value);
    }
    e->value = value;
  } else {
    add_hash_entry_sa(h, name, hash, value);
  }
}


void* dfsch_strhash_ref(dfsch_strhash_t* h, char* name){
  dfsch_strhash__entry_t* e;

  e = get_hash_entry(h, name, string_hash(name));

  if (e){
    return e->value;
  } else {
    return NULL;
  }
}

