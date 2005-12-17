#include <dfsch/hash.h>
#include <stdlib.h>


typedef struct hash_entry_t hash_entry_t;
typedef struct hash_t{

  dfsch_object_t* proc;
  size_t count;
  size_t mask;
  hash_entry_t** vector;

}hash_t;

struct hash_entry_t {
  size_t hash;
  dfsch_object_t* key;
  dfsch_object_t* value;

  hash_entry_t* next;
};

static dfsch_object_t* hash_type(){
  static dfsch_object_t* cache = NULL;

  if (cache)
    return cache;

  return cache = dfsch_make_symbol("hash");
}

static void alloc_vector(hash_t* hash){
  hash->vector = GC_MALLOC(sizeof(hash_entry_t)*(hash->mask+1));
}

dfsch_object_t* dfsch_hash_make(dfsch_object_t* hash_proc){
  hash_t *h = GC_NEW(hash_t); 

  if (hash_proc && !dfsch_object_procedure_p(hash_proc))
    DFSCH_THROW("exception:not-a-procedure", hash_proc);

  h->proc = hash_proc;
  h->count = 0;
  h->mask = 0x03;
  alloc_vector(h);

  return dfsch_make_native_data(h, hash_type());
}
int dfsch_hash_p(dfsch_object_t* obj){
  return dfsch_native_data_type(obj) == hash_type();
}

static size_t get_hash(hash_t* hash, dfsch_object_t*key){
  
  if (hash->proc){
    return (size_t)dfsch_number(dfsch_apply(hash->proc,dfsch_list(1,key)));
  }else{

    /*
     * We don't have any procedure for computing hashes - so we will
     * compute something based on object pointer.
     */

    size_t a = (size_t)key;        
    size_t b = (size_t)key >> 16;

    a ^= b >> 2;
    b ^= a >> 3;
    a ^= b >> 5;
    b ^= a >> 7;
    a ^= b >> 11;
    b ^= a >> 13;
    a ^= b >> 17;
    b ^= a >> 23;

    return b ^ a;
  }
}

#define GET_HASH(obj,hash)\
   hash = dfsch_native_data(obj, hash_type());\
   if (!hash)\
     DFSCH_THROW("exception:not-a-hash", obj)

dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash_obj, 
                               dfsch_object_t* key){
  
  size_t h;
  hash_t *hash;
  hash_entry_t *i;

  GET_HASH(hash_obj, hash);

  h = get_hash(hash, key);  
  i = hash->vector[h & hash->mask];
  
  while (i){
    if (h = i->hash && dfsch_eq_p(i->key, key))
      return dfsch_list(1,i->value);
    
    i = i->next;
  }

  return NULL;
}

static hash_entry_t* alloc_entry(size_t hash, 
                                 dfsch_object_t* key,
                                 dfsch_object_t* value,
                                 hash_entry_t* next){
  hash_entry_t *e = GC_NEW(hash_entry_t);
  e->hash = hash;
  e->key = key;
  e->value = value;
  e->next = next;
  return e;
}

dfsch_object_t* dfsch_hash_set(dfsch_object_t* hash_obj,
                               dfsch_object_t* key,
                               dfsch_object_t* value){
  size_t h, len, count;
  hash_t *hash;
  hash_entry_t *entry;
  hash_entry_t *i;

  GET_HASH(hash_obj,hash);

  h = get_hash(hash, key);  
  i = entry = hash->vector[h & hash->mask];

  while (i){
    if (h == i->hash && dfsch_eq_p(i->key, key)){
      i->value = value;
      return hash_obj;
    }
    
    i = i->next;
  }

  
  // It isn't here, so we will add new item

  hash->count++;
  if (hash->count > (hash->mask+1)*2){ // Should table grow?
    int j;
    hash_entry_t **vector = hash->vector; // Save pointer to old contents
    size_t ol = hash->mask+1;
    hash->mask = ((hash->mask+1) * 2) - 1;
    alloc_vector(hash);

    for (j=0; j<ol; j++){
      i = vector[j];
      while (i){
        hash_entry_t *next = i->next;
        size_t h = i->hash;
        i->next = hash->vector[h & hash->mask];
        hash->vector[h & hash->mask] = i;
        i = next;
      }
    }
  }

  hash->vector[h & hash->mask] = alloc_entry(h,
                                             key,
                                             value,
                                             hash->vector[h & hash->mask]);
  
  return hash_obj;
}
dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash,
                                 dfsch_object_t* key){
  // TODO
}
dfsch_object_t* dfsch_hash_set_if_exists(dfsch_object_t* hash_obj, 
                                         dfsch_object_t* key,
                                         dfsch_object_t* value){
  
  size_t h;
  hash_t *hash;
  hash_entry_t *i;

  GET_HASH(hash_obj, hash);

  h = get_hash(hash, key);  
  i = hash->vector[h & hash->mask];
  
  while (i){
    if (h = i->hash && dfsch_eq_p(i->key, key)){
      i->value = value;
      return dfsch_list(1,value);
    }
    i = i->next;
  }

  return NULL;
}
dfsch_object_t* dfsch_hash_2_alist(dfsch_object_t* hash_obj){
  dfsch_object_t *alist = NULL;
  int j;
  hash_entry_t *i;
  hash_t *hash;
  
  GET_HASH(hash_obj, hash);
  
  for (j=0; j<(hash->mask+1); j++){
    i = hash->vector[j];
    while (i){
      alist = dfsch_cons(dfsch_list(2,
                                    i->key,
                                    i->value), 
                         alist);
      i = i->next;
    }
  }

  return alist;
}
