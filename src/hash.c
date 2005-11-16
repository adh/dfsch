#include <dfsch/hash.h>

#include <stdlib.h>

static dfsch_object_t* hash_tag(){
  static cache = NULL;

  if (cache)
    return cache;

  return cache = dfsch_make_symbol("*hash*");
}

dfsch_object_t* dfsch_hash_make(dfsch_object_t* hash_proc){
  return dfsch_vector(4,           
                      hash_tag(),
                      hash_proc,
                      dfsch_make_number(0), 
                      dfsch_make_vector(4, NULL));
}
int dfsch_hash_p(dfsch_object_t* obj){
  return dfsch_object_vector_p(obj) && 
    (dfsch_vector_ref(obj, 0) == hash_tag());
}

static size_t get_hash(dfsch_object_t* hash, dfsch_object_t*key){
  dfsch_object_t* proc = dfsch_vector_ref(hash, 1);
  
  if (proc){
    return (size_t)dfsch_number(dfsch_apply(proc,dfsch_list(1,key)));
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

#define CHECK_HASH(h)\
  if (!dfsch_hash_p(h))\
    DFSCH_THROW("exception:not-a-hash", h)

dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash, 
                               dfsch_object_t* key){
  
  size_t h, len;
  dfsch_object_t *entry;
  dfsch_object_t *vector;

  CHECK_HASH(hash);

  h = get_hash(hash, key);
  vector = dfsch_vector_ref(hash, 3);
  len = dfsch_vector_length(vector);
  
  entry = dfsch_vector_ref(vector, h % len);
  
  while (entry){
    if (dfsch_car(dfsch_car(entry)) == key)
      return dfsch_list(1,dfsch_cdr(dfsch_car(entry)));
    
    entry = dfsch_cdr(entry);
  }

  return NULL;
}
dfsch_object_t* dfsch_hash_set(dfsch_object_t* hash,
                               dfsch_object_t* key,
                               dfsch_object_t* value){
  size_t h, len, count;
  dfsch_object_t *entry;
  dfsch_object_t *i;
  dfsch_object_t *vector;

  CHECK_HASH(hash);

  h = get_hash(hash, key);
  vector = dfsch_vector_ref(hash, 3);
  count = (size_t)dfsch_number(dfsch_vector_ref(hash,2));
  len = dfsch_vector_length(vector);
  
  i = entry = dfsch_vector_ref(vector, h % len);
  
  while (i){
    if (dfsch_car(dfsch_car(i)) == key){
      return dfsch_set_cdr(dfsch_car(i), value);
    }
    i = dfsch_cdr(i);
  }

  // It isn't here, so we will add new item

  dfsch_vector_set(hash, 2, dfsch_make_number(++count));
  if (count > len*2){ // Should table grow?
    int i;
    dfsch_object_t* nv = dfsch_make_vector(len*2,NULL);

    for (i=0; i<len; i++){
      dfsch_object_t* item = dfsch_vector_ref(vector, i);
      while (item){
        dfsch_object_t* cons = dfsch_car(item);
        dfsch_object_t* tmp;
        size_t ih = get_hash(hash, dfsch_car(cons));
        
        dfsch_vector_set(nv, ih % (len * 2), 
                         dfsch_cons(cons,
                                    dfsch_vector_ref(nv,
                                                     ih % (len * 2))));
        
        item = dfsch_cdr(item);
      }
    }

    vector = nv;
    dfsch_vector_set(hash,3,vector);
    len *= 2;

  }

  dfsch_vector_set(vector, 
                   h % len, 
                   dfsch_cons(dfsch_cons(key, 
                                         value), 
                              entry));

  return value;
}
dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash,
                                 dfsch_object_t* key){

}
