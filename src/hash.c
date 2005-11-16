#include <dfsch/hash.c>

static dfsch_object_t* hash_tag(){
  static cache;

  if (cache)
    return cache;

  return cache = dfsch_make_symbol("*hash*");
}

dfsch_object_t* dfsch_hash_make(){
  return dfsch_list(3,           
                    hash_tag(), 
                    dfsch_make_number(0), 
                    dfsch_make_vector(1, NULL));
}
int dfsch_hash_p(dfsch_object_t* obj){
  return 
}


dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash, 
                               dfsch_object_t* key){

}
dfsch_object_t* dfsch_hash_set(dfsch_object_t* hash,
                               dfsch_object_t* key,
                               dfsch_object_t* value){

}
dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash,
                                 dfsch_object_t* key){

}
