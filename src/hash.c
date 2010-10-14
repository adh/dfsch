/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Hash tables
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <dfsch/hash.h>
#include <dfsch/serdes.h>
#include "internal.h"
#include <stdlib.h>
#include "util.h"

#include <stdio.h>

/*
 * Initial size of hash - 1. Must be 2^n-1 for some integer n 
 *
 * This probably has no significant performance relevance other that it should
 * be larger than FH_DEPTH (if defined), values lower than 0x03 probably do 
 * not make sense anywhere.
 */
#define INITIAL_MASK 0x07

typedef struct hash_entry_t hash_entry_t;

typedef struct hash_t{
  dfsch_type_t* type;
  int equal;
  hash_entry_t** vector;
  size_t count;
  size_t mask;
  dfsch_rwlock_t lock;
}hash_t;

struct hash_entry_t {
  size_t hash;
  dfsch_object_t* key;
  dfsch_object_t* value;

  hash_entry_t* next;
};

static dfsch_collection_methods_t hash_table_col = {
  .get_iterator = dfsch_hash_2_alist
};
static dfsch_mapping_methods_t hash_table_map = {
  .ref = dfsch_hash_ref,
  .set = dfsch_hash_set,
  .unset = dfsch_hash_unset,
  .set_if_exists = dfsch_hash_set_if_exists,
};

static void hash_serialize(hash_t* h, dfsch_serializer_t* s){
  int j;
  hash_entry_t *i;

  DFSCH_RWLOCK_RDLOCK(&h->lock);
  dfsch_serialize_stream_symbol(s, "hash-table");
  dfsch_serialize_integer(s, h->equal);

  for (j=0; j<(h->mask+1); j++){
    i = h->vector[j];
    while (i){
      dfsch_serialize_integer(s, 1);
      dfsch_serialize_object(s, i->key);
      dfsch_serialize_object(s, i->value);
      i = i->next;
    }
  }
  dfsch_serialize_integer(s, 0);
  DFSCH_RWLOCK_UNLOCK(&h->lock);
}

DFSCH_DEFINE_DESERIALIZATION_HANDLER("hash-table", hash_table){
  int equal = dfsch_deserialize_integer(ds);
  dfsch_object_t* hash = dfsch_hash_make(equal 
                                         ? DFSCH_HASH_EQUAL 
                                         : DFSCH_HASH_EQ);
  dfsch_deserializer_put_partial_object(ds, hash);
  while (dfsch_deserialize_integer(ds) == 1){
    dfsch_object_t* key = dfsch_deserialize_object(ds);
    dfsch_object_t* value = dfsch_deserialize_object(ds);
    dfsch_hash_set(hash, key, value);
  }
  return hash;
}

dfsch_type_t dfsch_hash_table_type = {
  DFSCH_STANDARD_TYPE,
  NULL, //DFSCH_MAPPING_TYPE,
  sizeof(hash_t),
  "hash-table",
  NULL,
  NULL,
  NULL,

  .collection = &hash_table_col,
  .mapping = &hash_table_map,
  .serialize = hash_serialize,
};


static hash_entry_t** alloc_vector(size_t mask){
  return GC_MALLOC((sizeof(hash_entry_t*))*(mask+1));
}

#ifdef DFSCH_THREADS_FINALIZE
static void hash_finalizer(hash_t* h, void* cd) {
  DFSCH_DESTROY_RWLOCK(&h->lock);
}
#endif

dfsch_object_t* dfsch_hash_make(int mode){
  hash_t *h = (hash_t*)dfsch_make_object(DFSCH_HASH_TABLE_TYPE); 

  h->count = 0;
  h->mask = INITIAL_MASK;
  h->vector = alloc_vector(h->mask);
  h->equal = mode != DFSCH_HASH_EQ;
  DFSCH_INIT_RWLOCK(&h->lock);
#ifdef DFSCH_THREADS_FINALIZE
  GC_REGISTER_FINALIZER_NO_ORDER(h, (GC_finalization_proc)hash_finalizer,
                                 NULL, NULL, NULL);
#endif
  return (dfsch_object_t*)h;
}
int dfsch_hash_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_HASH_TABLE_TYPE);
}


#define HASH(hash, key) ((hash)->equal?dfsch_hash((key))\
                         :((((size_t)key) >> 3)) ^ (((size_t)key) >> 15))


#define GET_HASH(obj,hash)                                              \
  hash = DFSCH_ASSERT_INSTANCE(obj, DFSCH_HASH_TABLE_TYPE)


int dfsch_hash_ref_fast(dfsch_object_t* hash_obj,
                        dfsch_object_t* key,
                        dfsch_object_t** res){
  size_t h;
  hash_t *hash;
  hash_entry_t *i;
  int j;

  GET_HASH(hash_obj, hash);

  DFSCH_RWLOCK_RDLOCK(&hash->lock);
  h = HASH(hash, key);

  i = hash->vector[h & hash->mask];

  if (hash->equal){
    while (i){
      if (h == i->hash && dfsch_equal_p(i->key, key)){
        *res = i->value;
        DFSCH_RWLOCK_UNLOCK(&hash->lock);
        return 1;
      }
      i = i->next;
    }
  } else { 
    while (i){
      if (h == i->hash && i->key == key){
        *res = i->value;
        DFSCH_RWLOCK_UNLOCK(&hash->lock);
        return 1;
      }
      i = i->next;
    }
  }

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return 0;
}

dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash_obj, 
                               dfsch_object_t* key){
  dfsch_object_t* res;

  if (dfsch_hash_ref_fast(hash_obj, key, &res)){
    return dfsch_list(1, res);
  } else {
    return NULL;
  }
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

static void hash_change_size(hash_t* hash, size_t new_mask){
  int j;
  hash_entry_t *i;
  hash_entry_t **vector = alloc_vector(new_mask);
  for (j = 0; j <= hash->mask; j++){
    i = hash->vector[j];
    while (i){
      hash_entry_t *next = i->next;
      size_t h = i->hash;
      if (i->next != vector[h & new_mask]){
        vector[h & new_mask] = alloc_entry(h,
                                           i->key,
                                           i->value,
                                           vector[h & new_mask]);
      } else {
        vector[h & new_mask] = i;
      }
      i = next;
    }
  }

  hash->mask = new_mask;
  hash->vector = vector;
}


void dfsch_hash_put(dfsch_object_t* hash_obj,
                    dfsch_object_t* key,
                    dfsch_object_t* value){
  hash_t *hash;
  size_t h;
  int j;

  GET_HASH(hash_obj, hash);

  h = HASH(hash, key); 

  DFSCH_RWLOCK_WRLOCK(&hash->lock);

  hash->count++;
  if (hash->count > (hash->mask+1)){ // Should table grow?
    hash_change_size(hash, ((hash->mask+1) * 2) - 1);
  }

  hash->vector[h & hash->mask] = alloc_entry(h,
                                             key,
                                             value,
                                             hash->vector[h & hash->mask]);
  
  DFSCH_RWLOCK_UNLOCK(&hash->lock);  
}

void dfsch_hash_set(dfsch_object_t* hash_obj,
                    dfsch_object_t* key,
                    dfsch_object_t* value){
  size_t h;
  hash_t *hash;
  hash_entry_t *entry;
  hash_entry_t *i;
  int j;

  GET_HASH(hash_obj, hash);


  h = HASH(hash, key);

  DFSCH_RWLOCK_WRLOCK(&hash->lock);
  i = entry = hash->vector[h & hash->mask];

  while (i){
    if (h == i->hash && 
        (hash->equal ? dfsch_equal_p(i->key, key) : i->key == key)){
      i->value = value;
      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return;
    } 
    i = i->next;
  }
  
  // It isn't here, so we will add new item

  hash->count++;
  if (hash->count > (hash->mask+1)){ // Should table grow?
    hash_change_size(hash, ((hash->mask+1) * 2) - 1);
  }

  hash->vector[h & hash->mask] = alloc_entry(h,
                                             key,
                                             value,
                                             hash->vector[h & hash->mask]);
  
  DFSCH_RWLOCK_UNLOCK(&hash->lock);
}

int dfsch_hash_unset(dfsch_object_t* hash_obj,
                     dfsch_object_t* key){
  size_t h;
  hash_t *hash;
  hash_entry_t *i, *j;
  int k;

  GET_HASH(hash_obj, hash);

  h = HASH(hash, key);  
  DFSCH_RWLOCK_WRLOCK(&hash->lock);
  i = hash->vector[h & hash->mask];
  j = NULL;

  while (i){
    if (h == i->hash && 
        (hash->equal ? dfsch_equal_p(i->key, key) : i->key == key)){
      if (j){
        j->next = i->next;
      } else {
        hash->vector[h & hash->mask] = NULL;
      }
      hash->count --;
        
      if (hash->count+16 < (hash->mask+1)/2 
          && hash->mask != 0x3){ // Should table shrink?
        hash_change_size(hash, ((hash->mask+1) / 2) - 1);
      }
        
        
      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return 1;
    }
      
    j = i;
    i = i->next;
  }

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return 0;
  
}

int dfsch_hash_set_if_exists(dfsch_object_t* hash_obj, 
                             dfsch_object_t* key,
                             dfsch_object_t* value){
  
  size_t h;
  hash_t *hash;
  hash_entry_t *i;
  int j;

  GET_HASH(hash_obj, hash);

  h = HASH(hash, key);  

  DFSCH_RWLOCK_RDLOCK(&hash->lock);

  i = hash->vector[h & hash->mask];
  
  while (i){
    if (h == i->hash && 
        (hash->equal ? dfsch_equal_p(i->key, key) : i->key == key)){
      i->value = value;

      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return 1;
    }
    i = i->next;
  }

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return 0;
}


dfsch_object_t* dfsch_hash_2_alist(dfsch_object_t* hash_obj){
  dfsch_object_t *alist = NULL;
  int j;
  hash_entry_t *i;
  hash_t *hash;
  
  GET_HASH(hash_obj, hash);

  DFSCH_RWLOCK_RDLOCK(&hash->lock);

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
  DFSCH_RWLOCK_UNLOCK(&hash->lock);

  return alist;
}

dfsch_object_t* dfsch_alist_2_hash(dfsch_object_t* alist,
                                   int mode){
  dfsch_object_t* hash = dfsch_hash_make(mode);
  dfsch_object_t* i = alist;
  
  while (dfsch_pair_p(i)){
    dfsch_object_t* item = dfsch_car(i);
    dfsch_object_t* name = dfsch_list_item(item, 0);
    dfsch_object_t* value = dfsch_list_item(item, 1);

    dfsch_hash_set(hash, name, value);

    i = dfsch_cdr(i);
  }

  return hash;
}


/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(make_hash, NULL){
  dfsch_object_t *mode;
  DFSCH_OBJECT_ARG_OPT(args, mode, NULL);
  DFSCH_ARG_END(args);

  if (!mode)
    return dfsch_hash_make(DFSCH_HASH_EQ);
  if (dfsch_compare_keyword(mode, "equal?"))
    return dfsch_hash_make(DFSCH_HASH_EQUAL);
  if (dfsch_compare_keyword(mode, "eqv?"))
    return dfsch_hash_make(DFSCH_HASH_EQV);
  if (dfsch_compare_keyword(mode, "eq?"))
    return dfsch_hash_make(DFSCH_HASH_EQ);

  dfsch_error("Unknown hash comparison mode", mode);
}
DFSCH_DEFINE_PRIMITIVE(hash_p, NULL){
  dfsch_object_t* obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_hash_p(obj));
}
DFSCH_DEFINE_PRIMITIVE(hash_ref, NULL){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_hash_ref(hash, key);
}
DFSCH_DEFINE_PRIMITIVE(hash_unset, NULL){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_hash_unset(hash, key));
}
DFSCH_DEFINE_PRIMITIVE(hash_set, NULL){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  dfsch_hash_set(hash, key, value);
  return hash;
}
DFSCH_DEFINE_PRIMITIVE(hash_set_if_exists, NULL){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_hash_set_if_exists(hash, key, value));
}
DFSCH_DEFINE_PRIMITIVE(hash_2_alist, NULL){
  dfsch_object_t* hash;

  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_ARG_END(args);

  return dfsch_hash_2_alist(hash);
}

DFSCH_DEFINE_PRIMITIVE(alist_2_hash, NULL){
  dfsch_object_t* alist;
  dfsch_object_t *mode;

  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_OBJECT_ARG_OPT(args, mode, NULL);
  DFSCH_ARG_END(args);

  if (!mode)
    return dfsch_alist_2_hash(alist, DFSCH_HASH_EQ);
  if (dfsch_compare_keyword(mode, "equal?"))
    return dfsch_alist_2_hash(alist, DFSCH_HASH_EQUAL);
  if (dfsch_compare_keyword(mode, "eqv?"))
    return dfsch_alist_2_hash(alist, DFSCH_HASH_EQV);
  if (dfsch_compare_keyword(mode, "eq?"))
    return dfsch_alist_2_hash(alist, DFSCH_HASH_EQ);

  dfsch_error("exception:unknown-mode", mode);
}


void dfsch__hash_native_register(dfsch_object_t *ctx){
  //dfsch_defcanon_cstr(ctx, "<hash>", DFSCH_HASH_BASETYPE);
  dfsch_defcanon_cstr(ctx, "<hash-table>", DFSCH_HASH_TABLE_TYPE);
  //  dfsch_defcanon_cstr(ctx, "<custom-hash-type>", DFSCH_CUSTOM_HASH_TYPE_TYPE);

  dfsch_defcanon_cstr(ctx, "make-hash", 
                    DFSCH_PRIMITIVE_REF(make_hash));
  dfsch_defcanon_cstr(ctx, "hash?", 
                    DFSCH_PRIMITIVE_REF(hash_p));
  dfsch_defcanon_cstr(ctx, "hash-ref", 
                    DFSCH_PRIMITIVE_REF(hash_ref));
  dfsch_defcanon_cstr(ctx, "hash-unset!", 
                    DFSCH_PRIMITIVE_REF(hash_unset));
  dfsch_defcanon_cstr(ctx, "hash-set!", 
                    DFSCH_PRIMITIVE_REF(hash_set));
  dfsch_defcanon_cstr(ctx, "hash-set-if-exists!", 
                    DFSCH_PRIMITIVE_REF(hash_set_if_exists));
  dfsch_defcanon_cstr(ctx, "hash->alist", 
                    DFSCH_PRIMITIVE_REF(hash_2_alist));
  dfsch_defcanon_cstr(ctx, "alist->hash", 
                    DFSCH_PRIMITIVE_REF(alist_2_hash));

}
