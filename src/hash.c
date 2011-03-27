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
 */
#define INITIAL_MASK 0x07

typedef struct hash_entry_t hash_entry_t;

struct dfsch_hash_t{
  dfsch_type_t* type;
  hash_entry_t** vector;
  size_t count;
  size_t mask;
  dfsch_rwlock_t lock;
};

struct hash_entry_t {
  size_t hash;
  dfsch_object_t* key;
  dfsch_object_t* value;

  hash_entry_t* next;
};

static dfsch_object_t* hash_make_constructor(dfsch_type_t* discard){
  return dfsch_make_mapping_constructor(dfsch_make_hash());
}

static dfsch_collection_methods_t hash_table_col = {
  .get_iterator = dfsch_hash_2_alist,
  .make_constructor = hash_make_constructor,
};
static dfsch_mapping_methods_t hash_table_map = {
  .ref = dfsch_hash_ref,
  .set = dfsch_hash_set,
  .unset = dfsch_hash_unset,
  .set_if_exists = dfsch_hash_set_if_exists,
  //.set_if_not_exists = dfsch_hash_set_if_not_exists,
};

static void hash_serialize(dfsch_hash_t* h, dfsch_serializer_t* s){
  int j;
  hash_entry_t *i;

  DFSCH_RWLOCK_RDLOCK(&h->lock);
  dfsch_serialize_stream_symbol(s, "hash-table");

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
  dfsch_object_t* hash = dfsch_make_hash();
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
  sizeof(dfsch_hash_t),
  "hash-table",
  NULL,
  NULL,
  NULL,

  .collection = &hash_table_col,
  .mapping = &hash_table_map,
  .serialize = hash_serialize,
};

static dfsch_object_t* idhash_make_constructor(dfsch_type_t* discard){
  return dfsch_make_mapping_constructor(dfsch_make_idhash());
}

static dfsch_collection_methods_t idhash_table_col = {
  .get_iterator = dfsch_hash_2_alist,
  .make_constructor = idhash_make_constructor,
};
static dfsch_mapping_methods_t idhash_table_map = {
  .ref = dfsch_idhash_ref,
  .set = dfsch_idhash_set,
  .unset = dfsch_idhash_unset,
  .set_if_exists = dfsch_idhash_set_if_exists,
  //  .set_if_not_exists = dfsch_idhash_set_if_not_exists,
};

static void idhash_serialize(dfsch_hash_t* h, dfsch_serializer_t* s){
  int j;
  hash_entry_t *i;

  DFSCH_RWLOCK_RDLOCK(&h->lock);
  dfsch_serialize_stream_symbol(s, "identity-hash-table");

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

DFSCH_DEFINE_DESERIALIZATION_HANDLER("identity-hash-table", 
                                     identity_hash_table){
  dfsch_object_t* hash = dfsch_make_idhash();
  dfsch_deserializer_put_partial_object(ds, hash);
  while (dfsch_deserialize_integer(ds) == 1){
    dfsch_object_t* key = dfsch_deserialize_object(ds);
    dfsch_object_t* value = dfsch_deserialize_object(ds);
    dfsch_idhash_set(hash, key, value);
  }
  return hash;
}

dfsch_type_t dfsch_identity_hash_table_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_HASH_TABLE_TYPE,
  sizeof(dfsch_hash_t),
  "identity-hash-table",
  NULL,
  NULL,
  NULL,

  .collection = &idhash_table_col,
  .mapping = &idhash_table_map,
  .serialize = idhash_serialize,
};



static hash_entry_t** alloc_vector(size_t mask){
  return GC_MALLOC((sizeof(hash_entry_t*))*(mask+1));
}

#ifdef DFSCH_THREADS_FINALIZE
static void hash_finalizer(hash_t* h, void* cd) {
  DFSCH_DESTROY_RWLOCK(&h->lock);
}
#endif

static dfsch_hash_t* make_hash(dfsch_type_t* type){
  dfsch_hash_t *h = dfsch_make_object(DFSCH_HASH_TABLE_TYPE); 

  h->count = 0;
  h->mask = INITIAL_MASK;
  h->vector = alloc_vector(h->mask);
  DFSCH_INIT_RWLOCK(&h->lock);
#ifdef DFSCH_THREADS_FINALIZE
  GC_REGISTER_FINALIZER_NO_ORDER(h, (GC_finalization_proc)hash_finalizer,
                                 NULL, NULL, NULL);
#endif
  return h;
}

dfsch_object_t* dfsch_make_hash(){
  return make_hash(DFSCH_HASH_TABLE_TYPE);
}
dfsch_object_t* dfsch_make_idhash(){
  return make_hash(DFSCH_IDENTITY_HASH_TABLE_TYPE);
}


#define HASH(key) (((((size_t)key) >> 3)) ^ (((size_t)key) >> 15))

#define GET_HASH(obj,hash)                                              \
  hash = DFSCH_ASSERT_INSTANCE(obj, DFSCH_HASH_TABLE_TYPE)

dfsch_object_t* dfsch_hash_ref(dfsch_hash_t* hash, 
                               dfsch_object_t* key){
  size_t h;
  hash_entry_t *i;
  int j;
  dfsch_object_t* res;

  h = dfsch_hash(key);
  DFSCH_RWLOCK_RDLOCK(&hash->lock);

  i = hash->vector[h & hash->mask];

  while (i){
    if (h == i->hash && dfsch_equal_p(i->key, key)){
      res = i->value;
      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return res;
    }
    i = i->next;
  }
  
  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return DFSCH_INVALID_OBJECT;
}

dfsch_object_t* dfsch_idhash_ref(dfsch_hash_t* hash, 
                                 dfsch_object_t* key){
  size_t h;
  hash_entry_t *i;
  int j;
  dfsch_object_t* res;

  DFSCH_RWLOCK_RDLOCK(&hash->lock);
  h = HASH(key);

  i = hash->vector[h & hash->mask];

  while (i){
    if (i->key == key){
      res = i->value;
      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return res;
    }
    i = i->next;
  }

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return DFSCH_INVALID_OBJECT;
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

static void hash_change_size(dfsch_hash_t* hash, size_t new_mask){
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


static void hash_put(dfsch_hash_t* hash,
                     dfsch_object_t* key,
                     size_t h,
                     dfsch_object_t* value){
  hash->count++;
  if (hash->count > (hash->mask+1)){ // Should table grow?
    hash_change_size(hash, ((hash->mask+1) * 2) - 1);
  }

  hash->vector[h & hash->mask] = alloc_entry(h,
                                             key,
                                             value,
                                             hash->vector[h & hash->mask]);  
}

void dfsch_hash_set(dfsch_hash_t* hash,
                    dfsch_object_t* key,
                    dfsch_object_t* value){
  size_t h;
  hash_entry_t *entry;
  hash_entry_t *i;
  int j;

  h = dfsch_hash(key);

  DFSCH_RWLOCK_WRLOCK(&hash->lock);
  i = entry = hash->vector[h & hash->mask];

  while (i){
    if (h == i->hash && dfsch_equal_p(i->key, key)){
      i->value = value;
      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return;
    } 
    i = i->next;
  }
  
  // It isn't here, so we will add new item
  hash_put(hash, key, h, value);

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
}

void dfsch_idhash_set(dfsch_hash_t* hash,
                      dfsch_object_t* key,
                      dfsch_object_t* value){
  size_t h;
  hash_entry_t *entry;
  hash_entry_t *i;
  int j;

  h = HASH(key);

  DFSCH_RWLOCK_WRLOCK(&hash->lock);
  i = entry = hash->vector[h & hash->mask];

  while (i){
    if (i->key == key){
      i->value = value;
      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return;
    } 
    i = i->next;
  }
  
  // It isn't here, so we will add new item
  hash_put(hash, key, h, value);

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
}


int dfsch_hash_unset(dfsch_hash_t* hash,
                     dfsch_object_t* key){
  size_t h;
  hash_entry_t *i, *j;
  int k;

  h = dfsch_hash(key);  
  DFSCH_RWLOCK_WRLOCK(&hash->lock);
  i = hash->vector[h & hash->mask];
  j = NULL;

  while (i){
    if (h == i->hash && dfsch_equal_p(i->key, key)){
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

int dfsch_idhash_unset(dfsch_hash_t* hash,
                     dfsch_object_t* key){
  size_t h;
  hash_entry_t *i, *j;
  int k;

  h = dfsch_hash(key);  
  DFSCH_RWLOCK_WRLOCK(&hash->lock);
  i = hash->vector[h & hash->mask];
  j = NULL;

  while (i){
    if (i->key == key){
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


int dfsch_hash_set_if_exists(dfsch_hash_t* hash, 
                             dfsch_object_t* key,
                             dfsch_object_t* value){
  
  size_t h;
  hash_entry_t *i;
  int j;

  h = dfsch_hash(key);  

  DFSCH_RWLOCK_RDLOCK(&hash->lock);

  i = hash->vector[h & hash->mask];
  
  while (i){
    if (h == i->hash && dfsch_equal_p(i->key, key)){
      i->value = value;

      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return 1;
    }
    i = i->next;
  }

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return 0;
}
int dfsch_idhash_set_if_exists(dfsch_hash_t* hash, 
                             dfsch_object_t* key,
                             dfsch_object_t* value){
  
  size_t h;
  hash_entry_t *i;
  int j;

  h = dfsch_hash(key);  

  DFSCH_RWLOCK_RDLOCK(&hash->lock);

  i = hash->vector[h & hash->mask];
  
  while (i){
    if (i->key == key){
      i->value = value;

      DFSCH_RWLOCK_UNLOCK(&hash->lock);
      return 1;
    }
    i = i->next;
  }

  DFSCH_RWLOCK_UNLOCK(&hash->lock);
  return 0;
}



dfsch_object_t* dfsch_hash_2_alist(dfsch_hash_t* hash){
  dfsch_object_t *alist = NULL;
  int j;
  hash_entry_t *i;
  
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

static void alist_2_hash_core(dfsch_object_t* alist, dfsch_hash_t* hash){
  dfsch_object_t* i = alist;
  
  while (dfsch_pair_p(i)){
    dfsch_object_t* item = dfsch_car(i);
    dfsch_object_t* name = dfsch_list_item(item, 0);
    dfsch_object_t* value = dfsch_list_item(item, 1);

    dfsch_hash_set(hash, name, value);

    i = dfsch_cdr(i);
  }

}

dfsch_object_t* dfsch_alist_2_hash(dfsch_object_t* alist){
  dfsch_hash_t* hash = dfsch_make_hash();
  alist_2_hash_core(alist, hash);
  return hash;
}
dfsch_object_t* dfsch_alist_2_idhash(dfsch_object_t* alist){
  dfsch_hash_t* hash = dfsch_make_idhash();
  alist_2_hash_core(alist, hash);
  return hash;
}


/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_PRIMITIVE(make_hash, NULL){
  dfsch_object_t *mode;
  DFSCH_ARG_END(args);

  return dfsch_make_hash();
}

DFSCH_DEFINE_PRIMITIVE(alist_2_hash, NULL){
  dfsch_object_t* alist;
  dfsch_object_t *mode;

  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_ARG_END(args);

  return dfsch_alist_2_hash(alist);
}

DFSCH_DEFINE_PRIMITIVE(make_idhash, NULL){
  dfsch_object_t *mode;
  DFSCH_ARG_END(args);

  return dfsch_make_idhash();
}

DFSCH_DEFINE_PRIMITIVE(alist_2_idhash, NULL){
  dfsch_object_t* alist;
  dfsch_object_t *mode;

  DFSCH_OBJECT_ARG(args, alist);
  DFSCH_ARG_END(args);

  return dfsch_alist_2_idhash(alist);
}



void dfsch__hash_native_register(dfsch_object_t *ctx){
  dfsch_defcanon_cstr(ctx, "<hash-table>", DFSCH_HASH_TABLE_TYPE);
  dfsch_defcanon_cstr(ctx, "<identity-hash-table>", 
                      DFSCH_IDENTITY_HASH_TABLE_TYPE);

  dfsch_defcanon_cstr(ctx, "make-hash", 
                    DFSCH_PRIMITIVE_REF(make_hash));
  dfsch_defcanon_cstr(ctx, "alist->hash", 
                    DFSCH_PRIMITIVE_REF(alist_2_hash));
  dfsch_defcanon_cstr(ctx, "make-identity-hash", 
                    DFSCH_PRIMITIVE_REF(make_idhash));
  dfsch_defcanon_cstr(ctx, "alist->identity-hash", 
                    DFSCH_PRIMITIVE_REF(alist_2_idhash));

}
