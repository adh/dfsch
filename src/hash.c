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

typedef struct hash_iterator_t {
  dfsch_type_t* type;
  dfsch_hash_t* hash;
  size_t bucket;
  hash_entry_t* entry;
} hash_iterator_t;

static hash_iterator_t* hash_iterator_next(hash_iterator_t* it){
  DFSCH_RWLOCK_RDLOCK(&it->hash->lock);
  if (!it->entry || !it->entry->next){
    if (it->entry){
      it->bucket++;
    }
    it->entry = NULL;
    for (; it->bucket <= it->hash->mask; it->bucket++){
      if (it->hash->vector[it->bucket]){
        it->entry = it->hash->vector[it->bucket];
        break;
      }
    }
    if (!it->entry){
      DFSCH_RWLOCK_UNLOCK(&it->hash->lock);
      return NULL;
    }
  } else {
    it->entry = it->entry->next;
  }
  DFSCH_RWLOCK_UNLOCK(&it->hash->lock);
  return it;
}
static dfsch_object_t* hash_iterator_this_item(hash_iterator_t* it){
  return dfsch_list(2, it->entry->key, it->entry->value);
}
static dfsch_object_t* hash_iterator_this_key(hash_iterator_t* it){
  return it->entry->key;
}
static dfsch_object_t* hash_iterator_this_value(hash_iterator_t* it){
  return it->entry->value;
}

static dfsch_iterator_methods_t hash_item_it_methods = {
  .next = hash_iterator_next,
  .this = hash_iterator_this_item,
};
dfsch_type_t dfsch_hash_items_iterator_type = {
  .type = DFSCH_STANDARD_TYPE, 
  .name = "hash-items-iterator",
  .size = sizeof(hash_iterator_t),
  .collection = &dfsch_iterator_collection_methods,
  .iterator = &hash_item_it_methods,
};

static dfsch_iterator_methods_t hash_key_it_methods = {
  .next = hash_iterator_next,
  .this = hash_iterator_this_key,
};
dfsch_type_t dfsch_hash_keys_iterator_type = {
  .type = DFSCH_STANDARD_TYPE, 
  .name = "hash-keys-iterator",
  .size = sizeof(hash_iterator_t),
  .collection = &dfsch_iterator_collection_methods,
  .iterator = &hash_key_it_methods,
};

static dfsch_iterator_methods_t hash_value_it_methods = {
  .next = hash_iterator_next,
  .this = hash_iterator_this_value,
};
dfsch_type_t dfsch_hash_values_iterator_type = {
  .type = DFSCH_STANDARD_TYPE, 
  .name = "hash-values-iterator",
  .size = sizeof(hash_iterator_t),
  .collection = &dfsch_iterator_collection_methods,
  .iterator = &hash_value_it_methods,
};

static dfsch_object_t* get_hash_iterator(dfsch_hash_t* h, dfsch_type_t* type){
  hash_iterator_t* it = dfsch_make_object(type);
  it->hash = h;
  return hash_iterator_next(it);
}

static dfsch_object_t* get_hash_items_iterator(dfsch_hash_t* h){
  return get_hash_iterator(h, DFSCH_HASH_ITEMS_ITERATOR_TYPE);
}
static dfsch_object_t* get_hash_keys_iterator(dfsch_hash_t* h){
  return get_hash_iterator(h, DFSCH_HASH_KEYS_ITERATOR_TYPE);
}
static dfsch_object_t* get_hash_values_iterator(dfsch_hash_t* h){
  return get_hash_iterator(h, DFSCH_HASH_VALUES_ITERATOR_TYPE);
}

static dfsch_object_t* hash_make_constructor(dfsch_type_t* discard){
  return dfsch_make_mapping_constructor(dfsch_make_hash());
}

static dfsch_collection_methods_t hash_table_col = {
  .get_iterator = get_hash_items_iterator,
  .make_constructor = hash_make_constructor,
};
static dfsch_mapping_methods_t hash_table_map = {
  .ref = dfsch_hash_ref,
  .set = dfsch_hash_set,
  .unset = dfsch_hash_unset,
  .set_if_exists = dfsch_hash_set_if_exists,
  //.set_if_not_exists = dfsch_hash_set_if_not_exists,

  .get_keys_iterator = get_hash_keys_iterator,
  .get_values_iterator = get_hash_values_iterator,
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

static void set_serialize(dfsch_hash_t* h, dfsch_serializer_t* s){
  int j;
  hash_entry_t *i;

  DFSCH_RWLOCK_RDLOCK(&h->lock);
  dfsch_serialize_stream_symbol(s, "set");

  for (j=0; j<(h->mask+1); j++){
    i = h->vector[j];
    while (i){
      dfsch_serialize_integer(s, 1);
      dfsch_serialize_object(s, i->key);
      i = i->next;
    }
  }
  dfsch_serialize_integer(s, 0);
  DFSCH_RWLOCK_UNLOCK(&h->lock);
}

DFSCH_DEFINE_DESERIALIZATION_HANDLER("set", set){
  dfsch_object_t* hash = dfsch_make_set();
  dfsch_deserializer_put_partial_object(ds, hash);
  while (dfsch_deserialize_integer(ds) == 1){
    dfsch_object_t* key = dfsch_deserialize_object(ds);
    dfsch_hash_set(hash, key, DFSCH_SYM_TRUE);
  }
  return hash;
}

dfsch_type_t dfsch_base_hash_table_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  sizeof(dfsch_hash_t),
  "base-hash-table",
  NULL,
  NULL,
  NULL,
};

dfsch_type_t dfsch_hash_table_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_BASE_HASH_TABLE_TYPE,
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
  .get_iterator = get_hash_items_iterator,
  .make_constructor = idhash_make_constructor,
};
static dfsch_mapping_methods_t idhash_table_map = {
  .ref = dfsch_idhash_ref,
  .set = dfsch_idhash_set,
  .unset = dfsch_idhash_unset,
  .set_if_exists = dfsch_idhash_set_if_exists,
  //.set_if_not_exists = dfsch_idhash_set_if_not_exists,

  .get_keys_iterator = get_hash_keys_iterator,
  .get_values_iterator = get_hash_values_iterator,
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

static void idset_serialize(dfsch_hash_t* h, dfsch_serializer_t* s){
  int j;
  hash_entry_t *i;

  DFSCH_RWLOCK_RDLOCK(&h->lock);
  dfsch_serialize_stream_symbol(s, "identity-set");

  for (j=0; j<(h->mask+1); j++){
    i = h->vector[j];
    while (i){
      dfsch_serialize_integer(s, 1);
      dfsch_serialize_object(s, i->key);
      i = i->next;
    }
  }
  dfsch_serialize_integer(s, 0);
  DFSCH_RWLOCK_UNLOCK(&h->lock);
}

DFSCH_DEFINE_DESERIALIZATION_HANDLER("identity-set", 
                                     identity_set){
  dfsch_hash_t* hash = dfsch_make_idset();
  dfsch_deserializer_put_partial_object(ds, hash);
  while (dfsch_deserialize_integer(ds) == 1){
    dfsch_object_t* key = dfsch_deserialize_object(ds);
    dfsch_idhash_set(hash, key, DFSCH_SYM_TRUE);
  }
  return hash;
}


dfsch_type_t dfsch_identity_hash_table_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_BASE_HASH_TABLE_TYPE,
  sizeof(dfsch_hash_t),
  "identity-hash-table",
  NULL,
  NULL,
  NULL,

  .collection = &idhash_table_col,
  .mapping = &idhash_table_map,
  .serialize = idhash_serialize,
};

typedef struct set_constructor_t {
  dfsch_collection_constructor_type_t* type;
  dfsch_object_t* set;
} set_constructor_t;

static void set_constructor_add(set_constructor_t* sc,
                                dfsch_object_t* element){
  dfsch_mapping_set(sc->set, element, DFSCH_SYM_TRUE);
}

static dfsch_object_t* set_constructor_done(set_constructor_t* sc){
  return sc->set;
}

static dfsch_collection_constructor_type_t set_constructor_type = {
  .type = {
    .type = DFSCH_COLLECTION_CONSTRUCTOR_TYPE_TYPE,
    .name = "set-constructor",
    .size = sizeof(set_constructor_t),
  },
  .add = set_constructor_add,
  .done = set_constructor_done,
};

static dfsch_object_t* dfsch_make_set_constructor(dfsch_object_t* set){
  set_constructor_t* sc = dfsch_make_object(&set_constructor_type);
  sc->set = set;
  return sc;
}

static dfsch_object_t* idset_make_constructor(dfsch_type_t* discard){
  return dfsch_make_set_constructor(dfsch_make_idhash());
}
static dfsch_object_t* set_make_constructor(dfsch_type_t* discard){
  return dfsch_make_set_constructor(dfsch_make_idhash());
}


static dfsch_collection_methods_t set_col = {
  .get_iterator = get_hash_keys_iterator,
  .make_constructor = set_make_constructor,
};
static dfsch_mapping_methods_t set_map = {
  .ref = dfsch_hash_ref,
  .set = dfsch_set_set,
  .unset = dfsch_hash_unset,

  .get_keys_iterator = get_hash_keys_iterator,
};

dfsch_type_t dfsch_set_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_BASE_HASH_TABLE_TYPE,
  sizeof(dfsch_hash_t),
  "set",
  NULL,
  NULL,
  NULL,

  .collection = &set_col,
  .mapping = &set_map,
  .serialize = set_serialize,
};

static dfsch_collection_methods_t idset_col = {
  .get_iterator = get_hash_keys_iterator,
  .make_constructor = idset_make_constructor,
};
static dfsch_mapping_methods_t idset_map = {
  .ref = dfsch_idhash_ref,
  .set = dfsch_idset_set,
  .unset = dfsch_idhash_unset,

  .get_keys_iterator = get_hash_keys_iterator,
};

dfsch_type_t dfsch_identity_set_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_BASE_HASH_TABLE_TYPE,
  sizeof(dfsch_hash_t),
  "identity-set",
  NULL,
  NULL,
  NULL,

  .collection = &idset_col,
  .mapping = &idset_map,
  .serialize = idset_serialize,
};



static hash_entry_t** alloc_vector(size_t mask){
  return GC_MALLOC((sizeof(hash_entry_t*))*(mask+1));
}

#ifdef DFSCH_THREADS_FINALIZE
static void hash_finalizer(dfsch_hash_t* h, void* cd) {
  DFSCH_DESTROY_RWLOCK(&h->lock);
}
#endif

static dfsch_hash_t* make_hash(dfsch_type_t* type){
  dfsch_hash_t *h = dfsch_make_object(type); 

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
dfsch_object_t* dfsch_make_set(){
  return make_hash(DFSCH_SET_TYPE);
}
dfsch_object_t* dfsch_make_idset(){
  return make_hash(DFSCH_IDENTITY_SET_TYPE);
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

void dfsch_set_set(dfsch_hash_t* hash,
                   dfsch_object_t* key,
                   dfsch_object_t* value){
  if (value) {
    dfsch_hash_set(hash, key, DFSCH_SYM_TRUE);
  } else {
    dfsch_hash_unset(hash, key);
  }
}

void dfsch_idset_set(dfsch_hash_t* hash,
                     dfsch_object_t* key,
                     dfsch_object_t* value){
  if (value) {
    dfsch_idhash_set(hash, key, DFSCH_SYM_TRUE);
  } else {
    dfsch_idhash_unset(hash, key);
  }
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
}

dfsch_object_t* dfsch_alist_2_hash(dfsch_object_t* alist){
  dfsch_hash_t* hash = dfsch_make_hash();
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
dfsch_object_t* dfsch_alist_2_idhash(dfsch_object_t* alist){
  dfsch_hash_t* hash = dfsch_make_hash();
  dfsch_object_t* i = alist;
  
  while (dfsch_pair_p(i)){
    dfsch_object_t* item = dfsch_car(i);
    dfsch_object_t* name = dfsch_list_item(item, 0);
    dfsch_object_t* value = dfsch_list_item(item, 1);

    dfsch_idhash_set(hash, name, value);

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

DFSCH_DEFINE_PRIMITIVE(make_set, NULL){
  dfsch_object_t *mode;
  DFSCH_ARG_END(args);

  return dfsch_make_set();
}
DFSCH_DEFINE_PRIMITIVE(make_idset, NULL){
  dfsch_object_t *mode;
  DFSCH_ARG_END(args);

  return dfsch_make_idset();
}


DFSCH_DEFINE_PRIMITIVE(hash, NULL){
  dfsch_hash_t* hash = dfsch_make_hash();
  while (DFSCH_PAIR_P(args)){
    dfsch_object_t* key;
    dfsch_object_t* value;
    DFSCH_OBJECT_ARG(args, key);
    DFSCH_OBJECT_ARG(args, value);
    dfsch_hash_set(hash, key, value);
  }
  return hash;
}
DFSCH_DEFINE_PRIMITIVE(idhash, NULL){
  dfsch_hash_t* hash = dfsch_make_idhash();
  while (DFSCH_PAIR_P(args)){
    dfsch_object_t* key;
    dfsch_object_t* value;
    DFSCH_OBJECT_ARG(args, key);
    DFSCH_OBJECT_ARG(args, value);
    dfsch_idhash_set(hash, key, value);
  }
  return hash;
}

DFSCH_DEFINE_PRIMITIVE(set, NULL){
  dfsch_hash_t* hash = dfsch_make_set();
  while (DFSCH_PAIR_P(args)){
    dfsch_object_t* key;
    DFSCH_OBJECT_ARG(args, key);
    dfsch_hash_set(hash, key, DFSCH_SYM_TRUE);
  }
  return hash;
}
DFSCH_DEFINE_PRIMITIVE(idset, NULL){
  dfsch_hash_t* hash = dfsch_make_idset();
  while (DFSCH_PAIR_P(args)){
    dfsch_object_t* key;
    DFSCH_OBJECT_ARG(args, key);
    dfsch_idhash_set(hash, key, DFSCH_SYM_TRUE);
  }
  return hash;
}


void dfsch__hash_native_register(dfsch_object_t *ctx){
  dfsch_defcanon_cstr(ctx, "<base-hash-table>", DFSCH_BASE_HASH_TABLE_TYPE);
  dfsch_defcanon_cstr(ctx, "<hash-table>", DFSCH_HASH_TABLE_TYPE);
  dfsch_defcanon_cstr(ctx, "<identity-hash-table>", 
                      DFSCH_IDENTITY_HASH_TABLE_TYPE);
  dfsch_defcanon_cstr(ctx, "<set>", DFSCH_SET_TYPE);
  dfsch_defcanon_cstr(ctx, "<identity-set>", 
                      DFSCH_IDENTITY_SET_TYPE);

  dfsch_defcanon_cstr(ctx, "make-hash", 
                    DFSCH_PRIMITIVE_REF(make_hash));
  dfsch_defcanon_cstr(ctx, "alist->hash", 
                    DFSCH_PRIMITIVE_REF(alist_2_hash));
  dfsch_defcanon_cstr(ctx, "make-identity-hash", 
                    DFSCH_PRIMITIVE_REF(make_idhash));
  dfsch_defcanon_cstr(ctx, "alist->identity-hash", 
                    DFSCH_PRIMITIVE_REF(alist_2_idhash));

  dfsch_defcanon_cstr(ctx, "make-set", 
                    DFSCH_PRIMITIVE_REF(make_set));
  dfsch_defcanon_cstr(ctx, "make-identity-set", 
                    DFSCH_PRIMITIVE_REF(make_idset));

  dfsch_defcanon_cstr(ctx, "hash", 
                    DFSCH_PRIMITIVE_REF(hash));
  dfsch_defcanon_cstr(ctx, "identity-hash", 
                    DFSCH_PRIMITIVE_REF(idhash));
  dfsch_defcanon_cstr(ctx, "set", 
                    DFSCH_PRIMITIVE_REF(set));
  dfsch_defcanon_cstr(ctx, "identity-set", 
                    DFSCH_PRIMITIVE_REF(idset));
}
