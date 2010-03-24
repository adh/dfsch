/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Multiple keyed hash tables
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

#include "dfsch/mkhash.h"
#include <dfsch/util.h>

#include <assert.h>

typedef struct mk_entry_t {
  uint32_t cum_hash;
  dfsch_object_t** keys;
  dfsch_object_t* value;
} mk_entry_t;

struct dfsch_mkhash_t {
  dfsch_type_t* type;

  int equal;
  size_t num_keys;
  size_t mask;
  size_t count;
  dfsch_rwlock_t* lock;

  mk_entry_t* entries;

  mk_entry_t direct_entries[8];
};

static uint32_t hash_keys(dfsch_object_t** keys,
                          size_t num){
  uint32_t res = 0;
  while (num){
    res ^= ((size_t)*keys);
    res += res >> 18;
    res -= (res >> num & 0x1f) | (res << 32 - (num & 0x1f));
    num--;
    keys++;
  }
  return res;
}

static uint32_t hash_keys_equal(dfsch_object_t** keys,
                                size_t num){
  uint32_t res = 0;
  while (num){
    res += dfsch_hash(*keys) * num;
    res += (res << num & 0x1f) | (res >> 32 - (num & 0x1f));
    num--;
    keys++;
  }
  return res;
}

void dfsch_mkhash_reset(dfsch_mkhash_t* h, size_t num_keys, int equalp){
  int i;

  h->count = 0;
  h->mask = 7;
  h->equal = equalp;
  h->num_keys = num_keys;
  h->lock = DFSCH_CREATE_RWLOCK();

  h->entries = h->direct_entries;
  
  for (i = 0; i < 8; i++){
    h->entries[i].keys = NULL;
    h->entries[i].value = DFSCH_INVALID_OBJECT;
  }
}

dfsch_mkhash_t* dfsch_make_mkhash(size_t num_keys,
                                  int equalp){
  dfsch_mkhash_t* h = dfsch_make_object(DFSCH_MKHASH_TYPE);

  dfsch_mkhash_reset(h, num_keys, equalp);

  return h;
}
static mk_entry_t* find_entry(dfsch_mkhash_t* h,
                              uint32_t cum_hash,
                              dfsch_object_t** keys){
  int equal;
  size_t mask;
  size_t num_keys;
  size_t initial_i;
  size_t i;
  size_t j;

  /* XXX: this should trick compiler to generate more efficient code*/
  equal = h->equal; 
  mask = h->mask;
  num_keys = h->num_keys;

  i = initial_i = cum_hash & mask;

  do {
    if (!h->entries[i].keys){
      return &(h->entries[i]);
    }
    if (cum_hash == h->entries[i].cum_hash){
      for (j = 0; j < num_keys; j++){
        if (keys[j] == h->entries[i].keys[j]){
          continue;
        }
        if (equal && dfsch_equal_p(keys[j], h->entries[i].keys[j])){
          continue;
        }
        goto no_match;
      }
      return &(h->entries[i]);
    }
  no_match:
    i++;
    i &= mask;
  } while (i != initial_i);

  return NULL;
}

static void resize_hash(dfsch_mkhash_t* h, size_t new_mask){
  size_t i;
  size_t j;
  size_t oj;
  mk_entry_t* new_entries = GC_MALLOC(sizeof(mk_entry_t) * (new_mask + 1));
  
  for (i = 0; i <= new_mask; i++){
    new_entries[i].keys = NULL;
    new_entries[i].value = DFSCH_INVALID_OBJECT;
  }

  for (i = 0; i <= h->mask; i++){
    if (h->entries[i].value != DFSCH_INVALID_OBJECT){
      j = h->entries[i].cum_hash & new_mask;

      while (new_entries[j].keys) { 
        j = (j + 1) & new_mask;
      }

      new_entries[j].keys = h->entries[i].keys;
      new_entries[j].value = h->entries[i].value;
      new_entries[j].cum_hash = h->entries[i].cum_hash;
    }
  }

  h->mask = new_mask;
  h->entries = new_entries;

}

int dfsch_mkhash_ref(dfsch_mkhash_t* h,
                     dfsch_object_t** keys,
                     dfsch_object_t** result){
  uint32_t cum_hash; 
  mk_entry_t* e;

  DFSCH_RWLOCK_RDLOCK(h->lock);
  if (h->equal){
    cum_hash = hash_keys_equal(keys, h->num_keys);
  } else {
    cum_hash = hash_keys(keys, h->num_keys);
  }

  e = find_entry(h, cum_hash, keys);
  DFSCH_RWLOCK_UNLOCK(h->lock);

  if (e && e->keys && e->value != DFSCH_INVALID_OBJECT){
    *result = e->value;
    return 1;
  } else {
    return 0;
  }
}
void dfsch_mkhash_set(dfsch_mkhash_t* h,
                      dfsch_object_t** keys,
                      dfsch_object_t* value){
  uint32_t cum_hash; 
  mk_entry_t* e;

  DFSCH_RWLOCK_WRLOCK(h->lock);
  if (h->equal){
    cum_hash = hash_keys_equal(keys, h->num_keys);
  } else {
    cum_hash = hash_keys(keys, h->num_keys);
  }

  e = find_entry(h, cum_hash, keys);

  if (!e){
    resize_hash(h, h->mask);
    assert(e);
  }

  if (value != e->value){
    if (e->value == DFSCH_INVALID_OBJECT){
      h->count++;
    } else if (value == DFSCH_INVALID_OBJECT){
      h->count--;
    }
  }

  e->value = value;
  e->keys = GC_MALLOC(sizeof(dfsch_object_t*)*h->num_keys);
  memcpy(e->keys, keys, h->num_keys * sizeof(dfsch_object_t*));
  e->cum_hash = cum_hash;

  if ((h->count * 3) > (h->mask * 2)){
    resize_hash(h, ((h->mask + 1) * 2) - 1);
  } else if ((h->count * 3) < (h->mask) && h->mask != 7){
    resize_hash(h, ((h->mask + 1) / 2) - 1);
  }

  DFSCH_RWLOCK_UNLOCK(h->lock);
}
void dfsch_mkhash_unset(dfsch_mkhash_t* hash,
                        dfsch_object_t** keys){
  dfsch_mkhash_set(hash, keys, DFSCH_INVALID_OBJECT);
}
dfsch_object_t* dfsch_mkhash_2_alist(dfsch_mkhash_t* h){
  int i;
  dfsch_object_t* result = NULL;

  for (i = 0; i <= h->mask; i++){
    if (h->entries[i].value != DFSCH_INVALID_OBJECT){
      result = dfsch_cons(dfsch_list(2, 
                                     dfsch_list_from_array(h->entries[i].keys,
                                                           h->num_keys),
                                     h->entries[i].value), result);
    }
  }

  return result;
}

static dfsch_object_t** destructure_keylist(dfsch_mkhash_t* h,
                                            dfsch_object_t* list){
  size_t length;
  dfsch_object_t** array = dfsch_list_as_array(list, &length);
  if (length != h->num_keys){
    dfsch_error("Invalid number of keys", NULL);
  }

  return array;
}

static int mkhash_ref(dfsch_mkhash_t* h,
                      dfsch_object_t* key,
                      dfsch_object_t** res){
  return dfsch_mkhash_ref(h, destructure_keylist(h, key), res);
}
static void mkhash_set(dfsch_mkhash_t* h,
                       dfsch_object_t* key,
                       dfsch_object_t* value){
  dfsch_mkhash_set(h, destructure_keylist(h, key), value);
}
static void mkhash_unset(dfsch_mkhash_t* h,
                         dfsch_object_t* key){
  dfsch_mkhash_unset(h, destructure_keylist(h, key));
}


dfsch_custom_hash_type_t dfsch_mkhash_type = {
  .parent = {
    .type = DFSCH_CUSTOM_HASH_TYPE_TYPE,
    .superclass = DFSCH_HASH_BASETYPE,
    .name = "multiple-key-hash",
    .size = sizeof(dfsch_mkhash_t)
  },

  .ref = mkhash_ref,
  .set = mkhash_set,
  .unset = mkhash_unset,
  .hash_2_alist = dfsch_mkhash_2_alist,
};


DFSCH_DEFINE_PRIMITIVE(make_mkhash, "Create new multiple-key-hash object"){
  size_t num_keys;
  dfsch_object_t* equal = NULL;
  DFSCH_LONG_ARG(args, num_keys);
  DFSCH_OBJECT_ARG_OPT(args, equal, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_mkhash(num_keys, equal != NULL);
}

void dfsch__mkhash_register(dfsch_object_t* env){
  dfsch_define_cstr(env, "make-multiple-key-hash",
                    DFSCH_PRIMITIVE_REF(make_mkhash));
  dfsch_define_cstr(env, "<multiple-key-hash>",
                    DFSCH_MKHASH_TYPE);
}

