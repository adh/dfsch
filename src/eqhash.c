/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Internal identity hash tables
 * Copyright (C) 2009 Ales Hakl
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

#include <dfsch/eqhash.h>
#include <dfsch/util.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>

#define INITIAL_MASK 0xf
//#define EQHASH_PRINT_STATS

static uint32_t fast_ptr_hash(dfsch_object_t* ptr){
  size_t p = (size_t) ptr;
  uint32_t r = p;

  r *= 2654435761;
  // r *= p | (p >> (CHAR_BIT*sizeof(size_t) / 2));
  r ^= (r >> 16);

  return r;
}

static dfsch_eqhash_entry_t** alloc_vector(size_t mask){
  size_t i;
  dfsch_eqhash_entry_t* vec = GC_MALLOC(sizeof(dfsch_eqhash_entry_t) * (mask + 1));

  for (i = 0; i <= mask; i++){
    vec[i].key = DFSCH_INVALID_OBJECT;
  }

  return vec;
}

void dfsch_eqhash_init(dfsch_eqhash_t* hash, int start_large){
  int i;
  if (start_large){
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE * 2; i++){
      hash->contents.large.cache[i] = NULL;
    }
    hash->contents.large.mask = INITIAL_MASK;
    hash->contents.large.count = 0;
    hash->contents.large.vector = alloc_vector(INITIAL_MASK);
  } else {
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      hash->contents.small.keys[i] = DFSCH_INVALID_OBJECT;
      hash->contents.small.values[i] = NULL;
      hash->contents.small.flags[i] = 0;
    }
  }
  hash->is_large = start_large;
}

static dfsch_eqhash_entry_t* alloc_entry(dfsch_object_t* key,
                                         dfsch_object_t* value,
                                         int flags,
                                         dfsch_eqhash_entry_t* next){
  dfsch_eqhash_entry_t* e = GC_NEW(dfsch_eqhash_entry_t);
  e->key = key;
  e->value = value;
  e->next = next;
  e->flags = flags;
  return e;
}

#define BUCKET(hash, index)                                             \
  (&(hash->contents.large.vector[(index) & hash->contents.large.mask]))

static void eqhash_large_put_low(dfsch_eqhash_entry_t* vector,
                                 size_t mask,
                                 dfsch_object_t* key, 
                                 dfsch_object_t* value,
                                 unsigned short flags){
  uint32_t h = fast_ptr_hash(key);
  
  if (vector[h & mask].key != DFSCH_INVALID_OBJECT){
    vector[h & mask].next = alloc_entry(key, value, flags, vector[h & mask].next);
  } else {
    vector[h & mask].key = key;
    vector[h & mask].value = value;
    vector[h & mask].flags = flags;
    vector[h & mask].next = NULL;
  }
}


static void convert_to_large(dfsch_eqhash_t* hash){
  dfsch_eqhash_entry_t** vector = alloc_vector(INITIAL_MASK);
  int i;
  
  for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
    if (hash->contents.small.keys[i] != DFSCH_INVALID_OBJECT){
      eqhash_large_put_low(vector, INITIAL_MASK, 
                           hash->contents.small.keys[i],
                           hash->contents.small.values[i],
                           hash->contents.small.flags[i]);
    }
  }

  for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE * 2; i++){
    hash->contents.large.cache[i] = NULL;
  }
  
  hash->contents.large.vector = vector;
  hash->contents.large.count = DFSCH_EQHASH_SMALL_SIZE;
  hash->contents.large.mask = INITIAL_MASK;  
  hash->is_large = 1;
}

static void grow_hash(dfsch_eqhash_t* hash){
  size_t new_mask = ((hash->contents.large.mask + 1) * 2) - 1;
  dfsch_eqhash_entry_t** vector = alloc_vector(new_mask);
  dfsch_eqhash_entry_t* i;
  dfsch_eqhash_entry_t* j;
  int k;

  for (k = 0; k <= hash->contents.large.mask; k++){
    i = &hash->contents.large.vector[k];
    while (i){
      eqhash_large_put_low(vector, new_mask, i->key, i->value, i->flags);
      i = i->next;
    }
  }

  hash->contents.large.vector = vector;
  hash->contents.large.mask = new_mask;
}


void dfsch_eqhash_put(dfsch_eqhash_t* hash,
                      dfsch_object_t* key, dfsch_object_t* value){
  if (DFSCH_LIKELY(!hash->is_large)){
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == DFSCH_INVALID_OBJECT){
        hash->contents.small.keys[i] = key;
        hash->contents.small.values[i] = value;
        return;
      }
    }

    convert_to_large(hash);
  }

  hash->contents.large.count++;
  if (DFSCH_UNLIKELY(hash->contents.large.count  
                     > hash->contents.large.mask)){
    grow_hash(hash);
  }
  eqhash_large_put_low(hash->contents.large.vector,
                       hash->contents.large.mask,
                       key, value, 0);
}

#ifdef EQHASH_PRINT_STATS
static int hits = 0;
#endif


static dfsch_eqhash_entry_t* find_entry(dfsch_eqhash_t* hash, 
                                        dfsch_object_t* key){
  dfsch_eqhash_entry_t* i;
  uint32_t h;

#ifdef EQHASH_PRINT_STATS
  int c = 0;
#endif

  h = fast_ptr_hash(key);
  i = hash->contents.large.cache[(h >> 16) % DFSCH_EQHASH_CACHE_SIZE];
  if (DFSCH_LIKELY(i) && 
      DFSCH_UNLIKELY(i->key == key)){
#ifdef EQHASH_PRINT_STATS
    hits++;
#endif
    return i;
  }

  
  i = BUCKET(hash, h);
  while (i){
    if (DFSCH_LIKELY(i->key == key)){
      hash->contents.large.cache[(h >> 16) % DFSCH_EQHASH_CACHE_SIZE] = i;
#ifdef EQHASH_PRINT_STATS
      printf(";; large eqhash lookup: %d (%d hits)\n", c, hits);
      hits = 0;
#endif
      return i;
    }
#ifdef EQHASH_PRINT_STATS
    c++;
#endif
    i = i->next;
  }
#ifdef EQHASH_PRINT_STATS
  printf(";; large eqhash lookup fail after %d\n", c);
#endif
  return NULL;
}

static int delete_entry(dfsch_eqhash_t* hash, 
                        dfsch_object_t* key){
  dfsch_eqhash_entry_t* i;
  dfsch_eqhash_entry_t* j = NULL;
  size_t h;
  h = fast_ptr_hash(key);
  
  i = BUCKET(hash, h);
  while (i){
    if (i->key == key){
      if (hash->contents.large.cache[(h >> 10) % DFSCH_EQHASH_CACHE_SIZE] == i){
        hash->contents.large.cache[(h >> 10) % DFSCH_EQHASH_CACHE_SIZE] = NULL;
      }
      if (j) {
        j->next = i->next;
      } else {
        if (i->next){
          i->key = i->next->key;
          i->value = i->next->value;
          i->flags = i->next->flags;
          i->next = i->next->next;
        } else {
          i->key = DFSCH_INVALID_OBJECT;
          i->value = DFSCH_INVALID_OBJECT;
        }
      }
      return 1;
    }
    j = i;
    i = i->next;
  }
  return 0;
  
  
  dfsch_error("Not implemented", NULL);
}

void dfsch_eqhash_set(dfsch_eqhash_t* hash,
                      dfsch_object_t* key, dfsch_object_t* value){
  if (hash->is_large){
    dfsch_eqhash_entry_t* e = find_entry(hash, key);
    if (e) {
      e->value = value;
      return;
    }
  } else {
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == key){
        hash->contents.small.values[i] = value;
        return;
      }
    }
  }
  dfsch_eqhash_put(hash, key, value);  
}
void dfsch_eqhash_set_flags(dfsch_eqhash_t* hash,
                            dfsch_object_t* key, unsigned short flags){
  if (hash->is_large){
    dfsch_eqhash_entry_t* e = find_entry(hash, key);
    if (e) {
      e->flags = flags;
      return;
    }
  } else {
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == key){
        hash->contents.small.flags[i] = flags;
        return;
      }
    }
  }
}
int dfsch_eqhash_set_if_exists(dfsch_eqhash_t* hash,
                               dfsch_object_t* key, dfsch_object_t* value,
                               unsigned short* flags){
  if (hash->is_large){
    dfsch_eqhash_entry_t* e = find_entry(hash, key);
    if (e) {
      e->value = value;
      return 1;
    }
  } else {
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == key){
        hash->contents.small.values[i] = value;
        return 1;
      }
    }
  }  
  return 0;
}
int dfsch_eqhash_unset(dfsch_eqhash_t* hash, dfsch_object_t* key){
  if (hash->is_large){
    return delete_entry(hash, key);
  } else {
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == key){
        hash->contents.small.keys[i] = DFSCH_INVALID_OBJECT;
        return 1;
      }
    }
  }  
  return 0;
}

dfsch_object_t* dfsch_eqhash_ref(dfsch_eqhash_t* hash,
                                 dfsch_object_t* key){
  if (DFSCH_UNLIKELY(hash->is_large)){
    dfsch_eqhash_entry_t* e = find_entry(hash, key);
    if (e) {
      return e->value;
    }
  } else {
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == key){
        return hash->contents.small.values[i];
      }
    }
  }  
  return DFSCH_INVALID_OBJECT;  
}
int dfsch_eqhash_ref_ex(dfsch_eqhash_t* hash,
                        dfsch_object_t* key, 
                        dfsch_object_t** value, unsigned short *flags,
                        dfsch_eqhash_entry_t** entry){
  if (hash->is_large){
    dfsch_eqhash_entry_t* e = find_entry(hash, key);
    if (e) {
      if (value){
        *value = e->value;
      }
      if (flags){
        *flags = e->flags;
      }
      if (entry){
        *entry = e;
      }
      return 1;
    }
  } else {
    int i;
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] == key){
        if (value) {
          *value = hash->contents.small.values[i];
        }
        if (flags) {
          *flags = hash->contents.small.flags[i];
        }
        return 1;
      }
    }
  }  
  return 0;  
}
dfsch_object_t* dfsch_eqhash_2_alist(dfsch_eqhash_t* hash){
  dfsch_object_t* result = NULL;
  int i;

  if (hash->is_large){
    for (i = 0; i <= hash->contents.large.mask; i++){
      dfsch_eqhash_entry_t* e = &hash->contents.large.vector[i];
      while (e){
        if (e->key != DFSCH_INVALID_OBJECT){
          result = dfsch_cons(dfsch_list(2, e->key, e->value), result);
        }
        e = e->next;
      }
    }
  } else {
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] != DFSCH_INVALID_OBJECT){
        result = dfsch_cons(dfsch_list(2,
                                       hash->contents.small.keys[i],
                                       hash->contents.small.values[i]),
                            result);
      }
    }
  }
  return result;
}
dfsch_object_t* dfsch_eqhash_revscan(dfsch_eqhash_t* hash,
                                     dfsch_object_t* value,
                                     unsigned short flags){
  int i;

  if (hash->is_large){
    for (i = 0; i <= hash->contents.large.mask; i++){
      dfsch_eqhash_entry_t* e = &hash->contents.large.vector[i];
      while (e){
        if (e->key != DFSCH_INVALID_OBJECT){
          if (e->value == value && (e->flags & flags) == flags){
            return e->key;
          }
        }
        e = e->next;
      }
    }
  } else {
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] != DFSCH_INVALID_OBJECT){
        if (hash->contents.small.values[i] == value &&
            (hash->contents.small.flags[i] & flags) == flags){
          return hash->contents.small.keys[i];
        }
      }
    }
  }
  return DFSCH_INVALID_OBJECT;  
}
dfsch_eqhash_entry_t* dfsch_eqhash_2_entry_list(dfsch_eqhash_t* hash){
  dfsch_eqhash_entry_t* result = NULL;
  dfsch_eqhash_entry_t* t;
  int i;

  if (hash->is_large){
    for (i = 0; i <= hash->contents.large.mask; i++){
      dfsch_eqhash_entry_t* e = &hash->contents.large.vector[i];
      while (e){
        if (e->key != DFSCH_INVALID_OBJECT){
          t = GC_NEW(dfsch_eqhash_entry_t);
          t->next = result;
          result = t;
          t->key = e->key;
          t->value = e->value;
          t->flags = e->flags;
        }
        e = e->next;
      }
    }
  } else {
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      if (hash->contents.small.keys[i] != DFSCH_INVALID_OBJECT){
        t = GC_NEW(dfsch_eqhash_entry_t);
        t->next = result;
        result = t;
        t->key = hash->contents.small.keys[i];
        t->value = hash->contents.small.values[i];
        t->flags = hash->contents.small.flags[i];
      }
    }
  }
  return result;

}
