/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Hash tables
 * Copyright (C) 2005 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <dfsch/hash.h>
#include "internal.h"
#include <stdlib.h>


typedef struct hash_entry_t hash_entry_t;
typedef struct hash_t{

  dfsch_type_t* type;
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

static const dfsch_type_t hash_type = {
  sizeof(hash_t),
  "hash",
  NULL,
  NULL
};

static void alloc_vector(hash_t* hash){
  hash->vector = GC_MALLOC(sizeof(hash_entry_t)*(hash->mask+1));
}

dfsch_object_t* dfsch_hash_make(dfsch_object_t* hash_proc){
  hash_t *h = dfsch_make_object(&hash_type); 

  if (hash_proc && !dfsch_procedure_p(hash_proc))
    DFSCH_THROW("exception:not-a-procedure", hash_proc);

  h->proc = hash_proc;
  h->count = 0;
  h->mask = 0x03;
  alloc_vector(h);

  return h;
}
int dfsch_hash_p(dfsch_object_t* obj){
  return obj->type == &hash_type;
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
   if (obj->type != &hash_type)\
     dfsch_throw("exception:not-a-hash", obj); \
   hash = obj;

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

static void hash_change_size(hash_t* hash, size_t new_mask){
  int j;
  hash_entry_t *i;
  hash_entry_t **vector = hash->vector; // Save pointer to old contents
  size_t ol = hash->mask+1;
  hash->mask = new_mask;
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
    hash_change_size(hash, ((hash->mask+1) * 2) - 1);
  }

  hash->vector[h & hash->mask] = alloc_entry(h,
                                             key,
                                             value,
                                             hash->vector[h & hash->mask]);
  
  return hash_obj;
}
dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash_obj,
                                 dfsch_object_t* key){
  size_t h;
  hash_t *hash;
  hash_entry_t *i, *j;

  GET_HASH(hash_obj, hash);

  h = get_hash(hash, key);  
  i = hash->vector[h & hash->mask];
  
  while (i){
    if (h = i->hash && dfsch_eq_p(i->key, key)) {
      j->next = i->next;
      hash->count --;

      if (hash->count+16 < (hash->mask+1)/2){ // Should table shrink?
        hash_change_size(hash, ((hash->mask+1) / 2) - 1);
      }
      

      return i->value;
    }
      
    j = i;
    i = i->next;
  }

  return NULL;
  
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

/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////

static dfsch_object_t* native_make_hash(void* baton, dfsch_object_t* args){
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG_OPT(args, proc, NULL);
  DFSCH_ARG_END(args);

  return dfsch_hash_make(proc);
}
static dfsch_object_t* native_hash_p(void* baton, dfsch_object_t* args){
  dfsch_object_t* obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_hash_p(obj));
}
static dfsch_object_t* native_hash_ref(void* baton, dfsch_object_t* args){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_hash_ref(hash, key);
}
static dfsch_object_t* native_hash_unset(void* baton, dfsch_object_t* args){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_hash_unset(hash, key);
}
static dfsch_object_t* native_hash_set(void* baton, dfsch_object_t* args){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  return dfsch_hash_set(hash, key, value);
}
static dfsch_object_t* native_hash_set_if_exists(void* baton, 
                                                 dfsch_object_t* args){
  dfsch_object_t* hash;
  dfsch_object_t* key;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_OBJECT_ARG(args, key);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  return dfsch_hash_set_if_exists(hash, key, value);
}
static dfsch_object_t* native_hash_2_alist(void *baton, dfsch_object_t* args){
  dfsch_object_t* hash;

  DFSCH_OBJECT_ARG(args, hash);
  DFSCH_ARG_END(args);

  return dfsch_hash_2_alist(hash);
}

void dfsch__hash_native_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx, "make-hash", 
                   dfsch_make_primitive(&native_make_hash,NULL));
  dfsch_ctx_define(ctx, "hash?", 
                   dfsch_make_primitive(&native_hash_p,NULL));
  dfsch_ctx_define(ctx, "hash-ref", 
                   dfsch_make_primitive(&native_hash_ref,NULL));
  dfsch_ctx_define(ctx, "hash-unset!", 
                   dfsch_make_primitive(&native_hash_set,NULL));
  dfsch_ctx_define(ctx, "hash-set!", 
                   dfsch_make_primitive(&native_hash_set,NULL));
  dfsch_ctx_define(ctx, "hash-set-if-exists!", 
                   dfsch_make_primitive(&native_hash_set_if_exists,NULL));
  dfsch_ctx_define(ctx, "hash->alist", 
                   dfsch_make_primitive(&native_hash_2_alist,NULL));

}
