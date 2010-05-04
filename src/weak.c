/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Weak references
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

#include "dfsch/weak.h"

#include <dfsch/number.h>

#include "internal.h"
#include "util.h"

#include <gc/gc.h>

#include <stdlib.h>


/*
 * Weak references
 */

typedef struct reference_t {
  dfsch_type_t* type;
  dfsch_object_t* object;
  size_t live;
} reference_t;

dfsch_type_t dfsch_weak_reference_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(reference_t),
  "weak-reference",
  NULL,
  NULL,
  NULL
};

static int register_weak_pointer(void** pointer, void* object){
  if (object && GC_base(object)){ 
    /* Causes random crashes with non GC'able objects*/
    GC_general_register_disappearing_link(pointer, object);
    return 1; 
  }
  return 0;
}

/* We cannot use dfsch_make_object() here - we need to call GC_MALLOC_ATOMIC */
dfsch_object_t* dfsch_make_weak_reference(dfsch_object_t* refered){
  reference_t* ref = (reference_t*)GC_MALLOC_ATOMIC(sizeof(reference_t));

  ref->type = DFSCH_WEAK_REFERENCE_TYPE;
  
  ref->object = refered;
  ref->live = 1;
  register_weak_pointer((void**)&ref->live, refered);

  return (dfsch_object_t*)ref;
}

int dfsch_weak_reference_live_p(dfsch_object_t* reference){
  reference_t* ref;
  if (DFSCH_TYPE_OF(reference) != DFSCH_WEAK_REFERENCE_TYPE)
    dfsch_error("exception:not-a-reference", reference);
  ref = (reference_t*) reference;

  return ref->live;
}

dfsch_object_t* dereference(reference_t* ref){
  if (!ref->live){
    ref->object = NULL;
    return NULL;
  } else {
    return ref->object;
  }
}

dfsch_object_t* dfsch_weak_reference_dereference(dfsch_object_t* reference){
  reference_t* ref;
  if (DFSCH_TYPE_OF(reference) != DFSCH_WEAK_REFERENCE_TYPE)
    dfsch_error("exception:not-a-reference", reference);
  ref = (reference_t*) reference;

  return GC_call_with_alloc_lock((GC_fn_type)dereference, ref);
}

typedef struct weak_vector_t {
  dfsch_type_t* type;
  size_t length;
  dfsch_object_t** data;
  int set;
} weak_vector_t;


dfsch_type_t dfsch_weak_vector_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(weak_vector_t),
  "weak-vector",
  NULL,
  NULL,
  NULL,
  NULL
};

dfsch_object_t* dfsch_make_weak_vector(size_t length, dfsch_object_t* fill){
  weak_vector_t* v = (weak_vector_t*)dfsch_make_object(DFSCH_WEAK_VECTOR_TYPE);
  size_t i;

  v->length = length;
  v->set = 0;
  v->data = GC_MALLOC_ATOMIC(sizeof(dfsch_object_t*) * length);

  for(i = 0; i<length; ++i){
    v->data[i] = fill;
    v->set = register_weak_pointer((void**)&v->data[i], fill);
  }

  return (dfsch_object_t*)v;
}
size_t dfsch_weak_vector_length(dfsch_object_t *vector){
  if (DFSCH_TYPE_OF(vector) != DFSCH_WEAK_VECTOR_TYPE)
    return 0;

  return ((weak_vector_t*)vector)->length;  
}

dfsch_object_t** dfsch_weak_vector_as_array(dfsch_object_t *vector, 
                                            size_t *length){
  if (DFSCH_TYPE_OF(vector) != DFSCH_WEAK_VECTOR_TYPE)
    dfsch_error("exception:not-a-weak-vector",vector);

  if (length){
    *length = ((weak_vector_t*)vector)->length;
  }

  return ((weak_vector_t*)vector)->data;
}

dfsch_object_t* dfsch_weak_vector_from_array(dfsch_object_t **array, 
                                             size_t length){
  weak_vector_t* v = (weak_vector_t*)dfsch_make_object(DFSCH_WEAK_VECTOR_TYPE);
  size_t i;

  v->length = length;
  v->data = GC_MALLOC_ATOMIC(sizeof(dfsch_object_t*) * length);

  for(i = 0; i<length; ++i){
    v->data[i] = array[i];
    register_weak_pointer((void**)&v->data[i], array[i]);
  }

  return (dfsch_object_t*)v;
}


dfsch_object_t* dfsch_weak_vector_ref(dfsch_object_t *vector, size_t k){
  weak_vector_t* v;

  if (DFSCH_TYPE_OF(vector) != DFSCH_WEAK_VECTOR_TYPE)
    dfsch_error("exception:not-a-weak-vector",vector);

  return v->data[k];
}

dfsch_object_t* dfsch_weak_vector_set(dfsch_object_t* vector, size_t k, 
                                      dfsch_object_t* obj){
  weak_vector_t* v;

  if (DFSCH_TYPE_OF(vector) != DFSCH_WEAK_VECTOR_TYPE)
    dfsch_error("exception:not-a-weak-vector",vector);

  v = (weak_vector_t*) vector;

  if (v->length <= k){
    dfsch_error("exception:invalid-index",dfsch_make_number_from_long(k));
  }
  
  if (v->set){
    GC_unregister_disappearing_link((void**)&v->data[k]);
  }

  v->data[k] = obj;
  v->set = register_weak_pointer((void**)&v->data[k], obj);

  return vector;
}

dfsch_object_t* dfsch_weak_vector_2_list(dfsch_object_t* vector){

  if (DFSCH_TYPE_OF(vector) != DFSCH_WEAK_VECTOR_TYPE)
    dfsch_error("exception:not-a-vector",vector);

  return dfsch_list_from_array(((weak_vector_t*)vector)->data, 
                               ((weak_vector_t*)vector)->length);
}

dfsch_object_t* dfsch_list_2_weak_vector(dfsch_object_t* list){
  dfsch_object_t** data;
  size_t length;
  data = dfsch_list_as_array(list, &length);
  return dfsch_weak_vector_from_array(data, length);
}

/*
 * weak-key-hash
 */ 

#define HIDE_OBJECT(o) ((dfsch_object_t*) ~((size_t) (o)))
#define REVEAL_OBJECT(o) HIDE_OBJECT(o)

typedef struct weak_hash_entry_t weak_hash_entry_t;

struct weak_hash_entry_t {
  dfsch_object_t* key;
  dfsch_object_t* value;
  size_t live;
  weak_hash_entry_t* next;
};

static weak_hash_entry_t* weak_hash_entry_create(dfsch_object_t* key,
                                                 dfsch_object_t* value,
                                                 weak_hash_entry_t* next){
  weak_hash_entry_t* e = GC_NEW(weak_hash_entry_t);

  e->key = key;
  e->value = value;
  e->next = next;
  e->live = 1;

  return e;
}
static weak_hash_entry_t* find_entry(weak_hash_entry_t** head,
                                     dfsch_object_t* key,
                                     size_t* count){
  weak_hash_entry_t* tmp;
  weak_hash_entry_t* i;
  weak_hash_entry_t* res = NULL;

  i = *head;

  while (i && !i->live){
    i = i->next;
    *head = i;
    (*count)--;
  }

  if (!i){
    return NULL;
  }

  if (i->key == key){
    res = i;
  }
  tmp = i;
  i = i->next;

  while (i){
    if (!i->live){
      tmp->next = i->next;
      i = i->next;
      (*count)--;
      continue;
    }
    if (i->key == key){
      res = i;
    }
    i = i->next;
  }

  return res;
}

typedef struct weak_key_hash_t {
  dfsch_type_t* type;
  dfsch_rwlock_t* lock;
  size_t mask;
  size_t count;
  weak_hash_entry_t** buckets;
} weak_key_hash_t;

#define DEFAULT_WEAK_KEY_HASH_SIZE 128

dfsch_object_t* dfsch_make_weak_key_hash(){
  weak_key_hash_t* h = 
    (weak_key_hash_t*)dfsch_make_object(DFSCH_WEAK_KEY_HASH_TYPE);

  h->mask = DEFAULT_WEAK_KEY_HASH_SIZE - 1;
  h->buckets = GC_MALLOC(DEFAULT_WEAK_KEY_HASH_SIZE*sizeof(weak_hash_entry_t*));
  h->count = 0;
  h->lock = DFSCH_CREATE_RWLOCK();

  return (dfsch_object_t*)h;
}

static weak_hash_entry_t* weak_key_entry_create(dfsch_object_t* key,
                                                dfsch_object_t* value,
                                                weak_hash_entry_t* next){
  weak_hash_entry_t* e = weak_hash_entry_create(HIDE_OBJECT(key), value, next);
  register_weak_pointer(((void**)&e->live), key);
  return e;
}
static size_t ptr_hash(dfsch_object_t* ptr){
  size_t a = (size_t)ptr;        
  size_t b = (size_t)ptr >> 16 | (size_t)ptr << 16;

  a ^= b >> 2;
  b ^= a >> 3;
  a ^= b << 5;
  b ^= a << 7;
  a ^= b >> 11;
  b ^= a >> 13;
  a ^= b << 17;
  b ^= a << 23;
  
  return b ^ a;
}
static dfsch_object_t* weak_key_hash_ref(weak_key_hash_t* h,
                                         dfsch_object_t* key){
  weak_hash_entry_t* e;
  dfsch_object_t* res;
  uint32_t hash = ptr_hash(key);
  
  /* 
   * There is no need to perform any special locking regarding GC,
   * worst possible outcome of race condition with GC here is returning
   * value that should be instead discarded, but this is still perfectly 
   * valid value. It is reasonable to expect that user code can cope with 
   * this in some meaningful manner (with doing nothing being pretty 
   * reasonable).
   */
  DFSCH_RWLOCK_RDLOCK(h->lock);

  e = find_entry(&h->buckets[hash & h->mask], HIDE_OBJECT(key), &h->count);

  if (!e){
    DFSCH_RWLOCK_UNLOCK(h->lock);
    return DFSCH_INVALID_OBJECT;
  }

  
  res = e->value;
  DFSCH_RWLOCK_UNLOCK(h->lock);
  return res;
}
static void weak_key_hash_set(weak_key_hash_t* h,
                             dfsch_object_t* key,
                             dfsch_object_t* value){
  uint32_t hash = ptr_hash(key);
  weak_hash_entry_t* e;
  
  DFSCH_RWLOCK_WRLOCK(h->lock);

  e = find_entry(&h->buckets[hash & h->mask], HIDE_OBJECT(key), &h->count);
  
  if (e){
    e->value = value;
  }else{
    h->count++;
    h->buckets[hash & h->mask] = 
      weak_key_entry_create(key,
                            value,
                            h->buckets[hash & h->mask]); 
  }

  DFSCH_RWLOCK_UNLOCK(h->lock);
}

static dfsch_mapping_methods_t weak_key_hash_map = {
  .ref = weak_key_hash_ref,
  .set = weak_key_hash_set
};

dfsch_type_t dfsch_weak_key_hash_type = {
  DFSCH_STANDARD_TYPE,
  NULL, //DFSCH_MAPPING_TYPE,
  sizeof(weak_key_hash_t),
  "weak-key-hash",
  NULL,
  NULL,
  NULL,
  NULL,

  .mapping = &weak_key_hash_map,
};



/*
 * Scheme binding
 */

DFSCH_DEFINE_PRIMITIVE(make_weak_reference, NULL){
  dfsch_object_t* object;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_weak_reference(object);
}
DFSCH_DEFINE_PRIMITIVE(weak_reference_live_p, NULL){
  dfsch_object_t* reference;

  DFSCH_OBJECT_ARG(args, reference);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_weak_reference_live_p(reference));
}
DFSCH_DEFINE_PRIMITIVE(weak_reference_dereference, NULL){
  dfsch_object_t* reference;

  DFSCH_OBJECT_ARG(args, reference);
  DFSCH_ARG_END(args);

  return dfsch_weak_reference_dereference(reference);
}


/***************************************************************/


DFSCH_DEFINE_PRIMITIVE(make_weak_vector, NULL){
  size_t length;
  dfsch_object_t* fill;

  DFSCH_LONG_ARG(args, length);
  DFSCH_OBJECT_ARG_OPT(args, fill, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_weak_vector(length,fill);
}

DFSCH_DEFINE_PRIMITIVE(weak_vector, NULL){
  return dfsch_list_2_weak_vector(args);
}
DFSCH_DEFINE_PRIMITIVE(weak_vector_length, NULL){
  dfsch_object_t* vector;
  
  DFSCH_OBJECT_ARG(args,vector);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_weak_vector_length(vector));

}
DFSCH_DEFINE_PRIMITIVE(weak_vector_ref, NULL){
  dfsch_object_t* vector;
  size_t k;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_ARG_END(args);

  return dfsch_weak_vector_ref(vector, k);
}

DFSCH_DEFINE_PRIMITIVE(weak_vector_set, NULL){
  dfsch_object_t* vector;
  size_t k;
  dfsch_object_t* obj;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_weak_vector_set(vector, k, obj);
}

DFSCH_DEFINE_PRIMITIVE(weak_vector_2_list, NULL){
  dfsch_object_t* vector;

  DFSCH_OBJECT_ARG(args, vector);
  DFSCH_ARG_END(args);

  return dfsch_weak_vector_2_list(vector);
}

DFSCH_DEFINE_PRIMITIVE(list_2_weak_vector, NULL){
  dfsch_object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_weak_vector(list);
}

/****************************************************************/

DFSCH_DEFINE_PRIMITIVE(make_weak_key_hash, 0){
  DFSCH_ARG_END(args);
  return dfsch_make_weak_key_hash();
}

/****************************************************************/


void dfsch__weak_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<weak-reference>", DFSCH_WEAK_REFERENCE_TYPE);
  dfsch_define_cstr(ctx, "<weak-vector>", DFSCH_WEAK_VECTOR_TYPE);
  dfsch_define_cstr(ctx, "<weak-key-hash>", DFSCH_WEAK_KEY_HASH_TYPE);

  dfsch_define_cstr(ctx, "make-weak-reference", 
                    DFSCH_PRIMITIVE_REF(make_weak_reference));
  dfsch_define_cstr(ctx, "weak-reference-live?", 
                    DFSCH_PRIMITIVE_REF(weak_reference_live_p));
  dfsch_define_cstr(ctx, "weak-reference-dereference", 
                    DFSCH_PRIMITIVE_REF(weak_reference_dereference));
  
  dfsch_define_cstr(ctx, "make-weak-vector", 
                   DFSCH_PRIMITIVE_REF(make_weak_vector));
  dfsch_define_cstr(ctx, "weak-vector", 
                   DFSCH_PRIMITIVE_REF(weak_vector));
  dfsch_define_cstr(ctx, "weak-vector-length", 
                   DFSCH_PRIMITIVE_REF(weak_vector_length));
  dfsch_define_cstr(ctx, "weak-vector-set!", 
                   DFSCH_PRIMITIVE_REF(weak_vector_set));
  dfsch_define_cstr(ctx, "weak-vector-ref", 
                   DFSCH_PRIMITIVE_REF(weak_vector_ref));
  dfsch_define_cstr(ctx, "weak-vector->list", 
                   DFSCH_PRIMITIVE_REF(weak_vector_2_list));
  dfsch_define_cstr(ctx, "list->weak-vector", 
                   DFSCH_PRIMITIVE_REF(list_2_weak_vector));

  dfsch_define_cstr(ctx, "make-weak-key-hash", 
                    DFSCH_PRIMITIVE_REF(make_weak_key_hash));

}


