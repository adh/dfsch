/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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

/** @file dfsch.c This is implementation of dfsch interpreter. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>
#include <dfsch/number.h>
#include <dfsch/parse.h>
#include <dfsch/strings.h>
#include <dfsch/magic.h>
#include <dfsch/conditions.h>
#include <dfsch/introspect.h>
#include <dfsch/weak.h>
#include <dfsch/backquote.h>
#include <dfsch/load.h>
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include "types.h"
#include "version.h"

//#define ALLOC_DEBUG

#ifdef ALLOC_DEBUG
static int obj_count = 0;
static int obj_size = 0;
#endif

dfsch_object_t* dfsch_make_object_var(const dfsch_type_t* type, size_t size){
  object_t* o = GC_MALLOC(type->size + size);
  if (!o)
    return NULL;

  o->type = (dfsch_type_t*)type;

#ifdef ALLOC_DEBUG
  obj_count ++;
  obj_size += type->size + size;
  printf(";; Alloc'd: #<%s 0x%x> serial %d arena %d\n", type->name, o, 
         obj_count, obj_size);
#endif

  return o;
}

object_t* dfsch_make_object(const dfsch_type_t* type){
  return dfsch_make_object_var(type, 0);
}


int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b){
  return (a==b);
}

int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a || !b)
    return 0;

  if ((DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b)) && dfsch_number_p(a))
    return dfsch_number_equal_p(a,b);

  return 0;
}

int dfsch_equal_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a || !b)
    return 0;

  if (DFSCH_TYPE_OF(a) != DFSCH_TYPE_OF(b)){
    if (DFSCH_PAIR_P(a) && DFSCH_PAIR_P(b)){
      return (dfsch_equal_p(DFSCH_FAST_CAR(a), DFSCH_FAST_CAR(b)) &&
              dfsch_equal_p(DFSCH_FAST_CDR(a), DFSCH_FAST_CDR(b)));
    } else {
      return 0;
    }
  }

  if (!DFSCH_TYPE_OF(a))
    return 0;
  if (!DFSCH_TYPE_OF(a)->equal_p)
    return 0;

  return DFSCH_TYPE_OF(a)->equal_p(a,b);
}

static uint32_t ptr_hash(dfsch_object_t* ptr){
  uint32_t a = (size_t)ptr;        
  uint32_t b = (size_t)ptr >> 16 | (size_t)ptr << 16;

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

uint32_t dfsch_hash(dfsch_object_t* obj){
  if (!obj){
    return 0;
  }
  if (!DFSCH_TYPE_OF(obj) || !DFSCH_TYPE_OF(obj)->hash){
    return ptr_hash(obj);
  }

  return DFSCH_TYPE_OF(obj)->hash(obj);
}

dfsch_type_t* dfsch_type_of(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj);
}

dfsch_type_t* dfsch_object_as_type(dfsch_object_t* obj){
  return (dfsch_type_t*)DFSCH_ASSERT_INSTANCE(obj, DFSCH_STANDARD_TYPE);
}

dfsch_object_t* dfsch_superclass(dfsch_object_t* obj){  
  return (dfsch_object_t*)dfsch_object_as_type(obj)->superclass;
}

int dfsch_superclass_p(dfsch_type_t* sub, dfsch_type_t* super){
  if (!super){
    return 1;
  }

  if (sub == super)
    return 1;

  while (sub){
    sub = sub->superclass;
    if (sub == super){
      return 1;
    }
  }

  return 0;
}
int dfsch_instance_p(dfsch_object_t* obj, dfsch_type_t* type){
  return dfsch_superclass_p(DFSCH_TYPE_OF(obj), type);
}

void* dfsch_assert_type(dfsch_object_t* obj, dfsch_type_t* type){
  dfsch_object_t* o = obj;
  while (DFSCH_TYPE_OF(o) != type){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_type_error(o, type, 0);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}
dfsch_object_t* dfsch_assert_instance(dfsch_object_t* obj, 
                                      dfsch_type_t* type){
  dfsch_object_t* o = obj;
  while (!DFSCH_INSTANCE_P(o, type)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_type_error(o, type, 1);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}

dfsch_object_t* dfsch_assert_collection(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!DFSCH_COLLECTION_P(o)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_type_error(o, NULL, 1); //XXX
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}
dfsch_object_t* dfsch_assert_mapping(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!DFSCH_MAPPING_P(o)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_type_error(o, NULL, 1); //XXX
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}
dfsch_object_t* dfsch_assert_sequence(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!DFSCH_SEQUENCE_P(o)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_type_error(o, NULL, 1); //XXX
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
}
size_t dfsch_assert_sequence_index(dfsch_object_t* seq, 
                                   size_t idx, size_t len){
  dfsch_object_t* o;
  while (idx >= len){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_index_error(seq, idx, len);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
    idx = dfsch_number_to_long(o);
  }
}

dfsch_object_t* dfsch_collection_get_iterator(dfsch_object_t* col){
  dfsch_object_t* c = DFSCH_ASSERT_COLLECTION(col);
  return DFSCH_TYPE_OF(c)->collection->get_iterator(c);
}
dfsch_object_t* dfsch_sequence_ref(dfsch_object_t* seq,
                                   size_t k){
  dfsch_object_t* s = DFSCH_ASSERT_SEQUENCE(seq);
  return DFSCH_TYPE_OF(s)->sequence->ref(s, k);  
}
void dfsch_sequence_set(dfsch_object_t* seq,
                                   size_t k,
                                   dfsch_object_t* value){
  dfsch_object_t* s = DFSCH_ASSERT_SEQUENCE(seq);
  if (!DFSCH_TYPE_OF(s)->sequence->set){
    dfsch_error("Sequence is immutable", s);
  }
  DFSCH_TYPE_OF(s)->sequence->set(s, k, value);  
}
size_t dfsch_sequence_length(dfsch_object_t* seq){
  dfsch_object_t* s = DFSCH_ASSERT_SEQUENCE(seq);
  if (!DFSCH_TYPE_OF(s)->sequence->length){
    return 0;
  }
  return DFSCH_TYPE_OF(s)->sequence->length(s);  
}
dfsch_object_t* dfsch_iterator_next(dfsch_object_t* iterator){
  if (DFSCH_PAIR_P(iterator)){
    dfsch_object_t* ret = DFSCH_FAST_CDR(iterator);
    if (DFSCH_PAIR_P(ret)){
      return ret;
    } else if (!ret) {
      return NULL;
    } else {
      dfsch_error("Not a proper list", ret);
    }
  }
  if (DFSCH_TYPE_OF(DFSCH_TYPE_OF(iterator)) != DFSCH_ITERATOR_TYPE_TYPE){
    dfsch_error("Object is not usable as iterator", iterator);
  }
  return ((dfsch_iterator_type_t*)DFSCH_TYPE_OF(iterator))->next(iterator);
}
dfsch_object_t* dfsch_iterator_this(dfsch_object_t* iterator){
  if (DFSCH_PAIR_P(iterator)){
    return DFSCH_FAST_CAR(iterator);
  }
  if (DFSCH_TYPE_OF(DFSCH_TYPE_OF(iterator)) != DFSCH_ITERATOR_TYPE_TYPE){
    dfsch_error("Object is not usable as iterator", iterator);
  }
  return ((dfsch_iterator_type_t*)DFSCH_TYPE_OF(iterator))->this(iterator);
}

dfsch_object_t* dfsch_mapping_ref(dfsch_object_t* map,
                                  dfsch_object_t* key){
  dfsch_object_t* m = DFSCH_ASSERT_MAPPING(map);
  return DFSCH_TYPE_OF(m)->mapping->ref(m, key);  
}

void dfsch_mapping_set(dfsch_object_t* map,
                       dfsch_object_t* key,
                       dfsch_object_t* value){
  dfsch_object_t* m = DFSCH_ASSERT_MAPPING(map);
  if (!DFSCH_TYPE_OF(m)->mapping->set){
    dfsch_error("Mapping is immutable", m);
  }
  DFSCH_TYPE_OF(m)->mapping->set(m, key, value);  
}

int dfsch_mapping_unset(dfsch_object_t* map,
                         dfsch_object_t* key){
  dfsch_object_t* m = DFSCH_ASSERT_MAPPING(map);
  if (!DFSCH_TYPE_OF(m)->mapping->unset){
    dfsch_error("Mapping does not support removal of keys", m);
  }
  return DFSCH_TYPE_OF(m)->mapping->unset(m, key);  
}

int dfsch_mapping_set_if_exists(dfsch_object_t* map,
                                 dfsch_object_t* key,
                                 dfsch_object_t* value){
  dfsch_object_t* m = DFSCH_ASSERT_MAPPING(map);
  if (!DFSCH_TYPE_OF(m)->mapping->set_if_exists){
    if (dfsch_mapping_ref(map, key) != DFSCH_INVALID_OBJECT){
      dfsch_mapping_set(map, key, value);
      return 1;
    } 
    return 0;
  } else {
    return DFSCH_TYPE_OF(m)->mapping->set_if_exists(m, key, value);  
  }
}

int dfsch_mapping_set_if_not_exists(dfsch_object_t* map,
                                     dfsch_object_t* key,
                                     dfsch_object_t* value){
  dfsch_object_t* m = DFSCH_ASSERT_MAPPING(map);
  if (!DFSCH_TYPE_OF(m)->mapping->set_if_not_exists){
    if (dfsch_mapping_ref(map, key) == DFSCH_INVALID_OBJECT){
      dfsch_mapping_set(map, key, value);
      return 1;
    } 
    return 0;
  } else {
    return DFSCH_TYPE_OF(m)->mapping->set_if_not_exists(m, key, value);  
  }
}




static pthread_key_t thread_key;
static pthread_once_t thread_once = PTHREAD_ONCE_INIT;

static void thread_info_destroy(void* ptr){
  if (ptr){
    GC_FREE(ptr);
  }
}
static void thread_key_alloc(){
  pthread_key_create(&thread_key, thread_info_destroy);
}

static int default_trace_depth = 3;

static dfsch__tracepoint_t* alloc_trace_buffer(dfsch__thread_info_t* ti,
                                               int depth){
  size_t size;
  depth = depth % (8 * sizeof(short));
  size = 1 << depth;
  ti->trace_depth = size - 1;
  ti->trace_ptr = 0;
  ti->trace_buffer = GC_MALLOC(sizeof(dfsch__tracepoint_t) * size);
}

void dfsch_set_trace_depth(int depth){
  alloc_trace_buffer(dfsch__get_thread_info(), depth);
}
void dfsch_set_default_trace_depth(int depth){
  default_trace_depth = depth;
  dfsch_set_trace_depth(depth);
}

struct dfsch_saved_trace_t {
  dfsch__tracepoint_t* trace_buffer;
  short trace_ptr;
  short trace_depth;
};
dfsch_saved_trace_t* dfsch_save_trace_buffer(){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  dfsch_saved_trace_t* st = GC_NEW(dfsch_saved_trace_t);
  
  st->trace_buffer = GC_MALLOC(sizeof(dfsch__tracepoint_t) * (ti->trace_depth + 1));
  memcpy(st->trace_buffer, ti->trace_buffer, 
         sizeof(dfsch__tracepoint_t) * (ti->trace_depth + 1));
  st->trace_depth = ti->trace_depth;
  st->trace_ptr = ti->trace_ptr;

  return st;
}
void dfsch_restore_trace_buffer(dfsch_saved_trace_t* st){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();

  ti->trace_buffer = st->trace_buffer;
  ti->trace_ptr = st->trace_ptr;
  ti->trace_depth = st->trace_depth;
}


dfsch__thread_info_t* dfsch__get_thread_info(){
  dfsch__thread_info_t *ei;
  pthread_once(&thread_once, thread_key_alloc);
  ei = pthread_getspecific(thread_key);
  if (DFSCH_UNLIKELY(!ei)){
    ei = GC_MALLOC_UNCOLLECTABLE(sizeof(dfsch__thread_info_t)); 
    ei->throw_ret = NULL;
    ei->async_apply = NULL;
    ei->restart_list = dfsch__get_default_restart_list();
#ifdef DFSCH_GC_MALLOC_MANY_PREALLOC
    ei->env_freelist = GC_malloc_many(sizeof(environment_t));
#endif
    alloc_trace_buffer(ei, default_trace_depth);
    pthread_setspecific(thread_key, ei);
  }
  return ei;
}

/*
 * It seems so volatile really isn't needed around this magic, only automatic
 * variables that have been _CHANGED_ since call to setjmp(3) are indeterminate
 * and there are none such. Actually only thing that happen in these functions
 * between setjmp(3) and opportunity to call longjmp(3) (from corresponding 
 * wrapper function) is function call. This is only case of undefined behavior
 * related to "proper" use of setjmp(3)/longjmp(3) in IEEE 1003.1.
 */

void dfsch__continue_unwind(dfsch__thread_info_t* ti){
  if (!ti->throw_ret){
    if (ti->throw_tag == DFSCH_INVALID_OBJECT){
      pthread_exit(ti->throw_value);
    }
    fputs("No unwind target!!!\n", stderr);
    abort();
  }
  longjmp(*ti->throw_ret, 1);
}
void dfsch__finalize_unwind(dfsch__thread_info_t* ti){
  ti->throw_tag = NULL;
  ti->throw_value = NULL;
}

void dfsch_terminate_thread(dfsch_object_t* ret){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  ti->throw_tag = DFSCH_INVALID_OBJECT;
  ti->throw_value = ret;
  dfsch__continue_unwind(ti);
}

void dfsch_throw(dfsch_object_t* tag,
                 dfsch_object_t* value){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  dfsch__catch_list_t* i = ti->catch_list;
  while (i){
    if (i->tag == tag){
      ti->throw_tag = tag;
      ti->throw_value = value;
      dfsch__continue_unwind(ti);
    }
    i = i->next;
  }
  dfsch_error("Invalid catch tag", tag);
}

void dfsch_error(char* name, 
                 dfsch_object_t* detail){
  dfsch_signal(dfsch_condition(DFSCH_ERROR_TYPE, 
                               "message", dfsch_make_string_cstr(name),
                               "object", detail,
                               NULL));
}
void dfsch_cerror(char* name, 
                  dfsch_object_t* detail){
  DFSCH_WITH_SIMPLE_RESTART(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                "continue"),
                            "Ignore error condition"){
    dfsch_signal(dfsch_condition(DFSCH_ERROR_TYPE, 
                                 "message", dfsch_make_string_cstr(name),
                                 "object", detail,
                                 NULL));
  } DFSCH_END_WITH_SIMPLE_RESTART;
}

void dfsch_async_apply_self(dfsch_object_t* proc){
  dfsch__get_thread_info()->async_apply = proc;
}

char* dfsch_object_2_string(dfsch_object_t* obj, 
                            int max_depth, int readable){
  str_list_t* sl = sl_create();
  if (max_depth >= 0){
    dfsch_writer_state_t* state = 
      dfsch_make_writer_state(max_depth,
                              readable?
                              DFSCH_WRITE:
                              DFSCH_PRINT,
                              (dfsch_output_proc_t)sl_append,
                              sl);
    dfsch_write_object(state, obj);
    dfsch_invalidate_writer_state(state);
  } else {
    dfsch_write_object_circular(obj, 
                                readable?
                                DFSCH_WRITE:
                                DFSCH_PRINT,
                                (dfsch_output_proc_t)sl_append,
                                sl);
  }
  return sl_value(sl);
}

typedef struct read_ctx_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;  
} read_ctx_t;

static read_callback(dfsch_object_t *obj, void* ctx){
  dfsch_object_t* new_tail = dfsch_cons(obj, NULL);

  if (!((read_ctx_t*)ctx)->head){
    ((read_ctx_t*)ctx)->head = new_tail;
  }else{
    dfsch_set_cdr(((read_ctx_t*)ctx)->tail, new_tail);
  }

  ((read_ctx_t*)ctx)->tail = new_tail;

  return 1;
}

dfsch_object_t* dfsch_string_2_object_list(char* str){
  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  read_ctx_t ctx;
  int err;
  dfsch_parser_callback(parser, read_callback, &ctx);

  ctx.head = ctx.tail = NULL;
  
  err = dfsch_parser_feed(parser, str);
  if (!err)
    dfsch_parser_feed(parser, " ");

  if ((err && err != DFSCH_PARSER_STOPPED)
      || dfsch_parser_get_level(parser)!=0){
    dfsch_error("Syntax error",NULL);
  }  
  
  return ctx.head;
}
dfsch_object_t* dfsch_string_2_object(char* str){
  object_t* list = dfsch_string_2_object_list(str);
  if (!list) {
    dfsch_error("Input empty", NULL);
  }
  return dfsch_car(list);
}

static dfsch_rwlock_t environment_rwlock = DFSCH_RWLOCK_INITIALIZER;

static environment_t* alloc_environment(dfsch__thread_info_t* ti){
  environment_t* e;

#ifdef DFSCH_GC_MALLOC_MANY
  if (!ti->env_freelist){
    ti->env_freelist = GC_malloc_many(sizeof(environment_t));
  }
  e = ti->env_freelist;
  ti->env_freelist = GC_NEXT(ti->env_freelist);
  ti->env_fl_depth--;
#else
  e = GC_NEW(environment_t);
#endif

  ((dfsch_object_t*)e)->type = DFSCH_ENVIRONMENT_TYPE;
  e->flags = 0;
  return e;
}

dfsch_object_t* dfsch_reify_environment(dfsch_object_t* env){
  environment_t* i = env;
  while (i && (i->flags & EFRAME_RETAIN) == 0){
    i->flags |= EFRAME_RETAIN;
    i = i->parent;
  }
  
  return env;
}

#define ENV_FREELIST_MAX_DEPTH 32

static void free_environment(environment_t* env, dfsch__thread_info_t* ti){
  if ((env->flags & EFRAME_RETAIN) == 0 &&
      ti->env_fl_depth < ENV_FREELIST_MAX_DEPTH){
    int old_serial = env->flags & EFRAME_SERIAL_MASK;
    memset(env, 0, sizeof(environment_t));
    env->type = ti->env_freelist;
    env->flags = old_serial;
    ti->env_freelist = env;
    ti->env_fl_depth++;
  }
}

static environment_t* initialize_frame(environment_t* e,
                                       environment_t* parent,
                                       dfsch_object_t* context,
                                       dfsch__thread_info_t* ti){
  dfsch_eqhash_init(&e->values, 0);
  e->flags += EFRAME_SERIAL_INCR;
  e->decls = NULL;
  e->context = context;
  e->owner = ti;
  e->parent = (environment_t*)parent;  
}

static environment_t* maybe_reuse_frame(environment_t* e,
                                        environment_t* parent,
                                        dfsch_object_t* context,
                                        dfsch__thread_info_t* ti){
  if ((e->flags & EFRAME_RETAIN) != 0){
    e = alloc_environment(ti);
  }
  
  initialize_frame(e, parent, context, ti);

  return e;
}

static environment_t* new_frame_impl(environment_t* parent,
                                     dfsch_object_t* context,
                                     dfsch__thread_info_t* ti){
  environment_t* e = alloc_environment(ti);
  
  initialize_frame(e, parent, context, ti);

  return e;
}

dfsch_object_t* dfsch_new_frame_with_context(dfsch_object_t* parent,
                                             dfsch_object_t* context){
  if (parent){
    parent = DFSCH_ASSERT_TYPE(parent, DFSCH_ENVIRONMENT_TYPE);
  }
  return (dfsch_object_t*)new_frame_impl((environment_t*)parent,
                                         context,
                                         dfsch__get_thread_info());
}
dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent){
  return dfsch_new_frame_with_context(parent, NULL);
}

static object_t* lookup_impl(object_t* name, 
                             environment_t* env,
                             dfsch__thread_info_t* ti){
  environment_t *i;
  object_t* ret;

  i = env;
  while (i){
    if (DFSCH_UNLIKELY(i->owner != ti)){
      i->owner = NULL;
      goto lock;
    }
    ret = dfsch_eqhash_ref(&i->values, name);
    if (ret != DFSCH_INVALID_OBJECT){
      return ret;
    }
    
    i = i->parent;
  }

  goto unbound;
 lock:
   DFSCH_RWLOCK_RDLOCK(&environment_rwlock);
  while (i){
    ret = dfsch_eqhash_ref(&i->values, name);
    if (ret != DFSCH_INVALID_OBJECT){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      return ret;
    }
    
    i->owner = NULL;
    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);

 unbound:
  if (DFSCH_SYMBOL_P(name) && 
      ((dfsch__symbol_t*)DFSCH_TAG_REF(name))->package == DFSCH_KEYWORD_PACKAGE){
    return name; /* keywords are self-evaluating when not redefined */
  }
  dfsch_error("Unbound variable", dfsch_cons(name, (dfsch_object_t*)env));
}

object_t* dfsch_lookup(object_t* name, object_t* env){
  return lookup_impl(name, 
                     DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE),
                     dfsch__get_thread_info());
}

object_t* dfsch_env_get(object_t* name, object_t* env){
  environment_t *i;
  object_t* ret;

  i = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  DFSCH_RWLOCK_RDLOCK(&environment_rwlock);
  while (i){
    ret = dfsch_eqhash_ref(&i->values, name);
    if (ret != DFSCH_INVALID_OBJECT){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      return ret;
    }

    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  if (DFSCH_SYMBOL_P(name) && 
      ((dfsch__symbol_t*)DFSCH_TAG_REF(name))->package == DFSCH_KEYWORD_PACKAGE){
    return name; /* keywords are self-evaluating when not redefined */
  }
  return DFSCH_INVALID_OBJECT;
}

dfsch_object_t* dfsch_env_revscan(dfsch_object_t* env, 
                                  dfsch_object_t* value, 
                                  int canonical){
  environment_t *i;
  object_t* ret;

  i = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  DFSCH_RWLOCK_RDLOCK(&environment_rwlock);
  while (i){
    ret = dfsch_eqhash_revscan(&i->values, value, 
                               canonical ? DFSCH_VAR_CANONICAL : 0);
    if (ret != DFSCH_INVALID_OBJECT){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      return ret;
    }

    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  return DFSCH_INVALID_OBJECT;  
}



dfsch_object_t* dfsch_variable_constant_value(object_t* name, object_t* env){
  environment_t *i;
  short flags;
  dfsch_object_t* value;

  i = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  DFSCH_RWLOCK_RDLOCK(&environment_rwlock);
  while (i){
    if (dfsch_eqhash_ref_ex(&i->values, name, &value, &flags, NULL)){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      if (flags & DFSCH_VAR_CONSTANT){
        return value;
      } else {
        return DFSCH_INVALID_OBJECT;
      }
    }

    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  return DFSCH_INVALID_OBJECT;
}

object_t* dfsch_set(object_t* name, object_t* value, object_t* env){
  environment_t *i;
  dfsch__thread_info_t *ti = dfsch__get_thread_info();

  i = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);

  while (i){
    if (i->owner != ti){
      i->owner = NULL;
      goto lock;
    }
    if(dfsch_eqhash_set_if_exists(&i->values, name, value, NULL))
      return value;

    i = i->parent;
  }

  dfsch_error("Unbound variable",name);
 lock:
  
  DFSCH_RWLOCK_WRLOCK(&environment_rwlock);
  while (i){
    if(dfsch_eqhash_set_if_exists(&i->values, name, value, NULL)){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      return value;
    }
    i->owner = NULL;
    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  dfsch_error("Unbound variable",name);

}
void dfsch_unset(object_t* name, object_t* env){
  environment_t *i;

  i = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  DFSCH_RWLOCK_WRLOCK(&environment_rwlock);
  while (i){
    if (i->decls){
      dfsch_hash_unset(i->decls, name);
    }
    if(dfsch_eqhash_unset(&i->values, name)){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      return;
    }
    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  
  dfsch_error("Unbound variable",name);
}


void dfsch_define(object_t* name, object_t* value, object_t* env,
                  unsigned short flags){
  environment_t* e = (environment_t*)DFSCH_ASSERT_TYPE(env, 
                                                       DFSCH_ENVIRONMENT_TYPE);
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  if (e->owner != ti){
    e->owner = NULL;
    DFSCH_RWLOCK_WRLOCK(&environment_rwlock);
  }
  dfsch_eqhash_set(&e->values, name, value);  
  if (flags){
    dfsch_eqhash_set_flags(&e->values, name, flags);  
  }
  if (e->owner != ti){
    DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  }
}



void dfsch_declare(dfsch_object_t* variable, dfsch_object_t* declaration,
                   dfsch_object_t* env){
  dfsch_object_t* old = NULL;
  environment_t* e = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  dfsch__thread_info_t *ti = dfsch__get_thread_info();

  if (e->owner != ti){
    e->owner = NULL;
    DFSCH_RWLOCK_WRLOCK(&environment_rwlock);
  }


  if (!e->decls){
    e->decls = dfsch_hash_make(DFSCH_HASH_EQ);
  } else {
    dfsch_hash_ref_fast(e->decls, variable, &old);
  }
  
  dfsch_hash_set(e->decls, variable, 
                 dfsch_cons(declaration, old));  

  if (e->owner != ti){
    DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  }
}

dfsch_object_t* dfsch_get_environment_variables(dfsch_object_t* env){
  environment_t* e = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  dfsch_object_t* res;
  DFSCH_RWLOCK_RDLOCK(&environment_rwlock);
  res = dfsch_eqhash_2_alist(&e->values);
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  return res;
}
dfsch_object_t* dfsch_find_lexical_context(dfsch_object_t* env,
                                           dfsch_type_t* klass){
  environment_t* e = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  while (e){
    if (DFSCH_INSTANCE_P(e->context, klass)){
      return e->context;
    }
    e = e->parent;
  }

  return NULL;
}

static dfsch_object_t* macro_expand_impl(dfsch_object_t* macro,
                                         dfsch_object_t* expr,
                                         dfsch__thread_info_t* ti){
  dfsch_object_t* new_expr;
  dfsch_object_t* old_expr;
  int old_flags;
  
  DFSCH_UNWIND {
    old_expr = ti->macroexpanded_expr;
    old_flags = ti->trace_flags;
    ti->macroexpanded_expr = expr;
    ti->trace_flags |= 
      ti->macroexpanded_expr ? DFSCH_TRACEPOINT_FLAG_MACROEXPAND : 0;
    new_expr = dfsch_apply(((macro_t*)DFSCH_ASSERT_TYPE(macro, 
                                                        DFSCH_MACRO_TYPE))->proc, 
                           DFSCH_FAST_CDR(expr));
  } DFSCH_PROTECT {
    ti->macroexpanded_expr = old_expr;    
    ti->trace_flags = old_flags;
  } DFSCH_PROTECT_END;

  return new_expr;
}

dfsch_object_t* dfsch_macro_expand(dfsch_object_t* macro,
                                   dfsch_object_t* args){
  return macro_expand_impl(macro, dfsch_cons(macro, args), 
                           dfsch__get_thread_info());
}


// Evaluator

/*
 * There are some kinds of structures that are passed extensively inside the 
 * evaluator, but are not exposed to public API much. Idea there is that 
 * dfsch__get_thread_info() can be somewhat slow (on linux, it isn't 
 * noticeably slow, but who knows) and thus it is not exactly bad idea to 
 * cache it's result, but passing this value to user code causes marginal 
 * speedup at cost of having one additional argument that is not useful in 
 * any meaningful way to user code.
 *
 * dfsch_tail_escape_t is used to implement tail recursion, it is used only 
 * in dfsch_eval_proc_impl() but it has to be passed through much of evaluator 
 * and some native functions. General idea is that dfsch_eval_proc_impl stores
 * jmp_buf to it's start here and passes it to dfsch_eval_impl() during 
 * evaluation of last form of procedure body. Native functions have to 
 * implement same mechanism (pass dfsch_tail_escape_t argument only to 
 * functions whose return value is return value of native function itself). 
 * When dfsch_eval_proc_impl() is called with non-NULL tail_escape it simply
 * jumps back to previous activation record. This actually causes slow-down,
 * but enables us to do tail-recursion in simple and consistent way, that 
 * works even through C-code.
 */

static void async_apply_check(dfsch__thread_info_t* ti){
  if (DFSCH_UNLIKELY(ti->async_apply)){
    dfsch_object_t* proc;
    proc = ti->async_apply;
    ti->async_apply = NULL;
    dfsch_apply(proc, NULL);
  }
}

void dfsch_async_apply_check(){
  async_apply_check(dfsch__get_thread_info());
}

typedef dfsch_tail_escape_t tail_escape_t;


static dfsch_object_t* dfsch_eval_proc_impl(dfsch_object_t* code, 
                                            environment_t* env,
                                            tail_escape_t* esc,
                                            dfsch__thread_info_t* ti);
static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       environment_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti);
static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        dfsch_object_t* context,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti);


static object_t* eval_list(object_t *list, environment_t* env, 
                           dfsch__thread_info_t* ti){
  size_t l = dfsch_list_length_check(list);
  dfsch_object_t* res = dfsch_null_immutable_list(l);
  dfsch_object_t* j = res;
  dfsch_object_t* i = list;

  while (DFSCH_PAIR_P(i)){
    if (DFSCH__COMPACT_LIST_CAR_FAST(j) == DFSCH_INVALID_OBJECT){
      break; /* Can happen due to race condition in user code */
    }
    
    DFSCH__COMPACT_LIST_CAR_FAST(j) = dfsch_eval_impl(DFSCH_FAST_CAR(i), env, NULL, ti);
    j = DFSCH__COMPACT_LIST_CDR_FAST(j);
    i = DFSCH_FAST_CDR(i);
  }

  return res;
}

dfsch_object_t* dfsch_eval_list(dfsch_object_t* list, dfsch_object_t* env){
  return eval_list(list, 
                   DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE),
                   dfsch__get_thread_info());
}

/*
 * This functions evaluates arguments to funcall and saves them on
 * it's own stack. Functions that are called by apply must explicitly
 * coppy argument list if they wish to reference it from managed
 * heap/otherwise store it.
 */

static dfsch_object_t* eval_args_and_apply(dfsch_object_t* proc,
                                           dfsch_object_t* args,
                                           dfsch_object_t* context,
                                           environment_t* arg_env,
                                           tail_escape_t* esc,
                                           dfsch__thread_info_t* ti){
  size_t l = dfsch_list_length_fast_bounded(args); /* Fast and safe,
                                                      but supports at
                                                      most 64k args */
  dfsch_object_t* rsa[l+4];
  dfsch_object_t** res = &rsa;
  size_t j = 0;
  dfsch_object_t* i = args;

  if (esc && l > 12){
    res = GC_MALLOC((l+4)*sizeof(dfsch_object_t*));
  }

  if (args){
    while (DFSCH_PAIR_P(i)){
      if (j >= l){
        break; /* Can happen due to race condition in user code */
      }
      
      res[j] = dfsch_eval_impl(DFSCH_FAST_CAR(i), arg_env, NULL, ti);
      j++;
      i = DFSCH_FAST_CDR(i);
    }
    
    res[l] = DFSCH_INVALID_OBJECT;
    res[l+1] = NULL;
    res[l+2] = NULL;
    res[l+3] = NULL;

    args = DFSCH_MAKE_CLIST(res);
  }

  if (args && esc && l <= 12){
    /* We have to copy arguments from stack to scratchpad instead of
       building directly in scratchpad, because scratchpad might be
       used by recursive invocations*/
    memcpy(ti->arg_scratch_pad, res, sizeof(dfsch_object_t*)*(l+4));
    args = DFSCH_MAKE_CLIST(ti->arg_scratch_pad);
  }

  return dfsch_apply_impl(proc, args, context, esc, ti);
}

#define DFSCH__TRACEPOINT_APPLY(ti, p, al, fl)                        \
  DFSCH__TRACEPOINT_SHIFT(ti);                                        \
  DFSCH__TRACEPOINT(ti).flags |= DFSCH_TRACEPOINT_KIND_APPLY | (fl);  \
  DFSCH__TRACEPOINT(ti).data.apply.proc = (p);                        \
  DFSCH__TRACEPOINT(ti).data.apply.args = (al);                       \
  DFSCH__TRACEPOINT_NOTIFY(ti)
#define DFSCH__TRACEPOINT_EVAL(ti, ex, en)                              \
  DFSCH__TRACEPOINT_SHIFT(ti);                                          \
  DFSCH__TRACEPOINT(ti).flags |= DFSCH_TRACEPOINT_KIND_EVAL;            \
  DFSCH__TRACEPOINT(ti).flags |= (en)->flags & EFRAME_SERIAL_MASK;      \
  DFSCH__TRACEPOINT(ti).data.eval.expr = (ex);                          \
  DFSCH__TRACEPOINT(ti).data.eval.env = (en);                           \
  DFSCH__TRACEPOINT_NOTIFY(ti)


static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       environment_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti){

  if (!exp) 
    return NULL;

  if(DFSCH_SYMBOL_P(exp)){
    DFSCH__TRACEPOINT_EVAL(ti, exp, env);
    return lookup_impl(exp, env, ti);
  }

  if(DFSCH_PAIR_P(exp)){
    object_t *f = DFSCH_FAST_CAR(exp);

    DFSCH__TRACEPOINT_EVAL(ti, exp, env);
    if (DFSCH_LIKELY(DFSCH_SYMBOL_P(f))){
      f = lookup_impl(f, env, ti);
    } else {
      f = dfsch_eval_impl(f , env, NULL, ti);
    }
    
    if (DFSCH_TYPE_OF(f) == DFSCH_FORM_TYPE){
      return ((dfsch_form_t*)f)->impl(((dfsch_form_t*)f), 
                                      (dfsch_object_t*)env, 
                                      DFSCH_FAST_CDR(exp), 
                                      esc);
    }

    if (DFSCH_TYPE_OF(f) == DFSCH_MACRO_TYPE){
      return dfsch_eval_impl(macro_expand_impl(f, exp, ti),
			     env,
 			     esc,
			     ti);
    }

    return eval_args_and_apply(f, DFSCH_FAST_CDR(exp), NULL, env, esc, ti);
  }
  
  return exp;
}

dfsch_object_t* dfsch_eval_tr(dfsch_object_t* exp, 
                              dfsch_object_t* env,
                              dfsch_tail_escape_t* esc){
  return dfsch_eval_impl(exp, 
                         DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE), 
                         esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env){
  return dfsch_eval_impl(exp, 
                         DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE), 
                         NULL, dfsch__get_thread_info());
}

typedef enum cll_mode {
  CLL_POSITIONAL,
  CLL_OPTIONAL,
  CLL_KEYWORD,
  CLL_NO_ARGUMENTS, /* no outstanding arguments allowed until next LK */
} cll_mode_t;

dfsch_object_t* dfsch_compile_lambda_list(dfsch_object_t* list){
  lambda_list_t* ll;
  dfsch_object_t* i = list;
  size_t j;
  size_t positional_count = 0;
  size_t keyword_count = 0;
  size_t optional_count = 0;
  size_t arg_count;
  size_t opt_arg_count;
  dfsch_object_t* rest = NULL;
  dfsch_object_t* aux_list = NULL;
  int flags = 0;

  dfsch_object_t* arg_list = NULL; 
  dfsch_object_t* defaults_list = NULL;
  dfsch_object_t* supplied_p_list = NULL;
  dfsch_object_t* keyword_list = NULL;

  cll_mode_t mode = CLL_POSITIONAL;

  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* arg = DFSCH_FAST_CAR(i);
    
    if (arg == DFSCH_LK_OPTIONAL){
      if (mode != CLL_POSITIONAL){
        dfsch_error("Optional arguments must follow positional arguments",
                    list);
      }
      mode = CLL_OPTIONAL;
    } else if (arg == DFSCH_LK_KEY){
      if (mode == CLL_OPTIONAL){
        dfsch_signal_condition(DFSCH_STYLE_WARNING_TYPE, 
                               "Combination of optional and keyword arguments "
                               "leads to surprising behavior", NULL);
      }
      mode = CLL_KEYWORD;
    } else if (arg == DFSCH_LK_REST || arg == DFSCH_LK_BODY){
      if (arg == DFSCH_LK_BODY){
        flags |= LL_FLAG_REST_IS_BODY;
      }
      i = DFSCH_FAST_CDR(i);
      if (!DFSCH_PAIR_P(i)){
        dfsch_error("Missing argument for &rest", list);
      }
      rest = DFSCH_FAST_CAR(i);
      mode = CLL_NO_ARGUMENTS;
    } else if (arg == DFSCH_LK_AUX){
      aux_list = DFSCH_FAST_CDR(i);
      i = NULL;
      break;
    } else if (arg == DFSCH_LK_ALLOW_OTHER_KEYS){
      flags |= LL_FLAG_ALLOW_OTHER_KEYS;
    } else {
      if (mode == CLL_NO_ARGUMENTS){
        dfsch_error("Junk positional argument in keyword position", list);
      }
      if (mode == CLL_POSITIONAL) {
        arg_list = dfsch_cons(arg, arg_list);
        positional_count++;
      } else {
        dfsch_object_t* name;
        dfsch_object_t* init_form;
        dfsch_object_t* supplied_p;
        if (!DFSCH_PAIR_P(arg)){
          name = arg;
          init_form = NULL;
          supplied_p = NULL;
        } else {
          DFSCH_OBJECT_ARG(arg, name);
          DFSCH_OBJECT_ARG_OPT(arg, init_form, NULL);
          DFSCH_OBJECT_ARG_OPT(arg, supplied_p, NULL);
          DFSCH_ARG_END(arg);  
        }
        arg_list = dfsch_cons(name, arg_list);
        defaults_list = dfsch_cons(init_form, defaults_list);
        supplied_p_list = dfsch_cons(supplied_p, supplied_p_list);
        if (mode == CLL_OPTIONAL){
          optional_count++;
        } else {
          keyword_count++;
          keyword_list = dfsch_cons(dfsch_symbol_2_keyword(name), 
                                    keyword_list);
        }
      }
    }
      

    i = DFSCH_FAST_CDR(i);
  }

  if (i) {
    if (rest){
      dfsch_error("Duplicated &rest argument", list);
    }
    rest = i;
  }

  arg_count = positional_count + keyword_count + optional_count;
  ll = (lambda_list_t*)dfsch_make_object_var(DFSCH_LAMBDA_LIST_TYPE, 
                                             arg_count
                                             * sizeof(dfsch_object_t*));
  ll->rest = rest;
  ll->aux_list = aux_list;
  ll->flags = flags;

  ll->positional_count = positional_count;
  ll->optional_count = optional_count;
  ll->keyword_count = keyword_count;
  for(j = arg_count; j && DFSCH_PAIR_P(arg_list);){
    j--;
    ll->arg_list[j] = DFSCH_FAST_CAR(arg_list);
    arg_list = DFSCH_FAST_CDR(arg_list);
  }

  if (ll->keyword_count){
    ll->keywords = GC_MALLOC(sizeof(dfsch_object_t*) * ll->keyword_count);
    for(j = ll->keyword_count; j && DFSCH_PAIR_P(defaults_list);){
      j--;
      ll->keywords[j] = DFSCH_FAST_CAR(keyword_list);
      keyword_list = DFSCH_FAST_CDR(keyword_list);
    }
  }

  opt_arg_count = ll->optional_count + ll->keyword_count;
  if (opt_arg_count > 0){
    ll->defaults = GC_MALLOC(sizeof(dfsch_object_t*) * opt_arg_count);
    ll->supplied_p = GC_MALLOC(sizeof(dfsch_object_t*) * opt_arg_count);

    for(j = opt_arg_count; j && DFSCH_PAIR_P(defaults_list);){
      j--;
      ll->defaults[j] = DFSCH_FAST_CAR(defaults_list);
      defaults_list = DFSCH_FAST_CDR(defaults_list);
    }

    for(j = opt_arg_count; j && DFSCH_PAIR_P(supplied_p_list);){
      j--;
      ll->supplied_p[j] = DFSCH_FAST_CAR(supplied_p_list);
      supplied_p_list = DFSCH_FAST_CDR(supplied_p_list);
    }
  }


  return ll;
}


static void destructure_keywords(lambda_list_t* ll,
                                 dfsch_object_t* list,
                                 environment_t* env,
                                 dfsch__thread_info_t* ti){
  int i;
  size_t kw_offset = ll->positional_count + ll->optional_count;
  dfsch_object_t* j;
  char supplied[ll->keyword_count]; /* ISO 9899 6.7.5.2.5 */
  memset(supplied, 0, ll->keyword_count);
  
  j = list;

  while (DFSCH_PAIR_P(j)){
    dfsch_object_t* keyword;
    dfsch_object_t* keyword_value;
    DFSCH_OBJECT_ARG(j, keyword);
    DFSCH_OBJECT_ARG(j, keyword_value);
    
    i = 0;
    for (;;){
      if (i >= ll->keyword_count){
        if (!(ll->flags & LL_FLAG_ALLOW_OTHER_KEYS)){
          dfsch_error("Unknown keyword", keyword);
        }
        break;
      }
      if (keyword == ll->keywords[i]){
        dfsch_eqhash_put(&env->values, ll->arg_list[i + kw_offset], 
                         keyword_value);

        supplied[i] = 1;
        break;
      }
      i++;
    }
  }

  if (DFSCH_UNLIKELY(j)){
    dfsch_error("Improper list passed to function with &key arguments",
                j);
  }

  for (i = 0; i < ll->keyword_count; i++){
    if (!supplied[i]){
      dfsch_eqhash_put(&env->values, ll->arg_list[i + kw_offset], 
                       dfsch_eval_impl(ll->defaults[i + ll->optional_count], 
                                       env, NULL, ti));
    }
    
    if (DFSCH_UNLIKELY(ll->supplied_p[i + ll->optional_count])){
      dfsch_eqhash_put(&env->values, 
                       ll->supplied_p[i + ll->optional_count], 
                       dfsch_bool(supplied[i]));
    }
  }
}

static void destructure_impl(lambda_list_t* ll,
                             dfsch_object_t* list,
                             environment_t* env,
                             dfsch__thread_info_t* ti){
  int i;
  dfsch_object_t* j = list;

  for (i = 0; i < ll->positional_count; i++){
    if (DFSCH_UNLIKELY(!DFSCH_PAIR_P(j))){
      dfsch_error("Too few arguments", dfsch_list(2, ll, list));
    }
    dfsch_eqhash_put(&env->values, ll->arg_list[i], 
                     DFSCH_FAST_CAR(j));
    j = DFSCH_FAST_CDR(j);
  }

  if (DFSCH_UNLIKELY(ll->optional_count)){
    for (i = 0; i < ll->optional_count; i++){
      if (DFSCH_UNLIKELY(!DFSCH_PAIR_P(j))){
        for (; i < ll->optional_count; i++){
          dfsch_eqhash_put(&env->values, ll->arg_list[ll->positional_count + i], 
                           dfsch_eval_impl(ll->defaults[i], env, NULL, ti));
          if (DFSCH_UNLIKELY(ll->supplied_p[i])){
            dfsch_eqhash_put(&env->values, ll->supplied_p[i], NULL);
          }
        }
        break;
      }
      dfsch_eqhash_put(&env->values, ll->arg_list[ll->positional_count + i], 
                       DFSCH_FAST_CAR(j));
      if (DFSCH_UNLIKELY(ll->supplied_p[i])){
        dfsch_eqhash_put(&env->values, ll->supplied_p[i], DFSCH_SYM_TRUE);
        
      }
      j = DFSCH_FAST_CDR(j);
    }
  }
  
  
  if (DFSCH_UNLIKELY(ll->rest)) {
    dfsch_object_t* rest = dfsch_list_copy_immutable(j);
    dfsch_eqhash_put(&env->values, ll->rest, rest);
    if (DFSCH_UNLIKELY(ll->keyword_count > 0)) {
      destructure_keywords(ll, rest, env, ti);
    }
  } else if (DFSCH_UNLIKELY(ll->keyword_count > 0)) {
    destructure_keywords(ll, j, env, ti);
  } else if (DFSCH_UNLIKELY(j)) {
    dfsch_error("Too many arguments", dfsch_list(2,ll, list));
  }

  if (DFSCH_UNLIKELY(ll->aux_list)){
    dfsch_object_t* vars = ll->aux_list;
    while (DFSCH_PAIR_P(vars)){
      dfsch_object_t* clause = DFSCH_FAST_CAR(vars);
      object_t* var;
      object_t* val;
      
      DFSCH_OBJECT_ARG(clause, var);
      DFSCH_OBJECT_ARG(clause, val);
      DFSCH_ARG_END(clause);
      
      val = dfsch_eval(val, env);
      
      dfsch_define(var, val, env, 0);
      
      vars = DFSCH_FAST_CDR(vars);
    }
  }

}

dfsch_object_t* dfsch_destructuring_bind(dfsch_object_t* arglist, 
                                         dfsch_object_t* list, 
                                         dfsch_object_t* env){
  environment_t* e = (environment_t*)dfsch_new_frame(env);
  lambda_list_t* l;
  if (DFSCH_TYPE_OF(arglist) != DFSCH_LAMBDA_LIST_TYPE){
    l = (lambda_list_t*)dfsch_compile_lambda_list(arglist);
  } else {
    l = (lambda_list_t*)arglist;
  }
  destructure_impl(l, list, e, dfsch__get_thread_info());
  return (dfsch_object_t*)e;
}

static dfsch_object_t* dfsch_eval_proc_impl(dfsch_object_t* code, 
                                            environment_t* env,
                                            tail_escape_t* esc,
                                            dfsch__thread_info_t* ti){
  dfsch_object_t *i;
  dfsch_object_t *old_frame;
  dfsch_object_t *my_frame;

  if (!env)
    return NULL;
  if (!code)
    return NULL;

  async_apply_check(ti);
    
  i = code;

  while (DFSCH_PAIR_P(i)){
    object_t* exp = DFSCH_FAST_CAR(i); 
    if (DFSCH_LIKELY(DFSCH_FAST_CDR(i))) {
      dfsch_eval_impl(exp, env, NULL, ti);
    } else {
      return dfsch_eval_impl(exp, env, esc, ti);
    }
   
    i = DFSCH_FAST_CDR(i);
  }

  dfsch_error("eval-proc called on improper list", code);
}

dfsch_object_t* dfsch_eval_proc_tr(dfsch_object_t* code, 
                                   dfsch_object_t* env,
                                   tail_escape_t* esc){
  return dfsch_eval_proc_impl(code,
                              DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE),
                              esc, 
                              dfsch__get_thread_info());
}
dfsch_object_t* dfsch_eval_proc_tr_free_env(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            tail_escape_t* esc){
  environment_t* e = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch_object_t* r = dfsch_eval_proc_impl(code, e, esc, ti);
  free_environment(e, ti);
  return r;
}

dfsch_object_t* dfsch_eval_proc(dfsch_object_t* code, dfsch_object_t* env){
  return dfsch_eval_proc_impl(code, 
                              DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE),
                              NULL, 
                              dfsch__get_thread_info());
}

struct dfsch_tail_escape_t {
  jmp_buf ret;
  object_t *proc;
  object_t *args;
  object_t *context;

  environment_t* reuse_frame;
};

/* it might be interesting to optionally disable tail-calls for slight 
 * performance boost (~5%) */

static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        dfsch_object_t* context,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti){
  dfsch_object_t* r;
  tail_escape_t myesc;


#ifndef DFSCH_NO_TCO
  if (DFSCH_UNLIKELY(esc)){
    esc->proc = proc;
    esc->args = args;
    esc->context = context;
    longjmp(esc->ret,1);
  }


  myesc.reuse_frame = NULL;
  if (setjmp(myesc.ret)){  
    proc = myesc.proc;
    args = myesc.args;
    context = myesc.context;
    DFSCH__TRACEPOINT_APPLY(ti, proc, NULL, 
                           DFSCH_TRACEPOINT_FLAG_APPLY_TAIL);
  } else {
    DFSCH__TRACEPOINT_APPLY(ti, proc, NULL, 0);
  }
#endif


  /*
   * Two most common cases are written here explicitly (for historical
   * and performance reasons)
   */

  if (DFSCH_TYPE_OF(proc) == DFSCH_PRIMITIVE_TYPE){
      return ((primitive_t*)proc)->proc(((primitive_t*)proc)->baton,args,
                                        &myesc, context);
  }

  if (DFSCH_TYPE_OF(proc) == DFSCH_STANDARD_FUNCTION_TYPE){
    environment_t* env;
    dfsch_object_t* r;
    if (myesc.reuse_frame){
      env = maybe_reuse_frame(myesc.reuse_frame, ((closure_t*) proc)->env, context, ti);
    } else {
      env = new_frame_impl(((closure_t*) proc)->env, context, ti);
    }

    myesc.reuse_frame = env;
    destructure_impl(((closure_t*)proc)->args, args, env, ti);
    r = dfsch_eval_proc_impl(((closure_t*)proc)->code,
                             env,
                             &myesc,
                             ti);
    free_environment(env, ti);
    return r;
  }

  if (DFSCH_TYPE_OF(proc)->apply){
    return DFSCH_TYPE_OF(proc)->apply(proc, args, &myesc, context);
  }

  dfsch_error("Not a procedure", proc);
}

dfsch_object_t* dfsch_apply_tr(dfsch_object_t* proc, 
                               dfsch_object_t* args,
                               tail_escape_t* esc){
  return dfsch_apply_impl(proc, args, NULL, 
                          esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args){
  return dfsch_apply_impl(proc, args, NULL, 
                          NULL, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_apply_with_context(dfsch_object_t* proc, 
                                         dfsch_object_t* args,
                                         dfsch_object_t* context,
                                         tail_escape_t* esc){
  return dfsch_apply_impl(proc, args, context,
                          esc, dfsch__get_thread_info());
}

dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, dfsch_object_t* arg){
  return dfsch_eval(dfsch_backquote_expand(arg), env);
}

extern char dfsch__std_lib[];

void dfsch_core_language_register(dfsch_object_t* ctx){
  dfsch_provide(ctx, "dfsch-language");

  dfsch_defcanon_cstr(ctx, "<standard-type>", DFSCH_STANDARD_TYPE);
  dfsch_defcanon_cstr(ctx, "<abstract-type>", DFSCH_ABSTRACT_TYPE);
  dfsch_defcanon_cstr(ctx, "<meta-type>", DFSCH_META_TYPE);
  dfsch_defcanon_cstr(ctx, "<special-type>", DFSCH_SPECIAL_TYPE);
  dfsch_defcanon_cstr(ctx, "<standard-function>", DFSCH_STANDARD_FUNCTION_TYPE);

  dfsch_defcanon_cstr(ctx, "<slot-type>", DFSCH_SLOT_TYPE_TYPE);
  dfsch_defcanon_cstr(ctx, "<slot>", DFSCH_SLOT_TYPE);
  dfsch_defcanon_cstr(ctx, "<slot-accessor>", DFSCH_SLOT_ACCESSOR_TYPE);
  dfsch_defcanon_cstr(ctx, "<slot-reader>", DFSCH_SLOT_READER_TYPE);
  dfsch_defcanon_cstr(ctx, "<slot-writer>", DFSCH_SLOT_WRITER_TYPE);
  dfsch_defcanon_cstr(ctx, "<object-slot>", DFSCH_OBJECT_SLOT_TYPE);
  dfsch_defcanon_cstr(ctx, "<boolean-slot>", DFSCH_BOOLEAN_SLOT_TYPE);
  dfsch_defcanon_cstr(ctx, "<string-slot>", DFSCH_STRING_SLOT_TYPE);
  dfsch_defcanon_cstr(ctx, "<size_t-slot>", DFSCH_SIZE_T_SLOT_TYPE);
  dfsch_defcanon_cstr(ctx, "<int-slot>", DFSCH_INT_SLOT_TYPE);
  dfsch_defcanon_cstr(ctx, "<long-slot>", DFSCH_LONG_SLOT_TYPE);

  dfsch_defcanon_cstr(ctx, "<list>", DFSCH_LIST_TYPE);
  dfsch_defcanon_cstr(ctx, "<pair>", DFSCH_PAIR_TYPE);
  dfsch_defcanon_cstr(ctx, "<mutable-pair>", DFSCH_MUTABLE_PAIR_TYPE);
  dfsch_defcanon_cstr(ctx, "<immutable-pair>", DFSCH_IMMUTABLE_PAIR_TYPE);
  dfsch_defcanon_cstr(ctx, "<empty-list>", DFSCH_EMPTY_LIST_TYPE);
  dfsch_defcanon_cstr(ctx, "<symbol>", DFSCH_SYMBOL_TYPE);
  dfsch_defcanon_cstr(ctx, "<primitive>", DFSCH_PRIMITIVE_TYPE);
  dfsch_defcanon_cstr(ctx, "<function>", DFSCH_FUNCTION_TYPE);
  dfsch_defcanon_cstr(ctx, "<macro>", DFSCH_MACRO_TYPE);
  dfsch_defcanon_cstr(ctx, "<form>", DFSCH_FORM_TYPE);
  dfsch_defcanon_cstr(ctx, "<vector>", DFSCH_VECTOR_TYPE);

  dfsch_defcanon_cstr(ctx, "<environment>", DFSCH_ENVIRONMENT_TYPE);

  dfsch_defcanon_cstr(ctx, "<iterator>", DFSCH_ITERATOR_TYPE);
  dfsch_defcanon_cstr(ctx, "<iterator-type>", DFSCH_ITERATOR_TYPE_TYPE);

  dfsch_defconst_cstr(ctx, "true", DFSCH_SYM_TRUE);
  dfsch_defconst_cstr(ctx, "nil", NULL);
  dfsch_defconst_cstr(ctx, "else", DFSCH_SYM_TRUE);
  dfsch_defconst_cstr(ctx, "t", DFSCH_SYM_TRUE);  
  dfsch_defconst_cstr(ctx, "T", DFSCH_SYM_TRUE);

  dfsch_defcanon_cstr(ctx, "top-level-environment", ctx);
  dfsch_defconst_cstr(ctx,"*dfsch-version*",
                      dfsch_make_string_cstr(PACKAGE_VERSION));
  dfsch_defconst_cstr(ctx,"*dfsch-build-id*",
                      dfsch_make_string_cstr(BUILD_ID));
  dfsch_defconst_cstr(ctx,"*dfsch-platform*",
                      dfsch_make_string_cstr(HOST_TRIPLET));

  dfsch__primitives_register(ctx);
  dfsch__native_cxr_register(ctx);
  dfsch__forms_register(ctx);
  dfsch__hash_native_register(ctx);
  dfsch__number_native_register(ctx);
  dfsch__string_native_register(ctx);
  dfsch__object_native_register(ctx);
  dfsch__format_native_register(ctx);
  dfsch__bignum_register(ctx);
  dfsch__conditions_register(ctx);
  dfsch__generic_register(ctx);
  dfsch__mkhash_register(ctx);
  dfsch__package_register(ctx);
  dfsch__macros_register(ctx);
  dfsch__port_native_register(ctx);

  dfsch_load_source(ctx, "*linked-standard-library*", 0, dfsch__std_lib);
}

void dfsch_core_system_register(dfsch_object_t* ctx){
  dfsch_provide(ctx, "dfsch-system");

  dfsch__system_register(ctx);
  dfsch__port_files_register(ctx);
  dfsch__random_register(ctx);
  dfsch__serdes_register(ctx);
  dfsch__load_register(ctx);
  dfsch__compile_register(ctx);
}


void dfsch_core_register(dfsch_object_t* ctx){
  dfsch_provide(ctx, "dfsch");

  dfsch_core_language_register(ctx);
  dfsch_core_system_register(ctx);
}


dfsch_object_t* dfsch_make_top_level_environment(){
  dfsch_object_t* ctx;

  ctx = dfsch_new_frame(NULL);

  dfsch_core_register(ctx);

  return ctx;
}

void dfsch_define_cstr(dfsch_object_t *ctx, 
                       char *name, 
                       void *obj){
  dfsch_define_pkgcstr(ctx, DFSCH_DFSCH_PACKAGE, name, obj);  
}
void dfsch_defconst_cstr(dfsch_object_t *ctx, 
                         char *name, 
                         void *obj){
  dfsch_defconst_pkgcstr(ctx, DFSCH_DFSCH_PACKAGE, name, obj);
}
void dfsch_defcanon_cstr(dfsch_object_t *ctx, 
                         char *name, 
                         void *obj){
  dfsch_defcanon_pkgcstr(ctx, DFSCH_DFSCH_PACKAGE, name, obj);
}
void dfsch_define_pkgcstr(dfsch_object_t *ctx,
                        dfsch_package_t* pkg,
                        char *name, 
                        void *obj){
  
  dfsch_define(dfsch_intern_symbol(pkg, name), 
               (dfsch_object_t*)obj, ctx, 0);
}
void dfsch_defcanon_pkgcstr(dfsch_object_t *ctx, 
                          dfsch_package_t* pkg,
                          char *name, 
                          void *obj){
  
  dfsch_define(dfsch_intern_symbol(pkg, name), 
               (dfsch_object_t*)obj, ctx, 
               DFSCH_VAR_CONSTANT | DFSCH_VAR_CANONICAL);
}
void dfsch_defconst_pkgcstr(dfsch_object_t *ctx, 
                          dfsch_package_t* pkg,
                          char *name, 
                          void *obj){
  
  dfsch_define(dfsch_intern_symbol(pkg, name), 
               (dfsch_object_t*)obj, ctx, 
               DFSCH_VAR_CONSTANT);
}
void dfsch_set_cstr(dfsch_object_t *ctx, 
                    char *name, 
                    dfsch_object_t *obj){
  
  dfsch_set(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE, name), obj, ctx);
}
dfsch_object_t* dfsch_lookup_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_lookup(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                          name), ctx);
}
dfsch_object_t* dfsch_env_get_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_env_get(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                           name), ctx);
}


char* dfsch_get_version(){
  return PACKAGE_VERSION;
}
char* dfsch_get_build_id(){
  return BUILD_ID;
}

pthread_mutex_t libc_mutex = PTHREAD_MUTEX_INITIALIZER;

void dfsch_lock_libc(){
  pthread_mutex_lock(&libc_mutex);
}
void dfsch_unlock_libc(){
  pthread_mutex_unlock(&libc_mutex);
}
