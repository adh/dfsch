/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include "types.h"

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
  obj_size += type->size;
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
    DFSCH_WITH_RETRY_WITH_RESTART(dfsch_make_symbol("use-value"), 
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
    DFSCH_WITH_RETRY_WITH_RESTART(dfsch_make_symbol("use-value"), 
                                  "Retry with alternate value") {
      dfsch_type_error(o, type, 1);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return o;
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

int dfsch_default_trace_depth = 3;

static dfsch__tracepoint_t* alloc_trace_buffer(dfsch__thread_info_t* ti,
                                               int depth){
  size_t size;
  depth = depth % (8 * sizeof(short));
  size = 1 << depth;
  ti->trace_depth = size - 1;
  ti->trace_ptr = 0;
  ti->trace_buffer = GC_MALLOC(sizeof(dfsch__tracepoint_t) * size);
}

dfsch__thread_info_t* dfsch__get_thread_info(){
  dfsch__thread_info_t *ei;
  pthread_once(&thread_once, thread_key_alloc);
  ei = pthread_getspecific(thread_key);
  if (DFSCH_UNLIKELY(!ei)){
    ei = GC_MALLOC_UNCOLLECTABLE(sizeof(dfsch__thread_info_t)); 
    ei->throw_ret = NULL;
    ei->async_apply = NULL;
    alloc_trace_buffer(ei, dfsch_default_trace_depth);
    pthread_setspecific(thread_key, ei);
  }
  return ei;
}

static dfsch_object_t* tl_cons(dfsch__thread_info_t* ti,
                               dfsch_object_t* car, dfsch_object_t* cdr){
  dfsch_pair_t* p;
  
#ifdef GC_NEXT
  if (!ti->pair_freelist){
    ti->pair_freelist = GC_malloc_many(sizeof(dfsch_pair_t));
  }
  
  p = ti->pair_freelist;
  ti->pair_freelist = GC_NEXT(ti->pair_freelist);
#else
  p = GC_NEW(dfsch_pair_t);
#endif

  p->car = car;
  p->cdr = cdr;

  return DFSCH_TAG_ENCODE(p, 1);
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
    fputs("No unwind target!!!\n", stderr);
    abort();
  }
  longjmp(*ti->throw_ret, 1);
}
void dfsch__finalize_unwind(dfsch__thread_info_t* ti){
  ti->throw_tag = NULL;
  ti->throw_value = NULL;
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
  DFSCH_WITH_SIMPLE_RESTART(dfsch_make_symbol("continue"),
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
  dfsch_writer_state_t* state = 
    dfsch_make_writer_state(max_depth,
                            readable?
                            DFSCH_WRITE:
                            DFSCH_PRINT,
                            (dfsch_output_proc_t)sl_append,
                            sl);
  dfsch_write_object(state, obj);
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

#ifdef GC_NEXT
  if (!ti->env_freelist){
    ti->env_freelist = GC_malloc_many(sizeof(environment_t));
  }
  e = ti->env_freelist;
  ti->env_freelist = GC_NEXT(ti->env_freelist);
#else
  e = GC_NEW(environment_t);
#endif

  ((dfsch_object_t*)e)->type = DFSCH_ENVIRONMENT_TYPE;
  return e;
}

static environment_t* new_frame_impl(environment_t* parent,
                                     dfsch_object_t* context,
                                     dfsch__thread_info_t* ti){
  environment_t* e = alloc_environment(ti);

  dfsch_eqhash_init(&e->values, 0);
  e->decls = NULL;
  e->context = context;
  e->owner = ti;
  e->parent = (environment_t*)parent;

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
  return DFSCH_INVALID_OBJECT;
}


int dfsch_variable_constant_p(object_t* name, object_t* env){
  environment_t *i;
  short flags;

  i = DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE);
  DFSCH_RWLOCK_RDLOCK(&environment_rwlock);
  while (i){
    if (dfsch_eqhash_ref_ex(&i->values, name, NULL, &flags, NULL)){
      DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
      return flags & DFSCH_VAR_CONSTANT;
    }

    i = i->parent;
  }
  DFSCH_RWLOCK_UNLOCK(&environment_rwlock);
  return 0;
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
                  short flags){
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
  return dfsch_eqhash_2_alist(&e->values);
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

dfsch_object_t* dfsch_macro_expand(dfsch_object_t* macro,
                                   dfsch_object_t* args){

  return dfsch_apply(((macro_t*)DFSCH_ASSERT_TYPE(macro, 
                                                  DFSCH_MACRO_TYPE))->proc, 
                     args);
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

/* TODO: finish new stack traces
 * general idea is that frames are construed by apply and then filed in 
 * relevant functions. Tail recursion handling should be part of apply. */

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
                                        environment_t* arg_env,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti);


static object_t* eval_list(object_t *list, environment_t* env, 
                           dfsch__thread_info_t* ti){
  dfsch_object_t *i;
  object_t *f=NULL;
  dfsch_object_t *t, *p;
  object_t *r; 

  if (!list)
    return NULL;
  
  if (!DFSCH_PAIR_P(list)){
    dfsch_error("Not a proper list", list);    
  }

  r = dfsch_eval_impl(DFSCH_FAST_CAR(list), env, NULL, ti);
  t = tl_cons(ti, r, NULL);
  f = p = t;

  i = DFSCH_FAST_CDR(list);
  while (DFSCH_PAIR_P(i)){
    r = DFSCH_FAST_CAR(i);
    if (DFSCH_SYMBOL_P(r)){
      r = lookup_impl(r, env, ti);
    } else {
      r = dfsch_eval_impl(r, env, NULL, ti);
    }

    t = tl_cons(ti, r, NULL);
    DFSCH_FAST_CDR_MUT(p) = (object_t*)t;
    p = t;

    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Not a proper list", list);    
  }

  return f;
}

dfsch_object_t* dfsch_eval_list(dfsch_object_t* list, dfsch_object_t* env){
  return eval_list(list, 
                   DFSCH_ASSERT_TYPE(env, DFSCH_ENVIRONMENT_TYPE),
                   dfsch__get_thread_info());
}

static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       environment_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti){
  DFSCH__TRACEPOINT_EVAL(ti, exp, (dfsch_object_t*)env);

  if (!exp) 
    return NULL;

  if(DFSCH_SYMBOL_P(exp)){
    return lookup_impl(exp, env, ti);
  }

  if(DFSCH_PAIR_P(exp)){
    object_t *f = DFSCH_FAST_CAR(exp);

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
      return dfsch_eval_impl(dfsch_macro_expand(f, DFSCH_FAST_CDR(exp)),
			     env,
 			     esc,
			     ti);
    }

    return dfsch_apply_impl(f, DFSCH_FAST_CDR(exp), NULL, env, esc, ti);
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

int dfsch_constant_p(dfsch_object_t* exp, dfsch_object_t* env){
  if (DFSCH_SYMBOL_P(exp)){
    return dfsch_variable_constant_p(exp, env);
  }

  if (DFSCH_PAIR_P(exp)){
    return 0;
  }  

  return 1;
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
    } else if (arg == DFSCH_LK_REST){
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
                                 environment_t* outer,
                                 dfsch__thread_info_t* ti){
  int i;
  size_t kw_offset = ll->positional_count + ll->optional_count;
  dfsch_object_t* j;
  char supplied[ll->keyword_count]; /* ISO 9899 6.7.5.2.5 */
  memset(supplied, 0, ll->keyword_count);
  
  j = list;

  while (DFSCH_PAIR_P(j)){
    dfsch_object_t* keyword;
    dfsch_object_t* value;
    DFSCH_OBJECT_ARG(j, keyword);
    DFSCH_OBJECT_ARG(j, value);
    
    if (DFSCH_LIKELY(outer)){
      keyword = dfsch_eval_impl(keyword, outer, NULL, ti);
    }

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
                         DFSCH_LIKELY(outer) ? 
                         dfsch_eval_impl(value, outer, NULL, ti):
                         value);

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
                             environment_t* outer,
                             dfsch__thread_info_t* ti){
  int i;
  dfsch_object_t* j = list;

  for (i = 0; i < ll->positional_count; i++){
    if (DFSCH_UNLIKELY(!DFSCH_PAIR_P(j))){
      dfsch_error("Too few arguments", dfsch_list(2, ll, list));
    }
    dfsch_eqhash_put(&env->values, ll->arg_list[i], 
                     DFSCH_LIKELY(outer) ? 
                     dfsch_eval_impl(DFSCH_FAST_CAR(j), outer, NULL, ti):
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
                       DFSCH_LIKELY(outer) ? 
                       dfsch_eval_impl(DFSCH_FAST_CAR(j), outer, NULL, ti):
                       DFSCH_FAST_CAR(j));
      if (DFSCH_UNLIKELY(ll->supplied_p[i])){
        dfsch_eqhash_put(&env->values, ll->supplied_p[i], DFSCH_SYM_TRUE);
        
      }
      j = DFSCH_FAST_CDR(j);
    }
  }
  
  
  if (DFSCH_UNLIKELY(ll->rest)) {
    dfsch_object_t* rest = DFSCH_LIKELY(outer) ? eval_list(j, outer, ti): j;
    dfsch_eqhash_put(&env->values, ll->rest, rest);
    if (DFSCH_UNLIKELY(ll->keyword_count > 0)) {
      destructure_keywords(ll, rest, env, NULL, ti);
    }
  } else if (DFSCH_UNLIKELY(ll->keyword_count > 0)) {
    destructure_keywords(ll, j, env, outer, ti);
  } else if (DFSCH_UNLIKELY(j)) {
      dfsch_error("Too many arguments", dfsch_list(2,ll, list));
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
  destructure_impl(l, list, e, NULL, NULL);
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
  object_t* context;
  environment_t *arg_env;
};

/* it might be interesting to optionally disable tail-calls for slight 
 * performance boost (~5%) */

static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        dfsch_object_t* context,
                                        environment_t* arg_env,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti){
  dfsch_object_t* r;
  tail_escape_t myesc;


  if (DFSCH_UNLIKELY(esc)){
    esc->proc = proc;
    esc->args = args;
    esc->arg_env = arg_env;
    esc->context = context;
    longjmp(esc->ret,1);
  }


  if (setjmp(myesc.ret)){  
    proc = myesc.proc;
    args = myesc.args;
    arg_env = myesc.arg_env;
    context = myesc.context;
    DFSCH__TRACEPOINT_APPLY(ti, proc, args, 
                           DFSCH_TRACEPOINT_FLAG_APPLY_TAIL | 
                           (arg_env ? DFSCH_TRACEPOINT_FLAG_APPLY_LAZY : 0));
  } else {
    DFSCH__TRACEPOINT_APPLY(ti, proc, args, 
                           (arg_env ? DFSCH_TRACEPOINT_FLAG_APPLY_LAZY : 0));
  }



  /*
   * Two most common cases are written here explicitly (for historical
   * and performance reasons)
   */

  if (DFSCH_TYPE_OF(proc) == DFSCH_PRIMITIVE_TYPE){
    if (DFSCH_LIKELY(arg_env)){
      args = eval_list(args, arg_env, ti);
    }
    return ((primitive_t*)proc)->proc(((primitive_t*)proc)->baton,args,
                                      &myesc, context);
  }

  if (DFSCH_TYPE_OF(proc) == DFSCH_STANDARD_FUNCTION_TYPE){
    environment_t* env = new_frame_impl(((closure_t*) proc)->env,
                                        context,
                                        ti);
    destructure_impl(((closure_t*)proc)->args, args, env, arg_env, ti);
    return dfsch_eval_proc_impl(((closure_t*)proc)->code,
                                env,
                                &myesc,
                                ti);
  }

  if (DFSCH_TYPE_OF(proc)->apply){
    if (DFSCH_LIKELY(arg_env)){
      args = eval_list(args, arg_env, ti);
    }
    return DFSCH_TYPE_OF(proc)->apply(proc, args, &myesc, context);
  }

  dfsch_error("Not a procedure", proc);

}

dfsch_object_t* dfsch_apply_tr(dfsch_object_t* proc, 
                               dfsch_object_t* args,
                               tail_escape_t* esc){
  return dfsch_apply_impl(proc, args, NULL, NULL, 
                          esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args){
  return dfsch_apply_impl(proc, args, NULL, NULL, 
                          NULL, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_apply_with_context(dfsch_object_t* proc, 
                                         dfsch_object_t* args,
                                         dfsch_object_t* context,
                                         tail_escape_t* esc){
  return dfsch_apply_impl(proc, args, context, NULL, 
                          esc, dfsch__get_thread_info());
}

dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, dfsch_object_t* arg){
  if (dfsch_pair_p(arg)){
    object_t* car = dfsch_car(arg);
    object_t* cdr = dfsch_cdr(arg);

    if (car == DFSCH_SYM_UNQUOTE && dfsch_pair_p(cdr)){
      return dfsch_eval(dfsch_car(cdr), env);
    }else if (dfsch_pair_p(car)){
      if (dfsch_car(car) == DFSCH_SYM_UNQUOTE_SPLICING){
        return dfsch_append(dfsch_list(2,
                                       dfsch_eval(dfsch_car(dfsch_cdr(car)), 
                                                  env),
                                       dfsch_quasiquote(env, cdr)));
      }
    }

    return dfsch_cons(dfsch_quasiquote(env,car), dfsch_quasiquote(env,cdr));
  }else{
    return arg;
  }
}

DFSCH_PRIMITIVE_HEAD(top_level_environment){
  return baton;
}

DFSCH_DEFINE_FORM_IMPL(current_environment, 
                       "Return lexically-enclosing environment"){
  return env;
}

dfsch_object_t* dfsch_make_top_level_environment(){
  dfsch_object_t* ctx;

  ctx = dfsch_new_frame(NULL);

  dfsch_define_cstr(ctx, "<standard-type>", DFSCH_STANDARD_TYPE);
  dfsch_define_cstr(ctx, "<abstract-type>", DFSCH_ABSTRACT_TYPE);
  dfsch_define_cstr(ctx, "<meta-type>", DFSCH_META_TYPE);
  dfsch_define_cstr(ctx, "<special-type>", DFSCH_SPECIAL_TYPE);
  dfsch_define_cstr(ctx, "<standard-function>", DFSCH_STANDARD_FUNCTION_TYPE);

  dfsch_define_cstr(ctx, "<slot-type>", DFSCH_SLOT_TYPE_TYPE);
  dfsch_define_cstr(ctx, "<slot>", DFSCH_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<slot-accessor>", DFSCH_SLOT_ACCESSOR_TYPE);
  dfsch_define_cstr(ctx, "<slot-reader>", DFSCH_SLOT_READER_TYPE);
  dfsch_define_cstr(ctx, "<slot-writer>", DFSCH_SLOT_WRITER_TYPE);
  dfsch_define_cstr(ctx, "<object-slot>", DFSCH_OBJECT_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<boolean-slot>", DFSCH_BOOLEAN_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<string-slot>", DFSCH_STRING_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<size_t-slot>", DFSCH_SIZE_T_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<int-slot>", DFSCH_INT_SLOT_TYPE);
  dfsch_define_cstr(ctx, "<long-slot>", DFSCH_LONG_SLOT_TYPE);

  dfsch_define_cstr(ctx, "<list>", DFSCH_LIST_TYPE);
  dfsch_define_cstr(ctx, "<pair>", DFSCH_PAIR_TYPE);
  dfsch_define_cstr(ctx, "<mutable-pair>", DFSCH_MUTABLE_PAIR_TYPE);
  dfsch_define_cstr(ctx, "<immutable-pair>", DFSCH_IMMUTABLE_PAIR_TYPE);
  dfsch_define_cstr(ctx, "<empty-list>", DFSCH_EMPTY_LIST_TYPE);
  dfsch_define_cstr(ctx, "<symbol>", DFSCH_SYMBOL_TYPE);
  dfsch_define_cstr(ctx, "<primitive>", DFSCH_PRIMITIVE_TYPE);
  dfsch_define_cstr(ctx, "<function>", DFSCH_FUNCTION_TYPE);
  dfsch_define_cstr(ctx, "<macro>", DFSCH_MACRO_TYPE);
  dfsch_define_cstr(ctx, "<form>", DFSCH_FORM_TYPE);
  dfsch_define_cstr(ctx, "<vector>", DFSCH_VECTOR_TYPE);

  dfsch_define_cstr(ctx, "<environment>", DFSCH_ENVIRONMENT_TYPE);

  dfsch_define_cstr(ctx, "top-level-environment", 
                    DFSCH_PRIMITIVE_REF_MAKE(top_level_environment, ctx));
  dfsch_define_cstr(ctx, "current-environment", 
                    DFSCH_FORM_REF(current_environment));
  dfsch_define_cstr(ctx,"*dfsch-version*",
                    dfsch_make_string_cstr(PACKAGE_VERSION));
  dfsch_define_cstr(ctx,"*dfsch-platform*",
                    dfsch_make_string_cstr(HOST_TRIPLET));

  dfsch__native_register(ctx);

  return ctx;
}


void dfsch_define_cstr(dfsch_object_t *ctx, 
                       char *name, 
                       void *obj){
  
  dfsch_define(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE, name), 
               (dfsch_object_t*)obj, ctx, 0);
}
void dfsch_defconst_cstr(dfsch_object_t *ctx, 
                         char *name, 
                         void *obj){
  
  dfsch_define(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE, name), 
               (dfsch_object_t*)obj, ctx, 
               DFSCH_VAR_CONSTANT);
}
void dfsch_set_cstr(dfsch_object_t *ctx, 
                    char *name, 
                    dfsch_object_t *obj){
  
  dfsch_set(dfsch_make_symbol(name), obj, ctx);
}
dfsch_object_t* dfsch_lookup_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_lookup(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                          name), ctx);
}
dfsch_object_t* dfsch_env_get_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_env_get(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                           name), ctx);
}
