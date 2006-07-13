#include "dfsch/threads.h"

#include <errno.h>
#include <string.h>

typedef struct thread_t {
  dfsch_type_t* type;
  pthread_t thread;
} thread_t;

static int thread_equal_p(thread_t* a, thread_t* b){
  return pthread_equal(a->thread, b->thread);
}

static const dfsch_type_t thread_type = {
  sizeof(thread_t), 
  "thread",
  (dfsch_type_equal_p_t)thread_equal_p,
  NULL
};

typedef struct thread_args_t {
  dfsch_object_t* function;
  dfsch_object_t* args;
} thread_args_t;

dfsch_object_t* thread_function(thread_args_t* args){
  return dfsch_apply(args->function, args->args);
}

dfsch_object_t* dfsch_thread_create(dfsch_object_t* function,
                                    dfsch_object_t* arguments){
  
  thread_t* thread = (thread_t*) dfsch_make_object(&thread_type);
  thread_args_t* args = GC_NEW(thread_args_t);
  
  args->function = function;
  args->args = arguments;
  
  pthread_create(&(thread->thread), 
                 NULL, 
                 (void*(*)(void*))thread_function, 
                 args);

  return (dfsch_object_t*)thread;  
}

dfsch_object_t* dfsch_thread_join(dfsch_object_t* thread){
  thread_t* t;
  dfsch_object_t* ret;
  int err;
  if (!thread || thread->type != &thread_type)
    dfsch_throw("thread:not-a-thread", thread);

  t = (thread_t*)thread;

  err = pthread_join(t->thread, (void*)&ret);

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }

  return ret;
}
void dfsch_thread_detach(dfsch_object_t* thread){
  thread_t* t;
  int err;
  if (!thread || thread->type != &thread_type)
    dfsch_throw("thread:not-a-thread", thread);

  err = pthread_detach(t->thread);

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

dfsch_object_t* dfsch_thread_self(){
  thread_t* thread = (thread_t*) dfsch_make_object(&thread_type);
  
  thread->thread = pthread_self();

  return (dfsch_object_t*)thread;
}

// Mutexes

typedef struct mutex_t {
  dfsch_type_t* type;
  pthread_mutex_t mutex;
} mutex_t;

static const dfsch_type_t mutex_type = {
  sizeof(mutex_t), 
  "mutex",
  NULL,
  NULL
};

static mutex_finalizer(mutex_t* mutex, void* cd){
  /*
   * When given mutex is locked we will do nothing. If user loses reference
   * to locked mutex, something is probably wrong.
   *
   * By the way, on LinuxThreads this is nevertheless a little more than 
   * no-op, so it probably doesn't matter. (althought some other 
   * impementations may leak some resources here)
   */

  pthread_mutex_destroy(&(mutex->mutex));
}

dfsch_object_t* dfsch_mutex_create(){
  mutex_t* mutex = (mutex_t*)dfsch_make_object(&mutex_type);

  GC_REGISTER_FINALIZER(mutex, 
                        (GC_finalization_proc)mutex_finalizer,
                        NULL, NULL, NULL);

  pthread_mutex_init(&(mutex->mutex), NULL);

  return (dfsch_object_t*)mutex;
}
void dfsch_mutex_lock(dfsch_object_t* mutex){
  mutex_t* m;
  int err;
  if (!mutex || mutex->type != &mutex_type)
    dfsch_throw("thread:not-a-mutex", mutex);

  m = (mutex_t*)mutex;

  err = pthread_mutex_lock(&(m->mutex));

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}
int dfsch_mutex_trylock(dfsch_object_t* mutex){
  mutex_t* m;
  if (!mutex || mutex->type != &mutex_type)
    dfsch_throw("thread:not-a-mutex", mutex);

  m = (mutex_t*)mutex;

  return (pthread_mutex_trylock(&(m->mutex)) == 0);
}
void dfsch_mutex_unlock(dfsch_object_t* mutex){
  mutex_t* m;
  int err;
  if (!mutex || mutex->type != &mutex_type)
    dfsch_throw("thread:not-a-mutex", mutex);

  m = (mutex_t*)mutex;

  err = pthread_mutex_unlock(&(m->mutex));

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}



static dfsch_object_t* native_thread_create(void*baton, dfsch_object_t* args, 
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* function;
  dfsch_object_t* arguments;
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_OBJECT_ARG(args, arguments);
  DFSCH_ARG_END(args);

  return dfsch_thread_create(function, arguments);
}

static dfsch_object_t* native_thread_join(void*baton, dfsch_object_t* args, 
                                          dfsch_tail_escape_t* esc){
  dfsch_object_t* thread;
  DFSCH_OBJECT_ARG(args, thread);
  DFSCH_ARG_END(args);

  return dfsch_thread_join(thread);
}
static dfsch_object_t* native_thread_detach(void*baton, dfsch_object_t* args, 
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* thread;
  DFSCH_OBJECT_ARG(args, thread);
  DFSCH_ARG_END(args);

  dfsch_thread_detach(thread);
  return thread;
}
static dfsch_object_t* native_thread_self(void*baton, dfsch_object_t* args, 
                                          dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_thread_self();
}

static dfsch_object_t* native_mutex_create(void*baton, dfsch_object_t* args, 
                                           dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_mutex_create();
}
static dfsch_object_t* native_mutex_lock(void*baton, dfsch_object_t* args, 
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  dfsch_mutex_lock(mutex);
  return mutex;
}
static dfsch_object_t* native_mutex_trylock(void*baton, dfsch_object_t* args, 
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_mutex_trylock(mutex));
}
static dfsch_object_t* native_mutex_unlock(void*baton, dfsch_object_t* args, 
                                           dfsch_tail_escape_t* esc){
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  dfsch_mutex_unlock(mutex);
  return mutex;
}


dfsch_object_t* dfsch_threads_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx, "thread:create", 
                   dfsch_make_primitive(&native_thread_create,NULL));
  dfsch_ctx_define(ctx, "thread:join", 
                   dfsch_make_primitive(&native_thread_join,NULL));
  dfsch_ctx_define(ctx, "thread:detach", 
                   dfsch_make_primitive(&native_thread_detach,NULL));
  dfsch_ctx_define(ctx, "thread:self", 
                   dfsch_make_primitive(&native_thread_self,NULL));

  dfsch_ctx_define(ctx, "mutex:create", 
                   dfsch_make_primitive(&native_mutex_create,NULL));
  dfsch_ctx_define(ctx, "mutex:lock", 
                   dfsch_make_primitive(&native_mutex_lock,NULL));
  dfsch_ctx_define(ctx, "mutex:trylock", 
                   dfsch_make_primitive(&native_mutex_trylock,NULL));
  dfsch_ctx_define(ctx, "mutex:unlock", 
                   dfsch_make_primitive(&native_mutex_unlock,NULL));

  return NULL;
}
