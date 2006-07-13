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
  if (!thread || thread->type != &thread_type)
    dfsch_throw("thread:not-a-thread", thread);

  t = (thread_t*)thread;

  if (pthread_join(t->thread, (void*)&ret) != 0){
    int err = errno;
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }

  return ret;
}
dfsch_object_t* dfsch_thread_detach(dfsch_object_t* thread){
  thread_t* t;
  if (!thread || thread->type != &thread_type)
    dfsch_throw("thread:not-a-thread", thread);

  if (pthread_detach(t->thread) != 0){
    int err = errno;
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }

  return thread;
}

dfsch_object_t* dfsch_thread_self(){
  thread_t* thread = (thread_t*) dfsch_make_object(&thread_type);
  
  thread->thread = pthread_self();

  return (dfsch_object_t*)thread;
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

  return dfsch_thread_join(thread);
}
static dfsch_object_t* native_thread_self(void*baton, dfsch_object_t* args, 
                                          dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_thread_self();
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
  return NULL;
}
