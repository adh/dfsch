#include "dfsch/lib/threads.h"

#include <dfsch/number.h>

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
  DFSCH_STANDARD_TYPE,
  sizeof(thread_t), 
  "thread",
  (dfsch_type_equal_p_t)thread_equal_p,
  NULL,
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

  t = (thread_t*)thread;

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
  DFSCH_STANDARD_TYPE,
  sizeof(mutex_t), 
  "mutex",
  NULL,
  NULL,
  NULL
};

static void mutex_finalizer(mutex_t* mutex, void* cd){
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

// Condition variables

typedef struct condition_t {
  dfsch_type_t* type;
  pthread_cond_t cond;
} condition_t;

static const dfsch_type_t condition_type = {
  DFSCH_STANDARD_TYPE,
  sizeof(condition_t), 
  "condition",
  NULL,
  NULL,
  NULL
};

static void condition_finalizer(condition_t* cond, void* cd){
  /* 
   * No-op when there are threads waiting (and associated resource leak 
   * somewhere).
   */

  pthread_cond_destroy(&(cond->cond));
}

dfsch_object_t* dfsch_condition_create(){
  condition_t* cond = (condition_t*)dfsch_make_object(&condition_type);

  GC_REGISTER_FINALIZER(cond, 
                        (GC_finalization_proc)condition_finalizer,
                        NULL, NULL, NULL);

  pthread_cond_init(&(cond->cond), NULL);

  return (dfsch_object_t*)cond;
}

void dfsch_condition_wait(dfsch_object_t* cond, dfsch_object_t* mutex){
  condition_t* c;
  int err;
  mutex_t* m;

  if (!mutex || mutex->type != &mutex_type)
    dfsch_throw("thread:not-a-mutex", mutex);
  if (!cond || cond->type != &condition_type)
    dfsch_throw("thread:not-a-condition", cond);

  c = (condition_t*)cond;
  m = (mutex_t*)mutex;

  err = pthread_cond_wait(&(c->cond), &(m->mutex));

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

void dfsch_condition_signal(dfsch_object_t* cond){
  condition_t* c;
  int err;
  if (!cond || cond->type != &condition_type)
    dfsch_throw("thread:not-a-condition", cond);

  c = (condition_t*)cond;

  err = pthread_cond_signal(&(c->cond));

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

void dfsch_condition_broadcast(dfsch_object_t* cond){
  condition_t* c;
  int err;
  if (!cond || cond->type != &condition_type)
    dfsch_throw("thread:not-a-condition", cond);

  c = (condition_t*)cond;

  err = pthread_cond_broadcast(&(c->cond));

  if (err != 0){
    dfsch_throw("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

// Channels

/*
 * Channel is simply inter-thread pipe for transferring objects. In theory,
 * it could be possible to use channel as a queue inside one thread - but you
 * are risking a deadlock here when channels' buffer becomes full or empty.
 *
 * It might be useful to use dfsch's wrapper objects here and leave resource
 * reclaimation and error handling to them, but that would have slight 
 * unnecessary overhead due to redundant type checking and would use more 
 * memory.
 */

typedef struct channel_t {
  dfsch_type_t* type;

  pthread_mutex_t mutex;
  pthread_cond_t read;
  pthread_cond_t write;
  
  dfsch_object_t** buf;
  
  size_t buf_len;
  size_t readptr;
  size_t writeptr;
} channel_t;

static const dfsch_type_t channel_type = {
  DFSCH_STANDARD_TYPE,
  sizeof(channel_t), 
  "channel",
  NULL,
  NULL,
  NULL
};

static void channel_finalizer(channel_t* ch, void* cd){
  /* 
   * When this is called, there are no outstanding references to this
   * so we can expect that no operations are in progress on this channel
   * which in turn should mean that there are no locks held and no threads 
   * waiting. This does not necessarily mean that buffer is empty.
   */

  pthread_cond_destroy(&(ch->read));
  pthread_cond_destroy(&(ch->write));
  pthread_mutex_destroy(&(ch->mutex));
  GC_FREE(ch->buf);
}

dfsch_object_t* dfsch_channel_create(size_t buffer){
  channel_t* ch = (channel_t*)dfsch_make_object(&channel_type);

  GC_REGISTER_FINALIZER(ch, 
                        (GC_finalization_proc)channel_finalizer,
                        NULL, NULL, NULL);

  pthread_cond_init(&(ch->read), NULL);
  pthread_cond_init(&(ch->write), NULL);
  pthread_mutex_init(&(ch->mutex), NULL);

  ch->buf = GC_MALLOC_UNCOLLECTABLE(sizeof(dfsch_object_t*)*buffer);
  ch->buf_len = buffer;
  ch->readptr = 0;
  ch->writeptr = 0;

  return (dfsch_object_t*)ch;
}

dfsch_object_t* dfsch_channel_read(dfsch_object_t* channel){
  channel_t* ch;
  dfsch_object_t* ret;

  /*
   * pthread functions returning errors here probably means that something is 
   * fundamentaly wrong, so we don't bother to check for them.
   */

  if (!channel || channel->type != &channel_type)
    dfsch_throw("thread:not-a-channel", channel);
  
  ch = (channel_t*) channel;

  pthread_mutex_lock(&(ch->mutex));
  
  while(ch->readptr == ch->writeptr){ // Buffer is empty
    pthread_cond_wait(&(ch->read), &(ch->mutex));
  }

  ret = ch->buf[ch->readptr];
  ch->buf[ch->readptr] = NULL;
  ch->readptr = (ch->readptr + 1) % ch->buf_len;

  pthread_mutex_unlock(&(ch->mutex));
  pthread_cond_signal(&(ch->write));

  return ret;
}

void dfsch_channel_write(dfsch_object_t* channel,
                         dfsch_object_t* object){
  channel_t* ch;
  size_t new_writeptr;

  /*
   * pthread functions returning errors here probably means that something is 
   * fundamentaly wrong, so we don't bother to check for them.
   */

  if (!channel || channel->type != &channel_type)
    dfsch_throw("thread:not-a-channel", channel);
  
  ch = (channel_t*) channel;

  pthread_mutex_lock(&(ch->mutex));
  
  new_writeptr = (ch->writeptr + 1) % ch->buf_len;

  while(ch->readptr == new_writeptr){ // Buffer is full
    pthread_cond_wait(&(ch->write), &(ch->mutex));
  }

  ch->buf[ch->writeptr] = object;
  ch->writeptr = new_writeptr;

  pthread_mutex_unlock(&(ch->mutex));
  pthread_cond_signal(&(ch->read));

}


// Scheme binding

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
static dfsch_object_t* native_condition_create(void*baton, 
                                               dfsch_object_t* args, 
                                               dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_condition_create();
}
static dfsch_object_t* native_condition_wait(void*baton, 
                                             dfsch_object_t* args, 
                                             dfsch_tail_escape_t* esc){
  dfsch_object_t* cond;
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, cond);
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  dfsch_condition_wait(cond, mutex);
  return cond;
}
static dfsch_object_t* native_condition_signal(void*baton, 
                                               dfsch_object_t* args, 
                                               dfsch_tail_escape_t* esc){
  dfsch_object_t* cond;
  DFSCH_OBJECT_ARG(args, cond);
  DFSCH_ARG_END(args);

  dfsch_condition_signal(cond);
  return cond;
}
static dfsch_object_t* native_condition_broadcast(void*baton, 
                                                  dfsch_object_t* args, 
                                                  dfsch_tail_escape_t* esc){
  dfsch_object_t* cond;
  DFSCH_OBJECT_ARG(args, cond);
  DFSCH_ARG_END(args);

  dfsch_condition_broadcast(cond);
  return cond;
}

static dfsch_object_t* native_channel_create(void*baton, 
                                             dfsch_object_t* args, 
                                             dfsch_tail_escape_t* esc){
  size_t buffer;
  DFSCH_LONG_ARG_OPT(args, buffer, 16);
  DFSCH_ARG_END(args);

  return dfsch_channel_create(buffer);
}
static dfsch_object_t* native_channel_read(void*baton, 
                                           dfsch_object_t* args, 
                                           dfsch_tail_escape_t* esc){
  dfsch_object_t* channel;
  DFSCH_OBJECT_ARG(args, channel);
  DFSCH_ARG_END(args);

  return dfsch_channel_read(channel);
}
static dfsch_object_t* native_channel_write(void*baton, 
                                            dfsch_object_t* args, 
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* channel;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, channel);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  dfsch_channel_write(channel, object);
 
  return object;
}



dfsch_object_t* dfsch_threads_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "thread:create", 
                   dfsch_make_primitive(&native_thread_create,NULL));
  dfsch_define_cstr(ctx, "thread:join", 
                   dfsch_make_primitive(&native_thread_join,NULL));
  dfsch_define_cstr(ctx, "thread:detach", 
                   dfsch_make_primitive(&native_thread_detach,NULL));
  dfsch_define_cstr(ctx, "thread:self", 
                   dfsch_make_primitive(&native_thread_self,NULL));

  dfsch_define_cstr(ctx, "mutex:create", 
                   dfsch_make_primitive(&native_mutex_create,NULL));
  dfsch_define_cstr(ctx, "mutex:lock", 
                   dfsch_make_primitive(&native_mutex_lock,NULL));
  dfsch_define_cstr(ctx, "mutex:trylock", 
                   dfsch_make_primitive(&native_mutex_trylock,NULL));
  dfsch_define_cstr(ctx, "mutex:unlock", 
                   dfsch_make_primitive(&native_mutex_unlock,NULL));

  dfsch_define_cstr(ctx, "condition:create", 
                   dfsch_make_primitive(&native_condition_create,NULL));
  dfsch_define_cstr(ctx, "condition:wait", 
                   dfsch_make_primitive(&native_condition_wait,NULL));
  dfsch_define_cstr(ctx, "condition:signal", 
                   dfsch_make_primitive(&native_condition_signal,NULL));
  dfsch_define_cstr(ctx, "condition:broadcast", 
                   dfsch_make_primitive(&native_condition_broadcast,NULL));

  dfsch_define_cstr(ctx, "channel:create", 
                   dfsch_make_primitive(&native_channel_create,NULL));
  dfsch_define_cstr(ctx, "channel:read", 
                   dfsch_make_primitive(&native_channel_read,NULL));
  dfsch_define_cstr(ctx, "channel:write", 
                   dfsch_make_primitive(&native_channel_write,NULL));

  return NULL;
}
