/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Multithreading API
 * Copyright (C) 2005-2008 Ales Hakl
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

#include "dfsch/lib/threads.h"

#include <dfsch/number.h>
#include "src/util.h"
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
  NULL,
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
  if (DFSCH_TYPE_OF(thread) != &thread_type)
    dfsch_error("thread:not-a-thread", thread);

  t = (thread_t*)thread;

  err = pthread_join(t->thread, (void*)&ret);

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }

  return ret;
}
void dfsch_thread_detach(dfsch_object_t* thread){
  thread_t* t;
  int err;
  if (DFSCH_TYPE_OF(thread) != &thread_type)
    dfsch_error("thread:not-a-thread", thread);

  t = (thread_t*)thread;

  err = pthread_detach(t->thread);

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
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
  NULL,
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
  if (DFSCH_TYPE_OF(mutex) != &mutex_type)
    dfsch_error("thread:not-a-mutex", mutex);

  m = (mutex_t*)mutex;

  err = pthread_mutex_lock(&(m->mutex));

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}
int dfsch_mutex_trylock(dfsch_object_t* mutex){
  mutex_t* m;
  if (DFSCH_TYPE_OF(mutex) != &mutex_type)
    dfsch_error("thread:not-a-mutex", mutex);

  m = (mutex_t*)mutex;

  return (pthread_mutex_trylock(&(m->mutex)) == 0);
}
void dfsch_mutex_unlock(dfsch_object_t* mutex){
  mutex_t* m;
  int err;
  if (DFSCH_TYPE_OF(mutex) != &mutex_type)
    dfsch_error("thread:not-a-mutex", mutex);

  m = (mutex_t*)mutex;

  err = pthread_mutex_unlock(&(m->mutex));

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

// Condition variables

typedef struct condition_t {
  dfsch_type_t* type;
  pthread_cond_t cond;
} condition_t;

static const dfsch_type_t condition_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
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

  if (DFSCH_TYPE_OF(mutex) != &mutex_type)
    dfsch_error("thread:not-a-mutex", mutex);
  if (DFSCH_TYPE_OF(cond) != &condition_type)
    dfsch_error("thread:not-a-condition", cond);

  c = (condition_t*)cond;
  m = (mutex_t*)mutex;

  err = pthread_cond_wait(&(c->cond), &(m->mutex));

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

void dfsch_condition_signal(dfsch_object_t* cond){
  condition_t* c;
  int err;
  if (DFSCH_TYPE_OF(cond) != &condition_type)
    dfsch_error("thread:not-a-condition", cond);

  c = (condition_t*)cond;

  err = pthread_cond_signal(&(c->cond));

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

void dfsch_condition_broadcast(dfsch_object_t* cond){
  condition_t* c;
  int err;
  if (DFSCH_TYPE_OF(cond) != &condition_type)
    dfsch_error("thread:not-a-condition", cond);

  c = (condition_t*)cond;

  err = pthread_cond_broadcast(&(c->cond));

  if (err != 0){
    dfsch_error("thread:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
}

// Channels

/*
 * Channel is simply inter-thread pipe for transferring objects. In theory,
 * it could be possible to use channel as a queue inside one thread - but you
 * are risking a deadlock here when channels' buffer becomes full or empty.
 */

typedef struct channel_t {
  dfsch_type_t* type;

  pthread_mutex_t* mutex;
  pthread_cond_t* read;
  pthread_cond_t* write;
  
  dfsch_object_t** buf;
  
  size_t buf_len;
  size_t readptr;
  size_t writeptr;
} channel_t;

static const dfsch_type_t channel_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(channel_t), 
  "channel",
  NULL,
  NULL,
  NULL
};

dfsch_object_t* dfsch_channel_create(size_t buffer){
  channel_t* ch = (channel_t*)dfsch_make_object(&channel_type);

  ch->read = create_finalized_cvar();
  ch->write = create_finalized_cvar();
  ch->mutex = create_finalized_mutex();

  ch->buf = GC_MALLOC(sizeof(dfsch_object_t*)*buffer);
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

  if (DFSCH_TYPE_OF(channel) != &channel_type)
    dfsch_error("thread:not-a-channel", channel);
  
  ch = (channel_t*) channel;

  pthread_mutex_lock(ch->mutex);
  
  while(ch->readptr == ch->writeptr){ // Buffer is empty
    pthread_cond_wait(ch->read, ch->mutex);
  }

  ret = ch->buf[ch->readptr];
  ch->buf[ch->readptr] = NULL;
  ch->readptr = (ch->readptr + 1) % ch->buf_len;

  pthread_mutex_unlock(ch->mutex);
  pthread_cond_signal(ch->write);

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

  if (DFSCH_TYPE_OF(channel) != &channel_type)
    dfsch_error("thread:not-a-channel", channel);
  
  ch = (channel_t*) channel;

  pthread_mutex_lock(ch->mutex);
  
  new_writeptr = (ch->writeptr + 1) % ch->buf_len;

  while(ch->readptr == new_writeptr){ // Buffer is full
    pthread_cond_wait(ch->write, ch->mutex);
  }

  ch->buf[ch->writeptr] = object;
  ch->writeptr = new_writeptr;

  pthread_mutex_unlock(ch->mutex);
  pthread_cond_signal(ch->read);

}


