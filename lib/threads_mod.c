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
// Scheme binding

static dfsch_object_t* native_thread_create(void*baton, dfsch_object_t* args, 
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* function;
  dfsch_object_t* arguments;
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_OBJECT_ARG_OPT(args, arguments, NULL);
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



dfsch_object_t* dfsch_module_threads_register(dfsch_object_t *ctx){
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

  dfsch_provide(ctx, "threads");

  return NULL;
}
