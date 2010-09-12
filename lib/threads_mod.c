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

DFSCH_DEFINE_PRIMITIVE(thread_create, "Create new thread"){
  dfsch_object_t* function;
  dfsch_object_t* arguments;
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_OBJECT_ARG_OPT(args, arguments, NULL);
  DFSCH_ARG_END(args);

  return dfsch_thread_create(function, arguments);
}

DFSCH_DEFINE_PRIMITIVE(thread_join, 
                       "Wait for thread to terminate it's execution"){
  dfsch_object_t* thread;
  DFSCH_OBJECT_ARG(args, thread);
  DFSCH_ARG_END(args);

  return dfsch_thread_join(thread);
}
DFSCH_DEFINE_PRIMITIVE(thread_detach, 
                       "Mark thread as not joinable"){
  dfsch_object_t* thread;
  DFSCH_OBJECT_ARG(args, thread);
  DFSCH_ARG_END(args);

  dfsch_thread_detach(thread);
  return thread;
}
DFSCH_DEFINE_PRIMITIVE(thread_self, 
                       "Return thread object representing calling thread"){
  DFSCH_ARG_END(args);

  return dfsch_thread_self();
}


DFSCH_DEFINE_PRIMITIVE(mutex_create, "Create new mutex object"){
  DFSCH_ARG_END(args);

  return dfsch_mutex_create();
}
DFSCH_DEFINE_PRIMITIVE(mutex_lock, "Lock mutex object"){
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  dfsch_mutex_lock(mutex);
  return mutex;
}
DFSCH_DEFINE_PRIMITIVE(mutex_trylock, 
                       "Lock mutex only when such operation would not block"){
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_mutex_trylock(mutex));
}
DFSCH_DEFINE_PRIMITIVE(mutex_unlock, "Unlock mutex object"){
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  dfsch_mutex_unlock(mutex);
  return mutex;
}
DFSCH_DEFINE_PRIMITIVE(condition_create, 
                       "Create new condition variable object"){
  DFSCH_ARG_END(args);

  return dfsch_condition_create();
}
DFSCH_DEFINE_PRIMITIVE(condition_wait, 
                       "Wait for contidion variable to be signalled"){
  dfsch_object_t* cond;
  dfsch_object_t* mutex;
  DFSCH_OBJECT_ARG(args, cond);
  DFSCH_OBJECT_ARG(args, mutex);
  DFSCH_ARG_END(args);

  dfsch_condition_wait(cond, mutex);
  return cond;
}
DFSCH_DEFINE_PRIMITIVE(condition_signal,
                       "Wake up one randomly selected blocked thread"){
  dfsch_object_t* cond;
  DFSCH_OBJECT_ARG(args, cond);
  DFSCH_ARG_END(args);

  dfsch_condition_signal(cond);
  return cond;
}
DFSCH_DEFINE_PRIMITIVE(condition_broadcast,
                       "Wake up all threads blocked on condition variable"){
  dfsch_object_t* cond;
  DFSCH_OBJECT_ARG(args, cond);
  DFSCH_ARG_END(args);

  dfsch_condition_broadcast(cond);
  return cond;
}

DFSCH_DEFINE_PRIMITIVE(channel_create, 
                       "Create new channel object (inter-thread object pipe)"){
  size_t buffer;
  DFSCH_LONG_ARG_OPT(args, buffer, 16);
  DFSCH_ARG_END(args);

  return dfsch_channel_create(buffer);
}
DFSCH_DEFINE_PRIMITIVE(channel_read, "Read object from channel"){
  dfsch_object_t* channel;
  DFSCH_OBJECT_ARG(args, channel);
  DFSCH_ARG_END(args);

  return dfsch_channel_read(channel);
}

DFSCH_DEFINE_PRIMITIVE(channel_write, "Write object into channel"){
  dfsch_object_t* channel;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, channel);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  dfsch_channel_write(channel, object);
 
  return object;
}



dfsch_object_t* dfsch_module_threads_register(dfsch_object_t *ctx){
  dfsch_package_t* threads = dfsch_make_package("threads");
  dfsch_provide(ctx, "threads");

  dfsch_defconst_pkgcstr(ctx, threads, "thread-create", 
                         DFSCH_PRIMITIVE_REF(thread_create));
  dfsch_defconst_pkgcstr(ctx, threads, "thread-join", 
                         DFSCH_PRIMITIVE_REF(thread_join));
  dfsch_defconst_pkgcstr(ctx, threads, "thread-detach", 
                         DFSCH_PRIMITIVE_REF(thread_detach));
  dfsch_defconst_pkgcstr(ctx, threads, "thread-self", 
                         DFSCH_PRIMITIVE_REF(thread_self));

  dfsch_defconst_pkgcstr(ctx, threads, "mutex-create", 
                         DFSCH_PRIMITIVE_REF(mutex_create));
  dfsch_defconst_pkgcstr(ctx, threads, "mutex-lock", 
                         DFSCH_PRIMITIVE_REF(mutex_lock));
  dfsch_defconst_pkgcstr(ctx, threads, "mutex-trylock", 
                         DFSCH_PRIMITIVE_REF(mutex_trylock));
  dfsch_defconst_pkgcstr(ctx, threads, "mutex-unlock", 
                         DFSCH_PRIMITIVE_REF(mutex_unlock));

  dfsch_defconst_pkgcstr(ctx, threads, "condition-create", 
                         DFSCH_PRIMITIVE_REF(condition_create));
  dfsch_defconst_pkgcstr(ctx, threads, "condition-wait", 
                         DFSCH_PRIMITIVE_REF(condition_wait));
  dfsch_defconst_pkgcstr(ctx, threads, "condition-signal", 
                         DFSCH_PRIMITIVE_REF(condition_signal));
  dfsch_defconst_pkgcstr(ctx, threads, "condition-broadcast", 
                         DFSCH_PRIMITIVE_REF(condition_broadcast));

  dfsch_defconst_pkgcstr(ctx, threads, "channel-create", 
                         DFSCH_PRIMITIVE_REF(channel_create));
  dfsch_defconst_pkgcstr(ctx, threads, "channel-read", 
                         DFSCH_PRIMITIVE_REF(channel_read));
  dfsch_defconst_pkgcstr(ctx, threads, "channel-write", 
                         DFSCH_PRIMITIVE_REF(channel_write));


  return NULL;
}
