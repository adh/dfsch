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

#ifndef H__dfsch__threads__
#define H__dfsch__threads__

#include <dfsch/dfsch.h>
#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_thread_create(dfsch_object_t* function,
                                             dfsch_object_t* arguments);
  extern dfsch_object_t* dfsch_thread_join(dfsch_object_t* thread);
  extern void dfsch_thread_detach(dfsch_object_t* thread);
  extern dfsch_object_t* dfsch_thread_self();

  extern dfsch_object_t* dfsch_mutex_create();
  extern void dfsch_mutex_lock(dfsch_object_t* mutex);
  extern int dfsch_mutex_trylock(dfsch_object_t* mutex);
  extern void dfsch_mutex_unlock(dfsch_object_t* mutex);

  extern dfsch_object_t* dfsch_condition_create();
  extern void dfsch_condition_wait(dfsch_object_t* condition,
                                   dfsch_object_t* mutex);
  extern void dfsch_condition_signal(dfsch_object_t* condition);
  extern void dfsch_condition_broadcast(dfsch_object_t* condition);

  extern dfsch_object_t* dfsch_channel_create(size_t buffer);
  extern dfsch_object_t* dfsch_channel_read(dfsch_object_t* channel);
  extern void dfsch_channel_write(dfsch_object_t* channel,
                                  dfsch_object_t* object);

  extern dfsch_object_t* dfsch_module_threads_register(dfsch_object_t *ctx);

#ifdef __cplusplus
}
#endif

#endif
