/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Promises
 * Copyright (C) 2005 Ales Hakl
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

#include <dfsch/promise.h>

dfsch_object_t* promise_type(){
  static dfsch_object_t* cache = NULL;
  if (!cache)
    cache = dfsch_make_symbol("promise");

  return cache;
}

typedef struct promise_t {
  dfsch_object_t* expr;
  dfsch_object_t* env;
  dfsch_object_t* value;
  int set;
} promise_t;


dfsch_object_t* dfsch_make_promise(dfsch_object_t* expr, dfsch_object_t* env){
  promise_t* p = GC_NEW(promise_t);

  p->expr = expr;
  p->env = env;
  p->value = NULL;
  p->set = 0;

  return dfsch_make_native_data(p, promise_type());
}

dfsch_object_t* dfsch_force_promise(dfsch_object_t* promise){
  promise_t* p = dfsch_native_data(promise, promise_type());

  if (!p->set){
    p->value = dfsch_eval_proc(p->expr, p->env);
    p->set = 1;
  }

  return p->value;

}
dfsch_object_t* dfsch_stream_tail(dfsch_object_t* stream){
  return dfsch_force_promise(dfsch_cdr(stream));
}
