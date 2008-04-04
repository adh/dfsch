/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Promises
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

#include <dfsch/promise.h>
#include <dfsch/compiler.h>
#include "util.h"

typedef struct promise_t {
  dfsch_type_t *type;
  dfsch_object_t* expr;
  dfsch_object_t* env;
  dfsch_object_t* value;
  pthread_mutex_t* mutex;
  int set;
} promise_t;

static const dfsch_type_t promise_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(promise_t),
  "promise",
  NULL,
  NULL
};

dfsch_object_t* dfsch_make_promise(dfsch_object_t* expr, dfsch_object_t* env){
  promise_t* p = (promise_t*)dfsch_make_object(&promise_type);

  p->expr = expr;
  p->env = env;
  p->value = NULL;
  p->set = 0;
  p->mutex = create_finalized_mutex();

  return (dfsch_object_t*)p;
}

/*
 * What are correct semantics of this function in multithreaded program is 
 * somehow ill-defined. With this implementation only one value will be ever 
 * returned, but underlying function could be called multiple times (which is 
 * simple extension of original single threaded R5RS semantics).
 */

dfsch_object_t* dfsch_force_promise(dfsch_object_t* promise){
  dfsch_object_t* val;
  promise_t* p = (promise_t*)promise;
  if (DFSCH_TYPE_OF(promise) != &promise_type)
    dfsch_error("exception:not-a-promise", promise);

  if (!p->set){
    val = dfsch_eval_proc(p->expr, p->env);
    pthread_mutex_lock(p->mutex);
    if (!p->set){
      p->set = 1;
      p->value = val;
    }
    pthread_mutex_unlock(p->mutex);
  }

  return p->value;

}
dfsch_object_t* dfsch_stream_cdr(dfsch_object_t* stream){
  return dfsch_force_promise(dfsch_cdr(stream));
}

/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////

DFSCH_DEFINE_FORM_IMPL(delay, dfsch_form_compiler_eval_all){
  return dfsch_make_promise(args, env);
}

static dfsch_object_t* native_force(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  dfsch_object_t* promise;
  DFSCH_OBJECT_ARG(args, promise);
  DFSCH_ARG_END(args);  

  return dfsch_force_promise(promise);
}

DFSCH_DEFINE_FORM_IMPL(stream_cons, dfsch_form_compiler_eval_all){
  dfsch_object_t* head;
  dfsch_object_t* tail;

  DFSCH_OBJECT_ARG(args, head);
  DFSCH_OBJECT_ARG(args, tail);
  DFSCH_ARG_END(args);  

  return dfsch_cons(dfsch_eval(head, env), 
                    dfsch_make_promise(dfsch_list(1,
                                                  tail),
                                       env));
}

static dfsch_object_t* native_stream_car(void* baton, dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* stream;
  DFSCH_OBJECT_ARG(args, stream);
  DFSCH_ARG_END(args);  

  return dfsch_car(stream);
}
static dfsch_object_t* native_stream_cdr(void* baton, dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* stream;
  DFSCH_OBJECT_ARG(args, stream);
  DFSCH_ARG_END(args);  

  return dfsch_stream_cdr(stream);
}

dfsch_object_t* dfsch__promise_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<promise>", &promise_type);
  dfsch_define_cstr(ctx, "delay", DFSCH_FORM_REF(delay));
  dfsch_define_cstr(ctx, "force", 
                   dfsch_make_primitive(&native_force,NULL));

  dfsch_define_cstr(ctx, "stream-cons", DFSCH_FORM_REF(stream_cons));
  dfsch_define_cstr(ctx, "stream-car", 
                   dfsch_make_primitive(&native_stream_car,NULL));
  dfsch_define_cstr(ctx, "stream-cdr", 
                   dfsch_make_primitive(&native_stream_cdr,NULL));

}
