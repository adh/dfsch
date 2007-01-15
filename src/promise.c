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

typedef struct promise_t {
  dfsch_type_t *type;
  dfsch_object_t* expr;
  dfsch_object_t* env;
  dfsch_object_t* value;
  int set;
} promise_t;

static const dfsch_type_t promise_type = {
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

  return (dfsch_object_t*)p;
}

dfsch_object_t* dfsch_force_promise(dfsch_object_t* promise){
  promise_t* p = (promise_t*)promise;
  if (promise->type != &promise_type)
    dfsch_throw("exception:not-a-promise", promise);

  if (!p->set){
    p->value = dfsch_eval_proc(p->expr, p->env);
    p->set = 1;
  }

  return p->value;

}
dfsch_object_t* dfsch_stream_tail(dfsch_object_t* stream){
  return dfsch_force_promise(dfsch_cdr(stream));
}

/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////

static dfsch_object_t* native_form_delay(void* baton, dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* env;
  DFSCH_OBJECT_ARG(args, env);

  return dfsch_make_promise(args, env);
}

static dfsch_object_t* native_force(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  dfsch_object_t* promise;
  DFSCH_OBJECT_ARG(args, promise);
  DFSCH_ARG_END(args);  

  return dfsch_force_promise(promise);
}

static dfsch_object_t* native_form_stream_cons(void* baton, 
                                               dfsch_object_t* args,
                                               dfsch_tail_escape_t* esc){
  dfsch_object_t* env;
  dfsch_object_t* head;
  dfsch_object_t* tail;

  DFSCH_OBJECT_ARG(args, env);
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

  return dfsch_stream_tail(stream);
}

dfsch_object_t* dfsch__promise_native_register(dfsch_object_t *ctx){
    dfsch_define_cstr(ctx, "delay", 
                   dfsch_make_form(dfsch_make_primitive(&native_form_delay,
                                                        NULL)));
  dfsch_define_cstr(ctx, "force", 
                   dfsch_make_primitive(&native_force,NULL));

  dfsch_define_cstr(ctx, "stream-cons", 
                   dfsch_make_form(dfsch_make_primitive(&native_form_stream_cons,
                                                        NULL)));
  dfsch_define_cstr(ctx, "stream-car", 
                   dfsch_make_primitive(&native_stream_car,NULL));
  dfsch_define_cstr(ctx, "stream-cdr", 
                   dfsch_make_primitive(&native_stream_cdr,NULL));

}
