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
dfsch_object_t* dfsch_stream_rest(dfsch_object_t* stream){
  return dfsch_force_promise(dfsch_cdr(stream));
}
