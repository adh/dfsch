#include <dfsch/lib/collections.h>

DFSCH_DEFINE_PRIMITIVE(make_priority_queue, 0){
  dfsch_object_t* lt;
  DFSCH_OBJECT_ARG(args, lt);
  DFSCH_ARG_END(args);
  return dfsch_collections_make_priority_queue(lt);
}
DFSCH_DEFINE_PRIMITIVE(priority_queue_empty_p, 0){
  dfsch_object_t* queue;
  DFSCH_OBJECT_ARG(args, queue);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_collections_priority_queue_empty_p(queue));
}
DFSCH_DEFINE_PRIMITIVE(priority_queue_pop, 0){
  dfsch_object_t* queue;
  DFSCH_OBJECT_ARG(args, queue);
  DFSCH_ARG_END(args);
  return dfsch_collections_priority_queue_pop(queue);
}
DFSCH_DEFINE_PRIMITIVE(priority_queue_push, 0){
  dfsch_object_t* queue;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, queue);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);
  dfsch_collections_priority_queue_push(queue, object);
  return NULL;
}

void dfsch_module_collections_register(dfsch_object_t* env){
  dfsch_provide(env, "collections");
  dfsch_define_cstr(env, "collections:<priority-queue>",
                    DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE);
  dfsch_define_cstr(env, "collections:make-priority-queue",
                    DFSCH_PRIMITIVE_REF(make_priority_queue));
  dfsch_define_cstr(env, "collections:priority-queue-push!",
                    DFSCH_PRIMITIVE_REF(priority_queue_push));
  dfsch_define_cstr(env, "collections:priority-queue-pop!",
                    DFSCH_PRIMITIVE_REF(priority_queue_pop));
  dfsch_define_cstr(env, "collections:priority-queue-empty?",
                    DFSCH_PRIMITIVE_REF(priority_queue_empty_p));
}
