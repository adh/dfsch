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

DFSCH_DEFINE_PRIMITIVE(make_bitvector, NULL){
  long length;
  DFSCH_LONG_ARG(args, length);
  DFSCH_ARG_END(args);

  return dfsch_collections_make_bitvector(length);
}
DFSCH_DEFINE_PRIMITIVE(bitvector, NULL){
  return dfsch_collections_list_2_bitvector(args);
}

DFSCH_DEFINE_PRIMITIVE(bitvector_increment, 
                       "Return next bitvector value in numeric ordering"){
  dfsch_object_t* bv;
  DFSCH_OBJECT_ARG(args, bv);
  return dfsch_collections_bitvector_increment(bv);
}

void dfsch_module_collections_register(dfsch_object_t* env){
  dfsch_package_t* collections = dfsch_make_package("collections",
                                                    "Advanced collections");
  dfsch_provide(env, "collections");

  dfsch_defcanon_pkgcstr(env, collections, "<priority-queue>",
                         DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE);
  dfsch_defcanon_pkgcstr(env, collections, "make-priority-queue",
                         DFSCH_PRIMITIVE_REF(make_priority_queue));
  dfsch_defcanon_pkgcstr(env, collections, "priority-queue-push!",
                         DFSCH_PRIMITIVE_REF(priority_queue_push));
  dfsch_defcanon_pkgcstr(env, collections, "priority-queue-pop!",
                         DFSCH_PRIMITIVE_REF(priority_queue_pop));
  dfsch_defcanon_pkgcstr(env, collections, "priority-queue-empty?",
                         DFSCH_PRIMITIVE_REF(priority_queue_empty_p));

  dfsch_defcanon_pkgcstr(env, collections, "<bitvector>",
                         DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  dfsch_defcanon_pkgcstr(env, collections, "make-bitvector",
                         DFSCH_PRIMITIVE_REF(make_bitvector));
  dfsch_defcanon_pkgcstr(env, collections, "bitvector",
                         DFSCH_PRIMITIVE_REF(bitvector));

  dfsch_defcanon_pkgcstr(env, collections, "bitvector-increment",
                         DFSCH_PRIMITIVE_REF(bitvector_increment));

}
