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
DFSCH_DEFINE_PRIMITIVE(bitvector_2_list, NULL){
  dfsch_object_t* bitvector;
  DFSCH_OBJECT_ARG(args, bitvector);
  DFSCH_ARG_END(args);

  return dfsch_collections_bitvector_2_list(bitvector);
}
DFSCH_DEFINE_PRIMITIVE(list_2_bitvector, NULL){
  dfsch_object_t* list;
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_collections_list_2_bitvector(list);
}
DFSCH_DEFINE_PRIMITIVE(bitvector_ref, NULL){
  long k;
  dfsch_object_t* bitvector;
  DFSCH_OBJECT_ARG(args, bitvector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_collections_bitvector_ref(bitvector, k));
}
DFSCH_DEFINE_PRIMITIVE(bitvector_set, NULL){
  long k;
  dfsch_object_t* bitvector;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, bitvector);
  DFSCH_LONG_ARG(args, k);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  dfsch_collections_bitvector_set(bitvector, k, value);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(bitvector_length, NULL){
  dfsch_object_t* bitvector;
  DFSCH_OBJECT_ARG(args, bitvector);
  DFSCH_ARG_END(args);

  return 
    dfsch_make_number_from_long(dfsch_collections_bitvector_length(bitvector));
}

void dfsch_module_collections_register(dfsch_object_t* env){
  dfsch_package_t* collections = dfsch_make_package("collections");
  dfsch_provide(env, "collections");

  dfsch_defconst_pkgcstr(env, collections, "<priority-queue>",
                         DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE);
  dfsch_defconst_pkgcstr(env, collections, "make-priority-queue",
                         DFSCH_PRIMITIVE_REF(make_priority_queue));
  dfsch_defconst_pkgcstr(env, collections, "priority-queue-push!",
                         DFSCH_PRIMITIVE_REF(priority_queue_push));
  dfsch_defconst_pkgcstr(env, collections, "priority-queue-pop!",
                         DFSCH_PRIMITIVE_REF(priority_queue_pop));
  dfsch_defconst_pkgcstr(env, collections, "priority-queue-empty?",
                         DFSCH_PRIMITIVE_REF(priority_queue_empty_p));

  dfsch_defconst_pkgcstr(env, collections, "make-bitvector",
                         DFSCH_PRIMITIVE_REF(make_bitvector));
  dfsch_defconst_pkgcstr(env, collections, "bitvector->list",
                         DFSCH_PRIMITIVE_REF(bitvector_2_list));
  dfsch_defconst_pkgcstr(env, collections, "list->bitvector",
                         DFSCH_PRIMITIVE_REF(list_2_bitvector));
  dfsch_defconst_pkgcstr(env, collections, "bitvector-ref",
                         DFSCH_PRIMITIVE_REF(bitvector_ref));
  dfsch_defconst_pkgcstr(env, collections, "bitvector-set!",
                         DFSCH_PRIMITIVE_REF(bitvector_set));
  dfsch_defconst_pkgcstr(env, collections, "bitvector-length",
                         DFSCH_PRIMITIVE_REF(bitvector_length));

}
