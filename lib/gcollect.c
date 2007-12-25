#include "dfsch/lib/gcollect.h"
#include <dfsch/number.h>
#include <dfsch/load.h>


static dfsch_object_t* gcollect_gcollect(void* baton,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  GC_gcollect();

  return NULL;
}

static dfsch_object_t* gcollect_heap_size(void* baton,
                                          dfsch_object_t* args,
                                          dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_heap_size());
}

static dfsch_object_t* gcollect_free_bytes(void* baton,
                                           dfsch_object_t* args,
                                           dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_free_bytes());
}

static dfsch_object_t* gcollect_bytes_since_gc(void* baton,
                                               dfsch_object_t* args,
                                               dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_bytes_since_gc());
}

static dfsch_object_t* gcollect_total_bytes(void* baton,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_total_bytes());
}


static dfsch_object_t* gcollect_count(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_gc_no);
}

dfsch_object_t* dfsch_module_gcollect_register(dfsch_object_t* env){
  dfsch_provide(env, "gcollect");

  dfsch_define_cstr(env, "gcollect:gcollect",
                    dfsch_make_primitive(gcollect_gcollect, NULL));
  dfsch_define_cstr(env, "gcollect:heap-size",
                    dfsch_make_primitive(gcollect_heap_size, NULL));
  dfsch_define_cstr(env, "gcollect:free-bytes",
                    dfsch_make_primitive(gcollect_free_bytes, NULL));
  dfsch_define_cstr(env, "gcollect:bytes-since-gc",
                    dfsch_make_primitive(gcollect_bytes_since_gc, NULL));
  dfsch_define_cstr(env, "gcollect:total-bytes",
                    dfsch_make_primitive(gcollect_total_bytes, NULL));
  dfsch_define_cstr(env, "gcollect:count",
                    dfsch_make_primitive(gcollect_count, NULL));

  return NULL;
}
