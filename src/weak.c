#include "dfsch/weak.h"

#include "internal.h"

#include <gc/gc.h>

#include <stdlib.h>


typedef struct reference_t {
  dfsch_type_t* type;
  dfsch_object_t* object;
  size_t live;
} reference_t;

static const dfsch_type_t reference_type = {
  DFSCH_STANDARD_TYPE,
  sizeof(reference_t),
  "weak-reference",
  NULL,
  NULL,
  NULL
};

/* We cannot use dfsch_make_object() here - we need to call GC_MALLOC_ATOMIC */
dfsch_object_t* dfsch_make_weak_reference(dfsch_object_t* refered){
  reference_t* ref = (reference_t*)GC_MALLOC_ATOMIC(sizeof(reference_t));

  ref->type = (dfsch_type_t*)&reference_type;
  
  ref->object = refered;
  ref->live = 1;
  if (refered && GC_base(refered)){ 
    /* Causes random crashes with non GC'able objects*/
    GC_general_register_disappearing_link((void**)&ref->live, refered);
  }

  return (dfsch_object_t*)ref;
}

int dfsch_weak_reference_live_p(dfsch_object_t* reference){
  reference_t* ref;
  if (!reference || reference->type != &reference_type)
    dfsch_throw("exception:not-a-reference", reference);
  ref = (reference_t*) reference;

  return ref->live;
}
dfsch_object_t* dfsch_weak_reference_dereference(dfsch_object_t* reference){
  reference_t* ref;
  if (!reference || reference->type != &reference_type)
    dfsch_throw("exception:not-a-reference", reference);
  ref = (reference_t*) reference;

  if (!ref->live){
    ref->object = NULL;
    return NULL;
  } else {
    return ref->object;
  }
}

static dfsch_object_t* native_make_weak_reference(void *baton, 
                                                  dfsch_object_t* args, 
                                                  dfsch_tail_escape_t* esc){
  dfsch_object_t* object;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_weak_reference(object);
}
static dfsch_object_t* native_weak_reference_live_p(void *baton, 
                                                    dfsch_object_t* args, 
                                                    dfsch_tail_escape_t* esc){
  dfsch_object_t* reference;

  DFSCH_OBJECT_ARG(args, reference);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_weak_reference_live_p(reference));
}
static dfsch_object_t* native_weak_reference_dereference(void *baton, 
                                                         dfsch_object_t* args,
                                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* reference;

  DFSCH_OBJECT_ARG(args, reference);
  DFSCH_ARG_END(args);

  return dfsch_weak_reference_dereference(reference);
}


void dfsch__weak_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "make-weak-reference", 
                    dfsch_make_primitive(&native_make_weak_reference,NULL));
  dfsch_define_cstr(ctx, "weak-reference-live?", 
                    dfsch_make_primitive(&native_weak_reference_live_p,NULL));
  dfsch_define_cstr(ctx, "weak-reference-dereference", 
                    dfsch_make_primitive(&native_weak_reference_dereference,
                                         NULL));

}
