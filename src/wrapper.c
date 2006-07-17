#include "dfsch/wrapper.h"

#include <dfsch/strings.h>

#include "util.h"

/*
 * WARNING: magic ahead
 */

/*
 * This is how you add your own private fields to type structure - wrap it to 
 * another structure. C guarantees that address of first field will be equal to
 * address of whole structure, so this will work (Gtk+ uses this hack too).
 *
 * Rationale: most code doesn't need this, so it's unnecessary overhead to 
 * store NULL pointer with every type.
 */

typedef struct wrapper_type_t {
  dfsch_type_t type;
  dfsch_object_t* write;
  dfsch_object_t* equal_p;
  dfsch_object_t* apply;
} wrapper_type_t;

typedef struct wrapper_type_obj_t {
  dfsch_type_t* dfsch_type;
  wrapper_type_t type;
} wrapper_type_obj_t;

typedef struct wrapper_t {
  dfsch_type_t* type;
  dfsch_object_t* object;
} wrapper_t;

static char* wrapper_write(dfsch_object_t* obj, int depth, int readable){
  wrapper_type_t* type = (wrapper_type_t*) obj->type;

  return 
    dfsch_string_to_cstr(dfsch_apply(type->write,
                                     dfsch_list(3,
                                                obj,
                                                dfsch_make_number_from_long
                                                  (depth),
                                                dfsch_bool(readable))));
}

static int wrapper_equal_p(dfsch_object_t* a, dfsch_object_t* b){
  wrapper_type_t* type = (wrapper_type_t*) a->type;

  return (dfsch_apply(type->equal_p,
                      dfsch_list(2, a, b))) != NULL;
  
}

static dfsch_object_t* wrapper_apply(dfsch_object_t* obj, 
                                     dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  wrapper_type_t* type = (wrapper_type_t*) obj->type;

  return dfsch_apply_tr(type->apply,
                        dfsch_list(2, obj, args),
                        esc);
}

static char*  wrapper_type_write(wrapper_type_obj_t* t, int max_depth, int readable){
    str_list_t* l = sl_create();
    char buf[sizeof(void*)*2+1];

    sl_append(l, "#<wrapper-type 0x");
    snprintf(buf, sizeof(void*)*2+1, "%x", t);
    sl_append(l, buf);   
    sl_append(l, " ");
    sl_append(l, t->type.type.name);
    sl_append(l,">");
    
    return sl_value(l);
}


static const dfsch_type_t wrapper_type = {
  sizeof(wrapper_type_obj_t),
  "wrapper-type",
  NULL,
  (dfsch_type_write_t)wrapper_type_write,
  NULL,
};


extern dfsch_object_t* dfsch_make_wrapper_type(char* name,
                                               dfsch_object_t* write,
                                               dfsch_object_t* equal_p,
                                               dfsch_object_t* apply){

  wrapper_type_obj_t* t = (wrapper_type_obj_t*)
    dfsch_make_object((dfsch_type_t*)&wrapper_type);
  
  t->type.type.name = name;
  t->type.type.size = sizeof(wrapper_t);
  
  if (write){
    t->type.type.write = wrapper_write;
    t->type.write = write;
  }else{
    t->type.type.write = NULL;
  }

  if (equal_p){
    t->type.type.equal_p = wrapper_equal_p;
    t->type.equal_p = equal_p;
  }else{
    t->type.type.equal_p = NULL;
  }

  if (apply){
    t->type.type.apply = wrapper_apply;
    t->type.apply = apply;
  }else{
    t->type.type.apply = NULL;
  }
  
  return (dfsch_object_t*)t;
}

extern dfsch_object_t* dfsch_wrap(dfsch_object_t* type,
                                  dfsch_object_t* object){
  wrapper_t* w;
  wrapper_type_obj_t* t;

  if (!type || type->type != &wrapper_type)
    dfsch_throw("exception:not-a-wrapper-type", type);

  t = (wrapper_type_obj_t*)type;

  w = (wrapper_t*)dfsch_make_object((dfsch_type_t*)&(t->type));
  w->object = object;
  return (dfsch_object_t*)w;
}

extern dfsch_object_t* dfsch_unwrap(dfsch_object_t* type,
                                    dfsch_object_t* wrapper){
  wrapper_t* w;
  wrapper_type_obj_t* t;

  if (!type || type->type != &wrapper_type)
    dfsch_throw("exception:not-a-wrapper-type", type);
  t = (wrapper_type_obj_t*)type;

  if (!wrapper || wrapper->type != (dfsch_type_t*)&(t->type))
    dfsch_throw("exception:type-mismatch", type);
  w = (wrapper_t*)wrapper;

  return w->object;
}

static dfsch_object_t* native_make_wrapper_type(void *baton, 
                                                dfsch_object_t* args, 
                                                dfsch_tail_escape_t* esc){
  char* name;
  dfsch_object_t* write;
  dfsch_object_t* equal_p;
  dfsch_object_t* apply;
  DFSCH_STRING_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, write, NULL);
  DFSCH_OBJECT_ARG_OPT(args, equal_p, NULL);
  DFSCH_OBJECT_ARG_OPT(args, apply, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_wrapper_type(name, write, equal_p, apply);
}

static dfsch_object_t* native_wrap(void *baton, 
                                   dfsch_object_t* args, 
                                   dfsch_tail_escape_t* esc){
  dfsch_object_t* type;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG_OPT(args, type, NULL);
  DFSCH_OBJECT_ARG_OPT(args, object, NULL);
  DFSCH_ARG_END(args);

  return dfsch_wrap(type, object);
}
static dfsch_object_t* native_unwrap(void *baton, 
                                     dfsch_object_t* args, 
                                     dfsch_tail_escape_t* esc){
  dfsch_object_t* type;
  dfsch_object_t* wrapper;
  DFSCH_OBJECT_ARG_OPT(args, type, NULL);
  DFSCH_OBJECT_ARG_OPT(args, wrapper, NULL);
  DFSCH_ARG_END(args);

  return dfsch_unwrap(type, wrapper);
}

void dfsch__wrapper_native_register(dfsch_ctx_t *ctx){ 
  dfsch_ctx_define(ctx, "make-wrapper-type", 
		   dfsch_make_primitive(&native_make_wrapper_type, NULL));
  dfsch_ctx_define(ctx, "wrap", 
		   dfsch_make_primitive(&native_wrap, NULL));
  dfsch_ctx_define(ctx, "unwrap", 
		   dfsch_make_primitive(&native_unwrap, NULL));
}
