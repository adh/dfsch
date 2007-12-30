/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Simple custom data-types
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
  dfsch_object_t* hash;
} wrapper_type_t;

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
static size_t wrapper_hash(dfsch_object_t* obj){
  wrapper_type_t* type = (wrapper_type_t*) obj->type;

  return 
    dfsch_make_number_from_long(dfsch_apply(type->hash,
                                            dfsch_list(1,
                                                       obj)));
}

static char*  wrapper_type_write(wrapper_type_t* t, int max_depth, int readable){
    str_list_t* l = sl_create();
    char buf[sizeof(void*)*2+1];

    sl_append(l, "#<wrapper-type 0x");
    snprintf(buf, sizeof(void*)*2+1, "%x", t);
    sl_append(l, buf);   
    sl_append(l, " ");
    sl_append(l, t->type.name);
    sl_append(l,">");
    
    return sl_value(l);
}


static const dfsch_type_t wrapper_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(wrapper_type_t),
  "wrapper-type",
  NULL,
  (dfsch_type_write_t)wrapper_type_write,
  NULL,
};


extern dfsch_object_t* dfsch_make_wrapper_type(char* name,
                                               dfsch_object_t* write,
                                               dfsch_object_t* equal_p,
                                               dfsch_object_t* apply,
                                               dfsch_object_t* hash){

  wrapper_type_t* t = (wrapper_type_t*)
    dfsch_make_object((dfsch_type_t*)&wrapper_type);
  
  t->type.name = name;
  t->type.size = sizeof(wrapper_t);
  
  if (write){
    t->type.write = wrapper_write;
    t->write = write;
  }else{
    t->type.write = NULL;
  }

  if (equal_p){
    t->type.equal_p = wrapper_equal_p;
    t->equal_p = equal_p;
  }else{
    t->type.equal_p = NULL;
  }

  if (apply){
    t->type.apply = wrapper_apply;
    t->apply = apply;
  }else{
    t->type.apply = NULL;
  }

  if (hash){
    t->type.hash = wrapper_hash;
    t->hash = hash;
  }else{
    t->type.hash = NULL;
  }
  
  return (dfsch_object_t*)t;
}

extern dfsch_object_t* dfsch_wrap(dfsch_object_t* type,
                                  dfsch_object_t* object){
  wrapper_t* w;
  wrapper_type_t* t;

  if (!type || type->type != &wrapper_type)
    dfsch_error("exception:not-a-wrapper-type", type);

  t = (wrapper_type_t*)type;

  w = (wrapper_t*)dfsch_make_object((dfsch_type_t*)t);
  w->object = object;
  return (dfsch_object_t*)w;
}

extern dfsch_object_t* dfsch_unwrap(dfsch_object_t* type,
                                    dfsch_object_t* wrapper){
  wrapper_t* w;
  wrapper_type_t* t;

  if (!type || type->type != &wrapper_type)
    dfsch_error("exception:not-a-wrapper-type", type);
  t = (wrapper_type_t*)type;

  if (!wrapper || wrapper->type != (dfsch_type_t*)t)
    dfsch_error("exception:type-mismatch", type);
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
  dfsch_object_t* hash;
  DFSCH_STRING_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, write, NULL);
  DFSCH_OBJECT_ARG_OPT(args, equal_p, NULL);
  DFSCH_OBJECT_ARG_OPT(args, apply, NULL);
  DFSCH_OBJECT_ARG_OPT(args, hash, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_wrapper_type(name, write, equal_p, apply, hash);
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

void dfsch__wrapper_native_register(dfsch_object_t *ctx){ 
  dfsch_define_cstr(ctx, "<wrapper-type>", &wrapper_type);

  dfsch_define_cstr(ctx, "make-wrapper-type", 
		   dfsch_make_primitive(&native_make_wrapper_type, NULL));
  dfsch_define_cstr(ctx, "wrap", 
		   dfsch_make_primitive(&native_wrap, NULL));
  dfsch_define_cstr(ctx, "unwrap", 
		   dfsch_make_primitive(&native_unwrap, NULL));
}
