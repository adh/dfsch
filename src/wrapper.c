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

static void wrapper_write(dfsch_object_t* obj, dfsch_writer_state_t* state){
  wrapper_type_t* type = (wrapper_type_t*) obj->type;

  dfsch_string_to_cstr(dfsch_apply(type->write,
                                   dfsch_list(2,
                                              obj,
                                              state)));
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
static uint32_t wrapper_hash(dfsch_object_t* obj){
  wrapper_type_t* type = (wrapper_type_t*) obj->type;

  return 
    dfsch_number_to_long(dfsch_apply(type->hash,
                                     dfsch_list(1,
                                                obj)));
}

static dfsch_type_t wrapper_basetype = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "wraped-object",
  NULL,
  NULL,
  NULL,
  NULL
};


static dfsch_type_t wrapper_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(wrapper_type_t),
  "wrapper-type",
  NULL,
  NULL,
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
  t->type.superclass = &wrapper_basetype;
  
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

  if (DFSCH_TYPE_OF(type) != &wrapper_type)
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

  if (DFSCH_TYPE_OF(type) != &wrapper_type)
    dfsch_error("exception:not-a-wrapper-type", type);
  t = (wrapper_type_t*)type;

  if (DFSCH_TYPE_OF(wrapper) != (dfsch_type_t*)t)
    dfsch_error("exception:type-mismatch", type);
  w = (wrapper_t*)wrapper;

  return w->object;
}

DFSCH_DEFINE_FORM_IMPL(define_wrapper_type, NULL){
  dfsch_object_t* write = NULL;
  dfsch_object_t* equal_p = NULL;
  dfsch_object_t* apply = NULL;
  dfsch_object_t* hash = NULL;
  dfsch_object_t* name;
  dfsch_object_t* type;
  char* typename;

  DFSCH_OBJECT_ARG(args, name);
  args = dfsch_eval_list(args, env);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("write", write);
  DFSCH_KEYWORD("equal?", equal_p);
  DFSCH_KEYWORD("apply", apply);
  DFSCH_KEYWORD("hash", hash);
  DFSCH_KEYWORD_PARSER_END(args);


  type = dfsch_make_wrapper_type(dfsch_symbol_2_typename(name), 
                                 write, equal_p, apply, hash);

  dfsch_define(name, type, env);
  return type;
}

DFSCH_DEFINE_PRIMITIVE(make_wrapper_type, NULL){
  dfsch_object_t* write = NULL;
  dfsch_object_t* equal_p = NULL;
  dfsch_object_t* apply = NULL;
  dfsch_object_t* hash = NULL;
  char* name;

  DFSCH_STRING_ARG(args, name);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("write", write);
  DFSCH_KEYWORD("equal?", equal_p);
  DFSCH_KEYWORD("apply", apply);
  DFSCH_KEYWORD("hash", hash);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_wrapper_type(name, write, equal_p, apply, hash);
}

DFSCH_DEFINE_PRIMITIVE(wrap, NULL){
  dfsch_object_t* type;
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG_OPT(args, type, NULL);
  DFSCH_OBJECT_ARG_OPT(args, object, NULL);
  DFSCH_ARG_END(args);

  return dfsch_wrap(type, object);
}
DFSCH_DEFINE_PRIMITIVE(unwrap, NULL){
  dfsch_object_t* type;
  dfsch_object_t* wrapper;
  DFSCH_OBJECT_ARG_OPT(args, type, NULL);
  DFSCH_OBJECT_ARG_OPT(args, wrapper, NULL);
  DFSCH_ARG_END(args);

  return dfsch_unwrap(type, wrapper);
}

void dfsch__wrapper_native_register(dfsch_object_t *ctx){ 
  dfsch_define_cstr(ctx, "<wrapper-type>", &wrapper_type);

  dfsch_define_cstr(ctx, "define-wrapper-type", 
                    DFSCH_FORM_REF(define_wrapper_type));
  dfsch_define_cstr(ctx, "make-wrapper-type", 
		   DFSCH_PRIMITIVE_REF(make_wrapper_type));
  dfsch_define_cstr(ctx, "wrap", 
		   DFSCH_PRIMITIVE_REF(wrap));
  dfsch_define_cstr(ctx, "unwrap", 
		   DFSCH_PRIMITIVE_REF(unwrap));
}
