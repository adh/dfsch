/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
 * Copyright (C) 2007, 2008 Ales Hakl
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, 
 * USA
 */

#include "dfsch/object.h"

#include <dfsch/hash.h>
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include "util.h"


typedef struct class_t {
  dfsch_type_t standard_type;
  dfsch_object_t* methods;
  dfsch_object_t* send_cache;
} class_t;

static dfsch_slot_t class_slots[] = {
  DFSCH_OBJECT_SLOT(class_t, methods, DFSCH_SLOT_ACCESS_RW),
  DFSCH_OBJECT_SLOT(class_t, send_cache, DFSCH_SLOT_ACCESS_RW),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_class_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(class_t),
  "class",

  NULL,
  NULL,
  NULL,
  NULL,

  &class_slots
};

static dfsch_slot_t* make_slots(dfsch_object_t* slot_desc){
  dfsch_object_t* i = slot_desc;
  size_t slot_count = dfsch_list_length(slot_desc);
  dfsch_slot_t* slots = GC_MALLOC((slot_count + 1) * sizeof(dfsch_slot_t));
  dfsch_slot_t* j = slots;

  while (slot_count && DFSCH_PAIR_P(i)){
    dfsch_object_t* name;
    dfsch_object_t* type;
    if (DFSCH_PAIR_P(DFSCH_FAST_CAR(i))){
      dfsch_object_t* args = DFSCH_FAST_CAR(i);
      DFSCH_OBJECT_ARG(args, name);
    } else {
      name = DFSCH_FAST_CAR(i);
    }

    j->type = DFSCH_OBJECT_SLOT_TYPE;
    j->name = dfsch_symbol(name);

    j++;
    slot_count--;
    i = DFSCH_FAST_CDR(i);
  }

  j->type = NULL;
  j->name = NULL;

  return slots;
}

static size_t adjust_sizes(dfsch_slot_t* slots, size_t parent_size){
  if (parent_size < sizeof(dfsch_object_t)){
    parent_size = sizeof(dfsch_object_t);
  }
  while (slots->type){
    slots->offset = parent_size;
    parent_size += sizeof(dfsch_object_t*);
    slots++;
  }
  return parent_size;
}

static char* class_write(){

}
static int class_equal_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_send(a, dfsch_s_equal_instance_p(), dfsch_cons(b, NULL)) != NULL;
}


static dfsch_object_t* class_apply;

dfsch_object_t* dfsch_make_class(dfsch_object_t* superclass,
                                 char* name,
                                 dfsch_object_t* slots){
  class_t* klass = dfsch_make_object(DFSCH_CLASS_TYPE);

  klass->standard_type.superclass = superclass;
  klass->standard_type.name = name;
  klass->standard_type.slots = make_slots(slots);
  if (superclass){
    if (!DFSCH_INSTANCE_P(superclass, DFSCH_STANDARD_TYPE)){
      dfsch_error("Not a type", superclass);
    }
    
    if (((dfsch_type_t*)superclass)->size == 0 && 
        !DFSCH_INSTANCE_P(superclass, DFSCH_ABSTRACT_TYPE)){
      dfsch_error("Cannot inherit from special type", superclass);
    }

    klass->standard_type.size = adjust_sizes(klass->standard_type.slots,
                                             ((dfsch_type_t*)
                                              superclass)->size);
  } else {
    klass->standard_type.size = adjust_sizes(klass->standard_type.slots,
                                             sizeof(dfsch_object_t));
  }
  /* klass->standard_type.equal_p = class_equal_p;
  klass->standard_type.write = class_write;
  klass->standard_type.apply = class_apply;
  klass->standard_type.hash = class_hash;*/

  klass->methods = dfsch_hash_make(DFSCH_HASH_EQ);
  klass->send_cache = dfsch_hash_make(DFSCH_HASH_EQ);

  return (dfsch_type_t*)klass;
}

dfsch_object_t* dfsch_make_instance(dfsch_object_t* klass,
                                    dfsch_object_t* args){
  dfsch_object_t* obj;

  if (!DFSCH_INSTANCE_P(klass, DFSCH_CLASS_TYPE)){
    dfsch_error("Not a class", klass);
  }
  obj = dfsch_make_object(klass);
  dfsch_send(obj, dfsch_s_initialize_instance(), args);
  return obj;
}

dfsch_object_t* dfsch_find_method(dfsch_object_t* klass, 
                                  dfsch_object_t* selector){
  dfsch_object_t* method;
  class_t* c;
  if (!DFSCH_INSTANCE_P(klass, DFSCH_CLASS_TYPE)){
    dfsch_error("Not a class", klass);
  }
  c = (class_t*)klass;

  if (dfsch_hash_ref_fast(c->send_cache, selector, &method)){
    return method;
  }
  
  while(DFSCH_INSTANCE_P(c, DFSCH_CLASS_TYPE)){
    if (dfsch_hash_ref_fast(c->methods, selector, &method)){
      dfsch_hash_set(((class_t*)klass)->send_cache, selector, method);
      return method;
    }
    
    c = (class_t*) c->standard_type.superclass;
  }

  return NULL;
}

void dfsch_class_add_method(dfsch_object_t* klass, 
                            dfsch_object_t* selector,
                            dfsch_object_t* method){
  class_t* c;
  if (!DFSCH_INSTANCE_P(klass, DFSCH_CLASS_TYPE)){
    dfsch_error("Not a class", klass);
  }
  c = (class_t*)klass;

  dfsch_hash_set(c->methods, selector, method);
  dfsch_hash_set(c->send_cache, selector, method);
}
void dfsch_class_remove_method(dfsch_object_t* klass, 
                               dfsch_object_t* selector){
  class_t* c;
  if (!DFSCH_INSTANCE_P(klass, DFSCH_CLASS_TYPE)){
    dfsch_error("Not a class", klass);
  }
  c = (class_t*)klass;

  dfsch_hash_unset(c->methods, selector);
  dfsch_hash_unset(c->send_cache, selector);
}

int dfsch_responds_to_p(dfsch_object_t* klass, 
                        dfsch_object_t* selector){
  return dfsch_find_method(klass, selector) != NULL;
}

dfsch_object_t* dfsch_perform_tr(dfsch_object_t* klass,
                                 dfsch_object_t* selector,
                                 dfsch_object_t* args,
                                 dfsch_tail_escape_t* esc){
  dfsch_object_t* method = dfsch_find_method(klass, selector);
  if (!method){
    dfsch_error("Message not understood", dfsch_list(2, klass, selector));
  }
  return dfsch_apply_tr(method, args, esc);
}
dfsch_object_t* dfsch_perform(dfsch_object_t* klass,
                              dfsch_object_t* selector,
                              dfsch_object_t* args){
  return dfsch_perform_tr(klass, selector, args, NULL);
}

dfsch_object_t* dfsch_send_tr(dfsch_object_t* obj, 
                              dfsch_object_t* selector,
                              dfsch_object_t* args,
                              dfsch_tail_escape_t* esc){
  return dfsch_perform_tr(DFSCH_TYPE_OF(obj), 
                          selector, 
                          dfsch_cons(obj, args), 
                          esc);
}
dfsch_object_t* dfsch_send(dfsch_object_t* obj, 
                           dfsch_object_t* selector,
                           dfsch_object_t* args){
  return dfsch_perform_tr(DFSCH_TYPE_OF(obj), 
                          selector, 
                          dfsch_cons(obj, args), 
                          NULL);
}

DFSCH_SYMBOL_CACHE("initialize-instance", dfsch_s_initialize_instance);
DFSCH_SYMBOL_CACHE("write-instance", dfsch_s_write_instance);
DFSCH_SYMBOL_CACHE("equal-instance?", dfsch_s_equal_instance_p);
DFSCH_SYMBOL_CACHE("apply-instance", dfsch_s_apply_instance);
DFSCH_SYMBOL_CACHE("instance-hash", dfsch_s_instance_hash);

DFSCH_DEFINE_PRIMITIVE(send, 0){
  dfsch_object_t* object;
  dfsch_object_t* selector;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, selector);
  return dfsch_send_tr(object, selector, args, esc);
}
DFSCH_DEFINE_PRIMITIVE(perform, 0){
  dfsch_object_t* klass;
  dfsch_object_t* selector;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, selector);
  return dfsch_perform_tr(klass, selector, args, esc);
}
DFSCH_DEFINE_PRIMITIVE(find_method, 0){
  dfsch_object_t* klass;
  dfsch_object_t* selector;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_ARG_END(args);
  return dfsch_find_method(klass, selector);  
}
DFSCH_DEFINE_PRIMITIVE(responds_to_p, 0){
  dfsch_object_t* klass;
  dfsch_object_t* selector;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_responds_to_p(klass, selector));  
}
DFSCH_DEFINE_PRIMITIVE(class_add_method, 0){
  dfsch_object_t* klass;
  dfsch_object_t* selector;
  dfsch_object_t* method;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_OBJECT_ARG(args, method);
  DFSCH_ARG_END(args);
  dfsch_class_add_method(klass, selector, method);  
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(class_remove_method, 0){
  dfsch_object_t* klass;
  dfsch_object_t* selector;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_ARG_END(args);
  dfsch_class_remove_method(klass, selector);  
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(make_instance, 0){
  dfsch_object_t* klass;
  DFSCH_OBJECT_ARG(args, klass);

  return dfsch_make_instance(klass, args);
}

DFSCH_DEFINE_FORM_IMPL(define_class){
  dfsch_object_t* name;
  dfsch_object_t* superclass;
  dfsch_object_t* slots;
  dfsch_object_t* klass;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, superclass);
  DFSCH_OBJECT_ARG(args, slots);
  DFSCH_ARG_END(args);

  superclass= dfsch_eval(superclass, env);
  klass = dfsch_make_class(superclass, 
                           dfsch_symbol_2_typename(name),
                           slots);

  dfsch_define(name, klass, env);
  return klass;
}
DFSCH_DEFINE_FORM_IMPL(define_method){
  dfsch_object_t* klass;
  dfsch_object_t* lambda_list;
  dfsch_object_t* selector;
  dfsch_object_t* code;
  dfsch_object_t* method;
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, lambda_list);
  DFSCH_ARG_REST(args, code);
  DFSCH_OBJECT_ARG(lambda_list, selector);

  klass = dfsch_eval(klass, env);
  method = dfsch_named_lambda(env, lambda_list, code,
                              dfsch_list(2, klass, selector));
  dfsch_class_add_method(klass, selector, method);
  return method;
}


void dfsch__object_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<class>", DFSCH_CLASS_TYPE);
  dfsch_define_cstr(ctx, "send", DFSCH_PRIMITIVE_REF(send));
  dfsch_define_cstr(ctx, "perform", DFSCH_PRIMITIVE_REF(perform));
  dfsch_define_cstr(ctx, "responds-to?", DFSCH_PRIMITIVE_REF(responds_to_p));
  dfsch_define_cstr(ctx, "find-method", DFSCH_PRIMITIVE_REF(find_method));
  dfsch_define_cstr(ctx, "class-add-method!", 
                    DFSCH_PRIMITIVE_REF(class_add_method));
  dfsch_define_cstr(ctx, "class-remove-method!", 
                    DFSCH_PRIMITIVE_REF(class_remove_method));
  dfsch_define_cstr(ctx, "make-instance", DFSCH_PRIMITIVE_REF(make_instance));

  dfsch_define_cstr(ctx, "define-class", DFSCH_FORM_REF(define_class));
  dfsch_define_cstr(ctx, "define-method", DFSCH_FORM_REF(define_method));
}
