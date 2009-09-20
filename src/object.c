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
} class_t;


dfsch_type_t dfsch_class_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(class_t),
  "class",

  NULL,
  NULL,
  NULL,
  NULL,

  NULL,
  "Metaclass for user-defined classes"
};

static dfsch_slot_t* make_slots(dfsch_object_t* slot_desc){
  dfsch_object_t* i = slot_desc;
  size_t slot_count = dfsch_list_length_check(slot_desc);
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
    j->documentation = NULL;

    j++;
    slot_count--;
    i = DFSCH_FAST_CDR(i);
  }

  j->type = NULL;
  j->name = NULL;
  j->access = DFSCH_SLOT_ACCESS_RW;
  j->documentation = NULL;

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
  class_t* klass = (class_t*)dfsch_make_object(DFSCH_CLASS_TYPE);

  klass->standard_type.name = name;
  klass->standard_type.slots = make_slots(slots);
  klass->standard_type.flags = DFSCH_TYPEF_USER_EXTENSIBLE;
  if (superclass){
    klass->standard_type.superclass = 
      (dfsch_type_t*)DFSCH_ASSERT_INSTANCE(superclass,
                                           DFSCH_CLASS_TYPE);

    klass->standard_type.size = 
      adjust_sizes(klass->standard_type.slots,
                   klass->standard_type.superclass->size);
  } else {
    klass->standard_type.superclass = NULL;
    klass->standard_type.size = adjust_sizes(klass->standard_type.slots,
                                             sizeof(dfsch_object_t));
  }
  /* klass->standard_type.equal_p = class_equal_p;
  klass->standard_type.write = class_write;
  klass->standard_type.apply = class_apply;
  klass->standard_type.hash = class_hash;*/

  return (dfsch_object_t*)klass;
}

dfsch_object_t* dfsch_make_instance(dfsch_object_t* klass,
                                    dfsch_object_t* args){
  dfsch_object_t* obj;

  obj = 
    dfsch_make_object((dfsch_type_t*)DFSCH_ASSERT_INSTANCE(klass, 
                                                           DFSCH_CLASS_TYPE));
  return obj;
}

DFSCH_DEFINE_PRIMITIVE(make_instance, 0){
  dfsch_object_t* klass;
  DFSCH_OBJECT_ARG(args, klass);

  return dfsch_make_instance(klass, args);
}

DFSCH_DEFINE_FORM_IMPL(define_class, NULL){
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

  dfsch_define(name, klass, env, 0);
  return klass;
}


void dfsch__object_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<class>", DFSCH_CLASS_TYPE);
  dfsch_define_cstr(ctx, "make-instance", DFSCH_PRIMITIVE_REF(make_instance));

  dfsch_define_cstr(ctx, "define-class", DFSCH_FORM_REF(define_class));
}
