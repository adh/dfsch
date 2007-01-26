/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
 * Copyright (C) 2007 Ales Hakl
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

typedef struct instance_t {
  dfsch_type_t* klass;
  dfsch_object_t* inst_vars;  // Hashmap
} instance_t;


typedef struct class_t class_t;

struct class_t {
  dfsch_type_t type;
  class_t* superclass;
  dfsch_object_t* methods;
};

static char* class_write(class_t* klass, int depth, 
                         int readable){
  str_list_t *sl = sl_create();
  char buf[sizeof(void*)*2+4];
  
  sl_append(sl, "#<standard-class ");
  snprintf(buf, sizeof(void*)*2+4, "0x%x ", klass);
  sl_append(sl, buf);
  sl_append(sl, klass->type.name);
  sl_append(sl, ">");
  
  return sl_value(sl);
}

DFSCH_LOCAL_SYMBOL_CACHE("write", sel_write);
DFSCH_LOCAL_SYMBOL_CACHE("equal?", sel_equal_p);
DFSCH_LOCAL_SYMBOL_CACHE("init", sel_init);

static dfsch_object_t* class_apply(dfsch_object_t* object, 
                                   dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  instance_t* ins = (instance_t*)dfsch_make_object((dfsch_type_t*)object);
  
  ins->inst_vars = dfsch_hash_make(NULL, DFSCH_HASH_EQ);

  dfsch_object_send((dfsch_object_t*)ins, sel_init(), args);

  return (dfsch_object_t*)ins;
}

static const dfsch_type_t class_type = {
  DFSCH_STANDARD_TYPE,
  sizeof(class_t),
  "standard-class",
  NULL,
  (dfsch_type_write_t)class_write,
  class_apply
};

static char* instance_write(dfsch_object_t* object, int depth, 
                            int readable){
  return dfsch_string_to_cstr
    (dfsch_object_send(object, 
                       sel_write(),
                       dfsch_list(2,
                                  dfsch_make_number_from_long(depth),
                                  dfsch_bool(readable))));
}

static int instance_equal_p(dfsch_object_t* a, dfsch_object_t* b){
  return (dfsch_object_send(a,
                            sel_equal_p(),
                            dfsch_cons(b, NULL))) != NULL;  
}

DFSCH_SYMBOL_CACHE("does-not-understand", dfsch_object_does_not_understand);

static dfsch_object_t* instance_apply(dfsch_object_t* object, 
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_object_t* selector = dfsch_car(args);
  args = dfsch_cdr(args);
  class_t* klass = (class_t*) object->type;

  while (selector == dfsch_object_super()){ // 'super
    if (klass->superclass)
      klass = klass->superclass;
    selector = dfsch_car(args);
    args = dfsch_cdr(args);
  }

 retry:
  while (klass){
    dfsch_object_t* res = dfsch_hash_ref(klass->methods, selector);
    if (res){
      return dfsch_apply_tr(dfsch_car(res),
                            dfsch_cons(object,
                                       args),
                            esc);
    }
    klass = klass->superclass;
  }
  
  // Not found (root of class hierarchy must implement 'does-not-understand)

  klass = (class_t*) object->type;
  args = dfsch_cons(selector, args);
  selector = dfsch_object_does_not_understand();
  goto retry;
}



static class_t* alloc_class(class_t* superclass, char* name){
  class_t* klass = (class_t*)dfsch_make_object(&class_type);

  klass->type.size = sizeof(instance_t);
  klass->type.name = name;
  klass->type.equal_p = instance_equal_p;
  klass->type.write = instance_write;
  klass->type.apply = instance_apply;

  klass->methods = dfsch_hash_make(NULL, DFSCH_HASH_EQ);
  klass->superclass = superclass;

  return klass;
}

dfsch_object_t* dfsch_object_make_class(dfsch_object_t* superclass, 
                                          char* name){
  if (!superclass || superclass->type != &class_type)
    dfsch_throw("exception:not-a-class", superclass);    

  return (dfsch_object_t*)alloc_class((class_t*) superclass, name);
}
void dfsch_object_define_method(dfsch_object_t* klass,
                                dfsch_object_t* selector,
                                dfsch_object_t* proc){
  if (!klass || klass->type != &class_type)
    dfsch_throw("exception:not-a-class", klass);

  dfsch_hash_set(((class_t*)klass)->methods, selector, proc);
}

DFSCH_SYMBOL_CACHE("super", dfsch_object_super)

dfsch_object_t* dfsch_object_send_tr(dfsch_object_t* object,
                                     dfsch_object_t* selector,
                                     dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  return dfsch_apply_tr(object, dfsch_cons(selector, args), esc);
}
dfsch_object_t* dfsch_object_send(dfsch_object_t* object,
                                  dfsch_object_t* selector,
                                  dfsch_object_t* args){
  return dfsch_object_send_tr(object, selector, args, NULL);
}

dfsch_object_t* dfsch_object_send_super_tr(dfsch_object_t* object,
                                           dfsch_object_t* selector,
                                           dfsch_object_t* args,
                                           dfsch_tail_escape_t* esc){
  return dfsch_object_send_tr(object, dfsch_object_super(), 
                              dfsch_cons(selector, args),
                              esc);
}
dfsch_object_t* dfsch_object_send_super(dfsch_object_t* object,
                                        dfsch_object_t* selector,
                                        dfsch_object_t* args){
  return dfsch_object_send_super_tr(object, selector, args, NULL);
}

dfsch_object_t* dfsch_object_slot_set(dfsch_object_t* object,
                                      dfsch_object_t* name,
                                      dfsch_object_t* value){

  if (!object || !object->type || object->type->type != &class_type) // XXX
    dfsch_throw("exception:not-a-object", object);

  return dfsch_hash_set(((instance_t*)object)->inst_vars, name, value);
  
}
dfsch_object_t* dfsch_object_slot_ref(dfsch_object_t* object,
                                      dfsch_object_t* name){
  dfsch_object_t* ret;
  if (!object || !object->type || object->type->type != &class_type) // XXX
    dfsch_throw("exception:not-a-object", object);

  ret = dfsch_hash_ref(((instance_t*)object)->inst_vars, name);
  if (!ret){
    dfsch_throw("exception:slot-undefined", name);
  } else {
    return dfsch_car(ret);
  }
}

// <object> class

static dfsch_object_t* object_does_not_understand(void* baton,
                                                  dfsch_object_t* args,
                                                  dfsch_tail_escape_t* esc){
  dfsch_throw("exception:message-not-understood", args);
}
static dfsch_object_t* object_write(void* baton,
                                    dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* readable;
  long depth;
  str_list_t* sl;
  char buf[sizeof(void*)*2+6];

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_LONG_ARG_OPT(args, depth, 256);
  DFSCH_OBJECT_ARG_OPT(args, readable, NULL);
  DFSCH_ARG_END(args);

  sl = sl_create();
  sl_append(sl, "#<instance of ");
  sl_append(sl, object->type->name);
  snprintf(buf, sizeof(void*)*2+6, " 0x%x>", object);
  sl_append(sl, buf);

  return dfsch_make_string_cstr(sl_value(sl));
}
static dfsch_object_t* object_equal_p(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  return NULL; // eq? case handled by C code in dfsch.c
}

static dfsch_object_t* object_init(void* baton,
                                   dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  dfsch_object_t* key = NULL;
  dfsch_object_t* ins = dfsch_car(args);
  args = dfsch_cdr(args);

  if (!ins || !ins->type || ins->type->type != &class_type) // XXX
    dfsch_throw("exception:not-a-ins", ins);

  while (dfsch_pair_p(args)){
    if (key){
      dfsch_hash_set(((instance_t*)ins)->inst_vars, key, dfsch_car(args));
      key = NULL;
    }else{
      key = dfsch_car(args);
    }
    args = dfsch_cdr(args);
  }

  if (key)
    dfsch_throw("exception:value-expected", key);

  return ins;
}

static dfsch_object_t* make_object_class(){
  dfsch_object_t* klass = (dfsch_object_t*)alloc_class(NULL, "<object>");
  
  dfsch_object_define_method(klass, dfsch_object_does_not_understand(), 
                             dfsch_make_primitive(object_does_not_understand,
                                                  NULL));
  dfsch_object_define_method(klass, sel_equal_p(), 
                             dfsch_make_primitive(object_equal_p,
                                                  NULL));
  dfsch_object_define_method(klass, sel_write(), 
                             dfsch_make_primitive(object_write,
                                                  NULL));
  dfsch_object_define_method(klass, sel_init(), 
                             dfsch_make_primitive(object_init,
                                                  NULL));
  return klass;
}

DFSCH_LOCAL_SYMBOL_CACHE("delegate-to", slot_delegate_to);

static dfsch_object_t* delegator_does_not_understand(void* baton,
                                                     dfsch_object_t* args,
                                                     dfsch_tail_escape_t* esc){
  dfsch_object_t* object = dfsch_car(args);
  args = dfsch_cdr(args); 

  return dfsch_apply_tr(dfsch_object_slot_ref(object, slot_delegate_to()),
                        args,
                        esc);
}
static dfsch_object_t* delegator_init(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* delegate_to;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, delegate_to);
  DFSCH_ARG_END(args);
  
  dfsch_object_slot_set(object, slot_delegate_to(), delegate_to);

  return object;
}

static dfsch_object_t* make_delegator_class(dfsch_object_t* superclass){
  dfsch_object_t* klass = dfsch_object_make_class(superclass, "<delegator>");

  dfsch_object_define_method(klass, dfsch_object_does_not_understand(), 
                             dfsch_make_primitive(delegator_does_not_understand,
                                           NULL));
  dfsch_object_define_method(klass, sel_init(), 
                             dfsch_make_primitive(delegator_init,
                                                  NULL));
  return klass;
}

// Scheme binding

static dfsch_object_t* native_make_class(void* baton,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* superclass;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, superclass);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  return dfsch_object_make_class(superclass, dfsch_symbol(name));
}

static dfsch_object_t* native_form_define_class(void* baton,
                                                dfsch_object_t* args,
                                                dfsch_tail_escape_t* esc){
  dfsch_object_t* env;
  dfsch_object_t* name;
  dfsch_object_t* superclass;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, superclass);
  DFSCH_ARG_END(args);

  superclass = dfsch_eval(superclass, env);
  
  return dfsch_define(name, 
                      dfsch_object_make_class(superclass, 
                                              dfsch_symbol(name)),
                      env);
}

static dfsch_object_t* native_form_define_method(void* baton,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* env;
  dfsch_object_t* klass;
  dfsch_object_t* selector;
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, klass);
  DFSCH_OBJECT_ARG(args, selector);
  klass = dfsch_eval(klass, env);

  if (dfsch_pair_p(selector)){
    proc = dfsch_lambda(env, dfsch_cdr(selector), args);
    selector = dfsch_car(selector);
  } else {
    DFSCH_OBJECT_ARG(args, proc);
    DFSCH_ARG_END(args);
  }
  
  dfsch_object_define_method(klass, selector, proc);

  return proc;
}
static dfsch_object_t* native_slot_set(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* name;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);
  
  return dfsch_object_slot_set(object, name, value);
}
static dfsch_object_t* native_slot_ref(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  dfsch_object_t* name;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_ARG_END(args);
  
  return dfsch_object_slot_ref(object, name);
}

void dfsch__object_native_register(dfsch_object_t *ctx){
  dfsch_object_t* object_class = make_object_class();
  dfsch_define_cstr(ctx, "<object>", object_class);
  dfsch_define_cstr(ctx, "<delegator>", make_delegator_class(object_class));

  dfsch_define_cstr(ctx, "make-class",
                    dfsch_make_primitive(native_make_class,
                                         NULL));
  dfsch_define_cstr(ctx, "define-class",
                    dfsch_make_form(dfsch_make_primitive(native_form_define_class,
                                                         NULL)));
  dfsch_define_cstr(ctx, "define-method",
                    dfsch_make_form(dfsch_make_primitive(native_form_define_method,
                                                         NULL)));
  dfsch_define_cstr(ctx, "slot-set!",
                    dfsch_make_primitive(native_slot_set,
                                         NULL));
  dfsch_define_cstr(ctx, "slot-ref",
                    dfsch_make_primitive(native_slot_ref,
                                         NULL));
  
}
