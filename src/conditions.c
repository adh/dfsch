#include "dfsch/conditions.h"
#include "dfsch/magic.h"
#include <stdio.h>
#include "util.h"

dfsch_object_t* dfsch_make_condition(dfsch_type_t* type){
  dfsch__condition_t* c = (dfsch__condition_t*)dfsch_make_object(type);
  c->fields = NULL;
  return (dfsch_object_t*)c;
}

char* dfsch__condition_write(dfsch__condition_t* c, int depth, int readable){
  str_list_t* sl = sl_create();
  dfsch_object_t* i = c->fields;
  dfsch_object_t* j;
  
  sl_append(sl, saprintf("#<%s %p", c->type->name, c));
  
  while (DFSCH_PAIR_P(i)){
    j = DFSCH_FAST_CAR(i);
    while (DFSCH_PAIR_P(j)){
      sl_append(sl, " ");
      sl_append(sl, dfsch_obj_write(DFSCH_FAST_CAR(j), depth-1, 1));
      j = DFSCH_FAST_CDR(j);
    }
    i = DFSCH_FAST_CDR(i);
  }

  sl_append(sl, ">");
  
  return sl_value(sl);
}


dfsch_object_t* dfsch_condition_field(dfsch_object_t* condition,
                                      dfsch_object_t* name){
  dfsch_object_t* al;
  if (!DFSCH_INSTANCE_P(condition, DFSCH_CONDITION_TYPE)){
    dfsch_error("exception:not-a-condition", condition);
  }
  al = dfsch_assq(name, ((dfsch__condition_t*)condition)->fields);
  if (!al){
    return NULL;
  } else {
    return dfsch_car(dfsch_cdr(al));
  }
}
void dfsch_condition_put_field(dfsch_object_t* condition,
                               dfsch_object_t* name,
                               dfsch_object_t* value){
  if (!DFSCH_INSTANCE_P(condition, DFSCH_CONDITION_TYPE)){
    dfsch_error("exception:not-a-condition", condition);
  }

  ((dfsch__condition_t*)condition)->fields = 
    dfsch_cons(dfsch_list(2, name, value), 
               ((dfsch__condition_t*)condition)->fields);
}
dfsch_object_t* dfsch_condition_field_cstr(dfsch_object_t* condition,
                                           char* name){
  return dfsch_condition_field(condition, dfsch_make_symbol(name));
}
void dfsch_condition_put_field_cstr(dfsch_object_t* condition,
                                    char* name,
                                    dfsch_object_t* value){
  return dfsch_condition_put_field(condition, dfsch_make_symbol(name), value);
}
dfsch_object_t* dfsch_condition_fields(dfsch_object_t* condition){
  if (!DFSCH_INSTANCE_P(condition, DFSCH_CONDITION_TYPE)){
    dfsch_error("exception:not-a-condition", condition);
  }
  return ((dfsch__condition_t*)condition)->fields;;
}

dfsch_object_t* dfsch_condition(dfsch_type_t* type, ...){
  va_list al;
  dfsch_object_t* c = dfsch_make_condition(type);
  char* name;

  dfsch_condition_put_field_cstr(c, "stack-trace", dfsch_get_stack_trace());

  va_start(al, type);
  while (name = va_arg(al, char*)){
    dfsch_condition_put_field_cstr(c, name, va_arg(al, dfsch_object_t*));
  }
  va_end(al);

  return c;
}

dfsch_type_t dfsch_condition_type = 
  DFSCH_CONDITION_TYPE_INIT(NULL, "condition");

dfsch_type_t dfsch_warning_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_CONDITION_TYPE, "warning");

dfsch_type_t dfsch_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_CONDITION_TYPE, "error");

dfsch_type_t dfsch_runtime_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_ERROR_TYPE, "runtime-error");

void dfsch_signal(dfsch_object_t* condition){
  dfsch__handler_list_t* save;
  dfsch__handler_list_t* i;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();

  i = save = ti->handler_list;

  while (i){
    if (DFSCH_INSTANCE_P(condition, i->type)){
      ti->handler_list = i->next;
      dfsch_apply(i->handler, dfsch_cons(condition, NULL));
    }
    i = i->next;
  }


  if (DFSCH_INSTANCE_P(condition, DFSCH_ERROR_TYPE)){
    fputs("Unhandled error condition!\n\n", stderr);
    fprintf(stderr, "%s\n", dfsch_obj_write(condition, 10, 1));
    abort();
  }

  ti->handler_list = save;
}

typedef struct restart_t {
  dfsch_type_t* type;
  dfsch_object_t* name;
  dfsch_object_t* proc;
  char* description;
  dfsch__handler_list_t* handlers;
} restart_t;

dfsch_type_t dfsch_restart_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(restart_t),
  "restart"
};
dfsch_object_t* dfsch_make_restart(dfsch_object_t* name,
                                   dfsch_object_t* proc,
                                   char* description){
  restart_t* r = (restart_t*) dfsch_make_object(DFSCH_RESTART_TYPE);

  r->name = name;
  r->proc = proc;
  r->description = description;

  return (dfsch_object_t*) r;
}

void dfsch_restart_bind(dfsch_object_t* restart){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch__restart_list_t* l = GC_NEW(dfsch__restart_list_t);

  l->next = ti->restart_list;
  l->restart = restart;

  ti->restart_list = l;
}
void dfsch_handler_bind(dfsch_type_t* type,
                        dfsch_object_t* handler){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch__handler_list_t* l = GC_NEW(dfsch__handler_list_t);
  
  l->next = ti->handler_list;
  l->type = type;
  l->handler = handler;

  ti->handler_list = l;
}
