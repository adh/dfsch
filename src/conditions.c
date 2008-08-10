#include "dfsch/conditions.h"
#include "dfsch/magic.h"
#include <stdio.h>
#include "util.h"

dfsch_object_t* dfsch_make_condition(dfsch_type_t* type){
  dfsch__condition_t* c;
  if (!DFSCH_INSTANCE_P(type, DFSCH_STANDARD_TYPE) ||
      !dfsch_superclass_p(type, DFSCH_CONDITION_TYPE)){
    dfsch_error("Not a condition type", (dfsch_object_t*)type);
  }

  c = (dfsch__condition_t*)dfsch_make_object(type);
  c->fields = NULL;
  dfsch_condition_put_field_cstr(c, "stack-trace", dfsch_get_stack_trace());
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
dfsch_object_t* dfsch_restart_name(dfsch_object_t* restart){
  if (DFSCH_TYPE_OF(restart) != DFSCH_RESTART_TYPE){
    dfsch_error("Not a restart", restart);
  }
  return ((restart_t*)restart)->name;
}
dfsch_object_t* dfsch_restart_proc(dfsch_object_t* restart){
  if (DFSCH_TYPE_OF(restart) != DFSCH_RESTART_TYPE){
    dfsch_error("Not a restart", restart);
  }
  return ((restart_t*)restart)->proc;
}
char* dfsch_restart_description(dfsch_object_t* restart){
  if (DFSCH_TYPE_OF(restart) != DFSCH_RESTART_TYPE){
    dfsch_error("Not a restart", restart);
  }
  return ((restart_t*)restart)->description;
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

dfsch_object_t* dfsch_compute_restarts(){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch__restart_list_t* i = ti->restart_list;
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;

  while (i){
    dfsch_object_t* tmp = dfsch_cons(i->restart, NULL); 
    if (head){
      DFSCH_FAST_CDR(tail) = tmp;
      tail = tmp;
    } else {
      head = tail = tmp;
    }
    i = i->next;
  }

  return head;
}

dfsch_object_t* dfsch_invoke_restart(dfsch_object_t* restart){
  if (DFSCH_TYPE_OF(restart) != DFSCH_RESTART_TYPE){
    dfsch__thread_info_t* ti = dfsch__get_thread_info();
    dfsch__restart_list_t* i = ti->restart_list;
    while (i){
      if (dfsch_restart_name(i->restart) == restart){
        break;
      }
      i = i->next;
    }
    if (!i){
      dfsch_error("No such restart", restart);
    }
    restart = i->restart;
  }

  return dfsch_apply(dfsch_restart_proc(restart), NULL);
}


static dfsch_object_t* throw_proc(dfsch_object_t* tag,
                                  dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  dfsch_throw(tag, args);
}

dfsch_object_t* dfsch_make_throw_proc(dfsch_object_t* catch_tag){
  return dfsch_make_primitive(throw_proc, catch_tag);
}


/*
 * Scheme binding
 */

DFSCH_DEFINE_PRIMITIVE(make_condition, 0){
  dfsch_object_t* type;
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_ARG_END(args);
  
  return dfsch_make_condition(type);
}
DFSCH_DEFINE_PRIMITIVE(condition_field, 0){
  dfsch_object_t* condition;
  dfsch_object_t* field;
  DFSCH_OBJECT_ARG(args, condition);
  DFSCH_OBJECT_ARG(args, field);
  DFSCH_ARG_END(args);

  return dfsch_condition_field(condition, field);
}
DFSCH_DEFINE_PRIMITIVE(condition_put_field, 0){
  dfsch_object_t* condition;
  dfsch_object_t* field;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, condition);
  DFSCH_OBJECT_ARG(args, field);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);

  dfsch_condition_put_field(condition, field, value);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(condition_fields, 0){
  dfsch_object_t* condition;
  DFSCH_OBJECT_ARG(args, condition);
  DFSCH_ARG_END(args);

  return dfsch_condition_fields(condition);
}



DFSCH_DEFINE_PRIMITIVE(signal, 0){
  dfsch_object_t* condition;
  DFSCH_OBJECT_ARG(args, condition);
  DFSCH_ARG_END(args);

  dfsch_signal(condition);
  return NULL;
}

void dfsch__conditions_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "<condition>", DFSCH_CONDITION_TYPE);
  dfsch_define_cstr(ctx, "<warning>", DFSCH_WARNING_TYPE);
  dfsch_define_cstr(ctx, "<error>", DFSCH_ERROR_TYPE);
  dfsch_define_cstr(ctx, "<runtime-error>", DFSCH_RUNTIME_ERROR_TYPE);
  
  dfsch_define_cstr(ctx, "make-condition", 
                    DFSCH_PRIMITIVE_REF(make_condition));
  dfsch_define_cstr(ctx, "condition-field", 
                    DFSCH_PRIMITIVE_REF(condition_field));
  dfsch_define_cstr(ctx, "condition-put-field", 
                    DFSCH_PRIMITIVE_REF(condition_put_field));
  dfsch_define_cstr(ctx, "condition-fields", 
                    DFSCH_PRIMITIVE_REF(condition_fields));


  dfsch_define_cstr(ctx, "signal",
                    DFSCH_PRIMITIVE_REF(signal));
}
