/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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

#include "dfsch/conditions.h"
#include "dfsch/magic.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "util.h"

dfsch_object_t* dfsch_make_condition(dfsch_type_t* type){
  dfsch__condition_t* c;
  if (!DFSCH_INSTANCE_P((dfsch_object_t*)type, DFSCH_STANDARD_TYPE) ||
      !dfsch_superclass_p(type, DFSCH_CONDITION_TYPE)){
    dfsch_error("Not a condition type", (dfsch_object_t*)type);
  }

  c = (dfsch__condition_t*)dfsch_make_object(type);
  c->fields = NULL;

  return (dfsch_object_t*)c;
}

static void condition_write(dfsch__condition_t* c, 
                            dfsch_writer_state_t* state){
  dfsch_object_t* i = c->fields;
  dfsch_object_t* j;
  
  dfsch_write_unreadable_start(state, (dfsch_object_t*)c);
  
  while (DFSCH_PAIR_P(i)){
    j = DFSCH_FAST_CAR(i);
    i = DFSCH_FAST_CDR(i);
    if (!DFSCH_PAIR_P(j)){
      dfsch_write_string(state, "*malformed-fileds*");
      dfsch_write_unreadable_end(state);
      return;
    }
    if (DFSCH_TYPE_OF(DFSCH_FAST_CAR(j)) == DFSCH_SYMBOL_TYPE &&
        dfsch_compare_keyword(DFSCH_FAST_CAR(j), "stack-trace")){
      continue;
    }
    dfsch_write_object(state, DFSCH_FAST_CAR(j)); 
    dfsch_write_string(state, " "); 
    dfsch_write_object(state, DFSCH_FAST_CDR(j)); 
    if (DFSCH_PAIR_P(i)){
      dfsch_write_string(state, " ");
    }
  }

  dfsch_write_unreadable_end(state);
}

dfsch_object_t* dfsch_condition_field(dfsch_object_t* condition,
                                      dfsch_object_t* name){
  dfsch_object_t* al;
  dfsch__condition_t* c = 
    (dfsch__condition_t*)dfsch_assert_instance(condition, 
                                               DFSCH_CONDITION_TYPE);
  al = dfsch_assq(name, c->fields);
  if (!al){
    return NULL;
  } else {
    return dfsch_cdr(al);
  }
}
void dfsch_condition_put_field(dfsch_object_t* condition,
                               dfsch_object_t* name,
                               dfsch_object_t* value){
  dfsch__condition_t* c = 
    (dfsch__condition_t*)dfsch_assert_instance(condition, 
                                               DFSCH_CONDITION_TYPE);

  c->fields = dfsch_cons(dfsch_cons(name, value), c->fields);
}
dfsch_object_t* dfsch_condition_field_cstr(dfsch_object_t* condition,
                                           char* name){
  return dfsch_condition_field(condition, dfsch_make_keyword(name));
}
void dfsch_condition_put_field_cstr(dfsch_object_t* condition,
                                    char* name,
                                    dfsch_object_t* value){
  return dfsch_condition_put_field(condition, dfsch_make_keyword(name), value);
}
dfsch_object_t* dfsch_condition_fields(dfsch_object_t* condition){
  return ((dfsch__condition_t*)
          dfsch_assert_instance(condition, 
                                DFSCH_CONDITION_TYPE))->fields;
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
void dfsch_signal_condition(dfsch_type_t* type, 
                            char* message,
                            ...){
  va_list al;
  dfsch_object_t* c = dfsch_make_condition(type);
  char* name;

  va_start(al, message);
  while (name = va_arg(al, char*)){
    dfsch_condition_put_field_cstr(c, name, va_arg(al, dfsch_object_t*));
  }
  va_end(al);
  dfsch_condition_put_field_cstr(c, "message", 
                                 dfsch_make_string_cstr(message));

  dfsch_signal(c);
}


dfsch_object_t* dfsch_condition_with_fields(dfsch_type_t* type,
                                            dfsch_object_t* message,
                                            dfsch_object_t* fields){
  dfsch_object_t* c = dfsch_make_condition(type);
  dfsch_object_t* name;
  dfsch_object_t* value;
  
  dfsch_condition_put_field_cstr(c, "message", message);
  while (DFSCH_PAIR_P(fields)){
    name = DFSCH_FAST_CAR(fields);
    fields = DFSCH_FAST_CDR(fields);
    if (!DFSCH_PAIR_P(fields)){
      dfsch_error("Missing value for keyword", name);
    }
    value = DFSCH_FAST_CAR(fields);
    fields = DFSCH_FAST_CDR(fields);
    dfsch_condition_put_field(c, name, value);
  }

  return c;
}

dfsch_type_t dfsch_condition_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  DFSCH_CONDITION_SIZE,
  "condition",
  NULL,
  (dfsch_type_write_t)condition_write
};

dfsch_type_t dfsch_warning_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_CONDITION_TYPE, "warning");

dfsch_type_t dfsch_style_warning_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_CONDITION_TYPE, "style-warning");

dfsch_type_t dfsch_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_CONDITION_TYPE, "error");

dfsch_type_t dfsch_type_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_ERROR_TYPE, "type-error");

dfsch_type_t dfsch_runtime_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_ERROR_TYPE, "runtime-error");

static int invoke_debugger_on_all_conditions = 0;

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
    dfsch_enter_debugger(condition);
    fputs("Unhandled error condition!\n\n", stderr);
    fprintf(stderr, "%s\n", dfsch_object_2_string(condition, 10, 1));
    dfsch_print_trace_buffer();
    abort();
  } else if (invoke_debugger_on_all_conditions){
    dfsch_enter_debugger(condition);    
  }

  ti->handler_list = save;
}



static dfsch_object_t* debugger_proc = NULL;
static int max_debugger_recursion = 10;

void dfsch_set_debugger(dfsch_object_t* proc){
  debugger_proc = proc;
}
void dfsch_set_invoke_debugger_on_all_conditions(int val){
  invoke_debugger_on_all_conditions = val;
}

static int debugger_depth = 0;
static pthread_mutex_t debugger_depth_mutex= PTHREAD_MUTEX_INITIALIZER;

void dfsch_enter_debugger(dfsch_object_t* reason){
  pthread_mutex_lock(&debugger_depth_mutex);
  if (debugger_depth > max_debugger_recursion){
    pthread_mutex_unlock(&debugger_depth_mutex);
    fputs("Debugger recursion limit exceeded!\n\n", stderr);
    return;
  }
  debugger_depth++;
  pthread_mutex_unlock(&debugger_depth_mutex);

  DFSCH_UNWIND {
    if (debugger_proc){
      dfsch_apply(debugger_proc, dfsch_cons(reason, NULL));
    }
  } DFSCH_PROTECT {
    pthread_mutex_lock(&debugger_depth_mutex);
    debugger_depth--;
    pthread_mutex_unlock(&debugger_depth_mutex);
  } DFSCH_PROTECT_END;
}

int dfsch_get_debugger_depth(){
  int r;
  pthread_mutex_lock(&debugger_depth_mutex);
  r = debugger_depth;
  pthread_mutex_unlock(&debugger_depth_mutex);
  return r;
}

typedef struct restart_t {
  dfsch_type_t* type;
  dfsch_object_t* name;
  dfsch_object_t* proc;
  char* description;
  char* arg_descriptions;
} restart_t;

static void restart_write(restart_t* r, 
                            dfsch_writer_state_t* state){
  dfsch_write_unreadable_start(state, (dfsch_object_t*)r);
  
  dfsch_write_object(state, r->name); 
  dfsch_write_string(state, ": "); 
  dfsch_write_string(state, r->description);
  dfsch_write_unreadable_end(state);  
}

dfsch_type_t dfsch_restart_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(restart_t),
  "restart",

  .write = (dfsch_type_write_t) restart_write,
};


dfsch_object_t* dfsch_make_restart(dfsch_object_t* name,
                                   dfsch_object_t* proc,
                                   char* description,
                                   char* arg_descriptions){
  restart_t* r = (restart_t*) dfsch_make_object(DFSCH_RESTART_TYPE);

  r->name = name;
  r->proc = proc;
  r->description = description;
  r->arg_descriptions = arg_descriptions;

  return (dfsch_object_t*) r;
}
dfsch_object_t* dfsch_restart_name(dfsch_object_t* restart){
  return ((restart_t*)dfsch_assert_type(restart, DFSCH_RESTART_TYPE))->name;
}
dfsch_object_t* dfsch_restart_proc(dfsch_object_t* restart){
  return ((restart_t*)dfsch_assert_type(restart, DFSCH_RESTART_TYPE))->proc;
}
char* dfsch_restart_description(dfsch_object_t* restart){
  return ((restart_t*)dfsch_assert_type(restart, DFSCH_RESTART_TYPE))->description;
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
      DFSCH_FAST_CDR_MUT(tail) = tmp;
      tail = tmp;
    } else {
      head = tail = tmp;
    }
    i = i->next;
  }

  return head;
}

dfsch_object_t* dfsch_invoke_restart(dfsch_object_t* restart, 
                                     dfsch_object_t* args){
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

  return dfsch_apply(dfsch_restart_proc(restart), args);
}


static dfsch_object_t* throw_proc(dfsch_object_t* tag,
                                  dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);
  dfsch_throw(tag, NULL);
}

dfsch_object_t* dfsch_make_throw_proc(dfsch_object_t* catch_tag){
  return dfsch_make_primitive(throw_proc, catch_tag);
}
static dfsch_object_t* throw_proc_arg(dfsch_object_t* tag,
                                  dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  dfsch_object_t* arg;
  DFSCH_OBJECT_ARG(args, arg);
  DFSCH_ARG_END(args);
  dfsch_throw(tag, arg);
}

dfsch_object_t* dfsch_make_throw_proc_arg(dfsch_object_t* catch_tag){
  return dfsch_make_primitive(throw_proc_arg, catch_tag);
}

void dfsch_type_error(dfsch_object_t* datum, dfsch_type_t* type, 
                      int instance_suffices){
  char* m;
  dfsch_object_t* c = dfsch_make_condition(DFSCH_TYPE_ERROR_TYPE);
  dfsch_condition_put_field_cstr(c, "datum", datum);
  dfsch_condition_put_field_cstr(c, "type", type);
  dfsch_condition_put_field_cstr(c, "instance-suffices?", 
                                 dfsch_bool(instance_suffices));
  if (instance_suffices){
    m = dfsch_saprintf("%s is not an instance of %s",
                       dfsch_object_2_string(datum, 10, 1),
                       dfsch_object_2_string((dfsch_object_t*)type, 10, 1));
  } else {
    m = dfsch_saprintf("%s is not of type %s",
                       dfsch_object_2_string(datum, 10, 1),
                       dfsch_object_2_string((dfsch_object_t*)type, 10, 1));
  }
  dfsch_condition_put_field_cstr(c, "message", dfsch_make_string_cstr(m));
  dfsch_signal(c);
}

dfsch_type_t dfsch_operating_system_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_RUNTIME_ERROR_TYPE, 
                            "operating-system-error");
void dfsch_operating_system_error_saved(int e, char* funname){
  dfsch_object_t* c = dfsch_make_condition(DFSCH_OPERATING_SYSTEM_ERROR_TYPE);
  char* m = strerror(e);

  dfsch_condition_put_field_cstr(c, "errno", DFSCH_MAKE_FIXNUM(e));
  dfsch_condition_put_field_cstr(c, "function", 
                                 dfsch_make_string_cstr(funname));
  dfsch_condition_put_field_cstr(c, "message", dfsch_make_string_cstr(m));
  dfsch_signal(c);
}
void dfsch_operating_system_error(char* funname){
  dfsch_operating_system_error_saved(errno, funname);
}

DFSCH_DEFINE_PRIMITIVE(terminate_thread, NULL){
  DFSCH_ARG_END(args);

  pthread_exit(DFSCH_SYM_TERMINATE_THREAD);
}

static restart_t terminate_thread_restart = {
  .type = DFSCH_RESTART_TYPE,
  .name = NULL,
  .proc = DFSCH_PRIMITIVE_REF(terminate_thread),
  .description = "Terminate current thread",
};
static dfsch__restart_list_t default_restart_list = {
  .restart = &terminate_thread_restart,
  .next = NULL
};

dfsch__restart_list_t* dfsch__get_default_restart_list(){
  terminate_thread_restart.name = DFSCH_SYM_TERMINATE_THREAD;
  /* DFSCH_SYM_TERMIANTE_THREAD is too complex to be recognized as constant */
  return &default_restart_list;
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
DFSCH_DEFINE_PRIMITIVE(make_condition_with_fields, 0){
  dfsch_object_t* type;
  dfsch_object_t* message;
  dfsch_object_t* fields;
  DFSCH_OBJECT_ARG(args, type);
  DFSCH_OBJECT_ARG(args, message);
  DFSCH_ARG_REST(args, fields);
  
  return dfsch_condition_with_fields(type, message, fields);
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
DFSCH_DEFINE_PRIMITIVE(invoke_restart, 0){
  dfsch_object_t* restart;
  DFSCH_OBJECT_ARG(args, restart);

  dfsch_invoke_restart(restart, args);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(compute_restarts, 0){
  DFSCH_ARG_END(args);

  return dfsch_compute_restarts();
}

DFSCH_DEFINE_PRIMITIVE(warning, "Signal a warning condition"){
  dfsch_object_t* message;
  dfsch_object_t* fields;
  DFSCH_OBJECT_ARG(args, message);
  DFSCH_ARG_REST(args, fields);

  dfsch_signal(dfsch_condition_with_fields(DFSCH_WARNING_TYPE, 
                                           message, fields));
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(error, "Signal an error condition"){
  dfsch_object_t* message;
  dfsch_object_t* fields;
  DFSCH_OBJECT_ARG(args, message);
  DFSCH_ARG_REST(args, fields);

  dfsch_signal(dfsch_condition_with_fields(DFSCH_ERROR_TYPE, 
                                           message, fields));
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(runtime_error, "Signal an runtime-error condition"){
  dfsch_object_t* message;
  dfsch_object_t* fields;
  DFSCH_OBJECT_ARG(args, message);
  DFSCH_ARG_REST(args, fields);

  dfsch_signal(dfsch_condition_with_fields(DFSCH_RUNTIME_ERROR_TYPE, 
                                           message, fields));
  return NULL;
}


DFSCH_DEFINE_PRIMITIVE(restart_name, 0){
  dfsch_object_t* restart;
  DFSCH_OBJECT_ARG(args, restart);
  DFSCH_ARG_END(args);

  return dfsch_restart_name(restart);
}
DFSCH_DEFINE_PRIMITIVE(restart_description, 0){
  dfsch_object_t* restart;
  DFSCH_OBJECT_ARG(args, restart);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_restart_description(restart));
}

/*
 * This is generally non-sufficient interface, but rest could be implemented 
 * as macros.
 */

DFSCH_DEFINE_FORM_IMPL(handler_bind, NULL){
  dfsch_object_t* ret;
  dfsch_object_t* bindings;
  dfsch_object_t* code;
  DFSCH_OBJECT_ARG(args, bindings);
  DFSCH_ARG_REST(args, code);

  DFSCH_SAVE_HANDLERS;

  while (DFSCH_PAIR_P(bindings)){
    dfsch_object_t* type;
    dfsch_object_t* handler;
    DFSCH_OBJECT_ARG(DFSCH_FAST_CAR(bindings), type);
    DFSCH_OBJECT_ARG(DFSCH_FAST_CAR(bindings), handler);
    DFSCH_ARG_END(DFSCH_FAST_CAR(bindings));

    type = dfsch_eval(type, env);
    handler = dfsch_eval(handler, env);

    dfsch_handler_bind(type, handler);
    bindings = DFSCH_FAST_CDR(bindings);
  }

  ret = dfsch_eval_proc(code, env);

  DFSCH_RESTORE_HANDLERS;
  return ret;
}

DFSCH_DEFINE_FORM_IMPL(restart_bind, NULL){
  dfsch_object_t* ret;
  dfsch_object_t* bindings;
  dfsch_object_t* code;
  DFSCH_OBJECT_ARG(args, bindings);
  DFSCH_ARG_REST(args, code);

  DFSCH_SAVE_HANDLERS;

  while (DFSCH_PAIR_P(bindings)){
    dfsch_object_t* name;
    dfsch_object_t* proc;
    DFSCH_OBJECT_ARG(DFSCH_FAST_CAR(bindings), name);
    DFSCH_OBJECT_ARG(DFSCH_FAST_CAR(bindings), proc);
    DFSCH_ARG_END(DFSCH_FAST_CAR(bindings));

    proc = dfsch_eval(proc, env);

    // TODO
    dfsch_restart_bind(dfsch_make_restart(name, proc, "", NULL));
    bindings = DFSCH_FAST_CDR(bindings);
  }

  ret = dfsch_eval_proc(code, env);

  DFSCH_RESTORE_HANDLERS;
  return ret;
}


void dfsch__conditions_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "<condition>", DFSCH_CONDITION_TYPE);
  dfsch_define_cstr(ctx, "<warning>", DFSCH_WARNING_TYPE);
  dfsch_define_cstr(ctx, "<style-warning>", DFSCH_STYLE_WARNING_TYPE);
  dfsch_define_cstr(ctx, "<error>", DFSCH_ERROR_TYPE);
  dfsch_define_cstr(ctx, "<runtime-error>", DFSCH_RUNTIME_ERROR_TYPE);
  dfsch_define_cstr(ctx, "<type-error>", DFSCH_TYPE_ERROR_TYPE);
  dfsch_define_cstr(ctx, "<operating-system-error>", 
                    DFSCH_OPERATING_SYSTEM_ERROR_TYPE);
  
  dfsch_define_cstr(ctx, "make-condition", 
                    DFSCH_PRIMITIVE_REF(make_condition)); 
  dfsch_define_cstr(ctx, "make-condition-with-fields", 
                    DFSCH_PRIMITIVE_REF(make_condition_with_fields));
  dfsch_define_cstr(ctx, "condition-field", 
                    DFSCH_PRIMITIVE_REF(condition_field));
  dfsch_define_cstr(ctx, "condition-put-field", 
                    DFSCH_PRIMITIVE_REF(condition_put_field));
  dfsch_define_cstr(ctx, "condition-fields", 
                    DFSCH_PRIMITIVE_REF(condition_fields));


  dfsch_define_cstr(ctx, "signal",
                    DFSCH_PRIMITIVE_REF(signal));
  dfsch_define_cstr(ctx, "invoke-restart",
                    DFSCH_PRIMITIVE_REF(invoke_restart));
  dfsch_define_cstr(ctx, "compute-restarts",
                    DFSCH_PRIMITIVE_REF(compute_restarts));

  dfsch_define_cstr(ctx, "warning",
                    DFSCH_PRIMITIVE_REF(warning));
  dfsch_define_cstr(ctx, "error",
                    DFSCH_PRIMITIVE_REF(error));
  dfsch_define_cstr(ctx, "runtime-error",
                    DFSCH_PRIMITIVE_REF(runtime_error));


  dfsch_define_cstr(ctx, "restart-name",
                    DFSCH_PRIMITIVE_REF(restart_name));
  dfsch_define_cstr(ctx, "restart-description",
                    DFSCH_PRIMITIVE_REF(restart_description));

  dfsch_define_cstr(ctx, "handler-bind",
                    DFSCH_FORM_REF(handler_bind));
  dfsch_define_cstr(ctx, "restart-bind",
                    DFSCH_FORM_REF(restart_bind));
}
