#include "dfsch/introspect.h"
#include <dfsch/magic.h>
#include "types.h"
#include "util.h"

typedef struct stack_frame_t {
  dfsch_type_t* type;
  dfsch_object_t* procedure;
  dfsch_object_t* arguments;
  int tail_recursive;

  dfsch_object_t* code;
  dfsch_object_t* env;
  dfsch_object_t* expr; 
  /* user stack frame should always contain some guess of relevant expression */
} stack_frame_t;

static void stack_frame_write(stack_frame_t* sf, dfsch_writer_state_t* state){
  str_list_t* sl = sl_create();

  dfsch_write_unreadable_start(state, sf);
  dfsch_write_object(state, sf->procedure);
  dfsch_write_string(state, " ");
  dfsch_write_object(state, sf->arguments);
  if (sf->tail_recursive){
    dfsch_write_string(state, " tail-recursive");
  } 
  dfsch_write_unreadable_end(state);   
}

static dfsch_object_t* stack_frame_apply(stack_frame_t* sf, 
                                         dfsch_object_t* args, 
                                         dfsch_tail_escape_t* esc){
  dfsch_object_t* selector;
  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_ARG_END(args);
  
  if (dfsch_compare_symbol(selector, "procedure")){
    return sf->procedure;
  }
  if (dfsch_compare_symbol(selector, "arguments")){
    return sf->arguments;
  }
  if (dfsch_compare_symbol(selector, "tail-recursive")){
    return dfsch_bool(sf->tail_recursive);
  }
  if (dfsch_compare_symbol(selector, "code")){
    return sf->code;
  }
  if (dfsch_compare_symbol(selector, "environment")){
    return sf->env;
  }
  if (dfsch_compare_symbol(selector, "expression")){
    return sf->expr;
  }

  return NULL;
}

dfsch_type_t dfsch_user_stack_frame_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(stack_frame_t),
  "user-stack-frame",
  NULL,
  (dfsch_type_write_t)stack_frame_write,
  (dfsch_type_apply_t)stack_frame_apply,
  NULL
};

static dfsch_object_t* make_user_stack_frame(dfsch__stack_frame_t* nsf){
  stack_frame_t* sf = dfsch_make_object(DFSCH_USER_STACK_FRAME_TYPE);

  sf->procedure = nsf->procedure;
  sf->arguments = nsf->arguments;
  sf->tail_recursive = nsf->tail_recursive;

  sf->code = nsf->code;
  sf->env = nsf->env;
  sf->expr = nsf->expr;

  return sf;
}

dfsch_object_t* dfsch_get_stack_trace(){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  dfsch_object_t* head;
  dfsch_object_t* tail;
  dfsch__stack_frame_t* i = ti->stack_frame;
  
  head = tail = dfsch_cons(make_user_stack_frame(i), NULL);
  i = i->next;

  while(i){
    dfsch_object_t* tmp = dfsch_cons(make_user_stack_frame(i), NULL);
    DFSCH_FAST_CDR(tail) = tmp;
    tail = tmp;
    i = i->next;
  }

  return head;
}

static char* trace_line(dfsch_object_t* line){
  stack_frame_t* frame;
  if (DFSCH_TYPE_OF(line) != DFSCH_USER_STACK_FRAME_TYPE) {
    dfsch_error("Not a user stack frame", line);
  }
  frame = line;

  if (frame->expr){
    return saprintf("  %s\n    %s\n", 
		    dfsch_obj_write(frame->procedure, 3, 1),
		    dfsch_obj_write(frame->expr, 4, 1));
  } else {
    return saprintf("  %s\n", 
		    dfsch_obj_write(frame->procedure, 3, 1));
  }
}

char* dfsch_format_stack_trace(dfsch_object_t* trace){
  dfsch_object_t* i = trace;
  str_list_t* sl = sl_create();
  while (DFSCH_PAIR_P(i)){
    sl_append(sl, trace_line(DFSCH_FAST_CAR(i)));
    i = DFSCH_FAST_CDR(i);
  }
  return sl_value(sl);
}


DFSCH_DEFINE_PRIMITIVE(stack_trace, 0){
  if (args)
    dfsch_error("exception:too-many-arguments", args);

  return dfsch_get_stack_trace();
}

DFSCH_DEFINE_PRIMITIVE(set_debugger, 0){
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, proc);
  DFSCH_ARG_END(args);
  
  dfsch_set_debugger(proc);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_invoke_debugger_on_all_conditions, 0){
  dfsch_object_t* val;
  DFSCH_OBJECT_ARG(args, val);
  DFSCH_ARG_END(args);
  
  dfsch_set_invoke_debugger_on_all_conditions(val != NULL);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(enter_debugger, 0){
  dfsch_object_t* reason;
  DFSCH_OBJECT_ARG(args, reason);
  DFSCH_ARG_END(args);
  
  dfsch_enter_debugger(reason);

  return NULL;
}


void dfsch_introspect_register(dfsch_object_t* env){
  dfsch_provide(env, "introspect");

  dfsch_define_cstr(env, "<user-stack-frame>", DFSCH_USER_STACK_FRAME_TYPE);
  dfsch_define_cstr(env, "stack-trace", DFSCH_PRIMITIVE_REF(stack_trace));

  dfsch_define_cstr(env, "set-invoke-debugger-on-all-conditions", 
                    DFSCH_PRIMITIVE_REF(set_invoke_debugger_on_all_conditions));
  dfsch_define_cstr(env, "set-debugger", DFSCH_PRIMITIVE_REF(set_debugger));
  dfsch_define_cstr(env, "enter-debugger", DFSCH_PRIMITIVE_REF(enter_debugger));

}
