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
} stack_frame_t;

static char* stack_frame_write(stack_frame_t* sf, int depth, int readable){
  str_list_t* sl = sl_create();

  sl_append(sl, "#<user-stack-frame ");
  sl_append(sl, dfsch_obj_write(sf->procedure, depth - 1, 1));
  sl_append(sl, " ");
  sl_append(sl, dfsch_obj_write(sf->arguments, depth - 1, 1));
  if (sf->tail_recursive){
    sl_append(sl, " tail-recursive");
  }
    
  sl_append(sl, ">");

  return sl_value(sl);
}

static dfsch_object_t* stack_frame_apply(stack_frame_t* sf, 
                                         dfsch_object_t* args, 
                                         dfsch_tail_escape_t* esc){
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


DFSCH_DEFINE_PRIMITIVE(frobnicate, 0){

}

void dfsch_introspect_register(dfsch_object_t* env){
  
}
