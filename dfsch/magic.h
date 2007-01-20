#ifndef H__dfsch__magic__
#define H__dfsch__magic__

#include <dfsch/dfsch.h>
#include <setjmp.h>

typedef struct dfsch__continuation_t dfsch__continuation_t;
struct dfsch__continuation_t {
  dfsch_type_t* type;
  jmp_buf ret;
  dfsch_object_t* value;
  int active;
  pthread_t thread;
  dfsch__continuation_t* next;
};

typedef struct dfsch__thread_info_t {
  jmp_buf* exception_ret;
  dfsch_object_t* exception_obj;
  dfsch_object_t* stack_trace;
  dfsch__continuation_t* cont_stack;
  char* break_type;
} dfsch__thread_info_t;

extern void dfsch__invalidate_continuations(dfsch__thread_info_t* ti, 
                                            dfsch__continuation_t* cont);
extern dfsch__thread_info_t* dfsch__get_thread_info();

#define DFSCH_TRY \
{  \
  dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();\
  jmp_buf *dfsch___old_ret;\
  dfsch_object_t* dfsch___old_frame;\
  dfsch__continuation_t* dfsch___cont;\
  \
  dfsch___old_ret = dfsch___ei->exception_ret;\
  dfsch___old_frame = dfsch___ei->stack_trace;\
  dfsch___cont = dfsch___ei->cont_stack;\
  dfsch___ei->exception_ret = GC_NEW(jmp_buf);\
  \
  if(setjmp(*dfsch___ei->exception_ret) != 1){

#define DFSCH_CATCH(var) \
    dfsch___ei->exception_ret = (jmp_buf*)dfsch___old_ret;\
    dfsch___ei->stack_trace = (dfsch_object_t*)dfsch___old_frame;\
  } else {\
    dfsch___ei->exception_ret = (jmp_buf*)dfsch___old_ret;\
    dfsch___ei->stack_trace = (dfsch_object_t*)dfsch___old_frame;\
    dfsch__invalidate_continuations(dfsch___ei, dfsch___cont);\
    dfsch___ei->cont_stack = dfsch___cont;\
  { dfsch_object_t* var = dfsch___ei->exception_obj;

#define DFSCH_END_TRY \
}}}

#endif
