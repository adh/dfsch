/*
 * dfsch - dfox's quick and dirty scheme implementation
 *     Exception and continuation handling implementation
 * Copyright (C) 2005-2010 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

/*
 * This file contains some very ugly code, that has great potential to break 
 * things if used improperly. Generaly, don't include this unless you are sure
 * you need to.
 *
 * Only the macros defined here are considered public interface, but 
 * unfortunately, they are macros so changes to them breaks binary 
 * compatibility and requires recompile of user code including this file.
 */

#ifndef H__dfsch__magic__
#define H__dfsch__magic__

#include <dfsch/dfsch.h>
#include <dfsch/conditions.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct dfsch__thread_info_t dfsch__thread_info_t;
  typedef struct dfsch__restart_list_t dfsch__restart_list_t;
  typedef struct dfsch__catch_list_t dfsch__catch_list_t;
  typedef struct dfsch__handler_list_t dfsch__handler_list_t;
  typedef struct dfsch__stack_frame_t dfsch__stack_frame_t;

  struct dfsch__restart_list_t {
    dfsch_object_t* restart;
    dfsch__restart_list_t* next;
  };
  struct dfsch__catch_list_t {
    dfsch_object_t* tag;
    dfsch__catch_list_t* next;
  };
  struct dfsch__handler_list_t {
    dfsch_type_t* type;
    dfsch_object_t* handler;
    dfsch__handler_list_t* next;
  };

#define DFSCH_STACK_TRACE_KIND_APPLY            1
#define DFSCH_STACK_TRACE_KIND_EVAL             2

#define DFSCH_STACK_TRACE_FLAG_APPLY_TAIL       256

  typedef struct dfsch__stack_trace_frame_t dfsch__stack_trace_frame_t;
  struct dfsch__stack_trace_frame_t {
    int flags;
    union {
      struct {
        dfsch_object_t* expr;
        dfsch_object_t* env;
      } eval;
      struct {
        dfsch_object_t* proc;
        dfsch_object_t* args;
      } apply;
    } data;
    dfsch__stack_trace_frame_t* next;
  };

#define DFSCH_SCRATCH_PAD_SIZE 16

  struct dfsch__thread_info_t {
    dfsch_object_t* async_apply;

    dfsch__stack_trace_frame_t* stack_trace;
    dfsch_object_t** values;
    
    dfsch_object_t* macroexpanded_expr;

    void* env_freelist;
    int env_fl_depth;

    jmp_buf* throw_ret;
    dfsch_object_t* throw_tag;
    dfsch_object_t* throw_value;
    dfsch_object_t** throw_values;

    dfsch__catch_list_t* catch_list;
    dfsch__handler_list_t* handler_list;
    dfsch__restart_list_t* restart_list; 

    dfsch_object_t* scratch_pad[DFSCH_SCRATCH_PAD_SIZE];
    int error_policy;
    dfsch_package_t* current_package;
  };

  extern dfsch__thread_info_t* dfsch__get_thread_info();
  extern void dfsch__continue_unwind();
  extern void dfsch__finalize_unwind();

  dfsch__restart_list_t* dfsch__get_default_restart_list();

  typedef struct dfsch_saved_trace_t dfsch_saved_trace_t;
  dfsch_saved_trace_t* dfsch_save_trace_buffer();
  void dfsch_restore_trace_buffer(dfsch_saved_trace_t* st);


#define DFSCH__DEBUG_TAG printf(";; %s:%d %s\n", __FILE__, __LINE__, __func__)

  /**
   * Low-level stack unwinding construct. throw in code between BEGIN and 
   * SCATCH causes control transfer to code between SCATCH and END.
   *
   * Generaly not useful for normal user code, except for unwinding stack 
   * through unrelated C code (callbacks from C libraries like expat etc.)
   */

#define DFSCH_SCATCH_BEGIN                                      \
  {                                                             \
  dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();  \
  jmp_buf *dfsch___old_ret;                                     \
  jmp_buf dfsch___tmpbuf;                                       \
  dfsch__catch_list_t* dfsch___old_catch;                       \
  dfsch__handler_list_t* dfsch___old_handlers;                  \
  dfsch__restart_list_t* dfsch___old_restarts;                  \
  dfsch__stack_trace_frame_t* dfsch___old_stack_trace;          \
  dfsch_object_t* dfsch___old_scratch_pad[DFSCH_SCRATCH_PAD_SIZE];      \
                                                                \
  dfsch___old_ret = dfsch___ei->throw_ret;                      \
  dfsch___old_catch = dfsch___ei->catch_list;                   \
  dfsch___old_handlers = dfsch___ei->handler_list;              \
  dfsch___old_restarts = dfsch___ei->restart_list;              \
  dfsch___old_stack_trace = dfsch___ei->stack_trace;            \
  memcpy(dfsch___old_scratch_pad, dfsch___ei->scratch_pad,      \
         sizeof(dfsch_object_t*) * DFSCH_SCRATCH_PAD_SIZE);     \
  dfsch___ei->throw_ret = &dfsch___tmpbuf;                      \
                                                                \
  if(setjmp(*dfsch___ei->throw_ret) != 1){

#define DFSCH_SCATCH                                            \
  dfsch___ei->throw_ret = (jmp_buf*)dfsch___old_ret;            \
  dfsch___ei->catch_list = dfsch___old_catch;                   \
  dfsch___ei->handler_list = dfsch___old_handlers;              \
  dfsch___ei->restart_list = dfsch___old_restarts;              \
  dfsch___ei->stack_trace = dfsch___old_stack_trace;            \
  memcpy(dfsch___ei->scratch_pad, dfsch___old_scratch_pad,      \
         sizeof(dfsch_object_t*) * DFSCH_SCRATCH_PAD_SIZE);     \
} else {                                                        \
  dfsch___ei->throw_ret = (jmp_buf*)dfsch___old_ret;            \
  dfsch___ei->catch_list = dfsch___old_catch;                   \
  dfsch___ei->handler_list = dfsch___old_handlers;              \
  dfsch___ei->restart_list = dfsch___old_restarts;              \
  dfsch___ei->stack_trace = dfsch___old_stack_trace;            \
  memcpy(dfsch___ei->scratch_pad, dfsch___old_scratch_pad,      \
         sizeof(dfsch_object_t*) * DFSCH_SCRATCH_PAD_SIZE);     \
  {

#define DFSCH_CATCH_TAG (dfsch___ei->throw_tag)
#define DFSCH_CATCH_VALUE (dfsch___ei->throw_value)
#define DFSCH_CATCH_RESTORE_VALUES \
  dfsch___ei->values = dfsch___ei->throw_values;\
  dfsch___ei->throw_values = NULL;
  
#define DFSCH_SCATCH_END                        \
  }}}
  
  /**
   * unwind-protect 
   *
   * Code between PROTECT and PROTECT_END is executed in any case, even when 
   * unwinding stack.
   */

#define DFSCH_UNWIND                            \
  {                                             \
  dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();  \
  int dfsch___caught = 0;                       \
  DFSCH_SCATCH_BEGIN {

#define DFSCH_PROTECT                           \
  } DFSCH_SCATCH {                              \
    dfsch___caught = 1;                         \
  } DFSCH_SCATCH_END
#define DFSCH_UNWIND_DETECT                     \
  if (dfsch___caught)
#define DFSCH_PROTECT_END                       \
  if (dfsch___caught){                          \
    dfsch__continue_unwind(dfsch___ei);         \
  }                                             \
}

#define DFSCH_CATCH_BEGIN(atag)                                 \
  {                                                             \
  dfsch__catch_list_t dfsch___cl;                               \
  dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();  \
  dfsch___cl.tag = (atag);                                      \
  dfsch___cl.next = dfsch___ei->catch_list;                     \
  dfsch___ei->catch_list = &dfsch___cl;                         \
  DFSCH_SCATCH_BEGIN {
#define DFSCH_CATCH                             \
  } DFSCH_SCATCH {                              \
  if (DFSCH_CATCH_TAG == dfsch___cl.tag) {
#define DFSCH_CATCH_END                         \
  dfsch__finalize_unwind(dfsch___ei);           \
} else {                                        \
    dfsch__continue_unwind(dfsch___ei);         \
  }                                             \
} DFSCH_SCATCH_END                              \
  dfsch___ei->catch_list = dfsch___cl.next;     \
}

#define DFSCH_SAVE_HANDLERS                                             \
  {                                                                     \
  dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();          \
  dfsch__handler_list_t* dfsch___saved_handlers = dfsch___ei->handler_list;

#define DFSCH_RESTORE_HANDLERS                          \
  dfsch___ei->handler_list = dfsch___saved_handlers;   \
}

#define DFSCH_SAVE_RESTARTS                                             \
  {                                                                     \
  dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();          \
  dfsch__restart_list_t* dfsch___saved_restarts = dfsch___ei->restart_list;

#define DFSCH_RESTORE_RESTARTS                          \
  dfsch___ei->restart_list = dfsch___saved_restarts;   \
}

#define DFSCH_WITH_SIMPLE_RESTART(name, description)                    \
  {                                                                     \
  dfsch_object_t* dfsch___tag = dfsch_gensym();                         \
  DFSCH_CATCH_BEGIN(dfsch___tag){                                       \
  dfsch_restart_bind(dfsch_make_restart(name,                           \
                                        dfsch_make_throw_proc(dfsch___tag), \
                                        description,                    \
                                        NULL));
#define DFSCH_END_WITH_SIMPLE_RESTART           \
  } DFSCH_CATCH {} DFSCH_CATCH_END              \
}

#define DFSCH_WITH_RETRY_WITH_RESTART(name, description)                \
  {                                                                     \
  dfsch_object_t* dfsch___tag = dfsch_gensym();                         \
  DFSCH_CATCH_BEGIN(dfsch___tag){                                       \
  dfsch_restart_bind(dfsch_make_restart(name,                           \
                                        dfsch_make_throw_proc_arg(dfsch___tag), \
                                        description,                    \
                                        dfsch_make_argument_reader_proc("Alternate value")));
#define DFSCH_END_WITH_RETRY_WITH_RESTART(obj)                          \
  } DFSCH_CATCH { obj = DFSCH_CATCH_VALUE; } DFSCH_CATCH_END \
}
  

#ifdef __cplusplus
}
#endif


#endif
