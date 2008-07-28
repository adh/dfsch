/*
 * dfsch - dfox's quick and dirty scheme implementation
 *     Exception and continuation handling implementation
 * Copyright (C) 2005-2008 Ales Hakl
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

  /*
   * TODO: Mechanism used for unwind-protect should be unified between 
   * continuations and exceptions, but this works for now.
   */

  typedef struct dfsch__thread_info_t dfsch__thread_info_t;
  typedef struct dfsch__restart_list_t dfsch__restart_list_t;
  typedef struct dfsch__catch_list_t dfsch__catch_list_t;
  typedef struct dfsch__handler_list_t dfsch__handler_list_t;

  struct dfsch__restart_list_t {
    dfsch_object_t* restart;
    dfsch__handler_list_t* handlers;
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

  struct dfsch__thread_info_t {
    jmp_buf* throw_ret;
    dfsch_object_t* throw_tag;
    dfsch_object_t* throw_value;
    dfsch_object_t* stack_trace;

    dfsch__catch_list_t* catch_list;
    dfsch__handler_list_t* handler_list;
    dfsch__restart_list_t* restart_list; 

    char* break_type;
  };

  extern dfsch__thread_info_t* dfsch__get_thread_info();
  extern void dfsch__continue_unwind();
  extern void dfsch__finalize_unwind();


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
  dfsch_object_t* dfsch___old_frame;                            \
  dfsch__catch_list_t* dfsch___old_catch;                       \
  dfsch__handler_list_t* dfsch___old_handlers;                  \
  dfsch__restart_list_t* dfsch___old_restarts;                  \
                                                                \
  dfsch___old_ret = dfsch___ei->throw_ret;                      \
  dfsch___old_frame = dfsch___ei->stack_trace;                  \
  dfsch___old_catch = dfsch___ei->catch_list;                   \
  dfsch___old_handlers = dfsch___ei->handler_list;              \
  dfsch___old_restarts = dfsch___ei->restart_list;              \
  dfsch___ei->throw_ret = &dfsch___tmpbuf;                      \
                                                                \
  if(setjmp(*dfsch___ei->throw_ret) != 1){

#define DFSCH_SCATCH                                            \
  dfsch___ei->throw_ret = (jmp_buf*)dfsch___old_ret;            \
  dfsch___ei->stack_trace = (dfsch_object_t*)dfsch___old_frame; \
  dfsch___ei->catch_list = dfsch___old_catch;                   \
  dfsch___ei->handler_list = dfsch___old_handlers;              \
  dfsch___ei->restart_list = dfsch___old_restarts;              \
} else {                                                        \
  dfsch___ei->throw_ret = (jmp_buf*)dfsch___old_ret;            \
  dfsch___ei->stack_trace = (dfsch_object_t*)dfsch___old_frame; \
  dfsch___ei->catch_list = dfsch___old_catch;                   \
  dfsch___ei->handler_list = dfsch___old_handlers;              \
  dfsch___ei->restart_list = dfsch___old_restarts;              \
  {

#define DFSCH_CATCH_TAG (dfsch___ei->throw_tag)
#define DFSCH_CATCH_VALUE (dfsch___ei->throw_value)
  
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
  dfsch_object_t* dfsch___exception;            \
  int dfsch___caught = 0;                       \
  DFSCH_SCATCH_BEGIN {

#define DFSCH_PROTECT                           \
  } DFSCH_SCATCH {                              \
    dfsch___caught = 1;                         \
  } DFSCH_SCATCH_END
  
#define DFSCH_PROTECT_END                       \
  if (dfsch___caught){                          \
    dfsch__continue_unwind();                   \
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


#ifdef __cplusplus
}
#endif


#endif
