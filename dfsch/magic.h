/*
 * dfsch - dfox's quick and dirty scheme implementation
 *     Exception and continuation handling implementation
 * Copyright (C) 2005 Ales Hakl
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
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

  /*
   * TODO: Mechanism used for unwind-protect should be unified between 
   * continuations and exceptions, but this works for now.
   */

typedef struct dfsch__continuation_t dfsch__continuation_t;
typedef struct dfsch__unwind_protect_t dfsch__unwind_protect_t;
typedef struct dfsch__thread_info_t dfsch__thread_info_t;

struct dfsch__continuation_t {
  dfsch_type_t* type;
  jmp_buf ret;
  dfsch_object_t* value;
  int active;
  dfsch__thread_info_t* ti;
  dfsch__unwind_protect_t* top;
  dfsch__continuation_t* next;
};

struct dfsch__unwind_protect_t {
  jmp_buf after;  
  dfsch__unwind_protect_t* next;
};

struct dfsch__thread_info_t {
  jmp_buf* exception_ret;
  dfsch_object_t* exception_obj;
  dfsch__unwind_protect_t* exception_top;

  dfsch_object_t* stack_trace;


  char* break_type;
};

extern dfsch__thread_info_t* dfsch__get_thread_info();
extern void dfsch__continue_continuation(dfsch__thread_info_t* ti);

#define DFSCH_TRY                                                       \
  {                                                                     \
    dfsch__thread_info_t *dfsch___ei = dfsch__get_thread_info();        \
    jmp_buf *dfsch___old_ret;                                           \
    dfsch_object_t* dfsch___old_frame;                                  \
                                                                        \
    dfsch___old_ret = dfsch___ei->exception_ret;                        \
    dfsch___old_frame = dfsch___ei->stack_trace;                        \
    dfsch___ei->exception_ret = GC_NEW(jmp_buf);                        \
                                                                        \
    if(setjmp(*dfsch___ei->exception_ret) != 1){

#define DFSCH_CATCH(var)                                                \
  dfsch___ei->exception_ret = (jmp_buf*)dfsch___old_ret;                \
  dfsch___ei->stack_trace = (dfsch_object_t*)dfsch___old_frame;         \
} else {                                                                \
      dfsch___ei->exception_ret = (jmp_buf*)dfsch___old_ret;            \
      dfsch___ei->stack_trace = (dfsch_object_t*)dfsch___old_frame;     \
      { dfsch_object_t* var = dfsch___ei->exception_obj;
  
#define DFSCH_END_TRY                           \
  }}}
  
#define DFSCH_UNWIND                            \
  {                                             \
    dfsch_object_t* dfsch___exception;          \
    int dfsch___caught;                         \
    DFSCH_TRY {

#define DFSCH_PROTECT              \
  } DFSCH_CATCH(exception) {       \
    dfsch___caught = 1;            \
    dfsch___exception = exception; \
  } DFSCH_END_TRY
  
#define DFSCH_END_UNWIND                        \
  if (dfsch___caught){                          \
    dfsch_raise(dfsch___exception);             \
  }                                             \
}

#ifdef __cplusplus
}
#endif


#endif
