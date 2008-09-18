/*
 * dfsch - Scheme-like Lisp dialect
 *   Interactive debugger
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

#include "dfsch/lib/cdebug.h"
#include <dfsch/dfsch.h>
#include <dfsch/conditions.h>
#include <stdio.h>
#include <dfsch/lib/console.h>

static dfsch_object_t* debugger_env = NULL;

static dfsch_object_t* debug_invoke_restart(dfsch_object_t* list,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  int index;
  DFSCH_LONG_ARG(args, index);

  dfsch_invoke_restart(dfsch_list_item(list, index), args);
  return NULL;
}

static void debug_main(dfsch_object_t* reason){
  dfsch_object_t* restarts = dfsch_compute_restarts();
  dfsch_object_t* env;
  char buf[512];
  int i;

  if (!debugger_env){
    debugger_env = dfsch_make_context();
  }

  env = dfsch_new_frame(debugger_env);
  dfsch_define_cstr(env, "reason", reason);

  if (DFSCH_INSTANCE_P(reason, DFSCH_CONDITION_TYPE)){
    fprintf(stderr, "debugger invoked on %s:\n",
            DFSCH_TYPE_OF(reason)->name);
    fprintf(stderr,"  %s\n", 
            dfsch_obj_write(dfsch_condition_field_cstr(reason,
                                                       "message"),
                            10, 0));
  } else {
    fprintf(stderr,"debugger invoked on:\n  %s\n", 
            dfsch_obj_write(reason, 10, 0));
  }

  dfsch_define_cstr(env, "restarts", restarts);
  dfsch_define_cstr(env, "r", dfsch_make_primitive(debug_invoke_restart,
                                                   restarts));
  
  fprintf(stderr, "\nrestarts:\n");
  i = 0;
  while (DFSCH_PAIR_P(restarts)){
    dfsch_object_t* restart = DFSCH_FAST_CAR(restarts);
    fprintf(stderr, "  (r %d): [%s] %s\n",
            i,
            dfsch_obj_write(dfsch_restart_name(restart), 1, 1),
            dfsch_restart_description(restart));
    i++;
    restarts = DFSCH_FAST_CDR(restarts);
  }

  dfsch_console_run_repl(dfsch_saprintf("dbg%d> ", 
                                        dfsch_get_debugger_depth()), 
                         env);
}

void dfsch_cdebug_enter_debugger(dfsch_object_t* reason){
  debug_main(reason);
}

DFSCH_DEFINE_PRIMITIVE(enter_debugger, 0){
  dfsch_object_t* condition;
  DFSCH_OBJECT_ARG(args, condition);
  DFSCH_ARG_END(args);
  
  dfsch_cdebug_enter_debugger(condition);

  return NULL;
}

dfsch_object_t* dfsch_cdebug_get_procedure(){
  return DFSCH_PRIMITIVE_REF(enter_debugger);
}
