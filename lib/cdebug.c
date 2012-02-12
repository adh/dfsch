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
#include <dfsch/introspect.h>
#include <dfsch/util.h>
#include <dfsch/magic.h>
#include <stdio.h>
#include <dfsch/lib/console.h>

static dfsch_object_t* debugger_env = NULL;

static dfsch_object_t* debug_invoke_restart(dfsch_object_t* list,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  int index;
  DFSCH_LONG_ARG(args, index);
  DFSCH_ARG_REST(args, args);

  dfsch_invoke_restart(dfsch_list_item(list, index), args);
  return NULL;
}

static void prettyprint_condition_fields(dfsch_object_t* fields){
  dfsch_object_t* i = fields;
  dfsch_object_t* j;
  while (DFSCH_PAIR_P(i)){
    j = DFSCH_FAST_CAR(i);
    i = DFSCH_FAST_CDR(i);
    if (!DFSCH_PAIR_P(j)){
      return;
    }
    if (DFSCH_TYPE_OF(DFSCH_FAST_CAR(j)) == DFSCH_SYMBOL_TYPE &&
        (dfsch_compare_keyword(DFSCH_FAST_CAR(j), "stack-trace") ||
         dfsch_compare_keyword(DFSCH_FAST_CAR(j), "message"))){
      continue;
    }
    fprintf(stderr, "    %s: %s\n" ,
            dfsch_object_2_string(DFSCH_FAST_CAR(j), 10, 1),
            dfsch_object_2_string(DFSCH_FAST_CDR(j), 10, 1));
  }
  
}

static void prettyprint_locals(dfsch_object_t* locals){
  dfsch_object_t* i = locals;
  dfsch_object_t* j;
  while (DFSCH_PAIR_P(i)){
    j = DFSCH_FAST_CAR(i);
    i = DFSCH_FAST_CDR(i);
    if (!DFSCH_PAIR_P(j)){
      return;
    }
    fprintf(stderr, "    %s: %s\n" ,
            dfsch_object_2_string(dfsch_list_item(j, 0), 10, 1),
            dfsch_object_2_string(dfsch_list_item(j, 1), 10, 1));
  }
  
}


typedef struct cdebug_ctx_t {
  dfsch_object_t* env;
  dfsch_object_t* reason;
  dfsch_object_t* restarts;
  dfsch_object_t* ustack;
  dfsch_object_t* bp_env;
  dfsch_object_t* bp_expr;
} cdebug_ctx_t;

static dfsch_object_t* cdebug_callback(dfsch_object_t *obj,  cdebug_ctx_t* ctx){
  if (!ctx->bp_env && dfsch_integer_p(obj)){
    return dfsch_invoke_restart(dfsch_list_item(ctx->restarts, 
                                                dfsch_number_to_long(obj)), 
                                DFSCH_INVALID_OBJECT);
  }


  return dfsch_eval(obj, ctx->env);
}

static dfsch_object_t debugger_exit = {DFSCH_INVALID_OBJECT_TYPE};

static void command_variables(char* cmdline, cdebug_ctx_t* ctx){
  dfsch_inspect_object(ctx->bp_env);
}
static void command_disable(char* cmdline, cdebug_ctx_t* ctx){
  dfsch_remove_breakpoint(ctx->bp_expr);
}

static void command_stack(char* cmdline, cdebug_ctx_t* ctx){
  dfsch_inspect_object(ctx->ustack);
}
static void command_condition(char* cmdline, cdebug_ctx_t* ctx){
  dfsch_inspect_object(ctx->reason);
}
static void command_step(char* cmdline, cdebug_ctx_t* ctx){
  dfsch_prepare_single_step_breakpoint();
  dfsch_throw(&debugger_exit, NULL);
}
static void command_continue(char* cmdline, cdebug_ctx_t* ctx){
  dfsch_prepare_trace_trap(NULL, NULL);
  dfsch_throw(&debugger_exit, NULL);
}


static void debug_main(dfsch_object_t* reason){
  dfsch_object_t* restarts = dfsch_compute_restarts();
  dfsch_object_t* env;
  dfsch_object_t* ustack = dfsch_get_trace();
  cdebug_ctx_t ctx;
  char buf[512];
  int i;
  dfsch_console_repl_command_t* cmds = NULL;

  if (!debugger_env){
    debugger_env = dfsch_make_top_level_environment();
    dfsch_introspect_register(debugger_env);
  }

  env = dfsch_new_frame(debugger_env);

  ctx.reason = reason;
  ctx.restarts = restarts;
  ctx.env = env;
  ctx.bp_env = NULL;
  ctx.ustack = ustack;

  dfsch_define_cstr(env, "reason", reason);
  dfsch_define_cstr(env, "stack-trace", ustack);

  if (DFSCH_INSTANCE_P(reason, DFSCH_CONDITION_TYPE)){
    if (DFSCH_INSTANCE_P(reason, DFSCH_BREAKPOINT_CONDITION_TYPE)){
      dfsch_object_t* bp_env = dfsch_condition_field_cstr(reason,
                                                          "environment");

      ctx.bp_env = bp_env;
      fprintf(stderr, "breakpoint on %s:\n",
              dfsch_object_2_string(dfsch_condition_field_cstr(reason,
                                                               "expression"),
                                    10, 0));
      dfsch_define_cstr(env, "environment", bp_env);
      fprintf(stderr, "\nLocal variables:\n");
      prettyprint_locals(dfsch_get_environment_variables(bp_env));
    } else {
      fprintf(stderr, "debugger invoked on %s:\n",
              DFSCH_TYPE_OF(reason)->name);
      fprintf(stderr,"  %s\n", 
              dfsch_object_2_string(dfsch_condition_field_cstr(reason,
                                                               "message"),
                                    10, 0));
      prettyprint_condition_fields(dfsch_condition_fields(reason));
    }
  } else {
    fprintf(stderr,"debugger invoked on:\n  %s\n", 
            dfsch_object_2_string(reason, 10, 1));
  }

  fprintf(stderr, "\nstack trace:\n%s\n", dfsch_format_trace(ustack));

  cmds = dfsch_console_add_command(cmds, "stack",
                                   "Inspect stack trace",
                                   command_stack, &ctx);
  cmds = dfsch_console_add_command(cmds, "condition",
                                   "Inspect condition object",
                                   command_condition, &ctx);
  cmds = dfsch_console_add_command(cmds, "step",
                                   "Run program for single evaluator step",
                                   command_step, &ctx);
  cmds = dfsch_console_add_command(cmds, "continue",
                                   "Run program without single stepping",
                                   command_continue, &ctx);


  if (!ctx.bp_env){
    dfsch_define_cstr(env, "restarts", restarts);
    dfsch_define_cstr(env, "r", dfsch_make_primitive("r",
                                                     debug_invoke_restart,
                                                     restarts,
                                                     "Invoke numbered restart with arguments",
                                                     0));
    
    fprintf(stderr, "restarts:\n");
    i = 0;
    while (DFSCH_PAIR_P(restarts)){
      dfsch_object_t* restart = DFSCH_FAST_CAR(restarts);
      fprintf(stderr, "  %2d: [%s] %s\n",
              i,
              dfsch_object_2_string(dfsch_restart_name(restart), 1, 1),
              dfsch_restart_description(restart));
      i++;
      restarts = DFSCH_FAST_CDR(restarts);
    }
  } else {
    cmds = dfsch_console_add_command(cmds, "variables",
                                     "Inspect environment frame",
                                     command_variables, &ctx);
    cmds = dfsch_console_add_command(cmds, "disable",
                                     "Remove current breakpoint",
                                     command_disable, &ctx);
    fprintf(stderr, "EOF to continue, ;disable to remove breakpoint, ;help for more commands\n");
  }
  
  if (dfsch_have_trace_trap_p()){
    fprintf(stderr, "Program has trace trap, type ;continue to disable\n");
  }

  DFSCH_CATCH_BEGIN(&debugger_exit){
    dfsch_console_run_repl_eval(dfsch_saprintf("dbg%d> ", 
                                               dfsch_get_debugger_depth()), 
                                cdebug_callback, &ctx, cmds);
  } DFSCH_CATCH {
  } DFSCH_CATCH_END;
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

DFSCH_DEFINE_PRIMITIVE(query_for_object, 0){
  char* prompt;
  DFSCH_STRING_ARG(args, prompt);
  DFSCH_ARG_END(args);

  return dfsch_console_read_object(dfsch_saprintf("%s: ", prompt));
}


dfsch_object_t* dfsch_cdebug_get_procedure(){
  return DFSCH_PRIMITIVE_REF(enter_debugger);
}
void dfsch_cdebug_set_as_debugger(){
  dfsch_set_debugger(dfsch_cdebug_get_procedure());
  dfsch_set_query_for_object_proc(DFSCH_PRIMITIVE_REF(query_for_object));
}
