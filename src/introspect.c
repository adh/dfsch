/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Introspection support
 * Copyright (C) 2005-2008 Ales Hakl
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

#include "dfsch/introspect.h"
#include <dfsch/magic.h>
#include "types.h"
#include "util.h"

#include <stdio.h>

static void safe_print_object();

dfsch_object_t* dfsch_find_source_annotation(dfsch_object_t* list){
  dfsch_object_t* anot = dfsch_get_list_annotation(list);
  while (DFSCH_PAIR_P(anot) &&
         (DFSCH_FAST_CAR(anot) == DFSCH_SYM_MACRO_EXPANDED_FROM ||
          DFSCH_FAST_CAR(anot) == DFSCH_SYM_COMPILED_FROM)){
    anot = dfsch_get_list_annotation(DFSCH_FAST_CDR(anot));
  }
  return anot;
}


char* dfsch_format_trace(dfsch_object_t* trace){
  str_list_t* sl = sl_create();
  
  while (DFSCH_PAIR_P(trace)){
    if (dfsch_vector_p(DFSCH_FAST_CAR(trace))){
      dfsch_object_t* tag = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 0);
      if (dfsch_compare_keyword(tag, "apply")){
        dfsch_object_t* proc = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 1);
        dfsch_object_t* flags = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 3);
        sl_printf(sl, "  APPLY %s %s\n",
                  dfsch_object_2_string(proc, 10, DFSCH_WRITE),
                  dfsch_object_2_string(flags, 10, DFSCH_WRITE));

      } else if (dfsch_compare_keyword(tag, "eval")){
        dfsch_object_t* expr = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 1);
        dfsch_object_t* flags = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 3);
        dfsch_object_t* annot = dfsch_get_list_annotation(expr);
        sl_printf(sl, "  EVAL %s %s\n",
                  dfsch_object_2_string(expr, 10, DFSCH_WRITE),
                  dfsch_object_2_string(flags, 10, DFSCH_WRITE));

        while (DFSCH_PAIR_P(annot)){
          if (DFSCH_FAST_CAR(annot) == DFSCH_SYM_MACRO_EXPANDED_FROM){
            sl_printf(sl, "    <- %s\n", 
                      dfsch_object_2_string(DFSCH_FAST_CDR(annot), 10, 
                                            DFSCH_WRITE));
          } else if (DFSCH_FAST_CAR(annot) == DFSCH_SYM_COMPILED_FROM) {
            sl_printf(sl, "    <= %s\n", 
                      dfsch_object_2_string(DFSCH_FAST_CDR(annot), 10, 
                                            DFSCH_WRITE));
          } else {
            break;
          }
          annot = dfsch_get_list_annotation(DFSCH_FAST_CDR(annot));
        }
        if (annot){
          sl_printf(sl, "     @ %s:%s\n", 
                  dfsch_object_2_string(DFSCH_FAST_CAR(annot), 10, 
                                        DFSCH_PRINT),
                  dfsch_object_2_string(DFSCH_FAST_CDR(annot), 10,
                                        DFSCH_PRINT));
          
        }

      } else {
        sl_printf(sl, "  UNKNOWN %s\n", 
                  dfsch_object_2_string(DFSCH_FAST_CAR(trace),
                                        10, DFSCH_WRITE));
      }
    } else {
      sl_printf(sl, "  INVALID %s\n", 
                dfsch_object_2_string(DFSCH_FAST_CAR(trace),
                                      10, DFSCH_WRITE));
    }

    trace = DFSCH_FAST_CDR(trace);
  }
  return sl_value(sl);
}

void dfsch_print_trace_buffer(){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch__stack_trace_frame_t* i = ti->stack_trace;

  while (i){
    fprintf(stderr, "  ");
    switch (i->flags & 0xff){
    case DFSCH_STACK_TRACE_KIND_APPLY:
      fprintf(stderr, "0x%08x %p (%s)\n", 
              i->flags,
              i->data.apply.proc,
              DFSCH_TYPE_OF(i->data.apply.proc)->name);
      break;
    case DFSCH_STACK_TRACE_KIND_EVAL:
      fprintf(stderr, "0x%08x %p (%s) %p\n", 
              i->flags,
              i->data.eval.expr,
              DFSCH_TYPE_OF(i->data.eval.expr)->name,
              i->data.eval.env);
      break;
    default:
      fprintf(stderr, "0x%08x\n", 
              i->flags);

    }
    i = i->next;
  }
}

dfsch_object_t* dfsch_get_trace(){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch_object_t* list = NULL;
  dfsch_object_t* record;
  dfsch_object_t* flags;
  dfsch__stack_trace_frame_t* i = ti->stack_trace;
  dfsch_object_t* args;

  while (i) {
    flags = NULL;

    switch (i->flags & 0xff){
    case DFSCH_STACK_TRACE_KIND_APPLY:

      if (i->flags & DFSCH_STACK_TRACE_FLAG_APPLY_TAIL) {
        flags = dfsch_cons_immutable(dfsch_make_keyword("tail"), flags);
      }

      if (i->data.apply.args == DFSCH_MAKE_CLIST(ti->scratch_pad)){
        args = dfsch_make_keyword("arguments-optimized-out");
      } else {
        args = dfsch_list_copy_immutable(i->data.apply.args);
      }

      record = dfsch_vector(4,
                            dfsch_make_keyword("apply"),
                            i->data.apply.proc,
                            args,
                            flags);
      break;
    case DFSCH_STACK_TRACE_KIND_EVAL:
      record = dfsch_vector(4,
                            dfsch_make_keyword("eval"),
                            i->data.eval.expr,
                            dfsch_reify_environment(i->data.eval.env),
                            flags);
      break;
        
    default:
      record = DFSCH_MAKE_FIXNUM(i->flags);
      break;
    }

    list = dfsch_cons_immutable(record, list);

    i = i->next;
  };
  
  return list;
}

static dfsch_object_t* inspector = NULL;

void dfsch_set_inspector(dfsch_object_t* proc){
  inspector = proc;
}
void dfsch_inspect_object(dfsch_object_t* obj){
  if (!inspector){
    dfsch_cerror("No inspector avaiable", NULL);
  } else {
    dfsch_apply(inspector, dfsch_list(1, obj));
  }
}

dfsch_object_t* dfsch_describe_object(dfsch_object_t* obj){
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  dfsch_object_t* i;
  dfsch_type_t* klass;

  if (DFSCH_PAIR_P(obj)){
    if (dfsch_list_length(obj, NULL) > 0){
      i = obj;
      while (DFSCH_PAIR_P(i)){
        dfsch_list_collect(lc, 
                           dfsch_list(2,
                                      NULL,
                                      DFSCH_FAST_CAR(i)));
        i = DFSCH_FAST_CDR(i);
      }


      return dfsch_cons(dfsch_make_string_cstr(dfsch_list_mutable_p(obj)?
                                               "proper list" : 
                                               "immutable proper list"),
                        dfsch_collected_list(lc));
    } else {
      return dfsch_list(3,
                        dfsch_make_string_cstr(DFSCH_TYPE_OF(obj)->name),
                        dfsch_list(2, 
                                   dfsch_make_string_cstr("car"),
                                   DFSCH_FAST_CAR(obj)),
                        dfsch_list(2, 
                                   dfsch_make_string_cstr("cdr"),
                                   DFSCH_FAST_CDR(obj)));
    }
  }
  
  klass = DFSCH_TYPE_OF(obj);
  while (klass){
    if (klass->describe){
      return klass->describe(obj);
    }
    klass = klass->superclass;
  }

  i = dfsch_get_slots(DFSCH_TYPE_OF(obj));

  while (DFSCH_PAIR_P(i)){
    dfsch_slot_t* slot = (dfsch_slot_t*)DFSCH_FAST_CAR(i);
    dfsch_list_collect(lc, 
                       dfsch_list(2,
                                  dfsch_make_string_cstr(slot->name),
                                  dfsch_slot_ref(obj, slot, 1)));
    
    i = DFSCH_FAST_CDR(i);
  }

  return dfsch_cons(dfsch_make_string_cstr(DFSCH_TYPE_OF(obj)->name),
                    dfsch_collected_list(lc));
}

static char* trace_indent(int level){
  char* buf = "| | | | | | | | | | ";
  if (level > 10){
    return "| | ...................";
  }
  return buf + 2*(10 - level);
}

static int trace_level = 0;

static void* default_trace_entry(void* discard,
                                 dfsch_object_t* function,
                                 dfsch_object_t* arguments,
                                 dfsch_object_t* context){
  fprintf(stderr, ";; %s+<< %s\n",
          trace_indent(trace_level),
          dfsch_object_2_string(function, 3, DFSCH_WRITE));
  fprintf(stderr, ";; %s| \\  %s\n",
          trace_indent(trace_level),
          dfsch_object_2_string(arguments, 3, DFSCH_WRITE));
  if (context){
    fprintf(stderr, ";; %s|   ... with context %s\n",
            trace_indent(trace_level),
            dfsch_object_2_string(context, 3, DFSCH_WRITE));
  }

  trace_level++;


  return dfsch_list_copy(arguments);
}

static void default_trace_exit(void* discard,
                               dfsch_object_t* function,
                               dfsch_object_t* result,
                               dfsch_object_t* context,
                               dfsch_object_t* saved_args){
  trace_level --;
  fprintf(stderr, ";; %s+>> %s\n",
          trace_indent(trace_level),
          dfsch_object_2_string(function, 3, DFSCH_WRITE));
  fprintf(stderr, ";; %s  \\  %s -> %s\n",
          trace_indent(trace_level),
          dfsch_object_2_string(saved_args, 3, DFSCH_WRITE),
          dfsch_object_2_string(result, 3, DFSCH_WRITE));

}

void dfsch_trace_function(dfsch_object_t* func){
  dfsch_add_traced_function(func, 
                            default_trace_entry, 
                            default_trace_exit, 
                            NULL);
}

static void standard_breakpoint_hook(void* baton,
                                     dfsch_object_t* exp,
                                     dfsch_object_t* env){
  dfsch_object_t* bp = dfsch_make_condition(DFSCH_BREAKPOINT_CONDITION_TYPE);
  dfsch_condition_put_field_cstr(bp, "expression", exp);
  dfsch_condition_put_field_cstr(bp, "environment", env);
  dfsch_enter_debugger(bp);
}

void dfsch_add_standard_breakpoint(dfsch_object_t* expr){
  dfsch_add_breakpoint(expr, standard_breakpoint_hook, NULL);
}
void dfsch_add_function_breakpoint(dfsch_object_t* fun){
  closure_t* f = DFSCH_ASSERT_INSTANCE(fun, DFSCH_STANDARD_FUNCTION_TYPE);

  if (!DFSCH_PAIR_P(f->code) || !DFSCH_PAIR_P(DFSCH_FAST_CAR(f->code))){
    dfsch_error("Cannot create breakpoint on first form of function", f);
  }

  dfsch_add_standard_breakpoint(DFSCH_FAST_CAR(f->code));
}


DFSCH_DEFINE_PRIMITIVE(set_debugger, 0){
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, proc);
  DFSCH_ARG_END(args);
  
  dfsch_set_debugger(proc);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(get_trace, 0){
  DFSCH_ARG_END(args);
  
  return dfsch_get_trace();
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
DFSCH_DEFINE_PRIMITIVE(set_inspector, 0){
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, proc);
  DFSCH_ARG_END(args);
  
  dfsch_set_inspector(proc);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(inspect_object, 0){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);
  
  dfsch_inspect_object(object);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(describe_object, 0){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);
  
  return dfsch_describe_object(object);
}

DFSCH_DEFINE_PRIMITIVE(lookup_in_environment, 0){
  dfsch_object_t* name;
  dfsch_object_t* env;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);

  return dfsch_lookup(name, env);
}
DFSCH_DEFINE_PRIMITIVE(find_in_environment, 0){
  dfsch_object_t* env;
  dfsch_object_t* value;
  dfsch_object_t* ret;
  dfsch_object_t* canonical;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_OBJECT_ARG_OPT(args, canonical, NULL);
  DFSCH_ARG_END(args);

  ret = dfsch_env_revscan(env, value, canonical != NULL);
  if (ret == DFSCH_INVALID_OBJECT){
    return NULL;
  } else {
    return dfsch_cons(ret, NULL);
  }
}

DFSCH_DEFINE_PRIMITIVE(set_in_environment, 0){
  dfsch_object_t* name;
  dfsch_object_t* env;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);
  
  dfsch_set(name, value, env);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(define_in_environment, 0){
  dfsch_object_t* name;
  dfsch_object_t* env;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);
  
  dfsch_define(name, value, env, 0);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(unset_from_environment, 0){
  dfsch_object_t* name;
  dfsch_object_t* env;
  dfsch_object_t* value;
  DFSCH_OBJECT_ARG(args, name);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);
  
  dfsch_unset(name, env);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(get_variables, 0){
  dfsch_object_t* env;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);
  
  return dfsch_get_environment_variables(env);
}

DFSCH_DEFINE_PRIMITIVE(load_into_environment, 0){
  dfsch_object_t* env;
  char* name;
  dfsch_object_t* path_list;
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, path_list, NULL);
  DFSCH_ARG_END(args);
  
  dfsch_load(env, name, path_list);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(make_environment, 0){
  dfsch_object_t* parent;
  DFSCH_OBJECT_ARG(args, parent);
  DFSCH_ARG_END(args);

  return dfsch_new_frame(parent);
}
DFSCH_DEFINE_PRIMITIVE(make_empty_environment, 0){
  DFSCH_ARG_END(args);

  return dfsch_new_frame(NULL);
}
DFSCH_DEFINE_PRIMITIVE(make_top_level_environment, 0){
  DFSCH_ARG_END(args);

  return dfsch_make_top_level_environment();
}
DFSCH_DEFINE_PRIMITIVE(trace_function, 
                       "Add function to set of traced functions"){
  dfsch_object_t* func;
  DFSCH_OBJECT_ARG(args, func);

  dfsch_trace_function(func);

  return func;
}
DFSCH_DEFINE_PRIMITIVE(untrace_function, 
                       "Remove function from set of traced functions"){
  dfsch_object_t* func;
  DFSCH_OBJECT_ARG(args, func);
  DFSCH_ARG_END(args);

  dfsch_remove_traced_function(func);

  return func;
}

DFSCH_DEFINE_PRIMITIVE(untrace_all_functions, 
                       "Clear set of traced functions"){
  DFSCH_ARG_END(args);

  dfsch_clear_traced_functions();

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(add_function_breakpoint,
                       "Put breakpoint on first form of function"){
  dfsch_object_t* func;
  DFSCH_OBJECT_ARG(args, func);
  DFSCH_ARG_END(args);

  dfsch_add_function_breakpoint(func);

  return func;
}


void dfsch_introspect_register(dfsch_object_t* env){
  dfsch_provide(env, "introspect");

  dfsch_defcanon_cstr(env, "set-invoke-debugger-on-all-conditions!", 
                    DFSCH_PRIMITIVE_REF(set_invoke_debugger_on_all_conditions));
  dfsch_defcanon_cstr(env, "set-debugger!", DFSCH_PRIMITIVE_REF(set_debugger));
  dfsch_defcanon_cstr(env, "enter-debugger", 
                    DFSCH_PRIMITIVE_REF(enter_debugger));

  dfsch_defcanon_cstr(env, "set-inspector!", DFSCH_PRIMITIVE_REF(set_inspector));
  dfsch_defcanon_cstr(env, "inspect-object", DFSCH_PRIMITIVE_REF(inspect_object));
  dfsch_defcanon_cstr(env, "describe-object", 
                    DFSCH_PRIMITIVE_REF(describe_object));

  dfsch_defcanon_cstr(env, "get-trace", DFSCH_PRIMITIVE_REF(get_trace));

  dfsch_defcanon_cstr(env, "lookup-in-environment",
                    DFSCH_PRIMITIVE_REF(lookup_in_environment));
  dfsch_defcanon_cstr(env, "find-in-environment",
                    DFSCH_PRIMITIVE_REF(find_in_environment));
  dfsch_defcanon_cstr(env, "set-in-environment!",
                    DFSCH_PRIMITIVE_REF(set_in_environment));
  dfsch_defcanon_cstr(env, "unset-from-environment!",
                    DFSCH_PRIMITIVE_REF(unset_from_environment));
  dfsch_defcanon_cstr(env, "define-in-environment!",
                    DFSCH_PRIMITIVE_REF(define_in_environment));
  dfsch_defcanon_cstr(env, "get-variables",
                    DFSCH_PRIMITIVE_REF(get_variables));
  dfsch_defcanon_cstr(env, "load-into-environment!",
                    DFSCH_PRIMITIVE_REF(load_into_environment));

  dfsch_defcanon_cstr(env, "make-environment",
                    DFSCH_PRIMITIVE_REF(make_environment));
  dfsch_defcanon_cstr(env, "make-empty-environment",
                    DFSCH_PRIMITIVE_REF(make_empty_environment));
  dfsch_defcanon_cstr(env, "make-top-level-environment",
                    DFSCH_PRIMITIVE_REF(make_top_level_environment));

  dfsch_defcanon_cstr(env, "trace-function!",
                    DFSCH_PRIMITIVE_REF(trace_function));
  dfsch_defcanon_cstr(env, "untrace-function!",
                    DFSCH_PRIMITIVE_REF(untrace_function));
  dfsch_defcanon_cstr(env, "untrace-all-functions!",
                    DFSCH_PRIMITIVE_REF(untrace_all_functions));
  dfsch_defcanon_cstr(env, "add-function-breakpoint!",
                    DFSCH_PRIMITIVE_REF(add_function_breakpoint));

}
