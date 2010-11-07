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


char* dfsch_format_trace(dfsch_object_t* trace){
  str_list_t* sl = sl_create();
  
  while (DFSCH_PAIR_P(trace)){
    if (dfsch_vector_p(DFSCH_FAST_CAR(trace))){
      dfsch_object_t* tag = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 0);
      if (dfsch_compare_keyword(tag, "apply")){
        dfsch_object_t* proc = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 1);
        dfsch_object_t* flags = dfsch_vector_ref(DFSCH_FAST_CAR(trace), 2);
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

        while (DFSCH_PAIR_P(annot) && 
               DFSCH_FAST_CAR(annot) == DFSCH_SYM_MACRO_EXPANDED_FROM){
          sl_printf(sl, "    <- %s\n", 
                    dfsch_object_2_string(DFSCH_FAST_CDR(annot), 10, 
                                          DFSCH_WRITE));
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
  int i;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();

  for (i = 0; i <= ti->trace_depth; i++){
    if (i == ti->trace_ptr){
      fprintf(stderr, "> ");
    } else {
      fprintf(stderr, "  ");
    }
    switch (ti->trace_buffer[i].flags & 0xff){
    case DFSCH_TRACEPOINT_KIND_INVALID:
      fprintf(stderr, "0x00000000\n");
      break;
    case DFSCH_TRACEPOINT_KIND_APPLY:
      fprintf(stderr, "0x%08x %p (%s)\n", 
              ti->trace_buffer[i].flags,
              ti->trace_buffer[i].data.apply.proc,
              DFSCH_TYPE_OF(ti->trace_buffer[i].data.apply.proc)->name);
      break;
    case DFSCH_TRACEPOINT_KIND_EVAL:
      fprintf(stderr, "0x%08x %p (%s) %p\n", 
              ti->trace_buffer[i].flags,
              ti->trace_buffer[i].data.eval.expr,
              DFSCH_TYPE_OF(ti->trace_buffer[i].data.eval.expr)->name,
              ti->trace_buffer[i].data.eval.env);
      break;
    case DFSCH_TRACEPOINT_KIND_ANON:
      fprintf(stderr, "0x%08x %s %p\n", 
              ti->trace_buffer[i].flags,
              ti->trace_buffer[i].data.anon.location,
              ti->trace_buffer[i].data.anon.data);
      break;
    }
  }
}

static dfsch_object_t* reachable_env(environment_t* env, int flags){
  if (env->type != DFSCH_ENVIRONMENT_TYPE) {
    return NULL;
  }
  if (env->flags & EFRAME_SERIAL_MASK != flags & EFRAME_SERIAL_MASK){
    return NULL;
  }
  
  return dfsch_reify_environment(env);
}

dfsch_object_t* dfsch_get_trace(){
  int i;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  dfsch_object_t* list = NULL;
  dfsch_object_t* record;
  dfsch_object_t* flags;
  i = ti->trace_ptr;

  do {
    if ((ti->trace_buffer[i].flags & 0xff) == DFSCH_TRACEPOINT_KIND_INVALID){
      break;
    }

    flags = NULL;

    if (ti->trace_buffer[i].flags & DFSCH_TRACEPOINT_FLAG_MACROEXPAND) {
      flags = dfsch_cons_immutable(dfsch_make_keyword("macro-expansion"), 
                                   flags);
    }
    
    switch (ti->trace_buffer[i].flags & 0xff){
    case DFSCH_TRACEPOINT_KIND_APPLY:

      if (ti->trace_buffer[i].flags & DFSCH_TRACEPOINT_FLAG_APPLY_TAIL) {
        flags = dfsch_cons_immutable(dfsch_make_keyword("tail"), flags);
      }
      if (ti->trace_buffer[i].flags & DFSCH_TRACEPOINT_FLAG_APPLY_LAZY) {
        flags = dfsch_cons_immutable(dfsch_make_keyword("lazy"), flags);
      }

      record = dfsch_vector(3,
                            dfsch_make_keyword("apply"),
                            ti->trace_buffer[i].data.apply.proc,
                            flags);
      break;
    case DFSCH_TRACEPOINT_KIND_EVAL:
      record = dfsch_vector(4,
                            dfsch_make_keyword("eval"),
                            ti->trace_buffer[i].data.eval.expr,
                            reachable_env(ti->trace_buffer[i].data.eval.env,
                                          ti->trace_buffer[i].flags),
                            flags);
      break;
        
    default:
      record = DFSCH_MAKE_FIXNUM(ti->trace_buffer[i].flags);
      break;
    }

    list = dfsch_cons_immutable(record, list);

    i = (i - 1) & ti->trace_depth;
  } while (i != ti->trace_ptr);
  
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
}
