#include <dfsch/lib/cinspect.h>
#include <dfsch/dfsch.h>
#include <dfsch/lib/console.h>
#include <dfsch/introspect.h>
#include <stdio.h>

typedef struct inspector_state_t {
  dfsch_object_t* object_stack;

  dfsch_object_t* current_object;
  char* description;
  dfsch_object_t* slot_list;

  int slot_print_depth;
  dfsch_object_t* env;
} inspector_state_t;

static void init_state(inspector_state_t*is){
  static dfsch_object_t* inspector_env;

  if (!inspector_env){
    inspector_env = dfsch_make_top_level_environment();
    dfsch_introspect_register(inspector_env);
  }

  is->object_stack = NULL;
  is->slot_print_depth = 3;
  is->env = dfsch_new_frame(inspector_env);
}

static void update_state(inspector_state_t* is, dfsch_object_t* obj){
  dfsch_object_t* desc = dfsch_describe_object(obj);
  
  is->description = dfsch_string_to_cstr(dfsch_car(desc));
  is->slot_list = dfsch_cdr(desc);
  dfsch_defcanon_pkgcstr(is->env, DFSCH_DFSCH_PACKAGE, "object", obj);
}

static void push_object(inspector_state_t* is, dfsch_object_t* obj){
  is->object_stack = dfsch_cons(obj, is->object_stack);
  update_state(is, obj);
}

static void pop_object(inspector_state_t* is){
  is->object_stack = dfsch_cdr(is->object_stack);
  update_state(is, dfsch_car(is->object_stack));
}


static void redisplay_object(inspector_state_t* is){
  dfsch_object_t* i;
  size_t j;
  int max_len = -3;

  fprintf(stderr, "object %p is %s %s\n", 
          dfsch_car(is->object_stack),
          strchr("aeiou", is->description[0]) == NULL ? "a" : "an",
          is->description);

  i = is->slot_list;
  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* slot = DFSCH_FAST_CAR(i);
    dfsch_object_t* name;
    DFSCH_OBJECT_ARG(slot, name);

    if (name){
      int l = strlen(dfsch_object_2_string(name, 1, 0));
      if (l > 25){
        max_len = 25;
        break;
      }
      if (l > max_len){
        max_len = l;
      }
    }
    i = DFSCH_FAST_CDR(i);
  }


  i = is->slot_list;
  j = 0;
  fprintf(stderr, "\nSlots:\n");
  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* slot = DFSCH_FAST_CAR(i);
    dfsch_object_t* name;
    dfsch_object_t* value;

    DFSCH_OBJECT_ARG(slot, name);
    DFSCH_OBJECT_ARG(slot, value);

    if (name){
      fprintf(stderr, "%4d: [%-*s] %s\n",
              j,
              max_len,
              dfsch_object_2_string(name, 1, 0),
              dfsch_object_2_string(value, is->slot_print_depth, 1));
    } else {
      fprintf(stderr, "%4d: %s\n",
              j,
              dfsch_object_2_string(value, is->slot_print_depth, 1));      
    }
    j++;
    i = DFSCH_FAST_CDR(i);
  }
  fprintf(stderr, "\nEnter slot index or keyword to inspect it's value.\n");
}

static dfsch_object_t* cinspect_callback(dfsch_object_t *obj,  inspector_state_t* is){
  if (dfsch_integer_p(obj)){
    push_object(is, dfsch_list_item(dfsch_list_item(is->slot_list,
                                                    dfsch_number_to_long(obj)),
                                    1));
    redisplay_object(is);
    return dfsch_car(is->object_stack);
  } else if (dfsch_keyword_p(obj)){
    push_object(is, dfsch_slot_ref_by_name(dfsch_car(is->object_stack),
                                           dfsch_string_or_symbol_to_cstr(obj),
                                           1));
    redisplay_object(is);
    return dfsch_car(is->object_stack);
  } else {
    return dfsch_eval(obj, is->env);
  }
}

static void command_slot_print_depth(char* cmdline, inspector_state_t* is){
  if (*cmdline){
    is->slot_print_depth = atoi(cmdline);
  }
  fprintf(stderr, "Slot print depth is %d\n", is->slot_print_depth);
}

static void command_pop(char* cmdline, inspector_state_t* is){
  pop_object(is);
  redisplay_object(is);
}


static void inspect_object(dfsch_object_t* obj){
  inspector_state_t is;
  dfsch_console_repl_command_t* cmds = NULL;
  init_state(&is);
  push_object(&is, obj);
  redisplay_object(&is);

  cmds = dfsch_console_add_command(cmds, "slot-print-depth",
                                   "Set print depth for slot values in inspector",
                                   command_slot_print_depth, &is);
  cmds = dfsch_console_add_command(cmds, "pop",
                                   "Pop object from inspector stack",
                                   command_pop, &is);

  dfsch_console_run_repl_eval("inspect> ", 
                              cinspect_callback, &is, cmds);
}


void dfsch_cinspect_inspect_object(dfsch_object_t* obj){
  inspect_object(obj);
}

DFSCH_DEFINE_PRIMITIVE(inspect_object, ""){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  inspect_object(object);

  return object;
}

dfsch_object_t* dfsch_cinspect_get_procedure(){
  return DFSCH_PRIMITIVE_REF(inspect_object);
}
void dfsch_cinspect_set_as_inspector(){
  dfsch_set_inspector(dfsch_cinspect_get_procedure());
}
