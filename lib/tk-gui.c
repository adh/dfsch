#include <dfsch/lib/tk-gui.h>

#include <dfsch/magic.h>
#include <dfsch/util.h>

typedef struct interpreter_t {
  dfsch_type_t* type;
  Tcl_Interp* interpreter;
  int active;

  dfsch__thread_info_t* owner;
} interpreter_t;

dfsch_type_t dfsch_tcl_interpreter_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(interpreter_t),
  "tcltk:interpreter",
  NULL,
  NULL,
  NULL,
  NULL
};

typedef struct command_wrapper_t {
  dfsch_type_t* type;
  char* name;
  dfsch_object_t* interpreter;
} command_wrapper_t;

static dfsch_object_t* command_wrapper_apply(command_wrapper_t* cw,
                                             dfsch_object_t* args,
                                             dfsch_tail_escape_t* esc){
  Tcl_Interp* interpreter;

  if (cw->interpreter){
    interpreter = dfsch_tcl_interpreter(cw->interpreter);
  } else {
    DFSCH_TCL_INTERPRETER_ARG(args, interpreter);
  }

  return dfsch_tcl_eval(interpreter,
                            dfsch_stracat(cw->name,
                                          dfsch_tcl_quote_list(args)));
}

dfsch_type_t dfsch_tcl_command_wrapper_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_FUNCTION_TYPE,
  sizeof(command_wrapper_t),
  "tcltk:command-wrapper",
  NULL,
  NULL,
  (dfsch_type_apply_t)command_wrapper_apply,
  NULL
};

typedef struct command_t {

} command_t;
dfsch_type_t dfsch_tcl_callback_command_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .size = sizeof(command_t)
  
};


static interpreter_t* interpreter(dfsch_object_t* obj){
  interpreter_t* i = DFSCH_ASSERT_TYPE(obj, DFSCH_TCL_INTERPRETER_TYPE);
  if (!i->active){
    dfsch_error("Interpreter already destroyed", obj);
  }

  if (i->owner != dfsch__get_thread_info()){
    dfsch_error("Intepreter used from incorrect apartment", i);
  }

  return i;
}

Tcl_Interp* dfsch_tcl_interpreter(dfsch_object_t* obj){
  return interpreter(obj)->interpreter;
}

static void interpreter_finalizer(interpreter_t* i, void* cd){
  if (i->active){
    i->active = 0;
    Tcl_DeleteInterp(i->interpreter);
  }
}

dfsch_object_t* dfsch_tcl_make_interpreter(Tcl_Interp* interp){
  interpreter_t* i 
    = (interpreter_t*)dfsch_make_object(DFSCH_TCL_INTERPRETER_TYPE);

  i->interpreter = interp;
  i->active = 1;
  i->owner = dfsch__get_thread_info();
  GC_register_finalizer(i, (GC_finalization_proc)interpreter_finalizer,
                        NULL, NULL, NULL);

  return (dfsch_object_t*)i;
}

dfsch_object_t* dfsch_tcl_create_interpreter(){
  Tcl_Interp* i = Tcl_CreateInterp();
  if (Tcl_Init(i) == TCL_ERROR){
    dfsch_tcl_error(i);
  }
  if (Tk_Init(i) == TCL_ERROR){
    dfsch_tcl_error(i);
  }
  return dfsch_tcl_make_interpreter(i);
}
void dfsch_tcl_destroy_interpreter(dfsch_object_t* obj){
  interpreter_t* i = interpreter(obj);

  i->active = 0;
  Tcl_DeleteInterp(i->interpreter);
}

dfsch_object_t* dfsch_tcl_wrap_command(char* name,
                                       dfsch_object_t* interp){
  command_wrapper_t* cw 
    = (command_wrapper_t*)dfsch_make_object(DFSCH_TCL_COMMAND_WRAPPER_TYPE);


  cw->interpreter = interpreter(interp);
  cw->name = name;

  return (dfsch_object_t*)cw;
}

typedef struct {
  dfsch_object_t* proc;
} command_context_t;

static int command_proc(command_context_t* ctx,
                        Tcl_Interp* interp,
                        int argc, char** argv){
  dfsch_object_t *head; 
  dfsch_object_t *cur;
  dfsch_object_t *res; 
  int i;
  int ret;

  head = cur = dfsch_multicons(argc);

  for(i = 0; i < argc; ++i){
    DFSCH_FAST_CAR(cur) = dfsch_make_string_cstr(argv[i]);
    cur = DFSCH_FAST_CDR(cur);
  }

  DFSCH_SCATCH_BEGIN {
    res = dfsch_apply(ctx->proc, head);
    Tcl_SetResult(interp, dfsch_object_2_string(res, 100, 0), 
                  TCL_VOLATILE);
    ret = TCL_OK;
  } DFSCH_SCATCH {
    ret = TCL_ERROR;    
  } DFSCH_SCATCH_END;

  return ret;
}

static int cmd_counter = 0;

dfsch_object_t* dfsch_tcl_create_command(dfsch_object_t* interp, 
                                         char* name, 
                                         dfsch_object_t* proc){
  command_context_t* ctx = GC_NEW_UNCOLLECTABLE(command_context_t);

  if (!name){
    name = dfsch_saprintf("dfschAnonCmd%x", cmd_counter);
    cmd_counter++;
  }

  ctx->proc = proc;

  Tcl_CreateCommand(interp, name, command_proc, ctx, GC_free);
}

void dfsch_tcl_error(Tcl_Interp* interp){
  dfsch_error("tcltk:error", 
              dfsch_make_string_cstr(Tcl_GetStringResult(interp)));
}

char* dfsch_tcl_eval(Tcl_Interp* interp, char* string){
  if (Tcl_Eval(interp, string) == TCL_ERROR){
    dfsch__thread_info_t* ti = dfsch__get_thread_info();
    if (ti->throw_tag){
      dfsch__continue_unwind(ti);
    }
    dfsch_tcl_error(interp);
  }
  return dfsch_stracpy(Tcl_GetStringResult(interp));
}

char* dfsch_tcl_quote(char* str){
  char* ret;
  char* out;
  ret = out = GC_MALLOC_ATOMIC(strlen(str)*2+1);
  while (*str){
    switch (*str){
    case '\\':
      (*out++) = '\\';
      (*out++) = '\\';
      str++;
      break;
    case ' ':
      (*out++) = '\\';
      (*out++) = ' ';
      str++;
      break;
    case '\t':
      (*out++) = '\\';
      (*out++) = 't';
      str++;
      break;
    case '\n':
      (*out++) = '\\';
      (*out++) = 'n';
      str++;
      break;
    case '$':
      (*out++) = '\\';
      (*out++) = '$';
      str++;
      break;
    default:
      (*out++) = (*str++);
    }
  }

  (*out) = '\0';
  return ret;
}

char* dfsch_tcl_quote_list(dfsch_object_t* list){
  dfsch_str_list_t* sl = dfsch_sl_create();
  dfsch_object_t* i;

  while (DFSCH_PAIR_P(list)){
    dfsch_sl_append(sl, " ");
    i = DFSCH_FAST_CAR(list);
    if (dfsch_string_p(i)){
      dfsch_sl_append(sl, dfsch_tcl_quote(dfsch_string_to_cstr(i)));
    } else {
      dfsch_sl_append(sl, dfsch_tcl_quote(dfsch_object_2_string(i, 10, 1)));      
    }
    list = DFSCH_FAST_CDR(list);
  }

  return dfsch_sl_value(sl);
}
