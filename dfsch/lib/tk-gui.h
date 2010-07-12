#ifndef H__dfsch_lib__tk_gui__
#define H__dfsch_lib__tk_gui__

#include <dfsch/dfsch.h>
#include <tcl.h>
#include <tk.h>

extern dfsch_type_t dfsch_tcl_interpreter_type;
#define DFSCH_TCL_INTERPRETER_TYPE               \
  (&dfsch_tcl_interpreter_type)
extern dfsch_type_t dfsch_tcl_command_wrapper_type;
#define DFSCH_TCL_COMMAND_WRAPPER_TYPE      \
  (&dfsch_tcl_command_wrapper_type)

Tcl_Interp* dfsch_tcl_interpreter(dfsch_object_t* obj);
dfsch_object_t* dfsch_tcl_make_interpreter(Tcl_Interp* interp);
dfsch_object_t* dfsch_tcl_create_interpreter();
void dfsch_tcl_destroy_interpreter(dfsch_object_t* obj);

dfsch_object_t* dfsch_tcl_wrap_command(char* name, dfsch_object_t* interp);
void dfsch_tcl_create_command(Tcl_Interp* interp, 
                              char* name, 
                              dfsch_object_t* proc);
void dfsch_tcl_error(Tcl_Interp* interp);
char* dfsch_tcl_eval(Tcl_Interp* interp, char* string);
char* dfsch_tcl_quote(char* str);
char* dfsch_tcl_quote_list(dfsch_object_t* list);


void dfsch_tcl_event_loop();


#define DFSCH_TCL_INTERPRETER_ARG(al, name)                         \
  DFSCH_GENERIC_ARG(al, name, Tcl_Interp*, dfsch_tcl_interpreter)
#define DFSCH_TCL_INTERPRETER_ARG_OPT(al, name, default)         \
  DFSCH_GENERIC_ARG_OPT(al, name, default,                           \
                        Tcl_Interp*, dfsch_tcl_interpreter)

#endif
