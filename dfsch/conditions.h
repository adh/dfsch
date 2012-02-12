#ifndef H__dfsch__conditions__
#define H__dfsch__conditions__

#include <dfsch/dfsch.h>
#include <stdarg.h>


typedef struct dfsch__condition_t {
  dfsch_type_t* type;
  dfsch_object_t* fields;
} dfsch__condition_t;

char* dfsch__condition_write(dfsch__condition_t* c, int depth, int readable);


dfsch_object_t* dfsch_make_condition(dfsch_type_t* type);

dfsch_object_t* dfsch_condition_field(dfsch_object_t* condition,
                                      dfsch_object_t* name);
void dfsch_condition_put_field(dfsch_object_t* condition,
                               dfsch_object_t* name,
                               dfsch_object_t* value);
dfsch_object_t* dfsch_condition_field_cstr(dfsch_object_t* condition,
                                           char* name);
void dfsch_condition_put_field_cstr(dfsch_object_t* condition,
                                    char* name,
                                    dfsch_object_t* value);
dfsch_object_t* dfsch_condition_fields(dfsch_object_t* condition);

dfsch_object_t* dfsch_condition(dfsch_type_t* type, ...);
void dfsch_signal_condition(dfsch_type_t* type, char* message, ...);
dfsch_object_t* dfsch_condition_with_fields(dfsch_type_t* type,
                                            dfsch_object_t* message,
                                            dfsch_object_t* fields);


#define DFSCH_CONDITION_SIZE (sizeof(dfsch__condition_t))
#define DFSCH_CONDITION_TYPE_INIT(super, name)                  \
  {DFSCH_STANDARD_TYPE, super, DFSCH_CONDITION_SIZE, name}

extern dfsch_type_t dfsch_condition_type;
#define DFSCH_CONDITION_TYPE (&dfsch_condition_type)

extern dfsch_type_t dfsch_serious_condition_type;
#define DFSCH_SERIOUS_CONDITION_TYPE (&dfsch_serious_condition_type)

extern dfsch_type_t dfsch_breakpoint_condition_type;
#define DFSCH_BREAKPOINT_CONDITION_TYPE (&dfsch_breakpoint_condition_type)


extern dfsch_type_t dfsch_warning_type;
#define DFSCH_WARNING_TYPE (&dfsch_warning_type)

extern dfsch_type_t dfsch_style_warning_type;
#define DFSCH_STYLE_WARNING_TYPE (&dfsch_style_warning_type)

extern dfsch_type_t dfsch_error_type;
#define DFSCH_ERROR_TYPE (&dfsch_error_type)

extern dfsch_type_t dfsch_runtime_error_type;
#define DFSCH_RUNTIME_ERROR_TYPE (&dfsch_runtime_error_type)

#define DFSCH_EP_PROCESS 0
#define DFSCH_EP_THREAD  1

void dfsch_set_error_policy(int pol);
void dfsch_signal(dfsch_object_t* condition);
void dfsch_set_debugger(dfsch_object_t* proc);
void dfsch_set_invoke_debugger_on_all_conditions(int val);
void dfsch_set_query_for_object_proc(dfsch_object_t* proc);
void dfsch_enter_debugger(dfsch_object_t* reason);
int dfsch_get_debugger_depth();

dfsch_object_t* dfsch_query_for_object(dfsch_object_t* prompt);
dfsch_object_t* dfsch_query_for_object_cstr(dfsch_object_t* prompt);

dfsch_object_t* dfsch_make_argument_reader_proc(char* prompt, ...);

dfsch_object_t* dfsch_make_restart(dfsch_object_t* name,
                                   dfsch_object_t* proc,
                                   char* description,
                                   dfsch_object_t* interactive);

extern dfsch_type_t dfsch_restart_type;
#define DFSCH_RESTART_TYPE (&dfsch_restart_type)
dfsch_object_t* dfsch_restart_name(dfsch_object_t* restart);
dfsch_object_t* dfsch_restart_proc(dfsch_object_t* restart);
char* dfsch_restart_description(dfsch_object_t* restart);

void dfsch_restart_bind(dfsch_object_t* restart);
void dfsch_handler_bind(dfsch_type_t* type,
                        dfsch_object_t* handler);

dfsch_object_t* dfsch_compute_restarts();
dfsch_object_t* dfsch_invoke_restart(dfsch_object_t* restart,
                                     dfsch_object_t* args);
dfsch_object_t* dfsch_invoke_restart_cstr(char* restart,
                                          dfsch_object_t* args);


dfsch_object_t* dfsch_make_throw_proc(dfsch_object_t* catch_tag);
dfsch_object_t* dfsch_make_throw_proc_arg(dfsch_object_t* catch_tag);

extern dfsch_type_t dfsch_type_error_type;
#define DFSCH_TYPE_ERROR_TYPE (&dfsch_type_error_type)
void dfsch_type_error(dfsch_object_t* datum, dfsch_type_t* type, 
                      int instance_suffices);

extern dfsch_type_t dfsch_operating_system_error_type;
#define DFSCH_OPERATING_SYSTEM_ERROR_TYPE (&dfsch_operating_system_error_type)
void dfsch_operating_system_error_saved(int e, char* funname);
void dfsch_operating_system_error(char* funname);

extern dfsch_type_t dfsch_index_error_type;
#define DFSCH_INDEX_ERROR_TYPE (&dfsch_index_error_type)
void dfsch_index_error(dfsch_object_t* seq,
                       size_t index,
                       size_t length);

#endif
