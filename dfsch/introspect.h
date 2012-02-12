#ifndef H__dfsch__introspect__
#define H__dfsch__introspect__

#include <dfsch/dfsch.h>

void dfsch_print_trace_buffer();
dfsch_object_t* dfsch_get_trace();

void dfsch_introspect_register(dfsch_object_t* env);

void dfsch_set_inspector(dfsch_object_t* proc);
void dfsch_inspect_object(dfsch_object_t* obj);
dfsch_object_t* dfsch_describe_object(dfsch_object_t* obj);

dfsch_object_t* dfsch_find_source_annotation(dfsch_object_t* list);

typedef void (*dfsch_breakpoint_hook_t)(void* baton,
                                        dfsch_object_t* exp,
                                        dfsch_object_t* env);
typedef void* (*dfsch_function_entry_hook_t)(void* baton,
                                             dfsch_object_t* func,
                                             dfsch_object_t* args,
                                             dfsch_object_t* context);
typedef void (*dfsch_function_exit_hook_t)(void* baton,
                                           dfsch_object_t* func,
                                           dfsch_object_t* values,
                                           dfsch_object_t* context,
                                           void* entry_token);

void dfsch_add_breakpoint(dfsch_object_t* expr,
                          dfsch_breakpoint_hook_t hook,
                          void* baton);
void dfsch_remove_breakpoint(dfsch_object_t* expr);
void dfsch_clear_breakpoints();

void dfsch_add_traced_function(dfsch_object_t* func,
                               dfsch_function_entry_hook_t entry,
                               dfsch_function_exit_hook_t exit,
                               void* baton);
void dfsch_remove_traced_function(dfsch_object_t* func);
void dfsch_clear_traced_functions();

void dfsch_trace_function(dfsch_object_t* func);
void dfsch_add_standard_breakpoint(dfsch_object_t* expr);
void dfsch_add_function_breakpoint(dfsch_object_t* fun);

void dfsch_prepare_trace_trap(dfsch_breakpoint_hook_t hook,
                              void* baton);
void dfsch_prepare_single_step_breakpoint();
int dfsch_have_trace_trap_p();

#endif
