#ifndef H__dfsch__introspect__
#define H__dfsch__introspect__

#include <dfsch/dfsch.h>

void dfsch_print_trace_buffer();
dfsch_object_t* dfsch_get_trace();

void dfsch_introspect_register(dfsch_object_t* env);

#endif
