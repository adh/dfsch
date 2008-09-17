#ifndef H__dfsch__introspect__
#define H__dfsch__introspect__

#include <dfsch/dfsch.h>


dfsch_object_t* dfsch_get_stack_trace();

extern dfsch_type_t dfsch_user_stack_frame_type;
#define DFSCH_USER_STACK_FRAME_TYPE (&dfsch_user_stack_frame_type)

void dfsch_introspect_register(dfsch_object_t* env);

#endif
