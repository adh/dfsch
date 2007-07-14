#ifndef H__dfsch__weak__
#define H__dfsch__weak__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_make_weak_reference(dfsch_object_t* refered);

int dfsch_weak_reference_live_p(dfsch_object_t* reference);
dfsch_object_t* dfsch_weak_reference_dereference(dfsch_object_t* reference);

#endif
