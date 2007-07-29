#ifndef H__dfsch___internal__
#define H__dfsch___internal__

#include <dfsch/dfsch.h>

extern void dfsch__native_register(dfsch_object_t *ctx);
extern void dfsch__control_register(dfsch_object_t *ctx);
extern int dfsch__number_eqv_p(dfsch_object_t* a, dfsch_object_t* b); 
extern void dfsch__hash_native_register(dfsch_object_t *ctx);
extern void dfsch__promise_native_register(dfsch_object_t *ctx);
extern void dfsch__number_native_register(dfsch_object_t *ctx);
extern void dfsch__string_native_register(dfsch_object_t *ctx);
extern void dfsch__wrapper_native_register(dfsch_object_t *ctx);
extern void dfsch__weak_native_register(dfsch_object_t *ctx);
extern void dfsch__format_native_register(dfsch_object_t *ctx);

#endif
