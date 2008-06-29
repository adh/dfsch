#ifndef H__dfsch___internal__
#define H__dfsch___internal__

#include <dfsch/dfsch.h>

extern void dfsch__native_register(dfsch_object_t *ctx);
extern void dfsch__control_register(dfsch_object_t *ctx);
extern void dfsch__hash_native_register(dfsch_object_t *ctx);
extern void dfsch__promise_native_register(dfsch_object_t *ctx);
extern void dfsch__number_native_register(dfsch_object_t *ctx);
extern void dfsch__string_native_register(dfsch_object_t *ctx);
extern void dfsch__wrapper_native_register(dfsch_object_t *ctx);
extern void dfsch__weak_native_register(dfsch_object_t *ctx);
extern void dfsch__format_native_register(dfsch_object_t *ctx);
extern void dfsch__port_native_register(dfsch_object_t *ctx);
extern void dfsch__system_register(dfsch_object_t *ctx);
extern void dfsch__generic_register(dfsch_object_t *ctx);
extern void dfsch__bignum_register(dfsch_object_t* ctx);

#endif
