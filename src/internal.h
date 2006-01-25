#ifndef H__dfsch___internal__
#define H__dfsch___internal__

#include <dfsch/dfsch.h>

extern dfsch_object_t* dfsch_native_register(dfsch_ctx_t *ctx);
extern int dfsch__number_eqv_p(dfsch_object_t* a, dfsch_object_t* b); 
extern void dfsch__hash_native_register(dfsch_ctx_t *ctx);

#endif
