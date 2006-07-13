#ifndef H__dfsch__threads__
#define H__dfsch__threads__

#include <dfsch/dfsch.h>

extern dfsch_object_t* dfsch_thread_create(dfsch_object_t* function,
                                           dfsch_object_t* arguments);
extern dfsch_object_t* dfsch_thread_join(dfsch_object_t* thread);
extern void dfsch_thread_detach(dfsch_object_t* thread);
extern dfsch_object_t* dfsch_thread_self();

extern dfsch_object_t* dfsch_mutex_create();
extern void dfsch_mutex_lock(dfsch_object_t* mutex);
extern int dfsch_mutex_trylock(dfsch_object_t* mutex);
extern void dfsch_mutex_unlock(dfsch_object_t* mutex);



extern dfsch_object_t* dfsch_threads_register(dfsch_ctx_t *ctx);

#endif
