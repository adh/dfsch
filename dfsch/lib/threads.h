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

extern dfsch_object_t* dfsch_condition_create();
extern void dfsch_condition_wait(dfsch_object_t* condition,
                                 dfsch_object_t* mutex);
extern void dfsch_condition_signal(dfsch_object_t* condition);
extern void dfsch_condition_broadcast(dfsch_object_t* condition);

extern dfsch_object_t* dfsch_channel_create(size_t buffer);
extern dfsch_object_t* dfsch_channel_read(dfsch_object_t* channel);
extern void dfsch_channel_write(dfsch_object_t* channel,
                                dfsch_object_t* object);

extern dfsch_object_t* dfsch_threads_register(dfsch_ctx_t *ctx);

#endif
