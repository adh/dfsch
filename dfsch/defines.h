#ifndef H__dfsch__defines__
#define H__dfsch__defines__

#include <pthread.h>

#if !defined(__linux__)
/* pthread_mutex_destroy() is noop on (at least) linux */
#define DFSCH_THREADS_FINALIZE
#endif

#if defined(__GNUC__)
#define DFSCH_UNLIKELY(cond) __builtin_expect(!!(cond), 0)
#define DFSCH_LIKELY(cond) __builtin_expect(!!(cond), 1)
#define DFSCH_PREFETCH(addr) __builtin_prefetch(addr)
#else
#define DFSCH_UNLIKELY(cond) (cond)
#define DFSCH_LIKELY(cond) (cond)
#define DFSCH_PREFETCH(addr) 
#endif


#endif
