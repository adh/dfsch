#ifndef H__dfsch__defines__
#define H__dfsch__defines__

#include <pthread.h>

#if !defined(__linux__)
/* pthread_mutex_destroy() is noop on (at least) linux */
#define DFSCH_THREADS_FINALIZE
#endif

#if defined(__GNUC__)
#define dfsch_unlikely(cond) __builtin_expect((cond), 0)
#define dfsch_likely(cond) __builtin_expect(!!(cond), 1)
#else
#define dfsch_unlikely(cond) (cond)
#define dfsch_likely(cond) (cond)
#endif

#if defined(__GNUC__) && defined(__linux__) && !defined(__arm__)
#define DFSCH__USE_TLS
#endif

#endif
