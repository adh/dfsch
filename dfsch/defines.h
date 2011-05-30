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
#define DFSCH_FUNC_PURE __attributte((hot))

#else
#define DFSCH_UNLIKELY(cond) (cond)
#define DFSCH_LIKELY(cond) (cond)
#define DFSCH_PREFETCH(addr)
#define DFSCH_FUNC_PURE
#endif

#if (__GNUC__ > 4) && (__GNUC_MINOR__ > 3)
#define DFSCH_FUNC_HOT __attributte((hot))
#define DFSCH_FUNC_COLD __attributte((hot))
#else
#define DFSCH_FUNC_HOT
#define DFSCH_FUNC_COLD
#endif

#if defined(GC_NEXT) && !defined(__CYGWIN__)
#define DFSCH_GC_MALLOC_MANY
#undef DFSCH_GC_MALLOC_MANY_PREALLOC
#endif

/* DFSCH_DOC_STRING has multiple arguments as to allow DFSCH_DOC_ARGUMENTS()
 * like hacks and remove also it's expansions when docstrings are omitted */

#ifdef DFSCH_OMIT_DOCUMENTATION
#define DFSCH_DOC_STRING(str...) NULL
#else
#define DFSCH_DOC_STRING(str...) str
#endif

#endif
