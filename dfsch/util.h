/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Utility functions
 * Copyright (C) 2005-2010 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

/*
 * Random utility functions. Can be roughly categorized into string handling
 * and platform abstraction.
 */

#ifndef H__dfsch__util__
#define H__dfsch__util__

#include <dfsch/strings.h>
#include <dfsch/defines.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <pthread.h>


typedef struct dfsch_str_list_t dfsch_str_list_t;

/*
 * String list - building strings from arbitrary number of parts.
 */
extern dfsch_str_list_t* dfsch_sl_create();
extern void dfsch_sl_append(dfsch_str_list_t* list, char* string);
extern void dfsch_sl_nappend(dfsch_str_list_t* list, char* string, size_t l);
extern void dfsch_sl_printf(dfsch_str_list_t* sl, char* format, ...);
extern char* dfsch_sl_value(dfsch_str_list_t* list);
extern dfsch_strbuf_t* dfsch_sl_value_strbuf(dfsch_str_list_t* list);

extern char* dfsch_stracat(char* a, char* b);
extern char* dfsch_stracat3(char* a, char* b, char* c);
extern char* dfsch_strancat(char* a, size_t an, char* b, size_t bn);
extern char* dfsch_stracpy(char* x);
extern char* dfsch_strancpy(char* x, size_t n);
extern char* dfsch_straquote(char *s);

/* Ignores locales, case-insensitive only for ASCII characters */
extern int dfsch_ascii_strcasecmp(char* a, char* b);

char* dfsch_vsaprintf(char* format, va_list ap);
char* dfsch_saprintf(char* format, ...);

/*
 * allocates synchronization objects on garbage collected heap and calls
 * finalizers if they are not known to be no-op (as is the case on Linux).
 */
extern pthread_mutex_t* dfsch_create_finalized_mutex();
extern pthread_cond_t* dfsch_create_finalized_cvar();

#ifdef PTHREAD_RWLOCK_INITIALIZER
extern pthread_rwlock_t* dfsch_create_finalized_rwlock();
#define DFSCH_RWLOCK_INITIALIZER PTHREAD_RWLOCK_INITIALIZER
#define DFSCH_RWLOCK_RDLOCK(l) pthread_rwlock_rdlock(l)
#define DFSCH_RWLOCK_WRLOCK(l) pthread_rwlock_wrlock(l)
#define DFSCH_RWLOCK_UNLOCK(l) pthread_rwlock_unlock(l)
#define DFSCH_CREATE_RWLOCK dfsch_create_finalized_rwlock
#define DFSCH_INIT_RWLOCK(l) pthread_rwlock_init(l, NULL)
#define DFSCH_DESTROY_RWLOCK(l) pthread_rwlock_destroy(l)
typedef pthread_rwlock_t dfsch_rwlock_t;
#else
/*
 * emulate rwlocks by mutexes if not avaiable
 */
#define DFSCH_RWLOCK_INITIALIZER PTHREAD_MUTEX_INITIALIZER
#define DFSCH_RWLOCK_RDLOCK(l) pthread_mutex_lock(l)
#define DFSCH_RWLOCK_WRLOCK(l) pthread_mutex_lock(l)
#define DFSCH_RWLOCK_UNLOCK(l) pthread_mutex_unlock(l)
#define DFSCH_CREATE_RWLOCK dfsch_create_finalized_mutex
#define DFSCH_INIT_RWLOCK(l) pthread_mutex_init(l, NULL)
#define DFSCH_DESTROY_RWLOCK(l) pthread_mutex_destroy(l)
typedef pthread_mutex_t dfsch_rwlock_t;
#endif

char* dfsch_getcwd();
char* dfsch_get_path_directory(char* path);
char* dfsch_realpath(char* path);

/* 
 * These two functions returns same value on unices but different on Windows.
 */
char* dfsch_get_user_home_directory();
char* dfsch_get_user_local_data_directory();

#endif
