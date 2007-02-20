/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Utility functions
 * Copyright (C) 2005 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifndef H__dfsch__util__
#define H__dfsch__util__

#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>

typedef struct str_li_t str_li_t;
typedef struct str_list_t {
  str_li_t* head;
  str_li_t* tail;
  size_t len;
} str_list_t;
struct str_li_t {
  char* str;
  size_t len;
  str_li_t* next;
};

#define ASCII_tolower(c) ((c)<='Z'&&(c)>='A'?(c)+('a'-'A'):(c))


extern str_list_t* dfsch__sl_create();
#define sl_create dfsch__sl_create
extern void dfsch__sl_append(str_list_t* list, char* string);
#define sl_append dfsch__sl_append
extern char* dfsch__sl_value(str_list_t* list);
#define sl_value dfsch__sl_value
extern char* dfsch__stracat(char* a, char* b);
#define stracat dfsch__stracat
extern char* dfsch__stracpy(char* x);
#define stracpy dfsch__stracpy
extern char* dfsch__strancpy(char* x, size_t n);
#define strancpy dfsch__strancpy
extern char* dfsch__straquote(char *s);
#define straquote dfsch__straquote
extern int dfsch__ascii_strcasecmp(char* a, char* b);
#define ascii_strcasecmp dfsch__ascii_strcasecmp

extern pthread_mutex_t* dfsch__create_finalized_mutex();
#define create_finalized_mutex dfsch__create_finalized_mutex


#endif
