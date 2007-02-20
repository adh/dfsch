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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "util.h"

#include <dfsch/dfsch.h>
#include <string.h>

str_list_t* dfsch__sl_create(){
  str_list_t* list = GC_MALLOC(sizeof(str_list_t));
  
  list->head = NULL;
  list->tail = NULL;
  list->len = 0;

  return list;
}

void dfsch__sl_append(str_list_t* list, char* string){
  str_li_t* i = GC_MALLOC(sizeof(str_li_t));

  i->str = string;
  i->len = strlen(string);
  i->next = NULL;

  if (list->head){
    list->tail->next = i;
    list->tail = i;
    list->len = list->len + i->len;
  }else{
    list->head = list->tail = i;
    list->len = i->len;
  }
}

char* dfsch__sl_value(str_list_t* list){
  char *buf = GC_MALLOC_ATOMIC(list->len+1);
  str_li_t *i = list->head;
  char *ptr = buf;

  while (i){
    memcpy(ptr, i->str, i->len);
    ptr += i->len;
    i = i->next;
  }
  
  *ptr = '\0';

  return buf;
}

char* dfsch__stracat(char* a, char* b){
  size_t s = strlen(a)+strlen(b)+1;
  char* o = GC_MALLOC_ATOMIC(s);
  strncpy(o,a,s);
  strncat(o,b,s);
  return o;
}

char* dfsch__stracpy(char* x){
  char *b;
  size_t s = strlen(x)+1;
  b = GC_MALLOC_ATOMIC(s);
  strncpy(b,x,s);
  return b;
}
char* dfsch__strancpy(char* x, size_t n){
  char *b;
  size_t s = n+1;
  b = GC_MALLOC_ATOMIC(s);
  strncpy(b,x,s-1);
  b[s-1]=0;
  return b;
}
char* dfsch__straquote(char *s){
  char *b = GC_MALLOC_ATOMIC(strlen(s)*2+3); // worst case, to lazy to optimize
  char *i = b;

  *i='"';
  i++;

  while (*s){
    switch (*s){
    case '"':
      i[0]='\\';
      i[1]='"';
      i+=2;
    default:
      *i = *s;
      ++i;
    }
    s++;
  }

  *i='"';
  i[1]=0;

  return b;

}

int dfsch__ascii_strcasecmp(char* a, char* b){ 
  /*
   * XXX: this function HASN'T same API as strcasecmp 
   *      - it doesn't distinguish between < and >, only == and !=
   */

  while (*a && *b){
    if (ASCII_tolower(*a) != ASCII_tolower(*b))
      return 1;
    a++;
    b++;
  }
  return (*a != *b);
}

static void mutex_finalizer(pthread_mutex_t* mutex, void* cd){
  pthread_mutex_destroy(mutex);
}

pthread_mutex_t* dfsch__create_finalized_mutex(){
  pthread_mutex_t* mutex = GC_MALLOC_ATOMIC(sizeof(pthread_mutex_t));
  GC_REGISTER_FINALIZER(mutex, (GC_finalization_proc)mutex_finalizer,
                        NULL, NULL, NULL);
  pthread_mutex_init(mutex, NULL);
  return mutex;
}
static void cvar_finalizer(pthread_cond_t* cvar, void* cd){
  pthread_cond_destroy(cvar);
}

pthread_cond_t* dfsch__create_finalized_cvar(){
  pthread_cond_t* cvar = GC_MALLOC_ATOMIC(sizeof(pthread_cond_t));
  GC_REGISTER_FINALIZER(cvar, (GC_finalization_proc)cvar_finalizer,
                        NULL, NULL, NULL);
  pthread_cond_init(cvar, NULL);
  return cvar;
}
