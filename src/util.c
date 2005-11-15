#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "util.h"

#include <gc/gc.h>

str_list_t* dfsch__sl_create(){
  str_list_t* list = GC_MALLOC(sizeof(str_list_t));
  
  list->head = NULL;
  list->tail = NULL;
  list->len = NULL;

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
