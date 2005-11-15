#ifndef H__dfsch__util__
#define H__dfsch__util__

#include <stdint.h>
#include <stdlib.h>

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



#endif
