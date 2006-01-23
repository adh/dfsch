#ifndef H__dfsch__strings__
#define H__dfsch__strings__

#include <dfsch/dfsch.h>

typedef struct dfsch_strbuf_t{
  char* ptr;
  size_t len;
} dfsch_strbuf_t;

extern dfsch_strbuf_t* dfsch_strbuf_create(char* ptr, size_t len);

extern dfsch_object_t* dfsch_make_string_cstr(char* string);
extern dfsch_object_t* dfsch_make_string_strbuf(dfsch_strbuf_t* strbuf);
extern dfsch_object_t* dfsch_make_string_buf(char* ptr, size_t len);

extern char* dfsch_string_to_cstr(dfsch_object_t* obj);
extern dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj);

extern int dfsch_string_eq_p(dfsch_object_t* a, dfsch_object_t* b);

// TODO: some basic opertations: concatenation, indexing...

#endif
