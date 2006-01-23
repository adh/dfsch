#include <dfsch/strings.h>
#include "object.h"
#include <string.h>

typedef struct dfsch_string_t {
  type_t string;
  char* ptr;
  size_t len;
} dfsch_string_t;

dfsch_strbuf_t* dfsch_strbuf_create(char* ptr, size_t len){
  dfsch_strbuf_t *sb = GC_NEW(dfsch_strbuf_t);

  sb->ptr = ptr;
  sb->len = len;

  return sb;
}

dfsch_object_t* dfsch_make_string_cstr(char* string){
  if (!string)
    return NULL;

  return dfsch_make_string_buf(string, strlen(string));
}
dfsch_object_t* dfsch_make_string_strbuf(dfsch_strbuf_t* strbuf){
  return dfsch_make_string_buf(strbuf->ptr, strbuf->len);
}
dfsch_object_t* dfsch_make_string_buf(char* ptr, size_t len){
  dfsch_string_t *s = 
    (dfsch_string_t*)dfsch__make_object(STRING, sizeof(dfsch_string_t));

  s->ptr = ptr;
  s->len = len;

  return (dfsch_object_t*)s;
}

char* dfsch_string_to_cstr(dfsch_object_t* obj){
  TYPE_CHECK(obj, STRING, "string");

  return ((dfsch_string_t*)obj)->ptr;
}
dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj){
  TYPE_CHECK(obj, STRING, "string");

  return dfsch_strbuf_create(((dfsch_string_t*)obj)->ptr, 
			     ((dfsch_string_t*)obj)->len);  
}

int dfsch_string_eq_p(dfsch_object_t* a, dfsch_object_t* b){
  TYPE_CHECK(a, STRING, "string");
  TYPE_CHECK(b, STRING, "string");
  
  if (((dfsch_string_t*)a)->len != ((dfsch_string_t*)b)->len)
    return 0;

  return memcmp(((dfsch_string_t*)a)->ptr, ((dfsch_string_t*)b)->ptr, 
		((dfsch_string_t*)a)->len) == 0;

}
