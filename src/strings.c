#include <dfsch/strings.h>
#include "object.h"
#include <string.h>

typedef struct dfsch_string_t {
  dfsch_type_t type;
  int immutable;
  char* ptr;
  size_t len;
} dfsch_string_t;

int string_equal_p(dfsch_string_t* a, dfsch_string_t* b){
  if (a->len != b->len)
    return 0;

  return memcmp(a->ptr, b->ptr, a->len) == 0;
}
char* string_write(dfsch_string_t* o, int max_depth){
  char *b = GC_MALLOC_ATOMIC(o->len*2+3);
  char *i = b;
  int j;

  *i='"';
  i++;

  for (j = 0; j < o->len; ++j){
    switch (o->ptr[j]){
    case '"':
      i[0]='\\';
      i[1]='"';
      i+=2;
      break;
    case '\0':
      i[0]='\\';
      i[1]='0';
      i+=2;
      break;
    case '\a':
      i[0]='\\';
      i[1]='a';
      i+=2;
      break;
    case '\b':
      i[0]='\\';
      i[1]='b';
      i+=2;
      break;
    case '\t':
      i[0]='\\';
      i[1]='t';
      i+=2;
      break;
    case '\n':
      i[0]='\\';
      i[1]='n';
      i+=2;
      break;
    case '\v':
      i[0]='\\';
      i[1]='v';
      i+=2;
      break;
    case '\f':
      i[0]='\\';
      i[1]='f';
      i+=2;
      break;
    case '\r':
      i[0]='\\';
      i[1]='r';
      i+=2;
      break;

    default:
      *i = o->ptr[j];
      ++i;
    }
  }

  *i='"';
  i[1]=0;

  return b;

}

static const dfsch_type_t string_type = {
  sizeof(dfsch_string_t),
  "string",
  string_equal_p,
  string_write,
};
#define STRING (&string_type)

int dfsch_string_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == &string_type;

}

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
    (dfsch_string_t*)dfsch_make_object(&string_type);

  s->ptr = GC_MALLOC_ATOMIC(len+1);
  s->len = len;
  memcpy(s->ptr, ptr, len);
  s->ptr[len] = 0;

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
