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
char* string_write(dfsch_string_t* o, int max_depth, int readable){
  char *b;
  char *i;
  int j;

  if (!readable){
    return o->ptr;
  }

  b = GC_MALLOC_ATOMIC(o->len*2+3);
  i = b;
 
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
  (dfsch_type_equal_p_t)string_equal_p,
  (dfsch_type_write_t)string_write,
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

dfsch_object_t* dfsch_string_list_append(dfsch_object_t* list){
  object_t* i = list;
  size_t len = 0;
  dfsch_string_t *r = dfsch_make_object(STRING);
  char* ptr;

  while (i){
    dfsch_string_t *s = (dfsch_string_t*)dfsch_car(i);

    TYPE_CHECK(s, STRING, "string");
    
    len += s->len;

    i = dfsch_cdr(i);
  }

  r->len = len;
  r->ptr = ptr = GC_MALLOC_ATOMIC(len+1);

  i = list;

  while (i){
    dfsch_string_t *s = (dfsch_string_t*)dfsch_car(i);

    memcpy(ptr, s->ptr, s->len);
    ptr += s->len;

    i = dfsch_cdr(i);
  }
  
  *ptr = 0;

  return r;

}

/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////


static object_t* native_string_append(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  
  return dfsch_string_list_append(args);

}
static object_t* native_string_ref(void *baton, object_t* args, dfsch_tail_escape_t* esc){
}
static object_t* native_string_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
}

void dfsch__string_native_register(dfsch_ctx_t *ctx){

  dfsch_ctx_define(ctx, "string-append", 
		   dfsch_make_primitive(&native_string_append,NULL));
  dfsch_ctx_define(ctx, "string-ref", 
		   dfsch_make_primitive(&native_string_ref,NULL));
  dfsch_ctx_define(ctx, "string-length", 
		   dfsch_make_primitive(&native_string_length,NULL));


}
