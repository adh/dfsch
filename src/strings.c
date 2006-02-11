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

int dfsch_string_cmp(dfsch_object_t* a, dfsch_object_t* b){
  TYPE_CHECK(a, STRING, "string");
  TYPE_CHECK(b, STRING, "string");
  
  if (((dfsch_string_t*)a)->len != ((dfsch_string_t*)b)->len){
    size_t l = (((dfsch_string_t*)a)->len < ((dfsch_string_t*)b)->len) ?
       ((dfsch_string_t*)a)->len : ((dfsch_string_t*)b)->len;

    int r = memcmp(((dfsch_string_t*)a)->ptr, ((dfsch_string_t*)b)->ptr, 
                   l);

    if (r == 0){
      return (((dfsch_string_t*)a)->len < ((dfsch_string_t*)b)->len) ? -1 : 1;
    }else {
      return r;
    }

  }

  return memcmp(((dfsch_string_t*)a)->ptr, ((dfsch_string_t*)b)->ptr, 
		((dfsch_string_t*)a)->len);

}

int dfsch_string_eq_p(dfsch_object_t* a, dfsch_object_t* b){
  TYPE_CHECK(a, STRING, "string");
  TYPE_CHECK(b, STRING, "string");
  
  if (((dfsch_string_t*)a)->len != ((dfsch_string_t*)b)->len)
      return 0;

  return memcmp(((dfsch_string_t*)a)->ptr, ((dfsch_string_t*)b)->ptr, 
		((dfsch_string_t*)a)->len) == 0;

}
int dfsch_string_lt_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp(a, b) < 0;
}
int dfsch_string_gt_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp(a, b) > 0;
}
int dfsch_string_lte_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp(a, b) <= 0;
}
int dfsch_string_gte_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp(a, b) >= 0;
}

int dfsch_string_cmp_ci(dfsch_object_t* a_, dfsch_object_t* b_){
  size_t i;
  size_t l;

  dfsch_string_t* a=(dfsch_string_t*)a_;
  dfsch_string_t* b=(dfsch_string_t*)b_;

  TYPE_CHECK(a, STRING, "string");
  TYPE_CHECK(b, STRING, "string");

  l = a->len; 
  if (a->len > b->len){
    l = b->len; 
  }

  for (i=0; i < l; i++){
    char ac = toupper(a->ptr[i]);
    char bc = toupper(b->ptr[i]);

    if (ac < bc){
      return -1;
    }
    if (ac > bc){
      return 1;
    }
  }
  
  if (a->len == b->len)
    return 0;

  if (a->len < b->len)
    return -1;

  return 1;
}

int dfsch_string_ci_eq_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp_ci(a, b) == 0;
}
int dfsch_string_ci_lt_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp_ci(a, b) < 0;
}
int dfsch_string_ci_gt_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp_ci(a, b) > 0;
}
int dfsch_string_ci_lte_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp_ci(a, b) <= 0;
}
int dfsch_string_ci_gte_p(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_string_cmp_ci(a, b) >= 0;
}



dfsch_object_t* dfsch_string_list_append(dfsch_object_t* list){
  object_t* i = list;
  size_t len = 0;
  dfsch_string_t *r = (dfsch_string_t*)dfsch_make_object(STRING);
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

  return (dfsch_object_t*)r;

}

char dfsch_string_ref(dfsch_object_t* string, size_t index){
  dfsch_string_t* s = (dfsch_string_t*) string;
  TYPE_CHECK(s, STRING, "string");

  if (index >= s->len)
    dfsch_throw("exception:index-out-of-bounds",
                dfsch_make_number_from_long(index));

  return s->ptr[index];
}

size_t dfsch_string_length(dfsch_object_t* string){
  dfsch_string_t* s = (dfsch_string_t*) string;
  TYPE_CHECK(s, STRING, "string");

  return s->len;
}

dfsch_object_t* dfsch_string_substring(dfsch_object_t* string, size_t start,
                                       size_t end){
  dfsch_string_t* s = (dfsch_string_t*) string;
  TYPE_CHECK(s, STRING, "string");

  if (end > s->len)
    dfsch_throw("exception:index-out-of-bounds",
                dfsch_make_number_from_long(end));

  if (start > end)
    dfsch_throw("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));

  return dfsch_make_string_buf(s->ptr+start, end-start);

}

size_t dfsch_string_utf8_length(dfsch_object_t* string){
  dfsch_string_t* s = (dfsch_string_t*) string;
  size_t i = 0;
  size_t len = 0;
  size_t state = 0;
  TYPE_CHECK(s, STRING, "string");

  while(i<s->len){
    if((s->ptr[i] & 0x80) == 0){ // U+0000 - U+007F, one byte
      i++;
      len++;
      state = 0;
    }else{
      if ((s->ptr[i] & 0xe0) == 0xc0){ // U+0080 - U+07FF, two bytes
        i++;
        state = 1;
      }else if ((s->ptr[i] & 0xf0) == 0xe0){ // U+0800 - U+FFFF, three bytes
        i++;
        state = 2;
      }else if ((s->ptr[i] & 0xf7) == 0xf0){ // U+10000 - U+10FFFF, four bytes
        i++;
        state = 3;
      }else if ((s->ptr[i] & 0xc0) == 0x80){ // internal byte
        if (state != 0){
          state--;
          if (state == 0) len++;
        }
        i++;
      }else{
        i++;
      }
    }
  }

  return len;
}

uint32_t dfsch_string_utf8_ref(dfsch_object_t* string, size_t index){
  dfsch_string_t* s = (dfsch_string_t*) string;
  size_t i = 0;
  size_t len = 0;
  size_t state = 0;
  uint32_t ch;
  TYPE_CHECK(s, STRING, "string");

  while(i<s->len){
    if((s->ptr[i] & 0x80) == 0){ // U+0000 - U+007F, one byte
      if (len == index){
        return s->ptr[i];
      }
      i++;
      len++;
      state = 0;
    }else{
      if ((s->ptr[i] & 0xe0) == 0xc0){ // U+0080 - U+07FF, two bytes
        ch = s->ptr[i] & 0x1f;
        i++;
        state = 1;
      }else if ((s->ptr[i] & 0xf0) == 0xe0){ // U+0800 - U+FFFF, three bytes
        ch = s->ptr[i] & 0xf;
        i++;
        state = 2;
      }else if ((s->ptr[i] & 0xf7) == 0xf0){ // U+10000 - U+10FFFF, four bytes
        ch = s->ptr[i] & 0x7;
        i++;
        state = 3;
      }else if ((s->ptr[i] & 0xc0) == 0x80){ // internal byte
        if (state != 0){
          state--;
          ch = (ch << 6) | s->ptr[i] & 0x3f;
          if (state == 0){ 
            if (len == index){
              return ch;
            }
            len++;
          }
        }
        i++;
      }else{
        i++;
      }
    }
  }

  dfsch_throw("exception:index-out-of-bounds",
              dfsch_make_number_from_long(index));
}

uint32_t dfsch_string_utf8_substring(dfsch_object_t* string, size_t start,
                                     size_t end){
  dfsch_string_t* s = (dfsch_string_t*) string;
  size_t i = 0;
  size_t len = 0;
  size_t state = 0;
  char* sptr = NULL;
  char* eptr = NULL;
  uint32_t ch;
  TYPE_CHECK(s, STRING, "string");

  if (start > end)
    dfsch_throw("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));

  while(i<s->len){
    if((s->ptr[i] & 0x80) == 0){ // U+0000 - U+007F, one byte
      if (len == start){
        sptr = s->ptr+i;
      }
      if (len == end){
        eptr = s->ptr+i;
        break;
      }
      i++;
      len++;
      state = 0;
    }else{
      if ((s->ptr[i] & 0xe0) == 0xc0){ // U+0080 - U+07FF, two bytes
        if (len == start){
          sptr = s->ptr+i;
        }
        if (len == end){
          eptr = s->ptr+i;
          break;
        }
        i++;
        state = 1;
      }else if ((s->ptr[i] & 0xf0) == 0xe0){ // U+0800 - U+FFFF, three bytes
        if (len == start){
          sptr = s->ptr+i;
        }
        if (len == end){
          eptr = s->ptr+i;
          break;
        }
        i++;
        state = 2;
      }else if ((s->ptr[i] & 0xf7) == 0xf0){ // U+10000 - U+10FFFF, four bytes
        if (len == start){
          sptr = s->ptr+i;
        }
        if (len == end){
          eptr = s->ptr+i;
          break;
        }
        i++;
        state = 3;
      }else if ((s->ptr[i] & 0xc0) == 0x80){ // internal byte
        i++;
        if (state != 0){
          state--;
          if (state == 0){ 
            len++;
          }
        }
      }else{
        i++;
      }
    }
  }

  if (len == end && !eptr)
    eptr = s->ptr + s->len;

  if (!eptr) // sptr must be non-null when eptr is non-null
    dfsch_throw("exception:index-out-of-bounds",
                dfsch_make_number_from_long(end));

  return dfsch_make_string_buf(sptr, eptr-sptr);
  
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
  size_t index;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, index);

  return dfsch_make_number_from_long(dfsch_string_ref(string, index));

}
static object_t* native_string_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);

  return dfsch_make_number_from_long(dfsch_string_length(string));
}

static object_t* native_string_utf8_ref(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t index;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, index);

  return dfsch_make_number_from_long(dfsch_string_utf8_ref(string, index));

}
static object_t* native_string_utf8_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);

  return dfsch_make_number_from_long(dfsch_string_utf8_length(string));
}

static object_t* native_string_cmp_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);

  return dfsch_bool(((int (*)(object_t*,object_t*)) baton)(a, b));
}
static object_t* native_substring(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);

  return dfsch_string_substring(string, start, end);

}
static object_t* native_utf8_substring(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);

  return dfsch_string_utf8_substring(string, start, end);

}


void dfsch__string_native_register(dfsch_ctx_t *ctx){

  dfsch_ctx_define(ctx, "string-append", 
		   dfsch_make_primitive(&native_string_append,NULL));
  dfsch_ctx_define(ctx, "substring", 
		   dfsch_make_primitive(&native_substring,NULL));
  dfsch_ctx_define(ctx, "utf8-substring", 
		   dfsch_make_primitive(&native_utf8_substring,NULL));
  dfsch_ctx_define(ctx, "string-ref", 
		   dfsch_make_primitive(&native_string_ref,NULL));
  dfsch_ctx_define(ctx, "string-utf8-ref", 
		   dfsch_make_primitive(&native_string_utf8_ref,NULL));
  dfsch_ctx_define(ctx, "string-length", 
		   dfsch_make_primitive(&native_string_length,NULL));
  dfsch_ctx_define(ctx, "string-utf8-length", 
		   dfsch_make_primitive(&native_string_utf8_length,NULL));

  dfsch_ctx_define(ctx, "string=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_eq_p));
  dfsch_ctx_define(ctx, "string<?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_lt_p));
  dfsch_ctx_define(ctx, "string>?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_gt_p));
  dfsch_ctx_define(ctx, "string<=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_lte_p));
  dfsch_ctx_define(ctx, "string>=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_gte_p));

  dfsch_ctx_define(ctx, "string-ci=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_eq_p));
  dfsch_ctx_define(ctx, "string-ci<?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_lt_p));
  dfsch_ctx_define(ctx, "string-ci>?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_gt_p));
  dfsch_ctx_define(ctx, "string-ci<=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_lte_p));
  dfsch_ctx_define(ctx, "string-ci>=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_gte_p));

}
