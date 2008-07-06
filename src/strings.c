/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   String handling
 * Copyright (C) 2005-2008 Ales Hakl
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

#include <dfsch/strings.h>
#include <dfsch/number.h>
#include "types.h"
#include <string.h>

#include "udata.h"

typedef struct dfsch_string_t {
  dfsch_type_t* type;
  char* ptr;
  size_t len;
} dfsch_string_t;

int string_equal_p(dfsch_string_t* a, dfsch_string_t* b){
  if (a->len != b->len)
    return 0;

  return memcmp(a->ptr, b->ptr, a->len) == 0;
}

/*
 * 0 - literal character
 * 1 - hexadecimal escape
 * charater - escape with given character
 */
char escape_table[] = {
  /* 0 */   1, 1, 1, 1, 1, 1, 1, 'a', 'b', 't', 'n', 'v', 'f', 'r', 1, 1, 
  /* 1 */   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  /* 2 */   0, 0,'\"', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 3 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 4 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 5 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\\', 0, 0, 0, 
  /* 6 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 7 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 8 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* upper half */
  /* 9 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* a */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* b */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* c */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* d */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* e */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* f */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
};

char hex_table[] = "0123456789abcdef";

char* string_write(dfsch_string_t* o, int max_depth, int readable){
  char *b;
  char *i;
  int j;
  size_t len = 0;

  if (!readable){
    return o->ptr;
  }

  for (j = 0; j < o->len; ++j){
    switch (escape_table[o->ptr[j]]){
    case 0:
      len += 1;
      break;
    case 1:
      len += 4;
      break;
    default:
      len += 2;
      break;
    }
  }

  b = GC_MALLOC_ATOMIC(len+3);
  i = b;
 
  *i='"';
  i++;

  for (j = 0; j < o->len; ++j){
    switch (escape_table[(unsigned char)(o->ptr[j])]){
    case 0:
      *i = o->ptr[j];
      i++;
      break;
    case 1:
      i[0] = '\\';
      i[1] = 'x';
      i[2] = hex_table[(((unsigned char)o->ptr[j]) >> 4) & 0xf];
      i[3] = hex_table[(((unsigned char)o->ptr[j])     ) & 0xf];
      i += 4;
      break;
    default:
      i[0] = '\\';
      i[1] = escape_table[(unsigned char)(o->ptr[j])];
      i += 2;
      break;
    }
  }


  *i='"';
  i[1]=0;

  return b;

}

static size_t string_hash(dfsch_string_t* s){
  size_t ret = s->len;
  size_t i;

  for (i = 0; i < s->len; i++){
    ret ^= s->ptr[i] ^ (ret << 7);
    ret ^= ((size_t)s->ptr[i] << 23) ^ (ret >> 7);
  }

  return ret;
}

static dfsch_type_t string_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_string_t),
  "string",
  (dfsch_type_equal_p_t)string_equal_p,
  (dfsch_type_write_t)string_write,
  NULL,
  (dfsch_type_hash_t)string_hash
};
#define STRING (&string_type)

int dfsch_string_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == &string_type;
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

  if(ptr) // For allocating space to be used later
    memcpy(s->ptr, ptr, len);

  s->ptr[len] = 0;

  return (dfsch_object_t*)s;
}
dfsch_object_t* dfsch_make_string_nocopy(dfsch_strbuf_t* buf){
  dfsch_string_t *s = 
    (dfsch_string_t*)dfsch_make_object(&string_type);

  s->ptr = buf->ptr;
  s->len = buf->len;

  return (dfsch_object_t*)s;
}
char* dfsch_string_to_cstr(dfsch_object_t* obj){
  TYPE_CHECK(obj, STRING, "string");

  return ((dfsch_string_t*)obj)->ptr;
}
char* dfsch_string_or_symbol_to_cstr(dfsch_object_t* obj){
  if (dfsch_symbol_p(obj)){
    return dfsch_symbol(obj);
  }

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

  TYPE_CHECK(a_, STRING, "string");
  TYPE_CHECK(b_, STRING, "string");

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
    dfsch_error("exception:index-out-of-bounds",
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
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(end));

  if (start > end)
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));

  return dfsch_make_string_buf(s->ptr+start, end-start);

}

dfsch_object_t* dfsch_string_2_list(dfsch_object_t* string){

  dfsch_string_t* s = (dfsch_string_t*) string;
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  size_t i;

  TYPE_CHECK(s, STRING, "string");

  if (s->len == 0)
    return NULL;

  head = tail = dfsch_cons(dfsch_make_number_from_long(s->ptr[0]), 
                           NULL);

  for(i = 1; i < s->len; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(dfsch_make_number_from_long(s->ptr[i]),NULL);
    DFSCH_FAST_CDR(tail) = tmp;
    tail = tmp;

  }
  return head;
}

dfsch_object_t* dfsch_list_2_string(dfsch_object_t* list){
  dfsch_string_t* string;
  dfsch_object_t* j = list;
  size_t i=0;
  string = 
    (dfsch_string_t*)dfsch_make_string_buf(NULL,
                                           dfsch_list_length_check(list));
  
  while (dfsch_pair_p((object_t*)j)){
    string->ptr[i] = dfsch_number_to_long(DFSCH_FAST_CAR(j));
    j = DFSCH_FAST_CDR(j);
    i++;
  }

  return (object_t*)string;
}

// UTF-8 support

static char* next_char(char* p, char *e){
  p++;
  while (p < e){
    if ((*p & 0xc0) != 0x80){
      return p;
    }
    p++;
  }
  return NULL;
}

static uint32_t get_char(char* p, char* e){
  uint32_t ch;
  int i;
  if ((*p & 0x80) == 0){ /* one byte */
    return *p;
  }
  if ((*p & 0xe0) == 0xc0){ /* two bytes */
    ch = *p & 0x1f;
    ch <<= 6;
    p++;
    if (p >= e || (*p & 0xc0) != 0x80){
      return 0xfffd; /* REPLACEMENT CHARACTER */
    }
    ch |= *p & 0x3f;
    if (ch < 0x80 && ch != 0){ 
      /* Special case for overlong NUL, which is arguably good thing */
      return 0xfffd; /* REPLACEMENT CHARACTER */
    }
    return ch;
  }
  if ((*p & 0xf0) == 0xe0){ /* three bytes */
    ch = *p & 0x1f;
    for (i = 0; i < 2; i++){
      ch <<= 6;
      p++;
      if (p >= e || (*p & 0xc0) != 0x80){
        return 0xfffd; /* REPLACEMENT CHARACTER */
      }
      ch |= *p & 0x3f;
    }
    if (ch < 0x800){
      return 0xfffd; /* REPLACEMENT CHARACTER */
    }
    return ch;
  }
  if ((*p & 0xf8) == 0xf0){ /* four bytes */
    ch = *p & 0x1f;
    for (i = 0; i < 3; i++){
      ch <<= 6;
      p++;
      if (p >= e || (*p & 0xc0) != 0x80){
        return 0xfffd; /* REPLACEMENT CHARACTER */
      }
      ch |= *p & 0x3f;
    }
    if (ch > 0x10ffff){
      return 0xfffd; /* REPLACEMENT CHARACTER */
    }
    return ch;
  }
  return 0xfffd; /* REPLACEMENT CHARACTER */
}




size_t dfsch_string_utf8_length(dfsch_object_t* string){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  size_t l = 0;

  if (buf->len == 0){
    return 0;
  }

  while (i){
    l++;
    i = next_char(i, e);
  }

  return l;
}

uint32_t dfsch_string_utf8_ref(dfsch_object_t* string, size_t index){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  size_t l = 0;

  if (buf->len == 0){
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(index));
  }

  while (i){
    if (l == index){
      return get_char(i, e);
    }

    l++;
    i = next_char(i, e);
  }

  return l;
}

dfsch_object_t* dfsch_string_substring_utf8(dfsch_object_t* string,
                                            size_t start, size_t end){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  size_t l = 0;
  char* sp = NULL;
  char* ep = NULL;

  if (start > end)
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));

  if (buf->len == 0){
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));
  }

  while (i){
    if (l == start){
      sp = i;
    }
    if (l == end){
      ep = i;
    }
    l++;
    i = next_char(i, e);
  }
  if (end == l){
    ep = e;
  }

  return dfsch_make_string_buf(sp, ep - sp);

}
typedef struct utf8_list_ctx_t{
  dfsch_object_t* head;
  dfsch_object_t* tail;
} utf8_list_ctx_t;

static int utf8_list_cb(uint32_t ch, utf8_list_ctx_t* c, 
                        size_t start, size_t end){
  if (c->head){
    dfsch_object_t* tmp;
    tmp = dfsch_cons(dfsch_make_number_from_long(ch), NULL);
    DFSCH_FAST_CDR(c->tail) = tmp;
    c->tail = tmp;
  }else{
    c->head = c->tail = dfsch_cons(dfsch_make_number_from_long(ch), 
                                   NULL);
  }
  return 0;
}
dfsch_object_t* dfsch_string_utf8_2_list(dfsch_object_t* string){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  dfsch_object_t* head;
  dfsch_object_t* tail;
  

  if (buf->len == 0){
    return 0;
  }

  while (i){
    dfsch_object_t* tmp;
    tmp = dfsch_cons(dfsch_make_number_from_long(get_char(i, e)), 
                     NULL);
    if (head){
      DFSCH_FAST_CDR(tail) = tmp;
      tail = tmp;
    }else{
      head = tail = tmp;
    }

    i = next_char(i, e);
  }
  
  return head;           
}

dfsch_object_t* dfsch_list_2_string_utf8(dfsch_object_t* list){
  
  dfsch_string_t* string;
  dfsch_object_t* j = list;
  size_t i=0;
  size_t len;
  if (list && !dfsch_pair_p(list))
    dfsch_error("exception:not-a-list",list);
  
  len = 0;
  while (dfsch_pair_p((object_t*)j)){
    uint32_t ch = dfsch_number_to_long(DFSCH_FAST_CAR(j));
    if (ch <= 0x7f){
      len += 1;
    } else if (ch <= 0x7ff) {
      len += 2;
    } else if (ch <= 0xffff) {
      len += 3;
    } else if (ch <= 0x10ffff){
      len += 4;
    } else {
      dfsch_error("exception:invalid-unicode-character", DFSCH_FAST_CAR(j));
    }
    j = DFSCH_FAST_CDR(j);
  }

  string = (dfsch_string_t*)dfsch_make_string_buf(NULL, len);

  j = list;
  while (dfsch_pair_p((object_t*)j)){
    uint32_t ch = dfsch_number_to_long(DFSCH_FAST_CAR(j));

    if (ch <= 0x7f){
      string->ptr[i] = ch;
      i += 1;
    } else if (ch <= 0x7ff) {
      string->ptr[i] = 0xc0 | ((ch >> 6) & 0x1f); 
      string->ptr[i+1] = 0x80 | (ch & 0x3f);
      i += 2;
    } else if (ch <= 0xffff) {
      string->ptr[i] = 0xe0 | ((ch >> 12) & 0x0f); 
      string->ptr[i+1] = 0x80 | ((ch >> 6) & 0x3f);
      string->ptr[i+2] = 0x80 | (ch & 0x3f);
      i += 3;
    } else {
      string->ptr[i] = 0xf0 | ((ch >> 18) & 0x07); 
      string->ptr[i+1] = 0x80 | ((ch >> 12) & 0x3f);
      string->ptr[i+2] = 0x80 | ((ch >> 6) & 0x3f);
      string->ptr[i+3] = 0x80 | (ch & 0x3f);
      i += 4;
    } 

    j = DFSCH_FAST_CDR(j);
  }

  return (object_t*)string;
}

uint32_t dfsch_char_downcase(uint32_t c){
  return UDATA_ENTRY(c).lower_offset + c;
}
uint32_t dfsch_char_upcase(uint32_t c){
  return UDATA_ENTRY(c).upper_offset + c;
}
uint32_t dfsch_char_titlecase(uint32_t c){
  return UDATA_ENTRY(c).title_offset + c;
}

char* dfsch_char_category(uint32_t c){
  return UDATA_ENTRY(c).category;
}

/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////


DFSCH_DEFINE_PRIMITIVE(string_append, 0){ 
  
  return dfsch_string_list_append(args);

}
DFSCH_DEFINE_PRIMITIVE(string_ref, 0){
  size_t index;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, index);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long((unsigned char)dfsch_string_ref(string, index));

}
DFSCH_DEFINE_PRIMITIVE(string_length, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_length(string));
}
DFSCH_DEFINE_PRIMITIVE(string_utf8_ref, 0){
  size_t index;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, index);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_utf8_ref(string, index));

}
DFSCH_DEFINE_PRIMITIVE(string_utf8_length, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_utf8_length(string));
}
DFSCH_DEFINE_PRIMITIVE(string_2_list, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_list(string);
}
DFSCH_DEFINE_PRIMITIVE(string_utf8_2_list, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_utf8_2_list(string);
}
DFSCH_DEFINE_PRIMITIVE(list_2_string, 0){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_string(list);
}
DFSCH_DEFINE_PRIMITIVE(list_2_string_utf8, 0){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_string_utf8(list);
}
DFSCH_PRIMITIVE_HEAD(string_cmp_p){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  DFSCH_ARG_END(args);

  return dfsch_bool(((int (*)(object_t*,object_t*)) baton)(a, b));
}
DFSCH_DEFINE_PRIMITIVE(substring, 0){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);
  DFSCH_ARG_END(args);

  return dfsch_string_substring(string, start, end);

}
DFSCH_DEFINE_PRIMITIVE(substring_utf8, 0){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);
  DFSCH_ARG_END(args);

  return dfsch_string_substring_utf8(string, start, end);

}

DFSCH_DEFINE_PRIMITIVE(char_downcase, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_make_number_from_long(dfsch_char_downcase(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_upcase, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_make_number_from_long(dfsch_char_upcase(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_titlecase, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_make_number_from_long(dfsch_char_titlecase(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_category, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_make_string_cstr(dfsch_char_category(ch));
}



void dfsch__string_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<string>", &string_type);


  dfsch_define_cstr(ctx, "string-append", 
		   DFSCH_PRIMITIVE_REF(string_append));
  dfsch_define_cstr(ctx, "substring-bytes", 
		   DFSCH_PRIMITIVE_REF(substring));
  dfsch_define_cstr(ctx, "substring", 
		   DFSCH_PRIMITIVE_REF(substring_utf8));
  dfsch_define_cstr(ctx, "string-byte-res", 
		   DFSCH_PRIMITIVE_REF(string_ref));
  dfsch_define_cstr(ctx, "string-ref", 
		   DFSCH_PRIMITIVE_REF(string_utf8_ref));
  dfsch_define_cstr(ctx, "string-byte-length", 
		   DFSCH_PRIMITIVE_REF(string_length));
  dfsch_define_cstr(ctx, "string-length", 
		   DFSCH_PRIMITIVE_REF(string_utf8_length));
  dfsch_define_cstr(ctx, "string->byte-list", 
		   DFSCH_PRIMITIVE_REF(string_2_list));
  dfsch_define_cstr(ctx, "string->list", 
		   DFSCH_PRIMITIVE_REF(string_utf8_2_list));
  dfsch_define_cstr(ctx, "byte-list->string", 
		   DFSCH_PRIMITIVE_REF(list_2_string));
  dfsch_define_cstr(ctx, "list->string", 
		   DFSCH_PRIMITIVE_REF(list_2_string_utf8));


  dfsch_define_cstr(ctx, "string=?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_eq_p));
  dfsch_define_cstr(ctx, "string<?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_lt_p));
  dfsch_define_cstr(ctx, "string>?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_gt_p));
  dfsch_define_cstr(ctx, "string<=?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_lte_p));
  dfsch_define_cstr(ctx, "string>=?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_gte_p));

  dfsch_define_cstr(ctx, "string-ci=?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_ci_eq_p));
  dfsch_define_cstr(ctx, "string-ci<?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_ci_lt_p));
  dfsch_define_cstr(ctx, "string-ci>?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_ci_gt_p));
  dfsch_define_cstr(ctx, "string-ci<=?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_ci_lte_p));
  dfsch_define_cstr(ctx, "string-ci>=?", 
		   dfsch_make_primitive(&p_string_cmp_p_impl,
                                        &dfsch_string_ci_gte_p));

  dfsch_define_cstr(ctx, "char-upcase", 
		   DFSCH_PRIMITIVE_REF(char_upcase));
  dfsch_define_cstr(ctx, "char-downcase", 
		   DFSCH_PRIMITIVE_REF(char_downcase));
  dfsch_define_cstr(ctx, "char-titlecase", 
		   DFSCH_PRIMITIVE_REF(char_titlecase));
  dfsch_define_cstr(ctx, "char-category", 
		   DFSCH_PRIMITIVE_REF(char_category));

}
