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
  dfsch_strbuf_t buf;
} dfsch_string_t;

int string_equal_p(dfsch_string_t* a, dfsch_string_t* b){
  if (a->buf.len != b->buf.len)
    return 0;

  return memcmp(a->buf.ptr, b->buf.ptr, a->buf.len) == 0;
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
  /* c */   1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* d */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* e */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* f */   0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
};

char hex_table[] = "0123456789abcdef";

void string_write(dfsch_string_t* o, dfsch_writer_state_t* state){
  char *b;
  char *i;
  int j;
  size_t len = 0;

  if (dfsch_writer_state_print_p(state)){
    dfsch_write_string(state, o->buf.ptr);
    return;
  }

  for (j = 0; j < o->buf.len; ++j){
    switch (escape_table[o->buf.ptr[j]]){
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

  for (j = 0; j < o->buf.len; ++j){
    switch (escape_table[(unsigned char)(o->buf.ptr[j])]){
    case 0:
      *i = o->buf.ptr[j];
      i++;
      break;
    case 1:
      i[0] = '\\';
      i[1] = 'x';
      i[2] = hex_table[(((unsigned char)o->buf.ptr[j]) >> 4) & 0xf];
      i[3] = hex_table[(((unsigned char)o->buf.ptr[j])     ) & 0xf];
      i += 4;
      break;
    default:
      i[0] = '\\';
      i[1] = escape_table[(unsigned char)(o->buf.ptr[j])];
      i += 2;
      break;
    }
  }

  *i='"';
  i[1]=0;

  dfsch_write_string(state, b);
}

static size_t string_hash(dfsch_string_t* s){
  size_t ret = s->buf.len;
  size_t i;

  for (i = 0; i < s->buf.len; i++){
    ret ^= s->buf.ptr[i] ^ (ret << 7);
    ret ^= ((size_t)s->buf.ptr[i] << 23) ^ (ret >> 7);
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
  dfsch_string_t *s = GC_MALLOC_ATOMIC(sizeof(dfsch_string_t)+len+1);

  s->type = &string_type;

  s->buf.ptr = (char *)(s + 1);
  s->buf.len = len;

  if(ptr) // For allocating space to be used later
    memcpy(s->buf.ptr, ptr, len);

  s->buf.ptr[len] = 0;

  return (dfsch_object_t*)s;
}
dfsch_object_t* dfsch_make_string_nocopy(dfsch_strbuf_t* buf){
  dfsch_string_t *s = 
    (dfsch_string_t*)dfsch_make_object(&string_type);

  s->buf.ptr = buf->ptr;
  s->buf.len = buf->len;

  return (dfsch_object_t*)s;
}
char* dfsch_string_to_cstr(dfsch_object_t* obj){
  TYPE_CHECK(obj, STRING, "string");

  return ((dfsch_string_t*)obj)->buf.ptr;
}
char* dfsch_string_or_symbol_to_cstr(dfsch_object_t* obj){
  if (dfsch_symbol_p(obj)){
    return dfsch_symbol(obj);
  }

  TYPE_CHECK(obj, STRING, "string");

  return ((dfsch_string_t*)obj)->buf.ptr;
}
dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj){
  TYPE_CHECK(obj, STRING, "string");

  return &(((dfsch_string_t*)obj)->buf);  
}

int dfsch_string_cmp(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  if (a->len != b->len){
    size_t l = (a->len < b->len) ? a->len : b->len;
    int r = memcmp(a->ptr, b->ptr, l);
    if (r == 0){
      return (a->len < b->len) ? -1 : 1;
    }else {
      return r;
    }
  }

  return memcmp(a->ptr, b->ptr, a->len);
}

int dfsch_string_eq_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  if (a->len != b->len)
      return 0;

  return memcmp(a->ptr, b->ptr, a->len) == 0;
}
int dfsch_string_lt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp(a, b) < 0;
}
int dfsch_string_gt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp(a, b) > 0;
}
int dfsch_string_lte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp(a, b) <= 0;
}
int dfsch_string_gte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp(a, b) >= 0;
}

dfsch_object_t* dfsch_string_list_append(dfsch_object_t* list){
  object_t* i = list;
  size_t len = 0;
  dfsch_string_t *r = (dfsch_string_t*)dfsch_make_object(STRING);
  char* ptr;

  while (i){
    dfsch_string_t *s = (dfsch_string_t*)dfsch_car(i);

    TYPE_CHECK(s, STRING, "string");
    
    len += s->buf.len;

    i = dfsch_cdr(i);
  }

  r->buf.len = len;
  r->buf.ptr = ptr = GC_MALLOC_ATOMIC(len+1);

  i = list;

  while (i){
    dfsch_string_t *s = (dfsch_string_t*)dfsch_car(i);

    memcpy(ptr, s->buf.ptr, s->buf.len);
    ptr += s->buf.len;

    i = dfsch_cdr(i);
  }
  
  *ptr = 0;

  return (dfsch_object_t*)r;
}

char dfsch_string_ref(dfsch_object_t* string, size_t index){
  dfsch_string_t* s = (dfsch_string_t*) string;
  TYPE_CHECK(s, STRING, "string");

  if (index >= s->buf.len)
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(index));

  return s->buf.ptr[index];
}

size_t dfsch_string_length(dfsch_object_t* string){
  dfsch_string_t* s = (dfsch_string_t*) string;
  TYPE_CHECK(s, STRING, "string");

  return s->buf.len;
}

dfsch_object_t* dfsch_string_substring(dfsch_object_t* string, size_t start,
                                       size_t end){
  dfsch_string_t* s = (dfsch_string_t*) string;
  TYPE_CHECK(s, STRING, "string");

  if (end > s->buf.len)
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(end));

  if (start > end)
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));

  return dfsch_make_string_buf(s->buf.ptr+start, end-start);
}

dfsch_object_t* dfsch_string_2_list(dfsch_object_t* string){

  dfsch_string_t* s = (dfsch_string_t*) string;
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  size_t i;

  TYPE_CHECK(s, STRING, "string");

  if (s->buf.len == 0)
    return NULL;

  head = tail = dfsch_cons(DFSCH_MAKE_FIXNUM(s->buf.ptr[0]), 
                           NULL);

  for(i = 1; i < s->buf.len; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(DFSCH_MAKE_FIXNUM(s->buf.ptr[i]),NULL);
    DFSCH_FAST_CDR_MUT(tail) = tmp;
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
  
  while (DFSCH_PAIR_P((object_t*)j)){
    string->buf.ptr[i] = dfsch_number_to_long(DFSCH_FAST_CAR(j));
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
dfsch_object_t* dfsch_string_utf8_2_list(dfsch_object_t* string){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  dfsch_object_t* head;
  dfsch_object_t* tail;
  
  head = NULL;

  if (buf->len == 0){
    return 0;
  }

  while (i){
    dfsch_object_t* tmp;
    tmp = dfsch_cons(dfsch_make_number_from_long(get_char(i, e)), 
                     NULL);
    if (head){
      DFSCH_FAST_CDR_MUT(tail) = tmp;
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
      string->buf.ptr[i] = ch;
      i += 1;
    } else if (ch <= 0x7ff) {
      string->buf.ptr[i] = 0xc0 | ((ch >> 6) & 0x1f); 
      string->buf.ptr[i+1] = 0x80 | (ch & 0x3f);
      i += 2;
    } else if (ch <= 0xffff) {
      string->buf.ptr[i] = 0xe0 | ((ch >> 12) & 0x0f); 
      string->buf.ptr[i+1] = 0x80 | ((ch >> 6) & 0x3f);
      string->buf.ptr[i+2] = 0x80 | (ch & 0x3f);
      i += 3;
    } else {
      string->buf.ptr[i] = 0xf0 | ((ch >> 18) & 0x07); 
      string->buf.ptr[i+1] = 0x80 | ((ch >> 12) & 0x3f);
      string->buf.ptr[i+2] = 0x80 | ((ch >> 6) & 0x3f);
      string->buf.ptr[i+3] = 0x80 | (ch & 0x3f);
      i += 4;
    } 

    j = DFSCH_FAST_CDR(j);
  }

  return (object_t*)string;
}

char* dfsch_char_encode(uint32_t c){
  char* ptr = GC_MALLOC_ATOMIC(5);
  if (c <= 0x7f){
    ptr[0] = c;
    ptr[1] = '\0';
  } else if (c <= 0x7ff) {
    ptr[0] = 0xc0 | ((c >> 6) & 0x1f); 
    ptr[1] = 0x80 | (c & 0x3f);
    ptr[2] = '\0';
  } else if (c <= 0xffff) {
    ptr[0] = 0xe0 | ((c >> 12) & 0x0f); 
    ptr[1] = 0x80 | ((c >> 6) & 0x3f);
    ptr[2] = 0x80 | (c & 0x3f);
    ptr[3] = '\0';
  } else {
    ptr[0] = 0xf0 | ((c >> 18) & 0x07); 
    ptr[1] = 0x80 | ((c >> 12) & 0x3f);
    ptr[2] = 0x80 | ((c >> 6) & 0x3f);
    ptr[3] = 0x80 | (c & 0x3f);
    ptr[4] = '\0';
  }   
  return ptr;
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

int dfsch_char_alphabetic_p(uint32_t c){
  return dfsch_char_category(c)[0] == 'L';
}
int dfsch_char_numeric_p(uint32_t c){
  return dfsch_char_category(c)[0] == 'N';
}
int dfsch_char_whitespace_p(uint32_t c){
  if (c == 13 || c == 10 || c == 9){ 
    /* CR, LF and TAB, which are in category Cc */
    return 1;
  }
  return dfsch_char_category(c)[0] == 'Z';
}
int dfsch_char_decimal_p(uint32_t c){
  return strcmp(dfsch_char_category(c), "Nd") == 0;
}
int dfsch_char_upper_case_p(uint32_t c){
  return strcmp(dfsch_char_category(c), "Lu") == 0;
}
int dfsch_char_lower_case_p(uint32_t c){
  return strcmp(dfsch_char_category(c), "Ll") == 0;
}
int dfsch_char_mark_p(uint32_t c){
  return dfsch_char_category(c)[0] == 'M';
}


#define UPCASE    0
#define DOWNCASE  1
#define TITLECASE 2

static dfsch_object_t* string_case(dfsch_object_t* s, int m){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(s);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  dfsch_string_t* string;
  int f;
  size_t j = 0;
  size_t len = 0;
  
  if (buf->len == 0){
    return dfsch_make_string_buf(NULL, 0);
  }

  len = 0;
  f = 1;
  while (i){
    uint32_t ch = get_char(i, e);
    switch (m) {
    case UPCASE:
      ch = dfsch_char_upcase(ch);
      break;
    case DOWNCASE:
      ch = dfsch_char_downcase(ch);
      break;
    case TITLECASE:
      if (dfsch_char_alphabetic_p(ch)){
        if (f){
          f = 0;
          ch = dfsch_char_titlecase(ch);
        }
      } else {
        if (!dfsch_char_mark_p(ch)){
          f = 1;
        }
      }
      break;
    }

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

    i = next_char(i, e);
  }

  string = (dfsch_string_t*)dfsch_make_string_buf(NULL, len);

  i = buf->ptr;
  j = 0;
  f = 1;
  while (i){
    uint32_t ch = get_char(i, e);
    switch (m) {
    case UPCASE:
      ch = dfsch_char_upcase(ch);
      break;
    case DOWNCASE:
      ch = dfsch_char_downcase(ch);
      break;
    case TITLECASE:
      if (dfsch_char_alphabetic_p(ch)){
        if (f){
          f = 0;
          ch = dfsch_char_titlecase(ch);
        }
      } else {
        if (!dfsch_char_mark_p(ch)){
          f = 1;
        }
      }
      break;
    }

    if (ch <= 0x7f){
      string->buf.ptr[j] = ch;
      j += 1;
    } else if (ch <= 0x7ff) {
      string->buf.ptr[j] = 0xc0 | ((ch >> 6) & 0x1f); 
      string->buf.ptr[j+1] = 0x80 | (ch & 0x3f);
      j += 2;
    } else if (ch <= 0xffff) {
      string->buf.ptr[j] = 0xe0 | ((ch >> 12) & 0x0f); 
      string->buf.ptr[j+1] = 0x80 | ((ch >> 6) & 0x3f);
      string->buf.ptr[j+2] = 0x80 | (ch & 0x3f);
      j += 3;
    } else {
      string->buf.ptr[j] = 0xf0 | ((ch >> 18) & 0x07); 
      string->buf.ptr[j+1] = 0x80 | ((ch >> 12) & 0x3f);
      string->buf.ptr[j+2] = 0x80 | ((ch >> 6) & 0x3f);
      string->buf.ptr[j+3] = 0x80 | (ch & 0x3f);
      j += 4;
    } 

    i = next_char(i, e);
  }

  return (object_t*)string;
}

dfsch_object_t* dfsch_string_upcase(dfsch_object_t* s){
  return string_case(s, UPCASE);
}
dfsch_object_t* dfsch_string_downcase(dfsch_object_t* s){
  return string_case(s, DOWNCASE);
}
dfsch_object_t* dfsch_string_titlecase(dfsch_object_t* s){
  return string_case(s, TITLECASE);
}


/* 
 * I assume that this is rougly the thing that R5RS means by case 
 * insensitive comparison. Which does not necessary mean that this is useful.
 */
int dfsch_string_cmp_ci(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  char* ai = a->ptr;
  char* ae = a->ptr + a->len;
  char* bi = b->ptr;
  char* be = b->ptr + b->len;
  uint32_t ac;
  uint32_t bc;

  while (ai && bi){
    ac = dfsch_char_upcase(get_char(ai, ae));
    bc = dfsch_char_upcase(get_char(bi, be));

    if (ac < bc){
      return -1;
    }
    if (ac > bc){
      return 1;
    }

    ai = next_char(ai, ae);
    bi = next_char(bi, be);
  }

  if (ai) {
    return 1;
  } else if (bi) {
    return -1;
  } else {
    return 0;
  }
}

int dfsch_string_ci_eq_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp_ci(a, b) == 0;
}
int dfsch_string_ci_lt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp_ci(a, b) < 0;
}
int dfsch_string_ci_gt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp_ci(a, b) > 0;
}
int dfsch_string_ci_lte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp_ci(a, b) <= 0;
}
int dfsch_string_ci_gte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b){
  return dfsch_string_cmp_ci(a, b) >= 0;
}

static int search_impl(char* ni, char* ne,  char* hi, char* he){
  int pos = 0;

  if (ni == ne){
    return 0;
  }

  while ((he - hi) >= (ne - ni)){
    if (memcmp(ni, hi, ne - ni) == 0){
      return pos;
    }
    hi = next_char(hi, he);
    pos++;
  }
  
  return -1;  
}

int dfsch_string_search(dfsch_strbuf_t* n, dfsch_strbuf_t* h){
  return search_impl(n->ptr, n->ptr + n->len, h->ptr, h->ptr + h->len);
}

static int ci_prefix_p(char* ai, char* ae, char* bi, char* be){
  uint32_t ac;
  uint32_t bc;

  while (ai && bi){
    ac = dfsch_char_upcase(get_char(ai, ae));
    bc = dfsch_char_upcase(get_char(bi, be));

    if (ac != bc){
      return 0;
    }

    ai = next_char(ai, ae);
    bi = next_char(bi, be);
  }
  if (ai){
    return 0;
  } else {
    return 1;
  }
}

static int search_ci_impl(char* ni, char* ne,  char* hi, char* he){
  int pos = 0;

  if (ni == ne){
    return 0;
  }

  while (hi){
    if (ci_prefix_p(ni, ne, hi, he)){
      return pos;
    }
    hi = next_char(hi, he);
    pos++;
  }
  
  return -1;
}

int dfsch_string_search_ci(dfsch_strbuf_t* n, 
                           dfsch_strbuf_t* h){
  return search_ci_impl(n->ptr, n->ptr + n->len, h->ptr, h->ptr + h->len);
}

dfsch_object_t* dfsch_string_split(dfsch_strbuf_t* str,
                                   dfsch_strbuf_t* separator,
                                   int max_parts,
                                   int case_sensitive,
                                   int preserve_empty){
}

dfsch_object_t* dfsch_string_split_byte(dfsch_strbuf_t* str,
                                        dfsch_strbuf_t* separator,
                                        int max_parts,
                                        int preserve_empty){
  char* i = str->ptr;
  char* e = str->ptr + str->len;
  char* l = i;
  int c = 1;

  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;
  dfsch_object_t* tmp;

  if (max_parts != -1 && max_parts < 2){
    dfsch_error("Invalid maximal number of string parts", NULL);
  }

  while (i != e){
    if (memchr(separator->ptr, *i, separator->len) != NULL){
      if (i != l || preserve_empty) {
        tmp = dfsch_cons(dfsch_make_string_buf(l, i - l), NULL);
        if (head){
          DFSCH_FAST_CDR_MUT(tail) = tmp;
          tail = tmp;
        } else {
          head = tail = tmp;
        }
      }
      i++;
      l = i;
      c++;
      if (c == max_parts){
        break;
      }
    } else {
      i++;
    }
  }

  if (l != e || preserve_empty){
    tmp = dfsch_cons(dfsch_make_string_buf(l, e - l), NULL);
    if (head){
      DFSCH_FAST_CDR_MUT(tail) = tmp;
      tail = tmp;
    } else {
      head = tail = tmp;
    }
  }
  return head;
}

static int contains_char(dfsch_strbuf_t* str, uint32_t ch){
  char* i = str->ptr;
  char* e = str->ptr + str->len;

  while (i){
    if (ch == get_char(i, e)){
      return 1;
    }
    i = next_char(i, e);
  }
  return 0;
}

dfsch_object_t* dfsch_string_split_char(dfsch_strbuf_t* str,
                                        dfsch_strbuf_t* separator,
                                        int max_parts,
                                        int preserve_empty){
  char* i = str->ptr;
  char* e = str->ptr + str->len;
  char* l = i;
  int c = 1;

  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;
  dfsch_object_t* tmp;

  if (max_parts != -1 && max_parts < 2){
    dfsch_error("Invalid maximal number of string parts", NULL);
  }  

  while (i){
    if (contains_char(separator, get_char(i, e))){
      if (i != l || preserve_empty) {
        tmp = dfsch_cons(dfsch_make_string_buf(l, i - l), NULL);
        if (head){
          DFSCH_FAST_CDR_MUT(tail) = tmp;
          tail = tmp;
        } else {
          head = tail = tmp;
        }
      }
      i = next_char(i, e);
      l = i;
      c++;
      if (c == max_parts){
        break;
      }
    } else {
      i = next_char(i, e);
    }
  }
  
  if (l != e || preserve_empty){
    tmp = dfsch_cons(dfsch_make_string_buf(l, e - l), NULL);
    if (head){
      DFSCH_FAST_CDR_MUT(tail) = tmp;
      tail = tmp;
    } else {
      head = tail = tmp;
    }
  }
  return head;
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
  dfsch_strbuf_t* a;
  dfsch_strbuf_t* b;

  DFSCH_BUFFER_ARG(args, a);
  DFSCH_BUFFER_ARG(args, b);
  DFSCH_ARG_END(args);

  return dfsch_bool(((int (*)(dfsch_strbuf_t*,dfsch_strbuf_t*)) baton)(a, b));
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

DFSCH_DEFINE_PRIMITIVE(char_alphabetic_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_alphabetic_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_numeric_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_numeric_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_whitespace_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_whitespace_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_upper_case_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_upper_case_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_lower_case_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_lower_case_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_decimal_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_decimal_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_mark_p, 0){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);

  return dfsch_bool(dfsch_char_mark_p(ch));
}


DFSCH_DEFINE_PRIMITIVE(string_upcase, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_upcase(string);
}
DFSCH_DEFINE_PRIMITIVE(string_downcase, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_downcase(string);
}
DFSCH_DEFINE_PRIMITIVE(string_titlecase, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_titlecase(string);
}

DFSCH_DEFINE_PRIMITIVE(string_search, 0){
  dfsch_strbuf_t* needle;
  dfsch_strbuf_t* haystack;

  DFSCH_BUFFER_ARG(args, needle);
  DFSCH_BUFFER_ARG(args, haystack);
  DFSCH_ARG_END(args);

  return DFSCH_MAKE_FIXNUM(dfsch_string_search(needle, haystack));
}
DFSCH_DEFINE_PRIMITIVE(string_search_ci, 0){
  dfsch_strbuf_t* needle;
  dfsch_strbuf_t* haystack;

  DFSCH_BUFFER_ARG(args, needle);
  DFSCH_BUFFER_ARG(args, haystack);
  DFSCH_ARG_END(args);

  return DFSCH_MAKE_FIXNUM(dfsch_string_search_ci(needle, haystack));
}

DFSCH_DEFINE_PRIMITIVE(string_split_on_byte, 
                       "Split string into parts separated by "
                       "bytes from separator set"){
  dfsch_strbuf_t* string;
  dfsch_strbuf_t* separator;
  int max_parts;
  dfsch_object_t* preserve_empty;

  DFSCH_BUFFER_ARG(args, string);
  DFSCH_BUFFER_ARG(args, separator);
  DFSCH_LONG_ARG_OPT(args, max_parts, -1);
  DFSCH_OBJECT_ARG_OPT(args, preserve_empty, NULL);
  DFSCH_ARG_END(args);

  return dfsch_string_split_byte(string, separator, max_parts, 
                                 preserve_empty != NULL);
}
DFSCH_DEFINE_PRIMITIVE(string_split_on_character, 
                       "Split string into parts separated by "
                       "unicode characters from separator set"){
  dfsch_strbuf_t* string;
  dfsch_strbuf_t* separator;
  int max_parts;
  dfsch_object_t* preserve_empty;

  DFSCH_BUFFER_ARG(args, string);
  DFSCH_BUFFER_ARG(args, separator);
  DFSCH_LONG_ARG_OPT(args, max_parts, -1);
  DFSCH_OBJECT_ARG_OPT(args, preserve_empty, NULL);
  DFSCH_ARG_END(args);

  return dfsch_string_split_char(string, separator, max_parts, 
                                 preserve_empty != NULL);
}



void dfsch__string_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<string>", &string_type);

  dfsch_define_cstr(ctx, "string-append", 
		   DFSCH_PRIMITIVE_REF(string_append));
  dfsch_define_cstr(ctx, "substring-bytes", 
		   DFSCH_PRIMITIVE_REF(substring));
  dfsch_define_cstr(ctx, "substring", 
		   DFSCH_PRIMITIVE_REF(substring_utf8));
  dfsch_define_cstr(ctx, "string-byte-ref", 
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

  dfsch_define_cstr(ctx, "char-alphabetic?", 
		   DFSCH_PRIMITIVE_REF(char_alphabetic_p));
  dfsch_define_cstr(ctx, "char-numeric?", 
		   DFSCH_PRIMITIVE_REF(char_numeric_p));
  dfsch_define_cstr(ctx, "char-whitespace?", 
		   DFSCH_PRIMITIVE_REF(char_whitespace_p));
  dfsch_define_cstr(ctx, "char-lower-case?", 
		   DFSCH_PRIMITIVE_REF(char_lower_case_p));
  dfsch_define_cstr(ctx, "char-upper-case?", 
		   DFSCH_PRIMITIVE_REF(char_upper_case_p));
  dfsch_define_cstr(ctx, "char-decimal?", 
		   DFSCH_PRIMITIVE_REF(char_decimal_p));
  dfsch_define_cstr(ctx, "char-mark?", 
		   DFSCH_PRIMITIVE_REF(char_mark_p));

  dfsch_define_cstr(ctx, "string-upcase", 
		   DFSCH_PRIMITIVE_REF(string_upcase));
  dfsch_define_cstr(ctx, "string-downcase", 
		   DFSCH_PRIMITIVE_REF(string_downcase));
  dfsch_define_cstr(ctx, "string-titlecase", 
		   DFSCH_PRIMITIVE_REF(string_titlecase));

  dfsch_define_cstr(ctx, "string-search", 
		   DFSCH_PRIMITIVE_REF(string_search));
  dfsch_define_cstr(ctx, "string-search-ci", 
		   DFSCH_PRIMITIVE_REF(string_search_ci));

  dfsch_define_cstr(ctx, "string-split-on-byte", 
		   DFSCH_PRIMITIVE_REF(string_split_on_byte));
  dfsch_define_cstr(ctx, "string-split-on-character", 
		   DFSCH_PRIMITIVE_REF(string_split_on_character));
}
