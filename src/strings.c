/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   String handling
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <dfsch/strings.h>
#include <dfsch/number.h>
#include <dfsch/serdes.h>
#include "types.h"
#include <string.h>

#include "udata.h"
#include "util.h"

dfsch_strbuf_t dfsch_empty_strbuf = {
  .len = 0,
  .ptr = NULL
};


static void * internal_memrchr(const void *buf, int c, size_t num){
  unsigned char *pMem = (unsigned char *) buf + num;
  
  for (;;) {
    if (num-- == 0) {
      return NULL;
    }
    
    if (*pMem-- == (unsigned char) c) {
      break;
    }
    
  }
  
  return (void *) (pMem + 1);
}

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
static char escape_table[] = {
  /* 0 */   1, 1, 1, 1, 1, 1, 1, 'a', 'b', 't', 'n', 'v', 'f', 'r', 1, 1, 
  /* 1 */   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  /* 2 */   0, 0,'\"', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 3 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 4 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 5 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\\', 0, 0, 0, 
  /* 6 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* 7 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, /* DEL */ 
  /* 8 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* upper half */
  /* 9 */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* a */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* b */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* c */   1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* d */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* e */   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  /* f */   0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
};

static char hex_table[] = "0123456789abcdef";

static void string_write(dfsch_string_t* o, dfsch_writer_state_t* state){
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

static void string_serialize(dfsch_string_t* str, dfsch_serializer_t* se){
  dfsch_serialize_stream_symbol(se, "string");
  dfsch_serialize_string(se, str->buf.ptr, str->buf.len);
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("string", string){
  dfsch_object_t* str = dfsch_make_string_nocopy(dfsch_deserialize_strbuf(ds));
  dfsch_deserializer_put_partial_object(ds, str);
  return str;
}


static dfsch_collection_methods_t string_collection = {
  .get_iterator = dfsch_string_2_list,
};

static dfsch_object_t* string_ref(dfsch_object_t* string, int k){
  return DFSCH_MAKE_CHARACTER(dfsch_string_ref(string, k));
}

static dfsch_sequence_methods_t string_sequence = {
  .ref = string_ref,
  .length = dfsch_string_length,
};

dfsch_type_t dfsch_proto_string_type = {
  .type = DFSCH_ABSTRACT_TYPE,
  .superclass = NULL,
  .name = "proto-string",
};

dfsch_type_t dfsch_string_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_PROTO_STRING_TYPE,
  sizeof(dfsch_string_t),
  "string",
  (dfsch_type_equal_p_t)string_equal_p,
  (dfsch_type_write_t)string_write,
  NULL,
  (dfsch_type_hash_t)string_hash,

  .collection = &string_collection,
  .sequence = &string_sequence,
  .serialize = &string_serialize,
};
#define STRING (&dfsch_string_type)

int dfsch_string_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj) == STRING;
}

int dfsch_proto_string_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, STRING);
}

dfsch_strbuf_t* dfsch_strbuf_create(char* ptr, size_t len){
  dfsch_strbuf_t *sb = GC_NEW(dfsch_strbuf_t);

  sb->ptr = ptr;
  sb->len = len;

  return sb;
}

dfsch_strbuf_t* dfsch_alloc_strbuf(size_t len){
  dfsch_strbuf_t* sb = dfsch_strbuf_create(GC_MALLOC_ATOMIC(len+1), len);
  sb->ptr[len] = '\0';
  return sb;
}

dfsch_strbuf_t* dfsch_copy_strbuf(dfsch_strbuf_t* sb){
  return dfsch_strbuf_create(sb->ptr, sb->len);
}
ssize_t dfsch_strbuf_inputproc(dfsch_strbuf_t* strbuf, 
                               char* buf, size_t len){
  if (len > strbuf->len){
    len = strbuf->len;
  }
  memcpy(buf, strbuf->ptr, len);
  strbuf->len -= len;
  strbuf->ptr += len;
  return len;
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

  s->type = DFSCH_STRING_TYPE;

  s->buf.ptr = (char *)(s + 1);
  s->buf.len = len;

  if(ptr) // For allocating space to be used later
    memcpy(s->buf.ptr, ptr, len);

  s->buf.ptr[len] = 0;

  return (dfsch_object_t*)s;
}

dfsch_object_t* dfsch_make_string_for_write(size_t len, char**buf){
  dfsch_string_t *s = GC_MALLOC_ATOMIC(sizeof(dfsch_string_t)+len+1);

  s->type = DFSCH_STRING_TYPE;

  s->buf.ptr = (char *)(s + 1);
  s->buf.len = len;

  (*buf) = s->buf.ptr;

  s->buf.ptr[len] = 0;

  return (dfsch_object_t*)s;
}

dfsch_object_t* dfsch_make_string_nocopy(dfsch_strbuf_t* buf){
  dfsch_string_t *s = 
    (dfsch_string_t*)dfsch_make_object(DFSCH_STRING_TYPE);

  s->buf.ptr = buf->ptr;
  s->buf.len = buf->len;

  return (dfsch_object_t*)s;
}
char* dfsch_string_to_cstr(dfsch_object_t* obj){
  dfsch_string_t* s = DFSCH_ASSERT_TYPE(obj, STRING);

  return s->buf.ptr;
}
char* dfsch_proto_string_to_cstr(dfsch_object_t* obj){
  dfsch_strbuf_t* sb = dfsch_string_to_buf(obj);
  char* res = GC_MALLOC_ATOMIC(sb->len + 1);
  memcpy(res, sb->ptr, sb->len);
  res[sb->len] = '\0';
  return res;
}

char* dfsch_string_or_symbol_to_cstr(dfsch_object_t* obj){
  dfsch_string_t* s;
  if (dfsch_symbol_p(obj)){
    return dfsch_symbol(obj);
  }

  s = DFSCH_ASSERT_TYPE(obj, STRING);

  return s->buf.ptr;
}
dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj){
  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(obj, DFSCH_PROTO_STRING_TYPE);

  return &(s->buf);  
}
dfsch_strbuf_t* dfsch_byte_vector_to_buf(dfsch_object_t* obj){
  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(obj, DFSCH_BYTE_VECTOR_TYPE);

  return &(s->buf);  
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

  while (DFSCH_PAIR_P(i)){
    dfsch_string_t *s = DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(i),
                                              DFSCH_PROTO_STRING_TYPE);

    
    len += s->buf.len;

    i = DFSCH_FAST_CDR(i);
  }

  r->buf.len = len;
  r->buf.ptr = ptr = GC_MALLOC_ATOMIC(len+1);

  i = list;

  while (DFSCH_PAIR_P(i)){
    dfsch_string_t *s = DFSCH_ASSERT_INSTANCE(DFSCH_FAST_CAR(i),
                                              DFSCH_PROTO_STRING_TYPE);

    memcpy(ptr, s->buf.ptr, s->buf.len);
    ptr += s->buf.len;

    i = DFSCH_FAST_CDR(i);
  }
  
  *ptr = 0;

  return (dfsch_object_t*)r;
}

char dfsch_string_byte_ref(dfsch_object_t* string, size_t index){
  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(string, DFSCH_PROTO_STRING_TYPE);

  if (index >= s->buf.len)
    dfsch_error("Index out of bounds",
                dfsch_make_number_from_long(index));

  return s->buf.ptr[index];
}

size_t dfsch_string_byte_length(dfsch_object_t* string){
  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(string, DFSCH_PROTO_STRING_TYPE);

  return s->buf.len;
}

dfsch_object_t* dfsch_string_byte_substring(dfsch_object_t* string, 
                                            ssize_t start,
                                            ssize_t end){
  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(string, DFSCH_PROTO_STRING_TYPE);

  if (start < 0){
    start = s->buf.len + start + 1;
  }
  if (end < 0){
    end = s->buf.len + end + 1;
  }


  if (end > s->buf.len)
    dfsch_error("Index out of bounds",
                dfsch_make_number_from_long(end));

  if (start > end)
    dfsch_error("Index out of bounds",
                dfsch_make_number_from_long(start));

  return dfsch_make_string_buf(s->buf.ptr+start, end-start);
}

dfsch_object_t* dfsch_string_2_byte_list(dfsch_object_t* string){

  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(string, DFSCH_PROTO_STRING_TYPE);
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  size_t i;


  if (s->buf.len == 0)
    return NULL;

  head = tail = dfsch_cons(DFSCH_MAKE_FIXNUM(((unsigned char)s->buf.ptr[0])), 
                           NULL);

  for(i = 1; i < s->buf.len; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(DFSCH_MAKE_FIXNUM(((unsigned char)s->buf.ptr[i])),NULL);
    DFSCH_FAST_CDR_MUT(tail) = tmp;
    tail = tmp;

  }
  return head;
}

dfsch_object_t* dfsch_byte_list_2_string(dfsch_object_t* list){
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

static size_t string_length(char* i, char* e){
  size_t l = 0;

  if (i == e){
    return 0;
  }


  while (i){
    l++;
    i = next_char(i, e);
  }

  return l;
}

size_t dfsch_string_length(dfsch_object_t* string){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  return string_length(buf->ptr, buf->ptr + buf->len);
}

uint32_t dfsch_string_ref(dfsch_object_t* string, size_t index){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  size_t l = 0;

  if (buf->len == 0){
    dfsch_error("Index out of bound",
                dfsch_make_number_from_long(index));
  }

  while (i){
    if (l == index){
      return get_char(i, e);
    }

    l++;
    i = next_char(i, e);
  }

  dfsch_error("Index out of bound",
              dfsch_make_number_from_long(index));
}

dfsch_object_t* dfsch_string_substring(dfsch_object_t* string,
                                       ssize_t start, ssize_t end){
  dfsch_strbuf_t* buf = dfsch_string_to_buf(string);
  char* i = buf->ptr;
  char* e = buf->ptr + buf->len;
  size_t l = 0;
  char* sp = NULL;
  char* ep = NULL;

  if (start < 0 || end < -1){
    size_t len = dfsch_string_length(string);
    if (start < 0){
      start = len + start + 1;
    }
    if (end < 0){
      end = len + end + 1;
    }
  }

  if (start > end && end != -1)
    dfsch_error("Index out of bounds",
                dfsch_make_number_from_long(start));

  if (buf->len == 0){
    dfsch_error("Index out of bounds",
                dfsch_make_number_from_long(start));
  }

  while (i){
    if (l == start){
      sp = i;
      if (end == -1){
        ep = buf->ptr + buf->len; 
      }
    }
    if (l == end){
      break;
    }
    l++;
    i = next_char(i, e);
  }
  if (end == l){
    if (i) {
      ep = i;
    } else {
      ep = e;
    }
  }
  if (start == l){
    sp = ep;
  }

  if (!sp){
    dfsch_error("Index out of bounds", 
                dfsch_make_number_from_long(start));
  }
  if (!ep){
    dfsch_error("Index out of bounds", 
                dfsch_make_number_from_long(end));
  }

  return dfsch_make_string_buf(sp, ep - sp);
}
dfsch_object_t* dfsch_string_2_list(dfsch_object_t* string){
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
    tmp = dfsch_cons(DFSCH_MAKE_CHARACTER(get_char(i, e)), 
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

dfsch_object_t* dfsch_list_2_string(dfsch_object_t* list){
  
  dfsch_string_t* string;
  dfsch_object_t* j = list;
  size_t i=0;
  size_t len;
  if (list && !dfsch_pair_p(list))
    dfsch_error("Not a list",list);
  
  len = 0;
  while (dfsch_pair_p((object_t*)j)){
    uint32_t ch = dfsch_character(DFSCH_FAST_CAR(j));
    if (ch <= 0x7f){
      len += 1;
    } else if (ch <= 0x7ff) {
      len += 2;
    } else if (ch <= 0xffff) {
      len += 3;
    } else if (ch <= 0x10ffff){
      len += 4;
    } else {
      dfsch_error("Invalid unicode character", DFSCH_FAST_CAR(j));
    }
    j = DFSCH_FAST_CDR(j);
  }

  string = (dfsch_string_t*)dfsch_make_string_buf(NULL, len);

  j = list;
  while (dfsch_pair_p((object_t*)j)){
    uint32_t ch = dfsch_character(DFSCH_FAST_CAR(j));

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
      dfsch_error("Invalid unicode character", DFSCH_MAKE_FIXNUM(ch));
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

static char* skip_chars(char* i, char* e, int count){
  while (count && i){
    count--;
    i = next_char(i, e);
  }
  return i;
}

dfsch_object_t* dfsch_string_split(dfsch_strbuf_t* str,
                                   dfsch_strbuf_t* separator,
                                   int max_parts,
                                   int case_sensitive,
                                   int preserve_empty){
  char* i = str->ptr;
  char* e = str->ptr + str->len;
  char* l = i;
  int c = 1;
  int pos;
  int sep_len = string_length(separator->ptr, 
                              separator->ptr + separator->len);

  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;
  dfsch_object_t* tmp;

  if (max_parts != -1 && max_parts < 2){
    dfsch_error("Invalid maximal number of string parts", NULL);
  }

  while (i) {
    if (case_sensitive){
      pos = search_impl(separator->ptr, separator->ptr + separator->len, i, e);
    } else {
      pos = search_ci_impl(separator->ptr, separator->ptr + separator->len, 
                           i, e);
    }

    if (pos == -1){
      break;
    }

    i = skip_chars(i, e, pos); // XXX: naive, slow, whatever
    if (i != l || preserve_empty) {
      tmp = dfsch_cons(dfsch_make_string_buf(l, i - l), NULL);
      if (head){
        DFSCH_FAST_CDR_MUT(tail) = tmp;
        tail = tmp;
      } else {
        head = tail = tmp;
      }
    }
    i = skip_chars(i, e, sep_len);
    l = i;
    if (!l){
      l = e;
    }
    c++;
    if (c == max_parts){
      break;
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

dfsch_strbuf_t* dfsch_string_replace(dfsch_strbuf_t* str,
                                     dfsch_strbuf_t* from,
                                     dfsch_strbuf_t* to,
                                     int max_matches,
                                     int case_sensitive){
  char* i = str->ptr;
  char* e = str->ptr + str->len;
  char* l = i;
  int c = 0;
  int pos;
  int sep_len = string_length(from->ptr, 
                              from->ptr + from->len);

  str_list_t* sl = sl_create();

  if (max_matches != -1 && max_matches < 1){
    dfsch_error("Invalid maximal number of string parts", NULL);
  }

  while (i) {
    if (case_sensitive){
      pos = search_impl(from->ptr, from->ptr + from->len, i, e);
    } else {
      pos = search_ci_impl(from->ptr, from->ptr + from->len, 
                           i, e);
    }

    if (pos == -1){
      break;
    }

    i = skip_chars(i, e, pos); // XXX: naive, slow, whatever
    if (i != l) {
      sl_nappend(sl, l, i - l);
    }
    i = skip_chars(i, e, sep_len);
    sl_nappend(sl, to->ptr, to->len);
    l = i;
    if (!l){
      l = e;
    }
    c++;
    if (c == max_matches){
      break;
    }
  }
 

  if (l != e){
    sl_nappend(sl, l, e - l);
  }

  return sl_value_strbuf(sl); 
}

dfsch_object_t* dfsch_byte_vector_translate(dfsch_object_t* str,
                                            dfsch_strbuf_t* from,
                                            dfsch_strbuf_t* to){
  dfsch_string_t* res = dfsch_make_string_strbuf(dfsch_string_to_buf(str));
  char* ptr = res->buf.ptr;
  size_t len = res->buf.len;
  char* optr = ptr;
  size_t olen = 0;

  while (len){
    char* found = memchr(from->ptr, *ptr, from->len);

    if (found){
      size_t off = found - from->ptr;
      if (off < to->len){
        *optr = to->ptr[off];
        optr++;
        olen++;
      }
      
    } else {
      olen++;
      optr++;
    }
    len--;
    ptr++;
  }
  
  res->buf.len = olen;

  return res;
}

#define CHAR_DELETE 0xffffffff

static uint32_t translate_char(uint32_t ch,
                               dfsch_strbuf_t* from,
                               dfsch_strbuf_t* to){
  size_t idx = 0;
  char* i = from->ptr;
  char* e = from->ptr + from->len;

  for (;;){
    if (!i || i == e){
      return ch;
    }
    if (get_char(i,e) == ch){
      break;
    }
    i = next_char(i, e);
    idx++;
  }

  i = to->ptr;
  e = to->ptr + to->len;
  
  while (i && i != e){
    if (!idx){
      return get_char(i, e);
    }
    i = next_char(i, e);
    idx--;
  }

  return CHAR_DELETE;
}

dfsch_object_t* dfsch_string_translate(dfsch_object_t* s, 
                                       dfsch_strbuf_t* from,
                                       dfsch_strbuf_t* to){
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

    ch = translate_char(ch, from, to);

    if (ch <= 0x7f){
      len += 1;
    } else if (ch <= 0x7ff) {
      len += 2;
    } else if (ch <= 0xffff) {
      len += 3;
    } else if (ch <= 0x10ffff){
      len += 4;
    } else if (ch != CHAR_DELETE) {
      dfsch_error("Invalid unicode character", DFSCH_MAKE_FIXNUM(ch));
    }

    i = next_char(i, e);
  }

  string = (dfsch_string_t*)dfsch_make_string_buf(NULL, len);

  i = buf->ptr;
  j = 0;
  f = 1;
  while (i){
    uint32_t ch = get_char(i, e);
    ch = translate_char(ch, from, to);

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
    } else if (ch != CHAR_DELETE) {
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


static dfsch_object_t* pathname_dirname(dfsch_object_t* s){
  dfsch_strbuf_t* str = dfsch_string_to_buf(s);
  char *slash = internal_memrchr(str->ptr, '/', str->len);
  if (slash){
    return dfsch_make_string_buf(str->ptr, (slash - str->ptr));
  } else {
    return NULL;
  }
}
static dfsch_object_t* pathname_basename(dfsch_object_t* s){
  dfsch_strbuf_t* str = dfsch_string_to_buf(s);
  char *slash = internal_memrchr(str->ptr, '/', str->len);
  if (slash){
    return dfsch_make_string_buf(slash + 1, str->len - (slash - str->ptr) - 1);
  } else {
    return s;
  }
}
static dfsch_object_t* pathname_filename(dfsch_object_t* s){
  dfsch_strbuf_t* str = dfsch_string_to_buf(s);
  char *slash = internal_memrchr(str->ptr, '/', str->len);
  char *dot;
  char *start;

  if (!slash) {
    start = str->ptr;
  } else {
    start = slash + 1;
  }
  
  dot = internal_memrchr(start, '.', str->len - (start - str->ptr));

  if (dot){
    return dfsch_make_string_buf(start, (dot - start));
  } else {
    if (start == str->ptr){
      return s;
    } else {
      return dfsch_make_string_buf(start, str->len - (start - str->ptr));
    }
  }
}
static dfsch_object_t* pathname_extension(dfsch_object_t* s){
  dfsch_strbuf_t* str = dfsch_string_to_buf(s);
  char *dot ;
  char *start;
  char *slash = internal_memrchr(str->ptr, '/', str->len);

  if (!slash) {
    start = str->ptr;
  } else {
    start = slash + 1;
  }

  dot = internal_memrchr(start, '.', str->len - (start - str->ptr));
  if (dot){
    return dfsch_make_string_buf(dot + 1, str->len - (dot - str->ptr) - 1);
  } else {
    return NULL;
  }
}

/*
 * "Filenames should be construed from portable filename character set"
 *
 * This set consists of upper and lower case ASCII letters, numbers and
 * dot, hypen and underscore. As some filesystems are not case sensitive
 * or even case preserving, and dot has special meaning on some systems,
 * only lowercase letters, numbers and hypen are passed thru unchanged.
 * Underscore is used as escape character (with assumption that, in lisp
 * world, hypens are used more frequently than underscores).
 *
 * Primary motivation of this encoding is to be both unique and easily
 * reversable. Note that there is no function that actualy does the
 * inverse of this :)
 *
 * Empty names are converted to single underscore.
 */

char* dfsch_strbuf_2_safe_filename(dfsch_strbuf_t* buf,
                                   int preserve_uppercase,
                                   int convert_to_hypen){
  size_t res_len = 0;
  char* res;
  char* r;
  char* i = buf->ptr;
  size_t len = buf->len;
  
  if (len == 0){
    return "_";
  }

  while (len){
    if ((*i >= 'a' && *i <= 'z') || (*i >= '0' && *i <= '9' || 
                                     *i == convert_to_hypen) ||
        (preserve_uppercase && (*i >= 'A' && *i <= 'Z'))){
      res_len++;
    } else if (*i == '_'){
      res_len += 2;
    } else {
      res_len += 3;
    }
    i++;
    len--;
  }

  if (res_len > 255){
    dfsch_error("String too long to be converted to safe filename", 
                dfsch_make_string_strbuf(buf));
  }

  r = res = GC_MALLOC_ATOMIC(res_len + 1);
  i = buf->ptr;
  len = buf->len;
  
  while (len){
    if ((*i >= 'a' && *i <= 'z') || (*i >= '0' && *i <= '9') ||
        (preserve_uppercase && (*i >= 'A' && *i <= 'Z'))){
      *r = *i;
      r++;
    } else if (*i == convert_to_hypen){
      *r = '-';
      r++;
    } else if (*i == '_'){
      *r = '_';
      r++;
      *r = '_';
      r++;
    } else {
      *r = '_';
      r++;
      *r = hex_table[(*i >> 4) & 0xf];
      r++;
      *r = hex_table[*i & 0xf];
      r++;
    }
    i++;
    len--;
  }

  *r = '\0';

  return res;
}

char* dfsch_strbuf_2_hexstring(dfsch_strbuf_t* buf){
  size_t len = buf->len;
  char* res = GC_MALLOC(buf->len * 2 + 1);
  char* r = res;
  char* i = buf->ptr;

  while (len){
    *r = hex_table[(*i >> 4) & 0xf];
    r++;
    *r = hex_table[*i & 0xf];
    r++;
    len--;
    i++;
  }
  
  *r = '\0';
  return res;
}

dfsch_strbuf_t* dfsch_hexstring_2_strbuf(char* str){
  size_t len = strlen(str);
  char* i = str;
  char* r;
  dfsch_strbuf_t* res;
  char tmp;
  int j;

  if (len % 2 != 0){
    dfsch_error("Length of hexadecimal string is not even",
                dfsch_make_string_cstr(str));
  }

  len /= 2;
  r = GC_MALLOC(len + 1);
  
  res = dfsch_strbuf_create(r, len);

  while (len){
    tmp = 0;
    for (j = 0; j < 2; j++){
      tmp <<= 4;
      if (*i >= '0' && *i <= '9'){
        tmp |= *i - '0';
      } else if (*i >= 'a' && *i <= 'f'){
        tmp |= 10 + *i - 'a';
      } else if (*i >= 'A' && *i <= 'F'){
        tmp |= 10 + *i - 'a';
      } else {
        dfsch_error("Invalid hexadecimal character",
                    DFSCH_MAKE_FIXNUM(*i));
      }
      i++;
    }
    *r = tmp;
    r++;
    len--;
  }

  *r = '\0';
  return res;
}

/********************* Byte vectors **************/

int byte_vector_equal_p(dfsch_string_t* a, dfsch_string_t* b){
  if (a->buf.len != b->buf.len)
    return 0;

  return memcmp(a->buf.ptr, b->buf.ptr, a->buf.len) == 0;
}

static void byte_vector_write(dfsch_string_t* o, dfsch_writer_state_t* state){
  char *b;
  char *i;
  int j;
  size_t len = 0;

  if (dfsch_writer_state_print_p(state)){
    dfsch_write_string(state, o->buf.ptr);
    return;
  }

  for (j = 0; j < o->buf.len; ++j){
    switch ((unsigned char)(o->buf.ptr[j]) < 128
            ? escape_table[(unsigned char)(o->buf.ptr[j])]
            : 1){
    case 0:
      len += 1;
      break;
    default:
      len += 4;
      break;
    }
  }

  b = GC_MALLOC_ATOMIC(len+4);
  i = b;
 
  *i='#';
  i++;
  *i='"';
  i++;

  for (j = 0; j < o->buf.len; ++j){
    switch ((unsigned char)(o->buf.ptr[j]) < 128
            ? escape_table[(unsigned char)(o->buf.ptr[j])]
            : 1){
    case 0:
      *i = o->buf.ptr[j];
      i++;
      break;
    default:
      i[0] = '\\';
      i[1] = 'x';
      i[2] = hex_table[(((unsigned char)o->buf.ptr[j]) >> 4) & 0xf];
      i[3] = hex_table[(((unsigned char)o->buf.ptr[j])     ) & 0xf];
      i += 4;
    }
  }

  *i='"';
  i[1]=0;

  dfsch_write_string(state, b);
}

static size_t byte_vector_hash(dfsch_string_t* s){
  size_t ret = s->buf.len ^ 0xa5a5a5a5;
  size_t i;

  for (i = 0; i < s->buf.len; i++){
    ret ^= s->buf.ptr[i] ^ (ret << 7);
    ret ^= ((size_t)s->buf.ptr[i] << 23) ^ (ret >> 7);
  }

  return ret;
}

static void byte_vector_serialize(dfsch_string_t* str, dfsch_serializer_t* se){
  dfsch_serialize_stream_symbol(se, "byte-vector");
  dfsch_serialize_string(se, str->buf.ptr, str->buf.len);
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("byte-vector", byte_vector){
  dfsch_strbuf_t* sb = dfsch_deserialize_strbuf(ds);
  dfsch_object_t* str = dfsch_make_byte_vector_nocopy(sb->ptr, sb->len);
  dfsch_deserializer_put_partial_object(ds, str);
  return str;
}

static dfsch_collection_methods_t byte_vector_collection = {
  .get_iterator = dfsch_string_2_byte_list,
};

static dfsch_object_t* byte_vector_seq_ref(dfsch_string_t* bv,
                                           size_t k){
  k = DFSCH_ASSERT_SEQUENCE_INDEX(bv, k, bv->buf.len);
  return DFSCH_MAKE_FIXNUM((unsigned char)bv->buf.ptr[k]);
}
static void byte_vector_seq_set(dfsch_string_t* bv,
                                size_t k,
                                dfsch_object_t* v) {
  k = DFSCH_ASSERT_SEQUENCE_INDEX(bv, k, bv->buf.len);
  bv->buf.ptr[k] = dfsch_number_to_long(v);
}

static dfsch_sequence_methods_t byte_vector_sequence = {
  .ref = byte_vector_seq_ref,
  .set = byte_vector_seq_set,
  .length = dfsch_string_byte_length,
};

dfsch_type_t dfsch_byte_vector_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = DFSCH_PROTO_STRING_TYPE,
  .name = "byte-vector",
  .size = sizeof(dfsch_string_t),

  .equal_p = (dfsch_type_equal_p_t)byte_vector_equal_p,
  .write = (dfsch_type_write_t)byte_vector_write,
  .hash = (dfsch_type_hash_t)byte_vector_hash,

  .collection = &byte_vector_collection,
  .sequence = &byte_vector_sequence,
  .serialize = byte_vector_serialize,
};

dfsch_object_t* dfsch_make_byte_vector(char* ptr, size_t len){
  dfsch_string_t *s = GC_MALLOC_ATOMIC(sizeof(dfsch_string_t)+len);

  s->type = DFSCH_BYTE_VECTOR_TYPE;

  s->buf.ptr = (char *)(s + 1);
  s->buf.len = len;

  if (ptr) // For allocating space to be used later
    memcpy(s->buf.ptr, ptr, len);

  return (dfsch_object_t*)s;
}
dfsch_object_t* dfsch_alloc_byte_vector(char** ptr, size_t len){
  dfsch_string_t *s = GC_MALLOC_ATOMIC(sizeof(dfsch_string_t)+len);

  s->type = DFSCH_BYTE_VECTOR_TYPE;

  s->buf.ptr = (char *)(s + 1);
  s->buf.len = len;

  if (ptr){
    *ptr = s->buf.ptr;
  }

  return (dfsch_object_t*)s;
}
dfsch_object_t* dfsch_make_byte_vector_strbuf(dfsch_strbuf_t* strbuf){
  return dfsch_make_byte_vector(strbuf->ptr, strbuf->len);
}
dfsch_object_t* dfsch_make_byte_vector_nocopy(char* ptr, size_t len){
  dfsch_string_t* s = dfsch_make_object(DFSCH_BYTE_VECTOR_TYPE);
  
  s->buf.ptr = ptr;
  s->buf.len = len;

  return s;
}

dfsch_object_t* dfsch_list_2_byte_vector(dfsch_object_t* list){
  dfsch_string_t* string;
  dfsch_object_t* j = list;
  size_t i=0;
  string = 
    (dfsch_string_t*)dfsch_make_byte_vector(NULL,
                                            dfsch_list_length_check(list));
  
  while (DFSCH_PAIR_P((object_t*)j)){
    string->buf.ptr[i] = dfsch_number_to_long(DFSCH_FAST_CAR(j));
    j = DFSCH_FAST_CDR(j);
    i++;
  }

  return (object_t*)string;
}

void dfsch_byte_vector_set(dfsch_object_t* bv, size_t k, char b){
  dfsch_string_t* v = DFSCH_ASSERT_INSTANCE(bv, DFSCH_BYTE_VECTOR_TYPE);

  if (k >= v->buf.len){
    dfsch_error("Index out of bounds", bv);
  }

  v->buf.ptr[k] = b;
}
void dfsch_byte_vector_copy(dfsch_object_t* dest,
                            size_t dest_off,
                            dfsch_object_t* src,
                            size_t src_off,
                            size_t len){
  dfsch_strbuf_t* sb = dfsch_string_to_buf(src);
  dfsch_string_t* ds = DFSCH_ASSERT_INSTANCE(dest, DFSCH_BYTE_VECTOR_TYPE);
  
  if (src_off + len > sb->len){
    dfsch_index_error(src, src_off + len, sb->len);
  }

  if (dest_off + len > ds->buf.len){
    dfsch_index_error(dest, dest_off + len, ds->buf.len);
  }

  memcpy(ds->buf.ptr + dest_off, sb->ptr + src_off, len);
}
dfsch_object_t* dfsch_byte_vector_subvector(dfsch_object_t* bv,
                                            size_t off,
                                            size_t len){
  dfsch_string_t* s = DFSCH_ASSERT_INSTANCE(bv, DFSCH_BYTE_VECTOR_TYPE);
  
  if (off + len >= s->buf.len){
    dfsch_index_error(s, off + len, s->buf.len);
  }

  return dfsch_make_byte_vector_nocopy(s->buf.ptr + off, len);
}

dfsch_object_t* dfsch_proto_string_2_string(dfsch_object_t* ps){
  return dfsch_make_string_strbuf(dfsch_string_to_buf(ps));
}
dfsch_object_t* dfsch_proto_string_2_byte_vector(dfsch_object_t* ps){
  return dfsch_make_byte_vector_strbuf(dfsch_string_to_buf(ps));
}

dfsch_object_t* dfsch_string_trim(dfsch_strbuf_t* string,
                                  dfsch_strbuf_t* bag,
                                  int side){
  char* i = string->ptr;
  char* e = i + string->len;
  char* sp = NULL;
  char* ep = NULL;

  if (side == -1){
    sp = i;
  }

  while (i){
    if (side >= 0){
      if (!contains_char(bag, get_char(i, e))){
        sp = i;
        if (side == 1){
          ep = e;
          break;
        }
        side = -1;
      }
    }

    if (side <= 0){
      if (!contains_char(bag, get_char(i, e))){
        ep = next_char(i, e);
        if (!ep){
          ep = e;
          break;
        }
      }      
    }

    i = next_char(i, e);
  }


  return dfsch_make_string_buf(sp, ep - sp);
}
dfsch_object_t* dfsch_byte_vector_trim(dfsch_strbuf_t* string,
                                       dfsch_strbuf_t* bag,
                                       int side){
  char* ptr = string->ptr;
  size_t len = string->len;

  if (side >= 0){
    while (len && memchr(bag->ptr, *ptr, bag->len)){
      ptr++;
      len--;
    }
  }

  if (side <= 0){
    while (len && memchr(bag->ptr, ptr[len-1], bag->len)){
      len--;
    }
  }


  return dfsch_make_byte_vector_nocopy(ptr, len);
}


/* C utilities */

dfsch_object_t* dfsch_string_assoc(dfsch_object_t* alist,
                                   char* string){
  dfsch_object_t* i = alist;

  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* val = dfsch_car(DFSCH_FAST_CAR(i));

    if (DFSCH_INSTANCE_P(val, DFSCH_PROTO_STRING_TYPE)){
      if (strcmp(dfsch_string_to_cstr(val), string) == 0){
        return DFSCH_FAST_CAR(i);
      }
    }

    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Improper alist", alist);
  }
  return NULL;
}


/************** characters ***********/

static void character_write(dfsch_object_t* obj, dfsch_writer_state_t* ws){
  uint32_t ch = DFSCH_SMALL_VALUE_REF(obj);

  if (dfsch_writer_state_print_p(ws)){
    char* res = GC_MALLOC_ATOMIC(5);
    if (ch <= 0x7f){
      res[0] = ch;
      res[1] = 0;
    } else if (ch <= 0x7ff) {
      res[0] = 0xc0 | ((ch >> 6) & 0x1f); 
      res[1] = 0x80 | (ch & 0x3f);
      res[2] = 0;
    } else if (ch <= 0xffff) {
      res[0] = 0xe0 | ((ch >> 12) & 0x0f); 
      res[1] = 0x80 | ((ch >> 6) & 0x3f);
      res[2] = 0x80 | (ch & 0x3f);
      res[3] = 0;
    } else {
      res[0] = 0xf0 | ((ch >> 18) & 0x07); 
      res[1] = 0x80 | ((ch >> 12) & 0x3f);
      res[2] = 0x80 | ((ch >> 6) & 0x3f);
      res[3] = 0x80 | (ch & 0x3f);
      res[4] = 0;
    } 

    dfsch_write_string(ws, res);
  } else if (ch < 0x80 && ch > 0x20){
    dfsch_write_string(ws, dfsch_saprintf("#\\%c", ch));
  } else {
    dfsch_write_string(ws, dfsch_saprintf("#\\u%x", ch));
  }
}
static void character_serialize(dfsch_object_t* obj, dfsch_serializer_t* ser){
  uint32_t ch = DFSCH_SMALL_VALUE_REF(obj);
  dfsch_serialize_stream_symbol(ser, "character");
  dfsch_serialize_integer(ser, ch);
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("character", character){
  return DFSCH_MAKE_CHARACTER(dfsch_deserialize_integer(ds));  
}


dfsch_type_t dfsch_character_type = {
  .type = DFSCH_SPECIAL_TYPE,
  .name = "character",
  .size = 0,
  .write = character_write,
  .serialize = character_serialize,
};

uint32_t dfsch_character(dfsch_object_t* obj){
  dfsch_object_t* ch = DFSCH_ASSERT_TYPE(obj, DFSCH_CHARACTER_TYPE);
  return DFSCH_SMALL_VALUE_REF(ch);
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

  return DFSCH_MAKE_CHARACTER(dfsch_string_ref(string, index));

}
DFSCH_DEFINE_PRIMITIVE(string_length, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_length(string));
}
DFSCH_DEFINE_PRIMITIVE(string_byte_ref, 0){
  size_t index;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, index);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_byte_ref(string, index));

}
DFSCH_DEFINE_PRIMITIVE(string_byte_length, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_byte_length(string));
}
DFSCH_DEFINE_PRIMITIVE(string_2_list, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_list(string);
}
DFSCH_DEFINE_PRIMITIVE(string_2_byte_list, 0){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_byte_list(string);
}
DFSCH_DEFINE_PRIMITIVE(list_2_string, 0){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_string(list);
}
DFSCH_DEFINE_PRIMITIVE(string, 0){
  return dfsch_list_2_string(args);
}
DFSCH_DEFINE_PRIMITIVE(byte_list_2_string, 0){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_byte_list_2_string(list);
}
DFSCH_PRIMITIVE_HEAD(string_cmp_p){
  dfsch_strbuf_t* a;
  dfsch_strbuf_t* b;

  DFSCH_BUFFER_ARG(args, a);
  DFSCH_BUFFER_ARG(args, b);
  DFSCH_ARG_END(args);

  return dfsch_bool(((int (*)(dfsch_strbuf_t*,dfsch_strbuf_t*)) baton)(a, b));
}
DFSCH_DEFINE_PRIMITIVE(byte_substring, 0){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG_OPT(args, end, -1);
  DFSCH_ARG_END(args);

  return dfsch_string_byte_substring(string, start, end);
}
DFSCH_DEFINE_PRIMITIVE(substring, 0){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG_OPT(args, end, -1);
  DFSCH_ARG_END(args);

  return dfsch_string_substring(string, start, end);
}

DFSCH_DEFINE_PRIMITIVE(char_downcase, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return DFSCH_MAKE_CHARACTER(dfsch_char_downcase(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_upcase, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return DFSCH_MAKE_CHARACTER(dfsch_char_upcase(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_titlecase, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return DFSCH_MAKE_CHARACTER(dfsch_char_titlecase(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_category, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_make_string_cstr(dfsch_char_category(ch));
}

DFSCH_DEFINE_PRIMITIVE(char_alphabetic_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_bool(dfsch_char_alphabetic_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_numeric_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_bool(dfsch_char_numeric_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_whitespace_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_bool(dfsch_char_whitespace_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_upper_case_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_bool(dfsch_char_upper_case_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_lower_case_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_bool(dfsch_char_lower_case_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_decimal_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

  return dfsch_bool(dfsch_char_decimal_p(ch));
}
DFSCH_DEFINE_PRIMITIVE(char_mark_p, 0){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);

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

DFSCH_DEFINE_PRIMITIVE(string_split, 
                       "Split string into parts separated by separator"){
  dfsch_strbuf_t* string;
  dfsch_strbuf_t* separator;
  int max_parts;
  dfsch_object_t* case_sensitive;
  dfsch_object_t* preserve_empty;

  DFSCH_BUFFER_ARG(args, string);
  DFSCH_BUFFER_ARG(args, separator);
  DFSCH_LONG_ARG_OPT(args, max_parts, -1);
  DFSCH_OBJECT_ARG_OPT(args, case_sensitive, DFSCH_INVALID_OBJECT);
  DFSCH_OBJECT_ARG_OPT(args, preserve_empty, NULL);
  DFSCH_ARG_END(args);

  return dfsch_string_split(string, separator, max_parts, 
                            case_sensitive != NULL,
                            preserve_empty != NULL);
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
DFSCH_DEFINE_PRIMITIVE(string_replace, 
                       "Replace occurences of FROM in STRING to TO"){
  dfsch_strbuf_t* string;
  dfsch_strbuf_t* from;
  dfsch_strbuf_t* to;
  int max_matches;
  dfsch_object_t* case_sensitive;

  DFSCH_BUFFER_ARG(args, string);
  DFSCH_BUFFER_ARG(args, from);
  DFSCH_BUFFER_ARG(args, to);
  DFSCH_OBJECT_ARG_OPT(args, case_sensitive, DFSCH_INVALID_OBJECT);
  DFSCH_LONG_ARG_OPT(args, max_matches, -1);
  DFSCH_ARG_END(args);

  return dfsch_make_string_nocopy(dfsch_string_replace(string, from, to, 
                                                       max_matches, 
                                                       case_sensitive != NULL));
}


DFSCH_DEFINE_PRIMITIVE(pathname_basename, "Return basename of given pathname"){
  dfsch_object_t* pathname;
  DFSCH_OBJECT_ARG(args, pathname);
  DFSCH_ARG_END(args);
  return pathname_basename(pathname);
}
DFSCH_DEFINE_PRIMITIVE(pathname_dirname, 
                       "Return directory path part of given pathname"){
  dfsch_object_t* pathname;
  DFSCH_OBJECT_ARG(args, pathname);
  DFSCH_ARG_END(args);
  return pathname_dirname(pathname);
}
DFSCH_DEFINE_PRIMITIVE(pathname_filename, 
                       "Return filename without last extension"){
  dfsch_object_t* pathname;
  DFSCH_OBJECT_ARG(args, pathname);
  DFSCH_ARG_END(args);
  return pathname_filename(pathname);
}
DFSCH_DEFINE_PRIMITIVE(pathname_extension, 
                       "Return last extension of given pathname"){
  dfsch_object_t* pathname;
  DFSCH_OBJECT_ARG(args, pathname);
  DFSCH_ARG_END(args);
  return pathname_extension(pathname);
}

DFSCH_DEFINE_PRIMITIVE(make_byte_vector,
                       "Create new empty byte vector"){
  size_t len;
  DFSCH_LONG_ARG(args, len);
  DFSCH_ARG_END(args);

  return dfsch_make_byte_vector(NULL, len);
}

DFSCH_DEFINE_PRIMITIVE(proto_string_2_string,
                       "Copy contents of proto-string into fresh string"){
  dfsch_object_t* ps;
  DFSCH_OBJECT_ARG(args, ps);
  DFSCH_ARG_END(args);
  return dfsch_proto_string_2_string(ps);
}
DFSCH_DEFINE_PRIMITIVE(proto_string_2_byte_vector,
                       "Copy contents of proto-string into fresh byte-vector"){
  dfsch_object_t* ps;
  DFSCH_OBJECT_ARG(args, ps);
  DFSCH_ARG_END(args);
  return dfsch_proto_string_2_byte_vector(ps);
}
DFSCH_DEFINE_PRIMITIVE(list_2_byte_vector, 
                       "Create byte-vector from list"){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_byte_vector(list);
}
DFSCH_DEFINE_PRIMITIVE(copy_into_byte_vector,
                       "Copy consecutive bytes from proto-string to byte-vector"){
  
  dfsch_object_t* destination;
  ssize_t destination_offset = 0;
  dfsch_object_t* source;
  ssize_t source_offset = 0;
  ssize_t length = -1;

  DFSCH_OBJECT_ARG(args, destination);
  DFSCH_OBJECT_ARG(args, source);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("destination-offset", destination_offset,
                        dfsch_number_to_long);
  DFSCH_KEYWORD_GENERIC("source-offset", source_offset,
                        dfsch_number_to_long);
  DFSCH_KEYWORD_GENERIC("length", length,
                        dfsch_number_to_long);
  DFSCH_KEYWORD_PARSER_END(args);

  if (length == -1){
    size_t slen = dfsch_string_length(source);
    size_t dlen = dfsch_string_length(destination);
    if (slen - source_offset > dlen - destination_offset){
      length = dlen - destination_offset;
    } else {
      length = slen - source_offset;      
    }
  }

  dfsch_byte_vector_copy(destination, destination_offset,
                         source, source_offset,
                         length);

  return destination;
}
DFSCH_DEFINE_PRIMITIVE(byte_vector_subvector,
                       "Create byte-vector accessing subsequence of original"){
  dfsch_object_t* original;
  size_t offset;
  size_t length;

  DFSCH_OBJECT_ARG(args, original);
  DFSCH_LONG_ARG(args, offset);
  DFSCH_LONG_ARG(args, length);
  DFSCH_ARG_END(args);

  return dfsch_byte_vector_subvector(original, offset, length);
}
DFSCH_DEFINE_PRIMITIVE(byte_vector_translate, 
                       "Replace bytes in FROM with coresponding bytes in TO"){
  dfsch_object_t* string;
  dfsch_strbuf_t* from;
  dfsch_strbuf_t* to;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_BUFFER_ARG(args, from);
  DFSCH_BUFFER_ARG(args, to);
  DFSCH_ARG_END(args);

  return dfsch_byte_vector_translate(string, from, to);
}
DFSCH_DEFINE_PRIMITIVE(string_translate, 
                       "Replace characters in FROM with coresponding from TO"){
  dfsch_object_t* string;
  dfsch_strbuf_t* from;
  dfsch_strbuf_t* to;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_BUFFER_ARG(args, from);
  DFSCH_BUFFER_ARG(args, to);
  DFSCH_ARG_END(args);

  return dfsch_string_translate(string, from, to);
}

DFSCH_DEFINE_PRIMITIVE(string_starts_with_p,
                       "Does string begin with given substring?"){
  dfsch_strbuf_t* string;
  dfsch_strbuf_t* start;

  DFSCH_BUFFER_ARG(args, string);
  DFSCH_BUFFER_ARG(args, start);
  DFSCH_ARG_END(args);

  if (string->len < start->len){
    return NULL;
  }

  return dfsch_bool(memcmp(string->ptr, start->ptr, start->len) == 0);
}

static char* get_argument(dfsch_object_t* args,
                          char* name){
  if (strspn(name, "0123456789") == strlen(name)){
    size_t idx = atol(name);
    return dfsch_sequence_ref(args, idx);
  } else {
    dfsch_object_t* i = args;
    while (DFSCH_PAIR_P(i)){
      dfsch_object_t* keyword;
      dfsch_object_t* value;
      DFSCH_OBJECT_ARG(i, keyword);
      DFSCH_OBJECT_ARG(i, value);

      if (dfsch_compare_keyword(keyword, name)){
        return value;
      }
    }
    dfsch_error("Key not found", dfsch_make_string_cstr(name));
  }
}

static size_t name_end(char* str, char delim){
  size_t res = 0;
  while (*str){
    switch(*str){
    case ' ':
    case '\t':
    case '\n':
    case ':':
    case '(':
    case ')':
    case '\r':
    case '\f':
    case ';':
      return res;
    }
    if (*str == delim){
      break;
    }
    res++;
    str++;
  }
  return res;
}

DFSCH_DEFINE_PRIMITIVE(construct_string,
                       "Simple templating mechanism"){
  char* query;
  dfsch_object_t* arguments;
  char* pos;
  dfsch_str_list_t* sl;
  char delim = ':';
  dfsch_object_t* convert_non_string = NULL;
  dfsch_object_t* convert_all = NULL;
  DFSCH_STRING_ARG(args, query);
  DFSCH_OBJECT_ARG(args, arguments);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("escape-character", delim, dfsch_number_to_long);
  DFSCH_KEYWORD("convert-non-string", convert_non_string);
  DFSCH_KEYWORD("convert-all", convert_all);
  DFSCH_KEYWORD_PARSER_END(args);

  sl = dfsch_sl_create();

  while (pos = strchr(query, delim)){
    dfsch_sl_nappend(sl, query, pos - query);
    query = pos + 1;
    if (*query == ':'){
      dfsch_sl_append(sl, delim);
      query++;
    } else {
      size_t name_len = name_end(query, delim);
      char* name = dfsch_strancpy(query, name_len);
      dfsch_object_t* obj = get_argument(arguments, name);
      dfsch_strbuf_t* sb;
      query += name_len;
      if (*query == delim){
        query++;
      }

      if (convert_all){
        obj = dfsch_apply(convert_all, dfsch_list(1, obj));
      } else if (convert_non_string && 
                 !DFSCH_INSTANCE_P(obj, DFSCH_PROTO_STRING_TYPE)){
        obj = dfsch_apply(convert_non_string, dfsch_list(1, obj));
      }

      sb = dfsch_string_to_buf(obj);

      dfsch_sl_nappend(sl, sb->ptr, sb->len);
    }
  }
  dfsch_sl_append(sl, query);
  return dfsch_make_string_nocopy(dfsch_sl_value_strbuf(sl));
}

DFSCH_DEFINE_PRIMITIVE(string_join, 
                       "Join elements of string collection by separator"){
  dfsch_object_t* collection;
  dfsch_strbuf_t* separator;
  dfsch_object_t* i;
  dfsch_str_list_t* sl;
  int sep = 0;

  DFSCH_OBJECT_ARG(args, collection);
  DFSCH_BUFFER_ARG_OPT(args, separator, NULL);

  sl = dfsch_sl_create();
  i = dfsch_collection_get_iterator(collection);
  while (i){
    dfsch_strbuf_t* sb = dfsch_string_to_buf(dfsch_iterator_this(i));

    if (separator){
      if (sep){
        dfsch_sl_nappend(sl, separator->ptr, separator->len);        
      } else {
        sep = 1;
      }
    } 

    dfsch_sl_nappend(sl, sb->ptr, sb->len);
    i = dfsch_iterator_next(i); 
  }
  return dfsch_make_string_nocopy(dfsch_sl_value_strbuf(sl));
}

DFSCH_DEFINE_PRIMITIVE(string_2_safe_filename, 
                       NULL){
  dfsch_strbuf_t* string;
  dfsch_object_t* preserve_uppercase;
  int convert_to_hypen;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_OBJECT_ARG_OPT(args, preserve_uppercase, NULL);
  DFSCH_CHARACTER_ARG_OPT(args, convert_to_hypen, '-');
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_strbuf_2_safe_filename(string,
                                                             preserve_uppercase != NULL,
                                                             convert_to_hypen));
}

DFSCH_DEFINE_PRIMITIVE(string_2_hexstring, 
                       NULL){
  dfsch_strbuf_t* string;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_strbuf_2_hexstring(string));
}
DFSCH_DEFINE_PRIMITIVE(hexstring_2_string, NULL){
  char* hex;
  DFSCH_STRING_ARG(args, hex);
  DFSCH_ARG_END(args);

  return dfsch_make_string_nocopy(dfsch_hexstring_2_strbuf(hex));
}
DFSCH_DEFINE_PRIMITIVE(hexstring_2_byte_vector, NULL){
  char* hex;
  DFSCH_STRING_ARG(args, hex);
  DFSCH_ARG_END(args);

  return dfsch_make_byte_vector_strbuf(dfsch_hexstring_2_strbuf(hex));
}

DFSCH_DEFINE_PRIMITIVE(string_trim, 
                       "Remove characters in bag from edges of string"
                       DFSCH_DOC_SYNOPSIS("(bag string)")){
  dfsch_strbuf_t* bag;
  dfsch_strbuf_t* string;
  DFSCH_BUFFER_ARG(args, bag);
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_trim(string, bag, 0);
}
DFSCH_DEFINE_PRIMITIVE(string_trim_left, 
                       "Remove characters in bag from left edge of string"
                       DFSCH_DOC_SYNOPSIS("(bag string)")){
  dfsch_strbuf_t* bag;
  dfsch_strbuf_t* string;
  DFSCH_BUFFER_ARG(args, bag);
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_trim(string, bag, 1);
}
DFSCH_DEFINE_PRIMITIVE(string_trim_right, 
                       "Remove characters in bag from right edge of string"
                       DFSCH_DOC_SYNOPSIS("(bag string)")){
  dfsch_strbuf_t* bag;
  dfsch_strbuf_t* string;
  DFSCH_BUFFER_ARG(args, bag);
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_trim(string, bag, -1);
}

DFSCH_DEFINE_PRIMITIVE(byte_vector_trim, 
                       "Remove bytes in bag from edges of byte vector"
                       DFSCH_DOC_SYNOPSIS("(bag byte-vector)")){
  dfsch_strbuf_t* bag;
  dfsch_strbuf_t* byte_vector;
  DFSCH_BUFFER_ARG(args, bag);
  DFSCH_BUFFER_ARG(args, byte_vector);
  DFSCH_ARG_END(args);

  return dfsch_byte_vector_trim(byte_vector, bag, 0);
}
DFSCH_DEFINE_PRIMITIVE(byte_vector_trim_left, 
                       "Remove bytes in bag from left edge of byte vector"
                       DFSCH_DOC_SYNOPSIS("(bag byte-vector)")){
  dfsch_strbuf_t* bag;
  dfsch_strbuf_t* byte_vector;
  DFSCH_BUFFER_ARG(args, bag);
  DFSCH_BUFFER_ARG(args, byte_vector);
  DFSCH_ARG_END(args);

  return dfsch_byte_vector_trim(byte_vector, bag, 1);
}
DFSCH_DEFINE_PRIMITIVE(byte_vector_trim_right, 
                       "Remove bytes in bag from right edge of byte vector"
                       DFSCH_DOC_SYNOPSIS("(bag byte-vector)")){
  dfsch_strbuf_t* bag;
  dfsch_strbuf_t* byte_vector;
  DFSCH_BUFFER_ARG(args, bag);
  DFSCH_BUFFER_ARG(args, byte_vector);
  DFSCH_ARG_END(args);

  return dfsch_byte_vector_trim(byte_vector, bag, -1);
}

DFSCH_DEFINE_PRIMITIVE(character_2_ordinal, NULL){
  uint32_t ch;
  DFSCH_CHARACTER_ARG(args, ch);
  DFSCH_ARG_END(args);
  return DFSCH_MAKE_FIXNUM(ch);
}
DFSCH_DEFINE_PRIMITIVE(ordinal_2_character, NULL){
  uint32_t ch;
  DFSCH_LONG_ARG(args, ch);
  DFSCH_ARG_END(args);
  
  if (ch < 0 || ch > 0x10ffff){
    dfsch_error("No such character", DFSCH_MAKE_FIXNUM(ch));
  }

  return DFSCH_MAKE_CHARACTER(ch);
}


void dfsch__string_native_register(dfsch_object_t *ctx){
  dfsch_defcanon_cstr(ctx, "<string>", &dfsch_string_type);
  dfsch_defcanon_cstr(ctx, "<proto-string>", DFSCH_PROTO_STRING_TYPE);
  dfsch_defcanon_cstr(ctx, "<byte-vector>", DFSCH_BYTE_VECTOR_TYPE);
  dfsch_defcanon_cstr(ctx, "<character>", DFSCH_CHARACTER_TYPE);

  dfsch_defcanon_cstr(ctx, "string-append", 
		   DFSCH_PRIMITIVE_REF(string_append));
  dfsch_defcanon_cstr(ctx, "byte-substring", 
		   DFSCH_PRIMITIVE_REF(byte_substring));
  dfsch_defcanon_cstr(ctx, "substring", 
		   DFSCH_PRIMITIVE_REF(substring));
  dfsch_defcanon_cstr(ctx, "string-byte-ref", 
		   DFSCH_PRIMITIVE_REF(string_byte_ref));
  dfsch_defcanon_cstr(ctx, "string-ref", 
		   DFSCH_PRIMITIVE_REF(string_ref));
  dfsch_defcanon_cstr(ctx, "string-byte-length", 
		   DFSCH_PRIMITIVE_REF(string_byte_length));
  dfsch_defcanon_cstr(ctx, "string-length", 
		   DFSCH_PRIMITIVE_REF(string_length));
  dfsch_defcanon_cstr(ctx, "string->byte-list", 
		   DFSCH_PRIMITIVE_REF(string_2_byte_list));
  dfsch_defcanon_cstr(ctx, "string->list", 
		   DFSCH_PRIMITIVE_REF(string_2_list));
  dfsch_defcanon_cstr(ctx, "byte-list->string", 
		   DFSCH_PRIMITIVE_REF(byte_list_2_string));
  dfsch_defcanon_cstr(ctx, "list->string", 
		   DFSCH_PRIMITIVE_REF(list_2_string));
  dfsch_defcanon_cstr(ctx, "string", 
		   DFSCH_PRIMITIVE_REF(string));

#define STRING_REL(name, fun, doc)                                      \
  dfsch_defcanon_cstr(ctx, name,                                        \
                      dfsch_make_primitive(name,                        \
                                           &p_string_cmp_p_impl,        \
                                           fun,                         \
                                           doc,                         \
                                           0))         

  STRING_REL("string=?", dfsch_string_eq_p, 
             "Are given strings equal?");
  STRING_REL("string<?", dfsch_string_lt_p, 
             "Are arguments in strictly lexicographically ascending order?");
  STRING_REL("string>?", dfsch_string_gt_p, 
             "Are arguments in strictly lexicographically decending order?");
  STRING_REL("string<=?", dfsch_string_lte_p, 
             "Are arguments in lexicographically ascending order?");
  STRING_REL("string>=?", dfsch_string_gte_p, 
             "Are arguments in lexicographically decending order?");

  STRING_REL("string-ci=?", dfsch_string_ci_eq_p, 
             "Are given strings eqivalent without regard to casing?");
  STRING_REL("string-ci<?", dfsch_string_ci_lt_p, 
             "Are arguments in strictly lexicographically ascending order "
             "(without regard to case)?");
  STRING_REL("string-ci>?", dfsch_string_ci_gt_p, 
             "Are arguments in strictly lexicographically decending order "
             "(without regard to case)?");
  STRING_REL("string-ci<=?", dfsch_string_ci_lte_p, 
             "Are arguments in lexicographically ascending order "
             "(without regard to case)?");
  STRING_REL("string-ci>=?", dfsch_string_ci_gte_p, 
             "Are arguments in lexicographically decending order "
             "(without regard to case)?");


  dfsch_defcanon_cstr(ctx, "char-upcase", 
		   DFSCH_PRIMITIVE_REF(char_upcase));
  dfsch_defcanon_cstr(ctx, "char-downcase", 
		   DFSCH_PRIMITIVE_REF(char_downcase));
  dfsch_defcanon_cstr(ctx, "char-titlecase", 
		   DFSCH_PRIMITIVE_REF(char_titlecase));
  dfsch_defcanon_cstr(ctx, "char-category", 
		   DFSCH_PRIMITIVE_REF(char_category));

  dfsch_defcanon_cstr(ctx, "char-alphabetic?", 
		   DFSCH_PRIMITIVE_REF(char_alphabetic_p));
  dfsch_defcanon_cstr(ctx, "char-numeric?", 
		   DFSCH_PRIMITIVE_REF(char_numeric_p));
  dfsch_defcanon_cstr(ctx, "char-whitespace?", 
		   DFSCH_PRIMITIVE_REF(char_whitespace_p));
  dfsch_defcanon_cstr(ctx, "char-lower-case?", 
		   DFSCH_PRIMITIVE_REF(char_lower_case_p));
  dfsch_defcanon_cstr(ctx, "char-upper-case?", 
		   DFSCH_PRIMITIVE_REF(char_upper_case_p));
  dfsch_defcanon_cstr(ctx, "char-decimal?", 
		   DFSCH_PRIMITIVE_REF(char_decimal_p));
  dfsch_defcanon_cstr(ctx, "char-mark?", 
		   DFSCH_PRIMITIVE_REF(char_mark_p));

  dfsch_defcanon_cstr(ctx, "string-upcase", 
		   DFSCH_PRIMITIVE_REF(string_upcase));
  dfsch_defcanon_cstr(ctx, "string-downcase", 
		   DFSCH_PRIMITIVE_REF(string_downcase));
  dfsch_defcanon_cstr(ctx, "string-titlecase", 
		   DFSCH_PRIMITIVE_REF(string_titlecase));

  dfsch_defcanon_cstr(ctx, "string-search", 
		   DFSCH_PRIMITIVE_REF(string_search));
  dfsch_defcanon_cstr(ctx, "string-search-ci", 
		   DFSCH_PRIMITIVE_REF(string_search_ci));

  dfsch_defcanon_cstr(ctx, "string-split", 
		   DFSCH_PRIMITIVE_REF(string_split));
  dfsch_defcanon_cstr(ctx, "string-split-on-byte", 
		   DFSCH_PRIMITIVE_REF(string_split_on_byte));
  dfsch_defcanon_cstr(ctx, "string-split-on-character", 
		   DFSCH_PRIMITIVE_REF(string_split_on_character));
  dfsch_defcanon_cstr(ctx, "string-replace", 
		   DFSCH_PRIMITIVE_REF(string_replace));

  dfsch_defcanon_cstr(ctx, "pathname-basename", 
		   DFSCH_PRIMITIVE_REF(pathname_basename));
  dfsch_defcanon_cstr(ctx, "pathname-dirname", 
		   DFSCH_PRIMITIVE_REF(pathname_dirname));
  dfsch_defcanon_cstr(ctx, "pathname-filename", 
		   DFSCH_PRIMITIVE_REF(pathname_filename));
  dfsch_defcanon_cstr(ctx, "pathname-extension", 
		   DFSCH_PRIMITIVE_REF(pathname_extension));

  dfsch_defcanon_cstr(ctx, "make-byte-vector", 
		   DFSCH_PRIMITIVE_REF(make_byte_vector));
  dfsch_defcanon_cstr(ctx, "proto-string->string", 
		   DFSCH_PRIMITIVE_REF(proto_string_2_string));
  dfsch_defcanon_cstr(ctx, "proto-string->byte-vector", 
		   DFSCH_PRIMITIVE_REF(proto_string_2_byte_vector));
  dfsch_defcanon_cstr(ctx, "list->byte-vector", 
		   DFSCH_PRIMITIVE_REF(list_2_byte_vector));
  dfsch_defcanon_cstr(ctx, "copy-into-byte-vector", 
		   DFSCH_PRIMITIVE_REF(copy_into_byte_vector));
  dfsch_defcanon_cstr(ctx, "byte-vector-subvector", 
		   DFSCH_PRIMITIVE_REF(byte_vector_subvector));
  dfsch_defcanon_cstr(ctx, "byte-vector-translate", 
		   DFSCH_PRIMITIVE_REF(byte_vector_translate));
  dfsch_defcanon_cstr(ctx, "string-translate", 
		   DFSCH_PRIMITIVE_REF(string_translate));

  dfsch_defcanon_cstr(ctx, "string-starts-with?", 
		   DFSCH_PRIMITIVE_REF(string_starts_with_p));
  dfsch_defcanon_cstr(ctx, "construct-string", 
		   DFSCH_PRIMITIVE_REF(construct_string));
  dfsch_defcanon_cstr(ctx, "string-join", 
		   DFSCH_PRIMITIVE_REF(string_join));

  dfsch_defcanon_cstr(ctx, "string->safe-filename", 
		   DFSCH_PRIMITIVE_REF(string_2_safe_filename));

  dfsch_defcanon_cstr(ctx, "string->hexstring", 
		   DFSCH_PRIMITIVE_REF(string_2_hexstring));
  dfsch_defcanon_cstr(ctx, "hexstring->string", 
		   DFSCH_PRIMITIVE_REF(hexstring_2_string));
  dfsch_defcanon_cstr(ctx, "hexstring->byte-vector", 
		   DFSCH_PRIMITIVE_REF(hexstring_2_byte_vector));

  dfsch_defcanon_cstr(ctx, "string-trim", 
		   DFSCH_PRIMITIVE_REF(string_trim));
  dfsch_defcanon_cstr(ctx, "string-trim-left", 
		   DFSCH_PRIMITIVE_REF(string_trim_left));
  dfsch_defcanon_cstr(ctx, "string-trim-right", 
		   DFSCH_PRIMITIVE_REF(string_trim_right));
  dfsch_defcanon_cstr(ctx, "byte-vector-trim", 
		   DFSCH_PRIMITIVE_REF(byte_vector_trim));
  dfsch_defcanon_cstr(ctx, "byte-vector-trim-left", 
		   DFSCH_PRIMITIVE_REF(byte_vector_trim_left));
  dfsch_defcanon_cstr(ctx, "byte-vector-trim-right", 
		   DFSCH_PRIMITIVE_REF(byte_vector_trim_right));

  dfsch_defcanon_cstr(ctx, "character->ordinal", 
                      DFSCH_PRIMITIVE_REF(character_2_ordinal));
  dfsch_defcanon_cstr(ctx, "ordinal->character", 
                      DFSCH_PRIMITIVE_REF(ordinal_2_character));

}
