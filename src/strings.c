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
  pair_t *head; 
  pair_t *tail;
  size_t i;

  TYPE_CHECK(s, STRING, "string");

  if (s->len == 0)
    return NULL;

  head = tail = (pair_t*)dfsch_cons(dfsch_make_number_from_long(s->ptr[0]), 
                                    NULL);

  for(i = 1; i < s->len; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(dfsch_make_number_from_long(s->ptr[i]),NULL);
    tail->cdr = tmp;
    tail = (pair_t*)tmp;

  }
  return (object_t*)head;
}

dfsch_object_t* dfsch_list_2_string(dfsch_object_t* list){
  dfsch_string_t* string;
  pair_t* j = (pair_t*)list;
  size_t i=0;
  string = 
    (dfsch_string_t*)dfsch_make_string_buf(NULL,
                                           dfsch_list_length_check(list));
  
  while (dfsch_pair_p((object_t*)j)){
    string->ptr[i] = dfsch_number_to_long(j->car);
    j = (pair_t*)j->cdr;
    i++;
  }

  return (object_t*)string;
}


int dfsch_string_for_each(dfsch_string_callback_t proc,
                               dfsch_object_t* string,
                               void *baton){
  dfsch_string_t* s = (dfsch_string_t*) string;
  int i;
  TYPE_CHECK(s, STRING, "string");


  for (i=0; i<s->len; i++){
    int r = proc(s->ptr[i], baton);
    if (r)
      return r;
  }

  return 0;
}

// UTF-8 support

int dfsch_string_utf8_for_each(dfsch_string_unicode_callback_t proc,
                               dfsch_object_t* string,
                               void *baton,
                               dfsch_string_unicode_invalid_callback_t iproc,
                               void* ibaton){

  dfsch_string_t* s = (dfsch_string_t*) string;
  size_t i = 0;
  size_t state = 0;
  size_t char_start = 0;
  uint32_t ch;
  TYPE_CHECK(s, STRING, "string");

  while(i<s->len){
    if((s->ptr[i] & 0x80) == 0){ // U+0000 - U+007F, one byte
      int r = proc(s->ptr[i], baton, i, i);
      if (r)
        return r;
      i++;
      state = 0;
    }else{
      if ((s->ptr[i] & 0xe0) == 0xc0 &&
          (s->ptr[i] & 0x1f) != 0x00){ // U+0080 - U+07FF, two bytes
        ch = s->ptr[i] & 0x1f;
        char_start = i;
        i++;
        state = 1;
      }else if ((s->ptr[i] & 0xf0) == 0xe0 &&
                (s->ptr[i] & 0x0f) != 0x00){ // U+0800 - U+FFFF, three bytes
        ch = s->ptr[i] & 0xf;
        char_start = i;
        i++;
        state = 2;
      }else if ((s->ptr[i] & 0xf8) == 0xf0 &&
                (s->ptr[i] & 0x07) != 0x00){ // U+10000 - U+10FFFF, four bytes
        ch = s->ptr[i] & 0x7;
        char_start = i;
        i++;
        state = 3;
      }else if ((s->ptr[i] & 0xc0) == 0x80 &&
                state != 0){ // internal byte
        state--;
        ch = (ch << 6) | s->ptr[i] & 0x3f;
        if (state == 0){ 
          int r = proc(ch, baton, char_start, i);
          if (r)
            return r;
        }
        i++;
      }else{ // Invalid byte
        state = 0;
        
        if (iproc){
          int r = iproc(s->ptr[i], ibaton, i);
          if (r)
            return r;
        }

        i++;
      }
    }
  }

  return 0;
}

static int utf8_len_cb(uint32_t ch, size_t*len, size_t start, size_t end){
  (*len)++;
  return 0;
}

size_t dfsch_string_utf8_length(dfsch_object_t* string){
  size_t len = 0;

  dfsch_string_utf8_for_each((dfsch_string_unicode_callback_t)utf8_len_cb, 
                             string, &len, NULL, NULL);

  return len;
}

typedef struct utf8_ref_ctx_t{
  size_t len;
  size_t index;
  uint32_t ch;
}utf8_ref_ctx_t;

static int utf8_ref_cb(uint32_t ch, utf8_ref_ctx_t* c, 
                       size_t start, size_t end){
  if (c->len == c->index){
    c->ch = ch;
    return 1;
  }
  c->len++;
  return 0;
}


uint32_t dfsch_string_utf8_ref(dfsch_object_t* string, size_t index){
  utf8_ref_ctx_t c;
  
  c.len = 0;
  c.index = index;

  if (dfsch_string_utf8_for_each((dfsch_string_unicode_callback_t)utf8_ref_cb, 
                                 string, &c, NULL, NULL)){
    return c.ch;
  }

  dfsch_error("exception:index-out-of-bounds",
              dfsch_make_number_from_long(index));
}

typedef struct utf8_substring_ctx_t{
  size_t len;
  size_t start;
  size_t end;
  size_t sptr;
  size_t eptr;
}utf8_substring_ctx_t;

static int utf8_substring_cb(uint32_t ch, utf8_substring_ctx_t* c, 
                             size_t start, size_t end){
  
  if (c->len = c->start){
    c->sptr = start;
  }
  if (c->len = c->end){
    c->eptr = start;
    return 1;
  }
  c->len++;
  return 0;
}


dfsch_object_t* dfsch_string_substring_utf8(dfsch_object_t* string,
                                            size_t start, size_t end){

  utf8_substring_ctx_t c;
  dfsch_string_t *s = (dfsch_string_t*)string;

  if (start > end)
    dfsch_error("exception:index-out-of-bounds",
                dfsch_make_number_from_long(start));

  c.start = start;
  c.end = end;
  c.len = 0;

  if (!dfsch_string_utf8_for_each((dfsch_string_unicode_callback_t)
                                  utf8_substring_cb, 
                                 string, &c, NULL, NULL)){

    if (c.len != c.end)
      dfsch_error("exception:index-out-of-bounds",
                  dfsch_make_number_from_long(end));
      
    c.eptr = s->len;
  }
  
  

  return dfsch_make_string_buf(s->ptr + c.sptr, c.eptr - c.sptr);

}
typedef struct utf8_list_ctx_t{
  pair_t* head;
  pair_t* tail;
} utf8_list_ctx_t;

static int utf8_list_cb(uint32_t ch, utf8_list_ctx_t* c, 
                        size_t start, size_t end){
  if (c->head){
    pair_t* tmp;
    tmp = (pair_t*)dfsch_cons(dfsch_make_number_from_long(ch), NULL);
    c->tail->cdr = (object_t*)tmp;
    c->tail = tmp;
  }else{
    c->head = c->tail = (pair_t*)dfsch_cons(dfsch_make_number_from_long(ch), 
                                            NULL);
  }
  return 0;
}
dfsch_object_t* dfsch_string_utf8_2_list(dfsch_object_t* string){
  utf8_list_ctx_t ctx;
  ctx.head = NULL;
  ctx.tail = NULL;

  dfsch_string_utf8_for_each((dfsch_string_unicode_callback_t)
                             utf8_list_cb, 
                             string, &ctx, NULL, NULL);
  
  return (object_t*)ctx.head;           
}

dfsch_object_t* dfsch_list_2_string_utf8(dfsch_object_t* list){
  
  dfsch_string_t* string;
  pair_t* j = (pair_t*)list;
  size_t i=0;
  size_t len;
  if (list && !dfsch_pair_p(list))
    dfsch_error("exception:not-a-list",list);
  
  len = 0;
  while (dfsch_pair_p((object_t*)j)){
    uint32_t ch = dfsch_number_to_long(j->car);
    if (ch <= 0x7f){
      len += 1;
    } else if (ch <= 0x7ff) {
      len += 2;
    } else if (ch <= 0xffff) {
      len += 3;
    } else if (ch <= 0x10ffff){
      len += 4;
    } else {
      dfsch_error("exception:invalid-unicode-character", j->car);
    }
    j = (pair_t*)j->cdr;
  }

  string = (dfsch_string_t*)dfsch_make_string_buf(NULL, len);

  j = (pair_t*)list;
  while (dfsch_pair_p((object_t*)j)){
    uint32_t ch = dfsch_number_to_long(j->car);

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

    j = (pair_t*)j->cdr;
  }

  return (object_t*)string;

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
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long((unsigned char)dfsch_string_ref(string, index));

}
static object_t* native_string_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_length(string));
}
static object_t* native_string_utf8_ref(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t index;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, index);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_utf8_ref(string, index));

}
static object_t* native_string_utf8_length(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(dfsch_string_utf8_length(string));
}
static object_t* native_string_2_list(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_2_list(string);
}
static object_t* native_string_utf8_2_list(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_utf8_2_list(string);
}
static object_t* native_list_2_string(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_string(list);
}
static object_t* native_list_2_string_utf8(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* list;

  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  return dfsch_list_2_string_utf8(list);
}
static object_t* native_string_cmp_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  DFSCH_ARG_END(args);

  return dfsch_bool(((int (*)(object_t*,object_t*)) baton)(a, b));
}
static object_t* native_substring(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);
  DFSCH_ARG_END(args);

  return dfsch_string_substring(string, start, end);

}
static object_t* native_substring_utf8(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  size_t start, end;
  object_t* string;

  DFSCH_OBJECT_ARG(args, string);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);
  DFSCH_ARG_END(args);

  return dfsch_string_substring_utf8(string, start, end);

}


void dfsch__string_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<string>", &string_type);


  dfsch_define_cstr(ctx, "string-append", 
		   dfsch_make_primitive(&native_string_append,NULL));
  dfsch_define_cstr(ctx, "substring", 
		   dfsch_make_primitive(&native_substring,NULL));
  dfsch_define_cstr(ctx, "substring-utf8", 
		   dfsch_make_primitive(&native_substring_utf8,NULL));
  dfsch_define_cstr(ctx, "string-ref", 
		   dfsch_make_primitive(&native_string_ref,NULL));
  dfsch_define_cstr(ctx, "string-utf8-ref", 
		   dfsch_make_primitive(&native_string_utf8_ref,NULL));
  dfsch_define_cstr(ctx, "string-length", 
		   dfsch_make_primitive(&native_string_length,NULL));
  dfsch_define_cstr(ctx, "string-utf8-length", 
		   dfsch_make_primitive(&native_string_utf8_length,NULL));
  dfsch_define_cstr(ctx, "string->list", 
		   dfsch_make_primitive(&native_string_2_list,NULL));
  dfsch_define_cstr(ctx, "string-utf8->list", 
		   dfsch_make_primitive(&native_string_utf8_2_list,NULL));
  dfsch_define_cstr(ctx, "list->string", 
		   dfsch_make_primitive(&native_list_2_string,NULL));
  dfsch_define_cstr(ctx, "list->string-utf8", 
		   dfsch_make_primitive(&native_list_2_string_utf8,NULL));


  dfsch_define_cstr(ctx, "string=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_eq_p));
  dfsch_define_cstr(ctx, "string<?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_lt_p));
  dfsch_define_cstr(ctx, "string>?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_gt_p));
  dfsch_define_cstr(ctx, "string<=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_lte_p));
  dfsch_define_cstr(ctx, "string>=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_gte_p));

  dfsch_define_cstr(ctx, "string-ci=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_eq_p));
  dfsch_define_cstr(ctx, "string-ci<?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_lt_p));
  dfsch_define_cstr(ctx, "string-ci>?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_gt_p));
  dfsch_define_cstr(ctx, "string-ci<=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_lte_p));
  dfsch_define_cstr(ctx, "string-ci>=?", 
		   dfsch_make_primitive(&native_string_cmp_p,
                                        &dfsch_string_ci_gte_p));

}
