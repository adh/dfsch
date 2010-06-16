/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Internet data handling
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

#include "dfsch/lib/inet.h"

#include <dfsch/strings.h>
#include <dfsch/hash.h>
#include <string.h>

static dfsch_object_t* urldecode_to_string(char* buf, size_t len){
  dfsch_strbuf_t sb;
  sb.ptr = buf;
  sb.len = len;
  return dfsch_make_string_nocopy(dfsch_inet_urldecode(&sb));
}

dfsch_object_t* dfsch_http_split_path(char* path){
  char* pos;
  dfsch_object_t* head;
  dfsch_object_t* tail;
  dfsch_object_t* tmp;
  
  head = tail = NULL;

  while (1){
    while (*path == '/'){
      path++;
    }

    if (*path == 0){
        tmp = dfsch_cons(NULL, NULL);      
        if (tail){
          dfsch_set_cdr(tail, tmp);
          tail = tmp;
        } else {
          head = tail = tmp;
        }
        break;
    } else {

      pos = strchr(path, '/');
      if (!pos){
        tmp = dfsch_cons(urldecode_to_string(path, strlen(path)), NULL);

        if (tail){
          dfsch_set_cdr(tail, tmp);
          tail = tmp;
        } else {
          head = tail = tmp;
        }
        break;
      } else {
        tmp = dfsch_cons(urldecode_to_string(path, pos - path), NULL);
        path = pos;

        if (tail){
          dfsch_set_cdr(tail, tmp);
          tail = tmp;
        } else {
          head = tail = tmp;
        }

      }
    }
  }

  return head;

}

dfsch_object_t* dfsch_http_query_2_hash(char* query){
  size_t delim;
  char* value;
  dfsch_object_t* hash;

  hash = dfsch_hash_make(DFSCH_HASH_EQUAL);

  while (*query){
    query += strspn(query, "&;");
    delim = strcspn(query, "&;");
    
    value = memchr(query, '=', delim);
    if (value){
      value++;
      dfsch_hash_set(hash, 
                     urldecode_to_string(query, value-query-1),
                     urldecode_to_string(value, (query+delim) - value));
    } else {
      dfsch_hash_set(hash, 
                     urldecode_to_string(query, delim),
                     NULL);
    }

    query += delim;
  }

  return hash;
}
dfsch_object_t* dfsch_http_query_2_alist(char* query){
  size_t delim;
  char* value;
  dfsch_object_t* tmp;
  dfsch_object_t* head = NULL;
  dfsch_object_t* tail;

  while (*query){
    query += strspn(query, "&;");
    delim = strcspn(query, "&;");
    
    value = memchr(query, '=', delim);
    if (value){
      value++;
      tmp = dfsch_cons(dfsch_list(2, 
                                  urldecode_to_string(query, 
                                                      value - query - 1),
                                  urldecode_to_string(value, 
                                                      (query+delim) - value)),
                       NULL);
    } else {
      tmp = dfsch_cons(dfsch_list(1, 
                                  urldecode_to_string(query, delim)),
                       NULL);
    }

    if (head){
      dfsch_set_cdr(tail, tmp);
      tail = tmp;
    } else {
      head = tail = tmp;
    }

    query += delim;
  }

  return head;
}

static int xdigit_to_num(char digit){
  switch(digit){
  case '0':
    return 0;
  case '1':
    return 1;
  case '2':
    return 2;
  case '3':
    return 3;
  case '4':
    return 4;
  case '5':
    return 5;
  case '6':
    return 6;
  case '7':
    return 7;
  case '8':
    return 8;
  case '9':
    return 9;
  case 'A':
  case 'a':
    return 10;
  case 'B':
  case 'b':
    return 11;
  case 'C':
  case 'c':
    return 12;
  case 'D':
  case 'd':
    return 13;
  case 'E':
  case 'e':
    return 14;
  case 'F':
  case 'f':
    return 15;
  default:
    return 255;
  }
}

dfsch_strbuf_t* dfsch_inet_urldecode(dfsch_strbuf_t* strbuf){
  dfsch_strbuf_t* res = GC_NEW(dfsch_strbuf_t);
  size_t i;
  char* out;

  res->len = 0;
  i = 0;
  while (i < strbuf->len){
    if (strbuf->ptr[i] == '%'){
      i++;
      res->len++;
      if (i < strbuf->len && isxdigit(strbuf->ptr[i])){
        i++;
        if (i < strbuf->len && isxdigit(strbuf->ptr[i])){
          i++;
        } else {
          i--;
        }
      }
    } else {
      res->len++;
      i++;
    }
  }

  res->ptr = out = GC_MALLOC_ATOMIC(res->len+1);
  i = 0;
  while (i < strbuf->len){
    if (strbuf->ptr[i] == '%'){
      i++;
      if (i < strbuf->len && isxdigit(strbuf->ptr[i])){
        *out = xdigit_to_num(strbuf->ptr[i]) << 4;
        i++;
        if (i < strbuf->len && isxdigit(strbuf->ptr[i])){
          *out |= xdigit_to_num(strbuf->ptr[i]);
          i++;
          out++;
        } else {
          i--;
        }
      } else {
        *out++ = '%'; 
      }
    } else if (strbuf->ptr[i] == '+'){
      i++;
      *out++ = ' ';      
    } else {
      *out++ = strbuf->ptr[i++];
    }    
  }

  *out = 0;

  return res;
}

static char hex_digits[] = "0123456789abcdef";

dfsch_strbuf_t* dfsch_inet_urlencode(dfsch_strbuf_t* strbuf){
  dfsch_strbuf_t* res = GC_NEW(dfsch_strbuf_t);
  size_t i;
  char* out;

  i = 0;
  while (i < strbuf->len){
    if (isalpha(strbuf->ptr[i]) ||
        isdigit(strbuf->ptr[i]) ||
        strchr("-_.~", strbuf->ptr[i])){
      res->len++;
      i++;
    } else if (strbuf->ptr[i] == ' ') {
      res->len++;
      i++;
    } else {
      res->len+=3;
      i++;      
    }
  }

  res->ptr = out = GC_MALLOC_ATOMIC(res->len + 1);

  i = 0;
  while (i < strbuf->len){
    if (isalpha(strbuf->ptr[i]) ||
        isdigit(strbuf->ptr[i]) ||
        strchr("-_.~", strbuf->ptr[i])){
      *out++ = strbuf->ptr[i++];
    } else if (strbuf->ptr[i] == ' ') {
      *out++ = '+';
      i++;
    }else {
      *out++ = '%';
      *out++ = hex_digits[((unsigned char)strbuf->ptr[i]) >> 4];
      *out++ = hex_digits[strbuf->ptr[i] & 0xf];
      i++;      
    }
  }
  
  return res;
}

static char base64_chars[] = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static int base64char_value(char ch){
  if ((ch >= 'A') && (ch <= 'Z')){
    return ch - 'A';
  } else if ((ch >= 'a') && (ch <= 'z')){
    return ch - 'a' + 26;
  } else if ((ch >= '0') && (ch <= '9')){
    return ch - '0' + 52;
  } else if (ch == '+'){
    return 62;
  } else if (ch == '/'){
    return 63;
  }
  return -1;
}
static char ubase64_chars[] = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.-";
static int ubase64char_value(char ch){
  if ((ch >= 'A') && (ch <= 'Z')){
    return ch - 'A';
  } else if ((ch >= 'a') && (ch <= 'z')){
    return ch - 'a' + 26;
  } else if ((ch >= '0') && (ch <= '9')){
    return ch - '0' + 52;
  } else if (ch == '.'){
    return 62;
  } else if (ch == '-'){
    return 63;
  }
  return -1;
}

dfsch_strbuf_t* dfsch_inet_base64_decode(dfsch_strbuf_t* str_buf){
  dfsch_strbuf_t* res = GC_NEW(dfsch_strbuf_t);
  size_t i;
  size_t valid;
  uint32_t tmp;
  int val;
  char* out;

  valid = 0;

  for (i = 0; i < str_buf->len; i++){
    if (base64char_value(str_buf->ptr[i]) >= 0){
      valid++;
    } 
  }
  
  switch (valid % 4){
  case 0:
  case 1:
    res->len = valid / 4 * 3;
    break;
  case 2:
    res->len = valid / 4 * 3 + 1;    
    break;
  case 3:
    res->len = valid / 4 * 3 + 2;    
    break;
  }

  tmp = 0;
  valid = 0;
  res->ptr = out = GC_MALLOC_ATOMIC(res->len+1);

  for (i = 0; i < str_buf->len; i++){
    val = base64char_value(str_buf->ptr[i]);
    if (val >= 0){
      tmp <<= 6; 
      tmp |= val;
      valid++;
      if (valid == 4){
        valid = 0;
        *out++ = (tmp >> 16) & 0xff;
        *out++ = (tmp >> 8) & 0xff;
        *out++ = (tmp) & 0xff;
        tmp = 0;
      }
    }
  }

  switch (valid){
  case 2:
    tmp <<= 12;
    *out++ = (tmp >> 16) & 0xff;
  case 3:
    tmp <<= 6;
    *out++ = (tmp >> 16) & 0xff;
    *out++ = (tmp >> 8) & 0xff;
  }

  *out = 0;

  return res;
}
dfsch_strbuf_t* dfsch_inet_base64_encode(dfsch_strbuf_t* str_buf,
                                         int wrap,
                                         int pad){
  dfsch_strbuf_t* res = GC_NEW(dfsch_strbuf_t);
  size_t i;
  char* out;
  uint32_t tmp;

  switch (str_buf->len % 3){
  case 0:
    res->len = str_buf->len / 3 * 4;
    break;
  case 1:
    res->len = str_buf->len / 3 * 4 + (pad ? 4 : 2);
    break;
  case 2:
    res->len = str_buf->len / 3 * 4 + (pad ? 4 : 3);
    break;
  }
  
  if (wrap){
    res->len += (res->len / 64) * 2;
  }

  res->ptr = out = GC_MALLOC_ATOMIC(res->len+1);
  
  for (i = 0; i + 2 < str_buf->len;  i+=3){
    tmp = ((((unsigned)str_buf->ptr[i]) & 0xff) << 16) | 
      ((((unsigned)str_buf->ptr[i+1]) & 0xff) << 8) | 
      (((unsigned)str_buf->ptr[i+2]) & 0xff);

    *out++ = base64_chars[(tmp >> 18) & 0x3f];
    *out++ = base64_chars[(tmp >> 12) & 0x3f];
    *out++ = base64_chars[(tmp >> 6) & 0x3f];
    *out++ = base64_chars[tmp & 0x3f];

    if (wrap && i % 48 == 45){
      *out++ = '\r';
      *out++ = '\n';      
    }
  }

  switch (str_buf->len - i){
  case 1:
    tmp = ((((unsigned)str_buf->ptr[i]) & 0xff) << 16);

    *out++ = base64_chars[(tmp >> 18) & 0x3f];
    *out++ = base64_chars[(tmp >> 12) & 0x3f];
    if (pad){
      *out++ = '=';
      *out++ = '=';
    }
    break;
  case 2:
    tmp = ((((unsigned)str_buf->ptr[i]) & 0xff) << 16) | 
      ((((unsigned)str_buf->ptr[i+1]) & 0xff)<< 8);

    *out++ = base64_chars[(tmp >> 18) & 0x3f];
    *out++ = base64_chars[(tmp >> 12) & 0x3f];
    *out++ = base64_chars[(tmp >> 6) & 0x3f];
    if (pad){
      *out++ = '=';
    }    
    break;
  }
  
  *out = 0;

  return res;
}
dfsch_strbuf_t* dfsch_inet_uri_base64_decode(dfsch_strbuf_t* str_buf){
  dfsch_strbuf_t* res = GC_NEW(dfsch_strbuf_t);
  size_t i;
  size_t valid;
  uint32_t tmp;
  int val;
  char* out;

  valid = 0;

  for (i = 0; i < str_buf->len; i++){
    if (ubase64char_value(str_buf->ptr[i]) >= 0){
      valid++;
    } 
  }
  
  switch (valid % 4){
  case 0:
  case 1:
    res->len = valid / 4 * 3;
    break;
  case 2:
    res->len = valid / 4 * 3 + 1;    
    break;
  case 3:
    res->len = valid / 4 * 3 + 2;    
    break;
  }

  tmp = 0;
  valid = 0;

  res->ptr = out = GC_MALLOC_ATOMIC(res->len+1);

  for (i = 0; i < str_buf->len; i++){
    val = ubase64char_value(str_buf->ptr[i]);
    if (val >= 0){
      tmp <<= 6; 
      tmp |= val;
      valid++;
      if (valid == 4){
        valid = 0;
        *out++ = (tmp >> 16) & 0xff;
        *out++ = (tmp >> 8) & 0xff;
        *out++ = (tmp) & 0xff;
      }
    }
  }

  switch (valid){
  case 2:
    tmp <<= 12;
    *out++ = (tmp >> 16) & 0xff;
  case 3:
    tmp <<= 6;
    *out++ = (tmp >> 16) & 0xff;
    *out++ = (tmp >> 8) & 0xff;
  }

  *out = 0;

  return res;
}
dfsch_strbuf_t* dfsch_inet_uri_base64_encode(dfsch_strbuf_t* str_buf){
  dfsch_strbuf_t* res = GC_NEW(dfsch_strbuf_t);
  size_t i;
  char* out;
  uint32_t tmp;

  switch (str_buf->len % 3){
  case 0:
    res->len = str_buf->len / 3 * 4;
    break;
  case 1:
    res->len = str_buf->len / 3 * 4 + 2;
    break;
  case 2:
    res->len = str_buf->len / 3 * 4 + 3;
    break;
  }
  
  res->ptr = out = GC_MALLOC_ATOMIC(res->len+1);
  
  for (i = 0; i + 2 < str_buf->len;  i+=3){
    tmp = ((((unsigned)str_buf->ptr[i]) & 0xff) << 16) | 
      ((((unsigned)str_buf->ptr[i+1]) & 0xff) << 8) | 
      (((unsigned)str_buf->ptr[i+2]) & 0xff);

    *out++ = ubase64_chars[(tmp >> 18) & 0x3f];
    *out++ = ubase64_chars[(tmp >> 12) & 0x3f];
    *out++ = ubase64_chars[(tmp >> 6) & 0x3f];
    *out++ = ubase64_chars[tmp & 0x3f];
  }

  switch (str_buf->len - i){
  case 1:
    tmp = (((unsigned)str_buf->ptr[i]) << 16) & 0xff;

    *out++ = ubase64_chars[(tmp >> 18) & 0x3f];
    *out++ = ubase64_chars[(tmp >> 12) & 0x3f];
    break;
  case 2:
    tmp = ((((unsigned)str_buf->ptr[i]) & 0xff) << 16) | 
      ((((unsigned)str_buf->ptr[i+1]) & 0xff) << 8);

    *out++ = ubase64_chars[(tmp >> 18) & 0x3f];
    *out++ = ubase64_chars[(tmp >> 12) & 0x3f];
    *out++ = ubase64_chars[(tmp >> 6) & 0x3f];
    break;
  }
  
  *out = 0;

  return res;
}

char* dfsch_inet_xml_escape(char* str){
  char* res;
  size_t len;
  size_t i;
  char* out;

  len = 0;

  for (i = 0; str[i]; i++){
    switch (str[i]){
    case '<':
    case '>':
      len += 4;
      break;
    case '\"':
    case '\'':
      len += 6;
      break;
    case '&':
      len += 5;
      break;
    default:
      len++;
    }
  }

  res = out = GC_MALLOC_ATOMIC(len + 1);

  for (i = 0; str[i]; i++){
    switch (str[i]){
    case '<':
      *out++ = '&';
      *out++ = 'l';
      *out++ = 't';
      *out++ = ';';
      break;
    case '>':
      *out++ = '&';
      *out++ = 'g';
      *out++ = 't';
      *out++ = ';';
      break;
    case '\"':
      *out++ = '&';
      *out++ = 'q';
      *out++ = 'u';
      *out++ = 'o';
      *out++ = 't';
      *out++ = ';';
      break;
    case '\'':
      *out++ = '&';
      *out++ = 'a';
      *out++ = 'p';
      *out++ = 'o';
      *out++ = 's';
      *out++ = ';';
      break;
    case '&':
      *out++ = '&';
      *out++ = 'a';
      *out++ = 'm';
      *out++ = 'p';
      *out++ = ';';
      break;
    default:
      *out++ = str[i];
    }
  }

  *out = 0;

  return res;
}

/* void dfsch_http_header_parser_parse_line(dfsch_http_header_parser_t* hp, */
/*                                          char* line){ */
/*   char* value; */

/*   if (*line == ' ' || *line == '\t') { */
/*     if (hp->header_name) { /\* Continuation *\/ */
/*       hp->cb(hp->baton, hp->header_name, line); */
/*     } else { /\* Continuation of nothing *\/ */
/*       dfsch_error("Continuation of empty header",  */
/*                   dfsch_make_string_cstr(line)); */
/*     } */
/*   } else { */
/*     value = strchr(line, ':'); */
/*     if (value){ /\* Header *\/       */
/*       hp->cb(hp->baton, hp->header_name, value + 1); */
/*     } else { /\*  Random junk *\/ */
/*       dfsch_error("Junk in header stream",  */
/*                   dfsch_make_string_cstr(line)); */
/*     } */
/*   }  */
/* } */

static void header_name_inplace(char* name){
  char* i = name;
  int state = 0;

  while (*i){
    if (isalnum(*i)){
      if (state){
        *i = tolower(*i);
      }else{
        *i = toupper(*i);
      }
      state = 1;
    } else {
      state = 0;
    }
    i++;
  }

  i--;

  while (i >= name && 
         (*i == ' ' || 
          *i == '\t')){
    *i = '\0';
    i--;
  }
}


void dfsch_inet_read_822_headers(dfsch_object_t* port,
                                 dfsch_inet_header_cb_t cb,
                                 void* baton){
  dfsch_strbuf_t* line;
  char* name = NULL;
  char* value;

  for (;;){
    line = dfsch_port_readline(port);

    if (!line){
      return;
    }

    while (line->len && (line->ptr[line->len - 1] == '\n' ||
                         line->ptr[line->len - 1] == '\r' )){
      line->len --;
      line->ptr[line->len] = '\0';
    }
    
    if (line->len == 0){
      if (name){
        cb(baton, name, value);
      }
      break;
    }

    if (line->ptr[0] == ' ' || line->ptr[0] == '\t'){
      if (name == NULL){
        dfsch_error("Unexpected continuation line", 
                    dfsch_make_string_strbuf(line));
      }
      value = dfsch_stracat3(value, "\n", line->ptr+1);
    } else {
      if (name){
        cb(baton, name, value);
      }
      name = line->ptr;
      value = strchr(line->ptr, ':');
      if (!value){
        dfsch_error("Not a header line", dfsch_make_string_strbuf(line));
      }
      value[0] = '\0';
      header_name_inplace(name);
      value += 1;
      while (value[0] == ' ' || value[0] == '\t'){
        value++;
      }
    }
  }
}
