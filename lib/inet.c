#include "dfsch/lib/inet.h"

#include <dfsch/strings.h>
#include <dfsch/hash.h>
#include <string.h>

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
        tmp = dfsch_cons(dfsch_make_string_cstr(path), NULL);

        if (tail){
          dfsch_set_cdr(tail, tmp);
          tail = tmp;
        } else {
          head = tail = tmp;
        }
        break;
      } else {
        tmp = dfsch_cons(dfsch_make_string_buf(path, pos - path), NULL);
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

void dfsch_http_for_each_query(char* query, 
                               dfsch_object_t* callback){
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
                     dfsch_make_string_buf(query, value-query-1),
                     dfsch_make_string_buf(query, (query+delim) - value));
    } else {
      dfsch_hash_set(hash, 
                     dfsch_make_string_buf(query, delim),
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
                                  dfsch_make_string_buf(query, 
                                                        value - query - 1),
                                  dfsch_make_string_buf(value, 
                                                        (query+delim) - value)),
                       NULL);
    } else {
      tmp = dfsch_cons(dfsch_list(1, 
                                  dfsch_make_string_buf(query, delim)),
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



static dfsch_object_t* http_split_query(void* baton,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc){
  char* pos;
  char* uri;
  DFSCH_STRING_ARG(args, uri);
  DFSCH_ARG_END(args);

  pos = strchr(uri, '?');

  if (!pos){
    return dfsch_list(1,
                      dfsch_make_string_cstr(uri));
  } else {
    return dfsch_list(2,
                      dfsch_make_string_buf(uri, pos-uri),
                      dfsch_make_string_cstr(pos+1));
  }
}

static dfsch_object_t* http_split_path(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  return dfsch_http_split_path(path);
}

static dfsch_object_t* http_query_2_alist(void* baton,
                                          dfsch_object_t* args,
                                          dfsch_tail_escape_t* esc){
  char* query;
  DFSCH_STRING_ARG(args, query);
  DFSCH_ARG_END(args);

  return dfsch_http_query_2_alist(query);
}
static dfsch_object_t* http_query_2_hash(void* baton,
                                         dfsch_object_t* args,
                                         dfsch_tail_escape_t* esc){
  char* query;
  DFSCH_STRING_ARG(args, query);
  DFSCH_ARG_END(args);

  return dfsch_http_query_2_hash(query);
}
static dfsch_object_t* inet_urldecode(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_urldecode(str));
}
static dfsch_object_t* inet_urlencode(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_urlencode(str));
}


dfsch_object_t* dfsch_module_inet_register(dfsch_object_t* env){
  dfsch_provide(env, "inet");

  dfsch_define_cstr(env, "http:split-query",
                    dfsch_make_primitive(http_split_query, NULL));
  dfsch_define_cstr(env, "http:split-path",
                    dfsch_make_primitive(http_split_path, NULL));
  dfsch_define_cstr(env, "http:query->alist",
                    dfsch_make_primitive(http_query_2_alist, NULL));
  dfsch_define_cstr(env, "http:query->hash",
                    dfsch_make_primitive(http_query_2_hash, NULL));

  dfsch_define_cstr(env, "inet:urldecode",
                    dfsch_make_primitive(inet_urldecode, NULL));
  dfsch_define_cstr(env, "inet:urlencode",
                    dfsch_make_primitive(inet_urlencode, NULL));

}
