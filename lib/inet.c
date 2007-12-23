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
}
