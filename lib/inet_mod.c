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

static dfsch_object_t* inet_base64_encode(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  dfsch_object_t* wrap;
  dfsch_object_t* pad;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_OBJECT_ARG_OPT(args, wrap, NULL);
  DFSCH_OBJECT_ARG_OPT(args, pad, NULL);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_base64_encode(str, 
                                                           wrap!=NULL, 
                                                           pad!=NULL));
}
static dfsch_object_t* inet_uri_base64_encode(void* baton,
                                              dfsch_object_t* args,
                                              dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_uri_base64_encode(str));
}
static dfsch_object_t* inet_base64_decode(void* baton,
                                          dfsch_object_t* args,
                                          dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_base64_decode(str));
}
static dfsch_object_t* inet_uri_base64_decode(void* baton,
                                              dfsch_object_t* args,
                                              dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_uri_base64_decode(str));
}

static dfsch_object_t* inet_xml_escape(void* baton,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_xml_escape(str));
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

  dfsch_define_cstr(env, "inet:base64-encode",
                    dfsch_make_primitive(inet_base64_encode, NULL));
  dfsch_define_cstr(env, "inet:uri-base64-encode",
                    dfsch_make_primitive(inet_uri_base64_encode, NULL));
  dfsch_define_cstr(env, "inet:base64-decode",
                    dfsch_make_primitive(inet_base64_decode, NULL));
  dfsch_define_cstr(env, "inet:uri-base64-decode",
                    dfsch_make_primitive(inet_uri_base64_decode, NULL));

  dfsch_define_cstr(env, "inet:xml-escape",
                    dfsch_make_primitive(inet_xml_escape, NULL));

}
