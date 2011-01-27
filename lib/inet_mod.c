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

DFSCH_DEFINE_PRIMITIVE(http_split_query, NULL){
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

DFSCH_DEFINE_PRIMITIVE(http_split_path, NULL){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  return dfsch_http_split_path(path);
}

DFSCH_DEFINE_PRIMITIVE(http_query_2_alist, NULL){
  char* query;
  DFSCH_STRING_ARG(args, query);
  DFSCH_ARG_END(args);

  return dfsch_http_query_2_alist(query);
}
DFSCH_DEFINE_PRIMITIVE(http_query_2_hash, NULL){
  char* query;
  DFSCH_STRING_ARG(args, query);
  DFSCH_ARG_END(args);

  return dfsch_http_query_2_hash(query);
}

DFSCH_DEFINE_PRIMITIVE(http_avpairs_2_alist, NULL){
  char* avpairs;
  DFSCH_STRING_ARG(args, avpairs);
  DFSCH_ARG_END(args);

  return dfsch_http_avpairs_2_alist(avpairs);
}

DFSCH_DEFINE_PRIMITIVE(inet_urldecode, NULL){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_urldecode(str));
}
DFSCH_DEFINE_PRIMITIVE(inet_urlencode, NULL){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_urlencode(str));
}

DFSCH_DEFINE_PRIMITIVE(inet_base64_encode, NULL){
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
DFSCH_DEFINE_PRIMITIVE(inet_uri_base64_encode, NULL){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_nocopy(dfsch_inet_uri_base64_encode(str));
}
DFSCH_DEFINE_PRIMITIVE(inet_base64_decode, NULL){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  str = dfsch_inet_base64_decode(str);

  return dfsch_make_byte_vector_nocopy(str->ptr, str->len);
}
DFSCH_DEFINE_PRIMITIVE(inet_uri_base64_decode, NULL){
  dfsch_strbuf_t* str;
  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);
  
  str = dfsch_inet_uri_base64_decode(str);

  return dfsch_make_byte_vector_nocopy(str->ptr, str->len);
}

DFSCH_DEFINE_PRIMITIVE(inet_xml_escape, NULL){
  char* str;
  DFSCH_STRING_ARG(args, str);
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_cstr(dfsch_inet_xml_escape(str));
}

DFSCH_DEFINE_PRIMITIVE(headers_2_list, 
                       "Read email style headers from port into new list"){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);
 
  return dfsch_inet_read_822_headers_list(port);
}

DFSCH_DEFINE_PRIMITIVE(headers_2_map, 
                       "Read email style headers from port into mapping object"){
  dfsch_object_t* map = NULL;
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_OBJECT_ARG_OPT(args, map, NULL);
  DFSCH_ARG_END(args);
  
  return dfsch_inet_read_822_headers_map(port, map);
}




dfsch_object_t* dfsch_module_inet_register(dfsch_object_t* env){
  dfsch_package_t* inet_pkg = dfsch_make_package("inet",
                                                 "Internet data handling");  
  dfsch_provide(env, "inet");

  dfsch_defcanon_pkgcstr(env, inet_pkg, "http-split-query",
                    DFSCH_PRIMITIVE_REF(http_split_query));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "http-split-path",
                    DFSCH_PRIMITIVE_REF(http_split_path));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "http-query->alist",
                    DFSCH_PRIMITIVE_REF(http_query_2_alist));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "http-query->hash",
                    DFSCH_PRIMITIVE_REF(http_query_2_hash));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "http-avpairs->alist",
                    DFSCH_PRIMITIVE_REF(http_avpairs_2_alist));

  dfsch_defcanon_pkgcstr(env, inet_pkg, "urldecode",
                    DFSCH_PRIMITIVE_REF(inet_urldecode));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "urlencode",
                    DFSCH_PRIMITIVE_REF(inet_urlencode));

  dfsch_defcanon_pkgcstr(env, inet_pkg, "base64-encode",
                    DFSCH_PRIMITIVE_REF(inet_base64_encode));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "uri-base64-encode",
                    DFSCH_PRIMITIVE_REF(inet_uri_base64_encode));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "base64-decode",
                    DFSCH_PRIMITIVE_REF(inet_base64_decode));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "uri-base64-decode",
                    DFSCH_PRIMITIVE_REF(inet_uri_base64_decode));

  dfsch_defcanon_pkgcstr(env, inet_pkg, "xml-escape",
                    DFSCH_PRIMITIVE_REF(inet_xml_escape));

  dfsch_defcanon_pkgcstr(env, inet_pkg, "headers->list",
                       DFSCH_PRIMITIVE_REF(headers_2_list));
  dfsch_defcanon_pkgcstr(env, inet_pkg, "headers->map",
                       DFSCH_PRIMITIVE_REF(headers_2_map));

}
