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

#ifndef H__dfsch_lib__inet__
#define H__dfsch_lib__inet__

#include <dfsch/dfsch.h>
#include <dfsch/strings.h>

dfsch_object_t* dfsch_module_inet_register(dfsch_object_t* env);

dfsch_object_t* dfsch_http_split_path(char* path);


dfsch_object_t* dfsch_http_query_2_hash(char* query);
dfsch_object_t* dfsch_http_query_2_alist(char* query);

dfsch_strbuf_t* dfsch_inet_urldecode(dfsch_strbuf_t* str_buf);
dfsch_strbuf_t* dfsch_inet_urlencode(dfsch_strbuf_t* str_buf);

dfsch_strbuf_t* dfsch_inet_base64_decode(dfsch_strbuf_t* str_buf);
dfsch_strbuf_t* dfsch_inet_base64_encode(dfsch_strbuf_t* str_buf,
                                         int wrap,
                                         int pad);
dfsch_strbuf_t* dfsch_inet_uri_base64_decode(dfsch_strbuf_t* str_buf);
dfsch_strbuf_t* dfsch_inet_uri_base64_encode(dfsch_strbuf_t* str_buf);

char* dfsch_inet_xml_escape(char* str_buf);


#endif
