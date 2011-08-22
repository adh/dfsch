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

#ifndef H__dfsch__strings__
#define H__dfsch__strings__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct dfsch_strbuf_t{
    char* ptr;
    size_t len;
  } dfsch_strbuf_t;

  extern dfsch_strbuf_t dfsch_empty_strbuf;
#define DFSCH_EMPTY_STRBUF (&dfsch_empty_strbuf)

  typedef struct dfsch_string_t {
    dfsch_type_t* type;
    dfsch_strbuf_t buf;
  } dfsch_string_t;

  extern int dfsch_string_p(dfsch_object_t* obj);
  extern int dfsch_proto_string_p(dfsch_object_t* obj);

  extern dfsch_strbuf_t* dfsch_strbuf_create(char* ptr, size_t len);
  extern dfsch_strbuf_t* dfsch_alloc_strbuf(size_t len);
  extern dfsch_strbuf_t* dfsch_copy_strbuf(dfsch_strbuf_t* sb);
  extern ssize_t dfsch_strbuf_inputproc(dfsch_strbuf_t* strbuf, 
                                        char* buf, size_t len);
  
  extern dfsch_object_t* dfsch_make_string_cstr(char* string);
  extern dfsch_object_t* dfsch_make_string_strbuf(dfsch_strbuf_t* strbuf);
  extern dfsch_object_t* dfsch_make_string_buf(char* ptr, size_t len);
  extern dfsch_object_t* dfsch_make_string_for_write(size_t len, char**buf);

  extern dfsch_object_t* dfsch_make_string_nocopy(dfsch_strbuf_t* buf);

  extern char* dfsch_string_to_cstr(dfsch_object_t* obj);
  extern char* dfsch_proto_string_to_cstr(dfsch_object_t* obj);
  extern dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj);
  extern dfsch_strbuf_t* dfsch_byte_vector_to_buf(dfsch_object_t* obj);
  extern char* dfsch_string_or_symbol_to_cstr(dfsch_object_t* obj);

  extern int dfsch_string_cmp(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_eq_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_lt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_gt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_lte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_gte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);

  extern int dfsch_string_cmp_ci(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_ci_eq_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_ci_lt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_ci_gt_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_ci_lte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);
  extern int dfsch_string_ci_gte_p(dfsch_strbuf_t* a, dfsch_strbuf_t* b);

  extern dfsch_object_t* dfsch_string_list_append(dfsch_object_t* list);

  extern uint32_t dfsch_string_ref(dfsch_object_t* string, size_t index);
  extern size_t dfsch_string_length(dfsch_object_t* string);

  extern dfsch_object_t* dfsch_string_substring(dfsch_object_t* string, 
                                                ssize_t start,
                                                ssize_t end);

  extern dfsch_object_t* dfsch_string_byte_substring(dfsch_object_t* string, 
                                                     ssize_t start,
                                                     ssize_t end);

  extern dfsch_object_t* dfsch_string_2_list(dfsch_object_t* string);


#define DFSCH_STRING_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_string_to_cstr)
#define DFSCH_PROTO_STRING_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_proto_string_to_cstr)
#define DFSCH_STRING_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, char*, dfsch_string_to_cstr)

#define DFSCH_STRING_OR_SYMBOL_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_string_or_symbol_to_cstr)
#define DFSCH_STRING_OR_SYMBOL_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, char*, dfsch_string_or_symbol_to_cstr)

#define DFSCH_BUFFER_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, dfsch_strbuf_t*, dfsch_string_to_buf)
#define DFSCH_BUFFER_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, dfsch_strbuf_t*, dfsch_string_to_buf)

#define DFSCH_BYTE_VECTOR_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, dfsch_strbuf_t*, dfsch_byte_vector_to_buf)
#define DFSCH_BYTE_VECTOR_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, dfsch_strbuf_t*, dfsch_byte_vector_to_buf)


  extern char* dfsch_char_encode(uint32_t c);
  extern uint32_t dfsch_char_downcase(uint32_t c);
  extern uint32_t dfsch_char_upcase(uint32_t c);
  extern uint32_t dfsch_char_titlecase(uint32_t c);

  extern char* dfsch_char_category(uint32_t c);

  extern int dfsch_string_search(dfsch_strbuf_t* needle, 
                                 dfsch_strbuf_t* haystack);
  extern int dfsch_string_search_ci(dfsch_strbuf_t* needle, 
                                    dfsch_strbuf_t* haystack);

  extern dfsch_object_t* dfsch_string_split(dfsch_strbuf_t* str,
                                            dfsch_strbuf_t* separator,
                                            int max_parts,
                                            int case_sensitive,
                                            int preserve_empty);
  extern dfsch_object_t* dfsch_string_split_byte(dfsch_strbuf_t* str,
                                                 dfsch_strbuf_t* separator,
                                                 int max_parts,
                                                 int preserve_empty);
  extern dfsch_object_t* dfsch_string_split_char(dfsch_strbuf_t* str,
                                                 dfsch_strbuf_t* separator,
                                                 int max_parts,
                                                 int preserve_empty);

  extern dfsch_strbuf_t* dfsch_string_replace(dfsch_strbuf_t* str,
                                              dfsch_strbuf_t* from,
                                              dfsch_strbuf_t* to,
                                              int max_matches,
                                              int case_sensitive);
  dfsch_object_t* dfsch_byte_vector_translate(dfsch_object_t* str,
                                              dfsch_strbuf_t* from,
                                              dfsch_strbuf_t* to);
  dfsch_object_t* dfsch_string_translate(dfsch_object_t* s, 
                                         dfsch_strbuf_t* from,
                                         dfsch_strbuf_t* to);

  extern dfsch_type_t dfsch_proto_string_type;
#define DFSCH_PROTO_STRING_TYPE (&dfsch_proto_string_type)
  extern dfsch_type_t dfsch_string_type;
#define DFSCH_STRING_TYPE (&dfsch_string_type)
  extern dfsch_type_t dfsch_byte_vector_type;
#define DFSCH_BYTE_VECTOR_TYPE (&dfsch_byte_vector_type)

  dfsch_object_t* dfsch_make_byte_vector(char* ptr, size_t len);
  dfsch_object_t* dfsch_alloc_byte_vector(char** ptr, size_t len);
  dfsch_object_t* dfsch_make_byte_vector_strbuf(dfsch_strbuf_t* strbuf);
  dfsch_object_t* dfsch_make_byte_vector_nocopy(char* ptr, size_t len);
  void dfsch_byte_vector_set(dfsch_object_t* bv, size_t k, char b);
  void dfsch_byte_vector_copy(dfsch_object_t* dest,
                              size_t dest_off,
                              dfsch_object_t* src,
                              size_t src_off,
                              size_t len);
  dfsch_object_t* dfsch_byte_vector_subvector(dfsch_object_t* bv,
                                              size_t off,
                                              size_t len);

  dfsch_object_t* dfsch_string_assoc(dfsch_object_t* alist,
                                     char* string);

  char* dfsch_strbuf_2_safe_filename(dfsch_strbuf_t* buf,
                                     int preserve_uppercase);

  dfsch_object_t* dfsch_string_trim(dfsch_strbuf_t* string,
                                    dfsch_strbuf_t* bag,
                                    int side);
  dfsch_object_t* dfsch_byte_vector_trim(dfsch_strbuf_t* string,
                                         dfsch_strbuf_t* bag,
                                         int side);


#ifdef __cplusplus
}
#endif

#endif
