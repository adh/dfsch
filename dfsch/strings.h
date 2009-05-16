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

  extern int dfsch_string_p(dfsch_object_t* obj);

  extern dfsch_strbuf_t* dfsch_strbuf_create(char* ptr, size_t len);
  
  extern dfsch_object_t* dfsch_make_string_cstr(char* string);
  extern dfsch_object_t* dfsch_make_string_strbuf(dfsch_strbuf_t* strbuf);
  extern dfsch_object_t* dfsch_make_string_buf(char* ptr, size_t len);

  extern dfsch_object_t* dfsch_make_string_nocopy(dfsch_strbuf_t* buf);

  extern char* dfsch_string_to_cstr(dfsch_object_t* obj);
  extern dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj);
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

  extern char dfsch_string_ref(dfsch_object_t* string, size_t index);

  extern size_t dfsch_string_length(dfsch_object_t* string);

  extern dfsch_object_t* dfsch_string_substring(dfsch_object_t* string, 
                                                size_t start,
                                                size_t end);

  extern dfsch_object_t* dfsch_string_substring_utf8(dfsch_object_t* string, 
                                                     size_t start,
                                                     size_t end);

  extern dfsch_object_t* dfsch_string_2_list(dfsch_object_t* string);


#define DFSCH_STRING_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_string_to_cstr)
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
                                            dfsch_strbuf_t* separator);

#ifdef __cplusplus
}
#endif

#endif
