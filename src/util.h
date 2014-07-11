/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Utility functions
 * Copyright (C) 2005-2014 Ales Hakl
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

#ifndef H__dfsch___util__
#define H__dfsch___util__

#include "dfsch/util.h"

#define SL_BUF_LEN 512

typedef struct str_li_t str_li_t;
typedef dfsch_str_list_t str_list_t;
struct dfsch_str_list_t {
  str_li_t* head;
  str_li_t* tail;
  size_t len;

  char buf[SL_BUF_LEN];
  size_t buf_used;
};
struct str_li_t {
  char* str;
  size_t len;
  str_li_t* next;
};

#define ASCII_tolower(c) ((c)<='Z'&&(c)>='A'?(c)+('a'-'A'):(c))

#define sl_create dfsch_sl_create
#define sl_append dfsch_sl_append
#define sl_printf dfsch_sl_printf
#define sl_nappend dfsch_sl_nappend
#define sl_value dfsch_sl_value
#define sl_value_strbuf dfsch_sl_value_strbuf
#define stracat dfsch_stracat
#define strancat dfsch_strancat
#define stracpy dfsch_stracpy
#define strancpy dfsch_strancpy
#define straquote dfsch_straquote
#define ascii_strcasecmp dfsch_ascii_strcasecmp

#define create_finalized_mutex dfsch_create_finalized_mutex
#define create_finalized_cvar dfsch_create_finalized_cvar
#define create_finalized_rwlock dfsch_create_finalized_rwlock

#define vsaprintf dfsch_vsaprintf
#define saprintf dfsch_saprintf

#endif
