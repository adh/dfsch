/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Utility functions
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

#ifndef H__dfsch___util__
#define H__dfsch___util__

#include "dfsch/util.h"

typedef struct str_li_t str_li_t;
typedef dfsch_str_list_t str_list_t;
struct dfsch_str_list_t {
  str_li_t* head;
  str_li_t* tail;
  size_t len;
};
struct str_li_t {
  char* str;
  size_t len;
  str_li_t* next;
};

#define ASCII_tolower(c) ((c)<='Z'&&(c)>='A'?(c)+('a'-'A'):(c))


#define sl_create dfsch_sl_create
#define sl_append dfsch_sl_append
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

#define vsaprintf dfsch_vsaprintf
#define saprintf dfsch_saprintf

#endif
