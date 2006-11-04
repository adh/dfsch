/*
 * dfsch_import - Library for loading scheme and C code into dfsch interpreter
 * Copyright (C) 2005 Ales Hakl
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
#ifndef H__dfsch__load__
#define H__dfsch__load__

#include <dfsch/dfsch.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

  /** Load scheme file into given context. Search for file in directories named
      in scheme variable load:path. File is loaded into top level environment 
      of context and only top level definition of load:path is taken into 
      account */
  extern dfsch_object_t* dfsch_load_scm(dfsch_object_t* ctx, char* scm_name);
  /** Append directory to load:path */
  dfsch_object_t* dfsch_load_extend_path(dfsch_object_t* ctx, char* dir);
  /** Read scheme list from given file. */
  extern dfsch_object_t* dfsch_read_scm(char* scm_name);
  /** Read scheme list from given file descriptor. */
  extern dfsch_object_t* dfsch_read_scm_fd(int f, 
                                           char* name);
  /** Read scheme list from given stdio stream. */
  extern dfsch_object_t* dfsch_read_scm_stream(FILE* f, 
                                               char* name);

  /** Load given binary module and register it into given context. */
  extern dfsch_object_t* dfsch_load_so(dfsch_object_t* ctx, 
                                       char* so_name, 
                                       char* sym_name);

  /** Register shared object related part of this module. */
  extern dfsch_object_t* dfsch_load_so_register(dfsch_object_t *ctx);
  /** Register scheme related part of this module. */
  extern dfsch_object_t* dfsch_load_scm_register(dfsch_object_t *ctx);
  /** Register this module. */
  extern dfsch_object_t* dfsch_load_register(dfsch_object_t *ctx);

#ifdef __cplusplus
}
#endif

#endif
