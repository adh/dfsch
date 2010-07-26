/*
 * dfsch - Scheme-like Lisp dialect
 *   Sub-process handling
 * Copyright (C) 2009 Ales Hakl
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

#ifndef H__dfsch__zlib__
#define H__dfsch__zlib__

#include <dfsch/dfsch.h>
#include <dfsch/ports.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_type_t dfsch_gzip_port_type;
#define DFSCH_GZIP_PORT_TYPE (&dfsch_gzip_port_type)
  extern dfsch_port_type_t dfsch_process_gzip_port_type;
#define DFSCH_GZIP_OUTPUT_PORT_TYPE (&dfsch_gzip_output_port_type)
  extern dfsch_port_type_t dfsch_process_gzip_port_type;
#define DFSCH_GZIP_INPUT_PORT_TYPE (&dfsch_gzip_input_port_type)

  dfsch_object_t* dfsch_gzip_open_for_input(char* filename);
  dfsch_object_t* dfsch_gzip_open_for_output(char* filename);
  dfsch_object_t* dfsch_gzip_open_for_append(char* filename);

  void dfsch_gzip_close_port(dfsch_object_t* port);

#ifdef __cplusplus
}
#endif

#endif
