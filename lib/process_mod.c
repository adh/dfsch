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

#include "dfsch/lib/process.h"
#include <string.h>
#include <errno.h>

DFSCH_DEFINE_PRIMITIVE(spawn, NULL){
  char* cmd_line;
  int r;
  DFSCH_STRING_ARG(args, cmd_line);
  DFSCH_ARG_END(args);
  
  r = system(cmd_line);

  if (r == -1){
    dfsch_error("Cannot spawn process",
                dfsch_make_string_cstr(strerror(errno)));
  }
  return DFSCH_MAKE_FIXNUM(r);
}

DFSCH_DEFINE_PRIMITIVE(spawn_with_input_port, NULL){
  char* cmd_line;
  DFSCH_STRING_ARG(args, cmd_line);
  DFSCH_ARG_END(args);
  return dfsch_process_spawn_with_input_port(cmd_line);
}
DFSCH_DEFINE_PRIMITIVE(spawn_with_output_port, NULL){
  char* cmd_line;
  DFSCH_STRING_ARG(args, cmd_line);
  DFSCH_ARG_END(args);
  return dfsch_process_spawn_with_output_port(cmd_line);
}

DFSCH_DEFINE_PRIMITIVE(close_port, NULL){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);

  return dfsch_process_close_port(port);
}


dfsch_object_t* dfsch_module_process_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "process:<port>", 
                    DFSCH_PROCESS_PORT_TYPE);
  dfsch_define_cstr(ctx, "process:<input-port>", 
                    DFSCH_PROCESS_INPUT_PORT_TYPE);
  dfsch_define_cstr(ctx, "process:<output-port>", 
                    DFSCH_PROCESS_OUTPUT_PORT_TYPE);

  dfsch_define_cstr(ctx, "process:spawn!",
                    DFSCH_PRIMITIVE_REF(spawn));
  dfsch_define_cstr(ctx, "process:spawn-with-input-port!",
                    DFSCH_PRIMITIVE_REF(spawn_with_input_port));
  dfsch_define_cstr(ctx, "process:spawn-with-output-port!",
                    DFSCH_PRIMITIVE_REF(spawn_with_output_port));
  dfsch_define_cstr(ctx, "process:close-port!",
                    DFSCH_PRIMITIVE_REF(close_port));
}
