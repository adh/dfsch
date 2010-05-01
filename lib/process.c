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

typedef struct processs_port_t {
  dfsch_type_t* type;
  FILE* file;
  int open;
  char* cmd_line;
} process_port_t;

static void process_port_write(process_port_t* port, 
                               dfsch_writer_state_t* state){
  if (port->open){
    dfsch_write_unreadable(state, port, 
                           "name %s fd %d", 
                           port->cmd_line,
                           fileno(port->file));
  } else {
    dfsch_write_unreadable(state, port, "*closed*");
  }
}

dfsch_type_t dfsch_process_port_type = {
  DFSCH_ABSTRACT_TYPE,
  DFSCH_PORT_TYPE,
  sizeof(process_port_t),
  "process:port",
  NULL,
  (dfsch_type_write_t)process_port_write,
  NULL
};

static void port_write_buf(process_port_t* port, 
                           char*buf, size_t len){
  size_t ret;
  
  if (!port->open){
    dfsch_error("Port is already closed", port);
  }
  
  if (len != 0){
    ret = fwrite(buf, len, 1, port->file);
    if (ret == 0){
      dfsch_error("Error writing to process port", strerror(errno));
    }
  }
}

dfsch_port_type_t dfsch_process_output_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PROCESS_PORT_TYPE,
    sizeof(process_port_t),
    "process-output-port",
    NULL,
    NULL,
    NULL
  },
  (dfsch_port_write_buf_t)port_write_buf,
  NULL,
  
  NULL,
  NULL,

  NULL, // TODO
  NULL,
  NULL
};

static ssize_t port_read_buf(process_port_t* port,
                             char* buf, size_t len){
  size_t ret;

  if (!port->open){
    dfsch_error("Port is already closed", port);
  }

  ret = fread(buf, 1, len, port->file);
  if (ret == 0){
    if (feof(port->file)){
      return 0;
    } else {
      dfsch_error("Error reading from process port", strerror(errno));
    }
  }
  return ret;
}


dfsch_port_type_t dfsch_process_input_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PROCESS_PORT_TYPE,
    sizeof(process_port_t),
    "process-input-port",
    NULL,
    NULL,
    NULL
  },
  NULL,
  (dfsch_port_read_buf_t)port_read_buf,
  
  NULL,
  NULL,

  NULL, // TODO
  NULL,
  NULL
};

static void port_finalizer(process_port_t* port, void* cd){
  if (port->open){
    pclose(port->file);
    port->open = 0;
  }
}

static dfsch_object_t* spawn_port(dfsch_object_t* klass,
                                  char* cmd_line){
  process_port_t* p = dfsch_make_object(klass);

  p->cmd_line = cmd_line;

  if (klass == DFSCH_PROCESS_INPUT_PORT_TYPE){
    p->file = popen(cmd_line, "r");
  } else {
    p->file = popen(cmd_line, "w");
  }

  if (!p->file){
    dfsch_error("Cannot spawn process",
                dfsch_make_string_cstr(strerror(errno)));
  }

  GC_REGISTER_FINALIZER(p, (GC_finalization_proc)port_finalizer,
                        NULL, NULL, NULL);

  p->open = 1;

  return (dfsch_object_t*)p;
}

dfsch_object_t* dfsch_process_spawn_with_input_port(char* cmd_line){
  return spawn_port(DFSCH_PROCESS_INPUT_PORT_TYPE, cmd_line);
}
dfsch_object_t* dfsch_process_spawn_with_output_port(char* cmd_line){
  return spawn_port(DFSCH_PROCESS_OUTPUT_PORT_TYPE, cmd_line); 
}

dfsch_object_t* dfsch_process_close_port(dfsch_object_t* port){
  process_port_t* p = DFSCH_ASSERT_INSTANCE(port, DFSCH_PROCESS_PORT_TYPE);
  int r;
  if (p->open){
    p->open = 0;
    r = pclose(p->file);
    if (r == -1){
      dfsch_error("Error while closing process port", 
                  dfsch_make_string_cstr(strerror(errno)));
    }
    return DFSCH_MAKE_FIXNUM(r);
  }
  return NULL;
}

