#include "dfsch/ports.h"

#include <dfsch/object.h>
#include <dfsch/magic.h>
#include "internal.h"
#include "util.h"

/*
 * Idea is that final implementation will fallback to calling these methods
 * on port object, but this is currently not of high priority and tedious 
 * to get right. So these selectors are currently unused and port operations 
 * on non-port object fail unconditionally.
 */
DFSCH_LOCAL_SYMBOL_CACHE("write-buf!", sel_write_buf);
DFSCH_LOCAL_SYMBOL_CACHE("read-buf!", sel_read_buf);
DFSCH_LOCAL_SYMBOL_CACHE("seek!", sel_seek);
DFSCH_LOCAL_SYMBOL_CACHE("tell", sel_tell);
DFSCH_LOCAL_SYMBOL_CACHE("batch-read-start", sel_batch_read_start);
DFSCH_LOCAL_SYMBOL_CACHE("batch-read-end", sel_batch_read_end);
DFSCH_LOCAL_SYMBOL_CACHE("batch-read", sel_batch_read);

const dfsch_type_t dfsch_port_type_type = {
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_port_type_t),
  "port-type",
  NULL,
  NULL,
  NULL
};

int dfsch_port_p(dfsch_object_t* port){
  return port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE;
}
int dfsch_output_port_p(dfsch_object_t* port){
  return port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE &&
    ((dfsch_port_type_t*)(port->type))->write_buf;
}
int dfsch_input_port_p(dfsch_object_t* port){
  return port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE &&
    ((dfsch_port_type_t*)(port->type))->read_buf;
}


ssize_t dfsch_port_write_buf(dfsch_object_t* port, char*buf, size_t size){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->write_buf){
      return ((dfsch_port_type_t*)(port->type))->write_buf(port, buf, size);
    } else {
      dfsch_error("exception:not-an-output-port", port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
ssize_t dfsch_port_read_buf(dfsch_object_t* port, char*buf, size_t size){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->read_buf){
      return ((dfsch_port_type_t*)(port->type))->read_buf(port, buf, size);
    } else {
      dfsch_error("exception:not-an-input-port", port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
int dfsch_port_seek(dfsch_object_t* port, off_t offset, int whence){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->seek){
      return ((dfsch_port_type_t*)(port->type))->seek(port, offset, whence);
    } else {
      dfsch_error("exception:port-not-seekable", port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
off_t dfsch_port_tell(dfsch_object_t* port){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->tell){
      return ((dfsch_port_type_t*)(port->type))->tell(port);
    } else {
      return -1;
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}

void dfsch_port_batch_read_start(dfsch_object_t* port){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->batch_read_start){
      ((dfsch_port_type_t*)(port->type))->batch_read_start(port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
void dfsch_port_batch_read_end(dfsch_object_t* port){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->batch_read_end){
      ((dfsch_port_type_t*)(port->type))->batch_read_end(port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
int dfsch_port_batch_read(dfsch_object_t* port){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->batch_read){
      return ((dfsch_port_type_t*)(port->type))->batch_read(port);
    } else {
      char buf;
      if (dfsch_port_read_buf(port, &buf, 1) != 1){
        return -1;
      } else {
        return buf;
      }
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}

dfsch_strbuf_t* dfsch_port_readline(dfsch_object_t* port){
  int ch;
  char* buf;
  size_t buflen;
  size_t len;

  buflen = 128;
  len = 0;
  buf = GC_MALLOC_ATOMIC(buflen);

  dfsch_port_batch_read_start(port);
  DFSCH_UNWIND {
    while (1){
      ch = dfsch_port_batch_read(port);
      if (ch == -1){
        break;
      }

      if (buflen <= len){
        buflen *= 2;
        buf = GC_REALLOC(buf, buflen);
      }

      buf[len] = ch;
      len++;

      if (ch == '\n'){
        break;
      }
    }

    if (len == 0){
      return NULL;
    }
  } DFSCH_PROTECT {
    dfsch_port_batch_read_end(port);
  } DFSCH_END_UNWIND;

  return dfsch_strbuf_create(buf, len);
}


typedef struct current_ports_t{
  dfsch_object_t* output_port;
  dfsch_object_t* input_port;
} current_ports_t;

current_ports_t* current_ports(){
  static current_ports_t p = {NULL, NULL};
  return &p;
}

dfsch_object_t* dfsch_current_output_port(){
  return current_ports()->output_port;
}
dfsch_object_t* dfsch_current_input_port(){
  return current_ports()->input_port;
}

void dfsch_set_current_output_port(dfsch_object_t* port){
  if (!dfsch_output_port_p(port)){
    dfsch_error("exception:not-an-output-port", port);
  }
  current_ports()->output_port = port;
}
void dfsch_set_current_input_port(dfsch_object_t* port){
  if (!dfsch_input_port_p(port)){
    dfsch_error("exception:not-an-input-port", port);
  }
  current_ports()->input_port = port;  
}

static dfsch_object_t* native_current_output_port(void* baton,
                                                  dfsch_object_t* args,
                                                  dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);
  return dfsch_current_output_port();
}
static dfsch_object_t* native_current_input_port(void* baton,
                                                 dfsch_object_t* args,
                                                 dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);
  return dfsch_current_input_port();
}
static dfsch_object_t* native_set_current_output_port(void* baton,
                                                      dfsch_object_t* args,
                                                      dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);
  dfsch_set_current_output_port(port);
  return NULL;
}
static dfsch_object_t* native_set_current_input_port(void* baton,
                                                     dfsch_object_t* args,
                                                     dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);
  dfsch_set_current_input_port(port);
  return NULL;
}



void dfsch__port_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "current-output-port", 
                    dfsch_make_primitive(native_current_output_port, NULL));
  dfsch_define_cstr(ctx, "current-input-port", 
                    dfsch_make_primitive(native_current_input_port, NULL));
}
void dfsch_port_unsafe_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "set-current-output-port!", 
                    dfsch_make_primitive(native_set_current_output_port, NULL));
  dfsch_define_cstr(ctx, "set-current-input-port!", 
                    dfsch_make_primitive(native_set_current_input_port, NULL));
  
}
