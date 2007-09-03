#include "dfsch/port.h"

#include <dfsch/object.h>
#include <dfsch/magic.h>

DFSCH_LOCAL_SYMBOL_CACHE("write-buf!", sel_write_buf);
DFSCH_LOCAL_SYMBOL_CACHE("read-buf!", sel_read_buf);
DFSCH_LOCAL_SYMBOL_CACHE("seek!", sel_seek);
DFSCH_LOCAL_SYMBOL_CACHE("tell", sel_tell);
DFSCH_LOCAL_SYMBOL_CACHE("batch-read-start", sel_tell);
DFSCH_LOCAL_SYMBOL_CACHE("batch-read-end", sel_tell);
DFSCH_LOCAL_SYMBOL_CACHE("batch-read", sel_tell);

ssize_t dfsch_port_write_buf(dfsch_object_t* port, char*buf, size_t size){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->write_buf){
      return ((dfsch_port_type_t)(port->type))->write_buf(port, buf, size);
    } else {
      dfsch_throw("exception:port-not-writable", port);
    }
  }

}
ssize_t dfsch_port_read_buf(dfsch_object_t* port, char*buf, size_t size){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->read_buf){
      return ((dfsch_port_type_t)(port->type))->read_buf(port, buf, size);
    } else {
      dfsch_throw("exception:port-not-readable", port);
    }
  }
}
int dfsch_port_seek(dfsch_object_t* port, off_t offset, int whence){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->seek){
      return ((dfsch_port_type_t)(port->type))->seek(port, buf, size);
    } else {
      dfsch_throw("exception:port-not-seekable", port);
    }
  }
}
off_t dfsch_port_tell(dfsch_object_t* port){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->tell){
      return ((dfsch_port_type_t)(port->type))->tell(port);
    } else {
      return -1;
    }
  }
}

void dfsch_port_batch_read_start(dfsch_object_t* port){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->batch_read_start){
      ((dfsch_port_type_t)(port->type))->batch_read_start(port);
    }
  }
}
void dfsch_port_batch_read_end(dfsch_object_t* port){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->batch_read_end){
      ((dfsch_port_type_t)(port->type))->batch_read_end(port);
    }
  }
}
int dfsch_port_batch_read(dfsch_object_t* port){
  if (port->type->type = DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t)(port->type))->batch_read_end){
      return ((dfsch_port_type_t)(port->type))->batch_read(port);
    } else {
      char buf;
      if (dfsch_port_read_buf(port, &buf, 1) != 1){
        return -1;
      } else {
        return buf;
      }
    }
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
