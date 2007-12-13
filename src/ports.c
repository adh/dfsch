#include "dfsch/ports.h"

#include <dfsch/strings.h>
#include <dfsch/object.h>
#include <dfsch/number.h>
#include <dfsch/magic.h>
#include <dfsch/parse.h>
#include "internal.h"
#include "util.h"

#include <string.h>

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


void dfsch_port_write_buf(dfsch_object_t* port, char*buf, size_t size){
  if (port && port->type && port->type->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(port->type))->write_buf){
      ((dfsch_port_type_t*)(port->type))->write_buf(port, buf, size);
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
  } DFSCH_PROTECT {
    dfsch_port_batch_read_end(port);
  } DFSCH_END_UNWIND;

  if (len == 0){
    return NULL;
  }

  return dfsch_strbuf_create(buf, len);
}

/*
 * null-port
 *
 * Discards anything written, reads return EOF
 */

typedef struct null_port_t {
  dfsch_port_type_t* type;
} null_port_t;

static void null_port_write_buf(dfsch_object_t* port, 
                               char*buf, size_t len){
}
static ssize_t null_port_read_buf(dfsch_object_t* port, 
                                  char*buf, size_t len){
  return 0;
}

static dfsch_port_type_t null_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    sizeof(null_port_t),
    "null-port",
    NULL,
    NULL,
    NULL
  },
  null_port_write_buf,
  null_port_read_buf,

  NULL,
  NULL,

  NULL,
  NULL,
  NULL
};

static null_port_t null_port = {
  &null_port_type
};

dfsch_object_t* dfsch_null_port(){
  return (dfsch_object_t*) &null_port;
}

/*
 * current-foo-port implementation
 */

/*
 * TODO: It's actually not bad idea to have this structure thread-specific
 * (it's more or less required for reliable operation) but this suffices
 * as current implementation.
 */

typedef struct current_ports_t{
  dfsch_object_t* output_port;
  dfsch_object_t* input_port;
  dfsch_object_t* error_port;
} current_ports_t;

current_ports_t* current_ports(){
  static current_ports_t p = {&null_port, &null_port, &null_port};
  return &p;
}

dfsch_object_t* dfsch_current_output_port(){
  return current_ports()->output_port;
}
dfsch_object_t* dfsch_current_input_port(){
  return current_ports()->input_port;
}
dfsch_object_t* dfsch_current_error_port(){
  return current_ports()->error_port;
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
void dfsch_set_current_error_port(dfsch_object_t* port){
  if (!dfsch_output_port_p(port)){
    dfsch_error("exception:not-an-output-port", port);
  }
  current_ports()->error_port = port;
}

/*
 * string-output-port
 */

typedef struct string_output_port_t {
  dfsch_type_t* type;
  str_list_t* list;
  pthread_mutex_t* mutex;
} string_output_port_t;


static void string_output_port_write_buf(string_output_port_t* port, 
                               char*buf, size_t len){
  pthread_mutex_lock(port->mutex);
  sl_nappend(port->list, buf, len);
  pthread_mutex_unlock(port->mutex);
}

static dfsch_port_type_t string_output_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    sizeof(string_output_port_t),
    "string-output-port",
    NULL,
    NULL,
    NULL
  },
  string_output_port_write_buf,
  NULL,

  NULL,
  NULL,

  NULL,
  NULL,
  NULL
};

dfsch_object_t* dfsch_string_output_port(){
  string_output_port_t* port = 
    (string_output_port_t*)dfsch_make_object(&string_output_port_type);

  port->mutex = create_finalized_mutex();
  port->list = sl_create();

  return port;
}
dfsch_strbuf_t* dfsch_string_output_port_value(dfsch_object_t* port){
  string_output_port_t* p;
  dfsch_strbuf_t* buf;

  if (!port || port->type != &string_output_port_type){
    dfsch_error("exceptiion:not-a-string-output-port", port);
  }
  p = (string_output_port_t*) port;

  pthread_mutex_lock(p->mutex);
  buf = sl_value_strbuf(p->list);
  pthread_mutex_unlock(p->mutex);
  
  return buf;
}

/*
 * string-input-port
 */

typedef struct string_input_port_t {
  dfsch_type_t* type;
  size_t len;
  size_t cur;
  char* buf;
  pthread_mutex_t* mutex;
} string_input_port_t;

static ssize_t string_input_port_read_buf(string_input_port_t* port,
                                          char* buf, size_t len){
  size_t rlen;

  pthread_mutex_lock(port->mutex);
  
  if (port->len < len + port->cur){
    len = port->len - port->cur;
  }

  memcpy(buf, port->buf + port->cur, len);
  port->cur += len;

  pthread_mutex_unlock(port->mutex);

  return len;
}

static dfsch_port_type_t string_input_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    sizeof(string_input_port_t),
    "string-input-port",
    NULL,
    NULL,
    NULL
  },
  NULL,
  string_input_port_read_buf,

  NULL,
  NULL,

  NULL, // TODO
  NULL,
  NULL
};

dfsch_object_t* dfsch_string_input_port(char* buf, size_t len){
  string_input_port_t* port = 
    (string_input_port_t*)dfsch_make_object(&string_input_port_type);

  port->buf = buf;
  port->len = len;
  port->cur = 0;
  port->mutex = create_finalized_mutex();

  return port;
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
static dfsch_object_t* native_current_error_port(void* baton,
                                                 dfsch_object_t* args,
                                                 dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);
  return dfsch_current_error_port();
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
static dfsch_object_t* native_set_current_error_port(void* baton,
                                                     dfsch_object_t* args,
                                                     dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);
  dfsch_set_current_error_port(port);
  return NULL;
}
static dfsch_object_t* native_null_port(void* baton,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);
  return dfsch_null_port();
}
static dfsch_object_t* native_write(void* baton,
                                    dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  dfsch_object_t* object;
  char *buf;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_ARG_END(args);

  buf = dfsch_obj_write(object, 1, 1000);
  dfsch_port_write_buf(port, buf, strlen(buf));
  
  return NULL;
}
static dfsch_object_t* native_display(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  dfsch_object_t* object;
  char *buf;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_ARG_END(args);

  buf = dfsch_obj_write(object, 0, 1000);
  dfsch_port_write_buf(port, buf, strlen(buf));
  
  return NULL;
}
static dfsch_object_t* native_newline(void* baton,
                                      dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  char *buf;
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_ARG_END(args);

  dfsch_port_write_buf(port, "\n", 1);
  
  return NULL;
}

static dfsch_object_t* native_read(void* baton,
                                   dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  char *buf;
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  return dfsch_parser_read_from_port(port);
}

static dfsch_object_t* native_port_read_buf(void* baton,
                                            dfsch_object_t* args,
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  size_t len;
  char* buf;
  DFSCH_LONG_ARG(args, len);
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  buf = GC_MALLOC_ATOMIC(len);
  len = dfsch_port_read_buf(port, buf, len);
  
  if (len == 0){
    return NULL;
  }

  return dfsch_make_string_buf(buf, len);
}

static dfsch_object_t* native_port_read_line(void* baton,
                                             dfsch_object_t* args,
                                             dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  dfsch_strbuf_t* buf;
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  buf = dfsch_port_readline(port);
  if (!buf){
    return NULL;
  }

  return dfsch_make_string_strbuf(buf);
}

static dfsch_object_t* native_port_write_buf(void* baton,
                                             dfsch_object_t* args,
                                             dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  dfsch_strbuf_t* buf;
  DFSCH_BUFFER_ARG(args, buf);
  DFSCH_OBJECT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  dfsch_port_write_buf(port, buf->ptr, buf->len);

  return NULL;
}
static dfsch_object_t* native_string_output_port(void* baton,
                                                 dfsch_object_t* args,
                                                 dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);
  return dfsch_string_output_port();
}
static dfsch_object_t* native_string_output_port_value(void* baton,
                                                       dfsch_object_t* args,
                                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);

  return dfsch_make_string_strbuf(dfsch_string_output_port_value(port));
}
static dfsch_object_t* native_string_input_port(void* baton,
                                                dfsch_object_t* args,
                                                dfsch_tail_escape_t* esc){
  dfsch_strbuf_t* string;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_input_port(string->ptr, string->len);
}


void dfsch__port_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "current-output-port", 
                    dfsch_make_primitive(native_current_output_port, NULL));
  dfsch_define_cstr(ctx, "current-input-port", 
                    dfsch_make_primitive(native_current_input_port, NULL));
  dfsch_define_cstr(ctx, "current-error-port", 
                    dfsch_make_primitive(native_current_error_port, NULL));
  dfsch_define_cstr(ctx, "null-port", 
                    dfsch_make_primitive(native_null_port, NULL));
  dfsch_define_cstr(ctx, "write", 
                    dfsch_make_primitive(native_write, NULL));
  dfsch_define_cstr(ctx, "display", 
                    dfsch_make_primitive(native_display, NULL));
  dfsch_define_cstr(ctx, "newline", 
                    dfsch_make_primitive(native_newline, NULL));
  dfsch_define_cstr(ctx, "read", 
                    dfsch_make_primitive(native_read, NULL));

  dfsch_define_cstr(ctx, "port-write-buf", 
                    dfsch_make_primitive(native_port_read_buf, NULL));
  dfsch_define_cstr(ctx, "port-read-buf", 
                    dfsch_make_primitive(native_port_read_buf, NULL));
  dfsch_define_cstr(ctx, "port-read-line", 
                    dfsch_make_primitive(native_port_read_line, NULL));

  dfsch_define_cstr(ctx, "string-output-port", 
                    dfsch_make_primitive(native_string_output_port, NULL));
  dfsch_define_cstr(ctx, "string-output-port-value", 
                    dfsch_make_primitive(native_string_output_port_value,
                                         NULL));
  dfsch_define_cstr(ctx, "string-input-port", 
                    dfsch_make_primitive(native_string_input_port, NULL));

}
void dfsch_port_unsafe_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "set-current-output-port!", 
                    dfsch_make_primitive(native_set_current_output_port, NULL));
  dfsch_define_cstr(ctx, "set-current-input-port!", 
                    dfsch_make_primitive(native_set_current_input_port, NULL));
  dfsch_define_cstr(ctx, "set-current-error-port!", 
                    dfsch_make_primitive(native_set_current_error_port, NULL));
  
}
