/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   I/O ports
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "dfsch/ports.h"

#include <dfsch/strings.h>
#include <dfsch/object.h>
#include <dfsch/number.h>
#include <dfsch/magic.h>
#include <dfsch/parse.h>
#include "internal.h"
#include "util.h"

#include <string.h>
#include <errno.h>

/*
 * Idea is that final implementation will fallback to calling these methods
 * on port object, but this is currently not of high priority and tedious 
 * to get right. So these selectors are currently unused and port operations 
 * on non-port object fail unconditionally.
 */

dfsch_type_t dfsch_port_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "port",
  NULL,
  NULL,
  NULL
};

dfsch_type_t dfsch_port_type_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_port_type_t),
  "port-type",
  NULL,
  NULL,
  NULL
};

int dfsch_port_p(dfsch_object_t* port){
  return DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE;
}
int dfsch_output_port_p(dfsch_object_t* port){
  return DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE &&
    ((dfsch_port_type_t*)DFSCH_TYPE_OF(port))->write_buf;
}
int dfsch_input_port_p(dfsch_object_t* port){
  return DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE &&
    ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->read_buf;
}


void dfsch_port_write_buf(dfsch_object_t* port, char*buf, size_t size){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->write_buf){
      ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->write_buf(port, buf, size);
    } else {
      dfsch_error("exception:not-an-output-port", port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
void dfsch_port_write_cstr(dfsch_object_t* port, char*str){
  dfsch_port_write_buf(port, str, strlen(str));
}


ssize_t dfsch_port_read_buf(dfsch_object_t* port, char*buf, size_t size){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->read_buf){
      return ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->read_buf(port, 
                                                                   buf, size);
    } else {
      dfsch_error("exception:not-an-input-port", port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}

dfsch_strbuf_t* dfsch_port_read_whole(dfsch_object_t* port){
  ssize_t ret;
  char* buf = GC_MALLOC_ATOMIC(1024);
  str_list_t* sl = sl_create();
  while ((ret = dfsch_port_read_buf(port, buf, 1024))){
    sl_nappend(sl, buf, ret);
    buf = GC_MALLOC_ATOMIC(1024);
  }
  return sl_value_strbuf(sl);
}

void dfsch_port_seek(dfsch_object_t* port, int64_t offset, int whence){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->seek){
      ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->seek(port, offset, whence);
    } else {
      dfsch_error("exception:port-not-seekable", port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
int64_t dfsch_port_tell(dfsch_object_t* port){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->tell){
      return ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->tell(port);
    } else {
      return -1;
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}

void dfsch_port_batch_read_start(dfsch_object_t* port){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_start){
      ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_start(port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
void dfsch_port_batch_read_end(dfsch_object_t* port){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_end){
      ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_end(port);
    }
  } else {
    dfsch_error("exception:not-a-port", port);
  }
}
int dfsch_port_batch_read(dfsch_object_t* port){
  if (DFSCH_TYPE_OF(port)->type == DFSCH_PORT_TYPE_TYPE){
    if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read){
      return ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read(port);
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
  } DFSCH_PROTECT_END;

  if (len == 0){
    return NULL;
  }

  return dfsch_strbuf_create(buf, len);
}

/*
 * eof object
 */

typedef struct eof_object_t{
  dfsch_type_t* type;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR eof_object_t;

dfsch_type_t dfsch_eof_object_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(eof_object_t),
  "eof-object",
  NULL,
  NULL,
  NULL
};

eof_object_t eof_object = {
  &dfsch_eof_object_type
};

dfsch_object_t* dfsch_eof_object(){
  return (dfsch_object_t*)&eof_object;
}
int dfsch_eof_object_p(dfsch_object_t* obj){
  return obj == &eof_object;
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

dfsch_port_type_t dfsch_null_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PORT_TYPE,
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
  &dfsch_null_port_type
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

dfsch_port_type_t dfsch_string_output_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PORT_TYPE,
    sizeof(string_output_port_t),
    "string-output-port",
    NULL,
    NULL,
    NULL
  },
  (dfsch_port_write_buf_t)string_output_port_write_buf,
  NULL,

  NULL,
  NULL,

  NULL,
  NULL,
  NULL
};

dfsch_object_t* dfsch_string_output_port(){
  string_output_port_t* port = 
    (string_output_port_t*)dfsch_make_object(DFSCH_STRING_OUTPUT_PORT_TYPE);

  port->mutex = create_finalized_mutex();
  port->list = sl_create();

  return (dfsch_object_t*)port;
}
dfsch_strbuf_t* dfsch_string_output_port_value(dfsch_object_t* port){
  string_output_port_t* p;
  dfsch_strbuf_t* buf;

  if (DFSCH_TYPE_OF(port) != DFSCH_STRING_OUTPUT_PORT_TYPE){
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

dfsch_port_type_t dfsch_string_input_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PORT_TYPE,
    sizeof(string_input_port_t),
    "string-input-port",
    NULL,
    NULL,
    NULL
  },
  NULL,
  (dfsch_port_read_buf_t)string_input_port_read_buf,

  NULL,
  NULL,

  NULL, // TODO
  NULL,
  NULL
};

dfsch_object_t* dfsch_string_input_port(char* buf, size_t len){
  string_input_port_t* port = 
    (string_input_port_t*)dfsch_make_object(DFSCH_STRING_INPUT_PORT_TYPE);

  port->buf = buf;
  port->len = len;
  port->cur = 0;
  port->mutex = create_finalized_mutex();

  return (dfsch_object_t*)port;
}

/*
 * file-port - port based on stdio FILE*
 *
 * This port is somehow special, because it's both input-port? and output-port?
 * but whenever it's usable for input, output or both directions depends on 
 * flags used when opening.
 */

typedef struct file_port_t {
  dfsch_type_t* type;
  FILE* file;
  int close;
  int open;
  char* name;
} file_port_t;


static void errno_error(char* name, dfsch_object_t* object, int e){
  dfsch_error(name, dfsch_list(3, 
                               object,
                               dfsch_make_number_from_long(e),
                               dfsch_make_string_cstr(strerror(e))));
}

static void file_port_write_buf(file_port_t* port, 
                                char*buf, size_t len){
  size_t ret;

  if (!port->open){
    dfsch_error("exception:port-closed", port);
  }

  if (len != 0){
    ret = fwrite(buf, len, 1, port->file);
    if (ret == 0){
      errno_error("exception:file-port-write-failed",
                  (dfsch_object_t*)port,
                  errno);
    }
  }
}
static ssize_t file_port_read_buf(file_port_t* port,
                                  char* buf, size_t len){
  size_t ret;

  if (!port->open){
    dfsch_error("exception:port-closed", port);
  }

  ret = fread(buf, 1, len, port->file);
  if (ret == 0){
    if (feof(port->file)){
      return 0;
    } else {
      errno_error("exception:file-port-read-failed",
                  (dfsch_object_t*)port,
                  errno);
    }
  }
  return ret;
}
static void file_port_write(file_port_t* port, dfsch_writer_state_t* state){
  if (port->open){
    if (port->name){
      dfsch_write_unreadable(state, port, 
                             "name %s fd %d", 
                             port->name,
                             fileno(port->file));
    } else {
      dfsch_write_unreadable(state, port, "fd %d", fileno(port->file));
    }
  } else {
    dfsch_write_unreadable(state, port, "*closed*");
  }
}

static void file_port_seek(file_port_t* port, int64_t offset, int whence){
  if (!port->open){
    dfsch_error("exception:port-closed", port);
  }

  if (fseek(port->file, offset, whence) != 0){
    errno_error("exception:file-port-seek-failed",
                (dfsch_object_t*)port,
                errno);
  }
}

static int64_t file_port_tell(file_port_t* port){
  off_t ret;
  if (!port->open){
    dfsch_error("exception:port-closed", port);
  }

  ret = ftell(port->file);

  if (ret == -1){
    errno_error("exception:file-port-tell-failed",
                (dfsch_object_t*)port,
                errno);
  }

  return ret;
}

dfsch_port_type_t dfsch_file_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PORT_TYPE,
    sizeof(file_port_t),
    "file-port",
    NULL,
    (dfsch_type_write_t)file_port_write,
    NULL
  },
  (dfsch_port_write_buf_t)file_port_write_buf,
  (dfsch_port_read_buf_t)file_port_read_buf,

  (dfsch_port_seek_t)file_port_seek,
  (dfsch_port_tell_t)file_port_tell,

  NULL, // TODO
  NULL,
  NULL
};

static void file_port_finalizer(file_port_t* port, void* cd){
  if (port->open && port->close){
    fclose(port->file);
    port->open = 0;
  }
}

dfsch_object_t* dfsch_make_file_port(FILE* file, int close, char* name){
  file_port_t* port = (file_port_t*)dfsch_make_object(DFSCH_FILE_PORT_TYPE);

  port->file = file;
  port->close = close;
  port->name = name;
  port->open = 1; /* Creating closed ports makes no sense */

  if (close){
    GC_REGISTER_FINALIZER(port, (GC_finalization_proc)file_port_finalizer,
                          NULL, NULL, NULL);
  }

  return (dfsch_object_t*)port;
}

DFSCH_OBJECT_CACHE(dfsch_make_file_port(stdin, 0, "(standard input)"), 
                   dfsch_standard_input_port);
DFSCH_OBJECT_CACHE(dfsch_make_file_port(stdout, 0, "(standard output)"), 
                   dfsch_standard_output_port);
DFSCH_OBJECT_CACHE(dfsch_make_file_port(stderr, 0, "(standard error)"), 
                   dfsch_standard_error_port);

dfsch_object_t* dfsch_open_file_port(char* filename, char* mode){
  FILE* file;

  if (mode[0] != 'r' && mode[0] != 'w' && mode[0] != 'a'){ /// XXX
    dfsch_error("exception:invalid-file-port-mode", 
                dfsch_make_string_cstr(mode));
  }
  if (mode[1] != 0){
    if (mode[1] != '+' && mode[1] != 'b'){
      dfsch_error("exception:invalid-file-port-mode", 
                  dfsch_make_string_cstr(mode));
      
    }
    if (mode[2] != 0){
      if ((mode[2] != '+' && mode[2] != 'b') 
          || (mode[2] == mode[1])
          || (mode[3] != 0)){
        dfsch_error("exception:invalid-file-port-mode", 
                    dfsch_make_string_cstr(mode));
      }
    }
  }

  file = fopen(filename, mode);
  
  if (!file){
      errno_error("exception:file-port-open-failed",
                  dfsch_make_string_cstr(filename),
                  errno);
  }

  return dfsch_make_file_port(file, 1, filename);
}

void dfsch_close_file_port(dfsch_object_t* port){
  file_port_t* p;

  if (DFSCH_TYPE_OF(port) != DFSCH_FILE_PORT_TYPE){
    dfsch_error("exception:not-a-file-port", port);
  }

  p = (file_port_t*) port;

  if (p->close && p->open){
    fclose(p->file);
    p->open = 0;
  }  
}

/*
 * Scheme interface
 */

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

  buf = dfsch_object_2_string(object, 1000, 1);
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

  buf = dfsch_object_2_string(object, 1000, 0);
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

DFSCH_DEFINE_PRIMITIVE(port_read_whole, 0){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);
  return dfsch_make_string_strbuf(dfsch_port_read_whole(port));
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
static dfsch_object_t* native_port_seek(void* baton,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  int64_t offset;
  int whence = SEEK_SET;
  
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_INT64_ARG(args, offset);
  DFSCH_FLAG_PARSER_BEGIN_ONE_OPT(args, whence);
  DFSCH_FLAG_VALUE("set", SEEK_SET, whence);
  DFSCH_FLAG_VALUE("cur", SEEK_CUR, whence);
  DFSCH_FLAG_VALUE("end", SEEK_END, whence);
  DFSCH_FLAG_PARSER_END(args);

  dfsch_port_seek(port, offset, whence);

  return NULL;
}
static dfsch_object_t* native_port_tell(void* baton,
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  char *buf;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);
  
  return dfsch_make_number_from_long(dfsch_port_tell(port));
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
static dfsch_object_t* native_eof_object_p(void* baton,
                                           dfsch_object_t* args,
                                           dfsch_tail_escape_t* esc){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);  
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_eof_object_p(object));
}

static dfsch_object_t* native_open_file_port(void* baton,
                                             dfsch_object_t* args,
                                             dfsch_tail_escape_t* esc){
  char* fname;
  char* mode;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_STRING_ARG(args, mode);
  DFSCH_ARG_END(args);

  return dfsch_open_file_port(fname, mode);
}
static dfsch_object_t* native_close_file_port(void* baton,
                                              dfsch_object_t* args,
                                              dfsch_tail_escape_t* esc){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);

  dfsch_close_file_port(port);
  return NULL;
}

void dfsch__port_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<port>", DFSCH_PORT_TYPE);
  dfsch_define_cstr(ctx, "<null-port>", DFSCH_NULL_PORT_TYPE);
  dfsch_define_cstr(ctx, "<file-port>", DFSCH_FILE_PORT_TYPE);
  dfsch_define_cstr(ctx, "<string-input-port>", DFSCH_STRING_INPUT_PORT_TYPE);
  dfsch_define_cstr(ctx, "<string-output-port>", DFSCH_STRING_OUTPUT_PORT_TYPE);
  dfsch_define_cstr(ctx, "<eof-object>", DFSCH_EOF_OBJECT_TYPE);

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

  dfsch_define_cstr(ctx, "eof-object?", 
                    dfsch_make_primitive(native_eof_object_p, NULL));

  dfsch_define_cstr(ctx, "port-write-buf", 
                    dfsch_make_primitive(native_port_write_buf, NULL));
  dfsch_define_cstr(ctx, "port-read-buf", 
                    dfsch_make_primitive(native_port_read_buf, NULL));
  dfsch_define_cstr(ctx, "port-read-whole", 
                    DFSCH_PRIMITIVE_REF(port_read_whole));
  dfsch_define_cstr(ctx, "port-read-line", 
                    dfsch_make_primitive(native_port_read_line, NULL));
  dfsch_define_cstr(ctx, "port-seek!", 
                    dfsch_make_primitive(native_port_seek, NULL));
  dfsch_define_cstr(ctx, "port-tell", 
                    dfsch_make_primitive(native_port_tell, NULL));

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
  
  dfsch_define_cstr(ctx, "open-file-port", 
                    dfsch_make_primitive(native_open_file_port, NULL));
  dfsch_define_cstr(ctx, "close-file-port!", 
                    dfsch_make_primitive(native_close_file_port, NULL));

  dfsch_define_cstr(ctx, "*standard-input-port*",
                    dfsch_standard_input_port());
  dfsch_define_cstr(ctx, "*standard-output-port*",
                    dfsch_standard_output_port());
  dfsch_define_cstr(ctx, "*standard-error-port*",
                    dfsch_standard_error_port());
  

}
