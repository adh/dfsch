/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   I/O ports
 * Copyright (C) 2005-2010 Ales Hakl
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


void dfsch_port_write_buf(dfsch_port_t* port, char*buf, size_t size){
  if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->write_buf){
    int freshline = buf[size - 1] == '\n';
    ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->write_buf(port, buf, size);
    port->freshline = freshline;
  } else {
    dfsch_error("Not an output port", port);
  }
}
void dfsch_port_write_cstr(dfsch_port_t* port, char*str){
  dfsch_port_write_buf(port, str, strlen(str));
}


ssize_t dfsch_port_read_buf(dfsch_port_t* port, char*buf, size_t size){
  if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->read_buf){
    port->pushback = 0;
    return ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->read_buf(port, 
                                                                 buf, size);
  } else {
    dfsch_error("Not an input port", port);
  }
}

dfsch_strbuf_t* dfsch_port_read_whole(dfsch_port_t* port){
  ssize_t ret;
  char* buf = GC_MALLOC_ATOMIC(1024);
  str_list_t* sl = sl_create();
  while ((ret = dfsch_port_read_buf(port, buf, 1024))){
    sl_nappend(sl, buf, ret);
    buf = GC_MALLOC_ATOMIC(1024);
  }
  return sl_value_strbuf(sl);
}

void dfsch_port_seek(dfsch_port_t* port, int64_t offset, int whence){
  if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->seek){
    ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->seek(port, offset, whence);
  } else {
    dfsch_error("Port is not seekable", port);
  }
}
int64_t dfsch_port_tell(dfsch_port_t* port){
  if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->tell){
    return ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->tell(port);
  } else {
    return -1;
  }
}

void dfsch_port_batch_read_start(dfsch_port_t* port){
  if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_start){
    ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_start(port);
  }
}
void dfsch_port_batch_read_end(dfsch_port_t* port){
  if (((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_end){
    ((dfsch_port_type_t*)(DFSCH_TYPE_OF(port)))->batch_read_end(port);
  }
}
int dfsch_port_batch_read(dfsch_port_t* port){
  if (port->pushback){
    port->pushback = 0;
    return port->pushback_content;
  }
  if (port->type->batch_read){
    return port->type->batch_read(port);
  } else {
    char buf;
    if (dfsch_port_read_buf(port, &buf, 1) != 1){
      return -1;
    } else {
      return buf;
    }
  }
}

void dfsch_port_batch_unread(dfsch_port_t* port, char ch){
  if (port->pushback){
    dfsch_error("Unread called multiple times", port);
  }
  port->pushback = 1;
  port->pushback_content = ch;
}

void dfsch_port_freshline(dfsch_port_t* port){
  if (!port->freshline){
    port->freshline = 1;
    dfsch_port_write_buf(port, "\n", 1);
  }
}


dfsch_strbuf_t* dfsch_port_readline(dfsch_port_t* port){
  return dfsch_port_readline_len(port, 0);
}
dfsch_strbuf_t* dfsch_port_readline_len(dfsch_port_t* port,
                                        size_t max_len){
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

      if (ch == '\n' || (max_len && len == max_len)){
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
  return obj == (dfsch_object_t*)&eof_object;
}


/*
 * null-port
 *
 * Discards anything written, reads return EOF
 */

typedef struct null_port_t {
  dfsch_port_t port;
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

typedef struct current_ports_t{
  dfsch_object_t* output_port;
  dfsch_object_t* input_port;
  dfsch_object_t* error_port;
} current_ports_t;

static pthread_key_t current_ports_key;
static pthread_once_t current_ports_key_init = PTHREAD_ONCE_INIT;

static void current_ports_destroy(void* ptr){
  if (ptr){
    GC_FREE(ptr);
  }
}
static void current_ports_key_alloc(){
  pthread_key_create(&current_ports_key, current_ports_destroy);
}
static current_ports_t* current_ports(){
  current_ports_t* cp;
  pthread_once(&current_ports_key_init, current_ports_key_alloc);
  cp = pthread_getspecific(current_ports_key);
  if (DFSCH_UNLIKELY(!cp)){
    cp = GC_NEW_UNCOLLECTABLE(current_ports_t);
    cp->output_port = (dfsch_object_t*)&null_port;
    cp->input_port = (dfsch_object_t*)&null_port;
    cp->error_port = (dfsch_object_t*)&null_port;
    pthread_setspecific(current_ports_key, cp);
  }
  return cp;
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

void dfsch_set_current_output_port(dfsch_port_t* port){
  if (!dfsch_output_port_p(port)){
    dfsch_error("Not an output port", port);
  }
  current_ports()->output_port = port;
}
void dfsch_set_current_input_port(dfsch_port_t* port){
  if (!dfsch_input_port_p(port)){
    dfsch_error("Not an input port", port);
  }
  current_ports()->input_port = port;  
}
void dfsch_set_current_error_port(dfsch_port_t* port){
  if (!dfsch_output_port_p(port)){
    dfsch_error("Not an output port", port);
  }
  current_ports()->error_port = port;
}

/*
 * string-output-port
 */

typedef struct string_output_port_t {
  dfsch_port_t super;
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
    NULL,
    NULL,
    
    NULL,
    "Output only port backed by string"
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
    (string_output_port_t*)dfsch_make_object((dfsch_type_t*)
                                             DFSCH_STRING_OUTPUT_PORT_TYPE);

  port->mutex = create_finalized_mutex();
  port->list = sl_create();

  return (dfsch_object_t*)port;
}
dfsch_strbuf_t* dfsch_string_output_port_value(dfsch_object_t* port){
  string_output_port_t* p;
  dfsch_strbuf_t* buf;

  p = (string_output_port_t*)DFSCH_ASSERT_TYPE(port, 
                                               (dfsch_type_t*)
                                               DFSCH_STRING_OUTPUT_PORT_TYPE);

  pthread_mutex_lock(p->mutex);
  buf = sl_value_strbuf(p->list);
  pthread_mutex_unlock(p->mutex);
  
  return buf;
}

/*
 * string-input-port
 */

typedef struct string_input_port_t {
  dfsch_port_t super;
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
    NULL,
    NULL,

    NULL,
    "Input-only port backed by string"
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
    (string_input_port_t*)dfsch_make_object((dfsch_type_t*)
                                            DFSCH_STRING_INPUT_PORT_TYPE);

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
  dfsch_port_t super;
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
    dfsch_error("Port is closed", (dfsch_object_t*)port);
  }

  if (len != 0){
    ret = fwrite(buf, len, 1, port->file);
    if (ret == 0){
      dfsch_operating_system_error("fwrite");    
    }
  }
}
static ssize_t file_port_read_buf(file_port_t* port,
                                  char* buf, size_t len){
  size_t ret;

  if (!port->open){
    dfsch_error("Port is closed", (dfsch_object_t*)port);
  }

  ret = fread(buf, 1, len, port->file);
  if (ret == 0){
    if (feof(port->file)){
      return 0;
    } else {
      dfsch_operating_system_error("fread");    
    }
  }
  return ret;
}
static void file_port_write(file_port_t* port, dfsch_writer_state_t* state){
  if (port->open){
    if (port->name){
      dfsch_write_unreadable(state, (dfsch_object_t*)port, 
                             "name %s fd %d", 
                             port->name,
                             fileno(port->file));
    } else {
      dfsch_write_unreadable(state, (dfsch_object_t*)port, 
                             "fd %d", fileno(port->file));
    }
  } else {
    dfsch_write_unreadable(state, (dfsch_object_t*)port, 
                           "*closed*");
  }
}

static void file_port_seek(file_port_t* port, int64_t offset, int whence){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }

  if (fseek(port->file, offset, whence) != 0){
    dfsch_operating_system_error("fseek");    
  }
}

static int64_t file_port_tell(file_port_t* port){
  off_t ret;
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }

  ret = ftell(port->file);

  if (ret == -1){
    dfsch_operating_system_error("ftell");    
  }

  return ret;
}

#ifdef __unix__
static void file_port_batch_read_start(file_port_t* port){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  flockfile(port->file);
}
static void file_port_batch_read_end(file_port_t* port){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  funlockfile(port->file);
}
static int file_port_batch_read(file_port_t* port){
  int ch;

  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  ch = getc_unlocked(port->file);

  if (ch == EOF){
    if (feof(port->file)){
      return EOF;
    } else {
      dfsch_operating_system_error("getc_unlocked");    
    }
  }

  return ch;
}
#endif


static dfsch_slot_t file_port_slots[] = {
  DFSCH_STRING_SLOT(file_port_t, name, DFSCH_SLOT_ACCESS_RO,
                    "Filename associated to port"),
  DFSCH_BOOLEAN_SLOT(file_port_t, open, DFSCH_SLOT_ACCESS_RO,
                     "Is port open for accesses?"),
  DFSCH_BOOLEAN_SLOT(file_port_t, close, DFSCH_SLOT_ACCESS_RO,
                     "Will port be automatically closed by GC?"),
  DFSCH_SLOT_TERMINATOR,
};

dfsch_port_type_t dfsch_file_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_PORT_TYPE,
    sizeof(file_port_t),
    "file-port",
    NULL,
    (dfsch_type_write_t)file_port_write,
    NULL,
    NULL,
    
    file_port_slots,
    
    "port backed by stdio stream"
  },
  (dfsch_port_write_buf_t)file_port_write_buf,
  (dfsch_port_read_buf_t)file_port_read_buf,

  (dfsch_port_seek_t)file_port_seek,
  (dfsch_port_tell_t)file_port_tell,
#ifdef __unix__
  (dfsch_port_batch_read_start_t)file_port_batch_read_start,
  (dfsch_port_batch_read_end_t)file_port_batch_read_end,
  (dfsch_port_batch_read_t)file_port_batch_read,
#endif
};

static void file_port_finalizer(file_port_t* port, void* cd){
  if (port->open && port->close){
    fclose(port->file);
    port->open = 0;
  }
}

dfsch_object_t* dfsch_make_file_port(FILE* file, int close, char* name){
  file_port_t* port = (file_port_t*)dfsch_make_object((dfsch_type_t*)
                                                      DFSCH_FILE_PORT_TYPE);

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

void dfsch_set_standard_io_ports(){
  dfsch_set_current_input_port(dfsch_standard_input_port());
  dfsch_set_current_output_port(dfsch_standard_output_port());
  dfsch_set_current_error_port(dfsch_standard_error_port());
}

dfsch_object_t* dfsch_open_file_port(char* filename, char* mode){
  FILE* file;

  if (mode[0] != 'r' && mode[0] != 'w' && mode[0] != 'a'){ /// XXX
    dfsch_error("Invalid file port mode", 
                dfsch_make_string_cstr(mode));
  }
  if (mode[1] != 0){
    if (mode[1] != '+' && mode[1] != 'b'){
      dfsch_error("Invalid file port mode", 
                  dfsch_make_string_cstr(mode));
      
    }
    if (mode[2] != 0){
      if ((mode[2] != '+' && mode[2] != 'b') 
          || (mode[2] == mode[1])
          || (mode[3] != 0)){
        dfsch_error("Invalid file port mode", 
                    dfsch_make_string_cstr(mode));
      }
    }
  }

  file = fopen(filename, mode);
  
  if (!file){
      dfsch_operating_system_error("fopen");    
  }

  return dfsch_make_file_port(file, 1, filename);
}

void dfsch_close_file_port(dfsch_object_t* port){
  file_port_t* p = 
    (file_port_t*)DFSCH_ASSERT_INSTANCE(port, 
                                        (dfsch_type_t*)DFSCH_FILE_PORT_TYPE);

  p = (file_port_t*) port;

  if (p->close && p->open){
    fclose(p->file);
    p->open = 0;
  }  
}

typedef struct line_iterator_t {
  dfsch_type_t* type;
  dfsch_object_t* port;
  dfsch_strbuf_t* this;
} line_iterator_t;

static dfsch_object_t* li_this(line_iterator_t* li){
  return dfsch_make_byte_vector_nocopy(li->this->ptr, li->this->len);
}
static dfsch_object_t* li_next(line_iterator_t* li){
  li->this = dfsch_port_readline(li->port);
  if (!li->this){
    return NULL;
  } else {
    return li;
  }
}
dfsch_iterator_methods_t li_iterator = {
  .next = li_next,
  .this = li_this
};

dfsch_type_t dfsch_port_line_iterator_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "port-line-iterator",
  .size = sizeof(line_iterator_t),
  .collection = &dfsch_iterator_collection_methods,
  .iterator = &li_iterator
};

dfsch_object_t* dfsch_make_port_line_iterator(dfsch_object_t* port){
  line_iterator_t* li = dfsch_make_object(DFSCH_PORT_LINE_ITERATOR_TYPE);
  li->port = port;
  return li_next(li);
}


/*
 * Scheme interface
 */

DFSCH_DEFINE_PRIMITIVE(current_output_port, NULL){
  DFSCH_ARG_END(args);
  return dfsch_current_output_port();
}
DFSCH_DEFINE_PRIMITIVE(current_input_port, NULL){
  DFSCH_ARG_END(args);
  return dfsch_current_input_port();
}
DFSCH_DEFINE_PRIMITIVE(current_error_port, NULL){
  DFSCH_ARG_END(args);
  return dfsch_current_error_port();
}
DFSCH_DEFINE_PRIMITIVE(set_current_output_port, NULL){
  dfsch_port_t* port;
  DFSCH_PORT_ARG(args, port);  
  DFSCH_ARG_END(args);
  dfsch_set_current_output_port(port);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_current_input_port, NULL){
  dfsch_port_t* port;
  DFSCH_PORT_ARG(args, port);  
  DFSCH_ARG_END(args);
  dfsch_set_current_input_port(port);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_current_error_port, NULL){
  dfsch_port_t* port;
  DFSCH_PORT_ARG(args, port);  
  DFSCH_ARG_END(args);
  dfsch_set_current_error_port(port);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(null_port, NULL){
  DFSCH_ARG_END(args);
  return dfsch_null_port();
}
DFSCH_DEFINE_PRIMITIVE(write, NULL){
  dfsch_port_t* port;
  dfsch_object_t* object;
  dfsch_object_t* strict;
  char *buf;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_OBJECT_ARG_OPT(args, strict, NULL);  
  DFSCH_ARG_END(args);

  buf = dfsch_object_2_string(object, 1000, (strict != NULL) ? 
                              DFSCH_STRICT_WRITE : DFSCH_WRITE);
  dfsch_port_write_buf(port, buf, strlen(buf));
  
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(display, NULL){
  dfsch_port_t* port;
  dfsch_object_t* object;
  char *buf;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_ARG_END(args);

  buf = dfsch_object_2_string(object, 1000, DFSCH_PRINT);
  dfsch_port_write_buf(port, buf, strlen(buf));
  
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(newline, NULL){
  dfsch_port_t* port;
  char *buf;
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_ARG_END(args);

  dfsch_port_write_buf(port, "\n", 1);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(freshline, 
                       "Output newline if previous character wasn't newline"){
  dfsch_port_t* port;
  char *buf;
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_output_port());  
  DFSCH_ARG_END(args);

  dfsch_port_freshline(port);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(read, NULL){
  dfsch_port_t* port;
  char *buf;
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  return dfsch_parser_read_from_port(port);
}

DFSCH_DEFINE_PRIMITIVE(port_read_buf, NULL){
  dfsch_port_t* port;
  size_t len;
  char* buf;
  DFSCH_LONG_ARG(args, len);
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  buf = GC_MALLOC_ATOMIC(len+1);
  len = dfsch_port_read_buf(port, buf, len);
  
  if (len == 0){
    return NULL;
  }

  buf[len] = '\0';
  return dfsch_make_string_buf(buf, len);
}

DFSCH_DEFINE_PRIMITIVE(port_read_whole, 0){
  dfsch_port_t* port;
  DFSCH_PORT_ARG(args, port);
  DFSCH_ARG_END(args);
  return dfsch_make_string_strbuf(dfsch_port_read_whole(port));
}

DFSCH_DEFINE_PRIMITIVE(port_read_line, NULL){
  dfsch_port_t* port;
  dfsch_strbuf_t* buf;
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  buf = dfsch_port_readline(port);
  if (!buf){
    return NULL;
  }

  return dfsch_make_string_strbuf(buf);
}

DFSCH_DEFINE_PRIMITIVE(port_write_buf, "Write byte vector to port"
                       DFSCH_DOC_SYNOPSIS("(byte-vector port)")){
  dfsch_port_t* port;
  dfsch_strbuf_t* buf;
  DFSCH_BUFFER_ARG(args, buf);
  DFSCH_PORT_ARG_OPT(args, port, dfsch_current_input_port());  
  DFSCH_ARG_END(args);

  dfsch_port_write_buf(port, buf->ptr, buf->len);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(port_seek, NULL){
  dfsch_port_t* port;
  int64_t offset;
  int whence = SEEK_SET;
  
  DFSCH_PORT_ARG(args, port);
  DFSCH_INT64_ARG(args, offset);
  DFSCH_FLAG_PARSER_BEGIN_ONE_OPT(args, whence);
  DFSCH_FLAG_VALUE("set", SEEK_SET, whence);
  DFSCH_FLAG_VALUE("cur", SEEK_CUR, whence);
  DFSCH_FLAG_VALUE("end", SEEK_END, whence);
  DFSCH_FLAG_PARSER_END(args);

  dfsch_port_seek(port, offset, whence);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(port_tell, NULL){
  dfsch_port_t* port;
  char *buf;
  DFSCH_PORT_ARG(args, port);  
  DFSCH_ARG_END(args);
  
  return dfsch_make_number_from_long(dfsch_port_tell(port));
}

DFSCH_DEFINE_PRIMITIVE(string_output_port, NULL){
  DFSCH_ARG_END(args);
  return dfsch_string_output_port();
}
DFSCH_DEFINE_PRIMITIVE(string_output_port_value, NULL){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);

  return dfsch_make_string_strbuf(dfsch_string_output_port_value(port));
}
DFSCH_DEFINE_PRIMITIVE(string_input_port, NULL){
  dfsch_strbuf_t* string;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  return dfsch_string_input_port(string->ptr, string->len);
}
DFSCH_DEFINE_PRIMITIVE(eof_object_p, NULL){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);  
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_eof_object_p(object));
}

DFSCH_DEFINE_PRIMITIVE(open_file_port, NULL){
  char* fname;
  char* mode;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_STRING_ARG(args, mode);
  DFSCH_ARG_END(args);

  return dfsch_open_file_port(fname, mode);
}
DFSCH_DEFINE_PRIMITIVE(close_file_port, NULL){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);

  dfsch_close_file_port(port);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(make_port_line_iterator, NULL){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);  
  DFSCH_ARG_END(args);

  return dfsch_make_port_line_iterator(port);
}

void dfsch__port_native_register(dfsch_object_t *ctx){
  dfsch_defcanon_cstr(ctx, "<port>", DFSCH_PORT_TYPE);
  dfsch_defcanon_cstr(ctx, "<null-port>", DFSCH_NULL_PORT_TYPE);
  dfsch_defcanon_cstr(ctx, "<file-port>", DFSCH_FILE_PORT_TYPE);
  dfsch_defcanon_cstr(ctx, "<string-input-port>", DFSCH_STRING_INPUT_PORT_TYPE);
  dfsch_defcanon_cstr(ctx, "<string-output-port>", DFSCH_STRING_OUTPUT_PORT_TYPE);
  dfsch_defcanon_cstr(ctx, "<eof-object>", DFSCH_EOF_OBJECT_TYPE);
  dfsch_defcanon_cstr(ctx, "<port-line-iterator>", 
                      DFSCH_PORT_LINE_ITERATOR_TYPE);

  dfsch_defcanon_cstr(ctx, "current-output-port", 
                    DFSCH_PRIMITIVE_REF(current_output_port));
  dfsch_defcanon_cstr(ctx, "current-input-port", 
                    DFSCH_PRIMITIVE_REF(current_input_port));
  dfsch_defcanon_cstr(ctx, "current-error-port", 
                    DFSCH_PRIMITIVE_REF(current_error_port));
  dfsch_defcanon_cstr(ctx, "null-port", 
                    DFSCH_PRIMITIVE_REF(null_port));
  dfsch_defcanon_cstr(ctx, "write", 
                    DFSCH_PRIMITIVE_REF(write));
  dfsch_defcanon_cstr(ctx, "display", 
                    DFSCH_PRIMITIVE_REF(display));
  dfsch_defcanon_cstr(ctx, "newline", 
                    DFSCH_PRIMITIVE_REF(newline));
  dfsch_defcanon_cstr(ctx, "freshline", 
                    DFSCH_PRIMITIVE_REF(freshline));
  dfsch_defcanon_cstr(ctx, "read", 
                    DFSCH_PRIMITIVE_REF(read));

  dfsch_defcanon_cstr(ctx, "eof-object?", 
                    DFSCH_PRIMITIVE_REF(eof_object_p));

  dfsch_defcanon_cstr(ctx, "port-write-buf", 
                    DFSCH_PRIMITIVE_REF(port_write_buf));
  dfsch_defcanon_cstr(ctx, "port-read-buf", 
                    DFSCH_PRIMITIVE_REF(port_read_buf));
  dfsch_defcanon_cstr(ctx, "port-read-whole", 
                    DFSCH_PRIMITIVE_REF(port_read_whole));
  dfsch_defcanon_cstr(ctx, "port-read-line", 
                    DFSCH_PRIMITIVE_REF(port_read_line));
  dfsch_defcanon_cstr(ctx, "port-seek!", 
                    DFSCH_PRIMITIVE_REF(port_seek));
  dfsch_defcanon_cstr(ctx, "port-tell", 
                    DFSCH_PRIMITIVE_REF(port_tell));

  dfsch_defcanon_cstr(ctx, "string-output-port", 
                    DFSCH_PRIMITIVE_REF(string_output_port));
  dfsch_defcanon_cstr(ctx, "string-output-port-value", 
                    DFSCH_PRIMITIVE_REF(string_output_port_value));
  dfsch_defcanon_cstr(ctx, "string-input-port", 
                    DFSCH_PRIMITIVE_REF(string_input_port));

  dfsch_defcanon_cstr(ctx, "set-current-output-port!", 
                    DFSCH_PRIMITIVE_REF(set_current_output_port));
  dfsch_defcanon_cstr(ctx, "set-current-input-port!", 
                    DFSCH_PRIMITIVE_REF(set_current_input_port));
  dfsch_defcanon_cstr(ctx, "set-current-error-port!", 
                    DFSCH_PRIMITIVE_REF(set_current_error_port));

  dfsch_defcanon_cstr(ctx, "make-port-line-iterator", 
                      DFSCH_PRIMITIVE_REF(make_port_line_iterator));
  
  dfsch_defcanon_cstr(ctx, "*standard-input-port*",
                    dfsch_standard_input_port());
  dfsch_defcanon_cstr(ctx, "*standard-output-port*",
                    dfsch_standard_output_port());
  dfsch_defcanon_cstr(ctx, "*standard-error-port*",
                    dfsch_standard_error_port());
}

void dfsch__port_files_register(dfsch_object_t* ctx){
  dfsch_defcanon_cstr(ctx, "open-file-port", 
                    DFSCH_PRIMITIVE_REF(open_file_port));
  dfsch_defcanon_cstr(ctx, "close-file-port!", 
                    DFSCH_PRIMITIVE_REF(close_file_port));
}
