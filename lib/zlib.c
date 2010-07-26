#include <dfsch/lib/zlib.h>
#include <zlib.h>


typedef struct gzip_port_t {
  dfsch_type_t* type;
  gzFile file;
  int open;
  char* fname;
} gzip_port_t;

static void gzip_port_write(gzip_port_t* port, 
                            dfsch_writer_state_t* state){
  if (port->open){
    dfsch_write_unreadable(state, port, 
                           "%s", 
                           port->fname);
  } else {
    dfsch_write_unreadable(state, port, "*closed*");
  }
}

dfsch_type_t dfsch_gzip_port_type = {
  DFSCH_ABSTRACT_TYPE,
  DFSCH_PORT_TYPE,
  sizeof(gzip_port_t),
  "zlib:gzip-port",
  NULL,
  (dfsch_type_write_t)gzip_port_write,
  NULL
};

static void port_write_buf(gzip_port_t* port, 
                           char*buf, size_t len){
  size_t ret;
  
  if (!port->open){
    dfsch_error("Port is already closed", port);
  }
  
  if (len != 0){
    ret = gzwrite(port->file, buf, len);
    if (ret == 0){
      dfsch_operating_system_error("Error writing to gzip port");
    }
  }
}

dfsch_port_type_t dfsch_gzip_output_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_GZIP_PORT_TYPE,
    sizeof(gzip_port_t),
    "zlib:gzip-output-port",
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

static void port_read_buf(gzip_port_t* port, 
                          char*buf, size_t len){
  size_t ret;
  
  if (!port->open){
    dfsch_error("Port is already closed", port);
  }
  
  if (len != 0){
    ret = gzread(port->file, buf, len);
    if (ret == -1){
      dfsch_operating_system_error("Error writing to gzip port");
    }
    return ret;
  }
  return 0;
}

dfsch_port_type_t dfsch_gzip_input_port_type = {
  {
    DFSCH_PORT_TYPE_TYPE,
    DFSCH_GZIP_PORT_TYPE,
    sizeof(gzip_port_t),
    "zlib:gzip-input-port",
    NULL,
    NULL,
    NULL
  },
  NULL,
  (dfsch_port_write_buf_t)port_read_buf,
  
  NULL,
  NULL,

  NULL, // TODO
  NULL,
  NULL
};

static void port_finalizer(gzip_port_t* port, void* cd){
  if (port->open){
    gzclose(port->file);
    port->open = 0;
  }
}

static dfsch_object_t* gzip_open(char mode,
                                 char* filename){
  gzip_port_t* p = dfsch_make_object(mode == 'r' 
                                     ? DFSCH_GZIP_INPUT_PORT_TYPE
                                     : DFSCH_GZIP_OUTPUT_PORT_TYPE);

  p->fname = filename;

  switch (mode){
  case 'r':
    p->file = gzopen(filename, "r");
    break;
  case 'w':
    p->file = gzopen(filename, "w");
    break;
  case 'a':
    p->file = gzopen(filename, "a");
    break;    
  }

  if (!p->file){
    dfsch_operating_system_error("Cannot open gzip port");
  }

  GC_REGISTER_FINALIZER(p, (GC_finalization_proc)port_finalizer,
                        NULL, NULL, NULL);

  p->open = 1;

  return (dfsch_object_t*)p;
}

dfsch_object_t* dfsch_gzip_open_for_input(char* filename){
  return gzip_open('r', filename);
}
dfsch_object_t* dfsch_gzip_open_for_output(char* filename){ 
  return gzip_open('w', filename);
}
dfsch_object_t* dfsch_gzip_open_for_append(char* filename){
  return gzip_open('a', filename);
}


void dfsch_gzip_close_port(dfsch_object_t* port){
  gzip_port_t* p = DFSCH_ASSERT_INSTANCE(port, DFSCH_GZIP_PORT_TYPE);
  int r;
  if (p->open){
    p->open = 0;
    r = gzclose(p->file);
    if (r != Z_OK){
      dfsch_error("Error closing gzip port", DFSCH_MAKE_FIXNUM(r));
    }
  }
  return NULL;
}

