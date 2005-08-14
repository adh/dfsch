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

#include <dfsch/stream.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>

dfsch_object_t* dfsch_load_so(dfsch_ctx_t* ctx, 
                    char* so_name, 
                    char* sym_name){
  DFSCH_THROW("load:unimplemented",NULL);
}

typedef struct import_ctx_t {
  dfsch_object_t* ret;
  dfsch_ctx_t* ctx;
} import_ctx_t;

static int load_callback(dfsch_object_t *obj, void* baton){
  ((import_ctx_t*)baton)->ret = dfsch_ctx_eval(((import_ctx_t*)baton)->ctx, 
                                               obj);
  return 1;
}

dfsch_object_t* dfsch_load_scm(dfsch_ctx_t* ctx, char* scm_name){
  int f = open(scm_name,O_RDONLY);
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;
  dfsch_object_t *obj;

  if (f<0){
    int err = errno;
    DFSCH_THROW("load:unix-error",dfsch_make_string(strerror(err)));
  }

  obj = dfsch_load_scm_fd(ctx,f);

  close(f);
    
  return obj;
}
dfsch_object_t* dfsch_load_scm_fd(dfsch_ctx_t* ctx, int f){
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;

  ictx.ctx = ctx;
  ictx.ret = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, load_callback, &ictx);

  while (!err & (r = read(f, buf, 8192))>0){
    buf[r]=0;
    err = dfsch_parser_feed(parser,buf);
  }
  close(f);

  if (err && err != DFSCH_PARSER_STOPPED){
    DFSCH_THROW("load:syntax-error",NULL);
  }

  return ictx.ret;
  
}


static dfsch_object_t* native_load_scm(void *baton, dfsch_object_t* args){
  if (dfsch_list_length(args)!=1)
    DFSCH_THROW("wrong-number-of-arguments",args);

  dfsch_object_t*arg = dfsch_car(args);
  if (!dfsch_object_string_p(arg))
    DFSCH_THROW("not-a-string",arg);

  return dfsch_load_scm(baton, dfsch_string(arg));
}


void dfsch_load_so_register(dfsch_ctx_t *ctx){
  
}
void dfsch_load_scm_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx,"load:scm!",dfsch_make_primitive(native_load_scm,ctx));
}
void dfsch_load_register(dfsch_ctx_t *ctx){
  dfsch_load_scm_register(ctx);
}
