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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/stream.h>

#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <dlfcn.h>
#include <dfsch/load.h>

dfsch_object_t* dfsch_load_so(dfsch_ctx_t* ctx, 
                    char* so_name, 
                    char* sym_name){
  void *handle;
  dfsch_object_t* (*entry)(dfsch_ctx_t*);
  char* err;

  err = dlerror();

  handle = dlopen(so_name, RTLD_NOW);

  err = dlerror();
  if (err)
    DFSCH_THROW("load:dlopen-failed",dfsch_make_string(err));
    
  entry = dlsym(handle, sym_name);

  err = dlerror();
  if (err)
    DFSCH_THROW("load:dlopen-failed",dfsch_make_string(err));
  
  return entry(ctx); // TODO: what if this routine fails?

}

typedef struct import_ctx_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;
} import_ctx_t;

static int load_callback(dfsch_object_t *obj, void* ctx){

  dfsch_object_t* new_tail = dfsch_cons(obj, NULL);

  if (!((import_ctx_t*)ctx)->head){
    ((import_ctx_t*)ctx)->head = new_tail;
  }else{
    dfsch_set_cdr(((import_ctx_t*)ctx)->tail, new_tail);
  }

  ((import_ctx_t*)ctx)->tail = new_tail;

  return 1;
}

dfsch_object_t* dfsch_load_scm(dfsch_ctx_t* ctx, char* scm_name){

  /*
   * Read whole file into big list and then evaluate it - yeah there might
   * be better ways to do this
   */
  
  return dfsch_ctx_eval_list(ctx, dfsch_read_scm(scm_name));
}
dfsch_object_t* dfsch_read_scm(char* scm_name){
  FILE* f = fopen(scm_name,"r");
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;
  dfsch_object_t *obj;

  if (!f){
    int err = errno;
    DFSCH_THROW("load:unix-error",dfsch_make_string(strerror(err)));
  }

  obj = dfsch_read_scm_stream(f,scm_name);

  fclose(f);
    
  return obj;
}
dfsch_object_t* dfsch_read_scm_fd(int f, char* name){
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;

  ictx.head = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, load_callback, &ictx);

  while (!err && (r = read(f, buf, 8192))>0){
    buf[r]=0;
    err = dfsch_parser_feed(parser,buf);
  }
  close(f);

  if (r<0){
    int err = errno;
    DFSCH_THROW("load:unix-error",dfsch_make_string(strerror(err)));
  }
 
  if ((err && err != DFSCH_PARSER_STOPPED) 
      || dfsch_parser_get_level(parser)!=0){
    if (name)
      DFSCH_THROW("load:syntax-error",dfsch_make_string(name));
    else
      DFSCH_THROW("load:syntax-error",NULL);

  }

  return ictx.head;
  
}
dfsch_object_t* dfsch_read_scm_stream(FILE* f, char* name){
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;
  int l=0;

  ictx.head = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, load_callback, &ictx);

  while (!err && (fgets(buf, 8192, f))){
    if (buf[strlen(buf)] == '\n') 
      // I'm not interested in '\r' or any other weird ideas
      l++;

    err = dfsch_parser_feed(parser,buf);
  }
  close(f);

  if ((err && err != DFSCH_PARSER_STOPPED) 
      || dfsch_parser_get_level(parser)!=0){
    if (name)
      DFSCH_THROW("load:syntax-error",dfsch_cons(dfsch_make_string(name),
                                                 dfsch_make_number(l)));
    else
      DFSCH_THROW("load:syntax-error",dfsch_cons(NULL,
                                                 dfsch_make_number(l)));
  }

  return ictx.head;
  
}


static dfsch_object_t* native_load_scm(void *baton, dfsch_object_t* args){
  dfsch_object_t* arg;
  if (dfsch_list_length(args)!=1)
    DFSCH_THROW("wrong-number-of-arguments",args);

  arg = dfsch_car(args);
  if (!dfsch_object_string_p(arg))
    DFSCH_THROW("not-a-string",arg);

  return dfsch_load_scm(baton, dfsch_string(arg));
}

static dfsch_object_t* native_load_so(void *baton, dfsch_object_t* args){
  dfsch_object_t *so, *sym;
  if (dfsch_list_length(args)!=2)
    DFSCH_THROW("wrong-number-of-arguments",args);

  so = dfsch_car(args);
  if (!dfsch_object_string_p(so))
    DFSCH_THROW("not-a-string",so);
  sym = dfsch_car(dfsch_cdr(args));
  if (!dfsch_object_string_p(sym))
    DFSCH_THROW("not-a-string",sym);

  return dfsch_load_so(baton, dfsch_string(so), dfsch_string(sym));
}


static dfsch_object_t* native_read_scm(void *baton, dfsch_object_t* args){
  if (dfsch_list_length(args)!=1)
    DFSCH_THROW("wrong-number-of-arguments",args);

  dfsch_object_t*arg = dfsch_car(args);
  if (!dfsch_object_string_p(arg))
    DFSCH_THROW("not-a-string",arg);

  return dfsch_read_scm(dfsch_string(arg));
}


dfsch_object_t* dfsch_load_so_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx,"load:so!",dfsch_make_primitive(native_load_so,ctx));
  return NULL;
}
dfsch_object_t* dfsch_load_scm_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx,"load:scm!",dfsch_make_primitive(native_load_scm,ctx));
  dfsch_ctx_define(ctx,"load:read-scm!",dfsch_make_primitive(native_read_scm,ctx));
  return NULL;
}
dfsch_object_t* dfsch_load_register(dfsch_ctx_t *ctx){
  dfsch_load_scm_register(ctx);
  dfsch_load_so_register(ctx);
  return NULL;
}
