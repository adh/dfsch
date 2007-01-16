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

#include "dfsch/lib/load.h"
#include "src/util.h"

#include <dfsch/parse.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <dlfcn.h>
#include <dfsch/number.h>
#include <dfsch/strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


dfsch_object_t* dfsch_load_so(dfsch_object_t* ctx, 
                    char* so_name, 
                    char* sym_name){
  void *handle;
  dfsch_object_t* (*entry)(dfsch_object_t*);
  char* err;

  err = dlerror();

  handle = dlopen(so_name, RTLD_NOW);

  err = dlerror();
  if (err)
    dfsch_throw("load:dlopen-failed",dfsch_make_string_cstr(err));
    
  entry = dlsym(handle, sym_name);

  err = dlerror();
  if (err)
    dfsch_throw("load:dlopen-failed",dfsch_make_string_cstr(err));
  
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

dfsch_object_t* dfsch_load_scm(dfsch_object_t* ctx, char* scm_name){
  struct stat st;
  dfsch_object_t* path;

  if (stat(scm_name, &st) == 0 && S_ISREG(st.st_mode))
    return dfsch_eval_proc(dfsch_read_scm(scm_name), ctx);

  path = dfsch_env_get_cstr(ctx, "load:path");

  if (path)
    path = dfsch_car(path);

  while (dfsch_pair_p(path)){
    char *fname;
    str_list_t* l = sl_create();
    sl_append(l, dfsch_string_to_cstr(dfsch_car(path)));
    sl_append(l, "/");
    sl_append(l, scm_name);
    fname = sl_value(l);
    if (stat(fname, &st) == 0 && S_ISREG(st.st_mode))
      return dfsch_eval_proc(dfsch_read_scm(fname), ctx);

    path = dfsch_cdr(path);
  }
  
  dfsch_throw("load:file-not-found", dfsch_make_string_cstr(scm_name));

}
dfsch_object_t* dfsch_load_extend_path(dfsch_object_t* ctx, char* dir){
  dfsch_object_t* path = dfsch_env_get_cstr(ctx, "load:path");
  if (path){
    dfsch_define_cstr(ctx, "load:path", 
                     dfsch_append(dfsch_list(2,
                                             dfsch_car(path),
                                             dfsch_list(1,
                                                        dfsch_make_string_cstr(dir)))));
  }else{
    dfsch_define_cstr(ctx, "load:path", 
                     dfsch_list(1, dfsch_make_string_cstr(dir)));
  }
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
    dfsch_throw("load:unix-error",dfsch_make_string_cstr(strerror(err)));
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
    dfsch_throw("load:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
 
  if ((err && err != DFSCH_PARSER_STOPPED) 
      || dfsch_parser_get_level(parser)!=0){
    if (name)
      dfsch_throw("load:syntax-error",dfsch_make_string_cstr(name));
    else
      dfsch_throw("load:syntax-error",NULL);

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
    if (buf[strlen(buf)-1] == '\n') 
      // I'm not interested in '\r' or any other weird ideas
      l++;

    err = dfsch_parser_feed(parser,buf);
  }
  fclose(f);

  if ((err && err != DFSCH_PARSER_STOPPED) 
      || dfsch_parser_get_level(parser)!=0){
    if (name)
      dfsch_throw("load:syntax-error",
                  dfsch_cons(dfsch_make_string_cstr(name),
                             dfsch_make_number_from_long(l)));
    else
      dfsch_throw("load:syntax-error",
                  dfsch_cons(NULL,
                             dfsch_make_number_from_long(l)));
  }

  return ictx.head;
  
}


static dfsch_object_t* native_load_scm(void *baton, dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* arg;
  if (dfsch_list_length(args)!=1)
    dfsch_throw("wrong-number-of-arguments",args);

  arg = dfsch_car(args);
  if (!dfsch_string_p(arg))
    dfsch_throw("not-a-string",arg);

  return dfsch_load_scm(baton, dfsch_string_to_cstr(arg));
}

static dfsch_object_t* native_load_so(void *baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_object_t *so, *sym;
  if (dfsch_list_length(args)!=2)
    dfsch_throw("wrong-number-of-arguments",args);

  so = dfsch_car(args);
  if (!dfsch_string_p(so))
    dfsch_throw("not-a-string",so);
  sym = dfsch_car(dfsch_cdr(args));
  if (!dfsch_string_p(sym))
    dfsch_throw("not-a-string",sym);

  return dfsch_load_so(baton, dfsch_string_to_cstr(so), 
                       dfsch_string_to_cstr(sym));
}


static dfsch_object_t* native_read_scm(void *baton, dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  if (dfsch_list_length(args)!=1)
    dfsch_throw("wrong-number-of-arguments",args);

  dfsch_object_t*arg = dfsch_car(args);
  if (!dfsch_string_p(arg))
    dfsch_throw("not-a-string",arg);

  return dfsch_read_scm(dfsch_string_to_cstr(arg));
}


dfsch_object_t* dfsch_load_so_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx,"load:so!",dfsch_make_primitive(native_load_so,ctx));
  return NULL;
}
dfsch_object_t* dfsch_load_scm_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx,"load:scm!",dfsch_make_primitive(native_load_scm,ctx));
  dfsch_define_cstr(ctx,"load:read-scm",dfsch_make_primitive(native_read_scm,ctx));
  return NULL;
}
dfsch_object_t* dfsch_load_register(dfsch_object_t *ctx){
  dfsch_load_scm_register(ctx);
  dfsch_load_so_register(ctx);
  return NULL;
}
