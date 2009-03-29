/*
 * dfsch_import - Library for loading scheme and C code into dfsch interpreter
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

#include "dfsch/load.h"
#include "src/util.h"

#include <dfsch/parse.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <dlfcn.h>
#include <dfsch/number.h>
#include <dfsch/strings.h>
#include <dfsch/introspect.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

//#define DFSCH_DEFAULT_LIBDIR "."

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
    dfsch_error("load:dlopen-failed",dfsch_make_string_cstr(err));
    
  entry = dlsym(handle, sym_name);

  err = dlerror();
  if (err)
    dfsch_error("load:dlopen-failed",dfsch_make_string_cstr(err));
  
  entry(ctx); // TODO: what if this routine fails?

  return DFSCH_SYM_TRUE;
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

dfsch_object_t* dfsch_load_scm(dfsch_object_t* ctx, 
			       char* fname){
  return dfsch_eval_proc(dfsch_read_scm(fname, ctx), ctx);
}

static int qs_strcmp(const void* a, const void* b){ /* To suppress warning */
  return strcmp(*((char**)a), *((char**)b));
}

static char** my_scandir(char* dirname){
  char **buf;
  size_t allocd;
  size_t count;
  DIR *dir;
  struct dirent* e;

  buf = GC_MALLOC(sizeof(char*)*16);
  allocd = 16;

  dir = opendir(dirname);
  if (!dir){
    return NULL;
  }

  count = 0;

  while((e = readdir(dir))){
    if (count >= (allocd - 1)){
      allocd *= 2;
      buf = GC_REALLOC(buf, sizeof(char*) * allocd);
    }

    buf[count] = stracpy(e->d_name);
    count++;
  }

  qsort(buf, count, sizeof(char*), qs_strcmp);

  buf[count] = NULL;
  return buf;
}

static char* get_module_symbol(char* name){
  str_list_t* l = sl_create();
  char* buf = stracpy(name);
  char* i = buf;

  while(*i){
    if (!strchr("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", *i)){
      *i = '_';
    }
    i++;
  }
  
  sl_append(l, "dfsch_module_");
  sl_append(l, buf);
  sl_append(l, "_register");

  return sl_value(l);
}

typedef struct builtin_module_t {
  char* name;
  void (*register_proc)(dfsch_object_t* env);
} builtin_module_t;

static builtin_module_t builtin_modules[] = {
  {"introspect", dfsch_introspect_register},
};

dfsch_object_t* dfsch_load(dfsch_object_t* env, char* name, 
                           dfsch_object_t* path_list){
  struct stat st;
  dfsch_object_t* path;
  char *pathpart;
  char *fname;
  str_list_t* l;
  int i;

  for (i = 0; i < sizeof(builtin_modules) / sizeof(builtin_module_t); i++){
    if (strcmp(builtin_modules[i].name, name) == 0){
      builtin_modules[i].register_proc(env);
      return NULL;
    }
  }

  if (path_list){
    path = path_list;
  } else {
    path = dfsch_env_get_cstr(env, "load:*path*");
    if (path)
      path = dfsch_car(path);
  }

  while (dfsch_pair_p(path)){
    l = sl_create();
    sl_append(l, dfsch_string_to_cstr(dfsch_car(path)));
    sl_append(l, "/");
    sl_append(l, name);
    pathpart = sl_value(l);
    if (stat(pathpart, &st) == 0){ 
      if (S_ISREG(st.st_mode) || S_ISLNK(st.st_mode)){
	if (strcmp(".so", pathpart+strlen(pathpart)-3) == 0){
	  return dfsch_load_so(env, pathpart, get_module_symbol(name));	      
	} else {
	  return dfsch_load_scm(env, pathpart);
	}
      }
      if (S_ISDIR(st.st_mode)){
	char** list = my_scandir(pathpart);
	
	while(*list){
	  l = sl_create();
	  sl_append(l, pathpart);
	  sl_append(l, "/");
	  sl_append(l, *list);
	  
	  if (strcmp(".so", (*list)+strlen(*list)-3) == 0){
	    dfsch_load_so(env, fname, get_module_symbol(name));	      
	  } else {
	    dfsch_load_scm(env, fname);
	  }
	  
	  list++;
	}
	return NULL;
      }
    }
    fname = stracat(pathpart, ".scm");
    if (stat(fname, &st) == 0 && (S_ISREG(st.st_mode) || S_ISLNK(st.st_mode))){
      return dfsch_load_scm(env, fname);	      
    }
    fname = stracat(pathpart, ".so");
    if (stat(fname, &st) == 0 && (S_ISREG(st.st_mode) || S_ISLNK(st.st_mode))){
      return dfsch_load_so(env, fname, get_module_symbol(name));	      
    }
    
    path = dfsch_cdr(path);
  }
  
  dfsch_error("load:module-not-found", dfsch_make_string_cstr(name));
}

static int search_modules(dfsch_object_t* modules, char* name){
  while(modules){
    if (strcmp(name, dfsch_string_to_cstr(dfsch_car(modules))) == 0){
      return 1;
    }
    modules = dfsch_cdr(modules);
  }
  return 0;
}

dfsch_object_t* dfsch_require(dfsch_object_t* env, char* name, dfsch_object_t* path_list){
  dfsch_object_t* modules = dfsch_env_get_cstr(env, "load:*modules*");
  if (modules){
    modules = dfsch_car(modules);
  }

  if (search_modules(modules, name)){
    return NULL;
  }

  return dfsch_load(env, name, path_list);
}

void dfsch_provide(dfsch_object_t* env, char* name){
  dfsch_object_t* modules = dfsch_env_get_cstr(env, "load:*modules*");
  if (modules){
    modules = dfsch_car(modules);
  }

  if (search_modules(modules, name)){
    dfsch_error("provide:module-already-provided", dfsch_make_string_cstr(name));
  }


  /*
   * there should be define - module list is related to environment, but
   * this distinction is in most cases totally irrelevant, because modules
   * are mostly loaded into toplevel environment.
   */
  dfsch_define_cstr(env, "load:*modules*", 
                    dfsch_cons(dfsch_make_string_cstr(name),
                               modules));
}

dfsch_object_t* dfsch_load_extend_path(dfsch_object_t* ctx, char* dir){
  dfsch_object_t* path = dfsch_env_get_cstr(ctx, "load:*path*");
  if (path){
    dfsch_set_cstr(ctx, "load:*path*", 
		   dfsch_append(dfsch_list(2,
					   dfsch_car(path),
					   dfsch_list(1,
						      dfsch_make_string_cstr(dir)))));
  }else{
    dfsch_define_cstr(ctx, "load:*path*", 
                     dfsch_list(1, dfsch_make_string_cstr(dir)));
  }
}

dfsch_object_t* dfsch_read_scm(char* scm_name, dfsch_object_t* eval_env){
  FILE* f = fopen(scm_name,"r");
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;
  dfsch_object_t *obj;

  if (!f){
    int err = errno;
    dfsch_error("load:unix-error",dfsch_make_string_cstr(strerror(err)));
  }

  obj = dfsch_read_scm_stream(f, scm_name, eval_env);

  fclose(f);
    
  return obj;
}

dfsch_object_t* dfsch_read_scm_fd(int f, char* name, dfsch_object_t* eval_env){
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;

  ictx.head = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, load_callback, &ictx);
  dfsch_parser_eval_env(parser, eval_env);

  while (!err && (r = read(f, buf, 8192))>0){
    buf[r]=0;
    err = dfsch_parser_feed(parser,buf);
  }

  if (r<0){
    int err = errno;
    dfsch_error("load:unix-error",dfsch_make_string_cstr(strerror(err)));
  }
 
  if ((err && err != DFSCH_PARSER_STOPPED) 
      || dfsch_parser_get_level(parser)!=0){
    if (name)
      dfsch_error("load:syntax-error",dfsch_make_string_cstr(name));
    else
      dfsch_error("load:syntax-error",NULL);

  }

  return ictx.head;
  
}
dfsch_object_t* dfsch_read_scm_stream(FILE* f, 
                                      char* name, 
                                      dfsch_object_t* eval_env){
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;
  int err=0;
  int l=0;

  ictx.head = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, load_callback, &ictx);
  dfsch_parser_set_source(parser, dfsch_make_string_cstr(name));
  dfsch_parser_eval_env(parser, eval_env);

  while (!err && (fgets(buf, 8192, f))){
    if (buf[strlen(buf)-1] == '\n') 
      // I'm not interested in '\r' or any other weird ideas
      l++;

    err = dfsch_parser_feed(parser,buf);
  }

  if ((err && err != DFSCH_PARSER_STOPPED) 
      || dfsch_parser_get_level(parser)!=0){
    if (name)
      dfsch_error("load:syntax-error",
                  dfsch_cons(dfsch_make_string_cstr(name),
                             dfsch_make_number_from_long(l)));
    else
      dfsch_error("load:syntax-error",
                  dfsch_cons(NULL,
                             dfsch_make_number_from_long(l)));
  }

  return ictx.head;
}

DFSCH_DEFINE_FORM_IMPL(load_scm, NULL){
  char* file_name;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_ARG(args, file_name);
  DFSCH_ARG_END(args);

  return dfsch_load_scm(env, file_name);
}

DFSCH_DEFINE_FORM_IMPL(load_so, NULL){
  char* sym_name;
  char* so_name;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_ARG(args, so_name);
  DFSCH_STRING_ARG(args, sym_name);
  DFSCH_ARG_END(args);

  return dfsch_load_so(env, so_name, sym_name);
}

DFSCH_DEFINE_FORM_IMPL(load, NULL){
  char* name;
  dfsch_object_t* path_list;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, path_list, NULL)
  DFSCH_ARG_END(args);

  return dfsch_load(env, name, path_list);  
}
DFSCH_DEFINE_FORM_IMPL(require, NULL){
  char* name;
  dfsch_object_t* path_list;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, path_list, NULL)
  DFSCH_ARG_END(args);

  return dfsch_require(env, name, path_list);  
}
DFSCH_DEFINE_FORM_IMPL(provide, NULL){
  char* name;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  dfsch_provide(env, name);  
  return NULL;
}

static dfsch_object_t* native_read_scm(void *baton, dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  char* filename;
  DFSCH_STRING_ARG(args, filename);

  return dfsch_read_scm(filename, NULL);
}


dfsch_object_t* dfsch_load_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "load:*path*", 
		    dfsch_list(3, 
			       dfsch_make_string_cstr("."),
                               dfsch_make_string_cstr(DFSCH_LIB_SCM_DIR),
                               dfsch_make_string_cstr(DFSCH_LIB_SO_DIR)));
  dfsch_define_cstr(ctx, "load:*modules*", NULL);
  dfsch_define_cstr(ctx, "load:scm!",  DFSCH_FORM_REF(load_scm));
  dfsch_define_cstr(ctx, "load:read-scm",
		    dfsch_make_primitive(native_read_scm,ctx));
  dfsch_define_cstr(ctx, "load:so!", DFSCH_FORM_REF(load_so));
  dfsch_define_cstr(ctx, "load!", DFSCH_FORM_REF(load));
  dfsch_define_cstr(ctx, "require", DFSCH_FORM_REF(require));
  dfsch_define_cstr(ctx, "provide", DFSCH_FORM_REF(provide));
  return NULL;
}
