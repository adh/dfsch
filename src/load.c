/*
 * dfsch_load - Library for loading scheme and C code into dfsch interpreter
 * Copyright (C) 2005-2008 Ales Hakl
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

#include "dfsch/load.h"
#include "src/util.h"
#include "zlib.h"

#include <dfsch/parse.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>

#ifdef __unix__
#include <dlfcn.h>
#endif

#ifdef __WIN32__
#include <windows.h>
#endif

#include <dfsch/number.h>
#include <dfsch/strings.h>
#include <dfsch/introspect.h>
#include <dfsch/magic.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>


//#define DFSCH_DEFAULT_LIBDIR "."

#ifdef __WIN32__
#define S_ISLNK(x) 0
#endif


void dfsch_load_so(dfsch_object_t* ctx, 
                   char* so_name, 
                   char* sym_name){
#if defined(__unix__)
  void *handle;
  dfsch_object_t* (*entry)(dfsch_object_t*);
  char* err;

  err = dlerror();

  handle = dlopen(so_name, RTLD_NOW);

  err = dlerror();
  if (err){
    dfsch_error("dlopen() failed", dfsch_make_string_cstr(err));
  }

  entry = dlsym(handle, sym_name);

  err = dlerror();
  if (err){
    dfsch_error("dlsym() failed", dfsch_make_string_cstr(err));
  }
  
  entry(ctx);
#elif defined(__WIN32__)
  HMODULE hModule;
  dfsch_object_t* (*entry)(dfsch_object_t*);

  hModule = LoadLibraryEx(so_name, NULL, 0);

  if (!hModule){
    /* XXX: This is ugly hack that can be probably solved slightly
     * better by SetDllDirectory(). But SetDllDirectory is not
     * supported before XP SP1 and also is not present in mingw's
     * import library for kernel32.dll. 
     */
    hModule = LoadLibraryEx(so_name, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
    
    if (!hModule){
      dfsch_error("LoadLibraryEx() failed", NULL);
    }
  }

  entry = GetProcAddress(hModule, sym_name);

  if (!entry){
    dfsch_error("GetProcAddress() failed", NULL);    
  }

  entry(ctx);
#else
  dfsch_error("Get real operating system!", NULL);
#endif
}

static pthread_key_t load_thread_key;
static pthread_once_t load_thread_once = PTHREAD_ONCE_INIT;

static void load_thread_info_destroy(void* ptr){
  if (ptr){
    GC_FREE(ptr);
  }
}
static void load_thread_key_alloc(){
  pthread_key_create(&load_thread_key, load_thread_info_destroy);
}

typedef struct load_operation_t load_operation_t;

struct load_operation_t {
  load_operation_t* next;
  char* fname;
  int toplevel;
};

typedef struct load_thread_info_t {
  load_operation_t* operation;
} load_thread_info_t;

static load_thread_info_t* get_load_ti(){
  load_thread_info_t *lti;
  pthread_once(&load_thread_once, load_thread_key_alloc);
  lti = pthread_getspecific(load_thread_key);
  if (DFSCH_UNLIKELY(!lti)){
    lti = GC_MALLOC_UNCOLLECTABLE(sizeof(load_thread_info_t)); 
    lti->operation = NULL;
    pthread_setspecific(load_thread_key, lti);
  }
  return lti;
  
}


char* read_file(char* fname){
  FILE* f;
  char* buf = GC_MALLOC_ATOMIC(8192);
  size_t r;
  str_list_t* sl = sl_create();

  f = fopen(fname, "r");
  if (!f){
    dfsch_operating_system_error(dfsch_saprintf("Cannot open file %d",
                                                fname));
  }

  while (!feof(f)){
    r = fread(buf, 1, 8192, f);
    if (r != 0){
      sl_nappend(sl, buf, r);
      buf = GC_MALLOC_ATOMIC(8192);
    } else {
      if (ferror(f)){
        dfsch_operating_system_error(dfsch_saprintf("Error reading file %d",
                                                    fname));
      }
    }
  }
  
  return sl_value(sl);
}

void dfsch_load_scm(dfsch_object_t* env, char* fname, int toplevel){
  dfsch_load_source(env, fname, toplevel, read_file(fname));
}

static char* read_dsz(FILE* f){
  size_t len;
  size_t clen;
  uint32_t cksum;
  unsigned char header_buf[20];
  unsigned char trailer_buf[12];
  unsigned char trailer_read[12];
  char* cbuf;
  char* payload;

  if (fread(header_buf, 20, 1, f) != 1){
    fclose(f);
    dfsch_operating_system_error("fread");
  }

  if (memcmp(header_buf, "DsZ0\r\n\xff\n\0\r\x80\x7f", 12) != 0){
    fclose(f);
    dfsch_error("Invalid DSZ header", NULL);
  }
  
  len = (((size_t)header_buf[12]) << 24)
    | (((size_t)header_buf[13]) << 16)
    | (((size_t)header_buf[14]) << 8)
    | (((size_t)header_buf[15]) << 0);
  clen = (((size_t)header_buf[16]) << 24)
    | (((size_t)header_buf[17]) << 16)
    | (((size_t)header_buf[18]) << 8)
    | (((size_t)header_buf[19]) << 0);

  cbuf = GC_MALLOC_ATOMIC(clen);
  payload = GC_MALLOC_ATOMIC(len);

  if (fread(cbuf, clen, 1, f) != 1){ 
    fclose(f);
    dfsch_operating_system_error("fread");
  }

  if (fread(trailer_read, 12, 1, f) != 1){
    fclose(f);
    dfsch_operating_system_error("fread");
  }
  fclose(f);


  if (uncompress(payload, &len, cbuf, clen) != Z_OK){
    dfsch_error("Invalid DSZ payload", NULL);
  }

  memcpy(trailer_buf, "DsZ!", 4);

  cksum = crc32(crc32(0, NULL, 0), payload, len);
  trailer_buf[4] = cksum >> 24;
  trailer_buf[5] = cksum >> 16;
  trailer_buf[6] = cksum >> 8;
  trailer_buf[7] = cksum >> 0;

  cksum = crc32(crc32(0, NULL, 0), cbuf, clen);
  trailer_buf[8] = cksum >> 24;
  trailer_buf[9] = cksum >> 16;
  trailer_buf[10] = cksum >> 8;
  trailer_buf[11] = cksum >> 0;

  if (memcmp(trailer_buf, trailer_read, 12) != 0){
    dfsch_error("Invalid DSZ trailer", NULL);
  }
  
  return payload;
}

void dfsch_load_dsz(dfsch_object_t* env, char* fname, int toplevel){
  FILE* f;
  int err=0;
  int l=0;
  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  load_thread_info_t* lti = get_load_ti();
  load_operation_t this_op;
  dfsch_package_t* saved_package = dfsch_get_current_package();
  char* source;

  f = fopen(fname, "rb");
  if (!f){
    dfsch_operating_system_error("fopen");
  }

  dfsch_load_source(env, fname, toplevel, read_dsz(f));
}

static int load_source_callback(dfsch_object_t* object,
                                dfsch_object_t* env){
  dfsch_eval(object, env);
  return 1;
}

void dfsch_load_source(dfsch_object_t* env,
                       char* fname,
                       int toplevel,
                       char* source){
  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  load_thread_info_t* lti = get_load_ti();
  load_operation_t this_op;
  dfsch_package_t* saved_package = dfsch_get_current_package();

  dfsch_parser_callback(parser, load_source_callback, env);
  dfsch_parser_set_source(parser, dfsch_make_string_cstr(fname));
  dfsch_parser_eval_env(parser, env);

  DFSCH_UNWIND {
    this_op.fname = fname;
    this_op.toplevel = toplevel;
    this_op.next = lti->operation;
    lti->operation = &this_op;

    dfsch_parser_feed(parser, source);    

  } DFSCH_PROTECT {
    lti->operation = this_op.next;
    dfsch_set_current_package(saved_package);
  } DFSCH_PROTECT_END;

  if (dfsch_parser_get_level(parser)!=0){
      dfsch_error("Syntax error at end of input",
                  dfsch_make_string_cstr(fname));
  }  
}


static int qs_strcmp(const void* a, const void* b){
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
  char* buf;
  char* i;

  i = strrchr(name, '/');
  
  if (i) {
    buf = stracpy(i + 1);
  } else {
    buf = stracpy(name);
  }
  
  i = strchr(buf, '.');
  if (i){
    *i = '\0';
  }

  i = buf;

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
  {"load", dfsch_load_register},
  {"port-unsafe", dfsch_port_unsafe_register},
};

static char* pathname_directory(char* path){
  char* res;
  char* pos = strrchr(path, '/');

  if (!pos){
    return "./";
  }

  return strancpy(path, pos - path);
}

typedef struct module_loader_t {
  char* path_ext;
  void (*load)(char* fname, dfsch_object_t* env);
} module_loader_t;

static void scm_loader(char* fname, dfsch_object_t* env){
  dfsch_load_scm(env, fname, 0);
}
static void dsz_loader(char* fname, dfsch_object_t* env){
  dfsch_load_dsz(env, fname, 0);
}
static void so_loader(char* fname, dfsch_object_t* env){
  dfsch_load_so(env, fname, get_module_symbol(fname));
}

static module_loader_t loaders[] = {
  {".scm", scm_loader},
  {".dsz", dsz_loader},
  {".so", so_loader},
  {".dsl", so_loader},
};

void dfsch_load(dfsch_object_t* env, char* name, 
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
      return;
    }
  }

  if (path_list){
    path = path_list;
  } else {
    path = dfsch_env_get_cstr(env, "*load-path*");
    if (path == DFSCH_INVALID_OBJECT){
      path = NULL;
    }
  }

  while (DFSCH_PAIR_P(path)){
    dfsch_object_t* pp = DFSCH_FAST_CAR(path);
    if (!dfsch_string_p(pp)){
      dfsch_apply(pp, dfsch_make_string_cstr(name));
      path = DFSCH_FAST_CDR(path);
      continue;
    }
    l = sl_create();
    sl_append(l, dfsch_string_to_cstr(DFSCH_FAST_CAR(path)));
    sl_append(l, "/");
    sl_append(l, name);
    pathpart = sl_value(l);
    if (stat(pathpart, &st) == 0){ 
      if (S_ISREG(st.st_mode) || S_ISLNK(st.st_mode)){

        for (i = 0; i < sizeof(loaders) / sizeof(module_loader_t); i++){
          if (strcmp(pathpart + strlen(pathpart) - strlen(loaders[i].path_ext),
                     loaders[i].path_ext) == 0){
            loaders[i].load(pathpart, env);	      
            return;
          }
        }

        dfsch_load_scm(env, pathpart, 0);
        return;
      }
      if (S_ISDIR(st.st_mode)){
	char** list = my_scandir(pathpart);
	
	while(*list){
	  l = sl_create();
	  sl_append(l, pathpart);
	  sl_append(l, "/");
	  sl_append(l, *list);
	  
	  if (strcmp(".so", (*list)+strlen(*list)-3) == 0){
	    dfsch_load_so(env, fname, get_module_symbol(fname));	      
	  } else {
	    dfsch_load_scm(env, fname, 0);
	  }
	  
	  list++;
	}
        return;
      }
    }

    for (i = 0; i < sizeof(loaders) / sizeof(module_loader_t); i++){
      fname = stracat(pathpart, loaders[i].path_ext);
      if (stat(fname, &st) == 0 && (S_ISREG(st.st_mode) || 
                                    S_ISLNK(st.st_mode))){
        loaders[i].load(fname, env);	      
        return;
      }
    }
    
    path = dfsch_cdr(path);
  }
  
  dfsch_error("Module not found", dfsch_make_string_cstr(name));
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

int dfsch_require(dfsch_object_t* env, char* name, dfsch_object_t* path_list){
  dfsch_object_t* modules = dfsch_env_get_cstr(env, "*load-modules*");
  if (modules == DFSCH_INVALID_OBJECT){
    modules = NULL;
  }

  if (search_modules(modules, name)){
    return 1;
  }

  dfsch_load(env, name, path_list);
  return 0;
}

void dfsch_provide(dfsch_object_t* env, char* name){
  dfsch_object_t* modules = dfsch_env_get_cstr(env, "*load-modules*");
  if (modules == DFSCH_INVALID_OBJECT){
    modules = NULL;
  }

  if (search_modules(modules, name)){
    dfsch_error("Module already provided", dfsch_make_string_cstr(name));
  }


  /*
   * there should be define - module list is related to environment, but
   * this distinction is in most cases totally irrelevant, because modules
   * are mostly loaded into toplevel environment.
   */
  dfsch_define_cstr(env, "*load-modules*", 
                    dfsch_cons(dfsch_make_string_cstr(name),
                               modules));
}

void dfsch_load_add_module_source(dfsch_object_t* ctx,
                                  dfsch_object_t* src){
  dfsch_object_t* path = dfsch_env_get_cstr(ctx, "*load-path*");
  if (path != DFSCH_INVALID_OBJECT){
    dfsch_set_cstr(ctx, "*load-path*", dfsch_cons(src, path));
  }else{
    dfsch_define_cstr(ctx, "*load-path*", dfsch_list(1, src));
  }
}

void dfsch_load_extend_path(dfsch_object_t* ctx, char* dir){
  dfsch_load_add_module_source(ctx, dfsch_make_string_cstr(dir));
}

typedef struct read_ctx_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;
} read_ctx_t;

static int read_callback(dfsch_object_t *obj, read_ctx_t* ctx){

  dfsch_object_t* new_tail = dfsch_cons(obj, NULL);

  if (!ctx->head){
    ctx->head = new_tail;
  }else{
    dfsch_set_cdr(ctx->tail, new_tail);
  }

  ctx->tail = new_tail;

  return 1;
}

dfsch_object_t* dfsch_read_scm(char* scm_name, dfsch_object_t* eval_env){
  FILE* f = fopen(scm_name,"r");
  char buf[8193];
  read_ctx_t ictx;
  ssize_t r;
  int err=0;
  dfsch_object_t *obj;

  if (!f){
    dfsch_operating_system_error("fopen");
  }

  obj = dfsch_read_scm_stream(f, scm_name, eval_env);

  fclose(f);
    
  return obj;
}

dfsch_object_t* dfsch_read_scm_fd(int f, char* name, dfsch_object_t* eval_env){
  char buf[8193];
  read_ctx_t ictx;
  ssize_t r;
  int err=0;

  ictx.head = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, read_callback, &ictx);
  dfsch_parser_eval_env(parser, eval_env);

  while (!err && (r = read(f, buf, 8192))>0){
    buf[r]=0;
    err = dfsch_parser_feed(parser,buf);
  }

  if (r<0){
    dfsch_operating_system_error("read");
  }
 
  if (dfsch_parser_get_level(parser)!=0){
      dfsch_error("Syntax error at end of input",
                  dfsch_make_string_cstr(name));
  }  

  return ictx.head;
  
}
dfsch_object_t* dfsch_read_scm_stream(FILE* f, 
                                      char* name, 
                                      dfsch_object_t* eval_env){
  char buf[8193];
  read_ctx_t ictx;
  ssize_t r;
  int err=0;
  int l=0;

  ictx.head = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, read_callback, &ictx);
  dfsch_parser_set_source(parser, dfsch_make_string_cstr(name));
  dfsch_parser_eval_env(parser, eval_env);

  while (fgets(buf, 8192, f)){
    dfsch_parser_feed(parser,buf);
  }

  if (dfsch_parser_get_level(parser)!=0){
      dfsch_error("Syntax error at end of input",
                  dfsch_make_string_cstr(name));
  }  

  return ictx.head;
}

DFSCH_DEFINE_FORM(load_scm, NULL, {}){
  char* file_name;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_ARG(args, file_name);
  DFSCH_ARG_END(args);

  dfsch_load_scm(env, file_name, 0);
  return NULL;
}

DFSCH_DEFINE_FORM(load_so, NULL, {}){
  char* sym_name;
  char* so_name;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_ARG(args, so_name);
  DFSCH_STRING_ARG(args, sym_name);
  DFSCH_ARG_END(args);

  dfsch_load_so(env, so_name, sym_name);
  return NULL;
}

DFSCH_DEFINE_FORM(load, NULL, {}){
  char* name;
  dfsch_object_t* path_list;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, path_list, NULL)
  DFSCH_ARG_END(args);

  dfsch_load(env, name, path_list);  
  return NULL;
}
DFSCH_DEFINE_FORM(require, NULL, {}){
  char* name;
  dfsch_object_t* path_list;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_OBJECT_ARG_OPT(args, path_list, NULL)
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_require(env, name, path_list));  
}
DFSCH_DEFINE_FORM(provide, NULL, {}){
  char* name;

  args = dfsch_eval_list(args, env);
  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  dfsch_provide(env, name);  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(read_scm, NULL){
  char* filename;
  DFSCH_STRING_ARG(args, filename);

  return dfsch_read_scm(filename, NULL);
}

DFSCH_DEFINE_FORM(when_toplevel, 
                  "Evaluate expressions only in top-level loaded file"
                  " (intended to support files both usable as scripts "
                  "and modules)", 
                  {}){
  load_thread_info_t* lti = get_load_ti();

  if (!lti->operation || lti->operation->toplevel){
    return dfsch_eval_proc(args, env);
  } else {
    return NULL;
  }
}


#ifdef __WIN32__
#define PATH_SEP ';'
#else
#define PATH_SEP ':'
#endif

dfsch_object_t* dfsch_load_construct_default_path(){
  char* env_path = getenv("DFSCH_PATH");
  dfsch_object_t* path;

#ifdef __WIN32__
  char* dfsch_home = dfsch_get_interpreter_home();
  if (dfsch_home){
    path = dfsch_list(2, 
                      dfsch_make_string_cstr(dfsch_stracat(dfsch_home, 
                                                           "\\share\\dfsch\\scm\\")),
                      dfsch_make_string_cstr(dfsch_stracat(dfsch_home, 
                                                           "\\lib\\dfsch\\")));
  } else {
    path = NULL;
    fprintf(stderr, "warning: dfsch home directory is not set!\n");
  }

#else
  path = dfsch_list(2, 
                    dfsch_make_string_cstr(DFSCH_LIB_SCM_DIR),
                    dfsch_make_string_cstr(DFSCH_LIB_SO_DIR));
#endif

  if (env_path && *env_path){
    char* part_ptr;
    env_path = dfsch_stracpy(env_path);

    while (part_ptr = strrchr(env_path, PATH_SEP)){
      path = dfsch_cons(dfsch_make_string_cstr(part_ptr + 1), path);
      *part_ptr = '\0';
    }

    path = dfsch_cons(dfsch_make_string_cstr(env_path), path); 
  }

  return path;
}
dfsch_object_t* dfsch_load_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "*load-path*", 
                    dfsch_load_construct_default_path());
  dfsch_define_cstr(ctx, "*load-modules*", NULL);
  dfsch_define_cstr(ctx, "load-scm!",  DFSCH_FORM_REF(load_scm));
  dfsch_define_cstr(ctx, "read-scm", DFSCH_PRIMITIVE_REF(read_scm));
  dfsch_define_cstr(ctx, "load-so!", DFSCH_FORM_REF(load_so));
  dfsch_define_cstr(ctx, "load!", DFSCH_FORM_REF(load));
  dfsch_define_cstr(ctx, "require", DFSCH_FORM_REF(require));
  dfsch_define_cstr(ctx, "provide", DFSCH_FORM_REF(provide));

  dfsch_define_cstr(ctx, "when-toplevel", DFSCH_FORM_REF(when_toplevel));

  return NULL;
}
