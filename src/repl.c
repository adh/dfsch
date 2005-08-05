/*
 * dfsch - DFox's quick and dirty scheme implementation
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

/** @file Simple test program for dfsch - REP loop. */

#include <dfsch/dfsch.h>
#include <dfsch/stream.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <fcntl.h>
#include <errno.h>
#include <gc/gc.h>
#include <signal.h>

typedef struct import_ctx_t {
  dfsch_object_t* ret;
  dfsch_ctx_t* ctx;
} import_ctx_t;

static int import_callback(dfsch_object_t *obj, void* baton){
  ((import_ctx_t*)baton)->ret = dfsch_ctx_eval(((import_ctx_t*)baton)->ctx, 
                                               obj);
  return 1;
}


static dfsch_object_t* import_impl(char *name, dfsch_ctx_t* ctx){
  int f = open(name,O_RDONLY);
  char buf[8193];
  import_ctx_t ictx;
  ssize_t r;

  if (f<0){
    int err = errno;
    DFSCH_THROW("import:unix-error",dfsch_make_string(strerror(err)));
  }

  ictx.ctx = ctx;
  ictx.ret = NULL;

  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  dfsch_parser_callback(parser, import_callback, &ictx);

  while ((r = read(f, buf, 8192))>0){
    buf[r]=0;
    dfsch_parser_feed(parser,buf);
  }

  close(f);

  return ictx.ret;
}

static dfsch_object_t* import(void *baton, dfsch_object_t* args){
  dfsch_object_t* arg = dfsch_car(args);
  if (dfsch_object_string_p(arg)){
    return import_impl(dfsch_string(arg), baton);
  }else if (dfsch_object_symbol_p(arg)){
    DFSCH_THROW("import:unimplemented",NULL);
  }else{
    DFSCH_THROW("import:unknown-entity",arg);
  }
}



static int callback(dfsch_object_t *obj, void* baton){
  char *out = dfsch_obj_write(dfsch_ctx_eval(baton, obj),100);
  puts(out);
  return 1;
}

dfsch_parser_ctx_t *parser;

void foo(int sig){
  dfsch_parser_reset(parser);
  rl_set_prompt("]=> ");
  rl_redisplay();
  signal(SIGINT, foo);
}

/**
 * REP (read, eval, print) loop of dfsch.
 *
 * Use this code as example if you want to embbed dfsch in your application.
 *
 */
int main(int argc, char**argv){
  
  GC_INIT();

  signal(SIGINT, foo);

  dfsch_ctx_t* ctx = dfsch_make_context();
  parser = dfsch_parser_create();

  dfsch_parser_callback(parser, callback, ctx);

  dfsch_ctx_define(ctx,"version",dfsch_make_string("0.2dev"));
  dfsch_ctx_define(ctx,"argv0",dfsch_make_string(argv[0]));
  dfsch_ctx_define(ctx,"arg-count",dfsch_make_number(argc));

  //  dfsch_ctx_define(ctx,"abort!",dfsch_make_primitive(abort,NULL));
  dfsch_ctx_define(ctx,"import!",dfsch_make_primitive(import,ctx));

  rl_bind_key ('\t', rl_insert);

  while (1){
    char *str;
    char *prompt;
    int rc;
    int level = dfsch_parser_get_level(parser);

    if (level){
      prompt = GC_MALLOC_ATOMIC((level*2)+5);
      memset(prompt, ' ', level*2);
      prompt[level*2+0] = '.';
      prompt[level*2+1] = '.';
      prompt[level*2+2] = '>';
      prompt[level*2+3] = ' ';
      prompt[level*2+4] = 0;
    }else{
      prompt = "]=> ";
    }

    str = readline(prompt);
    if (!str)
      break;
    add_history(str);

    if (rc = dfsch_parser_feed(parser,str)!=0){
      fprintf(stderr,";; Parse error: %d",rc);
    }
    dfsch_parser_feed(parser,"\n");

    free(str);
  }
  
  puts("");

  return 0;
}
