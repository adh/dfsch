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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <fcntl.h>
#include <errno.h>
#include <gc/gc.h>


static dfsch_ctx_t* ctx;

static dfsch_object_t* import_impl(char *name){
  int f = open(name,O_RDONLY);
  off_t size,l;
  char* buf;
  dfsch_object_t* exp;

  if (f<0){
    int err = errno;
    return dfsch_make_exception(dfsch_make_symbol("import:unix-error"),
				dfsch_make_string(strerror(err)));
  }

  size = lseek(f,0,SEEK_END);
  lseek(f,0,SEEK_SET);

  buf = malloc(size+1);
  if (!buf){
    close(f);
    return 0;
  }

  l = read(f,buf,size);
  printf(";; Read: %d Bytes out of %d \n",l,size);
  buf[size]=0;

  exp = dfsch_list_read(buf);
  free(buf);

  return dfsch_ctx_eval_list(ctx,exp);

}

dfsch_object_t* import(void *baton, dfsch_object_t* args){
  dfsch_object_t* arg = dfsch_car(args);
  if (dfsch_object_string_p(arg)){
    return import_impl(dfsch_string(arg));
  }else if (dfsch_object_symbol_p(arg)){

  }else{
    return dfsch_make_exception(dfsch_make_symbol("import:unknown-entity"),
				arg);
  }
}

/**
 * REP (read, eval, print) loop of dfsch.
 *
 * Use this code as example if you want to embbed dfsch in your application.
 *
 */
int main(int argc, char**argv){
  
  GC_INIT();

  ctx = dfsch_make_context();

  dfsch_ctx_define(ctx,"version",dfsch_make_string("0.1"));
  dfsch_ctx_define(ctx,"argv0",dfsch_make_string(argv[0]));
  dfsch_ctx_define(ctx,"arg-count",dfsch_make_number(argc));

  dfsch_ctx_define(ctx,"abort!",dfsch_make_primitive(&abort,NULL));
  dfsch_ctx_define(ctx,"import!",dfsch_make_primitive(&import,NULL));

  rl_bind_key ('\t', rl_insert);

  while (1){
    char *str;
    char *out;
    dfsch_object_t *exp, *res;

    str = readline("]=> ");
    if (!str)
      break;
    add_history(str);


    exp = dfsch_obj_read(str);
    free(str);
    res = dfsch_ctx_eval(ctx, exp);
    out = dfsch_obj_write(res,100);

    puts(out);
    
  }


  return 0;
}
