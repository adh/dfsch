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

#include "dfsch.h"
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

/**
 * REP (read, eval, print) loop of dfsch.
 *
 * Use this code as example if you want to embbed dfsch in your application.
 *
 */
int main(int argc, char**argv){
  
  dfsch_ctx_t* ctx = dfsch_make_context();

  dfsch_ctx_define(ctx,"version",dfsch_make_string("0.1"));
  dfsch_ctx_define(ctx,"argv0",dfsch_make_string(argv[0]));
  dfsch_ctx_define(ctx,"arg-count",dfsch_make_number(argc));

  dfsch_ctx_define(ctx,"abort!",dfsch_make_primitive(&abort));

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
    free(out);
    
    dfsch_gc();
  }

  dfsch_destroy_context(ctx);

  return 0;
}
