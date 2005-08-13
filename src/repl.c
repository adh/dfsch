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
#include <dfsch/load.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <fcntl.h>
#include <errno.h>
#include <gc/gc.h>
#include <signal.h>
#include <assert.h>




static int callback(dfsch_object_t *obj, void* baton){
  char *out = dfsch_obj_write(dfsch_ctx_eval(baton, obj),100);
  puts(out);
  return 1;
}

dfsch_parser_ctx_t *parser;

void sigint_handler(int sig){
  dfsch_parser_reset(parser);
  rl_set_prompt("]=> ");
  rl_redisplay();
  signal(SIGINT, sigint_handler);
}


     

static char * symbol_completion_cb (const char* text, int state){
  char *name;
  static dfsch_symbol_iter_t* iter;
  int len;


  if (state==0)
    iter = NULL;

  len = strlen(text);

  while (name = dfsch_get_next_symbol(&iter)){
    if (strncmp (name, text, len) == 0){
      return strdup(name);
    }
  }
  
  /* If no names matched, then return NULL. */
  return ((char *)NULL);
}

static char ** symbol_completion (const char* text, int start, int end){
  return rl_completion_matches (text, symbol_completion_cb);
}


static char* get_prompt(int level){
  if (level){
    char *prompt = GC_MALLOC_ATOMIC((level*2)+5);
    memset(prompt, ' ', level*2);
    prompt[level*2+0] = '.';
    prompt[level*2+1] = '.';
    prompt[level*2+2] = '>';
    prompt[level*2+3] = ' ';
    prompt[level*2+4] = 0;
    return prompt;
  }else{
    return "]=> ";
  }
}

dfsch_object_t* command_exit(void*baton, dfsch_object_t* args){
  switch (dfsch_list_length(args)){
  case 0:
    exit(0);
  case 1:
    if (dfsch_object_number_p(dfsch_car(args))){
      exit((int)dfsch_number(dfsch_car(args)));
    }
  default:
    fputs(dfsch_obj_write(args,100),stderr);
    fputs("\n",stderr);
    fflush(stderr);
    exit(1);
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

  dfsch_ctx_t* ctx = dfsch_make_context();
  parser = dfsch_parser_create();

  dfsch_parser_callback(parser, callback, ctx);
  signal(SIGINT, sigint_handler);

  dfsch_ctx_define(ctx,"version",dfsch_make_string("0.2dev"));
  dfsch_ctx_define(ctx,"argv0",dfsch_make_string(argv[0]));
  dfsch_ctx_define(ctx,"arg-count",dfsch_make_number(argc));

  dfsch_load_register(ctx);

  dfsch_ctx_define(ctx,"exit",dfsch_make_primitive(command_exit,NULL));

  //rl_bind_key ('\t', rl_insert);
  rl_readline_name = "dfsch";
  rl_attempted_completion_function = symbol_completion;




  while (1){
    char *str;
    int rc;


    str = readline(get_prompt(dfsch_parser_get_level(parser)));
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
