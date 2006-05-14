/*
 * dfsch - DFox's quick and dirty scheme implementation
 *   REP Loop
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/dfsch.h>
#include <dfsch/parse.h>
#include <dfsch/load.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <assert.h>


FILE *cmd_log;

typedef struct evaluator_ctx_t {
  dfsch_ctx_t *ctx;
  dfsch_object_t *expr;
} evaluator_ctx_t;

static void sigint_handler_break(int sig){
  dfsch_throw("user:sigint", NULL);
}

static dfsch_object_t* evaluator_thunk(evaluator_ctx_t *baton, 
                                       dfsch_object_t *args){
  dfsch_object_t *ret;

  signal(SIGINT, sigint_handler_break);

  ret = dfsch_ctx_eval(baton->ctx, baton->expr);
  puts(dfsch_obj_write(ret,100,1));

  if (cmd_log){
    fputs(dfsch_obj_write(baton->expr,1000,1),cmd_log);
    fputs("\n",cmd_log);
    fflush(cmd_log);
  }
}
static dfsch_object_t* evaluator_handler(dfsch_object_t *baton, 
                                         dfsch_object_t *args){
  fputs(dfsch_exception_write(dfsch_car(args)),stderr);      
}

static int callback(dfsch_object_t *obj, void *baton){
  evaluator_ctx_t ctx;

  ctx.ctx = baton;
  ctx.expr = obj;

  dfsch_try(dfsch_make_primitive((dfsch_primitive_t)evaluator_handler, obj),
            dfsch_make_primitive((dfsch_primitive_t)evaluator_thunk, &ctx));
  return 1;
}

dfsch_parser_ctx_t *parser;

#ifdef USE_READLINE
static void sigint_handler_rl(int sig){
  dfsch_parser_reset(parser);
  rl_set_prompt("]=> ");
  rl_replace_line("", 1);
  rl_redisplay();
  signal(SIGINT, sigint_handler_rl);
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
      return (char*)strdup(name);
    }
  }
  
  /* If no names matched, then return NULL. */
  return ((char *)NULL);
}

static char ** symbol_completion (const char* text, int start, int end){
  return rl_completion_matches (text, symbol_completion_cb);
}
#endif /* USE_READLINE */

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

static dfsch_object_t* command_exit(void*baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  switch (dfsch_list_length(args)){
  case 0:
    exit(0);
  case 1:
    if (dfsch_number_p(dfsch_car(args))){
      exit((int)dfsch_number_to_long(dfsch_car(args)));
    }
  default:
    fputs(dfsch_obj_write(args,100,0),stderr);
    fputs("\n",stderr);
    fflush(stderr);
    exit(1);
  }
}
static dfsch_object_t* command_print(void* arg, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  
  while (dfsch_pair_p(args)){
    fputs(dfsch_obj_write(dfsch_car(args), 100, 0), stdout);
    args = dfsch_cdr(args);
  }
  puts("");
  return NULL;
}



/**
 * REP (read, eval, print) loop of dfsch.
 *
 * Use this code as example if you want to embbed dfsch in your application.
 *
 */

#ifdef USE_READLINE
void interactive_repl(dfsch_ctx_t* ctx){

  parser = dfsch_parser_create();
  dfsch_parser_callback(parser, callback, ctx);

  rl_readline_name = "dfsch";
  rl_attempted_completion_function = symbol_completion;
  rl_basic_word_break_characters = " \t\n\"()";

  while (1){
    char *str;
    int rc;


    signal(SIGINT, sigint_handler_rl);
    str = readline(get_prompt(dfsch_parser_get_level(parser)));
    if (!str)
      break;
    add_history(str);

    if (rc = dfsch_parser_feed(parser,str)!=0){
      fprintf(stderr,"Parse error\n",rc);
    }
    dfsch_parser_feed(parser,"\n");

    free(str);
  }
  
  puts("");

}
#else
void interactive_repl(dfsch_ctx_t* ctx){

  parser = dfsch_parser_create();
  dfsch_parser_callback(parser, callback, ctx);

  fputs(get_prompt(dfsch_parser_get_level(parser)), stdout);

  while (!feof(stdin)){
    char str[4096];
    int rc;
    
    if(fgets(str, 4096, stdin) == NULL)
      break;

    if (rc = dfsch_parser_feed(parser,str)!=0){
      fprintf(stderr,"Parse error\n",rc);
    }

    if (str[strlen(str)-1] == '\n')
      fputs(get_prompt(dfsch_parser_get_level(parser)), stdout);

  }
  
  puts("");

}
#endif

void noninteractive_repl(dfsch_ctx_t* ctx){

  parser = dfsch_parser_create();
  dfsch_parser_callback(parser, callback, ctx);

  while (!feof(stdin)){
    char str[4096];
    int rc;
    
    if (fgets(str, 4096, stdin) == NULL)
      break;

    if (rc = dfsch_parser_feed(parser,str)!=0){
      fprintf(stderr,"Parse error\n",rc);
    }

  }
  
  puts("");

}


int main(int argc, char**argv){
  int c;
  dfsch_ctx_t* ctx;
  int interactive = 1;
  int force_interactive = 0;

  GC_INIT();
  signal(SIGINT, sigint_handler_break);

  cmd_log = NULL;
  ctx = dfsch_make_context();

  dfsch_ctx_define(ctx,"version",dfsch_make_string(VERSION));

  dfsch_load_register(ctx);

  dfsch_ctx_define(ctx,"exit",dfsch_make_primitive(command_exit,NULL));
  dfsch_ctx_define(ctx,"print",dfsch_make_primitive(command_print,NULL));

  while ((c=getopt(argc, argv, "+l:e:E:hvO:")) != -1){
    switch (c){
    case 'l':
      {
        dfsch_load_scm(ctx, optarg);
        break;
      }
    case 'O':
      cmd_log = fopen(optarg, "a");
      if (!cmd_log)
	perror(optarg);
      break;
    case 'e':
      {
        dfsch_ctx_eval_list(ctx, dfsch_list_read(optarg));
        interactive = 0;
        break;
      }
    case 'E':
      {
        puts(dfsch_obj_write(dfsch_ctx_eval_list(ctx, dfsch_list_read(optarg)),
                             100, 1));
        interactive = 0;

        break;
      }
    case 'i':
      force_interactive = 1;
      break;
    case 'v':
      printf("dfsch version %s\n\n", PACKAGE_VERSION);
      puts("Copyright (C) 2005 Ales Hakl");
      puts("dfsch comes with ABSOLUTELY NO WARRANTY");
      puts("This is free software, and you are welcome to redistribute it");
      puts("under certain conditions; see file COPYING for details.");
      return 0;
    default:
      printf("Usage: %s [<options>] [<filename> ...]\n\n", argv[0]);
      puts("Options:");
      puts("  -l <filename>     Load scheme file on startup");
      puts("  -e <expression>   Execute given expression");
      puts("  -E <expression>   Evaluate given expression");
      puts("  -O <filename>     Log sucessfuly executed statements");
      puts("  -i                Force interactive mode");

      puts("First non-option argument is treated as filename of program to run");
      puts("Run without non-option arguments to start in interactive mode");
      return 0;
    }
  }

  if (optind < argc) {
    dfsch_object_t* ret;
    dfsch_object_t* args = dfsch_make_vector(argc-optind,NULL);
    int i;

    for (i=0; i<argc-optind; i++){
      dfsch_vector_set(args, i, dfsch_make_string(argv[optind+i]));
    }
    dfsch_ctx_define(ctx, "argv", args);

    ret = dfsch_load_scm(ctx, argv[optind]);
    return 0;
  }

  if (interactive || force_interactive){
    if (isatty(0)){
      interactive_repl(ctx);
    }else{
      noninteractive_repl(ctx);
    }
  }
  

  return 0;
}
