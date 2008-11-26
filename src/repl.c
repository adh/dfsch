/*
 * dfsch - DFox's quick and dirty scheme implementation
 *   REP Loop
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

/** @file Simple test program for dfsch - REP loop. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/dfsch.h>
#include <dfsch/number.h>
#include <dfsch/parse.h>
#include <dfsch/load.h>
#include <dfsch/ports.h>
#include <dfsch/magic.h>
#include <dfsch/lib/cdebug.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <assert.h>

static void sigint_handler_break(int sig){
  dfsch_break("user:sigint"); 
}

dfsch_parser_ctx_t *parser;

static dfsch_object_t* command_exit(void*baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  switch (dfsch_list_length(args)){
  case 0:
    exit(0);
  case 1:
    if (dfsch_integer_p(dfsch_car(args))){
      exit((int)dfsch_number_to_long(dfsch_car(args)));
    }
  default:
    fputs(dfsch_obj_write(args,100,0),stderr);
    fputs("\n",stderr);
    fflush(stderr);
    exit(1);
  }
}
static dfsch_object_t* command_sleep(void*baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  long time;

  DFSCH_LONG_ARG(args, time);
  DFSCH_ARG_END(args);

  sleep(time);

  return NULL;
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
static dfsch_object_t* command_write(void* arg, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  
  while (dfsch_pair_p(args)){
    fputs(dfsch_obj_write(dfsch_car(args), 100, 1), stdout);
    args = dfsch_cdr(args);
  }
  puts("");
  return NULL;
}


void interactive_repl(dfsch_object_t* ctx){
  dfsch_console_run_repl("]=> ", ctx);
}

static int repl_callback(dfsch_object_t *obj, void *baton){
  dfsch_object_t* ret;
  ret = dfsch_eval(obj, baton);
  puts(dfsch_obj_write(ret,100,1));
}

void noninteractive_repl(dfsch_object_t* ctx){

  parser = dfsch_parser_create();
  dfsch_parser_callback(parser, repl_callback, ctx);

  while (!feof(stdin)){
    char str[4096];
    char *ret;
    
    if (fgets(str, 4096, stdin) == NULL)
      break;

    dfsch_parser_feed_catch(parser, str);
  }
  
  puts("");
}


int main(int argc, char**argv){
  int c;
  dfsch_object_t* ctx;
  int interactive = 1;
  int force_interactive = 0;

  GC_INIT();
  signal(SIGINT, sigint_handler_break);

  ctx = dfsch_make_context();

  dfsch_set_debugger(dfsch_cdebug_get_procedure());

  dfsch_load_register(ctx);
  dfsch_port_unsafe_register(ctx);

  dfsch_define_cstr(ctx,"exit",dfsch_make_primitive(command_exit,NULL));
  dfsch_define_cstr(ctx,"print",dfsch_make_primitive(command_print,NULL));
  dfsch_define_cstr(ctx,"sleep",dfsch_make_primitive(command_sleep,NULL));
  dfsch_restart_bind(dfsch_make_restart(dfsch_make_symbol("quit"),
                                        dfsch_make_primitive(command_exit,
                                                             NULL),
                                        "Exit interpreter"));
                                        

  while ((c=getopt(argc, argv, "+ir:l:L:e:E:hv")) != -1){
    switch (c){
    case 'r':
      dfsch_require(ctx, optarg, NULL);
      break;
    case 'l':
      dfsch_load_scm(ctx, optarg);
      break;
    case 'L':
      dfsch_load_extend_path(ctx, optarg);
      break;
    case 'e':
      {
        dfsch_eval_proc(dfsch_list_read(optarg), ctx);
        interactive = 0;
        break;
      }
    case 'E':
      {
        puts(dfsch_obj_write(dfsch_eval_proc(dfsch_list_read(optarg), ctx),
                             100, 1));
        interactive = 0;

        break;
      }
    case 'i':
      force_interactive = 1;
      break;
    case 'v':
      printf("dfsch version %s\n\n", PACKAGE_VERSION);
      puts("Copyright (C) 2005-2008 Ales Hakl");
      puts("dfsch comes with ABSOLUTELY NO WARRANTY");
      puts("This is free software, and you are welcome to redistribute it");
      puts("under certain conditions; see file COPYING for details.");
      return 0;
    default:
      printf("Usage: %s [<options>] [<filename> ...]\n\n", argv[0]);
      puts("Options:");
      puts("  -l <filename>     Load scheme file on startup");
      puts("  -r <module-name>  Require (load) module on startup");
      puts("  -L <directory>    Append directory to load:path");
      puts("  -e <expression>   Execute given expression");
      puts("  -E <expression>   Evaluate given expression");
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
      dfsch_vector_set(args, i, dfsch_make_string_cstr(argv[optind+i]));
    }
    dfsch_define_cstr(ctx, "*posix-argv*", args);

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
