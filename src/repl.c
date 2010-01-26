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
#include <dfsch/lib/cmdopts.h>
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

DFSCH_DEFINE_PRIMITIVE(break, 0){
  dfsch_cerror("SIGINT caught", NULL);
}

static void sigint_handler_break(int sig){
  dfsch_async_apply_self(DFSCH_PRIMITIVE_REF(break));
}

dfsch_parser_ctx_t *parser;

static dfsch_object_t* command_exit(void*baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  switch (dfsch_list_length_check(args)){
  case 0:
    exit(0);
  case 1:
    if (dfsch_integer_p(dfsch_car(args))){
      exit((int)dfsch_number_to_long(dfsch_car(args)));
    }
  default:
    fputs(dfsch_object_2_string(args,100,0),stderr);
    fputs("\n",stderr);
    fflush(stderr);
    exit(1);
  }
}


void interactive_repl(dfsch_object_t* ctx){
  printf("  /\\___/\\    dfsch version %s\n", 
         dfsch_get_version());
  printf(" ( o   o )     (%s) [%s %s]\n", 
         dfsch_get_build_id(), __DATE__, __TIME__);
  printf(" ==  *  ==   dfsch is free software, and you are welcome to redistribute it\n");
  printf("   )   (     under certain conditions; see file COPYING for details.\n");
  dfsch_console_run_repl("]=> ", ctx);
}

static int repl_callback(dfsch_object_t *obj, void *baton){
  dfsch_object_t* ret;
  signal(SIGINT, sigint_handler_break);
  ret = dfsch_eval(obj, baton);
  puts(dfsch_object_2_string(ret,100,1));
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

typedef struct reload_ctx_t {
  char* fname;
  dfsch_object_t* env;
} reload_ctx_t;
DFSCH_PRIMITIVE_HEAD(reload){
  reload_ctx_t* c = baton;
  DFSCH_ARG_END(args);
  dfsch_load_scm(c->env, c->fname, 0);
  return NULL;
}
static void load_scm(dfsch_object_t* env, char* fname){
  reload_ctx_t* c = GC_NEW(reload_ctx_t);
  dfsch_load_scm(env, fname, 0);
  c->fname = fname;
  c->env = env;
  dfsch_define_cstr(env, "reload", dfsch_make_primitive(p_reload_impl, c));
}

int main(int argc, char**argv){
  int c;
  dfsch_object_t* ctx;
  int interactive = 1;
  int force_interactive = 0;
#ifdef __unix__
  struct sigaction act;
#endif

  GC_INIT();

#ifdef __unix__
  act.sa_handler = sigint_handler_break;
  act.sa_flags = 0;
  sigemptyset(&act.sa_mask);
  sigaction(SIGINT, &act, NULL);
#endif


  ctx = dfsch_make_top_level_environment();


  dfsch_load_register(ctx);
  dfsch_port_unsafe_register(ctx);
  dfsch_set_standard_io_ports();

  dfsch_restart_bind(dfsch_make_restart(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                            "quit"),
                                        dfsch_make_primitive(command_exit,
                                                             NULL),
                                        "Exit interpreter",
                                        NULL));
                                        

  while ((c=getopt(argc, argv, "+ir:l:L:e:E:hvd")) != -1){
    switch (c){
    case 'r':
      dfsch_require(ctx, optarg, NULL);
      break;
    case 'l':
      load_scm(ctx, optarg);
      break;
    case 'L':
      dfsch_load_extend_path(ctx, optarg);
      break;
    case 'e':
      {
        dfsch_eval_proc(dfsch_string_2_object_list(optarg), ctx);
        interactive = 0;
        break;
      }
    case 'E':
      {
        puts(dfsch_object_2_string(dfsch_eval_proc(dfsch_string_2_object_list(optarg), ctx),
                                   100, 1));
        interactive = 0;

        break;
      }
    case 'd':
      dfsch_cdebug_set_as_debugger();
      break;
    case 'i':
      force_interactive = 1;
      break;
    case 'v':
      printf("dfsch version %s\n\n", PACKAGE_VERSION);
      puts("Copyright (C) 2005-2009 Ales Hakl");
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
      puts("  -d                Enable cdebug debugger early");
      puts("");
      puts("First non-option argument is treated as filename of program to run");
      puts("Run without non-option arguments to start in interactive mode");
      return 0;
    }
  }

  if (optind < argc) {
    char* directory = dfsch_get_path_directory(dfsch_realpath(argv[optind]));
    
    dfsch_load_extend_path(ctx, directory);

    dfsch_define_cstr(ctx, "*posix-argv*", 
                      dfsch_cmdopts_argv_to_list(argc - optind, 
                                                 argv + optind));
    dfsch_load_scm(ctx, argv[optind], 1);
    return 0;
  }

  if (interactive || force_interactive){
    dfsch_load_extend_path(ctx, dfsch_getcwd());
    if (isatty(0)){
      dfsch_cdebug_set_as_debugger();
      interactive_repl(ctx);
    }else{
      noninteractive_repl(ctx);
    }
  }
  

  return 0;
}
