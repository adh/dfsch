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
#include <dfsch/load.h>
#include <dfsch/ports.h>
#include <dfsch/lib/cmdopts.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <assert.h>

#ifdef __WIN32__
#include <windows.h>
#define WINDOWS_FLAGS "c"
#else
#define WINDOWS_FLAGS
#endif

DFSCH_DEFINE_PRIMITIVE(break, 0){
  dfsch_cerror("SIGINT caught", NULL);
}

static void sigint_handler_break(int sig){
  dfsch_async_apply_self(DFSCH_PRIMITIVE_REF(break));
}

int main(int argc, char**argv){
  int c;
  dfsch_object_t* ctx;
  int program_is_dsz = 0;
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

  dfsch_activate_segv_handler();

  ctx = dfsch_make_top_level_environment();


  dfsch_load_register(ctx);
  dfsch_port_unsafe_register(ctx);
  dfsch_set_standard_io_ports();
                                        
  while ((c=getopt(argc, argv, "+L:zv" WINDOWS_FLAGS)) != -1){
    switch (c){
    case 'L':
      dfsch_load_extend_path(ctx, optarg);
      break;
    case 'z':
      program_is_dsz = 1;
      break;

#ifdef __WIN32__
    case 'c':
      AllocConsole();
      break;
#endif

    case 'v':
      printf("dfsch version %s\n\n", PACKAGE_VERSION);
      puts("Copyright (C) 2005-2010 Ales Hakl");
      puts("dfsch comes with ABSOLUTELY NO WARRANTY");
      puts("This is free software, and you are welcome to redistribute it");
      puts("under certain conditions; see file COPYING for details.");
      return 0;
    default:
      printf("Usage: %s [<options>] [<filename> ...]\n\n", argv[0]);
      puts("Options:");
      puts("  -L <directory>    Append directory to load:path");
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
    if (program_is_dsz){
      dfsch_load_dsz(ctx, argv[optind], 1);
    } else {
      dfsch_load_scm(ctx, argv[optind], 1);
    }
    return 0;
  }
}
