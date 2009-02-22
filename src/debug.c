/*
 * dfsch - Scheme-like Lisp dialect
 *   Interactive debugger
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

#include "dfsch/debug.h"
#include <dfsch/dfsch.h>
#include <dfsch/conditions.h>
#include <stdio.h>

static void debug_main(dfsch_object_t* condition){
  dfsch_object_t* restarts = dfsch_compute_restarts();
  char buf[512];

  fprintf(stderr, "debugger invoked on %s:\n",
          DFSCH_TYPE_OF(condition)->name);
  fprintf(stderr,"  %s\n", 
          dfsch_object_2_string(dfsch_condition_field_cstr(condition,
                                                     "message"),
                          10, 0));

  
  fprintf(stderr, "\nrestarts:\n");
  while (DFSCH_PAIR_P(restarts)){
    dfsch_object_t* restart = DFSCH_FAST_CAR(restarts);
    fprintf(stderr, "  [%s]: %s\n", 
            dfsch_object_2_string(dfsch_restart_name(restart), 1, 1),
            dfsch_restart_description(restart));
    restarts = DFSCH_FAST_CDR(restarts);
  }
  
  fprintf(stderr, "dbg> ");
  fgets(buf, 512, stdin);
  buf[strlen(buf)-1]='\0';
  dfsch_invoke_restart(dfsch_make_symbol(buf));

}

DFSCH_DEFINE_PRIMITIVE(debug_handler, 0){
  dfsch_object_t* condition;
  DFSCH_OBJECT_ARG(args, condition);
  DFSCH_ARG_END(args);
  
  debug_main(condition);

  return NULL;
}

void dfsch_debug_register_handler(){
  dfsch_handler_bind(NULL, DFSCH_PRIMITIVE_REF(debug_handler));
}
