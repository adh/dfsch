/*
 * dfsch - Scheme-like Lisp dialect
 *   module loading code
 * Copyright (C) 2005-2010 Ales Hakl
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

/*
 * Modules are loaded into environments. By default, search path is given by
 * variable named dfsch:*load-path*, with dfsch:*modules* being list of 
 * already loaded modules.
 * 
 * Load path list can contain either pathnames as strings or callable objects
 * callable object is passed and environment and name of module to load,
 * it is expected to do any processing internally and return non-nil value if 
 * successful.
 *
 * Set of built-in modules and on-disk module formats is easily extensible, 
 * but currently only by modifying src/load.c.
 */

#ifndef H__dfsch__load__
#define H__dfsch__load__

#include <dfsch/dfsch.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

  /** Append directory to load:path */
  void dfsch_load_extend_path(dfsch_object_t* ctx, char* dir);
  void dfsch_load_add_module_source(dfsch_object_t* ctx,
                                    dfsch_object_t* src);
  /** Load given module (as by require, but unconditionally) */
  extern void dfsch_load(dfsch_object_t* env, char* name, 
                         dfsch_object_t* path_list,
                         int as_toplevel);
  /** Load given module if it is not provided yet */
  extern int dfsch_require(dfsch_object_t* env, char* name, 
                           dfsch_object_t* path_list);
  /** Load given module as top-level program */
  extern void dfsch_run_module(dfsch_object_t* env, char* name, 
                               dfsch_object_t* path_list);
  /** Provide given module (mark as loaded) */
  extern void dfsch_provide(dfsch_object_t* env, char* name);


  /** Load given shared object module and register it into given context. */
  extern void dfsch_load_so(dfsch_object_t* ctx, 
                            char* so_name, 
                            char* sym_name,
                            int as_toplevel);
  /** Load given source code buffer into given environment. */  
  extern void dfsch_load_source(dfsch_object_t* env,
                                char* fname,
                                int toplevel,
                                char* source);


  /** Load given source file (absolute path) into given environment. */
  extern void dfsch_load_scm(dfsch_object_t* ctx, char* scm_name, 
                             int toplevel);

  /** Read scheme list from given file. */
  extern dfsch_object_t* dfsch_read_scm(char* scm_name, 
                                        dfsch_object_t* eval_env);
  /** Read scheme list from given file descriptor. */
  extern dfsch_object_t* dfsch_read_scm_fd(int f, 
                                           char* name, 
                                           dfsch_object_t* eval_env);
  /** Read scheme list from given stdio stream. */
  extern dfsch_object_t* dfsch_read_scm_stream(FILE* f, 
                                               char* name, 
                                               dfsch_object_t* eval_env);

  /** Register shared object related part of this module. */
  extern dfsch_object_t* dfsch_load_so_register(dfsch_object_t *ctx);
  /** Register scheme related part of this module. */
  extern dfsch_object_t* dfsch_load_scm_register(dfsch_object_t *ctx);

#ifdef __cplusplus
}
#endif

#endif
