/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Regular expressions
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

#ifndef H__dfsch__regex__
#define H__dfsch__regex__

#include <dfsch/dfsch.h>
#include <regex.h>

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * Compile given regular expression into regex object. Flags are same as 
   * for regcomp(3).
   */
  dfsch_object_t* dfsch_regex_compile(char* expression, int flags);

  /**
   * Apply precompiled regex object to given string and return whetever 
   * regular expression matches
   */
  int dfsch_regex_match_p(dfsch_object_t* regex, char* string, int flags);

  /**
   * Apply precompiled regex object to given string and return vector of
   * matching substrings (each as vector of start offset, end offset and 
   * matching string).
   */
  dfsch_object_t* dfsch_regex_substrings(dfsch_object_t* regex, char* string,
                                         int flags);

  /**
   * Define regular expression primitives in given environment.
   */
  dfsch_object_t* dfsch_module_regex_register(dfsch_object_t *ctx);

#ifdef __cplusplus
}
#endif

#endif
