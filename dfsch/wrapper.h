/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Simple custom data-types
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

#ifndef H__dfsch__wrapper__
#define H__dfsch__wrapper__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_make_wrapper_type(char* name,
                                                 dfsch_object_t* write,
                                                 dfsch_object_t* equal_p,
                                                 dfsch_object_t* apply,
                                                 dfsch_object_t* hash);
  
  extern dfsch_object_t* dfsch_wrap(dfsch_object_t* type,
                                    dfsch_object_t* object);
  extern dfsch_object_t* dfsch_unwrap(dfsch_object_t* type,
                                      dfsch_object_t* wrapper);

#ifdef __cplusplus
}
#endif

#endif
