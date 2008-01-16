/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Generic functions
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

#ifndef H__dfsch__generic__
#define H__dfsch__generic__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_make_generic(char* name);

void dfsch_method_set(dfsch_object_t* generic,
                      dfsch_object_t* type,
                      dfsch_object_t* method);
void dfsch_method_unset(dfsch_object_t* generic,
                        dfsch_object_t* type);
dfsch_object_t* dfsch_method_ref(dfsch_object_t* generic,
                                 dfsch_object_t* type);
dfsch_object_t* dfsch_methods_2_alist(dfsch_object_t* generic);

dfsch_object_t* dfsch_define_generic(dfsch_object_t* name, 
                                     dfsch_object_t* env);
dfsch_object_t* dfsch_define_generic_cstr(char* name, 
                                          dfsch_object_t* env);
void dfsch_define_method_cstr(char* name, 
                              dfsch_object_t* type,
                              dfsch_object_t* method,
                              dfsch_object_t* env);

#endif
