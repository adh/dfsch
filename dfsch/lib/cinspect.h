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

#ifndef H__dfsch__cinspect__
#define H__dfsch__cinspect__

#include <dfsch/dfsch.h>

void dfsch_cinspect_inspect_object(dfsch_object_t* object);
dfsch_object_t* dfsch_cinspect_get_procedure();
void dfsch_cinspect_set_as_inspector();

#endif
