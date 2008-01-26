/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   External object references
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

#ifndef H__dfsch__lib__extref__
#define H__dfsch__lib__extref__

#include <dfsch/dfsch.h>
#include <time.h>

#define DFSCH_EXTREF_FROMNOW  0
#define DFSCH_EXTREF_ONCEONLY 1
#define DFSCH_EXTREF_REFRESH  2

char* dfsch_extref_create(dfsch_object_t* object, time_t timeout, int mode);
dfsch_object_t* dfsch_extref_ref(char* ref);

dfsch_object_t* dfsch_module_extref_register(dfsch_object_t* env);

#endif
