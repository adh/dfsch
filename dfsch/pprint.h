/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Pretty-printer
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

#ifndef H__dfsch__pprint__
#define H__dfsch__pprint__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern char* dfsch_pprint(dfsch_object_t* object, 
                             int margin_l, int margin_r,
                             int depth);

#ifdef __cplusplus
}
#endif

#endif
