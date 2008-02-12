/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
 * Copyright (C) 2007 Ales Hakl
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

#ifndef H__dfsch__object__
#define H__dfsch__object__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_object_make_class(dfsch_object_t* superclass,
                                                 char* name);
  extern dfsch_object_t* dfsch_object_make_instance(dfsch_object_t* klass);

  extern dfsch_object_t* dfsch_object_slot_set(dfsch_object_t* object,
                                               dfsch_object_t* name,
                                               dfsch_object_t* value);
  extern int dfsch_object_slot_unset(dfsch_object_t* object,
                                     dfsch_object_t* name);
  extern dfsch_object_t* dfsch_object_slot_ref(dfsch_object_t* object,
                                               dfsch_object_t* name);
  extern dfsch_object_t* dfsch_object_slots_2_alist(dfsch_object_t* object);


#ifdef __cplusplus
}
#endif

#endif
