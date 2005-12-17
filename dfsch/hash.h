/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Hash tables
 * Copyright (C) 2005 Ales Hakl
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


#ifndef H__dfsch__hash__
#define H__dfsch__hash__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_hash_make();
  extern int dfsch_hash_p(dfsch_object_t* obj);
  extern dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash, 
                                        dfsch_object_t* key);
  extern dfsch_object_t* dfsch_hash_set(dfsch_object_t* hash,
                                        dfsch_object_t* key,
                                        dfsch_object_t* value);
  extern dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash,
                                          dfsch_object_t* key);
  extern dfsch_object_t* dfsch_hash_set_if_exists(dfsch_object_t* hash,
                                                  dfsch_object_t* key,
                                                  dfsch_object_t* value);
  
  
  extern dfsch_object_t* dfsch_hash_2_alist(dfsch_object_t* hash_obj);
  
#ifdef __cplusplus
}
#endif


#endif
