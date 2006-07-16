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

#define DFSCH_HASH_EQ 0
#define DFSCH_HASH_EQV 1
#define DFSCH_HASH_EQUAL 2

  /**
   * Create new hash table object
   */
  extern dfsch_object_t* dfsch_hash_make(dfsch_object_t* hash_proc, 
                                         int mode);

  /**
   * Check whenever given object is hashtable
   */
  extern int dfsch_hash_p(dfsch_object_t* obj);

  /**
   * Get given entry in hashtable.
   *
   * Return value is pair, whose CAR is value associated with key
   * or empty list in case of non-existant key.
   */
  extern dfsch_object_t* dfsch_hash_ref(dfsch_object_t* hash, 
                                        dfsch_object_t* key);

  /**
   * Associate value with key
   */
  extern dfsch_object_t* dfsch_hash_set(dfsch_object_t* hash,
                                        dfsch_object_t* key,
                                        dfsch_object_t* value);

  /**
   * Delete given key.
   */
  extern dfsch_object_t* dfsch_hash_unset(dfsch_object_t* hash,
                                          dfsch_object_t* key);

  /**
   * Set value associated with given key only when there is already such 
   * key.
   */
  extern dfsch_object_t* dfsch_hash_set_if_exists(dfsch_object_t* hash,
                                                  dfsch_object_t* key,
                                                  dfsch_object_t* value);
  
  /**
   * Convert hash table to list of associations.
   */
  extern dfsch_object_t* dfsch_hash_2_alist(dfsch_object_t* hash_obj);

  /**
   * Convert list of associations to hash table.
   */
  extern dfsch_object_t* dfsch_alist_2_hash(dfsch_object_t* alist,
                                            dfsch_object_t* hash_proc, 
                                            int mode);
  
#ifdef __cplusplus
}
#endif


#endif
