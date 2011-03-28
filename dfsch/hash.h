/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Hash tables
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

/** \file dfsch/hash.h
 *
 * Hash table datatype.
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

  typedef struct dfsch_hash_t dfsch_hash_t;

  /**
   * Create new hash table object
   */
  extern dfsch_object_t* dfsch_make_hash();
  extern dfsch_object_t* dfsch_make_idhash();

  /**
   * Get given entry in hashtable.
   *
   * returns DFSCH_INVALID_OBJECT when not found.
   */
  extern dfsch_object_t* dfsch_hash_ref(dfsch_hash_t* hash, 
                                        dfsch_object_t* key);
  extern dfsch_object_t* dfsch_idhash_ref(dfsch_hash_t* hash, 
                                          dfsch_object_t* key);

  /**
   * Associate value with key
   */
  extern void dfsch_hash_set(dfsch_hash_t* hash,
                             dfsch_object_t* key,
                             dfsch_object_t* value);
  extern void dfsch_idhash_set(dfsch_hash_t* hash,
                               dfsch_object_t* key,
                               dfsch_object_t* value);

  /**
   * Delete given key.
   */
  extern int dfsch_hash_unset(dfsch_hash_t* hash,
                              dfsch_object_t* key);
  extern int dfsch_idhash_unset(dfsch_hash_t* hash,
                                dfsch_object_t* key);
  
  /**
   * Set value associated with given key only when there is already such 
   * key.
   */
  extern int dfsch_hash_set_if_exists(dfsch_hash_t* hash,
                                      dfsch_object_t* key,
                                      dfsch_object_t* value);
  extern int dfsch_idhash_set_if_exists(dfsch_hash_t* hash,
                                        dfsch_object_t* key,
                                        dfsch_object_t* value);

  extern int dfsch_hash_set_if_not_exists(dfsch_hash_t* hash,
                                          dfsch_object_t* key,
                                          dfsch_object_t* value);
  extern int dfsch_idhash_set_if_not_exists(dfsch_hash_t* hash,
                                            dfsch_object_t* key,
                                            dfsch_object_t* value);
  
  /**
   * Convert hash table to list of associations.
   */
  extern dfsch_object_t* dfsch_hash_2_alist(dfsch_hash_t* hash);

  /**
   * Convert list of associations to hash table.
   */
  extern dfsch_object_t* dfsch_alist_2_hash(dfsch_object_t* alist);
  extern dfsch_object_t* dfsch_alist_2_idhash(dfsch_object_t* alist);
  

  extern dfsch_type_t dfsch_hash_table_type;
#define DFSCH_HASH_TABLE_TYPE (&dfsch_hash_table_type)
  extern dfsch_type_t dfsch_identity_hash_table_type;
#define DFSCH_IDENTITY_HASH_TABLE_TYPE (&dfsch_identity_hash_table_type)


  extern dfsch_type_t dfsch_hash_items_iterator_type;
#define DFSCH_HASH_ITEMS_ITERATOR_TYPE (&dfsch_hash_items_iterator_type)
  extern dfsch_type_t dfsch_hash_keys_iterator_type;
#define DFSCH_HASH_KEYS_ITERATOR_TYPE (&dfsch_hash_keys_iterator_type)
  extern dfsch_type_t dfsch_hash_values_iterator_type;
#define DFSCH_HASH_VALUES_ITERATOR_TYPE (&dfsch_hash_values_iterator_type)


#ifdef __cplusplus
}
#endif


#endif
