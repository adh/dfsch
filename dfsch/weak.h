/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Weak references
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


#ifndef H__dfsch__weak__
#define H__dfsch__weak__

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_make_weak_reference(dfsch_object_t* refered);
  
  extern int dfsch_weak_reference_live_p(dfsch_object_t* reference);
  extern dfsch_object_t* dfsch_weak_reference_dereference(dfsch_object_t* reference);

  extern dfsch_object_t* dfsch_make_weak_vector(size_t length, 
                                                dfsch_object_t* fill);
  extern size_t dfsch_weak_vector_length(dfsch_object_t *vector);
  extern dfsch_object_t** dfsch_weak_vector_as_array(dfsch_object_t *vector, 
                                                     size_t *length);
  extern dfsch_object_t* dfsch_weak_vector_from_array(dfsch_object_t **array, 
                                                      size_t length);
  extern dfsch_object_t* dfsch_weak_vector_ref(dfsch_object_t *vector, 
                                               size_t k);
  extern dfsch_object_t* dfsch_weak_vector_set(dfsch_object_t* vector, 
                                               size_t k, 
                                               dfsch_object_t* obj);

  extern dfsch_type_t dfsch_weak_reference_type;
#define DFSCH_WEAK_REFERENCE_TYPE (&dfsch_weak_reference_type)
  extern dfsch_type_t dfsch_weak_vector_type;
#define DFSCH_WEAK_VECTOR_TYPE (&dfsch_weak_vector_type)
  extern dfsch_type_t dfsch_weak_key_hash_type;
#define DFSCH_WEAK_KEY_HASH_TYPE ((dfsch_type_t*)&dfsch_weak_key_hash_type)

#ifdef __cplusplus
}
#endif

#endif
