/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Internal identity hash tables
 * Copyright (C) 2009 Ales Hakl
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

#include <dfsch/eqhash.h>

#define INITIAL_MASK 0x07

void dfsch_eqhash_init(dfsch_eqhash_t* hash, int start_large){
  int i;
  if (start_large){
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE * 2; i++){
      hash->contents.large.cache[i] = NULL;
    }
    hash->contents.large.mask = INITIAL_MASK;
    hash->contents.large.count = 0;
  } else {
    for (i = 0; i < DFSCH_EQHASH_SMALL_SIZE; i++){
      hash->contents.small.keys[i] = DFSCH_INVALID_OBJECT;
      hash->contents.small.values[i] = NULL;
      hash->contents.small.flags[i] = 0;
    }
  }
  hash->is_large = start_large;
}

static void convert_to_large(dfsch_eqhash_t* hash){
  
}

void dfsch_eqhash_put(dfsch_eqhash_t* hash,
                      dfsch_object_t* key, dfsch_object_t* value){

}
void dfsch_eqhash_set(dfsch_eqhash_t* hash,
                      dfsch_object_t* key, dfsch_object_t* value){

}
void dfsch_eqhash_set_flags(dfsch_eqhash_t* hash,
                            dfsch_object_t* key, long flags){

}
int dfsch_eqhash_ref(dfsch_eqhash_t* hash,
                     dfsch_object_t* key, 
                     dfsch_object_t** value, long *flags,
                     dfsch_eqhash_entry_t** entry){
  
}
