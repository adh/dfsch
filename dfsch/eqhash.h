/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Internal identity hash tables
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

/*
 * This is special hash implementation. Intersting features:
 *
 * * It is inline object
 * * Supports only eq? keys
 * * Switches implementation between associative array and open hashing
 * * Object itself is pretty large
 *
 * Used in internal representation of environments and similar places, most
 * normal user code probably does not need this.
 * 
 * Both this and normal hash-table (hash.h) is optimized for negative lookups.
 */

#ifndef H__dfsch__eqhash__
#define H__dfsch__eqhash__

#include <dfsch/dfsch.h>

#define DFSCH_EQHASH_SMALL_SIZE 4
#define DFSCH_EQHASH_CACHE_SIZE 8

#define DFSCH_EQHASH_LARGE 1

typedef struct dfsch_eqhash_entry_t dfsch_eqhash_entry_t;

struct dfsch_eqhash_entry_t {
  dfsch_object_t* key;
  dfsch_eqhash_entry_t* next;
  dfsch_object_t* value;
  unsigned short flags;
};

typedef struct dfsch__eqhash_small_t {
  dfsch_object_t* keys[DFSCH_EQHASH_SMALL_SIZE];
  dfsch_object_t* values[DFSCH_EQHASH_SMALL_SIZE];
  short flags[DFSCH_EQHASH_SMALL_SIZE];
} dfsch__eqhash_small_t;

typedef struct dfsch__eqhash_large_t {
  dfsch_eqhash_entry_t* cache[DFSCH_EQHASH_CACHE_SIZE];
  dfsch_eqhash_entry_t* vector;
  size_t mask;
  size_t count;
} dfsch__eqhash_large_t;

#undef small
#undef large

typedef union dfsch__eqhash_contents_t {
    dfsch__eqhash_small_t small;
    dfsch__eqhash_large_t large;
} dfsch__eqhash_contents_t;

typedef struct dfsch_eqhash_t {
  int is_large; 
  /*this can be changed to char or bitfield or whatever when necessary*/
  dfsch__eqhash_contents_t contents;
} dfsch_eqhash_t; 

void dfsch_eqhash_init(dfsch_eqhash_t* hash, int start_large);
void dfsch_eqhash_put(dfsch_eqhash_t* hash,
                      dfsch_object_t* key, dfsch_object_t* value) DFSCH_FUNC_HOT;
void dfsch_eqhash_set(dfsch_eqhash_t* hash,
                      dfsch_object_t* key, dfsch_object_t* value) DFSCH_FUNC_HOT;
int dfsch_eqhash_unset(dfsch_eqhash_t* hash, dfsch_object_t* key);
void dfsch_eqhash_set_flags(dfsch_eqhash_t* hash,
                            dfsch_object_t* key, unsigned short flags);
int dfsch_eqhash_set_if_exists(dfsch_eqhash_t* hash,
                               dfsch_object_t* key, dfsch_object_t* value,
                               unsigned short* flags);
dfsch_object_t* dfsch_eqhash_ref(dfsch_eqhash_t* hash,
                                 dfsch_object_t* key) DFSCH_FUNC_HOT;
int dfsch_eqhash_ref_ex(dfsch_eqhash_t* hash,
                        dfsch_object_t* key, 
                        dfsch_object_t** value, 
                        unsigned short *flags, 
                        dfsch_eqhash_entry_t** entry);

dfsch_object_t* dfsch_eqhash_2_alist(dfsch_eqhash_t* hash);
dfsch_object_t* dfsch_eqhash_revscan(dfsch_eqhash_t* hash,
                                     dfsch_object_t* value,
                                     unsigned short flags);
dfsch_eqhash_entry_t* dfsch_eqhash_2_entry_list(dfsch_eqhash_t* hash);

int dfsch_eqhash_empty_p(dfsch_eqhash_t* hash);

#endif
