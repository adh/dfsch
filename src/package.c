/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>
#include <dfsch/number.h>
#include <dfsch/parse.h>
#include <dfsch/strings.h>
#include <dfsch/magic.h>
#include <dfsch/conditions.h>
#include <dfsch/introspect.h>
#include <dfsch/weak.h>
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include "types.h"

// Symbols

#define INITIAL_PACKAGE_SIZE 128
#define INITIAL_PACKAGE_MASK (INITIAL_PACKAGE_SIZE - 1)

typedef struct hash_entry_t hash_entry_t;
struct hash_entry_t {
  symbol_t* entry;
  size_t hash;
  hash_entry_t* next;
};

int dfsch_symbol_p(dfsch_object_t* obj){
  return DFSCH_SYMBOL_P(obj);
}

typedef struct pkg_hash_entry_t {
  size_t hash;
  dfsch__symbol_t* symbol;
} pkg_hash_entry_t;

struct dfsch_package_t {
  dfsch_type_t* type;
  dfsch_package_t* next;
  char* name;
  size_t sym_count;
  size_t mask;
  pkg_hash_entry_t* entries;
};

static pkg_hash_entry_t dfsch_entries[INITIAL_PACKAGE_SIZE];
static pkg_hash_entry_t dfsch_user_entries[INITIAL_PACKAGE_SIZE];
static pkg_hash_entry_t dfsch_keyword_entries[INITIAL_PACKAGE_SIZE];

dfsch_package_t dfsch_dfsch_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = NULL,
  .name = "dfsch",
  .sym_count = 0,
  .mask = INITIAL_PACKAGE_MASK,
  .entries = dfsch_entries,
};
dfsch_package_t dfsch_dfsch_user_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_DFSCH_PACKAGE,
  .name = "dfsch-user",
  .sym_count = 0,
  .mask = INITIAL_PACKAGE_MASK,
  .entries = dfsch_user_entries,
};
dfsch_package_t dfsch_keyword_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_DFSCH_PACKAGE,
  .name = "keyword",
  .sym_count = 0,
  .mask = INITIAL_PACKAGE_MASK,
  .entries = dfsch_keyword_entries,
};

dfsch_package_t dfsch_gensym_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_GENSYM_PACKAGE,
  .name = "*gensym*"
};

dfsch_type_t dfsch_package_type = {
  .type = DFSCH_STANDARD_TYPE
};

static size_t symbol_hash(char* string){
  size_t tmp=0;

  while (*string){
    char c = *string; 
    tmp *= c ^ (tmp << 7); 
    tmp ^= ((size_t)c << 17) ^ (tmp >> 11); 
    ++string;
  }

  return tmp; 
}

static void pkg_low_put_symbol(pkg_hash_entry_t* entries,
                               size_t mask,
                               dfsch__symbol_t* symbol,
                               size_t hash){
  size_t i;
  size_t initial_i;

  i = initial_i = hash & mask;

  do {
    if (!entries[i].symbol){
      entries[i].symbol = symbol;
      entries[i].hash = hash;
      return;
    }
    i = (i + 1) & mask;
  } while (i != initial_i);

  abort();
}

static void pkg_grow(dfsch_package_t* pkg){
  size_t new_mask = ((pkg->mask + 1) * 2) - 1;
  pkg_hash_entry_t* new = GC_MALLOC(sizeof(pkg_hash_entry_t) * (new_mask + 1));
  size_t i;

  for (i = 0; i <= pkg->mask; i++){
    if (pkg->entries[i].symbol){
      pkg_low_put_symbol(new, new_mask, 
                         pkg->entries[i].symbol, pkg->entries[i].hash);
    }
  }

  pkg->mask = new_mask;
  pkg->entries = new;
}

static void pkg_put_symbol(dfsch_package_t* pkg,
                           dfsch__symbol_t* symbol){
  pkg->sym_count++;
  
  if (pkg->sym_count / 2 > pkg->mask / 3){
    pkg_grow(pkg);
  }

  pkg_low_put_symbol(pkg->entries, pkg->mask, 
                     symbol, symbol_hash(symbol->name));
}
static dfsch__symbol_t* pkg_find_symbol(dfsch_package_t* pkg,
                                        char* name){
  size_t i;
  size_t initial_i;
  size_t hash = symbol_hash(name);

  i = initial_i = hash & pkg->mask;

  do {
    if (!pkg->entries[i].symbol){
      break;
    }

    if (pkg->entries[i].hash == hash &&
        strcmp(pkg->entries[i].symbol->name, name) == 0){
      return pkg->entries[i].symbol;
    }

    i = (i + 1) & pkg->mask;
  } while (i != initial_i);

  return NULL;
}


static pthread_mutex_t symbol_lock = PTHREAD_MUTEX_INITIALIZER;
dfsch__symbol_t dfsch__static_symbols[] = {
  {DFSCH_DFSCH_PACKAGE, "true"},
  {DFSCH_DFSCH_PACKAGE, "quote"},
  {DFSCH_DFSCH_PACKAGE, "quasiquote"},
  {DFSCH_DFSCH_PACKAGE, "unquote"},
  {DFSCH_DFSCH_PACKAGE, "unquote-splicing"},
  {DFSCH_DFSCH_PACKAGE, "else"},
  {DFSCH_DFSCH_PACKAGE, "=>"},
  {DFSCH_DFSCH_PACKAGE, "&optional"},
  {DFSCH_DFSCH_PACKAGE, "&key"},
  {DFSCH_DFSCH_PACKAGE, "&rest"},
  {DFSCH_DFSCH_PACKAGE, "&body"},
  {DFSCH_DFSCH_PACKAGE, "&allow-other-keys"},
  {DFSCH_DFSCH_PACKAGE, "&environment"},
  {DFSCH_DFSCH_PACKAGE, "&whole"},
  {DFSCH_DFSCH_PACKAGE, "&aux"},
  {DFSCH_KEYWORD_PACKAGE, "before"},
  {DFSCH_KEYWORD_PACKAGE, "after"},
  {DFSCH_KEYWORD_PACKAGE, "around"},
};

/*
 * It's possible to use rwlock here (althought such solution is not so 
 * straightforward), but it seem unnecessary - most symbol creations are 
 * done when reading source and in such case there will be probably only
 * one thread doing such things.
 */

static int gsh_init = 0;

static void gsh_check_init(){
  int i;
  if (gsh_init){
    return;
  }

  for (i = 0; i < sizeof(dfsch__static_symbols)/sizeof(symbol_t); i++){
    pkg_put_symbol(dfsch__static_symbols[i].package,
                   dfsch__static_symbols + i);
  }
  gsh_init = 1;
}

static symbol_t* lookup_symbol(char *symbol){

  return pkg_find_symbol(DFSCH_DFSCH_PACKAGE, symbol);
}

static symbol_t* make_symbol(char *symbol){
  symbol_t *s;
  symbol_t *f;

  s = GC_NEW(symbol_t); /* !!! free_symbol could be called by this */
  if (*symbol == ':'){
    s->name = stracpy(symbol+1);
    s->package = NULL;
  } else {
    s->name = stracpy(symbol);
    s->package = DFSCH_DFSCH_PACKAGE;
  }

  pthread_mutex_lock(&symbol_lock);

  f = lookup_symbol(symbol); 
  if (f){ 
    GC_FREE(s->name);
    GC_FREE(s);
    pthread_mutex_unlock(&symbol_lock);
    return f;
  }


  pkg_put_symbol(DFSCH_DFSCH_PACKAGE, s);
  pthread_mutex_unlock(&symbol_lock);
    
  
  return s;
}

dfsch_object_t* dfsch_gensym(){
  symbol_t *s = GC_NEW(symbol_t);

  s->package = DFSCH_GENSYM_PACKAGE;
  s->name = NULL;

  return DFSCH_TAG_ENCODE(s, 2);
}

dfsch_object_t* dfsch_make_symbol(char* symbol){

  symbol_t *s;

  if (!symbol){
    return dfsch_gensym();
  }

  pthread_mutex_lock(&symbol_lock);

  gsh_check_init(); 
  // This code is slow already, so this check does not matter (too much)

  s = lookup_symbol(symbol);

  pthread_mutex_unlock(&symbol_lock);

  if (!s)
    s = make_symbol(symbol);

  return DFSCH_TAG_ENCODE(s, 2);

}
char* dfsch_symbol(dfsch_object_t* symbol){
  return ((symbol_t*)DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                                     DFSCH_SYMBOL_TYPE)))->name;
}

char* dfsch_symbol_2_typename(dfsch_object_t* symbol){
  char *name;
  char *flt;

  name = dfsch_symbol(symbol);

  if (name[strlen(name)-1] == '>'){
    flt = strchr(name, '<');
    if (flt){
      if (flt == name){
        return strancpy(name+1, strlen(name) - 2);
      } else {
        return strancat(name, flt - name, flt+1, strlen(flt+1)-1);
      }
    }
  }

  return name;
}



int dfsch_compare_symbol(dfsch_object_t* symbol,
                         char* string){
  return (ascii_strcasecmp(string, dfsch_symbol(symbol)) == 0);
}

dfsch_object_t* dfsch_bool(int bool){
  return bool ? DFSCH_SYM_TRUE : NULL;
}



