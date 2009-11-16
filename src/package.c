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

#define HASH_BITS 10
#define HASH_SIZE (1 << HASH_BITS)

typedef struct hash_entry_t hash_entry_t;
struct hash_entry_t {
  symbol_t* entry;
  size_t hash;
  hash_entry_t* next;
};

int dfsch_symbol_p(dfsch_object_t* obj){
  return DFSCH_SYMBOL_P(obj);
}

static size_t string_hash(char* string){
  size_t tmp=0;

  while (*string){
    char c = *string; 
    tmp ^= c ^ (tmp << 7); 
    tmp ^= ((size_t)c << 17) ^ (tmp >> 11); 
    ++string;
  }

  return tmp & (HASH_SIZE - 1); 
}

struct dfsch_package_t {
  dfsch_type_t* type;
  dfsch_package_t* next;
  char* name;
};

dfsch_package_t dfsch_dfsch_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = NULL,
  .name = "dfsch"
};
dfsch_package_t dfsch_dfsch_user_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_DFSCH_PACKAGE,
  .name = "dfsch-user"
};

dfsch_package_t dfsch_gensym_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_GENSYM_PACKAGE,
  .name = "*gensym*"
};

dfsch_type_t dfsch_package_type = {
  .type = DFSCH_STANDARD_TYPE
};

static hash_entry_t*  global_symbol_hash[HASH_SIZE];
static unsigned int gsh_init = 0;
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
  {NULL, "before"},
  {NULL, "after"},
  {NULL, "around"},
};

/*
 * It's possible to use rwlock here (althought such solution is not so 
 * straightforward), but it seem unnecessary - most symbol creations are 
 * done when reading source and in such case there will be probably only
 * one thread doing such things.
 */

static void register_static_symbol(symbol_t* s){
  hash_entry_t *e = malloc(sizeof(hash_entry_t));

  e->entry = s;
  e->hash = string_hash(s->name);

  e->next = global_symbol_hash[e->hash];
  global_symbol_hash[e->hash] = e;
}

static void gsh_check_init(){
  int i;
  if (gsh_init){
    return;
  }

  memset(global_symbol_hash, 0, sizeof(hash_entry_t*)*HASH_SIZE);
  for (i = 0; i < sizeof(dfsch__static_symbols)/sizeof(symbol_t); i++){
    register_static_symbol(dfsch__static_symbols + i);
  }
  gsh_init = 1;
}

static symbol_t* lookup_symbol(char *symbol){

  size_t hash = string_hash(symbol);
  hash_entry_t *i = global_symbol_hash[hash];

  while (i){
    if (i->hash == hash && strcmp(i->entry->name, symbol)==0){
      return i->entry;
    }
    i = i->next;
  }

  return NULL;
}

static void free_symbol(symbol_t* s){
  hash_entry_t *i;
  hash_entry_t *j;

  pthread_mutex_lock(&symbol_lock);

  i = global_symbol_hash[string_hash(s->name)];
  j = NULL;
  
  while (i){
    if (i->entry == s){
      if (j){
        j->next = i->next;
      } else {
        global_symbol_hash[string_hash(s->name)] = i->next;
      }
      free(i);
      break;
    }
    j = i;
    i = i->next;
  }

  pthread_mutex_unlock(&symbol_lock);

  s->name = NULL;
}

static void symbol_finalizer(symbol_t* symbol, void* cd){
  free_symbol(symbol);
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


  GC_REGISTER_FINALIZER(s, 
                        (GC_finalization_proc)symbol_finalizer, NULL, 
                        NULL, NULL);
    
  hash_entry_t *e = malloc(sizeof(hash_entry_t));

  e->entry = s;
  e->hash = string_hash(symbol);

  e->next = global_symbol_hash[e->hash];
  global_symbol_hash[e->hash] = e;

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

struct dfsch_symbol_iter_t{
  hash_entry_t* item;
  size_t bucket;
};

char* dfsch_get_next_symbol(dfsch_symbol_iter_t **iter){ // deep magic
  if (*iter == NULL){
    *iter = GC_MALLOC(sizeof(dfsch_symbol_iter_t));
    (*iter)->bucket = 0;
    (*iter)->item = global_symbol_hash[(*iter)->bucket];
  }
  while ((*iter)->bucket < HASH_SIZE){
    if (!(*iter)->item){
      (*iter)->bucket ++;
      (*iter)->item = global_symbol_hash[(*iter)->bucket];
    }else{
      hash_entry_t *i = (*iter)->item;
      (*iter)->item = (*iter)->item->next;
      return i->entry->name;
    }
  }  
  return NULL;
}

