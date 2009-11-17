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

#define INITIAL_PACKAGE_SIZE 32
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
  dfsch_object_t* use_list;

  size_t sym_count;
  size_t mask;
  pkg_hash_entry_t* entries;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR;

static pkg_hash_entry_t dfsch_entries[INITIAL_PACKAGE_SIZE];
static pkg_hash_entry_t dfsch_user_entries[INITIAL_PACKAGE_SIZE];
static pkg_hash_entry_t dfsch_keyword_entries[INITIAL_PACKAGE_SIZE];

dfsch_package_t dfsch_dfsch_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_KEYWORD_PACKAGE,
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
  .next = NULL,
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
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "package",
  .size = sizeof(dfsch_package_t)
};

static pthread_mutex_t symbol_lock = PTHREAD_MUTEX_INITIALIZER;
static dfsch_package_t* packages = DFSCH_DFSCH_USER_PACKAGE;
static dfsch_package_t* current_package = DFSCH_DFSCH_USER_PACKAGE;
static int gsh_init = 0;
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

static dfsch_package_t* find_package(char* name){
  dfsch_package_t* i = packages;

  while (i){
    if (strcmp(i->name, name) == 0){
      return i;
    }
    i = i->next;
  }

  return NULL;
}


dfsch_package_t* dfsch_find_package(char* name){
  dfsch_package_t* pkg;

  pthread_mutex_lock(&symbol_lock);
  pkg = find_package(name);
  pthread_mutex_unlock(&symbol_lock);

  if (!pkg){
    dfsch_error("No such package", dfsch_make_string_cstr(name));
  }

  return pkg;
}

dfsch_object_t* dfsch_make_package(char* name){
  dfsch_package_t* pkg;

  pthread_mutex_lock(&symbol_lock);
  pkg = find_package(name);

  if (!pkg){
    pkg = dfsch_make_object(DFSCH_PACKAGE_TYPE);
    pkg->name = dfsch_stracpy(name);
    pkg->next = packages;
    pkg->sym_count = 0;
    pkg->mask = INITIAL_PACKAGE_MASK;
    pkg->entries = GC_MALLOC(sizeof(pkg_hash_entry_t)*INITIAL_PACKAGE_SIZE);
    packages = pkg;
  }
  pthread_mutex_unlock(&symbol_lock);

  return pkg;
  
}



dfsch_package_t* dfsch_get_current_package(){
  return current_package;
}

void dfsch_set_current_package(dfsch_package_t* package){
  current_package = package;
}

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

static dfsch__symbol_t* find_symbol(dfsch_package_t* pkg,
                                    char* name){
  dfsch__symbol_t* sym = pkg_find_symbol(pkg, name);
  dfsch_object_t* i = pkg->use_list;

  if (sym){
    return sym;
  }
  
  while (DFSCH_PAIR_P(i)){
    sym = find_symbol(DFSCH_FAST_CAR(i), name);
    if (sym){
      return sym;
    }
    i = DFSCH_FAST_CDR(i);
  }

  return NULL;
}


static void gsh_check_init(){
  int i;
  if (gsh_init){
    return;
  }

  for (i = 0; i < sizeof(dfsch__static_symbols)/sizeof(symbol_t); i++){
    pkg_put_symbol(dfsch__static_symbols[i].package,
                   dfsch__static_symbols + i);
  }
  
  dfsch_dfsch_user_package.use_list = dfsch_list(1, DFSCH_DFSCH_PACKAGE);

  gsh_init = 1;
}

static symbol_t* intern_symbol_in_package(dfsch_package_t* package,
                                          char* name){
  dfsch__symbol_t* sym = find_symbol(package, name);
  if (!sym){
    sym = GC_NEW(dfsch__symbol_t);
    sym->name = dfsch_stracpy(name);
    sym->package = package;
    pkg_put_symbol(package, sym);
  }
  return sym;
}

dfsch_object_t* dfsch_gensym(){
  symbol_t *s = GC_NEW(symbol_t);

  s->package = DFSCH_GENSYM_PACKAGE;
  s->name = NULL;

  return DFSCH_TAG_ENCODE(s, 2);
}
dfsch_object_t* dfsch_make_keyword(char* symbol){
  symbol_t *s;

  if (!symbol){
    return dfsch_gensym();
  }

  pthread_mutex_lock(&symbol_lock);

  gsh_check_init(); 
  // This code is slow already, so this check does not matter (too much)

  s = intern_symbol_in_package(DFSCH_KEYWORD_PACKAGE, symbol);


  pthread_mutex_unlock(&symbol_lock);
  return DFSCH_TAG_ENCODE(s, 2);
  
}

static void parse_symbol(char* symbol,
                         char** package_name,
                         char** symbol_name){
  char* colon = strrchr(symbol, ':');

  if (!colon){
    *symbol_name = dfsch_stracpy(symbol);
    *package_name = NULL;
  } else if (colon == symbol){
    *symbol_name = dfsch_stracpy(symbol+1);
    *package_name = "";
  } else {
    *symbol_name = dfsch_stracpy(colon+1);
    *package_name = dfsch_strancpy(symbol, colon - symbol);
  }
  
}

dfsch_object_t* dfsch_intern_symbol(dfsch_package_t* package,
                                    char* name){
  symbol_t *s;
  char* package_name;
  char* symbol_name;

  if (!name){
    return dfsch_gensym();
  }

  parse_symbol(name, &package_name, &symbol_name);

  if (package_name){
    if (*package_name){
      package = dfsch_find_package(package_name);
    } else {
      package = DFSCH_KEYWORD_PACKAGE;
    }
  }

  pthread_mutex_lock(&symbol_lock);

  gsh_check_init(); 
  // This code is slow already, so this check does not matter (too much)


  s = intern_symbol_in_package(package, symbol_name);


  pthread_mutex_unlock(&symbol_lock);
  return DFSCH_TAG_ENCODE(s, 2);
  
}

dfsch_object_t* dfsch_make_symbol(char* symbol){
  return dfsch_intern_symbol(dfsch_get_current_package(), symbol);
}


char* dfsch_symbol(dfsch_object_t* symbol){
  return ((symbol_t*)DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                                     DFSCH_SYMBOL_TYPE)))->name;
}
char* dfsch_package_name(dfsch_object_t* package){
  dfsch_package_t* pkg = DFSCH_ASSERT_TYPE(package, DFSCH_PACKAGE_TYPE);
  return pkg->name;
}

dfsch_object_t* dfsch_symbol_2_keyword(dfsch_object_t* sym){
  return DFSCH_TAG_ENCODE(intern_symbol_in_package(DFSCH_KEYWORD_PACKAGE,
                                                   dfsch_symbol(sym)),
                          2);
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
                         dfsch_package_t* package,
                         char* name){
  dfsch__symbol_t* sym = DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                                         DFSCH_SYMBOL_TYPE));

  if (sym->package != package){
    return 0;
  }

  return (strcmp(name, sym->name) == 0);

}

int dfsch_compare_keyword(dfsch_object_t* symbol,
                          char* name){
  return dfsch_compare_symbol(symbol, DFSCH_KEYWORD_PACKAGE, name);
}

dfsch_object_t* dfsch_bool(int bool){
  return bool ? DFSCH_SYM_TRUE : NULL;
}


DFSCH_DEFINE_PRIMITIVE(define_package, 
                       "Define new symbol package with given name if it does "
                       "not already exist"){
  char* name;

  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_make_package(name);
}
DFSCH_DEFINE_PRIMITIVE(in_package, 
                       "Set current package to package of supplied name"){
  char* name;
  dfsch_package_t* pkg;

  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  pkg = dfsch_find_package(name);

  dfsch_set_current_package(pkg);

  return pkg;
}
void dfsch__package_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "define-package",
                    DFSCH_PRIMITIVE_REF(define_package));
  dfsch_define_cstr(ctx, "in-package",
                    DFSCH_PRIMITIVE_REF(in_package));
}
