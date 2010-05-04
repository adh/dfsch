/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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

#define INITIAL_PACKAGE_SIZE 64
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

int dfsch_keyword_p(dfsch_object_t* obj){
  return DFSCH_SYMBOL_P(obj) && dfsch_symbol_package(obj) == DFSCH_KEYWORD_PACKAGE;
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
static pkg_hash_entry_t dfsch_internal_entries[INITIAL_PACKAGE_SIZE];
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
  .next = DFSCH_DFSCH_INTERNAL_PACKAGE,
  .name = "dfsch-user",
  .sym_count = 0,
  .mask = INITIAL_PACKAGE_MASK,
  .entries = dfsch_user_entries,
};
dfsch_package_t dfsch_dfsch_internal_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = DFSCH_DFSCH_PACKAGE,
  .name = "dfsch%internal",
  .sym_count = 0,
  .mask = INITIAL_PACKAGE_MASK,
  .entries = dfsch_internal_entries,
};
dfsch_package_t dfsch_keyword_package = {
  .type = DFSCH_PACKAGE_TYPE,
  .next = NULL,
  .name = "keyword",
  .sym_count = 0,
  .mask = INITIAL_PACKAGE_MASK,
  .entries = dfsch_keyword_entries,
};

static void package_write(dfsch_package_t* package, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)package, "%s %d", 
                         package->name, package->sym_count);
}
dfsch_type_t dfsch_package_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "package",
  .size = sizeof(dfsch_package_t),
  .write = (dfsch_type_write_t)package_write,
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
  {DFSCH_DFSCH_PACKAGE, "terminate-thread"},
  {DFSCH_DFSCH_PACKAGE, "use-value"},
  {DFSCH_DFSCH_PACKAGE, "*macro-expanded-from*"},
  {DFSCH_DFSCH_PACKAGE, "immutable-quasiquote"},
  {DFSCH_DFSCH_PACKAGE, "*compiled-from*"},
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


  return pkg;
}

dfsch_object_t* dfsch_make_package(char* name){
  dfsch_package_t* pkg;
  int i;

  pthread_mutex_lock(&symbol_lock);
  pkg = find_package(name);

  if (!pkg){
    pkg = (dfsch_package_t*)dfsch_make_object(DFSCH_PACKAGE_TYPE);
    pkg->name = dfsch_stracpy(name);
    pkg->next = packages;
    pkg->sym_count = 0;
    pkg->mask = INITIAL_PACKAGE_MASK;
    pkg->entries = GC_MALLOC_ATOMIC(sizeof(pkg_hash_entry_t)*INITIAL_PACKAGE_SIZE);
    for (i = 0; i <= INITIAL_PACKAGE_MASK; i++){
      pkg->entries[i].symbol = NULL;
      pkg->entries[i].hash = 0;
    }
    packages = pkg;
  }
  pthread_mutex_unlock(&symbol_lock);

  return (dfsch_object_t*)pkg;
}
void dfsch_use_package(dfsch_package_t* in,
                       dfsch_package_t* pkg){
  pthread_mutex_lock(&symbol_lock);
  if (!dfsch_member((dfsch_object_t*)pkg, in->use_list)){
    in->use_list = dfsch_cons((dfsch_object_t*)pkg, in->use_list);
  }
  pthread_mutex_unlock(&symbol_lock);
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
    tmp ^= ((size_t)c) ^ (tmp >> 11); 
    tmp *= c ^ (tmp << 7); 
    ++string;
  }

  tmp |= 0x80000000;

  return tmp; 
}

static void pkg_low_put_symbol(pkg_hash_entry_t* entries,
                               size_t mask,
                               dfsch__symbol_t* symbol,
                               size_t hash){
  size_t i;
  size_t initial_i;

  i = initial_i = hash & mask;

#ifdef DEBUG_PUT
  printf(";; package scan %08x %d ", hash, mask);
#endif
  do {
#ifdef DEBUG_PUT
    printf(" %d", i);
#endif
    if (!entries[i].hash || !entries[i].symbol || entries[i].symbol->package == NULL){
      entries[i].symbol = symbol;
      entries[i].hash = hash;
      if (GC_base(symbol) && GC_base(entries)){
        GC_general_register_disappearing_link(&(entries[i].symbol), symbol);
      }
#ifdef DEBUG_PUT
      printf(" Put\n");
#endif
      return;
    }
    i = (i + 1) & mask;
  } while (i != initial_i);

  fprintf(stderr, "Package full? WTF?\n");
  abort();
}

static void pkg_grow(dfsch_package_t* pkg){
  size_t new_count = 0;
  size_t new_mask = 7;
  pkg_hash_entry_t* new;
  size_t i;

  for (i = 0; i <= pkg->mask; i++){
    if (pkg->entries[i].hash && pkg->entries[i].symbol && 
        pkg->entries[i].symbol->package){
      new_count++;
    }
  }

  while (new_mask / 2 < new_count){
    new_mask = ((new_mask + 1) * 2) - 1;
  }

#ifdef DEBUG_GROW
  printf(";; %p %d/%d -> %d/%d\n", pkg, pkg->sym_count, pkg->mask, new_count, new_mask);
#endif

  new = GC_MALLOC_ATOMIC(sizeof(pkg_hash_entry_t) * (new_mask + 1));

  for (i = 0; i <= new_mask; i++){
    new[i].symbol = NULL;
    new[i].hash = 0;
  }

  for (i = 0; i <= pkg->mask; i++){
    if (pkg->entries[i].hash && pkg->entries[i].symbol && 
        pkg->entries[i].symbol->package){
      pkg_low_put_symbol(new, new_mask, 
                         pkg->entries[i].symbol, pkg->entries[i].hash);
    }
  }

  pkg->sym_count = new_count;
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
    if (!pkg->entries[i].hash){
      break;
    }

    if (pkg->entries[i].symbol &&
        pkg->entries[i].hash == hash && pkg->entries[i].symbol->package &&
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
    sym = pkg_find_symbol((dfsch_package_t*)DFSCH_FAST_CAR(i), name);
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
  dfsch_dfsch_internal_package.use_list = dfsch_list(1, DFSCH_DFSCH_PACKAGE);

  gsh_init = 1;
}

static symbol_t* intern_symbol(dfsch_package_t* package,
                               char* name){
  dfsch__symbol_t* sym;

  pthread_mutex_lock(&symbol_lock);
  gsh_check_init(); 
  // This code is slow already, so this check does not matter (too much)

  sym = find_symbol(package, name);
  if (!sym){
    sym = GC_NEW(dfsch__symbol_t);
    sym->name = dfsch_stracpy(name);
    sym->package = package;
    pkg_put_symbol(package, sym);
  }


  pthread_mutex_unlock(&symbol_lock);
  return sym;
}
static symbol_t* intern_symbol_in_package(dfsch_package_t* package,
                                          char* name){
  dfsch__symbol_t* sym;

  pthread_mutex_lock(&symbol_lock);
  gsh_check_init(); 
  // This code is slow already, so this check does not matter (too much)

  sym = pkg_find_symbol(package, name);
  if (!sym){
    sym = GC_NEW(dfsch__symbol_t);
    sym->name = dfsch_stracpy(name);
    sym->package = package;
    pkg_put_symbol(package, sym);
  }


  pthread_mutex_unlock(&symbol_lock);
  return sym;
}

dfsch_object_t* dfsch_gensym(){
  symbol_t *s = GC_NEW(symbol_t);

  s->package = NULL;
  s->name = NULL;

  return DFSCH_TAG_ENCODE(s, 2);
}
dfsch_object_t* dfsch_make_keyword(char* symbol){
  symbol_t *s;

  if (!symbol){
    return dfsch_gensym();
  }

  s = intern_symbol_in_package(DFSCH_KEYWORD_PACKAGE, symbol);
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
      if (!package){
        dfsch_error("No such package", dfsch_make_string_cstr(name));
      }
    } else {
      package = DFSCH_KEYWORD_PACKAGE;
    }
    s = intern_symbol_in_package(package, symbol_name);
  } else {
    s = intern_symbol(package, symbol_name);
  }
  return DFSCH_TAG_ENCODE(s, 2);  
}

dfsch_object_t* dfsch_make_symbol(char* symbol){
  return dfsch_intern_symbol(dfsch_get_current_package(), symbol);
}


char* dfsch_symbol(dfsch_object_t* symbol){
  symbol_t* s;
  s = DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                      DFSCH_SYMBOL_TYPE));
  if (!s->name){
    dfsch_error("Gensym has no name", symbol);
  }

  return s->name;
}
char* dfsch_symbol_qualified_name(dfsch_object_t* o){
  symbol_t* s;
  s = DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(o, 
                                      DFSCH_SYMBOL_TYPE));
  if (s->name){
    if (!s->package){
      dfsch_error("Uninterned symbol has no qualified name", o);
    } else {
      str_list_t* sl = sl_create();
      if (!dfsch_in_current_package(o)) {
        if (s->package != DFSCH_KEYWORD_PACKAGE) {
          sl_append(sl, s->package->name);
        }
        sl_append(sl, ":");      
        sl_append(sl, s->name);
        return sl_value(sl);
      } else {
        return s->name;
      }
    }
  } else {
    dfsch_error("Uninterned symbol has no qualified name", o);
  }
}

dfsch_package_t* dfsch_symbol_package(dfsch_object_t* symbol){
  return ((symbol_t*)DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                                     DFSCH_SYMBOL_TYPE)))->package;
}
char* dfsch_package_name(dfsch_object_t* package){
  dfsch_package_t* pkg = DFSCH_ASSERT_TYPE(package, DFSCH_PACKAGE_TYPE);
  return pkg->name;
}

int dfsch_interned_symbol_p(dfsch_object_t* sym){
  symbol_t* s = DFSCH_TAG_REF(sym);

  if (!DFSCH_SYMBOL_P(sym)){
    return 0;
  }
  
  return s->name && s->package;
}

dfsch_package_t* dfsch_package_designator(dfsch_object_t* obj){
  if (DFSCH_INSTANCE_P(obj, DFSCH_PACKAGE_TYPE)){
    return (dfsch_package_t*)obj;
  }
  return dfsch_find_package(dfsch_string_or_symbol_to_cstr(obj));
}

static int package_inherited(dfsch_package_t* to,
                             dfsch_package_t* from){
  dfsch_object_t* i = to->use_list;
  if (from == to){
    return 1;
  }

  while (DFSCH_PAIR_P(i)){
    if (DFSCH_FAST_CAR(i) == (dfsch_object_t*)from){
      return 1;
    }
    i = DFSCH_FAST_CDR(i);
  }
  
  return 0;
}

int dfsch_in_current_package(dfsch_object_t* symbol){
  int ret;
  symbol_t *s = DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                                DFSCH_SYMBOL_TYPE));
  
  if (s->package == dfsch_get_current_package()){
    return 1;
  }

  pthread_mutex_lock(&symbol_lock);

  ret = package_inherited(dfsch_get_current_package(), s->package);

  pthread_mutex_unlock(&symbol_lock);
  
  return ret;
}

dfsch_object_t* dfsch_list_all_packages(){
  dfsch_object_t* list = NULL;
  dfsch_package_t* i;

  pthread_mutex_lock(&symbol_lock);

  i = packages;

  while (i){
    list = dfsch_cons((dfsch_object_t*)i, list);
    i = i->next;
  }

  pthread_mutex_unlock(&symbol_lock);

  return list;
}
void dfsch_for_package_symbols(dfsch_package_t* pkg,
                               dfsch_package_iteration_cb_t cb,
                               void* baton){
  dfsch_object_t* list = NULL;
  size_t i;

  pthread_mutex_lock(&symbol_lock);

  for (i = 0; i <= pkg->mask; i++){
    if (pkg->entries[i].hash && pkg->entries[i].symbol && 
        pkg->entries[i].symbol->package){
      cb(baton, DFSCH_TAG_ENCODE(pkg->entries[i].symbol, 2));
    }
  }

  pthread_mutex_unlock(&symbol_lock);
  
  return;
}

void dfsch_for_all_package_symbols(dfsch_package_t* pkg,
                                   dfsch_package_iteration_cb_t cb,
                                   void* baton){
  dfsch_object_t* list = NULL;
  dfsch_object_t* j;
  size_t i;

  pthread_mutex_lock(&symbol_lock);

  for (i = 0; i <= pkg->mask; i++){
    if (pkg->entries[i].hash && pkg->entries[i].symbol && 
        pkg->entries[i].symbol->package){
      cb(baton, DFSCH_TAG_ENCODE(pkg->entries[i].symbol, 2));
    }
  }

  j = pkg->use_list;
  
  while (j){
    dfsch_package_t* p = (dfsch_package_t*)DFSCH_FAST_CAR(j);

    for (i = 0; i <= p->mask; i++){
      if (p->entries[i].hash && p->entries[i].symbol && 
          p->entries[i].symbol->package){
        cb(baton, DFSCH_TAG_ENCODE(p->entries[i].symbol, 2));
      }
    }

    j = DFSCH_FAST_CDR(j);
  }
  

  pthread_mutex_unlock(&symbol_lock);
  
  return;
}

static void cons_cb(dfsch_object_t** list,
                    dfsch_object_t* symbol){
  *list = dfsch_cons(symbol, *list);
}

dfsch_object_t* dfsch_list_package_symbols(dfsch_package_t* pkg){
  dfsch_object_t* list = NULL;
  dfsch_for_package_symbols(pkg, (dfsch_package_iteration_cb_t)cons_cb, 
                            &list);
  return list;
}
dfsch_object_t* dfsch_list_all_package_symbols(dfsch_package_t* pkg){
  dfsch_object_t* list = NULL;
  dfsch_for_all_package_symbols(pkg, (dfsch_package_iteration_cb_t)cons_cb, 
                                &list);
  return list;
}

void dfsch_unintern_symbol(dfsch_object_t* symbol){
  dfsch__symbol_t* sym = DFSCH_TAG_REF(DFSCH_ASSERT_TYPE(symbol, 
                                                         DFSCH_SYMBOL_TYPE));
  sym->package = NULL;
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
  dfsch_object_t* imports;
  dfsch_object_t* pkg;

  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_REST(args, imports);

  pkg = dfsch_make_package(name);

  while (DFSCH_PAIR_P(imports)){
    dfsch_use_package((dfsch_package_t*)pkg,
                      dfsch_package_designator(DFSCH_FAST_CAR(imports)));
    imports = DFSCH_FAST_CDR(imports);
  }

  return pkg;
}
DFSCH_DEFINE_PRIMITIVE(in_package, 
                       "Set current package to package of supplied name"){
  dfsch_package_t* package;

  DFSCH_PACKAGE_ARG(args, package);
  DFSCH_ARG_END(args);


  dfsch_set_current_package(package);

  return (dfsch_object_t*)package;
}
DFSCH_DEFINE_PRIMITIVE(use_package, 
                       "Set current package to package of supplied name"){
  dfsch_package_t* package;

  DFSCH_PACKAGE_ARG(args, package);
  DFSCH_ARG_END(args);

  dfsch_use_package(dfsch_get_current_package(), package);

  return (dfsch_object_t*)NULL;
}

DFSCH_DEFINE_PRIMITIVE(unintern, "Remove given symbol from it's package"){
  dfsch_object_t* symbol;
  DFSCH_OBJECT_ARG(args, symbol);
  DFSCH_ARG_END(args);

  dfsch_unintern_symbol(symbol);

  return symbol;
}

DFSCH_DEFINE_PRIMITIVE(find_package, 
                       "Return package with given name"){
  char* name;

  DFSCH_STRING_OR_SYMBOL_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_find_package(name);
}
DFSCH_DEFINE_PRIMITIVE(list_all_packages, 
                       "Return list of all registered packages"){
  DFSCH_ARG_END(args);

  return dfsch_list_all_packages();
}
DFSCH_DEFINE_PRIMITIVE(list_package_symbols, 
                       "Return list of all direct symbols in package"){
  dfsch_package_t* package;

  DFSCH_PACKAGE_ARG(args, package);
  DFSCH_ARG_END(args);

  return dfsch_list_package_symbols(package);
}
DFSCH_DEFINE_PRIMITIVE(list_all_package_symbols, 
                       "Return list of all symbols present package"){
  dfsch_package_t* package;

  DFSCH_PACKAGE_ARG(args, package);
  DFSCH_ARG_END(args);

  return dfsch_list_all_package_symbols(package);
}


void dfsch__package_register(dfsch_object_t *ctx){
  dfsch_defconst_cstr(ctx, "define-package",
                      DFSCH_PRIMITIVE_REF(define_package));
  dfsch_defconst_cstr(ctx, "in-package",
                      DFSCH_PRIMITIVE_REF(in_package));
  dfsch_defconst_cstr(ctx, "use-package",
                      DFSCH_PRIMITIVE_REF(use_package));

  dfsch_defconst_cstr(ctx, "unintern",
                      DFSCH_PRIMITIVE_REF(unintern));

  dfsch_defconst_cstr(ctx, "find-package",
                      DFSCH_PRIMITIVE_REF(find_package));
  dfsch_defconst_cstr(ctx, "list-all-packages",
                      DFSCH_PRIMITIVE_REF(list_all_packages));
  dfsch_defconst_cstr(ctx, "list-package-symbols",
                      DFSCH_PRIMITIVE_REF(list_package_symbols));
  dfsch_defconst_cstr(ctx, "list-all-package-symbols",
                      DFSCH_PRIMITIVE_REF(list_package_symbols));

}
