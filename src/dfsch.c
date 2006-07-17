/*
 * dfsch - dfox's quick and dirty scheme implementation
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

/** @file dfsch.c This is implementation of dfsch interpreter. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>
#include <dfsch/number.h>
#include <dfsch/parse.h>
#include <dfsch/strings.h>
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <setjmp.h>

#include "object.h"

//#define ALLOC_DEBUG

#ifdef ALLOC_DEBUG
static int obj_count = 0;
static int obj_size = 0;
#endif

object_t* dfsch_make_object(const dfsch_type_t* type){
  object_t* o = GC_MALLOC(type->size);
  if (!o)
    return NULL;

  o->type = (dfsch_type_t*)type;

#ifdef ALLOC_DEBUG
  obj_count ++;
  obj_size += type->size;
  printf(";; Alloc'd: #<%s 0x%x> serial %d arena %d\n", type->name, o, 
         obj_count, obj_size);
#endif

  return o;
}

int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b){
  return (a==b);
}

int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if ((a->type == b->type) && dfsch_number_p(a))
    return dfsch__number_eqv_p(a,b);

  return 0;
}

int dfsch_equal_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a || !b)
    return 0;

  if (a->type != b->type)
    return 0;

  if (!a->type)
    return 0;
  if (!a->type->equal_p)
    return 0;

  return a->type->equal_p(a,b);
}

static int pair_equal_p(pair_t*, pair_t*);
static char* pair_write(pair_t*, int, int);

static const dfsch_type_t pair_type = {
  sizeof(pair_t), 
  "pair",
  (dfsch_type_equal_p_t)pair_equal_p,
  (dfsch_type_write_t)pair_write
};
#define PAIR (&pair_type)

static int pair_equal_p(pair_t*a, pair_t*b){
  return dfsch_equal_p(a->car,b->car) && dfsch_equal_p(a->cdr, b->cdr);
}
static char* pair_write(pair_t*p, int max_depth, int readable){
  if (p->cdr && p->cdr->type!=PAIR){
    
    str_list_t* l = sl_create();
    
    sl_append(l, "(");
    sl_append(l, dfsch_obj_write(p->car, max_depth-1, readable));

    sl_append(l," . ");
    sl_append(l, dfsch_obj_write(p->cdr, max_depth-1, readable));
    sl_append(l, ")");
    
    return sl_value(l);
  }
  
  str_list_t* l = sl_create();
  pair_t* i=(pair_t*)p;
  pair_t* j=(pair_t*)p;
  int c = 0;
    
  sl_append(l,"(");
    
  while (i && i->type==PAIR){
    
    sl_append(l, dfsch_obj_write(i->car, max_depth-1, readable));
    i = (pair_t*)i->cdr;
    if (i == j){
      sl_append(l," ... #<infinite-list>)");
      return sl_value(l);
    }
    c++;

    if (c == 2){
      c = 0;
      j = (pair_t*)j->cdr;
    }
    if (i)
      sl_append(l," ");
    
  }
  
  if (i){
    sl_append(l, ". ");
    sl_append(l, dfsch_obj_write((object_t*)i, max_depth-1, readable));
  }
  
  sl_append(l,")");
  
  return sl_value(l);
}

static int symbol_equal_p(object_t*, object_t*);
static char* symbol_write(symbol_t*, int, int);
static const dfsch_type_t symbol_type = {
  sizeof(symbol_t), 
  "symbol",
  (dfsch_type_equal_p_t)symbol_equal_p,
  (dfsch_type_write_t)symbol_write
};
#define SYMBOL (&symbol_type) 
static int symbol_equal_p(object_t* a, object_t* b){
  return a == b;
}
static char* symbol_write(symbol_t* s, int max_depth, int readable){
  if (s->data){
    return s->data;
  } else {
    char* buf = GC_MALLOC_ATOMIC(64);
    snprintf(buf, 64, "#<gensym 0x%x>", s);
    return buf;
  }
}


static const dfsch_type_t primitive_type = {
  sizeof(primitive_t),
  "primitive",
  NULL,
  NULL
};
#define PRIMITIVE (&primitive_type)

static char* closure_write(closure_t* c, int max_depth, int readable){
    str_list_t* l = sl_create();
    char buf[sizeof(void*)*2+1];

    sl_append(l, "#<closure 0x");
    snprintf(buf, sizeof(void*)*2+1, "%x", c);
    sl_append(l, buf);
    
    if (c->name){
      sl_append(l, " ");
      sl_append(l, dfsch_obj_write(c->name, max_depth-1, readable));
    }

    sl_append(l,">");
    
    return sl_value(l);
}

static const dfsch_type_t closure_type = {
  sizeof(closure_t),
  "closure",
  NULL,
  (dfsch_type_write_t)closure_write
};
#define CLOSURE (&closure_type)

static const dfsch_type_t macro_type = {
  sizeof(macro_t),
  "macro",
  NULL,
  NULL
};
#define MACRO (&macro_type)

static const dfsch_type_t form_type = {
  sizeof(form_t),
  "form",
  NULL,
  NULL
};
#define FORM (&form_type)

static const dfsch_type_t exception_type = {
  sizeof(exception_t),
  "exception",
  NULL,
  NULL
};
#define EXCEPTION (&exception_type)

static int vector_equal_p(vector_t* a, vector_t* b){
  size_t i;
  if (a->length != b->length)
    return 0;

  for (i=0; i<a->length; i++){
    if (!dfsch_equal_p(a->data[i], b->data[i]))
      return 0;
  }

  return 1;
}

static char* vector_write(vector_t* v, int max_depth, int readable){
  str_list_t* l= sl_create();
  size_t i;
        
  sl_append(l,"#(");
  
  if (v->length > 0){
    for(i = 0; i < v->length-1; ++i){
      sl_append(l, dfsch_obj_write(v->data[i], max_depth-1, readable));
      sl_append(l, " ");
    }
  
    sl_append(l, dfsch_obj_write(v->data[v->length-1], max_depth-1, readable));
  }
  sl_append(l,")");
  return sl_value(l);
}

static const dfsch_type_t vector_type = {
  sizeof(vector_t),
  "vector",
  (dfsch_type_equal_p_t)vector_equal_p,
  (dfsch_type_write_t)vector_write
};
#define VECTOR (&vector_type)



int dfsch_null_p(dfsch_object_t* obj){
  return !obj;
}
int dfsch_pair_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == PAIR;
}
int dfsch_atom_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type != PAIR;
}
int dfsch_symbol_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == SYMBOL;
}
int dfsch_primitive_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == PRIMITIVE;

}
int dfsch_closure_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == CLOSURE;
}
int dfsch_procedure_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == PRIMITIVE || obj->type == CLOSURE;
}
int dfsch_macro_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == MACRO;
}
int dfsch_form_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == FORM;
}

int dfsch_exception_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == EXCEPTION;
}

int dfsch_vector_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == VECTOR;
}


dfsch_object_t* dfsch_nil(){
  return NULL;
}

// Pairs

dfsch_object_t* dfsch_cons(dfsch_object_t* car, dfsch_object_t* cdr){
  pair_t* p = (pair_t*)dfsch_make_object(PAIR);
  if (!p)
    return NULL;


  p->car = car;
  p->cdr = cdr;

  return (object_t*)p;
}
dfsch_object_t* dfsch_car(dfsch_object_t* pair){
  if (!pair || pair->type!=PAIR)
    dfsch_throw("exception:not-a-pair",pair);

  return ((pair_t*)pair)->car;
}
dfsch_object_t* dfsch_cdr(dfsch_object_t* pair){
  if (!pair || pair->type!=PAIR)
    dfsch_throw("exception:not-a-pair",pair);

  return ((pair_t*)pair)->cdr;
}

dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
			      dfsch_object_t* car){
  if (!pair || pair->type!=PAIR)
    dfsch_throw("exception:not-a-pair",pair);

  ((pair_t*)pair)->car = car;
  
  return pair;

}
dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
			      dfsch_object_t* cdr){
  if (!pair || pair->type!=PAIR)
    dfsch_throw("exception:not-a-pair",pair);
  
  ((pair_t*)pair)->cdr = cdr;
  
  return pair;

}
long dfsch_list_length_fast(object_t* list){
  pair_t *i;
  long count;

  if (!list)
    return 0;

  if (list->type!=PAIR)
    return -1;

  i = (pair_t*)list;
  count = 0;

  while (i && i->type==PAIR ){
    i = (pair_t*)i->cdr;
    ++count;
  }

  return count;
}
long dfsch_list_length(object_t* list){
  pair_t *i;
  pair_t *j; 
  long count;

  if (!list)
    return 0;

  if (list->type!=PAIR)
    return -1;

  i = j = (pair_t*)list;
  count = 0;

  while (i && i->type==PAIR ){
    i = (pair_t*)i->cdr;
    ++count;
    if (i == j)
      return -1;
    j = (pair_t*)j->cdr;
    if (!(i && i->type==PAIR ))
      break;
    i = (pair_t*)i->cdr;
    ++count;
    if (i == j)
      return -1;
  }

  if (i)
    return -1;

  return count;
}

long dfsch_list_length_check(object_t* list){
  long len;
  len = dfsch_list_length(list);
  if (len < 0)
    dfsch_throw("exception:not-a-list", list);
  return len;
}

dfsch_object_t* dfsch_list_item(dfsch_object_t* list, int index){
  pair_t* it = (pair_t*)list;
  int i;
  for (i=0; i<index; ++i){
    if (it && it->type == PAIR){
      it = (pair_t*)it->cdr;
    }else{
      dfsch_throw("exception:no-such-item",dfsch_make_number_from_long(index));
    }
  }
  return dfsch_car((object_t*)it);
}

dfsch_object_t* dfsch_append(dfsch_object_t* llist){
  pair_t* head=NULL;
  pair_t* tail=NULL;
  pair_t* i = (pair_t*)llist;
  pair_t* j;

  if (!llist)
    return NULL;

  while(i && i->type == PAIR && i->cdr && 
        i->cdr->type == PAIR){
    
    j = (pair_t*)i->car;
    while(j && j->type == PAIR){
      if (head){
        object_t* tmp = dfsch_cons(j->car,NULL);
        tail->cdr = tmp;
        tail = (pair_t*)tmp;
      }else{
        head = tail = (pair_t*)dfsch_cons(j->car,NULL);
      }
      j = (pair_t*)j->cdr;
    }
    if (j && j->type != PAIR)
      dfsch_throw("exception:not-a-pair", (object_t*)j);

    i = (pair_t*)i->cdr;
  }

  if (!i || i->type != PAIR)
    dfsch_throw("exception:not-a-pair", (object_t*)i);
  /*  if (i->car && i->car->type != PAIR)
      dfsch_throw("exception:not-a-pair", i->car);*/

  if (tail){
    tail->cdr = i->car;
  }else{
    head = (pair_t*)i->car;
  }

  return (object_t*)head;
}

dfsch_object_t* dfsch_list(size_t count, ...){
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  size_t i;
  va_list al;

  va_start(al,count);

  if (count == 0)
    return NULL;

  head = tail = dfsch_cons(va_arg(al, dfsch_object_t*), NULL);

  for(i = 1; i < count; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(va_arg(al, dfsch_object_t*),NULL);
    dfsch_set_cdr(tail, tmp);
    tail = tmp;

  }

  va_end(al);
  return head;

}

dfsch_object_t* dfsch_list_copy(dfsch_object_t* list){
  pair_t *head; 
  pair_t *tail;
  pair_t *i = (pair_t*) list;

  head = tail = NULL;

  while(i && i->type == PAIR){
    if (head){
      object_t* tmp = dfsch_cons(i->car,NULL);
      tail->cdr = tmp;
      tail = (pair_t*)tmp;
    }else{
        head = tail = (pair_t*)dfsch_cons(i->car,NULL);
      }
    i = (pair_t*)i->cdr;
  }
  if (i && i->type != PAIR)
    dfsch_throw("exception:not-a-pair", (object_t*)i);


  return (object_t*)head;

}

dfsch_object_t* dfsch_reverse(dfsch_object_t* list){
  object_t *head; 
  pair_t *i = (pair_t*) list;

  head = NULL;

  while(i && i->type == PAIR){
    head = dfsch_cons(i->car, head);
    i = (pair_t*)i->cdr;
  }
  if (i && i->type != PAIR)
    dfsch_throw("exception:not-a-pair", (object_t*)i);


  return (object_t*)head;

}

dfsch_object_t* dfsch_member(dfsch_object_t *key,
                             dfsch_object_t *list){
  pair_t* i;
  i=(pair_t*)list;
  
  while (i && i->type==PAIR){
    if (dfsch_equal_p(key,i->car)){
      return (object_t*)i;
    }

    i = (pair_t*)i->cdr;
  }

  if (i && i->type!=PAIR)
    dfsch_throw("exception:not-a-pair", (object_t*)i);

  return NULL;
}

dfsch_object_t* dfsch_memv(dfsch_object_t *key,
                           dfsch_object_t *list){
  pair_t* i;
  i=(pair_t*)list;
  
  while (i && i->type==PAIR){
    if (dfsch_eqv_p(key,i->car)){
      return (object_t*)i;
    }

    i = (pair_t*)i->cdr;
  }

  if (i && i->type!=PAIR)
    dfsch_throw("exception:not-a-pair", (object_t*)i);

  return NULL;
}

dfsch_object_t* dfsch_memq(dfsch_object_t *key,
                           dfsch_object_t *list){
  pair_t* i;
  i=(pair_t*)list;
  
  while (i && i->type==PAIR){
    if (key == i->car){
      return (object_t*)i;
    }

    i = (pair_t*)i->cdr;
  }

  if (i && i->type!=PAIR)
    dfsch_throw("exception:not-a-pair", (object_t*)i);

  return NULL;
}


// Alists 

dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
			    dfsch_object_t *alist){
  pair_t* i;
  
  if (!alist || alist->type!=PAIR)
    dfsch_throw("exception:not-a-pair",alist);

  i=(pair_t*)alist;
  
  while (i && i->type==PAIR){
    if (!i->car || i->car->type!=PAIR){
      dfsch_throw("exception:not-a-alist",(object_t*)alist);
    }

    if (dfsch_equal_p(key,((pair_t*)i->car)->car)){
      return i->car;
    }

    i = (pair_t*)i->cdr;
  }

  return NULL;

}
dfsch_object_t* dfsch_assq(dfsch_object_t *key,
			    dfsch_object_t *alist){
  pair_t* i;

  if (!alist || alist->type!=PAIR)
    dfsch_throw("exception:not-a-pair",alist);

  i=(pair_t*)alist;
  
  while (i && i->type==PAIR){
    if (!i->car || i->car->type!=PAIR){
      dfsch_throw("exception:not-a-alist",alist);
    }

    if (key == ((pair_t*)i->car)->car){
      return i->car;
    }

    i = (pair_t*)i->cdr;
  }

  return NULL;

}
dfsch_object_t* dfsch_assv(dfsch_object_t *key,
			    dfsch_object_t *alist){
  pair_t *i;

  if (!alist || alist->type!=PAIR)
    dfsch_throw("exception:not-a-pair",alist);

  i=(pair_t*)alist;
  
  while (i && i->type==PAIR){
    if (!i->car || i->car->type!=PAIR){
      dfsch_throw("exception:not-a-alist",alist);
    }

    if (dfsch_eqv_p(key,((pair_t*)i->car)->car)){
      return i->car;
    }

    i = (pair_t*)i->cdr;
  }

  return NULL;

}



// Symbols

#define HASH_BITS 10
#define HASH_SIZE (1 << HASH_BITS)

typedef struct hash_entry_t hash_entry_t;
struct hash_entry_t {
  symbol_t* entry;
  size_t hash;
  hash_entry_t* next;
};


/*
 * ugly case-insensitive string hash used for symbols
 */
static size_t string_hash(char* string){
  size_t tmp=0;

  while (*string){
    char c = ASCII_tolower(*string); 
    tmp ^= c ^ (tmp << 7); 
    tmp ^= ((size_t)c << 17) ^ (tmp >> 11); 
    ++string;
  }

  return tmp & (HASH_SIZE - 1); 
}

static hash_entry_t*  global_symbol_hash[HASH_SIZE];
static unsigned int gsh_init = 0;
static pthread_mutex_t symbol_lock = PTHREAD_MUTEX_INITIALIZER;

/*
 * It's possible to use rwlock here (althought such solution is not so 
 * straightforward), but it seem unnecessary - most symbol creations are 
 * done when reading source and in such case there will be probably only
 * one thread doing such things.
 */


static gsh_check_init(){
  int err;
  if (gsh_init)
    return;

  pthread_mutex_lock(&symbol_lock);

  memset(global_symbol_hash, 0, sizeof(hash_entry_t*)*HASH_SIZE);
  gsh_init = 1;

  pthread_mutex_unlock(&symbol_lock);
}

static symbol_t* lookup_symbol(char *symbol){

  size_t hash = string_hash(symbol);
  hash_entry_t *i = global_symbol_hash[hash];

  while (i){
    if (i->hash == hash && ascii_strcasecmp(i->entry->data, symbol)==0){
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

  i = global_symbol_hash[string_hash(s->data)];
  j = NULL;
  
  while (i){
    if (i->entry == s){
      if (j){
        j->next = i->next;
      } else {
        global_symbol_hash[string_hash(s->data)] = i->next;
      }
      free(i);
      break;
    }
    j = i;
    i = i->next;
  }

  pthread_mutex_unlock(&symbol_lock);

  s->data = NULL;
}

static symbol_finalizer(symbol_t* symbol, void* cd){
  free_symbol(symbol);
}

static symbol_t* make_symbol(char *symbol){
  symbol_t *s = (symbol_t*)dfsch_make_object(SYMBOL);

  GC_REGISTER_FINALIZER(s, 
                        (GC_finalization_proc)symbol_finalizer, NULL, 
                        NULL, NULL);
  
  s->data = symbol;
  
  hash_entry_t *e = malloc(sizeof(hash_entry_t));

  e->entry = s;
  e->hash = string_hash(symbol);

  e->next = global_symbol_hash[e->hash];
  global_symbol_hash[e->hash] = e;
  
  return s;
}


void dfsch_unintern(dfsch_object_t* symbol){
  if (symbol->type != SYMBOL)
    dfsch_throw("exception:not-a-symbol", symbol);

  free_symbol((symbol_t*)symbol);
}

dfsch_object_t* dfsch_gensym(){
  symbol_t *s = (symbol_t*)dfsch_make_object(SYMBOL);

  s->data = NULL;

  return (object_t*)s;
}

dfsch_object_t* dfsch_make_symbol(char* symbol){

  symbol_t *s;

  pthread_mutex_lock(&symbol_lock);

  s = lookup_symbol(symbol);

  if (!s)
    s = make_symbol(symbol);

  pthread_mutex_unlock(&symbol_lock);

  return (object_t*)s;

}
char* dfsch_symbol(dfsch_object_t* symbol){
  if (!symbol || symbol->type!=SYMBOL)
    return NULL;

  return ((symbol_t*)symbol)->data;
}
dfsch_object_t* dfsch_sym_true(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("true"); 
  // TODO: shouldn't this be something other? #t ? T ?
  return cache;
}
dfsch_object_t* dfsch_sym_quote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("quote");
  return cache;
}
dfsch_object_t* dfsch_sym_quasiquote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("quasiquote");
  return cache;
}
dfsch_object_t* dfsch_sym_unquote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("unquote");
  return cache;
}
dfsch_object_t* dfsch_sym_unquote_splicing(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("unquote-splicing");
  return cache;
}
dfsch_object_t* dfsch_sym_else(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("else");
  return cache;
}
dfsch_object_t* dfsch_sym_bold_right_arrow(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("=>");
  return cache;
}
dfsch_object_t* dfsch_sym_tail_recursive(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("tail-recursive");
  return cache;
}
dfsch_object_t* dfsch_bool(int bool){
  return bool?dfsch_sym_true():NULL;
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
      return i->entry->data;
    }
  }  
  return NULL;
}


// closures

extern dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
				    dfsch_object_t* args,
				    dfsch_object_t* code){
  closure_t *c = (closure_t*)dfsch_make_object(CLOSURE);
  if (!c)
    return NULL;
  
  c->env = env;
  c->args = args;
  c->code = code;
  c->name = NULL;

  return (object_t*)c;
  
}
extern dfsch_object_t* dfsch_named_lambda(dfsch_object_t* env,
                                          dfsch_object_t* args,
                                          dfsch_object_t* code,
                                          dfsch_object_t* name){
  closure_t *c = (closure_t*)dfsch_make_object(CLOSURE);
  if (!c)
    return NULL;
  
  c->env = env;
  c->args = args;
  c->code = code;
  c->name = name;

  return (object_t*)c;
  
}

// native code

object_t* dfsch_make_primitive(dfsch_primitive_t prim, void *baton){
  primitive_t* p = (primitive_t*)dfsch_make_object(PRIMITIVE);
  if (!p)
    return NULL;

  p->proc = prim;
  p->baton = baton;

  return (object_t*)p;
}

// macros

object_t* dfsch_make_macro(object_t *proc){
  macro_t *m = (macro_t*)dfsch_make_object(MACRO);
  
  if (!m)
    return NULL;

  m->proc = proc;

  return (object_t*)m;
}
object_t* dfsch_make_form(object_t *proc){
  form_t *f = (form_t*)dfsch_make_object(FORM);
  
  if (!f)
    return NULL;

  f->proc = proc;

  return (object_t*)f;
}

typedef struct continuation_t continuation_t;
struct continuation_t {
  dfsch_type_t* type;
  jmp_buf ret;
  object_t* value;
  int active;
  pthread_t thread;
  continuation_t* next;
};

typedef struct thread_info_t {
  jmp_buf* exception_ret;
  dfsch_object_t* exception_obj;
  dfsch_object_t* stack_trace;
  continuation_t* cont_stack;
  char* break_type;
} thread_info_t;

static pthread_key_t thread_key;
static pthread_once_t thread_once = PTHREAD_ONCE_INIT;

static void thread_info_destroy(void* ptr){
  if (ptr)
    GC_FREE(ptr);
}
static void thread_key_alloc(){
  pthread_key_create(&thread_key, thread_info_destroy);
}
static thread_info_t* get_thread_info(){
  thread_info_t *ei = pthread_getspecific(thread_key);
  if (!ei){
    pthread_once(&thread_once, thread_key_alloc);
    ei = GC_MALLOC_UNCOLLECTABLE(sizeof(thread_info_t));
    ei->exception_ret = NULL;
    ei->stack_trace = NULL;
    ei->cont_stack = NULL;
    pthread_setspecific(thread_key, ei);
  }
  return ei;
}

dfsch_object_t* dfsch_get_stack_trace(){
  thread_info_t *ti = get_thread_info();
  return ti->stack_trace;
}

void dfsch_raise(dfsch_object_t* exception){

  thread_info_t *ei = get_thread_info();

  if (!ei->exception_ret){
    fputs(dfsch_exception_write(exception),stderr);        
    abort();
  }

  ei->exception_obj = exception;
  longjmp(*ei->exception_ret, 1);
}

dfsch_object_t* dfsch_try(dfsch_object_t* handler,
                          dfsch_object_t* thunk){

  volatile thread_info_t *ei = (volatile thread_info_t*)get_thread_info();
  volatile jmp_buf *old_ret;
  volatile dfsch_object_t* old_frame;
  
  old_ret = (volatile jmp_buf*)ei->exception_ret;
  old_frame = (volatile object_t*) ei->stack_trace;
  ei->exception_ret = GC_NEW(jmp_buf);

  if(setjmp(*ei->exception_ret) == 1){
    ei->exception_ret = (jmp_buf*)old_ret;
    ei->stack_trace = (object_t*)old_frame;
    return dfsch_apply(handler, dfsch_list(1, ei->exception_obj));
  }else{
    object_t *r = dfsch_apply(thunk, NULL);
    ei->exception_ret = (jmp_buf*)old_ret;
    ei->stack_trace = (object_t*)old_frame;
    return r;
  }

}


dfsch_object_t* dfsch_make_exception(dfsch_object_t* type, 
				     dfsch_object_t* data,
                                     dfsch_object_t* stack_trace){
  exception_t* e = (exception_t*)dfsch_make_object(EXCEPTION);
  if (!e)
    return NULL;


  e->class = type;
  e->data = data;
  e->stack_trace = stack_trace;
  
  return (object_t*)e;
}

dfsch_object_t* dfsch_throw(char* type, 
                            dfsch_object_t* data){
  object_t* e = dfsch_make_exception(dfsch_make_symbol(type), data,
                                     dfsch_get_stack_trace());

  dfsch_raise(e);
    
}
dfsch_object_t* dfsch_break(char* type){
  thread_info_t *ti = get_thread_info();
  ti->break_type = type;
}

dfsch_object_t* dfsch_exception_type(dfsch_object_t* e){
  if (!dfsch_exception_p(e))
    return NULL;

  return ((exception_t*)e)->class;
}
dfsch_object_t* dfsch_exception_data(dfsch_object_t* e){
  if (!dfsch_exception_p(e))
    return NULL;

  return ((exception_t*)e)->data;
}


static object_t* continuation_apply(continuation_t *cont, 
                                    object_t* args,
                                    dfsch_tail_escape_t* esc){

  object_t* value;
  DFSCH_OBJECT_ARG(args, value);
  DFSCH_ARG_END(args);
  
  if (!cont->active)
    dfsch_throw("exception:already-returned",NULL);
  if (cont->thread != pthread_self())
    dfsch_throw("exception:cross-thread-continuation",NULL);


  cont->value = value;
  longjmp(cont->ret,1);
}

static struct dfsch_type_t continuation_type = {
  sizeof(continuation_t*),
  "escape-continuation",
  NULL,
  NULL,
  (dfsch_type_apply_t)continuation_apply
};

dfsch_object_t* dfsch_call_ec(dfsch_object_t* proc){
  object_t* value;
  continuation_t *cont = 
    (continuation_t*)dfsch_make_object(&continuation_type);
  continuation_t *i;
  thread_info_t *ti = get_thread_info();

  cont->active = 1;
  cont->next = ti->cont_stack;
  cont->thread = pthread_self();
  ti->cont_stack = cont;
  if(setjmp(cont->ret) == 1){
    value = cont->value;
  }else{
    value = dfsch_apply(proc,
                        dfsch_list(1, cont));
  }
  
  i = ti->cont_stack;

  while (i && i != cont){
    i->active = 0;
    i = i->next;
  }
  
  cont->active = 0;
  ti->cont_stack = cont->next;
  return value;

}

// Vectors

dfsch_object_t* dfsch_make_vector(size_t length, dfsch_object_t* fill){
  vector_t* v = (vector_t*)dfsch_make_object(VECTOR);
  size_t i;

  v->length = length;
  v->data = GC_MALLOC(sizeof(object_t*) * length);

  for(i = 0; i<length; ++i){
    v->data[i] = fill;
  }

  return (object_t*)v;
}
dfsch_object_t* dfsch_vector(size_t count, ...){
  size_t i;
  vector_t* v = (vector_t*)dfsch_make_object(VECTOR);
  va_list al;

  va_start(al,count);

  v->length = count;
  v->data = GC_MALLOC(sizeof(object_t*) * count);

  for(i = 0; i < count; ++i){
    v->data[i] = va_arg(al, dfsch_object_t*);
  }

  va_end(al);
  return (object_t*)v;

}

size_t dfsch_vector_length(dfsch_object_t *vector){
  if (!vector || vector->type != VECTOR)
    return 0;

  return ((vector_t*)vector)->length;  
}

dfsch_object_t* dfsch_vector_ref(dfsch_object_t *vector, size_t k){
  if (!vector || vector->type != VECTOR)
    dfsch_throw("exception:not-a-vector",vector);

  if (((vector_t*)vector)->length <= k)
    dfsch_throw("exception:invalid-index",dfsch_make_number_from_long(k));
  
  return ((vector_t*)vector)->data[k];
}

dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                 dfsch_object_t* obj){
  if (!vector || vector->type != VECTOR)
    dfsch_throw("exception:not-a-vector",vector);

  if (((vector_t*)vector)->length <= k)
    dfsch_throw("exception:invalid-index",dfsch_make_number_from_long(k));
  
  ((vector_t*)vector)->data[k] = obj;

  return vector;
}



dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector){
  pair_t *head; 
  pair_t *tail;
  size_t i;

  if (!vector || vector->type != VECTOR)
    dfsch_throw("exception:not-a-vector",vector);

  if (((vector_t*)vector)->length == 0)
    return NULL;

  head = tail = (pair_t*)dfsch_cons(((vector_t*)vector)->data[0], NULL);

  for(i = 1; i< ((vector_t*)vector)->length; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(((vector_t*)vector)->data[i],NULL);
    tail->cdr = tmp;
    tail = (pair_t*)tmp;

  }

  return (object_t*)head;
}

dfsch_object_t* dfsch_list_2_vector(dfsch_object_t* list){
  vector_t* vector;
  pair_t* j = (pair_t*)list;
  size_t i=0;
  vector = (vector_t*)dfsch_make_object(VECTOR);
  vector->length = dfsch_list_length_check(list);
  vector->data = GC_MALLOC(sizeof(object_t*)*vector->length);
  
  while (j && j->type == PAIR){
    vector->data[i] = j->car;
    j = (pair_t*)j->cdr;
    i++;
  }

  return (object_t*)vector;
}

char* dfsch_obj_write(dfsch_object_t* obj, int max_depth, int readable){
  if (!obj){
    return "()";
  }

  if (max_depth==0){
    return "...";
  }

  if (!obj->type){
    str_list_t *sl = sl_create();
    char buf[sizeof(void*)*2+1];
    sl_append(sl, "#<typeless-value ");
    sl_append(sl, " 0x");
    snprintf(buf, sizeof(void*)*2+1, "%x", obj);
    sl_append(sl, buf);
    sl_append(sl, ">");
    return sl_value(sl);
  }

  if (!obj->type->write){
    str_list_t *sl = sl_create();
    char buf[sizeof(void*)*2+1];
    sl_append(sl, "#<");
    sl_append(sl, obj->type->name);
    sl_append(sl, " 0x");
    snprintf(buf, sizeof(void*)*2+1, "%x", obj);
    sl_append(sl, buf);
    sl_append(sl, ">");
    return sl_value(sl);
  }

  return obj->type->write(obj, max_depth, readable);
}
char* dfsch_exception_write(dfsch_object_t* e){
  str_list_t *l = sl_create();

  sl_append(l,"Exception occured: ");

  if (!dfsch_exception_p(e)){
    sl_append(l,dfsch_obj_write(e,3,1));
  }else{
    dfsch_object_t *i = ((exception_t*)e)->stack_trace;
    sl_append(l,dfsch_obj_write(((exception_t*)e)->class,3,1));
    sl_append(l," with data: ");
    sl_append(l,dfsch_obj_write(((exception_t*)e)->data,3,1));
    sl_append(l,"\n\nCall stack:\n");
    while (i){
      object_t* item = dfsch_car(i);
      sl_append(l,"  ");

      if (dfsch_vector_ref(item, 4) == dfsch_sym_tail_recursive())
        sl_append(l,"...");
        
      sl_append(l,dfsch_obj_write(dfsch_vector_ref(item, 1),20,1));
      sl_append(l,"\n      ");
      sl_append(l,dfsch_obj_write(dfsch_vector_ref(item, 0),20,1));
      sl_append(l,"\n");
      i = dfsch_cdr(i);
    }
  }
  sl_append(l,"\n");

  return sl_value(l);
}

typedef struct read_ctx_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;  
} read_ctx_t;

static read_callback(dfsch_object_t *obj, void* ctx){
  dfsch_object_t* new_tail = dfsch_cons(obj, NULL);

  if (!((read_ctx_t*)ctx)->head){
    ((read_ctx_t*)ctx)->head = new_tail;
  }else{
    dfsch_set_cdr(((read_ctx_t*)ctx)->tail, new_tail);
  }

  ((read_ctx_t*)ctx)->tail = new_tail;

  return 1;
}

dfsch_object_t* dfsch_list_read(char* str){
  dfsch_parser_ctx_t *parser = dfsch_parser_create();
  read_ctx_t ctx;
  int err;
  dfsch_parser_callback(parser, read_callback, &ctx);

  ctx.head = ctx.tail = NULL;
  
  err = dfsch_parser_feed(parser, str);
  if (!err)
    dfsch_parser_feed(parser, " ");

  if ((err && err != DFSCH_PARSER_STOPPED)
      || dfsch_parser_get_level(parser)!=0){
      dfsch_throw("read:syntax-error",NULL);
  }  
  
  return ctx.head;
}
dfsch_object_t* dfsch_obj_read(char* str){
  object_t* list = dfsch_list_read(str);
  if (!list)
    return NULL;
  return dfsch_car(list);
}


dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent){
  return dfsch_cons(dfsch_hash_make(NULL, DFSCH_HASH_EQ), parent);
}

object_t* dfsch_lookup(object_t* name, object_t* env){
  pair_t *i;

  if (!env || env->type!=PAIR){
    dfsch_throw("exception:not-a-pair",env);
  }

  i = (pair_t*)env;
  while (i && i->type==PAIR){
    object_t* ret = dfsch_hash_ref(i->car, name);
    if (ret){
      return dfsch_car(ret);
    }

    i = (pair_t*)i->cdr;
  }
  
  dfsch_throw("exception:unbound-variable",name);
}
object_t* dfsch_env_get(object_t* name, object_t* env){
  pair_t *i;

  if (!env || env->type!=PAIR){
    dfsch_throw("exception:not-a-pair",env);
  }

  i = (pair_t*)env;
  while (i && i->type==PAIR){
    object_t* ret = dfsch_hash_ref(i->car, name);
    if (ret){
      return ret;
    }

    i = (pair_t*)i->cdr;
  }
  
  return NULL;
}


object_t* dfsch_set(object_t* name, object_t* value, object_t* env){
  pair_t *i;
  if (!env || env->type!=PAIR){
    dfsch_throw("exception:not-a-pair",env);
  }

  i = (pair_t*)env;
  while (i && i->type==PAIR){
    if(dfsch_hash_set_if_exists(i->car, name, value))
      return value;

    i = (pair_t*)i->cdr;
  }
  

  dfsch_throw("exception:unbound-variable",name);
}


object_t* dfsch_define(object_t* name, object_t* value, object_t* env){
  if (!env || env->type!=PAIR)
    dfsch_throw("exception:not-a-pair",env);

  dfsch_hash_set(((pair_t*)env)->car, name, value);  

  return value;

}


static object_t* eval_list(object_t *list, object_t* env){
  pair_t *i;
  object_t *f=NULL;
  pair_t *t, *p;
  object_t *r; 

  if (!list)
    return NULL;

  if (list->type!=PAIR){
    return dfsch_eval(list,env);
  }

  i = (pair_t*)list;
  while (i && i->type==PAIR){
    r = dfsch_eval(i->car,env);

    t = (pair_t*)dfsch_cons(r,NULL);
    if (f){
      p->cdr = (object_t*)t;
      p = t;
    }else{
      f = (object_t*)(p = t);
    }

    i = (pair_t*)i->cdr;
  }

  return f;
}

struct dfsch_tail_escape_t {
  jmp_buf ret;
  object_t *code;
  object_t *env;
  object_t *proc_name; 
};

typedef dfsch_tail_escape_t tail_escape_t;


dfsch_object_t* dfsch_eval_tr(dfsch_object_t* exp, 
                              dfsch_object_t* env,
                              dfsch_tail_escape_t* esc){
 start:
  
  if (!exp) 
    return NULL;

  if(exp->type == SYMBOL)
    return dfsch_lookup(exp,env);

  if(exp->type==PAIR){
    
    object_t *f = dfsch_eval_tr(((pair_t*)exp)->car,env,NULL);
    
    if (!f)
      dfsch_throw("exception:not-a-procedure-or-macro", f);
	
 
    if (f->type == FORM)
      return dfsch_apply_tr(((form_t*)f)->proc,     
                            dfsch_cons(env,
                                       ((pair_t*)exp)->cdr),
                            esc);
    if (f->type == MACRO)
      return dfsch_eval_proc_tr(dfsch_apply(((macro_t*)f)->proc,
                                            dfsch_cons(env, 
                                                       ((pair_t*)exp)->cdr)),
                                env,
                                NULL,
                                esc);
      
    return dfsch_apply_tr(f, 
                          eval_list(((pair_t*)exp)->cdr,env),
                          esc);
    
    
  }  
  
  return exp;
}

dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env){
  return dfsch_eval_tr(exp, env, NULL);
}

static object_t* lambda_extend(object_t* fa, object_t* aa, object_t* env){
  pair_t* i_f=(pair_t*)fa;
  pair_t* i_a=(pair_t*)aa;
  object_t* ext_env = dfsch_new_frame(env);

  while ((i_f && i_f->type==PAIR) &&
	 (i_a && i_a->type==PAIR)){

    dfsch_define(i_f->car, i_a->car, ext_env);

    i_f = (pair_t*)i_f->cdr;
    i_a = (pair_t*)i_a->cdr;
    
  }

  if (i_f && i_f->type==SYMBOL){

    dfsch_define((object_t*)i_f, (object_t*)i_a, ext_env);
    return ext_env;
  }

  if (!i_a  && i_f)
      dfsch_throw("exception:too-few-arguments", aa);
  if (!i_f && i_a) 
      dfsch_throw("exception:too-many-arguments", aa);
  

  return ext_env;
}

dfsch_object_t* dfsch_eval_proc_tr(dfsch_object_t* code, 
                                   dfsch_object_t* env,
                                   dfsch_object_t* proc_name,
                                   tail_escape_t* esc){
  pair_t *i;
  object_t *r=NULL;
  tail_escape_t myesc;
  thread_info_t *ti;
  dfsch_object_t *old_frame;
  dfsch_object_t *my_frame;

  if (!env)
    return NULL;
  if (!code)
    return NULL;

  ti = get_thread_info();

  if (ti->break_type){
    dfsch_throw("exception:break", dfsch_make_symbol(ti->break_type));
    ti->break_type = NULL;
  }

  if (esc){
    esc->code = code;
    esc->env = env;
    esc->proc_name = proc_name;
    longjmp(esc->ret,1);
  }
  
  old_frame = ti->stack_trace;  
  my_frame = dfsch_vector(5, NULL, proc_name, code, env, NULL);
  ti->stack_trace = dfsch_cons(my_frame,
                               ti->stack_trace);

  if (setjmp(myesc.ret)){  
    i = (pair_t*)myesc.code;
    env = myesc.env;
    my_frame = dfsch_vector(5, NULL, 
                            myesc.proc_name, 
                            myesc.code, 
                            myesc.env,
                            dfsch_sym_tail_recursive());
    dfsch_set_car(ti->stack_trace, my_frame);
  }else{
    i = (pair_t*)code;
  }

  while (i && i->type==PAIR ){
    object_t* exp = i->car; 

    ((vector_t*)my_frame)->data[0] = exp;

    if (i->cdr)
      r = dfsch_eval_tr(exp,env,NULL);
    else
      r = dfsch_eval_tr(exp,env,(tail_escape_t*)&myesc);
   
    i = (pair_t*)i->cdr;
  }

  
  ti->stack_trace = old_frame;
  return r;
}

dfsch_object_t* dfsch_eval_proc(dfsch_object_t* code, dfsch_object_t* env){
  return dfsch_eval_proc_tr(code, env, NULL, NULL);
}

dfsch_object_t* dfsch_apply_tr(dfsch_object_t* proc, 
                               dfsch_object_t* args,
                               tail_escape_t* esc){

  if (!proc)
    return NULL;

  /**
   * Two most common cases are written here explicitly (for historical
   * and performance reasons)
   */

  if (proc->type == PRIMITIVE){
    object_t* r = ((primitive_t*)proc)->proc(((primitive_t*)proc)->baton,args,
                                             esc);
    return r;
  }

  if (proc->type == CLOSURE){
    object_t* r = dfsch_eval_proc_tr(((closure_t*)proc)->code,
                                     lambda_extend(((closure_t*)proc)->args,
                                                   args,
                                                   ((closure_t*)proc)->env),
                                     dfsch_cons(proc, args),
                                     esc);
    return r;
  }

  if (proc->type->apply){
    return proc->type->apply(proc, args, esc);
  }

  dfsch_throw("exception:not-a-procedure", proc);
   
}

dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args){
  return dfsch_apply_tr(proc, args, NULL);
}

dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, dfsch_object_t* arg){
  if (dfsch_pair_p(arg)){
    object_t* car = dfsch_car(arg);
    object_t* cdr = dfsch_cdr(arg);

    if (car == dfsch_sym_unquote() && dfsch_pair_p(cdr)){
      return dfsch_eval(dfsch_car(cdr), env);
    }else if (dfsch_pair_p(car)){
      if (dfsch_car(car) == dfsch_sym_unquote_splicing()){
        return dfsch_append(dfsch_list(2,
                                       dfsch_eval(dfsch_car(dfsch_cdr(car)), 
                                                  env),
                                       dfsch_quasiquote(env, cdr)));
      }
    }

    return dfsch_cons(dfsch_quasiquote(env,car), dfsch_quasiquote(env,cdr));
  }else{
    return arg;
  }
}

static object_t* native_top_level_environment(void *baton, object_t* args,
                                              dfsch_tail_escape_t* esc){
  return baton;
}
static object_t* native_form_current_environment(void *baton, object_t* args,
                                                 dfsch_tail_escape_t* esc){
  return dfsch_car(args);
}

dfsch_ctx_t* dfsch_make_context(){
  dfsch_ctx_t* ctx=GC_malloc(sizeof(dfsch_ctx_t));
  if (!ctx)
    return NULL;

  gsh_check_init();

  ctx->env = dfsch_new_frame(NULL);

  dfsch_ctx_define(ctx, "top-level-environment", 
                   dfsch_make_primitive(&native_top_level_environment, ctx->env));
  dfsch_ctx_define(ctx, "current-environment", 
		   dfsch_make_form(dfsch_make_primitive(&native_form_current_environment,
                                                        NULL)));

  dfsch__native_register(ctx);

  return ctx;
}
dfsch_object_t* dfsch_ctx_eval(dfsch_ctx_t* ctx, dfsch_object_t* exp){
  return dfsch_eval(exp, ctx->env);
}
extern dfsch_object_t* dfsch_ctx_eval_list(dfsch_ctx_t* ctx, 
					   dfsch_object_t* list){
  return dfsch_eval_proc(list, ctx->env);
}

dfsch_object_t* dfsch_ctx_lambda(dfsch_ctx_t *ctx,
                      dfsch_object_t* args,
                      dfsch_object_t* code){
  return dfsch_lambda(ctx->env, args, code);
}

dfsch_object_t* dfsch_ctx_define(dfsch_ctx_t *ctx, 
                                 char *name, 
                                 dfsch_object_t *obj){
  
  return dfsch_define(dfsch_make_symbol(name),
                      obj,
                      ctx->env);
  
}
dfsch_object_t* dfsch_ctx_lookup(dfsch_ctx_t *ctx, char *name){
  return dfsch_lookup(ctx->env,dfsch_make_symbol(name));
}
dfsch_object_t* dfsch_ctx_environment(dfsch_ctx_t *ctx){
  return ctx->env;
}
