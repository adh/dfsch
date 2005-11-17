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

#include "../dfsch/dfsch.h"
#include <dfsch/hash.h>
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include <gc/gc.h>

//#define PARSER_DEBUG
//#define GC_DEBUG
//#define OBJ_LIST_DEBUG

typedef enum {
  PAIR,
  SYMBOL,
  NUMBER,
  STRING,
  PRIMITIVE,
  CLOSURE,
  MACRO,
  FORM,
  EXCEPTION,
  VECTOR,
  NATIVE // define new types here
} type_t ;

typedef dfsch_object_t object_t;
typedef dfsch_ctx_t context_t;

typedef struct symbol_t symbol_t;

struct symbol_t{
  char *data;
  //  object_t *next;
  //  object_t *prev;
};


struct dfsch_ctx_t{
  object_t* env;
};

typedef struct pair_t{
  object_t *car;
  object_t *cdr;
} pair_t;

typedef struct primitive_t {

  dfsch_primitive_t proc;
  void *baton;

} primitive_t;

typedef struct closure_t{
  object_t* args;
  object_t* code;
  object_t* env;
  object_t* name;
} closure_t;

typedef struct exception_t{
  object_t *type;
  object_t *data;
  object_t *trace;
} exception_t; 

typedef struct native_t {
  
  object_t* type;
  void *data; 

} native_t;
typedef struct vector_t {
  
  size_t length;
  object_t** data;

} vector_t;


struct dfsch_object_t{
  type_t type;
  union {
    pair_t pair;
    symbol_t symbol;
    double number;
    char* string;
    primitive_t primitive;
    closure_t closure;
    object_t *macro;
    exception_t exception;
    vector_t vector;
    native_t native;
  } data;
};






#define EXCEPTION_CHECK(x) {if (dfsch_object_exception_p(x)) return x;}

static object_t* make_object(type_t type){
  object_t* o = GC_MALLOC(sizeof(object_t));
  if (!o)
    return NULL;

  o->type = type;

  return o;
}

int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a)
    return 0;
  if (!b)
    return 0;
  
  if (a->type!=b->type)
    return 0;

  switch(a->type){
  case NUMBER:
    return (a->data.number == b->data.number);
  case STRING:
    return strcmp(a->data.string, b->data.string)==0?1:0;
  }

  return 0;
  
}

int dfsch_object_null_p(dfsch_object_t* obj){
  return !obj;
}
int dfsch_object_pair_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == PAIR;
}
int dfsch_object_atom_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type != PAIR;
}
int dfsch_object_symbol_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == SYMBOL;
}
int dfsch_object_number_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == NUMBER;
}
int dfsch_object_string_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == STRING;

}
int dfsch_object_primitive_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == PRIMITIVE;

}
int dfsch_object_closure_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == CLOSURE;
}
int dfsch_object_procedure_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == PRIMITIVE || obj->type == CLOSURE;
}
int dfsch_object_macro_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == MACRO;
}

int dfsch_object_exception_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == EXCEPTION;
}

int dfsch_object_vector_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == VECTOR;
}

int dfsch_object_native_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == NATIVE;
}

dfsch_object_t* dfsch_nil(){
  return NULL;
}

// Pairs

dfsch_object_t* dfsch_cons(dfsch_object_t* car, dfsch_object_t* cdr){
  object_t* p = make_object(PAIR);
  if (!p)
    return NULL;


  p->data.pair.car = car;
  p->data.pair.cdr = cdr;

  return p;
}
dfsch_object_t* dfsch_car(dfsch_object_t* pair){
  if (!pair || pair->type!=PAIR)
    DFSCH_THROW("exception:not-a-pair",pair);

  return pair->data.pair.car;
}
dfsch_object_t* dfsch_cdr(dfsch_object_t* pair){
  if (!pair || pair->type!=PAIR)
    DFSCH_THROW("exception:not-a-pair",pair);

  return pair->data.pair.cdr;
}

dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
			      dfsch_object_t* car){
  if (!pair || pair->type!=PAIR)
    DFSCH_THROW("exception:not-a-pair",pair);

  if (pair->data.pair.car!=car){
    pair->data.pair.car = car;
  }
  
  return pair;

}
dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
			      dfsch_object_t* cdr){
  if (!pair || pair->type!=PAIR)
    DFSCH_THROW("exception:not-a-pair",pair);
  
  if (pair->data.pair.cdr!=cdr){
    pair->data.pair.cdr = cdr;
  }
  
  return pair;

}
int dfsch_list_length(object_t* list){
  object_t *i;
  int count;

  if (!list)
    return 0;
  if (list->type!=PAIR)
    return -1;

  i = list;
  count = 0;

  while (i && i->type==PAIR ){
    object_t* exp = i->data.pair.car; 
    i = i->data.pair.cdr;
    ++count;
  }

  
  return count;
}

dfsch_object_t* dfsch_list_item(dfsch_object_t* list, int index){
  object_t* it = list;
  int i;
  for (i=0; i<index; ++i){
    if (dfsch_object_pair_p(it)){
      it = dfsch_cdr(it);
    }else{
      DFSCH_THROW("exception:no-such-item",dfsch_make_number(index));
    }
  }
  return dfsch_car(it);
}

dfsch_object_t* dfsch_append(dfsch_object_t* llist){
  object_t* head=NULL;
  object_t* tail=NULL;
  object_t* i = llist;
  object_t* j;

  if (!llist)
    return NULL;

  while(i && i->type == PAIR && i->data.pair.cdr && 
        i->data.pair.cdr->type == PAIR){
    
    j = i->data.pair.car;
    while(j && j->type == PAIR){
      if (head){
        object_t* tmp = dfsch_cons(j->data.pair.car,NULL);
        dfsch_set_cdr(tail, tmp);
        tail = tmp;
      }else{
        head = tail = dfsch_cons(j->data.pair.car,NULL);
      }
      j = j->data.pair.cdr;
    }
    if (!(dfsch_object_pair_p(j) || !j))
      DFSCH_THROW("exception:not-a-pair", j);

    i = i->data.pair.cdr;
  }

  if (!(dfsch_object_pair_p(i) || !i))
    DFSCH_THROW("exception:not-a-pair", i);
  if (!(dfsch_object_pair_p(i->data.pair.car) || !i->data.pair.car))
    DFSCH_THROW("exception:not-a-pair", i->data.pair.car);

  if (tail){
    tail->data.pair.cdr = i->data.pair.car;
  }else{
    head = i->data.pair.car;
  }

  return head;
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
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  dfsch_object_t *i;

  head = tail = NULL;

  while(i && i->type == PAIR){
    if (head){
      object_t* tmp = dfsch_cons(i->data.pair.car,NULL);
      dfsch_set_cdr(tail, tmp);
      tail = tmp;
    }else{
        head = tail = dfsch_cons(i->data.pair.car,NULL);
      }
    i = i->data.pair.cdr;
  }
  if (!(dfsch_object_pair_p(i) || !i))
    DFSCH_THROW("exception:not-a-pair", i);


  return head;

}


// Alists

dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
			    dfsch_object_t *alist){
  if (!alist || alist->type!=PAIR)
    DFSCH_THROW("exception:not-a-pair",alist);

  object_t* i=alist;
  
  while (i && i->type==PAIR){
    if (!i->data.pair.car || i->data.pair.car->type!=PAIR){
      DFSCH_THROW("exception:not-a-alist",alist);
    }

    if (dfsch_eq_p(key,i->data.pair.car->data.pair.car)){
      return i->data.pair.cdr;
    }

    i = i->data.pair.cdr;
  }

  return NULL;

}

// Numbers

dfsch_object_t* dfsch_make_number(double num){
  object_t *n;
  n = make_object(NUMBER);
  if (!n)
    return NULL;


  n->data.number = num;

  return n;
}
float dfsch_number(dfsch_object_t *n){
  if (!n || n->type!=NUMBER)
    return 0.0;

  return n->data.number;

}

// Strings

dfsch_object_t* dfsch_make_string(char* str){
  object_t *n;
  n = make_object(STRING);
  if (!n)
    return NULL;


  n->data.string = stracpy(str);

  return n;
}
char* dfsch_string(dfsch_object_t *n){
  if (!n || n->type!=STRING)
    return NULL;

  return n->data.string;

}


// Symbols

#define HASH_BITS 10
#define HASH_SIZE 1 << HASH_BITS

typedef struct hash_entry_t hash_entry_t;
struct hash_entry_t {
  dfsch_object_t* entry;
  hash_entry_t* next;
};


/*
 * ugly case-insensitive string hash used for symbols
 */
static size_t string_hash(char* string){
  size_t tmp=0;

  while (*string){
    char c = isupper(*string)?tolower(*string):*string; 
    tmp ^= c ^ tmp << 1; 
    tmp ^= c << 17 ^ tmp >> 3; 
    ++string;
  }

  return tmp & (HASH_SIZE - 1); 
}

static hash_entry_t*  global_symbol_hash[HASH_SIZE];
static unsigned int gsh_init = 0;

static gsh_check_init(){
  if (gsh_init)
    return;

  memset(global_symbol_hash, 0, sizeof(hash_entry_t*)*HASH_SIZE);
  gsh_init = 1;
}

static object_t* lookup_symbol(char *symbol){

  hash_entry_t *i = global_symbol_hash[string_hash(symbol)];

  while (i){
    if (strcasecmp(i->entry->data.symbol.data, symbol)==0){
      return i->entry;
    }
    i = i->next;
  }

  return NULL;
}
static object_t* make_symbol(char *symbol){
  object_t *s = make_object(SYMBOL);
  
  s->data.symbol.data = stracpy(symbol);
  
  hash_entry_t *e = GC_MALLOC(sizeof(hash_entry_t));

  e->entry = s;
  
  size_t hash = string_hash(symbol);

  e->next = global_symbol_hash[hash];
  global_symbol_hash[hash] = e;
  
  return s;
}


dfsch_object_t* dfsch_make_symbol(char* symbol){

  object_t *s = lookup_symbol(symbol);

  if (!s)
    s = make_symbol(symbol);

  return s;

}
char* dfsch_symbol(dfsch_object_t* symbol){
  if (!symbol || symbol->type!=SYMBOL)
    return NULL;

  return symbol->data.symbol.data;
}
dfsch_object_t* dfsch_sym_true(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("true");
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
dfsch_object_t* dfsch_bool(int bool){
  return bool?dfsch_sym_true():NULL;
}

struct dfsch_symbol_iter_t{
  hash_entry_t* item;
  size_t bucket;
};

char* dfsch_get_next_symbol(dfsch_symbol_iter_t **iter){
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
      return i->entry->data.symbol.data;
    }
  }  
  return NULL;
}


// closures

extern dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
				    dfsch_object_t* args,
				    dfsch_object_t* code){
  object_t *c = make_object(CLOSURE);
  if (!c)
    return NULL;
  
  c->data.closure.env = env;
  c->data.closure.args = args;
  c->data.closure.code = code;
  c->data.closure.name = NULL;

  return c;
  
}
extern dfsch_object_t* dfsch_named_lambda(dfsch_object_t* env,
                                          dfsch_object_t* args,
                                          dfsch_object_t* code,
                                          dfsch_object_t* name){
  object_t *c = make_object(CLOSURE);
  if (!c)
    return NULL;
  
  c->data.closure.env = env;
  c->data.closure.args = args;
  c->data.closure.code = code;
  c->data.closure.name = name;

  return c;
  
}

// native code

object_t* dfsch_make_primitive(dfsch_primitive_t prim, void *baton){
  object_t* p = make_object(PRIMITIVE);
  if (!p)
    return NULL;

  p->data.primitive.proc = prim;
  p->data.primitive.baton = baton;

  return p;
}

// macros

object_t* dfsch_make_macro(object_t *proc){
  object_t *m = make_object(MACRO);
  
  if (!m)
    return NULL;

  m->data.macro = proc;

  return m;
}
object_t* dfsch_make_form(object_t *proc){
  object_t *m = make_object(FORM);
  
  if (!m)
    return NULL;

  m->data.macro = proc;

  return m;
}



// Error handling

dfsch_object_t* dfsch_make_exception(dfsch_object_t* type, 
				     dfsch_object_t* data){
  object_t* e = make_object(EXCEPTION);
  if (!e)
    return NULL;


  e->data.exception.type = type;
  e->data.exception.data = data;
  e->data.exception.trace = NULL;

  return e;
}

dfsch_object_t* dfsch_throw(char* type, 
                            dfsch_object_t* data,
                            char* location){
  object_t* e = dfsch_make_exception(dfsch_make_symbol(type), data);

  if (location){
    dfsch_exception_push(e,dfsch_make_string(location));
  }

  return e;
}

void dfsch_exception_push(dfsch_object_t* e, dfsch_object_t* item){
  if (!dfsch_object_exception_p(e))
    return;

  if(item){
    e->data.exception.trace = dfsch_cons(item, e->data.exception.trace);
  }
}

dfsch_object_t* dfsch_exception_type(dfsch_object_t* e){
  if (!dfsch_object_exception_p(e))
    return NULL;

  return e->data.exception.type;
}
dfsch_object_t* dfsch_exception_data(dfsch_object_t* e){
  if (!dfsch_object_exception_p(e))
    return NULL;

  return e->data.exception.data;
}
dfsch_object_t* dfsch_exception_trace(dfsch_object_t* e){
  if (!dfsch_object_exception_p(e))
    return NULL;

  return e->data.exception.trace;
}

// Vectors

dfsch_object_t* dfsch_make_vector(size_t length, dfsch_object_t* fill){
  object_t* v = make_object(VECTOR);
  size_t i;

  v->data.vector.length = length;
  v->data.vector.data = GC_MALLOC(sizeof(object_t*) * length);

  for(i = 0; i<length; ++i){
    v->data.vector.data[i] = fill;
  }

  return v;
}
dfsch_object_t* dfsch_vector(size_t count, ...){
  size_t i;
  object_t* v = make_object(VECTOR);;
  va_list al;

  va_start(al,count);

  v->data.vector.length = count;
  v->data.vector.data = GC_MALLOC(sizeof(object_t*) * count);

  for(i = 0; i < count; ++i){
    dfsch_vector_set(v, i, va_arg(al, dfsch_object_t*));
  }

  va_end(al);
  return v;

}

size_t dfsch_vector_length(dfsch_object_t *vector){
  if (!vector || vector->type != VECTOR)
    return 0;

  return vector->data.vector.length;  
}

dfsch_object_t* dfsch_vector_ref(dfsch_object_t *vector, size_t k){
  if (!vector || vector->type != VECTOR)
    DFSCH_THROW("exception:not-a-vector",vector);

  if (vector->data.vector.length <= k)
    DFSCH_THROW("exception:invalid-index",dfsch_make_number(k));
  
  return vector->data.vector.data[k];
}

dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                 dfsch_object_t* obj){
  if (!vector || vector->type != VECTOR)
    DFSCH_THROW("exception:not-a-vector",vector);

  if (vector->data.vector.length <= k)
    DFSCH_THROW("exception:invalid-index",dfsch_make_number(k));
  
  vector->data.vector.data[k] = obj;

  return vector;
}

dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector){
  dfsch_object_t *head; 
  dfsch_object_t *tail;
  size_t i;

  if (!vector || vector->type != VECTOR)
    DFSCH_THROW("exception:not-a-vector",vector);

  if (vector->data.vector.length == 0)
    return NULL;

  head = tail = dfsch_cons(vector->data.vector.data[0], NULL);

  for(i = 1; i< vector->data.vector.length; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(vector->data.vector.data[i],NULL);
    dfsch_set_cdr(tail, tmp);
    tail = tmp;

  }

  return head;
}

dfsch_object_t* dfsch_list_2_vector(dfsch_object_t* list){
  dfsch_object_t* vector;
  size_t i=0;
  if (!list || list->type != PAIR)
    DFSCH_THROW("exception:not-a-pair",list);

  vector = dfsch_make_vector(dfsch_list_length(list),NULL);

  while (list && list->type == PAIR){
    vector->data.vector.data[i] = list->data.pair.car;
    list = list->data.pair.cdr;
    i++;
  }

  return vector;
}

// Native data

dfsch_object_t* dfsch_make_native_data(void *data, 
					      dfsch_object_t *type){
  object_t* n = make_object(NATIVE);
  
  n->data.native.type = type;
  n->data.native.data = data;

  return n;
}
void* dfsch_native_data(dfsch_object_t *object, dfsch_object_t* type){

  if (!object || object->type!=NATIVE || object->data.native.type != type)
    return NULL;
  
  return object->data.native.data;
}
dfsch_object_t* dfsch_native_data_type(dfsch_object_t *object){
  if (!object || object->type!=NATIVE)
    return NULL;

  return object->data.native.type;

}



char* dfsch_obj_write(dfsch_object_t* obj, int max_depth){
  if (!obj){
    return "()";
  }

  if (max_depth==0){
    return "...";
  }

  switch (obj->type){
  case NUMBER:
    {
      char  *s = GC_malloc(64);   
      // 64 bytes should be enought, even for 128 bit machines ^_~
      snprintf(s, 64, "%lf", obj->data.number);
      return s;
    }
  case SYMBOL:
    {
      return stracpy(dfsch_symbol(obj));
    }
  case STRING:
    return straquote(obj->data.string);
  case PRIMITIVE:
    return "<native-code>";
  case CLOSURE:
    if (obj->data.closure.name){
      str_list_t *l = sl_create();

      sl_append(l, "<closure: ");
      sl_append(l, dfsch_obj_write(obj->data.closure.name, max_depth-1));
      sl_append(l, ">");
      
      return sl_value(l);
    }
    return "<closure>";
  case MACRO:
    {
      str_list_t *l = sl_create();
      
      sl_append(l, "<macro: ");
      sl_append(l, dfsch_obj_write(obj->data.macro, max_depth-1));
      sl_append(l, ">");
      
      return sl_value(l);
    }
  case FORM:
    {
      str_list_t *l = sl_create();
      
      sl_append(l, "<special-form: ");
      sl_append(l, dfsch_obj_write(obj->data.macro, max_depth-1));
      sl_append(l, ">");
      
      return sl_value(l);
    }
  case NATIVE:
    {
      str_list_t *l = sl_create();
      
      sl_append(l, "<native-data: ");
      sl_append(l, dfsch_obj_write(obj->data.native.type, max_depth-1));
      sl_append(l, ">");
      
      return sl_value(l);
    }
  case EXCEPTION:
    {
      str_list_t *l = sl_create();
      
      sl_append(l, "<exception: ");
      sl_append(l, dfsch_obj_write(obj->data.exception.type, max_depth-1));
      sl_append(l, " . ");
      sl_append(l, dfsch_obj_write(obj->data.exception.data,
                                   max_depth-1));
      sl_append(l, " @ ");
      sl_append(l, dfsch_obj_write(obj->data.exception.trace,
                                   max_depth-1));
      sl_append(l, ">");
      
      return sl_value(l);
    } 
  case PAIR: 
    // TODO: at least semi-iterative solution? 
    {

      if (obj->data.pair.cdr && obj->data.pair.cdr->type!=PAIR){

        str_list_t* l = sl_create();

	sl_append(l, "(");
	sl_append(l, dfsch_obj_write(obj->data.pair.car,
                                     max_depth-1));

        sl_append(l," . ");
        sl_append(l, dfsch_obj_write(obj->data.pair.cdr,
                                     max_depth-1));
        sl_append(l, ")");
        return sl_value(l);
      }
      {
	str_list_t* l = sl_create();
	object_t* i=obj;
	
	sl_append(l,"(");

	while (i && i->type==PAIR){
	  
	  sl_append(l, dfsch_obj_write(i->data.pair.car,max_depth-1));
	  i = i->data.pair.cdr;

	  if (i)
	    sl_append(l," ");
	    
	}

	if (i){
	  sl_append(l, ". ");
	  sl_append(l, dfsch_obj_write(i,max_depth-1));
	}

	sl_append(l,")");
        return sl_value(l);
      }
    }
  case VECTOR:
    {
	str_list_t* l= sl_create();
        size_t i;
        
        sl_append(l,"#(");

        for(i = 0; i < obj->data.vector.length-1; ++i){
          sl_append(l, dfsch_obj_write(obj->data.vector.data[i],
                                          max_depth-1));
          sl_append(l, " ");
        }

        sl_append(l, dfsch_obj_write(obj->data.vector.data
                                        [obj->data.vector.length-1],
                                        max_depth-1));

        sl_append(l,")");
        return sl_value(l);

    }
  default:
    {
      return "<unknown-object>";
    }
  }
}

dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent){
  return dfsch_cons(dfsch_hash_make(NULL), parent);
}

object_t* dfsch_lookup(object_t* name, object_t* env){
  object_t *i;

  if (!env || env->type!=PAIR){
    DFSCH_RETHROW(env);
    DFSCH_THROW("exception:not-a-pair",env);
  }

  i = env;
  while (i && i->type==PAIR){
    object_t* ret = dfsch_hash_ref(i->data.pair.car, name);
    if (ret){
      return dfsch_car(ret);
    }

    i = i->data.pair.cdr;
  }
  

  DFSCH_THROW("exception:unbound-variable",name);
  

}


object_t* dfsch_set(object_t* name, object_t* value, object_t* env){
  object_t *i, *ie;
  if (!env || env->type!=PAIR){
    DFSCH_RETHROW(env);
    DFSCH_THROW("exception:not-a-pair",env);
  }

  i = env;
  while (i && i->type==PAIR){
    if(dfsch_hash_set_if_exists(i->data.pair.car, name, value))
      return value;

    i = i->data.pair.cdr;
  }
  

  DFSCH_THROW("exception:unbound-variable",name);
}


object_t* dfsch_define(object_t* name, object_t* value, object_t* env){
  if (!env || env->type!=PAIR)
    DFSCH_THROW("exception:not-a-pair",env);

  dfsch_hash_set(env->data.pair.car, name, value);  

  return value;

}


static object_t* eval_list(object_t *list, object_t* env){
  object_t *i;
  object_t *f=NULL;
  object_t *t, *p;
  object_t *r; 

  if (!list)
    return NULL;

  if (list->type!=PAIR){
    return dfsch_eval(list,env);
  }

  i = list;
  while (i && i->type==PAIR){
    r = dfsch_eval(i->data.pair.car,env);
    if (dfsch_object_exception_p(r)){
      return r;
    }

    t = dfsch_cons(r,NULL);
    if (f){
      dfsch_set_cdr(p,t);
      p = t;
    }else{
      f = p = t;
    }

    i=i->data.pair.cdr;
  }

  return f;
}

dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env){
 start:

  if (!exp) 
    return NULL;

  switch (exp->type){
  case PAIR:
    {

      object_t *f = dfsch_eval(exp->data.pair.car,env);
     
      if (!f)
	DFSCH_THROW("exception:not-a-procedure-or-macro", f);
	
 
      switch(f->type){
      case FORM:
	return dfsch_apply(f->data.macro,     
			   dfsch_cons(env,
				      exp->data.pair.cdr));
      case MACRO:
        return dfsch_eval_proc(dfsch_apply(f->data.macro,
                                           dfsch_cons(env, 
                                                      exp->data.pair.cdr)),
                               env);
	
      case CLOSURE:
      case PRIMITIVE:
	return dfsch_apply(f, 
			   eval_list(exp->data.pair.cdr,env));
      }
      
      DFSCH_RETHROW(f);
      DFSCH_THROW("exception:not-a-procedure-or-macro", f);

      
    }
  case SYMBOL:
    return dfsch_lookup(exp,env);
  default:
    return exp;
  }
}

static object_t* lambda_extend(object_t* fa, object_t* aa, object_t* env){
  object_t* i_f=fa;
  object_t* i_a=aa;
  object_t* ext_env = dfsch_new_frame(env);

  while ((i_f && i_f->type==PAIR) &&
	 (i_a && i_a->type==PAIR)){

    dfsch_define(i_f->data.pair.car, i_a->data.pair.car, ext_env);

    i_f = i_f->data.pair.cdr;
    i_a = i_a->data.pair.cdr;
    
  }

  if (i_f && i_f->type==SYMBOL){

    dfsch_define(i_f, i_a, ext_env);
  }

  if (!i_a  && i_f)
      DFSCH_THROW("exception:too-few-arguments", aa);
  if (!i_f && i_a) 
      DFSCH_THROW("exception:too-many-arguments", aa);
  

  return ext_env;
}

dfsch_object_t* dfsch_eval_proc(dfsch_object_t* code, dfsch_object_t* env){
  object_t *i, *r=NULL;

  /*
   * Non-trivial hack is needed here in order to support tail-recursion (at
   * least for most evident cases).
   *
   * There are two simple ways how to achieve this:
   * 1) Pass escape continuation to anything called from here and descend here
   *    in case of code like return eval(foo)
   * 2) Integrate code for apply and other functions here and do tail recursion
   *    explicitly
   *
   * First way could be extended to work even throught native functions stack 
   * frames, but that seems unnecesary.
   */

  if (!env)
    return NULL;
  if (env->type==EXCEPTION)
    return env;
  if (!code)
    return NULL;
  if (code->type==EXCEPTION)
    return env;


  i = code;

  while (i && i->type==PAIR ){
    object_t* exp = i->data.pair.car; 

    r = dfsch_eval(exp,env);

    if (dfsch_object_exception_p(r)){
      return r;
    }
   
    i = i->data.pair.cdr;
  }

  
  return r;
}

dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args){
  if (!proc)
    return NULL;
  if (proc->type==EXCEPTION)
    return proc;
  if (dfsch_object_exception_p(args))
    return args;

  switch (proc->type){
  case CLOSURE:
    {
      object_t* r = dfsch_eval_proc(proc->data.closure.code,
                                    lambda_extend(proc->data.closure.args,
                                                  args,
                                                  proc->data.closure.env));

      if (dfsch_object_exception_p(r)){
        dfsch_exception_push(r, proc->data.closure.name);
      }
      

      return r;
    }
  case PRIMITIVE:
    return (*proc->data.primitive.proc)(proc->data.primitive.baton,args);
  default:
    DFSCH_RETHROW(proc);
    DFSCH_THROW("exception:not-a-procedure", proc);

  }  
}

dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, dfsch_object_t* arg){
  if (dfsch_object_pair_p(arg)){
    object_t* car = dfsch_car(arg);
    object_t* cdr = dfsch_cdr(arg);

    if (car == dfsch_sym_unquote() && dfsch_object_pair_p(cdr)){
      return dfsch_eval(dfsch_car(cdr), env);
    }else if (dfsch_object_pair_p(car)){
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

#define NEED_ARGS(args,count) \
  if (dfsch_list_length(args)!=(count)) \
    DFSCH_THROW("exception:wrong-number-of-arguments",(args));
#define MIN_ARGS(args,count) \
  if (dfsch_list_length(args)<(count)) \
    DFSCH_THROW("exception:too-few-arguments", (args));

static object_t* native_env(void *baton, object_t* args){
  return baton;
}
static object_t* native_macro_env(void *baton, object_t* args){
  NEED_ARGS(dfsch_cdr(args),0);  
  return dfsch_car(args);
}

dfsch_ctx_t* dfsch_make_context(){
  dfsch_ctx_t* ctx=GC_malloc(sizeof(dfsch_ctx_t));
  if (!ctx)
    return NULL;

  gsh_check_init();

  ctx->env = dfsch_new_frame(NULL);

  dfsch_ctx_define(ctx, "top-level-environment", 
                   dfsch_make_primitive(&native_env, ctx->env));
  dfsch_ctx_define(ctx, "current-environment", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_env,
							 NULL)));

  dfsch_native_register(ctx);

  return ctx;
}
dfsch_object_t* dfsch_ctx_eval(dfsch_ctx_t* ctx, dfsch_object_t* exp){
  return dfsch_eval(exp, ctx->env);
}
extern dfsch_object_t* dfsch_ctx_eval_list(dfsch_ctx_t* ctx, 
					   dfsch_object_t* list){
  DFSCH_RETHROW(list);
  return dfsch_eval_proc(list, ctx->env);
}

dfsch_object_t* dfsch_ctx_lambda(dfsch_ctx_t *ctx,
                      dfsch_object_t* args,
                      dfsch_object_t* code){
  return dfsch_lambda(ctx->env, args, code);
}

int dfsch_ctx_define(dfsch_ctx_t *ctx, 
                     char *name, 
                     dfsch_object_t *obj){
  
  return !dfsch_object_exception_p(dfsch_define(dfsch_make_symbol(name),
                                                obj,
                                                ctx->env));
  
}
dfsch_object_t* dfsch_ctx_lookup(dfsch_ctx_t *ctx, char *name){
  return dfsch_lookup(ctx->env,dfsch_make_symbol(name));
}
dfsch_object_t* dfsch_ctx_environment(dfsch_ctx_t *ctx){
  return ctx->env;
}
