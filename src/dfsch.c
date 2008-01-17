/*
 * dfsch - dfox's quick and dirty scheme implementation
 * Copyright (C) 2005-2008 Ales Hakl
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
#include <dfsch/magic.h>
#include "util.h"
#include "internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include "types.h"

//#define ALLOC_DEBUG

#ifdef ALLOC_DEBUG
static int obj_count = 0;
static int obj_size = 0;
#endif

dfsch_object_t* dfsch_make_object_var(const dfsch_type_t* type, size_t size){
  object_t* o = GC_MALLOC(type->size + size);
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

object_t* dfsch_make_object(const dfsch_type_t* type){
  return dfsch_make_object_var(type, 0);
}


int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b){
  return (a==b);
}

int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b){
  if (a==b)
    return 1;

  if (!a || !b)
    return 0;

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

static size_t ptr_hash(dfsch_object_t* ptr){
  size_t a = (size_t)ptr;        
  size_t b = (size_t)ptr >> 16 | (size_t)ptr << 16;

  a ^= b >> 2;
  b ^= a >> 3;
  a ^= b << 5;
  b ^= a << 7;
  a ^= b >> 11;
  b ^= a >> 13;
  a ^= b << 17;
  b ^= a << 23;
  
  return b ^ a;
}

static size_t hash_combine(size_t a, size_t b){
  a ^= b >> 2;
  b ^= a >> 3;
  a ^= b << 5;
  b ^= a << 7;
  a ^= b >> 11;
  b ^= a >> 13;
  a ^= b << 17;
  b ^= a << 23;
  
  return b ^ a;
}


uint32_t dfsch_hash(dfsch_object_t* obj){
  if (!obj){
    return 0;
  }
  if (!obj->type || !obj->type->hash){
    return ptr_hash(obj);
  }

  return obj->type->hash(obj);
}

dfsch_type_t* dfsch_type_of(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(obj);
}

dfsch_object_t* dfsch_superclass(dfsch_object_t* obj){
  if (!DFSCH_INSTANCE_P(obj, DFSCH_STANDARD_TYPE)){
    dfsch_error("exception:not-a-standard-type", obj);
  }
  
  return (dfsch_object_t*)((dfsch_type_t*)obj)->superclass;
}

int dfsch_superclass_p(dfsch_type_t* sub, dfsch_type_t* super){
  if (sub == super)
    return 1;

  while (sub){
    sub = sub->superclass;
    if (sub==super){
      return 1;
    }
  }

  return 0;
}
int dfsch_instance_p(dfsch_object_t* obj, dfsch_type_t* type){
  return dfsch_superclass_p(DFSCH_TYPE_OF(obj), type);
}

static char* atype_write(dfsch_type_t* t, int max_depth, int readable){
    str_list_t* l = sl_create();

    sl_append(l, "#<abstract-type ");
    sl_append(l, saprintf("%p", t));
    
    sl_append(l, " ");
    sl_append(l, t->name);

    sl_append(l,">");
    
    return sl_value(l);
}

const dfsch_type_t dfsch_abstract_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_type_t),
  "abstract-type",
  NULL,
  (dfsch_type_write_t)atype_write,
  NULL
};

static char* type_write(dfsch_type_t* t, int max_depth, int readable){
    str_list_t* l = sl_create();

    sl_append(l, "#<standard-type ");
    sl_append(l, saprintf("%p", t));
    
    sl_append(l, " ");
    sl_append(l, t->name);
    sl_append(l, " instance-size: ");
    sl_append(l, saprintf("%d", t->size));

    sl_append(l,">");
    
    return sl_value(l);
}

const dfsch_type_t dfsch_standard_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_type_t),
  "standard-type",
  NULL,
  (dfsch_type_write_t)type_write,
  NULL
};

const dfsch_type_t list_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "list",
  NULL,
  NULL,
  NULL,
  NULL
};


const dfsch_type_t dfsch_empty_list_type = {
  DFSCH_STANDARD_TYPE,
  &list_type,
  0,
  "empty-list",
  NULL,
  NULL,
  NULL,
  NULL
};

static int pair_equal_p(pair_t*, pair_t*);
static char* pair_write(pair_t*, int, int);
static size_t pair_hash(pair_t* p);

static const dfsch_type_t pair_type = {
  DFSCH_STANDARD_TYPE,
  &list_type,
  sizeof(pair_t), 
  "pair",
  (dfsch_type_equal_p_t)pair_equal_p,
  (dfsch_type_write_t)pair_write,
  NULL,
  (dfsch_type_hash_t)pair_hash
};
#define PAIR (&pair_type)

static int pair_equal_p(pair_t*a, pair_t*b){
  return dfsch_equal_p(a->car,b->car) && dfsch_equal_p(a->cdr, b->cdr);
}
static size_t pair_hash(pair_t* p){
  return hash_combine(dfsch_hash(p->car), dfsch_hash(p->cdr));
}
static char* pair_write(pair_t*p, int max_depth, int readable){
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

static char* symbol_write(symbol_t*, int, int);
static const dfsch_type_t symbol_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(symbol_t), 
  "symbol",
  NULL,
  (dfsch_type_write_t)symbol_write,
  NULL
};
#define SYMBOL (&symbol_type) 
static char* symbol_write(symbol_t* s, int max_depth, int readable){
  if (s->data){
    return s->data;
  } else {
    return saprintf("#<gensym %p>", s);
  }
}


const dfsch_type_t dfsch_primitive_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(primitive_t),
  "primitive",
  NULL,
  NULL,
  NULL,
  NULL
};
#define PRIMITIVE (&dfsch_primitive_type)

static char* closure_write(closure_t* c, int max_depth, int readable){
    str_list_t* l = sl_create();

    if (c->code == c->orig_code){
      sl_append(l, "#<function ");
    } else {
      sl_append(l, "#<compiled-function ");
    }
    sl_append(l, saprintf("%p", c));
    
    if (c->name){
      sl_append(l, " ");
      sl_append(l, dfsch_obj_write(c->name, max_depth-1, 0));
    }

    sl_append(l, " ");
    sl_append(l, dfsch_obj_write(c->args, max_depth-1, 0));

    sl_append(l,">");
    
    return sl_value(l);
}

static const dfsch_type_t closure_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(closure_t),
  "function",
  NULL,
  (dfsch_type_write_t)closure_write,
  NULL
};
#define CLOSURE (&closure_type)

static const dfsch_type_t macro_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(macro_t),
  "macro",
  NULL,
  NULL,
  NULL
};
#define MACRO (&macro_type)

const dfsch_type_t dfsch_form_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_form_t),
  "form",
  NULL,
  NULL,
  NULL
};
#define FORM (&dfsch_form_type)

static const dfsch_type_t exception_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(exception_t),
  "exception",
  NULL,
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

static size_t vector_hash(vector_t* v){
  size_t i;
  size_t ret = 0;

  for (i=0; i<v->length; i++){
    ret = hash_combine(ret, dfsch_hash(v->data[i]));
  }

  return ret;
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
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(vector_t),
  "vector",
  (dfsch_type_equal_p_t)vector_equal_p,
  (dfsch_type_write_t)vector_write,
  NULL,
  (dfsch_type_hash_t)vector_hash
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
int dfsch_list_p(dfsch_object_t* obj){
  if (!obj)
    return 1;
  if (obj->type != PAIR)
    return 0;
  return dfsch_list_length(obj) > 0;
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
  return (obj->type == PRIMITIVE || obj->type == CLOSURE) || 
    (obj->type && obj->type->apply);
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
    dfsch_error("exception:not-a-pair",pair);

  return ((pair_t*)pair)->car;
}
dfsch_object_t* dfsch_cdr(dfsch_object_t* pair){
  if (!pair || pair->type!=PAIR)
    dfsch_error("exception:not-a-pair",pair);

  return ((pair_t*)pair)->cdr;
}

dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
			      dfsch_object_t* car){
  if (!pair || pair->type!=PAIR)
    dfsch_error("exception:not-a-pair",pair);

  ((pair_t*)pair)->car = car;
  
  return pair;

}
dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
			      dfsch_object_t* cdr){
  if (!pair || pair->type!=PAIR)
    dfsch_error("exception:not-a-pair",pair);
  
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
    dfsch_error("exception:not-a-list", list);
  return len;
}

dfsch_object_t* dfsch_list_item(dfsch_object_t* list, int index){
  pair_t* it = (pair_t*)list;
  int i;
  for (i=0; i<index; ++i){
    if (it && it->type == PAIR){
      it = (pair_t*)it->cdr;
    }else{
      dfsch_error("exception:no-such-item",dfsch_make_number_from_long(index));
    }
  }
  return dfsch_car((object_t*)it);
}

dfsch_object_t* dfsch_list_from_array(dfsch_object_t** array, size_t length){
  pair_t *head; 
  pair_t *tail;
  size_t i;

  if (length == 0)
    return NULL;

  head = tail = (pair_t*)dfsch_cons(array[0], NULL);

  for(i = 1; i < length; ++i){
    object_t *tmp;
    
    tmp = dfsch_cons(array[i],NULL);
    tail->cdr = tmp;
    tail = (pair_t*)tmp;

  }

  return (object_t*)head;
}
dfsch_object_t** dfsch_list_as_array(dfsch_object_t* list, size_t* length){
  pair_t* j = (pair_t*)list;
  size_t i=0;
  size_t len;
  object_t** data;

  len = dfsch_list_length_check(list);
  data = GC_MALLOC(sizeof(object_t*)*len);
  
  while (j && j->type == PAIR){
    if (i >= len){
      break; /* Can happen due to race condition in user code */
    }
    data[i] = j->car;
    j = (pair_t*)j->cdr;
    i++;
  }

  if (length){
    *length = len;
  }

  return data;
}

dfsch_object_t* dfsch_zip(dfsch_object_t* llist){
  size_t len;
  object_t** args = dfsch_list_as_array(llist, &len);

  pair_t* shead;
  pair_t* stail;

  pair_t *head = NULL; 
  pair_t *tail;

  pair_t* tmp;
  size_t i;

  if (len == 0){
    return NULL;
  }

  while(1){
    shead = NULL;

    for (i = 0; i<len; i++){
      if (!args[i]){
	if (i != 0){
          dfsch_error("exception:not-a-list-of-same-length-lists", llist);
	}
	goto out;
      }
      if (args[i]->type != PAIR){
	dfsch_error("exception:not-a-pair", args[i]);
      }

      tmp = (pair_t*)dfsch_cons(((pair_t*)(args[i]))->car, NULL);
      if (shead){
        stail->cdr = (object_t*) tmp;
      } else {
        shead = tmp;
      }
      stail = tmp;

      args[i] = ((pair_t*)(args[i]))->cdr;
    }


    tmp = (pair_t*)dfsch_cons((object_t*)shead, NULL);
    if (head){
      tail->cdr = (object_t*)tmp;
    } else {
      head = tmp;
    }
    tail = tmp;

  }
  

 out:
  for (i = 0; i<len; i++){
    if (args[i]){
      dfsch_error("exception:not-a-list-of-same-length-lists", llist);
    }
  }
  
  return (object_t*)head;
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
      dfsch_error("exception:not-a-pair", (object_t*)j);

    i = (pair_t*)i->cdr;
  }

  if (!i || i->type != PAIR)
    dfsch_error("exception:not-a-pair", (object_t*)i);
  /*  if (i->car && i->car->type != PAIR)
      dfsch_error("exception:not-a-pair", i->car);*/

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
    dfsch_error("exception:not-a-pair", (object_t*)i);


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
    dfsch_error("exception:not-a-pair", (object_t*)i);


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
    dfsch_error("exception:not-a-pair", (object_t*)i);

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
    dfsch_error("exception:not-a-pair", (object_t*)i);

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
    dfsch_error("exception:not-a-pair", (object_t*)i);

  return NULL;
}


// Alists 

dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
			    dfsch_object_t *alist){
  pair_t* i;
  
  if (!alist || alist->type!=PAIR)
    dfsch_error("exception:not-a-pair",alist);

  i=(pair_t*)alist;
  
  while (i && i->type==PAIR){
    if (!i->car || i->car->type!=PAIR){
      dfsch_error("exception:not-a-alist",(object_t*)alist);
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
    dfsch_error("exception:not-a-pair",alist);

  i=(pair_t*)alist;
  
  while (i && i->type==PAIR){
    if (!i->car || i->car->type!=PAIR){
      dfsch_error("exception:not-a-alist",alist);
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
    dfsch_error("exception:not-a-pair",alist);

  i=(pair_t*)alist;
  
  while (i && i->type==PAIR){
    if (!i->car || i->car->type!=PAIR){
      dfsch_error("exception:not-a-alist",alist);
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


static void gsh_check_init(){
  if (gsh_init)
    return;

  memset(global_symbol_hash, 0, sizeof(hash_entry_t*)*HASH_SIZE);
  gsh_init = 1;
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
  symbol_t *s;

  s = (symbol_t*)dfsch_make_object(SYMBOL);

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
  if (!symbol || symbol->type != SYMBOL)
    dfsch_error("exception:not-a-symbol", symbol);

  free_symbol((symbol_t*)symbol);
}

dfsch_object_t* dfsch_gensym(){
  symbol_t *s = (symbol_t*)dfsch_make_object(SYMBOL);

  s->data = NULL;

  return (object_t*)s;
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

  if (!s)
    s = make_symbol(symbol);

  pthread_mutex_unlock(&symbol_lock);

  return (object_t*)s;

}
char* dfsch_symbol(dfsch_object_t* symbol){
  if (!symbol || symbol->type!=SYMBOL)
    dfsch_error("exception:not-a-symbol", symbol);

  return ((symbol_t*)symbol)->data;
}


int dfsch_compare_symbol(dfsch_object_t* symbol,
                         char* string){
  return (ascii_strcasecmp(string, dfsch_symbol(symbol)) == 0);
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
  c->orig_code = code;
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
  c->orig_code = code;
  c->name = name;

  return (object_t*)c;
  
}

// native code
object_t* dfsch_make_primitive(dfsch_primitive_impl_t prim, void *baton){
  return dfsch_make_primitive_flags(prim, baton, 0);
}

object_t* dfsch_make_primitive_flags(dfsch_primitive_impl_t prim, 
                                     void *baton, 
                                     int flags){
  primitive_t* p = (primitive_t*)dfsch_make_object(PRIMITIVE);
  if (!p)
    return NULL;

  p->proc = prim;
  p->baton = baton;
  p->flags = 0;
  
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
dfsch_object_t* dfsch_make_form(dfsch_form_impl_t impl,
                                dfsch_form_compile_t compile,
                                void* baton,
                                char* name){
  dfsch_form_t *f = (dfsch_form_t*)dfsch_make_object(FORM);
  
  f->impl = impl;
  f->compile = compile;
  f->baton = baton;
  f->name = name;

  return (object_t*)f;
}


static pthread_key_t thread_key;
static pthread_once_t thread_once = PTHREAD_ONCE_INIT;


/*
 * It seems so volatile really isn't needed around this magic, only automatic
 * variables that have been _CHANGED_ since call to setjmp(3) are indeterminate
 * and there are none such. Actually only thing that happen in these functions
 * between setjmp(3) and opportunity to call longjmp(3) (from corresponding 
 * wrapper function) is function call. This is only case of undefined behavior
 * related to "proper" use of setjmp(3)/longjmp(3) in IEEE 1003.1.
 *
 * Beware that exceptions and escape-continuations propagate using "separate"
 * mechanisms, both honor unwind-protect, but this causes some complications
 * in unwind-protect handling (because unwind-protect is implemented as C 
 * macros in magic.h and uses setjmp/longjmp).
 */

static void thread_info_destroy(void* ptr){
  if (ptr)
    GC_FREE(ptr);
}
static void thread_key_alloc(){
  pthread_key_create(&thread_key, thread_info_destroy);
}
dfsch__thread_info_t* dfsch__get_thread_info(){
  dfsch__thread_info_t *ei;
  pthread_once(&thread_once, thread_key_alloc);
  ei = pthread_getspecific(thread_key);
  if (!ei){
    ei = GC_MALLOC_UNCOLLECTABLE(sizeof(dfsch__thread_info_t)); 
    ei->exception_ret = NULL;
    ei->stack_trace = NULL;
    ei->break_type = NULL;
    pthread_setspecific(thread_key, ei);
  }
  return ei;
}

dfsch_object_t* dfsch_get_stack_trace(){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
  return ti->stack_trace;
}

void dfsch_raise(dfsch_object_t* exception){

  dfsch__thread_info_t *ei = dfsch__get_thread_info();

  if (!ei->exception_ret){
    fputs(dfsch_exception_write(exception),stderr);        
    abort();
  }

  ei->exception_obj = exception;
  longjmp(*ei->exception_ret, 1);    
}

dfsch_object_t* dfsch_try(dfsch_object_t* handler,
                          dfsch_object_t* finally,
                          dfsch_object_t* thunk){

  dfsch_object_t *r = NULL;

  DFSCH_TRY
    r = dfsch_apply(thunk, NULL);
  DFSCH_CATCH(e)
    if (handler){
      r = dfsch_apply(handler, dfsch_list(1, e));
    }
  DFSCH_END_TRY

  if (finally){
    dfsch_apply(finally, NULL);
  }

  return r;
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

dfsch_object_t* dfsch_error(char* type, 
                            dfsch_object_t* data){
  object_t* e = dfsch_make_exception(dfsch_make_symbol(type), data,
                                     dfsch_get_stack_trace());

  dfsch_raise(e);
    
}
dfsch_object_t* dfsch_break(char* type){
  dfsch__thread_info_t *ti = dfsch__get_thread_info();
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
dfsch_object_t* dfsch_exception_stack_trace(dfsch_object_t* e){
  if (!dfsch_exception_p(e))
    return NULL;

  return ((exception_t*)e)->stack_trace;
}

// Vectors

dfsch_object_t* dfsch_make_vector(size_t length, dfsch_object_t* fill){
  vector_t* v;
  size_t i;

  v = dfsch_make_object_var(VECTOR, length * sizeof(dfsch_object_t*));
  v->length = length;

  for(i = 0; i<length; ++i){
    v->data[i] = fill;
  }

  return (object_t*)v;
}
dfsch_object_t* dfsch_vector(size_t count, ...){
  size_t i;
  vector_t* v = 
    (vector_t*)dfsch_make_object_var(VECTOR, count * sizeof(dfsch_object_t*));
  va_list al;

  va_start(al,count);

  v->length = count;

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

dfsch_object_t** dfsch_vector_as_array(dfsch_object_t *vector, size_t *length){
  if (!vector || vector->type != VECTOR)
    dfsch_error("exception:not-a-vector",vector);

  if (length){
    *length = ((vector_t*)vector)->length;
  }

  return ((vector_t*)vector)->data;
}

dfsch_object_t* dfsch_vector_from_array(dfsch_object_t **array, 
                                        size_t length){
  vector_t* v = 
    (vector_t*)dfsch_make_object_var(VECTOR, 
                                     sizeof(dfsch_object_t*) * length);

  v->length = length;
  memcpy(v->data, array, sizeof(object_t*) * length);

  return (object_t*)v;
}

dfsch_object_t* dfsch_vector_ref(dfsch_object_t *vector, size_t k){
  if (!vector || vector->type != VECTOR)
    dfsch_error("exception:not-a-vector",vector);

  if (((vector_t*)vector)->length <= k)
    dfsch_error("exception:invalid-index",dfsch_make_number_from_long(k));
  
  return ((vector_t*)vector)->data[k];
}

dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                 dfsch_object_t* obj){
  if (!vector || vector->type != VECTOR)
    dfsch_error("exception:not-a-vector",vector);

  if (((vector_t*)vector)->length <= k)
    dfsch_error("exception:invalid-index",dfsch_make_number_from_long(k));
  
  ((vector_t*)vector)->data[k] = obj;

  return vector;
}

dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector){

  if (!vector || vector->type != VECTOR)
    dfsch_error("exception:not-a-vector",vector);

  return dfsch_list_from_array(((vector_t*)vector)->data, 
                               ((vector_t*)vector)->length);
}

dfsch_object_t* dfsch_list_2_vector(dfsch_object_t* list){
  vector_t* vector;
  size_t length;
  dfsch_object_t** array;
  array = dfsch_list_as_array(list, &length);
  vector = (vector_t*)dfsch_make_object_var(VECTOR, 
                                            length * sizeof(dfsch_object_t*));
  vector->length = length;
  memcpy(vector->data, array, sizeof(dfsch_object_t*) * length);
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
  char* res;
  DFSCH_TRY {
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
    
    res = sl_value(l);
  } DFSCH_CATCH(e) {
    res = "Exception occured during formatting of exception message.\n\n";
  } DFSCH_END_TRY
  return res;
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
      dfsch_error("read:syntax-error",NULL);
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


  return dfsch_new_frame_from_hash(parent, dfsch_hash_make(DFSCH_HASH_EQ));
}
dfsch_object_t* dfsch_new_frame_from_hash(dfsch_object_t* parent, 
                                          dfsch_object_t* hash){
  dfsch_object_t* frame = dfsch_cons(hash, parent);
  return frame;
}

object_t* dfsch_lookup(object_t* name, object_t* env){
  pair_t *i;

  if (!env || env->type!=PAIR){
    dfsch_error("exception:not-a-pair",env);
  }


  i = (pair_t*)env;
  while (i && i->type==PAIR){

    object_t* ret = dfsch_hash_ref(i->car, name);
    if (ret){
      return dfsch_car(ret);
    }

    i = (pair_t*)i->cdr;
  }
  
  dfsch_error("exception:unbound-variable", dfsch_cons(name, env));
}
object_t* dfsch_env_get(object_t* name, object_t* env){
  pair_t *i;

  if (!env || env->type!=PAIR){
    dfsch_error("exception:not-a-pair",env);
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
    dfsch_error("exception:not-a-pair",env);
  }

  i = (pair_t*)env;
  while (i && i->type==PAIR){
    if(dfsch_hash_set_if_exists(i->car, name, value))
      return value;

    i = (pair_t*)i->cdr;
  }
  

  dfsch_error("exception:unbound-variable",name);
}
void dfsch_unset(object_t* name, object_t* env){
  pair_t *i;
  if (!env || env->type!=PAIR){
    dfsch_error("exception:not-a-pair",env);
  }

  i = (pair_t*)env;
  while (i && i->type==PAIR){
    if(dfsch_hash_unset(i->car, name))
      return;

    i = (pair_t*)i->cdr;
  }
  

  dfsch_error("exception:unbound-variable",name);
}


object_t* dfsch_define(object_t* name, object_t* value, object_t* env){
  if (!env || env->type!=PAIR)
    dfsch_error("exception:not-a-pair",env);

  dfsch_hash_set(((pair_t*)env)->car, name, value);  

  return value;

}

dfsch_object_t* dfsch_macro_expand(dfsch_object_t* macro,
                                   dfsch_object_t* args){
  if (!DFSCH_INSTANCE_P(macro, MACRO)){
    dfsch_error("exception:not-a-macro", macro);
  }

  return dfsch_apply(((macro_t*)macro)->proc, args);
}

// Evaluator

/*
 * There are some kinds of structures that are passed extensively inside the 
 * evaluator, but are not exposed to public API much. Idea there is that 
 * dfsch__get_thread_info() can be somewhat slow (on linux, it isn't 
 * noticeably slow, but who knows) and thus it is not exactly bad idea to 
 * cache it's result, but passing this value to user code causes marginal 
 * speedup at cost of having one additional argument that is not useful in 
 * any meaningful way to user code.
 *
 * dfsch_tail_escape_t is used to implement tail recursion, it is used only 
 * in dfsch_eval_proc_impl() but it has to be passed through much of evaluator 
 * and some native functions. General idea is that dfsch_eval_proc_impl stores
 * jmp_buf to it's start here and passes it to dfsch_eval_impl() during 
 * evaluation of last form of procedure body. Native functions have to 
 * implement same mechanism (pass dfsch_tail_escape_t argument only to 
 * functions whose return value is return value of native function itself). 
 * When dfsch_eval_proc_impl() is called with non-NULL tail_escape it simply
 * jumps back to previous activation record. This actually causes slow-down,
 * but enables us to do tail-recursion in simple and consistent way, that 
 * works even through C-code.
 */

struct dfsch_tail_escape_t {
  jmp_buf ret;
  object_t *code;
  object_t *env;
  object_t *proc_name; 
};

typedef dfsch_tail_escape_t tail_escape_t;


static dfsch_object_t* dfsch_eval_proc_impl(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            dfsch_object_t* proc_name,
                                            tail_escape_t* esc,
                                            dfsch__thread_info_t* ti);
static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       dfsch_object_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti);
static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        tail_escape_t* esc,
                                        dfsch__thread_info_t* ti);


static object_t* eval_list(object_t *list, object_t* env, 
                           dfsch__thread_info_t* ti){
  pair_t *i;
  object_t *f=NULL;
  pair_t *t, *p;
  object_t *r; 

  if (!list)
    return NULL;

  if (list->type!=PAIR){
    return dfsch_eval_impl(list,env,NULL,ti);
  }

  i = (pair_t*)list;
  while (i && i->type==PAIR){
    r = dfsch_eval_impl(i->car, env, NULL, ti);

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

dfsch_object_t* dfsch_eval_list(dfsch_object_t* list, dfsch_object_t* env){
  return eval_list(list, env, dfsch__get_thread_info());
}

static dfsch_object_t* dfsch_eval_impl(dfsch_object_t* exp, 
                                       dfsch_object_t* env,
                                       dfsch_tail_escape_t* esc,
                                       dfsch__thread_info_t* ti){
 start:
  
  if (!exp) 
    return NULL;

  if(exp->type == SYMBOL)
    return dfsch_lookup(exp,env);

  if(exp->type==PAIR){
    
    object_t *f = dfsch_eval_impl(((pair_t*)exp)->car, env, NULL, ti);
    
    if (!f)
      dfsch_error("exception:not-a-procedure-or-macro", f);
	
 
    if (f->type == FORM)
      return ((dfsch_form_t*)f)->impl(((dfsch_form_t*)f), 
                                      env, 
                                      ((pair_t*)exp)->cdr, 
                                      esc);

    if (f->type == MACRO)
      return dfsch_eval_impl(dfsch_macro_expand(f, ((pair_t*)exp)->cdr),
			     env,
 			     esc,
			     ti);
      
    return dfsch_apply_impl(f, 
                            eval_list(((pair_t*)exp)->cdr, env, ti),
                            esc,
                            ti);
    
    
  }  
  
  return exp;
}

dfsch_object_t* dfsch_eval_tr(dfsch_object_t* exp, 
                              dfsch_object_t* env,
                              dfsch_tail_escape_t* esc){
  return dfsch_eval_impl(exp, env, esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env){
  return dfsch_eval_impl(exp, env, NULL, dfsch__get_thread_info());
}

static void destructure_impl(pair_t* llist,
                             pair_t* list,
                             dfsch_object_t* hash){
  while ((llist && llist->type==PAIR) &&
	 (list && list->type==PAIR)){

    if (llist->car->type == PAIR){
      destructure_impl(llist->car, list->car, hash);
    } else {
      dfsch_hash_set(hash, llist->car, list->car);
    }

    llist = (pair_t*)llist->cdr;
    list = (pair_t*)list->cdr;
    
  }

  if (llist && llist->type!=PAIR){
    dfsch_hash_set(hash, (object_t*)llist, (object_t*)list);
    return;
  }

  if (!list  && llist){
    dfsch_error("exception:too-few-arguments", dfsch_list(2, 
                                                          llist, 
                                                          list));
  }
  if (!llist && list) {
    dfsch_error("exception:too-many-arguments", dfsch_list(2,
                                                           llist, 
                                                           list));
  }
  

}

dfsch_object_t* dfsch_destructure(dfsch_object_t* arglist,
                                  dfsch_object_t* list){
  object_t* hash = dfsch_hash_make(DFSCH_HASH_EQ);

  destructure_impl((pair_t*)arglist, (pair_t*)list, hash);

  return hash;
}

dfsch_object_t* dfsch_destructuring_bind(dfsch_object_t* arglist, 
                                         dfsch_object_t* list, 
                                         dfsch_object_t* env){
  return dfsch_new_frame_from_hash(env,
                                   dfsch_destructure(arglist,
                                                     list));
}

static dfsch_object_t* dfsch_eval_proc_impl(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            dfsch_object_t* proc_name,
                                            tail_escape_t* esc,
                                            dfsch__thread_info_t* ti){
  pair_t *i;
  object_t *r=NULL;
  tail_escape_t myesc;
  dfsch_object_t *old_frame;
  dfsch_object_t *my_frame;

  if (!env)
    return NULL;
  if (!code)
    return NULL;

  if (ti->break_type){
    char* type = ti->break_type;
    ti->break_type = NULL;
    dfsch_error("exception:break", dfsch_make_symbol(type));
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
      r = dfsch_eval_impl(exp,env,NULL, ti);
    else
      r = dfsch_eval_impl(exp,env,(tail_escape_t*)&myesc, ti);
   
    i = (pair_t*)i->cdr;
  }

  
  ti->stack_trace = old_frame;
  return r;
}

dfsch_object_t* dfsch_eval_proc_tr(dfsch_object_t* code, 
                                   dfsch_object_t* env,
                                   dfsch_object_t* proc_name,
                                   tail_escape_t* esc){
  return dfsch_eval_proc_impl(code, env, proc_name, esc, 
                              dfsch__get_thread_info());
}
dfsch_object_t* dfsch_eval_proc(dfsch_object_t* code, dfsch_object_t* env){
  return dfsch_eval_proc_impl(code, env, NULL, NULL, 
                              dfsch__get_thread_info());
}

static dfsch_object_t* dfsch_apply_impl(dfsch_object_t* proc, 
                                 dfsch_object_t* args,
                                 tail_escape_t* esc,
                                 dfsch__thread_info_t* ti){

  if (!proc)
    return NULL;
  if (!proc->type)
    return NULL;

  /*
   * Two most common cases are written here explicitly (for historical
   * and performance reasons)
   */

  if (proc->type == PRIMITIVE){
    object_t* r = ((primitive_t*)proc)->proc(((primitive_t*)proc)->baton,args,
                                             esc);
    return r;
  }

  if (proc->type == CLOSURE){
    object_t* r = 
      dfsch_eval_proc_impl(((closure_t*)proc)->code,
                           dfsch_destructuring_bind(((closure_t*)proc)->args,
                                                    args,
                                                    ((closure_t*)proc)->env),
                           dfsch_cons(proc, args),
                           esc,
                           ti);
    return r;
  }

  if (proc->type->apply){
    return proc->type->apply(proc, args, esc);
  }

  dfsch_error("exception:not-a-procedure", proc);
   
}

dfsch_object_t* dfsch_apply_tr(dfsch_object_t* proc, 
                               dfsch_object_t* args,
                               tail_escape_t* esc){
  return dfsch_apply_impl(proc, args, esc, dfsch__get_thread_info());
}
dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args){
  return dfsch_apply_impl(proc, args, NULL, dfsch__get_thread_info());
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

DFSCH_DEFINE_FORM_IMPL(current_environment, NULL){
  return env;
}

dfsch_object_t* dfsch_make_context(){
  dfsch_object_t* ctx;

  ctx = dfsch_new_frame(NULL);

  dfsch_define_cstr(ctx, "<standard-type>", DFSCH_STANDARD_TYPE);
  dfsch_define_cstr(ctx, "<abstract-type>", DFSCH_ABSTRACT_TYPE);


  dfsch_define_cstr(ctx, "<list>", &list_type);
  dfsch_define_cstr(ctx, "<pair>", PAIR);
  dfsch_define_cstr(ctx, "<empty-list>", DFSCH_EMPTY_LIST_TYPE);
  dfsch_define_cstr(ctx, "<symbol>", SYMBOL);
  dfsch_define_cstr(ctx, "<primitive>", PRIMITIVE);
  dfsch_define_cstr(ctx, "<function>", CLOSURE);
  dfsch_define_cstr(ctx, "<macro>", MACRO);
  dfsch_define_cstr(ctx, "<form>", FORM);
  dfsch_define_cstr(ctx, "<exception>", EXCEPTION);
  dfsch_define_cstr(ctx, "<vector>", VECTOR);


  dfsch_define_cstr(ctx, "top-level-environment", 
                   dfsch_make_primitive(&native_top_level_environment, ctx));
  dfsch_define_cstr(ctx, "current-environment", 
                    DFSCH_FORM_REF(current_environment));

  dfsch__native_register(ctx);

  return ctx;
}


dfsch_object_t* dfsch_define_cstr(dfsch_object_t *ctx, 
                                 char *name, 
                                 dfsch_object_t *obj){
  
  return dfsch_define(dfsch_make_symbol(name),
                      obj,
                      ctx);
  
}
dfsch_object_t* dfsch_set_cstr(dfsch_object_t *ctx, 
			       char *name, 
			       dfsch_object_t *obj){
  
  return dfsch_set(dfsch_make_symbol(name),
                      obj,
                      ctx);
  
}
dfsch_object_t* dfsch_lookup_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_lookup(dfsch_make_symbol(name), ctx);
}
dfsch_object_t* dfsch_env_get_cstr(dfsch_object_t *ctx, char *name){
  return dfsch_env_get(dfsch_make_symbol(name), ctx);
}
