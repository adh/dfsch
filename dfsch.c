/*
 * dfsch - DFox's quick and dirty scheme implementation
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

/** @file This is implementation of dfsch interpreter. */

#include "dfsch.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
  FLOW_MACRO,
  EXCEPTION // define new types here
} type_t ;

typedef dfsch_object_t object_t;
typedef dfsch_ctx_t context_t;

typedef struct symbol_t symbol_t;

struct symbol_t{
  char *data;
  object_t *next;
  object_t *prev;
};

static object_t *global_symbol_table=NULL;


struct dfsch_ctx_t{
  object_t* env;
};

typedef struct pair_t{
  object_t *car;
  object_t *cdr;
} pair_t;

typedef dfsch_primitive_t primitive_t;

typedef struct closure_t{
  object_t* args;
  object_t* code;
  object_t* env;
} closure_t;

typedef struct exception_t{
  object_t *type;
  object_t *data;
} exception_t; 

struct dfsch_object_t{
  type_t type;
  unsigned int marked;
  union {
    pair_t pair;
    symbol_t symbol;
    double number;
    char* string;
    primitive_t primitive;
    closure_t closure;
    object_t *macro;
    exception_t exception;
  } data;
};


static char* stracat(char* a, char* b){
  size_t s = strlen(a)+strlen(b)+1;
  char* o = malloc(s);
  strncpy(o,a,s);
  strncat(o,b,s);
  return o;
}

static char* strneko(char* a, char* b){ // ^_^
  char * s = stracat(a,b);
  free(a);
  free(b);
  return s;
}
static char* strneko_l(char* a, char* b){ // ^_^
  char * s = stracat(a,b);
  free(a);
  return s;
}
static char* strneko_r(char* a, char* b){ // ^_^
  char * s = stracat(a,b);
  free(b);
  return s;
}
static char* stracpy(char* x){
  char *b;
  size_t s = strlen(x)+1;
  b = malloc(s);
  strncpy(b,x,s);
  return b;
}
static char* strancpy(char* x, size_t n){
  char *b;
  size_t s = n+1;
  b = malloc(s);
  strncpy(b,x,s-1);
  b[s-1]=0;
  return b;
}
static char* straquote(char *s){
  char *b = malloc(strlen(s)*3+3);
  char *i = b;

  *i='"';
  i++;

  while (*s){
    switch (*s){
    case '"':
      i[0]='\\';
      i[1]='"';
      i+=2;
    default:
      *i = *s;
      ++i;
    }
    s++;
  }

  *i='"';
  i[1]=0;

  return b;

}

#define EXCEPTION_CHECK(x) {if (dfsch_object_exception_p(x)) return x;}


static void object_destroy(object_t* obj){
#ifdef OBJ_LIST_DEBUG
  printf(";; Destroy: %p\n",obj);
#endif
  if (!obj)
    return;

  switch (obj->type){
  case STRING:
    free(obj->data.string);
    break;
  case SYMBOL:
    free(obj->data.symbol.data);

    if (obj->data.symbol.prev){
      obj->data.symbol.prev->data.symbol.next = obj->data.symbol.next;
    }else{
      global_symbol_table = obj->data.symbol.next;
    }

    if (obj->data.symbol.next){
      obj->data.symbol.next->data.symbol.prev = obj->data.symbol.prev;
    }

    break;
    
  }

  free(obj);
}

typedef struct objlist_t objlist_t;

struct objlist_t{
  object_t* object;
  objlist_t* next;
};


static objlist_t* gc_objects=NULL;
static objlist_t* gc_roots=NULL;


static void gc_mark(object_t* obj){
#ifdef GC_DEBUG
  printf(";; Mark: %p\n",obj);
#endif

  if (!obj)
    return;

  if (obj->marked)
    return;

  obj->marked = 1;

  switch (obj->type){
  case PAIR:
    gc_mark(obj->data.pair.car);
    gc_mark(obj->data.pair.cdr);
    break;
  case CLOSURE:
    gc_mark(obj->data.closure.args);
    gc_mark(obj->data.closure.code);
    gc_mark(obj->data.closure.env);
    break;
  case MACRO:
  case FLOW_MACRO:
    gc_mark(obj->data.macro);
    break;
  case EXCEPTION:
    gc_mark(obj->data.exception.type);
    gc_mark(obj->data.exception.data);
  }
}

#ifdef OBJ_LIST_DEBUG
static void dump_obj_list(objlist_t* list){
  objlist_t *i = list;
  printf(";; ObjList dump: %p\n",list);

  while (i){
    printf(";; ObjList item: %p => %p\n",i,i->object);
    i = i->next;
  }

}
#endif



int dfsch_gc(){
  objlist_t* i_o=gc_objects;
  objlist_t *j, *d;
  objlist_t* i_r=gc_roots;
  int counter = 0;

  while (i_o){
    i_o->object->marked = 0;
    i_o = i_o->next;
  }

  while (i_r){
    gc_mark(i_r->object);
    i_r = i_r->next;
  }

  i_o=gc_objects;

  j = NULL;

  while (i_o){
#ifdef GC_DEBUG
    printf(";; Post-Mark: %p = %d\n",i_o->object,i_o->object->marked);
#endif

    if (!i_o->object->marked){
      d = i_o;
      if (j){
	i_o = j->next = i_o->next;
      }else{
	i_o = gc_objects = i_o->next;
      }

      object_destroy(d->object);
      free(d);
      ++counter;
    }else{
      j = i_o;
      i_o = i_o->next;
    }
  }

  return counter;

}
static void register_object(object_t* obj){
  objlist_t *o = malloc(sizeof(objlist_t));

  o->next = gc_objects;
  o->object = obj;
  gc_objects = o;

#ifdef OBJ_LIST_DEBUG
    printf(";; Register-object: %p, type %d\n",obj, obj->type);
#endif


}



void dfsch_object_ref(object_t* obj){
  objlist_t *o = malloc(sizeof(objlist_t));

  o->next = gc_roots;
  o->object = obj;
  gc_roots = o;
}
void dfsch_object_unref(object_t* obj){
  objlist_t *i,*d,*j;

  i = gc_roots;
  j = NULL;

  while (i){
    if (i->object==obj){
      d = i;
      if (j){
	i = j->next = i->next;
      }else{
	i = gc_roots = i->next;
      }
#ifdef OBJ_LIST_DEBUG
    printf(";; Unref-object: %p, type %d\n",obj, obj->type);
#endif

      free(d);
      break;
    }else{
      j = i;
      i = i->next;
    }
  }
#ifdef OBJ_LIST_DEBUG
  dump_obj_list(gc_roots);
#endif
  


}

void dfsch_object_all_unref(object_t* obj){
  objlist_t *i,*d,*j;

  i = gc_roots;
  j = NULL;

  while (i){
    if (i->object==obj){
      d = i;
      if (j){
	i = j->next = i->next;
      }else{
	i = gc_roots = i->next;
      }
#ifdef OBJ_LIST_DEBUG
    printf(";; Unref-object: %p, type %d\n",obj, obj->type);
#endif

      free(d);
    }else{
      j = i;
      i = i->next;
    }
  }
#ifdef OBJ_LIST_DEBUG
  dump_obj_list(gc_roots);
#endif
  

}


static object_t* make_object(type_t type){
  object_t* o = malloc(sizeof(object_t));
  if (!o)
    return NULL;

  o->type = type;

  register_object(o);

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
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				pair);

  return pair->data.pair.car;
}
dfsch_object_t* dfsch_cdr(dfsch_object_t* pair){
  if (!pair || pair->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				pair);

  return pair->data.pair.cdr;
}

dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
			      dfsch_object_t* car){
  if (!pair || pair->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				pair);

  if (pair->data.pair.car!=car){
    pair->data.pair.car = car;
  }
  
  return pair;

}
dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
			      dfsch_object_t* cdr){
  if (!pair || pair->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				pair);
  
  if (pair->data.pair.cdr!=cdr){
    pair->data.pair.cdr = cdr;
  }
  
  return pair;

}

// Alists

dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
			    dfsch_object_t *alist){
  if (!alist || alist->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				alist);

  object_t* i=alist;
  
  while (i && i->type==PAIR){
    if (!i->data.pair.car || i->data.pair.car->type!=PAIR){
      return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				  alist);
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

static object_t* lookup_symbol(char *symbol){
  object_t *i = global_symbol_table;

  while (i){
    if (strcasecmp(i->data.symbol.data, symbol)==0){
      return i;
    }
    i = i->data.symbol.next;
  }

  return NULL;
}
static object_t* make_symbol(char *symbol){
  object_t *s = make_object(SYMBOL);
  
  s->data.symbol.data = stracpy(symbol);

  s->data.symbol.prev = NULL;
  s->data.symbol.next = global_symbol_table;
  global_symbol_table = s;

  if (s->data.symbol.next){
    s->data.symbol.next->data.symbol.prev = s;
  }

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
dfsch_object_t* dfsch_true(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("true");
  dfsch_object_ref(cache);
  return cache;
}
dfsch_object_t* dfsch_quote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("quote");
  dfsch_object_ref(cache);
  return cache;
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

  return c;
  
}

// native code

object_t* dfsch_make_primitive(dfsch_primitive_t prim){
  object_t* p = make_object(PRIMITIVE);
  if (!p)
    return NULL;

  p->data.primitive = prim;

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
object_t* dfsch_make_flow_macro(object_t *proc){
  object_t *m = make_object(FLOW_MACRO);
  
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


  e->data.exception.type=type;
  e->data.exception.data=data;



  return e;
}

// Expression parser

typedef struct token_t {
  enum {
    T_OPEN,
    T_CLOSE,
    T_DOT,
    T_QUOTE,
    T_TERMINAL
  } type;
  object_t* data;
} token_t;

static int get_token(token_t* token, char **str) {
  while ((*str)[0]==' ' || (*str)[0]=='\t' || (*str)[0]=='\n')
    ++(*str);

  if ((*str)[0]==';'){
    while ((*str)[0]!=0 && (*str)[0]!='\n')
      ++(*str); 
  }

  while ((*str)[0]==' ' || (*str)[0]=='\t' || (*str)[0]=='\n')
    ++(*str);

  switch ((*str)[0]){
  case 0:
    return 0;
  case '.':
    if ((*str)[1]==' ' || (*str)[1]=='\t' || (*str)[1]=='\n'){
      token->type=T_DOT;
      (*str)+=2;    
      return 1;

    }
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case '-':
    {
      char *s, *e;

      s = *str;
      e = strpbrk(s,"() \t\n");
      if (!e)
	e = s + strlen(s);
      
      
      s = strancpy(s,(size_t)(e-s));

      (*str)=e;

#ifdef PARSER_DEBUG
      printf(";; Number: [%s]\n",s);
#endif
      if (strcmp(s,"-")==0){
	token->data = dfsch_make_symbol("-");
      }else{
	token->data = dfsch_make_number(atof(s));
      }
      free(s);

      token->type=T_TERMINAL;
      return 1;
    }
  case '"':
    {
      char s[1024];
      char *p=s;
      
      ++(*str);

      while (1){
	if (p>s+1024)
	  return 0; // overflow
	
	switch ((*str)[0]){
	case '"':
	  *p=0;
	  ++(*str);
#ifdef PARSER_DEBUG
	  printf(";; String: \"%s\"\n",s);
#endif
	  token->data = dfsch_make_string(s);
	  token->type = T_TERMINAL;
	  return 1;

	case '\\':
	  switch((*str)[1]){
	  case 0:
	    return 0;
	  default:
	    *p = (*str)[1];
	    ++p;
	  }
	  
	  ++(*str);

	  break;

	case 0:
	  return 0;
	  
	default:
	  *p = **str;
	  ++p;
	} 
	++(*str);
      }
      
    }
  case '(':
    token->type=T_OPEN;
    ++(*str);    
    return 1;
  case ')':
    token->type=T_CLOSE;
    ++(*str);
    return 1;
  case '\'':
    token->type=T_QUOTE;
    ++(*str);
    return 1;
  default:
    
    {
      char *s, *e;
      
      s = *str;
      e = strpbrk(s,"() \t\n");
      if (!e)
	e = s + strlen(s);
      
      (*str)=e;

      s = strancpy(s,(size_t)(e-s));

#ifdef PARSER_DEBUG
      printf(";; Symbol: %s\n",s);
#endif
      token->data = dfsch_make_symbol(s);

      free(s);

      token->type=T_TERMINAL;
      return 1;
    }

  }
}

static object_t* quote(object_t* d){
  return dfsch_cons(dfsch_quote(),dfsch_cons(d, NULL));
}


static object_t* one_obj_read(char**str);
static int one_obj_p(char **str, token_t *t, object_t **o);

static object_t* parse_list(char **str){
  
  token_t t;
  object_t *tmp;
  object_t *p = NULL;
  object_t *o;
  object_t *f = NULL; 

  if (!get_token(&t,str))
    return 
      dfsch_make_exception(dfsch_make_symbol("parse-error:token-expected"),
			  NULL);
  
  while (t.type!=T_CLOSE){

    if (one_obj_p(str, &t, &o)){
      if (f){
	tmp = dfsch_cons(o,NULL);
	dfsch_set_cdr(p,tmp);
	p = tmp;
      }else{
	f = p = dfsch_cons(o,NULL);
      }
    }else{
      if (t.type=T_DOT){
	if (!f)
	  return 
	    dfsch_make_exception(dfsch_make_symbol("parse-error:car-expected"),
				 NULL);


	dfsch_set_cdr(p,one_obj_read(str));

	if (!get_token(&t,str)){
	  return 
	    dfsch_make_exception(dfsch_make_symbol("parse-error:token-expected"),
				 NULL);
	}

	if (t.type!=T_CLOSE){
	  return 
	    dfsch_make_exception(dfsch_make_symbol("parse-error:token-expected"),
				 dfsch_make_symbol("T_CLOSE"));

	}


	return f;
	
      }else{
	return 
	  dfsch_make_exception(dfsch_make_symbol("parse-error:unexpected-token"),
			       NULL);
	
      }
    }

    if (!get_token(&t,str)){
      return 
	dfsch_make_exception(dfsch_make_symbol("parse-error:token-expected"),
			     NULL);
    }

  }

  return f;
}

static int one_obj_p(char **str, token_t *t, object_t **o){

  switch(t->type){
  case T_TERMINAL:
    *o = t->data;
    return 1;
  case T_QUOTE:
    *o = quote(one_obj_read(str));
    return 1;
  case T_OPEN:
    *o = parse_list(str);
    return 1;
  default:
    return 0;
  }
}

static object_t* one_obj_read(char** str){

  token_t t;
  object_t *o;
  if (!get_token(&t,str))
    return 
      dfsch_make_exception(dfsch_make_symbol("parse-error:token-expected"),
			   NULL);
  if (one_obj_p(str, &t,&o)){
    return o;
  }else{
    return 
      dfsch_make_exception(dfsch_make_symbol("parse-error:unexpected-token"),
			  NULL);
  }

}

dfsch_object_t* dfsch_obj_read(char* str){

  return one_obj_read(&str);

}
dfsch_object_t* dfsch_list_read(char* str){
  char *i = str;
  object_t* f = NULL;
  object_t *t, *p;

  while (*i){
    object_t *o = one_obj_read(&i);

    if (dfsch_object_exception_p(o))
      return o;

    t = dfsch_cons(o,NULL);
    if (f){
      dfsch_set_cdr(p,t);
      p = t;
    }else{
      f = p = t;
    }

    while (i[0]==' ' || i[0]=='\t' || i[0]=='\n')
      ++i;
    if (i[0]==';'){
      while (i[0]!=0 && i[0]!='\n')
	++i; 
    }
    while (i[0]==' ' || i[0]=='\t' || i[0]=='\n')
      ++i;

  }

  return f;

}



char* dfsch_obj_write(dfsch_object_t* obj, int max_depth){
  if (!obj){
    return stracpy("()");
  }

  if (max_depth==0){
    return stracpy("...");
  }

  switch (obj->type){
  case NUMBER:
    {
      char  *s = malloc(512);
      snprintf(s, 512, "%lf", obj->data.number);
      return s;
    }
  case SYMBOL:
    {
      return stracpy(dfsch_symbol(obj));
    }
  case STRING:
    return straquote(obj->data.string);
  case PRIMITIVE:
    return stracpy("<native-code>");
  case CLOSURE:
    return stracpy("<closure>");  // TODO: maybe dump some data?
  case MACRO:
    return strneko(strneko_r("<macro: ",
			     dfsch_obj_write(obj->data.macro,max_depth-1)),
		   ">");
  case EXCEPTION:
    return strneko(strneko_r("<exception: ",
			     dfsch_obj_write(obj->data.exception.type,
					     max_depth-1)),
		   strneko_l(strneko_r(" . ",
				       dfsch_obj_write(obj->data.exception.data,
						       max_depth-1)),
			     ">"));
 
  case PAIR: 
    // TODO: at least semi-iterative solution? 
    {
      if (obj->data.pair.cdr && obj->data.pair.cdr->type!=PAIR)
	return strneko(strneko_r("(",
				 dfsch_obj_write(obj->data.pair.car,
						 max_depth-1)),
		       strneko_l(strneko_r(" . ",
					   dfsch_obj_write(obj->data.pair.cdr,
							   max_depth-1)),
				 ")"));
      {
	char *s=malloc(2);
	object_t* i=obj;
	
	strncpy(s,"(",2);

	while (i && i->type==PAIR){
	  
	  s = strneko(s, dfsch_obj_write(i->data.pair.car,max_depth-1));
	  i = i->data.pair.cdr;

	  if (i)
	    s = strneko_l(s," ");
	    
	}

	if (i){
	  s = strneko_l(s,". ");
	  s = strneko(s, dfsch_obj_write(i,max_depth-1));
	}

	return strneko_l(s,")");
      }
    }
  default:
    {
      return stracpy("<native-object>");
    }
  }
}

// EVAL + APPLY

object_t* dfsch_lookup(object_t* name, object_t* env){
  object_t *i, *ie;
  if (!env || env->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				env);

  ie = env;
  while (ie && ie->type==PAIR){
    i = ie->data.pair.car;
    while (i && i->type==PAIR){
      if (!i->data.pair.car || i->data.pair.car->type!=PAIR){
	return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				    ie);
      }
      
      if (dfsch_eq_p(name,i->data.pair.car->data.pair.car)){
	if (!i->data.pair.car->data.pair.cdr || 
	    i->data.pair.car->data.pair.cdr->type!=PAIR)
	  return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				      ie);
	
	return i->data.pair.car->data.pair.cdr->data.pair.car;
      }
      
      i = i->data.pair.cdr;
      
    }
    ie = ie->data.pair.cdr;
  }
  

  return dfsch_make_exception(dfsch_make_symbol("exception:unbound-variable"),
			      name);
  

}


object_t* dfsch_set(object_t* name, object_t* value, object_t* env){
  object_t *i, *ie;
  if (!env || env->type!=PAIR)
    return 0;

  ie = env;
  while (ie && ie->type==PAIR){
    i = ie->data.pair.car;
    while (i && i->type==PAIR){
      if (!i->data.pair.car || i->data.pair.car->type!=PAIR){
	return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				    ie);
      }
      
      if (dfsch_eq_p(name,i->data.pair.car->data.pair.car)){
	if (!i->data.pair.car->data.pair.cdr || 
	    i->data.pair.car->data.pair.cdr->type!=PAIR)
	  return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				      ie);
	dfsch_set_car(i->data.pair.car->data.pair.cdr,value);
	return value;
      }
      
      i = i->data.pair.cdr;
      
    }
    ie = ie->data.pair.cdr;
  }
  

  return dfsch_make_exception(dfsch_make_symbol("exception:unbound-variable"),
			      name);
  

}

static void define(object_t* name, object_t* value, object_t* env){

  object_t *o, *d;

  o = env->data.pair.car;
  d = dfsch_cons(name,dfsch_cons(value,NULL));
  dfsch_set_car(env,dfsch_cons(d,o));

}

object_t* dfsch_define(object_t* name, object_t* value, object_t* env){
  if (!env || env->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				env);

  define(name,value,env);
  
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

  dfsch_object_ref(list);
  dfsch_object_ref(env);

  i = list;
  while (i && i->type==PAIR){
    r = dfsch_eval(i->data.pair.car,env);
    if (dfsch_object_exception_p(r)){
      dfsch_object_unref(list);
      dfsch_object_unref(env);
      dfsch_object_unref(f);
      return r;
    }

    t = dfsch_cons(r,NULL);
    if (f){
      dfsch_set_cdr(p,t);
      p = t;
    }else{
      dfsch_object_ref(f = p = t);
    }

    i=i->data.pair.cdr;
  }
  dfsch_object_unref(list);
  dfsch_object_unref(env);
  dfsch_object_unref(f);

  return f;
}

dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env){
  if (!exp) 
    return NULL;

  switch (exp->type){
  case PAIR:
    {

      object_t *f = dfsch_eval(exp->data.pair.car,env);
      
      switch(f->type){
      case MACRO:
	return dfsch_apply(f->data.macro,     
			   dfsch_cons(env,
				      exp->data.pair.cdr));
      case FLOW_MACRO:
	return dfsch_eval(dfsch_apply(f->data.macro,     
				      dfsch_cons(env,
						 exp->data.pair.cdr)),env);
	
      case CLOSURE:
      case PRIMITIVE:
	return dfsch_apply(f, 
			   eval_list(exp->data.pair.cdr,env));
      }

      return 
	dfsch_make_exception(dfsch_make_symbol("exception:not-a-procedure-or-macro"),
			     f);

      
    }
  case SYMBOL:
    return dfsch_lookup(exp,env);
  default:
    return exp;
  }
}

static object_t* lambda_extend(object_t* fa, object_t* aa, object_t* env){
  object_t* f=NULL;
  object_t* i_f=fa;
  object_t* i_a=aa;

  while ((i_f && i_f->type==PAIR) &&
	 (i_a && i_a->type==PAIR)){

    f = dfsch_cons(dfsch_cons(i_f->data.pair.car,
			      dfsch_cons(i_a->data.pair.car,
					 NULL)),
		   f);

    i_f = i_f->data.pair.cdr;
    i_a = i_a->data.pair.cdr;
    
  }

  if (i_f && i_f->type==SYMBOL){
    f = dfsch_cons(dfsch_cons(i_f,
			      dfsch_cons(i_a,
					 NULL)),
		   f);
  }

  if (!i_a  && i_f)
    return 
      dfsch_make_exception(dfsch_make_symbol("exception:too-few-arguments"),
			   aa);
  if (!i_f && i_a)
    return 
      dfsch_make_exception(dfsch_make_symbol("exception:too-many-arguments"),
			   aa);
  

  return dfsch_cons(f,env);
}

static object_t* eval_proc(object_t* code, object_t* env){
  object_t *i, *r=NULL;


  if (!env)
    return NULL;
  if (env->type==EXCEPTION)
    return env;
  if (!code)
    return NULL;
  if (code->type==EXCEPTION)
    return env;

  dfsch_object_ref(code);
  dfsch_object_ref(env);

  i = code;

  while (i && i->type==PAIR ){
    object_t* exp = i->data.pair.car; 

    r = dfsch_eval(exp,env);

    if (dfsch_object_exception_p(r)){
      dfsch_object_unref(code);
      dfsch_object_unref(env);
      return r;
    }
   
    i = i->data.pair.cdr;
  }

  dfsch_object_unref(code);
  dfsch_object_unref(env);
  
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
    return eval_proc(proc->data.closure.code,
		     lambda_extend(proc->data.closure.args,
				   args,
				   proc->data.closure.env));
    
  case PRIMITIVE:
    return (*proc->data.primitive)(args);
  default:
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-procedure"),
				proc);

  }  
}


static int count_list(object_t* list){
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

#define NEED_ARGS(args,count) \
  if (count_list(args)!=(count)) \
    return dfsch_make_exception( \
      dfsch_make_symbol("exception:wrong-number-of-arguments"), \
      (args));
#define MIN_ARGS(args,count) \
  if (count_list(args)<(count)) \
    return dfsch_make_exception( \
      dfsch_make_symbol("exception:too-few-arguments"), \
      (args));

// Native procedures:

static object_t* native_plus(object_t* args){
  object_t* i = args;
  double s=0;
  while(i && i->type==PAIR){
    if (dfsch_object_number_p(dfsch_car(i))){
      s+=dfsch_number(i->data.pair.car);
    }else{
      return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				  i->data.pair.car);
      
    }
    i = i->data.pair.cdr;
  }

  return dfsch_make_number(s); 
}
static object_t* native_minus(object_t* args){
  object_t* i = args;
  double s;
  if (dfsch_object_number_p(dfsch_car(i))){
    s=dfsch_number(i->data.pair.car);
  }else{
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				i->data.pair.car);
    
  }
  i = i->data.pair.cdr;
  while(i && i->type==PAIR){
    if (dfsch_object_number_p(dfsch_car(i))){
      s-=dfsch_number(i->data.pair.car);
    }else{
      return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				  i->data.pair.car);
      
    }
    i = i->data.pair.cdr;
  }

  return dfsch_make_number(s); 
}
static object_t* native_mult(object_t* args){
  object_t* i = args;
  double s=1;
  while(i && i->type==PAIR){
    if (dfsch_object_number_p(dfsch_car(i))){
      s*=dfsch_number(i->data.pair.car);
    }else{
      return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				  i->data.pair.car);
      
    }
    i = i->data.pair.cdr;
  }

  return dfsch_make_number(s); 
}
static object_t* native_slash(object_t* args){
  object_t* i = args;
  double s;
  if (dfsch_object_number_p(dfsch_car(i))){
    s=dfsch_number(i->data.pair.car);
  }else{
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				i->data.pair.car);
    
  }
  i = i->data.pair.cdr;
  
  while(i && i->type==PAIR){
    if (i->data.pair.car->type==NUMBER){
      s/=dfsch_number(i->data.pair.car);
    }else{
      return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				  i->data.pair.car);
      
    }
    i = i->data.pair.cdr;
  }

  return dfsch_make_number(s); 
}

static object_t* native_macro_lambda(object_t* args){

  MIN_ARGS(args,2);

  return dfsch_lambda(dfsch_car(args),
		      dfsch_car(dfsch_cdr(args)),
		      dfsch_cdr(dfsch_cdr(args)));

}

static object_t* native_macro_define(object_t* args){

  MIN_ARGS(args,2);  

  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));

  if (dfsch_object_pair_p(name)){
    object_t* lambda = dfsch_lambda(env,dfsch_cdr(name),
				    dfsch_cdr(dfsch_cdr(args)));
    define(dfsch_car(name), lambda ,env);
    return lambda;
  }else{
    object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);
    define(name,value,env);
    return value;
  }

}
static object_t* native_macro_set(object_t* args){
  
  NEED_ARGS(args,3);  

  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));
  object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);

  return dfsch_set(name, value, env);

}
static object_t* native_flow_macro_if(object_t* args){

  NEED_ARGS(args,4);    
  object_t* env = dfsch_car(args);
  object_t* cond = dfsch_car(dfsch_cdr(args));
  object_t* true = dfsch_car(dfsch_cdr(dfsch_cdr(args)));
  object_t* false = dfsch_car(dfsch_cdr(dfsch_cdr(dfsch_cdr(args))));

  EXCEPTION_CHECK(cond);

  return dfsch_eval(cond,env)?true:false;
}

static object_t* native_flow_macro_cond(object_t* args){
  
  MIN_ARGS(args,1)

  object_t* env = dfsch_car(args);
  object_t* i = dfsch_cdr(args);

  while (i && i->type == PAIR){
    object_t *o = dfsch_eval(dfsch_car(i->data.pair.car), env);
    EXCEPTION_CHECK(o);
    if (o){
      return dfsch_car(dfsch_cdr(i->data.pair.car));
    }
    
    i = i->data.pair.cdr; 
  }

  return NULL;
}


static object_t* native_macro_env(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_car(args);
}

static object_t* native_macro_quote(object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_car(dfsch_cdr(args));
}



static object_t* native_make_macro(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_make_macro(dfsch_car(args));
}
static object_t* native_car(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_car(dfsch_car(args));
}
static object_t* native_cdr(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_cdr(dfsch_car(args));
}
static object_t* native_cons(object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_cons(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
static object_t* native_list(object_t* args){
  return args;
}
static object_t* native_eq(object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_eq_p(dfsch_car(args),dfsch_car(dfsch_cdr(args)))?
    dfsch_true():
    NULL;
}
static object_t* native_set_car(object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_set_car(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
static object_t* native_set_cdr(object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_set_cdr(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}

static object_t* native_null_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_null_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_pair_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_pair_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_atom_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_atom_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_symbol_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_symbol_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_number_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_number_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_string_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_string_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_primitive_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_primitive_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_closure_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_closure_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_procedure_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_procedure_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_macro_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_macro_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_exception_p(object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_exception_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}

static object_t* native_gc(object_t* args){
  NEED_ARGS(args,0);  
  return dfsch_make_number((double)dfsch_gc());
}


static object_t* native_lt(object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_object_number_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				a);
  if (!dfsch_object_number_p(b))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				b);

  return dfsch_number(a)<dfsch_number(b)?
    dfsch_true():
    NULL;  
}
static object_t* native_gt(object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_object_number_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				a);
  if (!dfsch_object_number_p(b))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				b);
    

  return dfsch_number(a)>dfsch_number(b)?
    dfsch_true():
    NULL;  
}

static object_t* native_or(object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  EXCEPTION_CHECK(a);
  EXCEPTION_CHECK(b);
  return (a||b)?
    dfsch_true():
    NULL;  
}
static object_t* native_and(object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  EXCEPTION_CHECK(a);
  EXCEPTION_CHECK(b);
  return (a&&b)?
    dfsch_true():
    NULL;  
}



// Context


dfsch_ctx_t* dfsch_make_context(){
  dfsch_ctx_t* ctx=malloc(sizeof(dfsch_ctx_t));
  if (!ctx)
    return NULL;

  ctx->env = dfsch_cons(NULL,
			NULL);

  
  dfsch_ctx_define(ctx, "+", dfsch_make_primitive(&native_plus));
  dfsch_ctx_define(ctx, "-", dfsch_make_primitive(&native_minus));
  dfsch_ctx_define(ctx, "*", dfsch_make_primitive(&native_mult));
  dfsch_ctx_define(ctx, "/", dfsch_make_primitive(&native_slash));
  dfsch_ctx_define(ctx, "=", dfsch_make_primitive(&native_eq));
  dfsch_ctx_define(ctx, "<", dfsch_make_primitive(&native_lt));
  dfsch_ctx_define(ctx, ">", dfsch_make_primitive(&native_gt));
  dfsch_ctx_define(ctx, "and", dfsch_make_primitive(&native_and));
  dfsch_ctx_define(ctx, "or", dfsch_make_primitive(&native_or));

  dfsch_ctx_define(ctx, "lambda", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_lambda)));
  dfsch_ctx_define(ctx, "define", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_define)));
  dfsch_ctx_define(ctx, "set!", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_set)));
  dfsch_ctx_define(ctx, "env", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_env)));
  dfsch_ctx_define(ctx, "quote", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_quote)));
  dfsch_ctx_define(ctx, "if", 
		   dfsch_make_flow_macro(dfsch_make_primitive(&native_flow_macro_if)));
  dfsch_ctx_define(ctx, "cond", 
		   dfsch_make_flow_macro(dfsch_make_primitive(&native_flow_macro_cond)));

  dfsch_ctx_define(ctx, "make-macro", dfsch_make_primitive(&native_make_macro));
  dfsch_ctx_define(ctx, "cons", dfsch_make_primitive(&native_cons));
  dfsch_ctx_define(ctx, "list", dfsch_make_primitive(&native_list));
  dfsch_ctx_define(ctx, "car", dfsch_make_primitive(&native_car));
  dfsch_ctx_define(ctx, "cdr", dfsch_make_primitive(&native_cdr));
  dfsch_ctx_define(ctx, "set-car!", dfsch_make_primitive(&native_set_car));
  dfsch_ctx_define(ctx, "set-cdr!", dfsch_make_primitive(&native_set_cdr));

  dfsch_ctx_define(ctx, "null?", dfsch_make_primitive(&native_null_p));
  dfsch_ctx_define(ctx, "atom?", dfsch_make_primitive(&native_atom_p));
  dfsch_ctx_define(ctx, "pair?", dfsch_make_primitive(&native_pair_p));
  dfsch_ctx_define(ctx, "symbol?", dfsch_make_primitive(&native_symbol_p));
  dfsch_ctx_define(ctx, "number?", dfsch_make_primitive(&native_number_p));
  dfsch_ctx_define(ctx, "string?", dfsch_make_primitive(&native_string_p));
  dfsch_ctx_define(ctx, "primitive?", 
		   dfsch_make_primitive(&native_primitive_p));
  dfsch_ctx_define(ctx, "closure?", dfsch_make_primitive(&native_closure_p));
  dfsch_ctx_define(ctx, "prcedure?", 
		   dfsch_make_primitive(&native_procedure_p));
  dfsch_ctx_define(ctx, "macro?", dfsch_make_primitive(&native_macro_p));
  dfsch_ctx_define(ctx, "exception?", 
		   dfsch_make_primitive(&native_exception_p));

  dfsch_ctx_define(ctx, "gc", 
		   dfsch_make_primitive(&native_gc));


  dfsch_ctx_define(ctx, "true", dfsch_true());
  dfsch_ctx_define(ctx, "nil", NULL);
  dfsch_ctx_define(ctx, "else", dfsch_true());
  dfsch_ctx_define(ctx, "T", dfsch_true());



  dfsch_object_ref(ctx->env);

  return ctx;
}
dfsch_object_t* dfsch_ctx_eval(dfsch_ctx_t* ctx, dfsch_object_t* exp){
  return dfsch_eval(exp, ctx->env);
}
extern dfsch_object_t* dfsch_ctx_eval_list(dfsch_ctx_t* ctx, 
					   dfsch_object_t* list){
  return eval_proc(list, ctx->env);
}


void dfsch_destroy_context(dfsch_ctx_t* ctx){
  dfsch_object_unref(ctx->env);
  free(ctx);
}
void dfsch_ctx_define(dfsch_ctx_t *ctx, 
		      char *name, 
		      dfsch_object_t *obj){
  
  object_t *o,*d;


  o = ctx->env->data.pair.car;
  d = dfsch_cons(dfsch_make_symbol(name),dfsch_cons(obj,NULL));
  dfsch_set_car(ctx->env,dfsch_cons(d,o));
  
}
dfsch_object_t* dfsch_ctx_environment(dfsch_ctx_t *ctx){
  return ctx->env;
}
