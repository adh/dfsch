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
  FLOW_MACRO,
  EXCEPTION,
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

static object_t *global_symbol_table=NULL;


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
    native_t native;
  } data;
};


static char* stracat(char* a, char* b){
  size_t s = strlen(a)+strlen(b)+1;
  char* o = GC_MALLOC_ATOMIC(s);
  strncpy(o,a,s);
  strncat(o,b,s);
  return o;
}

static char* stracpy(char* x){
  char *b;
  size_t s = strlen(x)+1;
  b = GC_MALLOC_ATOMIC(s);
  strncpy(b,x,s);
  return b;
}
static char* strancpy(char* x, size_t n){
  char *b;
  size_t s = n+1;
  b = GC_MALLOC_ATOMIC(s);
  strncpy(b,x,s-1);
  b[s-1]=0;
  return b;
}
static char* straquote(char *s){
  char *b = GC_MALLOC_ATOMIC(strlen(s)*2+3); // worst case, to lazy to optimize
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

int dfsch_object_native_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == NATIVE;
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

#define HASH_BITS 10
#define HASH_SIZE 1 << HASH_BITS

typedef struct hash_entry_t hash_entry_t;
struct hash_entry_t {
  dfsch_object_t* entry;
  hash_entry_t* next;
};

static size_t string_hash(char* string){
  size_t tmp=0x76ac92de;

  while (*string){
    tmp ^= *string ^ tmp << 5; 
    ++string;
  }

  return tmp & 0x03FF;  // XXX: calculate this from macros
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
  
  hash_entry_t *e = malloc(sizeof(hash_entry_t));

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
dfsch_object_t* dfsch_true(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("true");
  return cache;
}
dfsch_object_t* dfsch_quote(){
  static object_t *cache = NULL;
  if (cache)
    return cache;

  cache = dfsch_make_symbol("quote");
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


  e->data.exception.type = type;
  e->data.exception.data = data;
  e->data.exception.trace = NULL;

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

// Native data

dfsch_object_t* dfsch_make_native_data(void *data, 
					      dfsch_object_t *type){
  object_t* n = make_object(EXCEPTION);
  
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

  while ((*str)[0]){
    while ((*str)[0]==' ' || (*str)[0]=='\t' || (*str)[0]=='\n')
      ++(*str);
    
    switch ((*str)[0]){
    case 0:
      return 0;
    case ';':
      while ((*str)[0]!=0 && (*str)[0]!='\n')
	++(*str);
      continue;
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
	
	
	token->type=T_TERMINAL;
	return 1;
      }
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

  
  while (get_token(&t,str)){

    if (one_obj_p(str, &t, &o)){
      if (f){
	tmp = dfsch_cons(o,NULL);
	dfsch_set_cdr(p,tmp);
	p = tmp;
      }else{
	f = p = dfsch_cons(o,NULL);
      }
    }else{
      if (t.type==T_DOT){
	if (!p)
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

      }else if (t.type==T_CLOSE){
	return f;
      }else{
	return 
	  dfsch_make_exception(dfsch_make_symbol("parse-error:unexpected-token"),
			       NULL);
	
      }
    }


  }
  return 
    dfsch_make_exception(dfsch_make_symbol("parse-error:token-expected"),
			 NULL);

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
      char  *s = GC_malloc(512);
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
    return stracat(stracat("<macro: ",
			   dfsch_obj_write(obj->data.macro,max_depth-1)),
		   ">");
  case NATIVE:
    return stracat(stracat("<native-data: ",
			   dfsch_obj_write(obj->data.native.type,max_depth-1)),
		   ">");
  case EXCEPTION:
    return stracat(stracat("<exception: ",
			     dfsch_obj_write(obj->data.exception.type,
					     max_depth-1)),
		   stracat(stracat(" . ",
                                   dfsch_obj_write(obj->data.exception.data,
						       max_depth-1)),
                           stracat(" @ ",
                                   stracat(dfsch_obj_write(obj->data.exception.trace,
                                                           max_depth-1),
                                           ">"))));
 
  case PAIR: 
    // TODO: at least semi-iterative solution? 
    {
      if (obj->data.pair.cdr && obj->data.pair.cdr->type!=PAIR)
	return stracat(stracat("(",
				 dfsch_obj_write(obj->data.pair.car,
						 max_depth-1)),
		       stracat(stracat(" . ",
					   dfsch_obj_write(obj->data.pair.cdr,
							   max_depth-1)),
				 ")"));
      {
	char *s=GC_malloc(2);
	object_t* i=obj;
	
	strncpy(s,"(",2);

	while (i && i->type==PAIR){
	  
	  s = stracat(s, dfsch_obj_write(i->data.pair.car,max_depth-1));
	  i = i->data.pair.cdr;

	  if (i)
	    s = stracat(s," ");
	    
	}

	if (i){
	  s = stracat(s,". ");
	  s = stracat(s, dfsch_obj_write(i,max_depth-1));
	}

	return stracat(s,")");
      }
    }
  default:
    {
      return stracpy("<unknown-object>");
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
      
      if ((name == i->data.pair.car->data.pair.car)){
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
      
      if (name == i->data.pair.car->data.pair.car){
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


object_t* dfsch_define(object_t* name, object_t* value, object_t* env){
  if (!env || env->type!=PAIR)
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-pair"),
				env);

  object_t *i = env->data.pair.car;

  while (i && i->type==PAIR){
    if (!i->data.pair.car || i->data.pair.car->type!=PAIR){
      return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				  i);
    }
    
    if (name == i->data.pair.car->data.pair.car){
      if (!i->data.pair.car->data.pair.cdr || 
	  i->data.pair.car->data.pair.cdr->type!=PAIR)
	return dfsch_make_exception(dfsch_make_symbol("exception:not-a-alist"),
				    i);
      
      return dfsch_make_exception(dfsch_make_symbol("exception:already-defined"),
				    i->data.pair.car);;
    }
    
    i = i->data.pair.cdr;
    
  }
  
  dfsch_set_car(env,dfsch_cons(dfsch_cons(name, 
					  dfsch_cons(value, 
						     NULL)),
			       env->data.pair.car));
  
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
	return 
	  dfsch_make_exception(dfsch_make_symbol("exception:not-a-procedure-or-macro"),
			       f);
	
 
      switch(f->type){
      case MACRO:
	return dfsch_apply(f->data.macro,     
			   dfsch_cons(env,
				      exp->data.pair.cdr));
      case FLOW_MACRO:
	exp = dfsch_apply(f->data.macro,dfsch_cons(env, exp->data.pair.cdr));
	goto start;
	
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
      object_t* r = eval_proc(proc->data.closure.code,
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

static object_t* native_plus(void *baton, object_t* args){
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
static object_t* native_minus(void *baton, object_t* args){
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
static object_t* native_mult(void *baton, object_t* args){
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
static object_t* native_slash(void *baton, object_t* args){
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

static object_t* native_macro_lambda(void *baton, object_t* args){

  MIN_ARGS(dfsch_cdr(args),1);

  return dfsch_lambda(dfsch_car(args),
		      dfsch_car(dfsch_cdr(args)),
		      dfsch_cdr(dfsch_cdr(args)));

}

static object_t* native_macro_define(void *baton, object_t* args){

  MIN_ARGS(dfsch_cdr(args),1);  

  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));

  if (dfsch_object_pair_p(name)){
    object_t* lambda = dfsch_named_lambda(env,dfsch_cdr(name),
                                          dfsch_cdr(dfsch_cdr(args)),
                                          dfsch_car(name));
    return dfsch_define(dfsch_car(name), lambda ,env);
  }else{
    object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);
    EXCEPTION_CHECK(value);
    return dfsch_define(name,value,env);
  }

}
static object_t* native_macro_set(void *baton, object_t* args){
  
  NEED_ARGS(dfsch_cdr(args),2);  

  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));
  object_t* value = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);

  EXCEPTION_CHECK(value);

  return dfsch_set(name, value, env);

}
static object_t* native_macro_defined_p(void *baton, object_t* args){
  NEED_ARGS(dfsch_cdr(args),1);
  object_t* env = dfsch_car(args);
  object_t* name = dfsch_car(dfsch_cdr(args));

  return (dfsch_object_exception_p(dfsch_lookup(name,env)))
    ?NULL
    :dfsch_true();
}

static object_t* native_flow_macro_if(void *baton, object_t* args){

  NEED_ARGS(dfsch_cdr(args),3);    
  object_t* env = dfsch_car(args);
  object_t* cond = dfsch_car(dfsch_cdr(args));
  object_t* true = dfsch_car(dfsch_cdr(dfsch_cdr(args)));
  object_t* false = dfsch_car(dfsch_cdr(dfsch_cdr(dfsch_cdr(args))));

  EXCEPTION_CHECK(cond);

  return dfsch_eval(cond,env)?true:false;
}

static object_t* native_flow_macro_cond(void *baton, object_t* args){
  

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


static object_t* native_macro_env(void *baton, object_t* args){
  NEED_ARGS(dfsch_car(args),0);  
  return dfsch_car(args);
}

static object_t* native_macro_quote(void *baton, object_t* args){
  NEED_ARGS(dfsch_car(args),1);  
  return dfsch_car(dfsch_cdr(args));
}
static object_t* native_macro_begin(void *baton, object_t* args){
  return eval_proc(dfsch_cdr(args),dfsch_car(args));
}
static object_t* native_macro_let(void *baton, object_t* args){
  MIN_ARGS(args,2);

  object_t *env = dfsch_car(args);
  object_t *ext = dfsch_car(dfsch_cdr(args));
  object_t *code = dfsch_cdr(dfsch_cdr(args));

  return eval_proc(code,dfsch_cons(ext,env));
}



static object_t* native_make_macro(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_make_macro(dfsch_car(args));
}
static object_t* native_make_flow_macro(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_make_flow_macro(dfsch_car(args));
}
static object_t* native_car(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_car(dfsch_car(args));
}
static object_t* native_cdr(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_cdr(dfsch_car(args));
}
static object_t* native_cons(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_cons(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
static object_t* native_list(void *baton, object_t* args){
  return args;
}
static object_t* native_length(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_make_number((double)count_list(args));
}
static object_t* native_eq(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_eq_p(dfsch_car(args),dfsch_car(dfsch_cdr(args)))?
    dfsch_true():
    NULL;
}
static object_t* native_set_car(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_set_car(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}
static object_t* native_set_cdr(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_set_cdr(dfsch_car(args),dfsch_car(dfsch_cdr(args)));  
}

static object_t* native_null_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_null_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_pair_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_pair_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_atom_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_atom_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_symbol_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_symbol_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_number_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_number_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_string_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_string_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_primitive_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_primitive_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_closure_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_closure_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_procedure_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_procedure_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}
static object_t* native_macro_p(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  return dfsch_object_macro_p(dfsch_car(args))?
    dfsch_true():
    NULL;  
}


static object_t* native_lt(void *baton, object_t* args){
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
static object_t* native_gt(void *baton, object_t* args){
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
static object_t* native_lte(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_object_number_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				a);
  if (!dfsch_object_number_p(b))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				b);

  return dfsch_number(a)<=dfsch_number(b)?
    dfsch_true():
    NULL;  
}
static object_t* native_gte(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_object_number_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				a);
  if (!dfsch_object_number_p(b))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-number"),
				b);
    

  return dfsch_number(a)>=dfsch_number(b)?
    dfsch_true():
    NULL;  
}

static object_t* native_or(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  return (a||b)?
    dfsch_true():
    NULL;  
}
static object_t* native_and(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  return (a&&b)?
    dfsch_true():
    NULL;  
}
static object_t* native_not(void *baton, object_t* args){
  NEED_ARGS(args,1);  
  object_t *a = dfsch_car(args);
  return (!a)?
    dfsch_true():
    NULL;  
}

static object_t* native_throw(void *baton, object_t* args){
  NEED_ARGS(args,2);  
  return dfsch_make_exception(dfsch_car(args),dfsch_car(dfsch_cdr(args)));
}
static object_t* native_macro_try(void *baton, object_t* args){
  NEED_ARGS(args,3);  
  object_t *env = dfsch_car(args);
  object_t *value = dfsch_eval(dfsch_car(dfsch_cdr(args)),env);
  object_t *except = dfsch_eval(dfsch_car(dfsch_cdr(dfsch_cdr(args))),env);

  EXCEPTION_CHECK(except);

  return dfsch_object_exception_p(value)
    ?dfsch_apply(except,dfsch_cons(value->data.exception.type,
				   dfsch_cons(value->data.exception.data,
					      dfsch_cons(value,
							 NULL))))
    :value;
  
}
static object_t* native_string_append(void *baton, object_t* args){
  NEED_ARGS(args,2);
  object_t* a = dfsch_car(args);
  object_t* b = dfsch_car(dfsch_cdr(args));
  char *s;

  if (!dfsch_object_string_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-string"),
				a);
  if (!dfsch_object_string_p(b))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-string"),
				b);

  s = stracat(dfsch_string(a),dfsch_string(b));

  object_t* o = dfsch_make_string(s); 
  return o;
}
static object_t* native_string_ref(void *baton, object_t* args){
  NEED_ARGS(args,2);
  object_t* a = dfsch_car(args);
  object_t* b = dfsch_car(dfsch_cdr(args));

  if (!dfsch_object_string_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-string"),
				a);

  char *s = dfsch_string(a);
  size_t len = strlen(s);
  size_t index = (size_t)(dfsch_number(b));
  
  if (index < 0)
    index = index + len;
  if (index>=len)
    return dfsch_make_exception(dfsch_make_symbol("exception:index-too-large"),
				b);



  return dfsch_make_number((double)s[index]);
}
static object_t* native_string_length(void *baton, object_t* args){
  NEED_ARGS(args,1);

  object_t* a = dfsch_car(args);
  if (!dfsch_object_string_p(a))
    return dfsch_make_exception(dfsch_make_symbol("exception:not-a-string"),
				a);

  return dfsch_make_number((double)strlen(dfsch_string(a)));
}




// Context


dfsch_ctx_t* dfsch_make_context(){
  dfsch_ctx_t* ctx=GC_malloc(sizeof(dfsch_ctx_t));
  if (!ctx)
    return NULL;

  gsh_check_init();

  ctx->env = dfsch_cons(NULL,
			NULL);

  
  dfsch_ctx_define(ctx, "+", dfsch_make_primitive(&native_plus,NULL));
  dfsch_ctx_define(ctx, "-", dfsch_make_primitive(&native_minus,NULL));
  dfsch_ctx_define(ctx, "*", dfsch_make_primitive(&native_mult,NULL));
  dfsch_ctx_define(ctx, "/", dfsch_make_primitive(&native_slash,NULL));
  dfsch_ctx_define(ctx, "=", dfsch_make_primitive(&native_eq,NULL));
  dfsch_ctx_define(ctx, "<", dfsch_make_primitive(&native_lt,NULL));
  dfsch_ctx_define(ctx, ">", dfsch_make_primitive(&native_gt,NULL));
  dfsch_ctx_define(ctx, "<=", dfsch_make_primitive(&native_lte,NULL));
  dfsch_ctx_define(ctx, ">=", dfsch_make_primitive(&native_gte,NULL));
  dfsch_ctx_define(ctx, "and", dfsch_make_primitive(&native_and,NULL));
  dfsch_ctx_define(ctx, "or", dfsch_make_primitive(&native_or,NULL));
  dfsch_ctx_define(ctx, "not", dfsch_make_primitive(&native_not,NULL));

  dfsch_ctx_define(ctx, "lambda", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_lambda,
							 NULL)));
  dfsch_ctx_define(ctx, "define", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_define,
							 NULL)));
  dfsch_ctx_define(ctx, "defined?", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_defined_p,
							 NULL)));
  dfsch_ctx_define(ctx, "begin", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_begin,
							 NULL)));
  dfsch_ctx_define(ctx, "let", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_let,
							 NULL)));

  dfsch_ctx_define(ctx, "set!", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_set,
							 NULL)));
  dfsch_ctx_define(ctx, "env", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_env,
							 NULL)));
  dfsch_ctx_define(ctx, "quote", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_quote,
							 NULL)));
  dfsch_ctx_define(ctx, "if", 
		   dfsch_make_flow_macro(dfsch_make_primitive(&native_flow_macro_if,
							      NULL)));
  dfsch_ctx_define(ctx, "cond", 
		   dfsch_make_flow_macro(dfsch_make_primitive(&native_flow_macro_cond,
							      NULL)));

  dfsch_ctx_define(ctx, "make-macro", 
		   dfsch_make_primitive(&native_make_macro,NULL));
  dfsch_ctx_define(ctx, "make-flow-macro", 
		   dfsch_make_primitive(&native_make_flow_macro,NULL));
  dfsch_ctx_define(ctx, "cons", dfsch_make_primitive(&native_cons,NULL));
  dfsch_ctx_define(ctx, "list", dfsch_make_primitive(&native_list,NULL));
  dfsch_ctx_define(ctx, "car", dfsch_make_primitive(&native_car,NULL));
  dfsch_ctx_define(ctx, "cdr", dfsch_make_primitive(&native_cdr,NULL));
  dfsch_ctx_define(ctx, "set-car!", dfsch_make_primitive(&native_set_car,
							 NULL));
  dfsch_ctx_define(ctx, "set-cdr!", dfsch_make_primitive(&native_set_cdr,
							 NULL));

  dfsch_ctx_define(ctx, "length", dfsch_make_primitive(&native_length,NULL));

  dfsch_ctx_define(ctx, "null?", dfsch_make_primitive(&native_null_p,NULL));
  dfsch_ctx_define(ctx, "atom?", dfsch_make_primitive(&native_atom_p,NULL));
  dfsch_ctx_define(ctx, "pair?", dfsch_make_primitive(&native_pair_p,NULL));
  dfsch_ctx_define(ctx, "symbol?", dfsch_make_primitive(&native_symbol_p,
							NULL));
  dfsch_ctx_define(ctx, "number?", dfsch_make_primitive(&native_number_p,
							NULL));
  dfsch_ctx_define(ctx, "string?", dfsch_make_primitive(&native_string_p,
							NULL));
  dfsch_ctx_define(ctx, "primitive?", 
		   dfsch_make_primitive(&native_primitive_p,NULL));
  dfsch_ctx_define(ctx, "closure?", dfsch_make_primitive(&native_closure_p,
							 NULL));
  dfsch_ctx_define(ctx, "procedure?", 
		   dfsch_make_primitive(&native_procedure_p,NULL));
  dfsch_ctx_define(ctx, "macro?", dfsch_make_primitive(&native_macro_p,NULL));


  dfsch_ctx_define(ctx, "throw", 
		   dfsch_make_primitive(&native_throw,NULL));
  dfsch_ctx_define(ctx, "try", 
		   dfsch_make_macro(dfsch_make_primitive(&native_macro_try,
							 NULL)));

  dfsch_ctx_define(ctx, "string-append", 
		   dfsch_make_primitive(&native_string_append,NULL));
  dfsch_ctx_define(ctx, "string-ref", 
		   dfsch_make_primitive(&native_string_ref,NULL));
  dfsch_ctx_define(ctx, "string-length", 
		   dfsch_make_primitive(&native_string_length,NULL));

  dfsch_ctx_define(ctx, "true", dfsch_true());
  dfsch_ctx_define(ctx, "nil", NULL);
  dfsch_ctx_define(ctx, "else", dfsch_true());
  dfsch_ctx_define(ctx, "T", dfsch_true());


  return ctx;
}
dfsch_object_t* dfsch_ctx_eval(dfsch_ctx_t* ctx, dfsch_object_t* exp){
  return dfsch_eval(exp, ctx->env);
}
extern dfsch_object_t* dfsch_ctx_eval_list(dfsch_ctx_t* ctx, 
					   dfsch_object_t* list){
  return eval_proc(list, ctx->env);
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
