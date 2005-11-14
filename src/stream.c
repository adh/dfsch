/** @file
 * Event driven parser for dfsch.
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

#include "../dfsch/stream.h"

#include <strings.h>
#include <unistd.h>
#include <stdlib.h>

#include <gc/gc.h>

//#define Q_DEBUG
//#define T_DEBUG
//#define P_DEBUG

typedef struct string_queue_t {
  char* buf;
  size_t all;
} string_queue_t;

static string_queue_t *create_queue(){
  string_queue_t *q = GC_MALLOC(sizeof(string_queue_t));
  q->buf = GC_MALLOC_ATOMIC(512);
  q->all = 512;
  if (!q->buf){
    abort();
  }
  q->buf[0]=0;
  return q;
}
static char* get_queue(string_queue_t *q){
  return q->buf;
}
static char* feed_queue(string_queue_t *q, char* data){
  size_t new_size = strlen(q->buf) + strlen(data)+1;
  if (new_size > q->all){
    q->buf = GC_REALLOC(q->buf, new_size+(new_size/2));
    q->all = new_size+(new_size/2);
    if (!q->buf){
      abort();
    }
  }
  strncat(q->buf, data, new_size);

#ifdef Q_DEBUG
  printf(";; Queue is now: [[[%s]]] \n",get_queue(q));
#endif

}
static void consume_queue(string_queue_t *q, char* new){
  size_t nl = strlen(new)+1;
  memmove(q->buf, new, nl);

#ifdef Q_DEBUG
  printf(";; Trimmed: [[[%s]]]",new);
#endif

  if (nl < q->all/2){
    q->buf = GC_REALLOC(q->buf, nl);
    q->all = nl;    
    if (!q->buf){
      abort();
    }
  }

#ifdef Q_DEBUG
  printf(";; Queue is now: [[[%s]]] \n",get_queue(q));
#endif


}
static void empty_queue(string_queue_t *q){
  *(q->buf)=0;
}

typedef struct parser_stack_t parser_stack_t;

struct parser_stack_t {
  enum {
    P_LIST,
    P_DOT,
    P_PREEND,
    P_QUOTE,
    P_VECTOR
  } state;

  dfsch_object_t *front;
  dfsch_object_t *last;
  dfsch_object_t *tag;

  parser_stack_t *next;
};

struct dfsch_parser_ctx_t {
  string_queue_t *q;

  enum {
    T_ATOM, // i.e. number or symbol
    T_STRING,
    T_COMMENT,
    T_NONE
  } tokenizer_state;
  
  parser_stack_t *parser;

  void *baton;
  dfsch_parser_callback_t callback;

  int error;
  int level;
};

dfsch_parser_ctx_t* dfsch_parser_create(){
  dfsch_parser_ctx_t *ctx = GC_MALLOC(sizeof(dfsch_parser_ctx_t));
  if (!ctx)
    return NULL;

  ctx->q  = create_queue();
  if (!ctx->q){
    return NULL;
  }

  ctx->tokenizer_state = T_NONE;
  ctx->parser = NULL;

  ctx->level = 0 ;
  ctx->error = DFSCH_PARSER_NOERROR;

  return ctx;
}

void dfsch_parser_callback(dfsch_parser_ctx_t *ctx, 
			   dfsch_parser_callback_t callback,
			   void *baton){

  ctx->callback = callback;
  ctx->baton = baton;

}


static void parser_pop(dfsch_parser_ctx_t *ctx){
  ctx->parser = ctx->parser->next;
  ctx->level--;
}


static void parser_push(dfsch_parser_ctx_t *ctx){
  parser_stack_t *tmp = GC_MALLOC(sizeof(parser_stack_t));

  tmp->next = ctx->parser;

  ctx->parser = tmp;
  ctx->level++;
}


#define PARSE_ERROR(err) ctx->error = err


static void parse_object(dfsch_parser_ctx_t *ctx, dfsch_object_t* obj){
  if (ctx->parser){
    switch(ctx->parser->state){
    case P_LIST:
      {
	dfsch_object_t *new = dfsch_cons(obj, NULL);

	if (ctx->parser->last){
	  dfsch_set_cdr(ctx->parser->last, new);
	}else{
	  ctx->parser->front = new;
	}
	ctx->parser->last = new;
	break;
      }
    case P_DOT:
      if (ctx->parser->last){
	dfsch_set_cdr(ctx->parser->last, obj);
	ctx->parser->state = P_PREEND;
	return;
      }else{
	ctx->error = DFSCH_PARSER_CAR_EXPECTED;
	return;
      }
    case P_QUOTE:
      {
        dfsch_object_t* tag = ctx->parser->tag;
        parser_pop(ctx);
        parse_object(ctx,dfsch_cons(tag, dfsch_cons(obj, NULL)));
        return;
      }
    case P_VECTOR:
      parser_pop(ctx);
      parse_object(ctx,dfsch_list_2_vector(obj));
      return;
    default:
      ctx->error = DFSCH_PARSER_UNEXPECTED_OBJECT;

    }
  }else{
    if (!(*ctx->callback)(obj,ctx->baton))
      ctx->error = DFSCH_PARSER_STOPPED;
  }
}

static void parse_open(dfsch_parser_ctx_t *ctx){
#ifdef P_DEBUG
  printf(";; parse_open\n");
#endif
  parser_push(ctx);
  ctx->parser->state = P_LIST;
  ctx->parser->last = NULL;
  ctx->parser->front = NULL;
}
static void parse_quote(dfsch_parser_ctx_t *ctx, dfsch_object_t* tag){
#ifdef P_DEBUG
  printf(";; parse_quote\n");
#endif
  parser_push(ctx);
  ctx->parser->state = P_QUOTE;
  ctx->parser->last = NULL;
  ctx->parser->front = NULL;
  ctx->parser->tag = tag;
}
static void parse_vector(dfsch_parser_ctx_t *ctx){
#ifdef P_DEBUG
  printf(";; parse_vector\n");
#endif
  parser_push(ctx);
  ctx->parser->state = P_VECTOR;
  ctx->parser->last = NULL;
  ctx->parser->front = NULL;
}
static void parse_close(dfsch_parser_ctx_t *ctx){
#ifdef P_DEBUG
  printf(";; parse_close\n");
#endif
  if (ctx->parser){
    dfsch_object_t *list;
    list = ctx->parser->front;
    parser_pop(ctx);
    parse_object(ctx,list);
  }else{
    ctx->error = DFSCH_PARSER_UNEXPECTED_CLOSE;
  }
}
static void parse_dot(dfsch_parser_ctx_t *ctx){
  if (ctx->parser){
    ctx->parser->state = P_DOT;
  }else{
    ctx->error = DFSCH_PARSER_UNEXPECTED_DOT;
  }
}



static void dispatch_string(dfsch_parser_ctx_t *ctx, char *data){
  char* out=data;
  char* in=data;

  while (*in){
    switch (*in){
    case '\\':
      ++in;
      switch (*in){
      case 'n':
	*out = '\n';
	++out;
	++in;
	continue;
      case 'r':
	*out = '\r';
	++out;
	++in;
	continue;
      case 'a':
	*out = '\a';
	++out;
	++in;
	continue;
      case 't':
	*out = '\t';
	++out;
	++in;
	continue;
      case 'b':
	*out = '\b';
	++out;
	++in;
	continue;
      case 'v':
	*out = '\v';
	++out;
	++in;
	continue;
      case 'f':
	*out = '\f';
	++out;
	++in;
	continue;
      }
    default:
      *out = *in;
      ++out;
      ++in;
    }
  }

  *out = 0;
  
  dfsch_object_t *s = dfsch_make_string(data);

  parse_object(ctx,s);
}
static void dispatch_atom(dfsch_parser_ctx_t *ctx, char *data){
#ifdef T_DEBUG
  printf(";; Atom: [%s]\n",data);
#endif

  switch (*data){
  case '-':
  case '.':

    if (data[1]<'0' || data[1]>'9')
      break;

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
    {
    
      double d = atof(data);
#ifdef T_DEBUG
  printf(";; Number: %lf \n",d);
#endif
      
      parse_object(ctx,dfsch_make_number(d));
      return;
    }
  }

  dfsch_object_t *d = dfsch_make_symbol(data);


  parse_object(ctx,d);

}

#ifdef T_DEBUG
#define STATE_TRANS(state) printf(";; State transition: %s\n",#state)
#else
#define STATE_TRANS(state)
#endif


static void tokenizer_process (dfsch_parser_ctx_t *ctx, char* data){
  while (*data){
    switch (ctx->tokenizer_state){
    case T_NONE:
      while(*data==' ' || *data=='\n' || *data == '\t'){
	++data;
      }
      switch (*data){
      case 0:
	empty_queue(ctx->q);
	return;
      case '"':
	++data;
	ctx->tokenizer_state = T_STRING;
	break;
      case '(':
	++data;
	
	parse_open(ctx);
	if (ctx->error) return;

	break;
      case '\'':
	++data;
	
	parse_quote(ctx,dfsch_sym_quote());
	if (ctx->error) return;

	break;
      case '`':
	++data;
	
	parse_quote(ctx,dfsch_sym_quasiquote());
	if (ctx->error) return;

	break;
      case ',':
	++data;
	
        if (*data == '@'){
          ++data;
          parse_quote(ctx,dfsch_sym_unquote_splicing()); 
        }else{
          parse_quote(ctx,dfsch_sym_unquote());
        }
	if (ctx->error) return;

	break;
      case '#':
	++data;
	
	parse_vector(ctx);
	if (ctx->error) return;

	break;
      case ')':
	++data;
	
	parse_close(ctx);
	if (ctx->error) return;

	break;
      case ';':
	++data;
	ctx->tokenizer_state = T_COMMENT;	
	break;
      case '.':
	++data;
	if (*data==' ' || *data=='\n' || *data=='\t' || 
	    *data==0   || *data=='('  || *data==')'){
	  parse_dot(ctx);
	  if (ctx->error) return;
	  
	  break;
	}
	--data;
      default:
	ctx->tokenizer_state = T_ATOM;
        STATE_TRANS(T_ATOM);
	break;	
      }
      break;
    case T_ATOM:
      {
	char *e = strpbrk(data,"() \t\n;");
	if (!e){
	  consume_queue(ctx->q,data);
	  return;
	}
	char *s = GC_MALLOC_ATOMIC((size_t)(e-data)+1);
	strncpy(s,data,e-data);
	s[e-data]=0;

	dispatch_atom(ctx,s);
	if (ctx->error) return;

	data = e;
	ctx->tokenizer_state = T_NONE;
        STATE_TRANS(T_NONE);
	break;
      }
    case T_STRING:
      {
	char *e= strchr(data,'"');
	if (!e){
	  consume_queue(ctx->q,data);
	  return;
	}
	if (e>data){
	  while (*(e-1)=='\\'){
	    e = strchr(e+1,'"');
	  }
	}

	char *s = GC_MALLOC_ATOMIC((size_t)(e-data)+1);
	strncpy(s,data,e-data);
	s[e-data]=0;

	dispatch_string(ctx,s);
	if (ctx->error) return;

	data = e+1;
	
	ctx->tokenizer_state = T_NONE;
	break;
     }
    case T_COMMENT:
      while (*data){
	if (*data=='\n'){
	  ctx->tokenizer_state = T_NONE;
	  break;
	}
	++data;
      }
    }
  }

  consume_queue(ctx->q,data);


}

int dfsch_parser_feed(dfsch_parser_ctx_t *ctx, char* data){
  if (!ctx)
    return DFSCH_PARSER_NULL;

  ctx->error = DFSCH_PARSER_NOERROR;

  feed_queue(ctx->q, data);

  
  
  tokenizer_process(ctx,get_queue(ctx->q));

  if (ctx->error)
    empty_queue(ctx->q);

  return ctx->error;

}

int dfsch_parser_get_level(dfsch_parser_ctx_t *ctx){
  return ctx->level;
}

void dfsch_parser_reset(dfsch_parser_ctx_t *ctx){

  empty_queue(ctx->q);
  ctx->tokenizer_state = T_NONE;
  ctx->parser = NULL;
  ctx->level = 0 ;
  ctx->error = DFSCH_PARSER_NOERROR;

}
