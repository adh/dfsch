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

#include "stream.h"

#include "strings.h"
#include "unistd.h"

#define Q_DEBUG
#define T_DEBUG

typedef struct string_queue_t {
  char* buf;
  size_t all;
} string_queue_t;

string_queue_t *create_queue(){
  string_queue_t *q = malloc(sizeof(string_queue_t));
  q->buf = malloc(512);
  q->all = 512;
  if (!q->buf){
    abort();
  }

  return q;
}
void destroy_queue(string_queue_t *q){
  if (q->buf){
    free(q->buf);
  }
  free(q);
}
char* get_queue(string_queue_t *q){
  return q->buf;
}
char* feed_queue(string_queue_t *q, char* data){
  size_t new_size = strlen(q->buf) + strlen(data)+1;
  if (new_size > q->all){
    q->buf = realloc(q->buf, new_size+(new_size/2));
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
void consume_queue(string_queue_t *q, char* new){
  size_t nl = strlen(new)+1;
  memmove(q->buf, new, nl);

  if (nl < q->all/2){
    q->buf = realloc(q->buf, nl);
    q->all = nl;    
    if (!q->buf){
      abort();
    }
  }

#ifdef Q_DEBUG
  printf(";; Queue is now: [[[%s]]] \n",get_queue(q));
#endif


}
void empty_queue(string_queue_t *q){
  *(q->buf)=0;
}


struct dfsch_parser_ctx_t {
  string_queue_t *q;

  enum {
    T_ATOM, // i.e. number or symbol
    T_STRING,
    T_COMMENT,
    T_NONE
  } tokenizer_state;

  void *baton;
  dfsch_parser_callback_t *callback;
};

dfsch_parser_ctx_t* dfsch_parser_create(){
  dfsch_parser_ctx_t *ctx = malloc(sizeof(dfsch_parser_ctx_t));
  if (!ctx)
    return NULL;

  ctx->q  = create_queue();
  if (!ctx->q){
    free(ctx);
    return NULL;
  }

  ctx->tokenizer_state = T_NONE;
  return ctx;
}
void dfsch_parser_destroy(dfsch_parser_ctx_t *ctx){
  free(ctx);
}

void dfsch_parser_callback(dfsch_parser_ctx_t *ctx, 
			   dfsch_parser_callback_t *callback,
			   void *baton){

  ctx->callback = callback;
  ctx->baton = baton;

}


static void parse_open(dfsch_parser_ctx_t *ctx){

}
static void parse_close(dfsch_parser_ctx_t *ctx){

}
static void parse_dot(dfsch_parser_ctx_t *ctx){

}
static void parse_terminal(dfsch_parser_ctx_t *ctx, dfsch_object_t* obj){

}




static void dispatch_string(dfsch_parser_ctx_t *ctx, char *data){

}
static void dispatch_atom(dfsch_parser_ctx_t *ctx, char *data){

}


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
	continue;
      case '(':
	++data;
	
	parse_open(ctx);

	continue;
      case ')':
	++data;
	
	parse_close(ctx);

	continue;
      case ';':
	++data;
	ctx->tokenizer_state = T_COMMENT;	
	continue;
      case '.':
	++data;
	if (*data==' ' || *data=='\n' || *data=='\t' || 
	    *data==0   || *data=='('  || *data==')'){
	  parse_dot(ctx);
	  
	  continue;
	}
	--data;
      default:
	ctx->tokenizer_state = T_ATOM;
	continue;	
      }
    case T_ATOM:
      {
	char *e = strpbrk(data,"() \t\n");
	if (!e){
	  consume_queue(ctx->q,data);
	  return;
	}
	char *s = malloc((size_t)(e-data)+1);
	strncpy(s,data,e-data);
	s[e-data]=0;

	dispatch_atom(ctx,s);

	free(s);
	data = e;
	ctx->tokenizer_state = T_NONE;
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

	char *s = malloc((size_t)(e-data)+1);
	strncpy(s,data,e-data);
	s[e-data]=0;

	dispatch_string(ctx,s);

	free(s);
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

  feed_queue(ctx->q, data);

  
  
  tokenizer_process(ctx,get_queue(ctx->q));

}
