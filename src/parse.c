/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Event driven parser.
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

#include "../dfsch/parse.h"
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include <dfsch/magic.h>

#include "util.h"

#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <stdlib.h>

//#define Q_DEBUG
//#define T_DEBUG
//#define P_DEBUG

typedef struct char_table_entry_t {
  char* name;
  uint32_t ch;
} char_table_entry_t;

char_table_entry_t char_table[]={
  {"newline", '\n'},
  {"space", ' '},
};

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
  if (q->buf){
    *(q->buf)=0;
  }
}

typedef struct symbol_map_t symbol_map_t;

struct symbol_map_t {
  char* name;
  dfsch_object_t* symbol;
  symbol_map_t* next;
};

typedef struct parser_directive_t parser_directive_t;

struct parser_directive_t {
  dfsch_object_t* tag;
  dfsch_parser_directive_t directive;
  void* baton;
  parser_directive_t* next;
};

typedef struct parser_stack_t parser_stack_t;

struct parser_stack_t {
  enum {
    P_LIST,
    P_DOT,
    P_PREEND,
    P_QUOTE,
    P_HASH,
    P_EVAL,
    P_DIRECTIVE
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
    T_NONE,
    T_HASH,
    T_CHAR,
    T_CHAR_CONT,
  } tokenizer_state;
  
  parser_stack_t *parser;

  void *baton;
  dfsch_parser_callback_t callback;

  int error;
  int level;
  int line;
  int column;

  symbol_map_t* symbol_map;
  parser_directive_t* directives;
  dfsch_object_t* env;
};

static void parser_reset(dfsch_parser_ctx_t *ctx){
  empty_queue(ctx->q);
  ctx->tokenizer_state = T_NONE;
  ctx->parser = NULL;
  ctx->level = 0 ;
  ctx->line = 1;
  ctx->column = 1;
  ctx->error = 0;
}

static void parser_abort(dfsch_parser_ctx_t *ctx, char* symbol){
  dfsch_object_t* pos = dfsch_cons(dfsch_make_number_from_long(ctx->line),
                                   dfsch_make_number_from_long(ctx->column));

  parser_reset(ctx);
  dfsch_error(symbol, pos);
}
static void parser_abort_ex(dfsch_parser_ctx_t *ctx, dfsch_object_t* ex){
  parser_reset(ctx);
  dfsch_raise(ex);
}

dfsch_parser_ctx_t* dfsch_parser_create(){
  dfsch_parser_ctx_t *ctx = GC_MALLOC(sizeof(dfsch_parser_ctx_t));
  if (!ctx)
    return NULL;

  ctx->q = create_queue();
  if (!ctx->q){
    return NULL;
  }

  ctx->tokenizer_state = T_NONE;
  ctx->parser = NULL;

  ctx->line = 1;
  ctx->column = 1;

  ctx->level = 0 ;
  ctx->error = 0;

  ctx->symbol_map = NULL;
  ctx->env = 0;
  dfsch_parser_define_default_directives(ctx);

  return ctx;
}

static dfsch_object_t* make_symbol(dfsch_parser_ctx_t* ctx, char* sym){
  symbol_map_t* i = ctx->symbol_map;

  while (i){
    if (ascii_strcasecmp(sym, i->name) == 0){
      return i->symbol;
    }
    i = i->next;
  }

  return dfsch_make_symbol(sym);
}

void dfsch_parser_define_symbol(dfsch_parser_ctx_t* ctx, 
                                char* name,
                                dfsch_object_t* symbol){
  symbol_map_t* entry;

  entry = ctx->symbol_map;
  while (entry) {
    if (ascii_strcasecmp(name, entry->name) == 0){
      entry->symbol = symbol;
      return;
    }
    entry = entry->next;
  }

  entry = GC_NEW(symbol_map_t);
  entry->symbol = symbol;
  entry->name = name;

  entry->next = ctx->symbol_map;
  ctx->symbol_map = entry;
}
void dfsch_parser_undefine_symbol(dfsch_parser_ctx_t* ctx, 
                                  char* name){
  symbol_map_t* i = ctx->symbol_map;
  symbol_map_t* j;

  if (!i){
    return;
  }

  if (ascii_strcasecmp(name, i->name) == 0){
    ctx->symbol_map = i->next;
  }

  while (i){
    if (ascii_strcasecmp(name, i->name) == 0){
      j->next = i->next;
      return;
    }
    i = i->next;
  }  
}
void dfsch_parser_import_symbol(dfsch_parser_ctx_t* ctx,
                                dfsch_object_t* symbol){
  char* name;

  name = rindex(dfsch_symbol(symbol), ':');

  if (name && name[1]!=0){
    dfsch_parser_define_symbol(ctx, name + 1, symbol);
  }
}

void dfsch_parser_callback(dfsch_parser_ctx_t *ctx, 
			   dfsch_parser_callback_t callback,
			   void *baton){

  ctx->callback = callback;
  ctx->baton = baton;

}
void dfsch_parser_eval_env(dfsch_parser_ctx_t *ctx, 
			   dfsch_object_t* env){
  ctx->env = env;
}
void dfsch_parser_define_directive(dfsch_parser_ctx_t* ctx, 
                                   dfsch_object_t* tag,
                                   dfsch_parser_directive_t directive,
                                   void* baton){
  parser_directive_t* entry = GC_NEW(parser_directive_t);

  entry->tag = tag;
  entry->directive = directive;
  entry->baton = baton;

  entry->next = ctx->directives;
  ctx->directives = entry;
}
void dfsch_parser_define_directive_cstr(dfsch_parser_ctx_t* ctx, 
                                        char* tag,
                                        dfsch_parser_directive_t directive,
                                        void* baton){
  dfsch_parser_define_directive(ctx, dfsch_make_symbol(tag), directive, baton);
}
void dfsch_parser_clear_directives(dfsch_parser_ctx_t* ctx){
  ctx->directives = NULL;
}
void dfsch_parser_clear_symbols(dfsch_parser_ctx_t* ctx){
  ctx->symbol_map = NULL;
}

static int directive_import(dfsch_parser_ctx_t* ctx, 
                            dfsch_object_t* args, 
                            void*baton){
  while (dfsch_pair_p(args)){
    dfsch_parser_import_symbol(ctx, dfsch_car(args));
    args = dfsch_cdr(args);
  }

  return 1;
}

void dfsch_parser_define_default_directives(dfsch_parser_ctx_t* ctx){
  ctx->directives = NULL;
  dfsch_parser_define_directive_cstr(ctx, "import", directive_import, NULL);
}

static void process_directive(dfsch_parser_ctx_t *ctx,
                              dfsch_object_t* object){
  dfsch_object_t* tag;
  dfsch_object_t* args;
  dfsch_object_t* obj;
  parser_directive_t* i = ctx->directives;

  if (!dfsch_pair_p(object)){
    parser_abort(ctx, "parser-invalid-directive");
    return;
  }

  tag = dfsch_car(object);
  args = dfsch_cdr(object);

  while (i){
    if (i->tag == tag){
      if(!i->directive(ctx, args, i->baton)){
        parser_abort(ctx, "parser-invalid-directive");
      }
      return;
    }
    i = i->next;
  }

  parser_abort(ctx, "parser-invalid-directive");
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

#define parse_object dfsch_parser_parse_object
void dfsch_parser_parse_object(dfsch_parser_ctx_t *ctx, dfsch_object_t* obj){
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
        parser_abort(ctx, "parser:car-expected");
      }
    case P_QUOTE:
      {
        dfsch_object_t* tag = ctx->parser->tag;
        parser_pop(ctx);
        parse_object(ctx, dfsch_cons(tag, dfsch_cons(obj, NULL)));
        return;
      }
    case P_HASH:
      parser_pop(ctx);
      if ((!obj) || dfsch_pair_p(obj)){
        parse_object(ctx, dfsch_list_2_vector(obj));
      }else{
        parser_abort(ctx, "parser:list-expected");
      }
      return;
    case P_EVAL:
      parser_pop(ctx);
      if (ctx->env){
        parse_object(ctx, dfsch_eval(obj, ctx->env));
        /* XXX: What happens when this throws exception is not entirely clear */
      }else{
        parser_abort(ctx, "parser:evaluation-not-permitted");        
      }
      return;
    case P_DIRECTIVE:
      parser_pop(ctx);
      process_directive(ctx, obj);
      return;
    default:
      parser_abort(ctx, "parser:unexpected-object");
    }
  }else{
    //    DFSCH_TRY { XXX
      if (!(*ctx->callback)(obj,ctx->baton)){
        ctx->error = 1;
      }
      /*} DFSCH_SCATCH(ex) {
      parser_abort_ex(ctx, ex);
      } DFSCH_SCATCHEND;*/
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
  ctx->parser->state = P_HASH;
  ctx->parser->last = NULL;
  ctx->parser->front = NULL;
}
static void parse_close(dfsch_parser_ctx_t *ctx){
#ifdef P_DEBUG
  printf(";; parse_close\n");
#endif
  if (ctx->parser && (ctx->parser->state == P_PREEND || 
                      ctx->parser->state == P_LIST)){
    dfsch_object_t *list;
    list = ctx->parser->front;
    parser_pop(ctx);
    parse_object(ctx, list);
  }else{
    parser_abort(ctx, "parser:unexpected-close");
  }
}
static void parse_dot(dfsch_parser_ctx_t *ctx){
  if (ctx->parser){
    ctx->parser->state = P_DOT;
  }else{
    parser_abort(ctx, "parser:unexpected-dot");
  }
}
static void parse_eval(dfsch_parser_ctx_t *ctx){
#ifdef P_DEBUG
  printf(";; parse_eval\n");
#endif
  parser_push(ctx);
  ctx->parser->state = P_EVAL;
  ctx->parser->last = NULL;
  ctx->parser->front = NULL;
}
static void parse_directive(dfsch_parser_ctx_t *ctx){
#ifdef P_DEBUG
  printf(";; parse_directive\n");
#endif
  parser_push(ctx);
  ctx->parser->state = P_DIRECTIVE;
  ctx->parser->last = NULL;
  ctx->parser->front = NULL;
}



static void dispatch_string(dfsch_parser_ctx_t *ctx, char *data){
  char* out=data;
  char* in=data;

  while (*in){
    switch (*in){
    case '\\':
      ++in;
      switch (*in){
      case '\n':
	++in;
	continue;        
      case '\r':
	++in;
        if (*in == '\n'){
          ++in;
        }
	continue;        
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
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
	*out = *in -'0';
        ++in;
        
        if (*in >= '0' && *in <= '7'){
          *out = ((*out)<<3) | (*in - '0');
          ++in;
          if (*in >= '0' && *in <= '7'){
            *out = ((*out)<<3) | (*in - '0');
            ++in;
          }          
        }
	++out;
	continue;
      case 'x':
        {
          int i;
          *out = 0;
          ++in;
          for (i=0; i<2; i++){
            *out <<= 4;
            if (!*in){
              parser_abort(ctx, "parser:invalid-escape");
            }
            if (*in >= 'A' && *in <= 'F'){
              *out |= *in - 'A' + 10;
            }else if (*in >= 'a' && *in <= 'f'){
              *out |= *in - 'a' + 10;
            }else if (*in >= '0' && *in <= '9'){
              *out |= *in - '0';
            }else{
              parser_abort(ctx, "parser:invalid-escape");
            }
            ++in;
          }
          ++out;
          continue;
        }
      case 'u':
      case 'U':
        {
          int i;
          uint32_t u = 0;
          for (i = (*(in++) == 'U') ? 0 : 4; i<8; i++){ // XXX: clever and ugly
            u <<= 4;
            if (!*in){
              parser_abort(ctx, "parser:invalid-escape");
            }
            if (*in >= 'A' && *in <= 'F'){
              u |= *in - 'A' + 10;
            }else if (*in >= 'a' && *in <= 'f'){
              u |= *in - 'a' + 10;
            }else if (*in >= '0' && *in <= '9'){
              u |= *in - '0';
            }else{
              parser_abort(ctx, "parser:invalid-escape");
            }
            ++in;
          }

          if (u <= 0x7f){
            *(out++) = u;
          } else if (u <= 0x7ff) {
            *(out++) = 0xc0 | ((u >> 6) & 0x1f); 
            *(out++) = 0x80 | (u & 0x3f);
          } else if (u <= 0xffff) {
            *(out++) = 0xe0 | ((u >> 12) & 0x0f); 
            *(out++) = 0x80 | ((u >> 6) & 0x3f);
            *(out++) = 0x80 | (u & 0x3f);
          } else {
            *(out++) = 0xf0 | ((u >> 18) & 0x07); 
            *(out++) = 0x80 | ((u >> 12) & 0x3f);
            *(out++) = 0x80 | ((u >> 6) & 0x3f);
            *(out++) = 0x80 | (u & 0x3f);
          } 
          continue;
        }        
      default:
        *out = *in;
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
  
  dfsch_object_t *s = dfsch_make_string_buf(data, out-data);

  parse_object(ctx, s);
}
static void dispatch_atom(dfsch_parser_ctx_t *ctx, char *data){
#ifdef T_DEBUG
  printf(";; Atom: [%s]\n", data);
#endif

  switch (*data){
  case '-':
  case '+':
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
      dfsch_object_t *d;
      //DFSCH_TRY {   XXX
        d = dfsch_make_number_from_string(data, 0);
        /*} DFSCH_CATCH(ex) {
        parser_abort_ex(ctx, ex);
        } DFSCH_END_TRY;*/
      if (!d) {
        parser_abort(ctx, "parser:invalid-number");
      }
      parse_object(ctx, d);
      return;
    }
  }

  dfsch_object_t *d = make_symbol(ctx, data);

  parse_object(ctx,d);
}

#ifdef T_DEBUG
#define STATE_TRANS(state) printf(";; State transition: %s\n",#state)
#else
#define STATE_TRANS(state)
#endif

/*
 * It's probably wise to somehow generalize this routine in order to support
 * "normal" scheme ports and such things.
 */
static void tokenizer_process (dfsch_parser_ctx_t *ctx, char* data){
  while (*data){
    switch (ctx->tokenizer_state){
    case T_NONE:
      while(*data==' ' || *data=='\n' || *data == '\t' || *data == '\r'){
        ctx->column++;
        if (*data == '\n'){
          ctx->column = 1;
          ctx->line++;
        
        }
	++data;
      }
      switch (*data){
      case 0:
	empty_queue(ctx->q);
	return;
      case '"':
	++data;
        ctx->column++;

	ctx->tokenizer_state = T_STRING;
	break;
      case '(':
	++data;
        ctx->column++;
	
	parse_open(ctx);
	if (ctx->error) return;

	break;
      case '\'':
	++data;
        ctx->column++;
	
	parse_quote(ctx, dfsch_sym_quote());
	if (ctx->error) return;

	break;
      case '`':
	++data;
        ctx->column++;

	
	parse_quote(ctx, dfsch_sym_quasiquote());
	if (ctx->error) return;

	break;
      case ',':
	++data;
        ctx->column++;	

        if (*data == '@'){
          ++data;
          ctx->column++;
        
          parse_quote(ctx, dfsch_sym_unquote_splicing()); 
        }else{
          parse_quote(ctx, dfsch_sym_unquote());
        }
	if (ctx->error) return;

	break;
      case '#':
	++data;
        ctx->column++;

	ctx->tokenizer_state = T_HASH;	
        break;

      case ')':
	++data;
        ctx->column++;
	
	parse_close(ctx);
	if (ctx->error) return;

	break;
      case ';':
	++data;
        ctx->column++;

	ctx->tokenizer_state = T_COMMENT;	
	break;
      case '.':
	++data;

	if (*data==' ' || *data=='\n' || *data=='\t' || *data=='\r' ||
	    *data==0   || *data=='('  || *data==')'){
          ctx->column++;
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
	char *e = strpbrk(data, "() \t\n\r;");
	if (!e){
          consume_queue(ctx->q, data);
	  return;
	}
	char *s = GC_MALLOC_ATOMIC((size_t)(e-data)+1);
	strncpy(s,data,e-data);
	s[e-data]=0;

	dispatch_atom(ctx, s);
	if (ctx->error) return;

        ctx->column += e-data;
	data = e;
	ctx->tokenizer_state = T_NONE;
        STATE_TRANS(T_NONE);
	break;
      }
    case T_STRING: // TODO: count characters and lines
      {
	char *e= strchr(data, '"');
	if (!e){
          consume_queue(ctx->q, data);
	  return;
	}
	if (e>data){
	  while (*(e-1)=='\\'){
	    e = strchr(e+1, '"');
	  }
	}

	char *s = GC_MALLOC_ATOMIC((size_t)(e-data)+1);
	strncpy(s, data, e-data);
	s[e-data]=0;

	dispatch_string(ctx, s);
	if (ctx->error) return;

	data = e+1;
	
	ctx->tokenizer_state = T_NONE;
	break;
     }
    case T_COMMENT:
      while (*data){
	if (*data=='\n'){
	  ctx->tokenizer_state = T_NONE;
          ctx->column = 1;
          ctx->line++;
	  break;
	}
	++data;
        ctx->column++;
      }
      break;
    case T_HASH:
      switch(*data){
      case 'n':
      case 'f':
      case 'N':
      case 'F':
        ++data;
        ctx->column++;

        parse_object(ctx,NULL);
	if (ctx->error) return;
        ctx->tokenizer_state = T_NONE;
        break;
      case 't':
      case 'T':
        ++data;
        ctx->column++;

        parse_object(ctx,dfsch_sym_true());
	if (ctx->error) return;
        ctx->tokenizer_state = T_NONE;  
        break;
      case '\\':
        ++data;
        ctx->column++;

        ctx->tokenizer_state = T_CHAR;
        break;
      case '(':
        parse_vector(ctx);
        if (ctx->error) return;
        ctx->tokenizer_state = T_NONE;
        break;
      case '<':
        parser_abort(ctx, "parser:unreadable");
        return;        
      case '!': /* for shebang */
        ++data;
        ctx->column++;
        ctx->tokenizer_state = T_COMMENT;
        break;
      case ',':
        ++data;
        ctx->column++;
        parse_directive(ctx);
        ctx->tokenizer_state = T_NONE;
        break;
      case '.':
        ++data;
        ctx->column++;
        parse_eval(ctx);
        ctx->tokenizer_state = T_NONE;
        break;
      default:
        parser_abort(ctx, "parser:invalid-escape");
        return;
      }
      break;
    case T_CHAR:
      {
        unsigned char c = *data;
        if ((c>='a' && c<= 'z') || (c>='A' && c<= 'Z')){
          char *e = strpbrk(data, "() \t\n\r;");
          if (!e){
            consume_queue(ctx->q, data);
            return;
          }
          if (e - data ==  1) // One character
            goto simple;

          char *s = GC_MALLOC_ATOMIC((size_t)(e-data)+1);
          int i;

          strncpy(s, data, e-data);
          ctx->column += e-data;
          s[e-data]=0;
          
          for (i=0; i < sizeof(char_table)/sizeof(char_table_entry_t); i++){

            if (ascii_strcasecmp(s, char_table[i].name)==0){
              parse_object(ctx, dfsch_make_number_from_long(char_table[i].ch));
              if (ctx->error) return;
              goto char_out;
            }
          } 

          parser_abort(ctx, "parser:invalid-escape");

        char_out:
          data = e;
          ctx->tokenizer_state = T_NONE;
          break;
          
        }else{
        simple:
          ++data;
          ctx->column++;
          if (c < 0x80){
            parse_object(ctx, dfsch_make_number_from_long(c));
            ctx->tokenizer_state = T_NONE;
          } else {
            long ch;

            if ((c & 0xe0) == 0xc0 && (c & 0x1f) != 0x00){
              if (*data == 0){
                consume_queue(ctx->q, data-1);
                return;
              }
              if ((*data & 0xc0) != 0x80){
                parser_abort(ctx, "parser:invalid-unicode-character0");
              }
              ch = (c & 0x1f);
              ch <<= 6;
              ch |= (*data & 0x3f);
              data++;
            } else if ((c & 0xf0) == 0xe0 && (c & 0x0f) != 0x00){
              if (data[0] == 0 || data[1] == 0){
                consume_queue(ctx->q, data-1);
                return;
              }
              if ((data[1] & 0xc0) != 0x80 || (data[1] & 0xc0) != 0x80){
                parser_abort(ctx, "parser:invalid-unicode-character1");
              }
              ch = (c & 0x0f);
              ch <<= 6;
              ch |= (*data & 0x3f);
              data++;
              ch <<= 6;
              ch |= (*data & 0x3f);
              data++;
            } else if ((c & 0xf8) == 0xf0 && (c & 0x07) != 0x00){
              if (data[0] == 0 || data[1] == 0 || data[2] == 0){
                consume_queue(ctx->q, data-1);
                return;
              }
              if ((data[0] & 0xc0) != 0x80 ||
                  (data[1] & 0xc0) != 0x80 || 
                  (data[2] & 0xc0) != 0x80){
                parser_abort(ctx, "parser:invalid-unicode-character2");
              }
              ch = (c & 0x07);
              ch <<= 6;
              ch |= (*data & 0x3f);
              data++;
              ch <<= 6;
              ch |= (*data & 0x3f);
              data++;
              ch <<= 6;
              ch |= (*data & 0x3f);
              data++;
            } else {
              parser_abort(ctx, "parser:invalid-unicode-character3");
            }

            parse_object(ctx, dfsch_make_number_from_long(ch));
            ctx->tokenizer_state = T_NONE;
          }
          break;          
        }
      }
    }
  }

  consume_queue(ctx->q, data);
}

int dfsch_parser_feed(dfsch_parser_ctx_t *ctx, char* data){
  ctx->error = 0;
  DFSCH_UNWIND{
  feed_queue(ctx->q, data);

  tokenizer_process(ctx, get_queue(ctx->q));
  }DFSCH_PROTECT{
    
  }DFSCH_UNWIND_DETECT{
    parser_reset(ctx);
  }DFSCH_PROTECT_END;

  return ctx->error;
}
int dfsch_parser_feed_line(dfsch_parser_ctx_t *ctx, char* data){
  int ret;
  if (ret = dfsch_parser_feed(ctx, data)){
    return ret;
  }
  return dfsch_parser_feed(ctx, "\n");
}


char* dfsch_parser_feed_catch(dfsch_parser_ctx_t *ctx, char* data){
  char *ret = NULL;
  
  dfsch_parser_feed(ctx, data);
    
  return ret;
}

int dfsch_parser_get_level(dfsch_parser_ctx_t *ctx){
  return ctx->level;
}

void dfsch_parser_reset(dfsch_parser_ctx_t *ctx){
  empty_queue(ctx->q);
  ctx->tokenizer_state = T_NONE;
  ctx->parser = NULL;
  ctx->level = 0 ;
  ctx->error = 0;
}

int dfsch_parser_top_level(dfsch_parser_ctx_t *ctx){
  return (ctx->parser == NULL) & (ctx->tokenizer_state == T_NONE);
}

static int read_callback(dfsch_object_t* obj, dfsch_object_t** res){
  *res = obj;
  return 0;
}

/*
 * There is slight bug - when first character after atom is (, ) or ;
 * it's silently thrown away. This clearly is undesired behavior, but
 * in my opinion does not break anyhing significant. So let's call it 
 * feature for now.
 */

dfsch_object_t* dfsch_parser_read_from_port(dfsch_object_t* port){
  dfsch_parser_ctx_t* parser = dfsch_parser_create();
  int ch;
  char buf[2];
  dfsch_object_t* res;
  int ok = 0;

  dfsch_port_batch_read_start(port);
  dfsch_parser_callback(parser, read_callback, &res);
  DFSCH_UNWIND {
    while ((ch = dfsch_port_batch_read(port)) != -1){
      buf[0] = ch;
      buf[1] = 0;
      
      if (dfsch_parser_feed(parser, buf)){
        dfsch_port_batch_read_end(port);
        ok = 1;
        break;
      }
    }
  } DFSCH_PROTECT {
    dfsch_port_batch_read_end(port);
  } DFSCH_PROTECT_END;
  if (ok){
    return res;
  }
  if (dfsch_parser_top_level(parser)){
    return dfsch_eof_object();
  } else {
    dfsch_error("parser:unexpected-end-of-file", port);
  }
}
