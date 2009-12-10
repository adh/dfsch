#include "dfsch/lib/json.h"
#include <dfsch/number.h>
#include <dfsch/hash.h>
#include <dfsch/magic.h>
#include <dfsch/ports.h>
#include "src/util.h"
#include <stdlib.h>

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

typedef enum {
  S_OBJECT_KEY,
  S_OBJECT_COLON,
  S_OBJECT_VALUE,
  S_OBJECT_COMMA,

  S_ARRAY_VALUE,
  S_ARRAY_COMMA,
} parser_state_t;

typedef struct parser_stack_t parser_stack_t;

struct parser_stack_t {
  parser_state_t state;
  dfsch_object_t* object;
  dfsch_object_t* tmp;
  parser_stack_t* next;
};

struct dfsch_json_parser_t {
  dfsch_type_t* type;
  
  string_queue_t* queue;
  parser_stack_t* state;

  dfsch_parser_callback_t callback;
  void *baton;
};

dfsch_type_t dfsch_json_parser_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "json:parser",
  .size = sizeof(dfsch_json_parser_t)
};

dfsch_json_parser_t* dfsch_make_json_parser(){
  dfsch_json_parser_t* jp = dfsch_make_object(DFSCH_JSON_PARSER_TYPE);

  jp->queue = create_queue();

  return jp;
}
void dfsch_json_parser_set_callback(dfsch_json_parser_t *jp, 
				    dfsch_parser_callback_t callback,
				    void *baton){
  jp->callback = callback;
  jp->baton = baton;
}

static void parse_object(dfsch_json_parser_t *jp, dfsch_object_t* obj){
  dfsch_object_t* v;
  if (!jp->state){
    jp->callback(obj, jp->baton);
  } else {
    switch (jp->state->state){
    case S_ARRAY_VALUE:
      v = dfsch_cons(obj, NULL);
      if (jp->state->object){
        DFSCH_FAST_CDR_MUT(jp->state->tmp) = v;
        jp->state->tmp = v;
      } else {
        jp->state->object = jp->state->tmp = v;
      }
      jp->state->state = S_ARRAY_COMMA;      
      break;
    case S_OBJECT_KEY:
      jp->state->tmp = obj;
      jp->state->state = S_OBJECT_COLON;
      break;
    case S_OBJECT_VALUE:
      dfsch_hash_set(jp->state->object, jp->state->tmp, obj);
      jp->state->state = S_OBJECT_COMMA;
      break;
    }
  }

}

static void parser_push(dfsch_json_parser_t* jp,
                        parser_state_t state){
  parser_stack_t* ps = GC_NEW(parser_stack_t);
  ps->next = jp->state;
  ps->state = state;
  jp->state = ps;
}

static void parser_pop(dfsch_json_parser_t* jp){
  dfsch_object_t* tmp = jp->state->object;
  jp->state = jp->state->next;
  parse_object(jp, tmp);
}

static void array_start(dfsch_json_parser_t* jp){
  if (jp->state && (jp->state->state != S_OBJECT_VALUE && 
                    jp->state->state != S_ARRAY_VALUE)){
    dfsch_error("Syntax error: array not permitted here", NULL);
  }

  parser_push(jp, S_ARRAY_VALUE);
}
static void array_end(dfsch_json_parser_t* jp){
  if (!jp->state || (jp->state->state != S_ARRAY_COMMA &&
                     jp->state->state != S_ARRAY_VALUE)){
    dfsch_error("Syntax error: unexpected ]", NULL);    
  }

  parser_pop(jp);
}
static void object_start(dfsch_json_parser_t* jp){
  if (jp->state && (jp->state->state != S_OBJECT_VALUE && 
                    jp->state->state != S_ARRAY_VALUE)){
    dfsch_error("Syntax error: object not permitted here", NULL);
  }

  parser_push(jp, S_OBJECT_KEY);
  jp->state->object = dfsch_hash_make(DFSCH_HASH_EQUAL);
}
static void object_end(dfsch_json_parser_t* jp){
  if (!jp->state || (jp->state->state != S_OBJECT_COMMA &&
                     jp->state->state != S_OBJECT_KEY)){
    dfsch_error("Syntax error: unexpected )", NULL);    
  }

  parser_pop(jp);
}
static void comma(dfsch_json_parser_t* jp){
  if (jp->state){
    switch (jp->state->state){
    case S_OBJECT_COMMA:
      jp->state->state = S_OBJECT_KEY;
      return;
    case S_ARRAY_COMMA:
      jp->state->state = S_ARRAY_VALUE;
      return;
    }
  }
  dfsch_error("Syntax error: unexpected ,", NULL);  
}
static void colon(dfsch_json_parser_t* jp){
  if (!jp->state || jp->state->state != S_OBJECT_COLON){
    dfsch_error("Syntax error: unexpected :", NULL);    
  }
  jp->state->state = S_OBJECT_VALUE;
}

static void parse_string(dfsch_json_parser_t *jp, char *data){
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
              dfsch_error("Invalid escape in JSON data", NULL);
            }
            if (*in >= 'A' && *in <= 'F'){
              *out |= *in - 'A' + 10;
            }else if (*in >= 'a' && *in <= 'f'){
              *out |= *in - 'a' + 10;
            }else if (*in >= '0' && *in <= '9'){
              *out |= *in - '0';
            }else{
              dfsch_error("Invalid escape in JSON data", NULL);
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
              dfsch_error("Invalid escape in JSON data", NULL);
            }
            if (*in >= 'A' && *in <= 'F'){
              u |= *in - 'A' + 10;
            }else if (*in >= 'a' && *in <= 'f'){
              u |= *in - 'a' + 10;
            }else if (*in >= '0' && *in <= '9'){
              u |= *in - '0';
            }else{
              dfsch_error("Invalid escape in JSON data", NULL);
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
  
  parse_object(jp, dfsch_make_string_cstr(data));
}

static void parse_atom(dfsch_json_parser_t *jp, char *data){
  char* e;
  
  if (strcmp(data, "false") == 0){
    parse_object(jp, NULL);
  } else if (strcmp(data, "true") == 0){
    parse_object(jp, DFSCH_SYM_TRUE);
  } else if (strcmp(data, "null") == 0){
    parse_object(jp, NULL);
  } else if (strcspn(data, ".eE") == strlen(data)){
    long long n = strtoll(data, &e, 10);
    if (*e){
      dfsch_error("Invalid value in JSON stream", 
                  dfsch_make_string_cstr(data));
    }
    parse_object(jp, dfsch_make_number_from_int64(n));
  } else {
    double n = strtod(data, &e);
    if (*e){
      dfsch_error("Invalid value in JSON stream", 
                  dfsch_make_string_cstr(data));
    }
    parse_object(jp, dfsch_make_number_from_double(n));
  }
}

static void parse_queue(dfsch_json_parser_t* jp){
  char* b = get_queue(jp->queue);

  while (*b){
    switch (*b){
    case '[':
      array_start(jp);
      b++;
      break;
    case ']':
      array_end(jp);
      b++;
      break;

    case '{':
      object_start(jp);
      b++;
      break;
    case '}':
      object_end(jp);
      b++;
      break;

    case ',':
      comma(jp);
      b++;
      break;

    case ':':
      colon(jp);
      b++;
      break;

    case '\n':
    case '\r':
    case ' ':
    case '\t':
    case '\f':
      b++;
      break;

    case '"':
      {
        char* e = strchr(b + 1, '"');
        if (!e){
          return;
        }
        while (e[-1] == '\\'){
          e = strchr(e + 1, '"');
          if (!e){
            return;
          }
        }

        parse_string(jp, dfsch_strancpy(b + 1, e - b - 1));
        b = e + 1;
      }
      break;
    default:
      {
	char *e = strpbrk(b, "{}[],() \t\n\r\f;:");
	if (!e){
          return;
        }

        parse_atom(jp, dfsch_strancpy(b, e - b));
        b = e;
      }
    }
  }

 out:
  consume_queue(jp->queue, b);
}

void dfsch_json_parser_feed(dfsch_json_parser_t* jp,
                            char* buf){
  feed_queue(jp->queue, buf);
  parse_queue(jp);
}

typedef struct callback_ctx_t {
  dfsch_object_t* head;
  dfsch_object_t* tail;  
} callback_ctx_t;

static list_callback(dfsch_object_t *obj, callback_ctx_t* ctx){
  dfsch_object_t* new_tail = dfsch_cons(obj, NULL);

  if (!ctx->tail){
    ctx->head = new_tail;
  }else{
    dfsch_set_cdr(ctx->tail, new_tail);
  }

  ctx->tail = new_tail;

  return 1;
}
static object_callback(dfsch_object_t *obj, callback_ctx_t* ctx){
  if (ctx->head != DFSCH_INVALID_OBJECT){
    dfsch_error("Too many objects in JSON stream", obj);
  }
  ctx->head = obj;
  return 1;
}


#define BUFFER_LENGTH 4096

dfsch_object_t* dfsch_json_parse_file(char* filename, int list){
  dfsch_json_parser_t* jp = dfsch_make_json_parser();
  callback_ctx_t ctx;
  FILE* f;
  size_t r;
  char buf[BUFFER_LENGTH];

  f = fopen(filename, "r");
  if (!f){
    dfsch_operating_system_error("Cannot open file");
  }

  DFSCH_UNWIND {

    ctx.head = DFSCH_INVALID_OBJECT;
    ctx.tail = NULL;
    
    if (list){
      dfsch_json_parser_set_callback(jp, list_callback, &ctx);
    } else {
      dfsch_json_parser_set_callback(jp, object_callback, &ctx);
    }
    
    
    while ((!feof(f)) && (!ferror(f))){
      if (!fgets(buf, BUFFER_LENGTH, f)){
        break;
      }
      dfsch_json_parser_feed(jp, buf);
    }
    if (ferror(f)){
      dfsch_operating_system_error("Read error");
    }
    
  } DFSCH_PROTECT {
    fclose(f);
  } DFSCH_PROTECT_END;
  
  if (ctx.head == DFSCH_INVALID_OBJECT){
    dfsch_error("Incomplete JSON object", NULL);
  }

  return ctx.head;
}
dfsch_object_t* dfsch_json_parse_port(dfsch_object_t* port, int list){
  dfsch_json_parser_t* jp = dfsch_make_json_parser();
  callback_ctx_t ctx;
  ssize_t r;
  char buf[BUFFER_LENGTH+1];

  ctx.head = DFSCH_INVALID_OBJECT;
  ctx.tail = NULL;

  if (list){
    dfsch_json_parser_set_callback(jp, list_callback, &ctx);
  } else {
    dfsch_json_parser_set_callback(jp, object_callback, &ctx);
  }

  while (1){
    r = dfsch_port_read_buf(port, buf, BUFFER_LENGTH);
    if (r==0){
      break;
    }
    buf[r] = '\0';
    dfsch_json_parser_feed(jp, buf);
  }


  if (ctx.head == DFSCH_INVALID_OBJECT){
    dfsch_error("Incomplete JSON object", NULL);
  }

  return ctx.head;  
}
dfsch_object_t* dfsch_json_parse_buf(char* buf, size_t len, int list){
  return dfsch_json_parse_cstr(buf, list);
}
dfsch_object_t* dfsch_json_parse_strbuf(dfsch_strbuf_t* b, int list){
  return dfsch_json_parse_cstr(b->ptr, list);
}
dfsch_object_t* dfsch_json_parse_cstr(char* s, int list){
  dfsch_json_parser_t* jp = dfsch_make_json_parser();
  callback_ctx_t ctx;

  ctx.head = DFSCH_INVALID_OBJECT;
  ctx.tail = NULL;

  if (list){
    dfsch_json_parser_set_callback(jp, list_callback, &ctx);
  } else {
    dfsch_json_parser_set_callback(jp, object_callback, &ctx);
  }

  dfsch_json_parser_feed(jp, s);

  if (ctx.head == DFSCH_INVALID_OBJECT){
    dfsch_error("Incomplete JSON object", NULL);
  }

  return ctx.head;
}

typedef void (*write_cb_t)(void* target, char* buf);

static void emit_json_object(dfsch_object_t* obj, 
                             write_cb_t cb, void* target);

static void emit_json_hash(dfsch_object_t* obj, 
                           write_cb_t cb, void* target){
  dfsch_object_t* i = dfsch_hash_2_alist(obj);
  int comma = 0;

  cb(target, "{");

  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* key = dfsch_list_item(DFSCH_FAST_CAR(i), 0);
    dfsch_object_t* value = dfsch_list_item(DFSCH_FAST_CAR(i), 1);
    
    if (comma){
      cb(target, ", ");
    }
    comma = 1;

    emit_json_object(key, cb, target);
    cb(target, ": ");
    emit_json_object(value, cb, target);

    i = DFSCH_FAST_CDR(i);
  }

  cb(target, "}");  
}

static void emit_json_list(dfsch_object_t* obj, 
                           write_cb_t cb, void* target){
  dfsch_object_t* i = obj;
  int comma = 0;

  cb(target, "[");

  while (DFSCH_PAIR_P(i)){
    if (comma){
      cb(target, ", ");
    }
    comma = 1;

    emit_json_object(DFSCH_FAST_CAR(i), cb, target);
    i = DFSCH_FAST_CDR(i);
  }

  if (i){
    dfsch_error("Improper list", obj);
  }

  cb(target, "]");  
}


static void emit_json_object(dfsch_object_t* obj, 
                             write_cb_t cb, void* target){
  if (DFSCH_INSTANCE_P(obj, DFSCH_HASH_BASETYPE)){
    emit_json_hash(obj, cb, target);
  } else if (DFSCH_INSTANCE_P(obj, DFSCH_LIST_TYPE)){
    emit_json_list(obj, cb, target);    
  } else if (DFSCH_INSTANCE_P(obj, DFSCH_VECTOR_TYPE)){
    emit_json_list(dfsch_vector_2_list(obj), cb, target);    
  } else if (obj == DFSCH_SYM_TRUE){
    cb(target, "true");
  } else if (DFSCH_SYMBOL_P(obj)){
    dfsch_object_t* str = dfsch_make_string_cstr(dfsch_symbol_qualified_name(obj));
    cb(target, dfsch_object_2_string(str, 100, 1));
  } else {
    cb(target, dfsch_object_2_string(obj, 100, 1));
  }
}

char* dfsch_json_emit_cstr(dfsch_object_t* obj){
  str_list_t* sl = sl_create();

  emit_json_object(obj, sl_append, sl);

  return sl_value(sl);
}
void dfsch_json_emit_port(dfsch_object_t* obj, dfsch_object_t* port){
  emit_json_object(obj, dfsch_port_write_cstr, port);
}

static void rfputs(FILE* f, char*s){
  if (fputs(s, f) == EOF){
    dfsch_error("fputs failed", NULL);
  }
}
void dfsch_json_emit_file(dfsch_object_t* obj, char* filename){
  FILE* f = fopen(filename, "w");
  if (!f){
    dfsch_operating_system_error("Cannot open output file");
  }
  
  DFSCH_UNWIND {
    emit_json_object(obj, rfputs, f);
  } DFSCH_PROTECT {
    fclose(f);
  } DFSCH_PROTECT_END;
}
