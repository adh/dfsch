#include "dfsch/lib/json.h"

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


struct dfsch_json_parser_t {
  dfsch_type_t* type;
  
  string_queue_t* queue;
  
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
				    void *baton);

void dfsch_json_parser_feed(dfsch_json_parser_t* jp,
                            char* buf){

}

dfsch_object_t* dfsch_json_parse_file(char* filename, int list);
dfsch_object_t* dfsch_json_parse_port(dfsch_object_t* port, int list);
dfsch_object_t* dfsch_json_parse_buf(char* buf, size_t len, int list);
dfsch_object_t* dfsch_json_parse_strbuf(dfsch_strbuf_t* b, int list);
dfsch_object_t* dfsch_json_parse_cstr(char* s, int list);

char* dfsch_json_emit_cstr(dfsch_object_t* infoset);
void dfsch_json_emit_port(dfsch_object_t* infoset, dfsch_object_t* port);
void dfsch_json_emit_file(dfsch_object_t* infoset, char* filename);
