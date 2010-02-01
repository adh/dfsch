#include <dfsch/lib/sxml.h>
#include <dfsch/lib/xml.h>
#include <dfsch/util.h>
#include <dfsch/ports.h>
#include <expat.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <dfsch/magic.h>

typedef struct sxml_stack_t sxml_stack_t;

typedef struct parser_ctx_t {
  sxml_stack_t* stack;
  dfsch_sxml_parser_params_t* params;
  dfsch_object_t* res;
} parser_ctx_t;

struct sxml_stack_t {
  sxml_stack_t* next;
  dfsch_object_t* head;
  dfsch_object_t* tail;
};

static XML_Memory_Handling_Suite gc_suite = {
  GC_malloc,
  GC_realloc,
  GC_free
};

DFSCH_LOCAL_SYMBOL_CACHE(":attributes", at_symbol);

static void sxml_push(parser_ctx_t* c, dfsch_object_t* tag){
  sxml_stack_t* s = GC_NEW(sxml_stack_t);
  s->next = c->stack;
  s->head = s->tail = dfsch_cons(tag, NULL);
  c->stack = s;
}
static void sxml_append(parser_ctx_t* c, dfsch_object_t* o){
  if (!c->stack){
    c->res = o;    
  } else { 
    dfsch_object_t* p = dfsch_cons(o, NULL);
    DFSCH_FAST_CDR_MUT(c->stack->tail) = p;
    c->stack->tail = p;
  }
}
static void sxml_pop(parser_ctx_t* c){
  dfsch_object_t* l = c->stack->head;
  c->stack = c->stack->next;
  sxml_append(c, l);
}

static XMLCALL void start_element_handler(parser_ctx_t* c, 
                                          char *name,
                                          char **attrs){
  sxml_push(c, dfsch_make_string_cstr(name));
  if (*attrs){
    dfsch_object_t* a_list = NULL;
    while (*attrs){
      a_list = dfsch_cons(dfsch_list(2,
                                     dfsch_make_string_cstr(*attrs),
                                     dfsch_make_string_cstr(*(attrs+1))),
                          a_list);
      attrs+=2;
    }
    sxml_append(c, dfsch_cons(at_symbol(), a_list));
  }
}
static XMLCALL void end_element_handler(parser_ctx_t* c, 
                                        char *name){
  sxml_pop(c);
}

static XMLCALL void character_data_handler(parser_ctx_t* c, 
                                           char *data,
                                           int len){
  if (dfsch_string_p(DFSCH_FAST_CAR(c->stack->tail))){
    dfsch_strbuf_t* o = dfsch_string_to_buf(DFSCH_FAST_CAR(c->stack->tail));
    char* buf = GC_MALLOC_ATOMIC(len+o->len);
    memcpy(buf, o->ptr, o->len);
    memcpy(buf+o->len, data, len);

    DFSCH_FAST_CAR(c->stack->tail) = dfsch_make_string_buf(buf, len+o->len);
  } else {
    sxml_append(c, dfsch_make_string_buf(data, len));
  }
}

static dfsch_sxml_parser_params_t default_parser_params = {
  0,
  NULL,
  0
};

static XML_Parser sxml_init(dfsch_sxml_parser_params_t* params){
  XML_Parser p;
  parser_ctx_t* c;
  static char sep = ':';

  if (!params){
    params = &default_parser_params;
  }

  p = XML_ParserCreate_MM(params->encoding, &gc_suite, 
                          params->namespaces ? &sep : NULL);

  c = GC_NEW(parser_ctx_t);
  c->params = params;
  c->stack = NULL;
  XML_SetUserData(p, c);
  XML_SetElementHandler(p, 
                        (XML_StartElementHandler)start_element_handler, 
                        (XML_EndElementHandler)end_element_handler);
  XML_SetCharacterDataHandler(p,
                              (XML_CharacterDataHandler)character_data_handler);

  

  return p;
}

static dfsch_object_t* sxml_done(XML_Parser p){
  parser_ctx_t* c = XML_GetUserData(p);
  assert(!c->stack);
  return c->res;
}

#define BUFFER_LENGTH 4096

dfsch_object_t* dfsch_sxml_parse_file(char* filename,
                                          dfsch_sxml_parser_params_t* params){
  XML_Parser p = sxml_init(params);
  FILE* f;
  size_t r;
  char* buf;

  f = fopen(filename, "r");
  if (!f){
    dfsch_error("Cannot open file", 
                dfsch_list(2,
                           dfsch_make_string_cstr(filename),
                           dfsch_make_string_cstr(strerror(errno))));
    
  }

  while ((!feof(f)) && (!ferror(f))){
    buf = XML_GetBuffer(p, BUFFER_LENGTH);
    r = fread(buf, 1, BUFFER_LENGTH, f);
    
    if (XML_ParseBuffer(p, r, 0) != XML_STATUS_OK){
      fclose(f);
      dfsch_xml_signal_error(p);
    }
  }
  if (ferror(f)){
    dfsch_object_t* tmp = dfsch_list(2,
                                     dfsch_make_string_cstr(filename),
                                     dfsch_make_string_cstr(strerror(errno)));
    fclose(f);
    dfsch_error("Error reading file", tmp);
  }
  
  fclose(f);  
  if (XML_Parse(p, NULL, 0, 1) != XML_STATUS_OK){
    dfsch_xml_signal_error(p);
  }

  return sxml_done(p);
}
dfsch_object_t* dfsch_sxml_parse_port(dfsch_object_t* port,
                                          dfsch_sxml_parser_params_t* params){
  XML_Parser p = sxml_init(params);
  size_t r;
  char* buf;

  while (1){
    buf = XML_GetBuffer(p, BUFFER_LENGTH);

    r = dfsch_port_read_buf(port, buf, BUFFER_LENGTH);
    if (r==0){
      break;
    }

    if (XML_ParseBuffer(p, r, 0) != XML_STATUS_OK){
      dfsch_xml_signal_error(p);
    }
  } 

  if (XML_Parse(p, NULL, 0, 1) != XML_STATUS_OK){
    dfsch_xml_signal_error(p);
  }
 
  return sxml_done(p);
}
dfsch_object_t* dfsch_sxml_parse_buf(char* buf, size_t len,
                                         dfsch_sxml_parser_params_t* params){
  XML_Parser p = sxml_init(params);
  if (XML_Parse(p, buf, len, 1) != XML_STATUS_OK){
    dfsch_xml_signal_error(p);
  }
  return sxml_done(p);
}
dfsch_object_t* dfsch_sxml_parse_strbuf(dfsch_strbuf_t* b,
                                            dfsch_sxml_parser_params_t* params){
  return dfsch_sxml_parse_buf(b->ptr, b->len, params);
}
dfsch_object_t* dfsch_sxml_parse_cstr(char* s,
                                          dfsch_sxml_parser_params_t* params){
  return dfsch_sxml_parse_buf(s, strlen(s), params);
}

dfsch_sxml_parser_params_t* dfsch_sxml_parser_params(dfsch_object_t* args){
  dfsch_sxml_parser_params_t* p = GC_NEW(dfsch_sxml_parser_params_t);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("namespaces", p->namespaces, DFSCH_TRUE_P);
  DFSCH_KEYWORD_GENERIC("encoding", p->encoding, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("collapse-whitespace", 
                        p->collapse_whitespace, DFSCH_TRUE_P);
  DFSCH_KEYWORD_PARSER_END(args);
  return p;
}

typedef void (*write_cb_t)(void* target, char* buf);
typedef struct emitter_t {
  dfsch_sxml_emitter_params_t* params;
  void* target;
  write_cb_t write;

  int start_col;
} emitter_t;

static emitter_t* emit_init(dfsch_sxml_emitter_params_t* params,
                            void* target,
                            write_cb_t write){
  emitter_t* e = GC_NEW(emitter_t);
  e->params = params;
  e->target = target;
  e->write = write;
  e->start_col = 0;
  return e;
} 

static void emit_attrs(emitter_t* e, dfsch_object_t* attrs){
  while (DFSCH_PAIR_P(attrs)){
    dfsch_object_t* a = DFSCH_FAST_CAR(attrs);
    char *name;
    char *value;

    switch (dfsch_list_length(a, NULL)){
    case 1:
      name = dfsch_string_or_symbol_to_cstr(DFSCH_FAST_CAR(a));
      e->write(e->target, dfsch_saprintf(" %s=\"%s\"", name, name));
      break;
    case 2:
      name = dfsch_string_or_symbol_to_cstr(DFSCH_FAST_CAR(a));
      a = DFSCH_FAST_CDR(a);
      value = dfsch_string_to_cstr(DFSCH_FAST_CAR(a));
      e->write(e->target, dfsch_saprintf(" %s=\"%s\"", name, 
                                         dfsch_inet_xml_escape(value)));
      break;
    default:
      dfsch_error("Invalid format of attribute", a);
    }


    attrs = DFSCH_FAST_CDR(attrs);
  }
}

static void emit_object(emitter_t* e, dfsch_object_t* o);

static void emit_element(emitter_t* e, char* name, dfsch_object_t* children){
  dfsch_object_t* attrs = NULL;
  
  if (DFSCH_PAIR_P(children) && 
      DFSCH_PAIR_P(DFSCH_FAST_CAR(children)) &&
      DFSCH_FAST_CAR(DFSCH_FAST_CAR(children)) == at_symbol()){
    attrs = DFSCH_FAST_CDR(DFSCH_FAST_CAR(children));
    children = DFSCH_FAST_CDR(children);
  }

  while (DFSCH_PAIR_P(children) && DFSCH_SYMBOL_P(DFSCH_FAST_CAR(children))){
    dfsch_object_t* aname = DFSCH_FAST_CAR(children);
    dfsch_object_t* avalue;
    children = DFSCH_FAST_CDR(children);
    if (!DFSCH_PAIR_P(children)){
      dfsch_error("Keyword requires an argument", aname);
    }
    avalue = DFSCH_FAST_CAR(children);
    children = DFSCH_FAST_CDR(children);
    if (avalue == DFSCH_SYM_TRUE){
      attrs = dfsch_cons(dfsch_list(1,aname), attrs); 
    } else {
      attrs = dfsch_cons(dfsch_list(2, aname, avalue), attrs); 
    }
  }


  e->write(e->target, "<");
  e->write(e->target, name);
  emit_attrs(e, attrs);
  if (children){
    e->write(e->target, ">");    
    while (DFSCH_PAIR_P(children)){
      emit_object(e, DFSCH_FAST_CAR(children));
      children = DFSCH_FAST_CDR(children);
    }
    e->write(e->target, dfsch_saprintf("</%s>", name));
  } else {
    e->write(e->target, " />");    
  }
}

static void emit_object(emitter_t* e, dfsch_object_t* o){
  if (dfsch_string_p(o)){
    e->write(e->target, dfsch_inet_xml_escape(dfsch_string_to_cstr(o)));
  } else if (DFSCH_PAIR_P(o)){
    emit_element(e, 
                 dfsch_string_or_symbol_to_cstr(DFSCH_FAST_CAR(o)), 
                 DFSCH_FAST_CDR(o));
  } else {
    dfsch_error("Invalid object in SXML infoset", o);
  }
}

static void emit_infoset(emitter_t* e, dfsch_object_t* infoset){
  if (e->params->xml_decl){
    e->write(e->target, "<?xml version=\"1.0\" ?>\n");
  }
  if (e->params->dtd_public && e->params->dtd_system){ 
    /* XML does not allow PUBLIC-only DTD*/
    e->write(e->target, 
             dfsch_saprintf("<!DOCTYPE PUBLIC \"%s\" \"%s\">", 
                            e->params->dtd_public,
                            e->params->dtd_system));    
  }
  emit_object(e, infoset);
}

char* dfsch_sxml_emit_cstr(dfsch_object_t* infoset,
                               dfsch_sxml_emitter_params_t* params){
  emitter_t* e = emit_init(params, dfsch_sl_create(), dfsch_sl_append);
  emit_infoset(e, infoset);
  return dfsch_sl_value(e->target);
}
void dfsch_sxml_emit_port(dfsch_object_t* infoset, dfsch_object_t* port,
                              dfsch_sxml_emitter_params_t* params){
  emitter_t* e = emit_init(params, port, dfsch_port_write_cstr);
  emit_infoset(e, infoset);
}

static void rfputs(FILE* f, char*s){
  if (fputs(s, f) == EOF){
    dfsch_error("fputs failed", NULL);
  }
}

void dfsch_sxml_emit_file(dfsch_object_t* infoset, char* filename,
                              dfsch_sxml_emitter_params_t* params){
  emitter_t* e;
  FILE* f = fopen(filename, "w");
  if (!f){
    dfsch_error("Cannot open file", 
                dfsch_list(2,
                           dfsch_make_string_cstr(filename),
                           dfsch_make_string_cstr(strerror(errno))));
    
  }
  
  DFSCH_UNWIND {
    e = emit_init(params, f, rfputs);
    emit_infoset(e, infoset);
  } DFSCH_PROTECT {
    fclose(f);
  } DFSCH_PROTECT_END;
}

dfsch_sxml_emitter_params_t* dfsch_sxml_emitter_params(dfsch_object_t* args){
  dfsch_sxml_emitter_params_t* p = GC_NEW(dfsch_sxml_emitter_params_t);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("pretty-print", p->pretty_print, DFSCH_TRUE_P);
  DFSCH_KEYWORD_GENERIC("xml-decl", p->xml_decl, DFSCH_TRUE_P);
  DFSCH_KEYWORD_GENERIC("dtd-public", p->dtd_public, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("dtd-system", p->dtd_system, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("prepend-string", p->prepend_string, 
                        dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);
  return p;
}
