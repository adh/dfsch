#include <dfsch/lib/shtml.h>
#include <dfsch/util.h>
#include <dfsch/ports.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <dfsch/magic.h>

DFSCH_LOCAL_SYMBOL_CACHE(":attributes", at_symbol);
DFSCH_LOCAL_SYMBOL_CACHE(":literal-output", literal_symbol);

static char* void_elem[] = {
  "area", "base", "br", "col", "command", "embed", "hr", "img", "input", 
  "keygen", "link", "meta", "param", "source", "track", "wbr"
};
static char* cdata_elem[] = {
  "script", "style"
};
static char* rcdata_elem[] = {
  "textarea", "title"
};

dfsch_shtml_element_type_t dfsch_shtml_get_element_type(char* name){
  int i;
  for (i = 0; i < sizeof(void_elem)/sizeof(char*); i++){
    if (dfsch_ascii_strcasecmp(name, void_elem[i]) == 0){
      return ELEM_VOID;
    }
  }
  for (i = 0; i < sizeof(cdata_elem)/sizeof(char*); i++){
    if (dfsch_ascii_strcasecmp(name, cdata_elem[i]) == 0){
      return ELEM_CDATA;
    }
  }
  for (i = 0; i < sizeof(rcdata_elem)/sizeof(char*); i++){
    if (dfsch_ascii_strcasecmp(name, rcdata_elem[i]) == 0){
      return ELEM_RCDATA;
    }
  }
  return ELEM_PCDATA;
}


typedef void (*write_cb_t)(void* target, char* buf);
typedef struct emitter_t {
  dfsch_shtml_emitter_params_t* params;
  void* target;
  write_cb_t write;

  int start_col;
} emitter_t;

static emitter_t* emit_init(dfsch_shtml_emitter_params_t* params,
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
      if (dfsch_string_p(DFSCH_FAST_CAR(a))){
        value = dfsch_string_to_cstr(DFSCH_FAST_CAR(a));
      } else {
        value = dfsch_object_2_string(DFSCH_FAST_CAR(a), 1, DFSCH_PRINT);
      }
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
  e->write(e->target, ">");    

  if (dfsch_ascii_strcasecmp(name, "textarea") == 0 ||
      dfsch_ascii_strcasecmp(name, "pre") == 0){
    e->write(e->target, "\n");    
  }
  
  switch (dfsch_shtml_get_element_type(name)){
  case ELEM_VOID:
    if (children){
      dfsch_error("Child elements supplied for void element", children);
    }
    return; /* No end tag for void elements */
  case ELEM_CDATA:
    while (DFSCH_PAIR_P(children)){
      e->write(e->target, 
               dfsch_string_to_cstr(DFSCH_FAST_CAR(children)));
      children = DFSCH_FAST_CDR(children);
    }
    break;
  case ELEM_RCDATA:
    while (DFSCH_PAIR_P(children)){
      e->write(e->target, 
               dfsch_inet_xml_escape(dfsch_string_to_cstr(DFSCH_FAST_CAR(children))));
      children = DFSCH_FAST_CDR(children);
    }
    break;
  case ELEM_PCDATA:
    while (DFSCH_PAIR_P(children)){
      emit_object(e, DFSCH_FAST_CAR(children));
      children = DFSCH_FAST_CDR(children);
    }
    break;
  }
  e->write(e->target, dfsch_saprintf("</%s>", name));
}

static void emit_object(emitter_t* e, dfsch_object_t* o){
  if (dfsch_string_p(o)){
    e->write(e->target, dfsch_inet_xml_escape(dfsch_string_to_cstr(o)));
  } else if (DFSCH_PAIR_P(o)){
    if (DFSCH_FAST_CAR(o) == literal_symbol()){
      e->write(e->target, dfsch_string_to_cstr(dfsch_car(DFSCH_FAST_CDR(o))));
    } else {
      emit_element(e, 
                   dfsch_string_or_symbol_to_cstr(DFSCH_FAST_CAR(o)), 
                   DFSCH_FAST_CDR(o));
    }
  } else {
    dfsch_error("Invalid object in SHTML infoset", o);
  }
}

static void emit_infoset(emitter_t* e, dfsch_object_t* infoset){
  e->write(e->target, "<!DOCTYPE html>");    
  emit_object(e, infoset);
}

char* dfsch_shtml_emit_cstr(dfsch_object_t* infoset,
                               dfsch_shtml_emitter_params_t* params){
  emitter_t* e = emit_init(params, dfsch_sl_create(), dfsch_sl_append);
  emit_infoset(e, infoset);
  return dfsch_sl_value(e->target);
}
void dfsch_shtml_emit_port(dfsch_object_t* infoset, dfsch_object_t* port,
                              dfsch_shtml_emitter_params_t* params){
  emitter_t* e = emit_init(params, port, dfsch_port_write_cstr);
  emit_infoset(e, infoset);
}

static void rfputs(FILE* f, char*s){
  if (fputs(s, f) == EOF){
    dfsch_error("fputs failed", NULL);
  }
}

void dfsch_shtml_emit_file(dfsch_object_t* infoset, char* filename,
                              dfsch_shtml_emitter_params_t* params){
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

dfsch_shtml_emitter_params_t* dfsch_shtml_emitter_params(dfsch_object_t* args){
  dfsch_shtml_emitter_params_t* p = GC_NEW(dfsch_shtml_emitter_params_t);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("pretty-print", p->pretty_print, DFSCH_TRUE_P);
  DFSCH_KEYWORD_GENERIC("prepend-string", p->prepend_string, 
                        dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);
  return p;
}
