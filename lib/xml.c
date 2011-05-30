#include <dfsch/lib/xml.h>
#include <dfsch/magic.h>
#include <dfsch/number.h>
#include <dfsch/ports.h>
#include <dfsch/types.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

typedef struct parser_t {
  dfsch_type_t* type;
  XML_Parser parser;
  
  dfsch_object_t* start_element_proc;
  dfsch_object_t* end_element_proc;
  dfsch_object_t* character_data_proc;
  dfsch_object_t* processing_instruction_proc;
  dfsch_object_t* comment_proc;

  dfsch_object_t* catch_tag;
  dfsch_object_t* catch_value;
} parser_t;

static dfsch_slot_t parser_slots[] = {
  DFSCH_OBJECT_SLOT(parser_t, start_element_proc, DFSCH_SLOT_ACCESS_RW,
                    "Procedure called for start-tags"),
  DFSCH_OBJECT_SLOT(parser_t, end_element_proc, DFSCH_SLOT_ACCESS_RW,
                    "Handler for end-tags"),
  DFSCH_OBJECT_SLOT(parser_t, character_data_proc, DFSCH_SLOT_ACCESS_RW,
                    "Handler of character data"),
  DFSCH_OBJECT_SLOT(parser_t, processing_instruction_proc, 
                    DFSCH_SLOT_ACCESS_RW,
                    "Handler for processing instructions (unimplemented)"),
  DFSCH_OBJECT_SLOT(parser_t, comment_proc, DFSCH_SLOT_ACCESS_RW,
                    "Handler for comments (unimplemented)"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_xml_parser_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(parser_t),
  "xml:parser",
  NULL,
  NULL,
  NULL,
  NULL,
  &parser_slots,
};

static XML_Memory_Handling_Suite gc_suite = {
  GC_malloc,
  GC_realloc,
  GC_free
};

static void parser_apply(parser_t* parser, 
                         dfsch_object_t* proc, 
                         dfsch_object_t* args){
  if (proc){
    DFSCH_SCATCH_BEGIN {
      dfsch_apply(proc, args);
    } DFSCH_SCATCH {
      parser->catch_tag = DFSCH_CATCH_TAG;
      parser->catch_value = DFSCH_CATCH_VALUE;
    } DFSCH_SCATCH_END;
  }
  
}

static XMLCALL void start_element_handler(parser_t* parser, 
                                          char *name,
                                          char **attrs){
  dfsch_object_t* a_list = NULL;

  while (*attrs){
    a_list = dfsch_cons(dfsch_list(2,
                                   dfsch_make_string_cstr(*attrs),
                                   dfsch_make_string_cstr(*(attrs+1))),
                        a_list);
    attrs+=2;
  }

  parser_apply(parser, 
               parser->start_element_proc,
               dfsch_list(2,
                          dfsch_make_string_cstr(name),
                          a_list));
}
static XMLCALL void end_element_handler(parser_t* parser, 
                                        char *name){
  parser_apply(parser, 
               parser->end_element_proc,
               dfsch_list(1,
                          dfsch_make_string_cstr(name)));
}
static XMLCALL void character_data_handler(parser_t* parser, 
                                           char *data,
                                           int len){
  parser_apply(parser, 
               parser->character_data_proc,
               dfsch_list(1,
                          dfsch_make_string_buf(data, len)));
}

dfsch_object_t* dfsch_xml_make_parser(char* encoding, int ns){
  static char sep = ':';
  parser_t* parser = (parser_t*)dfsch_make_object(DFSCH_XML_PARSER_TYPE);

  if (ns){
    parser->parser = XML_ParserCreate_MM(encoding, &gc_suite, &sep);
    /* Syntax for XML with namespace requires, that LocalPart of QName does not
     * contain colon, so last colon of combined string always separates 
     * Namespace URI and LocalPart.
     */
  } else {
    parser->parser = XML_ParserCreate_MM(encoding, &gc_suite, NULL);
  }

  XML_SetUserData(parser->parser, parser);
  XML_SetElementHandler(parser->parser, 
                        (XML_StartElementHandler)start_element_handler, 
                        (XML_EndElementHandler)end_element_handler);
  XML_SetCharacterDataHandler(parser->parser,
                              (XML_CharacterDataHandler)character_data_handler);
  

  return (dfsch_object_t*) parser;
}

void dfsch_xml_destroy_parser(dfsch_object_t* parser){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  if (p->parser){
    XML_ParserFree(p->parser);
    p->parser = NULL;
  }
}

XML_Parser dfsch_xml_parser_ref(dfsch_object_t* parser){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  if (!p->parser){
    dfsch_error("xml:parser-already-destroyed", parser);
  }

  return p->parser;
}

void dfsch_xml_set_start_element_proc(dfsch_object_t* parser,
                                          dfsch_object_t* proc){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  p->start_element_proc = proc;
}
void dfsch_xml_set_end_element_proc(dfsch_object_t* parser,
                                    dfsch_object_t* proc){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  p->end_element_proc = proc;
}
void dfsch_xml_ext_set_character_data_proc(dfsch_object_t* parser,
                                           dfsch_object_t* proc){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  p->character_data_proc = proc;

}
void dfsch_xml_set_processing_instruction_proc(dfsch_object_t* parser,
                                               dfsch_object_t* proc){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  p->processing_instruction_proc = proc;
}
void dfsch_xml_set_comment_proc(dfsch_object_t* parser,
                                    dfsch_object_t* proc){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  p->comment_proc = proc;
}

void dfsch_xml_parse_buf(dfsch_object_t* parser,
                             char* buf,
                             size_t len,
                             int eof){
  parser_t* p;
  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  if (!p->parser){
    dfsch_error("xml:parser-already-destroyed", parser);
  }

  p->catch_tag = NULL;
  p->catch_value = NULL;

  if (XML_Parse(p->parser, buf, len, eof) != XML_STATUS_OK){
    dfsch_xml_signal_error(p->parser);
  }
  
  if (p->catch_tag){
    dfsch_object_t* tag = p->catch_tag;
    dfsch_object_t* value = p->catch_value;
    p->catch_tag = NULL;
    p->catch_value = NULL;
    dfsch_throw(tag, value);
  }
}
void dfsch_xml_parse_strbuf(dfsch_object_t* parser,
                                dfsch_strbuf_t* buf,
                                int eof){
  dfsch_xml_parse_buf(parser, buf->ptr, buf->len, eof);
}
void dfsch_xml_parse_cstr(dfsch_object_t* parser,
                              char* str,
                              int eof){
  dfsch_xml_parse_buf(parser, str, strlen(str), eof);
}
void dfsch_xml_parse_eof(dfsch_object_t* parser){
  dfsch_xml_parse_buf(parser, NULL, 0, 1);
}

#define BUFFER_LENGTH 4096

void dfsch_xml_parse_file(dfsch_object_t* parser,
                              char* filename,
                              int eof){
  FILE* f;
  parser_t* p;
  size_t r;
  char* buf;

  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  if (!p->parser){
    dfsch_error("xml:parser-already-destroyed", parser);
  }

  p->catch_tag = NULL;
  p->catch_value = NULL;

  f = fopen(filename, "r");
  if (!f){
    dfsch_error("xml:cannot-open-file", 
                dfsch_list(2,
                           dfsch_make_string_cstr(filename),
                           dfsch_make_string_cstr(strerror(errno))));
  }
  
  while ((!feof(f)) && (!ferror(f))){
    buf = XML_GetBuffer(p->parser, BUFFER_LENGTH);
    r = fread(buf, 1, BUFFER_LENGTH, f);
    
    if (XML_ParseBuffer(p->parser, r, 0) != XML_STATUS_OK){
      fclose(f);
      dfsch_xml_signal_error(p->parser);
    }
    
    if (p->catch_tag){
      dfsch_object_t* tag = p->catch_tag;
      dfsch_object_t* value = p->catch_value;
      fclose(f);
      p->catch_tag = NULL;
      p->catch_value = NULL;
      dfsch_throw(tag, value);
    }
  }
  
  if (ferror(f)){
    dfsch_object_t* tmp = dfsch_list(2,
                                     dfsch_make_string_cstr(filename),
                                     dfsch_make_string_cstr(strerror(errno)));
    fclose(f);
    dfsch_error("xml:cannot-read-file", tmp);
  }
  
  fclose(f);

  if (eof){
    p->catch_tag = NULL;
    p->catch_value = NULL;

    if (XML_Parse(p->parser, NULL, 0, 1) != XML_STATUS_OK){
      dfsch_xml_signal_error(p->parser);
    }

    if (p->catch_tag){
      dfsch_object_t* tag = p->catch_tag;
      dfsch_object_t* value = p->catch_value;
      fclose(f);
      p->catch_tag = NULL;
      p->catch_value = NULL;
      dfsch_throw(tag, value);
    }    
  }
  
}
void dfsch_xml_parse_port(dfsch_object_t* parser,
                              dfsch_object_t* port,
                              int eof){
  parser_t* p;
  size_t r;
  char* buf;

  if (DFSCH_TYPE_OF(parser) != DFSCH_XML_PARSER_TYPE){
    dfsch_error("xml:not-a-parser", parser);
  }

  p = (parser_t*)parser;

  if (!p->parser){
    dfsch_error("xml:parser-already-destroyed", parser);
  }

  p->catch_tag = NULL;
  p->catch_value = NULL;


  while (1){
    buf = XML_GetBuffer(p->parser, BUFFER_LENGTH);

    r = dfsch_port_read_buf(port, buf, BUFFER_LENGTH);
    if (r==0){
      break;
    }

    if (XML_ParseBuffer(p->parser, r, 0) != XML_STATUS_OK){
      dfsch_xml_signal_error(p->parser);
    }
    
    if (p->catch_tag){
      dfsch_object_t* tag = p->catch_tag;
      dfsch_object_t* value = p->catch_value;
      p->catch_tag = NULL;
      p->catch_value = NULL;
      dfsch_throw(tag, value);
    }
  }
  
  if (eof){
    if (XML_Parse(p->parser, NULL, 0, 1) != XML_STATUS_OK){
      dfsch_xml_signal_error(p->parser);
    }
    
    if (p->catch_tag){
      dfsch_object_t* tag = p->catch_tag;
      dfsch_object_t* value = p->catch_value;
      p->catch_tag = NULL;
      p->catch_value = NULL;
      dfsch_throw(tag, value);
    }
  }
  
}

void dfsch_xml_signal_error(XML_Parser parser){
  enum XML_Error ec = XML_GetErrorCode(parser);
  int row = XML_GetCurrentLineNumber(parser);
  int col = XML_GetCurrentColumnNumber(parser);

  dfsch_error("Error parsing XML",
              dfsch_list(2,
                         dfsch_make_string_cstr(XML_ErrorString(ec)),
                         dfsch_cons(dfsch_make_number_from_long(row),
                                    dfsch_make_number_from_long(col))));
}
