#ifndef H__dfsch__sxml__
#define H__dfsch__sxml__

#include <dfsch/dfsch.h>

typedef struct dfsch_sxml_parser_params_t {
  int namespaces;
  char* encoding;
  int collapse_whitespace;
} dfsch_sxml_parser_params_t;

dfsch_sxml_parser_params_t* dfsch_sxml_parser_params(dfsch_object_t* args);

dfsch_object_t* dfsch_sxml_parse_file(char* filename,
                                      dfsch_sxml_parser_params_t* params);
dfsch_object_t* dfsch_sxml_parse_port(dfsch_object_t* port,
                                      dfsch_sxml_parser_params_t* params);
dfsch_object_t* dfsch_sxml_parse_buf(char* buf, size_t len,
                                     dfsch_sxml_parser_params_t* params);
dfsch_object_t* dfsch_sxml_parse_strbuf(dfsch_strbuf_t* b,
                                        dfsch_sxml_parser_params_t* params);
dfsch_object_t* dfsch_sxml_parse_cstr(char* s,
                                      dfsch_sxml_parser_params_t* params);

typedef struct dfsch_sxml_emitter_params_t {
  int pretty_print;
  int xml_decl;
  char* dtd_public;
  char* dtd_system;
  char* prepend_string;
} dfsch_sxml_emitter_params_t;

dfsch_sxml_emitter_params_t* dfsch_sxml_emitter_params(dfsch_object_t* args);


char* dfsch_sxml_emit_cstr(dfsch_object_t* infoset,
                           dfsch_sxml_emitter_params_t* params);
void dfsch_sxml_emit_port(dfsch_object_t* infoset, dfsch_object_t* port,
                          dfsch_sxml_emitter_params_t* params);
void dfsch_sxml_emit_file(dfsch_object_t* infoset, char* filename,
                          dfsch_sxml_emitter_params_t* params);

#endif
