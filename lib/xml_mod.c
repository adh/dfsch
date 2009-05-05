#include <dfsch/lib/xml.h>

#include <dfsch/dfsch.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(make_parser, 0){
  char* encoding = NULL;
  dfsch_object_t* ns;
  DFSCH_OBJECT_ARG_OPT(args, ns, NULL);
  DFSCH_STRING_ARG_OPT(args, encoding, NULL);
  DFSCH_ARG_END(args);

  return dfsch_xml_make_parser(encoding, ns != NULL);
}
DFSCH_DEFINE_PRIMITIVE(destroy_parser, 0){
  dfsch_object_t* parser;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_ARG_END(args);
  
  dfsch_xml_destroy_parser(parser);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_start_element_proc, 0){
  dfsch_object_t* parser;
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG_OPT(args, proc, NULL);
  DFSCH_ARG_END(args);

  dfsch_xml_set_start_element_proc(parser, proc);

  return proc;
}
DFSCH_DEFINE_PRIMITIVE(set_end_element_proc, 0){
  dfsch_object_t* parser;
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG_OPT(args, proc, NULL);
  DFSCH_ARG_END(args);

  dfsch_xml_set_end_element_proc(parser, proc);

  return proc;
}
DFSCH_DEFINE_PRIMITIVE(set_character_data_proc, 0){
  dfsch_object_t* parser;
  dfsch_object_t* proc;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG_OPT(args, proc, NULL);
  DFSCH_ARG_END(args);

  dfsch_xml_ext_set_character_data_proc(parser, proc);

  return proc;
}
DFSCH_DEFINE_PRIMITIVE(parse_eof, 0){
  dfsch_object_t* parser;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_ARG_END(args);
  
  dfsch_xml_parse_eof(parser);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(parse_buffer, 0){
  dfsch_object_t* parser;
  dfsch_strbuf_t* buf;
  dfsch_object_t* eof;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_BUFFER_ARG(args, buf);
  DFSCH_OBJECT_ARG_OPT(args, eof, NULL);
  DFSCH_ARG_END(args);
  
  dfsch_xml_parse_strbuf(parser, buf, eof != NULL);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(parse_file, 0){
  dfsch_object_t* parser;
  char* filename;
  dfsch_object_t* eof;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_STRING_ARG(args, filename);
  DFSCH_OBJECT_ARG_OPT(args, eof, DFSCH_SYM_TRUE);
  DFSCH_ARG_END(args);
  
  dfsch_xml_parse_file(parser, filename, eof != NULL);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(parse_port, 0){
  dfsch_object_t* parser;
  dfsch_object_t* port;
  dfsch_object_t* eof;
  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_OBJECT_ARG_OPT(args, eof, DFSCH_SYM_TRUE);
  DFSCH_ARG_END(args);
  
  dfsch_xml_parse_port(parser, port, eof != NULL);

  return NULL;
}



void dfsch_module_xml_register(dfsch_object_t* env){
  dfsch_provide(env, "xml");

  dfsch_define_cstr(env, "xml:<parser>", DFSCH_XML_PARSER_TYPE);

  dfsch_define_cstr(env, "xml:make-parser", 
                    DFSCH_PRIMITIVE_REF(make_parser));
  dfsch_define_cstr(env, "xml:destroy-parser!", 
                    DFSCH_PRIMITIVE_REF(destroy_parser));
  
  dfsch_define_cstr(env, "xml:set-start-element-proc!", 
                    DFSCH_PRIMITIVE_REF(set_start_element_proc));
  dfsch_define_cstr(env, "xml:set-end-element-proc!", 
                    DFSCH_PRIMITIVE_REF(set_end_element_proc));
  dfsch_define_cstr(env, "xml:set-character-data-proc!", 
                    DFSCH_PRIMITIVE_REF(set_character_data_proc));

  dfsch_define_cstr(env, "xml:parse-eof!", 
                    DFSCH_PRIMITIVE_REF(parse_eof));
  dfsch_define_cstr(env, "xml:parse-buffer!", 
                    DFSCH_PRIMITIVE_REF(parse_buffer));
  dfsch_define_cstr(env, "xml:parse-file!", 
                    DFSCH_PRIMITIVE_REF(parse_file));
  dfsch_define_cstr(env, "xml:parse-port!", 
                    DFSCH_PRIMITIVE_REF(parse_port));
  

}
