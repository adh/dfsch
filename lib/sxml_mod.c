#include <dfsch/lib/sxml.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(parse_string, 0){
  dfsch_strbuf_t* string;
  dfsch_object_t* params;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_REST(args, params);

  return dfsch_sxml_parse_strbuf(string, 
                                 dfsch_sxml_parser_params(params));
}
DFSCH_DEFINE_PRIMITIVE(parse_file, 0){
  char* filename;
  dfsch_object_t* params;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_REST(args, params);

  return dfsch_sxml_parse_file(filename, 
                               dfsch_sxml_parser_params(params));
}
DFSCH_DEFINE_PRIMITIVE(parse_port, 0){
  dfsch_object_t* port;
  dfsch_object_t* params;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_REST(args, params);

  return dfsch_sxml_parse_port(port, 
                               dfsch_sxml_parser_params(params));
}


DFSCH_DEFINE_PRIMITIVE(emit_string, 0){
  dfsch_object_t* params;
  dfsch_object_t* infoset;
  dfsch_sxml_emitter_params_t* p;
  DFSCH_OBJECT_ARG(args, infoset);
  DFSCH_ARG_REST(args, params);
  p = dfsch_sxml_emitter_params(params);

  return dfsch_make_string_cstr(dfsch_sxml_emit_cstr(infoset, p));
}
DFSCH_DEFINE_PRIMITIVE(emit_file, 0){
  dfsch_object_t* params;
  dfsch_object_t* infoset;
  char* filename;
  dfsch_sxml_emitter_params_t* p;
  DFSCH_OBJECT_ARG(args, infoset);
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_REST(args, params);
  p = dfsch_sxml_emitter_params(params);

  dfsch_sxml_emit_file(infoset, filename, p);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(emit_port, 0){
  dfsch_object_t* params;
  dfsch_object_t* infoset;
  dfsch_object_t* port;
  dfsch_sxml_emitter_params_t* p;
  DFSCH_OBJECT_ARG(args, infoset);
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_REST(args, params);
  p = dfsch_sxml_emitter_params(params);

  dfsch_sxml_emit_port(infoset, port, p);
  return NULL;
}

void dfsch_module_sxml_register(dfsch_object_t* env){
  dfsch_package_t* xml_pkg = dfsch_make_package("xml",
                                                "XML support");
  dfsch_provide(env, "sxml");
  dfsch_defcanon_pkgcstr(env, xml_pkg, "sxml-parse-string", 
                       DFSCH_PRIMITIVE_REF(parse_string));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "sxml-parse-file", 
                       DFSCH_PRIMITIVE_REF(parse_file));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "sxml-parse-port", 
                       DFSCH_PRIMITIVE_REF(parse_port));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "sxml-emit-string", 
                       DFSCH_PRIMITIVE_REF(emit_string));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "sxml-emit-file", 
                       DFSCH_PRIMITIVE_REF(emit_file));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "sxml-emit-port", 
                       DFSCH_PRIMITIVE_REF(emit_port));
}
