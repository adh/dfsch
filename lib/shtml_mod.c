#include <dfsch/lib/shtml.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(emit_string, 0){
  dfsch_object_t* params;
  dfsch_object_t* infoset;
  dfsch_shtml_emitter_params_t* p;
  DFSCH_OBJECT_ARG(args, infoset);
  DFSCH_ARG_REST(args, params);
  p = dfsch_shtml_emitter_params(params);

  return dfsch_make_string_cstr(dfsch_shtml_emit_cstr(infoset, p));
}
DFSCH_DEFINE_PRIMITIVE(emit_file, 0){
  dfsch_object_t* params;
  dfsch_object_t* infoset;
  char* filename;
  dfsch_shtml_emitter_params_t* p;
  DFSCH_OBJECT_ARG(args, infoset);
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_REST(args, params);
  p = dfsch_shtml_emitter_params(params);

  dfsch_shtml_emit_file(infoset, filename, p);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(emit_port, 0){
  dfsch_object_t* params;
  dfsch_object_t* infoset;
  dfsch_object_t* port;
  dfsch_shtml_emitter_params_t* p;
  DFSCH_OBJECT_ARG(args, infoset);
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_REST(args, params);
  p = dfsch_shtml_emitter_params(params);

  dfsch_shtml_emit_port(infoset, port, p);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(parse_string, "Parse HTML5 document from string"){
  dfsch_strbuf_t* document;
  char* encoding;

  DFSCH_BUFFER_ARG(args, document);
  DFSCH_STRING_ARG_OPT(args, encoding, NULL);
  DFSCH_ARG_END(args);

  return dfsch_shtml_parse_buf(document->ptr, document->len, encoding);
}

void dfsch_module_shtml_register(dfsch_object_t* env){
  dfsch_package_t* xml_pkg = dfsch_make_package("shtml",
                                                "HTML5 output support");
  dfsch_provide(env, "shtml");
  dfsch_defcanon_pkgcstr(env, xml_pkg, "emit-string", 
                       DFSCH_PRIMITIVE_REF(emit_string));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "emit-file", 
                       DFSCH_PRIMITIVE_REF(emit_file));
  dfsch_defcanon_pkgcstr(env, xml_pkg, "emit-port", 
                       DFSCH_PRIMITIVE_REF(emit_port));

  dfsch_defcanon_pkgcstr(env, xml_pkg, "parse-string", 
                       DFSCH_PRIMITIVE_REF(parse_string));
}
