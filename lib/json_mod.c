#include <dfsch/dfsch.h>
#include <dfsch/lib/json.h>

DFSCH_DEFINE_PRIMITIVE(parse_string, 
                       "Parse JSON object from string object"){
  char* string;
  dfsch_object_t* multiple_objects;

  DFSCH_STRING_ARG(args, string);
  DFSCH_OBJECT_ARG_OPT(args, multiple_objects, NULL);
  DFSCH_ARG_END(args);

  return dfsch_json_parse_cstr(string, multiple_objects);
}

DFSCH_DEFINE_PRIMITIVE(parse_file, 
                       "Parse JSON object from file"){
  char* string;
  dfsch_object_t* multiple_objects;

  DFSCH_STRING_ARG(args, string);
  DFSCH_OBJECT_ARG_OPT(args, multiple_objects, NULL);
  DFSCH_ARG_END(args);

  return dfsch_json_parse_file(string, multiple_objects);
}
DFSCH_DEFINE_PRIMITIVE(parse_port, 
                       "Parse JSON object from port"){
  dfsch_object_t* port;
  dfsch_object_t* multiple_objects;

  DFSCH_OBJECT_ARG(args, port);
  DFSCH_OBJECT_ARG_OPT(args, multiple_objects, NULL);
  DFSCH_ARG_END(args);

  return dfsch_json_parse_port(port, multiple_objects);
}


void dfsch_module_json_register(dfsch_object_t* env){
  dfsch_package_t* json_pkg = dfsch_make_package("json");
  dfsch_provide(env, "json");
  dfsch_define_pkgcstr(env, json_pkg, "<parser>", DFSCH_JSON_PARSER_TYPE);

  dfsch_define_pkgcstr(env, json_pkg, "parse-string", 
                       DFSCH_PRIMITIVE_REF(parse_string));
  dfsch_define_pkgcstr(env, json_pkg, "parse-file", 
                       DFSCH_PRIMITIVE_REF(parse_file));
  dfsch_define_pkgcstr(env, json_pkg, "parse-port", 
                       DFSCH_PRIMITIVE_REF(parse_port));
}
