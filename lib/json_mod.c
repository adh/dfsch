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
  char* file_name;
  dfsch_object_t* multiple_objects;

  DFSCH_STRING_ARG(args, file_name);
  DFSCH_OBJECT_ARG_OPT(args, multiple_objects, NULL);
  DFSCH_ARG_END(args);

  return dfsch_json_parse_file(file_name, multiple_objects);
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

DFSCH_DEFINE_PRIMITIVE(emit_string, 
                       "Serialize object into JSON string"){
  dfsch_object_t* object;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_json_emit_cstr(object));
}
DFSCH_DEFINE_PRIMITIVE(emit_file, 
                       "Serialize object into new file"){
  dfsch_object_t* object;
  char* filename;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);

  dfsch_json_emit_file(object, filename);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(emit_port, 
                       "Serialize object into port"){
  dfsch_object_t* object;
  dfsch_object_t* port;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);

  dfsch_json_emit_port(object, port);
  return NULL;
}


void dfsch_module_json_register(dfsch_object_t* env){
  dfsch_package_t* json_pkg = dfsch_make_package("json",
                                                 "JSON input and output");
  dfsch_provide(env, "json");
  dfsch_defcanon_pkgcstr(env, json_pkg, "<parser>", DFSCH_JSON_PARSER_TYPE);

  dfsch_defcanon_pkgcstr(env, json_pkg, "parse-string", 
                       DFSCH_PRIMITIVE_REF(parse_string));
  dfsch_defcanon_pkgcstr(env, json_pkg, "parse-file", 
                       DFSCH_PRIMITIVE_REF(parse_file));
  dfsch_defcanon_pkgcstr(env, json_pkg, "parse-port", 
                       DFSCH_PRIMITIVE_REF(parse_port));

  dfsch_defcanon_pkgcstr(env, json_pkg, "emit-string", 
                       DFSCH_PRIMITIVE_REF(emit_string));

}
