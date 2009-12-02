#include <dfsch/dfsch.h>
#include <dfsch/lib/json.h>

void dfsch_module_json_register(dfsch_object_t* env){
  dfsch_package_t* json_pkg = dfsch_make_package("json");
  dfsch_provide(env, "json");
  dfsch_define_pkgcstr(env, json_pkg, "<parser>", DFSCH_JSON_PARSER_TYPE);
}
