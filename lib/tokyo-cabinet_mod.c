#include "dfsch/lib/tokyo-cabinet.h"
#include <dfsch/dfsch.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(open, "Open abstract Tokyo Cabinet database"){
  char* name;
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_cabinet_db_open(name);
}

DFSCH_DEFINE_PRIMITIVE(close, "Close abstract Tokyo Cabinet database"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_db_close(db);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(prefix_search, 
                       "Find all keys starting with given prefix"){
  dfsch_object_t* db;
  dfsch_strbuf_t* prefix;
  long limit;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_BUFFER_ARG(args, prefix);
  DFSCH_LONG_ARG_OPT(args, limit, -1);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_cabinet_prefix_search(db, prefix->ptr, prefix->len, limit);
}

void dfsch_module_tokyo_cabinet_register(dfsch_object_t* env){
  dfsch_package_t* tc_pkg = dfsch_make_package("tokyo-cabinet");
  dfsch_provide(env, "tokyo-cabinet");

  dfsch_define_pkgcstr(env, tc_pkg, "open", DFSCH_PRIMITIVE_REF(open));
  dfsch_define_pkgcstr(env, tc_pkg, "close!", 
                       DFSCH_PRIMITIVE_REF(close));
  dfsch_define_pkgcstr(env, tc_pkg, "prefix-search", 
                       DFSCH_PRIMITIVE_REF(prefix_search));
  dfsch_define_pkgcstr(env, tc_pkg, "<db>", DFSCH_TOKYO_CABINET_DB_TYPE);
}
