#include "dfsch/lib/tokyo-tyrant.h"
#include <dfsch/dfsch.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(open, "Open Tokyo Tyrant database"){
  char* name;
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_tyrant_db_open(name);
}

DFSCH_DEFINE_PRIMITIVE(close, "Close Tokyo Tyrant database"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_tyrant_db_close(db);
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

  return dfsch_tokyo_tyrant_prefix_search(db, prefix->ptr, prefix->len, limit);
}

DFSCH_DEFINE_PRIMITIVE(open_table, "Open Tokyo Tyrant table database"){
  char* name;
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_tyrant_table_open(name);
}

DFSCH_DEFINE_PRIMITIVE(close_table, "Close Tokyo Tyrant table database"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_tyrant_table_close(db);
  return NULL;
}


void dfsch_module_tokyo_tyrant_register(dfsch_object_t* env){
  dfsch_package_t* tc_pkg = dfsch_make_package("tokyo-cabinet", NULL);
  dfsch_package_t* tt_pkg = dfsch_make_package("tokyo-tyrant",
                                               "Tokyo Tyrant remote database "
                                               "support");
  dfsch_provide(env, "tokyo-tyrant");
  dfsch_require(env, "tokyo-cabinet", NULL);

  dfsch_defcanon_pkgcstr(env, tt_pkg, "open", DFSCH_PRIMITIVE_REF(open));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "close!", 
                                DFSCH_TOKYO_TYRANT_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(close));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "prefix-search", 
                                DFSCH_TOKYO_TYRANT_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(prefix_search));
  dfsch_defcanon_pkgcstr(env, tc_pkg, "<db>", DFSCH_TOKYO_TYRANT_DB_TYPE);

  dfsch_defcanon_pkgcstr(env, tt_pkg, "open-table", 
                         DFSCH_PRIMITIVE_REF(open_table));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "close!", 
                                DFSCH_TOKYO_TYRANT_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(close_table));


}
