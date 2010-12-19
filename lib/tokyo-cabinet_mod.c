#include "dfsch/lib/tokyo-cabinet.h"
#include <dfsch/dfsch.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(open, "Open Tokyo Cabinet database"){
  char* name;
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_cabinet_db_open(name);
}

DFSCH_DEFINE_PRIMITIVE(close, "Close Tokyo Cabinet database"){
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

DFSCH_DEFINE_PRIMITIVE(begin_transaction, "Start atomic transaction"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_begin_transaction(db);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(commit_transaction, "Commit atomic transaction"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_commit_transaction(db);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(abort_transaction, "Abort atomic transaction"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_abort_transaction(db);
  return NULL;
}


void dfsch_module_tokyo_cabinet_register(dfsch_object_t* env){
  dfsch_package_t* tc_pkg = dfsch_make_package("tokyo-cabinet");
  dfsch_provide(env, "tokyo-cabinet");

  dfsch_defcanon_pkgcstr(env, tc_pkg, "open", DFSCH_PRIMITIVE_REF(open));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "close!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(close));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "prefix-search", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(prefix_search));
  dfsch_defcanon_pkgcstr(env, tc_pkg, "<db>", DFSCH_TOKYO_CABINET_DB_TYPE);

  dfsch_define_method_pkgcstr_1(env, tc_pkg, "begin-transaction!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(begin_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "commit-transaction!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(commit_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "abort-transaction!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(abort_transaction));

}
