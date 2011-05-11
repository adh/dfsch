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

DFSCH_DEFINE_PRIMITIVE(set_table_index, "Change index configuration of table"){
  dfsch_object_t* db;
  char* name;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_STRING_ARG(args, name);
  
  dfsch_tokyo_cabinet_table_set_index(db, name,
                                      dfsch_tokyo_cabinet_parse_index_type(args));

  return NULL;
}


DFSCH_DEFINE_PRIMITIVE(make_query, "Create new table query object"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_tyrant_make_query(db);
}
DFSCH_DEFINE_PRIMITIVE(add_query_condition, "Add new condition to query"){
  dfsch_object_t* query;
  char* col_name;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_STRING_ARG(args, col_name);

  dfsch_tokyo_tyrant_add_query_condition(query, col_name, args);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_query_order, "Set sorting order of query results"){
  dfsch_object_t* query;
  char* col_name;
  dfsch_object_t* t;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_STRING_ARG(args, col_name);
  DFSCH_OBJECT_ARG(args, t);
  DFSCH_ARG_END(args);
  
  dfsch_tokyo_tyrant_set_query_order(query, col_name, 
                                      dfsch_tokyo_tyrant_parse_order_type(t));

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_query_limit, "Set maximum for number of query results"){
  dfsch_object_t* query;
  int count;
  int skip;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_LONG_ARG(args, count);
  DFSCH_LONG_ARG(args, skip);
  DFSCH_ARG_END(args);

  dfsch_tokyo_tyrant_set_query_limit(query, count, skip);

  return NULL;
}


DFSCH_DEFINE_PRIMITIVE(query_search, "Execute query and return list of "
                       "matching record IDs"){
  dfsch_object_t* query;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_ARG_END(args);
  
  return dfsch_tokyo_tyrant_query_search(query);
}

DFSCH_DEFINE_PRIMITIVE(query_get_records, "Execute query and return list of "
                       "matching records"){
  dfsch_object_t* query;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_ARG_END(args);
  
  return dfsch_tokyo_tyrant_query_get_records(query);
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

  dfsch_define_method_pkgcstr_1(env, tc_pkg, "set-table-index!", 
                                DFSCH_TOKYO_TYRANT_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(set_table_index));

  dfsch_define_method_pkgcstr_1(env, tc_pkg, "make-query", 
                                DFSCH_TOKYO_TYRANT_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(make_query));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "add-condition!", 
                                DFSCH_TOKYO_TYRANT_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(add_query_condition));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "set-order!", 
                                DFSCH_TOKYO_TYRANT_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(set_query_order));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "set-limit!", 
                                DFSCH_TOKYO_TYRANT_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(set_query_limit));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "query-search", 
                                DFSCH_TOKYO_TYRANT_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(query_search));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "query-get-records", 
                                DFSCH_TOKYO_TYRANT_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(query_get_records));

}
