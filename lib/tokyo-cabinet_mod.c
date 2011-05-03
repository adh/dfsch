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

DFSCH_DEFINE_PRIMITIVE(db_sync, "Write changed records to disk"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_db_sync(db);
  return NULL;  
}

DFSCH_DEFINE_PRIMITIVE(open_table, "Open Tokyo Cabinet table database"){
  char* name;
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_cabinet_table_open(name);
}

DFSCH_DEFINE_PRIMITIVE(close_table, "Close Tokyo Cabinet table database"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_table_close(db);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(table_prefix_search, 
                       "Find all keys starting with given prefix"){
  dfsch_object_t* db;
  dfsch_strbuf_t* prefix;
  long limit;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_BUFFER_ARG(args, prefix);
  DFSCH_LONG_ARG_OPT(args, limit, -1);
  DFSCH_ARG_END(args);

  return dfsch_tokyo_cabinet_table_prefix_search(db, 
                                                 prefix->ptr, 
                                                 prefix->len, 
                                                 limit);
}

DFSCH_DEFINE_PRIMITIVE(table_begin_transaction, "Start atomic transaction"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_table_begin_transaction(db);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(table_commit_transaction, "Commit atomic transaction"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_table_commit_transaction(db);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(table_abort_transaction, "Abort atomic transaction"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_table_abort_transaction(db);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(table_sync, "Write changed records to disk"){
  dfsch_object_t* db;
  DFSCH_OBJECT_ARG(args, db);
  DFSCH_ARG_END(args);

  dfsch_tokyo_cabinet_table_sync(db);
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

  return dfsch_tokyo_cabinet_make_query(db);
}
DFSCH_DEFINE_PRIMITIVE(add_query_condition, "Add new condition to query"){
  dfsch_object_t* query;
  char* col_name;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_STRING_ARG(args, col_name);

  dfsch_tokyo_cabinet_add_query_condition(query, col_name, args);
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
  
  dfsch_tokyo_cabinet_set_query_order(query, col_name, 
                                      dfsch_tokyo_cabinet_parse_order_type(t));

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

  dfsch_tokyo_cabinet_set_query_limit(query, count, skip);

  return NULL;
}


DFSCH_DEFINE_PRIMITIVE(query_search, "Execute query and return list of "
                       "matching record IDs"){
  dfsch_object_t* query;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_ARG_END(args);
  
  return dfsch_tokyo_cabinet_query_search(query);
}

DFSCH_DEFINE_PRIMITIVE(query_get_records, "Execute query and return list of "
                       "matching records"){
  dfsch_object_t* query;
  DFSCH_OBJECT_ARG(args, query);
  DFSCH_ARG_END(args);
  
  return dfsch_tokyo_cabinet_query_get_records(query);
}

void dfsch_module_tokyo_cabinet_register(dfsch_object_t* env){
  dfsch_package_t* tc_pkg = dfsch_make_package("tokyo-cabinet",
                                               "Tokyo Cabinet interface");
  dfsch_provide(env, "tokyo-cabinet");

  dfsch_defcanon_pkgcstr(env, tc_pkg, "<db>", DFSCH_TOKYO_CABINET_DB_TYPE);
  dfsch_defcanon_pkgcstr(env, tc_pkg, "<table>", 
                         DFSCH_TOKYO_CABINET_TABLE_TYPE);


  dfsch_defcanon_pkgcstr(env, tc_pkg, "open", DFSCH_PRIMITIVE_REF(open));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "close!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(close));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "prefix-search", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(prefix_search));

  dfsch_define_method_pkgcstr_1(env, tc_pkg, "begin-transaction!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(begin_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "commit-transaction!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(commit_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "abort-transaction!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(abort_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "sync!", 
                                DFSCH_TOKYO_CABINET_DB_TYPE,
                                DFSCH_PRIMITIVE_REF(db_sync));

  dfsch_defcanon_pkgcstr(env, tc_pkg, "open-table", 
                         DFSCH_PRIMITIVE_REF(open_table));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "close!", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(close_table));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "prefix-search", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(table_prefix_search));

  dfsch_define_method_pkgcstr_1(env, tc_pkg, "begin-transaction!", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(table_begin_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "commit-transaction!", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(table_commit_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "abort-transaction!", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(table_abort_transaction));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "sync!", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(table_sync));

  dfsch_define_method_pkgcstr_1(env, tc_pkg, "set-table-index!", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(set_table_index));


  dfsch_define_method_pkgcstr_1(env, tc_pkg, "make-query", 
                                DFSCH_TOKYO_CABINET_TABLE_TYPE,
                                DFSCH_PRIMITIVE_REF(make_query));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "add-condition!", 
                                DFSCH_TOKYO_CABINET_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(add_query_condition));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "set-order!", 
                                DFSCH_TOKYO_CABINET_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(set_query_order));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "set-limit!", 
                                DFSCH_TOKYO_CABINET_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(set_query_limit));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "query-search", 
                                DFSCH_TOKYO_CABINET_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(query_search));
  dfsch_define_method_pkgcstr_1(env, tc_pkg, "query-get-records", 
                                DFSCH_TOKYO_CABINET_QUERY_TYPE,
                                DFSCH_PRIMITIVE_REF(query_get_records));

}
