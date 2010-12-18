#include <dfsch/load.h>
#include <dfsch/hash.h>
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include <sqlite3.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

typedef struct sqlite_database_t {
  dfsch_type_t* type;
  sqlite3* db;
} sqlite3_database_t;

static dfsch_type_t sqlite3_database_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(sqlite3_database_t),
  "sqlite3:database",
  NULL,
  NULL,
  NULL
};

typedef struct sqlite3_result_t {
  dfsch_type_t* type;
  sqlite3_database_t* db;
  sqlite3_stmt* stmt;
  dfsch_object_t* last_res;
} sqlite3_result_t;


static void finalize_result(sqlite3_result_t* res);

static dfsch_object_t* get_row_as_vector(sqlite3_stmt* stmt){
  size_t i;
  int n_columns = sqlite3_column_count(stmt);
  dfsch_object_t* vec = dfsch_make_vector(n_columns, NULL);

  for (i = 0; i < n_columns; i++){
    dfsch_object_t* obj;

    switch (sqlite3_column_type(stmt, i)){
    case SQLITE_INTEGER:
      obj = dfsch_make_number_from_int64(sqlite3_column_int64(stmt, i));
      break;
    case SQLITE_FLOAT:
      obj = dfsch_make_number_from_double(sqlite3_column_double(stmt, i));
      break;
    case SQLITE3_TEXT:
      obj = dfsch_make_string_cstr(sqlite3_column_text(stmt, i));
      break;
    case SQLITE_NULL:
      obj = NULL;
      break;

    default:
      {
        char* buf = sqlite3_column_blob(stmt, i);
        obj = dfsch_make_byte_vector(buf, sqlite3_column_bytes(stmt, i));
      }
    }

    dfsch_vector_set(vec, i, obj);
  }

  return vec;
}

static dfsch_object_t* result_next(sqlite3_result_t* res){
  
  char* err;
  int ret;
  char**values;
  ret = sqlite3_step(res->stmt);

  if (ret == SQLITE_ROW){
     res->last_res = get_row_as_vector(res->stmt);
     return res;
  } else if (ret == SQLITE_BUSY) {
    dfsch_error("Database is busy", (dfsch_object_t*)res->db);
  } else if (ret == SQLITE_ERROR){
    finalize_result(res);
    return NULL;
  } else {
    return NULL;
  }
}
static dfsch_object_t* result_this(sqlite3_result_t* res){
  return res->last_res;
}

static dfsch_iterator_methods_t result_iterator = {
  .next = result_next,
  .this = result_this,
};

static dfsch_type_t sqlite3_result_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(sqlite3_result_t),
  "sqlite3:result",
  NULL,
  NULL,
  NULL,
  .collection = &dfsch_iterator_collection_methods,
  .iterator = &result_iterator,
};


static void database_finalizer(sqlite3_database_t* db, void* cd){
  if (db->type = &sqlite3_database_type){
    sqlite3_close(db->db);
  }
}


#define SQLITE3_DATABASE_ARG(al, name) \
  DFSCH_INSTANCE_ARG(al, name, sqlite3_database_t*, &sqlite3_database_type)

static void result_finalizer(sqlite3_result_t* res, void* cd){
  if (res->type = &sqlite3_result_type){
    sqlite3_finalize(res->stmt);
  }
}

#define SQLITE3_RESULT_ARG(al, name) \
  DFSCH_INSTANCE_ARG(al, name, sqlite3_result_t*, &sqlite3_result_type)


DFSCH_DEFINE_PRIMITIVE(open_database, 
                       "Open sqlite3 database"){
  char* filename;
  char* err;
  dfsch_object_t* creator = NULL;
  sqlite3_database_t* db;
  int created;
  int busy_timeout;
  dfsch_object_t* read_only = NULL;
  struct stat s;

  DFSCH_STRING_ARG_OPT(args, filename, ":memory:");
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("busy-timeout", busy_timeout, dfsch_number_to_long);
  DFSCH_KEYWORD("creator", creator);
  DFSCH_KEYWORD("read-only", read_only);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  db = (sqlite3_database_t*)dfsch_make_object(&sqlite3_database_type);
  GC_REGISTER_FINALIZER(db, 
                        (GC_finalization_proc)database_finalizer,
                        NULL, NULL, NULL);
 
  created = (stat(filename, &s)==-1 || !S_ISREG(s.st_mode));
  
  if (sqlite3_open_v2(filename, &db->db, 
                      SQLITE_OPEN_FULLMUTEX | 
                      (read_only != NULL ? 
                       SQLITE_OPEN_READONLY :
                       SQLITE_OPEN_READWRITE | (creator != NULL 
                                                ? SQLITE_OPEN_CREATE 
                                                : 0)), 
                      NULL) != SQLITE_OK){
    dfsch_object_t* str = dfsch_make_string_cstr(sqlite3_errmsg(db->db));
    sqlite3_close(db->db);
    dfsch_error("Error opening sqlite3 database", str);
  }

  if (created && creator){
      dfsch_apply(creator, dfsch_list(1, db));
  }

  return (dfsch_object_t*)db;
}

DFSCH_DEFINE_PRIMITIVE(close_database,
                       "Close Sqlite3 database"){
  sqlite3_database_t* db;
  SQLITE3_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);

  sqlite3_close(db->db);
  dfsch_invalidate_object(db);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(exec_string,
                       "Execute query and discard results"){
  sqlite3_database_t* db;
  char* sql;
  char* err;
  int ret;

  SQLITE3_DATABASE_ARG(args, db);
  DFSCH_STRING_ARG(args, sql);
  DFSCH_ARG_END(args);

  ret = sqlite3_exec(db->db, sql, NULL, NULL, &err);
  
  if (ret != SQLITE_OK){
    if (ret == SQLITE_BUSY) {
      dfsch_error("Database is busy", (dfsch_object_t*)db);
    }
    dfsch_error("Sqlite3 error", 
                dfsch_make_string_cstr(sqlite3_errmsg(db->db)));    
  }

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(query_string,
                       "Execute query and return result object"){
  sqlite3_database_t* db;
  sqlite3_result_t* res;
  char* sql;
  char* err;
  int ret;

  SQLITE3_DATABASE_ARG(args, db);
  DFSCH_STRING_ARG(args, sql);
  DFSCH_ARG_END(args);
  
  res = (sqlite3_result_t*)dfsch_make_object(&sqlite3_result_type);

  ret = sqlite3_prepare_v2(db->db, sql, -1, &(res->stmt), NULL);
  if (ret != SQLITE_OK){
    if (ret == SQLITE_BUSY) {
      dfsch_error("Database is busy", (dfsch_object_t*)db);
    }
    dfsch_error("Sqlite3 error", 
                dfsch_make_string_cstr(sqlite3_errmsg(db->db)));    
  }
  
  return result_next(res);
}

static void finalize_result(sqlite3_result_t* res){
  char* err;
  int ret;

  sqlite3_finalize(res->stmt);
  dfsch_invalidate_object(res);
}

DFSCH_DEFINE_PRIMITIVE(close_result,
                       "Close result object"){
  sqlite3_result_t* res;

  SQLITE3_RESULT_ARG(args, res);
  DFSCH_ARG_END(args);

  finalize_result(res);

  return NULL;
}



DFSCH_DEFINE_PRIMITIVE(column_names,
                       "Return vector of column names"){
  sqlite3_result_t* res;
  size_t i;
  dfsch_object_t* vec;
  int n_columns;

  SQLITE3_RESULT_ARG(args, res);
  DFSCH_ARG_END(args);

  n_columns = sqlite3_column_count(res->stmt);
  vec = dfsch_make_vector(n_columns, NULL);

  for (i = 0; i < n_columns; i++){
    dfsch_vector_set(vec, i, 
                     dfsch_make_string_cstr(sqlite3_column_name(res->stmt, i)));
  }

  return vec;
}

static dfsch_object_t* native_sqlite3_changes(void *baton, 
                                             dfsch_object_t* args, 
                                             dfsch_tail_escape_t* esc){
  sqlite3_database_t* db;
  SQLITE3_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(sqlite3_changes(db->db));
}
static dfsch_object_t* native_sqlite3_last_insert_rowid(void *baton, 
                                                       dfsch_object_t* args, 
                                                       dfsch_tail_escape_t* esc){
  sqlite3_database_t* db;
  SQLITE3_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(sqlite3_last_insert_rowid(db->db));
}

void dfsch_module_sqlite3_register(dfsch_object_t* env){
  dfsch_package_t* sql = dfsch_make_package("sql");
  dfsch_package_t* sqlite3 = dfsch_make_package("sqlite3");

  dfsch_require(env, "sql", NULL);
  dfsch_provide(env, "sqlite3");

  dfsch_define_pkgcstr(env, sqlite3, "<database>", &sqlite3_database_type);
  dfsch_define_pkgcstr(env, sqlite3, "<result>", &sqlite3_result_type);
  

  dfsch_define_pkgcstr(env, sqlite3, "open-database", 
                       DFSCH_PRIMITIVE_REF(open_database));

  dfsch_define_method_pkgcstr(env, sql, "close-database!",
                              NULL, dfsch_list(1, &sqlite3_database_type),
                              DFSCH_PRIMITIVE_REF(close_database));
  dfsch_define_method_pkgcstr(env, sql, "exec-string!",
                              NULL, dfsch_list(1, &sqlite3_database_type),
                              DFSCH_PRIMITIVE_REF(exec_string));
  dfsch_define_method_pkgcstr(env, sql, "query-string",
                              NULL, dfsch_list(1, &sqlite3_database_type),
                              DFSCH_PRIMITIVE_REF(query_string));

  dfsch_define_method_pkgcstr(env, sql, "close-result!",
                              NULL, dfsch_list(1, &sqlite3_result_type),
                              DFSCH_PRIMITIVE_REF(close_result));

  dfsch_define_method_pkgcstr(env, sql, "column-names",
                              NULL, dfsch_list(1, &sqlite3_result_type),
                              DFSCH_PRIMITIVE_REF(column_names));


  /* sqlite specific functions */
  dfsch_define_pkgcstr(env, sqlite3, "changes", 
                       dfsch_make_primitive(native_sqlite3_changes, NULL));
  dfsch_define_pkgcstr(env, sqlite3, "last-insert-rowid", 
                       dfsch_make_primitive(native_sqlite3_last_insert_rowid, 
                                         NULL));
}
