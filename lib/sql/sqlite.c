#include <dfsch/load.h>
#include <dfsch/hash.h>
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include <sqlite.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

typedef struct sqlite_database_t {
  dfsch_type_t* type;
  sqlite* db;
} sqlite_database_t;

static dfsch_type_t sqlite_database_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(sqlite_database_t),
  "sqlite:database",
  NULL,
  NULL,
  NULL
};

typedef struct sqlite_result_t {
  dfsch_type_t* type;
  sqlite_database_t* db;
  sqlite_vm* vm;
  dfsch_object_t* last_res;
  int n_columns;
  char**names;
} sqlite_result_t;


static void finalize_result(sqlite_result_t* res);

static dfsch_object_t* get_row_as_vector(int n_columns, char**values){
  size_t i;
  dfsch_object_t* vec = dfsch_make_vector(n_columns, NULL);

  for (i = 0; i < n_columns; i++){
    dfsch_vector_set(vec, i, dfsch_make_string_cstr(values[i]));
  }

  return vec;
}

static dfsch_object_t* result_next(sqlite_result_t* res){
  
  char* err;
  int ret;
  char**values;
  ret = sqlite_step(res->vm, &res->n_columns, &values, &res->names);

  if (ret == SQLITE_ROW){
     res->last_res = get_row_as_vector(res->n_columns, values);
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
static dfsch_object_t* result_this(sqlite_result_t* res){
  return res->last_res;
}

static dfsch_iterator_methods_t result_iterator = {
  .next = result_next,
  .this = result_this,
};

static dfsch_type_t sqlite_result_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(sqlite_result_t),
  "sqlite:result",
  NULL,
  NULL,
  NULL,
  .collection = &dfsch_iterator_collection_methods,
  .iterator = &result_iterator,
};


static void database_finalizer(sqlite_database_t* db, void* cd){
  if (db->type = &sqlite_database_type){
    sqlite_close(db->db);
  }
}


#define SQLITE_DATABASE_ARG(al, name) \
  DFSCH_INSTANCE_ARG(al, name, sqlite_database_t*, &sqlite_database_type)

static void result_finalizer(sqlite_result_t* res, void* cd){
  if (res->type = &sqlite_result_type){
    sqlite_finalize(res->vm, NULL);
  }
}

#define SQLITE_RESULT_ARG(al, name) \
  DFSCH_INSTANCE_ARG(al, name, sqlite_result_t*, &sqlite_result_type)


DFSCH_DEFINE_PRIMITIVE(open_database, 
                       "Open sqlite database"){
  char* filename;
  char* err;
  dfsch_object_t* creator;
  sqlite_database_t* db;
  int created;
  int busy_timeout;
  struct stat s;

  DFSCH_STRING_ARG_OPT(args, filename, ":memory:");
  DFSCH_LONG_ARG_OPT(args, busy_timeout, -1);
  DFSCH_OBJECT_ARG_OPT(args, creator, NULL);
  DFSCH_ARG_END(args);

  db = (sqlite_database_t*)dfsch_make_object(&sqlite_database_type);
  GC_REGISTER_FINALIZER(db, 
                        (GC_finalization_proc)database_finalizer,
                        NULL, NULL, NULL);
 
  created = (stat(filename, &s)==-1 || !S_ISREG(s.st_mode));
  
  db->db = sqlite_open(filename, 0, &err);

  if (!db->db){
    dfsch_object_t* message;
    message = dfsch_make_string_cstr(err);
    sqlite_freemem(err);
    dfsch_error("Error opening sqlite database", message);
  }

  if (created && creator){
      dfsch_apply(creator, dfsch_list(1, db));
  }

  return (dfsch_object_t*)db;
}

DFSCH_DEFINE_PRIMITIVE(close_database,
                       "Close Sqlite database"){
  sqlite_database_t* db;
  SQLITE_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);

  sqlite_close(db->db);
  dfsch_invalidate_object(db);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(exec_string,
                       "Execute query and discard results"){
  sqlite_database_t* db;
  char* sql;
  char* err;
  int ret;

  SQLITE_DATABASE_ARG(args, db);
  DFSCH_STRING_ARG(args, sql);
  DFSCH_ARG_END(args);

  ret = sqlite_exec(db->db, sql, NULL, NULL, &err);
  
  if (ret != SQLITE_OK){
    if (ret == SQLITE_BUSY) {
      dfsch_error("Database is busy", (dfsch_object_t*)db);
    }
    dfsch_object_t* message;
    message = dfsch_make_string_cstr(err);
    sqlite_freemem(err);
    dfsch_error("Sqlite error", message);    
  }

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(query_string,
                       "Execute query and return result object"){
  sqlite_database_t* db;
  sqlite_result_t* res;
  char* sql;
  char* err;
  int ret;

  SQLITE_DATABASE_ARG(args, db);
  DFSCH_STRING_ARG(args, sql);
  DFSCH_ARG_END(args);
  
  res = (sqlite_result_t*)dfsch_make_object(&sqlite_result_type);

  ret = sqlite_compile(db->db, sql, NULL, &(res->vm), &err);
  if (ret != SQLITE_OK){
    if (ret == SQLITE_BUSY) {
      dfsch_error("Database is busy", (dfsch_object_t*)db);
    }
    dfsch_object_t* message;
    message = dfsch_make_string_cstr(err);
    sqlite_freemem(err);
    dfsch_error("Sqlite error", message);    
  }
  
  return result_next(res);
}

static void finalize_result(sqlite_result_t* res){
  char* err;
  int ret;

  ret = sqlite_finalize(res->vm, &err);
  if (ret != SQLITE_OK){
    dfsch_object_t* message;
    message = dfsch_make_string_cstr(err);
    sqlite_freemem(err);
    dfsch_error("sqlite:error", message);    
  }
  dfsch_invalidate_object(res);
}

DFSCH_DEFINE_PRIMITIVE(close_result,
                       "Close result object"){
  sqlite_result_t* res;

  SQLITE_RESULT_ARG(args, res);
  DFSCH_ARG_END(args);

  finalize_result(res);

  return NULL;
}



DFSCH_DEFINE_PRIMITIVE(column_names,
                       "Return vector of column names"){
  sqlite_result_t* res;
  size_t i;
  dfsch_object_t* vec;

  SQLITE_RESULT_ARG(args, res);
  DFSCH_ARG_END(args);

  vec = dfsch_make_vector(res->n_columns, NULL);

  for (i = 0; i < res->n_columns; i++){
    dfsch_vector_set(vec, i, dfsch_make_string_cstr(res->names[i]));
  }

  return vec;
}

DFSCH_DEFINE_PRIMITIVE(column_types,
                       "Return vector of column types"){
  sqlite_result_t* res;
  size_t i;
  dfsch_object_t* vec;

  SQLITE_RESULT_ARG(args, res);
  DFSCH_ARG_END(args);

  vec = dfsch_make_vector(res->n_columns, NULL);

  for (i = 0; i < res->n_columns; i++){
    dfsch_vector_set(vec, i, 
                     dfsch_make_string_cstr(res->names[i + res->n_columns]));
  }

  return vec;
}



static dfsch_object_t* native_sqlite_changes(void *baton, 
                                             dfsch_object_t* args, 
                                             dfsch_tail_escape_t* esc){
  sqlite_database_t* db;
  SQLITE_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(sqlite_changes(db->db));
}
static dfsch_object_t* native_sqlite_last_insert_rowid(void *baton, 
                                                       dfsch_object_t* args, 
                                                       dfsch_tail_escape_t* esc){
  sqlite_database_t* db;
  SQLITE_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(sqlite_last_insert_rowid(db->db));
}

dfsch_object_t* dfsch_module_sqlite_register(dfsch_object_t* env){
  dfsch_package_t* sql = dfsch_make_package("sql");
  dfsch_package_t* sqlite = dfsch_make_package("sqlite");

  dfsch_define_pkgcstr(env, sqlite, "<database>", &sqlite_database_type);
  dfsch_define_pkgcstr(env, sqlite, "<result>", &sqlite_result_type);
  

  dfsch_define_pkgcstr(env, sqlite, "open-database", 
                       DFSCH_PRIMITIVE_REF(open_database));

  dfsch_define_method_pkgcstr(env, sql, "close-database!",
                              NULL, dfsch_list(1, &sqlite_database_type),
                              DFSCH_PRIMITIVE_REF(close_database));
  dfsch_define_method_pkgcstr(env, sql, "exec-string!",
                              NULL, dfsch_list(1, &sqlite_database_type),
                              DFSCH_PRIMITIVE_REF(exec_string));
  dfsch_define_method_pkgcstr(env, sql, "query-string",
                              NULL, dfsch_list(1, &sqlite_database_type),
                              DFSCH_PRIMITIVE_REF(query_string));

  dfsch_define_method_pkgcstr(env, sql, "close-result!",
                              NULL, dfsch_list(1, &sqlite_result_type),
                              DFSCH_PRIMITIVE_REF(close_result));

  dfsch_define_method_pkgcstr(env, sql, "column-names",
                              NULL, dfsch_list(1, &sqlite_result_type),
                              DFSCH_PRIMITIVE_REF(column_names));
  dfsch_define_method_pkgcstr(env, sql, "column-types",
                              NULL, dfsch_list(1, &sqlite_result_type),
                              DFSCH_PRIMITIVE_REF(column_types));

  


  /* sqite specific functions */
  dfsch_define_pkgcstr(env, sqlite, "changes", 
                       dfsch_make_primitive(native_sqlite_changes, NULL));
  dfsch_define_pkgcstr(env, sqlite, "last-insert-rowid", 
                       dfsch_make_primitive(native_sqlite_last_insert_rowid, 
                                         NULL));


  dfsch_provide(env, "sqlite");
  return NULL;
}
