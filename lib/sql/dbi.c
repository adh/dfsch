#include <dfsch/load.h>
#include <dfsch/hash.h>
#include <dfsch/strings.h>
#include <dfsch/number.h>
#include <dfsch/util.h>
#include <dbi/dbi.h>
#include <assert.h>

typedef struct dbi_driver_t {
  dfsch_type_t* type;
  dbi_driver drv;
} dbi_driver_t;

static void driver_write(dbi_driver_t* drv, 
                         dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, drv, "%s", 
                         dbi_driver_get_name(drv->drv));
}


static dfsch_type_t driver_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "dbi:driver",
  .size = sizeof(dbi_driver_t),
  .write = driver_write,
};

static dfsch_object_t* cons_driver(dbi_driver drv){
  dbi_driver_t* dobj = dfsch_make_object(&driver_type);
  
  dobj->drv = drv;

  return dobj;
}

static dbi_driver get_driver(dfsch_object_t* obj){
  if (dfsch_proto_string_p(obj)){
    dbi_driver drv = dbi_driver_open(dfsch_string_to_cstr(obj));
    if (!drv){
      dfsch_error("No such DBI driver", obj);
    }
    return drv;
  } else {
    dbi_driver_t* drv = DFSCH_ASSERT_TYPE(obj, &driver_type);
    return drv->drv;
  }
}

#define DRIVER_ARG(al, name)\
  DFSCH_GENERIC_ARG(al, name, dbi_driver, get_driver)

typedef struct dbi_database_t {
  dfsch_type_t* type;
  dbi_conn conn;
  pthread_mutex_t* mutex;
} dbi_database_t;

static dfsch_type_t database_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "dbi:database",
  .size = sizeof(dbi_database_t)
};

static void conn_finalizer(dbi_database_t* db, void* cd){
  if (db->type == &database_type){
    dbi_conn_close(db->conn);
  }
}

static dfsch_object_t* cons_conn(dbi_conn conn){
  dbi_database_t* db = dfsch_make_object(&database_type);

  db->conn = conn;
  db->mutex = dfsch_create_finalized_mutex();

  GC_register_finalizer(db, conn_finalizer, NULL, NULL, NULL);

  return db;
}

#define DBI_DATABASE_ARG(al, name)                              \
  DFSCH_INSTANCE_ARG(al, name, dbi_database_t*, &database_type)

typedef struct dbi_result_t {
  dfsch_type_t* type;
  dbi_database_t* conn;
  dbi_result result;
} dbi_result_t;

static void conn_error(dbi_conn conn){
  char* message;
  dbi_conn_error(conn, &message);
  dfsch_error("DBI error", dfsch_make_string_cstr(message));
}


DFSCH_DEFINE_PRIMITIVE(initialize,
                       "Initialize libDBI"
                       DFSCH_DOC_SYNOPSIS("(&optional driver-path)")){
  char* driver_path;
  DFSCH_STRING_ARG_OPT(args, driver_path, NULL);
  DFSCH_ARG_END(args);

  dbi_initialize(driver_path);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(get_driver_list,
                       "Return list of supported drivers"){
  dbi_driver drv = NULL;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();

  DFSCH_ARG_END(args);

  while (drv = dbi_driver_list(drv)){
    dfsch_list_collect(lc, cons_driver(drv));
  }

  return dfsch_collected_list(lc);
}

static char* opt_convert(char* opt){
  char* i;
  i = opt = dfsch_stracpy(opt);
  while (*i){
    if (*i == '-'){
      *i = '_';
    }
    i++;
  }
  return opt;
}

DFSCH_DEFINE_PRIMITIVE(open_database, "Connect to DBI database"
                       DFSCH_DOC_SYNOPSIS("(driver &rest options)")){
  dbi_driver driver;
  dbi_conn conn;
  DRIVER_ARG(args, driver);
  
  conn = dbi_conn_open(driver);

  assert(conn);

  while (args){
    char* option;
    char* value;
    DFSCH_STRING_OR_SYMBOL_ARG(args, option);
    DFSCH_STRING_ARG(args, value);
    
    option = opt_convert(option);

    dbi_conn_set_option(conn, option, value);
  }

  if (dbi_conn_connect(conn) < 0){
    conn_error(conn);
  }

  return cons_conn(conn);
}

DFSCH_DEFINE_PRIMITIVE(close_database, "Close database connection"
                       DFSCH_DOC_SYNOPSIS("(db)")){
  dbi_database_t* db;
  DBI_DATABASE_ARG(args, db);
  DFSCH_ARG_END(args);
  
  dbi_conn_close(db->conn);
  dfsch_invalidate_object(db);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(exec_string,
                       "Execute query and discard results"
                       DFSCH_DOC_SYNOPSIS("(db sql)")){
  dbi_database_t* db;
  char* sql;
  dbi_result res;
  DBI_DATABASE_ARG(args, db);
  DFSCH_STRING_ARG(args, sql);
  DFSCH_ARG_END(args);
  
  res = dbi_conn_query(db->conn, sql);

  if (!res){
    conn_error(db->conn);
  }

  dbi_result_free(res);

  return NULL;
}


void dfsch_module_dbi_register(dfsch_object_t* env){
  dfsch_package_t* dbi = dfsch_make_package("dbi",
                                            "LibDBI-based database interface");
  dfsch_package_t* sql = dfsch_make_package("sql", NULL);

  dfsch_require(env, "sql", NULL);
  dfsch_provide(env, "dbi");

  dfsch_defcanon_pkgcstr(env, dbi, "initialize",
                         DFSCH_PRIMITIVE_REF(initialize));
  dfsch_defcanon_pkgcstr(env, dbi, "get-driver-list",
                         DFSCH_PRIMITIVE_REF(get_driver_list));

  dfsch_defcanon_pkgcstr(env, dbi, "open-database",
                         DFSCH_PRIMITIVE_REF(open_database));

  dfsch_define_method_pkgcstr(env, sql, "close-database!",
                              NULL, dfsch_list(1, &database_type),
                              DFSCH_PRIMITIVE_REF(close_database));
  dfsch_define_method_pkgcstr(env, sql, "exec-string!",
                              NULL, dfsch_list(1, &database_type),
                              DFSCH_PRIMITIVE_REF(exec_string));
}
