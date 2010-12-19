#include "dfsch/lib/tokyo-cabinet.h"

#include <tcutil.h>
#include <tcadb.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>


typedef struct db_t {
  dfsch_type_t* type;
  TCADB* adb;
} db_t;

static int db_ref(db_t* db,
                  dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  int len;
  char* res = tcadbget(db->adb, k->ptr, k->len, &len);
  dfsch_object_t* value;
  
  if (res){
    value = dfsch_make_string_buf(res, len);
    free(res);
    return value;
  } else {
    return DFSCH_INVALID_OBJECT;
  }
}

static void db_set(db_t* db,
                   dfsch_object_t* key,
                   dfsch_object_t* value){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  dfsch_strbuf_t* v = dfsch_string_to_buf(value);

  if (!tcadbput(db->adb, k->ptr, k->len, v->ptr, v->len)){
    dfsch_error("Error writing to database", db);
  }
}

static void db_unset(db_t* db,
                     dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);

  return tcadbout(db->adb, k->ptr, k->len);
}

static dfsch_mapping_methods_t db_mapping = {
  .ref = db_ref,
  .set = db_set,
  .unset = db_unset,
};

dfsch_type_t dfsch_tokyo_cabinet_db_type = {
  .type = DFSCH_STANDARD_TYPE,
  //  .superclass = DFSCH_HASH_BASETYPE,
  .name = "tokyo-cabinet:db",
  .size = sizeof(db_t),
  
  .mapping = &db_mapping,
};

static void db_finalizer(db_t* db, void* discard){
  tcadbdel(db->adb);
}

dfsch_object_t* dfsch_tokyo_cabinet_db_open(char* name){
  TCADB* adb = tcadbnew();
  db_t* db = dfsch_make_object(DFSCH_TOKYO_CABINET_DB_TYPE);

  if (!tcadbopen(adb, name)){
    tcadbdel(adb);
    dfsch_error("Cannot open database", dfsch_make_string_cstr(name));
  }

  db->adb = adb;
  
  return db;
}
void dfsch_tokyo_cabinet_db_close(dfsch_object_t*dbo){
  db_t* db = DFSCH_ASSERT_INSTANCE(db, DFSCH_TOKYO_CABINET_DB_TYPE);
  tcadbclose(db->adb);
}
