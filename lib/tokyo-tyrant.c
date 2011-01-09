#include "dfsch/lib/tokyo-tyrant.h"

#include <tcutil.h>
#include <tcrdb.h>
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
  char* res = tcrdbget(db->adb, k->ptr, k->len, &len);
  dfsch_object_t* value;
  
  if (res){
    value = dfsch_make_byte_vector(res, len);
    free(res);
    return value;
  } else {
    return NULL;
  }
}

static void db_set(db_t* db,
                   dfsch_object_t* key,
                   dfsch_object_t* value){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  dfsch_strbuf_t* v = dfsch_string_to_buf(value);

  if (!tcrdbput(db->adb, k->ptr, k->len, v->ptr, v->len)){
    dfsch_error("Error writing to database", db);
  }
}

static void db_unset(db_t* db,
                     dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);

  return tcrdbout(db->adb, k->ptr, k->len);
}

static dfsch_mapping_methods_t db_mapping = {
  .ref = db_ref,
  .set = db_set,
  .unset = db_unset,
};

static dfsch_object_t* db_get_iterator(db_t* db){
  int len;
  char* res;
  dfsch_object_t* it = NULL;
  if (!tcrdbiterinit(db->adb)){
    dfsch_error("Error in interinit", db);
  }

  while (res = tcrdbiternext(db->adb, &len)){
    it = dfsch_cons(dfsch_make_byte_vector(res, len),
                    it);
    free(res);
  }

  return it;
}

static dfsch_collection_methods_t db_collection = {
  .get_iterator = db_get_iterator,
};

dfsch_type_t dfsch_tokyo_tyrant_db_type = {
  .type = DFSCH_STANDARD_TYPE,
  //  .superclass = DFSCH_HASH_BASETYPE,
  .name = "tokyo-tyrant:db",
  .size = sizeof(db_t),
  
  .mapping = &db_mapping,
  .collection = &db_collection,
};

static void db_finalizer(db_t* db, void* discard){
  tcrdbdel(db->adb);
}

dfsch_object_t* dfsch_tokyo_tyrant_db_open(char* name){
  TCADB* adb = tcrdbnew();
  db_t* db = dfsch_make_object(DFSCH_TOKYO_TYRANT_DB_TYPE);

  if (!tcrdbopen2(adb, name)){
    tcrdbdel(adb);
    dfsch_error("Cannot open database", dfsch_make_string_cstr(name));
  }

  db->adb = adb;
  
  return db;
}
void dfsch_tokyo_tyrant_db_close(dfsch_object_t*dbo){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_TYRANT_DB_TYPE);
  tcrdbclose(db->adb);
}

dfsch_object_t* dfsch_tokyo_tyrant_prefix_search(dfsch_object_t* dbo,
                                                  char* buf, size_t len,
                                                  int limit){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_TYRANT_DB_TYPE);
  TCLIST* tcr;
  int i;
  int count;
  dfsch_object_t* it = NULL;
  
  tcr = tcrdbfwmkeys(db->adb, buf, len, limit);
  if (!tcr){
    dfsch_error("tcrdbfwmkeys returned null", NULL);
  }
  count = tclistnum(tcr);
  for (i = 0; i < count; i++){
    int len;
    char* res = tclistval(tcr, i, &len);
    it = dfsch_cons(dfsch_make_byte_vector(res, len),
                    it);
  }
  tclistdel(tcr);
  return it;
}

