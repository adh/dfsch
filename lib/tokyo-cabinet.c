#include "dfsch/lib/tokyo-cabinet.h"
#include <dfsch/hash.h>

#include <tcutil.h>
#include <tcadb.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>


typedef struct db_t {
  dfsch_type_t* type;
  TCADB* adb;
} db_t;

static dfsch_object_t* db_ref(db_t* db,
                              dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  int len;
  char* res = tcadbget(db->adb, k->ptr, k->len, &len);
  dfsch_object_t* value;
  
  if (res){
    value = dfsch_make_byte_vector(res, len);
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

static dfsch_object_t* db_get_iterator(db_t* db){
  int len;
  char* res;
  dfsch_object_t* it = NULL;
  if (!tcadbiterinit(db->adb)){
    dfsch_error("Error in interinit", db);
  }

  while (res = tcadbiternext(db->adb, &len)){
    it = dfsch_cons(dfsch_make_byte_vector(res, len),
                    it);
    free(res);
  }

  return it;
}

static dfsch_collection_methods_t db_collection = {
  .get_iterator = db_get_iterator,
};

dfsch_type_t dfsch_tokyo_cabinet_db_type = {
  .type = DFSCH_STANDARD_TYPE,
  //  .superclass = DFSCH_HASH_BASETYPE,
  .name = "tokyo-cabinet:db",
  .size = sizeof(db_t),
  
  .mapping = &db_mapping,
  .collection = &db_collection,
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
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_DB_TYPE);
  tcadbclose(db->adb);
}

dfsch_object_t* dfsch_tokyo_cabinet_prefix_search(dfsch_object_t* dbo,
                                                  char* buf, size_t len,
                                                  int limit){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_DB_TYPE);
  TCLIST* tcr;
  dfsch_object_t* res;
  
  tcr = tcadbfwmkeys(db->adb, buf, len, limit);
  if (!tcr){
    dfsch_error("tcadbfwmkeys returned null", NULL);
  }
  res = dfsch_tokyo_cabinet_list_2_object(tcr);
  tclistdel(tcr);
  return res;
}

void dfsch_tokyo_cabinet_begin_transaction(dfsch_object_t* dbo){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_DB_TYPE);
  if (!tcadbtranbegin(db->adb)){
    dfsch_error("tcadbtranbegin failed", NULL);
  }
}
void dfsch_tokyo_cabinet_commit_transaction(dfsch_object_t* dbo){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_DB_TYPE);
  if (!tcadbtrancommit(db->adb)){
    dfsch_error("Transaction commit failed", NULL);
  }
}
void dfsch_tokyo_cabinet_abort_transaction(dfsch_object_t* dbo){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_DB_TYPE);
  if (!tcadbtranabort(db->adb)){
    dfsch_error("Transaction abort failed", NULL);
  }
}

typedef struct table_t {
  dfsch_type_t* type;
  TCTDB* tdb;
} table_t;

static dfsch_object_t* table_ref(table_t* db,
                                 dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  TCMAP* res = tctdbget(db->tdb, k->ptr, k->len);
  dfsch_object_t* value;
  
  if (res){
    value = dfsch_tokyo_cabinet_map_2_object(res);
    tcmapdel(res);
    return value;
  } else {
    return NULL;
  }
}

static void table_set(table_t* db,
                      dfsch_object_t* key,
                      dfsch_object_t* value){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  TCMAP* v = dfsch_tokyo_cabinet_object_2_map(value);

  if (!tctdbput(db->tdb, k->ptr, k->len, v)){
    tcmapdel(v);
    dfsch_error("Error writing to database", db);
  }
  tcmapdel(v);
}

static void table_unset(table_t* db,
                        dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);

  return tctdbout(db->tdb, k->ptr, k->len);
}

static dfsch_mapping_methods_t table_mapping = {
  .ref = table_ref,
  .set = table_set,
  .unset = table_unset,
};

static dfsch_object_t* table_get_iterator(table_t* db){
  int len;
  char* res;
  dfsch_object_t* it = NULL;
  if (!tctdbiterinit(db->tdb)){
    dfsch_error("Error in interinit", db);
  }

  while (res = tctdbiternext(db->tdb, &len)){
    it = dfsch_cons(dfsch_make_byte_vector(res, len),
                    it);
    free(res);
  }

  return it;
}

static dfsch_collection_methods_t table_collection = {
  .get_iterator = table_get_iterator,
};

dfsch_type_t dfsch_tokyo_cabinet_table_type = {
  .type = DFSCH_STANDARD_TYPE,
  //  .superclass = DFSCH_HASH_BASETYPE,
  .name = "tokyo-cabinet:table",
  .size = sizeof(table_t),
  
  .mapping = &table_mapping,
  .collection = &table_collection,
};

static void table_finalizer(table_t* db, void* discard){
  tctdbdel(db->tdb);
}


dfsch_object_t* dfsch_tokyo_cabinet_table_open(char* name){
  TCTDB* tdb = tctdbnew();
  table_t* db = dfsch_make_object(DFSCH_TOKYO_CABINET_TABLE_TYPE);



  if (!tctdbopen(tdb, name, TDBOWRITER | TDBOCREAT)){
    tctdbdel(tdb);
    dfsch_error("Cannot open database", dfsch_make_string_cstr(name));
  }

  db->tdb = tdb;
  
  return db;
}
void dfsch_tokyo_cabinet_table_close(dfsch_object_t*dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  tctdbclose(db->tdb);
}

dfsch_object_t* dfsch_tokyo_cabinet_table_prefix_search(dfsch_object_t* dbo,
                                                        char* buf, size_t len,
                                                        int limit){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  TCLIST* tcr;
  dfsch_object_t* res;
  
  tcr = tctdbfwmkeys(db->tdb, buf, len, limit);
  if (!tcr){
    dfsch_error("tctdbfwmkeys returned null", NULL);
  }
  res = dfsch_tokyo_cabinet_list_2_object(tcr);
  tclistdel(tcr);
  return res;
}

void dfsch_tokyo_cabinet_table_begin_transaction(dfsch_object_t* dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  if (!tctdbtranbegin(db->tdb)){
    dfsch_error("tctdbtranbegin failed", NULL);
  }
}
void dfsch_tokyo_cabinet_table_commit_transaction(dfsch_object_t* dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  if (!tctdbtrancommit(db->tdb)){
    dfsch_error("Transaction commit failed", NULL);
  }
}
void dfsch_tokyo_cabinet_table_abort_transaction(dfsch_object_t* dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  if (!tctdbtranabort(db->tdb)){
    dfsch_error("Transaction abort failed", NULL);
  }
}


dfsch_object_t* dfsch_tokyo_cabinet_list_2_object(TCLIST* list){
  int i;
  int count;
  dfsch_object_t* it = NULL;
  count = tclistnum(list);
  for (i = 0; i < count; i++){
    int len;
    char* res = tclistval(list, count - i - 1, &len);
    it = dfsch_cons(dfsch_make_byte_vector(res, len),
                    it);
  }    
  return it;
}

TCMAP* dfsch_tokyo_cabinet_object_2_map(dfsch_object_t* obj){
  TCMAP* map = tcmapnew2(31);
  dfsch_object_t* i = dfsch_collection_get_iterator(obj);
  
  while (i){
    dfsch_object_t* j = dfsch_iterator_this(i);
    dfsch_strbuf_t* key;
    dfsch_strbuf_t* value;
    DFSCH_BUFFER_ARG(j, key);
    DFSCH_BUFFER_ARG(j, value);
    DFSCH_ARG_END(j);

    tcmapput(map, key->ptr, key->len, value->ptr, value->len);

    i = dfsch_iterator_next(i);
  }

  return map;
}
dfsch_object_t* dfsch_tokyo_cabinet_map_2_object(TCMAP* map){
  dfsch_object_t* res = dfsch_hash_make(DFSCH_HASH_EQUAL);
  char* kptr;
  int klen;
  char* vptr;
  int vlen;

  tcmapiterinit(map);

  while (kptr = tcmapiternext(map, &klen)){
    vptr = tcmapget(map, kptr, klen, &vlen);

    if (!vptr){
      dfsch_error("TCMAP key had disappeared", NULL);
    }

    dfsch_hash_set(res,
                   dfsch_make_string_buf(kptr, klen),
                   dfsch_make_string_buf(vptr, vlen));
  }

  return res;
}

