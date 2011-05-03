#include "dfsch/lib/tokyo-cabinet.h"
#include <dfsch/hash.h>
#include <dfsch/util.h>

#include <tcutil.h>
#include <tcadb.h>
#include <tctdb.h>
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

static dfsch_object_t* db_get_keys_iterator(db_t* db){
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

static dfsch_mapping_methods_t db_mapping = {
  .ref = db_ref,
  .set = db_set,
  .unset = db_unset,
  .get_keys_iterator = db_get_keys_iterator,
};

static dfsch_object_t* db_get_iterator(db_t* db){
  int len;
  char* res;
  int klen;
  char* kres;
  dfsch_object_t* it = NULL;
  if (!tcadbiterinit(db->adb)){
    dfsch_error("Error in interinit", db);
  }

  while (kres = tcadbiternext(db->adb, &klen)){
    res = tcadbget(db->adb, kres, klen, &len);
    it = dfsch_cons(dfsch_list(2, 
                               dfsch_make_byte_vector(kres, klen),
                               dfsch_make_byte_vector(res, len)),
                    it);
    free(res);
    free(kres);
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
  if (db->type == DFSCH_TOKYO_CABINET_DB_TYPE){
    tcadbdel(db->adb);
  }
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
  tcadbdel(db->adb);
  dfsch_invalidate_object(db);
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

void dfsch_tokyo_cabinet_db_sync(dfsch_object_t* dbo){
  db_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_DB_TYPE);
  tcadbsync(db->adb);
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

static dfsch_object_t* table_get_keys_iterator(table_t* db){
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


static dfsch_mapping_methods_t table_mapping = {
  .ref = table_ref,
  .set = table_set,
  .unset = table_unset,

  .get_keys_iterator = table_get_keys_iterator,
};

static dfsch_object_t* tcidl_2_entries(table_t* db, dfsch_object_t* idl){
  dfsch_object_t* res = NULL;

  while (DFSCH_PAIR_P(idl)){
    dfsch_object_t* k = DFSCH_FAST_CAR(idl);
    dfsch_object_t* v = table_ref(db, k);

    res = dfsch_cons(dfsch_list(2, k, v), res);

    idl = DFSCH_FAST_CDR(idl);
  }
  return res;
}

static dfsch_object_t* table_get_iterator(table_t* db){
  return tcidl_2_entries(db, table_get_keys_iterator(db));
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
  if (db->type == DFSCH_TOKYO_CABINET_TABLE_TYPE){
    tcadbdel(db->tdb);
  }
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
  tctdbdel(db->tdb);
  dfsch_invalidate_object(db);
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
void dfsch_tokyo_cabinet_table_sync(dfsch_object_t* dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  tctdbsync(db->tdb);
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
  dfsch_object_t* res = dfsch_make_hash();
  char* kptr;
  int klen;
  char* vptr;
  int vlen;

  tcmapiterinit(map);

  while (kptr = tcmapiternext(map, &klen)){
    vptr = tcmapget(map, kptr, klen, &vlen);

    if (!vptr){
      dfsch_error("TCMAP key has disappeared", NULL);
    }

    dfsch_hash_set(res,
                   dfsch_make_string_buf(kptr, klen),
                   dfsch_make_string_buf(vptr, vlen));
  }

  return res;
}

void dfsch_tokyo_cabinet_table_set_index(dfsch_object_t* dbo,
                                        char* name,
                                        int type){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  int tcr;

  tcr = tctdbsetindex(db->tdb, name, type);
  if (!tcr && !(type & TDBITKEEP)){
    dfsch_error("Error modifying table index", db);
  }
}

int dfsch_tokyo_cabinet_parse_index_type(dfsch_object_t* args){
  int type = TDBITKEEP;

  DFSCH_FLAG_PARSER_BEGIN_ONE(args, type);
  DFSCH_FLAG_SET("lexical", TDBITLEXICAL, type);
  DFSCH_FLAG_SET("decimal", TDBITDECIMAL, type);
  DFSCH_FLAG_SET("token", TDBITTOKEN, type);
  DFSCH_FLAG_SET("full-text", TDBITTOKEN, type);
  DFSCH_FLAG_SET("q-gram", TDBITQGRAM, type);
  DFSCH_FLAG_PARSER_END(args);

  DFSCH_FLAG_PARSER_BEGIN_ONE_OPT(args, type);
  DFSCH_FLAG_UNSET("rebuild", TDBITKEEP, type);
  DFSCH_FLAG_SET("optimize", TDBITOPT, type);
  DFSCH_FLAG_SET("remove", TDBITVOID, type);
  DFSCH_FLAG_PARSER_END(args);

  return type;
}


typedef struct query_t {
  dfsch_type_t* type;
  TDBQRY* qry;
  table_t* db;
} query_t;

dfsch_type_t dfsch_tokyo_cabinet_query_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "tokyo-cabinet:query",
  .size = sizeof(query_t),
};

static void query_finalizer(query_t* q, void* discard){
  if (q->type == DFSCH_TOKYO_CABINET_QUERY_TYPE){
    tctdbqrydel(q->qry);
  }
}

dfsch_object_t* dfsch_tokyo_cabinet_make_query(dfsch_object_t* dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_CABINET_TABLE_TYPE);
  query_t* q = dfsch_make_object(DFSCH_TOKYO_CABINET_QUERY_TYPE);
  
  q->db = db; /* for GC visibility */
  q->qry = tctdbqrynew(db->tdb);

  GC_REGISTER_FINALIZER(q, (GC_finalization_proc)query_finalizer,
                        NULL, NULL, NULL);

  return q;
}


static char* convert_value(dfsch_object_t* val){
  if (dfsch_proto_string_p(val)){
    return dfsch_proto_string_to_cstr(val);
  } else {
    return dfsch_object_2_string(val, 10, DFSCH_WRITE);    
  }
}

char* dfsch_tokyo_cabinet_build_expression(dfsch_object_t* args){
  dfsch_object_t* obj;
  char* str;
  dfsch_str_list_t* sl;

  if (!DFSCH_PAIR_P(args)){
    dfsch_error("Expression is empty" , args);
  }
  
  obj = DFSCH_FAST_CAR(args);
  args = DFSCH_FAST_CDR(args);

  str = convert_value(obj);

  if (!DFSCH_PAIR_P(args)){
    return str;
  }

  sl = dfsch_sl_create();
  dfsch_sl_append(sl, str);

  while (DFSCH_PAIR_P(args)){
    dfsch_sl_append(sl, " ");
    dfsch_sl_append(sl, convert_value(DFSCH_FAST_CAR(args)));
    args = DFSCH_FAST_CDR(args);
  }

  return dfsch_sl_value(sl);
}

void dfsch_tokyo_cabinet_add_query_condition(dfsch_object_t* qo,
                                             char* col_name,
                                             dfsch_object_t* args){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_CABINET_QUERY_TYPE);
  char* expr;

  int op = 0;

  if (DFSCH_PAIR_P(args) && 
      dfsch_compare_keyword(DFSCH_FAST_CAR(args), "not")){
    op |= TDBQCNEGATE;
    args = DFSCH_FAST_CDR(args);
  }

  DFSCH_FLAG_PARSER_BEGIN_ONE(args, op);
  DFSCH_FLAG_SET("equal", TDBQCSTREQ, op);
  DFSCH_FLAG_SET("includes", TDBQCSTRINC, op);
  DFSCH_FLAG_SET("begins-with", TDBQCSTRBW, op);
  DFSCH_FLAG_SET("ends-with", TDBQCSTREW, op);
  DFSCH_FLAG_SET("includes-all", TDBQCSTRAND, op);
  DFSCH_FLAG_SET("includes-some", TDBQCSTROR, op);
  DFSCH_FLAG_SET("one-of", TDBQCSTROREQ, op);
  DFSCH_FLAG_SET("regex", TDBQCSTRRX, op);

  DFSCH_FLAG_SET("=", TDBQCNUMEQ, op);
  DFSCH_FLAG_SET(">", TDBQCNUMGT, op);
  DFSCH_FLAG_SET(">=", TDBQCNUMGE, op);
  DFSCH_FLAG_SET("<", TDBQCNUMLT, op);
  DFSCH_FLAG_SET("<=", TDBQCNUMLE, op);
  DFSCH_FLAG_SET("between", TDBQCNUMBT, op);
  DFSCH_FLAG_SET("numerically-one-of", TDBQCNUMOREQ, op);
  
  DFSCH_FLAG_SET("ft-phrase", TDBQCFTSPH, op);
  DFSCH_FLAG_SET("ft-all", TDBQCFTSAND, op);
  DFSCH_FLAG_SET("ft-some", TDBQCFTSOR, op);
  DFSCH_FLAG_SET("ft-compound", TDBQCFTSEX, op);
  DFSCH_FLAG_PARSER_END(args);

  if (DFSCH_PAIR_P(args) && 
      dfsch_keyword_p(DFSCH_FAST_CAR(args)) &&
      dfsch_compare_keyword(DFSCH_FAST_CAR(args), "no-index")){
    op |= TDBQCNOIDX;
    args = DFSCH_FAST_CDR(args);
  }

  tctdbqryaddcond(q->qry, col_name, op, 
                  dfsch_tokyo_cabinet_build_expression(args));
}

void dfsch_tokyo_cabinet_set_query_order(dfsch_object_t* qo,
                                         char* colname,
                                         int type){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_CABINET_QUERY_TYPE);
  
  tctdbqrysetorder(q->qry, colname, type);
}
int dfsch_tokyo_cabinet_parse_order_type(dfsch_object_t* type){
  if (dfsch_compare_keyword(type, "ascending")) {
    return TDBQOSTRASC;
  }
  if (dfsch_compare_keyword(type, "descending")) {
    return TDBQOSTRDESC;
  }
  if (dfsch_compare_keyword(type, "ascending-value")) {
    return TDBQONUMASC;
  }
  if (dfsch_compare_keyword(type, "descending-value")) {
    return TDBQONUMDESC;
  }
}

void dfsch_tokyo_cabinet_set_query_limit(dfsch_object_t* qo,
                                         int count,
                                         int skip){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_CABINET_QUERY_TYPE);
  
  tctdbqrysetlimit(q->qry, count, skip);
}
dfsch_object_t* dfsch_tokyo_cabinet_query_search(dfsch_object_t* qo){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_CABINET_QUERY_TYPE);
  TCLIST* tcr;
  dfsch_object_t* res;
  
  tcr = tctdbqrysearch(q->qry);
  if (!tcr){
    dfsch_error("Query search failed", q);
  }
  res = dfsch_tokyo_cabinet_list_2_object(tcr);
  tclistdel(tcr);
  return res;
}
