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

static dfsch_object_t* db_get_keys_iterator(db_t* db){
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
  if (!tcrdbiterinit(db->adb)){
    dfsch_error("Error in interinit", db);
  }

  while (kres = tcrdbiternext(db->adb, &klen)){
    res = tcrdbget(db->adb, kres, klen, &len);
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

typedef struct table_t {
  dfsch_type_t* type;
  TCRDB* tdb;
} table_t;

static dfsch_object_t* table_ref(table_t* db,
                                 dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);
  TCMAP* res = tcrdbtblget(db->tdb, k->ptr, k->len);
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

  if (!tcrdbtblput(db->tdb, k->ptr, k->len, v)){
    tcmapdel(v);
    dfsch_error("Error writing to database", db);
  }
  tcmapdel(v);
}

static void table_unset(table_t* db,
                        dfsch_object_t* key){
  dfsch_strbuf_t* k = dfsch_string_to_buf(key);

  return tcrdbtblout(db->tdb, k->ptr, k->len);
}

static dfsch_object_t* table_get_keys_iterator(table_t* db){
  int len;
  char* res;
  dfsch_object_t* it = NULL;
  if (!tcrdbiterinit(db->tdb)){
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

dfsch_type_t dfsch_tokyo_tyrant_table_type = {
  .type = DFSCH_STANDARD_TYPE,
  //  .superclass = DFSCH_HASH_BASETYPE,
  .name = "tokyo-tyrant:table",
  .size = sizeof(table_t),
  
  .mapping = &table_mapping,
  .collection = &table_collection,
};

static void table_finalizer(table_t* db, void* discard){
  if (db->type == DFSCH_TOKYO_TYRANT_TABLE_TYPE){
    tcrdbdel(db->tdb);
  }
}


dfsch_object_t* dfsch_tokyo_tyrant_table_open(char* name){
  TCRDB* tdb = tcrdbnew();
  table_t* db = dfsch_make_object(DFSCH_TOKYO_TYRANT_TABLE_TYPE);



  if (!tcrdbopen2(tdb, name)){
    tcrdbdel(tdb);
    dfsch_error("Cannot open database", dfsch_make_string_cstr(name));
  }

  db->tdb = tdb;
  
  return db;
}
void dfsch_tokyo_tyrant_table_close(dfsch_object_t*dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_TYRANT_TABLE_TYPE);
  tcrdbclose(db->tdb);
  tcrdbdel(db->tdb);
  dfsch_invalidate_object(db);
}

dfsch_object_t* dfsch_tokyo_tyrant_table_prefix_search(dfsch_object_t* dbo,
                                                        char* buf, size_t len,
                                                        int limit){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_TYRANT_TABLE_TYPE);
  TCLIST* tcr;
  dfsch_object_t* res;
  
  tcr = tcrdbfwmkeys(db->tdb, buf, len, limit);
  if (!tcr){
    dfsch_error("tctdbfwmkeys returned null", NULL);
  }
  res = dfsch_tokyo_cabinet_list_2_object(tcr);
  tclistdel(tcr);
  return res;
}

void dfsch_tokyo_tyrant_table_set_index(dfsch_object_t* dbo,
                                        char* name,
                                        int type){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_TYRANT_TABLE_TYPE);
  int tcr;

  tcr = tcrdbtblsetindex(db->tdb, name, type);
  if (!tcr && !(type & TDBITKEEP)){
    dfsch_error("Error modifying table index", db);
  }
}

int dfsch_tokyo_tyrant_parse_index_type(dfsch_object_t* args){
  int type = RDBITKEEP;

  DFSCH_FLAG_PARSER_BEGIN_ONE(args, type);
  DFSCH_FLAG_SET("lexical", RDBITLEXICAL, type);
  DFSCH_FLAG_SET("decimal", RDBITDECIMAL, type);
  DFSCH_FLAG_SET("token", RDBITTOKEN, type);
  DFSCH_FLAG_SET("full-text", RDBITTOKEN, type);
  DFSCH_FLAG_SET("q-gram", RDBITQGRAM, type);
  DFSCH_FLAG_PARSER_END(args);

  DFSCH_FLAG_PARSER_BEGIN_ONE_OPT(args, type);
  DFSCH_FLAG_UNSET("rebuild", RDBITKEEP, type);
  DFSCH_FLAG_SET("optimize", RDBITOPT, type);
  DFSCH_FLAG_SET("remove", RDBITVOID, type);
  DFSCH_FLAG_PARSER_END(args);

  return type;
}

typedef struct query_t {
  dfsch_type_t* type;
  RDBQRY* qry;
  table_t* db;
} query_t;

dfsch_type_t dfsch_tokyo_tyrant_query_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "tokyo-tyrant:query",
  .size = sizeof(query_t),
};

static void query_finalizer(query_t* q, void* discard){
  if (q->type == DFSCH_TOKYO_TYRANT_QUERY_TYPE){
    tcrdbqrydel(q->qry);
  }
}

dfsch_object_t* dfsch_tokyo_tyrant_make_query(dfsch_object_t* dbo){
  table_t* db = DFSCH_ASSERT_INSTANCE(dbo, DFSCH_TOKYO_TYRANT_TABLE_TYPE);
  query_t* q = dfsch_make_object(DFSCH_TOKYO_TYRANT_QUERY_TYPE);
  
  q->db = db; /* for GC visibility */
  q->qry = tcrdbqrynew(db->tdb);

  GC_REGISTER_FINALIZER(q, (GC_finalization_proc)query_finalizer,
                        NULL, NULL, NULL);

  return q;
}

void dfsch_tokyo_tyrant_add_query_condition(dfsch_object_t* qo,
                                             char* col_name,
                                             dfsch_object_t* args){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_TYRANT_QUERY_TYPE);
  char* expr;

  int op = 0;

  if (DFSCH_PAIR_P(args) && 
      dfsch_compare_keyword(DFSCH_FAST_CAR(args), "not")){
    op |= RDBQCNEGATE;
    args = DFSCH_FAST_CDR(args);
  }

  DFSCH_FLAG_PARSER_BEGIN_ONE(args, op);
  DFSCH_FLAG_SET("equal", RDBQCSTREQ, op);
  DFSCH_FLAG_SET("includes", RDBQCSTRINC, op);
  DFSCH_FLAG_SET("begins-with", RDBQCSTRBW, op);
  DFSCH_FLAG_SET("ends-with", RDBQCSTREW, op);
  DFSCH_FLAG_SET("includes-all", RDBQCSTRAND, op);
  DFSCH_FLAG_SET("includes-some", RDBQCSTROR, op);
  DFSCH_FLAG_SET("one-of", RDBQCSTROREQ, op);
  DFSCH_FLAG_SET("regex", RDBQCSTRRX, op);

  DFSCH_FLAG_SET("=", RDBQCNUMEQ, op);
  DFSCH_FLAG_SET(">", RDBQCNUMGT, op);
  DFSCH_FLAG_SET(">=", RDBQCNUMGE, op);
  DFSCH_FLAG_SET("<", RDBQCNUMLT, op);
  DFSCH_FLAG_SET("<=", RDBQCNUMLE, op);
  DFSCH_FLAG_SET("between", RDBQCNUMBT, op);
  DFSCH_FLAG_SET("numerically-one-of", RDBQCNUMOREQ, op);
  
  DFSCH_FLAG_SET("ft-phrase", RDBQCFTSPH, op);
  DFSCH_FLAG_SET("ft-all", RDBQCFTSAND, op);
  DFSCH_FLAG_SET("ft-some", RDBQCFTSOR, op);
  DFSCH_FLAG_SET("ft-compound", RDBQCFTSEX, op);
  DFSCH_FLAG_PARSER_END(args);

  if (DFSCH_PAIR_P(args) && 
      dfsch_keyword_p(DFSCH_FAST_CAR(args)) &&
      dfsch_compare_keyword(DFSCH_FAST_CAR(args), "no-index")){
    op |= RDBQCNOIDX;
    args = DFSCH_FAST_CDR(args);
  }

  tcrdbqryaddcond(q->qry, col_name, op, 
                  dfsch_tokyo_cabinet_build_expression(args));
}

void dfsch_tokyo_tyrant_set_query_order(dfsch_object_t* qo,
                                         char* colname,
                                         int type){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_TYRANT_QUERY_TYPE);
  
  tcrdbqrysetorder(q->qry, colname, type);
}
int dfsch_tokyo_tyrant_parse_order_type(dfsch_object_t* type){
  if (dfsch_compare_keyword(type, "ascending")) {
    return RDBQOSTRASC;
  }
  if (dfsch_compare_keyword(type, "descending")) {
    return RDBQOSTRDESC;
  }
  if (dfsch_compare_keyword(type, "ascending-value")) {
    return RDBQONUMASC;
  }
  if (dfsch_compare_keyword(type, "descending-value")) {
    return RDBQONUMDESC;
  }
}

void dfsch_tokyo_tyrant_set_query_limit(dfsch_object_t* qo,
                                         int count,
                                         int skip){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_TYRANT_QUERY_TYPE);
  
  tcrdbqrysetlimit(q->qry, count, skip);
}
dfsch_object_t* dfsch_tokyo_tyrant_query_search(dfsch_object_t* qo){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_TYRANT_QUERY_TYPE);
  TCLIST* tcr;
  dfsch_object_t* res;
  
  tcr = tcrdbqrysearch(q->qry);
  if (!tcr){
    dfsch_error("Query search failed", q);
  }
  res = dfsch_tokyo_tyrant_list_2_object(tcr);
  tclistdel(tcr);
  return res;
}

static dfsch_object_t* tcr_list_2_kvs(table_t* db, TCLIST* list){
  int i;
  int count;
  dfsch_object_t* it = NULL;
  count = tclistnum(list);
  for (i = 0; i < count; i++){
    int len;
    char* res = tclistval(list, count - i - 1, &len);
    TCMAP* rr = tcrdbtblget(db->tdb, res, len);

    if (rr){
      it = dfsch_cons(dfsch_list(2,
                                 dfsch_make_byte_vector(res, len),
                                 dfsch_tokyo_cabinet_map_2_object(rr)),
                      it);
      tcmapdel(rr);
    }
  }    
  return it;
}


dfsch_object_t* dfsch_tokyo_tyrant_query_get_records(dfsch_object_t* qo){
  query_t* q = DFSCH_ASSERT_TYPE(qo, DFSCH_TOKYO_TYRANT_QUERY_TYPE);
  TCLIST* tcr;
  dfsch_object_t* res;
  
  tcr = tcrdbqrysearch(q->qry);
  if (!tcr){
    dfsch_error("Query search failed", q);
  }
  res = tcr_list_2_kvs(q->db, tcr);
  tclistdel(tcr);
  return res;
}
