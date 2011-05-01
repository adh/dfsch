#ifndef H__dfsch_ext__tokyocabinet__
#define H__dfsch_ext__tokyocabinet__

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>
#include <tcutil.h>
#include <tctdb.h>

extern dfsch_type_t dfsch_tokyo_cabinet_db_type;
#define DFSCH_TOKYO_CABINET_DB_TYPE (&dfsch_tokyo_cabinet_db_type)

dfsch_object_t* dfsch_tokyo_cabinet_db_open(char* name);
void dfsch_tokyo_cabinet_db_close(dfsch_object_t*db);
dfsch_object_t* dfsch_tokyo_cabinet_prefix_search(dfsch_object_t* dbo,
                                                  char* buf, size_t len,
                                                  int limit);
void dfsch_tokyo_cabinet_begin_transaction(dfsch_object_t* dbo);
void dfsch_tokyo_cabinet_commit_transaction(dfsch_object_t* dbo);
void dfsch_tokyo_cabinet_abort_transaction(dfsch_object_t* dbo);

void dfscgh_tokyo_cabinet_db_sync(dfsch_object_t* dbo);

extern dfsch_type_t dfsch_tokyo_cabinet_table_type;
#define DFSCH_TOKYO_CABINET_TABLE_TYPE (&dfsch_tokyo_cabinet_table_type)

dfsch_object_t* dfsch_tokyo_cabinet_table_open(char* name);
void dfsch_tokyo_cabinet_table_close(dfsch_object_t*db);
dfsch_object_t* dfsch_tokyo_cabinet_table_prefix_search(dfsch_object_t* dbo,
                                                        char* buf, size_t len,
                                                        int limit);
void dfsch_tokyo_cabinet_table_begin_transaction(dfsch_object_t* dbo);
void dfsch_tokyo_cabinet_table_commit_transaction(dfsch_object_t* dbo);
void dfsch_tokyo_cabinet_table_abort_transaction(dfsch_object_t* dbo);

void dfscgh_tokyo_cabinet_table_sync(dfsch_object_t* dbo);

dfsch_object_t* dfsch_tokyo_cabinet_list_2_object(TCLIST* list);
TCMAP* dfsch_tokyo_cabinet_object_2_map(dfsch_object_t* obj);
dfsch_object_t* dfsch_tokyo_cabinet_map_2_object(TCMAP* map);

void dfsch_tokyo_cabinet_table_set_index(dfsch_object_t* dbo,
                                        char* name,
                                        int type);
int dfsch_tokyo_cabinet_parse_index_type(dfsch_object_t* args);


extern dfsch_type_t dfsch_tokyo_cabinet_query_type;
#define DFSCH_TOKYO_CABINET_QUERY_TYPE (&dfsch_tokyo_cabinet_query_type)

dfsch_object_t* dfsch_tokyo_cabinet_make_query(dfsch_object_t* dbo);

void dfsch_tokyo_cabinet_add_query_condition(dfsch_object_t* qo,
                                             char* col_name,
                                             dfsch_object_t* args);
void dfsch_tokyo_cabinet_set_query_order(dfsch_object_t* qo,
                                         char* colname,
                                         int type);
int dfsch_tokyo_cabinet_parse_order_type(dfsch_object_t* type);
void dfsch_tokyo_cabinet_set_query_limit(dfsch_object_t* qo,
                                         int count,
                                         int skip);

#endif
