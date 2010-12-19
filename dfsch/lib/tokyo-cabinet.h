#ifndef H__dfsch_ext__tokyocabinet__
#define H__dfsch_ext__tokyocabinet__

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>

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


#endif
