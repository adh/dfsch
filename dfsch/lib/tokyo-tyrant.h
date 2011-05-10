#ifndef H__dfsch_ext__tokyotyrant__
#define H__dfsch_ext__tokyotyrant__

#include <dfsch/dfsch.h>
#include <dfsch/hash.h>
#include <dfsch/lib/tokyo-cabinet.h>

extern dfsch_type_t dfsch_tokyo_tyrant_db_type;
#define DFSCH_TOKYO_TYRANT_DB_TYPE (&dfsch_tokyo_tyrant_db_type)

extern dfsch_type_t dfsch_tokyo_tyrant_table_type;
#define DFSCH_TOKYO_TYRANT_TABLE_TYPE (&dfsch_tokyo_tyrant_table_type)

dfsch_object_t* dfsch_tokyo_tyrant_db_open(char* name);
void dfsch_tokyo_tyrant_db_close(dfsch_object_t*db);
dfsch_object_t* dfsch_tokyo_tyrant_prefix_search(dfsch_object_t* dbo,
                                                  char* buf, size_t len,
                                                  int limit);
void dfsch_tokyo_tyrant_begin_transaction(dfsch_object_t* dbo);
void dfsch_tokyo_tyrant_commit_transaction(dfsch_object_t* dbo);
void dfsch_tokyo_tyrant_abort_transaction(dfsch_object_t* dbo);


#endif
