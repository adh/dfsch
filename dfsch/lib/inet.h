#ifndef H__dfsch_lib__inet__
#define H__dfsch_lib__inet__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_module_inet_register(dfsch_object_t* env);

dfsch_object_t* dfsch_http_split_path(char* path);


dfsch_object_t* dfsch_http_query_2_hash(char* query);
dfsch_object_t* dfsch_http_query_2_alist(char* query);

#endif
