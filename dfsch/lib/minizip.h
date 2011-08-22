#ifndef H__dfsch_lib__minizip__
#define H__dfsch_lib__minizip__

#include <dfsch/dfsch.h>

extern dfsch_type_t dfsch_minizip_type;
#define DFSCH_MINIZIP_TYPE (&dfsch_minizip_type)

dfsch_object_t* dfsch_minizip_open(char* filename);
dfsch_object_t* dfsch_minizip_list_files(dfsch_object_t* mz);

#endif
