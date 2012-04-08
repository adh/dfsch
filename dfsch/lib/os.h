#ifndef H__dfsch__lib_os__
#define H__dfsch__lib_os__

#include <dfsch/dfsch.h>
#include <sys/stat.h>

dfsch_object_t* dfsch_os_opendir(char* name);
void dfsch_os_closedir(dfsch_object_t* dir_obj);
char* dfsch_os_readdir(dfsch_object_t* dir_obj);
dfsch_object_t* dfsch_os_make_stat_struct();
dfsch_object_t* dfsch_os_cons_stat_struct(struct stat* orig);
struct stat* dfsch_os_get_stat(dfsch_object_t* stat);

#endif
