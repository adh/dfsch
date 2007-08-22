#ifndef H__dfsch__unix__
#define H__dfsch__unix__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_object_t* dfsch_unix_register(dfsch_object_t* ctx);


  extern dfsch_object_t* dfsch_unix_opendir(char* name);
  extern void dfsch_unix_closedir(dfsch_object_t* dir_obj);
  extern char* dfsch_unix_readdir(dfsch_object_t* dir_obj);
  
  extern dfsch_object_t* dfsch_unix_make_stat_struct();
  extern struct stat* dfsch_unix_get_stat(dfsch_object_t* stat);

#ifdef __cplusplus
}
#endif

#endif
