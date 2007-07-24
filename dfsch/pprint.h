#ifndef H__dfsch__pprint__
#define H__dfsch__pprint__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern char* dfsch_pprint(dfsch_object_t* object, 
                             int margin_l, int margin_r,
                             int depth);

#ifdef __cplusplus
}
#endif

#endif
