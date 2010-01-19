#ifndef H__dfsch__compile__
#define H__dfsch__compile__

#include <dfsch/dfsch.h>

dfsch_object_t* dfsch_cons_walked(dfsch_object_t* head,
                                  dfsch_object_t* orig_expr,
                                  size_t count,
                                  ...);

#endif
