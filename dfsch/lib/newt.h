#ifndef H__dfsch_lib__newt__
#define H__dfsch_lib__newt__

#include <dfsch/dfsch.h>
#include <newt.h>

dfsch_object_t* dfsch_newt_make_component(dfsch_type_t* type,
                                          newtComponent component);
newtComponent dfsch_newt_component(dfsch_type_t* type,
                                   dfsch_object_t* obj);

dfsch_object_t* dfsch_newt_make_grid(newtGrid grid);
newtGrid dfsch_newt_grid(dfsch_object_t* obj);

#endif
