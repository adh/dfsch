#include <dfsch/dfsch.h>
#include <dfsch/lib/cinspect.h>

dfsch_object_t* dfsch_module_cinspect_register(dfsch_object_t* env){
  dfsch_package_t* cinspect_pkg = dfsch_make_package("cinspect",
                                                     "Console inspector");
  dfsch_defcanon_pkgcstr(env, cinspect_pkg, "debugger-procedure", dfsch_cinspect_get_procedure());
}
