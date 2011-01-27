#include <dfsch/dfsch.h>
#include <dfsch/lib/cdebug.h>

dfsch_object_t* dfsch_module_cdebug_register(dfsch_object_t* env){
  dfsch_package_t* cdebug_pkg = dfsch_make_package("cdebug",
                                                   "Console debugger");
  dfsch_defcanon_pkgcstr(env, cdebug_pkg, "debugger-procedure", dfsch_cdebug_get_procedure());
}
