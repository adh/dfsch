#include <dfsch/lib/gd.h>
#include <dfsch/load.h>

void dfsch_module_gd_register(dfsch_object_t* env){
  dfsch_package_t* gd = dfsch_make_package("gd",
                                           "GD bitmap graphics library");
  dfsch_provide(env, "libgd");
}
