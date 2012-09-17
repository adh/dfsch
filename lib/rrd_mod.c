#include <rrd.h>
#include <dfsch/dfsch.h>
#include <dfsch/load.h>

void dfsch_module_rrd_register(dfsch_object_t* env){
  dfsch_package_t* rrd = dfsch_make_package("rrd",
                                            "Native library based interface to RRDtool");
  dfsch_provide(env, "rrd");
  
}
