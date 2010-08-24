#include "dfsch-ext/example.h"
#include <dfsch/load.h>

/* Write dfsch interface code here */

void dfsch_module_example_register(dfsch_object_t* env){
  dfsch_provide(env, "example");
}
