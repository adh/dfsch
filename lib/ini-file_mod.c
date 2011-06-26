#include <dfsch/lib/ini-file.h>
#include <dfsch/load.h>

void dfsch_module_ini_file_register(dfsch_object_t* env){
  dfsch_provide(env, "ini-file");
  
}
