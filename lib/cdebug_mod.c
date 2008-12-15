#include <dfsch/dfsch.h>
#include <dfsch/lib/cdebug.h>

dfsch_object_t* dfsch_module_cdebug_register(dfsch_object_t* env){
  dfsch_define_cstr(env, "cdebug:debugger-procedure", dfsch_cdebug_get_procedure());
}
