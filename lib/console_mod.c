#include "dfsch/lib/console.h"
#include <dfsch/dfsch.h>
#include <dfsch/strings.h>

DFSCH_DEFINE_PRIMITIVE(read_line, 0){
  char* prompt;
  DFSCH_STRING_ARG_OPT(args, prompt, "> ");
  DFSCH_ARG_END(args);
  
  return dfsch_make_string_cstr(dfsch_console_read_line(prompt));
}
DFSCH_DEFINE_PRIMITIVE(read_object, 0){
  char* prompt;
  DFSCH_STRING_ARG_OPT(args, prompt, "> ");
  DFSCH_ARG_END(args);
  
  return dfsch_console_read_object(prompt);
}

dfsch_object_t* dfsch_module_console_register(dfsch_object_t* env){
  dfsch_package_t* console = dfsch_make_package("console");
  dfsch_provide(env, "console");

  dfsch_define_pkgcstr(env, console, "read-line", 
                       DFSCH_PRIMITIVE_REF(read_line));
  dfsch_define_pkgcstr(env, console, "read-object", 
                       DFSCH_PRIMITIVE_REF(read_object));
  return env;
}
