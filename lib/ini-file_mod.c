#include <dfsch/lib/ini-file.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(make_empty, 
                       "Create new empty ini-file object"){
  DFSCH_ARG_END(args);

  return dfsch_make_empty_ini_file();
}

DFSCH_DEFINE_PRIMITIVE(read_file, 
                       "Read configuration data from file"){
  char* fname;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_ARG_END(args);

  return dfsch_ini_file_read_file(fname);
}

void dfsch_module_ini_file_register(dfsch_object_t* env){
  dfsch_package_t* ini_file = dfsch_make_package("ini-file",
                                                 "INI-like configuration parser");
  dfsch_provide(env, "ini-file");
  dfsch_defcanon_pkgcstr(env, ini_file, "make-empty",
                         DFSCH_PRIMITIVE_REF(make_empty));
  dfsch_defcanon_pkgcstr(env, ini_file, "read-file",
                         DFSCH_PRIMITIVE_REF(read_file));
}
