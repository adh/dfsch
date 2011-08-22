#include <dfsch/lib/minizip.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(open, "Open zip file for reading"
                       DFSCH_DOC_SYNOPSIS("(filename)")){
  char* filename;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);

  return dfsch_minizip_open(filename);
}

void dfsch_module_minizip_register(dfsch_object_t* env){
  dfsch_package_t* minizip = dfsch_make_package("minizip",
                                                "Simple ZIP file handling");
  dfsch_provide(env, "minizip");
  dfsch_defcanon_pkgcstr(env, minizip, "<minizip>", DFSCH_MINIZIP_TYPE);
  dfsch_defcanon_pkgcstr(env, minizip, "open",
                         DFSCH_PRIMITIVE_REF(open));
}
