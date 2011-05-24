#include <dfsch/lib/ffi.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(load_library, "Load shared library for use with FFI"){
  char* filename;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);

  return dfsch_ffi_load_library(filename);
}

DFSCH_DEFINE_PRIMITIVE(call, "Call function from shared library"){
  dfsch_object_t* lib;
  char* fun_name;

  DFSCH_OBJECT_ARG(args, lib);
  DFSCH_STRING_ARG(args, fun_name);
  return dfsch_ffi_call(lib, fun_name, args);
}

DFSCH_DEFINE_PRIMITIVE(make_function, "Wrap function from shared library"){
  dfsch_object_t* lib;
  char* fun_name;
  dfsch_object_t* return_type;

  DFSCH_OBJECT_ARG(args, lib);
  DFSCH_STRING_ARG(args, fun_name);
  DFSCH_OBJECT_ARG(args, return_type);
  return dfsch_ffi_make_function(lib, fun_name, return_type, args);
}

void dfsch_module_ffi_register(dfsch_object_t* env){
  dfsch_package_t* ffi = dfsch_make_package("ffi",
                                            "Foreign function interface");
  dfsch_provide(env, "ffi");
  
  dfsch_defcanon_pkgcstr(env, ffi, "<library>",
                         DFSCH_FFI_LIBRARY_TYPE);
  dfsch_defcanon_pkgcstr(env, ffi, "<pointer>",
                         DFSCH_FFI_POINTER_TYPE);
  dfsch_defcanon_pkgcstr(env, ffi, "<function>",
                         DFSCH_FFI_FUNCTION_TYPE);

  dfsch_defcanon_pkgcstr(env, ffi, "load-library",
                         DFSCH_PRIMITIVE_REF(load_library));
  dfsch_defcanon_pkgcstr(env, ffi, "call",
                         DFSCH_PRIMITIVE_REF(call));
  dfsch_defcanon_pkgcstr(env, ffi, "make-function",
                         DFSCH_PRIMITIVE_REF(make_function));
  dfsch_defconst_pkgcstr(env, ffi, "*null*",
                         dfsch_ffi_wrap_pointer(NULL));
}
