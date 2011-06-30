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
DFSCH_DEFINE_PRIMITIVE(get_entry,
                       "Return value of entry in ini file"){
  dfsch_object_t* ifo;
  char* section;
  char* property;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, section);
  DFSCH_STRING_ARG(args, property);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_ini_file_get(ifo, section, property));
}

DFSCH_DEFINE_PRIMITIVE(write_file, 
                       "Write configuration data into file"){
  dfsch_object_t* ifo;
  char* fname;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, fname);
  DFSCH_ARG_END(args);

  dfsch_ini_file_write_file(ifo, fname);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(set_entry,
                       "Change value of entry in ini file"){
  dfsch_object_t* ifo;
  char* section;
  char* property;
  char* value;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, section);
  DFSCH_STRING_ARG(args, property);
  DFSCH_STRING_ARG(args, value);
  DFSCH_ARG_END(args);

  dfsch_ini_file_set(ifo, section, property, value);

  return NULL;
}


void dfsch_module_ini_file_register(dfsch_object_t* env){
  dfsch_package_t* ini_file = dfsch_make_package("ini-file",
                                                 "INI-like configuration parser");
  dfsch_provide(env, "ini-file");
  dfsch_defcanon_pkgcstr(env, ini_file, "make-empty",
                         DFSCH_PRIMITIVE_REF(make_empty));
  dfsch_defcanon_pkgcstr(env, ini_file, "read-file",
                         DFSCH_PRIMITIVE_REF(read_file));
  dfsch_defcanon_pkgcstr(env, ini_file, "get-entry",
                         DFSCH_PRIMITIVE_REF(get_entry));
  dfsch_defcanon_pkgcstr(env, ini_file, "set-entry!",
                         DFSCH_PRIMITIVE_REF(set_entry));
  dfsch_defcanon_pkgcstr(env, ini_file, "write-file!",
                         DFSCH_PRIMITIVE_REF(write_file));
}
