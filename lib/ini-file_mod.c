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
DFSCH_DEFINE_PRIMITIVE(get_property,
                       "Return value of property in ini file"){
  dfsch_object_t* ifo;
  char* section;
  char* property;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, section);
  DFSCH_STRING_ARG(args, property);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_ini_file_get(ifo, section, property));
}

DFSCH_DEFINE_PRIMITIVE(has_property_p,
                       "Does file contain given property? (directly, "
                       "not inherited from defaults)"){
  dfsch_object_t* ifo;
  char* section;
  char* property;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, section);
  DFSCH_STRING_ARG(args, property);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_ini_file_has_property_p(ifo, section, property));
}

DFSCH_DEFINE_PRIMITIVE(has_section_p,
                       "Does file contain given section? (directly, "
                       "not inherited from defaults)"){
  dfsch_object_t* ifo;
  char* section;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, section);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_ini_file_has_section_p(ifo, section));
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

DFSCH_DEFINE_PRIMITIVE(set_property,
                       "Change value of property in ini file"){
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

DFSCH_DEFINE_PRIMITIVE(add_comment,
                       "Add new comment line to file"){
  dfsch_object_t* ifo;
  char* section;
  char* comment;
  DFSCH_OBJECT_ARG(args, ifo);
  DFSCH_STRING_ARG(args, section);
  DFSCH_STRING_ARG(args, comment);
  DFSCH_ARG_END(args);

  dfsch_ini_file_add_comment(ifo, section, comment);

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
  dfsch_defcanon_pkgcstr(env, ini_file, "get-property",
                         DFSCH_PRIMITIVE_REF(get_property));
  dfsch_defcanon_pkgcstr(env, ini_file, "set-property!",
                         DFSCH_PRIMITIVE_REF(set_property));
  dfsch_defcanon_pkgcstr(env, ini_file, "write-file!",
                         DFSCH_PRIMITIVE_REF(write_file));

  dfsch_defcanon_pkgcstr(env, ini_file, "has-property?",
                         DFSCH_PRIMITIVE_REF(has_property_p));
  dfsch_defcanon_pkgcstr(env, ini_file, "has-section?",
                         DFSCH_PRIMITIVE_REF(has_section_p));

}
