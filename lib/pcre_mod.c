#include "dfsch/lib/pcre.h"


DFSCH_DEFINE_PRIMITIVE(compile, 
                       "Compile regular expression into internal "
                       "representation"){
  char* pattern;
  DFSCH_STRING_ARG(args, pattern);
  return dfsch_pcre_compile(pattern, 
                            dfsch_pcre_parse_options(args));
}

DFSCH_DEFINE_PRIMITIVE(match,
                       "Match regular expression against string"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BUFFER_ARG(args, str);

  return dfsch_bool(dfsch_pcre_match(pattern, 
                                     str->ptr, 
                                     str->len, 
                                     dfsch_pcre_parse_options(args)));
}

void dfsch_module_pcre_register(dfsch_object_t* env){
  dfsch_package_t* pcre = dfsch_make_package("pcre");

  dfsch_provide(env, "pcre");
  dfsch_defcanon_pkgcstr(env, pcre, "compile",
                         DFSCH_PRIMITIVE_REF(compile));
  dfsch_defcanon_pkgcstr(env, pcre, "match?",
                         DFSCH_PRIMITIVE_REF(match));
}
