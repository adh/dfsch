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

DFSCH_DEFINE_PRIMITIVE(substrings,
                       "Match regular expression against string returning"
                       " captured substrings"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BUFFER_ARG(args, str);

  return dfsch_pcre_match_substrings(pattern, 
                                     str->ptr, 
                                     str->len, 
                                     dfsch_pcre_parse_options(args),
                                     0);
}
DFSCH_DEFINE_PRIMITIVE(byte_vector_substrings,
                       "Match regular expression against byte-vector"
                       " and return captured subvectors that share"
                       " storage"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BYTE_VECTOR_ARG(args, str);

  return dfsch_pcre_match_substrings(pattern, 
                                     str->ptr, 
                                     str->len, 
                                     dfsch_pcre_parse_options(args),
                                     1);
}

DFSCH_DEFINE_PRIMITIVE(named_substrings,
                       "Match regular expression against string returning"
                       " captured substrings as hash"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BUFFER_ARG(args, str);

  return dfsch_pcre_match_named_substrings(pattern, 
                                     str->ptr, 
                                     str->len, 
                                     dfsch_pcre_parse_options(args),
                                     0);
}
DFSCH_DEFINE_PRIMITIVE(byte_vector_named_substrings,
                       "Match regular expression against byte-vector"
                       " and return captured subvectors that share"
                       " storage as hash"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BYTE_VECTOR_ARG(args, str);

  return dfsch_pcre_match_named_substrings(pattern, 
                                     str->ptr, 
                                     str->len, 
                                     dfsch_pcre_parse_options(args),
                                     1);
}

DFSCH_DEFINE_PRIMITIVE(split,
                       "Split string into parts delimited by pattern"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BUFFER_ARG(args, str);

  return dfsch_pcre_split(pattern, 
                          str->ptr, 
                          str->len, 
                          dfsch_pcre_parse_options(args),
                          0);
}
DFSCH_DEFINE_PRIMITIVE(split_byte_vector,
                       "Split string into parts delimited by pattern"){
  pcre* pattern;
  dfsch_strbuf_t* str;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BYTE_VECTOR_ARG(args, str);

  return dfsch_pcre_split(pattern, 
                          str->ptr, 
                          str->len, 
                          dfsch_pcre_parse_options(args),
                          1);
}
DFSCH_DEFINE_PRIMITIVE(replace,
                       "Split string into parts delimited by pattern"){
  pcre* pattern;
  dfsch_strbuf_t* str;
  dfsch_strbuf_t* template;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BUFFER_ARG(args, template);
  DFSCH_BUFFER_ARG(args, str);

  return dfsch_make_string_nocopy(dfsch_pcre_replace(pattern, 
                                                     str->ptr, 
                                                     str->len,
                                                     template->ptr,
                                                     template->len,
                                                     dfsch_pcre_parse_options(args)));
}
DFSCH_DEFINE_PRIMITIVE(replace_byte_vector,
                       "Split string into parts delimited by pattern"){
  pcre* pattern;
  dfsch_strbuf_t* str;
  dfsch_strbuf_t* template;

  DFSCH_PCRE_PATTERN_ARG(args, pattern);
  DFSCH_BUFFER_ARG(args, template);
  DFSCH_BUFFER_ARG(args, str);

  return dfsch_make_byte_vector_strbuf(dfsch_pcre_replace(pattern, 
                                                          str->ptr, 
                                                          str->len,
                                                          template->ptr,
                                                          template->len,
                                                          dfsch_pcre_parse_options(args)));
}


void dfsch_module_pcre_register(dfsch_object_t* env){
  dfsch_package_t* pcre = dfsch_make_package("pcre",
                                             "Perl-compatible regular "
                                             "expressions");

  dfsch_provide(env, "pcre");
  dfsch_defcanon_pkgcstr(env, pcre, "compile",
                         DFSCH_PRIMITIVE_REF(compile));
  dfsch_defcanon_pkgcstr(env, pcre, "match?",
                         DFSCH_PRIMITIVE_REF(match));
  dfsch_defcanon_pkgcstr(env, pcre, "substrings",
                         DFSCH_PRIMITIVE_REF(substrings));
  dfsch_defcanon_pkgcstr(env, pcre, "byte-vector-substrings",
                         DFSCH_PRIMITIVE_REF(substrings));
  dfsch_defcanon_pkgcstr(env, pcre, "named-substrings",
                         DFSCH_PRIMITIVE_REF(named_substrings));
  dfsch_defcanon_pkgcstr(env, pcre, "byte-vector-named-substrings",
                         DFSCH_PRIMITIVE_REF(named_substrings));
  dfsch_defcanon_pkgcstr(env, pcre, "split",
                         DFSCH_PRIMITIVE_REF(split));
  dfsch_defcanon_pkgcstr(env, pcre, "split-byte-vector",
                         DFSCH_PRIMITIVE_REF(split_byte_vector));
  dfsch_defcanon_pkgcstr(env, pcre, "replace",
                         DFSCH_PRIMITIVE_REF(replace));
  dfsch_defcanon_pkgcstr(env, pcre, "replace-byte-vector",
                         DFSCH_PRIMITIVE_REF(replace_byte_vector));
}
