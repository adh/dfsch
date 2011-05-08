#ifndef H__dfsch_lib__pcre__
#define H__dfsch_lib__pcre__

#include <dfsch/dfsch.h>
#include <pcre.h>

extern dfsch_type_t dfsch_pcre_pattern_type;
#define DFSCH_PCRE_PATTERN_TYPE (&dfsch_pcre_pattern_type)

pcre* dfsch_pcre_get_pattern(dfsch_object_t* pat);

#define DFSCH_PCRE_PATTERN_ARG(al, name)                        \
  DFSCH_GENERIC_ARG(al, name, pcre*, dfsch_pcre_get_pattern)

int dfsch_pcre_parse_options(dfsch_object_t* al);

dfsch_object_t* dfsch_pcre_compile(char* pattern,
                                   int options);

int dfsch_pcre_match(pcre* pattern,
                     char* string, size_t len,
                     int options);
dfsch_object_t* dfsch_pcre_match_substrings(pcre* pattern,
                                            char* string, size_t len,
                                            int options,
                                            int share_buf);
dfsch_object_t* dfsch_pcre_match_named_substrings(pcre* pattern,
                                                  char* string, size_t len,
                                                  int options,
                                                  int share_buf);
dfsch_object_t* dfsch_pcre_split(pcre* pattern,
                                 char* string, size_t len,
                                 int options,
                                 int share_buf);
dfsch_strbuf_t* dfsch_pcre_replace(pcre* pattern,
                                   char* string, size_t len,
                                   char* template, size_t tlen,
                                   int options);
dfsch_strbuf_t* dfsch_pcre_replace_func(pcre* pattern,
                                        char* string, size_t len,
                                        dfsch_object_t* exp,
                                        int options);

#endif
