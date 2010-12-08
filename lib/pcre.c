#include "dfch/lib/pcre.h"

typedef struct pattern_t {
  dfsch_type_t* type;
  pcre* pattern;
} pattern_t;

dfsch_type_t dfsch_pcre_pattern_type = {
  .type = DFSCH_STANDARD_TYPE.
  .superclass = NULL,
  .name = "pcre:pattern",
  .size = sizeof(pattern_t),
};


pcre* dfsch_pcre_get_pattern(dfsch_object_t* pat);
int dfsch_pcre_parse_options(dfsch_object_t* al);

dfsch_pcre_pattern_t* dfsch_pcre_compile(char* pattern,
                                         int options);

int dfsch_pcre_match_pattern(pcre* pattern,
                             char* string, size_t len,
                             int options);
dfsch_object_t* dfsch_pcre_match_substrings(pcre* pattern,
                                            char* string, size_t len,
                                            int options);
dfsch_object_t* dfsch_pcre_match_named_substrings(pcre* pattern,
                                                  char* string, size_t len,
                                                  int options);
dfsch_object_t* dfsch_pcre_split(pcre* pattern,
                                 char* string, size_t len,
                                 int options);
dfsch_strbuf_t* dfsch_pcre_replace(pcre* pattern,
                                   char* string, size_t len,
                                   char* template, size_t tlen,
                                   int options);
