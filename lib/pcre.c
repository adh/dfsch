#include "dfsch/lib/pcre.h"

typedef struct pattern_t {
  dfsch_type_t* type;
  char* pattern;
  void* data[];
} pattern_t;

dfsch_type_t dfsch_pcre_pattern_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "pcre:pattern",
  .size = sizeof(pattern_t),
};

pcre* dfsch_pcre_get_pattern(dfsch_object_t* pat){
  pattern_t* p;

  if (DFSCH_INSTANCE_P(pat, DFSCH_PROTO_STRING_TYPE)){
    p = dfsch_pcre_compile(dfsch_proto_string_to_cstr(pat), 
                           dfsch_string_p(pat) ? PCRE_UTF8 : 0);
  } else {
    p = DFSCH_ASSERT_TYPE(pat, DFSCH_PCRE_PATTERN_TYPE);
  }

  return (pcre*)p->data;
}
int dfsch_pcre_parse_options(dfsch_object_t* al){
  return 0;
}


dfsch_object_t* dfsch_pcre_compile(char* pattern,
                                   int options){
  char* errmsg;
  int erroffset;
  pcre* code = pcre_compile(pattern, options, &errmsg, &erroffset, NULL);
  pattern_t* res;
  size_t len;

  if (!code){
    dfsch_error(errmsg, DFSCH_MAKE_FIXNUM(erroffset));
  }

  pcre_fullinfo(code, NULL, PCRE_INFO_SIZE, &len);

  res = dfsch_make_object_var(DFSCH_PCRE_PATTERN_TYPE, len);
  memcpy(res->data, code, len);
  res->pattern = dfsch_stracpy(pattern);
  pcre_free(code);

  return res;
}

static match_res(int res){
  if (res < 0){
    switch (res){
    case PCRE_ERROR_NOMATCH:
      return 0;
    default:
      dfsch_error("pcre_exec error", DFSCH_MAKE_FIXNUM(res));
    }
  } else {
    return 1;
  }
}

int dfsch_pcre_match(pcre* pattern,
                     char* string, size_t len,
                     int options){
  int res;
  size_t vs;
  int backref;

  pcre_fullinfo(pattern, NULL, PCRE_INFO_BACKREFMAX, &backref);

  vs = backref * 3;
  int vec[vs];

  return match_res(pcre_exec(pattern, NULL, 
                             string, len, 0, 
                             options, vec, vs));
}
dfsch_object_t* dfsch_pcre_match_substrings(pcre* pattern,
                                            char* string, size_t len,
                                            int options,
                                            int share_buf){
  size_t vs;
  int ssl;
  int comp_options;
  int i;
  dfsch_object_t* res;
  int count;

  pcre_fullinfo(pattern, NULL, PCRE_INFO_CAPTURECOUNT, &ssl);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_OPTIONS, &comp_options);

  ssl++;
  vs = ssl * 3;
  int vec[vs];

  count = pcre_exec(pattern, NULL, 
                    string, len, 0, 
                    options, vec, vs);

  if (match_res(count) == 0){
    return NULL;
  }

  res = dfsch_make_vector(count, NULL);
  for (i = 0; i < count; i++){
    dfsch_object_t* ss;

    if (vec[i * 2] < 0){
      continue;
    }

    if (share_buf){
      ss = dfsch_make_byte_vector_nocopy(string + vec[i * 2],
                                         vec[i * 2 + 1] - vec[i * 2]);      
    } else if (comp_options & PCRE_UTF8){
      ss = dfsch_make_string_buf(string + vec[i * 2],
                                 vec[i * 2 + 1] - vec[i * 2]);
    } else {
      ss = dfsch_make_byte_vector(string + vec[i * 2],
                                  vec[i * 2 + 1] - vec[i * 2]);
    }

    dfsch_vector_set(res, i, ss);
  }
  
  return res;
}
dfsch_object_t* dfsch_pcre_match_named_substrings(pcre* pattern,
                                                  char* string, size_t len,
                                                  int options,
                                                  int share_buf){

}
dfsch_object_t* dfsch_pcre_split(pcre* pattern,
                                 char* string, size_t len,
                                 int options,
                                 int share_buf){

}
dfsch_strbuf_t* dfsch_pcre_replace(pcre* pattern,
                                   char* string, size_t len,
                                   char* template, size_t tlen,
                                   int options){

}
