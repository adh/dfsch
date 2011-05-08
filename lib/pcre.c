#include "dfsch/lib/pcre.h"
#include <dfsch/util.h>
#include <dfsch/hash.h>

#ifndef PCRE_UCP
#define PCRE_UCP 0
#endif

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
                           dfsch_string_p(pat) ? PCRE_UTF8 | PCRE_UCP : 0);
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

  backref++;
  vs = backref * 3;
  int vec[vs];

  return match_res(pcre_exec(pattern, NULL, 
                             string, len, 0, 
                             options, vec, vs));
}

static dfsch_object_t* make_string(char* ptr, size_t len,
                                   int comp_options, 
                                   int share_buf){
  if (share_buf){
    return dfsch_make_byte_vector_nocopy(ptr, len);      
  } else if (comp_options & PCRE_UTF8){
    return dfsch_make_string_buf(ptr, len);
  } else {
    return dfsch_make_byte_vector(ptr, len);
  }

}

static dfsch_object_t* make_substring(char* string,
                                      int* vec, int i,
                                      int comp_options, 
                                      int share_buf){
  if (vec[i * 2] < 0){
    return NULL;
  }

  return make_string(string + vec[i * 2],
                     vec[i * 2 + 1] - vec[i * 2],
                     comp_options,
                     share_buf);
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
    dfsch_vector_set(res, i, 
                     make_substring(string,
                                    vec, i,
                                    comp_options, 
                                    share_buf));
  }
  
  return res;
}
dfsch_object_t* dfsch_pcre_match_named_substrings(pcre* pattern,
                                                  char* string, size_t len,
                                                  int options,
                                                  int share_buf){
  size_t vs;
  int ssl;
  int comp_options;
  int i;
  int j;
  dfsch_object_t* res;
  int count;
  
  int namecount;
  int namesize;
  char* nametable;

  pcre_fullinfo(pattern, NULL, PCRE_INFO_CAPTURECOUNT, &ssl);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_OPTIONS, &comp_options);

  pcre_fullinfo(pattern, NULL, PCRE_INFO_NAMECOUNT, &namecount);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_NAMEENTRYSIZE, &namesize);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_NAMETABLE, &nametable);

  ssl++;
  vs = ssl * 3;
  int vec[vs];

  count = pcre_exec(pattern, NULL, 
                    string, len, 0, 
                    options, vec, vs);

  if (match_res(count) == 0){
    return NULL;
  }

  res = dfsch_hash_make(DFSCH_HASH_EQUAL);
  dfsch_hash_set(res,
                 NULL,
                 make_substring(string,
                                vec, 0,
                                comp_options, 
                                share_buf));

  for (j = 0; j < namecount; j++){
    int i;
    i = nametable[j * namesize + 0] << 8 | nametable[j * namesize + 1];

    if (vec[i * 2] < 0){
      continue;
    }

    dfsch_hash_set(res,
                   dfsch_make_string_cstr(nametable + j * namesize + 2),
                   make_substring(string,
                                    vec, i,
                                    comp_options, 
                                    share_buf));
  }
  
  return res;
}
dfsch_object_t* dfsch_pcre_split(pcre* pattern,
                                 char* string, size_t len,
                                 int options,
                                 int share_buf){
  int res;
  size_t vs;
  int backref;
  int off = 0;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  int comp_options;

  pcre_fullinfo(pattern, NULL, PCRE_INFO_OPTIONS, &comp_options);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_BACKREFMAX, &backref);

  backref++;
  vs = backref * 3;
  int vec[vs];

  while (match_res(pcre_exec(pattern, NULL, 
                             string, len, off, 
                             options, vec, vs))){
    dfsch_list_collect(lc, make_string(string + off,
                                       vec[0] - off,
                                       comp_options,
                                       share_buf));
    off = vec[1];
  }

  dfsch_list_collect(lc, make_string(string + off,
                                     len - off,
                                     comp_options,
                                     share_buf));

  return dfsch_collected_list(lc);
}

static void expand_replacement(dfsch_str_list_t* sl,
                               char* template, size_t tlen,
                               char* buf, int count, int *vec){
  char* pos;
  while (pos = memchr(template, '\\', tlen)){
    dfsch_sl_nappend(sl, template, pos - template);
    template = pos + 1;
    tlen -= (pos - template) + 2;
    if (*template == '\\'){
      dfsch_sl_append(sl, "\\");
      template++;
      tlen--;
    } else {
      int idx;
      
      if (*template < '0' || *template > '9'){
        dfsch_error("Invalid reference in template", 
                    DFSCH_MAKE_FIXNUM(*template));
      }

      idx = *template - '0';
      template++;
      tlen--;
      if (idx >= count){
        dfsch_error("Invalid reference index in template", 
                    DFSCH_MAKE_FIXNUM(idx));
      }
      
      dfsch_sl_nappend(sl, buf + vec[idx*2],
                       vec[idx*2 + 1] - vec[idx*2]);
    }
    
  }
  dfsch_sl_nappend(sl, template, tlen);
}

dfsch_strbuf_t* dfsch_pcre_replace(pcre* pattern,
                                   char* string, size_t len,
                                   char* template, size_t tlen,
                                   int options){
  int res;
  size_t vs;
  int ssl;
  int off = 0;
  dfsch_str_list_t* sl = dfsch_sl_create();
  int comp_options;
  int count;

  pcre_fullinfo(pattern, NULL, PCRE_INFO_OPTIONS, &comp_options);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_CAPTURECOUNT, &ssl);

  ssl++;
  vs = ssl * 3;
  int vec[vs];

  while (count = match_res(pcre_exec(pattern, NULL, 
                                     string, len, off, 
                                     options, vec, vs))){
    dfsch_sl_nappend(sl, string + off, vec[0] - off);
    expand_replacement(sl, template, tlen, string, count + 1, vec);
    off = vec[1];
  }

  dfsch_sl_nappend(sl, string + off, len - off);

  return dfsch_sl_value_strbuf(sl); 
}

dfsch_strbuf_t* dfsch_pcre_replace_func(pcre* pattern,
                                        char* string, size_t len,
                                        dfsch_object_t* exp,
                                        int options){
  int res;
  size_t vs;
  int ssl;
  int off = 0;
  dfsch_str_list_t* sl = dfsch_sl_create();
  int comp_options;
  int count;
  dfsch_strbuf_t* repl;
  int i;
  dfsch_object_t* v;

  pcre_fullinfo(pattern, NULL, PCRE_INFO_OPTIONS, &comp_options);
  pcre_fullinfo(pattern, NULL, PCRE_INFO_CAPTURECOUNT, &ssl);

  ssl++;
  vs = ssl * 3;
  int vec[vs];

  while (count = match_res(pcre_exec(pattern, NULL, 
                                     string, len, off, 
                                     options, vec, vs))){
    dfsch_sl_nappend(sl, string + off, vec[0] - off);

    v = dfsch_make_vector(count, NULL);
    for (i = 0; i < count; i++){
      dfsch_vector_set(v, i, 
                       make_substring(string,
                                      v, i,
                                      comp_options, 
                                      0));
    }

    repl = dfsch_string_to_buf(dfsch_apply(exp, dfsch_list(1, vec)));

    dfsch_sl_nappend(sl, repl->ptr, repl->len);
    off = vec[1];
  }

  dfsch_sl_nappend(sl, string + off, len - off);

  return dfsch_sl_value_strbuf(sl); 
}
