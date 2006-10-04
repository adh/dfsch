#include "dfsch/lib/regex.h"

typedef struct dfsch_regex_t {
  dfsch_type_t *type;
  regex_t regex;
  int sub_count;
} dfsch_regex_t;

dfsch_type_t regex_type = {
  sizeof(dfsch_regex_t),
  "regex",
  NULL, // equal?
  NULL, // write
  NULL, // apply
};

char* regex_get_error(int errcode, const regex_t *preg){
  size_t len;
  char* buf;
  len = regerror(errcode, preg, NULL, 0);
  buf = GC_MALLOC_ATOMIC(len);
  regerror(errcode, preg, buf, len);
  return buf;
}

dfsch_object_t* dfsch_regex_compile(char* expression, int flags){
  int err;
  int escape;
  dfsch_regex_t* r = (dfsch_regex_t*)dfsch_make_object(&regex_type);

  err = regcomp(&(r->regex), expression, flags);
  if (err != 0){
    dfsch_throw("regex:error", 
                dfsch_make_string_cstr(regex_get_error(err, &(r->regex))));
  }
  
  r->sub_count = 0;
  escape = 0;

  while (*expression){
    switch (*expression){
    case '\\':
      escape = 1;
      break;
    case '(':
      if ((flags & REG_EXTENDED) == REG_EXTENDED || escape){
        r->sub_count++;
      }
    default:
      escape = 0;
    }
    expression++;
  }

  return (dfsch_object_t*)r;
}
int dfsch_regex_match_p(dfsch_object_t* regex, char* string, int flags){
  if (regex->type != &regex_type)
    dfsch_throw("regex:not-a-regex", regex);

  return dfsch_bool(regexec(&(((dfsch_regex_t*) regex)->regex),
                            string,
                            0,
                            NULL,
                            flags) == 0);
}
dfsch_object_t* dfsch_regex_substrings(dfsch_object_t* regex, char* string,
                                       int flags){

}


#define FLAG_PARSER_BEGIN(args) \
  while (dfsch_pair_p((args))){ \
    dfsch_object_t* flag = dfsch_car((args));

#define FLAG_SET(name, value, variable)\
    if (flag == dfsch_make_symbol((name))) (variable) |= (value)    

#define FLAG_UNSET(name, value, variable)\
    if (flag == dfsch_make_symbol((name))) (variable) &= ~(value)    

#define FLAG_PARSER_END(args) \
    (args) = dfsch_cdr((args)); \
  }


static dfsch_object_t* native_regex_compile(void *baton, 
                                            dfsch_object_t* args, 
                                            dfsch_tail_escape_t* esc){
  char* expression;
  int flags = REG_EXTENDED;
  DFSCH_STRING_ARG(args, expression);

  FLAG_PARSER_BEGIN(args);
  FLAG_UNSET("basic", REG_EXTENDED, flags);
  FLAG_SET("icase", REG_ICASE, flags);
  FLAG_SET("nosub", REG_NOSUB, flags);
  FLAG_SET("newline", REG_NEWLINE, flags);
  FLAG_PARSER_END(args);

  return dfsch_regex_compile(expression, flags);
}

static dfsch_object_t* native_regex_match_p(void *baton, 
                                            dfsch_object_t* args, 
                                            dfsch_tail_escape_t* esc){
  dfsch_object_t* expression;
  char* string;
  int flags = REG_EXTENDED;
  DFSCH_OBJECT_ARG(args, expression);
  DFSCH_STRING_ARG(args, string);

  FLAG_PARSER_BEGIN(args);
  FLAG_SET("notbol", REG_NOTBOL, flags);
  FLAG_SET("noteol", REG_NOTEOL, flags);
  FLAG_PARSER_END(args);

  return dfsch_regex_match_p(expression, string, flags);
}



dfsch_object_t* dfsch_regex_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx, "regex:compile", 
                   dfsch_make_primitive(&native_regex_compile,NULL));
  dfsch_ctx_define(ctx, "regex:match?", 
                   dfsch_make_primitive(&native_regex_match_p,NULL));
  return NULL;
}
