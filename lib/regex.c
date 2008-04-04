/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Regular expressions
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "dfsch/lib/regex.h"

typedef struct dfsch_regex_t {
  dfsch_type_t *type;
  regex_t regex;
  int sub_count;
} dfsch_regex_t;

static dfsch_type_t regex_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_regex_t),
  "regex",
  NULL, // equal?
  NULL, // write
  NULL, // apply
};

static void regex_finalizer(dfsch_regex_t* regex, void* cd){
  regfree(&(regex->regex));
}

char* regex_get_error(int errcode, const regex_t *preg){
  size_t len;
  char* buf;
  len = regerror(errcode, preg, NULL, 0);
  buf = GC_MALLOC_ATOMIC(len);
  regerror(errcode, preg, buf, len);
  return buf;
}

static int get_sub_count(char* expression, int flags){
  int escape;
  int r;
  escape = 0;
  r = 0;
  while (*expression){
    switch (*expression){
    case '\\':
      escape = 1;
      break;
    case '(':
      if ((flags & REG_EXTENDED) == REG_EXTENDED || escape){
        r++;
      }
    default:
      escape = 0;
    }
    expression++;
  }
  return r+1;
}

static void regex_compile(regex_t* regex, char* expression, int flags){
  int err;

  err = regcomp(regex, expression, flags);
  if (err != 0){
    dfsch_error("regex:error", 
                dfsch_make_string_cstr(regex_get_error(err, regex)));
  }
}

dfsch_object_t* dfsch_regex_compile(char* expression, int flags){
  dfsch_regex_t* r = (dfsch_regex_t*)dfsch_make_object(&regex_type);
  
  regex_compile(&(r->regex), expression, flags);

  if ((flags & REG_NOSUB) == REG_NOSUB){
    r->sub_count = 0;
  }else{
    r->sub_count = get_sub_count(expression, flags);
  }
  GC_REGISTER_FINALIZER(r, (GC_finalization_proc)regex_finalizer,
                        NULL, NULL, NULL);


  return (dfsch_object_t*)r;
}

static int regex_match(regex_t* regex, char*string, int flags){
  return (regexec(regex, string, 0, NULL, flags) == 0);
}

int dfsch_regex_match_p(dfsch_object_t* regex, char* string, int flags){
  if (DFSCH_TYPE_OF(regex) != &regex_type)
    dfsch_error("regex:not-a-regex", regex);

  return regex_match(&(((dfsch_regex_t*)regex)->regex), string, flags);
}

static dfsch_object_t* regex_substrings(regex_t* regex, char* string, 
                                        int sub_count, int flags){
  regmatch_t *match;
  int i;
  int count;
  dfsch_object_t* vector;

  match = GC_MALLOC_ATOMIC(sizeof(regmatch_t)*sub_count);

  if (regexec(regex, string, sub_count, match, flags) == REG_NOMATCH){
    GC_FREE(match);
    return NULL;
  }

  count = 0;

  for (i = 0; i < sub_count; i++){
    if (match[i].rm_so == -1)  // No more substring matches
      break;
    count ++;
  }


  vector = dfsch_make_vector(count, NULL);
  
  for (i = 0; i < count; i++){
    dfsch_vector_set(vector, i, 
                     dfsch_vector(3,
                                  dfsch_make_number_from_long(match[i].rm_so),
                                  dfsch_make_number_from_long(match[i].rm_eo),
                                  dfsch_make_string_buf(string+match[i].rm_so,
                                                        match[i].rm_eo-match[i].rm_so)));
  } 

  GC_FREE(match);
    
  return vector;
}

dfsch_object_t* dfsch_regex_substrings(dfsch_object_t* regex, char* string,
                                       int flags){
  dfsch_regex_t* r;
  if (DFSCH_TYPE_OF(regex) != &regex_type)
    dfsch_error("regex:not-a-regex", regex);
  r = (dfsch_regex_t*)regex;
  if (r->sub_count == 0)
    dfsch_error("regex:compiled-with-nosub", regex);

  return regex_substrings(&(r->regex), string, r->sub_count, flags);
}

int dfsch_regex_match_once_p(char* expression, 
                             int cflags, int mflags, 
                             char* string){
  regex_t regex;
  int r;

  regex_compile(&regex, expression, cflags);
  r = regex_match(&regex, string, mflags);
  regfree(&regex);

  return r;
}

dfsch_object_t* dfsch_regex_substrings_once(char* expression, 
                                            int cflags, int mflags, 
                                            char* string){
  regex_t regex;
  dfsch_object_t* r;

  regex_compile(&regex, expression, cflags);
  r = regex_substrings(&regex, string, 
                       get_sub_count(expression, cflags),
                       mflags);

  regfree(&regex);

  return r;
}
