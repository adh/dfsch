/*
 * dfsch - Scheme-like Lisp dialect
 *   Command line parsing
 * Copyright (C) 2009 Ales Hakl
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

#include "dfsch/lib/cmdopts.h"
#include <dfsch/util.h>
#include <dfsch/conditions.h>

typedef struct option_t option_t;

struct option_t {
  option_t* next;
  int has_arg;
  char short_opt;
  char* long_opt;
  dfsch_cmdopts_callback_t callback;
  void* baton;
};

typedef struct argument_t argument_t;

struct argument_t {
  argument_t* next;
  int flags;
  dfsch_cmdopts_callback_t callback;
  void* baton;  
};

struct dfsch_cmdopts_t {
  dfsch_type_t* type;
  option_t* options;
  argument_t* arguments;
  int flags;
};

dfsch_type_t dfsch_cmdopts_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_RUNTIME_ERROR_TYPE, "cmdopts:error");

dfsch_type_t dfsch_cmdopts_parser_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_cmdopts_t),
  "cmdopts:parser"
};

dfsch_cmdopts_t* dfsch_cmdopts_make_parser(int flags){
  dfsch_cmdopts_t* p = dfsch_make_object(DFSCH_CMDOPTS_PARSER_TYPE);

  p->arguments = NULL;
  p->options = NULL;
  p->flags = flags;

  return p;
}
void dfsch_cmdopts_add_option(dfsch_cmdopts_t* parser, 
                              int has_arg,
                              char short_opt,
                              char* long_opt,
                              dfsch_cmdopts_callback_t callback,
                              void* baton){
  option_t* o = GC_NEW(option_t);

  o->has_arg = has_arg;
  o->short_opt = short_opt;
  o->long_opt = dfsch_stracpy(long_opt);
  o->callback = callback;
  o->baton = baton;
  o->next = parser->options;
  parser->options = o;
}

void dfsch_cmdopts_add_argument(dfsch_cmdopts_t* parser, 
                                int flags,
                                dfsch_cmdopts_callback_t callback,
                                void* baton){
  argument_t* a = GC_NEW(argument_t);
  argument_t* i = parser->arguments;

  a->flags = flags;
  a->callback = callback;
  a->baton = baton;
  a->next = NULL;
  
  if (!i){
    parser->arguments = a;
  } else {
    while (i->next){
      i = i->next;
    }
    i->next = a;
  }
}

static option_t* find_long_opt(dfsch_cmdopts_t* parser, char* name){
  size_t len = strlen(name);
  option_t* i = parser->options;
  option_t* f = NULL;

  while (i){
    if (i->long_opt && strncmp(i->long_opt, name, len) == 0){
      if (f){
        dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                               "Ambiguous abbreviation",
                               "abbreviation",
                               dfsch_make_string_cstr(name),
                               NULL);
      }
      f = i;
    }
    i = i->next;
  }

  if (!f){
    dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                           "Unknown option",
                           "option",
                           dfsch_make_string_cstr(name),
                           NULL);
  }

  return f;  
}

static option_t* find_short_opt(dfsch_cmdopts_t* parser, char opt){
  option_t* i = parser->options;

  while (i){
    if (i->short_opt == opt)
      return i;
    i = i->next;
  }

  dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                         "Unknown option",
                         "option",
                         DFSCH_MAKE_FIXNUM(opt), // XXX
                         NULL);
}


static char* get_argument(dfsch_cmdopts_source_t source,
                          void* baton){
  char* arg = source(baton);
  if (!arg){
    dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                           "Required argument missing",
                           NULL);
  }
  return arg;
}



static void parse_short_opts(dfsch_cmdopts_t* parser,
                             char* string,
                             dfsch_cmdopts_source_t source,
                             void* baton){
  while (*string){
    option_t* opt = find_short_opt(parser, *string);
    string++;

    if (opt->has_arg){
      if (*string){
        opt->callback(parser, opt->baton, string);
        return;
      } else {
        opt->callback(parser, opt->baton, get_argument(source, baton));
      }
    } else {
      opt->callback(parser, opt->baton, NULL);
    }
  }
}

void dfsch_cmdopts_parse(dfsch_cmdopts_t* parser,
                         dfsch_cmdopts_source_t source,
                         void* baton){
  char* argument;
  argument_t* next_arg = parser->arguments;
  option_t* opt;

  while ((argument = source(baton)) != NULL){
    if (argument[0] == '-'){
      if (argument[1] == '-'){
        if (argument[2] == 0){
          goto arg_only;
        }

        opt = find_long_opt(parser, argument + 2);
        if (opt->has_arg){
          opt->callback(parser, opt->baton, get_argument(source, baton));
        } else {
          opt->callback(parser, opt->baton, NULL);
        }
      } else {
        parse_short_opts(parser, argument + 1, source, baton);
      }
    } else {
      if (!next_arg){
        dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                               "Unexpected argument",
                               NULL);
      }
      next_arg->callback(parser,  next_arg->baton, argument);
      if (!(next_arg->flags & DFSCH_CMDOPTS_ARGUMENT_MULTIPLE)){
        next_arg = next_arg->next;
      }
      if (parser->flags & DFSCH_CMDOPTS_STRICT_ORDER){
        goto arg_only;
      }
    }
  }
  goto out;

 arg_only:
  while ((argument = source(baton)) != NULL){
    if (!next_arg){
      dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                             "Unexpected argument",
                             NULL);
    }
    next_arg->callback(parser,  next_arg->baton, argument);
    if (!(next_arg->flags & DFSCH_CMDOPTS_ARGUMENT_MULTIPLE)){
      next_arg = next_arg->next;
    }
  }

 out:
  if (next_arg && next_arg->flags & DFSCH_CMDOPTS_ARGUMENT_REQUIRED){
    dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                           "Required argument missing",
                           NULL);
  }
}

typedef struct argv_source_t {
  char** argv;
  int argc;
} argv_source_t;

static char* argv_source(argv_source_t* s){
  char* res;
  if (s->argc == 0){
    return NULL;
  }
  s->argc--;
  res = *s->argv;
  s->argv++;
  return res;
}
void dfsch_cmdopts_parse_argv(dfsch_cmdopts_t* parser,
                              char** argv, int argc){
  argv_source_t as;
  as.argv = argv;
  as.argc = argc;
  dfsch_cmdopts_parse(parser, argv_source, &as);
}
static char* list_source(dfsch_object_t** i){
  char* res;
  if (!*i){
    return NULL;
  }
  res = dfsch_string_to_cstr(dfsch_car(*i));
  *i = dfsch_cdr(*i);
  return res;
}
void dfsch_cmdopts_parse_list(dfsch_cmdopts_t* parser,
                              dfsch_object_t* list){
  dfsch_object_t* i = list;
  dfsch_cmdopts_parse(parser, list_source, &i);  
}


dfsch_object_t* dfsch_cmdopts_argv_to_list(int argc, char**argv){
  dfsch_object_t* head;
  dfsch_object_t* tail;
  dfsch_object_t* tmp;

  if (argc == 0){
    return NULL;
  }

  head = tail = dfsch_cons(dfsch_make_string_cstr(*argv), NULL);
  argc--;
  argv++;
  
  while (argc){
    tmp = dfsch_cons(dfsch_make_string_cstr(*argv), NULL);
    DFSCH_FAST_CDR_MUT(tail) = tmp;
    tail = tmp;
    argc--;
    argv++;
  }

  return head;
}

static void string_callback(dfsch_cmdopts_t* parser,
                            char** res,
                            char* value){
  *res = dfsch_stracpy(value);
}

void dfsch_cmdopts_add_string_argument(dfsch_cmdopts_t* parser,
                                       int required,
                                       char** value){
  dfsch_cmdopts_add_argument(parser, 
                             required ? DFSCH_CMDOPTS_ARGUMENT_REQUIRED : 0, 
                             (dfsch_cmdopts_callback_t)string_callback, value);
}
void dfsch_cmdopts_add_string_option(dfsch_cmdopts_t* parser,
                                     char short_opt,
                                     char* long_opt,
                                     char** value){
  dfsch_cmdopts_add_option(parser, 1, short_opt, long_opt, 
                           (dfsch_cmdopts_callback_t)string_callback, value);
}

static void long_callback(dfsch_cmdopts_t* parser,
                          long* res,
                          char* value){
  char* eptr;
  *res = strtol(value, &eptr, 0);
  if (*eptr != '\0' || eptr == value){
    dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                           "Invalid numeric value",
                           "value", dfsch_make_string_cstr(value),
                           NULL);
  }
}

void dfsch_cmdopts_add_long_argument(dfsch_cmdopts_t* parser,
                                     int required,
                                     long* value){
  dfsch_cmdopts_add_argument(parser, 
                             required ? DFSCH_CMDOPTS_ARGUMENT_REQUIRED : 0, 
                             (dfsch_cmdopts_callback_t)long_callback, value);
}
void dfsch_cmdopts_add_long_option(dfsch_cmdopts_t* parser,
                                   char short_opt,
                                   char* long_opt,
                                   long* value){
  dfsch_cmdopts_add_option(parser, 1, short_opt, long_opt, 
                           (dfsch_cmdopts_callback_t)long_callback, value);
}

static void double_callback(dfsch_cmdopts_t* parser,
                            double* res,
                            char* value){
  char* eptr;
  *res = strtod(value, &eptr);
  if (*eptr != '\0' || eptr == value){
    dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                           "Invalid numeric value",
                           "value", dfsch_make_string_cstr(value),
                           NULL);
  }
}

void dfsch_cmdopts_add_double_argument(dfsch_cmdopts_t* parser,
                                     int required,
                                     double* value){
  dfsch_cmdopts_add_argument(parser, 
                             required ? DFSCH_CMDOPTS_ARGUMENT_REQUIRED : 0, 
                             (dfsch_cmdopts_callback_t)double_callback, value);
}
void dfsch_cmdopts_add_double_option(dfsch_cmdopts_t* parser,
                                     char short_opt,
                                     char* long_opt,
                                     double* value){
  dfsch_cmdopts_add_option(parser, 1, short_opt, long_opt, 
                           (dfsch_cmdopts_callback_t)double_callback, value);
}

typedef struct flag_lambda_t {
  int value;
  int* place;
} flag_lambda_t;
static flag_lambda_t* cons_flag_lambda(int value, int* place){
  flag_lambda_t* fl = GC_NEW(flag_lambda_t);
  fl->value = value;
  fl->place = place;
  return fl;
}

static void flag_callback_set(dfsch_cmdopts_t* parser,
                              flag_lambda_t* lambda,
                              char* discard){
  *(lambda->place) = lambda->value;
}
static void flag_callback_increment(dfsch_cmdopts_t* parser,
                                    flag_lambda_t* lambda,
                                    char* discard){
  *(lambda->place) += lambda->value;
}

void dfsch_cmdopts_add_flag_set(dfsch_cmdopts_t* parser,
                                char short_opt,
                                char* long_opt,
                                int value,
                                int* place){
  dfsch_cmdopts_add_option(parser, 0, short_opt, long_opt, 
                           (dfsch_cmdopts_callback_t)flag_callback_set,
                           cons_flag_lambda(value, place));
}
void dfsch_cmdopts_add_flag_increment(dfsch_cmdopts_t* parser,
                                      char short_opt,
                                      char* long_opt,
                                      int value,
                                      int* place){
  dfsch_cmdopts_add_option(parser, 0, short_opt, long_opt, 
                           (dfsch_cmdopts_callback_t)flag_callback_increment,
                           cons_flag_lambda(value, place));
}
