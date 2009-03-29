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
    if (strncmp(i->long_opt, name, len) == 0){
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
    if (opt->has_arg){
      opt->callback(parser, opt->baton, get_argument(source, baton));
    } else {
      opt->callback(parser, opt->baton, NULL);
    }
    string++;
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
    }
  }

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

  if (next_arg && next_arg->flags & DFSCH_CMDOPTS_ARGUMENT_REQUIRED){
    dfsch_signal_condition(DFSCH_CMDOPTS_ERROR_TYPE, 
                           "Required argument missing",
                           NULL);
  }
}


void dfsch_cmdopts_parse_argv(dfsch_cmdopts_t* parser,
                              char** argv, int argc);
void dfsch_cmdopts_parse_vector(dfsch_cmdopts_t* parser,
                                dfsch_object_t* vector);
void dfsch_cmdopts_parse_list(dfsch_cmdopts_t* parser,
                              dfsch_object_t* list);


