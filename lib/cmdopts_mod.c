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

DFSCH_DEFINE_PRIMITIVE(make_parser, "Create new command line parser"){
  int flags = 0;
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("strict-order", DFSCH_CMDOPTS_STRICT_ORDER, flags);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_cmdopts_make_parser(flags);
}

static void lisp_callback(dfsch_cmdopts_t* parser,
                          dfsch_object_t* function,
                          char* value){
  dfsch_apply(function, dfsch_list(2, parser, dfsch_make_string_cstr(value)));
}

DFSCH_DEFINE_PRIMITIVE(add_option, "Define new command line option"){
  dfsch_object_t* parser;
  dfsch_object_t* has_arg = NULL;
  char short_opt = 0;
  char* long_opt = NULL;
  dfsch_object_t* function;

  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG(args, function);

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("has-argument", has_arg);
  DFSCH_KEYWORD_GENERIC("long-option", long_opt,
                        dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("short-option", short_opt,
                        dfsch_number_to_long);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);
  
  dfsch_cmdopts_add_option(DFSCH_ASSERT_TYPE(parser, 
                                             DFSCH_CMDOPTS_PARSER_TYPE),
                           DFSCH_TRUE_P(has_arg), short_opt, long_opt, 
                           lisp_callback, function);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(add_argument, "Define new command line argument"){
  dfsch_object_t* parser;
  dfsch_object_t* function;
  int flags = 0;

  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG(args, function);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("required", DFSCH_CMDOPTS_ARGUMENT_REQUIRED, flags);
  DFSCH_FLAG_SET("multiple", DFSCH_CMDOPTS_ARGUMENT_MULTIPLE, flags);
  DFSCH_FLAG_PARSER_END(args);
  
  dfsch_cmdopts_add_argument(DFSCH_ASSERT_TYPE(parser, 
                                               DFSCH_CMDOPTS_PARSER_TYPE),
                             flags,
                             lisp_callback, function);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(parse_list, "Parse command line from list"){
  dfsch_object_t* parser;
  dfsch_object_t* list;

  DFSCH_OBJECT_ARG(args, parser);
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);

  dfsch_cmdopts_parse_list(DFSCH_ASSERT_TYPE(parser, 
                                             DFSCH_CMDOPTS_PARSER_TYPE),
                           list);
  return NULL;
}

dfsch_object_t* dfsch_module_cmdopts_register(dfsch_object_t* env){
  dfsch_package_t* cmdopts_pkg = dfsch_make_package("cmdopts",
                                                    "Command line parsing");

  dfsch_provide(env, "cmdopts");

  dfsch_defcanon_pkgcstr(env, cmdopts_pkg, "<parser>", DFSCH_CMDOPTS_PARSER_TYPE);
  dfsch_defcanon_pkgcstr(env, cmdopts_pkg, "<error>", DFSCH_CMDOPTS_ERROR_TYPE);

  dfsch_defcanon_pkgcstr(env, cmdopts_pkg, "make-parser", 
                       DFSCH_PRIMITIVE_REF(make_parser));
  dfsch_defcanon_pkgcstr(env, cmdopts_pkg, "add-option", 
                       DFSCH_PRIMITIVE_REF(add_option));
  dfsch_defcanon_pkgcstr(env, cmdopts_pkg, "add-argument", 
                       DFSCH_PRIMITIVE_REF(add_argument));
  dfsch_defcanon_pkgcstr(env, cmdopts_pkg, "parse-list", 
                       DFSCH_PRIMITIVE_REF(parse_list));
}
