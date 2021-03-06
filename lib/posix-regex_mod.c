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

#include "dfsch/lib/posix-regex.h"
DFSCH_DEFINE_PRIMITIVE(regex_compile, NULL){
  char* expression;
  int flags = REG_EXTENDED;
  DFSCH_STRING_ARG(args, expression);

  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_UNSET("basic", REG_EXTENDED, flags);
  DFSCH_FLAG_SET("icase", REG_ICASE, flags);
  DFSCH_FLAG_SET("nosub", REG_NOSUB, flags);
  DFSCH_FLAG_SET("newline", REG_NEWLINE, flags);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_regex_compile(expression, flags);
}

DFSCH_DEFINE_PRIMITIVE(regex_match_p, NULL){
  dfsch_object_t* expression;
  char* string;
  int flags = 0;
  DFSCH_OBJECT_ARG(args, expression);
  DFSCH_STRING_ARG(args, string);

  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("notbol", REG_NOTBOL, flags);
  DFSCH_FLAG_SET("noteol", REG_NOTEOL, flags);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_bool(dfsch_regex_match_p(expression, string, flags));
}

DFSCH_DEFINE_PRIMITIVE(regex_substrings, NULL){
  dfsch_object_t* expression;
  char* string;
  int flags = 0;
  DFSCH_OBJECT_ARG(args, expression);
  DFSCH_STRING_ARG(args, string);

  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("notbol", REG_NOTBOL, flags);
  DFSCH_FLAG_SET("noteol", REG_NOTEOL, flags);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_regex_substrings(expression, string, flags);
}

DFSCH_DEFINE_PRIMITIVE(regex_match_once_p, NULL){
  char* expression;
  char* string;
  int mflags = 0;
  int cflags = REG_EXTENDED | REG_NOSUB;
  
  DFSCH_STRING_ARG(args, expression);
  DFSCH_STRING_ARG(args, string);

  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_UNSET("basic", REG_EXTENDED, cflags);
  DFSCH_FLAG_SET("icase", REG_ICASE, cflags);
  DFSCH_FLAG_SET("newline", REG_NEWLINE, cflags);
  DFSCH_FLAG_SET("notbol", REG_NOTBOL, mflags);
  DFSCH_FLAG_SET("noteol", REG_NOTEOL, mflags);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_bool(dfsch_regex_match_once_p(expression, 
                                             cflags, 
                                             mflags, 
                                             string));
}

DFSCH_DEFINE_PRIMITIVE(regex_substrings_once, NULL){
  char* expression;
  char* string;
  int mflags = 0;
  int cflags = REG_EXTENDED;
  
  DFSCH_STRING_ARG(args, expression);
  DFSCH_STRING_ARG(args, string);

  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_UNSET("basic", REG_EXTENDED, cflags);
  DFSCH_FLAG_SET("icase", REG_ICASE, cflags);
  DFSCH_FLAG_SET("newline", REG_NEWLINE, cflags);
  DFSCH_FLAG_SET("notbol", REG_NOTBOL, mflags);
  DFSCH_FLAG_SET("noteol", REG_NOTEOL, mflags);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_regex_substrings_once(expression, cflags, mflags, string);
}



dfsch_object_t* dfsch_module_posix_regex_register(dfsch_object_t *ctx){
  dfsch_package_t* posix_regex = dfsch_make_package("posix-regex",
                                                    "POSIX libc regex(3)");
  dfsch_provide(ctx, "posix-regex");

  dfsch_defcanon_pkgcstr(ctx, posix_regex, "compile", 
                   DFSCH_PRIMITIVE_REF(regex_compile));
  dfsch_defcanon_pkgcstr(ctx, posix_regex, "match?", 
                   DFSCH_PRIMITIVE_REF(regex_match_p));
  dfsch_defcanon_pkgcstr(ctx, posix_regex, "substrings", 
                   DFSCH_PRIMITIVE_REF(regex_substrings));
  dfsch_defcanon_pkgcstr(ctx, posix_regex, "match-once?", 
                   DFSCH_PRIMITIVE_REF(regex_match_once_p));
  dfsch_defcanon_pkgcstr(ctx, posix_regex, "substrings-once", 
                   DFSCH_PRIMITIVE_REF(regex_substrings_once));

}
