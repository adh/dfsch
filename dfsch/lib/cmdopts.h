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

#ifndef H__dfsch__cmdopts__
#define H__dfsch__cmdopts__

#include <dfsch/dfsch.h>

typedef struct dfsch_cmdopts_t dfsch_cmdopts_t;

typedef void (*dfsch_cmdopts_callback_t)(dfsch_cmdopts_t* parser,
                                         void* baton,
                                         char* value);

extern dfsch_type_t dfsch_cmdopts_error_type;
#define DFSCH_CMDOPTS_ERROR_TYPE (&dfsch_cmdopts_error_type)
extern dfsch_type_t dfsch_cmdopts_parser_type;
#define DFSCH_CMDOPTS_PARSER_TYPE (&dfsch_cmdopts_parser_type)

#define DFSCH_CMDOPTS_STRICT_ORDER 1

dfsch_cmdopts_t* dfsch_cmdopts_make_parser(int flags);
void dfsch_cmdopts_add_option(dfsch_cmdopts_t* parser, 
                              int has_arg,
                              char short_opt,
                              char* long_opt,
                              dfsch_cmdopts_callback_t callback,
                              void* baton);

#define DFSCH_CMDOPTS_ARGUMENT_REQUIRED 1
#define DFSCH_CMDOPTS_ARGUMENT_MULTIPLE 2

void dfsch_cmdopts_add_argument(dfsch_cmdopts_t* parser, 
                                int flags,
                                dfsch_cmdopts_callback_t callback,
                                void* baton);

typedef char* (*dfsch_cmdopts_source_t)(void* baton);

void dfsch_cmdopts_parse(dfsch_cmdopts_t* parser,
                         dfsch_cmdopts_source_t source,
                         void* baton);

void dfsch_cmdopts_parse_argv(dfsch_cmdopts_t* parser,
                              char** argv, int argc);
void dfsch_cmdopts_parse_list(dfsch_cmdopts_t* parser,
                              dfsch_object_t* list);

dfsch_object_t* dfsch_cmdopts_argv_to_list(int argc, char**argv);

#endif
