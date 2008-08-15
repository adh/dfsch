/*
 * dfsch - Scheme-like Lisp dialect
 *   Console input handling
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

#ifndef H__dfsch__console__
#define H__dfsch__console__

#include <dfsch/dfsch.h>
#include <dfsch/parse.h>

char* dfsch_console_read_line(char* prompt);

void dfsch_console_set_object_completion();
void dfsch_console_set_general_completion();

dfsch_object_t* dfsch_console_read_object(char* prompt);

typedef int (*dfsch_console_object_cb_t)(dfsch_object_t* obj, void* baton);

int dfsch_console_read_objects_parser(char* prompt,
                                      dfsch_parser_ctx_t* parser);
int dfsch_console_read_objects_list_parser(char* prompt,
                                           dfsch_parser_ctx_t* parser);

int dfsch_console_read_objects(char* prompt,
                               dfsch_console_object_cb_t cb,
                               void* baton);
int dfsch_console_read_objects_list(char * prompt,
                                    dfsch_console_object_cb_t cb,
                                    void* baton);
int dfsch_console_run_repl(char* prompt,
                           dfsch_object_t* env);

#endif
