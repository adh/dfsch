/*
 * dfsch - dfox's quick and dirty scheme implementation 
 *   Event driven parser
 * Copyright (C) 2005-2010 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

/** \file dfsch/parse.h
 *
 * Stream based S-expression parser.
 */

#ifndef H__dfsch__parse__
#define H__dfsch__parse__

#include <dfsch/dfsch.h>
#include <dfsch/ports.h>

#ifdef __cplusplus
extern "C" {
#endif
  
  
#define DFSCH_PARSER_NOERROR 0
#define DFSCH_PARSER_UNEXPECTED_CLOSE 1
#define DFSCH_PARSER_UNEXPECTED_DOT 2
#define DFSCH_PARSER_UNEXPECTED_OBJECT 3
#define DFSCH_PARSER_CAR_EXPECTED 4
#define DFSCH_PARSER_LIST_EXPECTED 5
#define DFSCH_PARSER_INVALID_ESCAPE 6
#define DFSCH_PARSER_INVALID_NUMBER 7
#define DFSCH_PARSER_UNREADABLE 8
#define DFSCH_PARSER_NULL 254
#define DFSCH_PARSER_STOPPED 255

extern dfsch_type_t dfsch_parse_error_type;
#define DFSCH_PARSE_ERROR_TYPE (&dfsch_parse_error_type)


  typedef struct dfsch_parser_ctx_t dfsch_parser_ctx_t; 
  typedef int (*dfsch_parser_callback_t)(dfsch_object_t* obj, void* baton); 
  

  /**
   * Creates new parser instance
   */
  extern dfsch_parser_ctx_t* dfsch_parser_create();
  
  /**
   * Sets callback for complete objects parsed.
   */
  extern void dfsch_parser_callback(dfsch_parser_ctx_t *ctx, 
				    dfsch_parser_callback_t callback,
				    void *baton);
  
  /**
   * Feed some data into parser.
   */
  extern int dfsch_parser_feed(dfsch_parser_ctx_t *ctx, char* data);
  extern int dfsch_parser_feed_line(dfsch_parser_ctx_t* ctx, char* data);

  /**
   * Get nesting level (i.e. some value proportional to depth of parser
   * stack)
   */
  extern int dfsch_parser_get_level(dfsch_parser_ctx_t *ctx);

  /**
   * Return true if parser is in default state.
   */
  extern int dfsch_parser_top_level(dfsch_parser_ctx_t *ctx);

  /**
   * Destroy current parser context and start from scratch (useful for 
   * C-c in interactive applications)
   */
  extern void dfsch_parser_reset(dfsch_parser_ctx_t *ctx);
  
  /** 
   * Read one object from port.
   */
  extern dfsch_object_t* dfsch_parser_read_from_port(dfsch_object_t* port);
  
  extern void dfsch_parser_eval_env(dfsch_parser_ctx_t *ctx, 
                                    dfsch_object_t* env);

  extern void dfsch_parser_set_source(dfsch_parser_ctx_t* ctx,
                                      dfsch_object_t* source);
#ifdef __cplusplus
}
#endif


#endif
