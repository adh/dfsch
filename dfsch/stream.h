/*
 * dfsch - dfox's quick and dirty scheme implementation 
 *   Event driven parser
 * Copyright (C) 2005 Ales Hakl
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

#ifndef H__dfsch__stream__
#define H__dfsch__stream__

#include "dfsch.h"

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
#define DFSCH_PARSER_NULL 254
#define DFSCH_PARSER_STOPPED 255

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

  /**
   * Get nesting level (i.e. some value proportional to depth of parser
   * stack)
   */
  extern int dfsch_parser_get_level(dfsch_parser_ctx_t *ctx);

  /**
   * Destroy current parser context and start from scratch (useful for 
   * C-c in interactive applications)
   */
  extern void dfsch_parser_reset(dfsch_parser_ctx_t *ctx);
  
  
#ifdef __cplusplus
}
#endif


#endif
