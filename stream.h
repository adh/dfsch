/** @file
 * Event driven parser for dfsch.
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
#define DFSCH_PARSER_NULL 5
#define DFSCH_PARSER_STOPPED 6

typedef struct dfsch_parser_ctx_t dfsch_parser_ctx_t; 
typedef int (*dfsch_parser_callback_t)(dfsch_object_t* obj, void* baton); 


extern dfsch_parser_ctx_t* dfsch_parser_create();
extern void dfsch_parser_destroy(dfsch_parser_ctx_t *ctx);

extern void dfsch_parser_callback(dfsch_parser_ctx_t *ctx, 
				  dfsch_parser_callback_t callback,
				  void *baton);

extern int dfsch_parser_feed(dfsch_parser_ctx_t *ctx, char* data);


#ifdef __cplusplus
}
#endif


#endif
