/*
 * dfsch - Scheme-like Lisp dialect
 *   Generic functions
 * Copyright (C) 2005-2009 Ales Hakl
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

#ifndef H__dfsch__generic__
#define H__dfsch__generic__

#include <dfsch/dfsch.h>

typedef struct dfsch_method_t {
  dfsch_type_t* type;
  dfsch_object_t* name;
  dfsch_object_t* qualifiers;
  dfsch_object_t* specializers;
  dfsch_object_t* function;
} dfsch_method_t;

typedef struct dfsch_generic_function_t dfsch_generic_function_t;

typedef void (*dfsch_generic_function_add_method_t)(dfsch_object_t* function,
                                                    dfsch_method_t* method);
typedef void (*dfsch_generic_function_remove_method_t)(dfsch_object_t* function,
                                                       dfsch_method_t* method);
typedef dfsch_object_t* 
(*dfsch_generic_function_methods_t)(dfsch_object_t* function);
typedef dfsch_object_t* (*dfsch_call_next_method_t)(dfsch_object_t* context,
                                                    dfsch_object_t* args,
                                                    dfsch_tail_escape_t* esc);

typedef struct dfsch_generic_function_type_t {
  dfsch_type_t super;
  dfsch_generic_function_add_method_t add_method;
  dfsch_generic_function_remove_method_t remove_method;
  dfsch_generic_function_methods_t methods;
} dfsch_generic_function_type_t;

typedef struct dfsch_method_context_type_t {
  dfsch_type_t super;
  dfsch_call_next_method_t call_next_method;
} dfsch_method_context_type_t;

struct dfsch_generic_function_t {
  dfsch_generic_function_type_t* type;
};

typedef struct dfsch_singleton_generic_function_t {
  dfsch_generic_function_type_t* type;
  dfsch_type_apply_t apply;
  dfsch_generic_function_add_method_t add_method;
  dfsch_generic_function_remove_method_t remove_method;
  dfsch_generic_function_methods_t methods;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_singleton_generic_function_t;


extern dfsch_type_t dfsch_generic_function_type_type;
#define DFSCH_GENERIC_FUNCTION_TYPE_TYPE (&dfsch_generic_function_type_type)

extern dfsch_type_t dfsch_generic_function_type;
#define DFSCH_GENERIC_FUNCTION_TYPE (&dfsch_generic_function_type)
extern dfsch_generic_function_type_t dfsch_standard_generic_function_type;
#define DFSCH_STANDARD_GENERIC_FUNCTION_TYPE \
  ((dfsch_type_t*)&dfsch_standard_generic_function_type)
extern dfsch_generic_function_type_t dfsch_singleton_generic_function_type;
#define DFSCH_SINGLETON_GENERIC_FUNCTION_TYPE \
  ((dfsch_type_t*)&dfsch_singleton_generic_function_type)

extern dfsch_type_t dfsch_method_context_type_type;
#define DFSCH_METHOD_CONTEXT_TYPE_TYPE \
  (&dfsch_method_context_type_type)
extern dfsch_type_t dfsch_method_context_type;
#define DFSCH_METHOD_CONTEXT_TYPE \
  ((dfsch_type_t*)&dfsch_method_context_type)
extern dfsch_method_context_type_t dfsch_standard_method_context_type;
#define DFSCH_STANDARD_METHOD_CONTEXT_TYPE \
  ((dfsch_type_t*)&dfsch_standard_method_context_type)

extern dfsch_type_t dfsch_standard_effective_method_type;
#define DFSCH_STANDARD_EFFECTIVE_METHOD_TYPE \
  (&dfsch_standard_effective_method_type)

extern dfsch_type_t dfsch_method_type;
#define DFSCH_METHOD_TYPE (&dfsch_method_type)


dfsch_object_t* dfsch_make_generic_function(dfsch_object_t* name);

void dfsch_generic_function_add_method(dfsch_object_t* function,
                                       dfsch_method_t* method);
void dfsch_generic_function_remove_method(dfsch_object_t* function,
                                          dfsch_method_t* method);
dfsch_object_t* dfsch_generic_function_methods(dfsch_object_t* function);


dfsch_method_t* dfsch_make_method(dfsch_object_t* name,
                                  dfsch_object_t* qualifiers,
                                  dfsch_object_t* specializers,
                                  dfsch_object_t* function);


void dfsch_parse_specialized_lambda_list(dfsch_object_t* s_l_l,
                                         dfsch_object_t* env,
                                         dfsch_object_t** l_l,
                                         dfsch_object_t** spec);

dfsch_generic_function_t* dfsch_assert_generic_function(dfsch_object_t* obj);

dfsch_object_t* dfsch_call_next_method(dfsch_object_t* context,
                                       dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc);

#endif
