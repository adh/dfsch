/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Number manipulation routines.
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

/** \file dfsch/number.h
 *
 * Number datatype.
 */

#ifndef H__dfsch__number__
#define H__dfsch__number__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  /** Makes number object from given floating-point number. */
  extern dfsch_object_t* dfsch_make_number_from_double(double n);
  /** Makes number object from given integer number. */
  extern dfsch_object_t* dfsch_make_number_from_long(long n);
  /** Returns value of given number as double. */
  extern double dfsch_number_to_double(dfsch_object_t *n);
  /** Returns value of given number as long. */
  extern long dfsch_number_to_long(dfsch_object_t *n);

  /** Creates number from external representation. */
  extern dfsch_object_t* dfsch_make_number_from_string(char* str);

  /** Add arguments. */
  extern dfsch_object_t* dfsch_number_add(dfsch_object_t* a, 
                                          dfsch_object_t* b);
  /** Substract arguments. */
  extern dfsch_object_t* dfsch_number_sub(dfsch_object_t* a, 
                                          dfsch_object_t* b);
  /** Multiply arguments */
  extern dfsch_object_t* dfsch_number_mul(dfsch_object_t* a, 
                                          dfsch_object_t* b);
  /** Divide arguments */
  extern dfsch_object_t* dfsch_number_div(dfsch_object_t* a, 
                                          dfsch_object_t* b);
  /** Integer division */
  extern dfsch_object_t* dfsch_number_div_i(dfsch_object_t* a, 
                                            dfsch_object_t* b);
  /** Modulo */
  extern dfsch_object_t* dfsch_number_mod(dfsch_object_t* a, 
                                          dfsch_object_t* b);

  /** Less than operator */
  extern int dfsch_number_lt(dfsch_object_t* a, dfsch_object_t* b);
  /** Greater than operator */
  extern int dfsch_number_gt(dfsch_object_t* a, dfsch_object_t* b);
  /** Less than or equal operator */
  extern int dfsch_number_lte(dfsch_object_t* a, dfsch_object_t* b);
  /** Greater than or equal operator */
  extern int dfsch_number_gte(dfsch_object_t* a, dfsch_object_t* b);


#ifdef __cplusplus
}
#endif

#define DFSCH_DOUBLE_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, double, dfsch_number_to_double)
#define DFSCH_DOUBLE_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, double, dfsch_number_to_double)

#define DFSCH_LONG_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, long, dfsch_number_to_long)
#define DFSCH_LONG_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, long, dfsch_number_to_long)



#endif
