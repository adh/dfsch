/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Number manipulation routines.
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

  typedef dfsch_type_t dfsch_number_type_t;

  extern dfsch_type_t dfsch_number_type;
#define DFSCH_NUMBER_TYPE (&dfsch_number_type)
  extern dfsch_type_t dfsch_real_type;
#define DFSCH_REAL_TYPE (&dfsch_real_type)
  extern dfsch_type_t dfsch_rational_type;
#define DFSCH_RATIONAL_TYPE (&dfsch_rational_type)
  extern dfsch_type_t dfsch_integer_type;
#define DFSCH_INTEGER_TYPE (&dfsch_integer_type)

  extern dfsch_number_type_t dfsch_fixnum_type;
#define DFSCH_FIXNUM_TYPE ((dfsch_type_t*)&dfsch_fixnum_type)
  extern dfsch_number_type_t dfsch_flonum_type;
#define DFSCH_FLONUM_TYPE ((dfsch_type_t*)&dfsch_flonum_type)
  extern dfsch_number_type_t dfsch_bignum_type;
#define DFSCH_BIGNUM_TYPE ((dfsch_type_t*)&dfsch_bignum_type)
  extern dfsch_number_type_t dfsch_fracnum_type;
#define DFSCH_FRACNUM_TYPE ((dfsch_type_t*)&dfsch_fracnum_type)

  int dfsch_number_p(dfsch_object_t* obj);
  int dfsch_real_p(dfsch_object_t* obj);
  int dfsch_rational_p(dfsch_object_t* obj);
  int dfsch_integer_p(dfsch_object_t* obj);
  int dfsch_number_exact_p(dfsch_object_t* obj);


  /** Makes number object from given floating-point number. */
  extern dfsch_object_t* dfsch_make_number_from_double(double n);
  /** Makes number object from given integer number. */
  extern dfsch_object_t* dfsch_make_number_from_long(long n);
  /** Makes number object from int64_t */
  extern dfsch_object_t* dfsch_make_number_from_int64(int64_t n);
  
  /** Returns value of given number as double. */
  extern double dfsch_number_to_double(dfsch_object_t *n);
  /** Returns value of given number as long. */
  extern long dfsch_number_to_long(dfsch_object_t *n);
  /** Returns value of given number as int64_t. */
  extern int64_t dfsch_number_to_int64(dfsch_object_t *n);
  extern char* dfsch_number_to_string(dfsch_object_t *n, int base);
  extern char* dfsch_number_format(dfsch_object_t* n, int width, int digits);
  extern dfsch_object_t* dfsch_number_to_inexact(dfsch_object_t* n);


  /** Creates number from external representation. */
  extern dfsch_object_t* dfsch_make_number_from_string(char* str, int base);

  extern dfsch_object_t* dfsch_number_numerator(dfsch_object_t* n);
  extern dfsch_object_t* dfsch_number_denominator(dfsch_object_t* n);

  /** Add arguments. */
  extern dfsch_object_t* dfsch_number_add(dfsch_object_t* a, 
                                          dfsch_object_t* b);
  /** Substract arguments. */
  extern dfsch_object_t* dfsch_number_sub(dfsch_object_t* a, 
                                          dfsch_object_t* b);
  /** Additive inverse */
  extern dfsch_object_t* dfsch_number_neg(dfsch_object_t* n);
  /** Absolute value */
  extern dfsch_object_t* dfsch_number_abs(dfsch_object_t* n);

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
  extern dfsch_object_t* dfsch_number_mod_inv(dfsch_object_t* a, 
                                              dfsch_object_t* b);

  /** Less than operator */
  extern int dfsch_number_lt(dfsch_object_t* a, dfsch_object_t* b);
  /** Greater than operator */
  extern int dfsch_number_gt(dfsch_object_t* a, dfsch_object_t* b);
  /** Less than or equal operator */
  extern int dfsch_number_lte(dfsch_object_t* a, dfsch_object_t* b);
  /** Greater than or equal operator */
  extern int dfsch_number_gte(dfsch_object_t* a, dfsch_object_t* b);

  extern int dfsch_number_sign(dfsch_object_t* n);
  extern int dfsch_number_negative_p(dfsch_object_t* n);
  extern int dfsch_number_positive_p(dfsch_object_t* n);
  extern int dfsch_number_zero_p(dfsch_object_t* n);
  extern int dfsch_number_even_p(dfsch_object_t* n);
  extern int dfsch_number_odd_p(dfsch_object_t* n);


  extern dfsch_object_t* dfsch_number_gcd(dfsch_object_t* a,
                                          dfsch_object_t* b);
  extern dfsch_object_t* dfsch_number_lcm(dfsch_object_t* a,
                                          dfsch_object_t* b);

dfsch_object_t* dfsch_number_logand(dfsch_object_t* a, dfsch_object_t* b);
dfsch_object_t* dfsch_number_logior(dfsch_object_t* a, dfsch_object_t* b);
dfsch_object_t* dfsch_number_logxor(dfsch_object_t* a, dfsch_object_t* b);
dfsch_object_t* dfsch_number_lognot(dfsch_object_t* a);


  extern dfsch_type_t dfsch_number_sequence_type;
#define DFSCH_NUMBER_SEQUENCE_TYPE (&dfsch_number_sequence_type)
  extern dfsch_iterator_type_t dfsch_number_sequence_iterator_type;
#define DFSCH_NUMBER_SEQUENCE_ITERATOR_TYPE (&dfsch_number_sequence_iterator_type)

  dfsch_object_t* dfsch_make_number_sequence(dfsch_object_t* from,
                                             dfsch_object_t* to,
                                             dfsch_object_t* step);

  dfsch_object_t* dfsch_number_next_prime(dfsch_object_t* n);


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

#define DFSCH_INT64_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, long, dfsch_number_to_int64)
#define DFSCH_INT64_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, long, dfsch_number_to_int64)



#endif
