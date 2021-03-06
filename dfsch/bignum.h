/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Direct manipulation of bignums
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

/*
 * This is special direct interface to bignum implementation. Most code should 
 * use interface in number.h.
 */

#ifndef H__dfsch__bignum__
#define H__dfsch__bignum__

#include <dfsch/dfsch.h>
#include <dfsch/strings.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct dfsch_bignum_t dfsch_bignum_t;

dfsch_bignum_t* dfsch_bignum_from_number(dfsch_object_t* n); 
dfsch_object_t* dfsch_bignum_to_number(dfsch_bignum_t* b);

#define DFSCH_BIGNUM_ARG(al, name)                                      \
  DFSCH_GENERIC_ARG(al, name, dfsch_bignum_t*, dfsch_bignum_from_number)
#define DFSCH_BIGNUM_ARG_OPT(al, name, default)                         \
  DFSCH_GENERIC_ARG_OPT(al, name, default,                              \
                        dfsch_bignum_t*, dfsch_bignum_from_number)

dfsch_bignum_t* dfsch_make_bignum_uint64(uint64_t n);
dfsch_bignum_t* dfsch_make_bignum_int64(int64_t n);

int dfsch_bignum_cmp_abs(dfsch_bignum_t* a, dfsch_bignum_t* b);
int dfsch_bignum_cmp(dfsch_bignum_t* a, dfsch_bignum_t* b);
int dfsch_bignum_equal_p(dfsch_bignum_t* a, dfsch_bignum_t* b);
int dfsch_bignum_sign(dfsch_bignum_t* a);
int dfsch_bignum_even_p(dfsch_bignum_t* a);

dfsch_bignum_t* dfsch_bignum_add(dfsch_bignum_t* a, dfsch_bignum_t* b);
dfsch_bignum_t* dfsch_bignum_sub(dfsch_bignum_t* a, dfsch_bignum_t* b);
dfsch_bignum_t* dfsch_bignum_neg(dfsch_bignum_t* a);
dfsch_bignum_t* dfsch_bignum_abs(dfsch_bignum_t* a);
dfsch_bignum_t* dfsch_bignum_mul(dfsch_bignum_t* a, dfsch_bignum_t* b);

void dfsch_bignum_div(dfsch_bignum_t* a, dfsch_bignum_t* b, 
                      dfsch_bignum_t**qp, dfsch_bignum_t** rp);

dfsch_bignum_t* dfsch_bignum_exp(dfsch_bignum_t* b, 
                                 dfsch_bignum_t* e, 
                                 dfsch_bignum_t* m);

dfsch_bignum_t* dfsch_bignum_logand(dfsch_bignum_t* a, dfsch_bignum_t* b);
dfsch_bignum_t* dfsch_bignum_logior(dfsch_bignum_t* a, dfsch_bignum_t* b);
dfsch_bignum_t* dfsch_bignum_logxor(dfsch_bignum_t* a, dfsch_bignum_t* b);
dfsch_bignum_t* dfsch_bignum_lognot(dfsch_bignum_t* a);
dfsch_bignum_t* dfsch_bignum_shr(dfsch_bignum_t* b, size_t count);
dfsch_bignum_t* dfsch_bignum_shl(dfsch_bignum_t* b, size_t count);


char* dfsch_bignum_to_string(dfsch_bignum_t* b, unsigned base);

dfsch_strbuf_t* dfsch_bignum_to_bytes(dfsch_bignum_t* b);
dfsch_bignum_t* dfsch_bignum_from_bytes(uint8_t* buf, size_t len, int negative);

int dfsch_bignum_to_uint64(dfsch_bignum_t* b, uint64_t* rp);
int dfsch_bignum_to_int64(dfsch_bignum_t* b, int64_t* rp);
double dfsch_bignum_to_double(dfsch_bignum_t* b);

#endif
