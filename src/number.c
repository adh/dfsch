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

#define _XOPEN_SOURCE 600 /* for math.h */
#include <dfsch/number.h>
#include <dfsch/bignum.h>
#include <dfsch/strings.h>
#include "util.h"
#include "internal.h"
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>

typedef dfsch_object_t object_t;

typedef struct flonum_t {
  dfsch_type_t *type;
  double flonum;
} flonum_t;

typedef struct fracnum_t {
  dfsch_type_t* type;
  dfsch_object_t* num;
  dfsch_object_t* denom;
} fracnum_t;

static size_t diffusion(size_t h){
  
  h ^= h >> 5 | h << sizeof(size_t)*8 - 5;
  h ^= h << 7 | h << sizeof(size_t)*8 - 7;
  h += 0x8025f5a6;
  h ^= h >> 11 | h << sizeof(size_t)*8 - 11;
  h ^= h << 13 | h << sizeof(size_t)*8 - 13;

  return h;
}

dfsch_type_t dfsch_number_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "number",
  NULL,
  NULL,
  NULL,
  NULL
};
dfsch_type_t dfsch_real_type = {
  DFSCH_ABSTRACT_TYPE,
  DFSCH_NUMBER_TYPE,
  0,
  "real",
  NULL,
  NULL,
  NULL,
  NULL
};
dfsch_type_t dfsch_rational_type = {
  DFSCH_ABSTRACT_TYPE,
  DFSCH_REAL_TYPE,
  0,
  "rational",
  NULL,
  NULL,
  NULL,
  NULL
};
dfsch_type_t dfsch_integer_type = {
  DFSCH_ABSTRACT_TYPE,
  DFSCH_RATIONAL_TYPE,
  0,
  "integer",
  NULL,
  NULL,
  NULL,
  NULL
};


static void fixnum_write(dfsch_object_t* n, dfsch_writer_state_t* state){
  dfsch_write_string(state, saprintf("%ld", (long)DFSCH_FIXNUM_REF(n)));
}

dfsch_number_type_t dfsch_fixnum_type = {
  DFSCH_SPECIAL_TYPE,
  DFSCH_INTEGER_TYPE,
  0,
  "fixnum",
  NULL,
  (dfsch_type_write_t)fixnum_write,
  NULL,
  NULL
};

static void flonum_write(flonum_t* n, dfsch_writer_state_t* state){
  char* res = saprintf("%.32g", n->flonum);
  dfsch_write_string(state, res);
  if (strchr(res, '.') == NULL){
    dfsch_write_string(state, ".");
  }
}
static uint32_t flonum_hash(flonum_t* n){
  char* ptr = ((char*)&(n->flonum));
  size_t i = sizeof(double);
  uint32_t hash = 0xc32e64c9;

  while (i){
    hash ^= (*ptr) + (hash << 3);
    i--;
    ptr++;
  }

  return hash;
}
static int flonum_equal_p(flonum_t* a, flonum_t* b){
  return a->flonum == b->flonum;
}

dfsch_number_type_t dfsch_flonum_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_REAL_TYPE,
  sizeof(flonum_t),
  "flonum",
  (dfsch_type_equal_p_t)flonum_equal_p,
  (dfsch_type_write_t)flonum_write,
  NULL,
  (dfsch_type_hash_t)flonum_hash,
};

static void fracnum_write(fracnum_t* n, dfsch_writer_state_t* state){
  dfsch_write_string(state, dfsch_number_to_string(n->num, 10));
  dfsch_write_string(state, "/");
  dfsch_write_string(state, dfsch_number_to_string(n->denom, 10));
}
static int fracnum_equal_p(fracnum_t* a, fracnum_t* b){
  return  
    dfsch_equal_p(a->num, b->num) &&
    dfsch_equal_p(a->denom, b->denom);
}
static uint32_t fracnum_hash(fracnum_t* n){
  uint32_t h;
  h = 0x7e9aa806;

  h ^= dfsch_hash(n->num);
  h = h << 16 | h >> 16;
  h ^= dfsch_hash(n->denom);
  return h;
}

dfsch_number_type_t dfsch_fracnum_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_RATIONAL_TYPE,
  sizeof(fracnum_t),
  "fracnum",
  (dfsch_type_equal_p_t)fracnum_equal_p,
  (dfsch_type_write_t)fracnum_write,
  NULL,
  (dfsch_type_hash_t)fracnum_hash,
};

static dfsch_object_t* frac_cons(dfsch_object_t* num, dfsch_object_t* denom){
  fracnum_t* f = (fracnum_t*)dfsch_make_object(DFSCH_FRACNUM_TYPE);
  dfsch_object_t* n;
  dfsch_object_t* d;
  dfsch_object_t* x;
  int negative = dfsch_number_negative_p(num) != dfsch_number_negative_p(denom);

  n = dfsch_number_abs(num);
  d = dfsch_number_abs(denom);

  x = dfsch_number_gcd(n, d);
  d = dfsch_number_div_i(d, x);
  n = dfsch_number_div_i(n, x);

  if (negative){
    n = dfsch_number_neg(n);
  }

  if (d == DFSCH_MAKE_FIXNUM(1)){
    return n;
  }

  f->num = n;
  f->denom = d;

  return (dfsch_object_t*)f;
}

dfsch_object_t* dfsch_number_numerator(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return ((fracnum_t*)n)->num;
  }
  if (dfsch_integer_p(n)){
    return n;
  }
  dfsch_error("exception:not-a-rational-number", n);
}
dfsch_object_t* dfsch_number_denominator(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return ((fracnum_t*)n)->denom;
  }
  if (dfsch_integer_p(n)){
    return DFSCH_MAKE_FIXNUM(1);
  }
  dfsch_error("exception:not-a-rational-number", n);
}

dfsch_object_t* dfsch_make_number_from_double(double num){
  flonum_t *n;
  n = (flonum_t*)dfsch_make_object(DFSCH_FLONUM_TYPE);

  n->flonum = num;

  return (dfsch_object_t*)n;
}
dfsch_object_t* dfsch_make_number_from_long(long num){
  if (num > DFSCH_FIXNUM_MAX || num < DFSCH_FIXNUM_MIN){
    return (dfsch_object_t*)dfsch_make_bignum_int64((int64_t)num);
  }

  return DFSCH_MAKE_FIXNUM(num);
}
dfsch_object_t* dfsch_make_number_from_int64(int64_t num){
  if (num > DFSCH_FIXNUM_MAX || num < DFSCH_FIXNUM_MIN){
    return (dfsch_object_t*)dfsch_make_bignum_int64(num);
  }

  return DFSCH_MAKE_FIXNUM(num);
}

static int dig_val[128] = {
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  0,  1,  2,  3,   4,  5,  6,  7,   8,  9,  37, 37,  37, 37, 37, 37,

  37, 10, 11, 12, 13,  14, 15, 16, 17,  18, 19, 20, 21,  22, 23, 24, 
  25, 26, 27, 28, 29,  30, 31, 32, 33,  34, 35, 36, 37,  37, 37, 37,
  37, 10, 11, 12, 13,  14, 15, 16, 17,  18, 19, 20, 21,  22, 23, 24, 
  25, 26, 27, 28, 29,  30, 31, 32, 33,  34, 35, 36, 37,  37, 37, 37
};

dfsch_object_t* dfsch_make_number_from_string(char* string, int obase){
  dfsch_object_t* n = DFSCH_MAKE_FIXNUM(0);
  int64_t sn = 0;
  int64_t on = 0;
  int base = obase;
  int d;
  int negative = 0;

  if (strchr(string, '.') != NULL || 
      (base == 10 && strpbrk(string, "eE") != NULL)){
    if (base != 10 && base != 0){
      dfsch_error("exception:non-supported-base-for-real-numbers",
                  DFSCH_MAKE_FIXNUM(base));
    }
    return dfsch_make_number_from_double(atof(string));
  }

  if (*string == '-'){
    string++;
    negative = 1;
  }

  if (base == 0){
    if (*string == '0'){
      string++;
      base = 8;
      if (*string == 'x' || *string == 'X'){
        string++;
        base = 16;
      }
    } else {
      base = 10;
    }
  }

  while (*string && *string != '/' && 
         sn <= DFSCH_FIXNUM_MAX && sn >= 0 && sn >= on){
    d = dig_val[*string];
    if (d >= base){
      dfsch_error("exception:invalid-digit", DFSCH_MAKE_FIXNUM(*string));
    }
    on = sn;
    sn *= base;
    sn += d;
    string++;
  }

  if (!*string){
    return dfsch_make_number_from_int64(negative ? -sn : sn);
  }
  if (*string == '/'){
    string++;
    if (strchr(string, '/') != NULL){
      dfsch_error("exception:too-many-slashes", NULL);
    }

    return dfsch_number_div(dfsch_make_number_from_int64(negative ? -sn : sn),
                            dfsch_make_number_from_string(string, obase));
  }

  n = DFSCH_MAKE_FIXNUM(on);
  string--;

  while (*string && *string != '/'){
    d = dig_val[*string];
    if (d >= base){
      dfsch_error("exception:invalid-digit", DFSCH_MAKE_FIXNUM(*string));
    }
    n = dfsch_number_mul(n, DFSCH_MAKE_FIXNUM(base));
    n = dfsch_number_add(n, DFSCH_MAKE_FIXNUM(d));
    string++;
  }

  if (*string == '/'){
    string++;
    if (strchr(string, '/') != NULL){
      dfsch_error("exception:too-many-slashes", NULL);
    }

    n = dfsch_number_div(n,
                         dfsch_make_number_from_string(string+1, obase));
  }

  if (negative){
    return dfsch_number_neg(n);
  } else {
    return n;
  }
}

double dfsch_number_to_double(dfsch_object_t *n){
  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    return (double)DFSCH_FIXNUM_REF(n);
  }
  if (DFSCH_TYPE_OF(n)==DFSCH_FLONUM_TYPE){
    return ((flonum_t*)n)->flonum;
  }  
  if (DFSCH_TYPE_OF(n)==DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_to_double((dfsch_bignum_t*)n);
  }
  if (DFSCH_TYPE_OF(n)==DFSCH_FRACNUM_TYPE){
    return dfsch_number_to_double(((fracnum_t*)n)->num) /
      dfsch_number_to_double(((fracnum_t*)n)->denom);
  }
  dfsch_error("exception:not-a-real-number", n);
}
dfsch_object_t* dfsch_number_to_inexact(dfsch_object_t* n){
  return dfsch_make_number_from_double(dfsch_number_to_double(n));
}
long dfsch_number_to_long(dfsch_object_t *n){
  int64_t r;

  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    return DFSCH_FIXNUM_REF(n); /* we expect that fixnum fits in long */
  } else if (DFSCH_TYPE_OF(n)==DFSCH_BIGNUM_TYPE){
    if (!dfsch_bignum_to_int64((dfsch_bignum_t*)n, &r) || 
        r < LONG_MIN || r > LONG_MAX){
      dfsch_error("exception:value-too-large", NULL);
    }

    return r;
  }
  dfsch_error("exception:not-an-integer", n);
}

int64_t dfsch_number_to_int64(dfsch_object_t *n){
  int64_t r;

  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    return DFSCH_FIXNUM_REF(n);
  } else if (DFSCH_TYPE_OF(n)==DFSCH_BIGNUM_TYPE){
    if (!dfsch_bignum_to_int64((dfsch_bignum_t*)n, &r)){
      dfsch_error("exception:value-too-large", NULL);
    }

    return r;
  }
  dfsch_error("exception:not-an-integer", n);
}

static char* digits = "0123456789abcdefghijklmnopqrstuvwxyz";

static char* int_to_str(long i, int base){
  long n;
  char* buf = GC_MALLOC_ATOMIC(32); /* where do you have larger fixnums? */
  buf += 31;
  *buf = '\0';

  if (base == 0 || base > 36){
    dfsch_error("Invalid base", NULL);
  }

  n = labs(i);

  while (n > 0){
    buf--;
    *buf = digits[n % base];
    n /= base;
  }

  if (i < 0){
    buf--;
    *buf = '-';
    return buf;
  } else {
    return buf;
  }

}

char* dfsch_number_to_string(dfsch_object_t *n, int base){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    return int_to_str(DFSCH_FIXNUM_REF(n), base);
  }
  if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_to_string((dfsch_bignum_t*)n, base);
  }

  if (!dfsch_number_p(n)){
    dfsch_error("Not a number", n);
  }

  if (base != 10){
    dfsch_error("Unsupported base for this numeric type", NULL);
  }

  return dfsch_object_2_string(n, 1, 1); /* fallback */
}
char* dfsch_number_format(dfsch_object_t* n, int width, int digits){
  double num = dfsch_number_to_double(n);
  return dfsch_saprintf("%*.*f", width, digits, num);
}

int dfsch_number_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_NUMBER_TYPE);
}
int dfsch_real_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_REAL_TYPE);
}
int dfsch_rational_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_RATIONAL_TYPE);
}
int dfsch_integer_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_INTEGER_TYPE);
}
int dfsch_number_exact_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_RATIONAL_TYPE);
}


int dfsch_number_equal_p(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b)){
    if (DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) == DFSCH_FIXNUM_REF(b);
    }
    if (DFSCH_TYPE_OF(a) == DFSCH_BIGNUM_TYPE){
      return dfsch_bignum_equal_p((dfsch_bignum_t*)a, 
                                  (dfsch_bignum_t*)b);
    }
    if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE){
      return dfsch_number_equal_p(((fracnum_t*)a)->num, ((fracnum_t*)a)->num) &&
        dfsch_number_equal_p(((fracnum_t*)a)->denom, ((fracnum_t*)a)->denom);
    }
  }
   
  return dfsch_number_to_double(a) == dfsch_number_to_double(b);
}

// Comparisons

int dfsch_number_cmp(dfsch_object_t* a, dfsch_object_t* b){
  if (a == b) {
    return 0;
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b)){
    if (DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) < DFSCH_FIXNUM_REF(b) ? -1 : 1;
    }
    if (DFSCH_TYPE_OF(a) == DFSCH_BIGNUM_TYPE){
      return dfsch_bignum_cmp((dfsch_bignum_t*)a, (dfsch_bignum_t*)b);
    }
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FRACNUM_TYPE){
    return dfsch_number_cmp(dfsch_number_mul(dfsch_number_numerator(a),
                                             dfsch_number_denominator(b)),
                            dfsch_number_mul(dfsch_number_numerator(b),
                                             dfsch_number_denominator(a)));
  }

  if ((DFSCH_TYPE_OF(a) == DFSCH_BIGNUM_TYPE &&
       DFSCH_TYPE_OF(b) == DFSCH_FIXNUM_TYPE) ||
      (DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE &&
       DFSCH_TYPE_OF(b) == DFSCH_BIGNUM_TYPE)){
    return dfsch_bignum_cmp(dfsch_bignum_from_number(a), 
                            dfsch_bignum_from_number(b));    
  }
   
  if (dfsch_number_to_double(a) == dfsch_number_to_double(b)){
    return 0;
  } else {
    return (dfsch_number_to_double(a) < dfsch_number_to_double(b)) ?
      -1 : 1;
  }
}

int dfsch_number_lt(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_number_cmp(a, b) < 0;
}
int dfsch_number_gt(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_number_cmp(a, b) > 0;
}
int dfsch_number_lte(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_number_cmp(a, b) <= 0;
}
int dfsch_number_gte(dfsch_object_t* a, dfsch_object_t* b){
  return dfsch_number_cmp(a, b) >= 0;
}

int dfsch_number_sign(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    if (DFSCH_FIXNUM_REF(n) == 0){
      return 0;
    } else {
      return (DFSCH_FIXNUM_REF(n) < 0) ? - 1 : 1;
    }
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_sign((dfsch_bignum_t*)n);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return dfsch_number_sign(((fracnum_t*)n)->num);
  } else {
    double nn = dfsch_number_to_double(n);
    if (nn == 0.0){
      return 0;
    } else {
      return (DFSCH_FIXNUM_REF(n) < 0) ? - 1 : 1;      
    }
  } 
}

/* numeric predicates */
int dfsch_number_negative_p(dfsch_object_t* n){
  return dfsch_number_sign(n) < 0;
}
int dfsch_number_positive_p(dfsch_object_t* n){
  return dfsch_number_sign(n) > 0;
}
int dfsch_number_zero_p(dfsch_object_t* n){
  return dfsch_number_sign(n) == 0;
}
int dfsch_number_even_p(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    return ((DFSCH_FIXNUM_REF(n) & 0x01) == 0);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_even_p((dfsch_bignum_t*)n);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return 0; /* proper fracnum cannot be integer */
  } else {
    double nn = dfsch_number_to_double(n);
    nn /= 2.0;
    return nn == round(nn);
  }   
}
int dfsch_number_odd_p(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    return ((DFSCH_FIXNUM_REF(n) & 0x01) == 1);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return !dfsch_bignum_even_p((dfsch_bignum_t*)n);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return 0; /* proper fracnum cannot be integer */
  } else {
    double nn = dfsch_number_to_double(n);
    nn = (nn + 1.0) /2.0;
    return nn == round(nn);
  }
}
size_t dfsch_number_lsb(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    long num = DFSCH_FIXNUM_REF(n);
    size_t res = 0;

    if (num < 0){
      num = -num;
    }

    if (num == 0){
      return -1;
    }

    while ((num & 0x01) == 0){
      num >>= 1;
      res++;
    }

    return res;
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_lsb(n);
  } else {
    dfsch_error("Not an integer", n);
  }
}
size_t dfsch_number_msb(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    long num = DFSCH_FIXNUM_REF(n);
    size_t res = 0;

    if (num < 0){
      num = -num;
    }

    while (num){
      num >>= 1;
      res++;
    }

    return res - 1;
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_msb(n);
  } else {
    dfsch_error("Not an integer", n);
  }
}


// Arithmetics

dfsch_object_t* dfsch_number_add(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
    long an = DFSCH_FIXNUM_REF(a)<<2;
    long bn = DFSCH_FIXNUM_REF(b)<<2;
    long x = an + bn;
      
    if ((an^x) >= 0 || (bn^x) >= 0) {
      return DFSCH_MAKE_FIXNUM(x>>2);
    }
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FLONUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FLONUM_TYPE){
    return dfsch_make_number_from_double 
      (dfsch_number_to_double((dfsch_object_t*) a) + 
       dfsch_number_to_double((dfsch_object_t*) b));   
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FRACNUM_TYPE){
    dfsch_object_t* c = dfsch_number_mul(dfsch_number_denominator(a), 
                                         dfsch_number_denominator(b));

    return 
      frac_cons(dfsch_number_add(dfsch_number_mul(dfsch_number_numerator(a), 
                                                  dfsch_number_denominator(b)),
                                 dfsch_number_mul(dfsch_number_numerator(b),
                                                  dfsch_number_denominator(a))),
                c);
  }


  return dfsch_bignum_to_number(dfsch_bignum_add(dfsch_bignum_from_number(a),
                                                 dfsch_bignum_from_number(b)));
}

dfsch_object_t* dfsch_number_sub(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
    long an = DFSCH_FIXNUM_REF(a)<<2;
    long bn = DFSCH_FIXNUM_REF(b)<<2;
    long x = an - bn;

    if ((an^x) >= 0 || (~bn^x) >= 0)
      return DFSCH_MAKE_FIXNUM(x>>2);
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FLONUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FLONUM_TYPE){
    return dfsch_make_number_from_double 
      (dfsch_number_to_double((dfsch_object_t*) a) - 
       dfsch_number_to_double((dfsch_object_t*) b));   
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FRACNUM_TYPE){
    dfsch_object_t* c = dfsch_number_mul(dfsch_number_denominator(a), 
                                         dfsch_number_denominator(b));

    return 
      frac_cons(dfsch_number_sub(dfsch_number_mul(dfsch_number_numerator(a), 
                                                  dfsch_number_denominator(b)),
                                 dfsch_number_mul(dfsch_number_numerator(b),
                                                  dfsch_number_denominator(a))),
                c);
  }

  return dfsch_bignum_to_number(dfsch_bignum_sub(dfsch_bignum_from_number(a),
                                                 dfsch_bignum_from_number(b)));
}

dfsch_object_t* dfsch_number_neg(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    return dfsch_make_number_from_long(-DFSCH_FIXNUM_REF(n));
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_to_number(dfsch_bignum_neg((dfsch_bignum_t*)n));
  } else if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return frac_cons(dfsch_number_neg(((fracnum_t*)n)->num),
                     ((fracnum_t*)n)->denom);
  } else {
    return dfsch_make_number_from_double(-dfsch_number_to_double(n));
  } 
}
dfsch_object_t* dfsch_number_abs(dfsch_object_t* n){
  if (dfsch_number_negative_p(n)){
    return dfsch_number_neg(n);
  } else {
    return n;
  }
}


dfsch_object_t* dfsch_number_mul(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    long an = DFSCH_FIXNUM_REF(a)<<2;
    long bn = DFSCH_FIXNUM_REF(b);
    long x = an * bn;
    double xd = (double)an * (double)bn;
    
    if (x == xd){
      return DFSCH_MAKE_FIXNUM(x>>2);
    }else{
      double d = x > xd ? x - xd : xd - x;
      double p = xd >= 0 ? xd : -xd;
      
      if (32.0 * d <= p){
	return DFSCH_MAKE_FIXNUM(x>>2);
      }
    }
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FLONUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FLONUM_TYPE){
    return dfsch_make_number_from_double 
      (dfsch_number_to_double((dfsch_object_t*) a) * 
       dfsch_number_to_double((dfsch_object_t*) b)); 
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FRACNUM_TYPE){
    dfsch_object_t* c = dfsch_number_mul(dfsch_number_denominator(a), 
                                         dfsch_number_denominator(b));

    return 
      frac_cons(dfsch_number_mul(dfsch_number_numerator(a), 
                                 dfsch_number_numerator(b)),
                dfsch_number_mul(dfsch_number_denominator(b),
                                 dfsch_number_denominator(a)));
  }


  return dfsch_bignum_to_number(dfsch_bignum_mul(dfsch_bignum_from_number(a),
                                                 dfsch_bignum_from_number(b)));
}

dfsch_object_t* dfsch_number_div (dfsch_object_t* a,  
                                  dfsch_object_t* b){ 

  if (DFSCH_TYPE_OF(a) == DFSCH_FLONUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FLONUM_TYPE){
    double an = dfsch_number_to_double((dfsch_object_t*) a);
    double bn = dfsch_number_to_double((dfsch_object_t*) b);
  
    if (bn == 0.0)
      dfsch_error("exception:division-by-zero", NULL);

    return dfsch_make_number_from_double(an / bn); 
  }

  if (b == DFSCH_MAKE_FIXNUM(0)){
    dfsch_error("exception:division-by-zero", NULL);
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE ||
      DFSCH_TYPE_OF(b) == DFSCH_FRACNUM_TYPE){
    dfsch_object_t* c = dfsch_number_mul(dfsch_number_denominator(a), 
                                         dfsch_number_denominator(b));

    return 
      frac_cons(dfsch_number_mul(dfsch_number_numerator(a), 
                                 dfsch_number_denominator(b)),
                dfsch_number_mul(dfsch_number_denominator(a),
                                 dfsch_number_numerator(b)));
  }

  return frac_cons(a, b);
}
dfsch_object_t* dfsch_number_div_i(dfsch_object_t* a,  
                                   dfsch_object_t* b){ 
  dfsch_bignum_t* r;

  if (b == DFSCH_MAKE_FIXNUM(0)){
    dfsch_error("exception:division-by-zero", NULL);
  }

  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    long an = DFSCH_FIXNUM_REF(a);
    long bn = DFSCH_FIXNUM_REF(b);
 
    
    return dfsch_make_number_from_long(an / bn); 
  }

  dfsch_bignum_div(dfsch_bignum_from_number(a),
                   dfsch_bignum_from_number(b),
                   &r, NULL);

  return dfsch_bignum_to_number(r);
}
dfsch_object_t* dfsch_number_mod (dfsch_object_t* a,  
                                  dfsch_object_t* b){ 
  dfsch_bignum_t* r;

  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    long an = dfsch_number_to_long((dfsch_object_t*) a);
    long bn = dfsch_number_to_long((dfsch_object_t*) b);
 
    if (bn == 0)
      dfsch_error("exception:division-by-zero", NULL);
    
    return dfsch_make_number_from_long(an % bn); 
  }

  dfsch_bignum_div(dfsch_bignum_from_number(a),
                   dfsch_bignum_from_number(b),
                   NULL, &r);

  return dfsch_bignum_to_number(r);
}

dfsch_object_t* dfsch_number_gcd(dfsch_object_t* a,
                                 dfsch_object_t* b){
  dfsch_object_t* t;
  
  if (dfsch_number_lt(a, b)){
    t = a;
    a = b;
    b = t;
  }

  while (dfsch_number_sign(b) != 0){
    a = dfsch_number_mod(a, b);
    if (dfsch_number_lt(a, b)){
      t = a;
      a = b;
      b = t;
    }
  }

  return a;
}
dfsch_object_t* dfsch_number_mod_inv(dfsch_object_t* n,
                                     dfsch_object_t* m){
  dfsch_object_t* a = n;
  dfsch_object_t* b = m;
  dfsch_object_t* t;
  dfsch_object_t* x = DFSCH_MAKE_FIXNUM(0);
  dfsch_object_t* lx = DFSCH_MAKE_FIXNUM(1);
  dfsch_object_t* q;

  a = dfsch_number_mod(a, b);

  while (b != DFSCH_MAKE_FIXNUM(0)){
    t = b;
    q = dfsch_number_div_i(a, b);
    b = dfsch_number_mod(a, b);
    a = t;
    
    t = x;
    x = dfsch_number_sub(lx, dfsch_number_mul(q, x));
    lx = t;
  }

  if (dfsch_number_negative_p(lx)){
    lx = dfsch_number_add(lx, m);
  }

  return lx;
}



dfsch_object_t* dfsch_number_lcm(dfsch_object_t* a,
                                 dfsch_object_t* b){
  return dfsch_number_div_i(dfsch_number_mul(a, b),
                            dfsch_number_gcd(a, b));
}

dfsch_object_t* dfsch_number_logand(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    return DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_REF(a) & DFSCH_FIXNUM_REF(b));
  }
  return 
    dfsch_bignum_to_number(dfsch_bignum_logand(dfsch_bignum_from_number(a),
                                               dfsch_bignum_from_number(b)));
}
dfsch_object_t* dfsch_number_logior(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    return DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_REF(a) | DFSCH_FIXNUM_REF(b));
  }
  return 
    dfsch_bignum_to_number(dfsch_bignum_logior(dfsch_bignum_from_number(a),
                                               dfsch_bignum_from_number(b)));
}
dfsch_object_t* dfsch_number_logxor(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    return DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_REF(a) ^ DFSCH_FIXNUM_REF(b));
  }
  return 
    dfsch_bignum_to_number(dfsch_bignum_logxor(dfsch_bignum_from_number(a),
                                               dfsch_bignum_from_number(b)));
}
dfsch_object_t* dfsch_number_lognot(dfsch_object_t* a){
  if (DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
    return DFSCH_MAKE_FIXNUM(-DFSCH_FIXNUM_REF(a));
  }
  return 
    dfsch_bignum_to_number(dfsch_bignum_lognot(dfsch_bignum_from_number(a)));
}


static const int small_primes[] = {
  3,    5,    7,   11,   13,   17,   19,   23,
  29,   31,   37,   41,   43,   47,   53,   59,
  61,   67,   71,   73,   79,   83,   89,   97,
  101,  103,  107,  109,  113,  127,  131,  137,
  139,  149,  151,  157,  163,  167,  173,  179,
  181,  191,  193,  197,  199,  211,  223,  227,
  229,  233,  239,  241,  251,  257,  263,  269,
  271,  277,  281,  283,  293,  307,  311,  313,
  317,  331,  337,  347,  349,  353,  359,  367,
  373,  379,  383,  389,  397,  401,  409,  419,
  421,  431,  433,  439,  443,  449,  457,  461,
  463,  467,  479,  487,  491,  499,  503,  509,
  521,  523,  541,  547,  557,  563,  569,  571,
  577,  587,  593,  599,  601,  607,  613,  617,
  619,  631,  641,  643,  647,  653,  659,  661,
  673,  677,  683,  691,  701,  709,  719,  727,
  733,  739,  743,  751,  757,  761,  769,  773,
  787,  797,  809,  811,  821,  823,  827,  829,
  839,  853,  857,  859,  863,  877,  881,  883,
  887,  907,  911,  919,  929,  937,  941,  947,
  953,  967,  971,  977,  983,  991,  997,
};

int dfsch_number_prime_p(dfsch_object_t* n){
  int i;
  dfsch_object_t* r;

  for (i = 0; i < sizeof(small_primes)/sizeof(int); i++){
    if (dfsch_number_zero_p(dfsch_number_mod(n, 
                                             DFSCH_MAKE_FIXNUM(small_primes[i])))){
      if (dfsch_number_equal_p(n, DFSCH_MAKE_FIXNUM(small_primes[i]))){
        return 1;
      } else {
        return 0;
      }
    }
  }

  r = dfsch_number_sub(n, DFSCH_MAKE_FIXNUM(1));
  
  
}


/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////


DFSCH_DEFINE_PRIMITIVE(number_p, NULL){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE(real_p, NULL){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_real_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE(rational_p, NULL){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_rational_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE(integer_p, NULL){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_integer_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE(exact_p, NULL){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_exact_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE(inexact_p, NULL){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(!dfsch_number_exact_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE(exact_2_inexact, 0){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  return dfsch_number_to_inexact(n);
}

DFSCH_DEFINE_PRIMITIVE(plus, NULL){
  object_t* i = args;
  dfsch_object_t* s = dfsch_make_number_from_long(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_add(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(minus, NULL){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("Too few arguments",i);

  if (!DFSCH_FAST_CDR(i))
    return dfsch_number_neg(DFSCH_FAST_CAR(i));
  s = DFSCH_FAST_CAR(i);
  i = DFSCH_FAST_CDR(i);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_sub(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(mult, NULL){
  object_t* i = args;
  object_t* s = dfsch_make_number_from_long(1);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_mul(s,DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(slash, NULL){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("Too few arguments",i);

  if (!DFSCH_FAST_CDR(i))
    return dfsch_number_div(dfsch_make_number_from_long(1), 
                            DFSCH_FAST_CAR(i));
  s = DFSCH_FAST_CAR(i);
  i = DFSCH_FAST_CDR(i);
  
  while(DFSCH_PAIR_P(i)){
    s=dfsch_number_div(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(slash_i, NULL){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("Too few arguments",i);

  if (!DFSCH_FAST_CDR(i))
    dfsch_error("Too few arguments",i);

  s = DFSCH_FAST_CAR(i);
  i = DFSCH_FAST_CDR(i);
  
  while(DFSCH_PAIR_P(i)){
    s=dfsch_number_div_i(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(modulo, NULL){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("Too few arguments",i);

  if (!DFSCH_FAST_CDR(i))
    return dfsch_number_div(dfsch_make_number_from_long(1), 
                            DFSCH_FAST_CAR(i));
  s = DFSCH_FAST_CAR(i);
  i = DFSCH_FAST_CDR(i);
  
  while(DFSCH_PAIR_P(i)){
    s=dfsch_number_mod(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}

DFSCH_DEFINE_PRIMITIVE(number_equal, NULL){
  object_t* z0, *z1;
  DFSCH_OBJECT_ARG(args, z0);
  DFSCH_OBJECT_ARG(args, z1);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_equal_p(z0,z1));
}

DFSCH_DEFINE_PRIMITIVE(lt, NULL){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (dfsch_number_cmp(a, b) >= 0)
      return NULL;
    a = b;
  }
  return DFSCH_SYM_TRUE;
}
DFSCH_DEFINE_PRIMITIVE(gt, NULL){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (dfsch_number_cmp(a, b) <= 0)
      return NULL;
    a = b;
  }
  return DFSCH_SYM_TRUE;
}
DFSCH_DEFINE_PRIMITIVE(lte, NULL){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (dfsch_number_cmp(a, b) > 0)
      return NULL;
    a = b;
  }
  return DFSCH_SYM_TRUE;
}
DFSCH_DEFINE_PRIMITIVE(gte, NULL){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (dfsch_number_cmp(a, b) < 0)
      return NULL;
    a = b;
  }
  return DFSCH_SYM_TRUE;
}

// Bitwise

DFSCH_DEFINE_PRIMITIVE(logand, NULL){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(-1); /* all ones */

  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logand(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(logior, NULL){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logior(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(logxor, NULL){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logxor(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(lognot, NULL){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_number_lognot(n);
}


// Functions

DFSCH_DEFINE_PRIMITIVE(abs, NULL){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  
  return dfsch_number_abs(n);
}

DFSCH_DEFINE_PRIMITIVE(exp, NULL){
  double z;
  DFSCH_DOUBLE_ARG_OPT(args, z, 1.0);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(exp(z));
}

DFSCH_DEFINE_PRIMITIVE(log, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z <= 0.0)
    dfsch_error("Argument not in domain", 
                dfsch_list(2, 
                           dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                               "log"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(log(z));
}

DFSCH_DEFINE_PRIMITIVE(sin, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(sin(z));
}
DFSCH_DEFINE_PRIMITIVE(cos, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(cos(z));
}
DFSCH_DEFINE_PRIMITIVE(tan, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(tan(z));
}

DFSCH_DEFINE_PRIMITIVE(asin, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z > 1.0 || z < -1.0)
    dfsch_error("Argument not in domain", 
                dfsch_list(2, 
                           dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                               "asin"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(asin(z));
}

DFSCH_DEFINE_PRIMITIVE(acos, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z > 1.0 || z < -1.0)
    dfsch_error("Argument not in domain", 
                dfsch_list(2, 
                           dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE, 
                                               "acos"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(acos(z));
}

DFSCH_DEFINE_PRIMITIVE(atan, NULL){
  double z0, z1;
  DFSCH_DOUBLE_ARG(args, z0);
  DFSCH_DOUBLE_ARG_OPT(args, z1, 1.0);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(atan(z0/z1));
}

DFSCH_DEFINE_PRIMITIVE(sqrt, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z < 0.0)
    dfsch_error("Argument not in domain", 
                dfsch_list(2, 
                           dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                               "sqrt"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(sqrt(z));
}

DFSCH_DEFINE_PRIMITIVE(expt, NULL){
  double z0, z1, v;
  DFSCH_DOUBLE_ARG(args, z0);
  DFSCH_DOUBLE_ARG(args, z1);
  DFSCH_ARG_END(args);

  errno = 0;
  v = pow(z0,z1);
  if (errno == EDOM) // XXX
    dfsch_error("Argument not in domain", 
                dfsch_list(3, 
                           dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                               "expt"),
                           dfsch_make_number_from_double(z0),
                           dfsch_make_number_from_double(z1)));

  return dfsch_make_number_from_double(v);
}

DFSCH_DEFINE_PRIMITIVE(zero_p, NULL){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_sign(n) == 0);
}

DFSCH_DEFINE_PRIMITIVE(positive_p, NULL){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_sign(n) > 0);
}

DFSCH_DEFINE_PRIMITIVE(negative_p, NULL){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_sign(n) < 0);
}

DFSCH_DEFINE_PRIMITIVE(even_p, NULL){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_even_p(n));
}

DFSCH_DEFINE_PRIMITIVE(odd_p, NULL){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_odd_p(n));
}

DFSCH_DEFINE_PRIMITIVE(max, NULL){
  object_t *max;
  object_t *i;

  DFSCH_OBJECT_ARG(args, max);
  while (args){
    DFSCH_OBJECT_ARG(args, i);
    if (dfsch_number_lt(max, i))
      max = i;
  }
  return max;
}

DFSCH_DEFINE_PRIMITIVE(min, NULL){
  object_t *max;
  object_t *i;

  DFSCH_OBJECT_ARG(args, max);
  while (args){
    DFSCH_OBJECT_ARG(args, i);
    if (dfsch_number_gt(max, i))
      max = i;
  }
  return max;
}

DFSCH_DEFINE_PRIMITIVE(round, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(round(z));
}
DFSCH_DEFINE_PRIMITIVE(floor, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(floor(z));
}
DFSCH_DEFINE_PRIMITIVE(ceiling, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(ceil(z));
}
DFSCH_DEFINE_PRIMITIVE(truncate, NULL){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(trunc(z));
}

DFSCH_DEFINE_PRIMITIVE(number_2_string, NULL){
  object_t *n;
  long base;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_LONG_ARG_OPT(args, base, 10);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_number_to_string(n, base));
}
DFSCH_DEFINE_PRIMITIVE(string_2_number, NULL){
  char *str;
  long base;

  DFSCH_STRING_ARG(args, str);
  DFSCH_LONG_ARG_OPT(args, base, 10);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_string(str, base);
}
DFSCH_DEFINE_PRIMITIVE(gcd, NULL){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  DFSCH_ARG_END(args);


  return dfsch_number_gcd(a, b);
}
DFSCH_DEFINE_PRIMITIVE(mod_inv, NULL){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  DFSCH_ARG_END(args);


  return dfsch_number_mod_inv(a, b);
}
DFSCH_DEFINE_PRIMITIVE(lcm, NULL){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  DFSCH_ARG_END(args);


  return dfsch_number_lcm(a, b);
}

DFSCH_DEFINE_PRIMITIVE(lsb, NULL){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return DFSCH_MAKE_FIXNUM(dfsch_number_lsb(n));
}

DFSCH_DEFINE_PRIMITIVE(msb, NULL){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return DFSCH_MAKE_FIXNUM(dfsch_number_msb(n));
}


void dfsch__number_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<number>", DFSCH_NUMBER_TYPE);
  dfsch_define_cstr(ctx, "<real>", DFSCH_REAL_TYPE);
  dfsch_define_cstr(ctx, "<rational>", DFSCH_RATIONAL_TYPE);
  dfsch_define_cstr(ctx, "<integer>", DFSCH_INTEGER_TYPE);

  dfsch_define_cstr(ctx, "<fixnum>", DFSCH_FIXNUM_TYPE);
  dfsch_define_cstr(ctx, "<flonum>", DFSCH_FLONUM_TYPE);
  dfsch_define_cstr(ctx, "<fracnum>", DFSCH_FRACNUM_TYPE);

  dfsch_defconst_cstr(ctx, "+", DFSCH_PRIMITIVE_REF(plus));
  dfsch_defconst_cstr(ctx, "-", DFSCH_PRIMITIVE_REF(minus));
  dfsch_defconst_cstr(ctx, "*", DFSCH_PRIMITIVE_REF(mult));
  dfsch_defconst_cstr(ctx, "/", DFSCH_PRIMITIVE_REF(slash));
  dfsch_defconst_cstr(ctx, "/i", DFSCH_PRIMITIVE_REF(slash_i));
  dfsch_defconst_cstr(ctx, "%", DFSCH_PRIMITIVE_REF(modulo));
  dfsch_defconst_cstr(ctx, "=", DFSCH_PRIMITIVE_REF(number_equal));
  dfsch_defconst_cstr(ctx, "<", DFSCH_PRIMITIVE_REF(lt));
  dfsch_defconst_cstr(ctx, ">", DFSCH_PRIMITIVE_REF(gt));
  dfsch_defconst_cstr(ctx, "<=", DFSCH_PRIMITIVE_REF(lte));
  dfsch_defconst_cstr(ctx, ">=", DFSCH_PRIMITIVE_REF(gte));
  dfsch_defconst_cstr(ctx, "number?", DFSCH_PRIMITIVE_REF(number_p));
  dfsch_defconst_cstr(ctx, "real?", DFSCH_PRIMITIVE_REF(real_p));
  dfsch_defconst_cstr(ctx, "rational?", DFSCH_PRIMITIVE_REF(rational_p));
  dfsch_defconst_cstr(ctx, "integer?", DFSCH_PRIMITIVE_REF(integer_p));
  dfsch_defconst_cstr(ctx, "exact?", DFSCH_PRIMITIVE_REF(exact_p));
  dfsch_defconst_cstr(ctx, "inexact?", DFSCH_PRIMITIVE_REF(inexact_p));
  dfsch_defconst_cstr(ctx, "exact->inexact", 
                      DFSCH_PRIMITIVE_REF(exact_2_inexact));

  dfsch_defconst_cstr(ctx, "pi", 
                      dfsch_make_number_from_double(4*atan(1)));


  dfsch_defconst_cstr(ctx, "abs", DFSCH_PRIMITIVE_REF(abs));

  dfsch_defconst_cstr(ctx, "exp", DFSCH_PRIMITIVE_REF(exp));
  dfsch_defconst_cstr(ctx, "log", DFSCH_PRIMITIVE_REF(log));
  dfsch_defconst_cstr(ctx, "expt", DFSCH_PRIMITIVE_REF(expt));

  dfsch_defconst_cstr(ctx, "sin", DFSCH_PRIMITIVE_REF(sin));
  dfsch_defconst_cstr(ctx, "cos", DFSCH_PRIMITIVE_REF(cos));
  dfsch_defconst_cstr(ctx, "tan", DFSCH_PRIMITIVE_REF(tan));

  dfsch_defconst_cstr(ctx, "asin", DFSCH_PRIMITIVE_REF(asin));
  dfsch_defconst_cstr(ctx, "acos", DFSCH_PRIMITIVE_REF(acos));
  dfsch_defconst_cstr(ctx, "atan", DFSCH_PRIMITIVE_REF(atan));

  dfsch_defconst_cstr(ctx, "sqrt", DFSCH_PRIMITIVE_REF(sqrt));

  dfsch_defconst_cstr(ctx, "min", DFSCH_PRIMITIVE_REF(min));
  dfsch_defconst_cstr(ctx, "max", DFSCH_PRIMITIVE_REF(max));

  dfsch_defconst_cstr(ctx, "zero?", DFSCH_PRIMITIVE_REF(zero_p));
  dfsch_defconst_cstr(ctx, "negative?", DFSCH_PRIMITIVE_REF(negative_p));
  dfsch_defconst_cstr(ctx, "positive?", DFSCH_PRIMITIVE_REF(positive_p));

  dfsch_defconst_cstr(ctx, "even?", DFSCH_PRIMITIVE_REF(even_p));
  dfsch_defconst_cstr(ctx, "odd?", DFSCH_PRIMITIVE_REF(odd_p));

  dfsch_defconst_cstr(ctx, "round", DFSCH_PRIMITIVE_REF(round));
  dfsch_defconst_cstr(ctx, "floor", DFSCH_PRIMITIVE_REF(floor));
  dfsch_defconst_cstr(ctx, "ceiling", DFSCH_PRIMITIVE_REF(ceiling));
  dfsch_defconst_cstr(ctx, "truncate", DFSCH_PRIMITIVE_REF(truncate));

  dfsch_defconst_cstr(ctx, "number->string", 
                      DFSCH_PRIMITIVE_REF(number_2_string));
  dfsch_defconst_cstr(ctx, "string->number", 
                      DFSCH_PRIMITIVE_REF(string_2_number));
  dfsch_defconst_cstr(ctx, "most-positive-fixnum",
                      DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_MAX));
  dfsch_defconst_cstr(ctx, "most-negative-fixnum",
                      DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_MIN));

  dfsch_defconst_cstr(ctx, "gcd", DFSCH_PRIMITIVE_REF(gcd));
  dfsch_defconst_cstr(ctx, "mod-inv", DFSCH_PRIMITIVE_REF(mod_inv));
  dfsch_defconst_cstr(ctx, "lcm", DFSCH_PRIMITIVE_REF(lcm));

  dfsch_defconst_cstr(ctx, "logand", DFSCH_PRIMITIVE_REF(logand));
  dfsch_defconst_cstr(ctx, "logior", DFSCH_PRIMITIVE_REF(logior));
  dfsch_defconst_cstr(ctx, "logxor", DFSCH_PRIMITIVE_REF(logxor));
  dfsch_defconst_cstr(ctx, "lognot", DFSCH_PRIMITIVE_REF(lognot));

  dfsch_defconst_cstr(ctx, "lsb", DFSCH_PRIMITIVE_REF(lsb));
  dfsch_defconst_cstr(ctx, "msb", DFSCH_PRIMITIVE_REF(msb));
  
}
