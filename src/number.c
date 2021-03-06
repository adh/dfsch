/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Number manipulation routines.
 * Copyright (C) 2005-2008 Ales Hakl
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

#define _XOPEN_SOURCE 600 /* for math.h */
#include <dfsch/number.h>
#include <dfsch/bignum.h>
#include <dfsch/strings.h>
#include <dfsch/random.h>
#include <dfsch/serdes.h>
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
#include <inttypes.h>
#include <stdint.h>

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
  ptrdiff_t num = DFSCH_FIXNUM_REF(n);

  dfsch_write_string(state, saprintf("%" PRIdPTR, num));
}

static void fixnum_serialize(dfsch_object_t* obj, dfsch_serializer_t* s){
  dfsch_serialize_stream_symbol(s, "fixnum");
  dfsch_serialize_integer(s, DFSCH_FIXNUM_REF(obj));
}

dfsch_number_type_t dfsch_fixnum_type = {
  DFSCH_SPECIAL_TYPE,
  DFSCH_INTEGER_TYPE,
  0,
  "fixnum",
  NULL,
  (dfsch_type_write_t)fixnum_write,
  NULL,
  NULL,
  .serialize = fixnum_serialize,
};

static void flonum_write(flonum_t* n, dfsch_writer_state_t* state){
  if (isnan(n->flonum)){
    dfsch_write_string(state, "+nan.");
  } else if (n->flonum == INFINITY) {
    dfsch_write_string(state, "+inf.");    
  } else if (n->flonum == -INFINITY) {
    dfsch_write_string(state, "-inf.");    
  } else {
    char* res = saprintf("%.32g", n->flonum);
    dfsch_write_string(state, res);
    if (strchr(res, '.') == NULL){
      dfsch_write_string(state, ".");
    }
  }
}

static uint32_t hash_double(double val){
  char* ptr = ((char*)&val);
  size_t i = sizeof(double);
  uint32_t hash = 0xc32e64c9;

  while (i){
    hash ^= (*ptr) + (hash << 3);
    i--;
    ptr++;
  }

  return hash;
}

static uint32_t flonum_hash(flonum_t* n){
  return hash_double(n->flonum);
}
static int flonum_equal_p(flonum_t* a, flonum_t* b){
  return a->flonum == b->flonum;
}

static uint64_t bit_reverse64(uint64_t v){
  uint64_t s = sizeof(v) * CHAR_BIT;
  uint64_t mask = ~0;         
  while ((s >>= 1) > 0) {
    mask ^= (mask << s);
    v = ((v >> s) & mask) | ((v << s) & ~mask);
  }  
  return v;
}

static void flonum_serialize(flonum_t* f, dfsch_serializer_t* s){
  int exp;
  double x;

  x = f->flonum;

  if (isnan(x)){
    dfsch_serialize_stream_symbol(s, "flonum-nan");
  } else if (x == INFINITY) {
    dfsch_serialize_stream_symbol(s, "flonum+inf");
  } else if (x == -INFINITY) {
    dfsch_serialize_stream_symbol(s, "flonum-inf");
  } else {
    dfsch_serialize_stream_symbol(s, "flonum");
    if (x < 0){
      dfsch_serialize_integer(s, -1);
      x = -x;
    } else if (x == 0) {
      dfsch_serialize_integer(s, 0);
      return;
    } else {
      dfsch_serialize_integer(s, 1);
    }

    x = frexp(x, &exp);
    
    dfsch_serialize_integer(s, exp);
    dfsch_serialize_integer(s, bit_reverse64((int64_t)((1LL << 53) * x)));
  }
}

DFSCH_DEFINE_DESERIALIZATION_HANDLER("flonum-nan", flonum_nan){
  dfsch_object_t* fn;
  fn = dfsch_make_number_from_double(NAN);
  dfsch_deserializer_put_partial_object(ds, fn);
  return fn;  
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("flonum+inf", flonum_pos_inf){
  dfsch_object_t* fn;
  fn = dfsch_make_number_from_double(INFINITY);
  dfsch_deserializer_put_partial_object(ds, fn);
  return fn;  
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("flonum-inf", flonum_neg_inf){
  dfsch_object_t* fn;
  fn = dfsch_make_number_from_double(-INFINITY);
  dfsch_deserializer_put_partial_object(ds, fn);
  return fn;  
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("flonum", flonum){
  dfsch_object_t* fn;
  double x;
  int64_t v;
  int exp;
  int sign;

  sign = dfsch_deserialize_integer(ds);
  if (sign == 0){
    x = 0;
  } else {
    exp = dfsch_deserialize_integer(ds);
    v = bit_reverse64(dfsch_deserialize_integer(ds));
    x = v;
    x = ldexp(x / ((double)(1LL << 53)), exp);
    if (sign == -1){
      x = -x;
    }
  }

  fn = dfsch_make_number_from_double(x);
  dfsch_deserializer_put_partial_object(ds, fn);
  return fn;
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
  .serialize = flonum_serialize,
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
static void fracnum_serialize(fracnum_t* n, dfsch_serializer_t* s){
  dfsch_serialize_stream_symbol(s, "fracnum");
  dfsch_serialize_object(s, n->num);
  dfsch_serialize_object(s, n->denom);
}
DFSCH_DEFINE_DESERIALIZATION_HANDLER("fracnum", fracnum){
  fracnum_t* f = (fracnum_t*)dfsch_make_object(DFSCH_FRACNUM_TYPE);
  dfsch_deserializer_put_partial_object(ds, f);
  f->num = dfsch_deserialize_object(ds);
  f->denom = dfsch_deserialize_object(ds);
  return f;
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
  .serialize = fracnum_serialize,
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
  dfsch_error("Not a rational number", n);
}
dfsch_object_t* dfsch_number_denominator(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_FRACNUM_TYPE){
    return ((fracnum_t*)n)->denom;
  }
  if (dfsch_integer_p(n)){
    return DFSCH_MAKE_FIXNUM(1);
  }
  dfsch_error("Not a rational number", n);
}

flonum_t* flonum_cache[256];

dfsch_object_t* dfsch_make_number_from_double(double num){
  flonum_t *n;
  uint32_t hash = hash_double(num);

  n = flonum_cache[hash & 0xff];
  
  if (n && n->flonum == num){
    return n;
  }

  n = (flonum_t*)dfsch_make_object(DFSCH_FLONUM_TYPE);

  n->flonum = num;
  flonum_cache[hash & 0xff] = n;

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
dfsch_object_t* dfsch_make_number_from_uint64(uint64_t num){
  if (num > DFSCH_FIXNUM_MAX){
    return (dfsch_object_t*)dfsch_make_bignum_uint64(num);
  }

  return DFSCH_MAKE_FIXNUM(num);
}


static int dig_val[256] = {
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  0,  1,  2,  3,   4,  5,  6,  7,   8,  9,  37, 37,  37, 37, 37, 37,

  37, 10, 11, 12, 13,  14, 15, 16, 17,  18, 19, 20, 21,  22, 23, 24, 
  25, 26, 27, 28, 29,  30, 31, 32, 33,  34, 35, 36, 37,  37, 37, 37,
  37, 10, 11, 12, 13,  14, 15, 16, 17,  18, 19, 20, 21,  22, 23, 24, 
  25, 26, 27, 28, 29,  30, 31, 32, 33,  34, 35, 36, 37,  37, 37, 37,

  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,

  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,  37, 37, 37, 37,
};

typedef struct flonum_constant_t {
  double value;
  char* name;
} flonum_constant_t;

static flonum_constant_t flonum_constants[] = {
  {INFINITY, "+inf."},
  {-INFINITY, "-inf."},
#ifdef NAN
  {NAN, "+nan."},
  {NAN, "-nan."},
#endif
};

dfsch_object_t* dfsch_make_number_from_string_noerror(char* string, int obase){
  dfsch_object_t* n = DFSCH_MAKE_FIXNUM(0);
  int base = obase;
  int d;
  int negative = 0;
  int i;

  for (i = 0; i < sizeof(flonum_constants) / sizeof(flonum_constant_t); i++){
    if (strcasecmp(string, flonum_constants[i].name) == 0){
      return dfsch_make_number_from_double(flonum_constants[i].value);
    }
  }

  if (strchr(string, '.') != NULL || 
      (base == 10 && strpbrk(string, "eE") != NULL)){
    if (base != 10 && base != 0){
      return NULL;
    }
    char* endptr;
    double d = strtod(string, &endptr);

    if (*endptr != '\0'){
      return NULL;
    }

    return dfsch_make_number_from_double(d);
  }

  if (*string == '-'){
    string++;
    negative = 1;
  } else if (*string == '+'){
    string++;
  }

  if (*string == '\0'){
    return NULL;
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

  n = DFSCH_MAKE_FIXNUM(0);

  while (*string && *string != '/'){
    d = dig_val[*string];
    if (d >= base){
      return NULL;
    }
    n = dfsch_number_mul(n, DFSCH_MAKE_FIXNUM(base));
    n = dfsch_number_add(n, DFSCH_MAKE_FIXNUM(d));
    string++;
  }

  if (*string == '/'){
    dfsch_object_t* t;
    string++;
    if (strchr(string, '/') != NULL){
      return NULL;
    }
    t = dfsch_make_number_from_string_noerror(string, obase);
    if (!t){
      return NULL;
    }
    n = dfsch_number_div(n, t);
  }

  if (negative){
    return dfsch_number_neg(n);
  } else {
    return n;
  }
}

dfsch_object_t* dfsch_make_number_from_string(char* string, int obase){
  dfsch_object_t* res = dfsch_make_number_from_string_noerror(string, obase);
  if (!res){
    dfsch_error("Number syntax invalid", dfsch_make_string_cstr(string));
  }
  return res;
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
  dfsch_error("Not a  real number", n);
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
      dfsch_error("Value too large", NULL);
    }

    return r;
  }
  dfsch_error("Not an integer", n);
}

unsigned long dfsch_number_to_ulong(dfsch_object_t *n){
  int64_t r;

  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    if (DFSCH_FIXNUM_REF(n) < 0){
      dfsch_error("Non-negative value expected", n);
    }
    return DFSCH_FIXNUM_REF(n);
  } else if (DFSCH_TYPE_OF(n)==DFSCH_BIGNUM_TYPE){
    if (!dfsch_bignum_to_int64((dfsch_bignum_t*)n, &r) || 
        r > ULONG_MAX){
      dfsch_error("Value too large", n);
    }

    if (r < 0){
      dfsch_error("Non-negative value expected", n);
    }


    return r;
  }
  dfsch_error("Not an integer", n);
}


int64_t dfsch_number_to_int64(dfsch_object_t *n){
  int64_t r;

  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    return DFSCH_FIXNUM_REF(n);
  } else if (DFSCH_TYPE_OF(n)==DFSCH_BIGNUM_TYPE){
    if (!dfsch_bignum_to_int64((dfsch_bignum_t*)n, &r)){
      dfsch_error("Value too large", NULL);
    }

    return r;
  }
  dfsch_error("Not an integer", n);
}
uint64_t dfsch_number_to_uint64(dfsch_object_t *n){
  uint64_t r;

  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    return DFSCH_FIXNUM_REF(n);
  } else if (DFSCH_TYPE_OF(n)==DFSCH_BIGNUM_TYPE){
    if (!dfsch_bignum_to_uint64((dfsch_bignum_t*)n, &r)){
      dfsch_error("Value too large", NULL);
    }

    return r;
  }
  dfsch_error("Not an integer", n);
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

  if (i == 0){
    return "0";
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

  return dfsch_object_2_string(n, 1, DFSCH_WRITE); /* fallback */
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

  if (DFSCH_TYPE_OF(a) == DFSCH_FRACNUM_TYPE &&
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
      return (nn < 0) ? - 1 : 1;      
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

dfsch_object_t* dfsch_number_shr(dfsch_object_t* n, size_t count){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    return DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_REF(n) >> count);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_to_number(dfsch_bignum_shr(n, count));
  } else {
    dfsch_error("Not an integer", n);
  } 
}

dfsch_object_t* dfsch_number_shl(dfsch_object_t* n, size_t count){
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    int64_t x = DFSCH_FIXNUM_REF(n) << count;
    return dfsch_make_number_from_int64(x);
  } else if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return dfsch_bignum_to_number(dfsch_bignum_shl(n, count));
  } else {
    dfsch_error("Not an integer", n);
  }
 
}


// Arithmetics

dfsch_object_t* dfsch_number_add(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (DFSCH_LIKELY(DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
                   DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE)){
    long an = DFSCH_FIXNUM_REF(a);
    long bn = DFSCH_FIXNUM_REF(b);
    long x = an + bn;
      
    if (x <= DFSCH_FIXNUM_MAX && x >= DFSCH_FIXNUM_MIN){
      return DFSCH_MAKE_FIXNUM(x);
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
  if (DFSCH_LIKELY(DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
                   DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE)){
    long an = DFSCH_FIXNUM_REF(a);
    long bn = DFSCH_FIXNUM_REF(b);
    long x = an - bn;

    if (x <= DFSCH_FIXNUM_MAX && x >= DFSCH_FIXNUM_MIN){
      return DFSCH_MAKE_FIXNUM(x);
    }
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
    long an = DFSCH_FIXNUM_REF(a)<<3;
    long bn = DFSCH_FIXNUM_REF(b);
    long x = an * bn;
    double xd = (double)an * (double)bn;
    
    if (x == xd){
      return DFSCH_MAKE_FIXNUM(x>>3);
    }else{
      double d = x > xd ? x - xd : xd - x;
      double p = xd >= 0 ? xd : -xd;
      
      if (32.0 * d <= p){
	return DFSCH_MAKE_FIXNUM(x>>3);
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
      dfsch_error("Division by zero", NULL);

    return dfsch_make_number_from_double(an / bn); 
  }

  if (b == DFSCH_MAKE_FIXNUM(0)){
    dfsch_error("Division by zero", NULL);
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
    dfsch_error("Division by zero", NULL);
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
      dfsch_error("Division by zero", NULL);
    
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
  
  a = dfsch_number_abs(a);
  b = dfsch_number_abs(b);

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
  return dfsch_number_div_i(dfsch_number_abs(dfsch_number_mul(a, b)),
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
    return DFSCH_MAKE_FIXNUM(~DFSCH_FIXNUM_REF(a));
  }
  return 
    dfsch_bignum_to_number(dfsch_bignum_lognot(dfsch_bignum_from_number(a)));
}

dfsch_object_t* dfsch_number_exp(dfsch_object_t* b, 
                                 dfsch_object_t* e, 
                                 dfsch_object_t* m){
  dfsch_bignum_t* bb;
  dfsch_bignum_t* eb;
  dfsch_bignum_t* mb;
  
  bb = dfsch_bignum_from_number(b);
  eb = dfsch_bignum_from_number(e);
  if (m){
    mb = dfsch_bignum_from_number(m);
  } else {
    mb = NULL;
  }


  return dfsch_bignum_to_number(dfsch_bignum_exp(bb, eb, mb));
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

  dfsch_object_t* a;
  dfsch_object_t* x;
  dfsch_object_t* max;
  dfsch_object_t* n_1;
  dfsch_object_t* d;

  int s;
  int r;

  if (dfsch_number_negative_p(n)){
    return (n == DFSCH_MAKE_FIXNUM(-1));
  }
 

  if (dfsch_number_even_p(n)){
    return (n == DFSCH_MAKE_FIXNUM(2));
  }

  if (n == DFSCH_MAKE_FIXNUM(1)){
    return 0;
  }

  for (i = 0; i < sizeof(small_primes)/sizeof(int); i++){
      if (dfsch_number_equal_p(n, DFSCH_MAKE_FIXNUM(small_primes[i]))){
        return 1;
      }
      if (dfsch_number_zero_p(dfsch_number_mod(n, 
                                               DFSCH_MAKE_FIXNUM(small_primes[i])))){
        return 0;
    }
  }

  max = dfsch_number_sub(n, DFSCH_MAKE_FIXNUM(3));
  n_1 = dfsch_number_sub(n, DFSCH_MAKE_FIXNUM(1));
  s = dfsch_number_lsb(n_1);
  d = dfsch_number_shr(n_1, s);
  

  for (i = 0; i < 5; i++){
    a = dfsch_random_get_number(NULL, max);
    a = dfsch_number_add(a, DFSCH_MAKE_FIXNUM(2));

    x = dfsch_number_exp(a, d, n);

    if (x != DFSCH_MAKE_FIXNUM(1) && !dfsch_number_equal_p(x, n_1)){

      for (r = 0; r < (s - 1); r++){
        x = dfsch_number_mod(dfsch_number_mul(x, x), n);
        
        if (x == DFSCH_MAKE_FIXNUM(1)){
          return 0;
        }
        if (dfsch_number_equal_p(x, n_1)){
          break;
        }
      }
      
      if (!dfsch_number_equal_p(x, n_1)){
        return 0;
      }
    }
  }
  
  return 1;
}

dfsch_object_t* dfsch_number_next_prime(dfsch_object_t* n){
  if (dfsch_number_cmp(n, DFSCH_MAKE_FIXNUM(2)) < 0){
    return DFSCH_MAKE_FIXNUM(2);
  }

  if (n == DFSCH_MAKE_FIXNUM(2)){
    return DFSCH_MAKE_FIXNUM(3);
  }

  if (dfsch_number_even_p(n)){
    n = dfsch_number_add(n, DFSCH_MAKE_FIXNUM(1));
    if (dfsch_number_prime_p(n)){
      return n;
    }
  }

  do {
    n = dfsch_number_add(n, DFSCH_MAKE_FIXNUM(2));
  } while (!dfsch_number_prime_p(n));

  return n;
}

dfsch_object_t* dfsch_number_factorize(dfsch_object_t* n){
  dfsch_object_t* factors = NULL;
  dfsch_object_t* p = DFSCH_MAKE_FIXNUM(2);

  if (dfsch_number_zero_p(n)){
    return dfsch_cons(DFSCH_MAKE_FIXNUM(0), NULL);
  }

  if (dfsch_number_negative_p(n)){
    factors = dfsch_cons(DFSCH_MAKE_FIXNUM(-1), NULL);
    n = dfsch_number_neg(n);
  }

  while (n != DFSCH_MAKE_FIXNUM(1)){
    while (dfsch_number_zero_p(dfsch_number_mod(n, p))){
      n = dfsch_number_div(n, p);
      factors = dfsch_cons(p, factors);
    }
    p = dfsch_number_next_prime(p);
  }

  return factors;
}

/************** Number sequences ***************/

typedef struct {
  dfsch_type_t* type;
  dfsch_object_t* from;
  dfsch_object_t* to;
  dfsch_object_t* step;
  int cond;
} number_sequence_t;

static int ns_is_in_p(number_sequence_t* ns, dfsch_object_t* res){
  if (!ns->to){
    return 1;
  }
  return ns->cond == dfsch_number_cmp(res, ns->to);
}

static dfsch_object_t* ns_get_iterator(number_sequence_t* ns){
  number_sequence_t* i = dfsch_make_object(DFSCH_NUMBER_SEQUENCE_ITERATOR_TYPE);
  i->from = ns->from;
  i->to = ns->to;
  i->step = ns->step;
  i->cond = ns->cond;
  return i;
}

static dfsch_object_t* ns_ref(number_sequence_t* ns, size_t k){
  dfsch_object_t* res = dfsch_number_add(ns->from, 
                                         dfsch_number_mul(ns->step, 
                                                          dfsch_make_number_from_long(k)));

  if (!ns_is_in_p(ns, res)){
    dfsch_error("Index out of bounds", dfsch_make_number_from_long(k));
  }

  return res;
}

static dfsch_collection_methods_t ns_collection = {
  .get_iterator = ns_get_iterator,
};

static dfsch_sequence_methods_t ns_sequence = {
  .ref = ns_ref,
};

dfsch_type_t dfsch_number_sequence_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "number-sequence",
  .size = sizeof(number_sequence_t),
  .collection = &ns_collection,
  .sequence = &ns_sequence
};

static dfsch_object_t* nsi_this(number_sequence_t* ns){
  return ns->from;
}

static dfsch_object_t* nsi_next(number_sequence_t* ns){
  ns->from = dfsch_number_add(ns->from, ns->step);
  if (!ns_is_in_p(ns, ns->from)){
    return NULL;
  }
  return ns;
}

static dfsch_iterator_methods_t nsi_iterator = {
  .next = nsi_next,
  .this = nsi_this
};

dfsch_type_t dfsch_number_sequence_iterator_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "number-sequence",
  .size = sizeof(number_sequence_t),
  .collection = &ns_collection,
  .sequence = &ns_sequence,
  .iterator = &nsi_iterator,
};

dfsch_object_t* dfsch_make_number_sequence(dfsch_object_t* from,
                                           dfsch_object_t* to,
                                           dfsch_object_t* step){
  number_sequence_t* ns = dfsch_make_object(DFSCH_NUMBER_SEQUENCE_TYPE);
  ns->from = from;
  ns->to = to;
  ns->step = step;

  if (dfsch_number_negative_p(step)){
    ns->cond = 1;
  } else {
    ns->cond = -1;
  }

  if (!ns_is_in_p(ns, ns->from)){
    return NULL;
  }

  return ns;
}



/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////


DFSCH_DEFINE_PRIMITIVE_EX(number_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE_EX(real_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_real_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE_EX(rational_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_rational_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE_EX(integer_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_integer_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE_EX(exact_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_exact_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE_EX(inexact_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(!dfsch_number_exact_p(obj));  
}
DFSCH_DEFINE_PRIMITIVE_EX(exact_2_inexact, 0, DFSCH_PRIMITIVE_PURE){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  return dfsch_number_to_inexact(n);
}

DFSCH_DEFINE_PRIMITIVE_EX(plus, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_add(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE_EX(minus, NULL, DFSCH_PRIMITIVE_PURE){
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
DFSCH_DEFINE_PRIMITIVE_EX(mult, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* i = args;
  object_t* s = DFSCH_MAKE_FIXNUM(1);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_mul(s,DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE_EX(slash, NULL, DFSCH_PRIMITIVE_PURE){
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
DFSCH_DEFINE_PRIMITIVE_EX(slash_i, NULL, DFSCH_PRIMITIVE_PURE){
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
DFSCH_DEFINE_PRIMITIVE_EX(modulo, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(number_equal, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* z0, *z1;
  DFSCH_OBJECT_ARG(args, z0);
  DFSCH_OBJECT_ARG(args, z1);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_equal_p(z0,z1));
}

DFSCH_DEFINE_PRIMITIVE_EX(lt, NULL, DFSCH_PRIMITIVE_PURE){
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
DFSCH_DEFINE_PRIMITIVE_EX(gt, NULL, DFSCH_PRIMITIVE_PURE){
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
DFSCH_DEFINE_PRIMITIVE_EX(lte, NULL, DFSCH_PRIMITIVE_PURE){
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
DFSCH_DEFINE_PRIMITIVE_EX(gte, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(logand, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(-1); /* all ones */

  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logand(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE_EX(logtest, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(-1); /* all ones */

  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logand(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return dfsch_bool(s != DFSCH_MAKE_FIXNUM(0)); 
}
DFSCH_DEFINE_PRIMITIVE_EX(logior, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logior(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE_EX(logxor, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* i = args;
  dfsch_object_t* s = DFSCH_MAKE_FIXNUM(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_logxor(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE_EX(lognot, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_number_lognot(n);
}


// Functions

DFSCH_DEFINE_PRIMITIVE_EX(abs, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  
  return dfsch_number_abs(n);
}

DFSCH_DEFINE_PRIMITIVE_EX(exp, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG_OPT(args, z, 1.0);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(exp(z));
}

DFSCH_DEFINE_PRIMITIVE_EX(log, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(sin, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(sin(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(cos, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(cos(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(tan, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(tan(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(sinh, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(sinh(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(cosh, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(cosh(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(tanh, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(tanh(z));
}

DFSCH_DEFINE_PRIMITIVE_EX(asin, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(acos, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(atan, NULL, DFSCH_PRIMITIVE_PURE){
  double z0, z1;
  DFSCH_DOUBLE_ARG(args, z0);
  DFSCH_DOUBLE_ARG_OPT(args, z1, 1.0);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(atan(z0/z1));
}

DFSCH_DEFINE_PRIMITIVE_EX(sqrt, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(expt, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(zero_p, NULL, DFSCH_PRIMITIVE_PURE){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_sign(n) == 0);
}

DFSCH_DEFINE_PRIMITIVE_EX(positive_p, NULL, DFSCH_PRIMITIVE_PURE){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_sign(n) > 0);
}

DFSCH_DEFINE_PRIMITIVE_EX(negative_p, NULL, DFSCH_PRIMITIVE_PURE){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_sign(n) < 0);
}

DFSCH_DEFINE_PRIMITIVE_EX(even_p, NULL, DFSCH_PRIMITIVE_PURE){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_even_p(n));
}

DFSCH_DEFINE_PRIMITIVE_EX(odd_p, NULL, DFSCH_PRIMITIVE_PURE){
  dfsch_object_t* n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  return dfsch_bool(dfsch_number_odd_p(n));
}

DFSCH_DEFINE_PRIMITIVE_EX(max, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(min, NULL, DFSCH_PRIMITIVE_PURE){
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

DFSCH_DEFINE_PRIMITIVE_EX(round, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(round(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(floor, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(floor(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(ceiling, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(ceil(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(truncate, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(trunc(z));
}

DFSCH_DEFINE_PRIMITIVE_EX(round_i, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_int64(round(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(floor_i, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_int64(floor(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(ceiling_i, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_int64(ceil(z));
}
DFSCH_DEFINE_PRIMITIVE_EX(truncate_i, NULL, DFSCH_PRIMITIVE_PURE){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_int64(trunc(z));
}


DFSCH_DEFINE_PRIMITIVE_EX(number_2_string, NULL, DFSCH_PRIMITIVE_PURE){
  object_t *n;
  long base;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_LONG_ARG_OPT(args, base, 10);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_number_to_string(n, base));
}
DFSCH_DEFINE_PRIMITIVE_EX(string_2_number, NULL, DFSCH_PRIMITIVE_PURE){
  char *str;
  long base;

  DFSCH_STRING_ARG(args, str);
  DFSCH_LONG_ARG_OPT(args, base, 10);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_string(str, base);
}
DFSCH_DEFINE_PRIMITIVE_EX(gcd, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* a;
  object_t* b;

  if (!DFSCH_PAIR_P(args)){
    return DFSCH_MAKE_FIXNUM(0);
  }
  DFSCH_OBJECT_ARG(args, a);

  while (DFSCH_PAIR_P(args)){
    DFSCH_OBJECT_ARG(args, b);
    a = dfsch_number_gcd(a, b);
  }

  return a;
}
DFSCH_DEFINE_PRIMITIVE_EX(mod_inv, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* a;
  object_t* b;

  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  DFSCH_ARG_END(args);


  return dfsch_number_mod_inv(a, b);
}
DFSCH_DEFINE_PRIMITIVE_EX(lcm, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* a = DFSCH_MAKE_FIXNUM(1);
  object_t* b;

  while (DFSCH_PAIR_P(args)){
    DFSCH_OBJECT_ARG(args, b);
    a = dfsch_number_lcm(a, b);
  }

  return a;
}

DFSCH_DEFINE_PRIMITIVE_EX(lsb, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return DFSCH_MAKE_FIXNUM(dfsch_number_lsb(n));
}

DFSCH_DEFINE_PRIMITIVE_EX(msb, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return DFSCH_MAKE_FIXNUM(dfsch_number_msb(n));
}

DFSCH_DEFINE_PRIMITIVE_EX(inc, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return dfsch_number_add(n, DFSCH_MAKE_FIXNUM(1));
}
DFSCH_DEFINE_PRIMITIVE_EX(dec, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return dfsch_number_sub(n, DFSCH_MAKE_FIXNUM(1));
}

DFSCH_DEFINE_PRIMITIVE_EX(shr, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;
  size_t count;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_LONG_ARG_OPT(args, count, 1);
  DFSCH_ARG_END(args);


  return dfsch_number_shr(n, count);
}
DFSCH_DEFINE_PRIMITIVE_EX(shl, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;
  size_t count;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_LONG_ARG_OPT(args, count, 1);
  DFSCH_ARG_END(args);


  return dfsch_number_shl(n, count);
}
DFSCH_DEFINE_PRIMITIVE_EX(prime_p, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return dfsch_bool(dfsch_number_prime_p(n));
}

DFSCH_DEFINE_PRIMITIVE_EX(next_prime, NULL, DFSCH_PRIMITIVE_PURE){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return dfsch_number_next_prime(n);
}
DFSCH_DEFINE_PRIMITIVE(factorize, NULL){
  object_t* n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);


  return dfsch_number_factorize(n);
}

DFSCH_DEFINE_PRIMITIVE(make_number_sequence, NULL){
  dfsch_object_t* from = DFSCH_MAKE_FIXNUM(0);
  dfsch_object_t* to = NULL;
  dfsch_object_t* step = DFSCH_MAKE_FIXNUM(1);

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("from", from);
  DFSCH_KEYWORD("to", to);
  DFSCH_KEYWORD("step", step);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_number_sequence(from, to, step);
}


void dfsch__number_native_register(dfsch_object_t *ctx){
  dfsch_defcanon_cstr(ctx, "<number>", DFSCH_NUMBER_TYPE);
  dfsch_defcanon_cstr(ctx, "<real>", DFSCH_REAL_TYPE);
  dfsch_defcanon_cstr(ctx, "<rational>", DFSCH_RATIONAL_TYPE);
  dfsch_defcanon_cstr(ctx, "<integer>", DFSCH_INTEGER_TYPE);

  dfsch_defcanon_cstr(ctx, "<fixnum>", DFSCH_FIXNUM_TYPE);
  dfsch_defcanon_cstr(ctx, "<flonum>", DFSCH_FLONUM_TYPE);
  dfsch_defcanon_cstr(ctx, "<fracnum>", DFSCH_FRACNUM_TYPE);

  dfsch_defcanon_cstr(ctx, "+", DFSCH_PRIMITIVE_REF(plus));
  dfsch_defcanon_cstr(ctx, "-", DFSCH_PRIMITIVE_REF(minus));
  dfsch_defcanon_cstr(ctx, "*", DFSCH_PRIMITIVE_REF(mult));
  dfsch_defcanon_cstr(ctx, "/", DFSCH_PRIMITIVE_REF(slash));
  dfsch_defcanon_cstr(ctx, "/i", DFSCH_PRIMITIVE_REF(slash_i));
  dfsch_defcanon_cstr(ctx, "%", DFSCH_PRIMITIVE_REF(modulo));
  dfsch_defcanon_cstr(ctx, "=", DFSCH_PRIMITIVE_REF(number_equal));
  dfsch_defcanon_cstr(ctx, "<", DFSCH_PRIMITIVE_REF(lt));
  dfsch_defcanon_cstr(ctx, ">", DFSCH_PRIMITIVE_REF(gt));
  dfsch_defcanon_cstr(ctx, "<=", DFSCH_PRIMITIVE_REF(lte));
  dfsch_defcanon_cstr(ctx, ">=", DFSCH_PRIMITIVE_REF(gte));
  dfsch_defcanon_cstr(ctx, "number?", DFSCH_PRIMITIVE_REF(number_p));
  dfsch_defcanon_cstr(ctx, "real?", DFSCH_PRIMITIVE_REF(real_p));
  dfsch_defcanon_cstr(ctx, "rational?", DFSCH_PRIMITIVE_REF(rational_p));
  dfsch_defcanon_cstr(ctx, "integer?", DFSCH_PRIMITIVE_REF(integer_p));
  dfsch_defcanon_cstr(ctx, "exact?", DFSCH_PRIMITIVE_REF(exact_p));
  dfsch_defcanon_cstr(ctx, "inexact?", DFSCH_PRIMITIVE_REF(inexact_p));
  dfsch_defcanon_cstr(ctx, "exact->inexact", 
                      DFSCH_PRIMITIVE_REF(exact_2_inexact));

  dfsch_defcanon_cstr(ctx, "pi", 
                      dfsch_make_number_from_double(4*atan(1)));


  dfsch_defcanon_cstr(ctx, "abs", DFSCH_PRIMITIVE_REF(abs));

  dfsch_defcanon_cstr(ctx, "exp", DFSCH_PRIMITIVE_REF(exp));
  dfsch_defcanon_cstr(ctx, "log", DFSCH_PRIMITIVE_REF(log));
  dfsch_defcanon_cstr(ctx, "expt", DFSCH_PRIMITIVE_REF(expt));

  dfsch_defcanon_cstr(ctx, "sin", DFSCH_PRIMITIVE_REF(sin));
  dfsch_defcanon_cstr(ctx, "cos", DFSCH_PRIMITIVE_REF(cos));
  dfsch_defcanon_cstr(ctx, "tan", DFSCH_PRIMITIVE_REF(tan));

  dfsch_defconst_cstr(ctx, "sinh", DFSCH_PRIMITIVE_REF(sinh));
  dfsch_defconst_cstr(ctx, "cosh", DFSCH_PRIMITIVE_REF(cosh));
  dfsch_defconst_cstr(ctx, "tanh", DFSCH_PRIMITIVE_REF(tanh));

  dfsch_defcanon_cstr(ctx, "asin", DFSCH_PRIMITIVE_REF(asin));
  dfsch_defcanon_cstr(ctx, "acos", DFSCH_PRIMITIVE_REF(acos));
  dfsch_defcanon_cstr(ctx, "atan", DFSCH_PRIMITIVE_REF(atan));

  dfsch_defcanon_cstr(ctx, "sqrt", DFSCH_PRIMITIVE_REF(sqrt));

  dfsch_defcanon_cstr(ctx, "min", DFSCH_PRIMITIVE_REF(min));
  dfsch_defcanon_cstr(ctx, "max", DFSCH_PRIMITIVE_REF(max));

  dfsch_defcanon_cstr(ctx, "zero?", DFSCH_PRIMITIVE_REF(zero_p));
  dfsch_defcanon_cstr(ctx, "negative?", DFSCH_PRIMITIVE_REF(negative_p));
  dfsch_defcanon_cstr(ctx, "positive?", DFSCH_PRIMITIVE_REF(positive_p));

  dfsch_defcanon_cstr(ctx, "even?", DFSCH_PRIMITIVE_REF(even_p));
  dfsch_defcanon_cstr(ctx, "odd?", DFSCH_PRIMITIVE_REF(odd_p));

  dfsch_defcanon_cstr(ctx, "round", DFSCH_PRIMITIVE_REF(round));
  dfsch_defcanon_cstr(ctx, "floor", DFSCH_PRIMITIVE_REF(floor));
  dfsch_defcanon_cstr(ctx, "ceiling", DFSCH_PRIMITIVE_REF(ceiling));
  dfsch_defcanon_cstr(ctx, "truncate", DFSCH_PRIMITIVE_REF(truncate));

  dfsch_defcanon_cstr(ctx, "round*", DFSCH_PRIMITIVE_REF(round_i));
  dfsch_defcanon_cstr(ctx, "floor*", DFSCH_PRIMITIVE_REF(floor_i));
  dfsch_defcanon_cstr(ctx, "ceiling*", DFSCH_PRIMITIVE_REF(ceiling_i));
  dfsch_defcanon_cstr(ctx, "truncate*", DFSCH_PRIMITIVE_REF(truncate_i));

  dfsch_defcanon_cstr(ctx, "number->string", 
                      DFSCH_PRIMITIVE_REF(number_2_string));
  dfsch_defcanon_cstr(ctx, "string->number", 
                      DFSCH_PRIMITIVE_REF(string_2_number));
  dfsch_defcanon_cstr(ctx, "most-positive-fixnum",
                      DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_MAX));
  dfsch_defcanon_cstr(ctx, "most-negative-fixnum",
                      DFSCH_MAKE_FIXNUM(DFSCH_FIXNUM_MIN));

  dfsch_defcanon_cstr(ctx, "gcd", DFSCH_PRIMITIVE_REF(gcd));
  dfsch_defcanon_cstr(ctx, "mod-inv", DFSCH_PRIMITIVE_REF(mod_inv));
  dfsch_defcanon_cstr(ctx, "lcm", DFSCH_PRIMITIVE_REF(lcm));

  dfsch_defcanon_cstr(ctx, "logand", DFSCH_PRIMITIVE_REF(logand));
  dfsch_defcanon_cstr(ctx, "logtest", DFSCH_PRIMITIVE_REF(logtest));
  dfsch_defcanon_cstr(ctx, "logior", DFSCH_PRIMITIVE_REF(logior));
  dfsch_defcanon_cstr(ctx, "logxor", DFSCH_PRIMITIVE_REF(logxor));
  dfsch_defcanon_cstr(ctx, "lognot", DFSCH_PRIMITIVE_REF(lognot));

  dfsch_defcanon_cstr(ctx, "lsb", DFSCH_PRIMITIVE_REF(lsb));
  dfsch_defcanon_cstr(ctx, "msb", DFSCH_PRIMITIVE_REF(msb));
 
  dfsch_defcanon_cstr(ctx, "1+", DFSCH_PRIMITIVE_REF(inc));
  dfsch_defcanon_cstr(ctx, "1-", DFSCH_PRIMITIVE_REF(dec));

  dfsch_defcanon_cstr(ctx, ">>", DFSCH_PRIMITIVE_REF(shr));
  dfsch_defcanon_cstr(ctx, "<<", DFSCH_PRIMITIVE_REF(shl));

  dfsch_defcanon_cstr(ctx, "prime?", DFSCH_PRIMITIVE_REF(prime_p));
  dfsch_defcanon_cstr(ctx, "next-prime", DFSCH_PRIMITIVE_REF(next_prime));
  dfsch_defcanon_cstr(ctx, "factorize", DFSCH_PRIMITIVE_REF(factorize));

  dfsch_defcanon_cstr(ctx, "<number-sequence>", 
                      DFSCH_NUMBER_SEQUENCE_TYPE);

  dfsch_defcanon_cstr(ctx, "make-number-sequence", 
                      DFSCH_PRIMITIVE_REF(make_number_sequence));
 
}
