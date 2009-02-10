/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Bignums
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

#include "dfsch/number.h"
#include "dfsch/bignum.h"

#include <dfsch/dfsch.h>
#include "util.h"
#include "internal.h"

#ifdef DFSCH_32BIT_BIGNUM
#define WORD_BITS 31
typedef uint32_t word_t;
typedef uint64_t dword_t;
typedef int64_t sword_t;
#else
#define WORD_BITS 15
typedef uint16_t word_t;
typedef uint32_t dword_t;
typedef int32_t sword_t;
#endif


#define WORD_BASE (1 << WORD_BITS)
#define WORD_MASK (WORD_BASE - 1)


struct dfsch_bignum_t {
  dfsch_type_t* type;
  int negative;
  size_t length;
  word_t words[];
};

#define UINT64_DIGITS (64 / WORD_BITS + 1)


typedef dfsch_bignum_t bignum_t;

static uint32_t bignum_hash(bignum_t* n){
  uint32_t r;
  size_t i;
  if (n->negative){
    r = 0x12345678;
  } else {
    r = 0x87654321;
  }
  r ^= n->length;
  for (i = 0; i < n->length; i++){
    r ^= (r << WORD_BITS) + n->words[i] + (r >> 3);
  }
  r ^= n->length;
  return r;
}

int dfsch_bignum_equal_p(bignum_t* a, bignum_t* b){
  if (a->length != b->length){
    return 0;
  }
  if (a->length != 0 && a->negative != b->negative){
    return 0;
  }
  return memcmp(a->words, b->words, a->length*sizeof(word_t)) == 0;
}


static void bignum_write(bignum_t* b, dfsch_writer_state_t* state){
  dfsch_write_string(state, dfsch_bignum_to_string(b, 10));
}

dfsch_number_type_t dfsch_bignum_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_INTEGER_TYPE,
  sizeof(bignum_t),
  "bignum",
  (dfsch_type_equal_p_t)dfsch_bignum_equal_p,
  (dfsch_type_write_t)bignum_write,
  NULL,
  (dfsch_type_hash_t)bignum_hash
};

static void normalize_bignum(bignum_t* n){
  while (n->length > 0 && n->words[n->length-1] == 0){
    n->length--;
  }
}
static bignum_t* make_bignum(size_t length){
  bignum_t* b = (bignum_t*) dfsch_make_object_var(DFSCH_BIGNUM_TYPE,
                                                  length * sizeof(word_t));
  b->length = length;
  return b;
}
static bignum_t* make_bignum_digit(word_t d){
  bignum_t* res = make_bignum(1);
  res->negative = 0;
  res->words[0] = d;
  normalize_bignum(res);
  return res;
}
static bignum_t* copy_bignum(bignum_t* s){
  bignum_t* b = make_bignum(s->length);
  b->negative = s->negative;
  memcpy(b->words, s->words, sizeof(word_t)*s->length);
  return b;
}


bignum_t* dfsch_make_bignum_uint64(uint64_t n){
  int i;
  bignum_t* b = make_bignum(UINT64_DIGITS);

  for (i=0; i < UINT64_DIGITS; i++){
    b->words[i] = n & WORD_MASK;
    n >>= WORD_BITS;
  }
  
  normalize_bignum(b);

  return b;
}

bignum_t* dfsch_make_bignum_int64(int64_t n){
  bignum_t* b;
  if (n < 0){
    b = dfsch_make_bignum_uint64(-n);
    b->negative = 1;
  } else {
    b = dfsch_make_bignum_uint64(n);
    b->negative = 0;
  }
  return b;
}

static int word_mag(word_t w){
  int i = 0;
  while (w){
    i++;
    w >>= 1;
  }
  return i;
}

int dfsch_bignum_to_uint64(dfsch_bignum_t* b, uint64_t* rp){
  uint64_t r = 0;
  int i;

  if (b->length == 0){
    return 1;
    if (rp){
      *rp = 0;
    }
  }
  if ((b->length-1)*WORD_BITS + word_mag(b->words[b->length - 1]) > 64 ){
    return 0;
  }
  
  if (rp){
    if (UINT64_DIGITS > b->length + 1){
      i = b->length + 1;
    } else {
      i = UINT64_DIGITS;
    }

    for (; i > 0; i--){
      r <<= WORD_BITS;
      r |= b->words[i-1] & WORD_MASK;
    }
    
    *rp = r;
  }
  return 1;
}

int dfsch_bignum_to_int64(bignum_t* b, int64_t* rp){
  uint64_t t;
  if (!dfsch_bignum_to_uint64(b, &t)){
    return 0;
  }
  if (b->negative){
    if (t > ((uint64_t)INT64_MAX) + 1){
      return 0;
    }
    if (rp){
      *rp = -t;
    }
    return 1;
  } else {
    if (t > INT64_MAX){
      return 0;
    }
    if (rp){
      *rp = t;
    }
    return 1;
  }
}

double dfsch_bignum_to_double(dfsch_bignum_t* b){
  double r = 0;
  int i;

  for (i = b->length+1; i > 0; i--){
    r *= WORD_BASE;
    r += b->words[i-1] & WORD_MASK;
  }

  return r;
}


dfsch_bignum_t* dfsch_bignum_from_number(dfsch_object_t* n){
  if (DFSCH_TYPE_OF(n) == DFSCH_BIGNUM_TYPE){
    return (dfsch_bignum_t*) n;
  }
  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    return dfsch_make_bignum_int64(DFSCH_FIXNUM_REF(n));
  }
  dfsch_error("Not an integer", n);
}


int dfsch_bignum_cmp_abs(bignum_t* a, bignum_t* b){
  size_t i;
  if (a->length < b->length){
    return -1;
  }
  if (a->length > b->length){
    return 1;
  }
  for (i = a->length; i > 0; i--){
    if (a->words[i-1] != b->words[i-1]){
      if (a->words[i-1] < b->words[i-1]){
        return -1;
      } else {
        return 1;
      }
    }
  }
  return 0;
}
int dfsch_bignum_cmp(bignum_t* a, bignum_t* b){
  size_t i;
  if (a->negative && !b->negative){
    return -1;
  }
  if (b->negative && !a->negative){
    return 1;
  }
  if (a->length < b->length){
    return a->negative ? 1 : -1;
  }
  if (a->length > b->length){
    return a->negative ? -1 : 1;
  }
  for (i = a->length; i > 0; i--){
    if (a->words[i-1] != b->words[i-1]){
      if (a->words[i-1] < b->words[i-1]){
        return a->negative ? 1 : -1;
      } else {
        return a->negative ? -1 : 1;
      }
    }
  }
  return 0;
}
int dfsch_bignum_sign(dfsch_bignum_t* a){
  if (a->length == 0){
    return 0;
  } else {
    return a->negative ? -1 : 1;
  }
}
int dfsch_bignum_even_p(dfsch_bignum_t* a){
  if (a->length == 0){
    return 1;
  }
  return a->words[0] & 0x01 == 0;
}

static size_t bignum_num_bits(bignum_t* b){
  return b->length * WORD_BITS;
}

static int bignum_get_bit(bignum_t* b, size_t n){
  return (b->words[n / WORD_BITS] >> (n % WORD_BITS)) & 0x01;
}
static word_t bignum_get_word(bignum_t* b, size_t n){
  if (n % WORD_BITS < WORD_BITS - 8){
    return (b->words[n / WORD_BITS] >> (n % WORD_BITS)) & WORD_MASK;
  } else if (n / WORD_BITS + 1 > b->length) {
    return (b->words[n / WORD_BITS] >> (n % WORD_BITS)) & WORD_MASK;
  } else {
    return (b->words[n / WORD_BITS] >> (n % WORD_BITS)) |
      (b->words[n / WORD_BITS +1] << (WORD_BITS - n % WORD_BITS)) & 0xff;
  }
}
static void bignum_set_word(bignum_t* b, size_t n, word_t v, word_t m){
  if (n % WORD_BITS < WORD_BITS - 8){
    b->words[n / WORD_BITS] = 
      ((b->words[n / WORD_BITS] & 
        ~(m << (n % WORD_BITS))) | 
       (v << (n % WORD_BITS))) & WORD_MASK;
  } else if (n / WORD_BITS + 1 > b->length) {
    b->words[n / WORD_BITS] = 
      ((b->words[n / WORD_BITS] & 
        ~(m << (n % WORD_BITS))) | 
       (v << (n % WORD_BITS))) & WORD_MASK;
  } else {
    b->words[n / WORD_BITS] = 
      ((b->words[n / WORD_BITS] & 
        ~(0xff << (n % WORD_BITS))) | 
       (v << (n % WORD_BITS))) & WORD_MASK;
    b->words[n / WORD_BITS + 1] = 
      ((b->words[n / WORD_BITS + 1] & 
        ~(m >> (WORD_BITS - n % WORD_BITS))) | 
       (v >> (WORD_BITS - n % WORD_BITS))) & WORD_MASK;
  }
}


static dfsch_bignum_t* bignum_add_abs(bignum_t* a, bignum_t* b){
  bignum_t* res;
  bignum_t* tmp;
  size_t i;
  word_t cy;

  if (a->length < b->length){
    tmp = a;
    a = b;
    b = tmp;
  }
  
  res = make_bignum(a->length + 1);

  cy = 0;
  i = 0;

  while (i < b->length){
    cy >>= WORD_BITS;
    cy += a->words[i] + b->words[i];
    res->words[i] = cy & WORD_MASK;
    i++;
  }

  while (i < a->length){
    cy >>= WORD_BITS;
    cy += a->words[i];
    res->words[i] = cy & WORD_MASK; 
    i++;
  }

  res->words[i] = cy >> WORD_BITS;
  normalize_bignum(res);
  return res;
}
static bignum_t* bignum_sub_abs(bignum_t* a, bignum_t* b){
  bignum_t* res;
  bignum_t* tmp;
  int res_negative;
  size_t i;
  word_t cy;

  res_negative = 0;

  if (a->length < b->length){
    tmp = a;
    a = b;
    b = tmp;
    res_negative = 1;
  } else if (a->length == b->length) {
    switch (dfsch_bignum_cmp_abs(a, b)) {
    case -1: 
      tmp = a;
      a = b;
      b = tmp;
      res_negative = 1;
      break;
    case 0:
      return make_bignum(0);
    case 1:
      break;
    }
  }

  res = make_bignum(a->length);
  res->negative = res_negative;

  cy = 0;
  i = 0;

  while (i < b->length){
    cy >>= WORD_BITS;
    cy &= 1;
    cy = a->words[i] - b->words[i] - cy;
    res->words[i] = cy & WORD_MASK;
    i++;
  }

  while (i < a->length){
    cy >>= WORD_BITS;
    cy &= 1;
    cy = a->words[i] - cy;
    res->words[i] = cy & WORD_MASK; 
    i++;
  }

  normalize_bignum(res);
  return res;
}


bignum_t* dfsch_bignum_add(bignum_t* a, bignum_t* b){
  bignum_t* res;
  if (a->negative == b->negative){
    res = bignum_add_abs(a, b);
    res->negative = a->negative;
  } else if (a->negative){
    res = bignum_sub_abs(b, a);
  } else {
    res = bignum_sub_abs(a, b);
  }

  return res;
}

bignum_t* dfsch_bignum_sub(bignum_t* a, bignum_t* b){
  bignum_t* res;

  if (a->negative == b->negative){
    res = bignum_sub_abs(a, b);
    if (a->negative){
      res->negative = !res->negative;
    }
  } else {
    res = bignum_add_abs(a, b);
    res->negative = a->negative;
  }


  return res;
}
dfsch_bignum_t* dfsch_bignum_neg(dfsch_bignum_t* a){
  bignum_t* r = copy_bignum(a);
  r->negative = ! a->negative;
  return r;
}
dfsch_bignum_t* dfsch_bignum_abs(dfsch_bignum_t* a){
  if (a->negative){
    return dfsch_bignum_neg(a);
  } else {
    return a;
  }
}

/*
 * Long multiplication (HAC 14.12, elementary school :))
 * O(n^2), but simple and works
 */
dfsch_bignum_t* dfsch_bignum_mul(bignum_t* a, bignum_t* b){
  bignum_t* res;
  size_t i;
  size_t j;
  dword_t cy;

  res = make_bignum(a->length + b->length);
  for (i = 0; i < a->length; i++){
    cy = 0;
    for (j = 0; j < b->length; j++){
      cy = res->words[i+j] + a->words[i] * b->words[j] + cy;
      res->words[i+j] = cy & WORD_MASK;
      cy >>= WORD_BITS;
    }
    res->words[i+j] += cy;
    
  }

  res->negative = !(a->negative == b->negative);

  normalize_bignum(res);
  return res;
}

static bignum_t* bignum_muladd_digit(bignum_t* a, word_t q, word_t d){
  size_t i;
  dword_t cy;
  bignum_t* res;

  res = make_bignum(a->length+1);
  
  cy = d;
  for (i = 0; i < a->length; i++){
    cy >>= WORD_BITS;
    cy = q * a->words[i] + cy;
    res->words[i] = cy & WORD_MASK;
  }
  res->words[a->length] = cy >> WORD_BITS;

  res->negative = a->negative;

  normalize_bignum(res);
  return res;
}

static bignum_t* bignum_add_abs_digit(bignum_t* a, word_t d){
  size_t i;
  dword_t cy;
  bignum_t* res;

  if (a->length == 0){
    return make_bignum_digit(d);
  }

  res = make_bignum(a->length+1);
  
  cy = a->words[0] + d;
  res->words[0] = cy;
  for (i = 1; i < a->length; i++){
    cy >>= WORD_BITS;
    cy = a->words[i] + cy;
    res->words[i] = cy & WORD_MASK;
  }
  res->words[a->length] = cy >> WORD_BITS;

  res->negative = a->negative;

  normalize_bignum(res);
  return res;
  
}
static bignum_t* bignum_sub_abs_digit(bignum_t* a, word_t d){
  size_t i;
  dword_t cy;
  bignum_t* res;

  if (a->length == 0){
    return make_bignum_digit(d);
  }

  res = make_bignum(a->length+1);
  
  cy = a->words[0] - d;
  res->words[0] = cy;
  for (i = 1; i < a->length; i++){
    cy >>= WORD_BITS;
    cy &= 1;
    cy = a->words[i] - cy;
    res->words[i] = cy & WORD_MASK;
  }
  res->words[a->length] = (cy >> WORD_BITS) & 1;

  res->negative = a->negative;

  normalize_bignum(res);
  return res;
  
}

static bignum_t* bignum_shl_words(bignum_t* b, size_t count){
  bignum_t* res;

  res = make_bignum(b->length+count);
  memcpy(res->words + count, b->words, b->length*sizeof(word_t));

  return res;
}
static bignum_t* bignum_shr_words(bignum_t* b, size_t count){
  bignum_t* res;

  if (b->length <= count){
    return make_bignum(0);
  }

  res = make_bignum(b->length - count);
  memcpy(res->words, b->words + count, (b->length - count)*sizeof(word_t));

  return res;
}

static void bignum_div_digit(bignum_t* a, word_t b,
                             bignum_t**qp, word_t* rp){
  bignum_t* q = make_bignum(a->length);
  dword_t r = 0;
  word_t d;
  size_t i;  

  for (i = a->length; i > 0; i--){
    r = (r << WORD_BITS) + a->words[i - 1];
    d = r / b;
    q->words[i - 1] = d & WORD_MASK;
    r -= d * b;
  }

  if (qp){
    normalize_bignum(q);
    *qp = q;
  }
  if (rp){
    *rp = (word_t)r;
  }
}

static void bignum_div_big(bignum_t* a, bignum_t* b, 
                           bignum_t**qp, bignum_t** rp){
  word_t l = (1 << WORD_BITS) / (b->words[b->length - 1] + 1);
  bignum_t* x = bignum_muladd_digit(a, l, 0);
  bignum_t* y = bignum_muladd_digit(b, l, 0);
  bignum_t* q;
  size_t i, j, k;
  word_t xi;
  dword_t d;
  sword_t cy;
  dword_t m;
  word_t u;

  j = x->length - y->length + 1;
  q = make_bignum(j);
  
  for (i = x->length; j > 0; --i, --j) {
    xi = (i >= x->length) ? 0 : x->words[i];

    if (xi == y->words[y->length - 1]){ /* 3.1 */
      d = WORD_MASK;
    
    } else {
      d = ((xi << WORD_BITS) + x->words[i - 1]) / y->words[y->length - 1];
    }

    while (y->words[y->length - 2] * d > 
           (((((dword_t)xi) << WORD_BITS) 
             + x->words[i - 1] 
             - d * y->words[y->length - 1]) << WORD_BITS)
           + x->words[i - 2]){ /* 3.2 */
      d--;
    }
    
    cy = 0;

    for (k = 0; k < y->length && k + j - 1 < x->length; k++){
      m = y->words[k] * d;
      u = m >> WORD_BITS;
      cy += x->words[k + j - 1] - m + (((dword_t)u) << WORD_BITS);
      x->words[k + j - 1] = cy & WORD_MASK;
#if -3 >> 1 == -1 // XXX
      cy = cy >> WORD_BITS;
#else
      if (cy < 0){
        cy = ~(~cy >> WORD_BITS);
      } else {
        cy = cy >> WORD_BITS;
      }
#endif
      cy -= u;
    }
    
    if (k + j - 1 < x->length){
      cy += x->words[k + j - 1];
      x->words[k + j - 1] = 0;
    }

    if (cy == 0){
      q->words[j - 1] = d;
    } else {
      if (cy != -1){
        abort();
      }
      q->words[j - 1] = d - 1;
      cy = 0;
      for (k = 0; k < y->length && k + j - 1 < x->length; k++){
        cy += x->words[k + j - 1] + y->words[k];
        x->words[k + j - 1] = cy & WORD_MASK;
#if -3 >> 1 == -1 // XXX
      cy = cy >> WORD_BITS;
#else
      if (cy < 0){
        cy = ~(~cy >> WORD_BITS);
      } else {
        cy = cy >> WORD_BITS;
      }
#endif
      }
    } 
  }

  if (qp){
    normalize_bignum(q);
    *qp = q;
  }
  if (rp){
    normalize_bignum(x);
    bignum_div_digit(x, l, rp, NULL);
  }
}
void dfsch_bignum_div(bignum_t* a, bignum_t* b, 
                      bignum_t**qp, bignum_t** rp){
  word_t wr;

  if (b->length == 0){
    dfsch_error("Division by zero", NULL);
  }
  if (a->length < b->length || 
      (a->length == b->length && 
       a->words[a->length - 1] < b->words[b->length - 1])){
    if (qp){
      *qp = make_bignum(0);
    }
    if (rp){
      *rp = a;
    }
    return;
  } 

  if (b->length == 1){
    bignum_div_digit(a, b->words[0], qp, &wr);
    if (rp){
      (*rp) = make_bignum_digit(wr);
    }
  } else {
    bignum_div_big(a, b, qp, rp);
  }

  if (rp){
    (*rp)->negative = a->negative;
  }
  if (qp){
    (*qp)->negative = a->negative != b->negative;
  }
}

bignum_t* dfsch_bignum_exp(bignum_t* b, bignum_t* e, bignum_t* m){
  bignum_t* r;
  size_t i;

  if (m && b->negative){
    dfsch_error("Negative base for modular exponentation", NULL);
  }
  if (m && m->length == 0){
    dfsch_error("Zero modulus", NULL);
  }

  r = make_bignum_digit(1);
  for (i = bignum_num_bits(e); i > 0; i--){
    r = dfsch_bignum_mul(r, r);
    if (bignum_get_bit(e, i-1)){
      r = dfsch_bignum_mul(r, b);
    }
    if (m){
      dfsch_bignum_div(r, m, NULL, &r);
    }
  }
  
  return r;
}

static bignum_t* logop(bignum_t* a, bignum_t* b, char op){
  /* Heavily inspired by python's long_bitwise() */
  word_t ma = 0;
  word_t mb = 0;
  bignum_t* r;
  int inv = 0;
  int i;

  if (a->negative){
    ma = WORD_MASK;
    a = dfsch_bignum_lognot(a);
  }
  if (b->negative){
    mb = WORD_MASK;
    b = dfsch_bignum_lognot(b);
  }
  
  switch (op){
  case '^':
    if (ma != mb){
      ma ^= WORD_MASK;
      inv = 1;
    }
    break;
  case '&':
    if (ma && mb){
      op = '|';
      ma ^= WORD_MASK;
      mb ^= WORD_MASK;
      inv = 1;
    }
    break;
  case '|':
    if (ma || mb){
      op = '&';
      ma ^= WORD_MASK;
      mb ^= WORD_MASK;
      inv = 1;
    }
    break;
  }

  r = make_bignum(a->length < b->length ? b->length : a->length);
  
  for (i = 0; i < r->length; i++){
    word_t wa = (i < a->length ? a->words[i] : 0) ^ ma;
    word_t wb = (i < b->length ? b->words[i] : 0) ^ mb;
    switch (op) {
    case '&': r->words[i] = wa & wb; break;
    case '|': r->words[i] = wa | wb; break;
    case '^': r->words[i] = wa ^ wb; break;
    }
  }

  normalize_bignum(r);

  if (inv){
    return dfsch_bignum_lognot(r);
  } else {
    return r;
  }

}

dfsch_bignum_t* dfsch_bignum_logand(bignum_t* a, bignum_t* b){
  return logop(a, b, '&');
}

dfsch_bignum_t* dfsch_bignum_logior(bignum_t* a, bignum_t* b){
  return logop(a, b, '|');
}

dfsch_bignum_t* dfsch_bignum_logxor(bignum_t* a, bignum_t* b){
  return logop(a, b, '^');
}
dfsch_bignum_t* dfsch_bignum_lognot(dfsch_bignum_t* a){
  bignum_t* r;
  if (a->negative){
    r = bignum_sub_abs_digit(a, 1);
  } else {
    r = bignum_add_abs_digit(a, 1);
  }
  r->negative = ! a->negative;
  return r;
}

dfsch_bignum_t* dfsch_bignum_shr(bignum_t* b, size_t count){
  bignum_t* r;
  size_t i;
  size_t bs = count % WORD_BITS;
  size_t ws = count / WORD_BITS;

  r = make_bignum(b->length - ws + 1);
  r->negative = b->negative;
  for (i = 0; i < b->length - ws - 1; i++){
    r->words[i] = ((b->words[i + ws] >> bs) | 
                   (b->words[i + ws + 1] << (WORD_BITS - bs))) & WORD_MASK;
  }
  r->words[b->length - ws] = (b->words[i + ws] >> bs) & WORD_MASK;
  normalize_bignum(r);
  return r;
}

static char* digits = "0123456789abcdefghijklmnopqrstuvwxyz";

static int get_digits(int base){
  int digs = 0;
  dword_t tmp = WORD_BASE;
  while (tmp){
    digs++;
    tmp /= base;
  }
  return digs;
}

char* dfsch_bignum_to_string(bignum_t* b, unsigned base){
  char* buf;
  bignum_t* r;
  word_t d;

  if (base == 0 || base > 36){
    dfsch_error("Invalid base", NULL);
  }

  if (b->length == 0){
    return "0";
  }

  buf = GC_MALLOC_ATOMIC(b->length * get_digits(base) + 2);  
  buf += b->length * get_digits(base) + 1;
  *(buf--) = 0;
  r = b;
 
  while (r->length > 0){
    bignum_div_digit(r, base, &r, &d);
    *(buf--) = digits[d];
  }
  
  if (b->negative){
    *buf = '-';
    return buf;
  } else {
    return buf+1;
  }
}

dfsch_strbuf_t* dfsch_bignum_to_bytes(dfsch_bignum_t* b){
  char* buf;
  size_t len;
  size_t i;
  len = bignum_num_bits(b) / 8 + 1;
  buf = GC_MALLOC_ATOMIC(len);
  
  for (i = 0; i < bignum_num_bits(b); i+=8){
    buf[len - 1 - (i >> 3)] = bignum_get_word(b, i);
  }

  while (len > 0 && *buf == 0){
    buf++;
    len--;
  }

  return dfsch_strbuf_create(buf, len);
}
dfsch_bignum_t* dfsch_bignum_from_bytes(uint8_t* buf, size_t len){
  bignum_t* b = make_bignum(len * (WORD_BITS / 8 + 1));
  size_t i;
  
  b->negative = 0;

  for (i = 0; i < len; i++){
    bignum_set_word(b, i << 3, buf[len - i - 1], 0xff);
  }
  
  normalize_bignum(b);

  return b;
}

dfsch_object_t* dfsch_bignum_to_number(dfsch_bignum_t* b){
  int64_t n;

  if (!dfsch_bignum_to_int64(b, &n)){
    return (dfsch_object_t*)b;
  }

  if (n > DFSCH_FIXNUM_MAX || n < DFSCH_FIXNUM_MIN){
    return (dfsch_object_t*)b;
  }

  return DFSCH_MAKE_FIXNUM(n);
}

static bignum_t* make_bignum_from_digits(dfsch_object_t* dl){
  size_t len;
  size_t i;
  bignum_t* b;
  len = dfsch_list_length_check(dl);
  b = make_bignum(len);
  i = len - 1;
  while (DFSCH_PAIR_P(dl)){
    b->words[i] = dfsch_number_to_long(DFSCH_FAST_CAR(dl)) & WORD_MASK;
    i--;
    dl = DFSCH_FAST_CDR(dl);
  }
  return b;
}

DFSCH_DEFINE_PRIMITIVE(integer_expt, 0){
  bignum_t* b;
  bignum_t* e;
  bignum_t* m;
  DFSCH_BIGNUM_ARG(args, b);
  DFSCH_BIGNUM_ARG(args, e);
  DFSCH_BIGNUM_ARG_OPT(args, m, NULL);
  DFSCH_ARG_END(args);

  return dfsch_bignum_to_number(dfsch_bignum_exp(b, e, m));
}

DFSCH_DEFINE_PRIMITIVE(bignum_2_bytes, 0){
  bignum_t* a;
  DFSCH_BIGNUM_ARG(args, a);
  DFSCH_ARG_END(args);

  return dfsch_make_string_strbuf(dfsch_bignum_to_bytes(a));
}
DFSCH_DEFINE_PRIMITIVE(bytes_2_bignum, 0){
  dfsch_strbuf_t* a;
  DFSCH_BUFFER_ARG(args, a);
  DFSCH_ARG_END(args);

  return dfsch_bignum_to_number(dfsch_bignum_from_bytes(a->ptr, a->len));
}


void dfsch__bignum_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "<bignum>", DFSCH_BIGNUM_TYPE);
  dfsch_define_cstr(ctx, "integer-expt", DFSCH_PRIMITIVE_REF(integer_expt));
  dfsch_define_cstr(ctx, "integer->bytes", DFSCH_PRIMITIVE_REF(bignum_2_bytes));
  dfsch_define_cstr(ctx, "bytes->integer", DFSCH_PRIMITIVE_REF(bytes_2_bignum));  
}

