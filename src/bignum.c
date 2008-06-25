#include "dfsch/number.h"

#include <dfsch/dfsch.h>
#include "util.h"
#include "internal.h"

#define WORD_BITS 15
#define WORD_BASE (1 << WORD_BITS)
#define WORD_MASK (WORD_BASE - 1)

typedef uint16_t word_t;
typedef uint32_t dword_t;
typedef int32_t sword_t;

typedef struct bignum_t {
  dfsch_type_t* type;
  int negative;
  size_t length;
  word_t words[];
} bignum_t;

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

static int bignum_equal_p(bignum_t* a, bignum_t* b){
  if (a->length != b->length){
    return 0;
  }
  return memcmp(a->words, b->words, a->length*sizeof(word_t)) == 0;
}

static char* hex_chars = "0123456789abcdef";

static char* bignum_write(bignum_t* b, int max_depth, int readable){
  str_list_t* l = sl_create();
  size_t i;
  sl_append(l, "#<transient-bignum");
  if (b->negative){
    sl_append(l, " negative");
  }
  for (i = b->length; i > 0; i--){
    sl_append(l, saprintf(" %d",b->words[i-1]));
  }
  sl_append(l, ">");
  return sl_value(l);
}

dfsch_number_type_t dfsch_bignum_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_NUMBER_TYPE,
  sizeof(bignum_t),
  "transient-bignum",
  (dfsch_type_equal_p_t)bignum_equal_p,
  (dfsch_type_write_t)bignum_write,
  NULL,
  (dfsch_type_hash_t)bignum_hash
};

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
  return res;
}
static bignum_t* copy_bignum(bignum_t* s){
  bignum_t* b = make_bignum(s->length);
  b->negative = s->negative;
  memcpy(b->words, s->words, sizeof(word_t)*s->length);
  return b;
}

static void normalize_bignum(bignum_t* n){
  while (n->length > 0 && n->words[n->length-1] == 0){
    n->length--;
  }
}

static int bignum_cmp_abs(bignum_t* a, bignum_t* b){
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
static int bignum_cmp(bignum_t* a, bignum_t* b){
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

static bignum_t* bignum_add_abs(bignum_t* a, bignum_t* b){
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
    switch (bignum_cmp_abs(a, b)) {
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


static bignum_t* bignum_add(bignum_t* a, bignum_t* b){
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

static bignum_t* bignum_sub(bignum_t* a, bignum_t* b){
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

/*
 * Long multiplication (HAC 14.12, elementary school :))
 * O(n^2), but simple and works
 */
static bignum_t* bignum_mul(bignum_t* a, bignum_t* b){
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
static void bignum_div(bignum_t* a, bignum_t* b, 
                       bignum_t**qp, bignum_t** rp){
  word_t wr;

  if (b->length == 0){
    dfsch_error("exception:bignum-division-by-zero", NULL);
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
    (*qp)->negative = a->negative;
  }
  if (qp){
    (*qp)->negative = a->negative != b->negative;
  }
}

static bignum_t* make_bignum_from_digits(dfsch_object_t* dl){
  size_t len;
  size_t i;
  bignum_t* b;
  len = dfsch_list_length(dl);
  b = make_bignum(len);
  i = len - 1;
  while (DFSCH_PAIR_P(dl)){
    b->words[i] = dfsch_number_to_long(DFSCH_FAST_CAR(dl)) & WORD_MASK;
    i--;
    dl = DFSCH_FAST_CDR(dl);
  }
  return b;
}

DFSCH_DEFINE_PRIMITIVE(make_bignum, 0){
  return make_bignum_from_digits(args);
}
DFSCH_DEFINE_PRIMITIVE(bignum_add, 0){
  bignum_t* a;
  bignum_t* b;
  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);


  return bignum_add(a, b);
}
DFSCH_DEFINE_PRIMITIVE(bignum_sub, 0){
  bignum_t* a;
  bignum_t* b;
  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);


  return bignum_sub(a, b);
}
DFSCH_DEFINE_PRIMITIVE(bignum_mul, 0){
  bignum_t* a;
  bignum_t* b;
  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);


  return bignum_mul(a, b);
}
DFSCH_DEFINE_PRIMITIVE(bignum_divmod, 0){
  bignum_t* a;
  bignum_t* b;
  bignum_t* q;
  bignum_t* r;
  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);
  
  bignum_div(a, b, &q, &r);

  return dfsch_vector(2, q, r);
}
DFSCH_DEFINE_PRIMITIVE(bignum_cmp, 0){
  bignum_t* a;
  bignum_t* b;
  DFSCH_OBJECT_ARG(args, a);
  DFSCH_OBJECT_ARG(args, b);


  return bignum_cmp(a, b);
}

void dfsch__bignum_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "<bignum>", DFSCH_BIGNUM_TYPE);
  dfsch_define_cstr(ctx, "make-bignum", DFSCH_PRIMITIVE_REF(make_bignum));
  dfsch_define_cstr(ctx, "bignum+", DFSCH_PRIMITIVE_REF(bignum_add));
  dfsch_define_cstr(ctx, "bignum-", DFSCH_PRIMITIVE_REF(bignum_sub));
  dfsch_define_cstr(ctx, "bignum*", DFSCH_PRIMITIVE_REF(bignum_mul));
  dfsch_define_cstr(ctx, "bignum/%", DFSCH_PRIMITIVE_REF(bignum_divmod));
  dfsch_define_cstr(ctx, "bignum-cmp", DFSCH_PRIMITIVE_REF(bignum_cmp));
  
}
