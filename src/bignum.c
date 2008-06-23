#include "dfsch/number.h"

#include <dfsch/dfsch.h>
#include "util.h"
#include "internal.h"

#define WORD_BITS 16
typedef uint16_t word_t;
typedef uint32_t dword_t;


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
  return memcmp(a->words, b->words, a->length*sizeof(word_t));
}

static char* hex_chars = "0123456789abcdef";

static char* word_2_hex(word_t w){
  char* s = GC_MALLOC_ATOMIC(5);
  s[0] = hex_chars[w >> 12 & 0xf];
  s[1] = hex_chars[w >> 8 & 0xf];
  s[2] = hex_chars[w >> 4 & 0xf];
  s[3] = hex_chars[w >> 0 & 0xf];
  s[4] = 0;
  return s;
}

static char* bignum_write(bignum_t* b, int max_depth, int readable){
  str_list_t* l = sl_create();
  size_t i;
  sl_append(l, "#<transient-bignum ");
  if (b->negative){
    sl_append(l, "negative ");
  }
  sl_append(l, "0x");
  for (i = b->length; i > 0; i--){
    sl_append(l, word_2_hex(b->words[i-1]));
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

static void compact_bignum(bignum_t* n){
  while (n->words[n->length-1] == 0){
    n->length--;
  }
}

static bignum_t* bignum_add(bignum_t* a, bignum_t* b){
  bignum_t* res;
  bignum_t* tmp;
  size_t i;
  dword_t cy;

  if (a->negative != b->negative){
    dfsch_error("unimplemented", NULL);
  }

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
    res->words[i] = cy;
    i++;
  }

  while (i < a->length){
    cy >>= WORD_BITS;
    cy += a->words[i];
    res->words[i] = cy; 
    i++;
  }

  res->words[i] = cy >> WORD_BITS;

  compact_bignum(res);
  return res;
}



static bignum_t* make_bignum_from_digits(dfsch_object_t* dl){
  size_t len;
  size_t i;
  bignum_t* b;
  len = dfsch_list_length(dl);
  b = make_bignum(len);
  i = len - 1;
  while (DFSCH_PAIR_P(dl)){
    b->words[i] = dfsch_number_to_long(DFSCH_FAST_CAR(dl));
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

void dfsch__bignum_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "<bignum>", DFSCH_BIGNUM_TYPE);
  dfsch_define_cstr(ctx, "make-bignum", DFSCH_PRIMITIVE_REF(make_bignum));
  dfsch_define_cstr(ctx, "bignum+", DFSCH_PRIMITIVE_REF(bignum_add));
  
}
