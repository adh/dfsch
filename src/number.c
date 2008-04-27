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

#include <dfsch/number.h>
#include <dfsch/strings.h>
#include "util.h"
#include "internal.h"
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>

#define SMALLNUM_ORIGIN  -32
#define SMALLNUM_COUNT   64

typedef dfsch_object_t object_t;

typedef struct flonum_t {
  dfsch_type_t *type;
  double flonum;
} flonum_t;


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

static char* fixnum_write(dfsch_object_t* n, int max_depth, int readable){
  return saprintf("%d", DFSCH_FIXNUM_REF(n));
}

dfsch_number_type_t dfsch_fixnum_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_NUMBER_TYPE,
  0,
  "fixnum",
  NULL,
  (dfsch_type_write_t)fixnum_write,
  NULL,
  NULL
};

static char* flonum_write(flonum_t* n, int max_depth, int readable){
  return saprintf("%.32g", n->flonum);
}
static uint32_t flonum_hash(flonum_t* n){
  return diffusion(((size_t)n->flonum) ^ 
    ((size_t) (exp(round(32-log(n->flonum))) * 
               (n->flonum - trunc(n->flonum)))));
}
static int flonum_equal_p(flonum_t* a, flonum_t* b){
  return a->flonum == b->flonum;
}

dfsch_number_type_t dfsch_flonum_type = {
  DFSCH_STANDARD_TYPE,
  DFSCH_NUMBER_TYPE,
  sizeof(flonum_t),
  "flonum",
  (dfsch_type_equal_p_t)flonum_equal_p,
  (dfsch_type_write_t)flonum_write,
  NULL,
  (dfsch_type_hash_t)flonum_hash,
};



dfsch_object_t* dfsch_make_number_from_double(double num){
  flonum_t *n;
  n = (flonum_t*)dfsch_make_object(DFSCH_FLONUM_TYPE);

  n->flonum = num;

  return (dfsch_object_t*)n;
}
dfsch_object_t* dfsch_make_number_from_long(long num){
  // TODO: num \in <FIXNUM_MAX, LONG_MAX>
  return DFSCH_MAKE_FIXNUM(num);
}

dfsch_object_t* dfsch_make_number_from_string(char* string){
  // TODO: This function is slightly flawed

  char *eptr;
  double d;
  long n;

  if (strchr(string, '.')){ // contains dot => floating-point
  flonum:
   
    d = strtod(string, &eptr);
    if (*eptr)
      return NULL;
    return dfsch_make_number_from_double(atof(string));
  }else{ // doesn't => fixed point 
    n = strtol(string, &eptr, 0);    

    if (*eptr)
      return NULL;

    if (n >= LONG_MAX/2 || n <= LONG_MIN/2)
      // overflow... so we will made it floating point
      goto flonum;
    return dfsch_make_number_from_long(n);
  }
}

double dfsch_number_to_double(dfsch_object_t *n){
  if (DFSCH_TYPE_OF(n)==DFSCH_FIXNUM_TYPE){
    return (double)DFSCH_FIXNUM_REF(n);
  }
  if (DFSCH_TYPE_OF(n)==DFSCH_FLONUM_TYPE){
    return ((flonum_t*)n)->flonum;
  }  

  dfsch_error("exception:not-a-real-number", n);
}
long dfsch_number_to_long(dfsch_object_t *n){
  if (DFSCH_TYPE_OF(n)!=DFSCH_FIXNUM_TYPE)
    dfsch_error("exception:not-a-exact-number", n);
  
  return DFSCH_FIXNUM_REF(n);

}
char* dfsch_number_to_string(dfsch_object_t *n){
}
int dfsch_number_p(dfsch_object_t* obj){
  return DFSCH_INSTANCE_P(obj, DFSCH_NUMBER_TYPE);
}
int dfsch_number_equal_p(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) == DFSCH_FIXNUM_REF(b);
  }else{
    return dfsch_number_to_double(a) == dfsch_number_to_double(b);
  }
  
}
int dfsch__number_eqv_p(dfsch_object_t* a, dfsch_object_t* b){
  /* 
   * No need to handle fixnum case, fixnums are eq? and thus eqv?
   *
   * So thus we have either two flonums or flonum + fixnum
   */

  return dfsch_number_to_double(a) == dfsch_number_to_double(b);
}

// Comparisons

int dfsch_number_lt(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a) == DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) < DFSCH_FIXNUM_REF(b);
  }else{
    return dfsch_number_to_double(a) < dfsch_number_to_double(b);
  }
}
int dfsch_number_gt(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) > DFSCH_FIXNUM_REF(b);
  }else{
    return dfsch_number_to_double(a) > dfsch_number_to_double(b);
  }
}
int dfsch_number_lte(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) <= DFSCH_FIXNUM_REF(b);
  }else{
    return dfsch_number_to_double(a) <= dfsch_number_to_double(b);
  }
}
int dfsch_number_gte(dfsch_object_t* a, dfsch_object_t* b){
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
      return DFSCH_FIXNUM_REF(a) >= DFSCH_FIXNUM_REF(b);
  }else{
    return dfsch_number_to_double(a) >= dfsch_number_to_double(b);
  }
}


// Arithmetics

dfsch_object_t* dfsch_number_add(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
    long an = DFSCH_FIXNUM_REF(a)<<1;
    long bn = DFSCH_FIXNUM_REF(b)<<1;
    long x = an + bn;

    if ((an^x) >= 0 || (bn^x) >= 0)
      return DFSCH_MAKE_FIXNUM(x>>1);
  }

  return dfsch_make_number_from_double 
    (dfsch_number_to_double((dfsch_object_t*) a) + 
     dfsch_number_to_double((dfsch_object_t*) b));   
}

dfsch_object_t* dfsch_number_sub(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (DFSCH_TYPE_OF(a) == DFSCH_TYPE_OF(b) &&
      DFSCH_TYPE_OF(a)== DFSCH_FIXNUM_TYPE){
    long an = DFSCH_FIXNUM_REF(a)<<1;
    long bn = DFSCH_FIXNUM_REF(b)<<1;
    long x = an - bn;

    if ((an^x) >= 0 || (~bn^x) >= 0)
      return DFSCH_MAKE_FIXNUM(x>>1);
  }

  return dfsch_make_number_from_double 
    (dfsch_number_to_double((dfsch_object_t*) a) - 
     dfsch_number_to_double((dfsch_object_t*) b));   
}

dfsch_object_t* dfsch_number_mul(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  /*  if (DFSCH_TYPE_OF(a)!=NUMBER) 
    dfsch_error("exception:not-a-number", a); 
  if (DFSCH_TYPE_OF(b)!=NUMBER) 
    dfsch_error("exception:not-a-number", b); 
 
  if (((number_t*)a)->n_type == ((number_t*)b)->n_type){ 
    switch(((number_t*)a)->n_type){ 
    case N_FIXNUM: 
      {
        long an = ((number_t*)a)->fixnum;
        long bn = ((number_t*)b)->fixnum;
        long x = an * bn;
        double xd = (double)an * (double)bn;

        if (x == xd){
          return dfsch_make_number_from_long(x);
        }else{
          double d = x > xd ? x - xd : xd - x;
          double p = xd >= 0 ? xd : -xd;
          
          if (32.0 * d <= p)
            return dfsch_make_number_from_long(x);
          else
            return dfsch_make_number_from_double(xd);
        }
      }
      case N_FLONUM: 
      return dfsch_make_number_from_double(((number_t*)a)->flonum *
                                           ((number_t*)b)->flonum); 
          } 
  }else{ 
  fallback:*/

  return dfsch_make_number_from_double 
    (dfsch_number_to_double((dfsch_object_t*) a) * 
     dfsch_number_to_double((dfsch_object_t*) b)); 
}

dfsch_object_t* dfsch_number_div (dfsch_object_t* a,  
                                  dfsch_object_t* b){ 

  double an = dfsch_number_to_double((dfsch_object_t*) a);
  double bn = dfsch_number_to_double((dfsch_object_t*) b);
 
  if (bn == 0.0)
    dfsch_error("exception:division-by-zero", NULL);

  return dfsch_make_number_from_double(an / bn); 
}
dfsch_object_t* dfsch_number_div_i(dfsch_object_t* a,  
                                   dfsch_object_t* b){ 

  long an = dfsch_number_to_long((dfsch_object_t*) a);
  long bn = dfsch_number_to_long((dfsch_object_t*) b);
 
  if (bn == 0)
    dfsch_error("exception:division-by-zero", NULL);

  return dfsch_make_number_from_long(an / bn); 
}
dfsch_object_t* dfsch_number_mod (dfsch_object_t* a,  
                                  dfsch_object_t* b){ 

  long an = dfsch_number_to_long((dfsch_object_t*) a);
  long bn = dfsch_number_to_long((dfsch_object_t*) b);
 
  if (bn == 0)
    dfsch_error("exception:division-by-zero", NULL);

  return dfsch_make_number_from_long(an % bn); 
}


/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////


DFSCH_DEFINE_PRIMITIVE(number_p, DFSCH_PRIMITIVE_CACHED){
  object_t *obj;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_p(obj));  
}

DFSCH_DEFINE_PRIMITIVE(plus, DFSCH_PRIMITIVE_CACHED){
  object_t* i = args;
  dfsch_object_t* s = dfsch_make_number_from_long(0);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_add(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(minus, DFSCH_PRIMITIVE_CACHED){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("exception:too-few-arguments",i);

  if (!DFSCH_FAST_CDR(i))
    return dfsch_number_sub(dfsch_make_number_from_long(0), DFSCH_FAST_CAR(i));
  s = DFSCH_FAST_CAR(i);
  i = DFSCH_FAST_CDR(i);
  while(DFSCH_PAIR_P(i)){
    s= dfsch_number_sub(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(mult, DFSCH_PRIMITIVE_CACHED){
  object_t* i = args;
  object_t* s = dfsch_make_number_from_long(1);
  while(DFSCH_PAIR_P(i)){
    s = dfsch_number_mul(s,DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(slash, DFSCH_PRIMITIVE_CACHED){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("exception:too-few-arguments",i);

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
DFSCH_DEFINE_PRIMITIVE(slash_i, DFSCH_PRIMITIVE_CACHED){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("exception:too-few-arguments",i);

  if (!DFSCH_FAST_CDR(i))
    return dfsch_number_div(dfsch_make_number_from_long(1), 
                            DFSCH_FAST_CAR(i));
  s = DFSCH_FAST_CAR(i);
  i = DFSCH_FAST_CDR(i);
  
  while(DFSCH_PAIR_P(i)){
    s=dfsch_number_div_i(s, DFSCH_FAST_CAR(i));
    i = DFSCH_FAST_CDR(i);
  }

  return s; 
}
DFSCH_DEFINE_PRIMITIVE(modulo, DFSCH_PRIMITIVE_CACHED){
  object_t* i = args;
  object_t* s;
  if (!DFSCH_PAIR_P(i))
    dfsch_error("exception:too-few-arguments",i);

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

DFSCH_DEFINE_PRIMITIVE(number_equal, DFSCH_PRIMITIVE_CACHED){
  object_t* z0, *z1;
  DFSCH_OBJECT_ARG(args, z0);
  DFSCH_OBJECT_ARG(args, z1);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_equal_p(z0,z1));
}

DFSCH_DEFINE_PRIMITIVE(lt, DFSCH_PRIMITIVE_CACHED){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (!dfsch_number_lt(a, b))
      return NULL;
    a = b;
  }
  return dfsch_sym_true();
}
DFSCH_DEFINE_PRIMITIVE(gt, DFSCH_PRIMITIVE_CACHED){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (!dfsch_number_gt(a, b))
      return NULL;
    a = b;
  }
  return dfsch_sym_true();
}
DFSCH_DEFINE_PRIMITIVE(lte, DFSCH_PRIMITIVE_CACHED){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (!dfsch_number_lte(a, b))
      return NULL;
    a = b;
  }
  return dfsch_sym_true();
}
DFSCH_DEFINE_PRIMITIVE(gte, DFSCH_PRIMITIVE_CACHED){
  object_t *a;
  object_t *b;

  DFSCH_OBJECT_ARG(args, a);
  while (args){
    DFSCH_OBJECT_ARG(args, b);
    if (!dfsch_number_gte(a, b))
      return NULL;
    a = b;
  }
  return dfsch_sym_true();
}

// Functions

DFSCH_DEFINE_PRIMITIVE(abs, DFSCH_PRIMITIVE_CACHED){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  
  if (!dfsch_number_p(n))
    dfsch_error("exception:not-a-number", n);

  if (DFSCH_TYPE_OF(n) == DFSCH_FIXNUM_TYPE){
    if (DFSCH_FIXNUM_REF(n) >= 0){
      return n;
    } else {
      return DFSCH_MAKE_FIXNUM(-DFSCH_FIXNUM_REF(n));
    }
  }

  return dfsch_make_number_from_double(fabs(dfsch_number_to_double(n)));
}

DFSCH_DEFINE_PRIMITIVE(exp, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG_OPT(args, z, 1.0);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(exp(z));
}

DFSCH_DEFINE_PRIMITIVE(log, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z <= 0.0)
    dfsch_error("exception:not-in-argument-domain", 
                dfsch_list(2, 
                           dfsch_make_symbol("log"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(log(z));
}

DFSCH_DEFINE_PRIMITIVE(sin, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(sin(z));
}
DFSCH_DEFINE_PRIMITIVE(cos, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(cos(z));
}
DFSCH_DEFINE_PRIMITIVE(tan, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(tan(z));
}

DFSCH_DEFINE_PRIMITIVE(asin, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z > 1.0 || z < -1.0)
    dfsch_error("exception:not-in-argument-domain", 
                dfsch_list(2, 
                           dfsch_make_symbol("asin"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(asin(z));
}

DFSCH_DEFINE_PRIMITIVE(acos, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z > 1.0 || z < -1.0)
    dfsch_error("exception:not-in-argument-domain", 
                dfsch_list(2, 
                           dfsch_make_symbol("acos"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(acos(z));
}

DFSCH_DEFINE_PRIMITIVE(atan, DFSCH_PRIMITIVE_CACHED){
  double z0, z1;
  DFSCH_DOUBLE_ARG(args, z0);
  DFSCH_DOUBLE_ARG_OPT(args, z1, 1.0);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(atan(z0/z1));
}

DFSCH_DEFINE_PRIMITIVE(sqrt, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);

  if (z < 0.0)
    dfsch_error("exception:not-in-argument-domain", 
                dfsch_list(2, 
                           dfsch_make_symbol("sqrt"),
                           dfsch_make_number_from_double(z)));

  return dfsch_make_number_from_double(sqrt(z));
}

DFSCH_DEFINE_PRIMITIVE(expt, DFSCH_PRIMITIVE_CACHED){
  double z0, z1, v;
  DFSCH_DOUBLE_ARG(args, z0);
  DFSCH_DOUBLE_ARG(args, z1);
  DFSCH_ARG_END(args);

  errno = 0;
  v = pow(z0,z1);
  if (errno == EDOM) // XXX
    dfsch_error("exception:not-in-argument-domain", 
                dfsch_list(3, 
                           dfsch_make_symbol("sqrt"),
                           dfsch_make_number_from_double(z0),
                           dfsch_make_number_from_double(z1)));

  return dfsch_make_number_from_double(v);
}

DFSCH_DEFINE_PRIMITIVE(zero_p, DFSCH_PRIMITIVE_CACHED){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);
  

  return dfsch_bool(dfsch_number_to_double(n) == 0.0);
}

DFSCH_DEFINE_PRIMITIVE(positive_p, DFSCH_PRIMITIVE_CACHED){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_to_double(n) > 0.0);  
}

DFSCH_DEFINE_PRIMITIVE(negative_p, DFSCH_PRIMITIVE_CACHED){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);

  return dfsch_bool(dfsch_number_to_double(n) < 0.0);    
}

DFSCH_DEFINE_PRIMITIVE(even_p, DFSCH_PRIMITIVE_CACHED){
  double n;

  DFSCH_DOUBLE_ARG(args, n);
  DFSCH_ARG_END(args);
  
  n = n / 2;
  return dfsch_bool(n == round(n));
}

DFSCH_DEFINE_PRIMITIVE(odd_p, DFSCH_PRIMITIVE_CACHED){
  double n;

  DFSCH_DOUBLE_ARG(args, n);
  DFSCH_ARG_END(args);
  
  n = (n + 1) / 2;
  return dfsch_bool(n == round(n));
}

DFSCH_DEFINE_PRIMITIVE(max, DFSCH_PRIMITIVE_CACHED){
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

DFSCH_DEFINE_PRIMITIVE(min, DFSCH_PRIMITIVE_CACHED){
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

DFSCH_DEFINE_PRIMITIVE(round, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(round(z));
}
DFSCH_DEFINE_PRIMITIVE(floor, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(floor(z));
}
DFSCH_DEFINE_PRIMITIVE(ceiling, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(ceil(z));
}
DFSCH_DEFINE_PRIMITIVE(truncate, DFSCH_PRIMITIVE_CACHED){
  double z;
  DFSCH_DOUBLE_ARG(args, z);
  DFSCH_ARG_END(args);
  return dfsch_make_number_from_double(trunc(z));
}

DFSCH_DEFINE_PRIMITIVE(number_2_string, DFSCH_PRIMITIVE_CACHED){
  object_t *n;
  DFSCH_OBJECT_ARG(args, n);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_number_to_string(n));
}
DFSCH_DEFINE_PRIMITIVE(string_2_number, DFSCH_PRIMITIVE_CACHED){
  char *str;
  object_t *i;

  DFSCH_STRING_ARG(args, str);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_string(str);
}


// TODO: exact?, inexact?, real?, integer? ...
// TODO: gcd, lcm

void dfsch__number_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<number>", DFSCH_NUMBER_TYPE);
  dfsch_define_cstr(ctx, "<fixnum>", DFSCH_FIXNUM_TYPE);
  dfsch_define_cstr(ctx, "<flonum>", DFSCH_FLONUM_TYPE);

  dfsch_define_cstr(ctx, "+", DFSCH_PRIMITIVE_REF(plus));
  dfsch_define_cstr(ctx, "-", DFSCH_PRIMITIVE_REF(minus));
  dfsch_define_cstr(ctx, "*", DFSCH_PRIMITIVE_REF(mult));
  dfsch_define_cstr(ctx, "/", DFSCH_PRIMITIVE_REF(slash));
  dfsch_define_cstr(ctx, "/i", DFSCH_PRIMITIVE_REF(slash_i));
  dfsch_define_cstr(ctx, "%", DFSCH_PRIMITIVE_REF(modulo));
  dfsch_define_cstr(ctx, "=", DFSCH_PRIMITIVE_REF(number_equal));
  dfsch_define_cstr(ctx, "<", DFSCH_PRIMITIVE_REF(lt));
  dfsch_define_cstr(ctx, ">", DFSCH_PRIMITIVE_REF(gt));
  dfsch_define_cstr(ctx, "<=", DFSCH_PRIMITIVE_REF(lte));
  dfsch_define_cstr(ctx, ">=", DFSCH_PRIMITIVE_REF(gte));
  dfsch_define_cstr(ctx, "number?", DFSCH_PRIMITIVE_REF(number_p));

  dfsch_define_cstr(ctx, "pi", 
                    dfsch_make_number_from_double(4*atan(1)));


  dfsch_define_cstr(ctx, "abs", DFSCH_PRIMITIVE_REF(abs));

  dfsch_define_cstr(ctx, "exp", DFSCH_PRIMITIVE_REF(exp));
  dfsch_define_cstr(ctx, "log", DFSCH_PRIMITIVE_REF(log));
  dfsch_define_cstr(ctx, "expt", DFSCH_PRIMITIVE_REF(expt));

  dfsch_define_cstr(ctx, "sin", DFSCH_PRIMITIVE_REF(sin));
  dfsch_define_cstr(ctx, "cos", DFSCH_PRIMITIVE_REF(cos));
  dfsch_define_cstr(ctx, "tan", DFSCH_PRIMITIVE_REF(tan));

  dfsch_define_cstr(ctx, "asin", DFSCH_PRIMITIVE_REF(asin));
  dfsch_define_cstr(ctx, "acos", DFSCH_PRIMITIVE_REF(acos));
  dfsch_define_cstr(ctx, "atan", DFSCH_PRIMITIVE_REF(atan));

  dfsch_define_cstr(ctx, "sqrt", DFSCH_PRIMITIVE_REF(sqrt));

  dfsch_define_cstr(ctx, "min", DFSCH_PRIMITIVE_REF(min));
  dfsch_define_cstr(ctx, "max", DFSCH_PRIMITIVE_REF(max));

  dfsch_define_cstr(ctx, "zero?", DFSCH_PRIMITIVE_REF(zero_p));
  dfsch_define_cstr(ctx, "negative?", DFSCH_PRIMITIVE_REF(negative_p));
  dfsch_define_cstr(ctx, "positive?", DFSCH_PRIMITIVE_REF(positive_p));

  dfsch_define_cstr(ctx, "even?", DFSCH_PRIMITIVE_REF(even_p));
  dfsch_define_cstr(ctx, "odd?", DFSCH_PRIMITIVE_REF(odd_p));

  dfsch_define_cstr(ctx, "round", DFSCH_PRIMITIVE_REF(round));
  dfsch_define_cstr(ctx, "floor", DFSCH_PRIMITIVE_REF(floor));
  dfsch_define_cstr(ctx, "ceiling", DFSCH_PRIMITIVE_REF(ceiling));
  dfsch_define_cstr(ctx, "truncate", DFSCH_PRIMITIVE_REF(truncate));

  dfsch_define_cstr(ctx, "number->string", 
		   DFSCH_PRIMITIVE_REF(number_2_string));
  dfsch_define_cstr(ctx, "string->number", 
		   DFSCH_PRIMITIVE_REF(string_2_number));
  
}
