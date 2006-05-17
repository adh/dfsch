#include <dfsch/number.h>
#include "internal.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define SMALLNUM_ORIGIN  -16
#define SMALLNUM_COUNT   32


typedef struct number_t {
  dfsch_type_t *type;
  enum {
    N_FIXNUM,
    N_FLONUM
  } n_type;
  union {
    long fixnum;
    double flonum;
  };
} number_t;

number_t smallnum_buf[SMALLNUM_COUNT];

static int n_equal_p(number_t* a, number_t* b){
  if (a->n_type == b->n_type){
    switch(a->n_type){
    case N_FIXNUM:
      return a->fixnum == b->fixnum;
    case N_FLONUM:
      return a->flonum == b->flonum;
    }
  }else{
    return dfsch_number_to_double((dfsch_object_t*) a) == 
      dfsch_number_to_double((dfsch_object_t*) b);
  }
}
static char* n_write(number_t*n, int max_depth){
  char  *s = GC_malloc(64);   
  // 64 bytes should be enought, even for 128 bit machines ^_~
  switch (n->n_type){
  case N_FLONUM:
    snprintf(s, 64, "%.32lg", n->flonum);
    break; 
  case N_FIXNUM:
    snprintf(s, 64, "%ld", n->fixnum);
    break;
  }
  return s;
}

static dfsch_type_t number_type = {
  sizeof(number_t),
  "number",
  (dfsch_type_equal_p_t)n_equal_p,
  (dfsch_type_write_t)n_write
};
#define NUMBER (&number_type)


int dfsch__number_eqv_p(dfsch_object_t* a, dfsch_object_t *b){
  return n_equal_p((number_t*)a, (number_t*) b);
}

dfsch_object_t* dfsch_make_number_from_double(double num){
  number_t *n;
  n = (number_t*)dfsch_make_object(NUMBER);
  if (!n)
    return NULL;

  n->n_type = N_FLONUM;
  n->flonum = num;

  return (dfsch_object_t*)n;
}
dfsch_object_t* dfsch_make_number_from_long(long num){
  number_t *n;

  if (num < SMALLNUM_COUNT + SMALLNUM_ORIGIN && num >= SMALLNUM_ORIGIN){
    n = smallnum_buf + (num - SMALLNUM_ORIGIN);
    n->type = NUMBER;
  }else{
    n = (number_t*)dfsch_make_object(NUMBER);
  }
  if (!n)
    return NULL;

  n->n_type = N_FIXNUM;
  n->fixnum = num;

  return (dfsch_object_t*)n;
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

    if (n == LONG_MAX || n == LONG_MIN)
      // overflow... so we will made it floating point
      goto flonum;
    return dfsch_make_number_from_long(n);
  }
}

double dfsch_number_to_double(dfsch_object_t *n){
  if (!n || n->type!=NUMBER)
    dfsch_throw("exception:not-a-number", n);

  switch (((number_t*)n)->n_type){
  case N_FLONUM:
    return ((number_t*)n)->flonum;
  case N_FIXNUM:
    return (double)((number_t*)n)->fixnum;
  }
}
long dfsch_number_to_long(dfsch_object_t *n){
  if (!n || n->type!=NUMBER)
    dfsch_throw("exception:not-a-number", n);
  if (((number_t*)n)->n_type!=N_FIXNUM)
    dfsch_throw("exception:not-an-exact-number", n);
  
  return (long)((number_t*)n)->fixnum;

}
int dfsch_number_p(dfsch_object_t* obj){
  if (!obj)
    return 0;
  return obj->type == NUMBER;
}
int dfsch_number_equal_p(dfsch_object_t* a, dfsch_object_t* b){
  if (!a || a->type!=NUMBER)
    dfsch_throw("exception:not-a-number", a);
  if (!b || b->type!=NUMBER)
    dfsch_throw("exception:not-a-number", b);

  return n_equal_p((number_t*)a, (number_t*)b);
}

// Arithmetics

dfsch_object_t* dfsch_number_add(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (!a || a->type!=NUMBER) 
    dfsch_throw("exception:not-a-number", a); 
  if (!b || b->type!=NUMBER) 
    dfsch_throw("exception:not-a-number", b); 
 
  if (((number_t*)a)->n_type == ((number_t*)b)->n_type){ 
    switch(((number_t*)a)->n_type){ 
    case N_FIXNUM: 
      {
        long an = ((number_t*)a)->fixnum;
        long bn = ((number_t*)b)->fixnum;
        long x = an + bn;

        if ((an^x) >= 0 || (bn^x) >= 0)
          return dfsch_make_number_from_long(x);
        goto fallback;
      }
    case N_FLONUM: 
      return dfsch_make_number_from_double(((number_t*)a)->flonum +
                                           ((number_t*)b)->flonum); 
    } 
  }else{ 
  fallback:
    return dfsch_make_number_from_double 
      (dfsch_number_to_double((dfsch_object_t*) a) + 
       dfsch_number_to_double((dfsch_object_t*) b)); 
  } 
  
}

dfsch_object_t* dfsch_number_sub(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (!a || a->type!=NUMBER) 
    dfsch_throw("exception:not-a-number", a); 
  if (!b || b->type!=NUMBER) 
    dfsch_throw("exception:not-a-number", b); 
 
  if (((number_t*)a)->n_type == ((number_t*)b)->n_type){ 
    switch(((number_t*)a)->n_type){ 
    case N_FIXNUM: 
      {
        long an = ((number_t*)a)->fixnum;
        long bn = ((number_t*)b)->fixnum;
        long x = an - bn;

        if ((an^x) >= 0 || (~bn^x) >= 0)
          return dfsch_make_number_from_long(x);
        goto fallback;
      }
    case N_FLONUM: 
      return dfsch_make_number_from_double(((number_t*)a)->flonum -
                                           ((number_t*)b)->flonum); 
    } 
  }else{ 
  fallback:
    return dfsch_make_number_from_double 
      (dfsch_number_to_double((dfsch_object_t*) a) - 
       dfsch_number_to_double((dfsch_object_t*) b)); 
  } 
  
}

dfsch_object_t* dfsch_number_mul(dfsch_object_t* a,  
                                 dfsch_object_t* b){ 
  if (!a || a->type!=NUMBER) 
    dfsch_throw("exception:not-a-number", a); 
  if (!b || b->type!=NUMBER) 
    dfsch_throw("exception:not-a-number", b); 
 
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
  fallback:
    return dfsch_make_number_from_double 
      (dfsch_number_to_double((dfsch_object_t*) a) * 
       dfsch_number_to_double((dfsch_object_t*) b)); 
  } 
}

dfsch_object_t* dfsch_number_div (dfsch_object_t* a,  
                                  dfsch_object_t* b){ 

  double an = dfsch_number_to_double((dfsch_object_t*) a);
  double bn = dfsch_number_to_double((dfsch_object_t*) b);
 
  if (bn == 0.0)
    dfsch_throw("exception:division-by-zero", NULL);

  return dfsch_make_number_from_double(an / bn); 
}
dfsch_object_t* dfsch_number_div_i(dfsch_object_t* a,  
                                   dfsch_object_t* b){ 

  long an = dfsch_number_to_long((dfsch_object_t*) a);
  long bn = dfsch_number_to_long((dfsch_object_t*) b);
 
  if (bn == 0)
    dfsch_throw("exception:division-by-zero", NULL);

  return dfsch_make_number_from_long(an / bn); 
}
dfsch_object_t* dfsch_number_mod (dfsch_object_t* a,  
                                  dfsch_object_t* b){ 

  long an = dfsch_number_to_long((dfsch_object_t*) a);
  long bn = dfsch_number_to_long((dfsch_object_t*) b);
 
  if (bn == 0)
    dfsch_throw("exception:division-by-zero", NULL);

  return dfsch_make_number_from_long(an % bn); 
}


/////////////////////////////////////////////////////////////////////////////
//
// Scheme binding
//
/////////////////////////////////////////////////////////////////////////////

/*
 * Number maniulation is simply brain damaged and needs considerable amount
 * of work.
 *
 * We also need support for different numeric types here.
 */

#define NEED_ARGS(args,count) \
  if (dfsch_list_length(args)!=(count)) \
    dfsch_throw("exception:wrong-number-of-arguments",(args));
#define MIN_ARGS(args,count) \
  if (dfsch_list_length(args)<(count)) \
    dfsch_throw("exception:too-few-arguments", (args));


static object_t* native_number_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_number_p(dfsch_car(args)));  
}

static object_t* native_plus(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  dfsch_object_t* s = dfsch_make_number_from_long(0);
  while(dfsch_pair_p(i)){
    s = dfsch_number_add(s, dfsch_car(i));
    i = dfsch_cdr(i);
  }

  return s; 
}
static object_t* native_minus(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  object_t* s;
  if (!dfsch_pair_p(i))
    dfsch_throw("exception:too-few-arguments",i);

  if (!dfsch_cdr(i))
    return dfsch_number_sub(dfsch_make_number_from_long(0), dfsch_car(i));
  s = dfsch_car(i);
  i = dfsch_cdr(i);
  while(dfsch_pair_p(i)){
    s= dfsch_number_sub(s, dfsch_car(i));
    i = dfsch_cdr(i);
  }

  return s; 
}
static object_t* native_mult(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  object_t* s = dfsch_make_number_from_long(1);
  while(dfsch_pair_p(i)){
    s = dfsch_number_mul(s,dfsch_car(i));
    i = dfsch_cdr(i);
  }

  return s; 
}
static object_t* native_slash(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  object_t* s;
  if (!dfsch_pair_p(i))
    dfsch_throw("exception:too-few-arguments",i);

  if (!dfsch_cdr(i))
    return dfsch_number_div(dfsch_make_number_from_long(1), 
                            dfsch_car(i));
  s = dfsch_car(i);
  i = dfsch_cdr(i);
  
  while(dfsch_pair_p(i)){
    s=dfsch_number_div(s, dfsch_car(i));
    i = dfsch_cdr(i);
  }

  return s; 
}
static object_t* native_slash_i(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  object_t* s;
  if (!dfsch_pair_p(i))
    dfsch_throw("exception:too-few-arguments",i);

  if (!dfsch_cdr(i))
    return dfsch_number_div(dfsch_make_number_from_long(1), 
                            dfsch_car(i));
  s = dfsch_car(i);
  i = dfsch_cdr(i);
  
  while(dfsch_pair_p(i)){
    s=dfsch_number_div_i(s, dfsch_car(i));
    i = dfsch_cdr(i);
  }

  return s; 
}
static object_t* native_modulo(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  object_t* s;
  if (!dfsch_pair_p(i))
    dfsch_throw("exception:too-few-arguments",i);

  if (!dfsch_cdr(i))
    return dfsch_number_div(dfsch_make_number_from_long(1), 
                            dfsch_car(i));
  s = dfsch_car(i);
  i = dfsch_cdr(i);
  
  while(dfsch_pair_p(i)){
    s=dfsch_number_mod(s, dfsch_car(i));
    i = dfsch_cdr(i);
  }

  return s; 
}

static object_t* native_number_equal(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  return dfsch_bool(dfsch_number_equal_p(dfsch_car(args),dfsch_car(dfsch_cdr(args))));
}

static object_t* native_lt(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    dfsch_throw("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    dfsch_throw("exception:not-a-number", b);

  return dfsch_bool(dfsch_number_to_double(a)<dfsch_number_to_double(b));  
}
static object_t* native_gt(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    dfsch_throw("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    dfsch_throw("exception:not-a-number", b);
    

  return dfsch_bool(dfsch_number_to_double(a)>dfsch_number_to_double(b));  
}
static object_t* native_lte(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    dfsch_throw("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    dfsch_throw("exception:not-a-number", b);

  return dfsch_bool(dfsch_number_to_double(a)<=dfsch_number_to_double(b));  
}
static object_t* native_gte(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    dfsch_throw("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    dfsch_throw("exception:not-a-number", b);
    

  return dfsch_bool(dfsch_number_to_double(a)>=dfsch_number_to_double(b));  
}

// Functions

static object_t* native_abs(void *baton, object_t* args, 
                            dfsch_tail_escape_t* esc){
  object_t *n;

  DFSCH_OBJECT_ARG(args, n);
  
  if (!dfsch_number_p(n))
    dfsch_throw("exception:not-a-number", n);

  switch (((number_t*)n)->n_type){
  case N_FLONUM:
    {
      double num = ((number_t*)n)->flonum;
      if (num >= 0)
        return n;
      return dfsch_make_number_from_double(-num);
    }
  case N_FIXNUM:
    {
      long num = ((number_t*)n)->fixnum;
      if (num >= 0)
        return n;
      return dfsch_make_number_from_long(-num);
    }
  }
}


void dfsch__number_native_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx, "+", dfsch_make_primitive(&native_plus,NULL));
  dfsch_ctx_define(ctx, "-", dfsch_make_primitive(&native_minus,NULL));
  dfsch_ctx_define(ctx, "*", dfsch_make_primitive(&native_mult,NULL));
  dfsch_ctx_define(ctx, "/", dfsch_make_primitive(&native_slash,NULL));
  dfsch_ctx_define(ctx, "/i", dfsch_make_primitive(&native_slash_i,NULL));
  dfsch_ctx_define(ctx, "%", dfsch_make_primitive(&native_modulo,NULL));
  dfsch_ctx_define(ctx, "=", dfsch_make_primitive(&native_number_equal,NULL));
  dfsch_ctx_define(ctx, "<", dfsch_make_primitive(&native_lt,NULL));
  dfsch_ctx_define(ctx, ">", dfsch_make_primitive(&native_gt,NULL));
  dfsch_ctx_define(ctx, "<=", dfsch_make_primitive(&native_lte,NULL));
  dfsch_ctx_define(ctx, ">=", dfsch_make_primitive(&native_gte,NULL));
  dfsch_ctx_define(ctx, "number?", dfsch_make_primitive(&native_number_p,
							NULL));

  dfsch_ctx_define(ctx, "pi", 
                   dfsch_make_number_from_double(3.1415926535897931));


  dfsch_ctx_define(ctx, "abs", dfsch_make_primitive(&native_abs,NULL));
  
}
