#include <dfsch/number.h>
#include "internal.h"
#include <stdio.h>

typedef struct number_t {
  dfsch_type_t *type;
  double number;
} number_t;

static int n_equal_p(number_t* a, number_t* b){
  return a->number == b->number;
}
static char* n_write(number_t*n, int max_depth){
  char  *s = GC_malloc(64);   
  // 64 bytes should be enought, even for 128 bit machines ^_~
  snprintf(s, 64, "%lf", n->number);
  return s;
}

static dfsch_type_t number_type = {
  sizeof(number_t),
  "number",
  (dfsch_type_equal_p_t)n_equal_p,
  (dfsch_type_write_t)n_write
};
#define NUMBER (&number_type)


dfsch_object_t* dfsch_make_number_from_double(double num){
  number_t *n;
  n = (number_t*)dfsch_make_object(NUMBER);
  if (!n)
    return NULL;


  n->number = num;

  return (dfsch_object_t*)n;
}
dfsch_object_t* dfsch_make_number_from_long(long n){
  return dfsch_make_number_from_double((double)n);
}

double dfsch_number_to_double(dfsch_object_t *n){
  if (!n || n->type!=NUMBER)
    dfsch_throw("exception:not-a-number", n);

  return ((number_t*)n)->number;

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

  return ((number_t*)a)->number == ((number_t*)b)->number;
  
}

int dfsch__number_eqv_p(dfsch_object_t* a, dfsch_object_t* b){
  return ((number_t*)a)->number == ((number_t*)b)->number;  
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

typedef dfsch_object_t object_t;

#define NEED_ARGS(args,count) \
  if (dfsch_list_length(args)!=(count)) \
    DFSCH_THROW("exception:wrong-number-of-arguments",(args));
#define MIN_ARGS(args,count) \
  if (dfsch_list_length(args)<(count)) \
    DFSCH_THROW("exception:too-few-arguments", (args));


static object_t* native_number_p(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,1);  
  return dfsch_bool(dfsch_number_p(dfsch_car(args)));  
}

static object_t* native_plus(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  double s=0;
  while(dfsch_pair_p(i)){
    
    if (dfsch_number_p(dfsch_car(i))){
      s+=dfsch_number(dfsch_car(i));
    }else{
      DFSCH_THROW("exception:not-a-number", dfsch_car(i));
      
    }
    i = dfsch_cdr(i);
  }

  return dfsch_make_number(s); 
}
static object_t* native_minus(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  double s;
  if (!dfsch_pair_p(i))
    DFSCH_THROW("exception:too-few-arguments",i);

  if (dfsch_number_p(dfsch_car(i))){
    if (!dfsch_cdr(i))
      return dfsch_make_number(0-dfsch_number(dfsch_car(i)));
    s=dfsch_number(dfsch_car(i));
  }else{
    DFSCH_THROW("exception:not-a-number", dfsch_car(i));
    
  }
  i = dfsch_cdr(i);
  while(dfsch_pair_p(i)){
    if (dfsch_number_p(dfsch_car(i))){
      s-=dfsch_number(dfsch_car(i));
    }else{
      DFSCH_THROW("exception:not-a-number", dfsch_car(i));
      
    }
    i = dfsch_cdr(i);
  }

  return dfsch_make_number(s); 
}
static object_t* native_mult(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  double s=1;
  while(dfsch_pair_p(i)){
    if (dfsch_number_p(dfsch_car(i))){
      s*=dfsch_number(dfsch_car(i));
    }else{
      DFSCH_THROW("exception:not-a-number", dfsch_car(i));
      
    }
    i = dfsch_cdr(i);
  }

  return dfsch_make_number(s); 
}
static object_t* native_slash(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  double s;
  if (!dfsch_pair_p(i))
    DFSCH_THROW("exception:too-few-arguments",i);

  if (dfsch_number_p(dfsch_car(i))){
    if (!dfsch_cdr(i))
      return dfsch_make_number(1/dfsch_number(dfsch_car(i)));
    s=dfsch_number(dfsch_car(i));
  }else{
    DFSCH_THROW("exception:not-a-number", dfsch_car(i));
  }
  i = dfsch_cdr(i);
  
  while(dfsch_pair_p(i)){
    if (dfsch_number_p(dfsch_car(i))){
      s/=dfsch_number(dfsch_car(i));
    }else{
      DFSCH_THROW("exception:not-a-number", dfsch_car(i));
      
    }
    i = dfsch_cdr(i);
  }

  return dfsch_make_number(s); 
}
static object_t* native_modulo(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  object_t* i = args;
  long s;
  if (!dfsch_pair_p(i))
    DFSCH_THROW("exception:too-few-arguments",i);

  if (dfsch_number_p(dfsch_car(i))){
    if (!dfsch_cdr(i))
      DFSCH_THROW("exception:too-few-arguments", i);
    s=(long)dfsch_number(dfsch_car(i));
  }else{
    DFSCH_THROW("exception:not-a-number", dfsch_car(i));
  }
  i = dfsch_cdr(i);
  
  while(dfsch_pair_p(i)){
    if (dfsch_number_p(dfsch_car(i))){
      s%=(long)dfsch_number(dfsch_car(i));
    }else{
      DFSCH_THROW("exception:not-a-number", dfsch_car(i));
      
    }
    i = dfsch_cdr(i);
  }

  return dfsch_make_number(s); 
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
    DFSCH_THROW("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    DFSCH_THROW("exception:not-a-number", b);

  return dfsch_bool(dfsch_number(a)<dfsch_number(b));  
}
static object_t* native_gt(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    DFSCH_THROW("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    DFSCH_THROW("exception:not-a-number", b);
    

  return dfsch_bool(dfsch_number(a)>dfsch_number(b));  
}
static object_t* native_lte(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    DFSCH_THROW("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    DFSCH_THROW("exception:not-a-number", b);

  return dfsch_bool(dfsch_number(a)<=dfsch_number(b));  
}
static object_t* native_gte(void *baton, object_t* args, dfsch_tail_escape_t* esc){
  NEED_ARGS(args,2);  
  object_t *a = dfsch_car(args);
  object_t *b = dfsch_car(dfsch_cdr(args));
  if (!dfsch_number_p(a))
    DFSCH_THROW("exception:not-a-number", a);
  if (!dfsch_number_p(b))
    DFSCH_THROW("exception:not-a-number", b);
    

  return dfsch_bool(dfsch_number(a)>=dfsch_number(b));  
}

void dfsch__number_native_register(dfsch_ctx_t *ctx){
  dfsch_ctx_define(ctx, "+", dfsch_make_primitive(&native_plus,NULL));
  dfsch_ctx_define(ctx, "-", dfsch_make_primitive(&native_minus,NULL));
  dfsch_ctx_define(ctx, "*", dfsch_make_primitive(&native_mult,NULL));
  dfsch_ctx_define(ctx, "/", dfsch_make_primitive(&native_slash,NULL));
  dfsch_ctx_define(ctx, "%", dfsch_make_primitive(&native_modulo,NULL));
  dfsch_ctx_define(ctx, "=", dfsch_make_primitive(&native_number_equal,NULL));
  dfsch_ctx_define(ctx, "<", dfsch_make_primitive(&native_lt,NULL));
  dfsch_ctx_define(ctx, ">", dfsch_make_primitive(&native_gt,NULL));
  dfsch_ctx_define(ctx, "<=", dfsch_make_primitive(&native_lte,NULL));
  dfsch_ctx_define(ctx, ">=", dfsch_make_primitive(&native_gte,NULL));
  dfsch_ctx_define(ctx, "number?", dfsch_make_primitive(&native_number_p,
							NULL));

  dfsch_ctx_define(ctx, "pi", dfsch_make_number(3.1415926535897931));
  
}
