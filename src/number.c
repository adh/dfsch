#include <dfsch/number.h>

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

