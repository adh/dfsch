#include <dfsch/number.h>
#include "object.h"


typedef struct number_t {
  dfsch_type_t *type;
  double number;
} number_t;

static const dfsch_type_t number_type = {
  sizeof(number_t),
  "number",
  NULL,
  NULL
};

#define NUMBER (&number_type)


dfsch_object_t* dfsch_make_number_from_double(double num){
  number_t *n;
  n = make_object(NUMBER);
  if (!n)
    return NULL;


  n->number = num;

  return n;
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
