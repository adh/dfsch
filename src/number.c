#include <dfsch/number.h>
#include "object.h"

dfsch_object_t* dfsch_make_number_from_double(double num){
  object_t *n;
  n = make_object(NUMBER);
  if (!n)
    return NULL;


  n->data.number = num;

  return n;
}
double dfsch_number_to_double(dfsch_object_t *n){
  if (!n || n->type!=NUMBER)
    dfsch_throw("exception:not-a-number", n);

  return n->data.number;

}
