#ifndef H__dfsch__lib__random__
#define H__dfsch__lib__random__

#include <dfsch/dfsch.h>

#include <stdlib.h>
#include <stdint.h>

typedef struct int dfsch_random_generator_bytes_t(dfsch_object_t* rg,
                                                  size_t size,
                                                  uint8_t* get);

typedef struct dfsch_random_generator_type_t {
  dfsch_type_t parent;

  dfsch_random_generator_bytes_t bytes;  
};

dfsch_object_t* dfsch_current_random_generator();

long dfsch_random_long(dfsch_object_t* rg);
double dfsch_random_double(dfsch_object_t* rg);
void dfsch_random_bytes(dfsch_object_t* rg, size_t size, uint8_t bytes);
long dfsch_random_below(dfsch_object_t* rg, long max);

#endif
