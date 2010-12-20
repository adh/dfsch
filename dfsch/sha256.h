#ifndef H__dfsch__sha256__
#define H__dfsch__sha256__

#include <dfsch/dfsch.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct dfsch_sha256_context_t{
  dfsch_type_t* type;
  uint64_t length;
  uint32_t state[8], curlen;
  unsigned char buf[64];
} dfsch_sha256_context_t;

void dfsch_sha256_setup(dfsch_sha256_context_t* md);

void dfsch_sha256_process(dfsch_sha256_context_t* md, 
                          const unsigned char *in,
                          unsigned long inlen);
void dfsch_sha256_result(dfsch_sha256_context_t * md, unsigned char *out);


#endif
