#ifndef H__dfsch_crypto___internal__
#define H__dfsch_crypto___internal__


typedef struct sha512_context_t {
  dfsch_crypto_hash_t* algo;
  uint64_t  length, state[8];
  unsigned long curlen;
  unsigned char buf[128];
} sha512_context_t;

void dfsch_sha512_setup(sha512_context_t * md, 
                        uint8_t* key, size_t keylen);
void dfsch_sha512_process(sha512_context_t * md, 
                          const unsigned char* in, 
                          unsigned long inlen);
void dfsch_sha512_result(sha512_context_t * md, unsigned char *out);


#endif
