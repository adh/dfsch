#include "dfsch/lib/crypto.h"
#include <dfsch/sha256.h>

void sha256_setup(dfsch_sha256_context_t * md, 
                  uint8_t* key, size_t keylen)
{
  if (keylen != 0){
    dfsch_error("SHA-512 is not keyed", NULL);
  }
  dfsch_sha256_setup(md);
}

dfsch_crypto_hash_t dfsch_crypto_sha256 = {
  .type = {
    .type = DFSCH_CRYPTO_HASH_TYPE,
    .name = "sha-256",
    .size = sizeof(dfsch_sha256_context_t),
  },

  .name = "SHA-256",
  
  .block_len = 64,
  .result_len = 32,

  .setup = sha256_setup,
  .process = dfsch_sha256_process,
  .result = dfsch_sha256_result
};
