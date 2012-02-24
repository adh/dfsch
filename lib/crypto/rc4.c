#include "dfsch/lib/crypto.h"

typedef struct rc4_context_t {
  dfsch_block_cipher_t* cipher;
  uint8_t i;
  uint8_t j;
  uint8_t s[256];
} rc4_context_t;

static void rc4_encrypt_bytes(rc4_context_t* ctx,
                              uint8_t *keystream,
                              size_t keystreamlen){
  uint8_t t;

  while (keystreamlen){
    ctx->i = (ctx->i + 1) & 0xff;
    ctx->j = (ctx->j + ctx->s[ctx->i]) & 0xff;
    t = ctx->s[ctx->j];
    ctx->s[ctx->j] = ctx->s[ctx->i];
    ctx->s[ctx->i] = t;
    *keystream ^= ctx->s[(ctx->s[ctx->i] + ctx->s[ctx->j]) & 0xff];
    keystream++;
    keystreamlen--;
  }
}

static void rc4_setup(rc4_context_t* ctx,
                      uint8_t* key,
                      size_t key_len,
                      uint8_t *nonce,
                      size_t nonce_len){
  int i;
  int j;
  uint8_t t;

  if (key_len < 1 || key_len > 256){
    dfsch_error("Key length for RC4 must be between 1 and 256", NULL);
  }
  if (nonce_len != 0){
    dfsch_error("RC4 does not support nonces", NULL);
  }

  for (i = 0; i < 256; i++){
    ctx->s[i] = i;
  }

  j = 0;

  for (i = 0; i < 256; i++){
    j = (j + ctx->s[i] + key[i % key_len]) & 0xff;
    t = ctx->s[j];
    ctx->s[j] = ctx->s[i];
    ctx->s[i] = t;
  }
  
  ctx->j = 0;
  ctx->i = 0;
}

dfsch_stream_cipher_t dfsch_crypto_rc4_cipher = {
  .type = {
    .type = DFSCH_STREAM_CIPHER_TYPE,
    .size = sizeof(rc4_context_t),
    .name = "crypto:rc4"
  },
  .name = "RC4",

  .setup = rc4_setup,
  .encrypt_bytes = rc4_encrypt_bytes,
};
