#include <dfsch/lib/crypto.h>

static void ecb_setup(dfsch_block_cipher_mode_context_t* cipher,
                      uint8_t* iv,
                      size_t iv_len){
  if (iv_len != 0){
    dfsch_error("ECB mode has no IV", NULL);
  }
}

void ecb_encrypt(dfsch_block_cipher_mode_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->cipher->cipher->block_size;
  int i;

  for (i = 0; i < blocks; i++){
    context->cipher->cipher->encrypt(context->cipher, 
                                     in + (bsize * i), out + (bsize * i));
  }
}

void ecb_decrypt(dfsch_block_cipher_mode_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->cipher->cipher->block_size;
  int i;

  for (i = 0; i < blocks; i++){
    context->cipher->cipher->decrypt(context->cipher, 
                                     in + (bsize * i), out + (bsize * i));
  }
}

dfsch_block_cipher_mode_t dfsch_crypto_ecb_mode = {
  .type = {
    .type = DFSCH_BLOCK_CIPHER_MODE_TYPE,
    .name = "crypto:ecb",
    .size = sizeof(dfsch_block_cipher_mode_context_t),
  },

  .name = "ECB",

  .encrypt = ecb_encrypt,
  .decrypt = ecb_decrypt,
  .setup = ecb_setup
};


