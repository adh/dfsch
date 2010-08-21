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

static void memxor(uint8_t* dst, uint8_t* src, size_t count){
  while (count){
    *dst ^= *src;
    dst++;
    src++;
    count--;
  }
}

typedef struct cbc_context_t {
  dfsch_block_cipher_mode_context_t parent;
  uint8_t* iv;
} cbc_context_t;

static void cbc_setup(cbc_context_t* context,
                      uint8_t* iv,
                      size_t iv_len){
  if (iv_len != context->parent.cipher->cipher->block_size){
    dfsch_error("CBC IV length must be equal to block size", NULL);
  }

  context->iv = GC_MALLOC_ATOMIC(iv_len);
  memcpy(context->iv, iv, iv_len);
}

void cbc_encrypt(cbc_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->parent.cipher->cipher->block_size;
  int i;

  for (i = 0; i < blocks; i++){
    memxor(context->iv, in + (bsize * i), bsize);
    context->parent.cipher->cipher->encrypt(context->parent.cipher, 
                                            context->iv, 
                                            context->iv);
    memcpy(out + (bsize * i), context->iv, bsize);
  }
}

void cbc_decrypt(cbc_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->parent.cipher->cipher->block_size;
  int i;
  uint8_t tmp[bsize];

  for (i = 0; i < blocks; i++){
    memcpy(tmp, in + (bsize * i), bsize);
    context->parent.cipher->cipher->decrypt(context->parent.cipher, 
                                            in + (bsize * i), 
                                            out + (bsize * i));
    memxor(out + (bsize * i), context->iv, bsize);
    memcpy(context->iv, tmp, bsize);
  }
}

dfsch_block_cipher_mode_t dfsch_crypto_cbc_mode = {
  .type = {
    .type = DFSCH_BLOCK_CIPHER_MODE_TYPE,
    .name = "crypto:cbc",
    .size = sizeof(cbc_context_t),
  },

  .name = "CBC",

  .encrypt = cbc_encrypt,
  .decrypt = cbc_decrypt,
  .setup = cbc_setup
};

typedef struct cfb_context_t {
  dfsch_block_cipher_mode_context_t parent;
  uint8_t* iv;
} cfb_context_t;

static void cfb_setup(cfb_context_t* context,
                      uint8_t* iv,
                      size_t iv_len){
  if (iv_len != context->parent.cipher->cipher->block_size){
    dfsch_error("CFB IV length must be equal to block size", NULL);
  }

  context->iv = GC_MALLOC_ATOMIC(iv_len);
  memcpy(context->iv, iv, iv_len);
}

void cfb_encrypt(cfb_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->parent.cipher->cipher->block_size;
  int i;

  for (i = 0; i < blocks; i++){
    context->parent.cipher->cipher->encrypt(context->parent.cipher, 
                                            context->iv, 
                                            context->iv);
    memxor(context->iv, in + (bsize * i), bsize);
    memcpy(out + (bsize * i), context->iv, bsize);
  }
}

void cfb_decrypt(cfb_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->parent.cipher->cipher->block_size;
  int i;
  uint8_t tmp[bsize];

  for (i = 0; i < blocks; i++){
    memcpy(tmp, in + (bsize * i), bsize);
    context->parent.cipher->cipher->encrypt(context->parent.cipher, 
                                            context->iv, 
                                            out + (bsize * i));
    memxor(out + (bsize * i), tmp, bsize);
    memcpy(context->iv, tmp, bsize);
  }
}

dfsch_block_cipher_mode_t dfsch_crypto_cfb_mode = {
  .type = {
    .type = DFSCH_BLOCK_CIPHER_MODE_TYPE,
    .name = "crypto:cfb",
    .size = sizeof(cfb_context_t),
  },

  .name = "CFB",

  .encrypt = cfb_encrypt,
  .decrypt = cfb_decrypt,
  .setup = cfb_setup
};


typedef struct ofb_context_t {
  dfsch_block_cipher_mode_context_t parent;
  uint8_t* iv;
} ofb_context_t;

static void ofb_setup(ofb_context_t* context,
                      uint8_t* iv,
                      size_t iv_len){
  if (iv_len != context->parent.cipher->cipher->block_size){
    dfsch_error("OFB IV length must be equal to block size", NULL);
  }

  context->iv = GC_MALLOC_ATOMIC(iv_len);
  memcpy(context->iv, iv, iv_len);
}

void ofb_operate(ofb_context_t* context,
                 uint8_t* in,
                 uint8_t* out,
                 size_t blocks){
  size_t bsize = context->parent.cipher->cipher->block_size;
  int i;

  for (i = 0; i < blocks; i++){
    context->parent.cipher->cipher->encrypt(context->parent.cipher, 
                                            context->iv, 
                                            context->iv);
    memcpy(out + (bsize * i), in + (bsize * i), bsize);
    memxor(out + (bsize * i), context->iv, bsize);
  }
}

dfsch_block_cipher_mode_t dfsch_crypto_ofb_mode = {
  .type = {
    .type = DFSCH_BLOCK_CIPHER_MODE_TYPE,
    .name = "crypto:ofb",
    .size = sizeof(ofb_context_t),
  },

  .name = "OFB",

  .encrypt = ofb_operate,
  .decrypt = ofb_operate,
  .setup = ofb_setup
};


