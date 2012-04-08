#include <dfsch/lib/crypto.h>

static void ecb_setup(dfsch_block_cipher_mode_context_t* cipher,
                      uint8_t* iv,
                      size_t iv_len){
  if (iv_len != 0){
    dfsch_error("ECB mode has no IV", NULL);
  }
}

static void ecb_encrypt(dfsch_block_cipher_mode_context_t* context,
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

static void ecb_decrypt(dfsch_block_cipher_mode_context_t* context,
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

static void cbc_encrypt(cbc_context_t* context,
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

static void cbc_decrypt(cbc_context_t* context,
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

static void cfb_encrypt(cfb_context_t* context,
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

static void cfb_decrypt(cfb_context_t* context,
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

static void ofb_operate(ofb_context_t* context,
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

/* This implementation of CTR mode comes from NIST recommendation,
   which is different in significant details from AES-CTR used by TLS
   and IPsec (which are even mutually different). CTR mode can use
   various additional data from underlying protocol, which
   unfortunately means that each protocol uses completely different
   method of construing CTR value */

typedef struct ctr_context_t {
  dfsch_block_cipher_mode_context_t parent;
  uint8_t* ctr;
} ctr_context_t;

static void ctr_setup(ctr_context_t* context,
                      uint8_t* iv,
                      size_t iv_len){
  if (iv_len != context->parent.cipher->cipher->block_size){
    dfsch_error("CTR IV length must be equal to block size", NULL);
  }

  context->ctr = GC_MALLOC_ATOMIC(iv_len);
  memcpy(context->ctr, iv, iv_len);
}

static void ctr_operate(ctr_context_t* context,
                        uint8_t* in,
                        uint8_t* out,
                        size_t blocks){
  size_t bsize = context->parent.cipher->cipher->block_size;
  int i;
  int j;
  uint8_t tmp[bsize];

  for (i = 0; i < blocks; i++){
    context->parent.cipher->cipher->encrypt(context->parent.cipher, 
                                            context->ctr, 
                                            tmp);
    memcpy(out + (bsize * i), in + (bsize * i), bsize);
    memxor(out + (bsize * i), tmp, bsize);

    /* Increment counter, little endian */
    for (j = 0; j < bsize; j++){
      context->ctr[j]++;
      if (context->ctr[j] != 0){
        break;
      }
    }
  }
}

dfsch_block_cipher_mode_t dfsch_crypto_ctr_mode = {
  .type = {
    .type = DFSCH_BLOCK_CIPHER_MODE_TYPE,
    .name = "crypto:ctr",
    .size = sizeof(ctr_context_t),
  },

  .name = "CTR",

  .encrypt = ctr_operate,
  .decrypt = ctr_operate,
  .setup = ctr_setup
};

typedef struct block_stream_mode_t {
  dfsch_stream_cipher_t parent;
  dfsch_block_cipher_t* cipher;
} block_stream_mode_t;

dfsch_type_t dfsch_block_stream_mode_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STREAM_CIPHER_TYPE,
  .name = "block-stream-mode",
  .size = sizeof(block_stream_mode_t),
};

typedef struct block_stream_context_t {
  block_stream_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;
  uint8_t* next_input;
  uint8_t* last_output;
  size_t output_offset;
  size_t output_size;
} block_stream_context_t;

static void bs_ofb_setup(block_stream_context_t* ctx,
                         uint8_t *key,
                         size_t keylen,
                         uint8_t *nonce,
                         size_t nonce_len){
  if (nonce_len != ctx->mode->cipher->block_size){
    dfsch_error("Nonce for OFB mode must be same size as cipher's block", 
                NULL);
  }

  ctx->cipher = dfsch_setup_block_cipher(ctx->mode->cipher, key, keylen);
  ctx->next_input = GC_MALLOC_ATOMIC(ctx->mode->cipher->block_size);
  ctx->last_output = GC_MALLOC_ATOMIC(ctx->mode->cipher->block_size);
  ctx->output_offset = ctx->mode->cipher->block_size;
  ctx->output_size = ctx->mode->cipher->block_size;

  memcpy(ctx->next_input, nonce, ctx->output_size);
}

static void bs_ofb_encrypt_bytes(block_stream_context_t* ctx,
                                 uint8_t* out,
                                 size_t outlen){
  while (outlen){
    if (ctx->output_offset >= ctx->output_size){
      ctx->cipher->cipher->encrypt(ctx->cipher, 
                                   ctx->next_input, 
                                   ctx->last_output);
      memcpy(ctx->next_input, ctx->last_output, ctx->output_size);
      ctx->output_offset = 0;
    }
    *out ^= ctx->last_output[ctx->output_offset];
    ctx->output_offset++;
    out++;
    outlen--;
  }
}

dfsch_stream_cipher_t* dfsch_make_ofb_cipher(dfsch_block_cipher_t* cipher){
  block_stream_mode_t* bs = dfsch_make_object(DFSCH_BLOCK_STREAM_MODE_TYPE);
    
  bs->parent.name = dfsch_saprintf("%s in OFB mode",
                                   cipher->name);
  bs->parent.type.name = dfsch_saprintf("%s-ofb", cipher->type.name);
  bs->parent.type.size = sizeof(block_stream_context_t);
  
  bs->parent.setup = bs_ofb_setup;
  bs->parent.encrypt_bytes = bs_ofb_encrypt_bytes;

  return bs;
}

static void bs_ctr_setup(block_stream_context_t* ctx,
                         uint8_t *key,
                         size_t keylen,
                         uint8_t *nonce,
                         size_t nonce_len){
  if (nonce_len != ctx->mode->cipher->block_size){
    dfsch_error("Nonce for OFB mode must be same size as cipher's block", 
                NULL);
  }

  ctx->cipher = dfsch_setup_block_cipher(ctx->mode->cipher, key, keylen);
  ctx->next_input = GC_MALLOC_ATOMIC(ctx->mode->cipher->block_size);
  ctx->last_output = GC_MALLOC_ATOMIC(ctx->mode->cipher->block_size);
  ctx->output_offset = ctx->mode->cipher->block_size;
  ctx->output_size = ctx->mode->cipher->block_size;

  memcpy(ctx->next_input, nonce, ctx->output_size);
}

static void bs_ctr_encrypt_bytes(block_stream_context_t* ctx,
                                 uint8_t* out,
                                 size_t outlen){
  int i;

  while (outlen){
    if (ctx->output_offset >= ctx->output_size){
      ctx->cipher->cipher->encrypt(ctx->cipher, 
                                   ctx->next_input, 
                                   ctx->last_output);
      
      for (i = 0; i < ctx->output_size; i++){
        ctx->next_input[i]++;
        if (ctx->next_input[i] != 0){
          break;
        }
      }

      ctx->output_offset = 0;
    }
    *out ^= ctx->last_output[ctx->output_offset];
    ctx->output_offset++;
    out++;
    outlen--;
  }
}


dfsch_stream_cipher_t* dfsch_make_ctr_cipher(dfsch_block_cipher_t* cipher){
  block_stream_mode_t* bs = dfsch_make_object(DFSCH_BLOCK_STREAM_MODE_TYPE);

  bs->parent.name = dfsch_saprintf("%s in CTR mode",
                                   cipher->name);
  bs->parent.type.name = dfsch_saprintf("%s-ctr", cipher->type.name);
  bs->parent.type.size = sizeof(block_stream_context_t);

  bs->parent.setup = bs_ctr_setup;
  bs->parent.encrypt_bytes = bs_ctr_encrypt_bytes;

  return bs;
}
