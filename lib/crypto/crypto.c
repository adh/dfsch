#include <dfsch/lib/crypto.h>
#include <dfsch/magic.h>

dfsch_type_t dfsch_block_cipher_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name = "crypto:block-cipher",
  .size = sizeof(dfsch_block_cipher_t)
};

dfsch_block_cipher_t* dfsch_block_cipher(dfsch_object_t* obj){
  return DFSCH_ASSERT_TYPE(obj, DFSCH_BLOCK_CIPHER_TYPE);
}

dfsch_block_cipher_context_t* 
dfsch_setup_block_cipher(dfsch_block_cipher_t* cipher,
                            uint8_t* key,
                            size_t key_len){
  dfsch_block_cipher_context_t* ctx = dfsch_make_object(cipher);
  
  cipher->setup(ctx, key, key_len);

  return ctx;
}

int dfsch_block_cipher_context_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(DFSCH_TYPE_OF(obj)) == DFSCH_BLOCK_CIPHER_TYPE;
}

dfsch_block_cipher_context_t* 
dfsch_block_cipher_context(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!dfsch_block_cipher_context_p(o)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_error("Not a block cipher context", o);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return (dfsch_block_cipher_context_t*)o;
}

extern dfsch_type_t dfsch_block_cipher_mode_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name = "crypto:block-cipher-mode",
  .size = sizeof(dfsch_block_cipher_mode_t)  
};

dfsch_block_cipher_t* dfsch_block_cipher_mode(dfsch_object_t* obj){
  return DFSCH_ASSERT_TYPE(obj, DFSCH_BLOCK_CIPHER_MODE_TYPE);  
}

dfsch_block_cipher_mode_context_t* 
dfsch_setup_block_cipher_mode(dfsch_block_cipher_mode_t* mode,
                              dfsch_block_cipher_context_t* cipher,
                              uint8_t* iv,
                              size_t iv_len){
  dfsch_block_cipher_mode_context_t* ctx = dfsch_make_object(mode);
  
  ctx->cipher = cipher;
  mode->setup(ctx, iv, iv_len);

  return ctx;  
}
int dfsch_block_cipher_mode_context_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(DFSCH_TYPE_OF(obj)) == DFSCH_BLOCK_CIPHER_MODE_TYPE;
}
dfsch_block_cipher_mode_context_t*  
dfsch_block_cipher_mode_context(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!dfsch_block_cipher_mode_context_p(o)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_error("Not a block cipher mode context", o);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return (dfsch_block_cipher_mode_context_t*)o;  
}

dfsch_type_t dfsch_crypto_hash_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .size = sizeof(dfsch_crypto_hash_t),
  .name = "crypto:hash"
};

dfsch_block_cipher_t* dfsch_crypto_hash(dfsch_object_t* obj){
  return DFSCH_ASSERT_TYPE(obj, DFSCH_CRYPTO_HASH_TYPE);
}

dfsch_crypto_hash_context_t* dfsch_crypto_hash_setup(dfsch_crypto_hash_t* hash,
                                                     uint8_t* key,
                                                     size_t key_len){
  dfsch_crypto_hash_context_t* ctx = dfsch_make_object(hash);
  
  hash->setup(ctx, key, key_len);

  return ctx;
}
int dfsch_crypto_hash_context_p(dfsch_object_t* obj){
  return DFSCH_TYPE_OF(DFSCH_TYPE_OF(obj)) == DFSCH_CRYPTO_HASH_TYPE;
}
dfsch_crypto_hash_context_t* dfsch_crypto_hash_context(dfsch_object_t* obj){
  dfsch_object_t* o = obj;
  while (!dfsch_crypto_hash_context_p(o)){
    DFSCH_WITH_RETRY_WITH_RESTART(DFSCH_SYM_USE_VALUE, 
                                  "Retry with alternate value") {
      dfsch_error("Not a block cryptographic hash context", o);
    } DFSCH_END_WITH_RETRY_WITH_RESTART(o);
  }
  return (dfsch_crypto_hash_context_t*)o;  
}

dfsch_strbuf_t* dfsch_crypto_hash_buffer(dfsch_crypto_hash_t* hash,
                                         char* buf, size_t len,
                                         char* key, size_t klen){
  dfsch_crypto_hash_context_t* ctx = dfsch_crypto_hash_setup(hash, key, klen);
  dfsch_strbuf_t* res = dfsch_alloc_strbuf(hash->result_len);

  ctx->algo->process(ctx, buf, len);
  ctx->algo->result(ctx, res->ptr);
  return res;
}
