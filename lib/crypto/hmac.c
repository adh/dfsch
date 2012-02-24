#include <dfsch/lib/crypto.h>

typedef struct hmac_t {
  dfsch_crypto_hash_t parent;
  dfsch_crypto_hash_t* hash;
} hmac_t;


dfsch_type_t dfsch_crypto_hmac_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_CRYPTO_HASH_TYPE,
  .name = "crypto:hmac-type",
  .size = sizeof(hmac_t)
};

typedef struct hmac_context_t {
  hmac_t* hmac;
} hmac_context_t;

static void hmac_setup(hmac_context_t* ctx, uint8_t* key, size_t keylen){
  int i;
  
  if (keylen > ctx->hmac->hash->block_len){
    dfsch_error("HMAC key too long", NULL);
  }

  ctx->hmac->hash->setup(ctx, NULL, 0);
  memset(((uint8_t*)ctx) + ctx->hmac->hash->type.size, 0, 
         ctx->hmac->hash->block_len);
  memcpy(((uint8_t*)ctx) + ctx->hmac->hash->type.size, key, keylen);

  for (i = 0; i < ctx->hmac->hash->block_len; i++){
    (((uint8_t*)ctx) + ctx->hmac->hash->type.size)[i] ^= 0x36;
  }
  
  ctx->hmac->hash->process(ctx, 
                           ((uint8_t*)ctx) + ctx->hmac->hash->type.size,
                           ctx->hmac->hash->block_len);
}

static void hmac_result(hmac_context_t* ctx, uint8_t* res){
  uint8_t buf[ctx->hmac->hash->result_len];
  dfsch_crypto_hash_context_t* oc;
  int i;

  for (i = 0; i < ctx->hmac->hash->block_len; i++){
    (((uint8_t*)ctx) + ctx->hmac->hash->type.size)[i] ^= (0x36 ^ 0x5c);
  }

  ctx->hmac->hash->result(ctx, buf);
  
  oc = dfsch_crypto_hash_setup(ctx->hmac->hash, NULL, 0);
  oc->algo->process(oc, 
                    ((uint8_t*)ctx) + ctx->hmac->hash->type.size,
                    ctx->hmac->hash->block_len);
  oc->algo->process(oc, buf, ctx->hmac->hash->result_len);
  oc->algo->result(oc, res);
}

dfsch_crypto_hash_t* dfsch_crypto_make_hmac(dfsch_crypto_hash_t* hash){
  hmac_t* hmac = dfsch_make_object(DFSCH_CRYPTO_HMAC_TYPE);

  hmac->hash = hash;

  hmac->parent.type.name = dfsch_saprintf("hmac-%s", hash->type.name);
  hmac->parent.type.size = hash->type.size + hash->block_len;

  hmac->parent.name = dfsch_saprintf("HMAC-%s", hash->name);
  hmac->parent.block_len = hash->block_len;
  hmac->parent.result_len = hash->result_len;

  hmac->parent.setup = hmac_setup;
  hmac->parent.process = hash->process;
  hmac->parent.result = hmac_result;
  return hmac;
}
