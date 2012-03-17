#include <dfsch/lib/crypto.h>
#include <dfsch/bignum.h>

DFSCH_DEFINE_PRIMITIVE(setup_block_cipher,
                       "Create new block cipher context (expanded key)"){
  dfsch_block_cipher_t* cipher;
  dfsch_strbuf_t* key;

  DFSCH_BLOCK_CIPHER_ARG(args, cipher);
  DFSCH_BUFFER_ARG(args, key);
  DFSCH_ARG_END(args);

  return dfsch_setup_block_cipher(cipher, key->ptr, key->len);
}

DFSCH_DEFINE_PRIMITIVE(encrypt_block,
                       "Encrypt block with given block cipher"){
  dfsch_block_cipher_context_t* ctx;
  dfsch_strbuf_t* block;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, block);
  DFSCH_ARG_END(args);

  if (ctx->cipher->block_size != block->len){
    dfsch_error("Block length does not match cipher block length", NULL);
  }

  str = dfsch_alloc_byte_vector(&buf, ctx->cipher->block_size);

  ctx->cipher->encrypt(ctx, block->ptr, buf);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(decrypt_block,
                       "Encrypt block with given block cipher"){
  dfsch_block_cipher_context_t* ctx;
  dfsch_strbuf_t* block;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, block);
  DFSCH_ARG_END(args);

  if (ctx->cipher->block_size != block->len){
    dfsch_error("Block length does not match cipher block length", NULL);
  }

  str = dfsch_alloc_byte_vector(&buf, ctx->cipher->block_size);

  ctx->cipher->decrypt(ctx, block->ptr, buf);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(setup_block_cipher_mode,
                       "Setup block cipher mode of operation with supplied"
                       " keyed cipher and IV"){
  dfsch_block_cipher_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;
  dfsch_strbuf_t* iv;

  DFSCH_BLOCK_CIPHER_MODE_ARG(args, mode);
  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, cipher);
  DFSCH_BUFFER_ARG(args, iv);
  DFSCH_ARG_END(args);

  return dfsch_setup_block_cipher_mode(mode, cipher, iv->ptr, iv->len);  
}


DFSCH_DEFINE_PRIMITIVE(encrypt_blocks,
                       "Encrypt blocks with given block cipher mode"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* blocks;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BLOCK_CIPHER_MODE_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, blocks);
  DFSCH_ARG_END(args);

  if (blocks->len % ctx->cipher->cipher->block_size != 0){
    dfsch_error("Length of supplied string is not multiple of block size", 
                NULL);
  }

  str = dfsch_alloc_byte_vector(&buf, blocks->len);

  ctx->mode->encrypt(ctx, blocks->ptr, buf, 
                     blocks->len / ctx->cipher->cipher->block_size);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(decrypt_blocks,
                       "Decrypt blocks with given block cipher mode"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* blocks;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BLOCK_CIPHER_MODE_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, blocks);
  DFSCH_ARG_END(args);

  if (blocks->len % ctx->cipher->cipher->block_size != 0){
    dfsch_error("Length of supplied string is not multiple of block size", 
                NULL);
  }

  str = dfsch_alloc_byte_vector(&buf, blocks->len);

  ctx->mode->decrypt(ctx, blocks->ptr, buf, 
                     blocks->len / ctx->cipher->cipher->block_size);

  return str;
}


DFSCH_DEFINE_PRIMITIVE(encrypt_bytes,
                       "Pad bytes to whole number of blocks and encrypt with given block cipher mode"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* bytes;
  size_t padded_len;
  size_t blocks;
  char* buf;
  dfsch_object_t* str;
  int i;

  DFSCH_BLOCK_CIPHER_MODE_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, bytes);
  DFSCH_ARG_END(args);

  blocks = bytes->len / ctx->cipher->cipher->block_size + 1;
  padded_len = blocks * ctx->cipher->cipher->block_size;

  str = dfsch_alloc_byte_vector(&buf, padded_len);

  memcpy(buf, bytes->ptr, bytes->len);
  for (i = bytes->len; i < padded_len; i++){
    buf[i] = padded_len - bytes->len;
  }

  ctx->mode->encrypt(ctx, buf, buf, blocks);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(decrypt_bytes,
                       "Decrypt blocks with given block cipher mode and remove padding"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* blocks;
  char* midbuf;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BLOCK_CIPHER_MODE_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, blocks);
  DFSCH_ARG_END(args);

  if (blocks->len % ctx->cipher->cipher->block_size != 0){
    dfsch_error("Length of supplied string is not multiple of block size", 
                NULL);
  }

  midbuf = GC_MALLOC_ATOMIC(blocks->len);

  ctx->mode->decrypt(ctx, blocks->ptr, midbuf, 
                     blocks->len / ctx->cipher->cipher->block_size);

  if (midbuf[blocks->len - 1] > ctx->cipher->cipher->block_size){
    dfsch_error("Invalid padding", NULL);
  }

  str = dfsch_make_byte_vector_nocopy(midbuf, blocks->len - midbuf[blocks->len - 1]);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(encrypt_bytes_with_iv,
                       "Pad bytes to whole number of blocks and encrypt with given block cipher mode"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* bytes;
  size_t padded_len;
  size_t blocks;
  char* buf;
  dfsch_object_t* str;
  int i;
  dfsch_object_t* random_source = NULL;
  size_t block_size;
  dfsch_block_cipher_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;

  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, cipher);
  DFSCH_BLOCK_CIPHER_MODE_ARG(args, mode);
  DFSCH_BUFFER_ARG(args, bytes);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("random-source", random_source);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  if (!random_source){
    random_source = dfsch_crypto_get_fast_prng_state();
  }

  block_size = cipher->cipher->block_size;

  blocks = bytes->len / block_size + 1;
  padded_len = blocks * block_size;

  str = dfsch_alloc_byte_vector(&buf, padded_len + block_size);

  dfsch_random_get_bytes(random_source, buf, block_size);

  ctx = dfsch_setup_block_cipher_mode(mode, cipher, buf, block_size);

  memcpy(buf + block_size, bytes->ptr, bytes->len);
  for (i = bytes->len; i < padded_len; i++){
    buf[i + block_size] = padded_len - bytes->len;
  }

  ctx->mode->encrypt(ctx, buf + block_size, buf + block_size, blocks);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(decrypt_bytes_with_iv,
                       "Decrypt blocks with given block cipher mode and remove padding"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* blocks;
  uint8_t* midbuf;
  char* buf;
  dfsch_object_t* str;
  size_t block_size;
  dfsch_block_cipher_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;
 
  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, cipher);
  DFSCH_BLOCK_CIPHER_MODE_ARG(args, mode);
  DFSCH_BUFFER_ARG(args, blocks);
  DFSCH_ARG_END(args);

  block_size = cipher->cipher->block_size;

  if (blocks->len % block_size != 0){
    dfsch_error("Length of supplied string is not multiple of block size", 
                NULL);
  }

  if (blocks->len < block_size * 2){
    dfsch_error("Supplied string is shorter than two blocks", 
                NULL);
  }

  ctx = dfsch_setup_block_cipher_mode(mode, cipher, blocks->ptr, block_size);

  midbuf = GC_MALLOC_ATOMIC(blocks->len - block_size);

  ctx->mode->decrypt(ctx, blocks->ptr + block_size, midbuf, 
                     blocks->len / block_size);

  if (midbuf[blocks->len - block_size - 1] > block_size){
    dfsch_error("Invalid padding", NULL);
  }

  str = dfsch_make_byte_vector_nocopy(midbuf, blocks->len - block_size - midbuf[blocks->len - block_size - 1]);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(encrypt_bytes_with_iv_and_mac,
                       "Pad bytes to whole number of blocks and encrypt with given block cipher mode and append MAC"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* bytes;
  size_t padded_len;
  size_t blocks;
  char* buf;
  dfsch_object_t* str;
  int i;
  dfsch_object_t* random_source = NULL;
  size_t block_size;
  dfsch_block_cipher_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;
  dfsch_crypto_hash_context_t* hash;

  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, cipher);
  DFSCH_BLOCK_CIPHER_MODE_ARG(args, mode);
  DFSCH_CRYPTO_HASH_CONTEXT_ARG(args, hash);
  DFSCH_BUFFER_ARG(args, bytes);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("random-source", random_source);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  if (!random_source){
    random_source = dfsch_crypto_get_fast_prng_state();
  }

  block_size = cipher->cipher->block_size;

  blocks = bytes->len / block_size + 1;
  padded_len = blocks * block_size;

  str = dfsch_alloc_byte_vector(&buf, padded_len + block_size + hash->algo->result_len);

  dfsch_random_get_bytes(random_source, buf, block_size);

  ctx = dfsch_setup_block_cipher_mode(mode, cipher, buf, block_size);

  memcpy(buf + block_size, bytes->ptr, bytes->len);
  for (i = bytes->len; i < padded_len; i++){
    buf[i + block_size] = padded_len - bytes->len;
  }

  ctx->mode->encrypt(ctx, buf + block_size, buf + block_size, blocks);

  hash->algo->process(hash, buf, padded_len + block_size);
  hash->algo->result(hash, buf + padded_len + block_size);

  return str;
}

static uint8_t memxmp(uint8_t* dst, uint8_t* src, size_t count){
  uint8_t res = 0;
  while (count){
    res |= *src ^ *dst;
    dst++;
    src++;
    count--;
  }
  return res;
}

DFSCH_DEFINE_PRIMITIVE(decrypt_bytes_with_iv_and_mac,
                       "Verify MAC, decrypt blocks with given block cipher mode and remove padding"){
  dfsch_block_cipher_mode_context_t* ctx;
  dfsch_strbuf_t* blocks;
  uint8_t* midbuf;
  char* buf;
  dfsch_object_t* str;
  size_t block_size;
  dfsch_block_cipher_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;
  dfsch_crypto_hash_context_t* hash;
  uint8_t* authenticator;
  size_t enc_len;
 
  DFSCH_BLOCK_CIPHER_CONTEXT_ARG(args, cipher);
  DFSCH_BLOCK_CIPHER_MODE_ARG(args, mode);
  DFSCH_CRYPTO_HASH_CONTEXT_ARG(args, hash);
  DFSCH_BUFFER_ARG(args, blocks);
  DFSCH_ARG_END(args);

  block_size = cipher->cipher->block_size;

  if ((blocks->len - hash->algo->result_len) % block_size != 0){
    dfsch_error("Length of supplied string is not multiple of block size", 
                NULL);
  }

  if ((blocks->len - hash->algo->result_len) < block_size * 2){
    dfsch_error("Supplied string is shorter than two blocks", 
                NULL);
  }

  enc_len = (blocks->len - hash->algo->result_len);

  ctx = dfsch_setup_block_cipher_mode(mode, cipher, blocks->ptr, block_size);

  authenticator = GC_MALLOC_ATOMIC(hash->algo->result_len);
  hash->algo->process(hash, blocks->ptr, blocks->len - hash->algo->result_len);
  hash->algo->result(hash, authenticator);
  
  if (memxmp(authenticator, 
             blocks->ptr + (blocks->len - hash->algo->result_len),
             hash->algo->result_len) != 0){
    return NULL;
  }

  midbuf = GC_MALLOC_ATOMIC(enc_len - block_size);

  ctx->mode->decrypt(ctx, blocks->ptr + block_size, midbuf, 
                     (enc_len / block_size) - 1);

  if (midbuf[enc_len - block_size - 1] > block_size){
    dfsch_error("Invalid padding", NULL);
  }

  str = dfsch_make_byte_vector_nocopy(midbuf, 
                                      enc_len - block_size - midbuf[enc_len - block_size - 1]);

  return str;
}



DFSCH_DEFINE_PRIMITIVE(setup_stream_cipher,
                       "Create new stream cipher context"){
  dfsch_block_cipher_t* cipher;
  dfsch_strbuf_t* key;
  dfsch_strbuf_t* nonce;

  DFSCH_STREAM_CIPHER_ARG(args, cipher);
  DFSCH_BUFFER_ARG(args, key);
  DFSCH_BUFFER_ARG_OPT(args, nonce, DFSCH_EMPTY_STRBUF);
  DFSCH_ARG_END(args);

  return dfsch_setup_stream_cipher(cipher, 
                                   key->ptr, key->len, 
                                   nonce->ptr, nonce->len);
}

DFSCH_DEFINE_PRIMITIVE(get_keystream,
                       "Get output of stream cipher"){
  dfsch_stream_cipher_context_t* ctx;
  char* buf;
  dfsch_object_t* str;
  long result_len;

  DFSCH_STREAM_CIPHER_CONTEXT_ARG(args, ctx);
  DFSCH_ULONG_ARG(args, result_len);
  DFSCH_ARG_END(args);

  str = dfsch_alloc_byte_vector(&buf, result_len);

  memset(buf, 0, result_len);
  ctx->cipher->encrypt_bytes(ctx, buf, result_len);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(apply_stream_cipher,
                       "XOR bytes with cipher output"){
  dfsch_stream_cipher_context_t* ctx;
  char* buf;
  dfsch_object_t* str;
  dfsch_strbuf_t* bytes;


  DFSCH_STREAM_CIPHER_CONTEXT_ARG(args, ctx);
  DFSCH_BUFFER_ARG(args, bytes);
  DFSCH_ARG_END(args);

  str = dfsch_alloc_byte_vector(&buf, bytes->len);

  memcpy(buf, bytes->ptr, bytes->len);
  ctx->cipher->encrypt_bytes(ctx, buf, bytes->len);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(make_ofb_cipher,
                       "Create new stream cipher type implementing "
                       "OFB mode of block cipher"){
  dfsch_block_cipher_t* cipher;
  dfsch_strbuf_t* key;

  DFSCH_BLOCK_CIPHER_ARG(args, cipher);
  DFSCH_ARG_END(args);

  return dfsch_make_ofb_cipher(cipher);
}
DFSCH_DEFINE_PRIMITIVE(make_ctr_cipher,
                       "Create new stream cipher type implementing "
                       "CTR mode of block cipher"){
  dfsch_block_cipher_t* cipher;
  dfsch_strbuf_t* key;

  DFSCH_BLOCK_CIPHER_ARG(args, cipher);
  DFSCH_ARG_END(args);

  return dfsch_make_ctr_cipher(cipher);
}



DFSCH_DEFINE_PRIMITIVE(setup_hash,
                       "Create new hash context"){
  dfsch_crypto_hash_t* hash;
  dfsch_strbuf_t* key;

  DFSCH_CRYPTO_HASH_ARG(args, hash);
  DFSCH_BUFFER_ARG_OPT(args, key, DFSCH_EMPTY_STRBUF);
  DFSCH_ARG_END(args);

  return dfsch_crypto_hash_setup(hash, key->ptr, key->len);
}

DFSCH_DEFINE_PRIMITIVE(hash_process,
                       "Process data for hashing"){
  dfsch_crypto_hash_context_t* hash;
  dfsch_strbuf_t* data;

  DFSCH_CRYPTO_HASH_CONTEXT_ARG(args, hash);
  DFSCH_BUFFER_ARG(args, data);
  DFSCH_ARG_END(args);

  hash->algo->process(hash, data->ptr, data->len);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(hash_result,
                       "Get message digest"){
  dfsch_crypto_hash_context_t* hash;
  char* buf;
  dfsch_object_t* str;

  DFSCH_CRYPTO_HASH_CONTEXT_ARG(args, hash);
  DFSCH_ARG_END(args);

  str = dfsch_alloc_byte_vector(&buf, hash->algo->result_len);

  hash->algo->result(hash, buf);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(curve25519,
                       "Calculate curve25519 function"){
  dfsch_strbuf_t* secret;
  dfsch_strbuf_t* basepoint;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BUFFER_ARG(args, secret);
  DFSCH_BUFFER_ARG(args, basepoint);
  DFSCH_ARG_END(args);

  if (secret->len != 32){
    dfsch_error("Private key must be 32 byte string", NULL);
  }
  if (basepoint->len != 32){
    dfsch_error("Basepoint must be 32 byte string", NULL);
  }

  str = dfsch_alloc_byte_vector(&buf, 32);

  dfsch_crypto_curve25519(buf, secret->ptr, basepoint->ptr);

  return str;
}

DFSCH_DEFINE_PRIMITIVE(curve25519_private_key,
                       "Clamp private key for curve25519 function"){
  dfsch_strbuf_t* string;
  char* buf;
  dfsch_object_t* str;

  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  if (string->len != 32){
    dfsch_error("Private key must be 32 byte string", NULL);
  }

  str = dfsch_alloc_byte_vector(&buf, 32);
  memcpy(buf, string->ptr, 32);

  buf[0] &= 248;
  buf[31] &= 127;
  buf[31] |= 64;


  return str;
}

DFSCH_DEFINE_PRIMITIVE(rsa_generate_key, "Generate new RSA private key"){
  dfsch_object_t* random_source;
  int length;
  DFSCH_OBJECT_ARG(args, random_source);
  DFSCH_LONG_ARG(args, length);
  DFSCH_ARG_END(args);

  return dfsch_rsa_generate_key(random_source, length);
}

DFSCH_DEFINE_PRIMITIVE(rsa_get_public_key, 
                       "Return public key matching given private key"){
  dfsch_rsa_private_key_t* private;
  DFSCH_RSA_PRIVATE_KEY_ARG(args, private);
  DFSCH_ARG_END(args);

  return dfsch_rsa_get_public_key(private);
}

DFSCH_DEFINE_PRIMITIVE(rsa_public_key_2_list, 
                       "Return public key components as list"){
  dfsch_rsa_private_key_t* public;
  DFSCH_RSA_PUBLIC_KEY_ARG(args, public);
  DFSCH_ARG_END(args);
  return dfsch_rsa_public_key_2_list(public);
}
DFSCH_DEFINE_PRIMITIVE(rsa_private_key_2_list, 
                       "Return private key components as list"){
  dfsch_rsa_private_key_t* private;
  DFSCH_RSA_PRIVATE_KEY_ARG(args, private);
  DFSCH_ARG_END(args);
  return dfsch_rsa_private_key_2_list(private);  
}
DFSCH_DEFINE_PRIMITIVE(make_rsa_public_key, 
                       "Make RSA public key from components"){
  return dfsch_rsa_public_key_from_list(args);
}
DFSCH_DEFINE_PRIMITIVE(make_rsa_private_key,
                       "Make RSA private key from components"){
  return dfsch_rsa_private_key_from_list(args);
}
DFSCH_DEFINE_PRIMITIVE(list_2_rsa_public_key,
                       "Make RSA public key from list of components"){
  dfsch_object_t* list;
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);
  return dfsch_rsa_public_key_from_list(list);
}
DFSCH_DEFINE_PRIMITIVE(list_2_rsa_private_key,
                       "Make RSA private key from list of components"){
  dfsch_object_t* list;
  DFSCH_OBJECT_ARG(args, list);
  DFSCH_ARG_END(args);
  return dfsch_rsa_private_key_from_list(list);
}

DFSCH_DEFINE_PRIMITIVE(rsa_encrypt_number,
                       "RSA encryption operation"){
  dfsch_rsa_public_key_t* public;
  dfsch_object_t* message;
  DFSCH_RSA_PUBLIC_KEY_ARG(args, public);
  DFSCH_OBJECT_ARG(args, message);

  return dfsch_rsa_encrypt(public, message);
}
DFSCH_DEFINE_PRIMITIVE(rsa_decrypt_number,
                       "RSA decryption operation"){
  dfsch_rsa_private_key_t* private;
  dfsch_object_t* message;
  DFSCH_RSA_PRIVATE_KEY_ARG(args, private);
  DFSCH_OBJECT_ARG(args, message);

  return dfsch_rsa_decrypt(private, message);
}

DFSCH_DEFINE_PRIMITIVE(oaep_encode,
                       "Encode message using OAEP scheme"){
  dfsch_crypto_hash_t* hash = DFSCH_CRYPTO_SHA256;
  dfsch_object_t* random_source = NULL;
  dfsch_strbuf_t* message;
  dfsch_strbuf_t* label = DFSCH_EMPTY_STRBUF;
  size_t length;

  DFSCH_LONG_ARG(args, length);
  DFSCH_BUFFER_ARG(args, message);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("random-source", random_source);
  DFSCH_KEYWORD_GENERIC("hash", hash, dfsch_crypto_hash);
  DFSCH_KEYWORD_GENERIC("label", label, dfsch_string_to_buf);
  DFSCH_KEYWORD_PARSER_END(args);

  if (!random_source){
    random_source = dfsch_crypto_get_fast_prng_state();
  }

  return dfsch_crypto_oaep_encode(hash, random_source, length,
                                  message->ptr, message->len,
                                  label->ptr, label->len);
}

DFSCH_DEFINE_PRIMITIVE(oaep_decode,
                       "Encode message using OAEP scheme"){
  dfsch_crypto_hash_t* hash = DFSCH_CRYPTO_SHA256;
  dfsch_object_t* message;
  dfsch_strbuf_t* label = DFSCH_EMPTY_STRBUF;
  size_t length;

  DFSCH_LONG_ARG(args, length);
  DFSCH_OBJECT_ARG(args, message);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("hash", hash, dfsch_crypto_hash);
  DFSCH_KEYWORD_GENERIC("label", label, dfsch_string_to_buf);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_string_strbuf(dfsch_crypto_oaep_decode(hash, length,
                                                           message,
                                                           label->ptr, 
                                                           label->len));
}

DFSCH_DEFINE_PRIMITIVE(pss_encode,
                       "Encode signature using PSS"){
  dfsch_crypto_hash_t* hash = DFSCH_CRYPTO_SHA256;
  dfsch_object_t* random_source = NULL;
  dfsch_strbuf_t* mh;
  size_t length;

  DFSCH_LONG_ARG(args, length);
  DFSCH_BUFFER_ARG(args, mh);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("random-source", random_source);
  DFSCH_KEYWORD_GENERIC("hash", hash, dfsch_crypto_hash);
  DFSCH_KEYWORD_PARSER_END(args);

  if (!random_source){
    random_source = dfsch_crypto_get_fast_prng_state();
  }

  return dfsch_crypto_pss_encode(hash, random_source, length,
                                 mh->ptr, mh->len);
}

DFSCH_DEFINE_PRIMITIVE(pss_verify,
                       "Verify signature encoded using PSS"){
  dfsch_crypto_hash_t* hash = DFSCH_CRYPTO_SHA256;
  dfsch_object_t* signature;
  dfsch_strbuf_t* mh;
  size_t length;

  DFSCH_LONG_ARG(args, length);
  DFSCH_OBJECT_ARG(args, signature);
  DFSCH_BUFFER_ARG(args, mh);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("hash", hash, dfsch_crypto_hash);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_bool(dfsch_crypto_pss_verify(hash, length, signature,
                                            mh->ptr, mh->len));
}

DFSCH_DEFINE_PRIMITIVE(prng_state,
                       "Returns one of internal predefined PRNG states, "
                       ":fast (deterministic) or :safe "
                       "(periodicaly reseeded)"){
  int safe;
  DFSCH_FLAG_PARSER_BEGIN_ONE(args, "type");
  DFSCH_FLAG_VALUE("safe", 1, safe);
  DFSCH_FLAG_VALUE("fast", 0, safe);
  DFSCH_FLAG_PARSER_END(args);

  return safe 
    ? dfsch_crypto_get_safe_prng_state() 
    : dfsch_crypto_get_fast_prng_state(); 
}


static const uint8_t curve25519_basepoint[32] = {9};

DFSCH_DEFINE_PRIMITIVE(sign25519_generate_key, 
                       "Generate new SIGN25519 private key"){
  dfsch_object_t* random_source;
  int version;
  DFSCH_OBJECT_ARG(args, random_source);
  DFSCH_LONG_ARG_OPT(args, version, DFSCH_SIGN25519_CURRENT_VERSION);
  DFSCH_ARG_END(args);

  return dfsch_sign25519_generate_key(random_source, version);
}

DFSCH_DEFINE_PRIMITIVE(sign25519_get_public_key, 
                       "Return public key matching given private key"){
  dfsch_sign25519_private_key_t* private;
  DFSCH_SIGN25519_PRIVATE_KEY_ARG(args, private);
  DFSCH_ARG_END(args);

  return dfsch_sign25519_get_public_key(private);
}

DFSCH_DEFINE_PRIMITIVE(sign25519_sign,
                       "Sign message with given private key"){
  dfsch_sign25519_private_key_t* key;
  dfsch_strbuf_t* message;
  dfsch_strbuf_t* signature;
  DFSCH_SIGN25519_PRIVATE_KEY_ARG(args, key);
  DFSCH_BUFFER_ARG(args, message);
  DFSCH_ARG_END(args);

  signature = dfsch_sign25519_sign(key, message->ptr, message->len);

  return dfsch_make_byte_vector_nocopy(signature->ptr, signature->len);
}

DFSCH_DEFINE_PRIMITIVE(sign25519_verify,
                       "Verify message signature"){
  dfsch_sign25519_public_key_t* key;
  dfsch_strbuf_t* message;
  dfsch_strbuf_t* signature;
  DFSCH_SIGN25519_PUBLIC_KEY_ARG(args, key);
  DFSCH_BUFFER_ARG(args, signature);
  DFSCH_BUFFER_ARG(args, message);
  DFSCH_ARG_END(args);


  return dfsch_bool(dfsch_sign25519_verify(key, 
                                            message->ptr, message->len, 
                                            signature->ptr, signature->len));
}

DFSCH_DEFINE_PRIMITIVE(sign25519_private_key_2_byte_vector,
                       "Return key data as byte vector (64 bytes)"){
  dfsch_sign25519_private_key_t* key;
  dfsch_strbuf_t* res = dfsch_alloc_strbuf(64);
  DFSCH_SIGN25519_PRIVATE_KEY_ARG(args, key);
  DFSCH_ARG_END(args);
  
  dfsch_sign25519_export_private_key(key, res->ptr);
  return dfsch_make_byte_vector_nocopy(res->ptr, res->len);
}

DFSCH_DEFINE_PRIMITIVE(sign25519_public_key_2_byte_vector,
                       "Return key data as byte vector (32 bytes)"){
  dfsch_sign25519_public_key_t* key;
  dfsch_strbuf_t* res = dfsch_alloc_strbuf(32);
  DFSCH_SIGN25519_PUBLIC_KEY_ARG(args, key);
  DFSCH_ARG_END(args);
  
  dfsch_sign25519_export_public_key(key, res->ptr);
  return dfsch_make_byte_vector_nocopy(res->ptr, res->len);
}

DFSCH_DEFINE_PRIMITIVE(sign25519_make_private_key,
                       "Create private key from raw key bytes"){
  dfsch_strbuf_t* key_data;
  DFSCH_BUFFER_ARG(args, key_data);
  DFSCH_ARG_END(args);
  if (key_data->len != 64){
    dfsch_error("Invalid length of passed key data", NULL);
  }

  return dfsch_sign25519_make_private_key(key_data->ptr);
}
DFSCH_DEFINE_PRIMITIVE(sign25519_make_public_key,
                       "Create public key from raw key bytes"){
  dfsch_strbuf_t* key_data;
  DFSCH_BUFFER_ARG(args, key_data);
  DFSCH_ARG_END(args);
  if (key_data->len != 32){
    dfsch_error("Invalid length of passed key data", NULL);
  }

  return dfsch_sign25519_make_public_key(key_data->ptr);
}


DFSCH_DEFINE_PRIMITIVE(rsa_pss_sign,
                       "Sign message with RSA-PSS"){
  dfsch_rsa_private_key_t* key;
  dfsch_strbuf_t* message;
  dfsch_object_t* m;
  dfsch_crypto_hash_t* hash = DFSCH_CRYPTO_SHA256;
  dfsch_object_t* random_source = NULL;

  DFSCH_RSA_PRIVATE_KEY_ARG(args, key);
  DFSCH_BUFFER_ARG(args, message);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("random-source", random_source);
  DFSCH_KEYWORD_GENERIC("hash", hash, dfsch_crypto_hash);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);
  
  message = dfsch_crypto_hash_buffer(hash, message->ptr, message->len,
                                     NULL, 0);

  if (!random_source){
    random_source = dfsch_crypto_get_fast_prng_state();
  }

  m = dfsch_crypto_pss_encode(hash, 
                              random_source, 
                              dfsch_rsa_private_key_length(key),
                              message->ptr, message->len);

  m = dfsch_rsa_decrypt(key, m);
  
  return dfsch_make_byte_vector_strbuf(dfsch_bignum_to_bytes(dfsch_bignum_from_number(m)));
}

DFSCH_DEFINE_PRIMITIVE(rsa_pss_verify,
                       "Verify RSA-PSS signature"){
  dfsch_rsa_public_key_t* key;
  dfsch_strbuf_t* signature;
  dfsch_strbuf_t* message;
  dfsch_object_t* s;
  dfsch_crypto_hash_t* hash = DFSCH_CRYPTO_SHA256;

  DFSCH_RSA_PUBLIC_KEY_ARG(args, key);
  DFSCH_BUFFER_ARG(args, signature);
  DFSCH_BUFFER_ARG(args, message);
  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("hash", hash, dfsch_crypto_hash);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  message = dfsch_crypto_hash_buffer(hash, message->ptr, message->len,
                                     NULL, 0);

  s = dfsch_bignum_from_bytes(signature->ptr, signature->len, 0);
  s = dfsch_rsa_encrypt(key, s);
  
  return dfsch_bool(dfsch_crypto_pss_verify(hash, 
                                            dfsch_rsa_public_key_length(key), 
                                            s,
                                            message->ptr, message->len));
}

void dfsch_module_crypto_register(dfsch_object_t* env){
  dfsch_package_t* crypto = dfsch_make_package("crypto",
                                               "Cryptographic algorithms");
  dfsch_provide(env, "crypto");

  dfsch_crypto_put_entropy(&crypto, sizeof(dfsch_package_t*));
  dfsch_crypto_put_entropy(&env, sizeof(dfsch_object_t*));

  dfsch_defcanon_pkgcstr(env, crypto, "<aes>",
                         DFSCH_CRYPTO_AES_CIPHER);
  dfsch_defcanon_pkgcstr(env, crypto, "<xtea>",
                         DFSCH_CRYPTO_XTEA_CIPHER);
  dfsch_defcanon_pkgcstr(env, crypto, "<blowfish>",
                         DFSCH_CRYPTO_BLOWFISH_CIPHER);
  dfsch_defcanon_pkgcstr(env, crypto, "<block-cipher>",
                         DFSCH_BLOCK_CIPHER_TYPE);

  dfsch_defcanon_pkgcstr(env, crypto, "<ecb>",
                         DFSCH_CRYPTO_ECB_MODE);
  dfsch_defcanon_pkgcstr(env, crypto, "<cbc>",
                         DFSCH_CRYPTO_CBC_MODE);
  dfsch_defcanon_pkgcstr(env, crypto, "<cfb>",
                         DFSCH_CRYPTO_CFB_MODE);
  dfsch_defcanon_pkgcstr(env, crypto, "<ofb>",
                         DFSCH_CRYPTO_OFB_MODE);
  dfsch_defcanon_pkgcstr(env, crypto, "<ctr>",
                         DFSCH_CRYPTO_CTR_MODE);

  dfsch_defcanon_pkgcstr(env, crypto, "<rc4>",
                         DFSCH_CRYPTO_RC4_CIPHER);
  dfsch_defcanon_pkgcstr(env, crypto, "<rc4-drop768>",
                         DFSCH_CRYPTO_RC4_DROP768_CIPHER);
  dfsch_defcanon_pkgcstr(env, crypto, "<rc4-drop3072>",
                         DFSCH_CRYPTO_RC4_DROP3072_CIPHER);
  dfsch_defcanon_pkgcstr(env, crypto, "<stream-cipher>",
                         DFSCH_STREAM_CIPHER_TYPE);


  dfsch_defcanon_pkgcstr(env, crypto, "<sha-256>",
                         DFSCH_CRYPTO_SHA256);
  dfsch_defcanon_pkgcstr(env, crypto, "<hmac-sha-256>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_SHA256));
  dfsch_defcanon_pkgcstr(env, crypto, "<sha-512>",
                         DFSCH_CRYPTO_SHA512);
  dfsch_defcanon_pkgcstr(env, crypto, "<hmac-sha-512>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_SHA512));
  dfsch_defcanon_pkgcstr(env, crypto, "<sha-1>",
                         DFSCH_CRYPTO_SHA1);
  dfsch_defcanon_pkgcstr(env, crypto, "<hmac-sha-1>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_SHA1));
  dfsch_defcanon_pkgcstr(env, crypto, "<md5>",
                         DFSCH_CRYPTO_MD5);
  dfsch_defcanon_pkgcstr(env, crypto, "<hmac-md5>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_MD5));
  dfsch_defcanon_pkgcstr(env, crypto, "<md4>",
                         DFSCH_CRYPTO_MD4);
  dfsch_defcanon_pkgcstr(env, crypto, "<hmac-md4>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_MD4));
  dfsch_defcanon_pkgcstr(env, crypto, "<hash>",
                         DFSCH_CRYPTO_HASH_TYPE);

  dfsch_defcanon_pkgcstr(env, crypto, "setup-block-cipher",
                         DFSCH_PRIMITIVE_REF(setup_block_cipher));
  dfsch_defcanon_pkgcstr(env, crypto, "encrypt-block",
                         DFSCH_PRIMITIVE_REF(encrypt_block));
  dfsch_defcanon_pkgcstr(env, crypto, "decrypt-block",
                         DFSCH_PRIMITIVE_REF(decrypt_block));

  dfsch_defcanon_pkgcstr(env, crypto, "setup-block-cipher-mode",
                         DFSCH_PRIMITIVE_REF(setup_block_cipher_mode));
  dfsch_defcanon_pkgcstr(env, crypto, "encrypt-blocks",
                         DFSCH_PRIMITIVE_REF(encrypt_blocks));
  dfsch_defcanon_pkgcstr(env, crypto, "decrypt-blocks",
                         DFSCH_PRIMITIVE_REF(decrypt_blocks));
  dfsch_defcanon_pkgcstr(env, crypto, "encrypt-bytes",
                         DFSCH_PRIMITIVE_REF(encrypt_bytes));
  dfsch_defcanon_pkgcstr(env, crypto, "decrypt-bytes",
                         DFSCH_PRIMITIVE_REF(decrypt_bytes));
  dfsch_defcanon_pkgcstr(env, crypto, "encrypt-bytes-with-iv",
                         DFSCH_PRIMITIVE_REF(encrypt_bytes_with_iv));
  dfsch_defcanon_pkgcstr(env, crypto, "decrypt-bytes-with-iv",
                         DFSCH_PRIMITIVE_REF(decrypt_bytes_with_iv));
  dfsch_defcanon_pkgcstr(env, crypto, "encrypt-bytes-with-iv-and-mac",
                         DFSCH_PRIMITIVE_REF(encrypt_bytes_with_iv_and_mac));
  dfsch_defcanon_pkgcstr(env, crypto, "decrypt-bytes-with-iv-and-mac",
                         DFSCH_PRIMITIVE_REF(decrypt_bytes_with_iv_and_mac));



  dfsch_defcanon_pkgcstr(env, crypto, "setup-stream-cipher",
                         DFSCH_PRIMITIVE_REF(setup_stream_cipher));
  dfsch_defcanon_pkgcstr(env, crypto, "get-keystream",
                         DFSCH_PRIMITIVE_REF(get_keystream));
  dfsch_defcanon_pkgcstr(env, crypto, "apply-stream-cipher",
                         DFSCH_PRIMITIVE_REF(apply_stream_cipher));

  dfsch_defcanon_pkgcstr(env, crypto, "make-ofb-cipher",
                         DFSCH_PRIMITIVE_REF(make_ofb_cipher));
  dfsch_defcanon_pkgcstr(env, crypto, "make-ctr-cipher",
                         DFSCH_PRIMITIVE_REF(make_ctr_cipher));

  dfsch_defcanon_pkgcstr(env, crypto, "setup-hash",
                         DFSCH_PRIMITIVE_REF(setup_hash));
  dfsch_defcanon_pkgcstr(env, crypto, "hash-process",
                         DFSCH_PRIMITIVE_REF(hash_process));
  dfsch_defcanon_pkgcstr(env, crypto, "hash-result",
                         DFSCH_PRIMITIVE_REF(hash_result));

  dfsch_defcanon_pkgcstr(env, crypto, "*curve25519-basepoint*",
                         dfsch_make_string_buf(curve25519_basepoint, 32));
  dfsch_defcanon_pkgcstr(env, crypto, "curve25519",
                         DFSCH_PRIMITIVE_REF(curve25519));
  dfsch_defcanon_pkgcstr(env, crypto, "curve25519-private-key",
                         DFSCH_PRIMITIVE_REF(curve25519_private_key));


  dfsch_defcanon_pkgcstr(env, crypto, "<rsa-public-key>",
                         DFSCH_RSA_PUBLIC_KEY_TYPE);
  dfsch_defcanon_pkgcstr(env, crypto, "<rsa-private-key>",
                         DFSCH_RSA_PRIVATE_KEY_TYPE);
  dfsch_defcanon_pkgcstr(env, crypto, "rsa-generate-key",
                         DFSCH_PRIMITIVE_REF(rsa_generate_key));
  dfsch_defcanon_pkgcstr(env, crypto, "rsa-get-public-key",
                         DFSCH_PRIMITIVE_REF(rsa_get_public_key));
  dfsch_defcanon_pkgcstr(env, crypto, "rsa-public-key->list",
                         DFSCH_PRIMITIVE_REF(rsa_public_key_2_list));
  dfsch_defcanon_pkgcstr(env, crypto, "rsa-private-key->list",
                         DFSCH_PRIMITIVE_REF(rsa_private_key_2_list));
  dfsch_defcanon_pkgcstr(env, crypto, "make-rsa-public-key",
                         DFSCH_PRIMITIVE_REF(make_rsa_public_key));
  dfsch_defcanon_pkgcstr(env, crypto, "make-rsa-private-key",
                         DFSCH_PRIMITIVE_REF(make_rsa_private_key));
  dfsch_defcanon_pkgcstr(env, crypto, "list->rsa-public-key",
                         DFSCH_PRIMITIVE_REF(list_2_rsa_public_key));
  dfsch_defcanon_pkgcstr(env, crypto, "list->rsa-private-key",
                         DFSCH_PRIMITIVE_REF(list_2_rsa_private_key));

  dfsch_defcanon_pkgcstr(env, crypto, "rsa-encrypt-number",
                         DFSCH_PRIMITIVE_REF(rsa_encrypt_number));
  dfsch_defcanon_pkgcstr(env, crypto, "rsa-decrypt-number",
                         DFSCH_PRIMITIVE_REF(rsa_decrypt_number));

  dfsch_defcanon_pkgcstr(env, crypto, "oaep-encode",
                         DFSCH_PRIMITIVE_REF(oaep_encode));
  dfsch_defcanon_pkgcstr(env, crypto, "oaep-decode",
                         DFSCH_PRIMITIVE_REF(oaep_decode));
  dfsch_defcanon_pkgcstr(env, crypto, "pss-encode",
                         DFSCH_PRIMITIVE_REF(pss_encode));
  dfsch_defcanon_pkgcstr(env, crypto, "pss-verify",
                         DFSCH_PRIMITIVE_REF(pss_verify));

  dfsch_defcanon_pkgcstr(env, crypto, "rsa-pss-sign",
                         DFSCH_PRIMITIVE_REF(rsa_pss_sign));
  dfsch_defcanon_pkgcstr(env, crypto, "rsa-pss-verify",
                         DFSCH_PRIMITIVE_REF(rsa_pss_verify));

  dfsch_defcanon_pkgcstr(env, crypto, "prng-state",
                         DFSCH_PRIMITIVE_REF(prng_state));

  dfsch_defcanon_pkgcstr(env, crypto, "<sign25519-public-key>",
                         DFSCH_SIGN25519_PUBLIC_KEY_TYPE);
  dfsch_defcanon_pkgcstr(env, crypto, "<sign25519-private-key>",
                         DFSCH_SIGN25519_PRIVATE_KEY_TYPE);
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-generate-key",
                         DFSCH_PRIMITIVE_REF(sign25519_generate_key));
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-get-public-key",
                         DFSCH_PRIMITIVE_REF(sign25519_get_public_key));
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-sign",
                         DFSCH_PRIMITIVE_REF(sign25519_sign));
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-verify",
                         DFSCH_PRIMITIVE_REF(sign25519_verify));

  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-private-key->byte-vector",
                         DFSCH_PRIMITIVE_REF(sign25519_private_key_2_byte_vector));
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-public-key->byte-vector",
                         DFSCH_PRIMITIVE_REF(sign25519_public_key_2_byte_vector));
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-make-private-key",
                         DFSCH_PRIMITIVE_REF(sign25519_make_private_key));
  dfsch_defcanon_pkgcstr(env, crypto, "sign25519-make-public-key",
                         DFSCH_PRIMITIVE_REF(sign25519_make_public_key));



  dfsch_define_method_pkgcstr_1(env, crypto, "sign-message",
                              DFSCH_RSA_PRIVATE_KEY_TYPE,
                              DFSCH_PRIMITIVE_REF(rsa_pss_sign));
  dfsch_define_method_pkgcstr_1(env, crypto, "sign-message",
                              DFSCH_SIGN25519_PRIVATE_KEY_TYPE,
                              DFSCH_PRIMITIVE_REF(sign25519_sign));

  dfsch_define_method_pkgcstr(env, crypto, "verify-message",
                              NULL, dfsch_list(1, DFSCH_RSA_PUBLIC_KEY_TYPE),
                              DFSCH_PRIMITIVE_REF(rsa_pss_verify));
  dfsch_define_method_pkgcstr(env, crypto, "verify-message",
                              NULL, 
                              dfsch_list(1, DFSCH_SIGN25519_PUBLIC_KEY_TYPE),
                              DFSCH_PRIMITIVE_REF(sign25519_verify));

  dfsch_define_method_pkgcstr_1(env, crypto, "get-public-key",
                              DFSCH_RSA_PRIVATE_KEY_TYPE,
                              DFSCH_PRIMITIVE_REF(rsa_get_public_key));
  dfsch_define_method_pkgcstr_1(env, crypto, "get-public-key",
                              DFSCH_SIGN25519_PRIVATE_KEY_TYPE,
                              DFSCH_PRIMITIVE_REF(sign25519_get_public_key));

}
