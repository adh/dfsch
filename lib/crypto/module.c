#include <dfsch/lib/crypto.h>

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

  str = dfsch_make_string_for_write(ctx->cipher->block_size, &buf);

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

  str = dfsch_make_string_for_write(ctx->cipher->block_size, &buf);

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

  str = dfsch_make_string_for_write(blocks->len, &buf);

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

  str = dfsch_make_string_for_write(blocks->len, &buf);

  ctx->mode->decrypt(ctx, blocks->ptr, buf, 
                     blocks->len / ctx->cipher->cipher->block_size);

  return str;
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

  str = dfsch_make_string_for_write(hash->algo->result_len, &buf);

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

  str = dfsch_make_string_for_write(32, &buf);

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

  str = dfsch_make_string_for_write(32, &buf);
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

  return dfsch_rsa_generate_key(random_source, length);
}

DFSCH_DEFINE_PRIMITIVE(rsa_get_public_key, 
                       "Return public key matching given private key"){
  dfsch_rsa_private_key_t* private;
  DFSCH_RSA_PRIVATE_KEY_ARG(args, private);
  
  return dfsch_rsa_get_public_key(private);
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
  dfsch_crypto_hash_t* hash = NULL;
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

  return dfsch_crypto_oaep_encode(hash, random_source, length,
                                  message->ptr, message->len,
                                  label->ptr, label->len);
}

DFSCH_DEFINE_PRIMITIVE(oaep_decode,
                       "Encode message using OAEP scheme"){
  dfsch_crypto_hash_t* hash = NULL;
  dfsch_object_t* random_source = NULL;
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




static const uint8_t curve25519_basepoint[32] = {9};

void dfsch_module_crypto_register(dfsch_object_t* env){
  dfsch_package_t* crypto = dfsch_make_package("crypto");
  dfsch_provide(env, "crypto");

  dfsch_defconst_pkgcstr(env, crypto, "<aes>",
                         DFSCH_CRYPTO_AES_CIPHER);
  dfsch_defconst_pkgcstr(env, crypto, "<xtea>",
                         DFSCH_CRYPTO_XTEA_CIPHER);
  dfsch_defconst_pkgcstr(env, crypto, "<blowfish>",
                         DFSCH_CRYPTO_BLOWFISH_CIPHER);
  dfsch_defconst_pkgcstr(env, crypto, "<block-cipher>",
                         DFSCH_BLOCK_CIPHER_TYPE);

  dfsch_defconst_pkgcstr(env, crypto, "<ecb>",
                         DFSCH_CRYPTO_ECB_MODE);
  dfsch_defconst_pkgcstr(env, crypto, "<cbc>",
                         DFSCH_CRYPTO_CBC_MODE);
  dfsch_defconst_pkgcstr(env, crypto, "<cfb>",
                         DFSCH_CRYPTO_CFB_MODE);
  dfsch_defconst_pkgcstr(env, crypto, "<ofb>",
                         DFSCH_CRYPTO_OFB_MODE);
  dfsch_defconst_pkgcstr(env, crypto, "<ctr>",
                         DFSCH_CRYPTO_CTR_MODE);

  dfsch_defconst_pkgcstr(env, crypto, "<sha-256>",
                         DFSCH_CRYPTO_SHA256);
  dfsch_defconst_pkgcstr(env, crypto, "<hmac-sha-256>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_SHA256));
  dfsch_defconst_pkgcstr(env, crypto, "<sha-512>",
                         DFSCH_CRYPTO_SHA512);
  dfsch_defconst_pkgcstr(env, crypto, "<hmac-sha-512>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_SHA512));
  dfsch_defconst_pkgcstr(env, crypto, "<sha-1>",
                         DFSCH_CRYPTO_SHA1);
  dfsch_defconst_pkgcstr(env, crypto, "<hmac-sha-1>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_SHA1));
  dfsch_defconst_pkgcstr(env, crypto, "<md5>",
                         DFSCH_CRYPTO_MD5);
  dfsch_defconst_pkgcstr(env, crypto, "<hmac-md5>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_MD5));
  dfsch_defconst_pkgcstr(env, crypto, "<md4>",
                         DFSCH_CRYPTO_MD4);
  dfsch_defconst_pkgcstr(env, crypto, "<hmac-md4>",
                         dfsch_crypto_make_hmac(DFSCH_CRYPTO_MD4));
  dfsch_defconst_pkgcstr(env, crypto, "<hash>",
                         DFSCH_CRYPTO_HASH_TYPE);



  dfsch_defconst_pkgcstr(env, crypto, "setup-block-cipher",
                         DFSCH_PRIMITIVE_REF(setup_block_cipher));
  dfsch_defconst_pkgcstr(env, crypto, "encrypt-block",
                         DFSCH_PRIMITIVE_REF(encrypt_block));
  dfsch_defconst_pkgcstr(env, crypto, "decrypt-block",
                         DFSCH_PRIMITIVE_REF(decrypt_block));

  dfsch_defconst_pkgcstr(env, crypto, "setup-block-cipher-mode",
                         DFSCH_PRIMITIVE_REF(setup_block_cipher_mode));
  dfsch_defconst_pkgcstr(env, crypto, "encrypt-blocks",
                         DFSCH_PRIMITIVE_REF(encrypt_blocks));
  dfsch_defconst_pkgcstr(env, crypto, "decrypt-blocks",
                         DFSCH_PRIMITIVE_REF(decrypt_blocks));

  dfsch_defconst_pkgcstr(env, crypto, "setup-hash",
                         DFSCH_PRIMITIVE_REF(setup_hash));
  dfsch_defconst_pkgcstr(env, crypto, "hash-process",
                         DFSCH_PRIMITIVE_REF(hash_process));
  dfsch_defconst_pkgcstr(env, crypto, "hash-result",
                         DFSCH_PRIMITIVE_REF(hash_result));

  dfsch_defconst_pkgcstr(env, crypto, "*curve25519-basepoint*",
                         dfsch_make_string_buf(curve25519_basepoint, 32));
  dfsch_defconst_pkgcstr(env, crypto, "curve25519",
                         DFSCH_PRIMITIVE_REF(curve25519));
  dfsch_defconst_pkgcstr(env, crypto, "curve25519-private-key",
                         DFSCH_PRIMITIVE_REF(curve25519_private_key));


  dfsch_defconst_pkgcstr(env, crypto, "<rsa-public-key>",
                         DFSCH_RSA_PUBLIC_KEY_TYPE);
  dfsch_defconst_pkgcstr(env, crypto, "<rsa-private-key>",
                         DFSCH_RSA_PRIVATE_KEY_TYPE);
  dfsch_defconst_pkgcstr(env, crypto, "rsa-generate-key",
                         DFSCH_PRIMITIVE_REF(rsa_generate_key));
  dfsch_defconst_pkgcstr(env, crypto, "rsa-get-public-key",
                         DFSCH_PRIMITIVE_REF(rsa_get_public_key));
  dfsch_defconst_pkgcstr(env, crypto, "rsa-encrypt-number",
                         DFSCH_PRIMITIVE_REF(rsa_encrypt_number));
  dfsch_defconst_pkgcstr(env, crypto, "rsa-decrypt-number",
                         DFSCH_PRIMITIVE_REF(rsa_decrypt_number));

  dfsch_defconst_pkgcstr(env, crypto, "oaep-encode",
                         DFSCH_PRIMITIVE_REF(oaep_encode));
  dfsch_defconst_pkgcstr(env, crypto, "oaep-decode",
                         DFSCH_PRIMITIVE_REF(oaep_decode));

}
