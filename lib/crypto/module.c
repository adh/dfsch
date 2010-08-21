#include <dfsch/lib/crypto.h>

DFSCH_DEFINE_PRIMITIVE(setup_block_cipher,
                       "Create new block cipher context (expanded key)"){
  dfsch_block_cipher_t* cipher;
  dfsch_strbuf_t* key;

  DFSCH_BLOCK_CIPHER_ARG(args, cipher);
  DFSCH_BUFFER_ARG(args, key);

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

  if (blocks->len % ctx->cipher->cipher->block_size != 0){
    dfsch_error("Length of supplied string is not multiple of block size", 
                NULL);
  }

  str = dfsch_make_string_for_write(blocks->len, &buf);

  ctx->mode->decrypt(ctx, blocks->ptr, buf, 
                     blocks->len / ctx->cipher->cipher->block_size);

  return str;
}


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

}
