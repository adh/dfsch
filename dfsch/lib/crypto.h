#ifndef H__dfsch__lib_crypto__
#define H__dfsch__lib_crypto__

#include <dfsch/dfsch.h>
#include <dfsch/random.h>
#include <stdint.h>

typedef struct dfsch_block_cipher_context_t dfsch_block_cipher_context_t;

typedef void (*dfsch_block_cipher_operation_t)
(dfsch_block_cipher_context_t* context,
 void* in,
 void* out);
typedef void (*dfsch_block_cipher_setup_t)
(dfsch_block_cipher_context_t* context,
 uint8_t* key,
 size_t key_len);

typedef struct dfsch_block_cipher_t {
  dfsch_type_t type;
  char* name;
  
  size_t block_size;
  
  dfsch_block_cipher_operation_t encrypt;
  dfsch_block_cipher_operation_t decrypt;
  dfsch_block_cipher_setup_t setup;

  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_block_cipher_t;

dfsch_block_cipher_t* dfsch_block_cipher(dfsch_object_t* obj);
#define DFSCH_BLOCK_CIPHER_ARG(al, name)                             \
  DFSCH_GENERIC_ARG(al, name, dfsch_block_cipher_t*, dfsch_block_cipher)

extern dfsch_block_cipher_t dfsch_crypto_aes_cipher;
#define DFSCH_CRYPTO_AES_CIPHER (&dfsch_crypto_aes_cipher)
extern dfsch_block_cipher_t dfsch_crypto_xtea_cipher;
#define DFSCH_CRYPTO_XTEA_CIPHER (&dfsch_crypto_xtea_cipher)
extern dfsch_block_cipher_t dfsch_crypto_blowfish_cipher;
#define DFSCH_CRYPTO_BLOWFISH_CIPHER (&dfsch_crypto_blowfish_cipher)

struct dfsch_block_cipher_context_t{
  dfsch_block_cipher_t* cipher;
};

extern dfsch_type_t dfsch_block_cipher_type;
#define DFSCH_BLOCK_CIPHER_TYPE (&dfsch_block_cipher_type)

dfsch_block_cipher_context_t* 
dfsch_setup_block_cipher(dfsch_block_cipher_t* cipher,
                         uint8_t* key,
                         size_t key_len);
int dfsch_block_cipher_context_p(dfsch_object_t* obj);
dfsch_block_cipher_context_t*  dfsch_block_cipher_context(dfsch_object_t* obj);


#define DFSCH_BLOCK_CIPHER_CONTEXT_ARG(al, name)             \
  DFSCH_GENERIC_ARG(al, name, dfsch_block_cipher_context_t*, \
                    dfsch_block_cipher_context)


typedef struct dfsch_block_cipher_mode_context_t 
dfsch_block_cipher_mode_context_t;

typedef void (*dfsch_block_cipher_mode_operation_t)
(dfsch_block_cipher_mode_context_t* context,
 uint8_t* in,
 uint8_t* out,
 size_t blocks);

typedef void (*dfsch_block_cipher_mode_setup_t)
(dfsch_block_cipher_mode_context_t* cipher,
 uint8_t* iv,
 size_t iv_len);

typedef struct dfsch_block_cipher_mode_t {
  dfsch_type_t type;

  char* name;

  dfsch_block_cipher_mode_operation_t decrypt;
  dfsch_block_cipher_mode_operation_t encrypt;
  dfsch_block_cipher_mode_setup_t setup;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_block_cipher_mode_t;

extern dfsch_block_cipher_mode_t dfsch_crypto_ecb_mode;
#define DFSCH_CRYPTO_ECB_MODE (&dfsch_crypto_ecb_mode)
extern dfsch_block_cipher_mode_t dfsch_crypto_cbc_mode;
#define DFSCH_CRYPTO_CBC_MODE (&dfsch_crypto_cbc_mode)
extern dfsch_block_cipher_mode_t dfsch_crypto_cfb_mode;
#define DFSCH_CRYPTO_CFB_MODE (&dfsch_crypto_cfb_mode)
extern dfsch_block_cipher_mode_t dfsch_crypto_ofb_mode;
#define DFSCH_CRYPTO_OFB_MODE (&dfsch_crypto_ofb_mode)
extern dfsch_block_cipher_mode_t dfsch_crypto_ctr_mode;
#define DFSCH_CRYPTO_CTR_MODE (&dfsch_crypto_ctr_mode)

dfsch_block_cipher_t* dfsch_block_cipher_mode(dfsch_object_t* obj);
#define DFSCH_BLOCK_CIPHER_MODE_ARG(al, name)                           \
  DFSCH_GENERIC_ARG(al, name, dfsch_block_cipher_mode_t*,               \
                    dfsch_block_cipher_mode)

extern dfsch_type_t dfsch_block_cipher_mode_type;
#define DFSCH_BLOCK_CIPHER_MODE_TYPE (&dfsch_block_cipher_mode_type)


dfsch_block_cipher_mode_context_t* 
dfsch_setup_block_cipher_mode(dfsch_block_cipher_mode_t* mode,
                              dfsch_block_cipher_context_t* cipher,
                              uint8_t* iv,
                              size_t iv_len);
int dfsch_block_cipher_mode_context_p(dfsch_object_t* obj);
dfsch_block_cipher_mode_context_t*  
dfsch_block_cipher_mode_context(dfsch_object_t* obj);


#define DFSCH_BLOCK_CIPHER_MODE_CONTEXT_ARG(al, name)             \
  DFSCH_GENERIC_ARG(al, name, dfsch_block_cipher_mode_context_t*, \
                    dfsch_block_cipher_mode_context)


struct dfsch_block_cipher_mode_context_t {
  dfsch_block_cipher_mode_t* mode;
  dfsch_block_cipher_context_t* cipher;
};


typedef struct dfsch_crypto_hash_context_t dfsch_crypto_hash_context_t;

typedef void (*dfsch_crypto_hash_setup_t)(dfsch_crypto_hash_context_t* ctx,
                                          uint8_t* key,
                                          size_t key_len);
typedef void (*dfsch_crypto_hash_process_t)(dfsch_crypto_hash_context_t* ctx,
                                            uint8_t* buf,
                                            size_t len);
typedef void (*dfsch_crypto_hash_result_t)(dfsch_crypto_hash_context_t* ctx,
                                           uint8_t* res);

typedef struct dfsch_crypto_hash_t {
  dfsch_type_t type;
  
  char* name;

  size_t block_len; /* size of block used internally by hash function */
  size_t result_len;
  
  dfsch_crypto_hash_setup_t setup;
  dfsch_crypto_hash_process_t process;
  dfsch_crypto_hash_result_t result;
} dfsch_crypto_hash_t;

extern dfsch_type_t dfsch_crypto_hash_type;
#define DFSCH_CRYPTO_HASH_TYPE (&dfsch_crypto_hash_type)

dfsch_block_cipher_t* dfsch_crypto_hash(dfsch_object_t* obj);
#define DFSCH_CRYPTO_HASH_ARG(al, name)                                 \
  DFSCH_GENERIC_ARG(al, name, dfsch_crypto_hash_t*,                     \
                    dfsch_crypto_hash)

dfsch_crypto_hash_context_t* dfsch_crypto_hash_setup(dfsch_crypto_hash_t* hash,
                                                     uint8_t* key,
                                                     size_t keylen);
int dfsch_crypto_hash_context_p(dfsch_object_t* obj);
dfsch_crypto_hash_context_t* dfsch_crypto_hash_context(dfsch_object_t* obj);

#define DFSCH_CRYPTO_HASH_CONTEXT_ARG(al, name)             \
  DFSCH_GENERIC_ARG(al, name, dfsch_crypto_hash_context_t*, \
                    dfsch_crypto_hash_context)

struct dfsch_crypto_hash_context_t {
  dfsch_crypto_hash_t* algo;
};

extern dfsch_crypto_hash_t dfsch_crypto_sha256;
#define DFSCH_CRYPTO_SHA256 (&dfsch_crypto_sha256)
extern dfsch_crypto_hash_t dfsch_crypto_sha512;
#define DFSCH_CRYPTO_SHA512 (&dfsch_crypto_sha512)
extern dfsch_crypto_hash_t dfsch_crypto_sha1;
#define DFSCH_CRYPTO_SHA1 (&dfsch_crypto_sha1)
extern dfsch_crypto_hash_t dfsch_crypto_md5;
#define DFSCH_CRYPTO_MD5 (&dfsch_crypto_md5)
extern dfsch_crypto_hash_t dfsch_crypto_md4;
#define DFSCH_CRYPTO_MD4 (&dfsch_crypto_md4)


dfsch_crypto_hash_t* dfsch_crypto_make_hmac(dfsch_crypto_hash_t* hash);
extern dfsch_type_t dfsch_crypto_hmac_type;
#define DFSCH_CRYPTO_HMAC_TYPE (&dfsch_crypto_hmac_type)


void dfsch_crypto_curve25519(uint8_t *mypublic, 
                             const uint8_t *secret, 
                             const uint8_t *basepoint);

extern dfsch_type_t dfsch_rsa_public_key_type;
#define DFSCH_RSA_PUBLIC_KEY_TYPE (&dfsch_rsa_public_key_type)
extern dfsch_type_t dfsch_rsa_private_key_type;
#define DFSCH_RSA_PRIVATE_KEY_TYPE (&dfsch_rsa_private_key_type)

typedef struct dfsch_rsa_public_key_t dfsch_rsa_public_key_t;
typedef struct dfsch_rsa_private_key_t dfsch_rsa_private_key_t;

#define DFSCH_RSA_PUBLIC_KEY_ARG(al, name)                              \
  DFSCH_TYPED_ARG(al, name, dfsch_rsa_public_key_t*,                    \
                  DFSCH_RSA_PUBLIC_KEY_TYPE)
#define DFSCH_RSA_PRIVATE_KEY_ARG(al, name)             \
  DFSCH_TYPED_ARG(al, name, dfsch_rsa_private_key_t*,   \
                  DFSCH_RSA_PRIVATE_KEY_TYPE)

dfsch_rsa_private_key_t* dfsch_rsa_generate_key(dfsch_object_t* random_source,
                                                int length);
dfsch_rsa_public_key_t* dfsch_rsa_get_public_key(dfsch_rsa_private_key_t* prk);

dfsch_object_t* dfsch_rsa_public_key_2_list(dfsch_rsa_public_key_t* puk);
dfsch_object_t* dfsch_rsa_private_key_2_list(dfsch_rsa_private_key_t* prk);
dfsch_rsa_public_key_t* dfsch_rsa_public_key_from_list(dfsch_object_t* list);
dfsch_rsa_private_key_t* dfsch_rsa_private_key_from_list(dfsch_object_t* list);

dfsch_object_t* dfsch_rsa_encrypt(dfsch_rsa_public_key_t* puk,
                                  dfsch_object_t* mn);
dfsch_object_t* dfsch_rsa_decrypt(dfsch_rsa_private_key_t* prk,
                                  dfsch_object_t* cn);

dfsch_object_t* dfsch_crypto_oaep_encode(dfsch_crypto_hash_t* hash,
                                         dfsch_object_t* random_source,
                                         size_t len,
                                         uint8_t* data,
                                         size_t dlen,
                                         uint8_t* label,
                                         size_t llen);
dfsch_strbuf_t* dfsch_crypto_oaep_decode(dfsch_crypto_hash_t* hash,
                                         size_t len,
                                         dfsch_object_t* m,
                                         uint8_t* label,
                                         size_t llen);
dfsch_object_t* dfsch_crypto_pss_encode(dfsch_crypto_hash_t* hash,
                                        dfsch_object_t* random_source,
                                        size_t len,
                                        uint8_t* mh,
                                        size_t mhlen);
int dfsch_crypto_pss_verify(dfsch_crypto_hash_t* hash,
                            size_t len,
                            dfsch_object_t* s,
                            uint8_t* mh,
                            size_t mhlen);

extern dfsch_random_state_type_t dfsch_crypto_prng_state_type;
#define DFSCH_CRYPTO_PRNG_STATE_TYPE (&dfsch_crypto_prng_state_type)

dfsch_object_t* dfsch_crypto_make_prng_state(uint8_t* seed, int seed_len,
                                             int use_system_sources_p);
dfsch_object_t* dfsch_crypto_get_fast_prng_state();
dfsch_object_t* dfsch_crypto_get_safe_prng_state();

void dfsch_crypto_put_entropy(uint8_t* buf, size_t len);


/* Salsa20 - used internally by PRNG code */
typedef struct dfsch_salsa20_state_t {
  uint32_t input[32];
} dfsch_salsa20_state_t;

void dfsch_salsa20_setkey(dfsch_salsa20_state_t* state, uint8_t key[32]);
void dfsch_salsa20_addkey(dfsch_salsa20_state_t* state, uint8_t key[32]);
void dfsch_salsa20_setiv(dfsch_salsa20_state_t* state, uint64_t iv);
void dfsch_salsa20_get_keystream_block(dfsch_salsa20_state_t* state, 
                                       uint8_t output[64]);
void dfsch_salsa20_seek(dfsch_salsa20_state_t* state, uint64_t offset);

typedef struct dfsch_sign25519_public_key_t dfsch_sign25519_public_key_t;
typedef struct dfsch_sign25519_private_key_t dfsch_sign25519_private_key_t;

extern dfsch_type_t dfsch_sign25519_public_key_type;
#define DFSCH_SIGN25519_PUBLIC_KEY_TYPE (&dfsch_sign25519_public_key_type) 
extern dfsch_type_t dfsch_sign25519_private_key_type;
#define DFSCH_SIGN25519_PRIVATE_KEY_TYPE (&dfsch_sign25519_private_key_type) 

#define DFSCH_SIGN25519_PUBLIC_KEY_ARG(al, name)                       \
  DFSCH_TYPED_ARG(al, name, dfsch_sign25519_public_key_t*,             \
                  DFSCH_SIGN25519_PUBLIC_KEY_TYPE)
#define DFSCH_SIGN25519_PRIVATE_KEY_ARG(al, name)             \
  DFSCH_TYPED_ARG(al, name, dfsch_sign25519_private_key_t*,   \
                  DFSCH_SIGN25519_PRIVATE_KEY_TYPE)

#define DFSCH_SIGN25519_VERSION_256_SHA512   0

#define DFSCH_SIGN25519_CURRENT_VERSION DFSCH_SIGN25519_VERSION_256_SHA512

dfsch_sign25519_private_key_t* 
dfsch_sign25519_generate_key(dfsch_object_t* random_source,
                             int version);
dfsch_sign25519_public_key_t* 
dfsch_sign25519_get_public_key(dfsch_sign25519_private_key_t* pk);

dfsch_strbuf_t* dfsch_sign25519_sign(dfsch_sign25519_private_key_t* key,
                                     char* m, size_t len);
int dfsch_sign25519_verify(dfsch_sign25519_public_key_t* key,
                           char* m, size_t len,
                           char* s, size_t slen);


#endif
