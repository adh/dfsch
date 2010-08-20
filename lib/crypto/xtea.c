/* LibTomCrypt, modular cryptographic library -- Tom St Denis
 *
 * LibTomCrypt is a library that provides various cryptographic
 * algorithms in a highly modular and flexible manner.
 *
 * The library is free for all purposes without any express
 * guarantee it works.
 *
 * Tom St Denis, tomstdenis@gmail.com, http://libtom.org
 */

/**
  @file xtea.c
  Implementation of LTC_XTEA, Tom St Denis
*/
#include <dfsch/lib/crypto.h>
#include "macros.h"

typedef struct xtea_key_t {
  dfsch_block_cipher_t* cipher;
  unsigned long A[32], B[32];  
} xtea_key_t;

static void xtea_setup(xtea_key_t* ctx, const unsigned char *key, int keylen)
{
   unsigned long x, sum, K[4];
   
   /* check arguments */
   if (keylen != 16) {
     dfsch_error("Invalid key length", DFSCH_MAKE_FIXNUM(keylen));
   }

   /* load key */
   LOAD32L(K[0], key+0);
   LOAD32L(K[1], key+4);
   LOAD32L(K[2], key+8);
   LOAD32L(K[3], key+12);
   
   for (x = sum = 0; x < 32; x++) {
       ctx->A[x] = (sum + K[sum&3]) & 0xFFFFFFFFUL;
       sum = (sum + 0x9E3779B9UL) & 0xFFFFFFFFUL;
       ctx->B[x] = (sum + K[(sum>>11)&3]) & 0xFFFFFFFFUL;
   }
}

/**
  Encrypts a block of text with LTC_XTEA
  @param pt The input plaintext (8 bytes)
  @param ct The output ciphertext (8 bytes)
  @param skey The key as scheduled
  @return CRYPT_OK if successful
*/
static void xtea_encrypt(xtea_key_t* key, 
                         const unsigned char *pt, unsigned char *ct)
{
   unsigned long y, z;
   int r;

   LOAD32L(y, &pt[0]);
   LOAD32L(z, &pt[4]);
   for (r = 0; r < 32; r += 4) {
       y = (y + ((((z<<4)^(z>>5)) + z) ^ key->A[r])) & 0xFFFFFFFFUL;
       z = (z + ((((y<<4)^(y>>5)) + y) ^ key->B[r])) & 0xFFFFFFFFUL;

       y = (y + ((((z<<4)^(z>>5)) + z) ^ key->A[r+1])) & 0xFFFFFFFFUL;
       z = (z + ((((y<<4)^(y>>5)) + y) ^ key->B[r+1])) & 0xFFFFFFFFUL;

       y = (y + ((((z<<4)^(z>>5)) + z) ^ key->A[r+2])) & 0xFFFFFFFFUL;
       z = (z + ((((y<<4)^(y>>5)) + y) ^ key->B[r+2])) & 0xFFFFFFFFUL;

       y = (y + ((((z<<4)^(z>>5)) + z) ^ key->A[r+3])) & 0xFFFFFFFFUL;
       z = (z + ((((y<<4)^(y>>5)) + y) ^ key->B[r+3])) & 0xFFFFFFFFUL;
   }
   STORE32L(y, &ct[0]);
   STORE32L(z, &ct[4]);
}

/**
  Decrypts a block of text with LTC_XTEA
  @param ct The input ciphertext (8 bytes)
  @param pt The output plaintext (8 bytes)
  @param skey The key as scheduled 
  @return CRYPT_OK if successful
*/
static void xtea_decrypt(xtea_key_t* key, 
                         const unsigned char *ct, unsigned char *pt)
{
   unsigned long y, z;
   int r;

   LOAD32L(y, &ct[0]);
   LOAD32L(z, &ct[4]);
   for (r = 31; r >= 0; r -= 4) {
       z = (z - ((((y<<4)^(y>>5)) + y) ^ key->B[r])) & 0xFFFFFFFFUL;
       y = (y - ((((z<<4)^(z>>5)) + z) ^ key->A[r])) & 0xFFFFFFFFUL;

       z = (z - ((((y<<4)^(y>>5)) + y) ^ key->B[r-1])) & 0xFFFFFFFFUL;
       y = (y - ((((z<<4)^(z>>5)) + z) ^ key->A[r-1])) & 0xFFFFFFFFUL;

       z = (z - ((((y<<4)^(y>>5)) + y) ^ key->B[r-2])) & 0xFFFFFFFFUL;
       y = (y - ((((z<<4)^(z>>5)) + z) ^ key->A[r-2])) & 0xFFFFFFFFUL;

       z = (z - ((((y<<4)^(y>>5)) + y) ^ key->B[r-3])) & 0xFFFFFFFFUL;
       y = (y - ((((z<<4)^(z>>5)) + z) ^ key->A[r-3])) & 0xFFFFFFFFUL;
   }
   STORE32L(y, &pt[0]);
   STORE32L(z, &pt[4]);
}

dfsch_block_cipher_t dfsch_crypto_xtea_cipher = {
  .type = {
    .type = DFSCH_BLOCK_CIPHER_TYPE,
    .name = "crypto:xtea",
    .size = sizeof(xtea_key_t)
  },

  .name = "XTEA",

  .block_size = 8,
  
  .encrypt = xtea_encrypt,
  .decrypt = xtea_decrypt,
  .setup = xtea_setup
};

