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
   @param md4.c
   Submitted by Dobes Vandermeer  (dobes@smartt.com) 
*/

#include <dfsch/lib/crypto.h>
#include "macros.h"

typedef struct md4_state {
    ulong64 length;
    ulong32 state[4], curlen;
    unsigned char buf[64];
} md4_context_t;


#define S11 3
#define S12 7
#define S13 11
#define S14 19
#define S21 3
#define S22 5
#define S23 9
#define S24 13
#define S31 3
#define S32 9
#define S33 11
#define S34 15

/* F, G and H are basic LTC_MD4 functions. */
#define F(x, y, z) (z ^ (x & (y ^ z)))
#define G(x, y, z) ((x & y) | (z & (x | y)))
#define H(x, y, z) ((x) ^ (y) ^ (z))

/* ROTATE_LEFT rotates x left n bits. */
#define ROTATE_LEFT(x, n) ROLc(x, n)

/* FF, GG and HH are transformations for rounds 1, 2 and 3 */ 
/* Rotation is separate from addition to prevent recomputation */ 

#define FF(a, b, c, d, x, s) { \
    (a) += F ((b), (c), (d)) + (x); \
    (a) = ROTATE_LEFT ((a), (s)); \
  }
#define GG(a, b, c, d, x, s) { \
    (a) += G ((b), (c), (d)) + (x) + 0x5a827999UL; \
    (a) = ROTATE_LEFT ((a), (s)); \
  }
#define HH(a, b, c, d, x, s) { \
    (a) += H ((b), (c), (d)) + (x) + 0x6ed9eba1UL; \
    (a) = ROTATE_LEFT ((a), (s)); \
  }

static int  md4_compress(md4_context_t *md, unsigned char *buf)
{
    ulong32 x[16], a, b, c, d;
    int i;

    /* copy state */
    a = md->state[0];
    b = md->state[1];
    c = md->state[2];
    d = md->state[3];

    /* copy the state into 512-bits into W[0..15] */
    for (i = 0; i < 16; i++) {
        LOAD32L(x[i], buf + (4*i));
    }
 
    /* Round 1 */ 
    FF (a, b, c, d, x[ 0], S11); /* 1 */ 
    FF (d, a, b, c, x[ 1], S12); /* 2 */ 
    FF (c, d, a, b, x[ 2], S13); /* 3 */ 
    FF (b, c, d, a, x[ 3], S14); /* 4 */ 
    FF (a, b, c, d, x[ 4], S11); /* 5 */ 
    FF (d, a, b, c, x[ 5], S12); /* 6 */ 
    FF (c, d, a, b, x[ 6], S13); /* 7 */ 
    FF (b, c, d, a, x[ 7], S14); /* 8 */ 
    FF (a, b, c, d, x[ 8], S11); /* 9 */ 
    FF (d, a, b, c, x[ 9], S12); /* 10 */
    FF (c, d, a, b, x[10], S13); /* 11 */ 
    FF (b, c, d, a, x[11], S14); /* 12 */
    FF (a, b, c, d, x[12], S11); /* 13 */
    FF (d, a, b, c, x[13], S12); /* 14 */ 
    FF (c, d, a, b, x[14], S13); /* 15 */ 
    FF (b, c, d, a, x[15], S14); /* 16 */ 
    
    /* Round 2 */ 
    GG (a, b, c, d, x[ 0], S21); /* 17 */ 
    GG (d, a, b, c, x[ 4], S22); /* 18 */ 
    GG (c, d, a, b, x[ 8], S23); /* 19 */ 
    GG (b, c, d, a, x[12], S24); /* 20 */ 
    GG (a, b, c, d, x[ 1], S21); /* 21 */ 
    GG (d, a, b, c, x[ 5], S22); /* 22 */ 
    GG (c, d, a, b, x[ 9], S23); /* 23 */ 
    GG (b, c, d, a, x[13], S24); /* 24 */ 
    GG (a, b, c, d, x[ 2], S21); /* 25 */ 
    GG (d, a, b, c, x[ 6], S22); /* 26 */ 
    GG (c, d, a, b, x[10], S23); /* 27 */ 
    GG (b, c, d, a, x[14], S24); /* 28 */ 
    GG (a, b, c, d, x[ 3], S21); /* 29 */ 
    GG (d, a, b, c, x[ 7], S22); /* 30 */ 
    GG (c, d, a, b, x[11], S23); /* 31 */ 
    GG (b, c, d, a, x[15], S24); /* 32 */ 
    
    /* Round 3 */
    HH (a, b, c, d, x[ 0], S31); /* 33 */ 
    HH (d, a, b, c, x[ 8], S32); /* 34 */ 
    HH (c, d, a, b, x[ 4], S33); /* 35 */ 
    HH (b, c, d, a, x[12], S34); /* 36 */ 
    HH (a, b, c, d, x[ 2], S31); /* 37 */ 
    HH (d, a, b, c, x[10], S32); /* 38 */ 
    HH (c, d, a, b, x[ 6], S33); /* 39 */ 
    HH (b, c, d, a, x[14], S34); /* 40 */ 
    HH (a, b, c, d, x[ 1], S31); /* 41 */ 
    HH (d, a, b, c, x[ 9], S32); /* 42 */ 
    HH (c, d, a, b, x[ 5], S33); /* 43 */ 
    HH (b, c, d, a, x[13], S34); /* 44 */ 
    HH (a, b, c, d, x[ 3], S31); /* 45 */ 
    HH (d, a, b, c, x[11], S32); /* 46 */ 
    HH (c, d, a, b, x[ 7], S33); /* 47 */ 
    HH (b, c, d, a, x[15], S34); /* 48 */ 
    

    /* Update our state */
    md->state[0] = md->state[0] + a;
    md->state[1] = md->state[1] + b;
    md->state[2] = md->state[2] + c;
    md->state[3] = md->state[3] + d;
}

/**
   Initialize the hash state
   @param md   The hash state you wish to initialize
   @return CRYPT_OK if successful
*/
int md4_setup(md4_context_t * md, uint8_t* key, size_t keylen)
{

  if (keylen != 0){
    dfsch_error("MD4 has no key", NULL);
  }

   md->state[0] = 0x67452301UL;
   md->state[1] = 0xefcdab89UL;
   md->state[2] = 0x98badcfeUL;
   md->state[3] = 0x10325476UL;
   md->length  = 0;
   md->curlen  = 0;
}

/**
   Process a block of memory though the hash
   @param md     The hash state
   @param in     The data to hash
   @param inlen  The length of the data (octets)
   @return CRYPT_OK if successful
*/
HASH_PROCESS(md4_process, md4_compress, md4_context_t, 64)

/**
   Terminate the hash to get the digest
   @param md  The hash state
   @param out [out] The destination of the hash (16 bytes)
   @return CRYPT_OK if successful
*/
int md4_result(md4_context_t * md, unsigned char *out)
{
    int i;

    /* increase the length of the message */
    md->length += md->curlen * 8;

    /* append the '1' bit */
    md->buf[md->curlen++] = (unsigned char)0x80;

    /* if the length is currently above 56 bytes we append zeros
     * then compress.  Then we can fall back to padding zeros and length
     * encoding like normal.
     */
    if (md->curlen > 56) {
        while (md->curlen < 64) {
            md->buf[md->curlen++] = (unsigned char)0;
        }
        md4_compress(md, md->buf);
        md->curlen = 0;
    }

    /* pad upto 56 bytes of zeroes */
    while (md->curlen < 56) {
        md->buf[md->curlen++] = (unsigned char)0;
    }

    /* store length */
    STORE64L(md->length, md->buf+56);
    md4_compress(md, md->buf);

    /* copy output */
    for (i = 0; i < 4; i++) {
        STORE32L(md->state[i], out+(4*i));
    }
}


dfsch_crypto_hash_t dfsch_crypto_md4 = {
  .type = {
    .type = DFSCH_CRYPTO_HASH_TYPE,
    .name = "md4",
    .size = sizeof(md4_context_t),
  },

  .name = "MD4",
  
  .block_len = 64,
  .result_len = 16,

  .setup = md4_setup,
  .process = md4_process,
  .result = md4_result
};
