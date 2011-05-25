#ifndef SC25519_H
#define SC25519_H

#define sc25519 dfsch_crypto_sign_edwards25519sha512batch_sc25519
#define sc25519_from32bytes dfsch_crypto_sign_edwards25519sha512batch_sc25519_from32bytes
#define sc25519_from64bytes dfsch_crypto_sign_edwards25519sha512batch_sc25519_from64bytes
#define sc25519_to32bytes dfsch_crypto_sign_edwards25519sha512batch_sc25519_to32bytes
#define sc25519_pack dfsch_crypto_sign_edwards25519sha512batch_sc25519_pack
#define sc25519_getparity dfsch_crypto_sign_edwards25519sha512batch_sc25519_getparity
#define sc25519_setone dfsch_crypto_sign_edwards25519sha512batch_sc25519_setone
#define sc25519_setzero dfsch_crypto_sign_edwards25519sha512batch_sc25519_setzero
#define sc25519_neg dfsch_crypto_sign_edwards25519sha512batch_sc25519_neg
#define sc25519_add dfsch_crypto_sign_edwards25519sha512batch_sc25519_add
#define sc25519_sub dfsch_crypto_sign_edwards25519sha512batch_sc25519_sub
#define sc25519_mul dfsch_crypto_sign_edwards25519sha512batch_sc25519_mul
#define sc25519_square dfsch_crypto_sign_edwards25519sha512batch_sc25519_square
#define sc25519_invert dfsch_crypto_sign_edwards25519sha512batch_sc25519_invert

#include <stdint.h>

typedef struct {
  uint32_t v[32]; 
} sc25519;

void sc25519_from32bytes(sc25519 *r, const unsigned char x[32]);

void sc25519_from64bytes(sc25519 *r, const unsigned char x[64]);

void sc25519_to32bytes(unsigned char r[32], const sc25519 *x);

void sc25519_pack(unsigned char r[32], const sc25519 *x);

unsigned char sc25519_getparity(const sc25519 *x);

void sc25519_setone(sc25519 *r);

void sc25519_setzero(sc25519 *r);

void sc25519_neg(sc25519 *r, const sc25519 *x);

void sc25519_add(sc25519 *r, const sc25519 *x, const sc25519 *y);

void sc25519_sub(sc25519 *r, const sc25519 *x, const sc25519 *y);

void sc25519_mul(sc25519 *r, const sc25519 *x, const sc25519 *y);

void sc25519_square(sc25519 *r, const sc25519 *x);

void sc25519_invert(sc25519 *r, const sc25519 *x);

#endif
