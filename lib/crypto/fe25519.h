#ifndef FE25519_H
#define FE25519_H

#define fe25519 dfsch_crypto_sign_edwards25519sha512batch_fe25519
#define fe25519_unpack dfsch_crypto_sign_edwards25519sha512batch_fe25519_unpack
#define fe25519_pack dfsch_crypto_sign_edwards25519sha512batch_fe25519_pack
#define fe25519_cmov dfsch_crypto_sign_edwards25519sha512batch_fe25519_cmov
#define fe25519_setone dfsch_crypto_sign_edwards25519sha512batch_fe25519_setone
#define fe25519_setzero dfsch_crypto_sign_edwards25519sha512batch_fe25519_setzero
#define fe25519_neg dfsch_crypto_sign_edwards25519sha512batch_fe25519_neg
#define fe25519_getparity dfsch_crypto_sign_edwards25519sha512batch_fe25519_getparity
#define fe25519_add dfsch_crypto_sign_edwards25519sha512batch_fe25519_add
#define fe25519_sub dfsch_crypto_sign_edwards25519sha512batch_fe25519_sub
#define fe25519_mul dfsch_crypto_sign_edwards25519sha512batch_fe25519_mul
#define fe25519_square dfsch_crypto_sign_edwards25519sha512batch_fe25519_square
#define fe25519_pow dfsch_crypto_sign_edwards25519sha512batch_fe25519_pow
#define fe25519_sqrt_vartime dfsch_crypto_sign_edwards25519sha512batch_fe25519_sqrt_vartime
#define fe25519_invert dfsch_crypto_sign_edwards25519sha512batch_fe25519_invert

#include <stdint.h>

typedef struct {
  uint32_t v[32]; 
} fe25519;

void fe25519_unpack(fe25519 *r, const unsigned char x[32]);

void fe25519_pack(unsigned char r[32], const fe25519 *x);

void fe25519_cmov(fe25519 *r, const fe25519 *x, unsigned char b);

void fe25519_setone(fe25519 *r);

void fe25519_setzero(fe25519 *r);

void fe25519_neg(fe25519 *r, const fe25519 *x);

unsigned char fe25519_getparity(const fe25519 *x);

void fe25519_add(fe25519 *r, const fe25519 *x, const fe25519 *y);

void fe25519_sub(fe25519 *r, const fe25519 *x, const fe25519 *y);

void fe25519_mul(fe25519 *r, const fe25519 *x, const fe25519 *y);

void fe25519_square(fe25519 *r, const fe25519 *x);

void fe25519_pow(fe25519 *r, const fe25519 *x, const unsigned char *e);

int fe25519_sqrt_vartime(fe25519 *r, const fe25519 *x, unsigned char parity);

void fe25519_invert(fe25519 *r, const fe25519 *x);

#endif
