#include "dfsch/lib/crypto.h"

#include <dfsch/random.h>
#include <dfsch/bignum.h>
#include <dfsch/number.h>

struct dfsch_rsa_public_key_t {
  dfsch_type_t* type;

  dfsch_object_t* modulus;
  dfsch_object_t* public_exponent;
};

static dfsch_slot_t public_slots[] = {
  DFSCH_OBJECT_SLOT(dfsch_rsa_public_key_t, modulus, DFSCH_SLOT_ACCESS_RO,
                    "RSA modulus"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_public_key_t, public_exponent, 
                    DFSCH_SLOT_ACCESS_RO,
                    "RSA public exponent"),
  DFSCH_SLOT_TERMINATOR
};

dfsch_type_t dfsch_rsa_public_key_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "rsa-public-key",
  .size = sizeof(dfsch_rsa_public_key_t),
  .slots = &public_slots
};

struct dfsch_rsa_private_key_t {
  dfsch_type_t* type;

  dfsch_object_t* modulus;
  dfsch_object_t* public_exponent;
  dfsch_object_t* private_exponent;
  dfsch_object_t* prime1;
  dfsch_object_t* prime2;
  dfsch_object_t* exponent1;
  dfsch_object_t* exponent2;
  dfsch_object_t* coefficient;
};

static dfsch_slot_t private_slots[] = {
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, modulus, DFSCH_SLOT_ACCESS_RO,
                    "RSA modulus (n)"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, public_exponent, 
                    DFSCH_SLOT_ACCESS_RO,
                    "RSA public exponent (e)"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, private_exponent, 
                    DFSCH_SLOT_ACCESS_RO,
                    "RSA private exponent (d)"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, prime1, 
                    DFSCH_SLOT_ACCESS_RO,
                    "First prime (p)"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, prime2, 
                    DFSCH_SLOT_ACCESS_RO,
                    "Second prime (q)"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, exponent1, 
                    DFSCH_SLOT_ACCESS_RO,
                    "First exponent for CRT (dP = d mod (p-1))"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, exponent2, 
                    DFSCH_SLOT_ACCESS_RO,
                    "Second exponent for CRT (dQ = d mod (q-1))"),
  DFSCH_OBJECT_SLOT(dfsch_rsa_private_key_t, coefficient, 
                    DFSCH_SLOT_ACCESS_RO,
                    "Coefficient for CRT (qInv = q mod p)"),
  DFSCH_SLOT_TERMINATOR
};


dfsch_type_t dfsch_rsa_private_key_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "rsa-private-key",
  .size = sizeof(dfsch_rsa_private_key_t),
  .slots = &private_slots
};

static dfsch_object_t* get_random_prime(dfsch_object_t* random_source,
                                        int length){
  dfsch_bignum_t* bn = dfsch_random_get_bignum(random_source, length);
  return dfsch_number_next_prime(bn);
}

dfsch_rsa_private_key_t* dfsch_rsa_generate_key(dfsch_object_t* random_source,
                                                int length){
  dfsch_rsa_private_key_t* prk = dfsch_make_object(DFSCH_RSA_PRIVATE_KEY_TYPE);
  dfsch_object_t* phi;
  dfsch_object_t* p0;
  dfsch_object_t* q0;

  prk->public_exponent = DFSCH_MAKE_FIXNUM(65537);

  do {

    prk->prime1 = get_random_prime(random_source, length/2);
    prk->prime2 = get_random_prime(random_source, length/2);

    if (dfsch_number_cmp(prk->prime1, prk->prime2) < 0){
      dfsch_object_t* tmp = prk->prime1;
      prk->prime1 = prk->prime2;
      prk->prime2 = tmp;
    }

    p0 = dfsch_number_sub(prk->prime1, DFSCH_MAKE_FIXNUM(1));
    q0 = dfsch_number_sub(prk->prime2, DFSCH_MAKE_FIXNUM(1));
    phi = dfsch_number_mul(p0, q0);
    
  } while (dfsch_number_gcd(prk->public_exponent, 
                            phi) != DFSCH_MAKE_FIXNUM(1));
  prk->modulus = dfsch_number_mul(prk->prime1, prk->prime2);
  prk->private_exponent = dfsch_number_mod_inv(prk->public_exponent, 
                                               phi);
  prk->exponent1 = dfsch_number_mod(prk->private_exponent, p0);
  prk->exponent2 = dfsch_number_mod(prk->private_exponent, q0);
  prk->coefficient = dfsch_number_mod_inv(prk->prime2,
                                          prk->prime1);
  return prk;
}
dfsch_rsa_public_key_t* dfsch_rsa_get_public_key(dfsch_rsa_private_key_t* prk){
  dfsch_rsa_public_key_t* puk = dfsch_make_object(DFSCH_RSA_PUBLIC_KEY_TYPE);
  
  puk->public_exponent = prk->public_exponent;
  puk->modulus = prk->modulus;

  return puk;
}

dfsch_object_t* dfsch_rsa_public_key_2_list(dfsch_rsa_public_key_t* puk){
  return dfsch_list(2,
                    puk->modulus,
                    puk->public_exponent);
}
dfsch_object_t* dfsch_rsa_private_key_2_list(dfsch_rsa_private_key_t* prk){
  return dfsch_list(8,
                    prk->modulus,
                    prk->public_exponent,
                    prk->private_exponent,
                    prk->prime1,        
                    prk->prime2,        
                    prk->exponent1,     
                    prk->exponent2,
                    prk->coefficient);
}
dfsch_rsa_public_key_t* dfsch_rsa_public_key_from_list(dfsch_object_t* list){
  dfsch_rsa_public_key_t* puk = dfsch_make_object(DFSCH_RSA_PUBLIC_KEY_TYPE);
  DFSCH_OBJECT_ARG(list, puk->modulus);
  DFSCH_OBJECT_ARG(list, puk->public_exponent);
  DFSCH_ARG_END(list);
  return puk;
}
dfsch_rsa_private_key_t* dfsch_rsa_private_key_from_list(dfsch_object_t* list){
  dfsch_rsa_private_key_t* prk = dfsch_make_object(DFSCH_RSA_PRIVATE_KEY_TYPE);
  DFSCH_OBJECT_ARG(list, prk->modulus);
  DFSCH_OBJECT_ARG(list, prk->public_exponent);
  DFSCH_OBJECT_ARG(list, prk->private_exponent);
  DFSCH_OBJECT_ARG(list, prk->prime1);
  DFSCH_OBJECT_ARG(list, prk->prime2);
  DFSCH_OBJECT_ARG(list, prk->exponent1);
  DFSCH_OBJECT_ARG(list, prk->exponent2);
  DFSCH_OBJECT_ARG(list, prk->coefficient);
  DFSCH_ARG_END(list);
  return prk;
}


dfsch_object_t* dfsch_rsa_encrypt(dfsch_rsa_public_key_t* puk,
                                  dfsch_object_t* mn){
  dfsch_bignum_t* n = dfsch_bignum_from_number(puk->modulus);
  dfsch_bignum_t* e = dfsch_bignum_from_number(puk->public_exponent);
  dfsch_bignum_t* m = dfsch_bignum_from_number(mn);
  dfsch_bignum_t* c = dfsch_bignum_exp(m, e, n);
  return dfsch_bignum_to_number(c);
}
dfsch_object_t* dfsch_rsa_decrypt(dfsch_rsa_private_key_t* prk,
                                  dfsch_object_t* cn){
  dfsch_bignum_t* c = dfsch_bignum_from_number(cn);
  dfsch_bignum_t* n = dfsch_bignum_from_number(prk->modulus);
  dfsch_bignum_t* p = dfsch_bignum_from_number(prk->prime1);
  dfsch_bignum_t* q = dfsch_bignum_from_number(prk->prime2);
  dfsch_bignum_t* dP = dfsch_bignum_from_number(prk->exponent1);
  dfsch_bignum_t* dQ = dfsch_bignum_from_number(prk->exponent2);
  dfsch_bignum_t* qInv = dfsch_bignum_from_number(prk->coefficient);

  dfsch_bignum_t* m1 = dfsch_bignum_exp(c, dP, p);
  dfsch_bignum_t* m2 = dfsch_bignum_exp(c, dQ, q);
  
  dfsch_bignum_t* h;
  dfsch_bignum_div(dfsch_bignum_mul(qInv,
                                    dfsch_bignum_sub(m1, m2)),
                   p,
                   NULL,
                   &h);

  return dfsch_number_add(dfsch_bignum_to_number(m2),
                          dfsch_number_mul(prk->prime2,
                                           dfsch_bignum_to_number(h)));
}
