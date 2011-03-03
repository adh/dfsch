#include <dfsch/lib/crypto.h>
#include "internal.h"

#include "ge25519.h"

struct dfsch_sign25519_private_key_t {
  dfsch_type_t* type;
  uint8_t private[64];
};

struct dfsch_sign25519_public_key_t {
  dfsch_type_t* type;
  uint8_t public[32];
};

dfsch_type_t dfsch_sign25519_public_key_type = {};
dfsch_type_t dfsch_sign25519_private_key_type = {};


static void hexdump(char* label, unsigned char*data, size_t len){
  printf("%s: ", label);
  while(len){
    printf("%02hhx", *data);
    data++;
    len--;
  }
  putchar('\n');
}


dfsch_sign25519_private_key_t* 
dfsch_sign25519_generate_key(dfsch_object_t* random_source){
  sha512_context_t sha;
  dfsch_sign25519_private_key_t* 
    k = GC_NEW_ATOMIC(dfsch_sign25519_private_key_t);

  k->type = DFSCH_SIGN25519_PRIVATE_KEY_TYPE;

  dfsch_random_get_bytes(random_source, k->private, 64);

  dfsch_sha512_setup(&sha, NULL, 0);
  dfsch_sha512_process(&sha, k->private, 64);
  dfsch_sha512_result(&sha, k->private);


  k->private[0] &= 248;
  k->private[31] &= 127;
  k->private[31] |= 64;

  return k;
}

dfsch_sign25519_public_key_t* 
dfsch_sign25519_get_public_key(dfsch_sign25519_private_key_t* pk){
  sc25519 scsk;
  ge25519 gepk;
  dfsch_sign25519_public_key_t* 
    k = GC_NEW_ATOMIC(dfsch_sign25519_public_key_t);

  k->type = DFSCH_SIGN25519_PUBLIC_KEY_TYPE;

  sc25519_from32bytes(&scsk, pk->private);  
  ge25519_scalarmult_base(&gepk, &scsk);
  ge25519_pack(k->public, &gepk);

  return k;  
}


/* dfsch_crypto_sign25519(unsigned char *sm,unsigned long long *smlen,
                           const unsigned char *m,unsigned long long mlen,
                           const unsigned char *sk)**/
dfsch_strbuf_t* dfsch_sign25519_sign(dfsch_sign25519_private_key_t* key,
                                      char* m, size_t len){
  sc25519 sck, scs, scsk;
  ge25519 ger;
  unsigned char r[32];
  unsigned char s[32];
  unsigned long long i;
  unsigned char hmg[64];
  unsigned char hmr[64];
  sha512_context_t sha;
  dfsch_strbuf_t* res = dfsch_alloc_strbuf(64);
#define HD(n) hexdump(#n, &n, sizeof(n))

  dfsch_sha512_setup(&sha, NULL, 0);
  dfsch_sha512_process(&sha, key->private + 32, 32);
  dfsch_sha512_process(&sha, m, len);
  dfsch_sha512_result(&sha, hmg);

  sc25519_from64bytes(&sck, hmg);
  ge25519_scalarmult_base(&ger, &sck);
  ge25519_pack(r, &ger);
  
  memcpy(res->ptr, r, 32);

  dfsch_sha512_setup(&sha, NULL, 0);
  dfsch_sha512_process(&sha, r, 32);
  dfsch_sha512_process(&sha, m, len);
  dfsch_sha512_result(&sha, hmr);

  sc25519_from64bytes(&scs, hmr);
  sc25519_mul(&scs, &scs, &sck);
  
  sc25519_from32bytes(&scsk, key->private);
  sc25519_add(&scs, &scs, &scsk);

  sc25519_to32bytes(s,&scs); /* cat s */

  memcpy(res->ptr + 32, s, 32);

  return res;
}

int dfsch_sign25519_verify(dfsch_sign25519_public_key_t* key,
                            char* m, size_t len,
                            char* s, size_t slen){
  int i;
  unsigned char t1[32], t2[32];
  ge25519 get1, get2, gepk;
  sc25519 schmr, scs;
  unsigned char hmr[64];
  sha512_context_t sha;

  if (slen != 64){
    dfsch_error("Invalid length of SIGN25519 signature", NULL);
  }

  if (ge25519_unpack_vartime(&get1, s)){
    dfsch_error("Error parsing signature", NULL);
  }
  if (ge25519_unpack_vartime(&gepk, key->public)){
    dfsch_error("Error in public key", NULL);
  };


  dfsch_sha512_setup(&sha, NULL, 0);
  dfsch_sha512_process(&sha, s, 32);
  dfsch_sha512_process(&sha, m, len);
  dfsch_sha512_result(&sha, hmr);

  sc25519_from64bytes(&schmr, hmr);
  ge25519_scalarmult(&get1, &get1, &schmr);
  ge25519_add(&get1, &get1, &gepk);
  ge25519_pack(t1, &get1);

  sc25519_from32bytes(&scs, s + 32);
  ge25519_scalarmult_base(&get2, &scs);
  ge25519_pack(t2, &get2);

  return memcmp(t1, t2, 32) == 0; /* No timing attack here */
}
