#include <dfsch/lib/crypto.h>

void dfsch_module_crypto_register(dfsch_object_t* env){
  dfsch_package_t* crypto = dfsch_make_package("crypto");
  dfsch_provide(env, "crypto");

  dfsch_defconst_pkgcstr(env, crypto, "<aes>",
                         DFSCH_CRYPTO_AES_CIPHER);
  dfsch_defconst_pkgcstr(env, crypto, "<symetric-cipher>",
                         DFSCH_SYMETRIC_CIPHER_TYPE);
}
