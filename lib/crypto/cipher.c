#include <dfsch/lib/crypto.h>

dfsch_type_t dfsch_symetric_cipher_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name = "crypto:symetric-cipher",
  .size = sizeof(dfsch_symetric_cipher_t)
};
