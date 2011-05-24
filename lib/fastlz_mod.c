#include <dfsch/dfsch.h>
#include "ext/fastlz/fastlz.h"

DFSCH_DEFINE_PRIMITIVE(compress,
                       "Compress string using FastLZ."){
  dfsch_strbuf_t* str;
  char* res;
  size_t rlen;

  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);

  rlen = str->len + (str->len / 19);
  res = GC_MALLOC_ATOMIC(rlen);
  rlen = dfsch__fastlz_compress(str->ptr, str->len, res);
  if (rlen == 0){
    dfsch_error("Internal error", NULL);
  }
  
  return dfsch_make_string_buf(res, rlen);
}

DFSCH_DEFINE_PRIMITIVE(uncompress,
                       "Uncompress result of fastlz:compress."){
  dfsch_strbuf_t* str;
  char* res;
  size_t rlen;
  int zres;

  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);

  rlen = str->len;

  do {
    rlen *= 4;
    res = GC_MALLOC_ATOMIC(rlen);
    zres = dfsch__fastlz_decompress(str->ptr, str->len, res, rlen);
  } while (zres == 0);
  
  return dfsch_make_string_buf(res, zres);
}

void dfsch_module_fastlz_register(dfsch_object_t* env){
  dfsch_package_t* flz = 
    dfsch_make_package("fastlz",
                       "FastLZ compression and decompression");
  dfsch_provide(env, "fastlz");

  dfsch_defcanon_pkgcstr(env, flz, "compress", 
                         DFSCH_PRIMITIVE_REF(compress));
  dfsch_defcanon_pkgcstr(env, flz, "uncompress", 
                         DFSCH_PRIMITIVE_REF(uncompress));
}
