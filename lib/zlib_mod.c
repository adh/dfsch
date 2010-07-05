#include <dfsch/dfsch.h>
#include "zlib.h"

DFSCH_DEFINE_PRIMITIVE(compress,
                       "Compress string using DEFLATE."){
  dfsch_strbuf_t* str;
  char* res;
  size_t rlen;

  DFSCH_BUFFER_ARG(args, str);
  DFSCH_ARG_END(args);

  rlen = compressBound(str->len);
  res = GC_MALLOC_ATOMIC(rlen);
  if (compress(res, &rlen, str->ptr, str->len) != Z_OK){
    dfsch_error("Internal error", NULL);
  }
  
  return dfsch_make_string_buf(res, rlen);
}

DFSCH_DEFINE_PRIMITIVE(uncompress,
                       "Uncompress result of zlib:compress."){
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
    zres = uncompress(res, &rlen, str->ptr, str->len);
    if (zres == Z_DATA_ERROR){
      dfsch_error("Internal error", NULL);
    }
  } while (zres != Z_OK);
  
  return dfsch_make_string_buf(res, rlen);
}

void dfsch_module_zlib_register(dfsch_object_t* env){
  dfsch_package_t* zlib = dfsch_make_package("zlib");
  dfsch_provide(env, "zlib");

  dfsch_defconst_pkgcstr(env, zlib, "compress", 
                         DFSCH_PRIMITIVE_REF(compress));
  dfsch_defconst_pkgcstr(env, zlib, "uncompress", 
                         DFSCH_PRIMITIVE_REF(uncompress));
}
