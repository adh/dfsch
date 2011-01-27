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

DFSCH_DEFINE_PRIMITIVE(gzip_open_for_input,
                       "Open gzip compressed file for reading"){
  char* filename;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);

  return dfsch_gzip_open_for_input(filename);
}
DFSCH_DEFINE_PRIMITIVE(gzip_open_for_output,
                       "Open gzip compressed file for writing"){
  char* filename;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);

  return dfsch_gzip_open_for_output(filename);
}
DFSCH_DEFINE_PRIMITIVE(gzip_open_for_append,
                       "Open gzip compressed file for appending"){
  char* filename;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);

  return dfsch_gzip_open_for_append(filename);
}

void dfsch_module_zlib_register(dfsch_object_t* env){
  dfsch_package_t* zlib = dfsch_make_package("zlib",
                                             "Zlib and gzip support");
  dfsch_provide(env, "zlib");

  dfsch_defcanon_pkgcstr(env, zlib, "compress", 
                         DFSCH_PRIMITIVE_REF(compress));
  dfsch_defcanon_pkgcstr(env, zlib, "uncompress", 
                         DFSCH_PRIMITIVE_REF(uncompress));

  dfsch_defcanon_pkgcstr(env, zlib, "gzip-open-for-input", 
                         DFSCH_PRIMITIVE_REF(gzip_open_for_input));
  dfsch_defcanon_pkgcstr(env, zlib, "gzip-open-for-output", 
                         DFSCH_PRIMITIVE_REF(gzip_open_for_output));
  dfsch_defcanon_pkgcstr(env, zlib, "gzip-open-for-append", 
                         DFSCH_PRIMITIVE_REF(gzip_open_for_append));
}
