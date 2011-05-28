#include <dfsch/lib/curl.h>
#include <dfsch/load.h>
#include <dfsch/magic.h>

DFSCH_DEFINE_PRIMITIVE(fetch, "Fetch contents of URL"){
  char* url;
  CURL* handle = curl_easy_init();
  dfsch_strbuf_t* ret;
  DFSCH_UNWIND {
    DFSCH_STRING_ARG(args, url);
    
    curl_easy_setopt(handle, CURLOPT_URL, url);

    while (dfsch_pair_p(args)){
      dfsch_object_t* name;
      dfsch_object_t* value;
      DFSCH_OBJECT_ARG(args, name);
      DFSCH_OBJECT_ARG(args, value);
      dfsch_curl_setopt(handle, name, value);
    }
    
    ret = dfsch_curl_perform(handle);
  } DFSCH_PROTECT {
    curl_easy_cleanup(handle);
  } DFSCH_PROTECT_END;
  
  return dfsch_make_byte_vector_nocopy(ret->ptr, ret->len);
}

void dfsch_module_curl_register(dfsch_object_t* env){
  dfsch_package_t* curl = dfsch_make_package("curl",
                                             "CURL URL handling library");
  
  dfsch_provide(env, "curl");

  dfsch_defcanon_pkgcstr(env, curl, "fetch", DFSCH_PRIMITIVE_REF(fetch));
}
