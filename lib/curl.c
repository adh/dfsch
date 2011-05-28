#include <dfsch/lib/curl.h>
#include <dfsch/util.h>

void dfsch_curl_setopt(CURL* handle,
                       dfsch_object_t* name,
                       dfsch_object_t* value){
  CURLcode ret;
  if (dfsch_compare_keyword("url", name)){
    ret = curl_easy_setopt(handle, CURLOPT_URL, 
                           dfsch_string_to_cstr(value));
  } else {
    dfsch_error("Unknown keyword", name);
  }
}

static size_t write_fun(void* ptr, size_t size, size_t nmemb, 
                        dfsch_str_list_t* sl){
  dfsch_sl_nappend(sl, ptr, size * nmemb);
  return size * nmemb;
}

dfsch_strbuf_t* dfsch_curl_perform(CURL* handle){
  dfsch_str_list_t* sl = dfsch_sl_create();
  CURLcode ret;
  char err[CURL_ERROR_SIZE];

  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, write_fun);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, sl);
  curl_easy_setopt(handle, CURLOPT_ERRORBUFFER, err);

  ret = curl_easy_perform(handle);
  if (ret != 0){
    dfsch_error("CURL returned error",
                dfsch_make_string_cstr(err));
  }
  

  return dfsch_sl_value_strbuf(sl);
}
