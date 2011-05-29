#include <dfsch/lib/curl.h>
#include <dfsch/util.h>

struct dfsch_curl_options_context_t {
  dfsch_strbuf_t* uploaded_data;
  struct curl_slist* headers;
};

dfsch_curl_options_context_t* dfsch_curl_make_options_context(){
  return GC_NEW(dfsch_curl_options_context_t);
}
void dfsch_curl_cleanup(CURL* handle,
                        dfsch_curl_options_context_t* ctx){
  if (ctx){
    if (ctx->headers){
      curl_slist_free_all(ctx->headers);
    }
  }
  curl_easy_cleanup(handle);
}

void dfsch_curl_set_http_headers(CURL* handle,
                                 dfsch_object_t* headers,
                                 dfsch_curl_options_context_t* ctx){
  struct curl_slist* list = NULL;
  dfsch_object_t* it = dfsch_collection_get_iterator(headers);

  while (it){
    dfsch_object_t* elem = dfsch_collection_this(it);
    list = curl_slist_append(list, dfsch_string_to_cstr(elem));
    it = dfsch_iterator_next(it);
  }
  
  if (ctx){
    ctx->headers = list;
  }
  curl_easy_xetopt(handle, CURLOPT_HTTPHEADER, list);
}

static size_t read_function(void* buf, size_t nmemb, size_t size, 
                            dfsch_strbuf_t* sb){
  size_t ret = dfsch_strbuf_inputproc(sb, buf, nmemb*size);
  return ret;
}

void dfsch_curl_set_upload(CURL* handle,
                           char* data, size_t len,
                           dfsch_curl_options_context_t* ctx){
  dfsch_strbuf_t* upload = dfsch_strbuf_create(data, len);

  curl_easy_setopt(handle, CURLOPT_UPLOAD, 1);
  curl_easy_setopt(handle, CURLOPT_READFUNCTION, read_function);
  if (ctx){
    ctx->uploaded_data = upload;
  }
  curl_easy_setopt(handle, CURLOPT_READDATA, upload);
  curl_easy_setopt(handle, CURLOPT_INFILESIZE_LARGE, (curl_off_t)len);
}

void dfsch_curl_set_post(CURL* handle,
                         char* data, size_t len,
                         dfsch_curl_options_context_t* ctx){
  curl_easy_setopt(handle, CURLOPT_POST, 1);
  curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE_LARGE, (curl_off_t)len);
  curl_easy_setopt(handle, CURLOPT_COPYPOSTFIELDS, data);
}

void dfsch_curl_setopt(CURL* handle,
                       dfsch_object_t* name,
                       dfsch_object_t* value,
                       dfsch_curl_options_context_t* ctx){

  if (dfsch_compare_keyword(name, "url")){
    curl_easy_setopt(handle, CURLOPT_URL, 
                     dfsch_string_to_cstr(value));

  } else if (dfsch_compare_keyword(name, "upload")){
    dfsch_strbuf_t* buf = dfsch_string_to_buf(value);
    dfsch_curl_set_upload(handle, buf->ptr, buf->len, ctx);

  } else if (dfsch_compare_keyword(name, "post")){
    dfsch_strbuf_t* buf = dfsch_string_to_buf(value);
    dfsch_curl_set_post(handle, buf->ptr, buf->len, ctx);

  } else if (dfsch_compare_keyword(name, "follow-redirects")){
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION,
                     value != NULL);

  } else if (dfsch_compare_keyword(name, "output-headers")){
    curl_easy_setopt(handle, CURLOPT_HEADER,
                     value != NULL);

  } else if (dfsch_compare_keyword(name, "headers")){
    dfsch_curl_set_http_headers(handle, value, ctx);

  } else if (dfsch_compare_keyword(name, "username")){
    curl_easy_setopt(handle, CURLOPT_USERNAME, 
                     dfsch_string_to_cstr(value));

  } else if (dfsch_compare_keyword(name, "password")){
    curl_easy_setopt(handle, CURLOPT_PASSWORD, 
                     dfsch_string_to_cstr(value));

  } else if (dfsch_compare_keyword(name, "user-agent")){
    curl_easy_setopt(handle, CURLOPT_USERAGENT, 
                     dfsch_string_to_cstr(value));

  } else if (dfsch_compare_keyword(name, "referer")){
    curl_easy_setopt(handle, CURLOPT_REFERER, 
                     dfsch_string_to_cstr(value));

  } else if (dfsch_compare_keyword(name, "cookies")){
    curl_easy_setopt(handle, CURLOPT_COOKIE, 
                     dfsch_string_to_cstr(value));

  } else if (dfsch_compare_keyword(name, "cookies")){
    curl_easy_setopt(handle, CURLOPT_COOKIE, 
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
