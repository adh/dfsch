#ifndef H__dfsch_lib__curl__
#define H__dfsch_lib__curl__

#include <curl/curl.h>
#include <dfsch/dfsch.h>

typedef struct dfsch_curl_options_context_t dfsch_curl_options_context_t;

dfsch_curl_options_context_t* dfsch_curl_make_options_context();
void dfsch_curl_cleanup(CURL* handle,
                        dfsch_curl_options_context_t* ctx);

void dfsch_curl_setopt(CURL* handle,
                       dfsch_object_t* name,
                       dfsch_object_t* value,
                       dfsch_curl_options_context_t* ctx);

dfsch_strbuf_t* dfsch_curl_perform(CURL* handle);

#endif
