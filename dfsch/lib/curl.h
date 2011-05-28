#ifndef H__dfsch_lib__curl__
#define H__dfsch_lib__curl__

#include <curl/curl.h>
#include <dfsch/dfsch.h>

void dfsch_curl_setopt(CURL* handle,
                       dfsch_object_t* name,
                       dfsch_object_t* value);

dfsch_strbuf_t* dfsch_curl_perform(CURL* handle);

#endif
