#include <dfsch/lib/http.h>

DFSCH_DEFINE_PRIMITIVE(make_request, "Create HTTP request object"){
  char* method = "GET";
  char* request_uri = "/";
  char* protocol = "HTTP/1.1";
  dfsch_object_t* headers = NULL;
  dfsch_strbuf_t* body = NULL;

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("method", method, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("request-uri", request_uri, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("protocol", protocol, dfsch_string_to_cstr);
  DFSCH_KEYWORD("headers", headers);
  DFSCH_KEYWORD_GENERIC("body", body, dfsch_string_to_buf);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_http_request(method, request_uri, protocol, headers, body);
}

DFSCH_DEFINE_PRIMITIVE(read_request, "Read HTTP request from port"){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);

  return dfsch_http_read_request(port);
}

DFSCH_DEFINE_PRIMITIVE(write_request, "Read HTTP request from port"){
  dfsch_object_t* port;
  dfsch_http_request_t* request;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_HTTP_REQUEST_ARG(args, request);
  DFSCH_ARG_END(args);

  dfsch_http_write_request(port, request);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(make_response, "Create HTTP response object"){
  int status;
  char* protocol = "HTTP/1.1";
  dfsch_object_t* headers = NULL;
  dfsch_strbuf_t* body = NULL;

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("status", status, dfsch_number_to_long);
  DFSCH_KEYWORD_GENERIC("protocol", protocol, dfsch_string_to_cstr);
  DFSCH_KEYWORD("headers", headers);
  DFSCH_KEYWORD_GENERIC("body", body, dfsch_string_to_buf);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_http_response(status, protocol, headers, body);
}

DFSCH_DEFINE_PRIMITIVE(read_response, "Read HTTP response from port"){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);

  return dfsch_http_read_response(port);
}

DFSCH_DEFINE_PRIMITIVE(write_response, "Read HTTP response from port"){
  dfsch_object_t* port;
  dfsch_http_response_t* response;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_HTTP_RESPONSE_ARG(args, response);
  DFSCH_ARG_END(args);

  dfsch_http_write_response(port, response);
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(run_http_server, "Run server loop (for one connection)"){
  dfsch_object_t* port;
  dfsch_object_t* handler;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_OBJECT_ARG(args, handler);
  DFSCH_ARG_END(args);
  
  dfsch_http_run_server(port, handler);

  return NULL;
}


void dfsch_module_http_register(dfsch_object_t* env){
  dfsch_package_t* http = dfsch_make_package("http",
                                             "Low-level HTTP handling");
  dfsch_provide(env, "http");

  dfsch_defcanon_pkgcstr(env, http, "make-request",
                         DFSCH_PRIMITIVE_REF(make_request));
  dfsch_defcanon_pkgcstr(env, http, "read-request",
                         DFSCH_PRIMITIVE_REF(read_request));
  dfsch_defcanon_pkgcstr(env, http, "write-request",
                         DFSCH_PRIMITIVE_REF(write_request));


  dfsch_defcanon_pkgcstr(env, http, "make-response",
                         DFSCH_PRIMITIVE_REF(make_response));
  dfsch_defcanon_pkgcstr(env, http, "read-response",
                         DFSCH_PRIMITIVE_REF(read_response));
  dfsch_defcanon_pkgcstr(env, http, "write-response",
                         DFSCH_PRIMITIVE_REF(write_response));

  dfsch_defcanon_pkgcstr(env, http, "run-http-server",
                         DFSCH_PRIMITIVE_REF(run_http_server));

  
  dfsch_define_slot_accessor(env, http, "request-uri",
                             DFSCH_HTTP_REQUEST_TYPE, "request_uri");
  dfsch_define_slot_accessor(env, http, "request-method",
                             DFSCH_HTTP_REQUEST_TYPE, "method");
  dfsch_define_slot_accessor(env, http, "request-protocol",
                             DFSCH_HTTP_REQUEST_TYPE, "protocol");
  dfsch_define_slot_accessor(env, http, "request-headers",
                             DFSCH_HTTP_REQUEST_TYPE, "headers");
  dfsch_define_slot_accessor(env, http, "request-body",
                             DFSCH_HTTP_REQUEST_TYPE, "body");

  dfsch_define_slot_accessor(env, http, "response-protocol",
                             DFSCH_HTTP_RESPONSE_TYPE, "body");


}
