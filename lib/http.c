#include <dfsch/lib/http.h>

#include <ctype.h>

static char *reasons[] = {
#define REASON_100 0
  /*  0 100 */ "Continue",
  /*  1 101 */ "Switching Protocols",
#define REASON_200 2
  /*  2 200 */ "OK",
  /*  3 201 */ "Created",
  /*  4 202 */ "Accepted",
  /*  5 203 */ "Non-Authoritative Information",
  /*  6 204 */ "No Content",
  /*  7 205 */ "Reset Content",
  /*  8 206 */ "Partial Content",
#define REASON_300 9
  /*  9 300 */ "Multiple Choices",
  /* 10 301 */ "Moved Permanently",
  /* 11 302 */ "Found",
  /* 12 303 */ "See Other",
  /* 13 304 */ "Not Modified",
  /* 14 305 */ "Use Proxy",
  /* 15 306 */ "", /* unused */
  /* 16 307 */ "Temporary Redirect",
#define REASON_400 17
  /* 17 400 */ "Bad Request",
  /* 18 401 */ "Unauthorized",
  /* 19 402 */ "Payment Required",
  /* 20 403 */ "Forbidden",
  /* 21 404 */ "Not Found",
  /* 22 405 */ "Method Not Allowed",
  /* 23 406 */ "Not Acceptable",
  /* 24 407 */ "Proxy Authentication Required",
  /* 25 408 */ "Request Timeout",
  /* 26 409 */ "Conflict",
  /* 27 410 */ "Gone",
  /* 28 411 */ "Length Required",
  /* 29 412 */ "Precondition Failed",
  /* 30 413 */ "Request Entity Too Large",
  /* 31 414 */ "Request-URI Too Large",
  /* 32 415 */ "Unsupported Media Type",
  /* 33 416 */ "Requested Range Not Satisfiable",
  /* 34 417 */ "Expectation Failed",
#define REASON_500 35
  /* 35 500 */ "Internal Server Error",
  /* 36 501 */ "Not Implemented",
  /* 37 502 */ "Bad Gateway",
  /* 38 503 */ "Service Unavailable",
  /* 39 504 */ "Gateway Timeout",
  /* 40 505 */ "HTTP Version Not Supported"
#define REASON_END 41
};

static int reason_offsets[] = {
  REASON_100,
  REASON_200,
  REASON_300,
  REASON_400,
  REASON_500,
  REASON_END
};

char* dfsch_http_header_name(char* name){
  char* res = dfsch_stracpy(name);
  char* i = res;
  int state = 0;

  while (*i){
    if (isalnum(*i)){
      if (state){
        *i = tolower(*i);
      }else{
        *i = toupper(*i);
      }
      state = 1;
    } else {
      state = 0;
    }
    i++;
  }

  return res;
}

char* dfsch_http_get_reason(int status){
  int class = status / 100 - 1;
  int code = status % 100;

  if (class < 0 || class >= (sizeof(reason_offsets)*sizeof(int) - 1) || 
      code >= reason_offsets[class+1] - reason_offsets[class]){
    return "Status Unknown, Situation Critical";
  }

  return reasons[reason_offsets[class] + code];
}

static int parse_protocol_fallback(char* protocol){
  char* slash;
  char* dot;
  char* proto;
  char* minor;
  char* major;
  int maj;
  int min;

  slash = strchr(protocol, '/');
  dot = strchr(slash, '.');

  proto = dfsch_strancpy(protocol, slash-protocol);
  major = dfsch_strancpy(slash+1, dot-slash-1);
  minor = dot;

  if (strcasecmp(proto, "HTTP") != 0){
    return DFSCH_HTTP_P_UNKNOWN;
  }

  maj = atoi(major);
  min = atoi(minor);

  if (maj == 1 && min == 0){
    return DFSCH_HTTP_P_HTTP10;    
  }
  if (maj == 1 && min == 1){
    return DFSCH_HTTP_P_HTTP11;    
  }

  return DFSCH_HTTP_P_UNKNOWN;
}

int dfsch_http_parse_protocol(char* protocol){
  if (!*protocol){
    return 1;
  }
  if (strcasecmp(protocol, "HTTP/1.1") == 0){
    return 3;
  }  
  if (strcasecmp(protocol, "HTTP/1.0") == 0){
    return 2;
  }

  return parse_protocol_fallback(protocol);
}

static char* protocols[] = {
  NULL,
  "",
  "HTTP/1.0",
  "HTTP/1.1"
};

char* dfsch_http_get_protocol(int protocol){
  if (protocol >= sizeof(protocols)/sizeof(char*))
    return NULL;
  return protocols[protocol];
}


struct dfsch_http_header_parser_t {
  dfsch_type_t* type;
  char* header_name;
  dfsch_http_header_parser_cb_t cb;
  void* baton;
};

dfsch_type_t dfsch_http_header_parser_type = {
  .type = DFSCH_STANDARD_TYPE,
  .size = sizeof(dfsch_http_header_parser_t),
  .name = "http:header-parser",
};


dfsch_http_header_parser_t* dfsch_http_make_header_parser(dfsch_http_header_parser_cb_t cb,
                                                          void* baton){
  dfsch_http_header_parser_t* hp = dfsch_make_object(DFSCH_HTTP_HEADER_PARSER_TYPE);

  hp->cb = cb;
  hp->baton = baton;

  return hp;
}
void dfsch_http_header_parser_parse_line(dfsch_http_header_parser_t* hp,
                                         char* line){
  char* value;

  if (*line == ' ' || *line == '\t') {
    if (hp->header_name) { /* Continuation */
      hp->cb(hp->baton, hp->header_name, line);
    } else { /* Continuation of nothing */
      dfsch_error("Continuation of empty header", 
                  dfsch_make_string_cstr(line));
    }
  } else {
    value = strchr(line, ':');
    if (value){ /* Header */      
      hp->cb(hp->baton, hp->header_name, value + 1);
    } else { /*  Random junk */
      dfsch_error("Junk in header stream", 
                  dfsch_make_string_cstr(line));
    }
  } 
}

typedef struct http_response_t {
  dfsch_type_t* type;
  int status;
  dfsch_object_t* headers;
  dfsch_strbuf_t* body;
} http_response_t;

typedef struct http_request_t {
  dfsch_type_t type;
  
  char *method;
  char *protocol;
  char *request_uri; 

  dfsch_strbuf_t* body;

  dfsch_object_t* headers;
} http_request_t;

dfsch_object_t* dfsch_make_http_response(int status,
                                         dfsch_object_t* headers,
                                         dfsch_strbuf_t* body){
  http_response_t* re = dfsch_make_object(DFSCH_HTTP_RESPONSE_TYPE);
}

void dfsch_http_run_server(){

}
