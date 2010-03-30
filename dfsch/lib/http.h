#ifndef H__dfsch__lib__http__
#define H__dfsch__lib__http__

#include <dfsch/dfsch.h>

#define DFSCH_HTTP_P_UNKNOWN 0
#define DFSCH_HTTP_P_HTTP09  1
#define DFSCH_HTTP_P_HTTP10  2
#define DFSCH_HTTP_P_HTTP11  3

#define DFSCH_HTTP_S_CONTINUE              100
#define DFSCH_HTTP_S_SWITCHING_PROTOCOLS   101

#define DFSCH_HTTP_S_OK                    200
#define DFSCH_HTTP_S_CREATED               201
#define DFSCH_HTTP_S_ACCEPTED              202
#define DFSCH_HTTP_S_NON_AUTHORITATIVE     203
#define DFSCH_HTTP_S_NO_CONTENT            204
#define DFSCH_HTTP_S_PARTIAL_CONTENT       206

#define DFSCH_HTTP_S_MULTIPLE_CHOICES      300
#define DFSCH_HTTP_S_MOVED_PERMANENTLY     301
#define DFSCH_HTTP_S_FOUND                 302
#define DFSCH_HTTP_S_SEE_OTHER             303
#define DFSCH_HTTP_S_NOT_MODIFIED          304
#define DFSCH_HTTP_S_USE_PROXY             305
#define DFSCH_HTTP_S_TEMPORARY_REDIRECT    307

#define DFSCH_HTTP_S_BAD_REQUEST           400
#define DFSCH_HTTP_S_UNAUTHORIZED          401
#define DFSCH_HTTP_S_PAYMENT_REQUIRED      402
#define DFSCH_HTTP_S_FORBIDDEN             403
#define DFSCH_HTTP_S_NOT_FOUND             404
#define DFSCH_HTTP_S_METHOD_NOT_ALLOWED    405
#define DFSCH_HTTP_S_NOT_ACCEPTABLE        406
#define DFSCH_HTTP_S_PROXY_AUTH            407
#define DFSCH_HTTP_S_REQUEST_TIMEOUT       408
#define DFSCH_HTTP_S_CONFLICT              409
#define DFSCH_HTTP_S_GONE                  410
#define DFSCH_HTTP_S_LENGTH_REQUIRED       411
#define DFSCH_HTTP_S_PRECONDITION_FAILED   412
#define DFSCH_HTTP_S_ENTITY_TOO_LARGE      413
#define DFSCH_HTTP_S_URI_TOO_LARGE         414
#define DFSCH_HTTP_S_UNSUPPORTED_TYPE      415
#define DFSCH_HTTP_S_RANGE_NOT_SATISFIABLE 416
#define DFSCH_HTTP_S_EXPECTATION_FAILED    417

#define DFSCH_HTTP_S_INTERNAL_SERVER_ERROR 500
#define DFSCH_HTTP_S_NOT_IMPLEMENTED       501
#define DFSCH_HTTP_S_BAD_GATEWAY           502
#define DFSCH_HTTP_S_SERVICE_UNAVAILABLE   503
#define DFSCH_HTTP_S_GATEWAY_TIMEOUT       504
#define DFSCH_HTTP_S_VERSION_NOT_SUPPORTED 505

char* dfsch_http_header_name(char* name);
char* dfsch_http_get_reason_string(int status);

int dfsch_http_parse_method(char* method);
char* dfsch_http_get_method(int method);

int dfsch_http_parse_protocol(char* protocol);
char* dfsch_http_get_protocol(int protocol);

typedef struct dfsch_http_header_parser_t dfsch_http_header_parser_t;

extern dfsch_type_t dfsch_http_header_parser_type;
#define DFSCH_HTTP_HEADER_PARSER_TYPE (&dfsch_http_header_parser_type)

typedef (*dfsch_http_header_parser_cb_t)(void* baton, char* name, char* value);

dfsch_http_header_parser_t* dfsch_http_make_header_parser(dfsch_http_header_parser_cb_t cb,
                                                          void* baton);
void dfsch_http_header_parser_parse_line(dfsch_http_header_parser_t* hp,
                                         char* line);

extern dfsch_type_t dfsch_http_response_type;
#define DFSCH_HTTP_RESPONSE_TYPE (&dfsch_http_response_type)
extern dfsch_type_t dfsch_http_request_type;
#define DFSCH_HTTP_RESPONSE_TYPE (&dfsch_http_request_type)

#endif
