#ifndef H__dfsch_lib__json__
#define H__dfsch_lib__json__

#include <dfsch/dfsch.h>
#include <dfsch/parse.h>

extern dfsch_type_t dfsch_json_parser_type;
#define DFSCH_JSON_PARSER_TYPE (&dfsch_json_parser_type)

typedef struct dfsch_json_parser_t dfsch_json_parser_t;

dfsch_json_parser_t* dfsch_make_json_parser();
void dfsch_json_parser_set_callback(dfsch_json_parser_t *jp, 
				    dfsch_parser_callback_t callback,
				    void *baton);

void dfsch_json_parser_feed(dfsch_json_parser_t* jp,
                            char* buf);

dfsch_object_t* dfsch_json_parse_file(char* filename, int list);
dfsch_object_t* dfsch_json_parse_port(dfsch_object_t* port, int list);
dfsch_object_t* dfsch_json_parse_buf(char* buf, size_t len, int list);
dfsch_object_t* dfsch_json_parse_strbuf(dfsch_strbuf_t* b, int list);
dfsch_object_t* dfsch_json_parse_cstr(char* s, int list);

char* dfsch_json_emit_cstr(dfsch_object_t* obj);
void dfsch_json_emit_port(dfsch_object_t* obj, dfsch_object_t* port);
void dfsch_json_emit_file(dfsch_object_t* obj, char* filename);

#endif
