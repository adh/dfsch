#ifndef H__dfsch_lib__xml__
#define H__dfsch_lib__xml__

#include <dfsch/dfsch.h>
#include <expat.h>

dfsch_object_t* dfsch_xml_make_parser(char* encoding, int ns);

void dfsch_xml_destroy_parser(dfsch_object_t* parser);

XML_Parser dfsch_xml_parser_ref(dfsch_object_t* parser);

void dfsch_xml_set_start_element_proc(dfsch_object_t* parser,
                                          dfsch_object_t* proc);
void dfsch_xml_set_end_element_proc(dfsch_object_t* parser,
                                        dfsch_object_t* proc);
void dfsch_xml_set_character_data_proc(dfsch_object_t* parser,
                                           dfsch_object_t* proc);
void dfsch_xml_set_processing_instruction_proc(dfsch_object_t* parser,
                                                   dfsch_object_t* proc);
void dfsch_xml_set_comment_proc(dfsch_object_t* parser,
                                    dfsch_object_t* proc);

void dfsch_xml_parse_buf(dfsch_object_t* parser,
                             char* buf,
                             size_t len,
                             int eof);
void dfsch_xml_parse_strbuf(dfsch_object_t* parser,
                                dfsch_strbuf_t* buf,
                                int eof);
void dfsch_xml_parse_cstr(dfsch_object_t* parser,
                              char* str,
                              int eof);
void dfsch_xml_parse_eof(dfsch_object_t* parser);

void dfsch_xml_parse_file(dfsch_object_t* parsr,
                              char* filename,
                              int eof);
void dfsch_xml_parse_port(dfsch_object_t* parser,
                              dfsch_object_t* port,
                              int eof);


extern dfsch_type_t dfsch_xml_parser_type;
#define DFSCH_XML_PARSER_TYPE (&dfsch_xml_parser_type)

void dfsch_xml_signal_error(XML_Parser parser);


#endif
