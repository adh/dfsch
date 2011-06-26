#include <dfsch/lib/ini-file.h>

dfsch_type_t dfsch_ini_file_type = {

};

dfsch_object_t* dfsch_make_empty_ini_file();
dfsch_object_t* dfsch_ini_file_read_file(char* fname);
dfsch_object_t* dfsch_ini_file_read_port(dfsch_object_t* port);

dfsch_object_t* dfsch_ini_file_set_defaults(dfsch_object_t* ifo,
                                            dfsch_object_t* defaults_ifo);

void dfsch_ini_file_write_file(dfsch_object_t* ifo,
                               char* fname);
void dfsch_ini_file_write_port(dfsch_object_t* ifo,
                               dfsch_object_t* port);

int dfsch_ini_file_has_section_p(dfsch_object_t* ifo,
                                 char* section);
int dfsch_ini_file_has_property_p(dfsch_object_t* ifo,
                                  char* section,
                                  char* property);

void dfsch_ini_file_add_section(dfsch_object_t* ifo,
                                char* section);
void dfsch_ini_file_add_comment(dfsch_object_t* ifo,
                                char* comment);

char* dfsch_ini_file_get(dfsch_object_t* ifo,
                         char* section,
                         char* property);
void dfsch_ini_file_set(dfsch_object_t* ifo,
                        char* section,
                        char* property,
                        char* value);
