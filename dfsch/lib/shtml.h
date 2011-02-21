#ifndef H__dfsch__shtml__
#define H__dfsch__shtml__

#include <dfsch/dfsch.h>

typedef enum {
  ELEM_VOID,
  ELEM_CDATA,
  ELEM_RCDATA,
  ELEM_PCDATA
} dfsch_shtml_element_type_t;

dfsch_shtml_element_type_t dfsch_shtml_get_element_type(char* name);

typedef struct dfsch_shtml_emitter_params_t {
  int pretty_print;
  char* prepend_string;
} dfsch_shtml_emitter_params_t;

dfsch_shtml_emitter_params_t* dfsch_shtml_emitter_params(dfsch_object_t* args);


char* dfsch_shtml_emit_cstr(dfsch_object_t* infoset,
                           dfsch_shtml_emitter_params_t* params);
void dfsch_shtml_emit_port(dfsch_object_t* infoset, dfsch_object_t* port,
                          dfsch_shtml_emitter_params_t* params);
void dfsch_shtml_emit_file(dfsch_object_t* infoset, char* filename,
                          dfsch_shtml_emitter_params_t* params);

#endif
