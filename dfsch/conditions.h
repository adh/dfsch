#ifndef H__dfsch__conditions__
#define H__dfsch__conditions__

#include <dfsch/dfsch.h>
#include <stdarg.h>


typedef struct dfsch__condition_t {
  dfsch_type_t* type;
  dfsch_object_t* fields;
} dfsch__condition_t;

dfsch_object_t* dfsch_make_condition(dfsch_type_t* type);

dfsch_object_t* dfsch_condition_field(dfsch_object_t* condition,
                                      dfsch_object_t* name);
void dfsch_condition_put_field(dfsch_object_t* condition,
                               dfsch_object_t* name,
                               dfsch_object_t* value);
dfsch_object_t* dfsch_condition_field_cstr(dfsch_object_t* condition,
                                           char* name);
void dfsch_condition_put_field_cstr(dfsch_object_t* condition,
                                    char* name,
                                    dfsch_object_t* value);
dfsch_object_t* dfsch_condition_fields(dfsch_object_t* condition);

dfsch_object_t* dfsch_condition(dfsch_type_t* type, ...);


#define DFSCH_CONDITION_SIZE (sizeof(dfsch__condition_t))
#define DFSCH_CONDITION_TYPE_INIT(super, name)                  \
  {DFSCH_STANDARD_TYPE, super, DFSCH_CONDITION_SIZE, name}

extern dfsch_type_t dfsch_condition_type;
#define DFSCH_CONDITION_TYPE (&dfsch_condition_type)

extern dfsch_type_t dfsch_warning_type;
#define DFSCH_WARNING_TYPE (&dfsch_warning_type)

extern dfsch_type_t dfsch_error_type;
#define DFSCH_ERROR_TYPE (&dfsch_error_type)

extern dfsch_type_t dfsch_runtime_error_type;
#define DFSCH_RUNTIME_ERROR_TYPE (&dfsch_runtime_error_type)

void dfsch_signal(dfsch_object_t* condition);


dfsch_object_t* dfsch_make_restart(dfsch_object_t* name,
                                   dfsch_object_t* proc,
                                   char* description);
extern dfsch_type_t dfsch_restart_type;
#define DFSCH_RESTART_TYPE (&dfsch_restart_type)


#endif
