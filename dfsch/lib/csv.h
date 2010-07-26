#ifndef H__dfsch_lib__csv__
#define H__dfsch_lib__csv__

#include <dfsch/dfsch.h>

typedef struct dfsch_csv_params_t {
  char delim;
  char quote;
  char escape;
} dfsch_csv_params_t;

dfsch_object_t* dfsch_csv_read_line(dfsch_object_t* port,
                                    dfsch_csv_params_t* params);
dfsch_object_t* dfsch_csv_read_file(dfsch_object_t* port,
                                    dfsch_csv_params_t* params);

dfsch_csv_params_t* dfsch_csv_params(dfsch_object_t* args);

#endif
