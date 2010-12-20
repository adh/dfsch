#ifndef H__dfsch__random__
#define H__dfsch__random__

#include <dfsch/dfsch.h>

typedef void (*dfsch_random_get_bytes_t)(dfsch_object_t* state, 
                                         uint8_t* buf, size_t len);

typedef struct dfsch_random_state_type_t {
  dfsch_type_t type;
  dfsch_random_get_bytes_t get_bytes;
  int deterministic;
} dfsch_random_state_type_t;

extern dfsch_type_t dfsch_random_state_type;
#define DFSCH_RANDOM_STATE_TYPE (&dfsch_random_state_type)
extern dfsch_type_t dfsch_random_state_type_type;
#define DFSCH_RANDOM_STATE_TYPE_TYPE (&dfsch_random_state_type_type)
extern dfsch_random_state_type_t dfsch_default_random_state_type;
#define DFSCH_DEFAULT_RANDOM_STATE_TYPE (&dfsch_default_random_state_type)
extern dfsch_random_state_type_t dfsch_file_random_state_type;
#define DFSCH_FILE_RANDOM_STATE_TYPE (&dfsch_file_random_state_type)
extern dfsch_random_state_type_t dfsch_lcg_random_state_type;
#define DFSCH_LCG_RANDOM_STATE_TYPE (&dfsch_lcg_random_state_type)

dfsch_object_t* dfsch_get_random_state();
void dfsch_set_random_state(dfsch_object_t* state);

void dfsch_random_get_bytes(dfsch_object_t* state, uint8_t* buf, size_t len);
int64_t dfsch_random_get_integer(dfsch_object_t* state, int64_t max);
double dfsch_random_get_double(dfsch_object_t* state);
dfsch_object_t* dfsch_random_get_number(dfsch_object_t* state, 
                                        dfsch_object_t* max);
dfsch_object_t* dfsch_random_get_bignum(dfsch_object_t* state,
                                        size_t len);

dfsch_object_t* dfsch_make_default_random_state(uint8_t* seed, size_t len);
dfsch_object_t* dfsch_make_file_random_state(char* filename);

void dfsch_get_random_id(char buf[18]);

#endif
