#ifndef H__dfsch__object__
#define H__dfsch__object__

#include <dfsch/magic.h>
#include <dfsch/eqhash.h>

typedef dfsch_object_t object_t;

typedef dfsch__symbol_t symbol_t;

typedef dfsch_primitive_t primitive_t;

#define LL_FLAG_ALLOW_OTHER_KEYS 1

typedef struct lambda_list_t {
  dfsch_type_t* type;
  uint16_t flags;
  uint16_t positional_count;
  uint16_t keyword_count;
  uint16_t optional_count;
  dfsch_object_t* rest;
  dfsch_object_t** defaults;
  dfsch_object_t** supplied_p;
  dfsch_object_t** keywords;
  dfsch_object_t* aux_list;
  dfsch_object_t* arg_list[];
} lambda_list_t;


typedef struct vector_t {
  dfsch_type_t* type;
  size_t length;
  object_t* data[];
} vector_t;

typedef dfsch_macro_t macro_t;

#define ENV_CONSTANT_FLAG 1

typedef struct environment_t environment_t;

#define EFRAME_RETAIN 1

struct environment_t {
  dfsch_type_t* type;
  environment_t* parent; 
  dfsch__thread_info_t* owner;
  dfsch_eqhash_t values;
  dfsch_object_t* decls;
  dfsch_object_t* context;
  int flags;
};

typedef struct closure_t{
  dfsch_type_t* type;
  lambda_list_t* args;
  object_t* code;
  environment_t* env;
  object_t* name;
  object_t* orig_code;
  object_t* documentation;
} closure_t;

struct dfsch__stack_frame_t {
  dfsch_object_t* procedure;
  dfsch_object_t* arguments;
  int tail_recursive;

  dfsch_object_t* code;
  dfsch_object_t* env;
  dfsch_object_t* expr;

  dfsch__stack_frame_t* next;
};


#endif
