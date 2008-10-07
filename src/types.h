#ifndef H__dfsch__object__
#define H__dfsch__object__

#include <dfsch/magic.h>

typedef dfsch_object_t object_t;

typedef struct symbol_t symbol_t;

struct symbol_t{
  dfsch_type_t* type;
  char *data;
};

typedef dfsch_primitive_t primitive_t;

typedef struct closure_t{
  dfsch_type_t* type;
  object_t* args;
  object_t* code;
  object_t* env;
  object_t* name;
  object_t* orig_code;
} closure_t;

typedef struct vector_t {
  dfsch_type_t* type;
  size_t length;
  object_t* data[];
} vector_t;

typedef struct macro_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
} macro_t;

typedef struct form_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
  dfsch_object_t* compilation_proc;
} form_t;

typedef struct environment_t environment_t;

struct environment_t {
  dfsch_type_t* type;
  dfsch_object_t* values;
  environment_t* parent;
  dfsch_object_t* proc;
  dfsch_object_t* args;
  dfsch_object_t* decls;
};

struct dfsch__stack_frame_t {
  dfsch__stack_frame_t* next;

  dfsch_object_t* procedure;
  dfsch_object_t* arguments;
  int tail_recursive;

  dfsch_object_t* code;
  dfsch_object_t* env;
  dfsch_object_t* expr;
};


#define TYPE_CHECK(obj, t, name)                                \
  if (DFSCH_TYPE_OF(obj) != t)                                  \
    dfsch_error("exception:not-a-" name, (dfsch_object_t*)obj);

#endif
