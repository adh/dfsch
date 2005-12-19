#ifndef H__dfsch__object__
#define H__dfsch__object__

typedef enum {
  PAIR,
  SYMBOL,
  NUMBER,
  STRING,
  PRIMITIVE,
  CLOSURE,
  MACRO,
  FORM,
  EXCEPTION,
  VECTOR,
  NATIVE // define new types here
} type_t ;

typedef dfsch_object_t object_t;
typedef dfsch_ctx_t context_t;

typedef struct symbol_t symbol_t;

struct symbol_t{
  char *data;
  //  object_t *next;
  //  object_t *prev;
};


struct dfsch_ctx_t{
  object_t* env;
};

typedef struct pair_t{
  object_t *car;
  object_t *cdr;
} pair_t;

typedef struct primitive_t {

  dfsch_primitive_t proc;
  void *baton;

} primitive_t;

typedef struct closure_t{
  object_t* args;
  object_t* code;
  object_t* env;
  object_t* name;
} closure_t;

typedef struct exception_t{
  object_t *type;
  object_t *data;
} exception_t; 

typedef struct native_t {
  
  object_t* type;
  void *data; 

} native_t;
typedef struct vector_t {
  
  size_t length;
  object_t** data;

} vector_t;


struct dfsch_object_t{
  type_t type;
  union {
    pair_t pair;
    symbol_t symbol;
    double number;
    char* string;
    primitive_t primitive;
    closure_t closure;
    object_t *macro;
    exception_t exception;
    vector_t vector;
    native_t native;
  } data;
};

#endif
