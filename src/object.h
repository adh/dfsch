#ifndef H__dfsch__object__
#define H__dfsch__object__

typedef dfsch_object_t object_t;
typedef dfsch_ctx_t context_t;

typedef struct symbol_t symbol_t;

struct symbol_t{
  dfsch_type_t* type;
  char *data;
  //  object_t *next;
  //  object_t *prev;
};


struct dfsch_ctx_t{
  object_t* env;
};

typedef struct pair_t{
  dfsch_type_t* type;
  object_t *car;
  object_t *cdr;
} pair_t;

typedef struct primitive_t {
  dfsch_type_t* type;
  dfsch_primitive_t proc;
  void *baton;

} primitive_t;

typedef struct closure_t{
  dfsch_type_t* type;
  object_t* args;
  object_t* code;
  object_t* env;
  object_t* name;
} closure_t;

typedef struct exception_t{
  dfsch_type_t* type;
  object_t *class;
  object_t *data;
} exception_t; 

/*typedef struct native_t {
  dfsch_type_t* type;
  object_t* type;
  void *data; 

  } native_t;*/
typedef struct vector_t {
  dfsch_type_t* type;
  size_t length;
  object_t** data;

} vector_t;


/*struct dfsch_object_t{
  dfsch_type_t* type;
  /*  union {
    pair_t pair;
    double number;
    symbol_t symbol;
    primitive_t primitive;
    closure_t closure;
    object_t *macro;
    exception_t exception;
    vector_t vector;
    native_t native;
    } data;
};*/

typedef struct macro_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
} macro_t;
typedef struct form_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
} form_t;


#define TYPE_CHECK(obj, t, name) \
  if (((dfsch_object_t*)obj)->type != t) \
    dfsch_throw("exception:not-a-" name, (dfsch_object_t*)obj);

#endif
