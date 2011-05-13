#define _GNU_SOURCE
#include <dfsch/lib/ffi.h>
#include <dlfcn.h>
#include <ffi.h>

typedef struct library_t {
  dfsch_type_t* type;
  void* handle;
} library_t;

dfsch_type_t dfsch_ffi_library_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "ffi:library",
  .size = sizeof(library_t)
};

dfsch_object_t* dfsch_ffi_load_library(char* filename){
  library_t* l = dfsch_make_object(DFSCH_FFI_LIBRARY_TYPE);

  l->handle = dlopen(filename, RTLD_NOW);
  if (!l->handle){
    dfsch_error("Unable to load library",
                dfsch_make_string_cstr(dlerror()));
  }

  return (dfsch_object_t*)l;
}

static void* get_handle(dfsch_object_t* lib){
  library_t* l;
#ifdef RTLD_DEFAULT
  if (!lib){
    return RTLD_DEFAULT;
  }
#endif
  l = DFSCH_ASSERT_INSTANCE(lib, DFSCH_FFI_LIBRARY_TYPE);
  return l->handle;
}

typedef struct pointer_t {
  dfsch_type_t* type;
  void* ptr;
} pointer_t;

static int pointer_equal_p(pointer_t* a, pointer_t* b){
  return a->ptr == b->ptr;
}
static size_t pointer_hash(pointer_t* p){
  return ((size_t)p->ptr) ^ (((size_t)p->ptr) >> 7);
}
static void pointer_write(pointer_t* p, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)p, 
                         "%p", 
                         p->ptr);
}


dfsch_type_t dfsch_ffi_pointer_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "ffi:pointer",
  .size = sizeof(pointer_t),
  .equal_p = pointer_equal_p,
  .write = pointer_write,
  .hash = pointer_hash
};

dfsch_object_t* dfsch_ffi_wrap_pointer(void* ptr){
  pointer_t* p = dfsch_make_object(DFSCH_FFI_POINTER_TYPE);
  p->ptr = ptr;
  return p;
}
void* dfsch_ffi_unwrap_pointer(dfsch_object_t* obj){
  pointer_t* p = DFSCH_ASSERT_INSTANCE(obj, DFSCH_FFI_POINTER_TYPE);
  return p->ptr;
}


typedef struct internal_type_t {
  char* name;
  dfsch_object_t* (*to_object)(void* data);
  void* (*from_object)(dfsch_object_t* obj);
  ffi_type* type;
  size_t size;
} internal_type_t;

#define NUM_CONV(type, name, conv)                      \
  static void* name##_from_object(dfsch_object_t* obj){ \
    type n = dfsch_number_to_##conv(obj);               \
    type* res = GC_NEW_ATOMIC(type);                    \
    *res = n;                                           \
    return res;                                         \
  }                                                     \
  static dfsch_object_t* name##_to_object(void* data){  \
    type* d = (type*)data;                              \
    return dfsch_make_number_from_##conv(*d);           \
  }

NUM_CONV(signed char, schar, long)
NUM_CONV(unsigned char, char, long)
NUM_CONV(short, short, long)
NUM_CONV(int, int, long)
NUM_CONV(long, long, long)
NUM_CONV(unsigned char, uchar, long)
NUM_CONV(unsigned short, ushort, long)
NUM_CONV(unsigned int, uint, long)
NUM_CONV(unsigned long, ulong, uint64)
NUM_CONV(float, float, double)
NUM_CONV(double, double, double)

#define SIMPLE_TYPE(type, name, ftype) \
  {#name, name##_to_object, name##_from_object, &ftype, sizeof(type)}

static dfsch_object_t* void_null(void* discard){
  return NULL;
}

static void* string_from_object(dfsch_object_t* obj){
  char* n = dfsch_string_to_cstr(obj);
  char** res = GC_NEW(char*);
  *res = n;
  return res;
}
static dfsch_object_t* string_to_object(void* data){
  char** d = (char**)data;
  return dfsch_make_string_cstr(*d);
}

static void* pointer_from_object(dfsch_object_t* obj){
  void* n = dfsch_ffi_unwrap_pointer(obj);
  void** res = GC_NEW(void*);
  *res = n;
  return res;
}
static dfsch_object_t* pointer_to_object(void* data){
  void** d = (void**)data;
  return dfsch_ffi_wrap_pointer(*d);
}


static internal_type_t internal_types[] = {
  {"void", void_null, NULL, &ffi_type_void, 0},
  SIMPLE_TYPE(int, int, ffi_type_sint),
  SIMPLE_TYPE(double, double, ffi_type_double),
  SIMPLE_TYPE(char*, string, ffi_type_pointer),
  SIMPLE_TYPE(void*, pointer, ffi_type_pointer),

  SIMPLE_TYPE(char, schar, ffi_type_schar),
  SIMPLE_TYPE(char, char, ffi_type_uchar),
  SIMPLE_TYPE(short, short, ffi_type_sshort),
  SIMPLE_TYPE(long, long, ffi_type_slong),
  SIMPLE_TYPE(unsigned char, uchar, ffi_type_uchar),
  SIMPLE_TYPE(unsigned short, ushort, ffi_type_ushort),
  SIMPLE_TYPE(unsigned int, uint, ffi_type_uint),
  SIMPLE_TYPE(unsigned long, ulong, ffi_type_ulong),
  SIMPLE_TYPE(float, float, ffi_type_float),
  
};


typedef struct function_t {
  dfsch_type_t* type;
  void* code;
  int num_args;
  
  ffi_cif cif;
} function_t;

dfsch_type_t dfsch_ffi_function_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "ffi:function",
  .size = sizeof(function_t)
};


dfsch_object_t* dfsch_ffi_make_function(dfsch_object_t* lib,
                                        char* fun_name,
                                        dfsch_object_t* ret_type,
                                        dfsch_object_t* arg_types){
  
}

static internal_type_t* find_type(dfsch_object_t* name){
  int i;
  for (i = 0; i < sizeof(internal_types) / sizeof(internal_type_t); i++){
    if (dfsch_compare_keyword(name, internal_types[i].name)){
      return &(internal_types[i]);
    }
  }
  dfsch_error("No such type", name);
}

static internal_type_t* get_object_type(dfsch_object_t* obj){
  if (dfsch_string_p(obj)){
    return &(internal_types[3]);
  }
  if (dfsch_integer_p(obj)){
    return &(internal_types[1]);
  }
  if (dfsch_real_p(obj)){
    return &(internal_types[2]);
  }
  if (DFSCH_INSTANCE_P(obj, DFSCH_FFI_POINTER_TYPE)){
    return &(internal_types[4]);
  }
  dfsch_error("No automatic conversion for this object", obj);
}

dfsch_object_t* dfsch_ffi_call(dfsch_object_t* lib,
                               char* fun_name,
                               dfsch_object_t* args){
  void* h = get_handle(lib);
  void* fn;
  ffi_cif cif;
  ffi_type **argtypes = GC_MALLOC(sizeof(ffi_type*) * 8);
  void** values = GC_MALLOC(sizeof(void*) * 8);
  int int_ret;
  void* ret = &int_ret;
  internal_type_t* return_type = internal_types + 1; /* int */
  size_t argcount = 0;
  size_t argalloc = 8;
  internal_type_t* type = NULL;
  ffi_status fr;

  fn = dlsym(h, fun_name);
  if (!fn){
    dfsch_error("Unable to call function",
                dfsch_make_string_cstr(dlerror()));
  }

  while (DFSCH_PAIR_P(args)){
    dfsch_object_t* a = DFSCH_FAST_CAR(args);
    args = DFSCH_FAST_CDR(args);

    if (dfsch_keyword_p(a)){
      if (dfsch_compare_keyword(a, "return-type")){
        dfsch_object_t* type;
        DFSCH_OBJECT_ARG(args, type);
        return_type = find_type(type);
        ret = GC_MALLOC(return_type->size);
        if (!return_type->to_object){
          dfsch_error("Type not supported for return values", type);
        }
        continue;
      }
      
      type = find_type(a);
      if (!type->to_object){
        dfsch_error("Type not supported for function arguments", a);
      }
    }

    if (argcount >= argalloc){
      argalloc *= 2;
      argtypes = GC_REALLOC(argtypes, sizeof(ffi_type*) * argalloc);
      values = GC_REALLOC(values, sizeof(void*) * argalloc);
    }

    if (!type){
      type = get_object_type(a);
    }

    argtypes[argcount] = type->type;
    values[argcount] = type->from_object(a);
    type = NULL;
    argcount++;
  }

  if (args){
    dfsch_error("Invalid argument list", NULL);
  }

  fr = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, argcount, 
                    return_type->type, argtypes);
  if (fr != FFI_OK){
    switch (fr){
    case FFI_BAD_TYPEDEF:
      dfsch_error("Incorect type passed to prepare_cif", NULL);
    case FFI_BAD_ABI:
      dfsch_error("Unknown ABI", NULL);
    }
  }

  ffi_call(&cif, fn, ret, values);
  return return_type->to_object(ret);
}

