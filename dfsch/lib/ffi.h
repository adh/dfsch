#ifndef H__dfsch_lib__ffi__
#define H__dfsch_lib__ffi__

#include <dfsch/dfsch.h>
#include <ffi.h>

extern dfsch_type_t dfsch_ffi_library_type;
#define DFSCH_FFI_LIBRARY_TYPE (&dfsch_ffi_library_type)
extern dfsch_type_t dfsch_ffi_function_type;
#define DFSCH_FFI_FUNCTION_TYPE (&dfsch_ffi_function_type)

extern dfsch_type_t dfsch_ffi_pointer_type;
#define DFSCH_FFI_POINTER_TYPE (&dfsch_ffi_pointer_type)

dfsch_object_t* dfsch_ffi_load_library(char* filename);

dfsch_object_t* dfsch_ffi_call(dfsch_object_t* lib,
                               char* fun_name,
                               dfsch_object_t* args);

dfsch_object_t* dfsch_ffi_make_function(dfsch_object_t* lib,
                                        char* fun_name,
                                        dfsch_object_t* ret_type,
                                        dfsch_object_t* arg_types);

dfsch_object_t* dfsch_ffi_wrap_pointer(void* ptr);
void* dfsch_ffi_unwrap_pointer(dfsch_object_t* obj);

#endif
