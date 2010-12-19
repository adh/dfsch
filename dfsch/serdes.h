#ifndef H__dfsch__serdes__
#define H__dfsch__serdes__

#include <dfsch/dfsch.h>
#include <dfsch/strings.h>

extern dfsch_type_t dfsch_serializer_type;
#define DFSCH_SERIALIZER_TYPE (&dfsch_serializer_type)

dfsch_serializer_t* dfsch_make_serializer(dfsch_output_proc_t op,
                                          void* baton);
dfsch_serializer_t* dfsch_serializer(dfsch_object_t* obj);

void dfsch_serializer_write_stream_header(dfsch_serializer_t* s,
                                          char* format);

int dfsch_type_serializable_p(dfsch_type_t* type);
void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj);

void dfsch_serialize_integer(dfsch_serializer_t* s,
                             int64_t i);
void dfsch_serialize_strbuf(dfsch_serializer_t* s,
                            dfsch_strbuf_t* sb);
void dfsch_serialize_stream_symbol(dfsch_serializer_t* s,
                                   char* sym);

typedef 
int (*dfsch_serializer_object_hook_t)(dfsch_serializer_t* serializer,
                                              dfsch_object_t* obj,
                                              void* baton);
typedef 
int (*dfsch_serializer_unserializable_hook_t)(dfsch_serializer_t* serializer,
                                              dfsch_object_t* obj,
                                              void* baton);

void dfsch_serializer_set_object_hook(dfsch_serializer_t* s,
                                      dfsch_serializer_object_hook_t h,
                                      void *baton);
void dfsch_serializer_set_unserializable_hook(dfsch_serializer_t* s,
                                              dfsch_serializer_unserializable_hook_t h,
                                              void* baton);
void dfsch_serializer_set_canonical_environment(dfsch_serializer_t* s,
                                                dfsch_object_t* env);

extern dfsch_type_t dfsch_deserializer_type;
#define DFSCH_DESERIALIZER_TYPE (&dfsch_deserializer_type)

typedef struct dfsch_deserializer_t dfsch_deserializer_t;

dfsch_deserializer_t* dfsch_make_deserializer(dfsch_input_proc_t ip,
                                              void* baton);
dfsch_deserializer_t* dfsch_deserializer(dfsch_object_t* obj);

void dfsch_deserializer_read_stream_header(dfsch_deserializer_t* ds,
                                           char* fmt);

dfsch_object_t* dfsch_deserialize_object(dfsch_deserializer_t* ds);

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds);
dfsch_strbuf_t* dfsch_deserialize_strbuf(dfsch_deserializer_t* ds);
char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds);

void dfsch_deserializer_put_partial_object(dfsch_deserializer_t* ds,
                                           dfsch_object_t* obj);
dfsch_object_t** dfsch_deserializer__skip_object(dfsch_deserializer_t* ds);

typedef 
dfsch_object_t* (*dfsch_deserializer_unknown_hook_t)(dfsch_deserializer_t* ds,
                                                     char* name,
                                                     void* baton);

void dfsch_deserializer_set_unknown_hook(dfsch_deserializer_t* ds,
                                         dfsch_deserializer_unknown_hook_t h,
                                         void* baton);
void dfsch_deserializer_set_canonical_environment(dfsch_deserializer_t* ds,
                                                  dfsch_object_t* env);

typedef dfsch_object_t* (*dfsch_deserializer_handler_t)(dfsch_deserializer_t* ds);

void dfsch_register_deserializer_handler(char* name,
                                         dfsch_deserializer_handler_t h);

#define DFSCH_DEFINE_DESERIALIZATION_HANDLER(name, cname)\
  static dfsch_object_t* deshandler_##cname(dfsch_deserializer_t* ds);  \
  static void __attribute__((constructor)) dhcons__##cname(){       \
    dfsch_register_deserializer_handler(name, deshandler_##cname);      \
  }                                                                     \
  static dfsch_object_t* deshandler_##cname(dfsch_deserializer_t* ds)

/* High-level C interface */

dfsch_strbuf_t* dfsch_serialize(dfsch_object_t* obj, 
                                dfsch_object_t* canon_env);
dfsch_object_t* dfsch_deserialize(dfsch_strbuf_t* sb,
                                  dfsch_object_t* canon_env);


#define DFSCH_SERIALIZER_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, dfsch_serializer_t*, dfsch_serializer)
#define DFSCH_DESERIALIZER_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, dfsch_deserializer_t*, dfsch_deserializer)

#endif
