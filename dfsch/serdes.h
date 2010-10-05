#ifndef H__dfsch__serdes__
#define H__dfsch__serdes__

#include <dfsch/dfsch.h>
#include <dfsch/strings.h>

extern dfsch_type_t dfsch_serializer_type;
#define DFSCH_SERIALIZER_TYPE (&dfsch_serializer_type)

dfsch_serializer_t* dfsch_make_serializer(dfsch_output_proc_t op,
                                          void* baton);

void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj);

void dfsch_serialize_integer(dfsch_serializer_t* s,
                             int64_t i);
void dfsch_serialize_strbuf(dfsch_serializer_t* s,
                            dfsch_strbuf_t* sb);
void dfsch_serialize_stream_symbol(dfsch_serializer_t* s,
                                   char* sym);

dfsch_serializer_t* dfsch_make_subserializer(dfsch_serializer_t* s);
void dfsch_commit_subserializer(dfsch_serializer_t* s);

typedef 
char* (*dfsch_serializer_persistent_id_hook_t)(dfsch_serializer_t* serializer,
                                               dfsch_object_t* obj,
                                               void* baton);
typedef 
int (*dfsch_serializer_object_hook_t)(dfsch_serializer_t* serializer,
                                              dfsch_object_t* obj,
                                              void* baton);
typedef 
int (*dfsch_serializer_unserializable_hook_t)(dfsch_serializer_t* serializer,
                                              dfsch_object_t* obj,
                                              void* baton);

void dfsch_serializer_set_persistent_id(dfsch_serializer_t* s,
                                        dfsch_serializer_persistent_id_hook_t h,
                                        void* baton);
void dfsch_serializer_set_object_hook(dfsch_serializer_t* s,
                                      dfsch_serializer_object_hook_t h,
                                      void *baton);
void dfsch_serializer_set_unserializable_hook(dfsch_serializer_t* s,
                                              dfsch_serializer_unserializable_hook_t h,
                                              void* baton);

extern dfsch_type_t dfsch_deserializer_type;
#define DFSCH_DESERIALIZER_TYPE (&dfsch_deserializer_type)

typedef struct dfsch_deserializer_t dfsch_deserializer_t;

dfsch_deserializer_t* dfsch_make_deserializer(dfsch_input_proc_t ip,
                                              void* baton);

dfsch_object_t* dfsch_deserialize_object(dfsch_deserializer_t* ds);

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds);
dfsch_strbuf_t* dfsch_deserialize_strbuf(dfsch_deserializer_t* ds);
char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds);

void dfsch_deserializer_put_partial_object(dfsch_deserializer_t* ds,
                                           dfsch_object_t* obj);

typedef 
dfsch_object_t* (*dfsch_deserializer_unknown_hook_t)(dfsch_deserializer_t* ds,
                                                     char* name,
                                                     void* baton);
typedef 
dfsch_object_t* (*dfsch_deserializer_persistent_hook_t)(dfsch_deserializer_t* ds,
                                                        char* name,
                                                        void* baton);

void dfsch_deserializer_set_unknown_hook(dfsch_deserializer_t* ds,
                                         dfsch_deserializer_unknown_hook_t h,
                                         void* baton);
void dfsch_deserializer_set_persistent_hook(dfsch_deserializer_t* ds,
                                            dfsch_deserializer_persistent_hook_t h,
                                            void* baton);

typedef dfsch_object_t* (*dfsch_deserializer_handler_t)(dfsch_deserializer_t* ds);

void dfsch_register_deserializer_handler(char* name,
                                         dfsch_deserializer_handler_t h);

char* dfsch_get_externalized_object_name(dfsch_object_t* obj);
dfsch_object_t* dfsch_get_externalized_object(char* name);

#define DFSCH_DEFINE_DESERIALIZATION_HANDLER(name, cname)\
  static dfsch_object_t* deshandler_##cname(dfsch_deserializer_t* ds);  \
  static void __attribute__((constructor)) dhcons__##cname(){           \
    dfsch_register_deserializer_handler(name, deshandler_##cname);      \
  }                                                                     \
  static dfsch_object_t* deshandler_##cname(dfsch_deserializer_t* ds)


#endif