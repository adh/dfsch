#ifndef H__dfsch__serdes__
#define H__dfsch__serdes__

#include <dfsch/dfsch.h>
#include <dfsch/strings.h>

typedef struct dfsch_serializer_t dfsch_serializer_t;

dfsch_serializer_t* dfsch_make_serializer(dfsch_object_t* port);

void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj);

void dfsch_serialize_integer(dfsch_serializer_t* s,
                             int64_t i);
void dfsch_serialize_strbuf(dfsch_serializer_t* s,
                            dfsch_strbuf_t* sb);
void dfsch_serialize_stream_symbol(dfsch_serializer_t* s,
                                   char* sym);

typedef int (*dfsch_serializer_object_hook_t)(dfsch_serializer_t* serializer,
                                              dfsch_object_t* obj,
                                              void* baton);
typedef int (*dfsch_serializer_unserializable_hook_t)(dfsch_serializer_t* serializer,
                                                      dfsch_object_t* obj,
                                                      void* baton);

void dfsch_serializer_set_object_hook(dfsch_serializer_t* s,
                                      dfsch_serializer_object_hook_t h,
                                      void *baton);
void dfsch_serializer_set_unserializable_hook(dfsch_serializer_t* s,
                                              dfsch_serializer_unserializable_hook_t h,
                                              void* baton);

typedef struct dfsch_deserializer_t dfsch_deserializer_t;

dfsch_deserializer_t* dfsch_make_deserializer(dfsch_object_t* port);

dfsch_object_t* dfsch_deserialize_object(dfsch_deserializer_t* ds);

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds);
dfsch_strbuf_t* dfsch_deserialize_strbuf(dfsch_deserializer_t* ds);
char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds);


char* dfsch_get_externalized_object_name(dfsch_object_t* obj);
dfsch_object_t* dfsch_get_externalized_object(char* name);

#endif
