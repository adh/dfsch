#include <dfsch/serdes.h>
#include <dfsch/eqhash.h>
#include <dfsch/strhash.h>

/*
 * Integer serialization format:
 *
 * 0xxx xxxx                                                         6b
 * 10xx xxxx  xxxx xxxx                                             13b
 * 110x xxxx  xxxx xxxx  xxxx xxxx                                  20b
 * 1110 xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx                       27b
 * 1111 0xxx  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx            34b
 * 1111 10xx  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx 41b
 * 1111 110x  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx 48b
 *            xxxx xxxx                                                
 * 1111 1110  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx 55b
 *            xxxx xxxx  xxxx xxxx                                     
 * 1111 1111  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx 63b
 *            xxxx xxxx  xxxx xxxx  xxxx xxxx                          
 *
 * Strbuf serialization format:
 *  - length as integer (above)
 *  - bytes (*length)
 *
 * deserialized strbufs have terminating zero byte appended, such byte is
 * not serialized into stream. Input to dfsch_serialize_strbuf() does not
 * need to have terminating zero byte.
 *
 * Stream symbol is ASCIIZ string that is used to identify various 
 * substructures of serialized stream (eg. names of deserialization 
 * functions and other C symbols). Repeated occurences of same symbol 
 * are coallesced together and replaced by back references.
 * 
 * either:
 *  - non-zero integer signifing already seen symbol
 * or:
 *  - zero integer
 *  - strbuf-serialized symbol name
 *
 */                                                                    
                                                                       

struct dfsch_serializer_t {
  dfsch_type_t* type;

  dfsch_eqhash_t obj_map;
  dfsch_strhash_t sym_map;
};

dfsch_serializer_t* dfsch_make_serializer(dfsch_object_t* port);

void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj){

}

void dfsch_serialize_integer(dfsch_serializer_t* s,
                             int64_t i){

}
void dfsch_serialize_strbuf(dfsch_serializer_t* s,
                            dfsch_strbuf_t* sb){

}
void dfsch_serialize_stream_symbol(dfsch_serializer_t* s,
                                   char* sym){
  
}

struct dfsch_deserializer_t {
  dfsch_type_t* type;
};

dfsch_deserializer_t* dfsch_make_deserializer(dfsch_object_t* port){
}

dfsch_object_t* dfsch_deserialize_object(dfsch_deserializer_t* ds){

}

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds){

}
dfsch_strbuf_t* dfsch_deserialize_strbuf(dfsch_deserializer_t* ds){

}
char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds){

}
