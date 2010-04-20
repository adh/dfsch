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
 * deserialized strbufs have terminating zero byte appended, such byte
 * is not serialized into stream. Input to dfsch_serialize_strbuf()
 * does not need to have terminating zero byte.
 *
 * Stream symbol is non-empty ASCIIZ string that is used to identify
 * various substructures of serialized stream (eg. names of
 * deserialization functions and other C symbols). Repeated occurences
 * of same symbol are coallesced together and replaced by back
 * references.
 * 
 * either:
 *  - non-positive integer signifing already seen symbol
 * or:
 *  - length
 *  - symbol name bytes (*length)
 *
 */                                                                     

struct dfsch_serializer_t {
  dfsch_type_t* type;

  dfsch_eqhash_t obj_map;
  int obj_idx;
  dfsch_strhash_t sym_map;
  int sym_idx;

  dfsch_output_proc_t oproc;
  void* op_baton;

  dfsch_serializer_persistent_id_hook_t persistent_id;
  void* pi_baton;

  dfsch_serializer_object_hook_t object_hook;
  void* oh_baton;

  dfsch_serializer_unserializable_hook_t unserializable;
  void* uh_baton;
};

dfsch_type_t dfsch_serializer_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "serializer",
  .size = sizeof(dfsch_serializer_t),
};

dfsch_serializer_t* dfsch_make_serializer(dfsch_output_proc_t op,
                                          void* baton){
  dfsch_serializer_t* s = dfsch_make_object(DFSCH_SERIALIZER_TYPE);

  s->oproc = op;
  s->op_baton = baton;

  dfsch_strhash_init(&s->sym_map);
  s->sym_idx = 0;
  dfsch_eqhash_init(&s->obj_map, 1);
  s->obj_idx = 0;

  return s;
}

void dfsch_serializer_set_persistent_id(dfsch_serializer_t* s,
                                        dfsch_serializer_persistent_id_hook_t h,
                                        void* baton){
  s->persistent_id = h;
  s->pi_baton = baton;
}
void dfsch_serializer_set_object_hook(dfsch_serializer_t* s,
                                      dfsch_serializer_object_hook_t h,
                                      void *baton){
  
  s->object_hook = h;
  s->oh_baton = baton;
}
void dfsch_serializer_set_unserializable_hook(dfsch_serializer_t* s,
                                              dfsch_serializer_unserializable_hook_t h,
                                              void* baton){
  s->unserializable = h;
  s->uh_baton = baton;
}


static void serialize_bytes(dfsch_serializer_t* s,
                            char* buf,
                            size_t len){
  s->oproc(s->op_baton, buf, len);
}

static void serialize_back_reference(dfsch_serializer_t* s,
                                     int ref){
  dfsch_serialize_stream_symbol(s, "back-reference");
  dfsch_serialize_integer(s, ref);
}

void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj){
  dfsch_type_t* klass;
  dfsch_object_t* idx = (int)dfsch_eqhash_ref(&s->obj_map, obj);
  
  dfsch_eqhash_set(&s->obj_map, obj, (dfsch_object_t*)(s->obj_idx + 1));
  if (idx != DFSCH_INVALID_OBJECT){
    serialize_back_reference(s, s->obj_idx - ((int)idx));
    s->obj_idx++;
  }

  if (s->object_hook){
    if (s->object_hook(s, obj, s->oh_baton)){
      return;
    }
  }

  if (s->persistent_id){
    char* pi = s->persistent_id(s, obj, s->pi_baton);
    if (pi){
      dfsch_serialize_stream_symbol(s, "persistent-id");
      dfsch_serialize_stream_symbol(s, pi);
      return;
    }
  }

  klass = DFSCH_TYPE_OF(obj);

  while (klass){
    if (klass->serialize){
      if (klass->serialize(obj, s)){
        return;
      } else {
        break;
      }
    }
    klass = klass->superclass;
  }

  if (s->unserializable){
    s->unserializable(s, obj, s->uh_baton);
  }

  dfsch_error("Object cannot be serialized", obj);
}

void dfsch_serialize_integer(dfsch_serializer_t* s,
                             int64_t i){
  char buf[9];

  if (i >= -(1 << 6) && i < (1 << 6)){
    buf[0] = i;
    serialize_bytes(s, &buf, 1);
  } else if (i >= -(1ll << 13) && i < (1ll << 13)){
    buf[0] = 0x80 | ((i >> 8) & 0x3f);
    buf[1] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 2);    
  } else if (i >= -(1ll << 20) && i < (1ll << 20)){
    buf[0] = 0xc0 | ((i >> 16) & 0x1f);
    buf[1] = (i >> 8) & 0xff;
    buf[2] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 3);    
  } else if (i >= -(1ll << 27) && i < (1ll << 27)){
    buf[0] = 0xe0 | ((i >> 24) & 0x0f);
    buf[1] = (i >> 16) & 0xff;
    buf[2] = (i >> 8) & 0xff;
    buf[3] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 4);    
  } else if (i >= -(1ll << 34) && i < (1ll << 34)){
    buf[0] = 0xf0 | ((i >> 32) & 0x07);
    buf[1] = (i >> 24) & 0xff;
    buf[2] = (i >> 16) & 0xff;
    buf[3] = (i >> 8) & 0xff;
    buf[4] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 5);    
  } else if (i >= -(1ll << 41) && i < (1ll << 41)){
    buf[0] = 0xf8 | ((i >> 40) & 0x03);
    buf[1] = (i >> 32) & 0xff;
    buf[2] = (i >> 24) & 0xff;
    buf[3] = (i >> 16) & 0xff;
    buf[4] = (i >> 8) & 0xff;
    buf[5] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 6);    
  } else if (i >= -(1ll << 48) && i < (1ll << 48)){
    buf[0] = 0xfc | ((i >> 48) & 0x01);
    buf[1] = (i >> 40) & 0xff;
    buf[2] = (i >> 32) & 0xff;
    buf[3] = (i >> 24) & 0xff;
    buf[4] = (i >> 16) & 0xff;
    buf[5] = (i >> 8) & 0xff;
    buf[6] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 7);    
  } else if (i >= -(1ll << 55) && i < (1ll << 55)){
    buf[0] = 0xfe; 
    buf[1] = (i >> 48) & 0xff;
    buf[2] = (i >> 40) & 0xff;
    buf[3] = (i >> 32) & 0xff;
    buf[4] = (i >> 24) & 0xff;
    buf[5] = (i >> 16) & 0xff;
    buf[6] = (i >> 8) & 0xff;
    buf[7] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 8);    
  } else {
    buf[0] = 0xff; 
    buf[1] = (i >> 56) & 0xff;
    buf[2] = (i >> 48) & 0xff;
    buf[3] = (i >> 40) & 0xff;
    buf[4] = (i >> 32) & 0xff;
    buf[5] = (i >> 24) & 0xff;
    buf[6] = (i >> 16) & 0xff;
    buf[7] = (i >> 8) & 0xff;
    buf[8] = (i >> 0) & 0xff;
    serialize_bytes(s, &buf, 8);    
  }
}
void dfsch_serialize_strbuf(dfsch_serializer_t* s,
                            dfsch_strbuf_t* sb){
  dfsch_serialize_integer(s, sb->len);
  serialize_bytes(s, sb->ptr, sb->len);
}
void dfsch_serialize_stream_symbol(dfsch_serializer_t* s,
                                   char* sym){
  int idx = (int)dfsch_strhash_ref(&s->sym_map, sym);
  
  if (idx){
    dfsch_serialize_integer(s, s->sym_idx - idx);
  } else {
    size_t len = strlen(sym);
    dfsch_serialize_integer(s, len);
    serialize_bytes(s, sym, len);
  }
  
  s->sym_idx++;
  dfsch_strhash_set(&s->sym_map, sym, (void*)s->sym_idx);
}

struct dfsch_deserializer_t {
  dfsch_type_t* type;
};

dfsch_deserializer_t* dfsch_make_deserializer(dfsch_input_proc_t ip,
                                              void* baton){

}

dfsch_object_t* dfsch_deserialize_object(dfsch_deserializer_t* ds){

}

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds){

}
dfsch_strbuf_t* dfsch_deserialize_strbuf(dfsch_deserializer_t* ds){

}
char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds){

}
