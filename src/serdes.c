#include <dfsch/serdes.h>
#include <dfsch/eqhash.h>
#include <dfsch/strhash.h>

#include "util.h"

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
  while (len){
    printf("%02hhx ", *buf);
    buf++; len--;
  }
  puts("");
}


static void serialize_back_reference(dfsch_serializer_t* s,
                                     int ref){
  dfsch_serialize_stream_symbol(s, "back-reference");
  dfsch_serialize_integer(s, ref);
}

void dfsch_put_serialized_object(dfsch_serializer_t* s,
                                 dfsch_object_t* obj){
  s->obj_idx++;
  dfsch_eqhash_set(&s->obj_map, obj, (dfsch_object_t*)(s->obj_idx));
}

void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj){
  dfsch_type_t* klass;
  dfsch_object_t* idx = (int)dfsch_eqhash_ref(&s->obj_map, obj);
  
  dfsch_put_serialized_object(s, obj);

  if (idx != DFSCH_INVALID_OBJECT){
    serialize_back_reference(s, ((int)idx));
    return;
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
      dfsch_serialize_cstr(s, pi);
      return;
    }
  }

  klass = DFSCH_TYPE_OF(obj);

  while (klass){
    if (klass->serialize){
      klass->serialize(obj, s);
      return;
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
  char *buf = GC_MALLOC_ATOMIC(9);

  printf("int %d\n",i);

  if (i >= -(1 << 6) && (i < (1 << 6))){
    buf[0] = i & 0x7f;
    serialize_bytes(s, buf, 1);
  } else if (i >= -(1ll << 13) && i < (1ll << 13)){
    buf[0] = 0x80 | ((i >> 8) & 0x3f);
    buf[1] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 2);    
  } else if (i >= -(1ll << 20) && i < (1ll << 20)){
    buf[0] = 0xc0 | ((i >> 16) & 0x1f);
    buf[1] = (i >> 8) & 0xff;
    buf[2] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 3);    
  } else if (i >= -(1ll << 27) && i < (1ll << 27)){
    buf[0] = 0xe0 | ((i >> 24) & 0x0f);
    buf[1] = (i >> 16) & 0xff;
    buf[2] = (i >> 8) & 0xff;
    buf[3] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 4);    
  } else if (i >= -(1ll << 34) && i < (1ll << 34)){
    buf[0] = 0xf0 | ((i >> 32) & 0x07);
    buf[1] = (i >> 24) & 0xff;
    buf[2] = (i >> 16) & 0xff;
    buf[3] = (i >> 8) & 0xff;
    buf[4] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 5);    
  } else if (i >= -(1ll << 41) && i < (1ll << 41)){
    buf[0] = 0xf8 | ((i >> 40) & 0x03);
    buf[1] = (i >> 32) & 0xff;
    buf[2] = (i >> 24) & 0xff;
    buf[3] = (i >> 16) & 0xff;
    buf[4] = (i >> 8) & 0xff;
    buf[5] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 6);    
  } else if (i >= -(1ll << 48) && i < (1ll << 48)){
    buf[0] = 0xfc | ((i >> 48) & 0x01);
    buf[1] = (i >> 40) & 0xff;
    buf[2] = (i >> 32) & 0xff;
    buf[3] = (i >> 24) & 0xff;
    buf[4] = (i >> 16) & 0xff;
    buf[5] = (i >> 8) & 0xff;
    buf[6] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 7);    
  } else if (i >= -(1ll << 55) && i < (1ll << 55)){
    buf[0] = 0xfe; 
    buf[1] = (i >> 48) & 0xff;
    buf[2] = (i >> 40) & 0xff;
    buf[3] = (i >> 32) & 0xff;
    buf[4] = (i >> 24) & 0xff;
    buf[5] = (i >> 16) & 0xff;
    buf[6] = (i >> 8) & 0xff;
    buf[7] = (i >> 0) & 0xff;
    serialize_bytes(s, buf, 8);    
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
    serialize_bytes(s, buf, 8);    
  }
}
void dfsch_serialize_string(dfsch_serializer_t* s,
                            char* str, size_t len){
  dfsch_serialize_integer(s, len);
  serialize_bytes(s, str, len);
}
void dfsch_serialize_cstr(dfsch_serializer_t* s,
                          char* str){
  dfsch_serialize_string(s, str, strlen(str));
}

void dfsch_serialize_strbuf(dfsch_serializer_t* s,
                            dfsch_strbuf_t* sb){
  dfsch_serialize_string(s, sb->ptr, sb->len);
}
void dfsch_serialize_stream_symbol(dfsch_serializer_t* s,
                                   char* sym){
  int idx = (int)dfsch_strhash_ref(&s->sym_map, sym);
  
  if (idx){
    dfsch_serialize_integer(s, -idx);
  } else {
    size_t len = strlen(sym);
    dfsch_serialize_integer(s, len);
    serialize_bytes(s, sym, len);
    s->sym_idx++;
    dfsch_strhash_set(&s->sym_map, sym, (void*)s->sym_idx);
  }
}

typedef struct stream_symbol_t {
  char* name;
  dfsch_deserializer_handler_t handler;
} stream_symbol_t;

struct dfsch_deserializer_t {
  dfsch_type_t* type;

  dfsch_input_proc_t iproc;
  void* ip_baton;
  
  dfsch_object_t** obj_map;
  size_t obj_map_len;
  size_t obj_idx;
  
  stream_symbol_t** sym_map;
  size_t sym_map_len;
  size_t sym_idx;

  dfsch_deserializer_unknown_hook_t unknown;
  void* uh_baton;
  dfsch_deserializer_persistent_hook_t persistent;
  void* ph_baton;
};

dfsch_type_t dfsch_deserializer_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "deserializer",
  .size = sizeof(dfsch_deserializer_t),
};

static void register_core_handlers();
static dfsch_strhash_t* get_deshandler_map(){
  static dfsch_strhash_t h;
  static init = 0;
  if (!init){
    dfsch_strhash_init(&h);
    init = 1;
    register_core_handlers();
  }
  return &h;
}

static void update_handler_cache(stream_symbol_t* ss){
  if (!ss->handler){
    ss->handler = dfsch_strhash_ref(get_deshandler_map(), ss->name);
  }
}

dfsch_deserializer_t* dfsch_make_deserializer(dfsch_input_proc_t ip,
                                              void* baton){
  dfsch_deserializer_t* ds = dfsch_make_object(DFSCH_DESERIALIZER_TYPE);

  ds->iproc = ip;
  ds->ip_baton = baton;

  return ds;
}

static void deserialize_bytes(dfsch_deserializer_t* ds, char*buf, size_t len){
  ssize_t ret = ds->iproc(ds->ip_baton, buf, len);
  if (ret != len){
    dfsch_error("Unexpected end of serialized stream", NULL);
  }
}

static stream_symbol_t* deserialize_stream_symbol(dfsch_deserializer_t* ds){
  ssize_t len = dfsch_deserialize_integer(ds);
  stream_symbol_t* res;
  
  if (len > 0){
    res = GC_NEW(stream_symbol_t);
    res->name = GC_MALLOC_ATOMIC(len + 1);
    deserialize_bytes(ds, res->name, len);
    res->name[len] = 0;

    ds->sym_idx++;
    
    if (ds->sym_idx >= ds->sym_map_len){
      ds->sym_map_len *= 2;
      ds->sym_map = GC_REALLOC(ds->sym_map, ds->sym_map_len * sizeof(stream_symbol_t*));
    }
    
    ds->sym_map[ds->sym_idx] = res;
  } else {
    int idx = -len;

    if (idx > ds->sym_idx){
      dfsch_error("Invalid back reference in stream", ds);
    }

    res = ds->sym_map[idx];
  }
  return res;
}

dfsch_object_t* dfsch_deserialize_object(dfsch_deserializer_t* ds){
  stream_symbol_t* type = deserialize_stream_symbol(ds);
  update_handler_cache(type);

  if (!type->handler){
    if (ds->unknown){
      return ds->unknown(ds, type->name, ds->uh_baton);
    } else {
      dfsch_error("Unknown type ID", dfsch_make_string_cstr(type->name));
    }
  }
  
  return type->handler(ds);
}

void dfsch_deserializer_put_partial_object(dfsch_deserializer_t* ds,
                                           dfsch_object_t* obj){
    if (ds->obj_idx >= ds->obj_map_len){
      ds->obj_map_len *= 2;
      ds->obj_map = GC_REALLOC(ds->obj_map, ds->obj_map_len * sizeof(dfsch_object_t*));
    }
    ds->obj_map[ds->obj_idx] = obj;
    ds->obj_idx++;
}

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds){
  char lead;
  char buf[8];
  int64_t val = 0;
  deserialize_bytes(ds, &lead, 1);

  if ((lead & 0x80) == 0x00){
    val = lead;
    val = (val ^ (1ll << 6)) - (1ll << 6);
  } else if ((lead & 0xc0) == 0x80){
    deserialize_bytes(ds, buf, 1);
    val = ((((uint64_t)lead) & 0x3f) << 8);
    val |= ((((uint64_t)buf[0]) & 0xff) << 0);
    val = (val ^ (1ll << 13)) - (1ll << 13);
  } else if ((lead & 0xe0) == 0xc0){
    deserialize_bytes(ds, buf, 2);
    val = ((((uint64_t)lead) & 0x1f) << 16);
    val |= ((((uint64_t)buf[0]) & 0xff) << 8);
    val |= ((((uint64_t)buf[1]) & 0xff) << 0);
    val = (val ^ (1ll << 20)) - (1ll << 20);
  } else if ((lead & 0xf0) == 0xe0){
    deserialize_bytes(ds, buf, 3);
    val = ((((uint64_t)lead) & 0x0f) << 24);
    val |= ((((uint64_t)buf[0]) & 0xff) << 16);
    val |= ((((uint64_t)buf[1]) & 0xff) << 8);
    val |= ((((uint64_t)buf[2]) & 0xff) << 0);
    val = (val ^ (1ll << 27)) - (1ll << 27);
  } else if ((lead & 0xf8) == 0xf0){
    deserialize_bytes(ds, buf, 4);
    val = ((((uint64_t)lead) & 0x07) << 32);
    val |= ((((uint64_t)buf[0]) & 0xff) << 24);
    val |= ((((uint64_t)buf[1]) & 0xff) << 16);
    val |= ((((uint64_t)buf[2]) & 0xff) << 8);
    val |= ((((uint64_t)buf[3]) & 0xff) << 0);
    val = (val ^ (1ll << 34)) - (1ll << 34);
  } else if ((lead & 0xfc) == 0xf8){
    deserialize_bytes(ds, buf, 5);
    val = ((((uint64_t)lead) & 0x03) << 40);
    val |= ((((uint64_t)buf[0]) & 0xff) << 32);
    val |= ((((uint64_t)buf[1]) & 0xff) << 24);
    val |= ((((uint64_t)buf[2]) & 0xff) << 16);
    val |= ((((uint64_t)buf[3]) & 0xff) << 8);
    val |= ((((uint64_t)buf[4]) & 0xff) << 0);
    val = (val ^ (1ll << 41)) - (1ll << 41);
  } else if ((lead & 0xfe) == 0xfc){
    deserialize_bytes(ds, buf, 6);
    val = ((((uint64_t)lead) & 0x01) << 48);
    val |= ((((uint64_t)buf[0]) & 0xff) << 40);
    val |= ((((uint64_t)buf[1]) & 0xff) << 32);
    val |= ((((uint64_t)buf[2]) & 0xff) << 24);
    val |= ((((uint64_t)buf[3]) & 0xff) << 16);
    val |= ((((uint64_t)buf[4]) & 0xff) << 8);
    val |= ((((uint64_t)buf[5]) & 0xff) << 0);
    val = (val ^ (1ll << 48)) - (1ll << 48);
  } else if ((lead & 0xfe) == 0xfc){
    deserialize_bytes(ds, buf, 7);
    val |= ((((uint64_t)buf[0]) & 0xff) << 48);
    val |= ((((uint64_t)buf[1]) & 0xff) << 40);
    val |= ((((uint64_t)buf[2]) & 0xff) << 32);
    val |= ((((uint64_t)buf[3]) & 0xff) << 24);
    val |= ((((uint64_t)buf[4]) & 0xff) << 16);
    val |= ((((uint64_t)buf[5]) & 0xff) << 8);
    val |= ((((uint64_t)buf[6]) & 0xff) << 0);
    val = (val ^ (1ll << 55)) - (1ll << 55);
  } else if (lead  == 0xff){
    deserialize_bytes(ds, buf, 8);
    val |= ((((uint64_t)buf[0]) & 0xff) << 56);
    val |= ((((uint64_t)buf[1]) & 0xff) << 48);
    val |= ((((uint64_t)buf[2]) & 0xff) << 40);
    val |= ((((uint64_t)buf[3]) & 0xff) << 32);
    val |= ((((uint64_t)buf[4]) & 0xff) << 24);
    val |= ((((uint64_t)buf[5]) & 0xff) << 16);
    val |= ((((uint64_t)buf[6]) & 0xff) << 8);
    val |= ((((uint64_t)buf[7]) & 0xff) << 0);
  }

  return val;
}
dfsch_strbuf_t* dfsch_deserialize_strbuf(dfsch_deserializer_t* ds){
  dfsch_strbuf_t* s = GC_NEW(dfsch_strbuf_t);

  s->len = dfsch_deserialize_integer(ds);
  s->ptr = GC_MALLOC_ATOMIC(s->len + 1);
  deserialize_bytes(ds, s->ptr, s->len);
  s->ptr[s->len] = 0;
  
  return s;
}

char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds){
  return deserialize_stream_symbol(ds)->name;
}


void dfsch_register_deserializer_handler(char* name,
                                         dfsch_deserializer_handler_t h){
  dfsch_strhash_set(get_deshandler_map(), name, (void*)h);
}

static dfsch_object_t* back_reference_handler(dfsch_deserializer_t* ds){
  int ref = dfsch_deserialize_integer(ds);
  
}
static dfsch_object_t* persistent_id_handler(dfsch_deserializer_t* ds){
  char* name = dfsch_deserialize_stream_symbol(ds);
  if (ds->persistent){
    return ds->persistent(ds, name, ds->ph_baton);
  } else {
    dfsch_error("Persistent object IDs not supported", ds);
  }
}
static dfsch_object_t* fixnum_handler(dfsch_deserializer_t* ds){
  int64_t u = dfsch_deserialize_integer(ds);
  dfsch_object_t* v = dfsch_make_number_from_int64(u);
  dfsch_deserializer_put_partial_object(ds, v);
  return v;
}

static void register_core_handlers(){
  dfsch_register_deserializer_handler("back-refrence",
                                      back_reference_handler);
  dfsch_register_deserializer_handler("persistent-id",
                                      persistent_id_handler);
  dfsch_register_deserializer_handler("fixnum",
                                      fixnum_handler);
}


DFSCH_DEFINE_PRIMITIVE(serialize_to_byte_vector,
                       "Serializes one object into byte_vector"){
  dfsch_object_t* obj;
  dfsch_serializer_t* ser;
  str_list_t* sl = sl_create();
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_ARG_END(args);

  ser = dfsch_make_serializer(sl_nappend, sl);
  dfsch_serialize_object(ser, obj);
  return dfsch_make_byte_vector_strbuf(dfsch_sl_value_strbuf(sl));
}

DFSCH_DEFINE_PRIMITIVE(deserialize_from_string,
                       "Deserialize one object from string"){
  dfsch_strbuf_t* string;
  dfsch_deserializer_t* ds;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  ds = dfsch_make_deserializer(dfsch_strbuf_inputproc,
                               dfsch_copy_strbuf(string));
  return dfsch_deserialize_object(ds);
}

void dfsch__serdes_register(dfsch_object_t* env){
  dfsch_defconst_cstr(env, "serialize-to-byte-vector",
                      DFSCH_PRIMITIVE_REF(serialize_to_byte_vector));
  dfsch_defconst_cstr(env, "deserialize-from-string",
                      DFSCH_PRIMITIVE_REF(deserialize_from_string));
}
