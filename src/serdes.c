#include <dfsch/serdes.h>
#include <dfsch/eqhash.h>
#include <dfsch/strhash.h>
#include <dfsch/ports.h>

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

  dfsch_serializer_object_hook_t object_hook;
  void* oh_baton;

  dfsch_serializer_unserializable_hook_t unserializable;
  void* uh_baton;

  dfsch_object_t* canon_env;
  int compress;
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
  s->compress = 0;

  return s;
}
dfsch_serializer_t* dfsch_serializer(dfsch_object_t* obj){
  return DFSCH_ASSERT_INSTANCE(obj, DFSCH_SERIALIZER_TYPE);
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
void dfsch_serializer_set_canonical_environment(dfsch_serializer_t* s,
                                                dfsch_object_t* env){
  s->canon_env = env;
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

#define DSS_MAGIC            "dSs1"
#define DSS_FLAG_CANON       1

#define DSS_UNKNOWN_FLAGS    (~1)

#define COMPRESS_CUTOFF      512

void dfsch_serializer_write_stream_header(dfsch_serializer_t* s,
                                          char* format){
  int flags = 0;
  serialize_bytes(s, DSS_MAGIC, 4);
  if (s->canon_env){
    flags |= DSS_FLAG_CANON;
  }
  dfsch_serialize_integer(s, flags);
  if (format){
    serialize_bytes(s, format, strlen(format));
  }
}


void dfsch_put_serialized_object(dfsch_serializer_t* s,
                                 dfsch_object_t* obj){
  dfsch_eqhash_set(&s->obj_map, obj, (dfsch_object_t*)(s->obj_idx));
  s->obj_idx++;
}

int dfsch_type_serializable_p(dfsch_type_t* type){
  while (type){
    if (type->serialize){
      return 1;
    }
    type = type->superclass;
  }
  
  return 0;
}

void dfsch_serialize_invalid_object(dfsch_serializer_t* s){
  dfsch_serialize_stream_symbol(s, "invalid-object");
}

void dfsch_serialize_object(dfsch_serializer_t* s,
                            dfsch_object_t* obj){
  dfsch_type_t* klass;
  if (obj && !DFSCH_FIXNUM_P(obj) && !DFSCH_CHARACTER_P(obj)){
    dfsch_object_t* idx = (int)dfsch_eqhash_ref(&s->obj_map, obj);  
    if (idx != DFSCH_INVALID_OBJECT){
      serialize_back_reference(s, ((int)idx));
      return;
    }  
    
    dfsch_put_serialized_object(s, obj);
  }

  if (s->canon_env){
    dfsch_object_t* sym = dfsch_env_revscan(s->canon_env, obj, 1);
    if (sym != DFSCH_INVALID_OBJECT && DFSCH_SYMBOL_P(sym)){
      dfsch_package_t* package = dfsch_symbol_package(sym);
      char* name = dfsch_symbol(sym);
      if (package && name){
        dfsch_serialize_stream_symbol(s, "canon-env-ref");
        dfsch_serialize_stream_symbol(s, dfsch_package_name(package));
        dfsch_serialize_cstr(s, name);
        return;
      }
    }
  }

  if (s->object_hook){
    if (s->object_hook(s, obj, s->oh_baton)){
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
    serialize_bytes(s, buf, 9);    
  }
}
void dfsch_serialize_string(dfsch_serializer_t* s,
                            char* str, size_t len){
  if (len > COMPRESS_CUTOFF){
    char* compressed_buffer = GC_MALLOC_ATOMIC(len + (len / 19));
    int compressed = dfsch__fastlz_compress(str, len, compressed_buffer);

    if ((compressed + 8) < len){
      dfsch_serialize_integer(s, -len);
      dfsch_serialize_integer(s, compressed);
      serialize_bytes(s, compressed_buffer, compressed);
      return;
    }
  }

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
  int idx;

  if (!sym || *sym == '\0'){
    dfsch_serialize_integer(s, 0);
    return;
  }

  idx = (int)dfsch_strhash_ref(&s->sym_map, sym);
    
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

  dfsch_object_t* canon_env;
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
    dfsch_strhash_init_sa(&h);
    init = 1;
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

  ds->sym_map = GC_MALLOC(sizeof(stream_symbol_t*)*256);
  ds->sym_map_len = 256;

  ds->obj_map = GC_MALLOC(sizeof(dfsch_object_t*)*256);
  ds->obj_map_len = 256;

  return ds;
}
dfsch_deserializer_t* dfsch_deserializer(dfsch_object_t* obj){
  return DFSCH_ASSERT_INSTANCE(obj, DFSCH_DESERIALIZER_TYPE);
}

void dfsch_deserializer_set_canonical_environment(dfsch_deserializer_t* ds,
                                                  dfsch_object_t* env){
  ds->canon_env = env;
}


static void deserialize_bytes(dfsch_deserializer_t* ds, char*buf, size_t len){
  ssize_t ret = ds->iproc(ds->ip_baton, buf, len);
  if (ret != len){
    dfsch_error("Unexpected end of serialized stream", NULL);
  }
}

void dfsch_deserializer_read_stream_header(dfsch_deserializer_t* ds,
                                           char* fmt){
  char buf[4];
  int flags;
  deserialize_bytes(ds, buf, 4);
  if (memcmp(buf, DSS_MAGIC, 4) != 0){
    dfsch_error("Invalid stream header", ds);
  }
  flags = dfsch_deserialize_integer(ds);
  if (flags & DSS_FLAG_CANON){
    if (!ds->canon_env){
      dfsch_error("Stream requires canonical environment", ds);
    }
  }
  if (flags & DSS_UNKNOWN_FLAGS){
    dfsch_error("Incompatible stream requirements", ds); 
  }
  if (fmt){
    size_t len = strlen(fmt);
    char fbuf[len];
    deserialize_bytes(ds, fbuf, len);
    if (memcmp(fbuf, fmt, len) != 0){
      dfsch_error("Stream uses incompatible extended format", ds);
    }
  }
}


static stream_symbol_t* deserialize_stream_symbol(dfsch_deserializer_t* ds){
  ssize_t len = dfsch_deserialize_integer(ds);
  stream_symbol_t* res;
  
  if (len == 0){
    return NULL;
  } else if (len > 0){
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

  if (!type){
    return NULL;
  }

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

dfsch_object_t** dfsch_deserializer__skip_object(dfsch_deserializer_t* ds){
  if (ds->obj_idx >= ds->obj_map_len){
    ds->obj_map_len *= 2;
    ds->obj_map = GC_REALLOC(ds->obj_map, ds->obj_map_len * sizeof(dfsch_object_t*));
  }
  ds->obj_idx++;
  return ds->obj_map + ds->obj_idx - 1;
}

void dfsch_deserializer_put_partial_object(dfsch_deserializer_t* ds,
                                           dfsch_object_t* obj){
  *(dfsch_deserializer__skip_object(ds)) = obj;
}

int64_t dfsch_deserialize_integer(dfsch_deserializer_t* ds){
  unsigned char lead;
  unsigned char buf[8];
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
  } else if (lead == 0xfe){
    deserialize_bytes(ds, buf, 7);
    val = ((((uint64_t)buf[0]) & 0xff) << 48);
    val |= ((((uint64_t)buf[1]) & 0xff) << 40);
    val |= ((((uint64_t)buf[2]) & 0xff) << 32);
    val |= ((((uint64_t)buf[3]) & 0xff) << 24);
    val |= ((((uint64_t)buf[4]) & 0xff) << 16);
    val |= ((((uint64_t)buf[5]) & 0xff) << 8);
    val |= ((((uint64_t)buf[6]) & 0xff) << 0);
    val = (val ^ (1ll << 55)) - (1ll << 55);
  } else if (lead  == 0xff){
    deserialize_bytes(ds, buf, 8);
    val = ((((uint64_t)buf[0]) & 0xff) << 56);
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
  long len;
  int ret;

  len = dfsch_deserialize_integer(ds);
  if (len < 0){
    char* buf;
    s->len = -len;
    s->ptr = GC_MALLOC_ATOMIC(s->len + 1);
    
    len = dfsch_deserialize_integer(ds);
    buf = GC_MALLOC_ATOMIC(len);
    
    deserialize_bytes(ds, buf, len);
    
    ret = dfsch__fastlz_decompress(buf, len, s->ptr, s->len);
    if (ret != s->len){
      dfsch_error("Invalid serialized stream: decompression error", ds);
    }
  } else {
    s->len = len;
    s->ptr = GC_MALLOC_ATOMIC(s->len + 1);

    deserialize_bytes(ds, s->ptr, s->len);
  }

  s->ptr[s->len] = 0;

  return s;
}

char* dfsch_deserialize_stream_symbol(dfsch_deserializer_t* ds){
  stream_symbol_t* ret = deserialize_stream_symbol(ds);
  if (ret){
    return ret->name;
  } else {
    return NULL;
  }
}


void dfsch_register_deserializer_handler(char* name,
                                         dfsch_deserializer_handler_t h){
  dfsch_strhash_set_sa(get_deshandler_map(), name, (void*)h);
}

static dfsch_object_t* back_reference_handler(dfsch_deserializer_t* ds){
  int ref = dfsch_deserialize_integer(ds);
  dfsch_object_t* res;
  if (ref < 0){
    dfsch_error("Not supported", NULL);
  }
  if (ref > ds->obj_idx){
    dfsch_error("Invalid back reference in stream", ds);
  }
  res = ds->obj_map[ref];
  return res;
}
static dfsch_object_t* fixnum_handler(dfsch_deserializer_t* ds){
  int64_t u = dfsch_deserialize_integer(ds);
  dfsch_object_t* v = dfsch_make_number_from_int64(u);
  return v;
}
static dfsch_object_t* invalid_object_handler(dfsch_deserializer_t* ds){
  return DFSCH_INVALID_OBJECT;
}
static dfsch_object_t* canon_env_ref_handler(dfsch_deserializer_t* ds){
  char* package = dfsch_deserialize_stream_symbol(ds);
  char* name = dfsch_deserialize_strbuf(ds)->ptr;
  dfsch_object_t* sym;
  dfsch_object_t* obj;
  if (package && name){
    sym = dfsch_intern_symbol(dfsch_make_package(package, NULL), 
                              name);
  } else {
    dfsch_error("Invalid serialized stream: dereferencing gensym", NULL);
  }
  
  if (!ds->canon_env){
    dfsch_error("No canonical environament specified for deserialization", 
                NULL);
  }

  obj = dfsch_lookup(sym, ds->canon_env);

  dfsch_deserializer_put_partial_object(ds, obj);
  return obj;
}


static void __attribute__((constructor)) register_core_handlers(){
  dfsch_register_deserializer_handler("back-reference",
                                      back_reference_handler);
  dfsch_register_deserializer_handler("fixnum",
                                      fixnum_handler);
  dfsch_register_deserializer_handler("canon-env-ref",
                                      canon_env_ref_handler);
  dfsch_register_deserializer_handler("invalid-object",
                                      invalid_object_handler);
}

dfsch_strbuf_t* dfsch_serialize(dfsch_object_t* obj, 
                                dfsch_object_t* canon_env,
                                int flags){
  dfsch_serializer_t* ser;
  str_list_t* sl = sl_create();
  ser = dfsch_make_serializer(sl_nappend, sl);
  if (canon_env){
    dfsch_serializer_set_canonical_environment(ser, canon_env);
  }

  dfsch_serialize_object(ser, obj);
  return dfsch_sl_value_strbuf(sl);
}
dfsch_object_t* dfsch_deserialize(dfsch_strbuf_t* sb,
                                  dfsch_object_t* canon_env){
  dfsch_deserializer_t* ds;
  ds = dfsch_make_deserializer(dfsch_strbuf_inputproc,
                               dfsch_copy_strbuf(sb));
  if (canon_env){
    dfsch_deserializer_set_canonical_environment(ds, canon_env);
  }
  return dfsch_deserialize_object(ds);
}

typedef struct smap_t {
  dfsch_type_t* type;
  dfsch_object_t* mapping;
  dfsch_object_t* canon_env;
  int flags;
} smap_t;

static dfsch_object_t* smap_ref(smap_t* sm, 
                                dfsch_object_t* key){
  dfsch_object_t* val = dfsch_mapping_ref(sm->mapping,
                                          key);
  if (val == DFSCH_INVALID_OBJECT){
    return DFSCH_INVALID_OBJECT;
  }
  return dfsch_deserialize(dfsch_string_to_buf(val),
                           sm->canon_env);
}
static void smap_set(smap_t* sm, 
                     dfsch_object_t* key,
                     dfsch_object_t* value){
  dfsch_strbuf_t* sb = dfsch_serialize(value,
                                       sm->canon_env,
                                       sm->flags);
  dfsch_mapping_set(sm->mapping,
                    key,
                    dfsch_make_byte_vector_nocopy(sb->ptr, sb->len));
}

static void smap_get_iterator(smap_t* sm){
  return dfsch_collection_get_iterator(sm->mapping);
}


static dfsch_mapping_methods_t smap_mapping = {
  .ref = smap_ref,
  .set = smap_set,
};

static dfsch_collection_methods_t smap_collection = {
  .get_iterator = smap_get_iterator,
};

dfsch_type_t dfsch_serializing_map_type = {
  .type = DFSCH_STANDARD_TYPE,
  .size = sizeof(smap_t),
  .name = "serializing-map",
  .collection = &smap_collection,
  .mapping = &smap_mapping,
};

dfsch_object_t* dfsch_make_serializing_map(dfsch_object_t* mapping,
                                           dfsch_object_t* canon_env,
                                           int flags){
  smap_t* sm = dfsch_make_object(DFSCH_SERIALIZING_MAP_TYPE);
  sm->mapping = mapping;
  sm->canon_env = canon_env;
  sm->flags = flags;
  return sm;
}



DFSCH_DEFINE_PRIMITIVE(serialize,
                       "Serializes one object into byte_vector"){
  dfsch_object_t* obj;
  dfsch_object_t* canon_env;
  int flags = 0;
  DFSCH_OBJECT_ARG(args, obj);
  DFSCH_OBJECT_ARG_OPT(args, canon_env, NULL);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_ARG_END(args);

  return dfsch_make_byte_vector_strbuf(dfsch_serialize(obj, canon_env, flags));
}

DFSCH_DEFINE_PRIMITIVE(deserialize,
                       "Deserialize one object from string"){
  dfsch_strbuf_t* string;
  dfsch_object_t* canon_env;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_OBJECT_ARG_OPT(args, canon_env, NULL);
  DFSCH_ARG_END(args);

  return dfsch_deserialize(string, canon_env);
}

DFSCH_DEFINE_PRIMITIVE(make_serializing_map,
                       "Make mapping objects that transparently serializes "
                       "it's values"){
  dfsch_object_t* mapping;
  dfsch_object_t* canon_env;
  int flags = 0;
  DFSCH_OBJECT_ARG(args, mapping);
  DFSCH_OBJECT_ARG_OPT(args, canon_env, NULL);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_make_serializing_map(mapping, canon_env, flags);
}


DFSCH_DEFINE_PRIMITIVE(make_serializer,
                       "Creates new serializer object writing into supplied "
                       "port"){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);

  return dfsch_make_serializer(dfsch_port_write_buf, port);
}

DFSCH_DEFINE_PRIMITIVE(serializer_set_canonical_environment,
                       "Sets cannonical environment used by serializer"){
  dfsch_serializer_t* serializer;
  dfsch_object_t* env;
  DFSCH_SERIALIZER_ARG(args, serializer);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);
  
  dfsch_serializer_set_canonical_environment(serializer, env);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(serialize_object,
                       "Serialize object into serialization stream"){
  dfsch_serializer_t* serializer;
  dfsch_object_t* object;
  DFSCH_SERIALIZER_ARG(args, serializer);
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  dfsch_serialize_object(serializer, object);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(serialize_stream_symbol,
                       "Write stream symbol (string marker) into "
                       "serialization stream"){
  dfsch_serializer_t* serializer;
  char* sym;
  DFSCH_SERIALIZER_ARG(args, serializer);
  DFSCH_STRING_OR_SYMBOL_ARG(args, sym);
  DFSCH_ARG_END(args);

  dfsch_serialize_stream_symbol(serializer, sym);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(serialize_bytes,
                       "Write raw binary data into serialization stream"){
  dfsch_serializer_t* serializer;
  dfsch_strbuf_t* string;
  DFSCH_SERIALIZER_ARG(args, serializer);
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);

  dfsch_serialize_strbuf(serializer, string);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(serialize_integer,
                       "Write raw binary data into serialization stream"){
  dfsch_serializer_t* serializer;
  int64_t number;
  DFSCH_SERIALIZER_ARG(args, serializer);
  DFSCH_INT64_ARG(args, number);
  DFSCH_ARG_END(args);

  dfsch_serialize_integer(serializer, number);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(serializer_write_stream_header,
                       "Write (optional) header of serialized stream"){
  dfsch_serializer_t* serializer;
  char* format;
  DFSCH_SERIALIZER_ARG(args, serializer);
  DFSCH_STRING_OR_SYMBOL_ARG(args, format);
  DFSCH_ARG_END(args);

  dfsch_serializer_write_stream_header(serializer, format);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(make_deserializer,
                       "Creates new deserializer object reading from supplied "
                       "port"){
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  DFSCH_ARG_END(args);

  return dfsch_make_deserializer(dfsch_port_read_buf, port);
}

DFSCH_DEFINE_PRIMITIVE(deserializer_set_canonical_environment,
                       "Sets cannonical environment used by deserializer"){
  dfsch_deserializer_t* deserializer;
  dfsch_object_t* env;
  DFSCH_DESERIALIZER_ARG(args, deserializer);
  DFSCH_OBJECT_ARG(args, env);
  DFSCH_ARG_END(args);
  
  dfsch_deserializer_set_canonical_environment(deserializer, env);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(deserializer_read_stream_header,
                       "Read (optional) header of serialized stream"){
  dfsch_deserializer_t* deserializer;
  char* format;
  DFSCH_DESERIALIZER_ARG(args, deserializer);
  DFSCH_STRING_OR_SYMBOL_ARG(args, format);
  DFSCH_ARG_END(args);

  dfsch_deserializer_read_stream_header(deserializer, format);

  return NULL;
}


DFSCH_DEFINE_PRIMITIVE(deserialize_object,
                       "Read one object from serialized stream"){
  dfsch_deserializer_t* deserializer;
  DFSCH_DESERIALIZER_ARG(args, deserializer);
  DFSCH_ARG_END(args);

  return dfsch_deserialize_object(deserializer);
}
DFSCH_DEFINE_PRIMITIVE(deserialize_stream_symbol,
                       "Read stream symbol from serialized stream"){
  dfsch_deserializer_t* deserializer;
  DFSCH_DESERIALIZER_ARG(args, deserializer);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_deserialize_stream_symbol(deserializer));
}
DFSCH_DEFINE_PRIMITIVE(deserialize_bytes,
                       "Read raw bytes from serialized stream"){
  dfsch_deserializer_t* deserializer;
  DFSCH_DESERIALIZER_ARG(args, deserializer);
  DFSCH_ARG_END(args);

  return dfsch_make_byte_vector_strbuf(dfsch_deserialize_strbuf(deserializer));
}
DFSCH_DEFINE_PRIMITIVE(deserialize_integer,
                       "Read integer from serialized stream"){
  dfsch_deserializer_t* deserializer;
  DFSCH_DESERIALIZER_ARG(args, deserializer);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_int64(dfsch_deserialize_integer(deserializer));
}

void dfsch__serdes_register(dfsch_object_t* env){
  dfsch_defcanon_cstr(env, "<serializer>", DFSCH_SERIALIZER_TYPE);
  dfsch_defcanon_cstr(env, "<deserializer>", DFSCH_DESERIALIZER_TYPE);

  dfsch_defcanon_cstr(env, "<serializing-map>", 
                      DFSCH_SERIALIZING_MAP_TYPE);  

  dfsch_defcanon_cstr(env, "serialize",
                      DFSCH_PRIMITIVE_REF(serialize));
  dfsch_defcanon_cstr(env, "deserialize",
                      DFSCH_PRIMITIVE_REF(deserialize));
  dfsch_defcanon_cstr(env, "make-serializing-map",
                      DFSCH_PRIMITIVE_REF(make_serializing_map));

  dfsch_defcanon_cstr(env, "make-serializer",
                      DFSCH_PRIMITIVE_REF(make_serializer));
  dfsch_defcanon_cstr(env, "serializer-set-canonical-environment!",
                      DFSCH_PRIMITIVE_REF(serializer_set_canonical_environment
                                          ));

  dfsch_defcanon_cstr(env, "serializer-write-stream-header!",
                      DFSCH_PRIMITIVE_REF(serializer_write_stream_header));
  dfsch_defcanon_cstr(env, "serialize-object!",
                      DFSCH_PRIMITIVE_REF(serialize_object));
  dfsch_defcanon_cstr(env, "serialize-bytes!",
                      DFSCH_PRIMITIVE_REF(serialize_bytes));
  dfsch_defcanon_cstr(env, "serialize-stream-symbol!",
                      DFSCH_PRIMITIVE_REF(serialize_stream_symbol));
  dfsch_defcanon_cstr(env, "serialize-integer!",
                      DFSCH_PRIMITIVE_REF(serialize_integer));

  dfsch_defcanon_cstr(env, "make-deserializer",
                      DFSCH_PRIMITIVE_REF(make_serializer));
  dfsch_defcanon_cstr(env, "deserializer-set-canonical-environment!",
                      DFSCH_PRIMITIVE_REF(deserializer_set_canonical_environment
                                          ));
  
  dfsch_defcanon_cstr(env, "deserializer-read-stream-header!",
                      DFSCH_PRIMITIVE_REF(deserializer_read_stream_header));
  dfsch_defcanon_cstr(env, "deserialize-object!",
                      DFSCH_PRIMITIVE_REF(deserialize_object));
  dfsch_defcanon_cstr(env, "deserialize-stream-symbol!",
                      DFSCH_PRIMITIVE_REF(deserialize_stream_symbol));
  dfsch_defcanon_cstr(env, "deserialize-integer!",
                      DFSCH_PRIMITIVE_REF(deserialize_integer));
  dfsch_defcanon_cstr(env, "deserialize-bytes!",
                      DFSCH_PRIMITIVE_REF(deserialize_bytes));
  
}
