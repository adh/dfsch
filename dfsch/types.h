/*
 * dfsch - Scheme-like Lisp dialect
 *   Basic types.
 * Copyright (C) 2005-2010 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#ifndef H__dfsch__types__
#define H__dfsch__types__

/*
 * Ugly hack:
 *
 * On non-gcc compilers one can effectively force alignment to be 8 bytes by
 * inserting dummy value of any type that requires 8-byte alignment into 
 * struct. double seems like something that requires 8-byte alignment.
 */

#if __SIZEOF_POINTER__ == 8
# define DFSCH_ALIGN8_ATTR  
# define DFSCH_ALIGN8_DUMMY 
#else
# ifdef __GNUC__
#  define DFSCH_ALIGN8_ATTR  __attribute__ ((__aligned__(8)))
#  define DFSCH_ALIGN8_DUMMY 
# else
#  define DFSCH_ALIGN8_ATTR  
#  define DFSCH_ALIGN8_DUMMY double alignment_dummy_value;
# endif
#endif

#include <stddef.h>
#include <stdlib.h>

extern dfsch_type_t dfsch_abstract_type;
#define DFSCH_ABSTRACT_TYPE ((dfsch_type_t*)&dfsch_abstract_type)
extern dfsch_type_t dfsch_meta_type;
#define DFSCH_META_TYPE ((dfsch_type_t*)&dfsch_meta_type)
extern dfsch_type_t dfsch_special_type;
#define DFSCH_SPECIAL_TYPE ((dfsch_type_t*)&dfsch_special_type)
extern dfsch_type_t dfsch_standard_type;
#define DFSCH_STANDARD_TYPE ((dfsch_type_t*)&dfsch_standard_type)
extern dfsch_type_t dfsch_list_type;
#define DFSCH_LIST_TYPE ((dfsch_type_t*)&dfsch_list_type)
extern dfsch_type_t dfsch_standard_function_type;
#define DFSCH_STANDARD_FUNCTION_TYPE                    \
  ((dfsch_type_t*)&dfsch_standard_function_type)
extern dfsch_type_t dfsch_empty_list_type;
#define DFSCH_EMPTY_LIST_TYPE ((dfsch_type_t*)&dfsch_empty_list_type)

extern dfsch_type_t dfsch_function_type;
#define DFSCH_FUNCTION_TYPE (&dfsch_function_type)
extern dfsch_type_t dfsch_macro_type;
#define DFSCH_MACRO_TYPE (&dfsch_macro_type)
extern dfsch_type_t dfsch_vector_type;
#define DFSCH_VECTOR_TYPE (&dfsch_vector_type)
extern dfsch_type_t dfsch_environment_type;
#define DFSCH_ENVIRONMENT_TYPE (&dfsch_environment_type)
extern dfsch_type_t dfsch_lambda_list_type;
#define DFSCH_LAMBDA_LIST_TYPE (&dfsch_lambda_list_type)
extern dfsch_type_t dfsch_writer_state_type;
#define DFSCH_WRITER_STATE_TYPE (&dfsch_writer_state_type)

extern dfsch_type_t dfsch_slot_accessor_type;
#define DFSCH_SLOT_ACCESSOR_TYPE (&dfsch_slot_accessor_type)
extern dfsch_type_t dfsch_slot_reader_type;
#define DFSCH_SLOT_READER_TYPE (&dfsch_slot_reader_type)
extern dfsch_type_t dfsch_slot_writer_type;
#define DFSCH_SLOT_WRITER_TYPE (&dfsch_slot_writer_type)

extern dfsch_type_t dfsch_package_type;
#define DFSCH_PACKAGE_TYPE (&dfsch_package_type)

extern dfsch_type_t dfsch_invalid_object_type;
#define DFSCH_INVALID_OBJECT_TYPE (&dfsch_invalid_object_type)

typedef struct dfsch_primitive_t {
  dfsch_type_t* type;
  dfsch_primitive_impl_t proc;
  void *baton;
  int flags;
  char* name;
  char* documentation;
  char* synopsis;

  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_primitive_t;

#define DFSCH_PRIMITIVE_PURE 1

extern dfsch_type_t dfsch_primitive_type;

#define DFSCH_DOC_SYNOPSIS(list) , .synopsis = list

#define DFSCH_PRIMITIVE_TYPE (&dfsch_primitive_type)

#define DFSCH_DECLARE_PRIMITIVE(name, documentation...) \
  static dfsch_primitive_t p_##name = {                 \
    DFSCH_PRIMITIVE_TYPE,                               \
    p_##name##_impl,                                    \
    NULL,                                               \
    0,                                                  \
    #name,                                              \
    DFSCH_DOC_STRING(documentation)                     \
  }
  
#define DFSCH_DECLARE_PRIMITIVE_EX(name, documentation, flags)		\
  static dfsch_primitive_t p_##name = {                                 \
    DFSCH_PRIMITIVE_TYPE,                                               \
    p_##name##_impl,                                                    \
    NULL,								\
    flags,                                                              \
    #name,                                                              \
    DFSCH_DOC_STRING(documentation)                                     \
  }

#define DFSCH_PRIMITIVE_HEAD(name)                                      \
  static dfsch_object_t* p_##name##_impl(void* baton,                   \
                                         dfsch_object_t* args,          \
                                         dfsch_tail_escape_t* esc,      \
                                         dfsch_object_t* context)
  
#define DFSCH_DEFINE_PRIMITIVE(name, documentation)     \
  DFSCH_PRIMITIVE_HEAD(name);                           \
  DFSCH_DECLARE_PRIMITIVE(name, documentation);         \
  DFSCH_PRIMITIVE_HEAD(name)

#define DFSCH_DEFINE_PRIMITIVE_EX(name, documentation, flags)	\
  DFSCH_PRIMITIVE_HEAD(name);					\
  DFSCH_DECLARE_PRIMITIVE_EX(name, documentation, flags);       \
  DFSCH_PRIMITIVE_HEAD(name)

#define DFSCH_PRIMITIVE_REF(name) ((dfsch_object_t*)&p_##name)
#define DFSCH_PRIMITIVE_REF_MAKE(name, baton, doc)                      \
  dfsch_make_primitive(#name, p_##name##_impl, (baton), doc, 0)

typedef struct dfsch_macro_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_macro_t;

#define DFSCH_DEFINE_MACRO(name, documentation)         \
  DFSCH_PRIMITIVE_HEAD(macro_##name);                   \
  DFSCH_DECLARE_PRIMITIVE(macro_##name, documentation); \
  static dfsch_macro_t macro_##name = {                 \
    DFSCH_MACRO_TYPE,                                   \
    DFSCH_PRIMITIVE_REF(macro_##name)                   \
  };                                                    \
  DFSCH_PRIMITIVE_HEAD(macro_##name)
   
#define DFSCH_MACRO_REF(name) ((dfsch_object_t*)&macro_##name)




typedef struct dfsch_form_t dfsch_form_t;

/* methods used by compiler, in different struct for easier expansion */
typedef struct dfsch_form_methods_t {
  dfsch_object_t* (*compile)(dfsch_form_t* form, 
                             dfsch_object_t* expr,
                             dfsch_object_t* env);
  int compile_time_eval;
} dfsch_form_methods_t;

typedef dfsch_object_t* (*dfsch_form_impl_t)(dfsch_form_t* form,
                                             dfsch_object_t* env,
                                             dfsch_object_t* args,
                                             dfsch_tail_escape_t* esc);
struct dfsch_form_t {
  dfsch_type_t* type;
  dfsch_form_impl_t impl;
  void* baton;
  char* name;
  char* documentation;
  char* synopsis;

  dfsch_form_methods_t methods;

  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR;

extern dfsch_type_t dfsch_form_type;

#define DFSCH_FORM_TYPE (&dfsch_form_type)
  
#define DFSCH_FORM_IMPLEMENTATION(name)                                 \
  static dfsch_object_t* form_##name##_impl(dfsch_form_t* form,         \
                                            dfsch_object_t* env,        \
                                            dfsch_object_t* args,       \
                                            dfsch_tail_escape_t* esc)

#define DFSCH_FORM_METHOD_COMPILE(name)   \
  static dfsch_object_t* form_##name##_compile                    \
  (dfsch_form_t* form,                                                  \
   dfsch_object_t* expr,                                                \
   dfsch_object_t* env)

#define DFSCH_FORM_COMPILE(name)                  \
  .compile = form_##name##_compile

#define DFSCH_FORM_COMPILE_TIME                 \
  .compile_time_eval = 1

#define DFSCH_DEFINE_FORM(name, meths, doc...)                  \
  DFSCH_FORM_IMPLEMENTATION(name);                              \
  static dfsch_form_t form_##name = {                           \
    DFSCH_FORM_TYPE,                                            \
    form_##name##_impl,                                         \
    NULL,                                                       \
    #name,                                                      \
    .methods = meths,                                           \
    .documentation = DFSCH_DOC_STRING(doc),                     \
  };                                                            \
  DFSCH_FORM_IMPLEMENTATION(name)

/* 
 * Environment argument passed to forms should not be directly accessed. 
 * By default, environments are not scavenged by GC, but directly reused 
 * when they come out of scope, this dfsch_reify_environment() disables 
 * this behavior.
 */
#define DFSCH_FORM_ENVIRONMENT (dfsch_reify_environemnt(env))

#define DFSCH_FORM_REF(name) ((dfsch_object_t*)&form_##name)

#define DFSCH_MAKE_FORM(name,baton)             \
  (dfsch_make_form(form_##name##_impl,          \
                   (baton),                     \
                   #name))

/** Equivalence metod prototype */
typedef int (*dfsch_type_equal_p_t)(dfsch_object_t*, dfsch_object_t*);

typedef void (*dfsch_output_proc_t)(void* baton, char* buf, size_t len);
typedef ssize_t (*dfsch_input_proc_t)(void* baton, char* buf, size_t len);
typedef struct dfsch_writer_state_t dfsch_writer_state_t;

/** Write / Display method prototype */
typedef void (*dfsch_type_write_t)(dfsch_object_t* obj,
                                   dfsch_writer_state_t* state);
/** Apply metod prototype */
typedef dfsch_object_t* (*dfsch_type_apply_t)(dfsch_object_t* object, 
                                              dfsch_object_t* args,
                                              dfsch_tail_escape_t* esc,
                                              dfsch_object_t* context);
/** Hash method prototype */
typedef uint32_t (*dfsch_type_hash_t)(dfsch_object_t* obj);

typedef dfsch_object_t* (*dfsch_type_describe_t)(dfsch_object_t* object);

typedef struct dfsch_serializer_t dfsch_serializer_t;
typedef void (*dfsch_type_serialize_t)(dfsch_object_t* obj,
                                       dfsch_serializer_t* s);



/** Disable weak references for this type */
#define DFSCH_TYPEF_NO_WEAK_REFERENCES 1
/** Allow user code to inherit from this type */
#define DFSCH_TYPEF_USER_EXTENSIBLE    2
/** Instances does not get backreferences in serialization and circular 
 *  prints */
#define DFSCH_TYPEF_NO_BACK_REFERENCES 4

typedef dfsch_object_t* (*dfsch_collection_get_iterator_t)(dfsch_object_t* c);

typedef dfsch_object_t* (*dfsch_collection_make_constructor_t)(dfsch_type_t* t);

typedef struct dfsch_collection_methods_t {
  dfsch_collection_get_iterator_t get_iterator;
  dfsch_collection_make_constructor_t make_constructor;
} dfsch_collection_methods_t;


typedef dfsch_object_t* (*dfsch_sequence_ref_t)(dfsch_object_t* s,
                                                int n);
typedef void (*dfsch_sequence_set_t)(dfsch_object_t* s,
                                     int n,
                                     dfsch_object_t* val);
typedef size_t (*dfsch_sequence_length_t)(dfsch_object_t* s);

typedef struct dfsch_sequence_methods_t {
  dfsch_sequence_ref_t ref;
  dfsch_sequence_set_t set;
  dfsch_sequence_length_t length;
} dfsch_sequence_methods_t;

typedef dfsch_object_t* (*dfsch_mapping_ref_t)(dfsch_object_t* hash, 
                                               dfsch_object_t* key);
typedef void (*dfsch_mapping_set_t)(dfsch_object_t* hash, 
                                    dfsch_object_t* key,
                                    dfsch_object_t* value);
typedef int (*dfsch_mapping_unset_t)(dfsch_object_t* hash, 
                                     dfsch_object_t* key);
typedef int (*dfsch_mapping_set_if_exists_t)(dfsch_object_t* hash, 
                                             dfsch_object_t* key,
                                             dfsch_object_t* value);
typedef int (*dfsch_mapping_set_if_not_exists_t)(dfsch_object_t* hash, 
                                                 dfsch_object_t* key,
                                                 dfsch_object_t* value);

typedef dfsch_object_t* (*dfsch_mapping_get_keys_iterator_t)(dfsch_object_t* map);
typedef dfsch_object_t* (*dfsch_mapping_get_values_iterator_t)(dfsch_object_t* map);


typedef struct dfsch_mapping_methods_t {
  dfsch_mapping_ref_t ref;
  dfsch_mapping_set_t set;
  dfsch_mapping_unset_t unset;
  dfsch_mapping_set_if_exists_t set_if_exists;
  dfsch_mapping_set_if_not_exists_t set_if_not_exists;

  dfsch_mapping_get_keys_iterator_t get_keys_iterator;
  dfsch_mapping_get_values_iterator_t get_values_iterator;
} dfsch_mapping_methods_t;

typedef dfsch_object_t* (*dfsch_iterator_next_t)(dfsch_object_t*);
typedef dfsch_object_t* (*dfsch_iterator_this_t)(dfsch_object_t*);

typedef struct dfsch_iterator_methods_t {
  dfsch_iterator_next_t next;
  dfsch_iterator_this_t this;
} dfsch_iterator_methods_t;

typedef struct dfsch_slot_t dfsch_slot_t;
struct dfsch_type_t {
  /** When we want to use type_t as first-class object */
  dfsch_type_t* type;
  /** Superclass */
  dfsch_type_t* superclass;
  /** Instance size */
  size_t size;
  /** Type name */
  char* name;
  /** Equal method - called with two instances of this type */
  dfsch_type_equal_p_t equal_p;
  /** 
   * Should return external representation of given object. In most cases
   * something like "#&gt;my-object bla bla bla&lt;"
   */
  dfsch_type_write_t write;
  /** 
   * Apply method - called when object of this type is applyed to 
   * something. Beware - primitives and closures are handled directly
   * in evaluator and have this field set to NULL
   */
  dfsch_type_apply_t apply;
  
  /**
   * Hash method - return hash for this object. Objects that are equal?
   * have same hash. When NULL, hash is derived from value of object 
   * pointer
   */
  dfsch_type_hash_t hash;
  /** Slot table */
  dfsch_slot_t* slots;
  /** Documentation string */
  char* documentation;
  /** type flags */
  int flags;

  dfsch_type_describe_t describe;

  dfsch_collection_methods_t* collection;
  dfsch_sequence_methods_t* sequence;
  dfsch_mapping_methods_t* mapping;
  dfsch_iterator_methods_t* iterator;

  dfsch_type_serialize_t serialize;

  dfsch_object_t* slot_metadata;
  dfsch_object_t* roles;

  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR;

typedef dfsch_object_t* (*dfsch_accessor_ref_t)(void* ptr, 
                                                dfsch_object_t* obj, 
                                                dfsch_slot_t* slot);
typedef void (*dfsch_accessor_set_t)(void* ptr, 
                                     dfsch_object_t* value,
                                     dfsch_object_t* obj,
                                     dfsch_slot_t* slot);
typedef void (*dfsch_slot_type_init_t)(dfsch_type_t* type,
                                       dfsch_slot_t* slot);
typedef dfsch_object_t* (*dfsch_slot_instance_init_t)(void* ptr, 
                                                      dfsch_object_t* obj, 
                                                      dfsch_slot_t* slot);

typedef struct dfsch_slot_type_t {
  dfsch_type_t standard_type;
  dfsch_accessor_ref_t ref;
  dfsch_accessor_set_t set;
  size_t size;
  size_t alignment;
  
  dfsch_slot_type_init_t type_init;
  dfsch_slot_instance_init_t instance_init;

  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_slot_type_t;

extern dfsch_type_t dfsch_slot_type_type;
#define DFSCH_SLOT_TYPE_TYPE (&dfsch_slot_type_type)
extern dfsch_type_t dfsch_slot_type;
#define DFSCH_SLOT_TYPE (&dfsch_slot_type)

extern dfsch_slot_type_t dfsch_object_slot_type;
#define DFSCH_OBJECT_SLOT_TYPE (&dfsch_object_slot_type)
extern dfsch_slot_type_t dfsch_boolean_slot_type;
#define DFSCH_BOOLEAN_SLOT_TYPE (&dfsch_boolean_slot_type)
extern dfsch_slot_type_t dfsch_string_slot_type;
#define DFSCH_STRING_SLOT_TYPE (&dfsch_string_slot_type)
extern dfsch_slot_type_t dfsch_buffer_slot_type;
#define DFSCH_BUFFER_SLOT_TYPE (&dfsch_buffer_slot_type)
extern dfsch_slot_type_t dfsch_size_t_slot_type;
#define DFSCH_SIZE_T_SLOT_TYPE (&dfsch_size_t_slot_type)
extern dfsch_slot_type_t dfsch_int_slot_type;
#define DFSCH_INT_SLOT_TYPE (&dfsch_int_slot_type)
extern dfsch_slot_type_t dfsch_long_slot_type;
#define DFSCH_LONG_SLOT_TYPE (&dfsch_long_slot_type)

struct dfsch_slot_t {
  dfsch_slot_type_t* type;
  char* name;
  size_t offset;
  int access;
  char* documentation;
  void* slot_data;
  dfsch_object_t* options;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR;

#define DFSCH_SLOT_ACCESS_RW          0
#define DFSCH_SLOT_ACCESS_DEBUG_WRITE 1
#define DFSCH_SLOT_ACCESS_RO          2
#define DFSCH_SLOT_ACCESS_DEBUG_READ  3

#define DFSCH_SLOT_TYPE_HEAD(name, documentation)                     \
  {DFSCH_SLOT_TYPE_TYPE, DFSCH_SLOT_TYPE, sizeof(dfsch_slot_t), name, \
      NULL, NULL, NULL, NULL, NULL, documentation}

#define DFSCH_OBJECT_SLOT(struct, name, access, doc)          \
  {DFSCH_OBJECT_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_BOOLEAN_SLOT(struct, name, access, doc)                   \
  {DFSCH_BOOLEAN_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_STRING_SLOT(struct, name, access, doc)                    \
  {DFSCH_STRING_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_BUFFER_SLOT(struct, name, access, doc)                    \
  {DFSCH_BUFFER_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_SIZE_T_SLOT(struct, name, access, doc)                    \
  {DFSCH_SIZE_T_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_INT_SLOT(struct, name, access, doc)               \
  {DFSCH_INT_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_LONG_SLOT(struct, name, access, doc)              \
  {DFSCH_LONG_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_SLOT_TERMINATOR {NULL, NULL, 0, 0, NULL}

extern dfsch_collection_methods_t dfsch_iterator_collection_methods;

extern dfsch_type_t dfsch_sequence_iterator_type;
#define DFSCH_SEQUENCE_ITERATOR_TYPE \
  ((dfsch_type_t*)&dfsch_sequence_iterator_type)

dfsch_object_t* dfsch_make_sequence_iterator(dfsch_object_t* sequence);
dfsch_object_t* dfsch_make_mapping_constructor(dfsch_object_t* map);

extern dfsch_collection_methods_t dfsch_sequence_collection_methods;
#define DFSCH_COLLECTION_AS_SEQUENCE (&dfsch_sequence_collection_methods)

typedef void (*dfsch_collection_constructor_add_t)(dfsch_object_t* constructor,
                                                   dfsch_object_t* element);
typedef dfsch_object_t* (*dfsch_collection_constructor_done_t)(dfsch_object_t* c);

typedef struct dfsch_collection_constructor_type_t {
  dfsch_type_t type;
  dfsch_collection_constructor_add_t add;
  dfsch_collection_constructor_done_t done;
} dfsch_collection_constructor_type_t;

extern dfsch_type_t dfsch_collection_constructor_type_type;
#define DFSCH_COLLECTION_CONSTRUCTOR_TYPE_TYPE \
  (&dfsch_collection_constructor_type_type)

extern dfsch_type_t dfsch_collection_constructor_type;
#define DFSCH_COLLECTION_CONSTRUCTOR_TYPE \
  (&dfsch_collection_constructor_type)

extern dfsch_collection_constructor_type_t dfsch_mutable_list_constructor_type;
#define DFSCH_MUTABLE_LIST_CONSTRUCTOR_TYPE \
  (&dfsch_mutable_list_constructor_type)
extern dfsch_collection_constructor_type_t dfsch_vector_constructor_type;
#define DFSCH_VECTOR_CONSTRUCTOR_TYPE (&dfsch_vector_constructor_type)

extern dfsch_collection_constructor_type_t dfsch_mapping_constructor_type;
#define DFSCH_MAPPING_CONSTRUCTOR_TYPE (&dfsch_mapping_constructor_type)


/*
 * Objects should be always 8-byte aligned in memory, even on 32b platforms.
 * When this is not true this encoding will not work. In fact, there are other
 * plausible representations, but introducing (backtroducing?) type pointers 
 * into pair is not good idea, as any overhead on pairs consumes memory, and
 * more importantly increases amount of cache misses.
 *
 * Object pointer tag meaning:
 * 000 - Normal object, first word is type
 * 010 - Mutable pair
 * 100 - Symbol
 * 110 - Immutable pair
 * 001 - Fixnum (cannot be 11, because of invalid object marker)
 * 101 - Small direct objects
 * x11 - Entry of cdr-coded list (pointer points to WORD(!), not object)
 *
 * And there are two special values of object pointer:
 * All zeroes - empty list
 * All ones - invalid object marker (e.g. end of CDR-coded list or invalid 
 *            weak reference, with important point is that weak-references 
 *            are set to this value by GC, and this is not simply changed)
 *
 * Idea is that structure of loaded code should consist of annotated pairs 
 * and CDR-coded sections. Immutable pairs are strictly not necessary, but 
 * still are good idea (for consistency and might be useful in user-code)
 */

extern dfsch_type_t dfsch_pair_type;
#define DFSCH_PAIR_TYPE (&dfsch_pair_type)

extern dfsch_type_t dfsch_tagged_types[4];
#define DFSCH_MUTABLE_PAIR_TYPE (&(dfsch_tagged_types[1]))
#define DFSCH_SYMBOL_TYPE (&(dfsch_tagged_types[2]))
#define DFSCH_IMMUTABLE_PAIR_TYPE (&(dfsch_tagged_types[3]))
#define DFSCH_COMPACT_LIST_TYPE (&(dfsch_tagged_types[0]))

extern dfsch_type_t* const dfsch_small_types[32];

#define DFSCH_SMALL_TAG_CHARACTER 0x00

#define DFSCH_SMALL_VALUE_REF(obj)  \
  (((unsigned long)(((size_t)(obj)) & ~0xffL)) >> 8)
#define DFSCH_MAKE_SMALL_VALUE(value, tag)          \
  ((dfsch_object_t*)((((size_t)(value)) << 8) | (tag << 5) | 0x5))
  


#define DFSCH_INVALID_OBJECT ((dfsch_object_t*)((ptrdiff_t) -1))

typedef struct dfsch_pair_t {
  dfsch_object_t* car;
  dfsch_object_t* cdr;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_pair_t;


#define DFSCH__FAST_CDR_CODED_P(pair)           \
  ((((size_t)(pair)) & 0x03) == 0x03)
#define DFSCH__COMPACT_LIST_CDR(ptr)                                    \
  (((dfsch_object_t**)(((size_t)(ptr)) & ~0x03))[1] == DFSCH_INVALID_OBJECT ? \
   ((dfsch_object_t**)(((size_t)(ptr)) & ~0x03))[2] :                   \
   (dfsch_object_t*)(((dfsch_object_t**)(ptr))+1))

#define DFSCH__COMPACT_LIST_DECODE(ptr)         \
  ((dfsch_object_t**)(((size_t)(ptr)) & ~0x03))
#define DFSCH__COMPACT_LIST_CDR_FAST(ptr)               \
  ((dfsch_object_t*)(((dfsch_object_t**)(ptr))+1))
#define DFSCH__COMPACT_LIST_CAR_FAST(ptr)               \
  (*((dfsch_object_t**)(((size_t)(ptr)) & ~0x03)))


#define DFSCH_PAIR_REF(obj)						\
  ((dfsch_pair_t*)(((size_t)(obj)) & (DFSCH__FAST_CDR_CODED_P(obj) ?	\
				      ~0x03L : ~0x07L)))
#define DFSCH_MAKE_CLIST(ptr)\
  ((dfsch_object_t*)(((size_t)(ptr)) | 0x03))  
#define DFSCH_TAG_ENCODE(obj, kind)             \
  ((dfsch_object_t*)(((size_t)(obj)) | (kind << 1)))
#define DFSCH_TAG_REF(obj)                      \
  ((void*)(((size_t)(obj)) & ~0x07L))

#define DFSCH_FAST_CAR(obj)                     \
  (DFSCH_PAIR_REF(obj)->car)
#define DFSCH_FAST_CDR_MUT(obj)                 \
  (DFSCH_PAIR_REF(obj)->cdr)
#define DFSCH_FAST_CDR(obj)                                     \
  (DFSCH__FAST_CDR_CODED_P(obj) ?                               \
   DFSCH__COMPACT_LIST_CDR(obj):                                \
   (DFSCH_PAIR_REF(obj)->cdr))
#define DFSCH_PAIR_P(obj) (((((size_t)(obj)) & 0x02) == 0x02))
#define DFSCH_SYMBOL_P(obj) (((((size_t)(obj)) & 0x07) == 0x04))
#define DFSCH_FIXNUM_P(obj) ((((size_t)(obj)) & 0x07) == 0x01)
#define DFSCH_INTERNED_SYMBOL_P(obj)                                    \
  (DFSCH_SYMBOL_P((obj))                                                \
   && ((dfsch__symbol_t*)DFSCH_TAG_REF((obj)))->name != NULL            \
   &&  ((dfsch__symbol_t*)DFSCH_TAG_REF((obj)))->package != NULL)

#define DFSCH_FIXNUM_REF(obj)                   \
  (((long)(((ptrdiff_t)(obj)) & ~0x03L)) >> 3)
#define DFSCH_MAKE_FIXNUM(obj)                                  \
  ((dfsch_object_t*) ((((ptrdiff_t)(obj)) << 3) | 0x01L))
#define DFSCH_FIXNUM_MAX (PTRDIFF_MAX / 8)
#define DFSCH_FIXNUM_MIN (PTRDIFF_MIN / 8)

#define DFSCH_MAKE_CHARACTER(chr)               \
  DFSCH_MAKE_SMALL_VALUE(chr, DFSCH_SMALL_TAG_CHARACTER)
#define DFSCH_CHARACTER_P(obj) ((((size_t)(obj)) & 0xff) == 0x05)
  

#define DFSCH_TYPE_OF(obj)                                              \
  ((obj)?(                                                              \
          (((size_t)(obj)) & 0x07) == 0 ? ((dfsch_object_t*)(obj))->type: \
          ((((size_t)(obj)) & 0x01) == 0x01 ?                           \
           ((((size_t)(obj)) & 0x02) == 0x00 ?                          \
            ((((size_t)(obj)) & 0x04) == 0x00 ? DFSCH_FIXNUM_TYPE :     \
             dfsch_small_types[((((size_t)(obj)) >> 3) & 0x1f)])        \
            : DFSCH_COMPACT_LIST_TYPE):                                 \
           &(dfsch_tagged_types[(((size_t)(obj)) & 0x06) >> 1]))):      \
   DFSCH_EMPTY_LIST_TYPE)

  
#define DFSCH_INSTANCE_P(o, t)                                  \
  ((DFSCH_TYPE_OF(o) == (t))||dfsch_instance_p((o), (t)))

#define DFSCH_ASSERT_TYPE(o, t)                                         \
  ((DFSCH_TYPE_OF((o)) == (t)) ? ((void*)(o)) : dfsch_assert_type((o), (t)))
#define DFSCH_ASSERT_INSTANCE(o, t)                                     \
  (DFSCH_INSTANCE_P((o), (t)) ? (o) : dfsch_assert_instance((o), (t)))
#define DFSCH_ASSERT_METACLASS_INSTANCE(o, t)                           \
  (DFSCH_INSTANCE_P(DFSCH_TYPE_OF((o)), (t)) ?                          \
   (o) : dfsch_assert_metaclass_instance((o), (t)))


#define DFSCH_TYPE_COLLECTION_P(t)                   \
  (((dfsch_type_t*)(t))->collection != NULL)
#define DFSCH_COLLECTION_P(o)                   \
  (DFSCH_TYPE_COLLECTION_P(DFSCH_TYPE_OF((o))))

#define DFSCH_TYPE_MAPPING_P(t)                   \
  (((dfsch_type_t*)(t))->mapping != NULL)
#define DFSCH_MAPPING_P(o)                   \
  (DFSCH_TYPE_MAPPING_P(DFSCH_TYPE_OF((o))))

#define DFSCH_TYPE_SEQUENCE_P(t)                   \
  (((dfsch_type_t*)(t))->sequence != NULL)
#define DFSCH_SEQUENCE_P(o)                   \
  (DFSCH_TYPE_SEQUENCE_P(DFSCH_TYPE_OF((o))))

#define DFSCH_TYPE_ITERATOR_P(t)                   \
  (((dfsch_type_t*)(t))->iterator != NULL)
#define DFSCH_ITERATOR_P(o)                   \
  (DFSCH_TYPE_ITERATOR_P(DFSCH_TYPE_OF((o))))

#define DFSCH_ASSERT_COLLECTION(o)                                      \
  (DFSCH_COLLECTION_P((o)) ? (o) : dfsch_assert_collection((o)))
#define DFSCH_ASSERT_MAPPING(o)                                 \
  (DFSCH_MAPPING_P((o)) ? (o) : dfsch_assert_mapping((o)))
#define DFSCH_ASSERT_SEQUENCE(o)                                \
  (DFSCH_SEQUENCE_P((o)) ? (o) : dfsch_assert_sequence((o)))
#define DFSCH_ASSERT_ITERATOR(o)                                \
  (DFSCH_ITERATOR_P((o)) ? (o) : dfsch_assert_iterator((o)))

#define DFSCH_ASSERT_PAIR(p)                                            \
  (DFSCH_PAIR_P((p)) ? (p) : dfsch_assert_instance((p), DFSCH_PAIR_TYPE))

#define DFSCH_ASSERT_SEQUENCE_INDEX(seq, idx, len)\
  (((idx) < (len)) ? (idx) : dfsch_assert_sequence_index(seq, idx, len))

typedef struct dfsch_package_t dfsch_package_t;

extern dfsch_package_t dfsch_dfsch_package;
#define DFSCH_DFSCH_PACKAGE (&dfsch_dfsch_package)
extern dfsch_package_t dfsch_dfsch_user_package;
#define DFSCH_DFSCH_USER_PACKAGE (&dfsch_dfsch_user_package)
extern dfsch_package_t dfsch_dfsch_internal_package;
#define DFSCH_DFSCH_INTERNAL_PACKAGE (&dfsch_dfsch_internal_package)
extern dfsch_package_t dfsch_keyword_package;
#define DFSCH_KEYWORD_PACKAGE (&dfsch_keyword_package)

typedef struct dfsch__symbol_t{
  dfsch_package_t* package;
  char *name;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch__symbol_t;

/* actual contents of this table is in src/package.c */
extern dfsch__symbol_t dfsch__static_symbols[];
#define DFSCH__STATIC_SYMBOL(index)			\
  DFSCH_TAG_ENCODE(dfsch__static_symbols + (index), 2)

#define DFSCH_SYM_TRUE DFSCH__STATIC_SYMBOL(0)
#define DFSCH_SYM_QUOTE DFSCH__STATIC_SYMBOL(1)
#define DFSCH_SYM_QUASIQUOTE DFSCH__STATIC_SYMBOL(2)
#define DFSCH_SYM_UNQUOTE DFSCH__STATIC_SYMBOL(3)
#define DFSCH_SYM_UNQUOTE_SPLICING DFSCH__STATIC_SYMBOL(4)
#define DFSCH_SYM_ELSE DFSCH__STATIC_SYMBOL(5)
#define DFSCH_SYM_BOLD_RIGHT_ARROW DFSCH__STATIC_SYMBOL(6)

#define DFSCH_LK_OPTIONAL DFSCH__STATIC_SYMBOL(7)
#define DFSCH_LK_KEY DFSCH__STATIC_SYMBOL(8)
#define DFSCH_LK_REST DFSCH__STATIC_SYMBOL(9)
#define DFSCH_LK_BODY DFSCH__STATIC_SYMBOL(10)
#define DFSCH_LK_ALLOW_OTHER_KEYS DFSCH__STATIC_SYMBOL(11)
#define DFSCH_LK_ENVIRONMENT DFSCH__STATIC_SYMBOL(12)
#define DFSCH_LK_WHOLE DFSCH__STATIC_SYMBOL(13)
#define DFSCH_LK_AUX DFSCH__STATIC_SYMBOL(14)

#define DFSCH_QUAL_BEFORE DFSCH__STATIC_SYMBOL(15)
#define DFSCH_QUAL_AFTER DFSCH__STATIC_SYMBOL(16)
#define DFSCH_QUAL_AROUND DFSCH__STATIC_SYMBOL(17)

#define DFSCH_SYM_TERMINATE_THREAD DFSCH__STATIC_SYMBOL(18)
#define DFSCH_SYM_USE_VALUE DFSCH__STATIC_SYMBOL(19)

#define DFSCH_SYM_MACRO_EXPANDED_FROM DFSCH__STATIC_SYMBOL(20)
#define DFSCH_SYM_IMMUTABLE_QUASIQUOTE DFSCH__STATIC_SYMBOL(21)
#define DFSCH_SYM_COMPILED_FROM DFSCH__STATIC_SYMBOL(22)
#define DFSCH_SYM_UNQUOTE_NCONCING DFSCH__STATIC_SYMBOL(23)

#define DFSCH_SYM_BREAK DFSCH__STATIC_SYMBOL(24)
#define DFSCH_SYM_MUFFLE_WARNING DFSCH__STATIC_SYMBOL(25)

#endif
