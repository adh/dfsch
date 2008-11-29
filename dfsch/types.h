/*
 * dfsch - Scheme-like Lisp dialect
 *   Basic types.
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifndef H__dfsch__types__
#define H__dfsch__types__

#include <stddef.h>

extern dfsch_type_t dfsch_abstract_type;
#define DFSCH_ABSTRACT_TYPE ((dfsch_type_t*)&dfsch_abstract_type)
extern dfsch_type_t dfsch_meta_type;
#define DFSCH_META_TYPE ((dfsch_type_t*)&dfsch_meta_type)
extern dfsch_type_t dfsch_standard_type;
#define DFSCH_STANDARD_TYPE ((dfsch_type_t*)&dfsch_standard_type)
extern dfsch_type_t dfsch_list_type;
#define DFSCH_LIST_TYPE ((dfsch_type_t*)&dfsch_list_type)
extern dfsch_type_t dfsch_standard_function_type;
#define DFSCH_STANDARD_FUNCTION_TYPE                    \
  ((dfsch_type_t*)&dfsch_standard_function_type)
extern dfsch_type_t dfsch_empty_list_type;
#define DFSCH_EMPTY_LIST_TYPE ((dfsch_type_t*)&dfsch_empty_list_type)

extern dfsch_type_t dfsch_symbol_type;
#define DFSCH_SYMBOL_TYPE (&dfsch_symbol_type)
extern dfsch_type_t dfsch_function_type;
#define DFSCH_FUNCTION_TYPE (&dfsch_function_type)
extern dfsch_type_t dfsch_macro_type;
#define DFSCH_MACRO_TYPE (&dfsch_macro_type)
extern dfsch_type_t dfsch_vector_type;
#define DFSCH_VECTOR_TYPE (&dfsch_vector_type)
extern dfsch_type_t dfsch_environment_type;
#define DFSCH_ENVIRONMENT_TYPE (&dfsch_environment_type)

typedef struct dfsch_primitive_t {
  dfsch_type_t* type;
  dfsch_primitive_impl_t proc;
  void *baton;
  int flags;
  char* name;
} dfsch_primitive_t;

extern dfsch_type_t dfsch_primitive_type;

#define DFSCH_PRIMITIVE_TYPE (&dfsch_primitive_type)

#define DFSCH_PRIMITIVE_CACHED 1
#define DFSCH_PRIMITIVE_PURE   2

#define DFSCH_DECLARE_PRIMITIVE(name, flags)    \
  static dfsch_primitive_t p_##name = {         \
    DFSCH_PRIMITIVE_TYPE,                       \
    p_##name##_impl,                            \
    NULL,                                       \
    flags,                                      \
    #name                                       \
  }
  
#define DFSCH_DECLARE_PRIMITIVE_EX(name, baton, flags)  \
  static dfsch_primitive_t p_##name = {                 \
    DFSCH_PRIMITIVE_TYPE,                               \
    p_##name##_impl,                                    \
    baton,                                              \
    flags,                                              \
    #name                                               \
  }

#define DFSCH_PRIMITIVE_HEAD(name)                                      \
  static dfsch_object_t* p_##name##_impl(void* baton,                   \
                                         dfsch_object_t* args,          \
                                         dfsch_tail_escape_t* esc)
  
#define DFSCH_DEFINE_PRIMITIVE(name, flags)     \
  DFSCH_PRIMITIVE_HEAD(name);                   \
  DFSCH_DECLARE_PRIMITIVE(name, flags);         \
  DFSCH_PRIMITIVE_HEAD(name)

#define DFSCH_PRIMITIVE_REF(name) ((dfsch_object_t*)&p_##name)

typedef struct dfsch_form_t dfsch_form_t;

typedef dfsch_object_t* (*dfsch_form_impl_t)(dfsch_form_t* form,
                                             dfsch_object_t* env,
                                             dfsch_object_t* args,
                                             dfsch_tail_escape_t* esc);
struct dfsch_form_t {
  dfsch_type_t* type;
  dfsch_form_impl_t impl;
  void* baton;
  char* name;
};

extern dfsch_type_t dfsch_form_type;

#define DFSCH_FORM_TYPE (&dfsch_form_type)
  
#define DFSCH_FORM_IMPLEMENTATION(name)                                 \
  static dfsch_object_t* form_##name##_impl(dfsch_form_t* form,         \
                                            dfsch_object_t* env,        \
                                            dfsch_object_t* args,       \
                                            dfsch_tail_escape_t* esc)

#define DFSCH_DEFINE_FORM(name)                 \
  static dfsch_form_t form_##name = {           \
    DFSCH_FORM_TYPE,                            \
    form_##name##_impl,                         \
    NULL,                                       \
    #name                                       \
  }

#define DFSCH_DEFINE_FORM_IMPL(name)            \
  DFSCH_FORM_IMPLEMENTATION(name);              \
  static dfsch_form_t form_##name = {           \
    DFSCH_FORM_TYPE,                            \
    form_##name##_impl,                         \
    NULL,                                       \
    #name                                       \
  };                                            \
  DFSCH_FORM_IMPLEMENTATION(name)


#define DFSCH_FORM_REF(name) ((dfsch_object_t*)&form_##name)

#define DFSCH_MAKE_FORM(name,baton)             \
  (dfsch_make_form(form_##name##_impl,          \
                   (baton),                     \
                   #name))

typedef struct dfsch_slot_t dfsch_slot_t;

struct dfsch_type_t {
  /** When we want to use type_t as first-class object */
  dfsch_type_t* type;
  /** Superclass (NULL for normal objects) */
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
  dfsch_slot_t* slots;
};

typedef dfsch_object_t* (*dfsch_accessor_ref_t)(void* ptr);
typedef void (*dfsch_accessor_set_t)(void* ptr, dfsch_object_t* obj);

typedef struct dfsch_slot_type_t {
  dfsch_type_t standard_type;
  dfsch_accessor_ref_t ref;
  dfsch_accessor_set_t set;
  size_t size;
} dfsch_slot_type_t;

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
};

#define DFSCH_SLOT_ACCESS_RW          0
#define DFSCH_SLOT_ACCESS_DEBUG_WRITE 1
#define DFSCH_SLOT_ACCESS_RO          2
#define DFSCH_SLOT_ACCESS_DEBUG_READ  3

#define DFSCH_SLOT_TYPE_HEAD(name) \
  {DFSCH_SLOT_TYPE_TYPE, DFSCH_SLOT_TYPE, sizeof(dfsch_slot_t), name, NULL, NULL, NULL, NULL, NULL}

#define DFSCH_OBJECT_SLOT(struct, name, access)                         \
  {DFSCH_OBJECT_SLOT_TYPE, #name, offsetof(struct, name), access}
#define DFSCH_BOOLEAN_SLOT(struct, name, access)                        \
  {DFSCH_BOOLEAN_SLOT_TYPE, #name, offsetof(struct, name), access}
#define DFSCH_STRING_SLOT(struct, name, access)                         \
  {DFSCH_STRING_SLOT_TYPE, #name, offsetof(struct, name), access}
#define DFSCH_SIZE_T_SLOT(struct, name, access)                         \
  {DFSCH_SIZE_T_SLOT_TYPE, #name, offsetof(struct, name), access}
#define DFSCH_INT_SLOT(struct, name, access)                         \
  {DFSCH_INT_SLOT_TYPE, #name, offsetof(struct, name), access}
#define DFSCH_LONG_SLOT(struct, name, access)                         \
  {DFSCH_LONG_SLOT_TYPE, #name, offsetof(struct, name), access}
#define DFSCH_SLOT_TERMINATOR {NULL, NULL, 0}


/*
 * Object pointer tag meaning:
 * 00 - Normal object, first word is type
 * 10 - Pair - points to two words (car, cdr)
 * x1 - Fixnum
 */

extern dfsch_type_t dfsch_pair_type;
#define DFSCH_PAIR_TYPE (&dfsch_pair_type)


typedef struct dfsch_pair_t {
  dfsch_object_t* car;
  dfsch_object_t* cdr;
} dfsch_pair_t;


#define DFSCH_PAIR_REF(obj)                     \
  ((dfsch_pair_t*)(((size_t)(obj)) & ~0x03L))
#define DFSCH_PAIR_ENCODE(obj)                  \
  ((dfsch_object_t*)(((size_t)(obj)) | 0x02L))

#define DFSCH_FAST_CAR(obj)                     \
  (DFSCH_PAIR_REF(obj)->car)
#define DFSCH_FAST_CDR(obj)                     \
  (DFSCH_PAIR_REF(obj)->cdr)
#define DFSCH_PAIR_P(obj)                       \
  ((((size_t)(obj)) & 0x03) == 2)

#define DFSCH_FIXNUM_REF(obj)                   \
  (((long)(((ptrdiff_t)(obj)) & ~0x01L)) >> 1)
#define DFSCH_MAKE_FIXNUM(obj)                                  \
  ((dfsch_object_t*) ((((ptrdiff_t)(obj)) << 1) | 0x01L))
#define DFSCH_FIXNUM_MAX (PTRDIFF_MAX / 2)
#define DFSCH_FIXNUM_MIN (PTRDIFF_MIN / 2)


#define DFSCH_TYPE_OF(obj)                                              \
  ((obj)?(                                                              \
          (((size_t)(obj)) & 0x03) == 0 ? ((dfsch_object_t*)(obj))->type: \
          ((((size_t)(obj)) & 0x03) == 2 ? DFSCH_PAIR_TYPE:             \
           DFSCH_FIXNUM_TYPE)):                                         \
   DFSCH_EMPTY_LIST_TYPE)

  
#define DFSCH_INSTANCE_P(o, t)                                  \
  ((DFSCH_TYPE_OF(o) == (t))||dfsch_instance_p((o), (t)))


#endif
