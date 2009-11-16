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

extern dfsch_type_t dfsch_package_type;
#define DFSCH_PACKAGE_TYPE (&dfsch_package_type)

typedef struct dfsch_primitive_t {
  dfsch_type_t* type;
  dfsch_primitive_impl_t proc;
  void *baton;
  int flags;
  char* name;
  char* documentation;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_primitive_t;

extern dfsch_type_t dfsch_primitive_type;

#define DFSCH_PRIMITIVE_TYPE (&dfsch_primitive_type)

#define DFSCH_DECLARE_PRIMITIVE(name, documentation)    \
  static dfsch_primitive_t p_##name = {                 \
    DFSCH_PRIMITIVE_TYPE,                               \
    p_##name##_impl,                                    \
    NULL,                                               \
    0,                                                  \
    #name,                                              \
    documentation                                       \
  }
  
#define DFSCH_DECLARE_PRIMITIVE_EX(name, baton, flags, documentation)   \
  static dfsch_primitive_t p_##name = {                                 \
    DFSCH_PRIMITIVE_TYPE,                                               \
    p_##name##_impl,                                                    \
    baton,                                                              \
    flags,                                                              \
    #name,                                                              \
    documentation                                                       \
  }

#define DFSCH_PRIMITIVE_HEAD(name)                                      \
  static dfsch_object_t* p_##name##_impl(void* baton,                   \
                                         dfsch_object_t* args,          \
                                         dfsch_tail_escape_t* esc,      \
                                         dfsch_object_t* context)
  
#define DFSCH_DEFINE_PRIMITIVE(name, flags)     \
  DFSCH_PRIMITIVE_HEAD(name);        \
  DFSCH_DECLARE_PRIMITIVE(name, flags);         \
  DFSCH_PRIMITIVE_HEAD(name)

#define DFSCH_PRIMITIVE_REF(name) ((dfsch_object_t*)&p_##name)
#define DFSCH_PRIMITIVE_REF_MAKE(name, baton)\
  dfsch_make_primitive(p_##name##_impl, (baton))

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
  char* documentation;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR;

extern dfsch_type_t dfsch_form_type;

#define DFSCH_FORM_TYPE (&dfsch_form_type)
  
#define DFSCH_FORM_IMPLEMENTATION(name)                                 \
  static dfsch_object_t* form_##name##_impl(dfsch_form_t* form,         \
                                            dfsch_object_t* env,        \
                                            dfsch_object_t* args,       \
                                            dfsch_tail_escape_t* esc)

#define DFSCH_DEFINE_FORM(name, documentation)   \
  static dfsch_form_t form_##name = {            \
    DFSCH_FORM_TYPE,                             \
    form_##name##_impl,                          \
    NULL,                                        \
    #name,                                       \
    documentation                                \
  }

#define DFSCH_DEFINE_FORM_IMPL(name, documentation)     \
  DFSCH_FORM_IMPLEMENTATION(name);                      \
  static dfsch_form_t form_##name = {                   \
    DFSCH_FORM_TYPE,                                    \
    form_##name##_impl,                                 \
    NULL,                                               \
    #name,                                              \
    documentation                                       \
  };                                                    \
  DFSCH_FORM_IMPLEMENTATION(name)


#define DFSCH_FORM_REF(name) ((dfsch_object_t*)&form_##name)

#define DFSCH_MAKE_FORM(name,baton)             \
  (dfsch_make_form(form_##name##_impl,          \
                   (baton),                     \
                   #name))

/** Equivalence metod prototype */
typedef int (*dfsch_type_equal_p_t)(dfsch_object_t*, dfsch_object_t*);

typedef (*dfsch_output_proc_t)(void* baton, char* buf, size_t len);
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

/** Disable weak references for this type */
#define DFSCH_TYPEF_NO_WEAK_REFERENCES 1
/** Allow user code to inherit from this type */
#define DFSCH_TYPEF_USER_EXTENSIBLE    2

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
  /** Slot table */
  dfsch_slot_t* slots;
  /** Documentation string */
  char* documentation;
  /** type flags */
  int flags;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR;

typedef dfsch_object_t* (*dfsch_accessor_ref_t)(void* ptr);
typedef void (*dfsch_accessor_set_t)(void* ptr, dfsch_object_t* obj);

typedef struct dfsch_slot_type_t {
  dfsch_type_t standard_type;
  dfsch_accessor_ref_t ref;
  dfsch_accessor_set_t set;
  size_t size;
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
#define DFSCH_SIZE_T_SLOT(struct, name, access, doc)                    \
  {DFSCH_SIZE_T_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_INT_SLOT(struct, name, access, doc)               \
  {DFSCH_INT_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_LONG_SLOT(struct, name, access, doc)              \
  {DFSCH_LONG_SLOT_TYPE, #name, offsetof(struct, name), access, doc}
#define DFSCH_SLOT_TERMINATOR {NULL, NULL, 0, 0, NULL}


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
 * x01 - Fixnum (cannot be 11, because of invalid object marker)
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

#define DFSCH_INVALID_OBJECT ((dfsch_object_t*)((size_t) -1))

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
#define DFSCH_FIXNUM_P(obj) ((((size_t)(obj)) & 0x03) == 0x01)

#define DFSCH_FIXNUM_REF(obj)                   \
  (((long)(((ptrdiff_t)(obj)) & ~0x03L)) >> 2)
#define DFSCH_MAKE_FIXNUM(obj)                                  \
  ((dfsch_object_t*) ((((ptrdiff_t)(obj)) << 2) | 0x01L))
#define DFSCH_FIXNUM_MAX (PTRDIFF_MAX / 4)
#define DFSCH_FIXNUM_MIN (PTRDIFF_MIN / 4)


#define DFSCH_TYPE_OF(obj)                                              \
  ((obj)?(                                                              \
          (((size_t)(obj)) & 0x07) == 0 ? ((dfsch_object_t*)(obj))->type: \
          ((((size_t)(obj)) & 0x01) == 0x01 ?                           \
           ((((size_t)(obj)) & 0x02) == 0x00 ?                          \
            DFSCH_FIXNUM_TYPE : DFSCH_COMPACT_LIST_TYPE):               \
           &(dfsch_tagged_types[(((size_t)(obj)) & 0x06) >> 1]))):        \
   DFSCH_EMPTY_LIST_TYPE)

  
#define DFSCH_INSTANCE_P(o, t)                                  \
  ((DFSCH_TYPE_OF(o) == (t))||dfsch_instance_p((o), (t)))

#define DFSCH_ASSERT_TYPE(o, t)                                         \
  ((DFSCH_TYPE_OF((o)) == (t)) ? ((void*)(o)) : dfsch_assert_type((o), (t)))
#define DFSCH_ASSERT_INSTANCE(o, t)                                     \
  (DFSCH_INSTANCE_P((o), (t)) ? (o) : dfsch_assert_instance((o), (t)))

#define DFSCH_ASSERT_PAIR(p)                                            \
  (DFSCH_PAIR_P((p)) ? (p) : dfsch_assert_instance((p), DFSCH_PAIR_TYPE))

typedef struct dfsch_package_t dfsch_package_t;

extern dfsch_package_t dfsch_dfsch_package;
#define DFSCH_DFSCH_PACKAGE (&dfsch_dfsch_package)
extern dfsch_package_t dfsch_dfsch_user_package;
#define DFSCH_DFSCH_USER_PACKAGE (&dfsch_dfsch_user_package)

typedef struct dfsch__symbol_t{
  dfsch_package_t* package;
  char *name;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch__symbol_t;

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

#endif
