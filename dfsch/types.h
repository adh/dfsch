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

  extern dfsch_type_t dfsch_abstract_type;
#define DFSCH_ABSTRACT_TYPE ((dfsch_type_t*)&dfsch_abstract_type)
  extern dfsch_type_t dfsch_standard_type;
#define DFSCH_STANDARD_TYPE ((dfsch_type_t*)&dfsch_standard_type)
  extern dfsch_type_t dfsch_list_type;
#define DFSCH_LIST_TYPE ((dfsch_type_t*)&dfsch_list_type)
  extern dfsch_type_t dfsch_empty_list_type;
#define DFSCH_EMPTY_LIST_TYPE ((dfsch_type_t*)&dfsch_empty_list_type)


  typedef struct dfsch_primitive_t {
    dfsch_type_t* type;
    dfsch_primitive_impl_t proc;
    void *baton;
    int flags;
  } dfsch_primitive_t;

  extern dfsch_type_t dfsch_primitive_type;

#define DFSCH_PRIMITIVE_TYPE (&dfsch_primitive_type)

#define DFSCH_PRIMITIVE_CACHED 1
#define DFSCH_PRIMITIVE_PURE   2

#define DFSCH_DECLARE_PRIMITIVE(name, flags)    \
  static dfsch_primitive_t p_##name = {   \
    DFSCH_PRIMITIVE_TYPE,                       \
    p_##name##_impl,                            \
    NULL,                                       \
    flags                                       \
  }
  
#define DFSCH_DECLARE_PRIMITIVE_EX(name, baton, flags)       \
  static dfsch_primitive_t p_##name = {                \
    DFSCH_PRIMITIVE_TYPE,                                    \
    p_##name##_impl,                                         \
    baton,                                                   \
    flags                                                    \
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
  typedef dfsch_object_t* (*dfsch_form_compile_t)(dfsch_form_t* form,
                                                  dfsch_object_t* env,
                                                  dfsch_object_t* args,
                                                  int depth);

  struct dfsch_form_t {
    dfsch_type_t* type;
    dfsch_form_impl_t impl;
    dfsch_form_compile_t compile;
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
#define DFSCH_FORM_COMPILATION(name)                                    \
  static dfsch_object_t* form_##name##_compile(dfsch_form_t* form,      \
                                               dfsch_object_t* env,     \
                                               dfsch_object_t* args,    \
                                               int depth                \
                                               )

#define DFSCH_DEFINE_FORM(name)                 \
  static dfsch_form_t form_##name = {           \
    DFSCH_FORM_TYPE,                            \
    form_##name##_impl,                         \
    form_##name##_compile,                      \
    NULL,                                       \
    #name                                       \
  }

#define DFSCH_DEFINE_FORM_IMPL(name, compile)           \
  DFSCH_FORM_IMPLEMENTATION(name);                      \
  static dfsch_form_t form_##name = {                   \
    DFSCH_FORM_TYPE,                                    \
    form_##name##_impl,                                 \
    compile,                                            \
    NULL,                                               \
    #name                                               \
  };                                                    \
  DFSCH_FORM_IMPLEMENTATION(name)


#define DFSCH_FORM_REF(name) ((dfsch_object_t*)&form_##name)

#define DFSCH_MAKE_FORM(name,baton)                                     \
  (dfsch_make_form(form_##name##_impl,                                  \
                   form_##name##_compile,                               \
                   (baton),                                             \
                   #name))

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
  ((dfsch_pair_t*)(((size_t)(obj)) & ~0x03))
#define DFSCH_PAIR_ENCODE(obj)                  \
  ((dfsch_object_t*)(((size_t)(obj)) | 0x02))

#define DFSCH_FAST_CAR(obj)                     \
  (DFSCH_PAIR_REF(obj)->car)
#define DFSCH_FAST_CDR(obj)                     \
  (DFSCH_PAIR_REF(obj)->cdr)

#define DFSCH_FIXNUM_REF(obj)\
  (((long)(((size_t)(obj)) & ~0x01)) >> 1)
#define DFSCH_MAKE_FIXNUM(obj)\
  ((dfsch_object_t*) ((((size_t)(obj)) << 1) | 0x01))


#define DFSCH_TYPE_OF(obj) \
  ((obj)?(                                                              \
          (((size_t)(obj)) & 0x03) == 0 ? (obj)->type:                  \
          ((((size_t)(obj)) & 0x03) == 2 ? DFSCH_PAIR_TYPE:             \
           DFSCH_FIXNUM_TYPE)):                                         \
   DFSCH_EMPTY_LIST_TYPE)

  
#define DFSCH_INSTANCE_P(o, t)                                  \
  ((DFSCH_TYPE_OF(o) == (t))||dfsch_instance_p((o), (t)))


#endif
