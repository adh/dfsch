/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
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

#ifndef H__dfsch__object__
#define H__dfsch__object__

#include <dfsch/dfsch.h>
#include <dfsch/specializers.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct dfsch_metaclass_t dfsch_metaclass_t;

  typedef dfsch_object_t* (*dfsch_alloc_instance_t)(dfsch_object_t* klass);
  typedef dfsch_object_t* (*dfsch_make_class_t)(dfsch_metaclass_t* mc,
                                                dfsch_object_t* super,
                                                char* name,
                                                dfsch_object_t* slots,
                                                dfsch_object_t* options,
                                                dfsch_object_t* roles);

  struct dfsch_metaclass_t {
    dfsch_type_t type;
    dfsch_alloc_instance_t allocate_instance;
    dfsch_make_class_t make_class;
  };

  extern dfsch_type_t dfsch_metaclass_type;
#define DFSCH_METACLASS_TYPE (&dfsch_metaclass_type)

  extern dfsch_metaclass_t dfsch_standard_class_type;
#define DFSCH_STANDARD_CLASS_TYPE (&dfsch_standard_class_type)

typedef struct dfsch_standard_class_t {
  dfsch_type_t standard_type;
  dfsch_object_t* write_instance;
  dfsch_object_t* initfuncs;
  dfsch_object_t* initargs;
} dfsch_standard_class_t;


  dfsch_object_t* dfsch_make_class(dfsch_object_t* superclass,
                                   dfsch_object_t* metaclass,
                                   char* name,
                                   dfsch_object_t* slots,
                                   dfsch_object_t* options,
                                   dfsch_object_t* roles);
  dfsch_object_t* dfsch_make_instance(dfsch_object_t* klass,
                                      dfsch_object_t* args);

  void dfsch_make_class_slots(dfsch_slot_type_t* default_slot_type,
                              dfsch_type_t* klass,
                              dfsch_object_t* defs);
  void dfsch_standard_class_prepare_slots(dfsch_standard_class_t* klass);

  extern dfsch_type_specializer_type_t dfsch_role_type;
#define DFSCH_ROLE_TYPE (&dfsch_role_type)

  dfsch_object_t* dfsch_make_role(char* name,
                                  dfsch_object_t* superroles,
                                  dfsch_object_t* slots,
                                  dfsch_object_t* options);
  int dfsch_role_inherited_p(dfsch_object_t* sub, 
                             dfsch_object_t* super);

#ifdef __cplusplus
}
#endif

#endif
