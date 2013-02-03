/*
 * dfsch - dfox's quick and dirty scheme implementation
 * Copyright (C) 2005-2008 Ales Hakl
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

#ifndef H__dfsch___internal__
#define H__dfsch___internal__

#include <dfsch/dfsch.h>
#include "types.h"

extern void dfsch__primitives_register(dfsch_object_t *ctx);
extern void dfsch__forms_register(dfsch_object_t *ctx);
extern void dfsch__macros_register(dfsch_object_t *ctx);
extern void dfsch__hash_native_register(dfsch_object_t *ctx);
extern void dfsch__promise_native_register(dfsch_object_t *ctx);
extern void dfsch__number_native_register(dfsch_object_t *ctx);
extern void dfsch__string_native_register(dfsch_object_t *ctx);
extern void dfsch__weak_native_register(dfsch_object_t *ctx);
extern void dfsch__format_native_register(dfsch_object_t *ctx);
extern void dfsch__port_native_register(dfsch_object_t *ctx);
extern void dfsch__port_files_register(dfsch_object_t *ctx);
extern void dfsch__system_register(dfsch_object_t *ctx);
extern void dfsch__generic_register(dfsch_object_t *ctx);
extern void dfsch__bignum_register(dfsch_object_t* ctx);
extern void dfsch__conditions_register(dfsch_object_t* ctx);
extern void dfsch__compile_register(dfsch_object_t* ctx);
extern void dfsch__load_register(dfsch_object_t* ctx);
extern void dfsch__specializers_register(dfsch_object_t* ctx);

dfsch_object_t* dfsch_make_number_from_string_noerror(char* string, int obase);


dfsch_object_t* dfsch__make_slot_accessor_for_slot(dfsch_type_t* type,
                                                   dfsch_slot_t* slot);
dfsch_object_t* dfsch__make_slot_reader_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot);
dfsch_object_t* dfsch__make_slot_writer_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot);

void dfsch__register_vm_param(int* var, char* name, char* desc);
#define DEFINE_VM_PARAM(name, default, desc)                     \
  int name = default;                                            \
  static void __attribute__((constructor)) vmp__init_##name(){   \
    dfsch__register_vm_param(&name, #name, desc);                \
  }

void dfsch__register_vm_param_proc(int* var, char* name, char* desc,
                                   void (*change)(char* value));

#define DEFINE_VM_PARAM_PROC(name, default, desc)                     \
  int name = default;                                                 \
  static void vmp__change_##name();                                   \
  static void __attribute__((constructor)) vmp__init_##name(){        \
    dfsch__register_vm_param_proc(&name, #name, desc,                 \
                                  vmp__change_##name);                \
  }                                                                   \
  static void vmp__change_##name()

dfsch_object_t* dfsch__reclose_closure(dfsch_object_t* closure,
                                       environment_t* env);
void dfsch__copy_breakpoint_to_compiled_ast_node(dfsch_object_t* src,
                                                 dfsch_object_t* dst);
void dfsch__allocate_breakpoint_table();
void dfsch__maybe_free_breakpoint_table();

void dfsch__write_internal_reference(dfsch_writer_state_t* state,
                                     dfsch_object_t* obj,
                                     char* tag);


#endif
