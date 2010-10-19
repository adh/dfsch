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
extern void dfsch__system_register(dfsch_object_t *ctx);
extern void dfsch__generic_register(dfsch_object_t *ctx);
extern void dfsch__bignum_register(dfsch_object_t* ctx);
extern void dfsch__conditions_register(dfsch_object_t* ctx);
extern void dfsch__compile_register(dfsch_object_t* ctx);
extern void dfsch__load_register(dfsch_object_t* ctx);

dfsch_object_t* dfsch_make_number_from_string_noerror(char* string, int obase);


dfsch_object_t* dfsch__make_slot_accessor_for_slot(dfsch_type_t* type,
                                                   dfsch_slot_t* slot);
dfsch_object_t* dfsch__make_slot_reader_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot);
dfsch_object_t* dfsch__make_slot_writer_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot);


#endif
