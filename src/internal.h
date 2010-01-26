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

dfsch_object_t* dfsch_make_number_from_string_noerror(char* string, int obase);


dfsch_object_t* dfsch__make_slot_accessor_for_slot(dfsch_type_t* type,
                                                   dfsch_slot_t* slot);
dfsch_object_t* dfsch__make_slot_reader_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot);
dfsch_object_t* dfsch__make_slot_writer_for_slot(dfsch_type_t* type,
                                                 dfsch_slot_t* slot);


#endif
