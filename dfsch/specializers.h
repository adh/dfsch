#ifndef H__dfsch__specializers__
#define H__dfsch__specializers__

#include <dfsch/dfsch.h>

typedef int (*dfsch_type_specializer_matches_p_t)(dfsch_object_t* specializer,
                                                  dfsch_type_t* type);

typedef struct dfsch_type_specializer_type_t {
  dfsch_type_t type;
  dfsch_type_specializer_matches_p_t matches_p;
} dfsch_type_specializer_type_t;

extern dfsch_type_t dfsch_type_specializer_metatype;
#define DFSCH_TYPE_SPECIALIZER_METATYPE (&dfsch_type_specializer_metatype)
extern dfsch_type_t dfsch_type_specializer_type;
#define DFSCH_TYPE_SPECIALIZER_TYPE (&dfsch_type_specialier_type)

int dfsch_specializer_matches_type_p(dfsch_object_t* specializer,
                                     dfsch_type_t* type);

typedef struct dfsch_metatype_specializer_t {
  dfsch_type_t* type;
  dfsch_type_t* required_metatype;
} dfsch_metatype_specializer_t;

extern dfsch_type_specializer_type_t dfsch_metatype_specializer_type;
#define DFSCH_METATYPE_SPECIALIZER_TYPE (&dfsch_metatype_specializer_type)

typedef struct dfsch_type_slot_specializer_t {
  dfsch_metatype_specializer_t parent;
  size_t slot_offset;
} dfsch_type_slot_specializer_t;

extern dfsch_type_specializer_type_t dfsch_type_slot_specializer_type;
#define DFSCH_TYPE_SLOT_SPECIALIZER_TYPE (&dfsch_type_slot_specializer_type)

typedef struct dfsch_singleton_type_specializer_t {
  dfsch_type_t* type;
  dfsch_type_specializer_matches_p_t matches_p;
} dfsch_singleton_type_specializer_t;

extern dfsch_type_specializer_type_t dfsch_singleton_type_specializer_type;
#define DFSCH_SINGLETON_TYPE_SPECIALIZER_TYPE (&dfsch_singleton_type_specializer_type)

extern dfsch_type_specializer_type_t dfsch_function_type_specializer_type;
#define DFSCH_FUNCTION_TYPE_SPECIALIZER_TYPE (&dfsch_function_type_specializer_type)
extern dfsch_type_specializer_type_t dfsch_compound_type_specializer_type;
#define DFSCH_COMPOUND_TYPE_SPECIALIZER_TYPE (&dfsch_compound_type_specializer_type)
extern dfsch_type_specializer_type_t dfsch_complement_type_specializer_type;
#define DFSCH_COMPLEMENT_TYPE_SPECIALIZER_TYPE (&dfsch_complement_type_specializer_type)

dfsch_object_t* dfsch_make_type_specializer(dfsch_object_t* fun);
dfsch_object_t* dfsch_union_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_intersect_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_difference_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_symetric_difference_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_complementary_specializer(dfsch_object_t* specializer);

#endif
