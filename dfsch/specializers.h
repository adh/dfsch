#ifndef H__dfsch__specializers__
#define H__dfsch__specializers__

#include <dfsch/dfsch.h>

typedef int (*dfsch_type_specializer_matches_p_t)(dfsch_object_t* specializer,
                                                  dfsch_object_t* type);

typedef struct dfsch_type_specializer_type_t {
  dfsch_type_t type;
  dfsch_type_specializer_matches_p_t matches_p;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_type_specializer_type_t;

extern dfsch_type_t dfsch_type_specializer_metatype;
#define DFSCH_TYPE_SPECIALIZER_METATYPE (&dfsch_type_specializer_metatype)
extern dfsch_type_t dfsch_type_specializer_type;
#define DFSCH_TYPE_SPECIALIZER_TYPE (&dfsch_type_specializer_type)

int dfsch_specializer_matches_type_p(dfsch_object_t* specializer,
                                     dfsch_object_t* type);

typedef struct dfsch_metatype_specializer_t {
  dfsch_type_t* type;
  dfsch_type_t* required_metatype;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_metatype_specializer_t;

extern dfsch_type_specializer_type_t dfsch_metatype_specializer_type;
#define DFSCH_METATYPE_SPECIALIZER_TYPE (&dfsch_metatype_specializer_type)

typedef struct dfsch_singleton_type_specializer_t {
  dfsch_type_t* type;
  dfsch_type_specializer_matches_p_t matches_p;
  dfsch_object_t* superspecializer;
  char* name;
  DFSCH_ALIGN8_DUMMY
} DFSCH_ALIGN8_ATTR dfsch_singleton_type_specializer_t;

#define DFSCH_DEFINE_SINGLETON_TYPE_SPECIALIZER(n, iname)               \
  static int spec_##n##_impl(dfsch_object_t* specializer,               \
                             dfsch_type_t* type);                       \
  dfsch_singleton_type_specializer_t n = {                              \
    .type = DFSCH_SINGLETON_TYPE_SPECIALIZER_TYPE,                      \
    .matches_p = spec_##n##_impl,                                       \
    .name = iname,                                                      \
  };                                                                    \
  static int spec_##n##_impl(dfsch_object_t* specializer,               \
                             dfsch_type_t* type)

extern dfsch_singleton_type_specializer_t dfsch_mapping_specializer;
#define DFSCH_MAPPING_SPECIALIZER (&dfsch_mapping_specializer)
extern dfsch_singleton_type_specializer_t dfsch_sequence_specializer;
#define DFSCH_SEQUENCE_SPECIALIZER (&dfsch_sequence_specializer)
extern dfsch_singleton_type_specializer_t dfsch_collection_specializer;
#define DFSCH_COLLECTION_SPECIALIZER (&dfsch_collection_specializer)
extern dfsch_singleton_type_specializer_t dfsch_iterator_specializer;
#define DFSCH_ITERATOR_SPECIALIZER (&dfsch_iterator_specializer)
extern dfsch_singleton_type_specializer_t dfsch_serializable_specializer;
#define DFSCH_SERIALIZABLE_SPECIALIZER (&dfsch_serializable_specializer)

extern dfsch_type_specializer_type_t dfsch_singleton_type_specializer_type;
#define DFSCH_SINGLETON_TYPE_SPECIALIZER_TYPE (&dfsch_singleton_type_specializer_type)

extern dfsch_type_specializer_type_t dfsch_function_type_specializer_type;
#define DFSCH_FUNCTION_TYPE_SPECIALIZER_TYPE (&dfsch_function_type_specializer_type)
extern dfsch_type_specializer_type_t dfsch_compound_type_specializer_type;
#define DFSCH_COMPOUND_TYPE_SPECIALIZER_TYPE (&dfsch_compound_type_specializer_type)
extern dfsch_type_specializer_type_t dfsch_complementary_type_specializer_type;
#define DFSCH_COMPLEMENTARY_TYPE_SPECIALIZER_TYPE (&dfsch_complementary_type_specializer_type)

dfsch_object_t* dfsch_make_type_specializer(dfsch_object_t* fun);
dfsch_object_t* dfsch_union_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_intersect_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_difference_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_symetric_difference_specializers(dfsch_object_t* speclist);
dfsch_object_t* dfsch_complementary_specializer(dfsch_object_t* specializer);

#endif
