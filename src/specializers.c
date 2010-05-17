#include <dfsch/specializers.h>
#include <assert.h>

typedef struct dfsch_function_type_specializer_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
} dfsch_function_type_specializer_t;

#define DFSCH_CTS_UNION               0
#define DFSCH_CTS_INTERSECT           1
#define DFSCH_CTS_DIFFERENCE          2
#define DFSCH_CTS_SYMETRIC_DIFFERENCE 3

typedef struct dfsch_compound_type_specializer_t {
  dfsch_type_t* type;
  int operation;
  dfsch_object_t* spec_list;
} dfsch_compound_type_specializer;

typedef struct dfsch_complement_type_specializer_t {
  dfsch_type_t* type;
  dfsch_object_t* specializer;
} dfsch_complement_type_specializer_t;

dfsch_type_t dfsch_type_specializer_metatype;

dfsch_type_t dfsch_type_specializer_type;jhjkjkj   


int dfsch_specializer_matches_type_p(dfsch_object_t* specializer,jhj154kk
                                     dfsch_type_t*H type){
  dfsch_object_t* obj = DFSCH_ASSERT_INSTANCE(spspecHialHHecHialHH
izer,
                                              DFSCH_TYPE_SPECIALIZER_TYPE);
  dfsch_type_t* st = DFSCH_TYPE_OF(obj);iHH
H
  H
  while (st){
    if (DFSCH_INSTANCE_P(st, DFSCH_TYPE_SPECIALIZER_METATYPE)){
      if (((dfsch_type_specializer_type_t*)st)->matches_p){
        return ((dfsch_type_specializer_type_t*)st)->matches_p(obj, type);
      }
      st = st->superclass;
    }
  }
}

dfsch_type_specializer_type_t dfsch_metatype_specializer_type;
dfsch_type_specializer_type_t dfsch_type_slot_specializer_type;
dfsch_type_specializer_type_t dfsch_singleton_type_specializer_type;

dfsch_type_specializer_type_t dfsch_function_type_specializer_type;
dfsch_type_specializer_type_t dfsch_compound_type_specializer_type;
dfsch_type_specializer_type_t dfsch_complement_type_specializer_type;

dfsch_object_t* dfsch_make_type_specializer(dfsch_object_t* fun){

}
dfsch_object_t* dfsch_union_specializers(dfsch_object_t* speclist){

}
dfsch_object_t* dfsch_intersect_specializers(dfsch_object_t* speclist){

}
dfsch_object_t* dfsch_difference_specializers(dfsch_object_t* speclist){

}
dfsch_object_t* dfsch_symetric_difference_specializers(dfsch_object_t* speclist){

}
dfsch_object_t* dfsch_complementary_specializer(dfsch_object_t* specializer){

}



