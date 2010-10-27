#include <dfsch/specializers.h>
#include <assert.h>

typedef struct function_type_specializer_t {
  dfsch_type_t* type;
  dfsch_object_t* proc;
} function_type_specializer_t;

#define CTS_UNION               0
#define CTS_INTERSECT           1
#define CTS_DIFFERENCE          2
#define CTS_SYMETRIC_DIFFERENCE 3

typedef struct compound_type_specializer_t {
  dfsch_type_t* type;
  int operation;
  dfsch_object_t* spec_list;
} compound_type_specializer_t;

typedef struct complementary_type_specializer_t {
  dfsch_type_t* type;
  dfsch_object_t* specializer;
} complementary_type_specializer_t;

dfsch_type_t dfsch_type_specializer_metatype = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name = "type-specializer-type",
  .size = sizeof(dfsch_type_specializer_type_t),
};

dfsch_type_t dfsch_type_specializer_type = {
  .type = DFSCH_ABSTRACT_TYPE,
  .superclass = NULL,
  .name = "type-specializer",
};


int dfsch_specializer_matches_type_p(dfsch_object_t* specializer,
                                     dfsch_type_t* type){
  dfsch_object_t* obj = DFSCH_ASSERT_INSTANCE(specializer,
                                              DFSCH_TYPE_SPECIALIZER_TYPE);
  dfsch_type_t* st = DFSCH_TYPE_OF(obj);
  
  if (type == obj){
    return 1;
  }

  while (st){
    if (DFSCH_INSTANCE_P(st, DFSCH_TYPE_SPECIALIZER_METATYPE)){
      if (((dfsch_type_specializer_type_t*)st)->matches_p){
        return ((dfsch_type_specializer_type_t*)st)->matches_p(obj, type);
      }
      st = st->superclass;
    } else {
      return dfsch_superclass_p(type, obj);
    }
  }
}

dfsch_type_specializer_type_t dfsch_metatype_specializer_type;


static void singleton_write(dfsch_singleton_type_specializer_t* s, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, (dfsch_object_t*)s, "%s", s->name);
}
static int singleton_matches_p(dfsch_singleton_type_specializer_t* spec,
                              dfsch_type_t* type){
  return spec->matches_p(spec, type);
}

dfsch_type_specializer_type_t dfsch_singleton_type_specializer_type = {
  .type = {
    .type = DFSCH_TYPE_SPECIALIZER_METATYPE,
    .superclass = DFSCH_TYPE_SPECIALIZER_TYPE,
    .size = sizeof(dfsch_singleton_type_specializer_t),
    .name = "singleton-type-specializer",
    .write = singleton_write,
  },
  .matches_p = singleton_matches_p
};

static int function_matches_p(function_type_specializer_t* spec,
                              dfsch_type_t* type){
  return dfsch_apply(spec->proc, dfsch_list(1, type)) != NULL;
}

dfsch_type_specializer_type_t dfsch_function_type_specializer_type = {
  .type = {
    .type = DFSCH_TYPE_SPECIALIZER_METATYPE,
    .superclass = DFSCH_TYPE_SPECIALIZER_TYPE,
    .size = sizeof(function_type_specializer_t),
    .name = "function-type-specializer"
  },
  .matches_p = function_matches_p
};


static int compound_matches_p(compound_type_specializer_t* spec,
                              dfsch_type_t* type){
  dfsch_object_t* i =spec->spec_list;
  int res;

  switch (spec->operation){
  case CTS_UNION:
    while (DFSCH_PAIR_P(i)){
      if (dfsch_specializer_matches_type_p(DFSCH_FAST_CAR(i), type)){
        return 1;
      }
      i = DFSCH_FAST_CDR(i);
    }
    return 0;

  case CTS_INTERSECT:
    while (DFSCH_PAIR_P(i)){
      if (!dfsch_specializer_matches_type_p(DFSCH_FAST_CAR(i), type)){
        return 0;
      }
      i = DFSCH_FAST_CDR(i);
    }
    return 1;

  case CTS_DIFFERENCE:
    if (!DFSCH_PAIR_P(i)){
      dfsch_error("Set difference requires at least one set", spec);
    }

    if (!dfsch_specializer_matches_type_p(DFSCH_FAST_CAR(i), type)){
      return 0;
    }

    i = DFSCH_FAST_CDR(i);
    while (DFSCH_PAIR_P(i)){
      if (dfsch_specializer_matches_type_p(DFSCH_FAST_CAR(i), type)){
        return 0;
      }
      i = DFSCH_FAST_CDR(i);
    }
    return 1;

  case CTS_SYMETRIC_DIFFERENCE:
    res = 0;
    while (DFSCH_PAIR_P(i)){
      if (dfsch_specializer_matches_type_p(DFSCH_FAST_CAR(i), type)){
        res = !res;
      }
      i = DFSCH_FAST_CDR(i);
    }
    return res;
  }
}
dfsch_type_specializer_type_t dfsch_compound_type_specializer_type = {
  .type = {
    .type = DFSCH_TYPE_SPECIALIZER_METATYPE,
    .superclass = DFSCH_TYPE_SPECIALIZER_TYPE,
    .size = sizeof(compound_type_specializer_t),
    .name = "compound-type-specializer"
  },
  .matches_p = compound_matches_p
};



static int complementary_matches_p(complementary_type_specializer_t* spec,
                                   dfsch_type_t* type){
  return !dfsch_specializer_matches_type_p(spec->specializer, type);
}
dfsch_type_specializer_type_t dfsch_complementary_type_specializer_type = {
  .type = {
    .type = DFSCH_TYPE_SPECIALIZER_METATYPE,
    .superclass = DFSCH_TYPE_SPECIALIZER_TYPE,
    .size = sizeof(complementary_type_specializer_t),
    .name = "complementary-type-specializer"
  },
  .matches_p = complementary_matches_p
};

dfsch_object_t* dfsch_make_type_specializer(dfsch_object_t* fun){
  function_type_specializer_t* spec = dfsch_make_object(DFSCH_FUNCTION_TYPE_SPECIALIZER_TYPE);

  spec->proc = fun;

  return spec;
}
dfsch_object_t* dfsch_union_specializers(dfsch_object_t* speclist){
  compound_type_specializer_t* spec = dfsch_make_object(DFSCH_COMPOUND_TYPE_SPECIALIZER_TYPE);

  spec->operation = CTS_UNION;
  spec->spec_list = dfsch_list_copy(speclist);

  return spec;
}
dfsch_object_t* dfsch_intersect_specializers(dfsch_object_t* speclist){
  compound_type_specializer_t* spec = dfsch_make_object(DFSCH_COMPOUND_TYPE_SPECIALIZER_TYPE);

  spec->operation = CTS_INTERSECT;
  spec->spec_list = dfsch_list_copy(speclist);

  return spec;
}
dfsch_object_t* dfsch_difference_specializers(dfsch_object_t* speclist){
  compound_type_specializer_t* spec = dfsch_make_object(DFSCH_COMPOUND_TYPE_SPECIALIZER_TYPE);

  spec->operation = CTS_DIFFERENCE;
  spec->spec_list = dfsch_list_copy(speclist);

  return spec;
}
dfsch_object_t* dfsch_symetric_difference_specializers(dfsch_object_t* speclist){
  compound_type_specializer_t* spec = dfsch_make_object(DFSCH_COMPOUND_TYPE_SPECIALIZER_TYPE);

  spec->operation = CTS_SYMETRIC_DIFFERENCE;
  spec->spec_list = dfsch_list_copy(speclist);

  return spec;
}
dfsch_object_t* dfsch_complementary_specializer(dfsch_object_t* specializer){
  complementary_type_specializer_t* spec = dfsch_make_object(DFSCH_COMPLEMENTARY_TYPE_SPECIALIZER_TYPE);

  spec->specializer = specializer;

  return (dfsch_object_t*)spec;
}

DFSCH_DEFINE_SINGLETON_TYPE_SPECIALIZER(mapping){
  return DFSCH_TYPE_MAPPING_P(type);
}
DFSCH_DEFINE_SINGLETON_TYPE_SPECIALIZER(sequence){
  return DFSCH_TYPE_SEQUENCE_P(type);
}
DFSCH_DEFINE_SINGLETON_TYPE_SPECIALIZER(collection){
  if (type == DFSCH_SINGLETON_TYPE_SPECIALIZER_REF(mapping) ||
      type == DFSCH_SINGLETON_TYPE_SPECIALIZER_REF(sequence)){
    return 1;
  }
  return DFSCH_TYPE_COLLECTION_P(type);
}

void dfsch__specializers_register(dfsch_object_t* ctx){
  dfsch_defcanon_cstr(ctx, "<<collection>>", 
                      DFSCH_SINGLETON_TYPE_SPECIALIZER_REF(collection));
  dfsch_defcanon_cstr(ctx, "<<mapping>>", 
                      DFSCH_SINGLETON_TYPE_SPECIALIZER_REF(mapping));
  dfsch_defcanon_cstr(ctx, "<<sequence>>", 
                      DFSCH_SINGLETON_TYPE_SPECIALIZER_REF(sequence));
}

