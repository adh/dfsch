#include <dfsch/lib/cinspect.h>
#include <dfsch/dfsch.h>
#include <dfsch/lib/console.h>
#include <dfsch/introspect.h>
#include <stdio.h>

typedef struct inspector_state_t {
  dfsch_object_t* object_stack;

  dfsch_object_t* current_object;
  char* description;
  dfsch_object_t* slot_list;

  int slot_print_depth;
} inspector_state_t;

static void init_state(inspector_state_t*is){
  is->object_stack = NULL;
  is->slot_print_depth = 3;
}

static void push_object(inspector_state_t* is, dfsch_object_t* obj){
  dfsch_object_t* desc;

  is->object_stack = dfsch_cons(obj, is->object_stack);
  desc = dfsch_describe_object(obj);
  
  is->description = dfsch_string_to_cstr(dfsch_car(desc));
  is->slot_list = dfsch_cdr(desc);
}


static void redisplay_object(inspector_state_t* is){
  dfsch_object_t* i;
  size_t j;

  fprintf(stderr, "object %p is %s %s\n", 
          dfsch_car(is->object_stack),
          strchr("aeiou", is->description[0]) == NULL ? "a" : "an",
          is->description);

  i = is->slot_list;
  j = 0;
  fprintf(stderr, "\nSlots:\n");
  while (DFSCH_PAIR_P(i)){
    dfsch_object_t* slot = DFSCH_FAST_CAR(i);
    dfsch_object_t* name;
    dfsch_object_t* value;

    DFSCH_OBJECT_ARG(slot, name);
    DFSCH_OBJECT_ARG(slot, value);

    if (name){
      fprintf(stderr, "%4d: [%s] %s\n",
              j,
              dfsch_object_2_string(name, 1, 0),
              dfsch_object_2_string(value, is->slot_print_depth, 1));
    } else {
      fprintf(stderr, "%4d: %s\n",
              j,
              dfsch_object_2_string(value, is->slot_print_depth, 1));      
    }
    j++;
    i = DFSCH_FAST_CDR(i);
  }
  fprintf(stderr, "\nEnter slot index or keyword to inspect it's value.\n");
}

static void inspect_object(dfsch_object_t* obj){
  inspector_state_t is;
  init_state(&is);
  push_object(&is, obj);
  redisplay_object(&is);

}


void dfsch_cinspect_inspect_object(dfsch_object_t* obj){
  inspect_object(obj);
}

DFSCH_DEFINE_PRIMITIVE(inspect_object, ""){
  dfsch_object_t* object;
  DFSCH_OBJECT_ARG(args, object);
  DFSCH_ARG_END(args);

  inspect_object(object);

  return object;
}

dfsch_object_t* dfsch_cinspect_get_procedure(){
  return DFSCH_PRIMITIVE_REF(inspect_object);
}
void dfsch_cinspect_set_as_inspector(){
  dfsch_set_inspector(dfsch_cinspect_get_procedure());
}
