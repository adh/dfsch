#include <dfsch/lib/cinspect.h>
#include <dfsch/dfsch.h>
#include <dfsch/lib/console.h>
#include <dfsch/introspect.h>

typedef struct inspector_state_t {
  dfsch_object_t* object_stack;

  dfsch_object_t* current_object;
  char* description;
  dfsch_object_t* slot_list;

  int slot_print_depth;
} inspector_state_t*;

static void push_object(inspector_state_t* is, dfsch_object_t* obj){
  
}


static void redisplay_object(inspector_state_t* is){
  
}

static void inspect_object(dfsch_object_t* obj){
  
}


void dfsch_cinspect_inspect_object(dfsch_object_t* obj){
  inspect_object(obj);
}
