#include "dfsch/pprint.h"

#include "util.h"

/*
 * this is just a placeholder.
 */

char* dfsch__pprint(dfsch_object_t* object, 
                    int margin_l, int margin_r, 
                    int depth){
  return stracat(dfsch_obj_write(object, depth, 1), "\n");
}
