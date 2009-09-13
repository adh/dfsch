#include "tests/test-macros.h"
#include <stdlib.h>
#include <gc/gc.h>

int test_alignment(){
  int i;
  for (i = 8; i < 2048; i++){
    if (((size_t)GC_MALLOC(i)) & 0x7){
      return 0;
    }
  }
  return 1;
}

int main(int argc, char**argv){
  TEST_INIT(argc, argv);
  TEST("allocation-alignment", test_alignment());
  TEST_EXIT(77);
}
