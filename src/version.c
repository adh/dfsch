#include "config.h"
#include "version.h"

char* dfsch_get_build_id(){
  return BUILD_ID;
}
char* dfsch_get_version(){
  return PACKAGE_VERSION;
}
