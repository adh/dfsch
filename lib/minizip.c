#include <dfsch/lib/minizip.h>
#include "ext/minizip/unzip.h"

typedef struct minizip_t {
  dfsch_type_t* type;
  unzFile file;
} minizip_t;

dfsch_type_t dfsch_minizip_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "minizip",
  .size = sizeof(minizip_t),
  .documentation = "ZIP file as a mapping"
};

static void mz_finalizer(minizip_t* mz, void* discard){
  unzClose(mz->file);
}

dfsch_object_t* dfsch_minizip_open(char* filename){
  minizip_t* mz = dfsch_make_object(DFSCH_MINIZIP_TYPE);

  mz->file = unzOpen(filename);

  if (!mz->file){
    dfsch_error("Cannot open zip file", dfsch_make_string_cstr(filename));
  }

  GC_register_finalizer(mz, (GC_finalization_proc)mz_finalizer, NULL,
                        NULL, NULL);

  return (dfsch_object_t*)mz;
}
