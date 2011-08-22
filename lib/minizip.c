#include <dfsch/lib/minizip.h>
#include "ext/minizip/unzip.h"

typedef struct minizip_t {
  dfsch_type_t* type;
  unzFile file;
  pthread_mutex_t* mutex;
} minizip_t;

static dfsch_object_t* mz_ref(minizip_t* mz, dfsch_object_t* name_obj){
  char* name = dfsch_string_to_cstr(name_obj);
  int ret;
  unz_file_info64 info;
  dfsch_object_t* contents;
  char* buf;

  pthread_mutex_lock(mz->mutex);
  ret = unzLocateFile(mz->file, name, 1); /* Always case sensitive */
  if (ret == UNZ_END_OF_LIST_OF_FILE){
    pthread_mutex_unlock(mz->mutex);
    return DFSCH_INVALID_OBJECT;
  }
  
  unzGetCurrentFileInfo64(mz->file, &info, NULL, 0, NULL, 0, NULL, 0);

  if (unzOpenCurrentFile(mz->file) != UNZ_OK){
    pthread_mutex_unlock(mz->mutex);
    dfsch_error("Error reading from archive", mz);
  }


  contents = dfsch_alloc_byte_vector(&buf, info.uncompressed_size);
  ret = unzReadCurrentFile(mz->file, buf, info.uncompressed_size);
  if (ret != info.uncompressed_size){
    unzCloseCurrentFile(mz->file);
    pthread_mutex_unlock(mz->mutex);
    dfsch_error("Error reading from archive", mz);
  }

  ret = unzCloseCurrentFile(mz->file);
  pthread_mutex_unlock(mz->mutex);
  if (ret == UNZ_CRCERROR){
    dfsch_error("CRC error", mz);
  } else if (ret != UNZ_OK){
    dfsch_error("Error reading from archive", mz);
  }

  return contents;
}

static dfsch_mapping_methods_t mz_map = {
  .ref = mz_ref,
};

dfsch_type_t dfsch_minizip_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "minizip",
  .size = sizeof(minizip_t),
  .documentation = "ZIP file as a mapping",
  .mapping = &mz_map
};

static void mz_finalizer(minizip_t* mz, void* discard){
  unzClose(mz->file);
}

dfsch_object_t* dfsch_minizip_open(char* filename){
  minizip_t* mz = dfsch_make_object(DFSCH_MINIZIP_TYPE);

  mz->file = unzOpen64(filename);

  if (!mz->file){
    dfsch_error("Cannot open zip file", dfsch_make_string_cstr(filename));
  }

  GC_register_finalizer(mz, (GC_finalization_proc)mz_finalizer, NULL,
                        NULL, NULL);
  mz->mutex = dfsch_create_finalized_mutex();

  return (dfsch_object_t*)mz;
}
