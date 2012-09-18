#include <rrd.h>
#include <dfsch/dfsch.h>
#include <dfsch/load.h>
#include <dfsch/hash.h>
#include <dfsch/number.h>
#include <dfsch/strings.h>
#include <dfsch/util.h>

static char* convert_arg(dfsch_object_t* obj){
  if (dfsch_keyword_p(obj)){
    char* str = dfsch_symbol(obj);
    if (strlen(str) == 1){
      return dfsch_saprintf("-%s", str);
    } else {
      return dfsch_saprintf("--%s", str);
    }
  } else if (dfsch_string_p(obj)) {
    return dfsch_string_to_cstr(obj);
  } else {
    return dfsch_object_2_string(obj, 10, 1);
  }
}

static void build_args(dfsch_object_t* list, int* pargc, char*** pargv){
  int alloc = 16;
  char** argv = GC_MALLOC(sizeof(char*) * alloc);
  int argc = 0;
  list = dfsch_collection_get_iterator(list);
  
  while (list){
    if (alloc <= argc){
      alloc *= 2;
      argv = GC_REALLOC(argv, sizeof(char*) * alloc);
    }

    argv[argc] = convert_arg(dfsch_iterator_this(list));

    argc++;
    list = dfsch_iterator_next(list);
  }
  
}

static dfsch_object_t* convert_info(rrd_info_t * data){
  dfsch_object_t* res = dfsch_make_idhash();
  while (data) {
    dfsch_object_t* val = NULL;
    
    switch (data->type) {
    case RD_I_VAL:
      val = isnan(data->value.u_val)
        ? NULL
        : dfsch_make_number_from_double(data->value.u_val);
      break;
    case RD_I_CNT:
      val = dfsch_make_number_from_uint64(data->value.u_cnt);
      break;
    case RD_I_INT:
      val = dfsch_make_number_from_long(data->value.u_int);
      break;
    case RD_I_STR:
      val = dfsch_make_string_cstr(data->value.u_str);
      break;
    case RD_I_BLO:
      val = dfsch_make_byte_vector((char *) data->value.u_blo.ptr,
                                  data->value.u_blo.size);
      break;
    }

    dfsch_idhash_set((dfsch_hash_t*)res, dfsch_make_keyword(data->key), val);

    data = data->next;
  }
  return res;
}


void dfsch_module_rrd_register(dfsch_object_t* env){
  dfsch_object_t* rrd = dfsch_make_package("rrd",
                                           "Native library based interface to RRDtool");
  dfsch_provide(env, "rrd");
  
}
