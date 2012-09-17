#include <rrd.h>
#include <dfsch/dfsch.h>
#include <dfsch/load.h>

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
  list = dfsch_collection_iterator(list);
  
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

void dfsch_module_rrd_register(dfsch_object_t* env){
  dfsch_package_t* rrd = dfsch_make_package("rrd",
                                            "Native library based interface to RRDtool");
  dfsch_provide(env, "rrd");
  
}
