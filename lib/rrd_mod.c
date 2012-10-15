#include <pthread.h>
#include <rrd.h>
#include <dfsch/dfsch.h>
#include <dfsch/load.h>
#include <dfsch/hash.h>
#include <dfsch/number.h>
#include <dfsch/strings.h>
#include <dfsch/util.h>
#include <dfsch/conditions.h>

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
  
  while (DFSCH_PAIR_P(list)){
    if (alloc <= argc){
      alloc *= 2;
      argv = GC_REALLOC(argv, sizeof(char*) * alloc);
    }

    argv[argc] = convert_arg(DFSCH_FAST_CAR(list));

    argc++;
    list = DFSCH_FAST_CDR(list);
  }
  *pargc = argc;
  *pargv = argv;
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

static pthread_mutex_t rrd_lock = PTHREAD_MUTEX_INITIALIZER;
#define RRD_ERROR_TYPE (&rrd_error_type)
static dfsch_type_t rrd_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_RUNTIME_ERROR_TYPE, "rrd-error");

static void rrd_error(char* fun){
  dfsch_object_t* c = dfsch_make_condition(RRD_ERROR_TYPE);
  dfsch_condition_put_field_cstr(c, "message", 
                                 dfsch_make_string_cstr(rrd_get_error()));
  dfsch_condition_put_field_cstr(c, "function", 
                                 dfsch_make_string_cstr(fun));  
  pthread_mutex_unlock(&rrd_lock);
  dfsch_signal(c);
}

DFSCH_DEFINE_PRIMITIVE(create,
                       "Create new RRD file"){
  int argc;
  char**argv;
  build_args(args, &argc, &argv);

  pthread_mutex_lock(&rrd_lock);
  if (rrd_create(argc, argv) == -1){
    rrd_error("create");
  }
  pthread_mutex_unlock(&rrd_lock);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(update,
                       "Write new values into RRD file"){
  int argc;
  char**argv;
  build_args(args, &argc, &argv);

  pthread_mutex_lock(&rrd_lock);
  if (rrd_update(argc, argv) == -1){
    rrd_error("update");
  }
  pthread_mutex_unlock(&rrd_lock);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(tune,
                       "Modify parameters of RRD file"){
  int argc;
  char**argv;
  build_args(args, &argc, &argv);

  pthread_mutex_lock(&rrd_lock);
  if (rrd_tune(argc, argv) == -1){
    rrd_error("tune");
  }
  pthread_mutex_unlock(&rrd_lock);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(resize,
                       "Resize an RRD file"){
  int argc;
  char**argv;
  build_args(args, &argc, &argv);

  pthread_mutex_lock(&rrd_lock);
  if (rrd_resize(argc, argv) == -1){
    rrd_error("resize");
  }
  pthread_mutex_unlock(&rrd_lock);
  
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(graph,
                       "Draw graph from data in RRD file(s)"){
  int argc;
  char**argv;
  rrd_info_t* info;
  dfsch_object_t* res;
  build_args(args, &argc, &argv);

  pthread_mutex_lock(&rrd_lock);
  if ((info = rrd_graph_v(argc, argv)) == NULL){
    rrd_error("graph");
  }
  res = convert_info(info);
  rrd_info_free(info);
  pthread_mutex_unlock(&rrd_lock);
  
  return res;
}

DFSCH_DEFINE_PRIMITIVE(info,
                       "Draw graph from data in RRD file(s)"){
  int argc;
  char**argv;
  rrd_info_t* info;
  dfsch_object_t* res;
  build_args(args, &argc, &argv);

  pthread_mutex_lock(&rrd_lock);
  if ((info = rrd_info(argc, argv)) == NULL){
    rrd_error("info");
  }
  res = convert_info(info);
  rrd_info_free(info);
  pthread_mutex_unlock(&rrd_lock);
  
  return res;
}

void dfsch_module_rrd_register(dfsch_object_t* env){
  dfsch_object_t* rrd = dfsch_make_package("rrd",
                                           "Native library based interface to RRDtool, "
                                           "see original RRDtool documentation for details");
  dfsch_provide(env, "rrd");
  
  dfsch_defcanon_pkgcstr(env, rrd, "<error>", RRD_ERROR_TYPE);

  dfsch_defcanon_pkgcstr(env, rrd, "create", 
                         DFSCH_PRIMITIVE_REF(create));
  dfsch_defcanon_pkgcstr(env, rrd, "update", 
                         DFSCH_PRIMITIVE_REF(update));
  dfsch_defcanon_pkgcstr(env, rrd, "tune", 
                         DFSCH_PRIMITIVE_REF(tune));
  dfsch_defcanon_pkgcstr(env, rrd, "resize", 
                         DFSCH_PRIMITIVE_REF(resize));
  dfsch_defcanon_pkgcstr(env, rrd, "graph", 
                         DFSCH_PRIMITIVE_REF(graph));
  dfsch_defcanon_pkgcstr(env, rrd, "info", 
                         DFSCH_PRIMITIVE_REF(info));


}
