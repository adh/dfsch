#include <dfsch/dfsch.h>
#include "util.h"
#include "internal.h"

static char *cxr_table[][2] = {
  {"caar", "aa"},
  {"cadr", "da"},
  {"cdar", "ad"},
  {"cddr", "dd"},

  {"caaar", "aaa"},
  {"caadr", "daa"},
  {"cadar", "ada"},
  {"caddr", "dda"},
  {"cdaar", "aad"},
  {"cdadr", "dad"},
  {"cddar", "add"},
  {"cdddr", "ddd"},

  {"caaaar", "aaaa"},
  {"caaadr", "daaa"},
  {"caadar", "adaa"},
  {"caaddr", "ddaa"},
  {"cadaar", "aada"},
  {"cadadr", "dada"},
  {"caddar", "adda"},
  {"cadddr", "ddda"},
  {"cdaaar", "aaad"},
  {"cdaadr", "daad"},
  {"cdadar", "adad"},
  {"cdaddr", "ddad"},
  {"cddaar", "aadd"},
  {"cddadr", "dadd"},
  {"cdddar", "addd"},
  {"cddddr", "dddd"},

};

DFSCH_PRIMITIVE_HEAD(cxr){
  dfsch_object_t* pair;
  char* action = (char*) baton;

  DFSCH_OBJECT_ARG(args, pair);
  DFSCH_ARG_END(args);

  while(*action){
    if ((*action) == 'a'){
      pair = dfsch_car(pair);
    } else {
      pair = dfsch_cdr(pair);
    }
    action++;
  }

  return pair;
}

static char* build_doc(char* name){
  str_list_t* sl = sl_create();
  name++;
  sl_append(sl, "Return ");

  while (*name){
    switch (*name){
    case 'a':
      sl_append(sl, "car of ");
      break;
    case 'd':
      sl_append(sl, "cdr of ");
      break;
    }
    name++;
  }
  sl_append(sl, "argument");

  return sl_value(sl);
}

void dfsch__native_cxr_register(dfsch_object_t *ctx){
  int i;

  for (i=0; i < (sizeof(cxr_table)/sizeof(cxr_table[0])); i++){
    dfsch_defcanon_cstr(ctx, cxr_table[i][0],
                        DFSCH_PRIMITIVE_REF_MAKE(cxr, cxr_table[i][1], 
                                                 build_doc(cxr_table[i][0])));

  }

}
