#include <dfsch/dfsch.h>
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

static dfsch_object_t* native_cxr(void *baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
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




#define CxR_DEFINE(sym) \


void dfsch__native_cxr_register(dfsch_ctx_t *ctx){
  int i;

  for (i=0; i < (sizeof(cxr_table)/sizeof(cxr_table[0])); i++){
    dfsch_ctx_define(ctx, cxr_table[i][0],
                     dfsch_make_primitive(&native_cxr,cxr_table[i][1]));

  }

}
