#include <dfsch/backquote.h>
#include <dfsch/generate.h>

dfsch_object_t* dfsch_backquote_expand(dfsch_object_t* arg){
  if (dfsch_pair_p(arg)){
    dfsch_object_t* car = dfsch_car(arg);
    dfsch_object_t* cdr = dfsch_cdr(arg);

    if (car == DFSCH_SYM_UNQUOTE && dfsch_pair_p(cdr)){
      return dfsch_car(cdr);
    }else if (dfsch_pair_p(car)){
      if (dfsch_car(car) == DFSCH_SYM_UNQUOTE_SPLICING){
        return dfsch_immutable_list(3,
                                    dfsch_get_append_primitive(),
                                    dfsch_car(dfsch_cdr(car)),
                                    dfsch_backquote_expand(cdr));
    }
    }

    return dfsch_generate_cons(dfsch_backquote_expand(car), 
                               dfsch_backquote_expand(cdr));
  } else if (DFSCH_SYMBOL_P(arg)){
    return dfsch_generate_quote(arg);
  } else {
    return arg;
  }
}
