/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2011 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <dfsch/backquote.h>
#include <dfsch/generate.h>

static dfsch_object_t* backquote_nested(dfsch_object_t* arg){
  if (dfsch_pair_p(arg)){
    dfsch_object_t* car = dfsch_car(arg);
    dfsch_object_t* cdr = dfsch_cdr(arg);

    if (car == DFSCH_SYM_UNQUOTE || car == DFSCH_SYM_UNQUOTE_SPLICING){
      return dfsch_generate_cons(dfsch_generate_quote(car),
                                 dfsch_backquote_expand(cdr));
    }

    return dfsch_generate_cons(backquote_nested(car), 
                               backquote_nested(cdr));
  } else if (DFSCH_SYMBOL_P(arg)){
    return dfsch_generate_quote(arg);
  } else {
    return arg;
  }  
}

dfsch_object_t* dfsch_backquote_expand(dfsch_object_t* arg){
  if (dfsch_pair_p(arg)){
    dfsch_object_t* car = dfsch_car(arg);
    dfsch_object_t* cdr = dfsch_cdr(arg);

    if (car == DFSCH_SYM_UNQUOTE && dfsch_pair_p(cdr)){
      return dfsch_car(cdr);
    } else if (car == DFSCH_SYM_QUASIQUOTE || 
               car == DFSCH_SYM_IMMUTABLE_QUASIQUOTE) {
      return backquote_nested(arg);
    } else if (dfsch_pair_p(car)){
      if (dfsch_car(car) == DFSCH_SYM_UNQUOTE_SPLICING){
        if (cdr){
          return dfsch_immutable_list(3,
                                      dfsch_get_append_primitive(),
                                      dfsch_car(dfsch_cdr(car)),
                                      dfsch_backquote_expand(cdr));
        } else {
          return dfsch_car(dfsch_cdr(car));
        }
      } else if (dfsch_car(car) == DFSCH_SYM_UNQUOTE_NCONCING){
        if (cdr){
          return dfsch_immutable_list(3,
                                      dfsch_get_nconc_primitive(),
                                      dfsch_car(dfsch_cdr(car)),
                                      dfsch_backquote_expand(cdr));
        } else {
          return dfsch_car(dfsch_cdr(car));
        }
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

static dfsch_object_t* bqi_process_list(dfsch_object_t* l){
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  
  while (DFSCH_PAIR_P(l)){
    dfsch_object_t* el = DFSCH_FAST_CAR(l);

    if (el == DFSCH_SYM_UNQUOTE){
      l = DFSCH_FAST_CDR(l);
      if (!DFSCH_PAIR_P(l)){
        dfsch_error("Unquote without value", NULL);
      }
      dfsch_list_collect(lc, DFSCH_FAST_CAR(l));
      if (DFSCH_FAST_CDR(l)){
        dfsch_error("Too many expressions after CDR-positioned unquote", NULL);
      }
      break;
    }

    if (DFSCH_PAIR_P(el)){
      dfsch_object_t* car = DFSCH_FAST_CAR(el);
      if (car == DFSCH_SYM_UNQUOTE_SPLICING ||
          car == DFSCH_SYM_UNQUOTE_NCONCING){
        dfsch_list_collect(lc, dfsch_car(DFSCH_FAST_CDR(el)));
        l = DFSCH_FAST_CDR(l);
        continue;
      }
    }
    dfsch_list_collect(lc, 
                       dfsch_generate_cons(dfsch_backquote_expand_immutable(el), NULL));

    l = DFSCH_FAST_CDR(l);
  }

  return dfsch_generate_append_immutable(dfsch_list_copy_immutable(dfsch_collected_list(lc)));
}

dfsch_object_t* dfsch_backquote_expand_immutable(dfsch_object_t* arg){
  if (DFSCH_PAIR_P(arg)){
    dfsch_object_t* car = DFSCH_FAST_CAR(arg);
    dfsch_object_t* cdr = DFSCH_FAST_CDR(arg);
    
    if (car == DFSCH_SYM_UNQUOTE) {
      return dfsch_car(cdr);
    } else if (car == DFSCH_SYM_QUASIQUOTE ||
               car == DFSCH_SYM_IMMUTABLE_QUASIQUOTE){
      return backquote_nested(arg);
    }
    
    return bqi_process_list(arg);
  } else if (DFSCH_SYMBOL_P(arg)){
    return dfsch_generate_quote(arg);
  } else {
    return arg;
  }
}
