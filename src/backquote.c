/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2009 Ales Hakl
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
