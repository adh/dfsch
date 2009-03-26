/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Garbage collector state
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "dfsch/lib/gcollect.h"
#include <dfsch/number.h>
#include <dfsch/load.h>


DFSCH_DEFINE_PRIMITIVE(gcollect, 0){
  DFSCH_ARG_END(args);

  GC_gcollect();

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(heap_size, 0){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_heap_size());
}

DFSCH_DEFINE_PRIMITIVE(free_bytes, 0){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_free_bytes());
}

DFSCH_DEFINE_PRIMITIVE(bytes_since_gc, 0){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_bytes_since_gc());
}

DFSCH_DEFINE_PRIMITIVE(total_bytes, 0){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_get_total_bytes());
}


DFSCH_DEFINE_PRIMITIVE(count, 0){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(GC_gc_no);
}

DFSCH_DEFINE_PRIMITIVE(enable, 0){
  DFSCH_ARG_END(args);
  GC_enable();
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(disable, 0){
  DFSCH_ARG_END(args);
  GC_disable();
  return NULL;
}

dfsch_object_t* dfsch_module_gcollect_register(dfsch_object_t* env){
  dfsch_provide(env, "gcollect");

  dfsch_define_cstr(env, "gcollect:gcollect!",
                    DFSCH_PRIMITIVE_REF(gcollect));
  dfsch_define_cstr(env, "gcollect:heap-size",
                    DFSCH_PRIMITIVE_REF(heap_size));
  dfsch_define_cstr(env, "gcollect:free-bytes",
                    DFSCH_PRIMITIVE_REF(free_bytes));
  dfsch_define_cstr(env, "gcollect:bytes-since-gc",
                    DFSCH_PRIMITIVE_REF(bytes_since_gc));
  dfsch_define_cstr(env, "gcollect:total-bytes",
                    DFSCH_PRIMITIVE_REF(total_bytes));
  dfsch_define_cstr(env, "gcollect:count",
                    DFSCH_PRIMITIVE_REF(count));
  dfsch_define_cstr(env, "gcollect:enable!",
                    DFSCH_PRIMITIVE_REF(enable));
  dfsch_define_cstr(env, "gcollect:disable!",
                    DFSCH_PRIMITIVE_REF(disable));

  return NULL;
}
