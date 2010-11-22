/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   External object references
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

#include "dfsch/lib/extref.h"
#include <dfsch/number.h>
#include <pthread.h>
#include <stdint.h>
#include <gc/gc.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <string.h>


DFSCH_DEFINE_PRIMITIVE(extref_make, 0){
  dfsch_object_t* object;
  time_t timeout;
  int mode = DFSCH_EXTREF_FROMNOW;

  DFSCH_OBJECT_ARG(args, object);
  DFSCH_LONG_ARG_OPT(args, timeout, 600);
  DFSCH_FLAG_PARSER_BEGIN_ONE_OPT(args, mode);
  DFSCH_FLAG_VALUE("fromnow", DFSCH_EXTREF_FROMNOW, mode);
  DFSCH_FLAG_VALUE("refresh", DFSCH_EXTREF_REFRESH, mode);
  DFSCH_FLAG_VALUE("onceonly", DFSCH_EXTREF_ONCEONLY, mode);
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_extref_create(object, timeout, mode));
}

DFSCH_DEFINE_PRIMITIVE(extref_ref, 0){
  char* handle;

  DFSCH_STRING_ARG(args, handle);
  DFSCH_ARG_END(args);

  return dfsch_extref_ref(handle);
}

dfsch_object_t* dfsch_module_extref_register(dfsch_object_t* env){
  dfsch_package_t* extref = dfsch_make_package("extref");
  dfsch_provide(env, "extref");

  dfsch_defcanon_pkgcstr(env, extref, "make-extref", 
                         DFSCH_PRIMITIVE_REF(extref_make));
  dfsch_defcanon_pkgcstr(env, extref, "ref-extref", 
                         DFSCH_PRIMITIVE_REF(extref_ref));
  return env;
}
