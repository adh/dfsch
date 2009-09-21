/*
 * dfsch - Scheme-like Lisp dialect
 *   Generic functions
 * Copyright (C) 2005-2009 Ales Hakl
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


dfsch_type_t dfsch_generic_function_type_type = {
  .type = DFSCH_META_TYPE,
  .superclass = DFSCH_STANDARD_TYPE,
  .name = "generic-function-type",
  .size = sizeof(dfsch_generic_type_t),
};

dfsch_type_t dfsch_generic_function_type = {
  .type = DFSCH_ABSTRACT_TYPE,
  .superclass = DFSCH_FUNCTION_TYPE,
  .name = "generic-function"
};

dfsch_generic_function_type_t dfsch_standard_generic_function_type = {
  .super = {
    .type = DFSCH_GENERIC_FUNCTION_TYPE_TYPE,
    .superclass = DFSCH_GENERIC_FUNCTION_TYPE,
    .name = "standard-generic-function",
    .size = sizeof(standard_generic_function_t),
    .apply = apply_standard_generic_function,
  }

  .add_method = standard_generic_function_add_method,
  .remove_method = standard_generic_function_remove_method,
  .methods = standard_generic_function_methods
};


dfsch_generic_function_type_t dfsch_singleton_generic_function_type = {
  .super = {

  }
};





dfsch_type_t dfsch_method_type = {

}

dfsch_object_t* dfsch_make_method(dfsch_object_t* name,
                                  dfsch_object_t* specializers,
                                  dfsch_object_t* function){

}
