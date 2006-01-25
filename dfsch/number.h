/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Number manipulation routines.
 * Copyright (C) 2005 Ales Hakl
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

#ifndef H__dfsch__number__
#define H__dfsch__number__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * Makes number object from given floating-point number.
   */
  extern dfsch_object_t* dfsch_make_number_from_double(double n);
  /**
   * Returns value of given number as double.
   */
  extern double dfsch_number_to_double(dfsch_object_t *n);

  /**
   * Creates number from external representation
   */
  extern dfsch_object_t* dfsch_make_number_from_string(char* str);


#ifdef __cplusplus
}
#endif
#endif
