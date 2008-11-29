/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Object system
 * Copyright (C) 2007, 2008 Ales Hakl
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

#ifndef H__dfsch__object__
#define H__dfsch__object__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  extern dfsch_type_t dfsch_class_type;
#define DFSCH_CLASS_TYPE (&dfsch_class_type)

  dfsch_object_t* dfsch_make_class(dfsch_object_t* superclass,
                                   char* name,
                                   dfsch_object_t* slots);
  dfsch_object_t* dfsch_make_instance(dfsch_object_t* klass,
                                      dfsch_object_t* args);
  dfsch_object_t* dfsch_find_method(dfsch_object_t* klass, 
                                    dfsch_object_t* selector);
  int dfsch_responds_to_p(dfsch_object_t* klass, 
                          dfsch_object_t* selector);
  dfsch_object_t* dfsch_perform_tr(dfsch_object_t* klass,
                                   dfsch_object_t* selector,
                                   dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc);
  dfsch_object_t* dfsch_perform(dfsch_object_t* klass,
                                dfsch_object_t* selector,
                                dfsch_object_t* args);
  dfsch_object_t* dfsch_send_tr(dfsch_object_t* obj, 
                                dfsch_object_t* selector,
                                dfsch_object_t* args,
                                dfsch_tail_escape_t* esc);
  dfsch_object_t* dfsch_send(dfsch_object_t* obj, 
                             dfsch_object_t* selector,
                             dfsch_object_t* args);



  dfsch_object_t* dfsch_s_equal_instance_p();
  dfsch_object_t* dfsch_s_write_instance();
  dfsch_object_t* dfsch_s_apply_instance();
  dfsch_object_t* dfsch_s_instance_hash();
  dfsch_object_t* dfsch_s_initialize_instance();

#ifdef __cplusplus
}
#endif

#endif
