/*
 * dfsch - Scheme-like Lisp dialect
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

#include <dfsch/writer.h>

#include "util.h"

struct dfsch_writer_state_t {
  dfsch_object_t object_head;
  dfsch_output_proc_t output_proc;
  void* output_baton;
  int depth;
  int readability;
};
dfsch_type_t dfsch_writer_state_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_writer_state_t),
  "writer-state"
};

dfsch_writer_state_t* dfsch_make_writer_state(int max_depth,
                                              int readability,
                                              dfsch_output_proc_t proc,
                                              void* baton){
  dfsch_writer_state_t* state = 
    (dfsch_writer_state_t*)dfsch_make_object(DFSCH_WRITER_STATE_TYPE);

  state->output_proc = proc;
  state->output_baton = baton;
  state->depth = max_depth;
  state->readability = readability;

  return state;
}
void dfsch_invalidate_writer_state(dfsch_writer_state_t* state){
  state->output_proc = NULL;
  state->output_baton = NULL;
}
int dfsch_writer_state_print_p(dfsch_writer_state_t* state){
  return state->readability == DFSCH_PRINT;
}
int dfsch_writer_state_pprint_p(dfsch_writer_state_t* state){
  return 0;
}
int dfsch_writer_state_cmark_p(dfsch_writer_state_t* state){
  return 0;
}

void dfsch_write_object(dfsch_writer_state_t* state,
                        dfsch_object_t* object){
  dfsch_type_t* type;
  char* ret;

  if (!object){
    dfsch_write_string(state, "()");
    return;
  }

  if (state->depth==0){
    dfsch_write_string(state, "...");
    return;
  }

  type = DFSCH_TYPE_OF(object);

  while (type){
    if (type->write){
      state->depth--;
      type->write(object, state);
      state->depth++;
      return;
    }
    type = type->superclass;
  }

  dfsch_write_unreadable(state, object, "");
}


void dfsch_write_string(dfsch_writer_state_t* state,
                        char* str){
  dfsch_write_strbuf(state, str, strlen(str));
}
void dfsch_write_strbuf(dfsch_writer_state_t* state,
                        char* str, size_t len){
  if (state->output_proc){
    state->output_proc(state->output_baton, str, len);
  } else {
    dfsch_error("Stale writer-state", state);
  }
}

void dfsch_write_unreadable(dfsch_writer_state_t* state,
                            dfsch_object_t* obj, char* format, ...){
  str_list_t* sl = sl_create();
  va_list args;
  char *ret;
  va_start(args, format);

  dfsch_write_unreadable_start(state, obj);
  dfsch_write_string(state, vsaprintf(format, args)); 
  dfsch_write_unreadable_end(state);
}
void dfsch_write_unreadable_start(dfsch_writer_state_t* state,
                                  dfsch_object_t* obj){
  if (state->readability == DFSCH_STRICT_WRITE){
    dfsch_error("Object has no readable representation", obj);
  }
  dfsch_write_string(state, 
                     saprintf("#<%s %p ", DFSCH_TYPE_OF(obj)->name, obj));
}
void dfsch_write_unreadable_end(dfsch_writer_state_t* state){
  dfsch_write_string(state, ">");
}

void dfsch_write_pprint_newline(dfsch_writer_state_t* state){

}
void dfsch_write_pprint_indent(dfsch_writer_state_t* state){

}
void dfsch_write_pprint_begin(dfsch_writer_state_t* state){

}
void dfsch_write_pprint_end(dfsch_writer_state_t* state){

}
