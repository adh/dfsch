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

#include <dfsch/writer.h>
#include <dfsch/eqhash.h>

#include "util.h"
#include <limits.h>
#include <stdio.h>
#include <assert.h>

struct dfsch_writer_state_t {
  dfsch_object_t object_head;
  dfsch_output_proc_t output_proc;
  void* output_baton;
  int depth;
  int readability;

  int circ_pass;
  dfsch_eqhash_t circ_hash;
  int circ_counter;
};
dfsch_type_t dfsch_writer_state_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dfsch_writer_state_t),
  "writer-state",
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "Internal class representing state of object writer"
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
  state->circ_pass = 0;

  return state;
}

void dfsch_write_object_circular(dfsch_object_t* obj,
                                 int readability,
                                 dfsch_output_proc_t proc,
                                 void* baton){
  dfsch_writer_state_t* state = 
    dfsch_make_writer_state(INT_MAX, readability, proc, baton);

  state->circ_pass = 1;
  dfsch_eqhash_init(&(state->circ_hash), 0);
  dfsch_write_object(state, obj);
  state->circ_counter = 0;
  state->circ_pass = 2;
  dfsch_write_object(state, obj);
  dfsch_invalidate_writer_state(state);
}

static void stdio_output_proc(FILE* stream, char* buf, size_t len){
  if (fwrite(buf, len, 1, stream) != 1){
    dfsch_operating_system_error("fwrite");
  }
}

void dfsch_put_object(FILE* f, dfsch_object_t* obj,
                      int max_depth, int mode){
  if (max_depth >= 0){
    dfsch_writer_state_t* state = 
      dfsch_make_writer_state(max_depth,
                              mode,
                              (dfsch_output_proc_t)stdio_output_proc,
                              f);
    dfsch_write_object(state, obj);
    dfsch_invalidate_writer_state(state);
  } else {
    dfsch_write_object_circular(obj, 
                                mode,
                                (dfsch_output_proc_t)stdio_output_proc,
                                f);
  }
}


void dfsch_invalidate_writer_state(dfsch_writer_state_t* state){
  state->output_proc = NULL;
  state->output_baton = NULL;
}
int dfsch_writer_state_print_p(dfsch_writer_state_t* state){
  return state->readability == DFSCH_PRINT;
}
int dfsch_writer_state_strict_write_p(dfsch_writer_state_t* state){
  return state->readability == DFSCH_STRICT_WRITE;
}
int dfsch_writer_state_pprint_p(dfsch_writer_state_t* state){
  return 0;
}
int dfsch_writer_state_cmark_p(dfsch_writer_state_t* state){
  return state->circ_pass == 1;
}

int dfsch_writer_get_readability(dfsch_writer_state_t* state){
  return state->readability;
}
void dfsch_writer_set_readability(dfsch_writer_state_t* state,
                                  int readability){
  state->readability = readability;
}


void dfsch_write_object(dfsch_writer_state_t* state,
                        dfsch_object_t* object){
  dfsch_type_t* type;
  char* ret;

  if (!object){
    dfsch_write_string(state, "()");
    return;
  }

  if (state->circ_pass == 1){
    if (!DFSCH_INTERNED_SYMBOL_P(object) && 
        !dfsch_number_p(object) && 
        DFSCH_TYPE_OF(object) != DFSCH_PRIMITIVE_TYPE &&
        DFSCH_TYPE_OF(object) != DFSCH_FORM_TYPE){
      if (!dfsch_eqhash_set_if_exists(&(state->circ_hash), 
                                      object, DFSCH_SYM_TRUE, NULL)){
        dfsch_eqhash_set(&(state->circ_hash), object, NULL);
      } else {
        return;
      }
    }

    if (DFSCH_PAIR_P(object)){
      dfsch_object_t* i = object;
      while (DFSCH_PAIR_P(i)){
        dfsch_write_object(state, DFSCH_FAST_CAR(i));
        i = DFSCH_FAST_CDR(i);
      }

      dfsch_write_object(state, i);
      return;
    }

  } else if (state->circ_pass == 2){
    dfsch_object_t* value = dfsch_eqhash_ref(&(state->circ_hash), object);
    
    if (value && value != DFSCH_INVALID_OBJECT){
      if (value == DFSCH_SYM_TRUE){
        dfsch_eqhash_set(&(state->circ_hash),
                         object,
                         DFSCH_MAKE_FIXNUM(state->circ_counter));
        dfsch_write_string(state, saprintf("#%d=", state->circ_counter)); 
        state->circ_counter++;
      } else {
        dfsch_write_string(state, saprintf("#%d#", 
                                           DFSCH_FIXNUM_REF(value)));
        return;
      }
    }

    if (DFSCH_PAIR_P(object)){
      dfsch_object_t* i = object;
      dfsch_write_string(state, "(");
      while (DFSCH_PAIR_P(i)){
        dfsch_write_object(state, DFSCH_FAST_CAR(i));
        i = DFSCH_FAST_CDR(i);
        if (i) {
          dfsch_write_string(state, " ");  
        }

        if (DFSCH_FIXNUM_P(dfsch_eqhash_ref(&(state->circ_hash), i))){
          break;
        }
      }
      
      if (i){  
        dfsch_write_string(state, ". ");  
        dfsch_write_object(state, i);
      }
      
      dfsch_write_string(state, ")");  
      return;
    }
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

  if (state->circ_pass != 1){
    dfsch_write_unreadable(state, object, "");
  }
}


void dfsch_write_string(dfsch_writer_state_t* state,
                        char* str){
  dfsch_write_strbuf(state, str, strlen(str));
}
void dfsch_write_strbuf(dfsch_writer_state_t* state,
                        char* str, size_t len){
  if (state->circ_pass == 1){
    return;
  }
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

void dfsch_write_unreadable_with_slots(dfsch_writer_state_t* state,
                                       dfsch_object_t* obj){
  str_list_t* sl = sl_create();
  va_list args;
  char *ret;
  dfsch_type_t* klass = DFSCH_TYPE_OF(obj);


  dfsch_write_unreadable_start(state, obj);
  
  while (klass){
    dfsch_slot_t* j = klass->slots;
    if (j){
      while (j->type){
        dfsch_write_string(state, j->name);
        dfsch_write_string(state, ": ");
        dfsch_write_object(state, dfsch_slot_ref(obj, j, 1));
        dfsch_write_string(state, " ");
        j++;
      }
    }
    klass = klass->superclass;
  }

  dfsch_write_unreadable_end(state);
}

void dfsch_write_unreadable_with_slots_method(dfsch_object_t* obj,
                                              dfsch_writer_state_t* state){
  dfsch_write_unreadable_with_slots(state, obj); // XXX
}

void dfsch_write_unreadable_start(dfsch_writer_state_t* state,
                                  dfsch_object_t* obj){
  if (state->readability == DFSCH_STRICT_WRITE){
    dfsch_error("Object has no readable representation", obj);
  }
  dfsch_write_pprint_begin(state);

  dfsch_write_string(state,
                     saprintf("#<%s ", DFSCH_TYPE_OF(obj)->name));
  dfsch_write_pprint_indent(state);
  dfsch_write_string(state,
                     saprintf("%p ", obj));
}
void dfsch_write_unreadable_end(dfsch_writer_state_t* state){
  dfsch_write_string(state, ">");
  dfsch_write_pprint_end(state);
}

void dfsch_write_pprint_newline(dfsch_writer_state_t* state){

}
void dfsch_write_pprint_indent(dfsch_writer_state_t* state){

}
void dfsch_write_pprint_begin(dfsch_writer_state_t* state){

}
void dfsch_write_pprint_end(dfsch_writer_state_t* state){

}
