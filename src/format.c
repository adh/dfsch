/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Format implementation
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

#include "dfsch/format.h"

#include <dfsch/strings.h>

#include "util.h"
#include "internal.h"

#include <string.h>

#define FLAG_COLON 1
#define FLAG_AT    2

static int read_num_arg(char**string){
  int neg = 0;
  int res = 0;

  if (**string == '-'){
    neg = 1;
    (*string)++;
  }else if (**string == '+'){
    (*string)++;
  }

 next:
  switch(**string){
  case '\0':
    dfsch_error("exception:incomplete-format-sequence", NULL);
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    res *= 10;
    res += **string - '0';
    (*string)++;
    goto next;
  }

  if (neg){
    return -res;
  } else {
    return res;
  }
}

#define ARG_MAX 16

static char* format_a(int flags, int argc, int*argv, dfsch_object_t* obj){
  if (argc == 0){
    return dfsch_obj_write(obj, 1000, 0);
  } else if (argc == 1){
    return dfsch_obj_write(obj, argv[0], 0);
  } else {
    dfsch_error("exception:too-many-arguments-to-format-sequence",
                NULL);
  }
}
static char* format_s(int flags, int argc, int*argv, dfsch_object_t* obj){
  if (argc == 0){
    return dfsch_obj_write(obj, 1000, 1);
  } else if (argc == 1){
    return dfsch_obj_write(obj, argv[0], 1);
  } else {
    dfsch_error("exception:too-many-arguments-to-format-sequence",
                NULL);
  }
}

typedef struct format_list_t {
  size_t cur_pos;
  dfsch_object_t* head;
  dfsch_object_t* cur;
} format_list_t;

static format_list_t* make_format_list(dfsch_object_t* list){
  format_list_t* l = GC_NEW(format_list_t);

  l->cur_pos = 0;
  l->head = list;
  l->cur = list;

  return l;
}

static dfsch_object_t* list_get(format_list_t* l){
  dfsch_object_t* ret;
  
  if (!l->cur){
    dfsch_error("exception:no-more-arguments", NULL);
  }

  ret = dfsch_car(l->cur);
  l->cur = dfsch_cdr(l->cur);
  l->cur_pos++;

  return ret;
}
static dfsch_object_t* list_peek(format_list_t* l){
  dfsch_object_t* ret;
  
  if (!l->cur){
    dfsch_error("exception:no-more-arguments", NULL);
  }

  ret = dfsch_car(l->cur);
  return ret;
}
static void list_skip(format_list_t* l, size_t count){
  while (l->cur){
    if (count == 0){
      return;
    }
    count--;
    l->cur = dfsch_cdr(l->cur);
    l->cur_pos++;
  }
  dfsch_error("exception:no-more-arguments", NULL);
}
static void list_seek(format_list_t* l, size_t count){
  l->cur = l->head;
  l->cur_pos = 0;
  while (l->cur){
    if (count == 0){
      return;
    }
    count--;
    l->cur = dfsch_cdr(l->cur);
    l->cur_pos++;
  }
  dfsch_error("exception:no-more-arguments", NULL);
}
static void list_backskip(format_list_t* l, size_t count){
  list_seek(l, l->cur_pos - count);
}

typedef struct format_stack_t format_stack_t;

struct format_stack_t {
  format_list_t* args;
  char* start;
  char type;
  format_stack_t* next;
};

static format_stack_t* format_push(format_stack_t* prev, 
                                   char* start, char type,
                                   format_list_t* args){
  format_stack_t* state = GC_NEW(format_stack_t);

  state->next = prev;
  state->start = start;
  state->type = type;
  state->args = args;

  return state;
}

char* dfsch_format(char* string, 
                   dfsch_object_t* args){
  int argc;
  int argv[ARG_MAX];
  int neg;
  size_t l;
  str_list_t* out = sl_create(); 
  int flags;
  format_stack_t* state = format_push(NULL, 
                                      string, '\0', make_format_list(args));

  while (*string){
    if (*string == '~'){
      string++;
      flags = 0;
      argc = 0;

      if (!*string){
        dfsch_error("exception:incomplete-format-sequence", NULL);
      }

      if (strchr("0123456789'\"#v,", *string)){
        argc = 1;
        while(1) {
          switch (*string){
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
          case '-':
          case '+':
            argv[argc-1] = read_num_arg(&string);
            break;
          case '#':
            argv[argc-1] = 7331;
            string++;
            break;
          case 'v':
          case 'V':
            argv[argc-1] = 1337;
            string++;
            break;
          case ',':
            string++;
            break;
          default:
            argv[argc-1] = 0;          
            string++;
            break;
          }
          
          if (*string == ','){
            argc++;
            if (argc == ARG_MAX){
              dfsch_error("exception:too-many-arguments-to-format-sequence",
                          NULL);
            }
            string++;
            continue;
          } else {
            break;
          }
        }
        
      }

    flags:
      if (!*string){
        dfsch_error("exception:incomplete-format-sequence", NULL);
      }
      if (*string == ':'){
        flags |= FLAG_COLON;
        string++;
        goto flags;
      }
      if (*string == '@'){
        flags |= FLAG_AT;
        string++;
        goto flags;
      }
      
      /* End of arguments and flags */

      switch(*string){
      case '\0':
        dfsch_error("exception:incomplete-format-sequence", NULL);
      case 'a':
      case 'A':
        sl_append(out, format_a(flags, argc, argv, list_get(state->args)));
        break;
      case 's':
      case 'S':
        sl_append(out, format_s(flags, argc, argv, list_get(state->args)));
        break;
      case '*':
        if (argc == 0){
          argc = 1;
          if (flags == FLAG_AT){
            argv[0] = 0;
          } else {
            argv[0] = 1;
          }
        }
        if (argc != 1){
          dfsch_error("exception:too-many-arguments-to-format-sequence",
                      NULL);
        }

        switch (flags){
        case 0:
          list_skip(state->args, argv[0]);
          break;
        case FLAG_COLON:
          list_backskip(state->args, argv[0]);
          break;
        case FLAG_AT:
          list_seek(state->args, argv[0]);
          break;
        default:
          dfsch_error("exception:unsupported-flag-combination",
                      NULL);          

        }

      }
      
      string++;

    } else {
      l = strcspn(string, "~");
      sl_nappend(out, string, l);
      string += l;
    }
  }

  return sl_value(out);
}

static dfsch_object_t* native_format(void *baton, 
                                     dfsch_object_t* args, 
                                     dfsch_tail_escape_t* esc){
  char* format;
  DFSCH_STRING_ARG(args, format);

  return dfsch_make_string_cstr(dfsch_format(format, args));
}

void dfsch__format_native_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "format", 
                    dfsch_make_primitive(&native_format,NULL));  
}
