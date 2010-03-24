/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Unicode character table compaction
 * Copyright (C) 2005-2010 Ales Hakl
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

typedef struct udata_entry_t{
  char category[3];
  int32_t upper_offset;
  int32_t lower_offset;
  int32_t title_offset;
} udata_entry_t;

#define BLOCK_NONE  0
#define BLOCK_START 1
#define BLOCK_END   2

typedef struct udata_line_t {
  uint32_t codepoint;
  char category[2];
  uint32_t upper;
  uint32_t lower;
  uint32_t title;
  int block;
} udata_line_t;

char* tokenize(char** lp){
  char* l = *lp;
  char* r;

  while (*l && *l != '\n' && *l != ';'){
    l++;
  }

  r = malloc(l - *lp + 1);
  memcpy(r, *lp, l - *lp);
  r[l - *lp] = 0;

  if (*l == ';'){
    l++;
  }
  *lp = l;
  return r;
}

int ends_with(char* s, char* e){
  if (strlen(s) < strlen(e)){
    return 0;
  }
  s += strlen(s) - strlen(e);
  return strcmp(s, e) == 0;
}

udata_line_t* parse_line(char* line){
  udata_line_t* l = malloc(sizeof(udata_line_t));
  char* s_codepoint = tokenize(&line);
  char* s_name = tokenize(&line);
  char* s_category = tokenize(&line);
  char* s_cclass = tokenize(&line);
  char* s_bdclass = tokenize(&line);
  char* s_dtype = tokenize(&line);
  char* s_ntv0 = tokenize(&line);
  char* s_ntv1 = tokenize(&line);
  char* s_ntv2 = tokenize(&line);
  char* s_bdmirror = tokenize(&line);
  char* s_oldname = tokenize(&line);
  char* s_comment = tokenize(&line);
  char* s_upper = tokenize(&line);
  char* s_lower = tokenize(&line);
  char* s_title = tokenize(&line);

  l->codepoint = strtol(s_codepoint, NULL, 16);
  memcpy(l->category, s_category, 2);
  if (*s_upper){
    l->upper = strtol(s_upper, NULL, 16);
  } else {
    l->upper = l->codepoint;
  }
  if (*s_lower){
    l->lower = strtol(s_lower, NULL, 16);
  } else {
    l->lower = l->codepoint;
  }
  if (*s_title){
    l->title = strtol(s_title, NULL, 16);
  } else {
    l->title = l->codepoint;
  }
  /*  if (strcmp(s_category, "Nd") == 0){
    l->digit = atol(s_ntv0);
    }*/

  if (ends_with(s_name, "First>")){
    l->block = BLOCK_START;
  } else if (ends_with(s_name, "Last>")){
    l->block = BLOCK_END;
  }

  return l;
}

uint32_t block_start;

udata_entry_t database[17*65536];

udata_entry_t* get_entry_for(uint32_t codepoint){
  return &(database[codepoint]);
}

void init_database(){
  uint32_t i;
  udata_entry_t* e;

  for (i = 0; i < 17*65536; i++){
      e = get_entry_for(i);
      e->category[0] = 'C';
      e->category[1] = 'n';
      e->upper_offset = 0;
      e->lower_offset = 0;
      e->title_offset = 0;    
  }
}

void process_line(udata_line_t* l){
  udata_entry_t* e;
  uint32_t i;

  switch (l->block) {
  case BLOCK_NONE:
    e = get_entry_for(l->codepoint);
    memcpy(e->category, l->category, 2);
    e->upper_offset = l->upper - l->codepoint;
    e->lower_offset = l->lower - l->codepoint;
    e->title_offset = l->title - l->codepoint;

    break;
  case BLOCK_START:
    block_start = l->codepoint;
    break;
  case BLOCK_END:
    for (i = block_start; i <= l->codepoint; i++){
      e = get_entry_for(i);
      memcpy(e->category, l->category, 2);
      e->upper_offset = 0;
      e->lower_offset = 0;
      e->title_offset = 0;
    }
    break;
  }
}

unsigned char property_type[17*65535];
udata_entry_t distinct[256];
int distinct_count = 0;

char* spinner="/-\\|";

void compact_props(){
  uint32_t i;
  int j;
  fprintf(stderr, "Compacting properties.../");

  for (i = 0; i < 17*65536; i++){
    for (j = 0; j <= distinct_count; j++){
      if (memcmp(&distinct[j], &database[i], sizeof(udata_entry_t)) == 0){
        property_type[i] = j;
        goto next;
      }
    }
    j = distinct_count;
    memcpy(&distinct[j], &database[i], sizeof(udata_entry_t));
    property_type[i] = j;
    distinct_count++;
  next:
    if ((i & 0x1f) == 0){
      fprintf(stderr, "\b%c", spinner[(i >> 5) & 0x3]);
    }
  }
  fprintf(stderr, "\bdone\n");
}

#define FIRST_SIZE 17*256
#define SECOND_SIZE 256

int table_type[FIRST_SIZE];
unsigned char tables[FIRST_SIZE][SECOND_SIZE];
int tables_count = 0;

void compact_tables(){
  uint32_t i;
  int j;
  fprintf(stderr, "Compacting tables...");

  for (i = 0; i < FIRST_SIZE; i++){
    for (j = 0; j <= tables_count; j++){
      if (memcmp(tables[j], &property_type[i*SECOND_SIZE], SECOND_SIZE) == 0){
        table_type[i] = j;
        goto next;
      }
    }
    j = tables_count;
    memcpy(tables[j], &property_type[i*SECOND_SIZE], SECOND_SIZE);
    table_type[i] = j;
    tables_count++;
  next:;
    fprintf(stderr, "\b%c", spinner[i & 0x3]);
  }
  fprintf(stderr, "\bdone\n");
}


void emit_header(FILE* f){
  fprintf(f, 
          "/* \n"
          " * This file is automatically generated from UnicodeData.txt\n"
          " * using udata-gen.c\n"
          " */\n\n");
  fprintf(f,
          "#include <stdint.h>\n"
          "typedef struct udata_entry_t{\n"
          "  char category[3];\n"
          "  int32_t upper_offset;\n"
          "  int32_t lower_offset;\n"
          "  int32_t title_offset;\n"
          "} udata_entry_t;\n\n");

  fprintf(f,
         "#define FIRST_SIZE %d\n", FIRST_SIZE);
  fprintf(f,
          "#define SECOND_SIZE %d\n\n", SECOND_SIZE);
  fprintf(f,
          "#define UDATA_ENTRY(n) \\\n"
          "   (dfsch__udata_properties[dfsch__udata_tables["
          "dfsch__udata_table_type[((n) / SECOND_SIZE) %% FIRST_SIZE]]"
          "[(n) %% SECOND_SIZE]])\n");
  fprintf(f, 
          "#define UDATA_MAX %d\n", FIRST_SIZE*SECOND_SIZE);

  fprintf(f,
          "extern unsigned char dfsch__udata_table_type[];\n");
  fprintf(f, 
          "extern unsigned char dfsch__udata_tables[][%d];\n", SECOND_SIZE);
  fprintf(f, 
          "extern udata_entry_t dfsch__udata_properties[];");
  
}

void emit_tables(FILE* f){
  uint32_t i;
  int j;
  int k;

  fprintf(f, 
          "/* \n"
          " * This file is automatically generated from UnicodeData.txt\n"
          " * using udata-gen.c\n"
          " */\n\n");
  fprintf(f,
          "#include \"udata.h\"\n");

  fprintf(f,
          "unsigned char dfsch__udata_table_type[] = {\n");
  for (i = 0; i < FIRST_SIZE/8; i++){
    fprintf(f, " ");
    for (k = 0; k < 8; k++){
      fprintf(f, " %5d,", table_type[(i*8)+k]);      
    }
    fprintf(f, "\n");
  }

  fprintf(f, "};\n\n");
  fprintf(f, "unsigned char dfsch__udata_tables[][%d] = {\n", SECOND_SIZE);
  for (i = 0; i < tables_count; i++){
    fprintf(f, "  {\n");
    for (j = 0; j < SECOND_SIZE / 8; j++) {
      fprintf(f, "   ");
      for (k = 0; k < 8; k++){
        fprintf(f, " %3d,", tables[i][(j*8)+k]);      
      }
      fprintf(f, "\n");
    }
    fprintf(f, "  },\n");
  }
  fprintf(f, "};\n\n");
  fprintf(f, "udata_entry_t dfsch__udata_properties[] = {\n");
  for (i = 0; i < distinct_count; i++){
    fprintf(f,
            "  {\"%c%c\", %d, %d, %d},\n", 
            distinct[i].category[0], distinct[i].category[1],
            distinct[i].upper_offset, distinct[i].lower_offset,
            distinct[i].title_offset);
  }
  fprintf(f, "};\n");
}

int main(int argc, char**argv){
  char buf[1024];
  FILE* in;
  FILE* out;

  if (argc == 4){
    in = fopen(argv[2], "r");
    if (!in){
      perror(argv[2]);
      return 1;
    }
    out = fopen(argv[3], "w");
    if (!out){
      perror(argv[3]);
      return 1;
    }
    if (strcmp(argv[1], "data") == 0){
      init_database();
      fprintf(stderr, "Parsing...");  
      while(fgets(buf, 1024, in)){
        process_line(parse_line(buf));
      }
      fprintf(stderr, "done\n");
      compact_props();
      compact_tables();
      emit_tables(out);
      fclose(out);
      return 0;
    } else if (strcmp(argv[1], "header") == 0){
      emit_header(out);
      fclose(out);
      return 0;
    }
  }
   
  fprintf(stderr, "usage: %s header|data <input-file> <output-file>\n", argv[0]);
  return 1;
}
