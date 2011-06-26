#include <dfsch/lib/ini-file.h>
#include <dfsch/strhash.h>

typedef struct file_line_t {
  char* indent;
  char* name;
  char* value;
  char* comment;
  file_line_t* next;
} file_line_t;

typedef struct section_t {
  file_line_t* first;
  file_line_t* next;
  dfsch_strhash_t entries;
} section_t;

typedef struct ini_file_t {
  dfsch_type_t* type;
  file_line_t* first;
  file_line_t* last;
  dfsch_strhash_t sections;
  ini_file_t* defaults;
} ini_file_t;

dfsch_type_t dfsch_ini_file_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "ini-file:ini-file",
  .size = sizeof(ini_file_t),
};

dfsch_object_t* dfsch_make_empty_ini_file(){
  ini_file_t* ifo = dfsch_make_object(DFSCH_INI_FILE_TYPE);

  dfsch_strhash_init(&ifo->sections);

  return ifo;
}

typedef struct ini_parser_ctx_t {
  ini_file_t* ifo;
  section_t* section;
  int line;
} ini_parser_ctx_t;

static void parser_init(ini_parser_ctx_t* ctx, ini_file_t* ifo){
  ctx->ifo = ifo;
  ctx->section = NULL;
  ctx->line = 0;
}

static char* split_comment(char* line){
  char* res;
  for(;;){
    switch (*line){
    case ';':
    case '#':
      res = dfsch_stracpy(line);
      *line = '\0';
      return res;
    case '\\':
      line++;
      if (*line == '\0'){
        return NULL;
      }
      break;
    }
    line++;
  }
  return NULL;
}

static char* split_value(char* line){
  char* res;
  for(;;){
    switch (*line){
    case '=':
      *line = '\0';
      while (*line == ' ' || *line == '\t') { 
        line++;
      }
      return dfsch_stracpy(line + 1);
    case '\\':
      line++;
      if (*line == '\0'){
        return NULL;
      }
      break;
    }
    line++;
  }
  return NULL;
}

static void line_rtrim(char* line){
  end = line + strlen(line) - 1;
  while (end != line && (*end == ' ' || *end == '\t')){
    if (end != line && *(end - 1) == '\\'){
      break;
    }
    *end = '\0';
    end--;
  }
}

static void replace_escapes(char* str){
  char* out = str;
  char* in = str;

  while (*in){
    switch (*in){
    case '\\':
      ++in;
      switch (*in){
      case 'n':
	*out = '\n';
	++out;
	++in;
	continue;
      case 'r':
	*out = '\r';
	++out;
	++in;
	continue;
      case 'a':
	*out = '\a';
	++out;
	++in;
	continue;
      case 't':
	*out = '\t';
	++out;
	++in;
	continue;
      case 'b':
	*out = '\b';
	++out;
	++in;
	continue;
      case 'v':
	*out = '\v';
	++out;
	++in;
	continue;
      case 'f':
	*out = '\f';
	++out;
	++in;
	continue;
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
	*out = *in -'0';
        ++in;
        
        if (*in >= '0' && *in <= '7'){
          *out = ((*out)<<3) | (*in - '0');
          ++in;
          if (*in >= '0' && *in <= '7'){
            *out = ((*out)<<3) | (*in - '0');
            ++in;
          }          
        }
	++out;
	continue;
      case 'x':
        {
          int i;
          *out = 0;
          ++in;
          for (i=0; i<2; i++){
            *out <<= 4;
            if (!*in){
              dfsch_error("Invalid escape sequence", NULL);
            }
            if (*in >= 'A' && *in <= 'F'){
              *out |= *in - 'A' + 10;
            }else if (*in >= 'a' && *in <= 'f'){
              *out |= *in - 'a' + 10;
            }else if (*in >= '0' && *in <= '9'){
              *out |= *in - '0';
            }else{
              dfsch_error("Invalid escape sequence", NULL);
            }
            ++in;
          }
          ++out;
          continue;
        }
      case 'u':
      case 'U':
        {
          int i;
          uint32_t u = 0;
          for (i = (*(in++) == 'U') ? 0 : 4; i<8; i++){ // XXX: clever and ugly
            u <<= 4;
            if (!*in){
              dfsch_error("Invalid escape sequence", NULL);
            }
            if (*in >= 'A' && *in <= 'F'){
              u |= *in - 'A' + 10;
            }else if (*in >= 'a' && *in <= 'f'){
              u |= *in - 'a' + 10;
            }else if (*in >= '0' && *in <= '9'){
              u |= *in - '0';
            }else{
              dfsch_error("Invalid escape sequence", NULL);
            }
            ++in;
          }

          if (u <= 0x7f){
            *(out++) = u;
          } else if (u <= 0x7ff) {
            *(out++) = 0xc0 | ((u >> 6) & 0x1f); 
            *(out++) = 0x80 | (u & 0x3f);
          } else if (u <= 0xffff) {
            *(out++) = 0xe0 | ((u >> 12) & 0x0f); 
            *(out++) = 0x80 | ((u >> 6) & 0x3f);
            *(out++) = 0x80 | (u & 0x3f);
          } else {
            *(out++) = 0xf0 | ((u >> 18) & 0x07); 
            *(out++) = 0x80 | ((u >> 12) & 0x3f);
            *(out++) = 0x80 | ((u >> 6) & 0x3f);
            *(out++) = 0x80 | (u & 0x3f);
          } 
          continue;
        }        
      case '\0':
        dfsch_error("Incomplete escape sequence", NULL);
      default:
        *out = *in;
        ++out;
        ++in;
        continue;  
      }
    default:
      *out = *in;
      ++out;
      ++in;
    }
  }
  
  *out = 0;
}

static void parse_line(ini_parser_ctx_t* ctx, char* line){
  file_line_t* ld = GC_NEW(file_line_t);
  size_t len;
  ssize_t comment_start;
  char* end;

  ctx->line++;

  len = strspn(line, " \t");

  if (len){
    ld->indent = dfsch_strancpy(line, len);
  } else {
    ld->indent = NULL;
  }

  line += len;

  line_rtrim(line);

  ld->comment = split_comment(line);
  line_rtrim(line);

  if (*line){
    if (*line == '[') {
      len = strlen(line);
      if (line[len - 1] != ']'){
        dfsch_error("Parser error: unmatched [", DFSCH_MAKE_FIXNUM(ctx->line));
      }
      ld->name = dfsch_strancpy(line + 1, len - 2);
    } else {
      ld->value = split_value(line);
      line_rtrim(line);
      ld->name = dfsch_stracpy(line);
      if (!ld->value){
        dfsch_error("Parser error: missing value", 
                    DFSCH_MAKE_FIXNUM(ctx->line));
      }
      replace_escapes(ld->value);
    }
    replace_escapes(ld->name);
  }

  if (ctx->ifo->first){
    ctx->ifo->last->next = ld;
  } else {
    ctx->ifo->first = ld;
  }
  ctx->ifo->last = ld;

  if (ld->name){
    if (ld->value){
      if (!ctx->section){
        dfsch_error("Parser error: option outside of section", 
                    DFSCH_MAKE_FIXNUM(ctx->line));
      }
      dfsch_strhash_set(&ctx->section->entries, ld->name, ld);
    } else {
      ctx->section = GC_NEW(section_t);
      ctx->section->first = ctx->section->last = ld;
      dfsch_strhash_init(&ctx->section->entries);
      dfsch_strhash_set(&ctx->ifo->sections, ld->name, ctx->section);
    }
  }

  if (ctx->section){
    ctx->section->last = ld;
  }
}

static char* read_line(FILE* stream){
  char* ret;
  char* tmp;
  size_t allocd;
  size_t offset;
  size_t i;

  ret = GC_MALLOC(512);
  allocd = 512;
  offset = 0;

  while (1){
    if(!fgets(ret + offset, allocd - offset, stream)){      
      if (feof(stream)){ /* because of EOF */
        return "";
      } else { /* because of some other failure */
        return NULL;
      }
    }

    offset += strlen(ret + offset);
    
    if (offset == 0){  /* nothing */
        return ret;
    }
    
    if (ret[offset-1] == '\n') {   /* full line */
      i = offset;

      while (i > 0 && (ret[i] == '\r' || ret[i] == '\n')){ 
        /* skip redundant CRs and LFs at end of line */
        i--;
      }

      if (ret[i] == '\\'){ /* backlash continuation */
        offset = i;
      } else {
        return ret;
      }
    }

    allocd += 512;
    tmp = GC_REALLOC(ret, allocd);
    ret = tmp;
  }
}

dfsch_object_t* dfsch_ini_file_read_file(char* fname){
  ini_file_t* ifo = dfsch_make_empty_ini_file();
  ini_parser_ctx_t ctx;

  parser_init(&ctx, ifo);

}
dfsch_object_t* dfsch_ini_file_read_port(dfsch_object_t* port){
  ini_file_t* ifo = dfsch_make_empty_ini_file();
  ini_parser_ctx_t ctx;

  parser_init(&ctx, ifo);

}

void dfsch_ini_file_set_defaults(dfsch_object_t* ifo,
                                 dfsch_object_t* defaults_ifo){
  ini_file_t* i = DFSCH_ASSERT_TYPE(ifo, DFSCH_INI_FILE_TYPE);
  ini_file_t* d = DFSCH_ASSERT_TYPE(defaults_ifo, DFSCH_INI_FILE_TYPE);
  
  i->defaults = d;
}

void dfsch_ini_file_write_file(dfsch_object_t* ifo,
                               char* fname);
void dfsch_ini_file_write_port(dfsch_object_t* ifo,
                               dfsch_object_t* port);

int dfsch_ini_file_has_section_p(dfsch_object_t* ifo,
                                 char* section);
int dfsch_ini_file_has_property_p(dfsch_object_t* ifo,
                                  char* section,
                                  char* property);

void dfsch_ini_file_add_section(dfsch_object_t* ifo,
                                char* section);
void dfsch_ini_file_add_comment(dfsch_object_t* ifo,
                                char* section,
                                char* comment);

char* dfsch_ini_file_get(dfsch_object_t* ifo,
                         char* section,
                         char* property);
void dfsch_ini_file_set(dfsch_object_t* ifo,
                        char* section,
                        char* property,
                        char* value);
