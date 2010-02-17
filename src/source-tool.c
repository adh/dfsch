#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gc/gc.h>
#include "zlib.h"
#include <stdio.h>
#include <getopt.h>

#define UTIL_STANDALONE
#include "util.c"

char* output_name = NULL;
char* c_symbol = NULL;
int do_compress = 1;

char* load_file(char* filename){
  FILE* f = fopen(filename, "r");
  str_list_t* sl = sl_create();
  char buf[8193];

  if (!f){
    perror(filename);
    exit(1);
  }

  while(fgets(buf, 8192, f)){
    sl_append(sl, stracpy(buf));
  }

  return sl_value(sl);
}

char* compact(char* ibuf){
  char* obuf = GC_MALLOC_ATOMIC(strlen(ibuf)+1);
  char* o = obuf;
  char ch;

 start:
  ch = *ibuf;
  ibuf++;
  switch (ch){
  case ' ':
  case '\n': 
  case '\r':
  case '\t':
  case '\f':
    *o = ' ';
    o++;
    goto white_space;
  case '"':
    *o = ch;
    o++;
    goto string;
  case ';':
    goto comment;
  case '\0':
    *o = ch;
    o++;
    return obuf;
  default:
    *o = ch;
    o++;
    goto start;
  }

 comment:
  ch = *ibuf;
  ibuf++;
  switch (ch){
  case '\n': 
  case '\r':
    goto start;
  default:
    goto comment;
  }

 white_space:
  ch = *ibuf;
  ibuf++;
  switch (ch){
  case ' ':
  case '\n': 
  case '\r':
  case '\t':
  case '\f':
    goto white_space;
  case ';':
    goto comment;
  case '"':
    *o = ch;
    o++;
    goto string;
  case '\0':
    *o = ch;
    o++;
    return obuf;
  default:
    *o = ch;
    o++;
    goto start;
  }
  
 string:
  ch = *ibuf;
  *o = ch;
  o++;
  ibuf++;
  switch (ch){
  case '"':
    goto start;
  case '\\':
    goto backslash;
  case '\0':
    return obuf;
  default:
    goto string;
  }

 backslash:
  ch = *ibuf;
  *o = ch;
  o++;
  ibuf++;
  switch (ch){
  case '\0':
    return obuf;
  default:
    goto string;
  }  
}

char* load_and_compact(char* filename){
  return compact(load_file(filename));
}

void write_dsz(FILE* of, char* buf){
  size_t len = strlen(buf);
  unsigned char header_buf[20];
  unsigned char trailer_buf[12];

  size_t clen = compressBound(len);
  char* cbuf = GC_MALLOC_ATOMIC(clen);
  uint32_t cksum;

  compress(cbuf, &clen, buf, len);

  memcpy(header_buf, "DsZ0\r\n\xff\n\0\r\x80\x7f", 12);
  header_buf[12] = len >> 24;
  header_buf[13] = len >> 16;
  header_buf[14] = len >> 8;
  header_buf[15] = len >> 0;
  header_buf[16] = clen >> 24;
  header_buf[17] = clen >> 16;
  header_buf[18] = clen >> 8;
  header_buf[19] = clen >> 0;

  memcpy(trailer_buf, "DsZ!", 4);

  cksum = crc32(crc32(0, NULL, 0), buf, len);
  trailer_buf[4] = cksum >> 24;
  trailer_buf[5] = cksum >> 16;
  trailer_buf[6] = cksum >> 8;
  trailer_buf[7] = cksum >> 0;

  cksum = crc32(crc32(0, NULL, 0), cbuf, clen);
  trailer_buf[8] = cksum >> 24;
  trailer_buf[9] = cksum >> 16;
  trailer_buf[10] = cksum >> 8;
  trailer_buf[11] = cksum >> 0;


  
  fwrite(header_buf, 20, 1, of);
  fwrite(cbuf, clen, 1, of);
  fwrite(trailer_buf, 12, 1, of);
  
}

void usage(char* progname){
  printf("Usage: %s [<options>] <filename> ...\n\n", progname);
  puts("Options:");
  puts("  -o <filename>     Load scheme file on startup");
  puts("  -c <symbol-name>  Generate C source file");
  puts("  -z                Only compact source, do not compress");
  exit(0);
}

int main(int argc, char** argv){
  int c;
  str_list_t* output = sl_create();
  FILE* of;
  int i;

  while ((c=getopt(argc, argv, "co:z")) != -1){
    switch (c){
    case 'c':
      c_symbol = stracpy(optarg);
      break;
    case 'o':
      output_name = stracpy(optarg);      
      break;
    case 'z':
      do_compress = 0;      
      break;
    case 'v':
      printf("dfsch-source-tool version %s\n\n", PACKAGE_VERSION);
      puts("Copyright (C) 2005-2010 Ales Hakl");
      puts("dfsch comes with ABSOLUTELY NO WARRANTY");
      puts("This is free software, and you are welcome to redistribute it");
      puts("under certain conditions; see file COPYING for details.");
      return 0;
    default:
      usage(argv[0]);
    }
  }

  if (optind == argc){
    usage(argv[0]);
  }

  for (i = optind; i < argc; i++){
    sl_append(output, load_and_compact(argv[i]));
  }

  if (output_name){
    of = fopen(output_name, "w");
    if (!of){
      perror(output_name);
    }
  } else {
    of = stdout;
  }

  if (c_symbol){
    char* o = sl_value(output);
    fprintf(of, "char %s[] = \"", c_symbol);
  } else {
    if (do_compress){
      write_dsz(of, sl_value(output));
    } else {
      fputs(sl_value(output), of);
    }
  }


}
