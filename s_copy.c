#include "stream.h"

#include "stdio.h"
#include <readline/readline.h>
#include <readline/history.h>
#include <gc.h>

int callback(dfsch_object_t *obj, void* baton){
  char *out = dfsch_obj_write(obj,100);
  puts(out);
  free(out);
}

int main(){
  GC_INIT();

  dfsch_parser_ctx_t *parser = dfsch_parser_create();

  puts(";; Parser created");

  dfsch_parser_callback(parser, &callback, NULL);

  rl_bind_key ('\t', rl_insert);
  
  while (1){
    char *str;
    str = readline("(> ");
    if (!str)
      break;
    add_history(str);

    dfsch_parser_feed(parser,str);
    dfsch_parser_feed(parser,"\n");

    free(str);
  }

  dfsch_parser_destroy(parser);

  return 0;
}
