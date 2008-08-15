#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "dfsch/lib/console.h"
#include <dfsch/util.h>
#include <dfsch/parse.h>
#include <dfsch/ports.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif


#ifdef USE_READLINE
char* dfsch_console_read_line(char* prompt){
  char* str;
  char* ret;
  str = readline(prompt);
  if (!str){
    return NULL;
  }
  add_history(str);
  ret = dfsch_stracpy(str);
  free(str);
  return ret;
}
#else
char* dfsch_console_read_line(char* prompt){
  char* ret;
  char* tmp;
  size_t allocd;
  size_t offset;
  size_t i;

  ret = GC_MALLOC(512);
  allocd = 512;
  offset = 0;

  fputs(prompt, stdout);

  while (1){
    if(!fgets(ret + offset, allocd - offset, stdin)){      
      return NULL;
    }

    offset += strlen(ret + offset);
    
    if (offset == 0){  /* nothing */
        return ret;
    }
    
    if (ret[offset-1] == '\n') {   /* full line */
      ret[offset-1] = '\0';
      return ret;
    }

    allocd += 512;
    ret = GC_REALLOC(ret, allocd);
  }
}
#endif

#ifdef USE_READLINE
static char * symbol_completion_cb (const char* text, int state){
  char *name;
  static dfsch_symbol_iter_t* iter;
  int len;


  if (state==0)
    iter = NULL;

  len = strlen(text);

  while (name = dfsch_get_next_symbol(&iter)){
    if (strncmp (name, text, len) == 0){
      return (char*)strdup(name);
    }
  }
  
  /* If no names matched, then return NULL. */
  return ((char *)NULL);
}

static char ** symbol_completion (const char* text, int start, int end){
  return rl_completion_matches (text, symbol_completion_cb);
}
void dfsch_console_set_object_completion(){
  rl_attempted_completion_function = symbol_completion;
  rl_basic_word_break_characters = " \t\n\"()";
}
void dfsch_console_set_general_completion(){
  rl_attempted_completion_function = NULL;
  rl_basic_word_break_characters = " \t\n\"\\'`@$><=;|&{("; 
  /* that ugly string comes from readline docs, presumably it is default
   * value for this variable and also what bash uses.
   */
}
#else
/* Do nothing */
void dfsch_console_set_object_completion(){
}
void dfsch_console_set_general_completion(){
}
#endif

static int read_callback(dfsch_object_t* obj, dfsch_object_t** res){
  *res = obj;
  return 0;
}

static char* get_prompt(dfsch_parser_ctx_t* parser, char* dprompt){
  int level = dfsch_parser_get_level(parser);
  if (level){
    char *prompt;
    int fill;
    if (strlen(dprompt) < 2){
      fill = level*2;
    } else {
      fill = level*2 + (strlen(dprompt) - 4);
      /* minimal fill is 0, minimal level 1, thus 1 * 2 + (2 - 4) = 0 */
    }
    prompt = GC_MALLOC_ATOMIC(fill+5);
    memset(prompt, ' ', fill);
    prompt[fill+0] = '.';
    prompt[fill+1] = '.';
    prompt[fill+2] = '>';
    prompt[fill+3] = ' ';
    prompt[fill+4] = 0;
    return prompt;
  }else{
    return dprompt;
  }
}

dfsch_object_t* dfsch_console_read_object(char* prompt){
  dfsch_parser_ctx_t* parser = dfsch_parser_create();
  char* line;
  dfsch_object_t* obj;

  dfsch_parser_callback(parser, read_callback, &obj);
  dfsch_console_set_object_completion();
  while (line = dfsch_console_read_line(get_prompt(parser, prompt))){
    if (dfsch_parser_feed(parser, line)){
      return obj;
    }
    if (dfsch_parser_feed(parser, "\n")){
      return obj;
    }
  }
  dfsch_console_set_general_completion();
  if (dfsch_parser_top_level(parser)){
    return dfsch_eof_object();
  } else {
    dfsch_error("Unexpected end of input", NULL);
  }
}

int dfsch_console_read_objects(dfsch_console_object_cb_t cb,
                               void* baton){
  dfsch_console_set_object_completion();
  

  dfsch_console_set_general_completion();
}
int dfsch_console_read_objects_list(dfsch_console_object_cb_t cb,
                                    void* baton){
  dfsch_console_set_object_completion();
  

  dfsch_console_set_general_completion();
}
int dfsch_console_run_repl(dfsch_object_t* env){

}
