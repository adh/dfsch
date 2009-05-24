#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "dfsch/lib/console.h"
#include <dfsch/util.h>
#include <dfsch/parse.h>
#include <dfsch/ports.h>
#include <dfsch/magic.h>

#ifdef USE_READLINE
#include <signal.h>
#include <readline/readline.h>
#include <readline/history.h>
#endif

typedef void (*sighandler_t)(int);

static dfsch_parser_ctx_t* volatile running_parser = NULL;
static char* volatile running_prompt = NULL;
#ifdef USE_READLINE
/* this is brain-bamaged and completely unsafe, but in line with readline 
 * documentation... go figure :) 
 */
static int rl_interrupt_function(int sig){
  if (running_parser){
    dfsch_parser_reset(running_parser);
  }
  rl_set_prompt(running_prompt);
  rl_replace_line("", 1);
  rl_redisplay();

  signal(SIGINT, rl_interrupt_function);
}

char* dfsch_console_read_line(char* prompt){
  char* str;
  char* ret;
  sighandler_t sh;

  sh = signal(SIGINT, rl_interrupt_function);
  str = readline(prompt);
  signal(SIGINT, sh);
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

char* dfsch_console_read_line_object(char* prompt){
  char* line;
  dfsch_console_set_object_completion();
  line = dfsch_console_read_line(prompt);
  dfsch_console_set_general_completion();
  return line;
}

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
  running_prompt = prompt;
  running_parser = parser;

  while (line = dfsch_console_read_line_object(get_prompt(parser, prompt))){
    if (dfsch_parser_feed_line(parser, line)){
      running_parser = NULL;
      running_prompt = NULL;
      return obj;
    }
  }

  running_parser = NULL;
  running_prompt = NULL;
  if (dfsch_parser_top_level(parser)){
    return dfsch_eof_object();
  } else {
    dfsch_error("Unexpected end of input", NULL);
  }
}

int dfsch_console_read_objects_parser(char* prompt,
                                      dfsch_parser_ctx_t* parser){
  int ret;
  char* line;

  running_prompt = prompt;
  __asm(";;");
  running_parser = parser;
  DFSCH_UNWIND {
    while (line = dfsch_console_read_line_object(get_prompt(parser, prompt))){
      DFSCH_WITH_SIMPLE_RESTART(dfsch_make_symbol("abort"), "Return to reader"){
        ret = dfsch_parser_feed_line(parser, line);
      }DFSCH_END_WITH_SIMPLE_RESTART;
      if (ret){
        break;
      }
    }
  } DFSCH_PROTECT {
    running_parser = NULL;
    running_prompt = NULL;
  } DFSCH_PROTECT_END;
  return ret;
}
int dfsch_console_read_objects_list_parser(char* prompt,
                                           dfsch_parser_ctx_t* parser){

}


int dfsch_console_read_objects(char* prompt,
                               dfsch_console_object_cb_t cb,
                               void* baton){
  dfsch_parser_ctx_t* parser = dfsch_parser_create();
  dfsch_parser_set_source(parser, dfsch_make_symbol("*console-io*"));
  dfsch_parser_callback(parser, cb, baton);
  return dfsch_console_read_objects_parser(prompt, parser);
}
int dfsch_console_read_objects_list(char* prompt,
                                    dfsch_console_object_cb_t cb,
                                    void* baton){
}

static int repl_callback(dfsch_object_t *obj, void *baton){
  dfsch_object_t* ret;
  ret = dfsch_eval(obj, baton);
  puts(dfsch_object_2_string(ret,100,1));
}

int dfsch_console_run_repl(char* prompt, 
                           dfsch_object_t* env){
  return dfsch_console_read_objects(prompt, repl_callback, env);
}
