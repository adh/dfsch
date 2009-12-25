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
}

char* dfsch_console_read_line(char* prompt){
  char* str;
  char* ret;
  struct sigaction oldact;
  struct sigaction act;

  act.sa_handler = rl_interrupt_function;
  act.sa_flags = 0;
  sigemptyset(&act.sa_mask);

  sigaction(SIGINT, &act, &oldact);

  str = readline(prompt);

  sigaction(SIGINT, &oldact, NULL);

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
typedef struct completion_entry_t completion_entry_t;

struct completion_entry_t {
  completion_entry_t* next;
  char* str;
};

typedef struct completion_ctx_t {
  completion_entry_t* list;
  char* text_part;
  char* package_name;
} completion_ctx_t;

static void compl_cb(completion_ctx_t* ctx, dfsch_object_t* symbol){
  dfsch__symbol_t* sym = DFSCH_TAG_REF(symbol);

  if (strncmp(sym->name, ctx->text_part, strlen(ctx->text_part)) == 0){
    completion_entry_t* e = GC_NEW(completion_entry_t);
    e->next = ctx->list;
    if (ctx->package_name){
      e->str = dfsch_saprintf("%s:%s ", ctx->package_name, sym->name);
    } else {
      e->str = dfsch_saprintf("%s ", sym->name);
    }
    ctx->list = e;
  }
}

static void parse_symbol(char* symbol,
                         char** package_name,
                         char** symbol_name){
  char* colon = strrchr(symbol, ':');

  if (!colon){
    *symbol_name = dfsch_stracpy(symbol);
    *package_name = NULL;
  } else if (colon == symbol){
    *symbol_name = dfsch_stracpy(symbol+1);
    *package_name = "";
  } else {
    *symbol_name = dfsch_stracpy(colon+1);
    *package_name = dfsch_strancpy(symbol, colon - symbol);
  }
  
}

static completion_entry_t* generate_completions(char* text_part){
  completion_ctx_t ctx;
  char* package_name;
  char* symbol_name;
  dfsch_package_t* package;

  parse_symbol(text_part, &package_name, &symbol_name);

  ctx.list = NULL;
  ctx.text_part = symbol_name;
  ctx.package_name = package_name;
  
  if (package_name){
    if (*package_name){
      package = dfsch_find_package(package_name);
    } else {
      package = DFSCH_KEYWORD_PACKAGE;
    }
    dfsch_for_package_symbols(package, compl_cb, &ctx);
  } else {
    dfsch_object_t* packages = dfsch_list_all_packages();
    while (DFSCH_PAIR_P(packages)){
      char* name = dfsch_package_name(DFSCH_FAST_CAR(packages));
      if (strncmp(name, text_part, strlen(text_part)) == 0){
        completion_entry_t* e = GC_NEW(completion_entry_t);
        e->next = ctx.list;
        e->str = dfsch_saprintf("%s:", name);
        ctx.list = e;
      }
      packages = DFSCH_FAST_CDR(packages);
    }
    

    dfsch_for_all_package_symbols(dfsch_get_current_package(), 
                                  compl_cb, &ctx);    
    
  }


  return ctx.list;
}

static char * symbol_completion_cb (const char* text, int state){
  static completion_entry_t* entries;

  if (state == 0){
    entries = generate_completions(text);
  }

  if (entries){
    char* ent = strdup(entries->str);
    entries = entries->next;
    return ent;
  }

  return ((char *)NULL);
}

static char ** symbol_completion (const char* text, int start, int end){
  return rl_completion_matches (text, symbol_completion_cb);
}
void dfsch_console_set_object_completion(){
  rl_attempted_completion_function = symbol_completion;
  rl_basic_word_break_characters = " \t\n\"()";
  rl_completion_append_character = '\0';
}
void dfsch_console_set_general_completion(){
  rl_attempted_completion_function = NULL;
  rl_basic_word_break_characters = " \t\n\"\\'`@$><=;|&{("; 
  /* that ugly string comes from readline docs, presumably it is default
   * value for this variable and also what bash uses.
   */
  rl_completion_append_character = ' ';
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
      DFSCH_WITH_SIMPLE_RESTART(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                    "abort"), "Return to reader"){
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
  dfsch_parser_set_source(parser, dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                      "*console-io*"));
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
  return 1;
}

int dfsch_console_run_repl(char* prompt, 
                           dfsch_object_t* env){
  return dfsch_console_read_objects(prompt, repl_callback, env);
}
