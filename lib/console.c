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
struct dfsch_console_repl_command_t {
  char* name;
  char* doc;
  void (*exec)(char* cmdline, void* baton);
  void* baton;
  dfsch_console_repl_command_t* next;
};

static dfsch_parser_ctx_t* volatile running_parser = NULL;
static char* volatile running_prompt = NULL;

static dfsch_console_repl_command_t* running_cmds = NULL;

#ifdef USE_READLINE
/* this is brain-bamaged and completely unsafe, but in line with readline 
 * documentation... go figure :) 
 */

static int complete_into_packages = 0;

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
    if (package){
      if (complete_into_packages){
        dfsch_for_package_symbols(package, compl_cb, &ctx);
      } else {
        dfsch_object_t* exp = dfsch_package_exported_symbols(package);
        if (!exp){
          dfsch_for_package_symbols(package, compl_cb, &ctx);
        } else {
          while (DFSCH_PAIR_P(exp)){
            compl_cb(&ctx, DFSCH_FAST_CAR(exp));
            exp = DFSCH_FAST_CDR(exp);
          }
        }
      }
    }
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

static char* command_completion_cb (const char*  text, int state){
  static dfsch_console_repl_command_t* i = NULL;

  if (state == 0){
    i = running_cmds;
  }

  while (i){
    if (strncmp(i->name, text, strlen(text)) == 0){
      char* name = strdup(i->name);
      i = i->next;
      return name;
    }
    i = i->next;
  }

  return NULL;
}

static char ** symbol_completion (const char* text, int start, int end){
  if (rl_line_buffer[0] == ';'){
    return rl_completion_matches(text, command_completion_cb);
  } else {
    rl_completion_suppress_append = 1;
    return rl_completion_matches(text, symbol_completion_cb);
  }
}
void dfsch_console_set_object_completion(){
  rl_attempted_completion_function = symbol_completion;
  rl_basic_word_break_characters = " \t\n\"();";
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
void dfsch_console_read_history(char* filename){
  read_history(filename);
}
void dfsch_console_save_history(char* filename, int count){
  stifle_history(count);
  write_history(filename);
}

#else
/* Do nothing */
void dfsch_console_set_object_completion(){
}
void dfsch_console_set_general_completion(){
}
void dfsch_console_read_history(char* filename){
}
void dfsch_console_save_history(char* filename, int count){
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


dfsch_console_repl_command_t* dfsch_console_add_command(dfsch_console_repl_command_t* cmdlist,
                                                        char* name,
                                                        char* doc,
                                                        void (*exec)(char* cmdline, void* baton),
                                                        void* baton){
  dfsch_console_repl_command_t* rc = GC_NEW(dfsch_console_repl_command_t);

  rc->next = cmdlist;
  rc->name = name;
  rc->doc = doc;
  rc->exec = exec;
  rc->baton = baton;
  
}

static void repl_execute_command(dfsch_console_repl_command_t* cmds,
                                 char* cmdline){
  dfsch_console_repl_command_t* i = cmds;
  size_t cmd_len = strcspn(cmdline, " \t");
  char* cmd_name = dfsch_strancpy(cmdline, cmd_len);

  if (cmdline[cmd_len]){
    cmdline = cmdline + cmd_len + strspn(cmdline+cmd_len, " \t");
  } else {
    cmdline = "";
  }
  
  while (i){
    if (strcmp(i->name, cmd_name) == 0){
      i->exec(cmdline, i->baton);
      return;
    }
    i = i->next;
  }

  fprintf(stderr, "No such command: %s\n", cmd_name);
}

static void command_help(char* cmdline, dfsch_console_repl_command_t** cmds){
  dfsch_console_repl_command_t* i = *cmds;
  fprintf(stderr, "Commands:\n");
  while (i){
    fprintf(stderr, "%15s: %s\n", i->name, i->doc);
    i = i->next;
  }  
}

#ifdef USE_READLINE
static void command_complete_into_pkgs(char* cmdline, void* discard){
  complete_into_packages = !complete_into_packages;
  fprintf(stderr, 
          complete_into_packages
          ?"Completing all symbols in package\n" 
          :"Completing only exported symbols\n");
}
#endif

int dfsch_console_read_objects_parser(char* prompt,
                                      dfsch_parser_ctx_t* parser,
                                      dfsch_console_repl_command_t* cmds){
  int ret;
  char* line;

#ifdef USE_READLINE
  cmds = dfsch_console_add_command(cmds, "complete-into-pkgs", 
                                   "Complete into packages",
                                   command_complete_into_pkgs, &cmds);
#endif

  cmds = dfsch_console_add_command(cmds, "help", 
                                   "Print list of supported commands",
                                   command_help, &cmds);

  running_prompt = prompt;
  running_cmds = cmds;
  __asm(";;");
  running_parser = parser;

  DFSCH_UNWIND {
    while (line = dfsch_console_read_line_object(get_prompt(parser, prompt))){
      ret = 0;
      DFSCH_WITH_SIMPLE_RESTART(dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                    "abort"), 
                                "Return to reader"){
        if (*line == ';'){
          repl_execute_command(cmds, line+1);
        } else {
          ret = dfsch_parser_feed_line(parser, line);
        }
        running_prompt = prompt;
        running_cmds = cmds;
        running_parser = parser;
      }DFSCH_END_WITH_SIMPLE_RESTART;
      if (ret){
        break;
      }
    }
  } DFSCH_PROTECT {
    running_parser = NULL;
    running_prompt = NULL;
    running_cmds = NULL;
  } DFSCH_PROTECT_END;

  if (!line){
    printf("EOF\n");
  }

  return ret;
}
int dfsch_console_read_objects_list_parser(char* prompt,
                                           dfsch_parser_ctx_t* parser){

}


int dfsch_console_read_objects(char* prompt,
                               dfsch_console_object_cb_t cb,
                               void* baton,
                               dfsch_console_repl_command_t* cmds){
  dfsch_parser_ctx_t* parser = dfsch_parser_create();
  dfsch_parser_set_source(parser, dfsch_intern_symbol(DFSCH_DFSCH_PACKAGE,
                                                      "*console-io*"));
  dfsch_parser_callback(parser, cb, baton);
  return dfsch_console_read_objects_parser(prompt, parser, cmds);
}
int dfsch_console_read_objects_list(char* prompt,
                                    dfsch_console_object_cb_t cb,
                                    void* baton){
}

typedef struct repl_context_t {
  dfsch_console_repl_eval_cb_t evalfun;
  void* baton;
  int print_depth;
  dfsch_object_t* last_result;
} repl_context_t;

static int repl_callback(dfsch_object_t *obj, repl_context_t* ctx){
  dfsch_object_t** ret;
  ret = dfsch_get_values(ctx->last_result = ctx->evalfun(obj, ctx->baton));
  while (*ret != DFSCH_INVALID_OBJECT){
    dfsch_put_object(stdout, *ret, ctx->print_depth, 1);
    printf("\n");
    ret++;
  }
  return 1;
}

static void command_print_depth(char* cmdline, repl_context_t* ctx){
  if (*cmdline){
    ctx->print_depth = atoi(cmdline);
  }
  fprintf(stderr, "Print depth is %d\n", ctx->print_depth);
}

static void command_inspect_result(char* cmdline, repl_context_t* ctx){
  dfsch_inspect_object(ctx->last_result);
}

int dfsch_console_run_repl_eval(char* prompt, 
                                dfsch_console_repl_eval_cb_t evalfun,
                                void* baton,
                                dfsch_console_repl_command_t* cmds){
  repl_context_t ctx;
  ctx.evalfun = evalfun;
  ctx.baton = baton;
  ctx.print_depth = -1;
  ctx.last_result = NULL;
  cmds = dfsch_console_add_command(cmds, "print-depth", 
                                   "Set print-depth, -1 for circular printer",
                                   command_print_depth, &ctx);
  cmds = dfsch_console_add_command(cmds, "inspect-result", 
                                   "Inspect last result object",
                                   command_inspect_result, &ctx);
  return dfsch_console_read_objects(prompt, repl_callback, &ctx, cmds);
}

int dfsch_console_run_repl(char* prompt, 
                           dfsch_object_t* env,
                           dfsch_console_repl_command_t* cmds){
  return dfsch_console_run_repl_eval(prompt, dfsch_eval, env, cmds);
}
