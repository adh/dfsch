#include <dfsch/lib/newt.h>
#include <newt.h>
#include <dfsch/dfsch.h>
#include <dfsch/load.h>

DFSCH_DEFINE_PRIMITIVE(init, "Initialize Newt library"){
  DFSCH_ARG_END(args);
  
  newtInit();

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(finished, "Deinitialize Newt library"){
  DFSCH_ARG_END(args);
  
  newtFinished();

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(suspend, "Suspend Newt library and return terminal "
                       "to intial state"){
  DFSCH_ARG_END(args);
  
  newtSuspend();

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(resume, "Resume usage of Newt library"){
  DFSCH_ARG_END(args);
  
  newtResume();

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(cls, "Clear screen"){
  DFSCH_ARG_END(args);
  
  newtCls();

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(refresh, "Redraw screen contents"){
  DFSCH_ARG_END(args);
  
  newtRefresh();

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(wait_for_key, "Wait for key to be pressed"){
  DFSCH_ARG_END(args);
  
  newtCls();

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(draw_root_text, "Draw text string into root window"){
  int left;
  int top;
  char* text;
  DFSCH_LONG_ARG(args, left);
  DFSCH_LONG_ARG(args, top);
  DFSCH_STRING_ARG(args, text);
  DFSCH_ARG_END(args);

  newtDrawRootText(left, top, text);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(push_help_line,
                       "Push text into help line stack (top of stack is shown "
                       "at bottom of screen)"){
  char* text;
  DFSCH_STRING_ARG_OPT(args, text, NULL);
  DFSCH_ARG_END(args);

  newtPushHelpLine(text);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(pop_help_line,
                       "Pop line from help stack"){
  DFSCH_ARG_END(args);

  newtPopHelpLine();

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(get_screen_size,
                       "Return size of terminal's screen"){
  int cols;
  int rows;
  DFSCH_ARG_END(args);

  newtGetScreenSize(&cols, &rows);

  return dfsch_list(2, 
                    dfsch_make_number_from_long(cols), 
                    dfsch_make_number_from_long(rows));;
}

DFSCH_DEFINE_PRIMITIVE(win_message,
                       "Creates message box window"){
  char* title = NULL;
  char* button_text = "OK";
  char* text = NULL;

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("title", title, dfsch_string_to_cstr); 
  DFSCH_KEYWORD_GENERIC("text", text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("button-text", button_text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);

  newtWinMessage(title, button_text, text, NULL);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(win_choice,
                       "Creates message box window with two buttons"){
  char* title = NULL;
  char* button1_text = "Yes";
  char* button2_text = "No";
  char* text = NULL;

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("title", title, dfsch_string_to_cstr); 
  DFSCH_KEYWORD_GENERIC("text", text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("button1-text", button1_text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("button2-text", button2_text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_number_from_long(newtWinChoice(title, 
                                                   button1_text, 
                                                   button2_text, 
                                                   text, 
                                                   NULL));
}
DFSCH_DEFINE_PRIMITIVE(win_ternary,
                       "Creates message box window with three buttons"){
  char* title = NULL;
  char* button1_text = "Yes";
  char* button2_text = "No";
  char* button3_text = "Cancel";
  char* text = NULL;

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("title", title, dfsch_string_to_cstr); 
  DFSCH_KEYWORD_GENERIC("text", text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("button1-text", button1_text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("button2-text", button2_text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_GENERIC("button3-text", button3_text, dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);

  return dfsch_make_number_from_long(newtWinTernary(title, 
                                                    button1_text, 
                                                    button2_text, 
                                                    button3_text, 
                                                    text, 
                                                    NULL));
}


void dfsch_module_newt_register(dfsch_object_t* env){
  dfsch_package_t* newt = dfsch_make_package("newt",
                                             "Newt terminal widget library");

  dfsch_provide(env, "newt");
  
  dfsch_defcanon_pkgcstr(env, newt, "init", DFSCH_PRIMITIVE_REF(init));
  dfsch_defcanon_pkgcstr(env, newt, "finished", DFSCH_PRIMITIVE_REF(finished));
  dfsch_defcanon_pkgcstr(env, newt, "suspend", DFSCH_PRIMITIVE_REF(suspend));
  dfsch_defcanon_pkgcstr(env, newt, "resume", DFSCH_PRIMITIVE_REF(resume));

  dfsch_defcanon_pkgcstr(env, newt, "cls", DFSCH_PRIMITIVE_REF(cls));
  dfsch_defcanon_pkgcstr(env, newt, "refresh", DFSCH_PRIMITIVE_REF(refresh));
  dfsch_defcanon_pkgcstr(env, newt, "wait-for-key", 
                         DFSCH_PRIMITIVE_REF(wait_for_key));
  dfsch_defcanon_pkgcstr(env, newt, "draw-root-text", 
                         DFSCH_PRIMITIVE_REF(draw_root_text));

  dfsch_defcanon_pkgcstr(env, newt, "push-help-line", 
                         DFSCH_PRIMITIVE_REF(push_help_line));
  dfsch_defcanon_pkgcstr(env, newt, "pop-help-line", 
                         DFSCH_PRIMITIVE_REF(pop_help_line));
  dfsch_defcanon_pkgcstr(env, newt, "get-screen-size", 
                         DFSCH_PRIMITIVE_REF(get_screen_size));
  dfsch_defcanon_pkgcstr(env, newt, "win-message", 
                         DFSCH_PRIMITIVE_REF(win_message));
  dfsch_defcanon_pkgcstr(env, newt, "win-choice", 
                         DFSCH_PRIMITIVE_REF(win_choice));
  dfsch_defcanon_pkgcstr(env, newt, "win-ternary", 
                         DFSCH_PRIMITIVE_REF(win_ternary));
}
