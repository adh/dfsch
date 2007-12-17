#ifndef H__dfsch__regex__
#define H__dfsch__regex__

#include <dfsch/dfsch.h>
#include <regex.h>

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * Compile given regular expression into regex object. Flags are same as 
   * for regcomp(3).
   */
  dfsch_object_t* dfsch_regex_compile(char* expression, int flags);

  /**
   * Apply precompiled regex object to given string and return whetever 
   * regular expression matches
   */
  int dfsch_regex_match_p(dfsch_object_t* regex, char* string, int flags);

  /**
   * Apply precompiled regex object to given string and return vector of
   * matching substrings (each as vector of start offset, end offset and 
   * matching string).
   */
  dfsch_object_t* dfsch_regex_substrings(dfsch_object_t* regex, char* string,
                                         int flags);

  /**
   * Define regular expression primitives in given environment.
   */
  dfsch_object_t* dfsch_module_regex_register(dfsch_object_t *ctx);

#ifdef __cplusplus
}
#endif

#endif
