#ifndef H__dfsch__regex__
#define H__dfsch__regex__

#include <dfsch/dfsch.h>
#include <regex.h>

dfsch_object_t* dfsch_regex_compile(char* expression, int flags);
int dfsch_regex_match_p(dfsch_object_t* regex, char* string, int flags);
dfsch_object_t* dfsch_regex_substrings(dfsch_object_t* regex, char* string,
                                       int flags);


#endif
