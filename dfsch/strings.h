#ifndef H__dfsch__strings__
#define H__dfsch__strings__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct dfsch_strbuf_t{
    char* ptr;
    size_t len;
  } dfsch_strbuf_t;

  extern int dfsch_string_p(dfsch_object_t* obj);

  extern dfsch_strbuf_t* dfsch_strbuf_create(char* ptr, size_t len);
  
  extern dfsch_object_t* dfsch_make_string_cstr(char* string);
  extern dfsch_object_t* dfsch_make_string_strbuf(dfsch_strbuf_t* strbuf);
  extern dfsch_object_t* dfsch_make_string_buf(char* ptr, size_t len);

  extern char* dfsch_string_to_cstr(dfsch_object_t* obj);
  extern dfsch_strbuf_t* dfsch_string_to_buf(dfsch_object_t* obj);
  extern char* dfsch_string_or_symbol_to_cstr(dfsch_object_t* obj);

  extern int dfsch_string_cmp(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_eq_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_lt_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_gt_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_lte_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_gte_p(dfsch_object_t* a, dfsch_object_t* b);

  extern int dfsch_string_cmp_ci(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_ci_eq_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_ci_lt_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_ci_gt_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_ci_lte_p(dfsch_object_t* a, dfsch_object_t* b);
  extern int dfsch_string_ci_gte_p(dfsch_object_t* a, dfsch_object_t* b);

  extern dfsch_object_t* dfsch_string_list_append(dfsch_object_t* list);

  extern char dfsch_string_ref(dfsch_object_t* string, size_t index);

  extern size_t dfsch_string_length(dfsch_object_t* string);

  extern dfsch_object_t* dfsch_string_substring(dfsch_object_t* string, 
                                                size_t start,
                                                size_t end);

  extern dfsch_object_t* dfsch_string_substring_utf8(dfsch_object_t* string, 
                                                     size_t start,
                                                     size_t end);

  extern dfsch_object_t* dfsch_string_2_list(dfsch_object_t* string);


  typedef int (*dfsch_string_callback_t)(char, void*);
  typedef int (*dfsch_string_unicode_callback_t)(uint32_t ch, 
                                                 void* baton,
                                                 size_t start,
                                                 size_t end);
  typedef int (*dfsch_string_unicode_invalid_callback_t)(uint32_t ch, 
                                                         void* baton,
                                                         size_t index);

  extern int dfsch_string_for_each(dfsch_string_callback_t proc,
                                   dfsch_object_t* string,
                                   void *baton);

  extern int dfsch_string_utf8_for_each(dfsch_string_unicode_callback_t proc,
                                        dfsch_object_t* string,
                                        void *baton,
                                        dfsch_string_unicode_invalid_callback_t iproc,
                                        void* ibaton);
#define DFSCH_STRING_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_string_to_cstr)
#define DFSCH_STRING_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, char*, dfsch_string_to_cstr)

#define DFSCH_STRING_OR_SYMBOL_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_string_or_symbol_to_cstr)
#define DFSCH_STRING_OR_SYMBOL_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, char*, dfsch_string_or_symbol_to_cstr)

#define DFSCH_BUFFER_ARG(al, name) \
  DFSCH_GENERIC_ARG(al, name, dfsch_strbuf_t*, dfsch_string_to_buf)
#define DFSCH_BUFFER_ARG_OPT(al, name, default) \
  DFSCH_GENERIC_ARG_OPT(al, name, default, dfsch_strbuf_t*, dfsch_string_to_buf)

#ifdef __cplusplus
}
#endif

#endif
