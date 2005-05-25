#ifndef H__dfsch__dsc__
#define H__dfsch__dsc__

#ifdef __cplusplus
extern "C" {
#endif

  typedef int (*dsc_code_callback_t)(char *code, void* baton);

  extern int dsc_generate(dfsch_object_t *obj, 
                          dsc_code_callback_t callback,
                          void *baton);

#ifdef __cplusplus
}
#endif
