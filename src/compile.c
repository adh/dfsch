#include <dfsch/compile.h>
#include <dfsch/magic.h>

dfsch_object_t* dfsch_cons_walked(dfsch_object_t* head,
                                  dfsch_object_t* orig_expr,
                                  size_t count,
                                  ...){
  size_t i;
  dfsch_object_t** data;
  va_list al;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();

  va_start(al, count);


  data = GC_MALLOC(sizeof(dfsch_object_t*)*(count+4));
  data[i] = head;
  i++;
  
  for(i = 0; i < count; i++){
    data[i] = va_arg(al, dfsch_object_t*);
  }

  data[i] = DFSCH_INVALID_OBJECT;
  i++;
  data[i] = NULL; // CDR
  if (orig_expr){
    data[i+1] = DFSCH_SYM_COMPILED_FROM;
    data[i+2] = orig_expr;
  } else {
    data[i+1] = NULL;
    data[i+2] = NULL;
  }

  va_end(al);
  return DFSCH_MAKE_CLIST(data);
  
}
