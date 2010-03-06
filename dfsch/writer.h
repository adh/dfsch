#ifndef H__dfsch__writer__
#define H__dfsch__writer__

#include <dfsch/dfsch.h>

#ifdef __cplusplus
extern "C" {
#endif


#define DFSCH_WRITE_CIRCULAR -1
#define DFSCH_STRICT_WRITE 0
#define DFSCH_WRITE        1
#define DFSCH_PRINT        2

  dfsch_writer_state_t* dfsch_make_writer_state(int max_depth,
                                                int readability,
                                                dfsch_output_proc_t proc,
                                                void* baton);
  void dfsch_invalidate_writer_state(dfsch_writer_state_t* state);
  int dfsch_writer_state_print_p(dfsch_writer_state_t* state);
  int dfsch_writer_state_pprint_p(dfsch_writer_state_t* state);
  int dfsch_writer_state_cmark_p(dfsch_writer_state_t* state);
  void dfsch_write_object(dfsch_writer_state_t* state,
                                 dfsch_object_t* object);
  void dfsch_write_string(dfsch_writer_state_t* state,
                                 char* str);
  void dfsch_write_strbuf(dfsch_writer_state_t* state,
                                 char* str, size_t len);
  void dfsch_write_unreadable(dfsch_writer_state_t* state,
                                     dfsch_object_t* obj, 
                                     char* format, ...);
  void dfsch_write_unreadable_with_slots(dfsch_writer_state_t* state,
                                         dfsch_object_t* obj);
  void dfsch_write_unreadable_start(dfsch_writer_state_t* state,
                                           dfsch_object_t* obj);
  void dfsch_write_unreadable_end(dfsch_writer_state_t* state);
  void dfsch_write_pprint_newline(dfsch_writer_state_t* state);
  void dfsch_write_pprint_indent(dfsch_writer_state_t* state);
  void dfsch_write_pprint_begin(dfsch_writer_state_t* state);
  void dfsch_write_pprint_end(dfsch_writer_state_t* state);


  void dfsch_write_unreadable_with_slots_method(dfsch_object_t* obj,
                                                dfsch_writer_state_t* state);

#ifdef __cplusplus
}
#endif
#endif
