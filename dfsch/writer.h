#ifndef H__dfsch__writer__
#define H__dfsch__writer__

#include <dfsch/dfsch.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif


#define DFSCH_WRITE_CIRCULAR -1
#define DFSCH_STRICT_WRITE 0
#define DFSCH_WRITE        1
#define DFSCH_PRINT        2

  typedef struct dfsch_pprint_params_t {
    int line_length;
    int indent;
  } dfsch_pprint_params_t;

  void dfsch_write_object_output(dfsch_object_t* obj,
                                 int depth,
                                 int readability,
                                 dfsch_pprint_params_t* ppp,
                                 dfsch_output_proc_t proc,
                                 void* baton);
  void dfsch_write_object_stdio(dfsch_object_t* obj,
                                int depth,
                                int readability,
                                dfsch_pprint_params_t* ppp,
                                FILE* f);


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
  void dfsch_write_space(dfsch_writer_state_t* state);
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


  void dfsch_write_unreadable_with_slots_method(dfsch_object_t* obj,
                                                dfsch_writer_state_t* state);

  
  typedef struct dfsch_pprint_formatter_t dfsch_pprint_formatter_t;

  dfsch_pprint_formatter_t* dfsch_make_pprint_formatter(dfsch_output_proc_t proc,
                                                        void* baton,
                                                        int line_length,
                                                        int initial_indent);
  void dfsch_pprint_begin_group(dfsch_pprint_formatter_t* pf,
                                int indent);
  void dfsch_pprint_end_group(dfsch_pprint_formatter_t* pf);
  void dfsch_pprint_text_block(dfsch_pprint_formatter_t* pf,
                               char* data,
                               size_t len);
  void dfsch_pprint_space(dfsch_pprint_formatter_t* pf,
                          int count);
  



#ifdef __cplusplus
}
#endif
#endif
