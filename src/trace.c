#include <dfsch/trace.h>
#include <dfsch/magic.h>
#include <stdio.h>

static void trace_listener(dfsch__thread_info_t* ti){
  dfsch__tracepoint_t* tp = &DFSCH__TRACEPOINT(ti);

  DFSCH_UNWIND {
    ti->trace_listener = NULL;
    
    switch(tp->flags & 0xff){
    case DFSCH_TRACEPOINT_KIND_INVALID:
      fprintf(stderr, "Tracepoint(%d): INVALID???\n",
              GC_get_total_bytes());
      break;
    case DFSCH_TRACEPOINT_KIND_APPLY:
      fprintf(stderr, "Tracepoint(%d): APPLY %s %s\n",
              GC_get_total_bytes(),
              dfsch_object_2_string(tp->data.apply.proc, 10, DFSCH_WRITE),
              dfsch_object_2_string(tp->data.apply.args, 10, DFSCH_WRITE));
      break;
    case DFSCH_TRACEPOINT_KIND_EVAL:
      fprintf(stderr, "Tracepoint(%d): EVAL %s %s\n",
              GC_get_total_bytes(),
              dfsch_object_2_string(tp->data.eval.expr, 10, DFSCH_WRITE),
              dfsch_object_2_string(tp->data.eval.env, 10, DFSCH_WRITE));
      break;
    case DFSCH_TRACEPOINT_KIND_ANON:
      fprintf(stderr, "Tracepoint(%d): ANON %s %p\n",
              GC_get_total_bytes(),
              tp->data.anon.location,
              tp->data.anon.data);
      break;
    }
  } DFSCH_PROTECT {
    ti->trace_listener = trace_listener;  
  } DFSCH_PROTECT_END;
  
}

static void trace_listener_safe(dfsch__thread_info_t* ti){
  fprintf(stderr, "Tracepoint(%d): 0x%08hhx %p %p\n",
          GC_get_total_bytes(), 
          DFSCH__TRACEPOINT(ti).flags,
          DFSCH__TRACEPOINT(ti).data.eval.expr,
          DFSCH__TRACEPOINT(ti).data.eval.env);          
}

void dfsch_enable_console_trace(int safe){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  ti->trace_listener = safe ? trace_listener_safe : trace_listener;
}
void dfsch_disable_console_trace(){
  dfsch__thread_info_t* ti = dfsch__get_thread_info();
  ti->trace_listener = NULL;

}
