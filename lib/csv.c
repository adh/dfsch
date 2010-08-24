#include <dfsch/lib/csv.h>
#include <dfsch/ports.h>
#include <dfsch/magic.h>

static dfsch_csv_params_t default_params = {
  .delim = ',',
  .quote = '"',
  .escape = 0
};

#define MUST_READ                               \
  ch = dfsch_port_batch_read(port);             \
  if (ch == -1) {                               \
    dfsch_error("Unexpected end of file");      \
  }

typedef struct partial_string_t {
  char* buf;
  size_t len;
  size_t ptr;
} partial_string_t;

static void ps_init(partial_string_t* ps){
  ps->buf = GC_MALLOC_ATOMIC(512);
  ps->len = 512;
  ps->ptr = 0;
}

static void ps_append(partial_string_t* ps, char ch){
  if (ps->len == ps->ptr){
    ps->len *= 2;
    ps->ptr = GC_REALLOC(ps->ptr, ps->len);
  }
  ps->buf[ps->ptr] = ch;
  ps->ptr++;
}

dfsch_object_t* dfsch_csv_read_line(dfsch_object_t* port,
                                    dfsch_csv_params_t* params){
  char ch;
  dfsch_object_t* ret = NULL;
  dfsch_list_collector_t* lc = NULL; 
  partial_string_t ps;

  if (!params){
    params = &default_params;
  }


  DFSCH_UNWIND {
    dfsch_port_batch_read_start(port);

  start:
    ch = dfsch_port_batch_read(port);
    if (ch != -1) {
      lc = dfsch_make_list_collector();
    } else {
      goto out;
    }
    ps_init(&ps);
    goto field_dispatch;
    
  field:
    ch = dfsch_port_batch_read(port);
  field_dispatch:
    if (ch == params->quote){
      goto quoted;
    } else if (ch == params->delim){
      dfsch_list_collect(lc, dfsch_make_string_buf(ps.buf, ps.ptr));
      ps_init(&ps);
    } else if (ch == -1 || ch == '\n'){
      dfsch_list_collect(lc, dfsch_make_string_buf(ps.buf, ps.ptr));
      goto out;
    } else if (ch == params->escape){
      ch = dfsch_port_batch_read(port);
      if (ch == -1){
        dfsch_error("Unexpected end of file", port);
      }
      ps_append(&ps, ch);
    } else {
      ps_append(&ps, ch);
    }
    goto field;

  quoted:
    ch = dfsch_port_batch_read(port);
    if (ch == params->quote){
      ch = dfsch_port_batch_read(port);
      if (ch != params->quote){
        goto field_dispatch;
      } else {
        ps_append(&ps, ch);
      }
    } else if (ch == params->escape){
      ch = dfsch_port_batch_read(port);
      if (ch == -1){
        dfsch_error("Unexpected end of file", port);
      }
      ps_append(&ps, ch);
    } else {
      if (ch == -1){
        dfsch_error("Unexpected end of file", port);
      }
      ps_append(&ps, ch);
    }
    goto quoted;
    
  out:
    if (lc){
      ret = dfsch_list_2_vector(dfsch_collected_list(lc));
    } else {
      ret = NULL;
    }
  } DFSCH_PROTECT {
    dfsch_port_batch_read_end(port);
  } DFSCH_PROTECT_END;

  return ret;
}
dfsch_object_t* dfsch_csv_read_file(dfsch_object_t* port,
                                    dfsch_csv_params_t* params){
  dfsch_object_t* res;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();

  for(;;) {
    res = dfsch_csv_read_line(port, params);
    if (!res){
      return dfsch_collected_list(lc);
    }
    dfsch_list_collect(lc, res);
  }
}



dfsch_csv_params_t* dfsch_csv_params(dfsch_object_t* args){
  dfsch_csv_params_t* params = GC_NEW(dfsch_csv_params_t);
  memcpy(params, &default_params, sizeof(dfsch_csv_params_t));

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD_GENERIC("delimiter", params->delim, 
                        dfsch_number_to_long);
  DFSCH_KEYWORD_GENERIC("quote-character", params->quote, 
                        dfsch_number_to_long);
  DFSCH_KEYWORD_GENERIC("escape-character", params->escape, 
                        dfsch_number_to_long);
  DFSCH_KEYWORD_PARSER_END(args);

  return params;
}
