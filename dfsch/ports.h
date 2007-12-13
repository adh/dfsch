#ifndef H__dfsch__ports__
#define H__dfsch__ports__

#include <dfsch/dfsch.h>
#include <sys/types.h>
#include <stdio.h>
#include <dfsch/strings.h>

#ifdef __cplusplus
extern "C" {
#endif

  /*
   * Port is either instace of instance of DFSCH_PORT_TYPE_TYPE or instance of 
   * class (or simple closure) that responds-to? given method (read-buf!, 
   * write-buf!, seek!, tell, batch-read-start!, batch-read-end!, batch-read!)
   */

  typedef void (*dfsch_port_write_buf_t)(dfsch_object_t* port, 
                                        char*buf, size_t size);
  typedef ssize_t (*dfsch_port_read_buf_t)(dfsch_object_t* port, 
                                         char*buf, size_t size);
  typedef int (*dfsch_port_seek_t)(dfsch_object_t* port, 
                                   off_t offset, int whence);
  typedef off_t (*dfsch_port_tell_t)(dfsch_object_t* port);

  typedef void (*dfsch_port_batch_read_start_t)(dfsch_object_t* port);
  typedef void (*dfsch_port_batch_read_end_t)(dfsch_object_t* port);
  typedef int (*dfsch_port_batch_read_t)(dfsch_object_t* port);

  /**
   * Type structure for ports (this structure itself must have type of
   * DFSCH_PORT_TYPE_TYPE).
   */
  typedef struct dfsch_port_type_t {
    /**
     * Common type fields
     */
    dfsch_type_t type;
    /**
     * Method for writing buffer of data to given stream. May be NULL when not
     * supported
     */
    dfsch_port_write_buf_t write_buf;
    /**
     * Method for reading buffer of data from given stream. May be NULL when 
     * not supported.
     */
    dfsch_port_read_buf_t read_buf;
    /**
     * Method for changing current position in stream. May be NULL when not
     * supported.
     */
    dfsch_port_seek_t seek;
    /**
     * Method for querying current position in stream. May be NULL when not 
     * supported.
     */ 
    dfsch_port_tell_t tell;

    /**
     * Prepare stream for series of batch_reads, typically locking of some 
     * mutex.
     */
    dfsch_port_batch_read_start_t batch_read_start;
    /**
     * Cleanup after series of batch_reads. Typically unlocking of some mutex.
     */
    dfsch_port_batch_read_end_t batch_read_end;
    /**
     * Fast read of one byte that must be inside 
     * batch_read_start()/batch_read_end() calls. When this is not 
     * implemented it will be emulated by means of read_buf().
     * General idea is that this will use some mechanism like 
     * unlocked_stdio(3).
     */
    dfsch_port_batch_read_t batch_read;
  } dfsch_port_type_t;

  /**
   * Type of port types.
   */
  extern const dfsch_type_t dfsch_port_type_type;

  /**
   * Pointer to type of port types.
   */
#define DFSCH_PORT_TYPE_TYPE ((dfsch_type_t*)&dfsch_port_type_type)

  int dfsch_port_p(dfsch_object_t* obj);
  int dfsch_output_port_p(dfsch_object_t* obj);
  int dfsch_input_port_p(dfsch_object_t* obj);

  void dfsch_port_write_buf(dfsch_object_t* port, char*buf, size_t size);
  ssize_t dfsch_port_read_buf(dfsch_object_t* port, char*buf, size_t size);
  int dfsch_port_seek(dfsch_object_t* port, off_t offset, int whence);
  off_t dfsch_port_tell(dfsch_object_t* port);
  
  void dfsch_port_batch_read_start(dfsch_object_t* port);
  void dfsch_port_batch_read_end(dfsch_object_t* port);
  int dfsch_port_batch_read(dfsch_object_t* port);
  
  dfsch_strbuf_t* dfsch_port_readline(dfsch_object_t* port);

  dfsch_object_t* dfsch_null_port();
  
  dfsch_object_t* dfsch_current_output_port();
  dfsch_object_t* dfsch_current_input_port();
  dfsch_object_t* dfsch_current_error_port();

  void dfsch_set_current_output_port(dfsch_object_t* port);
  void dfsch_set_current_input_port(dfsch_object_t* port);
  void dfsch_set_current_error_port(dfsch_object_t* port);

  dfsch_object_t* dfsch_string_output_port();
  dfsch_strbuf_t* dfsch_string_output_port_value(dfsch_object_t* port);

  dfsch_object_t* dfsch_eof_object();
  int dfsch_eof_object_p(dfsch_object_t* obj);


  void dfsch_port_unsafe_register(dfsch_object_t* ctx);

#ifdef __cplusplus
}
#endif

#endif
