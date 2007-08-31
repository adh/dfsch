#ifndef H__dfsch__ports__
#define H__dfsch__ports__

#include <dfsch/dfsch.h>
#include <sys/types.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

  /*
   * Port is either instace of instance of DFSCH_PORT_TYPE_TYPE or instance of 
   * class (or simple closure) that responds-to? given method (read-buf!, 
   * write-buf!, seek!, tell, batch-read-start!, batch-read-end!, batch-read!)
   */

  typedef ssize_t (*dfsch_port_write_buf_t)(dfsch_object_t* port, 
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
  typedef struct dfsch_port_type_t{
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
     * batch_read_start()/batch_reand_end() calls.
     */
    dfsch_port_batch_read_t batch_read;
  };

  /**
   * Type of port types.
   */
  extern const dfsch_type_t dfsch_port_type_type;

  /**
   * Pointer to type of port types.
   */
#define DFSCH_PORT_TYPE_TYPE ((dfsch_type_t*)&dfsch_port_type_type)


  ssize_t dfsch_port_write_buf_t(dfsch_object_t* port, char*buf, size_t size);
  ssize_t dfsch_port_read_buf_t(dfsch_object_t* port, char*buf, size_t size);
  int dfsch_port_seek_t(dfsch_object_t* port, off_t offset, int whence);
  off_t dfsch_port_tell_t(dfsch_object_t* port);
  
  void dfsch_port_batch_read_start_t(dfsch_object_t* port);
  void dfsch_port_batch_read_end_t(dfsch_object_t* port);
  int dfsch_port_batch_read_t(dfsch_object_t* port);
  
  

#ifdef __cplusplus
}
#endif

#endif
