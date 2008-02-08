/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   I/O ports
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

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
  typedef void (*dfsch_port_seek_t)(dfsch_object_t* port, 
                                    off_t offset, int whence);
  typedef off_t (*dfsch_port_tell_t)(dfsch_object_t* port);

  typedef void (*dfsch_port_batch_read_start_t)(dfsch_object_t* port);
  typedef void (*dfsch_port_batch_read_end_t)(dfsch_object_t* port);
  typedef int (*dfsch_port_batch_read_t)(dfsch_object_t* port);

  typedef int (*dfsch_port_get_caps_t)(dfsch_object_t* port);

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
  extern dfsch_type_t dfsch_port_type_type;

  /**
   * Pointer to type of port types.
   */
#define DFSCH_PORT_TYPE_TYPE (&dfsch_port_type_type)

  extern dfsch_type_t dfsch_port_basetype;
#define DFSCH_PORT_BASETYPE (&dfsch_port_basetype);

  int dfsch_port_p(dfsch_object_t* obj);
  int dfsch_output_port_p(dfsch_object_t* obj);
  int dfsch_input_port_p(dfsch_object_t* obj);

  void dfsch_port_write_buf(dfsch_object_t* port, char*buf, size_t size);
  ssize_t dfsch_port_read_buf(dfsch_object_t* port, char*buf, size_t size);
  void dfsch_port_seek(dfsch_object_t* port, off_t offset, int whence);
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

  dfsch_object_t* dfsch_make_file_port(FILE* file, int close, char* name);
  dfsch_object_t* dfsch_open_file_port(char* filename, char* mode);
  void dfsch_close_file_port(dfsch_object_t* port);


  void dfsch_port_unsafe_register(dfsch_object_t* ctx);

#ifdef __cplusplus
}
#endif

#endif
