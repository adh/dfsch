#include <dfsch/lib/socket-port.h>

#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>
#include <signal.h>
#include <pthread.h>

typedef struct socket_port_t {
  dfsch_type_t type;
  int fd;
  int open;
  char* buf;
  size_t buflen;
  size_t bufptr;
  pthread_mutex_t mutex;
  char* name;
} socket_port_t;

static void socket_port_write(socket_port_t* sp, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, sp, "%d %s", sp->fd, sp->name);
}

static void socket_port_write_buf(socket_port_t* port, 
                                  char*buf, size_t len){
  ssize_t ret;

  if (!port->open){
    dfsch_error("Port is closed", (dfsch_object_t*)port);
  }

  while (len){
    ret = write(port->fd, buf, len);
    if (ret == 0){
      return;
    }
    if (ret < 0){
      if (errno == EINTR){
        dfsch_async_apply_check();
      } else {
        dfsch_operating_system_error("write");    
      }
    }
    len -= ret;
    buf += ret;
  }
}
static ssize_t socket_port_read_buf(socket_port_t* port,
                                    char* buf, size_t len){
  ssize_t ret;
  ssize_t my_ret = len;

  if (!port->open){
    dfsch_error("Port is closed", (dfsch_object_t*)port);
  }

  while (len){
    ret = read(port->fd, buf, len);
    if (ret == 0){
      return 0;
    }
    if (ret < 0){
      if (errno == EINTR){
        dfsch_async_apply_check();
      } else {
        dfsch_operating_system_error("write");    
      }
    }
    len -= ret;
    buf += ret;
  }

  return my_ret;
}


dfsch_port_type_t dfsch_socket_port_type = {
  .type = {
    .type = DFSCH_PORT_TYPE_TYPE,
    .superclass = DFSCH_PORT_TYPE,
    .size = sizeof(socket_port_t),
    .name = "socket-port",
    .write = (dfsch_type_write_t)socket_port_write,
  },
  
  .write_buf = (dfsch_port_write_buf_t)socket_port_write_buf,
  .read_buf = (dfsch_port_read_buf_t)socket_port_read_buf,

  /*.batch_read_start = (dfsch_port_batch_read_start_t)socket_port_batch_read_start,
  .batch_read_end = (dfsch_port_batch_read_end_t)socket_port_batch_read_end,
  .batch_read = (dfsch_port_batch_read_t)socket_port_batch_read,*/
};

static void socket_port_finalizer(socket_port_t* port, void* cd){
  if (port->open){
    close(port->fd);
    port->open = 0;
  }
}

dfsch_object_t* dfsch_socket_port_tcp_connect(char* hostname,
                                              int port){
  struct sockaddr_in inet_addr;
  struct hostent* h;
  socket_port_t* sp = dfsch_make_object(DFSCH_SOCKET_PORT_TYPE);

  sp->name = dfsch_saprintf("inet %s:%d", hostname, port);
  sp->open = 1;

  GC_REGISTER_FINALIZER(sp, (GC_finalization_proc)socket_port_finalizer,
                        NULL, NULL, NULL);

  if ((h = gethostbyname(hostname)) == NULL){
    dfsch_operating_system_error("gethostbyname");
  }

  inet_addr.sin_family=AF_INET;
  inet_addr.sin_port=htons(port);
  memcpy(&(inet_addr.sin_addr), h->h_addr, h->h_length);

  sp->fd = socket(PF_INET, SOCK_STREAM, 0);
  if (sp->fd == -1){
    dfsch_operating_system_error("socket");
    return NULL;
  }
  
  if (connect(sp->fd,(struct sockaddr*)&inet_addr,
	      sizeof(inet_addr))==-1){
    int sav = errno;
    close(sp->fd);
    sp->open = 0;
    dfsch_operating_system_error_saved(sav, "connect");
  }
  
  return sp;
}
dfsch_object_t* dfsch_socket_port_unix_connect(char* path){
  socket_port_t* sp = dfsch_make_object(DFSCH_SOCKET_PORT_TYPE);

  sp->name = dfsch_saprintf("unix %s", path);

  return sp;
}
dfsch_object_t* dfsch_socket_port_accept(int fd){
  
}

