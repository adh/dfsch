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
#include <sys/uio.h>
#include <signal.h>
#include <pthread.h>

#define SOCK_BUFFER_SIZE 4096

typedef struct socket_port_t {
  dfsch_type_t type;
  int fd;
  int open;
  char* buf;
  char* bufhead;
  size_t buflen;
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

static ssize_t socket_port_real_read(socket_port_t* port,
                                     char* buf, size_t len){
  ssize_t ret;

 retry:
  ret = read(port->fd, buf, len);
  if (ret < 0){
    if (errno == EINTR){
      dfsch_async_apply_check();
      goto retry;
    } else {
      dfsch_operating_system_error("read");    
    }
  }

  return ret;
}

static ssize_t socket_port_read_buf(socket_port_t* sp,
                                    char* buf, size_t len){
  ssize_t my_ret = 0;
  ssize_t ret;
  size_t tmplen;
  struct iovec iov[2];
  char* tmpbuf;

  if (!sp->open){
    dfsch_error("Port is closed", (dfsch_object_t*)sp);
  }

  pthread_mutex_lock(&(sp->mutex));

  if (sp->buf != sp->bufhead){
    memmove(sp->buf, sp->bufhead, sp->buflen);
    sp->bufhead = sp->buf;
  }

  if (sp->buflen < len){
    memcpy(buf, sp->buf, sp->buflen);
    my_ret += sp->buflen;
    buf += sp->buflen;
    len -= sp->buflen;
    sp->buflen = 0;

    while (len > SOCK_BUFFER_SIZE){
      ret = socket_port_real_read(sp, buf, len);
      if (ret == 0){
        pthread_mutex_unlock(&(sp->mutex));
        return my_ret;
      }
      buf += ret;
      len -= ret;
      my_ret += ret;
    }

    while (len > sp->buflen){
      ret = socket_port_real_read(sp, sp->buf + sp->buflen, SOCK_BUFFER_SIZE - sp->buflen);
      if (ret == 0){
        memcpy(buf, sp->buf, sp->buflen);
        my_ret += sp->buflen;
        buf += sp->buflen;
        len -= sp->buflen;
        sp->buflen = 0;

        pthread_mutex_unlock(&(sp->mutex));
        return my_ret;
      }
      sp->buflen += ret;
    }
  }

  memcpy(buf, sp->buf, len);
  sp->buflen -= len;
  my_ret += len;
  memmove(sp->buf, sp->buf + len, sp->buflen);
  pthread_mutex_unlock(&(sp->mutex));
  return my_ret;  
}

static void socket_port_batch_read_start(socket_port_t* port){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  pthread_mutex_lock(&(port->mutex));
}
static void socket_port_batch_read_end(socket_port_t* port){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  pthread_mutex_unlock(&(port->mutex));
}
static int socket_port_batch_read(socket_port_t* sp){
  int ch;
  size_t ret;

  if (sp->buflen){
    ch = sp->bufhead[0];
    sp->buflen--;
    sp->bufhead++;
    return ch;
  } else {
    ret = socket_port_real_read(sp, sp->buf, SOCK_BUFFER_SIZE);
    sp->bufhead = sp->buf;
    if (ret == 0){
      return EOF;
    }

    ch = sp->buf[0];
    sp->buf = sp->buf + 1;
    sp->buf = ret - 1;
    return ch;
  }
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

  .batch_read_start = (dfsch_port_batch_read_start_t)socket_port_batch_read_start,
  .batch_read_end = (dfsch_port_batch_read_end_t)socket_port_batch_read_end,
  .batch_read = (dfsch_port_batch_read_t)socket_port_batch_read,
};

static void socket_port_finalizer(socket_port_t* port, void* cd){
  if (port->open){
    close(port->fd);
    port->open = 0;
  }
}

static dfsch_object_t* cons_socket_port(char* name,
                                        int fd){
  socket_port_t* sp = dfsch_make_object(DFSCH_SOCKET_PORT_TYPE);

  sp->name = name;
  sp->open = 1;
  GC_REGISTER_FINALIZER(sp, (GC_finalization_proc)socket_port_finalizer,
                        NULL, NULL, NULL);
  sp->fd = fd;

  sp->buf = GC_MALLOC_ATOMIC(SOCK_BUFFER_SIZE);
  sp->buflen = 0;
  sp->bufhead = sp->buf;

  return sp;
}

dfsch_object_t* dfsch_socket_port_tcp_connect(char* hostname,
                                              int port){
  struct sockaddr_in inet_addr;
  struct hostent* h;
  int fd;

  dfsch_lock_libc();
  if ((h = gethostbyname(hostname)) == NULL){
    dfsch_unlock_libc();
    dfsch_operating_system_error("gethostbyname");
  }

  inet_addr.sin_family=AF_INET;
  inet_addr.sin_port=htons(port);
  memcpy(&(inet_addr.sin_addr), h->h_addr, h->h_length);
  dfsch_unlock_libc();

  fd = socket(PF_INET, SOCK_STREAM, 0);
  if (fd == -1){
    dfsch_operating_system_error("socket");
    return NULL;
  }
  
  if (connect(fd,(struct sockaddr*)&inet_addr,
	      sizeof(inet_addr))==-1){
    int sav = errno;
    close(fd);
    dfsch_operating_system_error_saved(sav, "connect");
  }
  
  return cons_socket_port(dfsch_saprintf("inet %s:%d", hostname, port),
                          fd);
}
dfsch_object_t* dfsch_socket_port_unix_connect(char* path){
  socket_port_t* sp = dfsch_make_object(DFSCH_SOCKET_PORT_TYPE);

  sp->name = dfsch_saprintf("unix %s", path);

  return sp;
}

void dfsch_socket_port_close(dfsch_object_t* spo){
  socket_port_t* sp = DFSCH_ASSERT_TYPE(spo, DFSCH_SOCKET_PORT_TYPE);
  if (sp->open){
    sp->open = 0;
    close(sp->fd);
  }
}

typedef struct server_socket_t {
  dfsch_type_t* type;
  int fd;
  int open;
  char* name;
} server_socket_t;

static void server_socket_finalizer(server_socket_t* sock, void* cd){
  if (sock->open){
    close(sock->fd);
    sock->open = 0;
  }
}

static void server_socket_write(server_socket_t* s, 
                                dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, s, "%d %s", s->fd, s->name);
}

dfsch_type_t dfsch_server_socket_type = {
  .type = DFSCH_STANDARD_TYPE,
  .size = sizeof(socket_port_t),
  .name = "server-socket",
  .write = (dfsch_type_write_t)server_socket_write,
};


static dfsch_object_t* cons_server_socket(char* name,
                                          int fd){
  server_socket_t* s = dfsch_make_object(DFSCH_SERVER_SOCKET_TYPE);

  s->name = name;
  s->open = 1;
  GC_REGISTER_FINALIZER(s, (GC_finalization_proc)server_socket_finalizer,
                        NULL, NULL, NULL);
  s->fd = fd;

  return s;
}

dfsch_object_t* dfsch_server_socket_tcp_bind(char* hostname,
                                             int port){
  struct sockaddr_in inet_addr;
  int fd;
  struct hostent* h;

  dfsch_lock_libc();
  if ((h = gethostbyname(hostname)) == NULL){ 
    dfsch_unlock_libc();
    dfsch_operating_system_error("gethostbyname");
  }

  inet_addr.sin_family=AF_INET;
  inet_addr.sin_port=htons(port);
  memcpy(&(inet_addr.sin_addr), h->h_addr, h->h_length);
  dfsch_unlock_libc();

  fd = socket(PF_INET, SOCK_STREAM, 0);
  if (fd == -1){
    dfsch_operating_system_error("socket");
  }
  
  if (bind(fd,(struct sockaddr*)&inet_addr,
           sizeof(inet_addr))==-1){
    int sav = errno;
    close(fd);
    dfsch_operating_system_error_saved(sav, "connect");
  }
  
  if (listen(fd, 5) == -1){
    int sav = errno;
    close(fd);
    dfsch_operating_system_error_saved(sav, "listen");
  }

  return cons_server_socket(dfsch_saprintf("inet %s:%d", hostname, port),
                            fd); 
}
dfsch_object_t* dfsch_server_socket_unix_bind(char* path){

}
void dfsch_server_socket_close(dfsch_object_t* sso){
  server_socket_t* ss = DFSCH_ASSERT_TYPE(sso, DFSCH_SERVER_SOCKET_TYPE);
  if (ss->open){
    ss->open = 0;
    close(ss->fd);
  }
}

dfsch_object_t* dfsch_server_socket_accept(dfsch_object_t* server_socket){
  server_socket_t* ss = DFSCH_ASSERT_TYPE(server_socket, 
                                          DFSCH_SERVER_SOCKET_TYPE);
  int fd;

  if (!ss->open){
    dfsch_error("Socket is closed", (dfsch_object_t*)ss);
  }
  
 retry:
  fd = accept(ss->fd, NULL, NULL);
  if (fd == -1){
    if (errno == EINTR){
      dfsch_async_apply_check();
      goto retry;
    }
    dfsch_operating_system_error("accept");
  }
  
  return cons_socket_port(dfsch_saprintf("%s client", ss->name),
                          fd);
}

typedef struct stream_server_context_t {
  dfsch_object_t* port;
  dfsch_server_socket_accept_loop_cb_t cb;
  void* baton;
} stream_server_context_t;

static void* stream_server_thread(void* arg){
  stream_server_context_t* ctx = arg;
  ctx->cb(ctx->baton, ctx->port);
  dfsch_socket_port_close(ctx->port);
}

void dfsch_server_socket_run_accept_loop(dfsch_object_t* server_socket,
                                         dfsch_server_socket_accept_loop_cb_t cb,
                                         void* baton){
  dfsch_object_t* ss = DFSCH_ASSERT_TYPE(server_socket, 
                                         DFSCH_SERVER_SOCKET_TYPE);
  stream_server_context_t* ctx;
  pthread_t thread;

  for(;;){
    ctx = GC_NEW(stream_server_context_t);
    ctx->cb = cb;
    ctx->baton = baton;
    ctx->port = dfsch_server_socket_accept(ss);
    pthread_create(&thread, NULL, stream_server_thread, ctx);
    pthread_detach(thread);
  }
}
