#include <dfsch/lib/socket-port.h>
#include <dfsch/magic.h>

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
  pthread_mutex_t* mutex;
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

  signal(SIGPIPE, SIG_IGN);

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

  pthread_mutex_lock(sp->mutex);

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
        pthread_mutex_unlock(sp->mutex);
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

        pthread_mutex_unlock(sp->mutex);
        return my_ret;
      }
      sp->buflen += ret;
    }
  }

  memcpy(buf, sp->buf, len);
  sp->buflen -= len;
  my_ret += len;
  memmove(sp->buf, sp->buf + len, sp->buflen);
  pthread_mutex_unlock(sp->mutex);
  return my_ret;  
}

static void socket_port_batch_read_start(socket_port_t* port){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  pthread_mutex_lock(port->mutex);
}
static void socket_port_batch_read_end(socket_port_t* port){
  if (!port->open){
    dfsch_error("Port is already closed", (dfsch_object_t*)port);
  }
  
  pthread_mutex_unlock(port->mutex);
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
    if (ret == 0){
      return EOF;
    }

    ch = sp->buf[0];
    sp->buf = sp->buf;
    sp->bufhead = sp->buf + 1;
    sp->buflen = ret - 1;
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
  sp->mutex = dfsch_create_finalized_mutex();

  return sp;
}

dfsch_type_t dfsch_socket_port_error_type = 
  DFSCH_CONDITION_TYPE_INIT(DFSCH_RUNTIME_ERROR_TYPE, 
                            "socket-port:error");
static void gai_error(int e, char* hostname, char* service){
  dfsch_object_t* c = 
    dfsch_make_condition(DFSCH_SOCKET_PORT_ERROR_TYPE);
  char* m = gai_strerror(e);

  dfsch_condition_put_field_cstr(c, "error-code", DFSCH_MAKE_FIXNUM(e));
  if (hostname){
    dfsch_condition_put_field_cstr(c, "hostname", 
                                   dfsch_make_string_cstr(hostname));
  }
  if (service) {
    dfsch_condition_put_field_cstr(c, "service", 
                                   dfsch_make_string_cstr(service));
  }
  dfsch_condition_put_field_cstr(c, "message", dfsch_make_string_cstr(m));
  dfsch_signal(c);
}
static void socket_error(char* m, char* hostname, char* service){
  dfsch_object_t* c = 
    dfsch_make_condition(DFSCH_SOCKET_PORT_ERROR_TYPE);

  if (hostname){
    dfsch_condition_put_field_cstr(c, "hostname", 
                                   dfsch_make_string_cstr(hostname));
  }
  if (service) {
    dfsch_condition_put_field_cstr(c, "service", 
                                   dfsch_make_string_cstr(service));
  }
  dfsch_condition_put_field_cstr(c, "message", dfsch_make_string_cstr(m));
  dfsch_signal(c);
}




dfsch_object_t* dfsch_socket_port_tcp_connect(char* hostname,
                                              char* service){
  struct addrinfo hints;
  struct addrinfo* res;
  struct addrinfo* i;
  int ret;
  int fd;


  memset(&hints, 0, sizeof(hints));

  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_V4MAPPED | AI_ADDRCONFIG;
  hints.ai_protocol = 0;

  ret = getaddrinfo(hostname, service, &hints, &res);
  if (ret != 0){
    gai_error(ret, hostname, service);
  }

  for (i = res; i != NULL; i = i ->ai_next){
    fd = socket(i->ai_family, i->ai_socktype, i->ai_protocol);
    if (fd == -1){
      continue;
    }
    
    if (connect(fd, i->ai_addr, i->ai_addrlen) != -1){
      freeaddrinfo(res);
      return cons_socket_port(dfsch_saprintf("inet %s:%s", hostname, service),
                              fd);
    }
    close(fd);
  }
  freeaddrinfo(res);
  socket_error("No usable addresses", hostname, service);
}
dfsch_object_t* dfsch_socket_port_unix_connect(char* path){
  int sock;
  struct sockaddr_un unix_addr;

  sock=socket(PF_UNIX,SOCK_STREAM,0);

  if (sock < 0){
    perror("socket");
    return -1;
  }

  unix_addr.sun_family=AF_UNIX;
  strcpy(unix_addr.sun_path,path);

  if (connect(sock,(struct sockaddr*)&unix_addr, sizeof(unix_addr)) < 0){
    int sav = errno;
    close(sock);
    dfsch_operating_system_error_saved(sav, "connect");
  }


  return cons_socket_port(dfsch_saprintf("unix %s", path),
                          sock);
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
                                             char* service){
  struct addrinfo hints;
  struct addrinfo* res;
  struct addrinfo* i;
  int val = 1;
  int ret;
  int fd;


  memset(&hints, 0, sizeof(hints));

  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_ADDRCONFIG | AI_PASSIVE;
  hints.ai_protocol = 0;

  ret = getaddrinfo(hostname, service, &hints, &res);
  if (ret != 0){
    gai_error(ret, hostname, service);
  }

  for (i = res; i != NULL; i = i ->ai_next){
    fd = socket(i->ai_family, i->ai_socktype, i->ai_protocol);
    if (fd == -1){
      continue;
    }
    val = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val));

#ifdef IPV6_V6ONLY
    if (i->ai_family == AF_INET6){
      /* Assume we want both v6 _AND_ v4 clients, on many systems this
       * is disabled by default */
      val = 0;
      setsockopt(fd, IPPROTO_IPV6, IPV6_V6ONLY, &val, sizeof(val));
    }
#endif    

    if (bind(fd, i->ai_addr, i->ai_addrlen) != -1){
      freeaddrinfo(res);

      if (listen(fd, 5) == -1){
        int sav = errno;
        close(fd);
        dfsch_operating_system_error_saved(sav, "listen");
      }
      
      return cons_server_socket(dfsch_saprintf("inet %s:%s", hostname, service),
                                fd);
    }

    close(fd);
  }
  freeaddrinfo(res);
  socket_error("No usable addresses", hostname, service);
}
dfsch_object_t* dfsch_server_socket_unix_bind(char* path){
  int sock;
  struct sockaddr_un unix_addr;

  sock=socket(PF_UNIX,SOCK_STREAM,0);

  if (sock < 0){
    perror("socket");
    return -1;
  }

  unix_addr.sun_family=AF_UNIX;
  strcpy(unix_addr.sun_path,path);

  if (bind(sock,(struct sockaddr*)&unix_addr, sizeof(unix_addr)) < 0){
    int sav = errno;
    close(sock);
    dfsch_operating_system_error_saved(sav, "bind");
  }

  if (listen(sock, 10) < 0){
    int sav = errno;
    close(sock);
    dfsch_operating_system_error_saved(sav, "listen");
  }

  return cons_server_socket(dfsch_saprintf("unix %s", path),
                            sock); 
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
  dfsch_set_error_policy(DFSCH_EP_THREAD);
  DFSCH_UNWIND {
    ctx->cb(ctx->baton, ctx->port);
  } DFSCH_PROTECT {
    dfsch_socket_port_close(ctx->port);
  } DFSCH_PROTECT_END;
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
