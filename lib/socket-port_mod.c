#include <dfsch/lib/socket-port.h>

DFSCH_DEFINE_PRIMITIVE(tcp_connect, NULL){
  char* hostname;
  char* service;

  DFSCH_STRING_ARG(args, hostname);
  DFSCH_STRING_ARG(args, service);
  DFSCH_ARG_END(args);

  return dfsch_socket_port_tcp_connect(hostname, service);
}

DFSCH_DEFINE_PRIMITIVE(tcp_bind, NULL){
  char* hostname;
  char* service;

  DFSCH_STRING_ARG(args, hostname);
  DFSCH_STRING_ARG(args, service);
  DFSCH_ARG_END(args);

  return dfsch_server_socket_tcp_bind(hostname, service);
}

DFSCH_DEFINE_PRIMITIVE(unix_connect, NULL){
  char* path;

  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  return dfsch_socket_port_unix_connect(path);
}

DFSCH_DEFINE_PRIMITIVE(unix_bind, NULL){
  char* path;

  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  return dfsch_server_socket_unix_bind(path);
}

DFSCH_DEFINE_PRIMITIVE(server_socket_accept, NULL){
  dfsch_object_t* server_socket;
  DFSCH_OBJECT_ARG(args, server_socket);
  DFSCH_ARG_END(args);

  return dfsch_server_socket_accept(server_socket);
}

DFSCH_DEFINE_PRIMITIVE(socket_port_close, NULL){
  dfsch_object_t* socket_port;
  DFSCH_OBJECT_ARG(args, socket_port);
  DFSCH_ARG_END(args);

  dfsch_socket_port_close(socket_port);
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(server_socket_close, NULL){
  dfsch_object_t* server_socket;
  DFSCH_OBJECT_ARG(args, server_socket);
  DFSCH_ARG_END(args);

  dfsch_server_socket_close(server_socket);
  return NULL;
}

static void apply_one(dfsch_object_t* proc, dfsch_object_t* arg){
  dfsch_apply(proc, dfsch_cons(arg, NULL));
}

DFSCH_DEFINE_PRIMITIVE(server_socket_run_accept_loop, NULL){
  dfsch_object_t* server_socket;
  dfsch_object_t* client_proc;
  DFSCH_OBJECT_ARG(args, server_socket);
  DFSCH_OBJECT_ARG(args, client_proc);
  DFSCH_ARG_END(args);

  dfsch_server_socket_run_accept_loop(server_socket, apply_one, client_proc);
  return NULL;
}


void dfsch_module_socket_port_register(dfsch_object_t* env){
  dfsch_provide(env, "socket-port");

  dfsch_defcanon_cstr(env, "<socket-port>", DFSCH_SOCKET_PORT_TYPE);
  dfsch_defcanon_cstr(env, "<server-socket>", DFSCH_SERVER_SOCKET_TYPE);
  dfsch_defcanon_cstr(env, "tcp-connect", DFSCH_PRIMITIVE_REF(tcp_connect));
  dfsch_defcanon_cstr(env, "tcp-bind", DFSCH_PRIMITIVE_REF(tcp_bind));

  dfsch_defcanon_cstr(env, "unix-connect", DFSCH_PRIMITIVE_REF(unix_connect));
  dfsch_defcanon_cstr(env, "unix-bind", DFSCH_PRIMITIVE_REF(unix_bind));

  dfsch_defcanon_cstr(env, "server-socket-accept", 
                      DFSCH_PRIMITIVE_REF(server_socket_accept));
  dfsch_defcanon_cstr(env, "server-socket-close!", 
                      DFSCH_PRIMITIVE_REF(server_socket_close));
  dfsch_defcanon_cstr(env, "socket-port-close!", 
                      DFSCH_PRIMITIVE_REF(socket_port_close));

  dfsch_defcanon_cstr(env, "server-socket-run-accept-loop", 
                      DFSCH_PRIMITIVE_REF(server_socket_run_accept_loop));
}
