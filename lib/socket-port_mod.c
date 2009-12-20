#include <dfsch/lib/socket-port.h>

DFSCH_DEFINE_PRIMITIVE(tcp_connect, NULL){
  char* hostname;
  int port;

  DFSCH_STRING_ARG(args, hostname);
  DFSCH_LONG_ARG(args, port);
  DFSCH_ARG_END(args);

  return dfsch_socket_port_tcp_connect(hostname, port);
}

void dfsch_module_socket_port_register(dfsch_object_t* env){
  dfsch_provide(env, "socket-port");

  dfsch_defconst_cstr(env, "<socket-port>", DFSCH_SOCKET_PORT_TYPE);
  dfsch_defconst_cstr(env, "tcp-connect", DFSCH_PRIMITIVE_REF(tcp_connect));
}
