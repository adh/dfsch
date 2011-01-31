#ifndef H__dfsch__socket_port__
#define H__dfsch__socket_port__

#include <dfsch/dfsch.h>
#include <dfsch/ports.h>

extern dfsch_port_type_t dfsch_socket_port_type;
#define DFSCH_SOCKET_PORT_TYPE (&dfsch_socket_port_type)
extern dfsch_type_t dfsch_socket_port_error_type;
#define DFSCH_SOCKET_PORT_ERROR_TYPE (&dfsch_socket_port_error_type)

dfsch_object_t* dfsch_socket_port_tcp_connect(char* hostname,
                                              char* service);
dfsch_object_t* dfsch_socket_port_unix_connect(char* path);

void dfsch_socket_port_close(dfsch_object_t* spo);

extern dfsch_type_t dfsch_server_socket_type;
#define DFSCH_SERVER_SOCKET_TYPE (&dfsch_server_socket_type)

dfsch_object_t* dfsch_server_socket_tcp_bind(char* hostname,
                                             char* service);
dfsch_object_t* dfsch_server_socket_unix_bind(char* path);
void dfsch_server_socket_close(dfsch_object_t* sso);

dfsch_object_t* dfsch_server_socket_accept(dfsch_object_t* server_socket);

typedef void (*dfsch_server_socket_accept_loop_cb_t)(void* baton,
                                                     dfsch_object_t* port);
void dfsch_server_socket_run_accept_loop(dfsch_object_t* server_socket,
                                         dfsch_server_socket_accept_loop_cb_t cb,
                                         void* baton);

#endif
