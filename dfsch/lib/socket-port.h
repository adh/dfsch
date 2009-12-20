#ifndef H__dfsch__socket_port__
#define H__dfsch__socket_port__

#include <dfsch/dfsch.h>
#include <dfsch/ports.h>

extern dfsch_port_type_t dfsch_socket_port_type;
#define DFSCH_SOCKET_PORT_TYPE (&dfsch_socket_port_type)

dfsch_object_t* dfsch_socket_port_connect_inet(char* hostname,
                                               int port);
dfsch_object_t* dfsch_socket_port_connect_unix(char* path);
dfsch_object_t* dfsch_socket_port_accept(int fd);

#endif
