/* ******************************************************************** */
/* syssockets.h      Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Inter-processes communication types	                                */
/* ******************************************************************** */

#ifndef SYSSOCKETS_H
#define SYSSOCKETS_H

#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))

/*
#include <sys/ioctl.h>
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <ctype.h>
#include <netdb.h>
#include <signal.h>

extern int getdtablesize(void);

/* My types... */

typedef int SocketHandle;
typedef struct sockaddr SocketName;
typedef struct sockaddr_in SocketInName;
typedef int SocketPort;

typedef struct hostent Host;

#define SOCKET_VIRGIN (1)
#define SOCKET_LISTENING (2)
#define SOCKET_CONNECTED (3)
#define SOCKET_CLOSED (4)

#endif

#define SOCKET_BUFFER_SIZE 10240

#endif

