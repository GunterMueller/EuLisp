/* ******************************************************************** */
/* sockets.c         Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Inter-processes communication	                                */
/* ******************************************************************** */

#define PAUSE() 

/*
 * Change Log:
 *   Version 1, June 1990
 */

#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"

#include "calls.h"
#include "modboot.h"
#include "allocate.h"
#include "modules.h"

#include "symboot.h"
#include "class.h"

#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))

#include "syssockets.h"
#include "sio.h"

/*

 * Socket stuff... 

 */

/* Globals... */

SYSTEM_GLOBAL(char *,host_machine_name);
SYSTEM_GLOBAL(Host *,host_machine_ref);
SYSTEM_GLOBAL(LispObject,host_machine_lisp_name);

/* classes */
static LispObject Socket;
static LispObject Listener;

#define NO_CHAR -1

EUFUN_1( Fn_listernerp, obj)
{
  return((is_listener(obj) ? lisptrue : nil));
}
EUFUN_CLOSE

EUFUN_1( Fn_socketp, obj)
{
  return((is_socket(obj) ? lisptrue : nil));
}
EUFUN_CLOSE

EUFUN_0( Fn_make_listener)
{
  LispObject listener;
  int length;
  int port;

  listener = allocate_listener(stacktop);

  if ((listener->LISTENER.socket = socket(PF_INET,SOCK_STREAM,0)) < 0) 
    CallError(stacktop,
	      "make-listener: unable to make socket",nil,NONCONTINUABLE);

  /* Bind it to the next available unreserved port */

  listener->LISTENER.name.sin_family = AF_INET;
  listener->LISTENER.name.sin_addr.s_addr = htonl(INADDR_ANY);
  listener->LISTENER.name.sin_port = htons(0);

  length = sizeof(SocketInName);

  if (bind(listener->LISTENER.socket,
	   (SocketName *) &(listener->LISTENER.name),
	   length) < 0)
    CallError(stacktop,"make-listener: can't bind socket",nil,NONCONTINUABLE);

  if (getsockname(listener->LISTENER.socket,
		  (SocketName *) &(listener->LISTENER.name),
		  &length) < 0)
    CallError(stacktop,
	      "make-listener: can't get socket data",nil,NONCONTINUABLE);

  listener->LISTENER.state = SOCKET_VIRGIN;

  listen(listener->LISTENER.socket,5); /* One step at a time... */

  lval_classof(listener) = Listener; 
  return(listener);
}
EUFUN_CLOSE

EUFUN_0( Fn_make_socket)
{
  LispObject lispsock;

  lispsock = allocate_socket(stacktop);

  if ((lispsock->SOCKET.socket = socket(PF_INET,SOCK_STREAM,0)) < 0)
    CallError(stacktop,
	      "make-socket: unable to make socket",nil,NONCONTINUABLE);

  lispsock->SOCKET.state = SOCKET_VIRGIN;
  
  lval_classof(lispsock) = Socket;

  return(lispsock);
}
EUFUN_CLOSE
  
EUFUN_1( Fn_listener_id, sock)
{
  LispObject data;

  if (!is_listener(sock))
    CallError(stacktop,"listener-id: not a listener",sock,NONCONTINUABLE);

  if (sock->LISTENER.state != SOCKET_VIRGIN)
    CallError(stacktop,"listener-id: socket not virgin ",sock,NONCONTINUABLE);

  /* Should lock it for parallel safety I suppose... */

  /* Build a list of host machine and port number... */

  if (ntohs(sock->LISTENER.name.sin_port) > 0x7ffffff)
    CallError(stacktop,
	      "listener-id: port number overflow!",sock,NONCONTINUABLE);

  STACK(sock);

  data = (LispObject)
         allocate_integer(stacktop,(int) ntohs(sock->LISTENER.name.sin_port));
  EUCALLSET_2(data , Fn_cons, SYSTEM_GLOBAL_VALUE(host_machine_lisp_name),
	      data);

  return(data);
}
EUFUN_CLOSE

EUFUN_1( Fn_listen, sock)
{
  LispObject new;
  int length;

  if (!is_listener(sock))
    CallError(stacktop,"listen: not a listener",sock,NONCONTINUABLE);

  if (sock->LISTENER.state != SOCKET_VIRGIN)
    CallError(stacktop,"listen: listener not virgin",sock,NONCONTINUABLE);

  /* All is cool... */

  sock->LISTENER.state = SOCKET_LISTENING;

  sock->LISTENER.state = SOCKET_VIRGIN;

  /* Give back a 'copy'... */

  new = allocate_socket(stacktop);
  lval_classof(new) = Socket; 

  new->SOCKET.state = SOCKET_CONNECTED;
  new->SOCKET.lastchar = NO_CHAR;

  length = sizeof(SocketInName);

  new->SOCKET.socket = accept(sock->SOCKET.socket,
			      (SocketName *) &(new->SOCKET.name),
			      &length);

  if (new->SOCKET.socket < 0)
    CallError(stacktop,"listen: unable to accept connection"
	      ,sock,NONCONTINUABLE);

  /* All is cool I think... */

  return(new);
}
EUFUN_CLOSE

EUFUN_1( Fn_connect, list_id)
{
  LispObject sock;
  LispObject hostname,port;
  Host *hostptr;
  SocketInName their_name;

  if (!is_cons(list_id))
    CallError(stacktop,"connect: invalid listener id",list_id,NONCONTINUABLE);

  hostname = CAR(list_id); port = CDR(list_id);

  if (!is_symbol(hostname) || !is_fixnum(port))
    CallError(stacktop,"connect: invalid listener id elts",list_id,NONCONTINUABLE);

  /* Hokay... */

  STACK_TMP(port);
  STACK_TMP(hostname);
  sock = allocate_socket(stacktop);
  UNSTACK_TMP(hostname);
  UNSTACK_TMP(port);
  lval_classof(sock) = Socket; 

  if ((sock->SOCKET.socket = socket(PF_INET,SOCK_STREAM,0)) < 0)
    CallError(stacktop,"connect: can't get socket",list_id,NONCONTINUABLE);

  hostptr = gethostbyname(stringof(hostname->SYMBOL.pname));

  if (hostptr == NULL)
    CallError(stacktop,"connect: unknown host",hostname,NONCONTINUABLE);

  bcopy((char *) (hostptr->h_addr),
	(char *) &(their_name.sin_addr),
	hostptr->h_length);
  their_name.sin_family = AF_INET;
  their_name.sin_port = htons(intval(port));

  if (connect(sock->SOCKET.socket,
	      (SocketName *) &their_name,
	      sizeof(their_name)) < 0) 
    CallError(stacktop,"socket-connect: can't connect",list_id,NONCONTINUABLE);
  
  /* All is cool (hopefully)... */

  sock->SOCKET.state = SOCKET_CONNECTED;
  sock->SOCKET.lastchar=NO_CHAR;

  return(sock);
}
EUFUN_CLOSE

EUFUN_1( Fn_close_listener, list)
{
  if (!is_listener(list))
    CallError(stacktop,"close-listener: not a listeners",list,NONCONTINUABLE);

  if (list->LISTENER.state != SOCKET_VIRGIN)
    CallError(stacktop,"close-listener: not virgin",list,NONCONTINUABLE);

#ifdef notdef
/**/  These lines cause trouble on stardents...
/**/  shutdown(list->LISTENER.socket,2);
/**/
/**/  close(list->LISTENER.socket);
#endif
  list->LISTENER.state = SOCKET_CLOSED;


  return(nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_close_socket, sock)
{
  if (!is_socket(sock))
    CallError(stacktop,"close-socket: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_VIRGIN
      && sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,"close-socket: not virgin",sock,NONCONTINUABLE);

  shutdown(sock->SOCKET.socket,2);

  close(sock->SOCKET.socket);

  sock->SOCKET.state = SOCKET_CLOSED;

  return(nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_socket_write, sock, form)
{
  if (!is_socket(sock))
    CallError(stacktop,"socket-write: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,
	      "socket-write: socket not connected",sock,NONCONTINUABLE);

  /* Set up the buffer... */

  BUFFER_FORM() = form;
  BUFFER_PTR() = 0;

  /* Write form... */

  write_object(stacktop,form);
  *(BUFFER()) = '\0';

/*  fprintf(stderr,"written: '%s'\n",BUFFER_START()); */

  /* OK, now flush the socket... */

  /* catch busted pipe signals */
  
  write(sock->SOCKET.socket,(char *) &(BUFFER_PTR()),sizeof(int));
  write(sock->SOCKET.socket,BUFFER_START(),BUFFER_PTR());

  return(form);
}
EUFUN_CLOSE

EUFUN_2(Fn_socket_write_string,sock,form)
{
  if (!is_socket(sock))
    CallError(stacktop,"socket-write: not a socket",sock,NONCONTINUABLE);
  
  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,
	      "socket-write: socket not connected",sock,NONCONTINUABLE);

  if (write(sock->SOCKET.socket,stringof(form),strlen(stringof(form))) < 0)
    {
      perror("socket_write");
      return nil;
    }
  else
    return lisptrue;
}
EUFUN_CLOSE
#ifdef WITH_SYSTEMV_SOCKETS

#include <stropts.h>
#include <poll.h>

EUFUN_1( Fn_socket_readable_p, sock)
{
  struct pollfd fds[1];
  unsigned long nfds = 1;

  if (!is_socket(sock))
    CallError(stacktop,"socket-readable-p: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,"socket-readable-p: not connected",sock,NONCONTINUABLE);

  fds[0].fd = sock->SOCKET.socket;
  fds[0].events = POLLIN;
  fds[0].revents = 0;

  if (poll(fds,nfds,0) < 0)
    CallError(stacktop,"socket-readable-p: poll failed",sock,NONCONTINUABLE);

  if (fds[0].revents & POLLIN)
    return(lisptrue);
  else
    return(nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_listener_listenable_p, listener)
{
  struct pollfd fds[1];
  unsigned long nfds = 1;

  if (!is_listener(listener))
    CallError(stacktop,
	      "listener_listenable_p: not a listener",listener,NONCONTINUABLE);

  fds[0].fd = listener->SOCKET.socket;
  fds[0].events = POLLIN;
  fds[0].revents = 0;

  if (poll(fds,nfds,0) < 0)
    CallError(stacktop,
	      "socket-readable-p: poll failed",listener,NONCONTINUABLE);

  if (fds[0].revents & POLLIN)
    return(lisptrue);
  else
    return(nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_socket_writable_p, sock)
{
  struct pollfd fds[1];
  unsigned long nfds = 1;

  if (!is_socket(sock))
    CallError(stacktop,"socket-writable-p: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,"socket-writable-p: not connected",sock,NONCONTINUABLE);

  fds[0].fd = sock->SOCKET.socket;
  fds[0].events = POLLOUT;
  fds[0].revents = 0;

  if (poll(fds,nfds,0) < 0)
    CallError(stacktop,"socket-writable-p: poll failed",sock,NONCONTINUABLE);

  if (fds[0].revents & POLLOUT)
    return(lisptrue);
  else
    return(nil);
}
EUFUN_CLOSE

#else

/* BSD... */

#include <sys/time.h>

EUFUN_1( Fn_socket_readable_p, sock)
{
  fd_set mask;
  struct timeval wait;

  if (!is_socket(sock))
    CallError(stacktop,"socket-readable-p: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,"socket-readable-p: not connected",sock,NONCONTINUABLE);

  /* Do a select with 0 timeout... */

  wait.tv_sec = 0;
  wait.tv_usec = 0;

  FD_ZERO(&mask);
  FD_SET(sock->SOCKET.socket,&mask);

  if (select(getdtablesize(),&mask,NULL,NULL,&wait) < 0)
    CallError(stacktop,"socket-readable-p: select failed",sock,NONCONTINUABLE);

  if (FD_ISSET(sock->SOCKET.socket,&mask))
    return(lisptrue);
  else
    return(nil);

  return(nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_listener_listenable_p, listener)
{
  fd_set mask;
  struct timeval wait;

  if (!is_listener(listener))
    CallError(stacktop,
	      "socket-listenable-p: not a listener",listener,NONCONTINUABLE);

  /* Do a select with 0 timeout... */

  wait.tv_sec = 0;
  wait.tv_usec = 0;

  FD_ZERO(&mask);
  FD_SET(listener->LISTENER.socket,&mask);

  if (select(getdtablesize(),&mask,NULL,NULL,&wait) < 0)
    CallError(stacktop,
	      "socket-readable-p: select failed",listener,NONCONTINUABLE);

  if (FD_ISSET(listener->LISTENER.socket,&mask))
    return(lisptrue);
  else
    return(nil);

  return(nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_socket_writable_p, sock)
{
  fd_set mask;
  struct timeval wait;

  if (!is_socket(sock))
    CallError(stacktop,
	      "socket-readable-p: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,
	      "socket-readable-p: not connected",sock,NONCONTINUABLE);

  /* Do a select with 0 timeout... */

  wait.tv_sec = 0;
  wait.tv_usec = 0;

  FD_ZERO(&mask);
  FD_SET(sock->SOCKET.socket,&mask);

  if (select(getdtablesize(),NULL,&mask,NULL,&wait) < 0)
    CallError(stacktop,"socket-readable-p: select failed",sock,NONCONTINUABLE);

  if (FD_ISSET(sock->SOCKET.socket,&mask))
    return(lisptrue);
  else
    return(nil);

  return(nil);

}
EUFUN_CLOSE

#endif

EUFUN_1( Fn_socket_read, sock)
{
  int len,ret;
  LispObject obj;

  if (!is_socket(sock))
    CallError(stacktop,"socket-read: not a socket",sock,NONCONTINUABLE);

  if (sock->SOCKET.state != SOCKET_CONNECTED)
    CallError(stacktop,"socket-read: not connected",sock,NONCONTINUABLE);

#ifdef NOTDEFINED    /* Allow this call to block */
  if (Fn_socket_readable_p(sock) == nil)
    CallError(stacktop,"socket-read: socket unreadable",sock,NONCONTINUABLE);
#endif

  /* Get the length... */

  if ( (ret = read(sock->SOCKET.socket,(char *) &len,sizeof(int))) == -1)
    {
      CallError(stacktop,
		"socket-read: could not read socket",sock,NONCONTINUABLE);
    }

  /* Read the data... */

  BUFFER_START() = sock->SOCKET.buffer;
  
  if ((ret = read(sock->SOCKET.socket,(char *) (BUFFER_START()),len)) == -1)
    {
      CallError(stacktop,
		"socket-read: could not complete socket-read",
		sock,NONCONTINUABLE);
    }

  /*  fprintf(stderr,"read: '%s'\n",BUFFER_START()); */

  *(BUFFER_START() + len) = '\0';
  *(BUFFER_START() + len + 1) = '\n';

  /* Set up buffer... */

  BUFFER_PTR() = 0;

  obj = read_object(stacktop);

  return(obj);
}
EUFUN_CLOSE

static EUFUN_1(Fn_socket_read_char,sock)
{
  char x;
  int ret;

  if (!is_socket(sock))
    CallError(stacktop,"socket-read-char: not a socket",sock,NONCONTINUABLE);
  
  if (sock->SOCKET.lastchar==NO_CHAR)
    {
      ret=read(sock->SOCKET.socket,&x,1);
      if (ret==0)
	return q_eof;
      else
	return allocate_char(stacktop,x);
    }
  else
    {
      x=sock->SOCKET.lastchar;
      sock->SOCKET.lastchar=NO_CHAR;
      return allocate_char(stacktop,x);
    }
}
EUFUN_CLOSE

static EUFUN_2(Fn_socket_unread_char,sock,c)
{
  if (!is_socket(sock))
    CallError(stacktop,"socket-read-char: not a socket",sock,NONCONTINUABLE);
  
  if (sock->SOCKET.lastchar!=NO_CHAR)
    CallError(stacktop,"socket-unread: can't unread further",sock,NONCONTINUABLE);
  else
    sock->SOCKET.lastchar=c->CHAR.code;
  
  return nil;
}
EUFUN_CLOSE
/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */


#define SOCKETS_ENTRIES 19
MODULE Module_sockets;
LispObject Module_sockets_values[SOCKETS_ENTRIES];

void initialise_sockets(LispObject *stacktop)
{
  extern LispObject Standard_Class,Object, Primitive_Class;

  Socket = (LispObject) allocate_class(stacktop,NULL);
  add_root(&Socket);
  Listener = (LispObject) allocate_class(stacktop,NULL);	
  add_root(&Listener);

  SYSTEM_INITIALISE_GLOBAL(char *,host_machine_name,NULL);
  SYSTEM_INITIALISE_GLOBAL(Host *,host_machine_ref,NULL);
  SYSTEM_INITIALISE_GLOBAL(LispObject,host_machine_lisp_name,NULL);
  ADD_SYSTEM_GLOBAL_ROOT(host_machine_lisp_name);

  SYSTEM_GLOBAL_VALUE(host_machine_name) = (char *) malloc(64);
  gethostname(SYSTEM_GLOBAL_VALUE(host_machine_name),64);

  SYSTEM_GLOBAL_VALUE(host_machine_lisp_name)
    = (LispObject) get_symbol(stacktop,(char*)SYSTEM_GLOBAL_VALUE(host_machine_name));

  open_module(stacktop,
	      &Module_sockets,Module_sockets_values,"sockets",SOCKETS_ENTRIES);
  

  (void) make_module_function(stacktop,"socketp",Fn_socketp,1);
  (void) make_module_function(stacktop,"make-listener",
			      Fn_make_listener,0);
  (void) make_module_function(stacktop,"make-socket",
			      Fn_make_socket,0);
  (void) make_module_function(stacktop,"listener-id",Fn_listener_id,1);
  (void) make_module_function(stacktop,"listen",Fn_listen,1);
  (void) make_module_function(stacktop,"connect",Fn_connect,1);
  (void) make_module_function(stacktop,"close-listener",Fn_close_listener,1);
  (void) make_module_function(stacktop,"close-socket",Fn_close_socket,1);
  (void) make_module_function(stacktop,"socket-write",Fn_socket_write,2);
  (void) make_module_function(stacktop,"socket-write-string",Fn_socket_write_string,2);
  (void) make_module_function(stacktop,"socket-read",Fn_socket_read,1);
  (void) make_module_function(stacktop,"socket-read-char",Fn_socket_read_char,1);
  (void) make_module_function(stacktop,"socket-unread-char",Fn_socket_unread_char,2);
  (void) make_module_function(stacktop,"socket-readable-p",Fn_socket_readable_p,1);
  (void) make_module_function(stacktop,"socket-writable-p",Fn_socket_writable_p,1);
  (void) make_module_function(stacktop,"listener-listenable-p",Fn_listener_listenable_p,1);
  (void) make_module_entry(stacktop,"<listener>",Listener);
  (void) make_module_entry(stacktop,"<socket>",Socket);
  (void) make_module_entry(stacktop,"*sockets-available*", lisptrue);
  close_module();

}

#else /* no sockets */

/* For booting convenience we just define a couple of classes */
#define SOCKETS_ENTRIES 19
MODULE Module_sockets;
LispObject Module_sockets_values[SOCKETS_ENTRIES];
static LispObject Socket, Listener;

void initialise_sockets(LispObject *stacktop)
{
  open_module(stacktop,
              &Module_sockets,Module_sockets_values,"sockets",SOCKETS_ENTRIES);

  Socket = (LispObject) allocate_class(stacktop,NULL);
  add_root(&Socket);
  Listener = (LispObject) allocate_class(stacktop,NULL);
  add_root(&Listener);


  (void) make_module_function(stacktop,"socketp",NULL,1);
  (void) make_module_function(stacktop,"make-listener", NULL, 0);
  (void) make_module_function(stacktop,"make-socket", NULL, 0);
  (void) make_module_function(stacktop,"listener-id",NULL,1);
  (void) make_module_function(stacktop,"listen",NULL,1);
  (void) make_module_function(stacktop,"connect",NULL,1);
  (void) make_module_function(stacktop,"close-listener",NULL,1);
  (void) make_module_function(stacktop,"close-socket",NULL,1);
  (void) make_module_function(stacktop,"socket-write",NULL,2);
  (void) make_module_function(stacktop,"socket-write-string",NULL,2);
  (void) make_module_function(stacktop,"socket-read",NULL,1);
  (void) make_module_function(stacktop,"socket-read-char",NULL,1);
  (void) make_module_function(stacktop,"socket-unread-char",NULL,2);
  (void) make_module_function(stacktop,"socket-readable-p",NULL,1);
  (void) make_module_function(stacktop,"socket-writable-p",NULL,1);
  (void) make_module_function(stacktop,"listener-listenable-p",NULL,1);

  (void) make_module_entry(stacktop,"<listener>",Listener);
  (void) make_module_entry(stacktop,"<socket>",Socket);
  (void) make_module_entry(stacktop,"*sockets-available*", nil);

  close_module();
}
  
#endif
