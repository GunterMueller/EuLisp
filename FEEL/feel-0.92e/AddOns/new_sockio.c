/*
  * New Socket interface using reader 
  * module
  */

/**
  * Functions:
  *   socket-read
  *   socket-write
  */

#include <stdio.h>


#include <stdio.h>
#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "error.h"
#include "allocate.h"
#include "class.h"
#include "modboot.h"
#include "bootstrap.h"
#include "allocate.h"
#include "ngenerics.h"
#include "calls.h"

#include "obread.h"

#define SBUFSIZ (1024*16)

#define EUBUG(x) 

static EUFUN_2( Fn_socket_read_object, sock, reader)
{
 extern char *sys_errlist[];
 extern int errno;

 static unsigned char buf[SBUFSIZ];
 unsigned char *p_ptr=buf;
 LispObject ans;
 int n,len;
 
  n=read(sock->SOCKET.socket,(char *) &len,sizeof(int));
  if (n<0)
    {	
      sprintf((char *) &buf[0],
	      "Socket read[len]: %s",
	      sys_errlist[errno]);
      CallError(stacktop,(char *) &buf[0],sock,NONCONTINUABLE);
    }

 EUBUG(printf("len is: %d\n",len));

 n=read(sock->SOCKET.socket,(char *) buf,len);
 
 ans = read_obj(stacktop,&p_ptr,reader);
  
  return ans;
}
EUFUN_CLOSE;
    
static EUFUN_3( Fn_socket_write_object, sock, object, reader)
{
  static unsigned char buf[SBUFSIZ+sizeof(int)];
  
  int n,len;
  unsigned char *p_ptr,*buf_start;

  buf_start = buf+sizeof(int);

  p_ptr = buf_start;
  write_obj(stacktop,object,&p_ptr,reader);

  len = p_ptr - buf_start;
  EUBUG(printf("len is: %d\n",len));
  bcopy((char *) &len,(unsigned char **) &buf[0],sizeof(int));
  write(sock->SOCKET.socket,buf,len+sizeof(int));
  
  return nil;
}
EUFUN_CLOSE

#define NEW_SOCKET_ENTRIES (2)

MODULE Module_new_sockets;
LispObject Module_new_sockets_values[NEW_SOCKET_ENTRIES];

INIT_new_sockets(LispObject *stacktop)
{
  
  open_module(stacktop,&Module_new_sockets,Module_new_sockets_values,"new-sockets",
	      NEW_SOCKET_ENTRIES);
  
  (void) make_module_function(stacktop,"socket-read",Fn_socket_read_object,-2);
  (void) make_module_function(stacktop,"socket-write",Fn_socket_write_object,-3);
  close_module();
}

  
  
