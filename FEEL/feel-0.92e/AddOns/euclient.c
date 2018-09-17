
/* 
  Project: YY-X
  File: Euclient.c
  Created: 19/9/90
  ANSIfied: 1/10/90
*/

/* ***
  Eulisp 'C' module interface to YY-X
 
  Current implementation handles commands and 
  instructions only

  Functions:
    Initialise(port-no,server)  connects with the server
    YY-Function(cmd_num,args)   Executes commands
    YY-Function-Info(cmd_num)   Cheap help system  -- I'll write it one day

  PROBLEMS:
    Malloc abuse
  *** */

#ifdef WITH_SYSTEMV_SOCKETS
#include <poll.h>
#endif
#ifdef notdef /* Stream ioctls */
#include <stropts.h>
#endif
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h> 
#ifdef hlh
#include "orion-types.h"
#endif


/* #include <sys/resource.h>*/
/* #include <sys/socket.h> Included elsewhere*/
#include <sys/un.h>
/* #include <netinet/in.h> */
/* #include <netdb.h> Included elsewhere */
#include <strings.h>

/* Eulisp Stuff */
#include "defs.h"
#include "funcalls.h"
#include "structs.h"
#include "global.h"
#include "error.h"
#include "bootstrap.h"

/* YY stuff */
#include "yydefs.h"
#include "yypacket.h"
#include "eucmd.h"

/* Magic number */
#define YYMAGIC                         14876

#define CLIENTTIMEOUT_INUSEC            2000
/* incoming buffer size */
#define MAX_REPLY_STRING                 200

#define YYHEADERSIZE   sizeof(yy_packet_header)

/* Some Global Values */
static SYSTEM_GLOBAL(int,packet_size);

/* This should be safe... */
/* assuming I set it each time I enter the routine */
int YYPacketBlockSize;

	/* File descriptors */
static SYSTEM_GLOBAL(int,fd1);
static SYSTEM_GLOBAL(int,fd2);	

/* MAIN PROTOCOL FUNCTION */

extern command_entry ClientCmdTable[];

/* lispy stuff */
#define ALLOCATE_PAIR(car,cdr)   EUCALL_2(Fn_cons,(car),(cdr))


EUFUN_2(Fn_YY_Function, cmd_num, arg_list)
{
  LispObject packet2list(LispObject *stacktop,yy_packet **p_pkt, reply_entry *re);
  int read_reply2q(int sock,int cmd_num, yy_packet_system_queue *p_que,
		   yy_packet **p_pkt);
  void make_packet(LispObject *,int cmd,LispObject alist,yy_packet **p_pkt);
  int send_packet(int sock, yy_packet *pkt);

  yy_packet_system_queue que;
  yy_packet *pkt;
  reply_entry *re;

  /* Needed for returning stuff */
  LispObject ret_list;
  
  /* Stack ... */

  /* Set a global value */
  YYPacketBlockSize = SYSTEM_GLOBAL_VALUE(packet_size);

  if (!is_fixnum(cmd_num))
    CallError(stacktop,"YY-Funcion: Was expecting an integer command number", nil, NONCONTINUABLE);
	  

  make_packet(stacktop,intval(cmd_num),arg_list,&pkt);

  fix_yy_packet(pkt);

  if (!send_packet(SYSTEM_GLOBAL_VALUE(fd1),pkt))
    CallError(stacktop, "sending error", nil,NONCONTINUABLE);

  /* Free packet structure (!) */
  remove_yy_packet(pkt);
  
  /* Handle incoming junk */
  /* And return it */

  re = ClientCmdTable[intval(cmd_num)].ceReplyData;
  if (re!=NULL)
    {
      ret_list = nil;
      read_reply2q(SYSTEM_GLOBAL_VALUE(fd1),intval(cmd_num),&que,&pkt);

      ret_list=packet2list(stacktop,&pkt,re);
      return ret_list;
    }
  else
    {
      return nil;
    }
}
EUFUN_CLOSE

int read_reply2q(int sock,int cmd_num,
		 yy_packet_system_queue *p_que,
		 yy_packet **p_pkt)
{
  do
    {
      recv_packet(sock,p_que);
      *p_pkt= (yy_packet *) get_packet_from_readq(p_que);
    } while ( (*p_pkt)->pktCommand != cmd_num);
}

int read_event2q(int sock,
		 yy_packet_system_queue *p_que,
		 yy_packet **p_pkt,
		 int block)
{
  int poll_eventq(int fd);

  if (!block || poll_eventq(sock))
    {
      recv_packet(sock,p_que);
      *p_pkt = (yy_packet *) get_packet_from_readq(p_que);
      return TRUE;
    }
  else
    return FALSE;
}

/*
  * Event Handling function 
  */

EUFUN_1( Fn_get_next_event, block)
{     
  LispObject packet2list(LispObject *stacktop,yy_packet **p_pkt, reply_entry *re);
  LispObject ret_list=nil;
  yy_packet_system_queue que;
  yy_packet *pkt;
  reply_entry *re;

  if (read_event2q(SYSTEM_GLOBAL_VALUE(fd2),&que,&pkt,(block==nil?TRUE:FALSE)))
    {	
      int rval;

      re = ClientCmdTable[pkt->pktCommand].ceReplyData;
      ret_list=packet2list(stacktop,&pkt,re);
      return ret_list;
    }
  else 
    {
      return nil;
    }
}
EUFUN_CLOSE

LispObject packet2list(LispObject *stacktop,yy_packet **p_pkt, reply_entry *re)
{
  LispObject read_font_data(LispObject *stacktop, yy_packet *packet);
  LispObject ret=nil;
  LispObject end;

  char buf[MAX_REPLY_STRING];
  int len;

  if (re->reType== -1)
    return ret;
  else
    {
      while (re->reType!= -1)
	{
	  if (ret==nil)
	    {
	      ret = ALLOCATE_PAIR(nil,nil);
	      end = ret;
	    }
	  else 
	    {
	      CDR(end)=ALLOCATE_PAIR(nil,nil);
	      end=CDR(end);
	    }
	  switch(re->reType)
	    {
	    case ARG_STR:
	      len = read_packet_entry_integer(*p_pkt);
	      read_packet_entry_string(*p_pkt,len,buf);
	      CAR(end)=allocate_string(stacktop,buf,len);
	      break;
	      
	    case ARG_INT:
	      CAR(end) = real_allocate_integer(stacktop,read_packet_entry_integer(*p_pkt));
	      break;
	      
	    case ARG_FONTDATA:
	      CAR(end)=read_font_data(stacktop,*p_pkt);
	      break;

	    default:
	      fprintf(stderr,"Error in YY reply data\n");
	      break;
	    }
	  re ++;
	}	
    }
  remove_yy_packet(*p_pkt);
  return ret;
}

void make_packet(LispObject *stacktop,int cmd,LispObject alist,yy_packet **p_pkt)
{
  yy_packet *pkt;
  cmd_arg *ce;
  LispObject point_list;

  pkt = alloc_new_yy_packet(cmd,YYPACKETTYPE_COMMAND,0,NULL,0);
  
  if (ClientCmdTable[cmd].cmdArgs==NULL)
    CallError(stacktop,"YY-function: Unimplemented Operation", nil, NONCONTINUABLE);    

  ce = ClientCmdTable[cmd].cmdArgs;

  /* Send the data */
  while(ce->argType!= -1)
    {
      if (null(alist))
	{
	  CallError(stacktop,"Not enough arguments to YY-function",nil,NONCONTINUABLE);		
	}	

      switch(ce->argType)
	{
	case ARG_INT:
	  if (!is_fixnum(CAR(alist)))
	    CallError(stacktop,"YY-function: Expecting an integer", nil, NONCONTINUABLE);
	  
	  append_packet_entry_integer(pkt,intval((CAR(alist))));
	  break;
	  
	case ARG_STR:
	  if (!is_string(CAR(alist)))
	    CallError(stacktop,"YY-Function: YY-Function: Expecting a string",
		      CAR(alist),
		      NONCONTINUABLE);

	  append_packet_entry_string(pkt,stringof(CAR(alist)));
	  break;

	  /* Want a list with even number of elements */
	case ARG_PAIR_LIST:
	  point_list = CAR(alist);
	  if (null(point_list))
	    CallError(stacktop,"YY-Function: Lists must be non-empty",point_list,NONCONTINUABLE);

	      while (!null(point_list))
		{
		  if (!is_cons(point_list))
		    CallError(stacktop,"YY-Function: YY-Function: Expecting a list",
			      point_list, NONCONTINUABLE);
	  
		  if (!is_cons(CAR(point_list)))
		    CallError(stacktop,"YY-function: Need list-of coord pairs",
			      CAR(point_list), NONCONTINUABLE);

		  if (!is_fixnum(CAR(CAR(point_list))))
		    CallError(stacktop,"YY-function: Expecting an integer [1]", CAR(point_list),
			      NONCONTINUABLE);

		  if (!is_fixnum(CDR(CAR(point_list))))
		    CallError(stacktop,"YY-function: Expecting an integer [2]",
			      (CDR(CAR(point_list))),
			      NONCONTINUABLE);
		  
		  append_packet_entry_integer(pkt,intval(CAR(CAR(point_list))));
		  append_packet_entry_integer(pkt,intval(CDR(CAR(point_list))));
		  
		  point_list = (CDR(point_list));
		}
	  break;

	default:
	  fprintf(stderr,"Error in YY init data\n");
	  break;
	}
      ce ++;
      alist = CDR(alist);
    }
  
  if(!null(alist))
    {	
      CallError(stacktop,"Too many arguments to YY-Function",alist,NONCONTINUABLE);
    }
  
  *p_pkt = pkt;

}
/* Reading the font data */
/*  ****
* From:   (according to the documentation)
*  font_char $B9=B$BN(B for each ascii character 
*   struct font_char {
*     char code;        charcter code
*     char width;       width     
*     char height;      height    
*     char base;        base-height    
*   };
*
*    font $B9=B$BN(B for a font set
*  struct font {
*     int width;                           fixed width for kanji 
*     int height;                          fixed height for kanji  
*     int base;                            fixed base-height for kanji 
*     struct font_char code_char[256];     256 font_char structs 
*   };
*
*  into: 
*    (width height base (code width height base) ...)
*   *** */
LispObject read_font_data(LispObject *stacktop,yy_packet *packet,LispObject *l_ptr)
{
  LispObject res;
  LispObject font_ptr;
  LispObject char_ptr;
  int i;

  res = ALLOCATE_PAIR(nil,nil);
  font_ptr=res;
  /* width */
  CAR(font_ptr)=real_allocate_integer(stacktop,read_packet_entry_integer(packet));
  /* height */
  CDR(font_ptr) = ALLOCATE_PAIR(real_allocate_integer(stacktop,read_packet_entry_integer(packet)),nil);
  font_ptr = CDR(font_ptr);
  /* base */
  CDR(font_ptr) = ALLOCATE_PAIR(real_allocate_integer(stacktop,read_packet_entry_integer(packet)),nil);
  font_ptr = CDR(font_ptr);

  for (i=0; i<256 ; i++)
    {
      CDR(font_ptr)=ALLOCATE_PAIR(nil,nil);
      font_ptr = CDR(font_ptr);
      
      char_ptr=  ALLOCATE_PAIR(real_allocate_integer(stacktop,read_packet_entry_onebyte(packet)),nil);
      CAR(font_ptr)=char_ptr;

      /* width */
      CDR(char_ptr) =  ALLOCATE_PAIR(real_allocate_integer(stacktop,read_packet_entry_onebyte(packet)),nil);
      char_ptr = CDR(char_ptr);
      /* Height */
      CDR(char_ptr)=  ALLOCATE_PAIR(real_allocate_integer(stacktop,read_packet_entry_onebyte(packet)),nil);
      char_ptr = CDR(char_ptr);
      /* Base */
      CDR(char_ptr) =  ALLOCATE_PAIR(real_allocate_integer(stacktop,read_packet_entry_onebyte(packet)),nil);
      char_ptr = CDR(char_ptr);
    }
  return res;
}

    
      
int poll_eventq(int fd)
{
#ifdef WITH_SYSTEMV_SOCKETS  
  {
    struct pollfd rfds[1];

    int timeout;
    int ret;
  
    rfds[0].fd= fd;
    rfds[0].events= POLLIN;
    rfds[0].revents= 0;

    timeout = 10;
    if ((ret = poll(rfds, 1, timeout)) < 0) 
      {
	perror("poll(event)");
      }

    if (rfds[0].revents & POLLIN)
      return TRUE;
    else	
      return FALSE;
  }
#endif
#ifdef WITH_BSD_SOCKETS
  {
    fd_set readfds;
    struct timeval timeout;
    int ret;

    timeout.tv_sec = 0;
    timeout.tv_usec = 10;

    FD_ZERO(&readfds);
    FD_SET(fd,&readfds);
    if ( (ret =select (fd, &readfds, NULL, NULL, timeout)) == 1)
      return TRUE;
    else 
      {
	if (ret == -1)
	  {
	    perror("Poll_fd:Select");
	    exit(1);
	  }
	else 
	  return FALSE;
      }
  }
#endif  
}

/* STARTUP FUNCTIONS */
EUFUN_2( Fn_setup_server, hostname, dnumber)
{
	int data, nwords;
	int idnumber;

	idnumber= intval(dnumber);
	
	debug_init(".yydebug",stderr,NULL,NULL);

	/* $B%A%'%C%/(B $B%M%C%H%o!<%/(B */
	SYSTEM_GLOBAL_VALUE(fd1) = get_inet_domain(stringof(hostname), idnumber);
	
	data = YYMAGIC;
	/* $B%^%8%C%/HV9fAw?.(B */
	write(fd1,&data,4);

	
	data = 1024;  /* My preffered data size in bytes*/
	nwords = data>>2;

	write(fd1,&nwords,4);

	read(fd1,&data,4);

	/* $B%A%'%C%/(B */
	if( data != YYMAGIC ){
	  close(SYSTEM_GLOBAL_VALUE(fd1));
		return(nil);
	}
	idnumber++;
	
	/* $B%Q%1%C%H%5%$%:(B $B<u?.(B */
	read(SYSTEM_GLOBAL_VALUE(fd1),&data,4);

	SYSTEM_GLOBAL_VALUE(fd2) = get_inet_domain(stringof(hostname), idnumber);

	SYSTEM_GLOBAL_VALUE(packet_size)=data<<2;
	fprintf(stderr,"Initialised[%d]\n",data<<2);
	fflush(stderr);
	return lisptrue;
}
EUFUN_CLOSE

int get_inet_domain(char *host, int no)
{
        int sock;
        struct sockaddr_in addr;
        struct hostent *hp;
        bzero((char *)&addr, sizeof(addr));
        if ((hp = gethostbyname(host)) == (struct hostent *)NULL) {
                fprintf(stderr, "Unknown host %s\n", host);
                return -1;
        }
        addr.sin_family = hp->h_addrtype;
#ifdef hlh /* 4.2 BSD socketism */
	bcopy(hp->h_addr, &addr.sin_addr, hp->h_length);
#else
        bcopy(*hp->h_addr_list, &addr.sin_addr, hp->h_length);
#endif
        addr.sin_port = (YYPROTO_INET_PORT+no);
        if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
                perror("socket");
                return -1;
        }
        if (connect(sock, &addr, sizeof(addr)) < 0) {
                perror("connect");
                return -1;
        }
        fprintf(stderr, "Connect on %d/INET\n", sock);
        return sock;
}


/* 
  Shamelessly nicked from kclient
  */
 cmd_arg yy_init_arg[] = {
{ ARG_INT, YYPROTO_MAGIC, NULL, "Magic" },
{ ARG_INT, YYPROTO_VERSION, NULL, "Version" },
{ ARG_INT, YYPROTO_RELEASE, NULL, "Release" },
{ ARG_INT, YYPROTO_SYNC_COUNTER, NULL, "Sync" },
{ ARG_INT, YYPROTO_MOUSE_STAY_TIME, NULL, "Mouse" },
{ ARG_STR, 0, "prophet:0", "Server" },
{ ARG_STR, 0, NULL, "Window Name" },
{ ARG_STR, 0, NULL, "Icon Name" },
{ -1, 0, NULL, NULL }
} ;

reply_entry yy_init_reply[]= {
{ARG_INT,"Connect. Id"},
{ARG_INT,"Version"},
{ARG_INT,"Release"},
{ARG_INT,"Sync"},
{ARG_INT,"Mouse"},
{ARG_INT,"Width"},
{ARG_INT,"Height"},
{ARG_STR,"label"},
{-1,NULL}};

 cmd_arg yy_create_arg[] = {
{ ARG_INT, 0, NULL, "X Position" },
{ ARG_INT, 0, NULL, "Y Position" },
{ ARG_INT, YYWINDEFAULTWIDTH, NULL, "Windth" },
{ ARG_INT, YYWINDEFAULTHEIGHT, NULL, "Height" },
{ ARG_INT, -1, NULL, "Parent" },
{ ARG_INT, 1, NULL, "Display" },
{ ARG_INT, 1, NULL, "Drawable" },
{ -1, 0, NULL, NULL }
} ;

reply_entry yy_create_reply[] = {
{ ARG_INT, "Territory #"},
{ -1, NULL}};

 cmd_arg yy_display_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "Visible" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_move_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_resize_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, -1, NULL, "Windth" },
{ ARG_INT, -1, NULL, "Height" },
{ ARG_INT, -1, NULL, "New X Position" },
{ ARG_INT, -1, NULL, "New Y Position" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_destroy_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_reparent_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "Parent" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_raise_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_lower_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ -1, 0, NULL, NULL }
} ;

 cmd_arg yy_draw_point_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_draw_line_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X1 Position" },
{ ARG_INT, -1, NULL, "Y1 Position" },
{ ARG_INT, -1, NULL, "X2 Position" },
{ ARG_INT, -1, NULL, "Y2 Position" },
{ ARG_INT, 1, NULL, "Line Width" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Edge" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Dash" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_draw_rectangle_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, -1, NULL, "Width" },
{ ARG_INT, -1, NULL, "Height" },
{ ARG_INT, 1, NULL, "Line Width" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Dash" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_draw_lines_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, 3, NULL, "Number of Points" },
{ ARG_PAIR_LIST, -1, NULL, "Points"},
{ ARG_INT, 1, NULL, "Line Width" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Edge" },
{ ARG_INT, 0, NULL, "Joint" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Dash" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_draw_polygon_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, 3, NULL, "Number of Points" },
{ ARG_PAIR_LIST, -1, NULL, "Positions"},
{ ARG_INT, 1, NULL, "Line Width" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Joint" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Dash" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_draw_circle_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, -1, NULL, "Radious" },
{ ARG_INT, 1, NULL, "Line Width" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Dash" },
{ -1, 0, NULL, NULL }
} ;

 cmd_arg yy_fill_circle_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, -1, NULL, "Radious" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Pattern" },
{ -1, 0, NULL, NULL }
} ;

cmd_arg yy_draw_fill_rectangle_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, -1, NULL, "Width" },
{ ARG_INT, -1, NULL, "Height" },
{ ARG_INT, -1, NULL, "Operation"},
{ ARG_INT, -1, NULL, "Colour"},
{ ARG_INT, -1, NULL, "Fill"},
{ -1, -1, NULL, NULL}};


 cmd_arg yy_draw_arc_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, -1, NULL, "Radious" },
{ ARG_INT, -1, NULL, "Angle1" },
{ ARG_INT, -1, NULL, "Angle2" },
{ ARG_INT, 1, NULL, "Line Width" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 0, NULL, "Dash" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_load_font_arg[] = {
{ ARG_STR, -1, NULL, "Font Name" },
{ -1, 0, NULL, NULL }
} ;

reply_entry yy_load_font_reply[] = {
{ ARG_INT, "Font number"},
{ ARG_FONTDATA, "Font data"},
{ -1, NULL}};

 cmd_arg yy_draw_text_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 1, NULL, "Font ID" },
{ ARG_STR, 0, "TEST TEST", "Text" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_draw_rtext_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "X Position" },
{ ARG_INT, -1, NULL, "Y Position" },
{ ARG_INT, 15, NULL, "Operation" },
{ ARG_INT, 0, NULL, "Color" },
{ ARG_INT, 1, NULL, "Font ID" },
{ ARG_INT, 1, NULL, "X Scale" },
{ ARG_INT, 1, NULL, "Y Scale" },
{ ARG_INT, 0, NULL, "Angle" },
{ ARG_STR, 0, "TEST TEST", "Text" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_select_territory_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_set_event_mask_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, 511, NULL, "Mask" },
{ -1, 0, NULL, NULL }
} ;

 cmd_arg yy_mask_event_arg[] = {
{ ARG_INT, 0 , NULL,"Flag"},
{ -1, 0, NULL, NULL }
} ;

 cmd_arg yy_clear_territory_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, 0, NULL, "Color" },
{ -1, 0, NULL, NULL }
} ;

 cmd_arg yy_draw_background_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "Pattern" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_query_color_arg[] = {
{ ARG_INT, -1, NULL, "Red" },
{ ARG_INT, -1, NULL, "Green" },
{ ARG_INT, -1, NULL, "Blue" },
{ -1, 0, NULL, NULL }
} ;

reply_entry yy_query_color_reply[] = {
{ ARG_INT, "colour number"},
{ -1, NULL}};

cmd_arg yy_load_image_arg[] = {
{ ARG_INT, -1, NULL, "Must be 1 (# pics)"},
{ ARG_INT, -1, NULL, "territory"},
{ ARG_STR, -1, NULL, "File Name"},
{ -1, 0, NULL, NULL}
};

cmd_arg yy_operate_bitblt_arg[] = {
{ ARG_INT, -1, NULL, "Source Territory number "},
{ ARG_INT, -1, NULL, "left-top position X of source"},
{ ARG_INT, -1, NULL, "Y"},
{ ARG_INT, -1, NULL, "Destination Territory number"},
{ ARG_INT, -1, NULL,  "the left-top position X of a area to be filled"},
{ ARG_INT, -1, NULL,  "Y    "},
{ ARG_INT, -1, NULL,  "Width of the destination area "},
{ ARG_INT, -1, NULL,  "Height of the destination area"},
{ ARG_INT, -1, NULL,  "bitblt operation"},
{ -1 , 0, NULL, NULL}
};


cmd_arg yy_create_cursor_territory_arg[] = {
{ ARG_INT, -1, NULL, "Width" },
{ ARG_INT, -1, NULL, "Height" },
{ ARG_INT, -1, NULL, "Parent" },
{ ARG_INT, -1, NULL, "HotSpot (X)" },
{ ARG_INT, -1, NULL, "HotSpot (Y)" },
{ ARG_INT, -1, NULL, "BitMap" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_move_cursor_hotspot_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ ARG_INT, -1, NULL, "HotSpot (X)" },
{ ARG_INT, -1, NULL, "HotSpot (Y)" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_display_cursor_arg[] = {
{ ARG_INT, -1, NULL, "No" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_debug_reset_timer_arg[] = {
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_debug_set_timer_arg[] = {
{ ARG_INT, -1, NULL, "CMD No" },
{ ARG_INT, -1, NULL, "ON(1), OFF(0)" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_debug_show_table_arg[] = {
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_debug_list_color_arg[] = {
{ ARG_STR, 0, "black", "Color Name" },
{ -1, 0, NULL, NULL }
} ;
 cmd_arg yy_debug_list_territory_arg[] = {
{ -1, 0, NULL, NULL }
} ;

cmd_arg yy_mouse_event[] = {{ -1, 0, NULL, NULL}};

reply_entry yy_mouse_event_reply[]= {
{ARG_INT,"Territory"},
{ARG_INT,"input status"},
{ARG_INT,"X"},
{ARG_INT,"Y"},
{-1,NULL}};


command_entry ClientCmdTable[256] ;


/* Throwing data to and from the socket */
int send_packet(int sock, yy_packet *pkt)
{
  yy_packet_block *blk;

  debug_setfunc("control", "send_packet");

  errno=0;

  for (blk = &pkt->pktFirstBlock;
       blk != (yy_packet_block *)NULL; blk = blk->pbNextBlock) 
    {
      debug_print(1, "send packet (length=%d)\n", blk->pbLength);
      if (send(sock, blk->pbBuf, blk->pbLength, 0) <= 0)
	{
	  perror("send");
	  fprintf(stderr,"Sock:%d\n",sock);
	  debug_endfunc("send_packet");
	  return FALSE;
	}
    }
  debug_endfunc("send_packet");
  return TRUE;
}

recv_packet(int sock,yy_packet_system_queue *p_que)
{
  debug_setfunc("control", "recv_packet");
  bzero((char *)p_que, sizeof(*p_que));
  
  while (p_que->sysqRead==NULL)
      recv_one_block(sock, p_que);
  
  debug_endfunc("control", "recv_packet");
}

int recv_one_block(int sock, yy_packet_system_queue *que)
{
#ifdef WITH_SYSTEMV_SOCKETS  
  struct pollfd rfds[1];
  int timeout;
  int ret;
#endif  
#ifdef WITH_BSD_SOCKETS
	fd_set rfds;
	struct timeval timeout;
	int ret;
#endif
  debug_setfunc("network", "recv_one_block");
 retry:
#ifdef WITH_SYSTEMV_SOCKETS
  rfds[0].fd=sock;
  rfds[0].events= POLLIN;
  rfds[0].revents= 0;

  timeout =  CLIENTTIMEOUT_INUSEC ;
  if ((ret = poll(rfds, 1, timeout)) < 0) 
#endif
#ifdef WITH_BSD_SOCKETS
  FD_ZERO(&rfds);
  FD_SET(sock, &rfds);
  timeout.tv_sec = 0;
  timeout.tv_usec = CLIENTTIMEOUT_INUSEC;
  if ((ret = select(sock+1, &rfds, NULL, NULL, &timeout)) < 0) 
#endif
    {
      perror("select");
      return FALSE;
    }
  if (ret > 0)
    {
      int leng, pleng, body;
      byte *bp = (byte *)xalloc(packet_size+10);
      /* Recv Header Info */
      if ((leng = recv(sock, bp, YYHEADERSIZE, 0)) != YYHEADERSIZE)
	{
	  if (leng < 0 && errno == EWOULDBLOCK) 
	    {
	      debug_print(9, "recv() call would block\n");
	      return 0;
	    }
	  if (leng == 0)
	    return FALSE;

	  return FALSE;
	}	
      debug_print(5, "recv Header Info\n");
      
      /* Get Packet Length */
      bcopy((char *)bp+1, (char *)&pleng+THREE_BYTE_OFFSET, 3);

      pleng = ((pleng & 07777) << 2);
      body = pleng - YYHEADERSIZE;
      debug_print(5, "Header said length is %d\n", pleng);
      if ((leng = recv(sock, bp+YYHEADERSIZE, body, 0)) != body) 
	{
	  if (leng < 0 && errno == EWOULDBLOCK) 
	    {
	      debug_print(9, "recv() call would block\n");
	      return 0;
	    }
	  return FALSE;
	}
      /* If we have fixed packet, it will be moved to read queue */
      debug_print(9, " recv one block (LENGTH:%d)\n", pleng);
      put_block_on_recvq(que, bp, pleng);
      debug_endfunc("recv_yy_packet");
      return TRUE;
    }
  debug_endfunc("recv_one_block");
  return 0;			/* time out */
}

#define YY_ENTRIES (3)
MODULE Module_YY_low;
LispObject Module_YY_values[YY_ENTRIES];

void INIT_YY_low(LispObject *stacktop)
{
  SYSTEM_INITIALISE_GLOBAL(int,fd1,0);
  SYSTEM_INITIALISE_GLOBAL(int,fd2,0);

  /* Enter the initialisation */
  init_entry( & ClientCmdTable[YYCOMMAND_INIT],
	     "init", yy_init_arg, "initialisation",yy_init_reply);
  init_entry( & ClientCmdTable[YYCOMMAND_CREATE_TERRITORY],
	     "create", yy_create_arg, "Command Name" ,yy_create_reply);
  init_entry( & ClientCmdTable[YYCOMMAND_DISPLAY_TERRITORY],
	     "display", yy_display_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_MOVE_TERRITORY],
	     "move", yy_move_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_RESIZE_TERRITORY],
	     "resize", yy_resize_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DESTROY_TERRITORY],
	     "destroy", yy_destroy_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_REPARENT_TERRITORY],
	     "reparent", yy_reparent_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_RAISE_TERRITORY],
	     "raise", yy_raise_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_LOWER_TERRITORY],
	     "lower", yy_lower_arg, "Command Name" ,NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_LOAD_FONT],
	     "load-font", yy_load_font_arg, "Load Font",yy_load_font_reply);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_POINT],
	     "draw-point", yy_draw_point_arg, "Draw Point",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_LINE],
	     "draw-line", yy_draw_line_arg, "Draw Line",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_RECTANGLE],
	     "draw-rectangle", yy_draw_rectangle_arg, "Draw Rectangle",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_FILL_RECTANGLE],
	     "draw-fill-rectangle", yy_draw_fill_rectangle_arg,"Draw rectangle",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_LINES],
	     "draw-lines", yy_draw_lines_arg, "Draw Lines",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_POLYGON],
	     "draw-polygon", yy_draw_polygon_arg, "Draw Polygon",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_CIRCLE],
	     "draw-circle", yy_draw_circle_arg, "Draw Circle",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_FILL_CIRCLE],
	     "fill-circle", yy_fill_circle_arg, "Draw Circle",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_ARC],
	     "draw-arc", yy_draw_arc_arg, "Draw Arc",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_TEXT],
	     "draw-text", yy_draw_text_arg, "Draw Text",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_VTEXT],
	     "draw-vtext", yy_draw_text_arg, "Draw Text",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_RTEXT],
	     "draw-rtext", yy_draw_rtext_arg, "Draw Text",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_SELECT_TERRITORY],
	     "select-territory", yy_select_territory_arg, "Select Territory",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_SET_EVENT_MASK],
	     "set-event-mask", yy_set_event_mask_arg, "Set Event Mask",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_MASK_EVENT],
	     "Set event flag",yy_mask_event_arg,"",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_CLEAR_TERRITORY],
	     "clear-territory", yy_clear_territory_arg, "Clear Territory",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DRAW_BACKGROUND],
	     "draw-background", yy_draw_background_arg, "Draw Background",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_QUERY_COLOR],
	     "query-color", yy_query_color_arg, "Query Color",yy_query_color_reply);
  init_entry( &ClientCmdTable[YYCOMMAND_LOAD_PICTURE],
	     "Get-Image", yy_load_image_arg, "Load Image", NULL);
  init_entry( &ClientCmdTable[YYCOMMAND_OPERATE_BITBLT],
	     "OPERATE BITBLT", yy_operate_bitblt_arg, "Bitblt",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_CREATE_CURSOR_TERRITORY],
	     "create-cursor", yy_create_cursor_territory_arg, "Create Cursor Territory",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_MOVE_CURSOR_HOTSPOT],
	     "move-cursor-hotspot", yy_move_cursor_hotspot_arg, "Move Cursor Hotspot",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_DISPLAY_CURSOR],
	     "display-cursor", yy_display_cursor_arg, "Display Cursor",NULL);
  init_entry( & ClientCmdTable[YYCOMMAND_MOUSE_EVENT],
	     "Mouse Event",yy_mouse_event,"",yy_mouse_event_reply);

  open_module(stacktop,&Module_YY_low,Module_YY_values,"YY-low",YY_ENTRIES);
  
  (void) make_module_function(stacktop,"YY-function",Fn_YY_Function,-2);
  (void) make_module_function(stacktop,"YY-connect",Fn_setup_server,2);
  (void) make_module_function(stacktop,"YY-next-event",Fn_get_next_event,1);
  close_module();
}

init_entry(command_entry *ce,char *name,cmd_arg *ca,char *comment, reply_entry *re)
{
  ce->cmdLabel=name;
  ce->cmdArgs=ca;
  ce->cmdComment=comment;
  ce->ceReplyData=re;
}
