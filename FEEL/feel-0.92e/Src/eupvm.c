/*  
 *
 *  PVM/Feel interface
 *	      uses reader module...
 *
 */

/*
 * $Id: eupvm.c,v 1.4 1994/04/11 15:54:26 djb Exp $
 *
 * $Log: eupvm.c,v $
 * Revision 1.4  1994/04/11  15:54:26  djb
 * various ANSI stuff, moved static function declarations out of functions,
 * changed unsigned chars to chars where appropriate (pvm library uses
 * chars, not unsigned -- picky compilers complain when linking)
 *
 * Revision 1.3  1994/04/08  16:03:42  djb
 * updated list of PVM functions supported
 *
 * Revision 1.2  1994/04/07  09:41:27  djb
 * removed redundant eupvm_p.h include
 *
 * Revision 1.1  1994/04/06  18:10:32  djb
 * Initial revision
 *
 *
 */

/* PVM functions:
 *   pvm_enroll(name)
 *   pvm_initiate_by_type(hosttype, name)
 *   pvm_initiate_by_hostname(hostname, name)
 *   pvm_terminate()
 *   pvm_leave()
 *   pvm_snd(id type message)
 *   pvm_probe(type) -> bool
 *   pvm_probe_multi(type-list) -> bool
 *   pvm_recv(type info-p [reader]) -> [object, info]
 *   pvm_recv_multi(type-list info-p [reader]) -> [object, info]
 *   pvm_status(pvm_id) -> bool
 *   pvm_whoami() -> pvm_id
#ifdef PVM_WHO 
 *   pvm_who() -> [pvm_id,...]
#endif 
 *   pvm_barrier(name number)
 *   pvm_ready(name)
 *   pvm_wait_until(name)
 */

#ifdef PVM
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

#ifdef XAB
#include "xab.h"
#endif

/* Max message size */
#define PVM_MSGBUF 16384

/* class, returned by enroll, used by snd */

#define PVM_NAME(id) (stringof(CAR(id)))
#define PVM_NUMBER(id) (intval(CDR(id)))

#ifdef PVM_ID_CLASS
LispObject Pvm_Id;
#endif

static LispObject make_pvm_id(LispObject *stacktop,LispObject name,int n)
{
  LispObject new_id,xx;
  
  STACK_TMP(name);
  xx=allocate_integer(stacktop,n);
  UNSTACK_TMP(name);
  new_id = EUCALL_2(Fn_cons,name,xx);
#ifdef PVM_ID_CLASS
  lval_classof(new_id) = Pvm_Id;
#endif
  
  return new_id;
}

static EUFUN_1(Fn_make_pvm_id_from_pair, pair)
{
  LispObject new_ob;

  if (!is_cons(pair))
    CallError(stacktop,"make-id: Type error",pair,NONCONTINUABLE);

  new_ob = EUCALL_2(Fn_cons,CAR(pair),CDR(pair));
#ifdef PVM_ID_CLASS
  lval_classof(new_ob) = Pvm_Id;
#endif
  
  return new_ob;
}
EUFUN_CLOSE

static EUFUN_1( Fn_make_pvm_id, name)
{
  return make_pvm_id(stacktop,name,-1);
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_enroll, name)
{
  int ret;
  
  if (!is_string(name))
    CallError(stacktop,"enroll: expected a string",name,NONCONTINUABLE);

  if ((ret = enroll(stringof(name))) < 0)
    CallError(stacktop,"enroll: call failed",name,NONCONTINUABLE);

  return make_pvm_id(stacktop,name,ret);
}
EUFUN_CLOSE

/* Name is an executable in ~/pvm/<ARCH> */
/* type is a machine type, () if any will do.. */
static EUFUN_2( Fn_pvm_initiate_by_type, type, name)
{
  int ret;

  if(!is_string(type) || !is_string(name))
    CallError(stacktop,"initiate: type error",name,NONCONTINUABLE);

  if ((ret = initiate(stringof(name),stringof(type))) < 0)
    CallError(stacktop,"initiate: call failed",nil,NONCONTINUABLE);
  
  return make_pvm_id(stacktop,name,ret);

}
EUFUN_CLOSE

static EUFUN_2( Fn_pvm_initiate_by_host_name, hostname, name)
{
  int ret;

  if(!is_string(hostname) || !is_string(name))
    CallError(stacktop,"initiate: type error",hostname,NONCONTINUABLE);

  if ((ret = initiateM(stringof(name),stringof(hostname))) < 0)
    CallError(stacktop,"initiate: call failed",nil,NONCONTINUABLE);
  
  return make_pvm_id(stacktop,name,ret);
  
}
EUFUN_CLOSE

/* Note that this closes stdio buffers */
static EUFUN_0( Fn_pvm_leave)
{
  leave();
  
  return nil;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_terminate, pvm_id)
{
  int ret;

#ifdef PVM_ID_CLASS
  if (EUCALL_2(Fn_subclassp,classof(pvm_id),Pvm_Id)==nil)
    CallError(stacktop,"terminate: type error",nil,NONCONTINUABLE);
#endif
  
  if ((ret = terminate(PVM_NAME(pvm_id),PVM_NUMBER(pvm_id))) < 0)
    CallError(stacktop,"terminate: call failed",pvm_id,NONCONTINUABLE);

  return nil;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_status, pvm_id)
{
  int ret;

#ifdef PVM_ID_CLASS
  if (EUCALL_2(Fn_subclassp,classof(pvm_id),Pvm_Id)==nil)
    CallError(stacktop,"status: type error",nil,NONCONTINUABLE);
#endif
  
  if ((ret = status(PVM_NAME(pvm_id),PVM_NUMBER(pvm_id))) < 0)
    CallError(stacktop,"status: call failed",pvm_id,NONCONTINUABLE);
  
  if (ret)
    return lisptrue;
  else
    return nil;  
}
EUFUN_CLOSE

/* Message is any sendable object */

static EUFUN_4( Fn_pvm_snd, id, msg_type, msg, reader_maybe)
{
  LispObject xx;
  char *buf=NULL;
  unsigned char *ptr;
  int len;

  buf = (char *)malloc(PVM_MSGBUF);
  
  ptr = (unsigned char *)buf;
  write_obj(stacktop,msg,&ptr,reader_maybe);
  len = ptr - (unsigned char *)buf;
  EUBUG(fprintf(stderr,"Send: %d bytes sent\n",len));
  msg_type=ARG_1(stackbase);
  if (!is_fixnum(msg_type))
    CallError(stacktop,"send: Type error",msg_type,NONCONTINUABLE);

  id=ARG_0(stackbase);
  initsend();  
  putnint(&len,1);
  putbytes(buf,len);
  if (snd(PVM_NAME(id),PVM_NUMBER(id),
	  intval(msg_type))<0)
    CallError(stacktop,"send: call failed",id,NONCONTINUABLE);
  free(buf);

  return nil;
}
EUFUN_CLOSE

static LispObject read_msg(LispObject *, LispObject , LispObject );  

static EUFUN_3( Fn_pvm_blocking_rcv, msg_type, info_p, reader_maybe)
{
  if (!is_fixnum(msg_type))
    CallError(stacktop,"rcv: type error",msg_type,NONCONTINUABLE);

  if (rcv(intval(msg_type)) < 0)
    CallError(stacktop,"rcv: call failed",nil,NONCONTINUABLE);
  
  return (read_msg(stacktop,info_p, reader_maybe));
}
EUFUN_CLOSE

static EUFUN_3( Fn_pvm_rcv, msg_type, info_p, reader_maybe)
{
  if (!is_fixnum(msg_type))
    CallError(stacktop,"rcv: type error",msg_type,NONCONTINUABLE);

  while(probe(intval(ARG_0(stackbase))) < 0)
    EUCALL_0(Fn_thread_reschedule);

  if (rcv(intval(ARG_0(stackbase))) < 0)
    CallError(stacktop,"rcv: call failed",nil,NONCONTINUABLE);
  return (read_msg(stacktop,ARG_1(stackbase), ARG_2(stackbase)));
}
EUFUN_CLOSE

EUFUN_3( Fn_pvm_rcvmulti, typelist, info_p,  reader_maybe)
{
  LispObject ptr;
  int len;

  len = 0;
  ptr = typelist;

  while(is_cons(ptr))
    {	
      len++;
      ptr = CDR(ptr);
    }

  {	
#ifdef __GCC__
    int buf[len];
#else
    int buf[256];
#endif
    int i=0;

    ptr=typelist;
    while(is_cons(ptr))
      {
	buf[i]=intval(CAR(ptr));
	i++;
	ptr=CDR(ptr);
      }
    
    if (rcvmulti(len,buf)<0)
      CallError(stacktop,"rcvmulti: Call failed",nil,NONCONTINUABLE);
  }
  return(read_msg(stacktop,info_p, reader_maybe));
}
EUFUN_CLOSE

static LispObject read_msg(LispObject *stacktop,LispObject info_p,LispObject reader_maybe)
{
  char *buf=NULL;
  char nam_buf[128];
  unsigned char *ptr;
  LispObject new_obj;

  LispObject sender,result;
  int len,inum,type;

  if (getnint(&len,1) < 0)
    CallError(stacktop,"rcv: getnint call failed",nil,NONCONTINUABLE);
  
  EUBUG(fprintf(stderr,"Rcv: Got %d bytes\n",len));
  buf =  (char *)malloc(PVM_MSGBUF);

  ptr = (unsigned char *)buf;
  if (getbytes(buf,len) < 0)
    CallError(stacktop,"rcv: getbytes call failed",nil,NONCONTINUABLE);

  STACK_TMP(info_p);
  new_obj = read_obj(stacktop,&ptr,reader_maybe);
  UNSTACK_TMP(info_p);
  free(buf);
  EUBUG(fprintf(stderr,"Recv: used %d bytes\n",ptr-(unsigned char *)buf));
  if (info_p!=nil)
    {
      LispObject xx;
      STACK_TMP(new_obj);
      rcvinfo(&len,&type,&nam_buf[0],&inum);	
      xx=allocate_integer(stacktop,type);
      xx=EUCALL_2(Fn_cons,xx,nil);
      STACK_TMP(xx);
      xx=allocate_string(stacktop,nam_buf,strlen(nam_buf));
      sender = make_pvm_id(stacktop,xx,inum);
      UNSTACK_TMP(xx);
      xx=EUCALL_2(Fn_cons,sender,xx);
      UNSTACK_TMP(new_obj);
      result=EUCALL_2(Fn_cons,new_obj,xx);
      return result;
    }
  else
    {
      return new_obj;
    }
}


/* Readable-p */
static EUFUN_1( Fn_pvm_probe, type)
{
  int ret;

  if(!is_fixnum(type))
    CallError(stacktop,"probe: type error",type,NONCONTINUABLE);

  if((ret = probe(intval(type))) < 0)
    return nil;
  else 
    return allocate_integer(stacktop,ret);
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_probe_multi, typelist)
{
  LispObject ptr;
  int len,ret;

  len = 0;
  ptr = typelist;

  while(is_cons(ptr))
    {	
      len++;
      ptr = CDR(ptr);
    }

  {	
#ifdef __GCC__
    int buf[len];
#else
    int buf[256];
#endif
    int i=0;

    ptr=typelist;
    while(is_cons(ptr))
      {
	buf[i]=intval(CAR(ptr));
	i++;
	ptr=CDR(ptr);
      }
    ret=0;
    /*probemulti(len,buf); --- not yet written*/
  }
  return nil;
}
EUFUN_CLOSE

static EUFUN_2( Fn_pvm_barrier, name, number)
{
  if (!is_string(name))
    CallError(stacktop,"barrier: type error",name,NONCONTINUABLE);
  
  if (!is_fixnum(number))
    CallError(stacktop,"barrier: type error",number,NONCONTINUABLE);

  if (barrier(stringof(name),intval(number))<0)
    CallError(stacktop,"barrier: call error",number,NONCONTINUABLE);

  return nil;

}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_ready, name) /* simple semaphore */
{
  if (!is_string(name))
    CallError(stacktop," reader: type error",name,NONCONTINUABLE);

  if (ready(stringof(name))<0)
    CallError(stacktop," reader: call error",name,NONCONTINUABLE);

  return nil;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_waituntil, name)
{
  if (!is_string(name))
    CallError(stacktop," waituntil: type error",name,NONCONTINUABLE);

  if (waituntil(stringof(name))<0)
    CallError(stacktop,"waituntil: call error",name,NONCONTINUABLE);

  return nil;
}
EUFUN_CLOSE

static EUFUN_0( Fn_pvm_whoami)
{
  int ret;
  char buf[128];
  LispObject xx;

  if(whoami(buf,&ret)<0)
    CallError(stacktop,"whoami: call error",nil,NONCONTINUABLE);

  xx=allocate_string(stacktop,buf,(int) strlen(buf));
  return make_pvm_id(stacktop,xx,ret);
}
EUFUN_CLOSE

#ifdef PVM_WHO /* 'who' isn't defined as part of standard pvm */
static EUFUN_0( Fn_pvm_who)
{
  int n,ret[128];
  char *buf[128];
  LispObject new_id,xx;

  if((n=who(buf,ret))<0)
    CallError(stacktop,"who: call error",nil,NONCONTINUABLE);

  new_id=nil;
  for(;n>0;n--)
  {
    xx=allocate_string(stacktop,buf[n-1],(int) strlen(buf[n-1]));
    new_id= EUCALL_2(Fn_cons,make_pvm_id(stacktop,xx,ret[n-1]),new_id);
  }

  return new_id;
}
EUFUN_CLOSE
#endif

#ifdef PVM_ID_CLASS
#ifdef PVM_WHO
#define PVM_MODULE_ENTRIES (19)
#else
#define PVM_MODULE_ENTRIES (18)
#endif
#else
#ifdef PVM_WHO
#define PVM_MODULE_ENTRIES (16)
#else
#define PVM_MODULE_ENTRIES (15)
#endif
#endif

MODULE Module_pvm;
LispObject Module_pvm_values[PVM_MODULE_ENTRIES];

void initialise_pvm(LispObject *stacktop)
{
#ifdef PVM_ID_CLASS
  extern LispObject Standard_Class,Object, Primitive_Class;
/*  
  This was the original line: 

    make_class(stacktop,Pvm_Id,"pvm-id",Primitive_Class,Object,0);

  And this is what I replaced it with: 
*/
  Pvm_Id = allocate_class(stacktop,Object);
  set_class_size(stacktop,Pvm_Id,Primitive_Class,0);
  add_root(&Pvm_Id);
/*  
  ...which screws the world.
*/
#endif

  open_module(stacktop,&Module_pvm,Module_pvm_values,"pvm",
	      PVM_MODULE_ENTRIES);

#ifdef PVM_ID_CLASS 
  (void) make_module_entry(stacktop,"pvm-id",Pvm_Id);
  (void) make_module_function(stacktop,"make-pvm-id",Fn_make_pvm_id,1);
  (void) make_module_function(stacktop,"pvm-make-id-from-pair",
			      Fn_make_pvm_id_from_pair,1);
#endif
  (void) make_module_function(stacktop,"pvm-status",Fn_pvm_status,1);
  (void) make_module_function(stacktop,"pvm-leave",Fn_pvm_leave,0);
  (void) make_module_function(stacktop,"pvm-send",Fn_pvm_snd,-4);
  (void) make_module_function(stacktop,"pvm-recv",Fn_pvm_rcv,-3);
  (void) make_module_function(stacktop,"pvm-recv-multi",Fn_pvm_rcvmulti,-3);
  (void) make_module_function(stacktop,"pvm-initiate-by-type",Fn_pvm_initiate_by_type,2);
  (void) make_module_function(stacktop,"pvm-initiate-by-hostname",Fn_pvm_initiate_by_host_name,2);
  (void) make_module_function(stacktop,"pvm-enroll",Fn_pvm_enroll,1);
  (void) make_module_function(stacktop,"pvm-probe",Fn_pvm_probe,1);
  (void) make_module_function(stacktop,"pvm-probe-multi",Fn_pvm_probe_multi,1);
  (void) make_module_function(stacktop,"pvm-barrier",Fn_pvm_barrier,2);
  (void) make_module_function(stacktop,"pvm-ready",Fn_pvm_ready,1);
  (void) make_module_function(stacktop,"pvm-waituntil",Fn_pvm_waituntil,2);
  (void) make_module_function(stacktop,"pvm-terminate",Fn_pvm_terminate,2);
  (void) make_module_function(stacktop,"pvm-whoami",Fn_pvm_whoami,0);
#ifdef PVM_WHO
  (void) make_module_function(stacktop,"pvm-who",Fn_pvm_who,0);
#endif  
  close_module();

}		      
#endif /* PVM */
