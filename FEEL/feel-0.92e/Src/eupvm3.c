/*  
 *
 *  PVM3/Feel interface
 *	      uses reader module...
 */

/*
 * $Id: eupvm3.c,v 1.7 1995/02/20 10:52:07 djb Exp $
 *
 * $Log: eupvm3.c,v $
 * Revision 1.7  1995/02/20  10:52:07  djb
 * minor fix
 *
 * Revision 1.6  1995/01/09  10:40:31  djb
 * replaced pvm_spawn_on_host/arch with single pvm_spawn function
 * loads of #ifdefs for CS2
 *
 * Revision 1.5  1995/01/03  14:34:06  djb
 * changes for pvm3.3.4 + multicast fix
 *
 * Revision 1.4  1994/04/11  15:55:24  djb
 * various ANSI stuff, moved static function declarations out of functions,
 * changed unsigned chars to chars where appropriate (pvm library uses
 * chars, not unsigned -- picky compilers complain when linking)
 *
 * Revision 1.3  1994/04/08  16:04:07  djb
 * updated list of PVM3 functions supported
 *
 * Revision 1.2  1994/04/07  09:41:49  djb
 * made initialise_pvm -> initialise_pvm3 so that
 * I can link both modules in one executable
 *
 * Revision 1.1  1994/04/06  18:45:29  djb
 * Initial revision
 *
 *
 */

/* PVM3 functions:
 *   pvm_mytid() -> tid
 *   pvm_exit() -> nil

 *   pvm_config() -> ((host.arch), ...)
 *   pvm_spawn(name, host, arch, n, ...) -> (tid, ...)
 *   pvm_tasks() -> (tid, ...)
 *   pvm_pstat(tid) -> bool
 *   pvm_kill(tid) -> nil

 *   pvm_send(tid type message [reader]) -> nil
 *   pvm_mcast((tid,...) type message [reader]) -> nil
 *   pvm_receive(tid type info-p [reader]) -> [object|(object, info)]
 *        or alternatively
 *   pvm_recv(tid type) -> (tid, type)
 *        and then
 *   pvm_read([reader]) -> object

 *   pvm_probe(tid type) -> bool
 *   pvm_joingroup(group) -> nil
 *   pvm_lvgroup(group) -> nil
 *   pvm_gtasks(group) -> (tid, ...)
 *   pvm_barrier(group count) -> nil
 *   pvm_bcast(group type message) -> nil
 */

#ifdef PVM3
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
#include "pvm3.h"

#ifdef XAB
#include "xab.h"
#endif

#ifdef CS2
#define pvmhostinfo hostinfo
#define pvmtaskinfo taskinfo
#endif

/* Max message size */

#define PVM_MSGBUF 16384

/* Max tasks, hosts */

#define PVM_MAXTASKS 128
#define PVM_MAXHOSTS 128

/* first call to pvm_mytask enrolls the task */

static EUFUN_0( Fn_pvm_mytid)
{
  int ret;
  
  if ((ret = pvm_mytid()) < 0)
    CallError(stacktop,"pvm_mytid: call error",allocate_integer(stacktop,ret),NONCONTINUABLE);

  return allocate_integer(stacktop, ret);
}
EUFUN_CLOSE

/* Note that this closes stdio buffers */
static EUFUN_0( Fn_pvm_exit)
{
  pvm_exit();
  
  return nil;
}
EUFUN_CLOSE

static EUFUN_0( Fn_pvm_config)
{
  int nhosts, narches, ret;
  struct pvmhostinfo *hostp;
  LispObject hosts,xx;
  char *host, *arch;

  if((ret=pvm_config(&nhosts,&narches,&hostp))<0) 
    CallError(stacktop,"pvm_config: call error",allocate_integer(stacktop,ret),NONCONTINUABLE);

  hosts=nil;

  for(;nhosts>0;nhosts--)
  {
    host=hostp[nhosts-1].hi_name;
    arch=hostp[nhosts-1].hi_arch;
    STACK_TMP(hosts);
    xx= EUCALL_2(Fn_cons,allocate_string(stacktop,host,strlen(host)),allocate_string(stacktop,arch,strlen(arch)));
    UNSTACK_TMP(hosts);
    hosts=EUCALL_2(Fn_cons,xx,hosts);
  }

  return hosts;
}
EUFUN_CLOSE

/* name is an executable in ~/pvm/<ARCH> */
/* returns a list of tids */
/* host is a host name */
#define MAX_ARGS 10
static EUFUN_4( Fn_pvm_spawn, name_and_args, host, arch, ntasks)
{
  int ret, n, tid[PVM_MAXTASKS];
  int flag;
  LispObject tids;
  char *args[MAX_ARGS];
  char *name, *where;
  int arg_num;
  
  if(!is_string(name_and_args))
    CallError(stacktop,"pvm_spawn: type error",name_and_args,NONCONTINUABLE);

  name=(char *)strtok(stringof(name_and_args)," ");
  arg_num=0;
  do
  {
    args[arg_num]=(char *)strtok(NULL," ");
  }
  while(args[arg_num++]!=NULL);

  if(!null(host)&&!null(arch))
    CallError(stacktop,"pvm_spawn: arg error",arch,NONCONTINUABLE);

  if(null(host)&&null(arch))
  {
#ifdef DEBUG
    flag=PvmTaskDebug;
#else
    flag=PvmTaskDefault;
#endif
    where=NULL;
  }

  if(!null(host))
  {
    if(!is_string(host))
      CallError(stacktop,"pvm_spawn: type error",host,NONCONTINUABLE);
    flag=PvmTaskHost;
    where=stringof(host);
  }

  if(!null(arch))
  {
    if(!is_string(arch))
      CallError(stacktop,"pvm_spawn: type error",arch,NONCONTINUABLE);
    flag=PvmTaskArch;
    where=stringof(arch);
  }

  if(ntasks==nil)
  {
    n=1;
  }
  else 
  {
    ntasks=CAR(ntasks);
    if(!is_fixnum(ntasks))
      CallError(stacktop,"pvm_spawn: type error",ntasks,NONCONTINUABLE);
    else
      n=intval(ntasks);
  }

  if ((ret = pvm_spawn(name, args, flag, where, n, tid)) < n)
    CallError(stacktop,"pvm_spawn: call failed",allocate_integer(stacktop,ret),NONCONTINUABLE);
  
  tids=nil;

  for(;n>0;n--)
  {
    tids=EUCALL_2(Fn_cons,allocate_integer(stacktop,tid[n-1]),tids);
  }

  return tids;
}
EUFUN_CLOSE

/* returns a list of tids */
static EUFUN_0( Fn_pvm_tasks)
{
  int n, ret;
  struct pvmtaskinfo *taskp;
  LispObject tids;

  if((ret=pvm_tasks(0,&n,&taskp))<0) 
    CallError(stacktop,"pvm_tasks: call error",allocate_integer(stacktop,ret),NONCONTINUABLE);

  tids=nil;

  for(;n>0;n--)
  {
    tids=EUCALL_2(Fn_cons,allocate_integer(stacktop,taskp[n-1].ti_tid),tids);
  }

  return tids;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_pstat, tid)
{
  int ret;

  if (!is_fixnum(tid))
    CallError(stacktop,"pvm_status: type error",tid,NONCONTINUABLE);

  if ((ret = pvm_pstat(intval(tid))) == PvmBadParam) /* dodgy */
    CallError(stacktop,"pvm_pstat: call error",allocate_integer(stacktop, ret),NONCONTINUABLE);
  
  if (ret<0)
    return nil;  
  else
    return lisptrue;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_kill, tid)
{
  int ret;

  if ((ret = pvm_kill(intval(tid))) < 0)
    CallError(stacktop,"pvm_kill: call error",allocate_integer(stacktop,ret),NONCONTINUABLE);

  return nil;
}
EUFUN_CLOSE

/* Message is any sendable object */

static EUFUN_4( Fn_pvm_send, tid, msg_type, msg, reader_maybe)
{
  LispObject xx;
  char *buf=NULL;
  unsigned char *ptr;
  int len, ret;

  if (!is_fixnum(msg_type))
    CallError(stacktop,"pvm_send: type error",msg_type,NONCONTINUABLE);

  buf = (char *)malloc(PVM_MSGBUF);
  ptr = (unsigned char *)buf;
  write_obj(stacktop,msg,&ptr,reader_maybe);
  len = ptr - (unsigned char *)buf;
  EUBUG(fprintf(stderr,"pvm_send: %d bytes sent\n",len));

  pvm_initsend(PvmDataDefault);  
  pvm_pkint(&len,1,1);
  pvm_pkbyte(buf,len,1);

  msg_type=ARG_1(stackbase);
  tid=ARG_0(stackbase);
  if ((ret=pvm_send(intval(tid), intval(msg_type)))<0)
    CallError(stacktop,"pvm_send: call failed",allocate_integer(stacktop, ret),NONCONTINUABLE);

  free(buf);
  return nil;
}
EUFUN_CLOSE

static EUFUN_4( Fn_pvm_mcast, tids, msg_type, msg, reader_maybe)
{
  LispObject xx;
  char *buf=NULL;
  unsigned char *ptr;
  int len, ret;
  int task,tid[PVM_MAXTASKS];

  if (!is_fixnum(msg_type))
    CallError(stacktop,"pvm_multicast: type error",msg_type,NONCONTINUABLE);

  buf = (char *)malloc(PVM_MSGBUF);
  ptr = (unsigned char *)buf;
  write_obj(stacktop,msg,&ptr,reader_maybe);
  len = ptr - (unsigned char *)buf;
  EUBUG(fprintf(stderr,"pvm_multicast: %d bytes sent\n",len));

  pvm_initsend(PvmDataDefault);  
  pvm_pkint(&len,1,1);
  pvm_pkbyte(buf,len,1);

  msg_type=ARG_1(stackbase);
  tids=ARG_0(stackbase);
  for(task=0;tids!=nil;task++)
  {
    tid[task]=intval(CAR(tids));
    tids=CDR(tids);
  }

  if ((ret=pvm_mcast(tid,task,intval(msg_type)))<0)
    CallError(stacktop,"pvm_mcast: call error",allocate_integer(stacktop,ret),NONCONTINUABLE);

  free(buf);
  return nil;
}
EUFUN_CLOSE

static LispObject read_msg(LispObject *, LispObject , LispObject , int );  

/* this is blocking, but uses pvm_nrecv and thread_reschedule */

static EUFUN_4( Fn_pvm_receive, tid, msg_type, info_p, reader_maybe)
{
  int bufid;
  
  if (!is_fixnum(tid))
    CallError(stacktop,"pvm_receive: type error",tid,NONCONTINUABLE);

  if (!is_fixnum(msg_type))
    CallError(stacktop,"pvm_receive: type error",msg_type,NONCONTINUABLE);

  while((bufid=pvm_nrecv(intval(tid), intval(msg_type))) == 0)
    EUCALL_0(Fn_thread_reschedule);

  if (bufid < 0)
    CallError(stacktop,"pvm_nrecv: call error",allocate_integer(stacktop,bufid),NONCONTINUABLE);

  return (read_msg(stacktop,ARG_2(stackbase), ARG_3(stackbase), bufid));
}
EUFUN_CLOSE

static LispObject read_msg(LispObject *stacktop,LispObject info_p,LispObject reader_maybe, int bufid)
{
  char *buf=NULL;
  unsigned char *ptr;
  LispObject new_obj;

  LispObject sender,result;
  int len,tid,type,ret;

  if ((ret=pvm_upkint(&len,1,1)) < 0)
    CallError(stacktop,"pvm_upkint call error",allocate_integer(stacktop, ret),NONCONTINUABLE);
  
  EUBUG(fprintf(stderr,"pvm_receive: Got %d bytes\n",len));
  buf =  (char *)malloc(PVM_MSGBUF);

  ptr = (unsigned char *)buf;
  if ((ret=pvm_upkbyte(buf,len,1)) < 0)
    CallError(stacktop,"pvm_upkbyte call failed",allocate_integer(stacktop, ret),NONCONTINUABLE);

  STACK_TMP(info_p);
  new_obj = read_obj(stacktop,&ptr,reader_maybe);
  UNSTACK_TMP(info_p);
  free(buf);
  EUBUG(fprintf(stderr,"pvm_receive: used %d bytes\n",ptr-(unsigned char *)buf));
  if (info_p!=nil)
  {
    LispObject xx;
    STACK_TMP(new_obj);
    pvm_bufinfo(bufid,&len,&type,&tid);	
    xx=allocate_integer(stacktop,type);
    xx=EUCALL_2(Fn_cons,xx,nil);
    STACK_TMP(xx);
    sender = allocate_integer(stacktop,tid);
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

static int bufid;

static EUFUN_2( Fn_pvm_recv, tid, msg_type)
{
  LispObject sender, info;
  int buflen, buftype, buftid;

  if (!is_fixnum(tid))
    CallError(stacktop,"pvm_recv: type error",tid,NONCONTINUABLE);

  if (!is_fixnum(msg_type))
    CallError(stacktop,"pvm_recv: type error",msg_type,NONCONTINUABLE);

  while((bufid=pvm_nrecv(intval(tid), intval(msg_type))) == 0)
    EUCALL_0(Fn_thread_reschedule);

  if (bufid < 0)
    CallError(stacktop,"pvm_nrecv: call error",allocate_integer(stacktop,bufid),NONCONTINUABLE);

  pvm_bufinfo(bufid,&buflen,&buftype,&buftid);	
  info=allocate_integer(stacktop,buftype);
  info=EUCALL_2(Fn_cons,info,nil);
  STACK_TMP(info);
  sender = allocate_integer(stacktop,buftid);
  UNSTACK_TMP(info);
  info=EUCALL_2(Fn_cons,sender,info);
  return info;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_read, reader_maybe)
{
  char *buf=NULL;
  unsigned char *ptr;
  LispObject new_obj;
  int len, ret;

  if ((ret=pvm_upkint(&len,1,1)) < 0)
    CallError(stacktop,"pvm_upkint call error",allocate_integer(stacktop, ret),NONCONTINUABLE);
  
  EUBUG(fprintf(stderr,"pvm_read: Got %d bytes\n",len));
  buf =  (char *)malloc(PVM_MSGBUF);

  ptr = (unsigned char *)buf;
  if ((ret=pvm_upkbyte(buf,len,1)) < 0)
    CallError(stacktop,"pvm_upkbyte call failed",allocate_integer(stacktop, ret),NONCONTINUABLE);

  new_obj = read_obj(stacktop,&ptr,reader_maybe);
  free(buf);
  EUBUG(fprintf(stderr,"pvm_read: used %d bytes\n",ptr-(unsigned char *)buf));

  return new_obj;
}
EUFUN_CLOSE

static EUFUN_2( Fn_pvm_probe, tid, type)
{
  int ret;

  if(!is_fixnum(tid))
    CallError(stacktop,"pvm_probe: type error",tid,NONCONTINUABLE);

  if(!is_fixnum(type))
    CallError(stacktop,"pvm_probe: type error",type,NONCONTINUABLE);

  if((ret = pvm_probe(intval(tid), intval(type)))<0)
    CallError(stacktop,"pvm_probe: call error",allocate_integer(stacktop,ret),NONCONTINUABLE);

  if (ret>0)
    return lisptrue;
  else 
    return nil;
}
EUFUN_CLOSE

/* groups */

static EUFUN_1( Fn_pvm_joingroup, group)
{
#ifndef CS2
  int ret;

  if (!is_string(group))
    CallError(stacktop,"pvm_joingroup: type error",group,NONCONTINUABLE);

  if ((ret = pvm_joingroup(stringof(group)))<0)
    CallError(stacktop,"pvm_joingroup: call error",allocate_integer(stacktop, ret),NONCONTINUABLE);
#endif
  return nil;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_lvgroup, group)
{
#ifndef CS2  
  int ret;

  if (!is_string(group))
    CallError(stacktop,"pvm_leavegroup: type error",group,NONCONTINUABLE);

  if ((ret = pvm_lvgroup(stringof(group)))<0)
    CallError(stacktop,"pvm_lvgroup: call error",allocate_integer(stacktop, ret),NONCONTINUABLE);
#endif
  return nil;
}
EUFUN_CLOSE

static EUFUN_1( Fn_pvm_gtasks, group)
{
  int ntasks,inum,tid;
  LispObject tids;

  if (!is_string(group))
    CallError(stacktop,"pvm_grouptasks: type error",group,NONCONTINUABLE);

#ifdef CS2
  return EUCALL_0(Fn_pvm_tasks);
#else
  if((ntasks=pvm_gsize(stringof(group)))<0)
  {
    if (ntasks!=PvmNoGroup)
      CallError(stacktop,"pvm_gsize: call error",allocate_integer(stacktop, ntasks),NONCONTINUABLE);
    else
      ntasks=0;
  }

  tids=nil;
  inum=0;

  while(ntasks>0) /* this really could go horribly wrong */
  {
    if ((tid=pvm_gettid(stringof(group),inum))>0)
    {
      STACK_TMP(group);
      tids=EUCALL_2(Fn_cons,allocate_integer(stacktop,tid),tids);
      UNSTACK_TMP(group);
      ntasks--;
    }
    inum++;
  }

  return tids;
#endif
}
EUFUN_CLOSE

static EUFUN_2( Fn_pvm_barrier, group, count)
{
  int ret;

  if (!is_string(group))
    CallError(stacktop,"pvm_barrier: type error",group,NONCONTINUABLE);
  
  if (!is_fixnum(count))
    CallError(stacktop,"pvm_barrier: type error",count,NONCONTINUABLE);


  if ((ret = pvm_barrier(stringof(group),intval(count)))<0)
    CallError(stacktop,"pvm_barrier: call error",allocate_integer(stacktop, ret),NONCONTINUABLE);

  return nil;
}
EUFUN_CLOSE

static EUFUN_4( Fn_pvm_bcast, group, msg_type, msg, reader_maybe)
{
#ifdef CS2
  LispObject tids;
  tids = EUCALL_0(Fn_pvm_tasks);
  return EUCALL_4(Fn_pvm_mcast, tids, ARG_1(stackbase), ARG_2(stackbase),
		  ARG_3(stackbase));
#else
  LispObject xx;
  char *buf=NULL;
  unsigned char *ptr;
  int len, ret;

  if (!is_fixnum(msg_type))
    CallError(stacktop,"pvm_broadcast: type error",msg_type,NONCONTINUABLE);

  if (!is_string(group))
    CallError(stacktop,"pvm_broadcast: type error",group,NONCONTINUABLE);

  buf = (char *)malloc(PVM_MSGBUF);
  ptr = (unsigned char *)buf;
  write_obj(stacktop,msg,&ptr,reader_maybe);
  len = ptr - (unsigned char *)buf;
  EUBUG(fprintf(stderr,"pvm_send: %d bytes sent\n",len));

  pvm_initsend(PvmDataDefault);  
  pvm_pkint(&len,1,1);
  pvm_pkbyte(buf,len,1);

  msg_type=ARG_1(stackbase);
  group=ARG_0(stackbase);

  if ((ret=pvm_bcast(stringof(group), intval(msg_type)))<0)
    CallError(stacktop,"pvm_bcast: call error",allocate_integer(stacktop, ret),NONCONTINUABLE);

  free(buf);
  return nil;
#endif
}
EUFUN_CLOSE

#define PVM_MODULE_ENTRIES (18)

MODULE Module_pvm3;
LispObject Module_pvm3_values[PVM_MODULE_ENTRIES];

void initialise_pvm3(LispObject *stacktop)
{
  extern LispObject Standard_Class,Object;

  open_module(stacktop,&Module_pvm3,Module_pvm3_values,"pvm3",PVM_MODULE_ENTRIES);

  (void) make_module_function(stacktop,"pvm-mytid",Fn_pvm_mytid,0);
  (void) make_module_function(stacktop,"pvm-exit",Fn_pvm_exit,0);

  (void) make_module_function(stacktop,"pvm-config",Fn_pvm_config,0);
  (void) make_module_function(stacktop,"pvm-spawn",Fn_pvm_spawn,-4);
  (void) make_module_function(stacktop,"pvm-tasks",Fn_pvm_tasks,0);
  (void) make_module_function(stacktop,"pvm-pstat",Fn_pvm_pstat,1);
  (void) make_module_function(stacktop,"pvm-kill",Fn_pvm_kill,1);

  (void) make_module_function(stacktop,"pvm-send",Fn_pvm_send,-4);
  (void) make_module_function(stacktop,"pvm-mcast",Fn_pvm_mcast,-4);
  (void) make_module_function(stacktop,"pvm-receive",Fn_pvm_receive,-4);

  (void) make_module_function(stacktop,"pvm-recv",Fn_pvm_recv,2);
  (void) make_module_function(stacktop,"pvm-read",Fn_pvm_read,-1);

  (void) make_module_function(stacktop,"pvm-probe",Fn_pvm_probe,2);
  (void) make_module_function(stacktop,"pvm-joingroup",Fn_pvm_joingroup,1);
  (void) make_module_function(stacktop,"pvm-lvgroup",Fn_pvm_lvgroup,1);
  (void) make_module_function(stacktop,"pvm-gtasks",Fn_pvm_gtasks,1);
  (void) make_module_function(stacktop,"pvm-barrier",Fn_pvm_barrier,2);
  (void) make_module_function(stacktop,"pvm-bcast",Fn_pvm_bcast,-4);
  close_module();
}		      
#endif /* PVM3 */
