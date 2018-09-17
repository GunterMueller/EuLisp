/* ******************************************************************** */
/* system.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Environment specific code   		                                */
/* ******************************************************************** */

/*
 * $Id: system.c,v 1.4 1994/04/19 10:16:12 djb Exp $
 *
 * $Log: system.c,v $
 * Revision 1.4  1994/04/19  10:16:12  djb
 * removed catcher for ctl-c
 * 'Go away and leave me alone!' was really irritating
 *
 * Revision 1.3  1994/04/11  13:28:05  djb
 * added malloc/realloc alternatives to sbrk/brk
 * default is to use malloc/realloc, use NO_MALLOC
 * flag to compile sbrk/brk version
 * (sbrk/brk may clash with linked libraries that use
 * malloc/realloc such as pvm.
 *
 * Revision 1.2  1994/02/08  12:06:42  djb
 * added jap's code to pass stacktop down to low level reader code
 *
 * Revision 1.1  1994/02/08  12:05:33  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:59:26  pab
 * New Version.
 * Just Norcroft changes..
 * 
 * Revision 1.9  1992/11/26  16:09:59  pab
 * More changes
 *
 * Revision 1.7  1992/06/16  19:32:16  pab
 * MS-dross fixes
 *
 * Revision 1.6  1992/04/27  22:00:12  pab
 * more volatile
 *
 * Revision 1.5  1992/04/26  21:09:45  pab
 * added i860 support (needs checking)
 *
 * Revision 1.4  1992/02/10  12:01:10  pab
 * Gc on/off fixes
 *
 * Revision 1.3  1992/01/29  13:49:07  pab
 * sysV fixes
 *
 * Revision 1.2  1991/09/11  12:07:46  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:06  pab
 * Initial revision
 *
 * Revision 1.9  1991/05/16  11:29:26  pab
 * 'C' garbage collector additions
 *
 * Revision 1.8  1991/05/15  20:09:53  kjp
 * Tidied suignal handling.
 *
 * Revision 1.7  1991/04/02  16:42:03  kjp
 * BSD signal support.
 *
 * Revision 1.6  1991/02/28  13:53:54  kjp
 * Fixed semaphore interaction with GC and sysv signalling.
 *
 * Revision 1.5  1991/02/13  18:25:46  kjp
 * Pass.
 *
 */

#define SCHEDBUG(x) 

/*
 * Change Log:
 *   Version 1, April 1990
 */

/*

 * This file (and it's accompanying '.h') are intended to encapsulate all
 * of the system specific requirements of FEEL. For each target system
 * there must be a set of functions conforming to the requested 
 * configuration (e.g. if threads are required, the thread operations
 * must exist along with their defined types - otherwise, they may be
 * omitted).

 * It is expected that this file may degenerate into hash-includes.

 */

#include <signal.h>

#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"
#include "error.h"
#include "state.h"

#include "allocate.h"
#include "garbage.h"
#include "threads.h"

#include <sys/ioctl.h>
#include <sys/types.h>

#ifdef NORCROFT
#include "/opt/home/jpff/Norcroft/syscall.h"

int fileno(FILE *f)
{
  return _fileno(f);
}

int read(int fildes, void *buf, int nbyte)
{
  return _syscall3(SYS_read, fildes, buf, nbyte);
}

int write(int fildes, void *buf, int nbyte)
{
  return _syscall3(SYS_write, fildes, buf, nbyte);
}

int getpid(void)
{
  return _syscall0(SYS_getpid);
}

int fork(void)
{
  return _vfork();
}

#endif

/*
 * Nasty signal hackery! 
 */

#ifdef WITH_BSD_SIGNALS
#define sighold(sig) sigmask(sig)
#define sigset(sig,func) signal(sig,func)
#define my_sigpause(sig) sigpause(0)
#else
#define my_sigpause(sig) sigpause(sig)
#endif

/*
 * For a system to run with threads, the following must be provided:
 *
 *   Types: 
 *     SystemSemaphore 
 *
 *   Functions:
 *
 *     char* system_malloc(int)
 *     char* system_static_malloc(int)
 *
 *     void system_allocate_semaphore(SystemSemaphore *)
 *     void system_initialise_semaphore(SystemSemaphore *)
 *     void system_open_semaphore(SystemSemaphore *)
 *     void system_close_semaphore(SystemSemaphore *)
 *
 */

SYSTEM_THREAD_SPECIFIC_DECLARATION(int,system_scheduler_number);

int system_running_processors = 1; /* Unless initialised otherwise */

/*
 * Stack checking...
 */


SYSTEM_GLOBAL(int,system_interrupt_flag);

/* C-c, and others interrupt handler... */

void system_handle_interrupts(LispObject *stacktop, int sig)
{
  int flags;

  flags=SYSTEM_GLOBAL_VALUE(system_interrupt_flag);
  SYSTEM_GLOBAL_VALUE(system_interrupt_flag)=0;
  
  call_thread_signal(stacktop,CAR(interpreter_thread),flags);
}

static void system_control_catcher(int sig)
{
  if (GOT_SIGNAL(SYSTEM_GLOBAL_VALUE(system_interrupt_flag),sig)) {
    fprintf(stderr,"Go away and leave me alone!\n");
    fflush(stderr);
    sigset(sig,system_control_catcher); /* Reinstall */
    return;
  }
  else
    {
      REGISTER_SIGNAL(SYSTEM_GLOBAL_VALUE(system_interrupt_flag),sig);
      sigset(sig,system_control_catcher); /* Reinstall */
    }
}


/* ******************************************************************** */
/*                            Any Machine                               */
/* ******************************************************************** */

#ifdef MACHINE_ANY
char *system_malloc(int n)
{
#ifdef NO_MALLOC
  char *sbrk(int);
#else
  char *malloc(int);
#endif  
  char *addr;

#ifdef NO_MALLOC  
  if ((addr = (char *) sbrk(n)) == (char *) -1) {
#else  
  if ((addr = (char *) malloc(n)) == (char *) -1) {
#endif  
    perror("INITERROR: unable to allocate enough memory from system\n");
    system_lisp_exit(1);
  }

  return(addr);
}

char *system_static_malloc(int n)
{
#ifdef NO_MALLOC    
  char *sbrk(int);
#else  
  char *malloc(int);
#endif  
  char *addr;

#ifdef NO_MALLOC  
  if ((addr = (char *) sbrk(n)) == (char *) -1) {
#else  
  if ((addr = (char *) malloc(n)) == (char *) -1) {
#endif  
    perror("INITERROR: out of static memory\n");
    system_lisp_exit(1);
  }

  return(addr);
}

void system_lisp_exit(int n) 
{
  exit(n);
}

void system_abort(int sig)
{
  fprintf(stderr,"\n\nAborting feel on signal %d\n\n",sig);
  exit(sig);
}

void runtime_initialise_system() 
{
  system_scheduler_number = 0;

#ifdef TRAP_ALL
  signal(15,system_abort); 
  signal(11,system_abort);
  signal(10,system_abort);
  signal(02,system_control_catcher); 
#endif

  SYSTEM_INITIALISE_GLOBAL(int,system_interrupt_flag,FALSE);
}

#endif

/* ******************************************************************** */
/*                               BSD                                    */
/* ******************************************************************** */

#ifdef MACHINE_BSD

/*
 * Memory allocation... 
 */

char *system_malloc(int n)
{
#ifdef NO_MALLOC    
  char *sbrk(int);
#else  
  char *malloc(int);
#endif  
  char *addr;

#ifdef NO_MALLOC
  if ((addr = (char *) sbrk(n)) == (char *) -1) {
#else  
  if ((addr = (char *) malloc(n)) == (char *) -1) {
#endif  
    perror("INITERROR: unable to allocate enough memory from system\n");
    system_lisp_exit(1);
  }

  return(addr);
}

#define STATIC_MALLOC_HUNK_SIZE (4096)

char *static_free_ptr;
int static_free_count;

char *system_static_malloc(int n)
{
  char *val;

  n = n + (n%BYTE_ALIGNMENT == 0 ? n : (BYTE_ALIGNMENT-n%BYTE_ALIGNMENT)); 
  /* Alignment.. */

  if (static_free_count < n) {
    char *new;

    if ((new = system_malloc(STATIC_MALLOC_HUNK_SIZE)) == NULL) {
      fprintf(stderr,"INIT ERR: out of static memory\n");
      exit(1);
    }

    static_free_ptr = new;
    static_free_count = STATIC_MALLOC_HUNK_SIZE;
  }

  val = static_free_ptr;
  static_free_ptr = val + n;
  static_free_count -= n;

  return(val);
}

/*
 * Semaphores... (in header - dummies)
 */

/*
 * Signal handling...
 */

/* Bad news signal handler... */

void system_abort(int sig)
{
  fprintf(stderr,"\n\nAborting EuLisp on signal %d... ",sig);
  fprintf(stderr,"done\n\n");

  exit(1);
}

/*
 * Init and cleanup... 
 */

void system_lisp_exit(int n)
{
  exit(n);
}

void system_sleep_until_kicked() {}

void system_kick_sleepers() {}

void system_register_process(int pid) {IGNORE(pid);}

void runtime_initialise_system() 
{
  signal(SIGTERM,system_abort); /* Catch terminations */
/*
  signal(SIGSEGV,system_abort);
  signal(SIGBUS,system_abort);
*/
#ifdef CATCH_CTL_C  
  signal(SIGINT,system_control_catcher); /* C-c with any luck */
#else  
  signal(SIGINT,system_abort); /* C-c with any luck */
#endif  
  system_scheduler_number = 0;

  SYSTEM_INITIALISE_GLOBAL(int,system_interrupt_flag,FALSE);
}

#endif

/* ******************************************************************** */
/*                             System V                                 */
/* ******************************************************************** */

#ifdef MACHINE_SYSTEMV

/*
 * Memory allocation...
 */

/* Of shared memory segments... */

#define MAX_SHARED_SEGMENTS 100

int shared_ids[MAX_SHARED_SEGMENTS];
int shared_segment_count;

#if i860
#include <errno.h>
#define MAX_SHARED_PAGE_SZ 1024*1024
#define PAGE_BOUNDARY 4096
  char *system_malloc(int n)
  {
 char *alloc_memory_block(int size);
 char *addr=0;
 int k=0;
 int left=n;

 if (n==0)
   return NULL;

 while (left > MAX_SHARED_PAGE_SZ || addr==0)
   {
     if (addr==0)
	addr=alloc_memory_block(MAX_SHARED_PAGE_SZ);
     else 
	alloc_memory_block(MAX_SHARED_PAGE_SZ);
     left -=MAX_SHARED_PAGE_SZ;
   }
 if (left>0)
   alloc_memory_block(left);
 return addr;
}

char *alloc_memory_block(int size)
{
 static int id=0;
 char *addr, *newaddr;
 int res;

 if (id==0)
   id=25;
 printf("alloc: %d %d\n",id,size);

 if (size==0)
   return NULL;

 if ((size&(PAGE_BOUNDARY-1)))
   size=(size+PAGE_BOUNDARY)&(~(PAGE_BOUNDARY-1));

#ifdef NO_MALLOC
 addr=sbrk(0);
#else
 addr=malloc(0);
#endif
 
 if (((int)addr&(PAGE_BOUNDARY-1)))
   addr= (char *)(((int)addr+PAGE_BOUNDARY)&(~(PAGE_BOUNDARY-1)));

#ifdef NO_MALLOC
 if (brk(addr+size)==-1)
#else
 if (realloc(addr,size)!=addr)
#endif     
     perror("Brk");
 printf("allocating: %x,%x\n",addr,size);
 do
   {
     extern volatile int errno;
     errno=0;
     id++;
     res=create_shared_region(id,addr,size,0);
     if (res<0)
	perror("create");
   }
 while (res==-1 && errno==EINVAL);

 if (res== -1)
   perror("create");
 shared_ids[shared_segment_count] = id;

 ++shared_segment_count;  
 id++;
 
 return addr;
}


#else
char *system_malloc(int n)
{
  int seg;
  char *addr;

  if (shared_segment_count >= MAX_SHARED_SEGMENTS) {
    fprintf(stderr,"Can't allocate shared segment\n");
    system_lisp_exit(1);
  }

  if ((seg = shmget(IPC_PRIVATE,n,511|IPC_CREAT)) < 0) {
    perror("shmget\n");
    system_lisp_exit(1);
  }

  if ((int) (addr = shmat(seg,NULL,NULL)) == -1) {
    perror("shmat\n");
    system_lisp_exit(1);
  }

  shared_ids[shared_segment_count] = seg;

  ++shared_segment_count;

  return(addr);
}
#endif
/* Of static shared bits (assumes serial for now)... */

#define STATIC_MALLOC_HUNK_SIZE (4096)

char *static_free_ptr;
int static_free_count;

char *system_static_malloc(int n)
{
  char *val;

  n = n + (n%BYTE_ALIGNMENT == 0 ? n : (BYTE_ALIGNMENT-n%BYTE_ALIGNMENT)); 
  /* Alignment.. */

  if (static_free_count < n) {
    char *new;

    if ((new = system_malloc(STATIC_MALLOC_HUNK_SIZE)) == NULL) {
      fprintf(stderr,"INIT ERR: out of static memory\n");
      system_lisp_exit(1);
    }

    static_free_ptr = new;
    static_free_count = STATIC_MALLOC_HUNK_SIZE;
  }

  val = static_free_ptr;
  static_free_ptr = val + n;
  static_free_count -= n;

  return(val);
}

/*
 * Semaphores...
 */

void system_initialise_semaphore(volatile SystemSemaphore *ptr)
{
  *ptr = 1;
}

void system_allocate_semaphore(volatile SystemSemaphore *ptr)
{
  *ptr = 1;
}
/* SystemV Semaphores excluded */

#ifdef SEMAPHORES_SOFTWARE

#include "lamport.h"

LamportSemaphore system_semaphore;
int systemV_semaphore;

void system_open_semaphore(LispObject *stacktop,
			   volatile SystemSemaphore *ptr)
{
  extern SYSTEM_GLOBAL(SystemSemaphore,GC_sem);
  int mine_flag = FALSE;

 top:

  lamport_enter(system_semaphore,system_scheduler_number);

  if (*ptr == 1) {
    *ptr = 0;
    mine_flag = TRUE;
  }

  lamport_exit(system_semaphore,system_scheduler_number);

  if (mine_flag) return;

  if (ptr != &SYSTEM_GLOBAL_VALUE(GC_sem))
    while (*(volatile SystemSemaphore *)ptr != 1) GC_sync_test();
  else
    while (*(volatile SystemSemaphore *)ptr != 1);

  goto top;
}

#ifdef RETRY_OPEN_SEM
int system_maybe_open_semaphore(LispObject *stacktop, SystemSemaphore *ptr)
{
  int mine_flag = FALSE;
  int i=0;

  for (i=0 ; i<RETRY_OPEN_SEM ; i++)
    {
      lamport_enter(system_semaphore,system_scheduler_number);

      if (*ptr == 1) {
	*ptr = 0;
	mine_flag = TRUE;
	break;
      }
      lamport_exit(system_semaphore,system_scheduler_number);
    }
  GC_sync_test();

  return(mine_flag);
}

#else
int system_maybe_open_semaphore(LispObject *stacktop,
				volatile SystemSemaphore *ptr)
{
  int mine_flag = FALSE;
 
  lamport_enter(system_semaphore,system_scheduler_number);

  if (*ptr == 1) {
    *ptr = 0;
    mine_flag = TRUE;
  }

  lamport_exit(system_semaphore,system_scheduler_number);

  GC_sync_test();

  return(mine_flag);
}
#endif
#endif

void system_close_semaphore(volatile SystemSemaphore *ptr)
{
  *ptr = 1;
}

/*
 * Signal handling...
 */

static SYSTEM_GLOBAL_ARRAY1(int,system_pids,MAX_PROCESSORS);

/* Bad news, free up semaphores and shared memory... */

void system_abort(int sig)
{
  int i;

  fprintf(stderr,"\n\nAborting EuLisp on signal %d... ",sig);

  for (i=0;i<shared_segment_count;++i) {
    (void) shmctl(shared_ids[i],IPC_RMID,NULL);
  }


#if i860
  for (i=0; i<shared_segment_count ; i++)
    delete_shared_region(shared_ids[i]);
#else
    for (i=0;i<shared_segment_count;++i) {
      (void) shmctl(shared_ids[i],IPC_RMID,NULL);
    }
#endif
  /* Kill of other processes too */

  for (i=0; i<RUNNING_PROCESSORS(); ++i)
    if (i != system_scheduler_number)
      kill(SYSTEM_GLOBAL_ARRAY1_VALUE(system_pids,i),SIGQUIT);

  fprintf(stderr,"done\n\n");

  exit(1);
}

/*
 * Init and cleanup... 
 */

void system_lisp_exit(int n)
{
  int i;

  for (i=0;i<shared_segment_count;++i) {
    (void) shmctl(shared_ids[i],IPC_RMID,NULL);
  }

  (void) semctl(systemV_semaphore,NULL,IPC_RMID,NULL);

  /* Kill of other processes too */

  for (i=0; i<RUNNING_PROCESSORS(); ++i)
    if (i != system_scheduler_number)
      kill(SYSTEM_GLOBAL_ARRAY1_VALUE(system_pids,i),SIGQUIT);

  exit(n);
}

/*
 * Signal fiddling...
 */

#define KICK_SIGNAL (SIGUSR1)

void system_kick_pid(int pid)
{
  extern int kill(int,int);

  (void) kill(pid,KICK_SIGNAL);
}

static void system_nout()
{
/*
  fprintf(stderr,"%x in system_nout\n",getpid());
*/
  sigset(KICK_SIGNAL,system_nout);
}

void system_sleep_until_kicked()
{
  extern int sigpause(int);

  (void) my_sigpause(KICK_SIGNAL);
  sighold(KICK_SIGNAL);
/*
  fprintf(stderr,"{W:%d}\n",system_scheduler_number); fflush(stderr);
*/
  fflush(stderr);
}

static LispObject *saved_stacktop;

static void system_read_nout()
{
  LispObject *stacktop = saved_stacktop;
/*
  fprintf(stderr,"%x in system_read_nout, stacktop = %x\n",getpid(),saved_stacktop);
*/
  sigset(KICK_SIGNAL,system_read_nout);
/*  GC_sync_test();*/
}

#include <errno.h>

int system_read(LispObject* stacktop,int fno,char *buf,int max)
{
  int error;

  (void) sigset(KICK_SIGNAL,system_read_nout);

  do {

    error = read(fno,buf,max);
    if (error > 0) {

      sigset(KICK_SIGNAL,system_nout);
      (void) sighold(KICK_SIGNAL);

#ifndef i860
      PROFILE(printf("PVAL:%x\n",PROFILE_TIME(system_local_timer)));
#endif
      fflush(stdout);
      return(error);
    }

  } while (errno == EINTR);

  return(error);
}

void system_kick_sleepers()
{
  int i;

  for (i=0; i<RUNNING_PROCESSORS(); ++i) {
/*
    fprintf(stderr,"kicking pid %ld\n",
	    SYSTEM_GLOBAL_ARRAY1_VALUE(system_pids,i));
*/
    if (i != system_scheduler_number)
      kill(SYSTEM_GLOBAL_ARRAY1_VALUE(system_pids,i),KICK_SIGNAL);
  }
}

#ifndef i860
DEF_PROFILE_TIMER(system_local_timer)
#endif

void system_register_process(int n)
{
  SYSTEM_GLOBAL_ARRAY1_VALUE(system_pids,n) = getpid();
  sigset(KICK_SIGNAL,system_nout);
  sighold(KICK_SIGNAL);

#ifdef i860
  INIT_PROFILE_TIMER(system_local_timer);
#endif
}

SYSTEM_GLOBAL(SystemSemaphore, atomic_semaphore);

void runtime_initialise_system()
{

#ifdef SEMAPHORES_SYSTEMV

  if ((systemV_semaphore = semget(IPC_PRIVATE,1,511)) < 0) {
    perror("INIT ERROR: can't get semaphore\n");
    exit(1);
  }
  if (semctl(systemV_semaphore,0,SETVAL,1) < 0) {
    perror("INIT ERROR: initialise semaphore\n");
    exit(1);
  }

  system_sem_handler_array[0][0].sem_num = 0;
  system_sem_handler_array[0][0].sem_op = -1;
  system_sem_handler_array[0][0].sem_flg = NULL;

#endif
  shared_segment_count = 0;

  static_free_ptr = NULL;
  static_free_count = 0;

  /* Bad news signals */

  sigset(SIGTERM,system_abort); /* Catch terminations */
  sigset(SIGQUIT,system_abort); /* Quits */
  sigset(SIGSEGV,system_abort); /* Segmentation faults */
  sigset(SIGBUS,system_abort);  /* Bus errors */
  /* Error trapped signals */

#ifdef CATCH_CTL_C  
  sigset(SIGINT,system_control_catcher); /* C-c with any luck */
#else  
  sigset(SIGINT,system_abort); /* C-c with any luck */
#endif  

  /* Ignore kick signals until we need them */

  sighold(KICK_SIGNAL); 

#ifdef SEMAPHORES_SOFTWARE

  system_semaphore 
    = (LamportSemaphore) 
        system_static_malloc(sizeof(struct lamport_semaphore));
  lamport_initialise(system_semaphore);

  SYSTEM_INITIALISE_GLOBAL_ARRAY1(int,system_pids,MAX_PROCESSORS,0);
  SYSTEM_GLOBAL_ARRAY1_VALUE(system_pids,0) = getpid();
#endif

  SYSTEM_INITIALISE_GLOBAL(SystemSemaphore, atomic_semaphore, NULL);
  system_allocate_semaphore(&SYSTEM_GLOBAL_VALUE(atomic_semaphore));
  SYSTEM_INITIALISE_GLOBAL(int,system_interrupt_flag,FALSE);
}

#endif

