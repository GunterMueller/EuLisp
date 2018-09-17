/* ******************************************************************** */
/* system.h          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Environment specific code   		                                */
/* ******************************************************************** */

/*
 * $Id: system_p.h,v 1.2 1994/02/08 12:09:12 djb Exp $
 *
 * $Log: system_p.h,v $
 * Revision 1.2  1994/02/08  12:09:12  djb
 * added jap's changes to pass stacktop to lower level reader code
 *
 * Revision 1.1  1994/02/08  12:08:08  djb
 * Initial revision
 *
 * Revision 1.7  1992/11/26  16:11:33  pab
 * _ANY fixes
 *
 * Revision 1.5  1992/06/11  23:58:39  pab
 * fixed includes
 *
 * Revision 1.4  1992/05/28  16:10:42  rjb
 * comment text after #endif
 *
 * Revision 1.3  1992/01/29  13:49:32  pab
 * sysV fixes
 *
 * Revision 1.2  1991/09/11  12:07:47  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:06  pab
 * Initial revision
 *
 * Revision 1.7  1991/05/16  11:31:20  pab
 * 'C' garbage collector additions
 *
 * Revision 1.5  1991/02/28  13:53:19  kjp
 * Fixed fopen/close macros in sysv - added GC state codes.
 *
 * Revision 1.4  1991/02/13  18:26:05  kjp
 * Pass.
 *
 */

/*
 * System specific prototypes...
 *
 * (see '.c' for details)
 */

#ifndef SYSTEM_P_H

#define SYSTEM_P_H

/*
 * Interfaces...
 */

extern int system_running_processors;
#define RUNNING_PROCESSORS() (system_running_processors)

/* ******************************************************************** */
/*                             System V                                 */
/* ******************************************************************** */

#ifdef MACHINE_SYSTEMV

extern int system_scheduler_number;

#define system_fopen(f,m) (fopen(f,m))
#define system_fclose(f)  (fclose(f))

extern int system_read(LispObject *,int,char*,int);

extern char *system_malloc(int);
extern char *system_static_malloc(int);
/*
#define system_allocate_semaphore(addr) (*(addr)=semget(IPC_PRIVATE,1,NULL))
#define system_initialise_semaphore(addr) semctl(*(addr),0,SETVAL,1)
#define system_open_semaphore(addr) semop(*(addr),&system_sem_handler,1)
#define system_close_semaphore(addr) semctl(*(addr),0,SETVAL,1)
*/

extern void system_allocate_semaphore(volatile SystemSemaphore *);
extern void system_initialise_semaphore(volatile SystemSemaphore *);
extern void system_open_semaphore(LispObject *, volatile SystemSemaphore *);
extern void system_close_semaphore(volatile SystemSemaphore *);
extern int system_maybe_open_semaphore(LispObject *, volatile SystemSemaphore *);

extern LispObject system_thread_rig(LispObject*,LispObject);
extern LispObject system_thread_start(LispObject);
extern LispObject system_thread_call(LispObject);
extern LispObject system_thread_suspend(void);
extern LispObject system_thread_reschedule(void);
extern void system_thread_abort(void);

#define GC_sync_test() \
          (SYSTEM_GLOBAL_VALUE(GC_state) == GC_SINKING \
            ? ((void) garbage_collect(stacktop),TRUE) \
	    : FALSE)

extern SYSTEM_GLOBAL(SystemSemaphore,atomic_semaphore);

#define ATOMIC(stacktop,x) 	       \
{					\
  system_open_semaphore(stacktop,&SYSTEM_GLOBAL_VALUE(atomic_semaphore)); \
  x;					\
  system_close_semaphore(&SYSTEM_GLOBAL_VALUE(atomic_semaphore)); \
}


#endif

/* ******************************************************************** */
/*                            Any Machine                               */
/* ******************************************************************** */

#ifdef MACHINE_ANY

#ifndef PROCESSORS
#define PROCESSORS (1)
#endif

#define system_fopen(f,m) (fopen(f,m))
#define system_fclose(f)  (fclose(f))

#define system_read(s,a,b,c) (read(a,b,c))

extern char *system_malloc(int);
#ifdef CGC
#define  system_malloc(n) gc_malloc(n)
#define system_static_malloc(n) gc_malloc(n)
#else
extern char *system_static_malloc(int);
#endif

#define system_allocate_semaphore(addr) IGNORE(*addr)
#define system_initialise_semaphore(addr)
#define system_open_semaphore(x,addr)
#define system_close_semaphore(addr)
#define system_maybe_open_semaphore(x,addr) (1)

extern int system_scheduler_number;

/* Place where 'system_thread' calls would be */

#define GC_sync_test()
#define ATOMIC(s,x) x
#endif

/* ******************************************************************** */
/*                               BSD                                    */
/* ******************************************************************** */

#ifdef MACHINE_BSD

extern int system_scheduler_number;

#define system_fopen(f,m) (fopen(f,m))
#define system_fclose(f)  (fclose(f))

#define system_read(s,a,b,c) (read(a,b,c))

extern char *system_malloc(int);
extern char *system_static_malloc(int);

#define system_allocate_semaphore(addr) (*(addr)=1)
#define system_initialise_semaphore(addr) (*(addr)=1)
#define system_open_semaphore(x,addr) \
          { \
	    if (*(addr)!=1){ \
	      fprintf(stderr,"Unexpected value of semaphore"); \
		 \
	    } \
	    *(addr) = 0; \
	  }
#define system_close_semaphore(addr) (*(addr)=1)
#define system_maybe_open_semaphore(x,addr) (*(addr) == 1 ? *addr = 0,1 : 0)

extern LispObject system_thread_rig(LispObject*,LispObject);
extern LispObject system_thread_start(LispObject);
extern LispObject system_thread_call(LispObject);
extern LispObject system_thread_suspend(void);
extern LispObject system_thread_reschedule(void);
extern void system_thread_abort(void);

#define GC_sync_test()
#define ATOMIC(s,x) x
#endif

/* Thread system prototypes... */

extern SYSTEM_GLOBAL_ARRAY1(LispObject,system_scheduler_threads,MAX_PROCESSORS);

/* Initialisation prototypes... */

extern void runtime_initialise_system(void);
extern void system_initialise_scheduler(void);
extern void system_lisp_exit(int);

#define THREAD_LIMBO (0)
#define THREAD_READY (1)
#define THREAD_RUNNING (2)
#define THREAD_RETURNED (3)
#define THREAD_ABORTED (4)

#define GC_SINKING    (0)
#define GC_REGISTERED (1)
#define GC_MARKED     (2)
#define GC_DONE       (3)

extern SYSTEM_GLOBAL(int,GC_state);

#ifndef SYSTEM_MAX_SHARED_SIZE
#define SYSTEM_MAX_SHARED_SIZE (1*1024*1024)
#endif

#ifdef WITH_PROFILING

#include <sys/time.h>

#define PROFILE_RUNNING  ('R')
#define PROFILE_IDLE     ('I')
#define PROFILE_GC_START ('G')
#define PROFILE_GC_END   ('E')
#define PROFILE_BLOCKED  ('B')

#define PROFILE(x) x

#define DEF_PROFILE_TIMER(name) struct itimerval *name;

#define INIT_PROFILE_TIMER(name) \
          name \
            = (struct itimerval *) \
                system_static_malloc(sizeof(struct itimerval)); \
          name->it_interval.tv_sec = 0; \
          name->it_interval.tv_usec = 10; \
          name->it_value.tv_sec = 0; \
          name->it_value.tv_usec = 0; \
          setitimer(ITIMER_REAL,name,NULL); 

#define PROFILE_TIME(name) (name->it_value.tv_sec)

extern struct itimerval *system_local_timer;

#else
#define PROFILE(x)
#define DEF_PROFILE_TIMER(x)
#define INIT_PROFILE_TIMER(x)
#define PROFILE_TIME(x)
#endif

#endif /* SYSTEM_P_H */


