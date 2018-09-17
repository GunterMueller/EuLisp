/* ******************************************************************** */
/* system.h          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Environment specific code   		                                */
/* ******************************************************************** */

/*

 * System specific types...
 *
 * (see '.c' for details)

 */

#ifndef SYSTEM_T_H

#define SYSTEM_T_H

/*

 * Types and declarations...

 */

/* ******************************************************************** */
/*                             System V                                 */
/* ******************************************************************** */

#ifdef MACHINE_SYSTEMV

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

/* SGI says redeclaration
extern char *shmat(int,char *,int);
*/

typedef int SystemSemaphore;

#define SYSTEM_THREAD_SPECIFIC_DECLARATION(type,name) type name
#define SYSTEM_THREAD_SPECIFIC_VALUE(name)            (name)

#define SYSTEM_GLOBAL(type,name)            volatile type* name
#define SYSTEM_INITIALISE_GLOBAL(type,name,value) \
          (name=(volatile type*)(system_static_malloc(sizeof(type))),*name=value)
#define SYSTEM_GLOBAL_VALUE(name)           (*(name))
#define SYSTEM_GLOBAL_ARRAY1(type,name,n) volatile type* name
#define SYSTEM_INITIALISE_GLOBAL_ARRAY1(type,name,num,val) \
          { \
	    int siga1; \
	    name=(volatile type*)(system_static_malloc(sizeof(type)*num)); \
	    for (siga1=0;siga1<num;++siga1) { \
	      *(name+siga1)=val; \
	    } \
	  }
#define SYSTEM_GLOBAL_ARRAY1_VALUE(name,index) (*(name+index))
#define ADD_SYSTEM_GLOBAL_ROOT(name) add_root(name)

#endif

/* ******************************************************************** */
/*                             Any Machine                              */
/* ******************************************************************** */

#ifdef MACHINE_ANY

typedef int SystemSemaphore;

#define SYSTEM_THREAD_SPECIFIC_DECLARATION(type,name) type name
#define SYSTEM_THREAD_SPECIFIC_VALUE(name) (name)

#define SYSTEM_GLOBAL(type,name) type name
#define SYSTEM_GLOBAL_VALUE(name) (name)
#define SYSTEM_INITIALISE_GLOBAL(type,name,value) name=value
#define SYSTEM_GLOBAL_ARRAY1(type,name,dim) type name[dim]
#define SYSTEM_INITIALISE_GLOBAL_ARRAY1(type,name,num,val) \
          { \
	    int siga1; \
	    for (siga1=0;siga1<num;++siga1) { \
	      *(name+siga1)=val; \
	    } \
	  }
#define SYSTEM_GLOBAL_ARRAY1_VALUE(name,index) (name[index])
#define ADD_SYSTEM_GLOBAL_ROOT(name) add_root(&name)
#endif

/* ******************************************************************** */
/*                               BSD                                    */
/* ******************************************************************** */

#ifdef MACHINE_BSD

typedef int SystemSemaphore;

#define SYSTEM_THREAD_SPECIFIC_DECLARATION(type,name) type name
#define SYSTEM_THREAD_SPECIFIC_VALUE(name)            (name)

#define SYSTEM_GLOBAL(type,name)            type name
#define SYSTEM_INITIALISE_GLOBAL(type,name,value) (name=value)
#define SYSTEM_GLOBAL_VALUE(name)           (name)
#define SYSTEM_GLOBAL_ARRAY1(type,name,n) type name[n]
#define SYSTEM_INITIALISE_GLOBAL_ARRAY1(type,name,num,val) \
          { \
	    int siga1; \
	    for (siga1=0;siga1<num;++siga1) { \
	      *(name+siga1)=val; \
	    } \
	  }
#define SYSTEM_GLOBAL_ARRAY1_VALUE(name,index) (name[index])
#define ADD_SYSTEM_GLOBAL_ROOT(name) add_root(&name)
#endif

/*
  * Stack ckecking
*/
#define NSIGNALS 32
#define REGISTER_SIGNAL(flag,n) ((flag)|=1<<(n))
#define GOT_SIGNAL(flag,n) ((flag)&(1<<(n)))
#define UNREGISTER_SIGNAL(flag,n) ((flag)&=~(1<<n))
/* I-thread gets hit with signals */
extern SYSTEM_GLOBAL(int,system_interrupt_flag);


#define THREAD_STACK_MARGIN (3*1024)
#define THREAD_GC_STACK_MARGIN (512)

#ifdef CHECK_KEYBOARD
#define ON_CHECK_KBD(x) x
#else 
#define ON_CHECK_KBD(x)
#endif

#ifdef MACHINE_ANY
#define ON_NOTANY(x) 
#else
#define ON_NOTANY(x) x
#endif

#define STACKS_OK_P(stacktop,form)	\
{					\
  int handle;				\
    /**/				\
  ON_CHECK_KBD(				\
    {					\
      static int eval_loops;		\
    if (eval_loops++ == CHECK_KEYBOARD) \
      {  eval_loops = 0;		\
	 if (kbhit())			\
	   if (getkey() == 17)		\
	     system_control_catcher(2); \
       }				\
  }					\
 )		\
    \
  if (SYSTEM_GLOBAL_VALUE(system_interrupt_flag)) \
	system_handle_interrupts(stacktop,0);	\
	  \
   ON_NOTANY(if ((int) &handle - (int) (STACK_BASE()) < THREAD_STACK_MARGIN) \
    CallError(stacktop,"SYSTEM: C stack overflowing",form,NONCONTINUABLE); \
    )		\
      \
  if (thread_gc_stack_size((CURRENT_THREAD()))  			\
      - (int) (stacktop-GC_STACK_BASE()) * sizeof(LispObject)	\
      < THREAD_GC_STACK_MARGIN)						\
    {									\
      CallError(stacktop,"SYSTEM: GC stack overflowing",form,NONCONTINUABLE); \
    }									\
}

#endif /* SYSTEM_T_H */

