/* ******************************************************************** */
/*  copy.c        copyright (c) university of bath 1992			*/
/*                                                                      */
/* creation of modules							*/
/* ******************************************************************** */
/*
 * $Id: copy.c,v 1.5 1994/02/11 15:26:25 djb Exp $
 *
 * $Log: copy.c,v $
 * Revision 1.5  1994/02/11  15:26:25  djb
 * fixed copy alignment bug
 *
 * Revision 1.4  1994/02/10  17:24:03  djb
 * Fixed weak wrapper copy bug
 *
 * Revision 1.3  1994/01/28  15:42:43  djb
 * #ifndef DGC'd the generational gc code in do_gc_sync
 * (otherwise it breaks d-gc)
 *
 * Revision 1.2  1994/01/26  11:53:14  djb
 * merged in DGC diffs
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 2.2  1993/01/19  12:54:58  pab
 * Fixed up memory initialisation
 *
 * Revision 2.1  1993/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 *
 * Revision 1.38  1992/12/09  16:30:12  djb
 * de-botched botched DGC #ifdefs
 * de-staticed nroots
 * made the_stacktop a LispObject * in accordance with stacktop
 *
 * Revision 1.37  1992/11/26  16:57:13  pab
 * Much change
 *
 * Revision 1.33  1992/07/18  17:13:20  pab
 * TYPE_SPECIAL_METHOD support, general tidy.
 *
 * Revision 1.31  1992/06/16  19:36:24  pab
 * weak wrapper code
 *
 * Revision 1.30  1992/06/14  16:43:45  pab
 * incorporated branch from V1.26
 *
 * Revision 1.29  1992/05/29  12:18:03  pab
 * changed headers
 *
 * Revision 1.28  1992/05/29  09:53:44  rjb
 * ALIGN8 and a NULL -> 0
 *
 * Revision 1.27  1992/05/29  09:47:44  djb
 * hooks for CGC mark+sweep (all #ifdef CGC)
 *
 * Revision 1.26  1992/04/30  19:41:21  pab
 * fiddled with tracing
 *
 * Revision 1.25  1992/04/30  11:07:31  pab
 * lost end-page bug. Lowered rounding
 *
 * Revision 1.24  1992/04/29  12:33:18  pab
 * tracing code added
 *
 * Revision 1.23  1992/04/27  21:55:42  pab
 * if it moves, round it
 *
 * Revision 1.22  1992/04/26  20:55:46  pab
 * fixes for generic version, plus static vector type preliminary support,
 * no-sockets fixes
 *
 * Revision 1.21  1992/03/13  18:06:51  pab
 * SysV fixes (mainly relinquishing pages and synchonisation)
 *
 * Revision 1.20  1992/02/27  15:46:57  pab
 * bytecode + error changes
 *
 * Revision 1.19  1992/02/13  13:49:58  pab
 * *** empty log message ***
 *
 * Revision 1.17  1992/02/11  13:38:04  pab
 * removed printing gc_enabled
 *
 * Revision 1.16  1992/02/10  12:11:41  pab
 * fixed circular lists
 * gc_enabaled now global
 *
 * revision 1.12  1991/04/02  21:25:30  kjp
 * compiler tidying.
 * copying garbage collector. Replaces allocate + garbage.c */
#include <string.h>

#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "state.h"
#include "copy.h"
#include "weak.h"
#include "threads.h"

#define N_SLOTS_IN_CLASS N_SLOTS_IN_STRUCT(struct class_structure)
#define N_SLOTS_IN_THREAD N_SLOTS_IN_STRUCT(struct thread_structure)

#define OTHER_SPACE(x) 1-(x)

#define is_newspace(x) \
  ((gcof(x)&1) ==wspace)

#define set_wspace(x,v) \
  (gcof(x) = ((gcof(x) & 0xfe)|v))

#define forwardof(x) \
  (lval_classof(x))

#define set_forwarded(x, new) \
  ( *(&gcof(x))|=0x2 , forwardof(x)=new)

#define is_forwarded(x) \
  ((gcof(x))&0x2)

#define init_gcof(x,v) \
  (gcof(x)=(v)<<2)
  
#define is_old(x) \
  ((gcof(x))&0x4)

#define HEADERSIZE sizeof(Object_t)
/* should not need to allocate any fixed objects yet... */
#ifdef ALIGN8
#define ROUNDTO 8
#else
#define ROUNDTO 4
#endif
#define ROUND_ADDR(x) ((((int)x)&(ROUNDTO-1))==0 ? x : (x)+(ROUNDTO-(((int)x)&(ROUNDTO-1))))
#define is_fixed(x) 0

/* Sundry Generational arbitrary constants */
#define MIN_NEW_PAGES ((1024*1024)/PAGE_SIZE)
#define FILL_FACTOR 256
/* Want debugging */

#ifndef NODEBUG
#define TRACE_GC /* writes allocation logging to a file */
#define ON_DEBUG(x) x
#define COPY_BUG(x) x
#else
#define ON_DEBUG(x)
#define COPY_BUG(x) 
#endif

#ifdef TRACE_GC
#define ON_TRACE(x) x
#include <time.h>

  FILE *trace_file;
  int counters[256];
  int total_moved;
#else
#define ON_TRACE(x)
#endif
/* which space are we in */
static int wspace;
static char *free_ptr;
static char *pg_end;
int gc_paranoia=0;
static int collect_count;
static int minor_gc;
/* BSD + SYSV */
static LispObject GC_thread;
  
/* SYSV only */
SYSTEM_GLOBAL(SystemSemaphore,GC_sem);
SYSTEM_GLOBAL(SystemSemaphore,Rig_sem);
SYSTEM_GLOBAL(int,GC_state);
static SYSTEM_GLOBAL(int,GC_register);      /* Who's arrived so far... */
static SYSTEM_GLOBAL(int,GC_exit_register); /* Who's left... */
static SYSTEM_GLOBAL(int,GC_turn); 	    /* whose go */
static SYSTEM_GLOBAL(int,gc_enabled); 	    /* can we... */
static SYSTEM_GLOBAL_ARRAY1(LispObject,GC_register_array,MAX_PROCESSORS);
static LispObject GC_tame_continue;
static SYSTEM_GLOBAL(PageList, old_pages);
/* Valid only in non-gc time */
static SYSTEM_GLOBAL(PageList, free_pages);
static SYSTEM_GLOBAL(PageList, aged_pages);
static SYSTEM_GLOBAL(int, aged_page_count);
static SYSTEM_GLOBAL(char *, old_page_end);
static SYSTEM_GLOBAL(char *, free_old_ptr);
static SYSTEM_GLOBAL(int,npages);
static SYSTEM_GLOBAL(int,pagelim);
static SYSTEM_GLOBAL_ARRAY1(int,ages,256);
static SYSTEM_GLOBAL(LispObject, weak_list);
static SYSTEM_GLOBAL(int, major_gc_1);
static int major_gc;
static int gc_counter = 0;

static SYSTEM_GLOBAL(PageList, oldspace);
static SYSTEM_GLOBAL(int, agelim);

static ClassInfo class_info[256];


static PageList current_page;
static PageList used_pages;

/* Called from inside copier */
#define ALLOC_SPACE(new,type,size) \
  {  \
    new= (type) free_ptr; 		\
    free_ptr+=size; 			\
    if (free_ptr>=pg_end) \
      {	ON_TRACE(fprintf(trace_file,"Grab: Wasted: %d\n",pg_end-(free_ptr-size)));\
	GRAB_PAGE(NULL,free_ptr,pg_end);	\
	new= (type) free_ptr; 		\
	free_ptr+=size;		\
       }			\
      }

#ifdef MACHINE_ANY
#define GRAB_PAGE_INTERNAL(stacktop,ptr,top) 		\
   { 					\
      ptr=free_pages->start; 		\
      top=free_pages->end; 		\
      current_page=free_pages;		\
      free_pages=free_pages->next; 		\
      current_page->next=used_pages; 		\
      used_pages=current_page;      		\
      npages++;					\
      COPY_BUG(fprintf(stderr,"{Grab: %d}",	\
		       current_page->id));	\
    }



#define GRAB_PAGE(x,y,z) GRAB_PAGE_INTERNAL(x,y,z)

#else
#define GRAB_PAGE_INTERNAL(stacktop,ptr,top) 		\
   { 					\
      ptr=ROUND_ADDR(S_G_V(free_pages)->start); 		\
      top=S_G_V(free_pages)->end; 		\
      current_page=S_G_V(free_pages);		\
      S_G_V(free_pages)=S_G_V(free_pages)->next; 		\
      current_page->next=used_pages; 		\
      used_pages=current_page;      		\
      S_G_V(npages)++;					\
      COPY_BUG(fprintf(stderr,"{Grab(%d): %d}",	\
		       system_scheduler_number,		\
		       current_page->id));		\
      COPY_BUG(memset(ptr,'x',top-ptr));		\
    }

#define GRAB_PAGE(stacktop,ptr,top) 		\
  {							\
    system_open_semaphore(stacktop,&S_G_V(GC_sem)); 	\
    GRAB_PAGE_INTERNAL(stacktop,ptr,top);		\
    system_close_semaphore(&S_G_V(GC_sem));		\
  }

#endif

#define MAYBE_GRAB_PAGE(res,stacktop,ptr,top)             \
{							\
    system_open_semaphore(stacktop,&S_G_V(GC_sem)); 	\
    if (S_G_V(npages)<S_G_V(pagelim))			\
      {							\
        GRAB_PAGE_INTERNAL(stacktop,ptr,top);		\
        res=1;						\
      }							\
    else						\
      res=0;					\
  /**/						\
    system_close_semaphore(&S_G_V(GC_sem)); \
  }
  
#define GRAB_AGED_PAGE(xptr,xtop)		\
do {					\
  PageList tmp;				\
  fprintf(stderr,"{Old grab: ptr: %x top: %x",xptr,xtop); \
  xptr=S_G_V(free_pages)->start;		\
  xtop=S_G_V(free_pages)->end;			\
  tmp=S_G_V(free_pages);			\
  S_G_V(free_pages)=S_G_V(free_pages)->next;		\
  tmp->next=S_G_V(aged_pages);			\
  S_G_V(aged_pages)=tmp;			\
  ON_DEBUG(fprintf(stderr," %d %x->%x}",aged_pages->id,xptr,xtop)); \
  S_G_V(aged_page_count)++;			\
  S_G_V(npages)++;				\
} while (0)

#define ALLOC_AGED_SPACE(new,type,sz)		\
{						\
  if (S_G_V(free_old_ptr) + sz >= S_G_V(old_page_end))			\
    GRAB_AGED_PAGE(S_G_V(free_old_ptr),S_G_V(old_page_end));	\
  /**/						\
  new=(type)S_G_V(free_old_ptr);		\
  S_G_V(free_old_ptr)+=sz;			\
}

#define PRINT_LISTS(stream)		\
{			\
    PageList xx;		\
    fputs("Free: ",stream);	\
    xx=S_G_V(free_pages);		\
    while (xx!=NULL)		\
      { fprintf(stream,"%d ",xx->id);		\
	xx=xx->next;		\
      }				\
    fputs("\nUsed: ",stream);	\
    xx=used_pages;		\
    while (xx!=NULL)		\
      { fprintf(stream,"%d ",xx->id);		\
	xx=xx->next;		\
      }		\
    fputs("\nOld: ",stream);	\
    xx=S_G_V(aged_pages);		\
    while (xx!=NULL)		\
      { fprintf(stream,"%d ",xx->id);		\
	xx=xx->next;		\
      }		\
    fputc('\n',stream);		\
  }
/* Forward functions --- blasted ANSI !*/
static void free_old_pgs(void);
static void free_weak_ptrs(void);

void init_allocator(int size)
{
#ifdef DGC
  gc_init(size);
#else
  PageList *newpage;
  char *space;
  char *end;
  int allocated=0;
  int pg_count=0;
  int i;
  size=2*size;
#endif
#ifndef MACHINE_ANY

  SYSTEM_INITIALISE_GLOBAL(SystemSemaphore,GC_sem,0);
  system_allocate_semaphore(&S_G_V(GC_sem));
  SYSTEM_INITIALISE_GLOBAL(SystemSemaphore,Rig_sem,0);
  system_allocate_semaphore(&S_G_V(Rig_sem));
  SYSTEM_INITIALISE_GLOBAL(int,GC_state,GC_DONE);
  SYSTEM_INITIALISE_GLOBAL(int,GC_register,0);
  SYSTEM_INITIALISE_GLOBAL(int,GC_exit_register,0);
  SYSTEM_INITIALISE_GLOBAL(int,pagelim,0);
  SYSTEM_INITIALISE_GLOBAL(PageList,free_pages,NULL);
  SYSTEM_INITIALISE_GLOBAL(PageList,old_pages,NULL);
  SYSTEM_INITIALISE_GLOBAL(PageList,aged_pages,NULL);
  SYSTEM_INITIALISE_GLOBAL(int,aged_page_count,0);
  SYSTEM_INITIALISE_GLOBAL(int,agelim,0);
  SYSTEM_INITIALISE_GLOBAL(char *, old_page_end, NULL);
  SYSTEM_INITIALISE_GLOBAL(char *, free_old_ptr, NULL);
  SYSTEM_INITIALISE_GLOBAL(int,npages,0);
  SYSTEM_INITIALISE_GLOBAL(int,GC_turn,0);
  SYSTEM_INITIALISE_GLOBAL_ARRAY1(LispObject,
				  GC_register_array,MAX_PROCESSORS,NULL);
  SYSTEM_INITIALISE_GLOBAL(int,major_gc_1,0);
#endif /*MACHINE_ANY*/

  SYSTEM_INITIALISE_GLOBAL(int,gc_enabled,0);
  SYSTEM_INITIALISE_GLOBAL(LispObject,weak_list,NULL);
  SYSTEM_INITIALISE_GLOBAL_ARRAY1(int,ages,256,0);
#ifndef DGC
  newpage= (PageList*)&S_G_V(free_pages);  

#ifndef SYSTEM_MAX_SHARED_SIZE
#define SYSTEM_MAX_SHARED_SIZE 4*1024*1024
#endif

  while (allocated<size)
    {
      int sz;

      if (allocated+SYSTEM_MAX_SHARED_SIZE > size)
	sz=size-allocated;
      else
	sz=SYSTEM_MAX_SHARED_SIZE;
      
      space=system_malloc(sz);
      end=space+sz;
      COPY_BUG(memset(space,'T',sz));
  
      while (space<end)
	{	
	  *newpage=(PageList) space;
	  (*newpage)->status=PAGE_FREE;
	  (*newpage)->end= ((space+PAGE_SIZE) < end ? space+PAGE_SIZE : end);
	  (*newpage)->id=pg_count;
	  (*newpage)->next=NULL;
	  newpage= &((*newpage)->next);
	  space+=PAGE_SIZE;
	  pg_count++;
	}
      allocated+=sz;
    }

  *newpage=NULL;
  
/*  printf("Initialised with: %x [%d pages]\n",size,pg_count);*/
  COPY_BUG(PRINT_LISTS(stderr));
  used_pages=NULL;
  wspace=0;
  S_G_V(pagelim)=pg_count/2;
  S_G_V(npages)=0;
  GRAB_PAGE(NULL,free_ptr,pg_end);

  for (i=0 ; i<256 ; i++)
    {	
      class_info[i].size= -1;
      class_info[i].csize= 0;
    }
  class_info[TYPE_ENV].size = OBJ_SIZE(struct env_structure);
  class_info[TYPE_ENV].csize = 0;
  class_info[TYPE_FIXENV].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_FIXENV].csize = 0;
  class_info[TYPE_MBIND].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_MBIND].csize = 0;
  class_info[TYPE_FIXMBIND].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_FIXMBIND].csize = 0;
  class_info[TYPE_EXP_MBIND].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_EXP_MBIND].csize = 0;
  class_info[TYPE_EXP_FIXMBIND].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_EXP_FIXMBIND].csize = 0;
  class_info[TYPE_CONS].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_CONS].csize = 0;
  class_info[TYPE_CHAR].size = OBJ_SIZE(struct character_structure);
  class_info[TYPE_CHAR].csize = OBJ_SIZE(struct character_structure);
  /* string -- special case */
  class_info[TYPE_SYMBOL].size = OBJ_SIZE(struct symbol_structure);
  class_info[TYPE_SYMBOL].csize = OBJ_CSIZE(struct symbol_structure,lmodule);
  /* class -- std code */
  /* instance -- std code */
  class_info[TYPE_SPECIAL].size = OBJ_SIZE(struct special_structure);
  class_info[TYPE_SPECIAL].csize = OBJ_CSIZE(struct special_structure,name);
  /* vector -- special case */
  class_info[TYPE_INT].size = OBJ_SIZE(struct integer_structure);  
  class_info[TYPE_INT].csize = OBJ_SIZE(struct integer_structure);  
  class_info[TYPE_FLOAT].size = OBJ_SIZE(struct float_structure);
  class_info[TYPE_FLOAT].csize = OBJ_SIZE(struct float_structure);
  class_info[TYPE_SEMAPHORE].size = OBJ_SIZE(struct semaphore_structure);
  class_info[TYPE_SEMAPHORE].csize = OBJ_SIZE(struct semaphore_structure);
#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))
  class_info[TYPE_LISTENER].size = OBJ_SIZE(struct listener_structure);
  class_info[TYPE_LISTENER].csize = OBJ_SIZE(struct listener_structure);
  class_info[TYPE_SOCKET].size = OBJ_SIZE(struct socket_structure);
  class_info[TYPE_SOCKET].csize = OBJ_SIZE(struct socket_structure);
#endif
  class_info[TYPE_NULL].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_NULL].csize = 0;
  class_info[TYPE_WEAK_WRAPPER].size = OBJ_SIZE(struct cons_structure);
  class_info[TYPE_WEAK_WRAPPER].csize = 0;
  class_info[TYPE_CONTINUE].size = OBJ_SIZE(struct continue_structure);
  class_info[TYPE_C_MODULE].size = OBJ_SIZE(struct c_module_structure); /* negative => static */
  class_info[TYPE_I_MODULE].size = OBJ_SIZE(struct i_module_structure);
  class_info[TYPE_CONTINUE].size = OBJ_SIZE(struct continue_structure);
  class_info[TYPE_CONTINUE].csize = OBJ_CSIZE(struct continue_structure,value);
  class_info[TYPE_C_FUNCTION].size = OBJ_SIZE(struct c_function_structure);
  class_info[TYPE_C_FUNCTION].csize = OBJ_CSIZE(struct c_function_structure,env);
  class_info[TYPE_I_FUNCTION].size = OBJ_SIZE(struct i_function_structure);
  class_info[TYPE_I_FUNCTION].csize = OBJ_CSIZE(struct i_function_structure,env);
  /* generic -- std */
  /* method -- std */
  class_info[TYPE_C_MACRO].size = OBJ_SIZE(struct c_function_structure);
  class_info[TYPE_C_MACRO].csize = OBJ_CSIZE(struct c_function_structure,env);
  class_info[TYPE_I_MACRO].size = OBJ_SIZE(struct i_function_structure);
  class_info[TYPE_I_MACRO].csize = OBJ_CSIZE(struct i_function_structure,env);
  /* B -anything: std */
  class_info[TYPE_B_FUNCTION].size = 0;
  class_info[TYPE_B_MACRO].size = 0;
  class_info[TYPE_METHOD].size = 0;
  class_info[TYPE_GENERIC].size = 0;
  class_info[TYPE_INSTANCE].size = 0;
  class_info[TYPE_CLASS].size = 0;
  class_info[TYPE_THREAD].size = 0;

#endif /*DGC */
#ifdef TRACE_GC
  if (trace_file==NULL)
    {	
      char buf[20];
      sprintf(buf,"gc.%d",getpid());
  
      trace_file=fopen(buf,"w");
    }
#endif /*TRACE_GC*/
}


void runtime_initialise_garbage_collector(LispObject *stacktop)
{
  (GC_tame_continue)=allocate_continue(stacktop);
  GC_thread=nil;

  add_root(&GC_tame_continue);
  add_root(&GC_thread);
}

void initialise_garbage(LispObject *stacktop)
{  /* Pretend we're a module */
  LispObject garbage_collect(LispObject *);

  GC_thread = allocate_thread(stacktop,2048,1024,0);
  (void) make_module_function(stacktop,"GC",garbage_collect,0);
}

/* Called when a new process forks */
#ifndef MACHINE_ANY
void runtime_reset_allocator(LispObject *stacktop)
{
  COPY_BUG(fprintf(stderr,"Proc: %d starting\n",system_scheduler_number));

  used_pages=NULL;
  GRAB_PAGE(NULL,free_ptr,pg_end);

  GC_thread = allocate_thread(stacktop,2048,1024,0);
  add_root(&GC_thread);
  (GC_tame_continue)=allocate_continue(stacktop);
  add_root(&GC_tame_continue);
  system_open_semaphore(stacktop,&S_G_V(Rig_sem));
  RIG_GC_THREAD(stacktop);
  system_close_semaphore(&S_G_V(Rig_sem));

}
#endif

EUFUN_0(garbage_collect)
{
  void do_gc_sync(LispObject *);

  do_gc_sync(stacktop);
  if (CAR(Cb_GC_hook)!=nil && CDR(Cb_GC_hook)==nil)
    {	
      CDR(Cb_GC_hook)=lisptrue;
      EUCALL_2(Fn_apply,CAR(Cb_GC_hook),nil);
      CDR(Cb_GC_hook)=nil;
    }
  return nil;

}
EUFUN_CLOSE

int current_space()
{
  return wspace;
}

#ifndef MACHINE_ANY
extern void rig_gc_thread(LispObject *stacktop)
{
#ifndef MACHINE_ANY
  RIG_GC_THREAD(stacktop);
#endif
}
#endif

/* c-roots */
#define MAXROOTS 300
#ifdef DGC
int nroots=0;
#else
static int nroots=0;
#endif

LispObject *roots[MAXROOTS];

int add_root(volatile LispObject *root)
{	
  int x=nroots;

  roots[nroots++]=(LispObject*)root;
  
  return x;
}

void copy_root(LispObject *x)
{
  LispObject copy_object(LispObject);
  *x=copy_object(*x);
}

void copy_on()
{
  S_G_V(gc_enabled)++;
  COPY_BUG(fprintf(stderr,"{+%d}",S_G_V(gc_enabled)));
}

void copy_off()
{
  S_G_V(gc_enabled)--;
  COPY_BUG(fprintf(stderr,"{-%d}",S_G_V(gc_enabled)));
}

/* These will have to more complicated eventually */
void ON_collect()
{
  S_G_V(gc_enabled)++;
  COPY_BUG(fprintf(stderr,"{+%d}",S_G_V(gc_enabled)));
}

void OFF_collect()
{
  S_G_V(gc_enabled)--;
  COPY_BUG(fprintf(stderr,"{-%d}",S_G_V(gc_enabled)));
}
/****************************************
 * allocation 
 ****************************************/

static int a_count;
#define ALLOC_GAP 2048
int alloc_gap=ALLOC_GAP;

#ifdef DGC
LispObject *the_stacktop;

LispObject allocate_nbytes(LispObject *stacktop,int n,int type)
{
  LispObject object;

  object=(LispObject)gc_malloc(stacktop,n);
  lval_typeof(object)=type;
  return(object);
}
#else
/* Assume that n is rounded */
LispObject allocate_nbytes(LispObject *stacktop,int n,int type)
{
  void do_gc_sync(LispObject *);
  LispObject object;
  char *new;
  
  COPY_BUG(if (n<HEADERSIZE) fprintf(stderr,"Object too small to hold header\n") );

#ifdef TRACE_GC
  counters[type&255]+=n;
#endif
#ifdef ALIGN8
  n=ROUND_ADDR(n);
#endif
#ifndef NODEBUG  
  if (gc_paranoia)
    fprintf(stdout,"{%x:%d}",type,n);
#endif
  a_count+=n;
#ifdef NODEBUG
  if ( !(free_ptr+n<pg_end))
#else
  if ((gc_paranoia && a_count>alloc_gap && S_G_V(gc_enabled))
      || !(free_ptr+n<pg_end))
#endif    
    {
      int res;
#ifdef TRACE_GC
      fprintf(trace_file,"new page: wastage: %d\n",pg_end-free_ptr);
#endif

      MAYBE_GRAB_PAGE(res,stacktop,free_ptr,pg_end);
      
      if (!res)
	{
	  a_count=0;
	  if (S_G_V(gc_enabled)<1)
	    { 
	      fprintf(stderr,"{Grabbed Page 'cos I couldn't GC[%d]}\n",S_G_V(gc_enabled));
	      GRAB_PAGE(stacktop,free_ptr,pg_end);
	    }
	  else
	    {
	      do_gc_sync(stacktop);
	      if (CAR(Cb_GC_hook)!=nil && CDR(Cb_GC_hook)==nil)
		{		
		  CDR(Cb_GC_hook)=lisptrue;
		  EUCALL_2(Fn_apply,CAR(Cb_GC_hook),nil);
		  CDR(Cb_GC_hook)=nil;
		}
	    }
	}
    }

  ALLOC_SPACE(object,LispObject,n);

  lval_typeof(object)=type;
  gcof(object)=(short)wspace;
#ifdef GENERATIONAL
  ageof(object)=0;
#endif

  return(object);
}
#endif

#ifdef MACHINE_ANY
void do_gc_sync(LispObject *stacktop)
{
  void swap_spaces(LispObject *stacktop);
  int i;

/*  fprintf(stderr,"Collection %d initiated: %d used, %d bytes (%d%%) remaining\n",
	  collect_count,S_G_V(npages)*PAGE_SIZE,(S_G_V(pagelim)-S_G_V(npages))*PAGE_SIZE,0);*/
   fflush(stdout);
   fprintf(stderr, "GC %d starting...", ++gc_counter); fflush(stderr);  

   S_G_V(old_pages)=NULL;
   S_G_V(npages)=0;
   S_G_V(weak_list)=NULL;
   swap_spaces(stacktop);
  
   free_old_pgs();
   free_weak_ptrs();
 }
#else /* ! MACHINE_ANY */
void do_gc_sync(LispObject *stacktop)
{
  int i;

  /* we must save state early */
  save_state(stacktop,CURRENT_THREAD()->THREAD.state);
  /* Wait for the last gc to finish */
  while (  S_G_V(GC_state)!=GC_DONE
	 &&S_G_V(GC_state)!=GC_SINKING)
    ;
  /* register myself */
  system_open_semaphore(stacktop,&S_G_V(GC_sem));
  ++S_G_V(GC_register);

  fflush(stdout);
  if (S_G_V(GC_register) == 1)
    {                    /* First */
      S_G_V(GC_state) = GC_SINKING;
      system_kick_sleepers();
/*      fprintf(stderr,"GC sinking(%d) ---  ",S_G_V(gc_enabled));*/
/*      fprintf(stderr, "GC %d kicking/synching...", ++gc_counter); fflush(stderr);
*/
    }

/*  fprintf(stderr,"%d ",system_scheduler_number);*/
  /* if last, set flag */
  if (S_G_V(GC_register) == RUNNING_PROCESSORS())
    { /* Last */
      S_G_V(GC_state) = GC_REGISTERED;
      /*fprintf(stderr,"\n"); fflush(stdout);*/
/*      fprintf(stderr,"Collection %d initiated: %d used, %d bytes (%d%%) remaining\n",
	      collect_count,S_G_V(npages)*PAGE_SIZE,(S_G_V(pagelim)-S_G_V(npages))*PAGE_SIZE,0);*/
      fprintf(stderr, "starting..."); fflush(stderr);
      S_G_V(GC_turn)=0;
      S_G_V(npages)=S_G_V(aged_page_count);
      S_G_V(old_pages) = NULL;
      S_G_V(weak_list)=NULL;

#ifndef DGC
      /* Test for generational GC */

      if (S_G_V(pagelim) - S_G_V(npages) < MIN_NEW_PAGES)
	{
	  PageList ptr;
	  
	  fprintf(stderr,"Major GC: %d pages used, %d max",npages,S_G_V(pagelim));
	  S_G_V(major_gc_1)=1;
	  ptr=used_pages;
	  while (ptr->next!=NULL)
	    ptr=ptr->next;
	  ptr->next=S_G_V(aged_pages);
	  S_G_V(aged_pages)=NULL;
	  S_G_V(aged_page_count)=0;
	  S_G_V(npages)=0;
	  S_G_V(old_page_end)=NULL;
	  S_G_V(agelim)=256;
	}
      else
	{
	  int space=0;
	  S_G_V(major_gc_1)=0;
	  
	  for (i=0 ; i<256 && space < (S_G_V(pagelim)-S_G_V(npages))*((PAGE_SIZE*FILL_FACTOR)/1024) ; i++)
	    space+=ages[i];
	  S_G_V(agelim)= i-1;

	  if (S_G_V(agelim)==255 && S_G_V(aged_page_count)==0)
	    S_G_V(agelim)=3;
	    
	}
      for (i=0 ;i<256; i++) ages[i]=0;
#endif /* DGC */

    }		
  
  system_close_semaphore(&S_G_V(GC_sem));
  

  SYSTEM_GLOBAL_ARRAY1_VALUE(GC_register_array,system_scheduler_number) 
    = CURRENT_THREAD();
  
  /* boot any sleepers */

  system_kick_sleepers();

  /* wait until all get the idea */
  while (S_G_V(GC_state)!=GC_REGISTERED)
    ;
  /* Save myself */

  /* we all copy --- in serial 'cos its easier that way */

  while(S_G_V(GC_turn)!=system_scheduler_number)
    ;

  if (!set_continue(stacktop,(GC_tame_continue)))
    {
      LispObject temp = CURRENT_THREAD();
      LispObject *newstack;

      COPY_BUG(fprintf(stderr," {Proc: %d leaping %x %x %x}\n",system_scheduler_number,
		       (GC_tame_continue)->CONTINUE.thread,GC_thread,temp));
      newstack = load_thread(GC_thread);
      call_continue(newstack,GC_thread->THREAD.state,temp);
    }
  
  /* done: should signal this */

  S_G_V(GC_turn)++;
  
  if (system_scheduler_number==RUNNING_PROCESSORS()-1)
    {	
#ifndef DGC
      free_old_pgs();
      free_weak_ptrs();
#endif
      S_G_V(GC_state)=GC_MARKED;
    }

  while(S_G_V(GC_state)!=GC_MARKED)
      ;
  /* Now we can go */

  system_open_semaphore(stacktop,&S_G_V(GC_sem));
  --S_G_V(GC_register);
  if (S_G_V(GC_register)==0)
    S_G_V(GC_state)=GC_DONE;
  system_close_semaphore(&S_G_V(GC_sem));

  
/*  fprintf(stderr,"GC done\n");*/
  
}

#ifdef DGC
void gcollect(LispObject *stacktop)
{
  time_t time_now;

  (void)time(&time_now);
  fprintf(stderr,"GC started %s\n",ctime(&time_now));

  do_gc_sync(stacktop);

  if (CAR(Cb_GC_hook)!=nil && CDR(Cb_GC_hook)==nil)
    {	
      CDR(Cb_GC_hook)=lisptrue;
      EUCALL_2(Fn_apply,CAR(Cb_GC_hook),nil);
      CDR(Cb_GC_hook)=nil;
    }

  (void)time(&time_now);
  fprintf(stderr,"GC finished %s\n",ctime(&time_now));
}
#endif

void first_gc_mark_call(LispObject *stacktop)
{
#ifdef DGC
  void real_gcollect(LispObject *stacktop);
#else
  void swap_spaces(LispObject *stacktop);
#endif

  LispObject ret;

  COPY_BUG(printf("First invokation of GC mark: %x\n",stacktop); fflush(stdout));
  stacktop=thread_gc_stack_base(GC_thread);
 reset:

  ret = GC_thread->THREAD.state->CONTINUE.value;

  COPY_BUG(printf("Laying continue in GC mark: %x\n",stacktop); fflush(stdout));	
  if (set_continue(stacktop,(GC_thread->THREAD.state)))
    {	
      goto reset;
    }
  STACK_TMP(ret);

  COPY_BUG(printf("Marking in GC mark\n"); fflush(stdout));

#ifdef DGC
  save_state(stacktop,GC_thread->THREAD.state);
  real_gcollect(stacktop);
#else
  swap_spaces(stacktop);
#endif

  UNSTACK_TMP(ret);

  COPY_BUG(fprintf(stderr,"Jumping back: target: (%x %d) %x %d %d %d %d\n  gc_thread: (%x %d) %x %d %d\n",
		   ret,ret->THREAD.header.gc,
		   ret->THREAD.state, 
		   ret->THREAD.state->CONTINUE.header.gc,
		   ret->THREAD.state->CONTINUE.header.type,
		   ret->THREAD.state->CONTINUE.handler_stack->CONS.header.type,
		   ret->THREAD.state->CONTINUE.handler_stack->CONS.header.gc,
		   GC_thread,
		   GC_thread->THREAD.header.gc,
		   GC_thread->THREAD.state, 
		   GC_thread->THREAD.state->CONTINUE.header.gc,
		   GC_thread->THREAD.state->CONTINUE.header.type);
	   fflush(stdout));
  /**save_state(stacktop,GC_thread);**/
  (void) load_thread(ret); /* this returns the wrong value for our porpoises */
  call_continue(NULL,(GC_tame_continue),nil);
}
#endif



/* Collection */

void swap_spaces(LispObject *stacktop)
{
  void copy_root(LispObject *);
  void show_stack_space(void);

  char *oldspace;
  PageList pg,tmp,*ptr;
  int i;

#ifdef TRACE_GC
  {
    time_t time_now;
    char *str;
    int k,j=0;
    

    time_now=time(NULL);
    str=ctime(&time_now);
    fprintf(trace_file,"GC %d started: %s\n",collect_count,str);
    fprintf(trace_file,"Used: %d\n",S_G_V(npages)*PAGE_SIZE);

    for (k=0; k<255; k++)
      {	
	if (counters[k]!=0)
	  {
	    fprintf(trace_file,"%x: %6d ",k,counters[k]);
	    if ((++j)%6==0)
	      fputc('\n',trace_file);
	  }
	counters[k]=0;
      }	
    total_moved=0;
    fputc('\n',trace_file);
    PRINT_LISTS(trace_file);
    fflush(trace_file);
  }
#endif
  major_gc=S_G_V(major_gc_1);
  /* make sure that all is well */
  save_state(stacktop,CURRENT_THREAD()->THREAD.state);
  COPY_BUG(PRINT_LISTS(stderr));
  
  pg=current_page;
  used_pages=NULL;
  wspace=1-wspace;
  /* begin the copy process */
  GRAB_PAGE(stacktop,free_ptr,pg_end);

  for (i=nroots-1; i >= 0; i--)
    copy_root(roots[i]);

  /* Free all oldspace */
  /* Assumes that free_pages is unlocked */
  while (pg!=NULL)
    { /* insertion sort on the old pages */
      tmp=pg->next;

      ptr=(PageList*)&S_G_V(old_pages);
      if (*ptr!=NULL)
	{
	  while ((*ptr)->next!=NULL
		 && (*ptr)->next->id < pg->id)
	    ptr=&(*ptr)->next;
      
	  pg->next=(*ptr)->next;
	  (*ptr)->next=pg;
	}
      else 
	{
	  *ptr=pg;
	  pg->next=NULL;
	}
      pg=tmp;
    }

/*  fprintf(stderr,"Collection Completed: %d used, %d bytes (%d%%) remaining\n",*/
  fprintf(stderr, "done. %d used, %d bytes (%d%%) remaining\n",
	  S_G_V(npages)*PAGE_SIZE,
	  (S_G_V(pagelim)-S_G_V(npages))*PAGE_SIZE,
	  ((S_G_V(pagelim)-S_G_V(npages))*100)/
	  S_G_V(pagelim));
  /*show_stack_space();*/
  collect_count++;
  COPY_BUG(PRINT_LISTS(stderr));

#ifdef TRACE_GC
  {
    time_t time_now;
    char *str;
    int k,j;
    time_now=time(NULL);
    str=ctime(&time_now);
    fprintf(trace_file,"Using: %d\n",S_G_V(npages)*PAGE_SIZE);
    PRINT_LISTS(trace_file);
    fprintf(trace_file,"Totals: %d\n",total_moved);    
    for (k=0,j=0; k<255; k++)
      {	
	if (counters[k]!=0)
	  {
	    fprintf(trace_file,"%x: %6d ",k,counters[k]);
	    if ((++j)%6==0)
	      fputc('\n',trace_file);
	    counters[k]=0;
	  }
      }
    fprintf(trace_file,"GC %d complete: %s\n",collect_count,str);
    fflush(trace_file);
  }
#endif
  return;
}

static void free_old_pgs()
{
  PageList tmp;

  tmp=S_G_V(free_pages);
  
  if (tmp==NULL)
    S_G_V(free_pages)=S_G_V(old_pages);
  else 
    {
      while(tmp->next!=NULL)
	{
	  tmp=tmp->next;
	}
      tmp->next=S_G_V(old_pages);
    }
}

static void free_weak_ptrs()
{
  LispObject wptr;

  wptr=S_G_V(weak_list);
  
  while (wptr!=NULL)
    {
      if (is_forwarded(weak_ptr_val(wptr)))
	weak_ptr_val(wptr)=forwardof(weak_ptr_val(wptr));
      else
	weak_ptr_val(wptr)=nil;
      
      wptr=weak_ptr_chain(wptr);
    }
  S_G_V(weak_list)=NULL;
}
#ifndef NODEBUG
#define CAREFUL_DECLS   \
   LispObject copied
#ifdef NOLOWTAGINTS
#define copy_obj_careful(object) \
  (copied=copy_object(object),  \
   copied==NULL || ((gcof(copied)&1)==wspace)  \
   ? (copied) \
   : (fprintf(stderr,"Wrong space: %x %x\n", copied, gcof(copied)), system_lisp_exit(0), nil))
#define gc_return(copied)  { if ( (gcof(copied)&1)==wspace) return copied ; else system_lisp_exit(1); }
#else 
#define  copy_obj_careful(x) \
   (copied=copy_object(x),	\
    (copied==NULL || is_fixnum(x) || ((gcof(copied)&1)==wspace))  \
    ? (copied) \
    : (fprintf(stderr,"Wrong space: %x\n", copied), system_lisp_exit(0), nil))

#endif /**NOLOWTAGINTS**/
#else
#define CAREFUL_DECLS
#define copy_obj_careful(x) (copy_object(x))
#define gc_return(x) return x
#endif

#define FORWARD_HEADER(new,obj) \
  lval_typeof(new)=lval_typeof(obj);	\
  gcof(new)=wspace;			\
  class=lval_classof(obj);		\
  set_forwarded(obj,new);

#define COPY_ALLOC_SPACE(ptr,size)		\
  ALLOC_SPACE(new,LispObject,ptr,ROUND_ADDR(size)); 

#ifdef GENERATIONAL
/* Generational Copier */
#define leave_obj(x) (!major_gc && is_old(obj))
#define old_obj(x) (ageof(x)>=S_G_V(agelim))
#define inc_age(new,obj,size) \
  (ageof(new)=ageof(obj)+1, ages(ageof(new))+=size)
#else
#define leave_obj(x) 0
#define old_obj(x) 0
#define inc_age(new,obj,size) 0
#endif /* GENERATIONAL */

#if 1
LispObject copy_object(LispObject obj)
{
  int i,size;
  LispObject new,class;
  CAREFUL_DECLS;

  if (obj==NULL) return obj;

  if (is_forwarded(obj)) 
    gc_return(forwardof(obj));
  
  if (is_newspace(obj))
    gc_return(obj);

  switch (typeof(obj))
    {  /* Conses are always lists ... */
#ifndef GENERATIONAL
#if 1
    case TYPE_CONS:
      {
	LispObject walker,newcons;
	int count;

	ALLOC_SPACE(new,LispObject,sizeof(struct cons_structure));
#ifdef TRACE_GC
	counters[lval_typeof(obj)&255]+=sizeof(struct cons_structure);
#endif
	ON_DEBUG(fprintf(stderr,"(List start: {%x->%x} ",obj, new));
	init_gcof(new,0);

	class=classof(obj);
	init_gcof(new,0);
	set_forwarded(obj,new);
	set_wspace(new,wspace);
	lval_typeof(new)=TYPE_CONS;
	CAR(new)=class;
	
	count=1;
	walker=CDR(obj);
	while (walker !=NULL
	       && is_cons(walker)
	       && !is_forwarded(walker)
	       && !is_newspace(walker))
	  {
	    /* allocation phase */
#ifdef TRACE_GC
	    counters[lval_typeof(walker)&255]+=sizeof(struct cons_structure);
#endif
	    ALLOC_SPACE(newcons,LispObject,sizeof(struct cons_structure));
	    ON_DEBUG(fprintf(stderr," {%x->%x}",walker, newcons));
	    class=classof(walker);
	    init_gcof(newcons,0);
	    set_forwarded(walker,newcons);
	    set_wspace(newcons,wspace);
	    lval_typeof(newcons)=TYPE_CONS;
	    CAR(newcons)=class;
	    walker=CDR(walker);
	    count++;
	  }
	
	ON_DEBUG(fprintf(stderr,"Alloc done: %d items",count));
	newcons=new;	
	walker=obj;
	while (count>0)
	  {
	    /* Copying phase */
	    lval_classof(newcons)=copy_obj_careful(CAR(newcons));
	    CAR(newcons)=copy_obj_careful(CAR(walker));
	    CDR(newcons)=copy_obj_careful(CDR(walker));
	    ON_DEBUG(fprintf(stderr," %d",count));
	    walker=CDR(walker);
	    newcons=CDR(newcons);
	    count--;
	  }
	ON_DEBUG(fprintf(stderr,")"));
	gc_return(new);
      }
      break;
#endif
#endif    /* GENERATIONAL */  
    case TYPE_STRING:
      if (leave_obj(obj))
	{
	  set_wspace(obj,wspace);
	  lval_classof(obj)=copy_obj_careful(classof(obj));
	  gc_return(obj);
	}
      
      size=ROUND_ADDR(sizeof(Object_t)+obj->STRING.length+sizeof(int));
#ifdef TRACE_GC
	    counters[lval_typeof(obj)&255]+=size;
#endif
      if (old_obj(obj))
	{
	  ALLOC_AGED_SPACE(new,LispObject,size);
	  ageof(new)=S_G_V(agelim)+1;
	  inc_age(new,obj,0);
	  init_gcof(new,1);
	}
      else
	{
	  ALLOC_SPACE(new,LispObject,size);
	  inc_age(new,obj,size);
	  init_gcof(new,0);
	}
      
      class=classof(obj);
      set_forwarded(obj,new);
      set_wspace(new,wspace);
      lval_typeof(new)=TYPE_STRING;
      lval_classof(new)=copy_obj_careful(class);
      
      new->STRING.length=obj->STRING.length;
      memcpy(stringof(new),stringof(obj),obj->STRING.length);
      gc_return(new);
      break;

    case TYPE_VECTOR:

      if (leave_obj(obj))
	{
	  set_wspace(obj,wspace);
	  lval_classof(obj)=copy_obj_careful(classof(obj));
	  
	  for (i=0 ; i< obj->VECTOR.length ; i++)
	    vref(obj,i)=copy_obj_careful(vref(obj,i));
	  gc_return(obj);
	}
      else 
	{
	  size=ROUND_ADDR(sizeof(Object_t)+obj->VECTOR.length*sizeof(LispObject)+sizeof(int));
#ifdef TRACE_GC
	    counters[lval_typeof(obj)&255]+=size;
#endif
	  if (old_obj(obj))
	    {
	      ALLOC_AGED_SPACE(new,LispObject,size);
	      inc_age(new,obj,0);
	      init_gcof(new,1);
	    }
	  else
	    {
	      ALLOC_SPACE(new,LispObject,size);
	      init_gcof(new,0);
	      inc_age(new,obj,size);
	    }
	  class=classof(obj);
	  set_forwarded(obj,new);
	  set_wspace(new,wspace);
	  lval_typeof(new)=TYPE_VECTOR;
	  lval_classof(new)=copy_obj_careful(class);   
	  new->VECTOR.length=obj->VECTOR.length;
	  
	  for (i=0; i<obj->VECTOR.length; i++)
	    vref(new,i) = copy_obj_careful(vref(obj,i));
	  
	  gc_return(new);
	}
      break;

    case STATIC_TYPE | TYPE_STRING:
      set_wspace(obj,wspace);
      class=classof(obj);
      lval_classof(obj)=copy_obj_careful(class);
      gc_return(obj);
      break;

    case STATIC_TYPE | TYPE_VECTOR:
      set_wspace(obj,wspace);
      class=classof(obj);
      lval_classof(obj)=copy_obj_careful(class);
      for (i=0; i<obj->VECTOR.length; i++)
	vref(obj,i) = copy_obj_careful(vref(obj,i));
      gc_return(obj);
      break;
      
    case TYPE_WEAK_WRAPPER:
      ALLOC_SPACE(new,LispObject,sizeof(struct cons_structure));
#ifdef TRACE_GC
	    counters[lval_typeof(obj)&255]+=sizeof(struct cons_structure);
#endif
      class=classof(obj);
      init_gcof(new,0);
      set_forwarded(obj,new);
      set_wspace(new,wspace);
      lval_typeof(new)=TYPE_WEAK_WRAPPER;
      lval_classof(new) = copy_obj_careful(class);

      weak_ptr_chain(new)=S_G_V(weak_list);
      weak_ptr_val(new)=weak_ptr_val(obj);
      S_G_V(weak_list)=new;
      gc_return(new);
      break;

    case TYPE_B_FUNCTION:
    case TYPE_B_MACRO:
    case TYPE_METHOD:
    case TYPE_GENERIC:
    case TYPE_INSTANCE:
    case TYPE_CLASS:
    case TYPE_THREAD:
      {
	if (leave_obj(obj))
	  {
	    set_wspace(obj,wspace);
	    class=classof(obj);
	    lval_classof(obj)=copy_obj_careful(class);
	    for (i=0 ; i< intval(class->CLASS.local_count) ; i++)
	      slotref(obj,i)=copy_obj_careful(slotref(obj,i));

	    new=obj;
	  }
	else
	  {
	    size=ROUND_ADDR(sizeof(Object_t)+sizeof(LispObject)*intval(classof(obj)->CLASS.local_count));
#ifdef TRACE_GC
	    counters[lval_typeof(obj)&255]+=size;
#endif
	    if (old_obj(obj))
	      {
		ALLOC_AGED_SPACE(new,LispObject,size);
		init_gcof(new,1);
		inc_age(new,obj,0);
	      }
	    else
	      {
		ALLOC_SPACE(new,LispObject,size);
		init_gcof(new,0);
		inc_age(new,obj,size);
	      }
	    class=classof(obj);
	    set_forwarded(obj,new);
	    set_wspace(new,wspace);
	    lval_typeof(new)=typeof(obj);
	    lval_classof(new)=copy_obj_careful(class);
	    for (i=0 ; i< intval(class->CLASS.local_count) ; i++)
	      slotref(new,i)=copy_obj_careful(slotref(obj,i));
	  }
	if (typeof(new)==TYPE_THREAD)
	  {
	    LispObject *x=thread_gc_stack_base(obj);

	    if (x+thread_gc_stack_size(obj) < obj->THREAD.state->CONTINUE.gc_stack_pointer)
	      fprintf(stderr,"GC Stack overflow detected\n");
	    
	    while (x<obj->THREAD.state->CONTINUE.gc_stack_pointer)
	      { 
		if (!(((int) *x)&1)) /* Check for tags here */
		  *x = copy_obj_careful(*x);
		++x;
	      }
	  }
	gc_return(new);
      }
      break;

    default:
      {
	int type;
	type=typeof(obj);
	ON_DEBUG(if (class_info[type].size<0)
	  {
	    fprintf(stderr,"Invalid object type: %x\n",typeof(obj));
	    gc_return(obj);
	  });
	
	    if (leave_obj(obj))
	      {
		LispObject *ptr,*lim;

		lval_classof(obj)=copy_obj_careful(classof(obj));
		set_wspace(obj,wspace);

		ptr=(LispObject *) (((char *) obj)+sizeof(Object_t)+ (-class_info[type].csize)); 
		lim=(LispObject *) (((char *) obj)+sizeof(Object_t)+class_info[type].size); 
		while (ptr < lim)
		  {
		    *ptr=copy_obj_careful(*ptr);
		    ptr++;
		  }
		new=obj;
	      }
	    else 
	      {
		size=ROUND_ADDR(sizeof(Object_t)+class_info[type].size);
#ifdef TRACE_GC
	    counters[lval_typeof(obj)&255]+=size;
#endif
		if (old_obj(obj))
		  {
		    ALLOC_AGED_SPACE(new,LispObject,size);
		    inc_age(new,obj,0);
		    init_gcof(new,1);
		  }
		else
		  {
		    ALLOC_SPACE(new,LispObject,size);
		    init_gcof(new,0);
		    inc_age(new,obj,size);
		  }
		{
		  LispObject *xptr, *xoptr, *xlim;

		  class=classof(obj);
		  set_forwarded(obj,new);
		  set_wspace(new,wspace);
		  lval_typeof(new)=type;
		  lval_classof(new)=copy_obj_careful(class);
	    
		  if (class_info[type].csize!=0)
		    memcpy((char*)new+sizeof(Object_t),((char *) obj)+sizeof(Object_t), class_info[type].csize);
		  xoptr = (LispObject *) (((char *) obj)+sizeof(Object_t)+class_info[type].csize);
		  xptr =  (LispObject *) (((char *) new)+sizeof(Object_t)+class_info[type].csize);
		  xlim =  (LispObject *) (((char *) new)+sizeof(Object_t)+class_info[type].size);
		  while (xptr<xlim)
		    {
		      *xptr=copy_obj_careful(*xoptr);
		      ++xptr; ++xoptr;
		    }
		}
	      }
	gc_return(new);
      }
    } /* end switch */
  fprintf(stderr,"Unreachable code reached\n");
  exit(5);

}

/* Hack the stackpointer for GRAB_PAGE */
#else
LispObject copy_object(LispObject obj)
{
  int i;
  LispObject new;
  LispObject class;
  CAREFUL_DECLS;
#if 0
/* Adding these lines cures the SegV when compiled with ncc. */
  fprintf(trace_file,"{C: %p %x}",obj,obj==NULL ? 0 : typeof(obj));
  fflush(trace_file);
#endif  
/*  sprintf(abc,"%p",obj);*/
  if (obj==NULL) return NULL;
#ifndef NOLOWTAGINTS
  if (is_fixnum(obj)) return obj;
#endif

  if (is_forwarded(obj))
    return forwardof(obj);

  if (is_newspace(obj))
    return obj;
  else
    {
#ifdef TRACE_GC
      counters[lval_typeof(obj)&255]++;
#endif
/*  fprintf(trace_file,"{C: %p %x}",obj,obj==NULL ? 0 : typeof(obj));
  fflush(trace_file);*/
      switch(lval_typeof(obj))
	{
	case TYPE_NULL:
	case TYPE_MBIND:
	case TYPE_FIXMBIND:
	case TYPE_EXP_MBIND:
	case TYPE_EXP_FIXMBIND:
#if 0
	case TYPE_CONS:
#endif
	  /* Null is (cons nil  nil) with hacked type */
	  COPY_ALLOC_SPACE(free_ptr,  sizeof(struct cons_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  CAR(new)=copy_obj_careful(CAR(obj));
	  CDR(new)=copy_obj_careful(CDR(obj));
	  break;
#if 1
	case TYPE_CONS:
	  /* allocate space */
	  {	
	    LispObject walker,newcons;
	    int count, max;
	    COPY_ALLOC_SPACE(free_ptr,  sizeof(struct cons_structure));
	    FORWARD_HEADER(new,obj);

	    CAR(new)=class;
	    walker=CDR(obj);
	    max=1;
	    /* Note: this loop does not copy anything */
	    while (   walker!=NULL
#ifdef NOLOWTAGINTS
		   && !is_fixnum(walker)
#endif
		   && is_cons(walker)
		   && !is_forwarded(walker)
		   && !is_newspace(walker))
	      {
		ALLOC_SPACE(newcons,LispObject,free_ptr,  sizeof(struct cons_structure));
		FORWARD_HEADER(newcons,walker);
		/* Keep the class safe */
		CAR(newcons)=class;
		walker=CDR(walker);
		max++;
	      }
	    /* COPY_BUG(fprintf(stderr,"(List: %d elts",max)); */

	    newcons=new;
	    /* This loop does all the copying 
	       end is now the stopping point */
	    
	    count=0;
	    walker=obj;
	    while (count<max)
	      {
		lval_classof(newcons)=copy_obj_careful(CAR(newcons));
		CAR(newcons)=copy_obj_careful(CAR(walker));
		/* except for the end case equiv to CDR(newcons)=newcons+a bit */
		CDR(newcons)=copy_obj_careful(CDR(walker));
		walker=CDR(walker);
		newcons=CDR(newcons);
		count++;
	      }	
	  }
	  break;
#endif
#ifdef NOLOWTAGINTS	  
	case TYPE_INT:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct integer_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  intval(new)=intval(obj);
	  break;
#endif
	case TYPE_ENV:
	case TYPE_FIXENV:
 	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct env_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  new->ENV.variable = copy_obj_careful(obj->ENV.variable);
	  new->ENV.value = copy_obj_careful(obj->ENV.value);
	  new->ENV.next = copy_obj_careful(obj->ENV.next);
	  break;

	case TYPE_B_MACRO: /* All the standard types go here */
	case TYPE_METHOD:
	case TYPE_SPECIAL_METHOD:
	case TYPE_GENERIC:
	case TYPE_B_FUNCTION:
	case TYPE_INSTANCE:
	case TYPE_CLASS:
	  /* allocate space */
	  i=intval(classof(obj)->CLASS.local_count);
	  COPY_ALLOC_SPACE(free_ptr, sizeof(Object_t)+ i*sizeof(LispObject));
	  FORWARD_HEADER(new,obj);
	  
	  lval_classof(new)=copy_obj_careful(class);
	  for (i=0 ; i<intval(class->CLASS.local_count) ; i++)
	    slotref(new,i) = copy_obj_careful(slotref(obj,i));
	  break;
	  
	case TYPE_VECTOR:
	case TYPE_VECTOR|STATIC_TYPE:
	  if (is_static(obj))
	    {
	      set_wspace(obj,wspace); new=obj;
	      class=lval_classof(obj);
	    }
	  else
	    {
	      COPY_ALLOC_SPACE(free_ptr, sizeof(Object_t)+sizeof(int)+sizeof(LispObject)*obj->VECTOR.length);
	      FORWARD_HEADER(new,obj);
	    }
	  lval_classof(new)= copy_obj_careful(class);
	  new->VECTOR.length=obj->VECTOR.length;
	  for (i=0; i<obj->VECTOR.length; i++)
	    vref(new,i) = copy_obj_careful(vref(obj,i));
	  break;

	case TYPE_STRING|STATIC_TYPE:
	  set_wspace(obj,wspace);
	  new=obj;
	  class=lval_classof(obj);
	  lval_classof(new)=copy_obj_careful(class);
	  break;

	case TYPE_STRING:
	  COPY_ALLOC_SPACE(free_ptr, sizeof(Object_t)+obj->STRING.length+sizeof(int));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  new->STRING.length=obj->STRING.length;
	  bcopy(stringof(obj),stringof(new),obj->STRING.length);
	  break;


	case TYPE_CHAR:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct character_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  new->CHAR.font=obj->CHAR.font;
	  new->CHAR.code=obj->CHAR.code;
	  break; 

	case TYPE_CONTINUE:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct continue_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  (new->CONTINUE).thread = copy_obj_careful(obj->CONTINUE.thread);
	  
	  (new->CONTINUE).value = copy_obj_careful(obj->CONTINUE.value);
	  (new->CONTINUE).target = copy_obj_careful((obj->CONTINUE).target);

	  bcopy((char*)(obj->CONTINUE).machine_state, 
		(char *)new->CONTINUE.machine_state,
		sizeof(new->CONTINUE.machine_state));
	  (new->CONTINUE).gc_stack_pointer = obj->CONTINUE.gc_stack_pointer;

	  (new->CONTINUE).dynamic_env = copy_obj_careful(obj->CONTINUE.dynamic_env);
	  (new->CONTINUE).last_continue = copy_obj_careful(obj->CONTINUE.last_continue);
	  (new->CONTINUE).handler_stack = copy_obj_careful(obj->CONTINUE.handler_stack);
	  (new->CONTINUE).dp = copy_obj_careful(obj->CONTINUE.dp);

	  (new->CONTINUE).live = obj->CONTINUE.live;
	  (new->CONTINUE).unwind = obj->CONTINUE.unwind;  
	  break;
	  
	case TYPE_SPECIAL:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct special_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  new->SPECIAL.name = copy_obj_careful(obj->SPECIAL.name);
	  new->SPECIAL.env = copy_obj_careful(obj->SPECIAL.env);
	  new->SPECIAL.func = obj->SPECIAL.func;
	  break;

	case TYPE_SYMBOL:	
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct symbol_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  (new->SYMBOL).pname = copy_obj_careful(obj->SYMBOL.pname);
	  (new->SYMBOL).lvalue = copy_obj_careful(obj->SYMBOL.lvalue);
	  (new->SYMBOL).lmodule = copy_obj_careful(obj->SYMBOL.lmodule);
	  (new->SYMBOL).gvalue = copy_obj_careful(obj->SYMBOL.gvalue);
	  (new->SYMBOL).left = copy_obj_careful(obj->SYMBOL.left);
	  (new->SYMBOL).right = copy_obj_careful(obj->SYMBOL.right);
	  (new->SYMBOL).hash = (obj->SYMBOL.hash);
	  break;

	case TYPE_STREAM:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct stream_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new) = copy_obj_careful(class);
	  (new->STREAM).handle = obj->STREAM.handle;
	  (new->STREAM).name = copy_obj_careful(obj->STREAM.name);
	  (new->STREAM).mode = obj->STREAM.mode;
	  (new->STREAM).curchar = new->STREAM.curchar;
	  break;
	  
	case TYPE_C_MODULE: /* These are statically allocated, so just mark */
	  /* forward to here -- unset fwd bit+ set right space */
	  gcof(obj)=wspace; new=obj;
	  class=lval_classof(obj);
	  lval_classof(obj)=copy_obj_careful(class);
	  obj->C_MODULE.name=copy_obj_careful(obj->C_MODULE.name);
	  obj->C_MODULE.home=copy_obj_careful(obj->C_MODULE.home);
	  obj->C_MODULE.imported_modules=copy_obj_careful(obj->C_MODULE.imported_modules);
	  obj->C_MODULE.exported_names=copy_obj_careful(obj->C_MODULE.exported_names);
	  obj->C_MODULE.bindings=copy_obj_careful(obj->C_MODULE.bindings);
	  obj->C_MODULE.entry_count=copy_obj_careful(obj->C_MODULE.entry_count);
	  obj->C_MODULE.values=copy_obj_careful(obj->C_MODULE.values);

	  break;

	case TYPE_I_MODULE:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct i_module_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)= copy_obj_careful(class);
	  new->I_MODULE.name= copy_obj_careful(obj->I_MODULE.name);
	  new->I_MODULE.home= copy_obj_careful(obj->I_MODULE.home);
	  new->I_MODULE.imported_modules= copy_obj_careful(obj->I_MODULE.imported_modules);
	  new->I_MODULE.exported_names= copy_obj_careful(obj->I_MODULE.exported_names);
	  new->I_MODULE.bindings= copy_obj_careful(obj->I_MODULE.bindings);
	  new->I_MODULE.bounce_flag= obj->I_MODULE.bounce_flag;
	  break;

	case TYPE_C_FUNCTION:
	case TYPE_C_MACRO:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct c_function_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new) = copy_obj_careful(class);
	  new->C_FUNCTION.name = copy_obj_careful(obj->C_FUNCTION.name);
	  new->C_FUNCTION.home = copy_obj_careful(obj->C_FUNCTION.home);
	  new->C_FUNCTION.setter = copy_obj_careful(obj->C_FUNCTION.setter);
	  new->C_FUNCTION.env = copy_obj_careful(obj->C_FUNCTION.env);
	  new->C_FUNCTION.argtype = obj->C_FUNCTION.argtype;
	  new->C_FUNCTION.func=obj->C_FUNCTION.func;
	  break;
	  
	  
	case TYPE_I_FUNCTION:	
	case TYPE_I_MACRO:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct i_function_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  new->I_FUNCTION.name=copy_obj_careful(obj->I_FUNCTION.name);
	  new->I_FUNCTION.home=copy_obj_careful(obj->I_FUNCTION.home);
	  new->I_FUNCTION.env=copy_obj_careful(obj->I_FUNCTION.env);
	  new->I_FUNCTION.bvl=copy_obj_careful(obj->I_FUNCTION.bvl);
	  new->I_FUNCTION.body=copy_obj_careful(obj->I_FUNCTION.body);
	  new->I_FUNCTION.argtype=obj->I_FUNCTION.argtype;
	  break;

	case TYPE_FLOAT:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct float_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  new->FLOAT.fvalue=obj->FLOAT.fvalue;
	  break;
#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))
	case TYPE_LISTENER:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct listener_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  bcopy(&(obj->LISTENER.socket),&(new->LISTENER.socket),sizeof(new->LISTENER.socket));
	  bcopy(&(obj->LISTENER.name),&(new->LISTENER.name),sizeof(new->LISTENER.name));
	  bcopy(&(obj->LISTENER.state),&(new->LISTENER.state),sizeof(new->LISTENER.state));
	  break;
#endif
	case TYPE_SOCKET:
	  COPY_ALLOC_SPACE(free_ptr,sizeof(struct socket_structure));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new)=copy_obj_careful(class);
	  bcopy(&(obj->SOCKET.socket),&(new->SOCKET.socket),sizeof(new->SOCKET.socket));
	  bcopy(&(obj->SOCKET.name),&(new->SOCKET.name),sizeof(new->SOCKET.name));
	  bcopy(&(obj->SOCKET.state),&(new->SOCKET.state),sizeof(new->SOCKET.state));
	  bcopy((obj->SOCKET.buffer),(new->SOCKET.buffer),sizeof(new->SOCKET.buffer));
	  break;

	case TYPE_THREAD:
	  i=intval(classof(obj)->CLASS.local_count);
	  COPY_ALLOC_SPACE(free_ptr, sizeof(Object_t)+ i*sizeof(LispObject));
	  FORWARD_HEADER(new,obj);
	  lval_classof(new) = copy_obj_careful(class);
	  new->THREAD.stack_size = obj->THREAD.stack_size;
	  new->THREAD.gc_stack_size = obj->THREAD.gc_stack_size; 

	  new->THREAD.fun = copy_obj_careful(obj->THREAD.fun);
	  new->THREAD.args = copy_obj_careful(obj->THREAD.args);
	  new->THREAD.value = copy_obj_careful(obj->THREAD.value);

	  new->THREAD.status = obj->THREAD.status;

	  new->THREAD.parent = copy_obj_careful(obj->THREAD.parent);
	  new->THREAD.cochain = copy_obj_careful(obj->THREAD.cochain);
  
	  new->THREAD.state = copy_obj_careful(obj->THREAD.state);
    
	  new->THREAD.stack_base = obj->THREAD.stack_base;
	  new->THREAD.gc_stack_base = obj->THREAD.gc_stack_base;
	  for (i=N_SLOTS_IN_THREAD ; i<intval(class->CLASS.local_count) ; i++)
	    slotref(new,i) = copy_obj_careful(slotref(obj,i));
	  /* hack */
	  if (obj->THREAD.gc_stack_base+obj->THREAD.gc_stack_size < obj->THREAD.state->CONTINUE.gc_stack_pointer)
	    fprintf(stderr,"GC Stack overflow detected\n");

	  { 		
	    LispObject *x=obj->THREAD.gc_stack_base;
	    
	    while (x<obj->THREAD.state->CONTINUE.gc_stack_pointer)
	      { 
		if (!(((int) *x)&1)) /* Check for tags here */
		  *x = copy_obj_careful(*x);
		++x;
	      }
	  }
	  break;
	  
	case TYPE_WEAK_WRAPPER:
	  COPY_ALLOC_SPACE(free_ptr, WEAK_PTR_SIZE*sizeof(LispObject)+sizeof(Object_t));
	  FORWARD_HEADER(new,obj);	
	  lval_classof(new) = copy_obj_careful(class);  
	  weak_ptr_chain(new)=S_G_V(weak_list);
	  weak_ptr_val(new)=weak_ptr_val(obj);
	  S_G_V(weak_list)=new;
	  break;

	default:
	  fprintf(stderr,"Can't copy: %x\n",typeof(obj));
	  return obj;
	  break;
	}
      return new;
    }
}
#endif
