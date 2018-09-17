/* interface for copier */

int current_space(void);
LispObject allocate_nbytes(LispObject *,int size,int type);
int add_root(volatile LispObject *);

#define S_G_V(x) SYSTEM_GLOBAL_VALUE(x)


/* INTERNAL DEFINITIONS */
#ifdef PAGE_SIZE     /* clash on rs6000 */
#undef PAGE_SIZE
#endif
#define PAGE_SIZE (32*1024)
extern SYSTEM_GLOBAL(SystemSemaphore,GC_sem);

#ifdef NODEBUG
#define COPY_BUG(x) 
#else
#define COPY_BUG(x) x
#endif

typedef struct page_list_struct
{
  struct page_list_struct *next;
  int status;
  char *end;
  int id;
  char start[sizeof(int)];
} *PageList , PageEnt;

#define PAGE_FREE 0
#define PAGE_USED(space) 1+(space)

#define RIG_GC_THREAD(stacktop) \
{ \
  \
  (void) system_thread_rig(stacktop,(GC_thread)); \
    { LispObject* newstack; \
\
      newstack = load_thread(GC_thread); \
      COPY_BUG(fprintf(stderr,"{GC_THREAD: %x}",newstack)); \
      if (set_continue(stacktop,GC_thread->THREAD.state)) \
	{ \
	   void first_gc_mark_call(LispObject *); \
	   first_gc_mark_call(stacktop); \
	} 	\
    } \
}

#define OBJ_SIZE(type) (sizeof(type)-sizeof(Object_t))
#define OBJ_CSIZE(type,firstslot) 	   \
   ( (char *) (&(((type *) 0)->firstslot)) \
		- (char *)sizeof(Object_t))

typedef struct obj_info_t
{
  int size;
  int csize;
} ClassInfo;

