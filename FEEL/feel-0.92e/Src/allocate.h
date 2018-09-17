/* ******************************************************************** */
/*  allocate.h       Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Space allocation			                                */
/* ******************************************************************** */

/*
 * $Id: allocate.h,v 1.1 1994/01/25 13:45:36 djb Exp $
 *
 * $Log: allocate.h,v $
 * Revision 1.1  1994/01/25  13:45:36  djb
 * Initial revision
 *
 * Revision 1.6  1992/11/25  16:39:35  pab
 * Added static chars
 *
 * Revision 1.5  1992/06/09  13:47:32  pab
 * fixed includes
 *
 * Revision 1.4  1992/05/28  11:18:38  pab
 * more defines
 *
 * Revision 1.5  1992/02/10  12:13:38  pab
 * removed redundant code
 *
 * Revision 1.4  1992/01/17  22:27:39  pab
 * Added alloc_nconses
 *
 * Revision 1.2  1991/09/11  12:06:58  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:23  pab
 * Initial revision
 *
 * Revision 1.5  1991/02/14  02:13:54  kjp
 * Redid header.
 *
 */

#define ALIGN_SIZE(s) \
         ((s)+((s)%BYTE_ALIGNMENT == 0 \
           ? 0 \
           : BYTE_ALIGNMENT-(s)%BYTE_ALIGNMENT))

extern int pagesize;
/* A sick method to catch GC bugs... */
#define HUNK_PAGE_SIZE() 512
/** (pagesize= (pagesize+64)%1221 + 1) **/
/* Allocator function signatures... */

extern LispObject allocate_module_function(LispObject*,LispObject,LispObject,
				    LispObject (*)(LispObject*),int);
extern LispObject allocate_i_module(LispObject*,LispObject);
extern LispObject allocate_i_function(LispObject*,LispObject,LispObject,int);
extern LispObject allocate_buffered_continue(void);
extern LispObject allocate_special(LispObject*,LispObject,LispObject (*)());
extern LispObject allocate_generic(LispObject,int);

extern LispObject allocate_semaphore(LispObject*);
extern LispObject allocate_thread(LispObject*,int,int, int);
extern LispObject allocate_n_conses(LispObject *, int);

#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))

extern LispObject allocate_listener(LispObject*);
extern LispObject allocate_socket(LispObject*);

#endif

extern LispObject allocate_c_object(LispObject*, int,int);

/* Initialiser... */

extern void runtime_initialise_allocator(LispObject*);
extern void runtime_reset_allocator(LispObject *);

extern char *allocate_page(LispObject*,int);
extern void deallocate_page(LispObject*,char *,int);

extern char *allocate_space(LispObject*,int);
extern void deallocate_space(LispObject*,char *,int);
extern void promote_free_space(LispObject*);

extern char *allocate_stack(LispObject*,int);
extern void free_stack(LispObject*,char *,int);

extern void allocate_static_integers(LispObject*);
extern void allocate_static_chars(LispObject *);
