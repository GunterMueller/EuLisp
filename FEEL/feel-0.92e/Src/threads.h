/*
 * $Id: threads.h,v 1.1 1994/01/25 13:45:36 djb Exp $
 *
 * $Log: threads.h,v $
 * Revision 1.1  1994/01/25  13:45:36  djb
 * Initial revision
 *
 * Revision 1.5  1992/06/18  11:42:56  pab
 * added decl
 *
 * Revision 1.4  1992/01/21  22:56:35  pab
 * made allocation a little more generous
 *
 * Revision 1.3  1992/01/15  21:23:52  pab
 * Fixed alignment problems; made threads allocate int arrays
 *
 * Revision 1.2  1991/09/11  12:07:50  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:10  pab
 * Initial revision
 *
 * Revision 1.3  1991/02/28  14:16:07  kjp
 * Funtion proto-updates.
 *
 * Revision 1.2  1991/02/13  18:26:46  kjp
 * Pass.
 *
 */

#ifndef THREADS_H
#define THREADS_H

#define MY_THREAD_STACK_SIZE (1024*8)
#define MY_THREAD_GC_STACK_SIZE (1024*4)

extern LispObject Fn_make_thread(LispObject*);
extern LispObject Fn_thread_reschedule(LispObject*);
extern LispObject Fn_thread_start(LispObject*);
extern LispObject Fn_thread_value(LispObject*);
extern LispObject Fn_abort_thread(LispObject*);

void call_thread_signal(LispObject *,LispObject, int);

extern void runtime_begin_processes(LispObject *);

#define thread_stack_size(thread) (((ThreadData) stringof(thread->THREAD.sysdata))->stack_size)
#define thread_stack_base(thread) (((ThreadData) stringof(thread->THREAD.sysdata))->stack_base)
#define thread_gc_stack_size(thread) (((ThreadData) stringof(thread->THREAD.sysdata))->gc_stack_size)
#define thread_gc_stack_base(thread) (((ThreadData) stringof(thread->THREAD.sysdata))->gc_stack_base)
#define thread_status(thread) (((volatile ThreadData) stringof(thread->THREAD.sysdata))->status)
#define thread_signalled(thread) (((volatile ThreadData) stringof(thread->THREAD.sysdata))->signalled)

extern LispObject interpreter_thread;

#endif


