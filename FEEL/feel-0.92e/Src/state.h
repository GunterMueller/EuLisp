/* ******************************************************************** */
/* state.h           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Lisp state           		                                */
/* ******************************************************************** */

/* Fixed outside of a context switch... */

extern SYSTEM_THREAD_SPECIFIC_DECLARATION(int*,state_stack_base);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject*,state_gc_stack_base);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_current_thread);

#define STACK_BASE() (SYSTEM_THREAD_SPECIFIC_VALUE(state_stack_base))
#define GC_STACK_BASE() (SYSTEM_THREAD_SPECIFIC_VALUE(state_gc_stack_base))
#define CURRENT_THREAD() (SYSTEM_THREAD_SPECIFIC_VALUE(state_current_thread))

/* Forever wandering... */

extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject*,state_gc_stack_pointer);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_dynamic_env);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_last_continue);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_handler_stack);

#define GC_STACK_POINTER() (SYSTEM_THREAD_SPECIFIC_VALUE(state_gc_stack_pointer))
#define DYNAMIC_ENV()      (SYSTEM_THREAD_SPECIFIC_VALUE(state_dynamic_env))
#define LAST_CONTINUE()    (SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue))
#define HANDLER_STACK()    (SYSTEM_THREAD_SPECIFIC_VALUE(state_handler_stack))

extern int set_continue_1(LispObject *,LispObject);
extern int set_continue_2(LispObject);
extern void call_continue(LispObject *,LispObject,LispObject);
LispObject save_state(LispObject *,LispObject);
extern LispObject *load_thread(LispObject);

#define set_continue(stacktop,cont) \
  (set_continue_1(stacktop,cont), \
   (setjmp((cont)->CONTINUE.machine_state) \
    ? TRUE \
    : set_continue_2(cont)))

#define unset_continue(cont) \
  (cont->CONTINUE.live = FALSE, \
   SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue) \
     = cont->CONTINUE.last_continue, \
   cont->CONTINUE.last_continue = nil);


			     

