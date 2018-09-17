/* 
  * Header for weak ptr stuff 
  * defined in others.c
 */

#define weak_ptr_chain(o)	(o->CONS.cdr)
#define weak_ptr_val(o)		(o->CONS.car)

#define WEAK_PTR_SIZE 2
extern LispObject Cb_GC_hook;

