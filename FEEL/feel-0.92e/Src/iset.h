/* Numbers for instruction set. 
   Used by instruct.em and bytecodes.h
   */

#define BC_NOP 0
	  
	  /* Globals  etc */
#define BC_PUSH_GLOBAL 1 
#define BC_SET_GLOBAL 2

#define BC_PUSH_SPECIAL 3
#define BC_PUSH_STATIC  4
#define BC_PUSH_FIXNUM 5
#define BC_PUSH_SMALL_FIXNUM 6
#define BC_SET_STATIC  7

	  /* stack refs */
#define BC_PUSH_NTH 8
#define BC_PUSH_NTH_0	9 
#define BC_PUSH_NTH_1   10
#define BC_PUSH_NTH_2	11
#define BC_PUSH_NTH_3	12
#define BC_SET_NTH 	13
	  
	  /* Stack abuse */
#define BC_SLIDE_STACK 		14
#define BC_SLIDE_1 		15
#define BC_SWAP        	       	16
#define BC_DROP 		17
#define BC_DROP_1 		18
	  
	  /* env reference */
#define BC_ENV_REF 	19
#define BC_SET_ENV 	20
#define BC_POP_ENV 	21
#define BC_MAKE_ENV 	22

	  /* object reference */
#define BC_VREF 	23
#define BC_SET_VREF 	24

#define BC_SLOT_REF 	25
#define BC_SLOT_REF_0	26
#define BC_SLOT_REF_1	27
#define BC_SET_SLOT 	28
#define BC_SET_SLOT_1 	29
#define BC_SET_TYPE 	30

	  /* Leaping merrily */
#define BC_BRANCH 	31
#define BC_BRANCH_NIL 	32

	  /* Calling things */
#define BC_APPLY_ARGS		63
#define BC_APPLY_ANY 		33
#define BC_APPLY_BVF 		34
#define BC_APPLY_METHODS 	35
#define BC_PUSH_LABEL 		36
#define BC_APPLY_CFN		60	  
#define BC_APPLY_METHOD_LIST	61
#define BC_APPLY_CFN2		62
	  /* and return */
#define BC_RETURN 	37

#define BC_EXIT 	38

/* allocation */	
#define BC_CONS 		39
#define BC_ALLOC_CLOSURE 	40
#define BC_PUSH_INT 		41
#define BC_ALLOC_EXT_CLOSURE 	51 

/* Tests */
#define BC_NULLP 42
#define BC_EQP 	43
#define BC_CONSP 44
#define BC_ARG_CHECK 45


/* Reflection */
#define BC_CONTEXT	46
#define BC_ENSURE_STACK 47

/* Useful functions */
#define BC_ASSQ 48
#define BC_MEMQ 49
#define BC_SCANQ 50
/* Inline method list */

/* Arithmetic */
#define METHOD_INT_ADD 		0
#define METHOD_INT_DIFF		1
#define METHOD_INT_MULT		2
#define METHOD_INT_DIV		3
#define METHOD_INT_EQUAL	4

#define METHOD_SYMBOL_EQUAL	5

/* streams */
#define METHOD_STREAM_STRING_WRITE 6
#define METHOD_STREAM_READ 	7

/* slots */
#define METHOD_SLOT_REF_0 	8
#define METHOD_SLOT_REF_1 	9
#define METHOD_SLOT_REF_2 	10
#define METHOD_SLOT_REF_3 	11

#define METHOD_SLOT_SET_0 	12
#define METHOD_SLOT_SET_1 	13
#define METHOD_SLOT_SET_2 	14
#define METHOD_SLOT_SET_3 	15



