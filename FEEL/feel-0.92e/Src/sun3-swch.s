#NO_APP
gcc_compiled.:
.text
	.even
.globl _stack_switch_and_go
_stack_switch_and_go:
	link a6,#0
	movel a6@(8),d0
	movel a6@(12),a0
L1:
	unlk a6
	movl d0,sp
	jmp a0@

