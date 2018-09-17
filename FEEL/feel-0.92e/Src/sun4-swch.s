gcc_compiled.:
.text
	.align 4
.global _stack_switch_and_go
	.proc 1
_stack_switch_and_go:
	ta 0x3
	mov %o0, %sp
	mov %sp, %fp
	jmp %o1
	restore
