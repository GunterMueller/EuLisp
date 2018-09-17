	.verstamp	2 40
	.extern	value 4
	.text	
	.align	2
	.file	2 "swag.c"
	.globl	stack_switch_and_go
	.loc	2 4
	.set noreorder
 #   1	extern int value;
 #   2	
 #   3	void stack_switch_and_go(int stack,int fun)
 #   4	{
	.ent	stack_switch_and_go 2
stack_switch_and_go:
	.option	O1
	move $sp,$4
	jal $5
 #   7	}
	nop
	nop
	nop
	.end	stack_switch_and_go
