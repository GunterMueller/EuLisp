  # -S output
	.text
	.set	noat
	.set	noreorder
	.globl	stack_switch_and_go
stack_switch_and_go:
	move	$t7,$a1
	move    $sp,$a0
	j	$t7
	nop
	nop
	nop
	.data
	.ident
	.word	0x7468696e  #   1952999790     t   h   i   n 
	.word	0x672e633a  #   1731093306     g   .   c   : 
	.word	0x61373535  #   1631008053     a   7   5   5 
	.word	0x61626662  #   1633838690     a   b   f   b 
	.word	0x0a000000  #    167772160    nl  nul nul nul
	.word	0
  # end of assembler output
