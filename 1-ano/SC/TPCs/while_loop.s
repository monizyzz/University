	.file	"while_loop.c"
	.text
	.p2align 4
	.globl	while_loop
	.type	while_loop, @function
while_loop:
.LFB0:
	.cfi_startproc
	endbr64
	.p2align 4,,10
	.p2align 3
.L2:
	imull	%edx, %esi
	addl	%edx, %edi
	subl	$1, %edx
	testl	%edx, %edx
	jg	.L2
	leal	(%rdi,%rsi), %eax
	ret
	.cfi_endproc
.LFE0:
	.size	while_loop, .-while_loop
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	endbr64
	movl	$22, %eax
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 11.3.0-1ubuntu1~22.04) 11.3.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
