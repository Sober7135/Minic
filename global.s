	.text
	.file	"global.c"
	.globl	test                            # -- Begin function test
	.p2align	4, 0x90
	.type	test,@function
test:                                   # @test
# %bb.0:                                # %entry
	movl	%edi, -4(%rsp)
	movl	$5, -8(%rsp)
	movl	-8(%rsp), %eax
	addl	-4(%rsp), %eax
	retq
.Lfunc_end0:
	.size	test, .Lfunc_end0-test
                                        # -- End function
	.type	x,@object                       # @x
	.data
	.globl	x
	.p2align	2, 0x0
x:
	.long	5                               # 0x5
	.size	x, 4

	.ident	"clang version 17.0.6"
	.section	".note.GNU-stack","",@progbits
