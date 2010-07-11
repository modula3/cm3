	.text
.globl _return_parameter__ret_pi64
	.private_extern _return_parameter__ret_pi64
_return_parameter__ret_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _return_parameter__ret_pLC
	.private_extern _return_parameter__ret_pLC
_return_parameter__ret_pLC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _return_parameter__ret_pf32
	.private_extern _return_parameter__ret_pf32
_return_parameter__ret_pf32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	-20(%rbp), %xmm0
	leave
	ret
.globl _return_parameter__ret_pu8
	.private_extern _return_parameter__ret_pu8
_return_parameter__ret_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _return_parameter__ret_pI
	.private_extern _return_parameter__ret_pI
_return_parameter__ret_pI:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _return_parameter__ret_pi8
	.private_extern _return_parameter__ret_pi8
_return_parameter__ret_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _return_parameter__ret_pu32
	.private_extern _return_parameter__ret_pu32
_return_parameter__ret_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _return_parameter__ret_pC
	.private_extern _return_parameter__ret_pC
_return_parameter__ret_pC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _return_parameter__ret_pu16
	.private_extern _return_parameter__ret_pu16
_return_parameter__ret_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _return_parameter__ret_pu64
	.private_extern _return_parameter__ret_pu64
_return_parameter__ret_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _return_parameter__ret_pf64
	.private_extern _return_parameter__ret_pf64
_return_parameter__ret_pf64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	-24(%rbp), %xmm0
	leave
	ret
.globl _return_parameter__ret_pL
	.private_extern _return_parameter__ret_pL
_return_parameter__ret_pL:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _return_parameter__ret_pi16
	.private_extern _return_parameter__ret_pi16
_return_parameter__ret_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _return_parameter__ret_pi32
	.private_extern _return_parameter__ret_pi32
_return_parameter__ret_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _return_parameter_M3
_return_parameter_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_return_parameter(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "return_parameter_M3"
	.space 1
	.ascii "ret_pi32"
	.space 1
	.ascii "ret_pi16"
	.space 1
	.ascii "ret_pL"
	.space 1
	.ascii "ret_pf64"
	.space 1
	.ascii "ret_pu64"
	.space 1
	.ascii "ret_pu16"
	.space 1
	.ascii "ret_pC"
	.space 1
	.ascii "ret_pu32"
	.space 1
	.ascii "ret_pi8"
	.space 1
	.ascii "ret_pI"
	.space 1
	.ascii "ret_pu8"
	.space 1
	.ascii "ret_pf32"
	.space 1
	.ascii "ret_pLC"
	.space 1
	.ascii "ret_pi64"
	.space 8
	.quad	_return_parameter_M3
	.quad	_L_1
	.quad	_return_parameter__ret_pi32
	.quad	_L_1+20
	.quad	_return_parameter__ret_pi16
	.quad	_L_1+29
	.quad	_return_parameter__ret_pL
	.quad	_L_1+38
	.quad	_return_parameter__ret_pf64
	.quad	_L_1+45
	.quad	_return_parameter__ret_pu64
	.quad	_L_1+54
	.quad	_return_parameter__ret_pu16
	.quad	_L_1+63
	.quad	_return_parameter__ret_pC
	.quad	_L_1+72
	.quad	_return_parameter__ret_pu32
	.quad	_L_1+79
	.quad	_return_parameter__ret_pi8
	.quad	_L_1+88
	.quad	_return_parameter__ret_pI
	.quad	_L_1+96
	.quad	_return_parameter__ret_pu8
	.quad	_L_1+103
	.quad	_return_parameter__ret_pf32
	.quad	_L_1+111
	.quad	_return_parameter__ret_pLC
	.quad	_L_1+120
	.quad	_return_parameter__ret_pi64
	.quad	_L_1+128
	.space 8
	.ascii "../AMD64_DARWIN/return_parameter.m3"
	.space 5
	.data
	.align 5
_MM_return_parameter:
	.quad	_L_1+392
	.space 32
	.quad	_L_1+144
	.space 24
	.quad	_MM_return_parameter+208
	.space 8
	.quad	_return_parameter_M3
	.quad	3
	.quad	81
	.quad	82
	.long	1118285332
	.byte	85
	.space 3
	.quad	86
	.byte	87
	.space 3
	.long	88
	.quad	89
	.word	90
	.space 6
	.quad	91
	.long	515396076
	.long	1079458693
	.quad	94
	.word	95
	.space 2
	.long	96
	.space 24
	.quad	_return_parameter_I3
	.quad	_MM_return_parameter+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_return_parameter+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_return_parameter+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_return_parameter+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
