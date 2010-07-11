	.text
.globl _return_constant__ret_ku16
	.private_extern _return_constant__ret_ku16
_return_constant__ret_ku16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _return_constant__ret_ku64
	.private_extern _return_constant__ret_ku64
_return_constant__ret_ku64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2, %eax
	leave
	ret
.globl _return_constant__ret_kLC
	.private_extern _return_constant__ret_kLC
_return_constant__ret_kLC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$3, %eax
	leave
	ret
.globl _return_constant__ret_kf64
	.private_extern _return_constant__ret_kf64
_return_constant__ret_kf64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	LC0(%rip), %xmm0
	leave
	ret
.globl _return_constant__ret_ki16
	.private_extern _return_constant__ret_ki16
_return_constant__ret_ki16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$6, %eax
	leave
	ret
.globl _return_constant__ret_ki32
	.private_extern _return_constant__ret_ki32
_return_constant__ret_ki32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$7, %eax
	leave
	ret
.globl _return_constant__ret_kI
	.private_extern _return_constant__ret_kI
_return_constant__ret_kI:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$8, %eax
	leave
	ret
.globl _return_constant__ret_ki64
	.private_extern _return_constant__ret_ki64
_return_constant__ret_ki64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$9, %eax
	leave
	ret
.globl _return_constant__ret_kC
	.private_extern _return_constant__ret_kC
_return_constant__ret_kC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$10, %eax
	leave
	ret
.globl _return_constant__ret_kf32
	.private_extern _return_constant__ret_kf32
_return_constant__ret_kf32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	LC1(%rip), %xmm0
	leave
	ret
.globl _return_constant__ret_ku8
	.private_extern _return_constant__ret_ku8
_return_constant__ret_ku8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$13, %eax
	leave
	ret
.globl _return_constant__ret_ki8
	.private_extern _return_constant__ret_ki8
_return_constant__ret_ki8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$14, %eax
	leave
	ret
.globl _return_constant__ret_kL
	.private_extern _return_constant__ret_kL
_return_constant__ret_kL:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$15, %eax
	leave
	ret
.globl _return_constant__ret_ku32
	.private_extern _return_constant__ret_ku32
_return_constant__ret_ku32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$16, %eax
	leave
	ret
.globl _return_constant_M3
_return_constant_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_return_constant(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "return_constant_M3"
	.space 1
	.ascii "ret_ku32"
	.space 1
	.ascii "ret_kL"
	.space 1
	.ascii "ret_ki8"
	.space 1
	.ascii "ret_ku8"
	.space 1
	.ascii "ret_kf32"
	.space 1
	.ascii "ret_kC"
	.space 1
	.ascii "ret_ki64"
	.space 1
	.ascii "ret_kI"
	.space 1
	.ascii "ret_ki32"
	.space 1
	.ascii "ret_ki16"
	.space 1
	.ascii "ret_kf64"
	.space 1
	.ascii "ret_kLC"
	.space 1
	.ascii "ret_ku64"
	.space 1
	.ascii "ret_ku16"
	.space 1
	.quad	_return_constant_M3
	.quad	_L_1
	.quad	_return_constant__ret_ku32
	.quad	_L_1+19
	.quad	_return_constant__ret_kL
	.quad	_L_1+28
	.quad	_return_constant__ret_ki8
	.quad	_L_1+35
	.quad	_return_constant__ret_ku8
	.quad	_L_1+43
	.quad	_return_constant__ret_kf32
	.quad	_L_1+51
	.quad	_return_constant__ret_kC
	.quad	_L_1+60
	.quad	_return_constant__ret_ki64
	.quad	_L_1+67
	.quad	_return_constant__ret_kI
	.quad	_L_1+76
	.quad	_return_constant__ret_ki32
	.quad	_L_1+83
	.quad	_return_constant__ret_ki16
	.quad	_L_1+92
	.quad	_return_constant__ret_kf64
	.quad	_L_1+101
	.quad	_return_constant__ret_kLC
	.quad	_L_1+110
	.quad	_return_constant__ret_ku64
	.quad	_L_1+118
	.quad	_return_constant__ret_ku16
	.quad	_L_1+127
	.space 8
	.ascii "../AMD64_DARWIN/return_constant.m3"
	.space 6
	.data
	.align 5
_MM_return_constant:
	.quad	_L_1+384
	.space 32
	.quad	_L_1+136
	.space 24
	.quad	_MM_return_constant+208
	.space 8
	.quad	_return_constant_M3
	.quad	3
	.word	17
	.space 6
	.quad	18
	.quad	19
	.long	2405181686
	.long	1077163458
	.word	22
	.space 2
	.long	23
	.quad	24
	.quad	25
	.quad	26
	.long	1104821617
	.byte	29
	.byte	30
	.space 2
	.quad	31
	.long	32
	.space 28
	.quad	_return_constant_I3
	.quad	_MM_return_constant+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_return_constant+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_return_constant+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_return_constant+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.literal8
	.align 3
LC0:
	.long	0
	.long	1074921472
	.literal4
	.align 2
LC1:
	.long	1093790597
	.subsections_via_symbols
