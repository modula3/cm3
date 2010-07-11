	.text
.globl _return_variable__ret_vu16
	.private_extern _return_variable__ret_vu16
_return_variable__ret_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_return_variable(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _return_variable__ret_vu64
	.private_extern _return_variable__ret_vu64
_return_variable__ret_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_return_variable(%rip), %rax
	leave
	ret
.globl _return_variable__ret_vLC
	.private_extern _return_variable__ret_vLC
_return_variable__ret_vLC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_return_variable(%rip), %rax
	leave
	ret
.globl _return_variable__ret_vf64
	.private_extern _return_variable__ret_vf64
_return_variable__ret_vf64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_return_variable(%rip), %xmm0
	leave
	ret
.globl _return_variable__ret_vi16
	.private_extern _return_variable__ret_vi16
_return_variable__ret_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_return_variable(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _return_variable__ret_vi32
	.private_extern _return_variable__ret_vi32
_return_variable__ret_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_return_variable(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _return_variable__ret_vI
	.private_extern _return_variable__ret_vI
_return_variable__ret_vI:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_return_variable(%rip), %rax
	leave
	ret
.globl _return_variable__ret_vi64
	.private_extern _return_variable__ret_vi64
_return_variable__ret_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_return_variable(%rip), %rax
	leave
	ret
.globl _return_variable__ret_vC
	.private_extern _return_variable__ret_vC
_return_variable__ret_vC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_return_variable(%rip), %rax
	leave
	ret
.globl _return_variable__ret_vf32
	.private_extern _return_variable__ret_vf32
_return_variable__ret_vf32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_return_variable(%rip), %xmm0
	leave
	ret
.globl _return_variable__ret_vu8
	.private_extern _return_variable__ret_vu8
_return_variable__ret_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_return_variable(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _return_variable__ret_vi8
	.private_extern _return_variable__ret_vi8
_return_variable__ret_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_return_variable(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _return_variable__ret_vL
	.private_extern _return_variable__ret_vL
_return_variable__ret_vL:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_return_variable(%rip), %rax
	leave
	ret
.globl _return_variable__ret_vu32
	.private_extern _return_variable__ret_vu32
_return_variable__ret_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_return_variable(%rip), %eax
	cltq
	leave
	ret
.globl _return_variable_M3
_return_variable_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_return_variable(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "return_variable_M3"
	.space 1
	.ascii "ret_vu32"
	.space 1
	.ascii "ret_vL"
	.space 1
	.ascii "ret_vi8"
	.space 1
	.ascii "ret_vu8"
	.space 1
	.ascii "ret_vf32"
	.space 1
	.ascii "ret_vC"
	.space 1
	.ascii "ret_vi64"
	.space 1
	.ascii "ret_vI"
	.space 1
	.ascii "ret_vi32"
	.space 1
	.ascii "ret_vi16"
	.space 1
	.ascii "ret_vf64"
	.space 1
	.ascii "ret_vLC"
	.space 1
	.ascii "ret_vu64"
	.space 1
	.ascii "ret_vu16"
	.space 1
	.quad	_return_variable_M3
	.quad	_L_1
	.quad	_return_variable__ret_vu32
	.quad	_L_1+19
	.quad	_return_variable__ret_vL
	.quad	_L_1+28
	.quad	_return_variable__ret_vi8
	.quad	_L_1+35
	.quad	_return_variable__ret_vu8
	.quad	_L_1+43
	.quad	_return_variable__ret_vf32
	.quad	_L_1+51
	.quad	_return_variable__ret_vC
	.quad	_L_1+60
	.quad	_return_variable__ret_vi64
	.quad	_L_1+67
	.quad	_return_variable__ret_vI
	.quad	_L_1+76
	.quad	_return_variable__ret_vi32
	.quad	_L_1+83
	.quad	_return_variable__ret_vi16
	.quad	_L_1+92
	.quad	_return_variable__ret_vf64
	.quad	_L_1+101
	.quad	_return_variable__ret_vLC
	.quad	_L_1+110
	.quad	_return_variable__ret_vu64
	.quad	_L_1+118
	.quad	_return_variable__ret_vu16
	.quad	_L_1+127
	.space 8
	.ascii "../AMD64_DARWIN/return_variable.m3"
	.space 6
	.data
	.align 5
_MM_return_variable:
	.quad	_L_1+384
	.space 32
	.quad	_L_1+136
	.space 24
	.quad	_MM_return_variable+208
	.space 8
	.quad	_return_variable_M3
	.quad	3
	.word	49
	.space 6
	.quad	50
	.quad	51
	.long	171798692
	.long	1078608855
	.word	54
	.space 2
	.long	55
	.quad	56
	.quad	57
	.quad	58
	.long	1114531430
	.byte	61
	.byte	62
	.space 2
	.quad	63
	.long	64
	.space 28
	.quad	_return_variable_I3
	.quad	_MM_return_variable+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_return_variable+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_return_variable+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_return_variable+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
