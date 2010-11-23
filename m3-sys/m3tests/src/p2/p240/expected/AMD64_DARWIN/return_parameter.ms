	.text
.no_dead_strip _return_parameter__ret_pu16
_return_parameter__ret_pu16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movw	%di, -2(%rbp)
	movzwl	-2(%rbp), %eax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _return_parameter__ret_pu32
_return_parameter__ret_pu32:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	leave
LCFI5:
	ret
LFE1:
.no_dead_strip _return_parameter__ret_pLC
_return_parameter__ret_pLC:
LFB2:
	pushq	%rbp
LCFI6:
	movq	%rsp, %rbp
LCFI7:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI8:
	ret
LFE2:
.no_dead_strip _return_parameter__ret_pI
_return_parameter__ret_pI:
LFB3:
	pushq	%rbp
LCFI9:
	movq	%rsp, %rbp
LCFI10:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI11:
	ret
LFE3:
.no_dead_strip _return_parameter__ret_pi16
_return_parameter__ret_pi16:
LFB4:
	pushq	%rbp
LCFI12:
	movq	%rsp, %rbp
LCFI13:
	movw	%di, -2(%rbp)
	movzwl	-2(%rbp), %eax
	leave
LCFI14:
	ret
LFE4:
.no_dead_strip _return_parameter__ret_pu64
_return_parameter__ret_pu64:
LFB5:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI17:
	ret
LFE5:
.no_dead_strip _return_parameter__ret_pf64
_return_parameter__ret_pf64:
LFB6:
	pushq	%rbp
LCFI18:
	movq	%rsp, %rbp
LCFI19:
	movsd	%xmm0, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, -24(%rbp)
	movsd	-24(%rbp), %xmm0
	leave
LCFI20:
	ret
LFE6:
.no_dead_strip _return_parameter__ret_pi32
_return_parameter__ret_pi32:
LFB7:
	pushq	%rbp
LCFI21:
	movq	%rsp, %rbp
LCFI22:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	leave
LCFI23:
	ret
LFE7:
.no_dead_strip _return_parameter__ret_pC
_return_parameter__ret_pC:
LFB8:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI26:
	ret
LFE8:
.no_dead_strip _return_parameter__ret_pu8
_return_parameter__ret_pu8:
LFB9:
	pushq	%rbp
LCFI27:
	movq	%rsp, %rbp
LCFI28:
	movb	%dil, -1(%rbp)
	movzbl	-1(%rbp), %eax
	leave
LCFI29:
	ret
LFE9:
.no_dead_strip _return_parameter__ret_pi64
_return_parameter__ret_pi64:
LFB10:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI32:
	ret
LFE10:
.no_dead_strip _return_parameter__ret_pf32
_return_parameter__ret_pf32:
LFB11:
	pushq	%rbp
LCFI33:
	movq	%rsp, %rbp
LCFI34:
	movss	%xmm0, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, -20(%rbp)
	movss	-20(%rbp), %xmm0
	leave
LCFI35:
	ret
LFE11:
.no_dead_strip _return_parameter__ret_pL
_return_parameter__ret_pL:
LFB12:
	pushq	%rbp
LCFI36:
	movq	%rsp, %rbp
LCFI37:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI38:
	ret
LFE12:
.no_dead_strip _return_parameter__ret_pi8
_return_parameter__ret_pi8:
LFB13:
	pushq	%rbp
LCFI39:
	movq	%rsp, %rbp
LCFI40:
	movb	%dil, -1(%rbp)
	movzbl	-1(%rbp), %eax
	leave
LCFI41:
	ret
LFE13:
.globl _return_parameter_M3
.no_dead_strip _return_parameter_M3
_return_parameter_M3:
LFB14:
	pushq	%rbp
LCFI42:
	movq	%rsp, %rbp
LCFI43:
	movq	%rdi, -8(%rbp)
	leaq	_MM_return_parameter(%rip), %rax
	leave
LCFI44:
	ret
LFE14:
	.const_data
	.align 5
_L_1:
	.ascii "return_parameter_M3"
	.space 1
	.ascii "ret_pi8"
	.space 1
	.ascii "ret_pL"
	.space 1
	.ascii "ret_pf32"
	.space 1
	.ascii "ret_pi64"
	.space 1
	.ascii "ret_pu8"
	.space 1
	.ascii "ret_pC"
	.space 1
	.ascii "ret_pi32"
	.space 1
	.ascii "ret_pf64"
	.space 1
	.ascii "ret_pu64"
	.space 1
	.ascii "ret_pi16"
	.space 1
	.ascii "ret_pI"
	.space 1
	.ascii "ret_pLC"
	.space 1
	.ascii "ret_pu32"
	.space 1
	.ascii "ret_pu16"
	.space 8
	.quad	_return_parameter_M3
	.quad	_L_1
	.quad	_return_parameter__ret_pi8
	.quad	_L_1+20
	.quad	_return_parameter__ret_pL
	.quad	_L_1+28
	.quad	_return_parameter__ret_pf32
	.quad	_L_1+35
	.quad	_return_parameter__ret_pi64
	.quad	_L_1+44
	.quad	_return_parameter__ret_pu8
	.quad	_L_1+53
	.quad	_return_parameter__ret_pC
	.quad	_L_1+61
	.quad	_return_parameter__ret_pi32
	.quad	_L_1+68
	.quad	_return_parameter__ret_pf64
	.quad	_L_1+77
	.quad	_return_parameter__ret_pu64
	.quad	_L_1+86
	.quad	_return_parameter__ret_pi16
	.quad	_L_1+95
	.quad	_return_parameter__ret_pI
	.quad	_L_1+104
	.quad	_return_parameter__ret_pLC
	.quad	_L_1+111
	.quad	_return_parameter__ret_pu32
	.quad	_L_1+119
	.quad	_return_parameter__ret_pu16
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
	.quad	_MM_return_parameter+224
	.space 8
	.quad	_return_parameter_M3
	.quad	3
	.word	81
	.space 2
	.long	82
	.quad	83
	.quad	84
	.word	85
	.space 6
	.quad	86
	.long	-343597384
	.long	1079375953
	.long	89
	.space 4
	.quad	90
	.byte	91
	.space 7
	.quad	92
	.long	1119609160
	.space 4
	.quad	95
	.byte	96
	.space 31
	.quad	_return_parameter_I3
	.quad	_MM_return_parameter+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_return_parameter+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_return_parameter+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_return_parameter+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_return_parameter__ret_pu16.eh = 0
.no_dead_strip _return_parameter__ret_pu16.eh
_return_parameter__ret_pu32.eh = 0
.no_dead_strip _return_parameter__ret_pu32.eh
_return_parameter__ret_pLC.eh = 0
.no_dead_strip _return_parameter__ret_pLC.eh
_return_parameter__ret_pI.eh = 0
.no_dead_strip _return_parameter__ret_pI.eh
_return_parameter__ret_pi16.eh = 0
.no_dead_strip _return_parameter__ret_pi16.eh
_return_parameter__ret_pu64.eh = 0
.no_dead_strip _return_parameter__ret_pu64.eh
_return_parameter__ret_pf64.eh = 0
.no_dead_strip _return_parameter__ret_pf64.eh
_return_parameter__ret_pi32.eh = 0
.no_dead_strip _return_parameter__ret_pi32.eh
_return_parameter__ret_pC.eh = 0
.no_dead_strip _return_parameter__ret_pC.eh
_return_parameter__ret_pu8.eh = 0
.no_dead_strip _return_parameter__ret_pu8.eh
_return_parameter__ret_pi64.eh = 0
.no_dead_strip _return_parameter__ret_pi64.eh
_return_parameter__ret_pf32.eh = 0
.no_dead_strip _return_parameter__ret_pf32.eh
_return_parameter__ret_pL.eh = 0
.no_dead_strip _return_parameter__ret_pL.eh
_return_parameter__ret_pi8.eh = 0
.no_dead_strip _return_parameter__ret_pi8.eh
.globl _return_parameter_M3.eh
_return_parameter_M3.eh = 0
.no_dead_strip _return_parameter_M3.eh
	.subsections_via_symbols
