	.text
.no_dead_strip _return_variable__ret_vu16
_return_variable__ret_vu16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movzwl	104+_MM_return_variable(%rip), %eax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _return_variable__ret_vu32
_return_variable__ret_vu32:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	movl	108+_MM_return_variable(%rip), %eax
	leave
LCFI5:
	ret
LFE1:
.no_dead_strip _return_variable__ret_vLC
_return_variable__ret_vLC:
LFB2:
	pushq	%rbp
LCFI6:
	movq	%rsp, %rbp
LCFI7:
	movq	112+_MM_return_variable(%rip), %rax
	leave
LCFI8:
	ret
LFE2:
.no_dead_strip _return_variable__ret_vI
_return_variable__ret_vI:
LFB3:
	pushq	%rbp
LCFI9:
	movq	%rsp, %rbp
LCFI10:
	movq	120+_MM_return_variable(%rip), %rax
	leave
LCFI11:
	ret
LFE3:
.no_dead_strip _return_variable__ret_vi16
_return_variable__ret_vi16:
LFB4:
	pushq	%rbp
LCFI12:
	movq	%rsp, %rbp
LCFI13:
	movzwl	128+_MM_return_variable(%rip), %eax
	leave
LCFI14:
	ret
LFE4:
.no_dead_strip _return_variable__ret_vu64
_return_variable__ret_vu64:
LFB5:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	movq	136+_MM_return_variable(%rip), %rax
	leave
LCFI17:
	ret
LFE5:
.no_dead_strip _return_variable__ret_vf64
_return_variable__ret_vf64:
LFB6:
	pushq	%rbp
LCFI18:
	movq	%rsp, %rbp
LCFI19:
	leaq	_MM_return_variable(%rip), %rax
	addq	$144, %rax
	movq	(%rax), %rax
	movq	%rax, -8(%rbp)
	movsd	-8(%rbp), %xmm0
	leave
LCFI20:
	ret
LFE6:
.no_dead_strip _return_variable__ret_vi32
_return_variable__ret_vi32:
LFB7:
	pushq	%rbp
LCFI21:
	movq	%rsp, %rbp
LCFI22:
	movl	152+_MM_return_variable(%rip), %eax
	leave
LCFI23:
	ret
LFE7:
.no_dead_strip _return_variable__ret_vC
_return_variable__ret_vC:
LFB8:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	movq	160+_MM_return_variable(%rip), %rax
	leave
LCFI26:
	ret
LFE8:
.no_dead_strip _return_variable__ret_vu8
_return_variable__ret_vu8:
LFB9:
	pushq	%rbp
LCFI27:
	movq	%rsp, %rbp
LCFI28:
	movzbl	168+_MM_return_variable(%rip), %eax
	leave
LCFI29:
	ret
LFE9:
.no_dead_strip _return_variable__ret_vi64
_return_variable__ret_vi64:
LFB10:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	movq	176+_MM_return_variable(%rip), %rax
	leave
LCFI32:
	ret
LFE10:
.no_dead_strip _return_variable__ret_vf32
_return_variable__ret_vf32:
LFB11:
	pushq	%rbp
LCFI33:
	movq	%rsp, %rbp
LCFI34:
	leaq	_MM_return_variable(%rip), %rax
	addq	$184, %rax
	movl	(%rax), %eax
	movl	%eax, -4(%rbp)
	movss	-4(%rbp), %xmm0
	leave
LCFI35:
	ret
LFE11:
.no_dead_strip _return_variable__ret_vL
_return_variable__ret_vL:
LFB12:
	pushq	%rbp
LCFI36:
	movq	%rsp, %rbp
LCFI37:
	movq	192+_MM_return_variable(%rip), %rax
	leave
LCFI38:
	ret
LFE12:
.no_dead_strip _return_variable__ret_vi8
_return_variable__ret_vi8:
LFB13:
	pushq	%rbp
LCFI39:
	movq	%rsp, %rbp
LCFI40:
	movzbl	200+_MM_return_variable(%rip), %eax
	leave
LCFI41:
	ret
LFE13:
.globl _return_variable_M3
.no_dead_strip _return_variable_M3
_return_variable_M3:
LFB14:
	pushq	%rbp
LCFI42:
	movq	%rsp, %rbp
LCFI43:
	movq	%rdi, -8(%rbp)
	leaq	_MM_return_variable(%rip), %rax
	leave
LCFI44:
	ret
LFE14:
	.const_data
	.align 5
_L_1:
	.ascii "return_variable_M3"
	.space 1
	.ascii "ret_vi8"
	.space 1
	.ascii "ret_vL"
	.space 1
	.ascii "ret_vf32"
	.space 1
	.ascii "ret_vi64"
	.space 1
	.ascii "ret_vu8"
	.space 1
	.ascii "ret_vC"
	.space 1
	.ascii "ret_vi32"
	.space 1
	.ascii "ret_vf64"
	.space 1
	.ascii "ret_vu64"
	.space 1
	.ascii "ret_vi16"
	.space 1
	.ascii "ret_vI"
	.space 1
	.ascii "ret_vLC"
	.space 1
	.ascii "ret_vu32"
	.space 1
	.ascii "ret_vu16"
	.space 1
	.quad	_return_variable_M3
	.quad	_L_1
	.quad	_return_variable__ret_vi8
	.quad	_L_1+19
	.quad	_return_variable__ret_vL
	.quad	_L_1+27
	.quad	_return_variable__ret_vf32
	.quad	_L_1+34
	.quad	_return_variable__ret_vi64
	.quad	_L_1+43
	.quad	_return_variable__ret_vu8
	.quad	_L_1+52
	.quad	_return_variable__ret_vC
	.quad	_L_1+60
	.quad	_return_variable__ret_vi32
	.quad	_L_1+67
	.quad	_return_variable__ret_vf64
	.quad	_L_1+76
	.quad	_return_variable__ret_vu64
	.quad	_L_1+85
	.quad	_return_variable__ret_vi16
	.quad	_L_1+94
	.quad	_return_variable__ret_vI
	.quad	_L_1+103
	.quad	_return_variable__ret_vLC
	.quad	_L_1+110
	.quad	_return_variable__ret_vu32
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
	.quad	_MM_return_variable+224
	.space 8
	.quad	_return_variable_M3
	.quad	3
	.word	49
	.space 2
	.long	50
	.quad	51
	.quad	52
	.word	53
	.space 6
	.quad	54
	.long	343597384
	.long	1078708142
	.long	57
	.space 4
	.quad	58
	.byte	59
	.space 7
	.quad	60
	.long	1115060961
	.space 4
	.quad	63
	.byte	64
	.space 31
	.quad	_return_variable_I3
	.quad	_MM_return_variable+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_return_variable+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_return_variable+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_return_variable+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_return_variable__ret_vu16.eh = 0
.no_dead_strip _return_variable__ret_vu16.eh
_return_variable__ret_vu32.eh = 0
.no_dead_strip _return_variable__ret_vu32.eh
_return_variable__ret_vLC.eh = 0
.no_dead_strip _return_variable__ret_vLC.eh
_return_variable__ret_vI.eh = 0
.no_dead_strip _return_variable__ret_vI.eh
_return_variable__ret_vi16.eh = 0
.no_dead_strip _return_variable__ret_vi16.eh
_return_variable__ret_vu64.eh = 0
.no_dead_strip _return_variable__ret_vu64.eh
_return_variable__ret_vf64.eh = 0
.no_dead_strip _return_variable__ret_vf64.eh
_return_variable__ret_vi32.eh = 0
.no_dead_strip _return_variable__ret_vi32.eh
_return_variable__ret_vC.eh = 0
.no_dead_strip _return_variable__ret_vC.eh
_return_variable__ret_vu8.eh = 0
.no_dead_strip _return_variable__ret_vu8.eh
_return_variable__ret_vi64.eh = 0
.no_dead_strip _return_variable__ret_vi64.eh
_return_variable__ret_vf32.eh = 0
.no_dead_strip _return_variable__ret_vf32.eh
_return_variable__ret_vL.eh = 0
.no_dead_strip _return_variable__ret_vL.eh
_return_variable__ret_vi8.eh = 0
.no_dead_strip _return_variable__ret_vi8.eh
.globl _return_variable_M3.eh
_return_variable_M3.eh = 0
.no_dead_strip _return_variable_M3.eh
	.subsections_via_symbols
