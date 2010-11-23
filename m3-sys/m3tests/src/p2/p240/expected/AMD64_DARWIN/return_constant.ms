	.text
.no_dead_strip _return_constant__ret_ku16
_return_constant__ret_ku16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movl	$1, %eax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _return_constant__ret_ku32
_return_constant__ret_ku32:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	movl	$2, %eax
	leave
LCFI5:
	ret
LFE1:
.no_dead_strip _return_constant__ret_kLC
_return_constant__ret_kLC:
LFB2:
	pushq	%rbp
LCFI6:
	movq	%rsp, %rbp
LCFI7:
	movl	$3, %eax
	leave
LCFI8:
	ret
LFE2:
.no_dead_strip _return_constant__ret_kI
_return_constant__ret_kI:
LFB3:
	pushq	%rbp
LCFI9:
	movq	%rsp, %rbp
LCFI10:
	movl	$4, %eax
	leave
LCFI11:
	ret
LFE3:
.no_dead_strip _return_constant__ret_ki16
_return_constant__ret_ki16:
LFB4:
	pushq	%rbp
LCFI12:
	movq	%rsp, %rbp
LCFI13:
	movl	$5, %eax
	leave
LCFI14:
	ret
LFE4:
.no_dead_strip _return_constant__ret_ku64
_return_constant__ret_ku64:
LFB5:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	movl	$6, %eax
	leave
LCFI17:
	ret
LFE5:
.no_dead_strip _return_constant__ret_kf64
_return_constant__ret_kf64:
LFB6:
	pushq	%rbp
LCFI18:
	movq	%rsp, %rbp
LCFI19:
	movabsq	$4620468037700760371, %rax
	movq	%rax, -8(%rbp)
	movsd	-8(%rbp), %xmm0
	leave
LCFI20:
	ret
LFE6:
.no_dead_strip _return_constant__ret_ki32
_return_constant__ret_ki32:
LFB7:
	pushq	%rbp
LCFI21:
	movq	%rsp, %rbp
LCFI22:
	movl	$9, %eax
	leave
LCFI23:
	ret
LFE7:
.no_dead_strip _return_constant__ret_kC
_return_constant__ret_kC:
LFB8:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	movl	$10, %eax
	leave
LCFI26:
	ret
LFE8:
.no_dead_strip _return_constant__ret_ku8
_return_constant__ret_ku8:
LFB9:
	pushq	%rbp
LCFI27:
	movq	%rsp, %rbp
LCFI28:
	movl	$11, %eax
	leave
LCFI29:
	ret
LFE9:
.no_dead_strip _return_constant__ret_ki64
_return_constant__ret_ki64:
LFB10:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	movl	$12, %eax
	leave
LCFI32:
	ret
LFE10:
.no_dead_strip _return_constant__ret_kf32
_return_constant__ret_kf32:
LFB11:
	pushq	%rbp
LCFI33:
	movq	%rsp, %rbp
LCFI34:
	movl	$0x41523d71, %eax
	movl	%eax, -4(%rbp)
	movss	-4(%rbp), %xmm0
	leave
LCFI35:
	ret
LFE11:
.no_dead_strip _return_constant__ret_kL
_return_constant__ret_kL:
LFB12:
	pushq	%rbp
LCFI36:
	movq	%rsp, %rbp
LCFI37:
	movl	$15, %eax
	leave
LCFI38:
	ret
LFE12:
.no_dead_strip _return_constant__ret_ki8
_return_constant__ret_ki8:
LFB13:
	pushq	%rbp
LCFI39:
	movq	%rsp, %rbp
LCFI40:
	movl	$16, %eax
	leave
LCFI41:
	ret
LFE13:
.globl _return_constant_M3
.no_dead_strip _return_constant_M3
_return_constant_M3:
LFB14:
	pushq	%rbp
LCFI42:
	movq	%rsp, %rbp
LCFI43:
	movq	%rdi, -8(%rbp)
	leaq	_MM_return_constant(%rip), %rax
	leave
LCFI44:
	ret
LFE14:
	.const_data
	.align 5
_L_1:
	.ascii "return_constant_M3"
	.space 1
	.ascii "ret_ki8"
	.space 1
	.ascii "ret_kL"
	.space 1
	.ascii "ret_kf32"
	.space 1
	.ascii "ret_ki64"
	.space 1
	.ascii "ret_ku8"
	.space 1
	.ascii "ret_kC"
	.space 1
	.ascii "ret_ki32"
	.space 1
	.ascii "ret_kf64"
	.space 1
	.ascii "ret_ku64"
	.space 1
	.ascii "ret_ki16"
	.space 1
	.ascii "ret_kI"
	.space 1
	.ascii "ret_kLC"
	.space 1
	.ascii "ret_ku32"
	.space 1
	.ascii "ret_ku16"
	.space 1
	.quad	_return_constant_M3
	.quad	_L_1
	.quad	_return_constant__ret_ki8
	.quad	_L_1+19
	.quad	_return_constant__ret_kL
	.quad	_L_1+27
	.quad	_return_constant__ret_kf32
	.quad	_L_1+34
	.quad	_return_constant__ret_ki64
	.quad	_L_1+43
	.quad	_return_constant__ret_ku8
	.quad	_L_1+52
	.quad	_return_constant__ret_kC
	.quad	_L_1+60
	.quad	_return_constant__ret_ki32
	.quad	_L_1+67
	.quad	_return_constant__ret_kf64
	.quad	_L_1+76
	.quad	_return_constant__ret_ku64
	.quad	_L_1+85
	.quad	_return_constant__ret_ki16
	.quad	_L_1+94
	.quad	_return_constant__ret_kI
	.quad	_L_1+103
	.quad	_return_constant__ret_kLC
	.quad	_L_1+110
	.quad	_return_constant__ret_ku32
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
	.quad	_MM_return_constant+224
	.space 8
	.quad	_return_constant_M3
	.quad	3
	.word	17
	.space 2
	.long	18
	.quad	19
	.quad	20
	.word	21
	.space 6
	.quad	22
	.long	-1546188227
	.long	1077362032
	.long	25
	.space 4
	.quad	26
	.byte	27
	.space 7
	.quad	28
	.long	1105880678
	.space 4
	.quad	31
	.byte	32
	.space 31
	.quad	_return_constant_I3
	.quad	_MM_return_constant+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_return_constant+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_return_constant+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_return_constant+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_return_constant__ret_ku16.eh = 0
.no_dead_strip _return_constant__ret_ku16.eh
_return_constant__ret_ku32.eh = 0
.no_dead_strip _return_constant__ret_ku32.eh
_return_constant__ret_kLC.eh = 0
.no_dead_strip _return_constant__ret_kLC.eh
_return_constant__ret_kI.eh = 0
.no_dead_strip _return_constant__ret_kI.eh
_return_constant__ret_ki16.eh = 0
.no_dead_strip _return_constant__ret_ki16.eh
_return_constant__ret_ku64.eh = 0
.no_dead_strip _return_constant__ret_ku64.eh
_return_constant__ret_kf64.eh = 0
.no_dead_strip _return_constant__ret_kf64.eh
_return_constant__ret_ki32.eh = 0
.no_dead_strip _return_constant__ret_ki32.eh
_return_constant__ret_kC.eh = 0
.no_dead_strip _return_constant__ret_kC.eh
_return_constant__ret_ku8.eh = 0
.no_dead_strip _return_constant__ret_ku8.eh
_return_constant__ret_ki64.eh = 0
.no_dead_strip _return_constant__ret_ki64.eh
_return_constant__ret_kf32.eh = 0
.no_dead_strip _return_constant__ret_kf32.eh
_return_constant__ret_kL.eh = 0
.no_dead_strip _return_constant__ret_kL.eh
_return_constant__ret_ki8.eh = 0
.no_dead_strip _return_constant__ret_ki8.eh
.globl _return_constant_M3.eh
_return_constant_M3.eh = 0
.no_dead_strip _return_constant_M3.eh
	.subsections_via_symbols
