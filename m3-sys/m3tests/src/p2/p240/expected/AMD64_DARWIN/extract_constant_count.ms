	.text
.globl _extract_constant_count__Word_extract_constant_count_0
	.private_extern _extract_constant_count__Word_extract_constant_count_0
_extract_constant_count__Word_extract_constant_count_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L2
	movl	$1761, %edi
	call	__m3_fault
L2:
	movl	$0, %eax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_1
	.private_extern _extract_constant_count__Word_extract_constant_count_1
_extract_constant_count__Word_extract_constant_count_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L5
	movl	$1825, %edi
	call	__m3_fault
L5:
	movq	-24(%rbp), %rdx
	movl	$63, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$63, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_2
	.private_extern _extract_constant_count__Word_extract_constant_count_2
_extract_constant_count__Word_extract_constant_count_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$62, -16(%rbp)
	jle	L8
	movl	$1889, %edi
	call	__m3_fault
L8:
	movq	-24(%rbp), %rdx
	movl	$62, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$62, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_3
	.private_extern _extract_constant_count__Word_extract_constant_count_3
_extract_constant_count__Word_extract_constant_count_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$61, -16(%rbp)
	jle	L11
	movl	$1953, %edi
	call	__m3_fault
L11:
	movq	-24(%rbp), %rdx
	movl	$61, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$61, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_4
	.private_extern _extract_constant_count__Word_extract_constant_count_4
_extract_constant_count__Word_extract_constant_count_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$60, -16(%rbp)
	jle	L14
	movl	$2017, %edi
	call	__m3_fault
L14:
	movq	-24(%rbp), %rdx
	movl	$60, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$60, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_5
	.private_extern _extract_constant_count__Word_extract_constant_count_5
_extract_constant_count__Word_extract_constant_count_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$59, -16(%rbp)
	jle	L17
	movl	$2081, %edi
	call	__m3_fault
L17:
	movq	-24(%rbp), %rdx
	movl	$59, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$59, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_6
	.private_extern _extract_constant_count__Word_extract_constant_count_6
_extract_constant_count__Word_extract_constant_count_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$58, -16(%rbp)
	jle	L20
	movl	$2145, %edi
	call	__m3_fault
L20:
	movq	-24(%rbp), %rdx
	movl	$58, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$58, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_7
	.private_extern _extract_constant_count__Word_extract_constant_count_7
_extract_constant_count__Word_extract_constant_count_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$57, -16(%rbp)
	jle	L23
	movl	$2209, %edi
	call	__m3_fault
L23:
	movq	-24(%rbp), %rdx
	movl	$57, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$57, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_8
	.private_extern _extract_constant_count__Word_extract_constant_count_8
_extract_constant_count__Word_extract_constant_count_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$56, -16(%rbp)
	jle	L26
	movl	$2273, %edi
	call	__m3_fault
L26:
	movq	-24(%rbp), %rdx
	movl	$56, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$56, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_9
	.private_extern _extract_constant_count__Word_extract_constant_count_9
_extract_constant_count__Word_extract_constant_count_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$55, -16(%rbp)
	jle	L29
	movl	$2337, %edi
	call	__m3_fault
L29:
	movq	-24(%rbp), %rdx
	movl	$55, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$55, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_10
	.private_extern _extract_constant_count__Word_extract_constant_count_10
_extract_constant_count__Word_extract_constant_count_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$54, -16(%rbp)
	jle	L32
	movl	$2401, %edi
	call	__m3_fault
L32:
	movq	-24(%rbp), %rdx
	movl	$54, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$54, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_11
	.private_extern _extract_constant_count__Word_extract_constant_count_11
_extract_constant_count__Word_extract_constant_count_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$53, -16(%rbp)
	jle	L35
	movl	$2465, %edi
	call	__m3_fault
L35:
	movq	-24(%rbp), %rdx
	movl	$53, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$53, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_12
	.private_extern _extract_constant_count__Word_extract_constant_count_12
_extract_constant_count__Word_extract_constant_count_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$52, -16(%rbp)
	jle	L38
	movl	$2529, %edi
	call	__m3_fault
L38:
	movq	-24(%rbp), %rdx
	movl	$52, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$52, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_13
	.private_extern _extract_constant_count__Word_extract_constant_count_13
_extract_constant_count__Word_extract_constant_count_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$51, -16(%rbp)
	jle	L41
	movl	$2593, %edi
	call	__m3_fault
L41:
	movq	-24(%rbp), %rdx
	movl	$51, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$51, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_14
	.private_extern _extract_constant_count__Word_extract_constant_count_14
_extract_constant_count__Word_extract_constant_count_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$50, -16(%rbp)
	jle	L44
	movl	$2657, %edi
	call	__m3_fault
L44:
	movq	-24(%rbp), %rdx
	movl	$50, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$50, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_15
	.private_extern _extract_constant_count__Word_extract_constant_count_15
_extract_constant_count__Word_extract_constant_count_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$49, -16(%rbp)
	jle	L47
	movl	$2721, %edi
	call	__m3_fault
L47:
	movq	-24(%rbp), %rdx
	movl	$49, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$49, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_16
	.private_extern _extract_constant_count__Word_extract_constant_count_16
_extract_constant_count__Word_extract_constant_count_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$48, -16(%rbp)
	jle	L50
	movl	$2785, %edi
	call	__m3_fault
L50:
	movq	-24(%rbp), %rdx
	movl	$48, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$48, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_17
	.private_extern _extract_constant_count__Word_extract_constant_count_17
_extract_constant_count__Word_extract_constant_count_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$47, -16(%rbp)
	jle	L53
	movl	$2849, %edi
	call	__m3_fault
L53:
	movq	-24(%rbp), %rdx
	movl	$47, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$47, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_18
	.private_extern _extract_constant_count__Word_extract_constant_count_18
_extract_constant_count__Word_extract_constant_count_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$46, -16(%rbp)
	jle	L56
	movl	$2913, %edi
	call	__m3_fault
L56:
	movq	-24(%rbp), %rdx
	movl	$46, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$46, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_19
	.private_extern _extract_constant_count__Word_extract_constant_count_19
_extract_constant_count__Word_extract_constant_count_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$45, -16(%rbp)
	jle	L59
	movl	$2977, %edi
	call	__m3_fault
L59:
	movq	-24(%rbp), %rdx
	movl	$45, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$45, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_20
	.private_extern _extract_constant_count__Word_extract_constant_count_20
_extract_constant_count__Word_extract_constant_count_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$44, -16(%rbp)
	jle	L62
	movl	$3041, %edi
	call	__m3_fault
L62:
	movq	-24(%rbp), %rdx
	movl	$44, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$44, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_21
	.private_extern _extract_constant_count__Word_extract_constant_count_21
_extract_constant_count__Word_extract_constant_count_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$43, -16(%rbp)
	jle	L65
	movl	$3105, %edi
	call	__m3_fault
L65:
	movq	-24(%rbp), %rdx
	movl	$43, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$43, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_22
	.private_extern _extract_constant_count__Word_extract_constant_count_22
_extract_constant_count__Word_extract_constant_count_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$42, -16(%rbp)
	jle	L68
	movl	$3169, %edi
	call	__m3_fault
L68:
	movq	-24(%rbp), %rdx
	movl	$42, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$42, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_23
	.private_extern _extract_constant_count__Word_extract_constant_count_23
_extract_constant_count__Word_extract_constant_count_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$41, -16(%rbp)
	jle	L71
	movl	$3233, %edi
	call	__m3_fault
L71:
	movq	-24(%rbp), %rdx
	movl	$41, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$41, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_24
	.private_extern _extract_constant_count__Word_extract_constant_count_24
_extract_constant_count__Word_extract_constant_count_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$40, -16(%rbp)
	jle	L74
	movl	$3297, %edi
	call	__m3_fault
L74:
	movq	-24(%rbp), %rdx
	movl	$40, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$40, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_25
	.private_extern _extract_constant_count__Word_extract_constant_count_25
_extract_constant_count__Word_extract_constant_count_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$39, -16(%rbp)
	jle	L77
	movl	$3361, %edi
	call	__m3_fault
L77:
	movq	-24(%rbp), %rdx
	movl	$39, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$39, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_26
	.private_extern _extract_constant_count__Word_extract_constant_count_26
_extract_constant_count__Word_extract_constant_count_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$38, -16(%rbp)
	jle	L80
	movl	$3425, %edi
	call	__m3_fault
L80:
	movq	-24(%rbp), %rdx
	movl	$38, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$38, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_27
	.private_extern _extract_constant_count__Word_extract_constant_count_27
_extract_constant_count__Word_extract_constant_count_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$37, -16(%rbp)
	jle	L83
	movl	$3489, %edi
	call	__m3_fault
L83:
	movq	-24(%rbp), %rdx
	movl	$37, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$37, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_28
	.private_extern _extract_constant_count__Word_extract_constant_count_28
_extract_constant_count__Word_extract_constant_count_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$36, -16(%rbp)
	jle	L86
	movl	$3553, %edi
	call	__m3_fault
L86:
	movq	-24(%rbp), %rdx
	movl	$36, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$36, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_29
	.private_extern _extract_constant_count__Word_extract_constant_count_29
_extract_constant_count__Word_extract_constant_count_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$35, -16(%rbp)
	jle	L89
	movl	$3617, %edi
	call	__m3_fault
L89:
	movq	-24(%rbp), %rdx
	movl	$35, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$35, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_30
	.private_extern _extract_constant_count__Word_extract_constant_count_30
_extract_constant_count__Word_extract_constant_count_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$34, -16(%rbp)
	jle	L92
	movl	$3681, %edi
	call	__m3_fault
L92:
	movq	-24(%rbp), %rdx
	movl	$34, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$34, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_31
	.private_extern _extract_constant_count__Word_extract_constant_count_31
_extract_constant_count__Word_extract_constant_count_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$33, -16(%rbp)
	jle	L95
	movl	$3745, %edi
	call	__m3_fault
L95:
	movq	-24(%rbp), %rdx
	movl	$33, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$33, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_32
	.private_extern _extract_constant_count__Word_extract_constant_count_32
_extract_constant_count__Word_extract_constant_count_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32, -16(%rbp)
	jle	L98
	movl	$3809, %edi
	call	__m3_fault
L98:
	movq	-24(%rbp), %rdx
	movl	$32, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$32, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_33
	.private_extern _extract_constant_count__Word_extract_constant_count_33
_extract_constant_count__Word_extract_constant_count_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$31, -16(%rbp)
	jle	L101
	movl	$3873, %edi
	call	__m3_fault
L101:
	movq	-24(%rbp), %rdx
	movl	$31, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$31, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_34
	.private_extern _extract_constant_count__Word_extract_constant_count_34
_extract_constant_count__Word_extract_constant_count_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$30, -16(%rbp)
	jle	L104
	movl	$3937, %edi
	call	__m3_fault
L104:
	movq	-24(%rbp), %rdx
	movl	$30, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$30, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_35
	.private_extern _extract_constant_count__Word_extract_constant_count_35
_extract_constant_count__Word_extract_constant_count_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$29, -16(%rbp)
	jle	L107
	movl	$4001, %edi
	call	__m3_fault
L107:
	movq	-24(%rbp), %rdx
	movl	$29, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$29, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_36
	.private_extern _extract_constant_count__Word_extract_constant_count_36
_extract_constant_count__Word_extract_constant_count_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$28, -16(%rbp)
	jle	L110
	movl	$4065, %edi
	call	__m3_fault
L110:
	movq	-24(%rbp), %rdx
	movl	$28, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$28, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_37
	.private_extern _extract_constant_count__Word_extract_constant_count_37
_extract_constant_count__Word_extract_constant_count_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$27, -16(%rbp)
	jle	L113
	movl	$4129, %edi
	call	__m3_fault
L113:
	movq	-24(%rbp), %rdx
	movl	$27, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$27, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_38
	.private_extern _extract_constant_count__Word_extract_constant_count_38
_extract_constant_count__Word_extract_constant_count_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$26, -16(%rbp)
	jle	L116
	movl	$4193, %edi
	call	__m3_fault
L116:
	movq	-24(%rbp), %rdx
	movl	$26, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$26, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_39
	.private_extern _extract_constant_count__Word_extract_constant_count_39
_extract_constant_count__Word_extract_constant_count_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$25, -16(%rbp)
	jle	L119
	movl	$4257, %edi
	call	__m3_fault
L119:
	movq	-24(%rbp), %rdx
	movl	$25, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$25, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_40
	.private_extern _extract_constant_count__Word_extract_constant_count_40
_extract_constant_count__Word_extract_constant_count_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$24, -16(%rbp)
	jle	L122
	movl	$4321, %edi
	call	__m3_fault
L122:
	movq	-24(%rbp), %rdx
	movl	$24, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$24, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_41
	.private_extern _extract_constant_count__Word_extract_constant_count_41
_extract_constant_count__Word_extract_constant_count_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$23, -16(%rbp)
	jle	L125
	movl	$4385, %edi
	call	__m3_fault
L125:
	movq	-24(%rbp), %rdx
	movl	$23, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$23, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_42
	.private_extern _extract_constant_count__Word_extract_constant_count_42
_extract_constant_count__Word_extract_constant_count_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$22, -16(%rbp)
	jle	L128
	movl	$4449, %edi
	call	__m3_fault
L128:
	movq	-24(%rbp), %rdx
	movl	$22, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$22, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_43
	.private_extern _extract_constant_count__Word_extract_constant_count_43
_extract_constant_count__Word_extract_constant_count_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$21, -16(%rbp)
	jle	L131
	movl	$4513, %edi
	call	__m3_fault
L131:
	movq	-24(%rbp), %rdx
	movl	$21, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$21, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_44
	.private_extern _extract_constant_count__Word_extract_constant_count_44
_extract_constant_count__Word_extract_constant_count_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$20, -16(%rbp)
	jle	L134
	movl	$4577, %edi
	call	__m3_fault
L134:
	movq	-24(%rbp), %rdx
	movl	$20, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$20, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_45
	.private_extern _extract_constant_count__Word_extract_constant_count_45
_extract_constant_count__Word_extract_constant_count_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$19, -16(%rbp)
	jle	L137
	movl	$4641, %edi
	call	__m3_fault
L137:
	movq	-24(%rbp), %rdx
	movl	$19, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$19, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_46
	.private_extern _extract_constant_count__Word_extract_constant_count_46
_extract_constant_count__Word_extract_constant_count_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$18, -16(%rbp)
	jle	L140
	movl	$4705, %edi
	call	__m3_fault
L140:
	movq	-24(%rbp), %rdx
	movl	$18, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$18, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_47
	.private_extern _extract_constant_count__Word_extract_constant_count_47
_extract_constant_count__Word_extract_constant_count_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$17, -16(%rbp)
	jle	L143
	movl	$4769, %edi
	call	__m3_fault
L143:
	movq	-24(%rbp), %rdx
	movl	$17, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$17, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_48
	.private_extern _extract_constant_count__Word_extract_constant_count_48
_extract_constant_count__Word_extract_constant_count_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$16, -16(%rbp)
	jle	L146
	movl	$4833, %edi
	call	__m3_fault
L146:
	movq	-24(%rbp), %rdx
	movl	$16, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$16, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_49
	.private_extern _extract_constant_count__Word_extract_constant_count_49
_extract_constant_count__Word_extract_constant_count_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$15, -16(%rbp)
	jle	L149
	movl	$4897, %edi
	call	__m3_fault
L149:
	movq	-24(%rbp), %rdx
	movl	$15, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$15, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_50
	.private_extern _extract_constant_count__Word_extract_constant_count_50
_extract_constant_count__Word_extract_constant_count_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$14, -16(%rbp)
	jle	L152
	movl	$4961, %edi
	call	__m3_fault
L152:
	movq	-24(%rbp), %rdx
	movl	$14, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$14, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_51
	.private_extern _extract_constant_count__Word_extract_constant_count_51
_extract_constant_count__Word_extract_constant_count_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$13, -16(%rbp)
	jle	L155
	movl	$5025, %edi
	call	__m3_fault
L155:
	movq	-24(%rbp), %rdx
	movl	$13, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$13, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_52
	.private_extern _extract_constant_count__Word_extract_constant_count_52
_extract_constant_count__Word_extract_constant_count_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$12, -16(%rbp)
	jle	L158
	movl	$5089, %edi
	call	__m3_fault
L158:
	movq	-24(%rbp), %rdx
	movl	$12, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$12, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_53
	.private_extern _extract_constant_count__Word_extract_constant_count_53
_extract_constant_count__Word_extract_constant_count_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$11, -16(%rbp)
	jle	L161
	movl	$5153, %edi
	call	__m3_fault
L161:
	movq	-24(%rbp), %rdx
	movl	$11, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$11, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_54
	.private_extern _extract_constant_count__Word_extract_constant_count_54
_extract_constant_count__Word_extract_constant_count_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$10, -16(%rbp)
	jle	L164
	movl	$5217, %edi
	call	__m3_fault
L164:
	movq	-24(%rbp), %rdx
	movl	$10, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$10, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_55
	.private_extern _extract_constant_count__Word_extract_constant_count_55
_extract_constant_count__Word_extract_constant_count_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$9, -16(%rbp)
	jle	L167
	movl	$5281, %edi
	call	__m3_fault
L167:
	movq	-24(%rbp), %rdx
	movl	$9, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$9, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_56
	.private_extern _extract_constant_count__Word_extract_constant_count_56
_extract_constant_count__Word_extract_constant_count_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$8, -16(%rbp)
	jle	L170
	movl	$5345, %edi
	call	__m3_fault
L170:
	movq	-24(%rbp), %rdx
	movl	$8, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$8, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_57
	.private_extern _extract_constant_count__Word_extract_constant_count_57
_extract_constant_count__Word_extract_constant_count_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$7, -16(%rbp)
	jle	L173
	movl	$5409, %edi
	call	__m3_fault
L173:
	movq	-24(%rbp), %rdx
	movl	$7, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$7, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_58
	.private_extern _extract_constant_count__Word_extract_constant_count_58
_extract_constant_count__Word_extract_constant_count_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$6, -16(%rbp)
	jle	L176
	movl	$5473, %edi
	call	__m3_fault
L176:
	movq	-24(%rbp), %rdx
	movl	$6, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$6, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_59
	.private_extern _extract_constant_count__Word_extract_constant_count_59
_extract_constant_count__Word_extract_constant_count_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$5, -16(%rbp)
	jle	L179
	movl	$5537, %edi
	call	__m3_fault
L179:
	movq	-24(%rbp), %rdx
	movl	$5, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$5, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_60
	.private_extern _extract_constant_count__Word_extract_constant_count_60
_extract_constant_count__Word_extract_constant_count_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$4, -16(%rbp)
	jle	L182
	movl	$5601, %edi
	call	__m3_fault
L182:
	movq	-24(%rbp), %rdx
	movl	$4, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$4, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_61
	.private_extern _extract_constant_count__Word_extract_constant_count_61
_extract_constant_count__Word_extract_constant_count_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$3, -16(%rbp)
	jle	L185
	movl	$5665, %edi
	call	__m3_fault
L185:
	movq	-24(%rbp), %rdx
	movl	$3, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$3, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_62
	.private_extern _extract_constant_count__Word_extract_constant_count_62
_extract_constant_count__Word_extract_constant_count_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2, -16(%rbp)
	jle	L188
	movl	$5729, %edi
	call	__m3_fault
L188:
	movq	-24(%rbp), %rdx
	movl	$2, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$2, %rax
	leave
	ret
.globl _extract_constant_count__Word_extract_constant_count_63
	.private_extern _extract_constant_count__Word_extract_constant_count_63
_extract_constant_count__Word_extract_constant_count_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$1, -16(%rbp)
	jle	L191
	movl	$5793, %edi
	call	__m3_fault
L191:
	movq	-24(%rbp), %rdx
	movl	$1, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	%rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_0
	.private_extern _extract_constant_count__Long_extract_constant_count_0
_extract_constant_count__Long_extract_constant_count_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L194
	movl	$5857, %edi
	call	__m3_fault
L194:
	movl	$0, %eax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_1
	.private_extern _extract_constant_count__Long_extract_constant_count_1
_extract_constant_count__Long_extract_constant_count_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L197
	movl	$5921, %edi
	call	__m3_fault
L197:
	movq	-24(%rbp), %rdx
	movl	$63, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$63, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_2
	.private_extern _extract_constant_count__Long_extract_constant_count_2
_extract_constant_count__Long_extract_constant_count_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$62, -16(%rbp)
	jle	L200
	movl	$5985, %edi
	call	__m3_fault
L200:
	movq	-24(%rbp), %rdx
	movl	$62, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$62, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_3
	.private_extern _extract_constant_count__Long_extract_constant_count_3
_extract_constant_count__Long_extract_constant_count_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$61, -16(%rbp)
	jle	L203
	movl	$6049, %edi
	call	__m3_fault
L203:
	movq	-24(%rbp), %rdx
	movl	$61, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$61, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_4
	.private_extern _extract_constant_count__Long_extract_constant_count_4
_extract_constant_count__Long_extract_constant_count_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$60, -16(%rbp)
	jle	L206
	movl	$6113, %edi
	call	__m3_fault
L206:
	movq	-24(%rbp), %rdx
	movl	$60, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$60, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_5
	.private_extern _extract_constant_count__Long_extract_constant_count_5
_extract_constant_count__Long_extract_constant_count_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$59, -16(%rbp)
	jle	L209
	movl	$6177, %edi
	call	__m3_fault
L209:
	movq	-24(%rbp), %rdx
	movl	$59, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$59, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_6
	.private_extern _extract_constant_count__Long_extract_constant_count_6
_extract_constant_count__Long_extract_constant_count_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$58, -16(%rbp)
	jle	L212
	movl	$6241, %edi
	call	__m3_fault
L212:
	movq	-24(%rbp), %rdx
	movl	$58, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$58, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_7
	.private_extern _extract_constant_count__Long_extract_constant_count_7
_extract_constant_count__Long_extract_constant_count_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$57, -16(%rbp)
	jle	L215
	movl	$6305, %edi
	call	__m3_fault
L215:
	movq	-24(%rbp), %rdx
	movl	$57, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$57, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_8
	.private_extern _extract_constant_count__Long_extract_constant_count_8
_extract_constant_count__Long_extract_constant_count_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$56, -16(%rbp)
	jle	L218
	movl	$6369, %edi
	call	__m3_fault
L218:
	movq	-24(%rbp), %rdx
	movl	$56, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$56, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_9
	.private_extern _extract_constant_count__Long_extract_constant_count_9
_extract_constant_count__Long_extract_constant_count_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$55, -16(%rbp)
	jle	L221
	movl	$6433, %edi
	call	__m3_fault
L221:
	movq	-24(%rbp), %rdx
	movl	$55, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$55, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_10
	.private_extern _extract_constant_count__Long_extract_constant_count_10
_extract_constant_count__Long_extract_constant_count_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$54, -16(%rbp)
	jle	L224
	movl	$6497, %edi
	call	__m3_fault
L224:
	movq	-24(%rbp), %rdx
	movl	$54, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$54, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_11
	.private_extern _extract_constant_count__Long_extract_constant_count_11
_extract_constant_count__Long_extract_constant_count_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$53, -16(%rbp)
	jle	L227
	movl	$6561, %edi
	call	__m3_fault
L227:
	movq	-24(%rbp), %rdx
	movl	$53, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$53, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_12
	.private_extern _extract_constant_count__Long_extract_constant_count_12
_extract_constant_count__Long_extract_constant_count_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$52, -16(%rbp)
	jle	L230
	movl	$6625, %edi
	call	__m3_fault
L230:
	movq	-24(%rbp), %rdx
	movl	$52, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$52, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_13
	.private_extern _extract_constant_count__Long_extract_constant_count_13
_extract_constant_count__Long_extract_constant_count_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$51, -16(%rbp)
	jle	L233
	movl	$6689, %edi
	call	__m3_fault
L233:
	movq	-24(%rbp), %rdx
	movl	$51, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$51, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_14
	.private_extern _extract_constant_count__Long_extract_constant_count_14
_extract_constant_count__Long_extract_constant_count_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$50, -16(%rbp)
	jle	L236
	movl	$6753, %edi
	call	__m3_fault
L236:
	movq	-24(%rbp), %rdx
	movl	$50, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$50, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_15
	.private_extern _extract_constant_count__Long_extract_constant_count_15
_extract_constant_count__Long_extract_constant_count_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$49, -16(%rbp)
	jle	L239
	movl	$6817, %edi
	call	__m3_fault
L239:
	movq	-24(%rbp), %rdx
	movl	$49, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$49, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_16
	.private_extern _extract_constant_count__Long_extract_constant_count_16
_extract_constant_count__Long_extract_constant_count_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$48, -16(%rbp)
	jle	L242
	movl	$6881, %edi
	call	__m3_fault
L242:
	movq	-24(%rbp), %rdx
	movl	$48, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$48, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_17
	.private_extern _extract_constant_count__Long_extract_constant_count_17
_extract_constant_count__Long_extract_constant_count_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$47, -16(%rbp)
	jle	L245
	movl	$6945, %edi
	call	__m3_fault
L245:
	movq	-24(%rbp), %rdx
	movl	$47, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$47, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_18
	.private_extern _extract_constant_count__Long_extract_constant_count_18
_extract_constant_count__Long_extract_constant_count_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$46, -16(%rbp)
	jle	L248
	movl	$7009, %edi
	call	__m3_fault
L248:
	movq	-24(%rbp), %rdx
	movl	$46, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$46, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_19
	.private_extern _extract_constant_count__Long_extract_constant_count_19
_extract_constant_count__Long_extract_constant_count_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$45, -16(%rbp)
	jle	L251
	movl	$7073, %edi
	call	__m3_fault
L251:
	movq	-24(%rbp), %rdx
	movl	$45, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$45, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_20
	.private_extern _extract_constant_count__Long_extract_constant_count_20
_extract_constant_count__Long_extract_constant_count_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$44, -16(%rbp)
	jle	L254
	movl	$7137, %edi
	call	__m3_fault
L254:
	movq	-24(%rbp), %rdx
	movl	$44, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$44, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_21
	.private_extern _extract_constant_count__Long_extract_constant_count_21
_extract_constant_count__Long_extract_constant_count_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$43, -16(%rbp)
	jle	L257
	movl	$7201, %edi
	call	__m3_fault
L257:
	movq	-24(%rbp), %rdx
	movl	$43, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$43, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_22
	.private_extern _extract_constant_count__Long_extract_constant_count_22
_extract_constant_count__Long_extract_constant_count_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$42, -16(%rbp)
	jle	L260
	movl	$7265, %edi
	call	__m3_fault
L260:
	movq	-24(%rbp), %rdx
	movl	$42, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$42, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_23
	.private_extern _extract_constant_count__Long_extract_constant_count_23
_extract_constant_count__Long_extract_constant_count_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$41, -16(%rbp)
	jle	L263
	movl	$7329, %edi
	call	__m3_fault
L263:
	movq	-24(%rbp), %rdx
	movl	$41, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$41, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_24
	.private_extern _extract_constant_count__Long_extract_constant_count_24
_extract_constant_count__Long_extract_constant_count_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$40, -16(%rbp)
	jle	L266
	movl	$7393, %edi
	call	__m3_fault
L266:
	movq	-24(%rbp), %rdx
	movl	$40, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$40, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_25
	.private_extern _extract_constant_count__Long_extract_constant_count_25
_extract_constant_count__Long_extract_constant_count_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$39, -16(%rbp)
	jle	L269
	movl	$7457, %edi
	call	__m3_fault
L269:
	movq	-24(%rbp), %rdx
	movl	$39, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$39, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_26
	.private_extern _extract_constant_count__Long_extract_constant_count_26
_extract_constant_count__Long_extract_constant_count_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$38, -16(%rbp)
	jle	L272
	movl	$7521, %edi
	call	__m3_fault
L272:
	movq	-24(%rbp), %rdx
	movl	$38, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$38, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_27
	.private_extern _extract_constant_count__Long_extract_constant_count_27
_extract_constant_count__Long_extract_constant_count_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$37, -16(%rbp)
	jle	L275
	movl	$7585, %edi
	call	__m3_fault
L275:
	movq	-24(%rbp), %rdx
	movl	$37, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$37, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_28
	.private_extern _extract_constant_count__Long_extract_constant_count_28
_extract_constant_count__Long_extract_constant_count_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$36, -16(%rbp)
	jle	L278
	movl	$7649, %edi
	call	__m3_fault
L278:
	movq	-24(%rbp), %rdx
	movl	$36, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$36, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_29
	.private_extern _extract_constant_count__Long_extract_constant_count_29
_extract_constant_count__Long_extract_constant_count_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$35, -16(%rbp)
	jle	L281
	movl	$7713, %edi
	call	__m3_fault
L281:
	movq	-24(%rbp), %rdx
	movl	$35, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$35, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_30
	.private_extern _extract_constant_count__Long_extract_constant_count_30
_extract_constant_count__Long_extract_constant_count_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$34, -16(%rbp)
	jle	L284
	movl	$7777, %edi
	call	__m3_fault
L284:
	movq	-24(%rbp), %rdx
	movl	$34, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$34, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_31
	.private_extern _extract_constant_count__Long_extract_constant_count_31
_extract_constant_count__Long_extract_constant_count_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$33, -16(%rbp)
	jle	L287
	movl	$7841, %edi
	call	__m3_fault
L287:
	movq	-24(%rbp), %rdx
	movl	$33, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$33, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_32
	.private_extern _extract_constant_count__Long_extract_constant_count_32
_extract_constant_count__Long_extract_constant_count_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32, -16(%rbp)
	jle	L290
	movl	$7905, %edi
	call	__m3_fault
L290:
	movq	-24(%rbp), %rdx
	movl	$32, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$32, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_33
	.private_extern _extract_constant_count__Long_extract_constant_count_33
_extract_constant_count__Long_extract_constant_count_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$31, -16(%rbp)
	jle	L293
	movl	$7969, %edi
	call	__m3_fault
L293:
	movq	-24(%rbp), %rdx
	movl	$31, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$31, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_34
	.private_extern _extract_constant_count__Long_extract_constant_count_34
_extract_constant_count__Long_extract_constant_count_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$30, -16(%rbp)
	jle	L296
	movl	$8033, %edi
	call	__m3_fault
L296:
	movq	-24(%rbp), %rdx
	movl	$30, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$30, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_35
	.private_extern _extract_constant_count__Long_extract_constant_count_35
_extract_constant_count__Long_extract_constant_count_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$29, -16(%rbp)
	jle	L299
	movl	$8097, %edi
	call	__m3_fault
L299:
	movq	-24(%rbp), %rdx
	movl	$29, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$29, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_36
	.private_extern _extract_constant_count__Long_extract_constant_count_36
_extract_constant_count__Long_extract_constant_count_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$28, -16(%rbp)
	jle	L302
	movl	$8161, %edi
	call	__m3_fault
L302:
	movq	-24(%rbp), %rdx
	movl	$28, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$28, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_37
	.private_extern _extract_constant_count__Long_extract_constant_count_37
_extract_constant_count__Long_extract_constant_count_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$27, -16(%rbp)
	jle	L305
	movl	$8225, %edi
	call	__m3_fault
L305:
	movq	-24(%rbp), %rdx
	movl	$27, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$27, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_38
	.private_extern _extract_constant_count__Long_extract_constant_count_38
_extract_constant_count__Long_extract_constant_count_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$26, -16(%rbp)
	jle	L308
	movl	$8289, %edi
	call	__m3_fault
L308:
	movq	-24(%rbp), %rdx
	movl	$26, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$26, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_39
	.private_extern _extract_constant_count__Long_extract_constant_count_39
_extract_constant_count__Long_extract_constant_count_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$25, -16(%rbp)
	jle	L311
	movl	$8353, %edi
	call	__m3_fault
L311:
	movq	-24(%rbp), %rdx
	movl	$25, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$25, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_40
	.private_extern _extract_constant_count__Long_extract_constant_count_40
_extract_constant_count__Long_extract_constant_count_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$24, -16(%rbp)
	jle	L314
	movl	$8417, %edi
	call	__m3_fault
L314:
	movq	-24(%rbp), %rdx
	movl	$24, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$24, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_41
	.private_extern _extract_constant_count__Long_extract_constant_count_41
_extract_constant_count__Long_extract_constant_count_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$23, -16(%rbp)
	jle	L317
	movl	$8481, %edi
	call	__m3_fault
L317:
	movq	-24(%rbp), %rdx
	movl	$23, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$23, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_42
	.private_extern _extract_constant_count__Long_extract_constant_count_42
_extract_constant_count__Long_extract_constant_count_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$22, -16(%rbp)
	jle	L320
	movl	$8545, %edi
	call	__m3_fault
L320:
	movq	-24(%rbp), %rdx
	movl	$22, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$22, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_43
	.private_extern _extract_constant_count__Long_extract_constant_count_43
_extract_constant_count__Long_extract_constant_count_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$21, -16(%rbp)
	jle	L323
	movl	$8609, %edi
	call	__m3_fault
L323:
	movq	-24(%rbp), %rdx
	movl	$21, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$21, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_44
	.private_extern _extract_constant_count__Long_extract_constant_count_44
_extract_constant_count__Long_extract_constant_count_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$20, -16(%rbp)
	jle	L326
	movl	$8673, %edi
	call	__m3_fault
L326:
	movq	-24(%rbp), %rdx
	movl	$20, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$20, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_45
	.private_extern _extract_constant_count__Long_extract_constant_count_45
_extract_constant_count__Long_extract_constant_count_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$19, -16(%rbp)
	jle	L329
	movl	$8737, %edi
	call	__m3_fault
L329:
	movq	-24(%rbp), %rdx
	movl	$19, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$19, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_46
	.private_extern _extract_constant_count__Long_extract_constant_count_46
_extract_constant_count__Long_extract_constant_count_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$18, -16(%rbp)
	jle	L332
	movl	$8801, %edi
	call	__m3_fault
L332:
	movq	-24(%rbp), %rdx
	movl	$18, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$18, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_47
	.private_extern _extract_constant_count__Long_extract_constant_count_47
_extract_constant_count__Long_extract_constant_count_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$17, -16(%rbp)
	jle	L335
	movl	$8865, %edi
	call	__m3_fault
L335:
	movq	-24(%rbp), %rdx
	movl	$17, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$17, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_48
	.private_extern _extract_constant_count__Long_extract_constant_count_48
_extract_constant_count__Long_extract_constant_count_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$16, -16(%rbp)
	jle	L338
	movl	$8929, %edi
	call	__m3_fault
L338:
	movq	-24(%rbp), %rdx
	movl	$16, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$16, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_49
	.private_extern _extract_constant_count__Long_extract_constant_count_49
_extract_constant_count__Long_extract_constant_count_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$15, -16(%rbp)
	jle	L341
	movl	$8993, %edi
	call	__m3_fault
L341:
	movq	-24(%rbp), %rdx
	movl	$15, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$15, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_50
	.private_extern _extract_constant_count__Long_extract_constant_count_50
_extract_constant_count__Long_extract_constant_count_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$14, -16(%rbp)
	jle	L344
	movl	$9057, %edi
	call	__m3_fault
L344:
	movq	-24(%rbp), %rdx
	movl	$14, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$14, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_51
	.private_extern _extract_constant_count__Long_extract_constant_count_51
_extract_constant_count__Long_extract_constant_count_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$13, -16(%rbp)
	jle	L347
	movl	$9121, %edi
	call	__m3_fault
L347:
	movq	-24(%rbp), %rdx
	movl	$13, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$13, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_52
	.private_extern _extract_constant_count__Long_extract_constant_count_52
_extract_constant_count__Long_extract_constant_count_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$12, -16(%rbp)
	jle	L350
	movl	$9185, %edi
	call	__m3_fault
L350:
	movq	-24(%rbp), %rdx
	movl	$12, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$12, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_53
	.private_extern _extract_constant_count__Long_extract_constant_count_53
_extract_constant_count__Long_extract_constant_count_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$11, -16(%rbp)
	jle	L353
	movl	$9249, %edi
	call	__m3_fault
L353:
	movq	-24(%rbp), %rdx
	movl	$11, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$11, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_54
	.private_extern _extract_constant_count__Long_extract_constant_count_54
_extract_constant_count__Long_extract_constant_count_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$10, -16(%rbp)
	jle	L356
	movl	$9313, %edi
	call	__m3_fault
L356:
	movq	-24(%rbp), %rdx
	movl	$10, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$10, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_55
	.private_extern _extract_constant_count__Long_extract_constant_count_55
_extract_constant_count__Long_extract_constant_count_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$9, -16(%rbp)
	jle	L359
	movl	$9377, %edi
	call	__m3_fault
L359:
	movq	-24(%rbp), %rdx
	movl	$9, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$9, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_56
	.private_extern _extract_constant_count__Long_extract_constant_count_56
_extract_constant_count__Long_extract_constant_count_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$8, -16(%rbp)
	jle	L362
	movl	$9441, %edi
	call	__m3_fault
L362:
	movq	-24(%rbp), %rdx
	movl	$8, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$8, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_57
	.private_extern _extract_constant_count__Long_extract_constant_count_57
_extract_constant_count__Long_extract_constant_count_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$7, -16(%rbp)
	jle	L365
	movl	$9505, %edi
	call	__m3_fault
L365:
	movq	-24(%rbp), %rdx
	movl	$7, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$7, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_58
	.private_extern _extract_constant_count__Long_extract_constant_count_58
_extract_constant_count__Long_extract_constant_count_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$6, -16(%rbp)
	jle	L368
	movl	$9569, %edi
	call	__m3_fault
L368:
	movq	-24(%rbp), %rdx
	movl	$6, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$6, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_59
	.private_extern _extract_constant_count__Long_extract_constant_count_59
_extract_constant_count__Long_extract_constant_count_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$5, -16(%rbp)
	jle	L371
	movl	$9633, %edi
	call	__m3_fault
L371:
	movq	-24(%rbp), %rdx
	movl	$5, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$5, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_60
	.private_extern _extract_constant_count__Long_extract_constant_count_60
_extract_constant_count__Long_extract_constant_count_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$4, -16(%rbp)
	jle	L374
	movl	$9697, %edi
	call	__m3_fault
L374:
	movq	-24(%rbp), %rdx
	movl	$4, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$4, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_61
	.private_extern _extract_constant_count__Long_extract_constant_count_61
_extract_constant_count__Long_extract_constant_count_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$3, -16(%rbp)
	jle	L377
	movl	$9761, %edi
	call	__m3_fault
L377:
	movq	-24(%rbp), %rdx
	movl	$3, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$3, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_62
	.private_extern _extract_constant_count__Long_extract_constant_count_62
_extract_constant_count__Long_extract_constant_count_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2, -16(%rbp)
	jle	L380
	movl	$9825, %edi
	call	__m3_fault
L380:
	movq	-24(%rbp), %rdx
	movl	$2, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	$2, %rax
	leave
	ret
.globl _extract_constant_count__Long_extract_constant_count_63
	.private_extern _extract_constant_count__Long_extract_constant_count_63
_extract_constant_count__Long_extract_constant_count_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	192+_MM_extract_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$1, -16(%rbp)
	jle	L383
	movl	$9889, %edi
	call	__m3_fault
L383:
	movq	-24(%rbp), %rdx
	movl	$1, %eax
	movq	%rax, %rcx
	subq	-16(%rbp), %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	shrq	%rax
	leave
	ret
.globl _extract_constant_count_M3
_extract_constant_count_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_extract_constant_count(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_extract_constant_count(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "extract_constant_count_M3"
	.space 1
	.ascii "Long_extract_constant_count_63"
	.space 1
	.ascii "Long_extract_constant_count_62"
	.space 1
	.ascii "Long_extract_constant_count_61"
	.space 1
	.ascii "Long_extract_constant_count_60"
	.space 1
	.ascii "Long_extract_constant_count_59"
	.space 1
	.ascii "Long_extract_constant_count_58"
	.space 1
	.ascii "Long_extract_constant_count_57"
	.space 1
	.ascii "Long_extract_constant_count_56"
	.space 1
	.ascii "Long_extract_constant_count_55"
	.space 1
	.ascii "Long_extract_constant_count_54"
	.space 1
	.ascii "Long_extract_constant_count_53"
	.space 1
	.ascii "Long_extract_constant_count_52"
	.space 1
	.ascii "Long_extract_constant_count_51"
	.space 1
	.ascii "Long_extract_constant_count_50"
	.space 1
	.ascii "Long_extract_constant_count_49"
	.space 1
	.ascii "Long_extract_constant_count_48"
	.space 1
	.ascii "Long_extract_constant_count_47"
	.space 1
	.ascii "Long_extract_constant_count_46"
	.space 1
	.ascii "Long_extract_constant_count_45"
	.space 1
	.ascii "Long_extract_constant_count_44"
	.space 1
	.ascii "Long_extract_constant_count_43"
	.space 1
	.ascii "Long_extract_constant_count_42"
	.space 1
	.ascii "Long_extract_constant_count_41"
	.space 1
	.ascii "Long_extract_constant_count_40"
	.space 1
	.ascii "Long_extract_constant_count_39"
	.space 1
	.ascii "Long_extract_constant_count_38"
	.space 1
	.ascii "Long_extract_constant_count_37"
	.space 1
	.ascii "Long_extract_constant_count_36"
	.space 1
	.ascii "Long_extract_constant_count_35"
	.space 1
	.ascii "Long_extract_constant_count_34"
	.space 1
	.ascii "Long_extract_constant_count_33"
	.space 1
	.ascii "Long_extract_constant_count_32"
	.space 1
	.ascii "Long_extract_constant_count_31"
	.space 1
	.ascii "Long_extract_constant_count_30"
	.space 1
	.ascii "Long_extract_constant_count_29"
	.space 1
	.ascii "Long_extract_constant_count_28"
	.space 1
	.ascii "Long_extract_constant_count_27"
	.space 1
	.ascii "Long_extract_constant_count_26"
	.space 1
	.ascii "Long_extract_constant_count_25"
	.space 1
	.ascii "Long_extract_constant_count_24"
	.space 1
	.ascii "Long_extract_constant_count_23"
	.space 1
	.ascii "Long_extract_constant_count_22"
	.space 1
	.ascii "Long_extract_constant_count_21"
	.space 1
	.ascii "Long_extract_constant_count_20"
	.space 1
	.ascii "Long_extract_constant_count_19"
	.space 1
	.ascii "Long_extract_constant_count_18"
	.space 1
	.ascii "Long_extract_constant_count_17"
	.space 1
	.ascii "Long_extract_constant_count_16"
	.space 1
	.ascii "Long_extract_constant_count_15"
	.space 1
	.ascii "Long_extract_constant_count_14"
	.space 1
	.ascii "Long_extract_constant_count_13"
	.space 1
	.ascii "Long_extract_constant_count_12"
	.space 1
	.ascii "Long_extract_constant_count_11"
	.space 1
	.ascii "Long_extract_constant_count_10"
	.space 1
	.ascii "Long_extract_constant_count_9"
	.space 1
	.ascii "Long_extract_constant_count_8"
	.space 1
	.ascii "Long_extract_constant_count_7"
	.space 1
	.ascii "Long_extract_constant_count_6"
	.space 1
	.ascii "Long_extract_constant_count_5"
	.space 1
	.ascii "Long_extract_constant_count_4"
	.space 1
	.ascii "Long_extract_constant_count_3"
	.space 1
	.ascii "Long_extract_constant_count_2"
	.space 1
	.ascii "Long_extract_constant_count_1"
	.space 1
	.ascii "Long_extract_constant_count_0"
	.space 1
	.ascii "Word_extract_constant_count_63"
	.space 1
	.ascii "Word_extract_constant_count_62"
	.space 1
	.ascii "Word_extract_constant_count_61"
	.space 1
	.ascii "Word_extract_constant_count_60"
	.space 1
	.ascii "Word_extract_constant_count_59"
	.space 1
	.ascii "Word_extract_constant_count_58"
	.space 1
	.ascii "Word_extract_constant_count_57"
	.space 1
	.ascii "Word_extract_constant_count_56"
	.space 1
	.ascii "Word_extract_constant_count_55"
	.space 1
	.ascii "Word_extract_constant_count_54"
	.space 1
	.ascii "Word_extract_constant_count_53"
	.space 1
	.ascii "Word_extract_constant_count_52"
	.space 1
	.ascii "Word_extract_constant_count_51"
	.space 1
	.ascii "Word_extract_constant_count_50"
	.space 1
	.ascii "Word_extract_constant_count_49"
	.space 1
	.ascii "Word_extract_constant_count_48"
	.space 1
	.ascii "Word_extract_constant_count_47"
	.space 1
	.ascii "Word_extract_constant_count_46"
	.space 1
	.ascii "Word_extract_constant_count_45"
	.space 1
	.ascii "Word_extract_constant_count_44"
	.space 1
	.ascii "Word_extract_constant_count_43"
	.space 1
	.ascii "Word_extract_constant_count_42"
	.space 1
	.ascii "Word_extract_constant_count_41"
	.space 1
	.ascii "Word_extract_constant_count_40"
	.space 1
	.ascii "Word_extract_constant_count_39"
	.space 1
	.ascii "Word_extract_constant_count_38"
	.space 1
	.ascii "Word_extract_constant_count_37"
	.space 1
	.ascii "Word_extract_constant_count_36"
	.space 1
	.ascii "Word_extract_constant_count_35"
	.space 1
	.ascii "Word_extract_constant_count_34"
	.space 1
	.ascii "Word_extract_constant_count_33"
	.space 1
	.ascii "Word_extract_constant_count_32"
	.space 1
	.ascii "Word_extract_constant_count_31"
	.space 1
	.ascii "Word_extract_constant_count_30"
	.space 1
	.ascii "Word_extract_constant_count_29"
	.space 1
	.ascii "Word_extract_constant_count_28"
	.space 1
	.ascii "Word_extract_constant_count_27"
	.space 1
	.ascii "Word_extract_constant_count_26"
	.space 1
	.ascii "Word_extract_constant_count_25"
	.space 1
	.ascii "Word_extract_constant_count_24"
	.space 1
	.ascii "Word_extract_constant_count_23"
	.space 1
	.ascii "Word_extract_constant_count_22"
	.space 1
	.ascii "Word_extract_constant_count_21"
	.space 1
	.ascii "Word_extract_constant_count_20"
	.space 1
	.ascii "Word_extract_constant_count_19"
	.space 1
	.ascii "Word_extract_constant_count_18"
	.space 1
	.ascii "Word_extract_constant_count_17"
	.space 1
	.ascii "Word_extract_constant_count_16"
	.space 1
	.ascii "Word_extract_constant_count_15"
	.space 1
	.ascii "Word_extract_constant_count_14"
	.space 1
	.ascii "Word_extract_constant_count_13"
	.space 1
	.ascii "Word_extract_constant_count_12"
	.space 1
	.ascii "Word_extract_constant_count_11"
	.space 1
	.ascii "Word_extract_constant_count_10"
	.space 1
	.ascii "Word_extract_constant_count_9"
	.space 1
	.ascii "Word_extract_constant_count_8"
	.space 1
	.ascii "Word_extract_constant_count_7"
	.space 1
	.ascii "Word_extract_constant_count_6"
	.space 1
	.ascii "Word_extract_constant_count_5"
	.space 1
	.ascii "Word_extract_constant_count_4"
	.space 1
	.ascii "Word_extract_constant_count_3"
	.space 1
	.ascii "Word_extract_constant_count_2"
	.space 1
	.ascii "Word_extract_constant_count_1"
	.space 1
	.ascii "Word_extract_constant_count_0"
	.space 3
	.quad	_extract_constant_count_M3
	.quad	_L_1
	.quad	_extract_constant_count__Long_extract_constant_count_63
	.quad	_L_1+26
	.quad	_extract_constant_count__Long_extract_constant_count_62
	.quad	_L_1+57
	.quad	_extract_constant_count__Long_extract_constant_count_61
	.quad	_L_1+88
	.quad	_extract_constant_count__Long_extract_constant_count_60
	.quad	_L_1+119
	.quad	_extract_constant_count__Long_extract_constant_count_59
	.quad	_L_1+150
	.quad	_extract_constant_count__Long_extract_constant_count_58
	.quad	_L_1+181
	.quad	_extract_constant_count__Long_extract_constant_count_57
	.quad	_L_1+212
	.quad	_extract_constant_count__Long_extract_constant_count_56
	.quad	_L_1+243
	.quad	_extract_constant_count__Long_extract_constant_count_55
	.quad	_L_1+274
	.quad	_extract_constant_count__Long_extract_constant_count_54
	.quad	_L_1+305
	.quad	_extract_constant_count__Long_extract_constant_count_53
	.quad	_L_1+336
	.quad	_extract_constant_count__Long_extract_constant_count_52
	.quad	_L_1+367
	.quad	_extract_constant_count__Long_extract_constant_count_51
	.quad	_L_1+398
	.quad	_extract_constant_count__Long_extract_constant_count_50
	.quad	_L_1+429
	.quad	_extract_constant_count__Long_extract_constant_count_49
	.quad	_L_1+460
	.quad	_extract_constant_count__Long_extract_constant_count_48
	.quad	_L_1+491
	.quad	_extract_constant_count__Long_extract_constant_count_47
	.quad	_L_1+522
	.quad	_extract_constant_count__Long_extract_constant_count_46
	.quad	_L_1+553
	.quad	_extract_constant_count__Long_extract_constant_count_45
	.quad	_L_1+584
	.quad	_extract_constant_count__Long_extract_constant_count_44
	.quad	_L_1+615
	.quad	_extract_constant_count__Long_extract_constant_count_43
	.quad	_L_1+646
	.quad	_extract_constant_count__Long_extract_constant_count_42
	.quad	_L_1+677
	.quad	_extract_constant_count__Long_extract_constant_count_41
	.quad	_L_1+708
	.quad	_extract_constant_count__Long_extract_constant_count_40
	.quad	_L_1+739
	.quad	_extract_constant_count__Long_extract_constant_count_39
	.quad	_L_1+770
	.quad	_extract_constant_count__Long_extract_constant_count_38
	.quad	_L_1+801
	.quad	_extract_constant_count__Long_extract_constant_count_37
	.quad	_L_1+832
	.quad	_extract_constant_count__Long_extract_constant_count_36
	.quad	_L_1+863
	.quad	_extract_constant_count__Long_extract_constant_count_35
	.quad	_L_1+894
	.quad	_extract_constant_count__Long_extract_constant_count_34
	.quad	_L_1+925
	.quad	_extract_constant_count__Long_extract_constant_count_33
	.quad	_L_1+956
	.quad	_extract_constant_count__Long_extract_constant_count_32
	.quad	_L_1+987
	.quad	_extract_constant_count__Long_extract_constant_count_31
	.quad	_L_1+1018
	.quad	_extract_constant_count__Long_extract_constant_count_30
	.quad	_L_1+1049
	.quad	_extract_constant_count__Long_extract_constant_count_29
	.quad	_L_1+1080
	.quad	_extract_constant_count__Long_extract_constant_count_28
	.quad	_L_1+1111
	.quad	_extract_constant_count__Long_extract_constant_count_27
	.quad	_L_1+1142
	.quad	_extract_constant_count__Long_extract_constant_count_26
	.quad	_L_1+1173
	.quad	_extract_constant_count__Long_extract_constant_count_25
	.quad	_L_1+1204
	.quad	_extract_constant_count__Long_extract_constant_count_24
	.quad	_L_1+1235
	.quad	_extract_constant_count__Long_extract_constant_count_23
	.quad	_L_1+1266
	.quad	_extract_constant_count__Long_extract_constant_count_22
	.quad	_L_1+1297
	.quad	_extract_constant_count__Long_extract_constant_count_21
	.quad	_L_1+1328
	.quad	_extract_constant_count__Long_extract_constant_count_20
	.quad	_L_1+1359
	.quad	_extract_constant_count__Long_extract_constant_count_19
	.quad	_L_1+1390
	.quad	_extract_constant_count__Long_extract_constant_count_18
	.quad	_L_1+1421
	.quad	_extract_constant_count__Long_extract_constant_count_17
	.quad	_L_1+1452
	.quad	_extract_constant_count__Long_extract_constant_count_16
	.quad	_L_1+1483
	.quad	_extract_constant_count__Long_extract_constant_count_15
	.quad	_L_1+1514
	.quad	_extract_constant_count__Long_extract_constant_count_14
	.quad	_L_1+1545
	.quad	_extract_constant_count__Long_extract_constant_count_13
	.quad	_L_1+1576
	.quad	_extract_constant_count__Long_extract_constant_count_12
	.quad	_L_1+1607
	.quad	_extract_constant_count__Long_extract_constant_count_11
	.quad	_L_1+1638
	.quad	_extract_constant_count__Long_extract_constant_count_10
	.quad	_L_1+1669
	.quad	_extract_constant_count__Long_extract_constant_count_9
	.quad	_L_1+1700
	.quad	_extract_constant_count__Long_extract_constant_count_8
	.quad	_L_1+1730
	.quad	_extract_constant_count__Long_extract_constant_count_7
	.quad	_L_1+1760
	.quad	_extract_constant_count__Long_extract_constant_count_6
	.quad	_L_1+1790
	.quad	_extract_constant_count__Long_extract_constant_count_5
	.quad	_L_1+1820
	.quad	_extract_constant_count__Long_extract_constant_count_4
	.quad	_L_1+1850
	.quad	_extract_constant_count__Long_extract_constant_count_3
	.quad	_L_1+1880
	.quad	_extract_constant_count__Long_extract_constant_count_2
	.quad	_L_1+1910
	.quad	_extract_constant_count__Long_extract_constant_count_1
	.quad	_L_1+1940
	.quad	_extract_constant_count__Long_extract_constant_count_0
	.quad	_L_1+1970
	.quad	_extract_constant_count__Word_extract_constant_count_63
	.quad	_L_1+2000
	.quad	_extract_constant_count__Word_extract_constant_count_62
	.quad	_L_1+2031
	.quad	_extract_constant_count__Word_extract_constant_count_61
	.quad	_L_1+2062
	.quad	_extract_constant_count__Word_extract_constant_count_60
	.quad	_L_1+2093
	.quad	_extract_constant_count__Word_extract_constant_count_59
	.quad	_L_1+2124
	.quad	_extract_constant_count__Word_extract_constant_count_58
	.quad	_L_1+2155
	.quad	_extract_constant_count__Word_extract_constant_count_57
	.quad	_L_1+2186
	.quad	_extract_constant_count__Word_extract_constant_count_56
	.quad	_L_1+2217
	.quad	_extract_constant_count__Word_extract_constant_count_55
	.quad	_L_1+2248
	.quad	_extract_constant_count__Word_extract_constant_count_54
	.quad	_L_1+2279
	.quad	_extract_constant_count__Word_extract_constant_count_53
	.quad	_L_1+2310
	.quad	_extract_constant_count__Word_extract_constant_count_52
	.quad	_L_1+2341
	.quad	_extract_constant_count__Word_extract_constant_count_51
	.quad	_L_1+2372
	.quad	_extract_constant_count__Word_extract_constant_count_50
	.quad	_L_1+2403
	.quad	_extract_constant_count__Word_extract_constant_count_49
	.quad	_L_1+2434
	.quad	_extract_constant_count__Word_extract_constant_count_48
	.quad	_L_1+2465
	.quad	_extract_constant_count__Word_extract_constant_count_47
	.quad	_L_1+2496
	.quad	_extract_constant_count__Word_extract_constant_count_46
	.quad	_L_1+2527
	.quad	_extract_constant_count__Word_extract_constant_count_45
	.quad	_L_1+2558
	.quad	_extract_constant_count__Word_extract_constant_count_44
	.quad	_L_1+2589
	.quad	_extract_constant_count__Word_extract_constant_count_43
	.quad	_L_1+2620
	.quad	_extract_constant_count__Word_extract_constant_count_42
	.quad	_L_1+2651
	.quad	_extract_constant_count__Word_extract_constant_count_41
	.quad	_L_1+2682
	.quad	_extract_constant_count__Word_extract_constant_count_40
	.quad	_L_1+2713
	.quad	_extract_constant_count__Word_extract_constant_count_39
	.quad	_L_1+2744
	.quad	_extract_constant_count__Word_extract_constant_count_38
	.quad	_L_1+2775
	.quad	_extract_constant_count__Word_extract_constant_count_37
	.quad	_L_1+2806
	.quad	_extract_constant_count__Word_extract_constant_count_36
	.quad	_L_1+2837
	.quad	_extract_constant_count__Word_extract_constant_count_35
	.quad	_L_1+2868
	.quad	_extract_constant_count__Word_extract_constant_count_34
	.quad	_L_1+2899
	.quad	_extract_constant_count__Word_extract_constant_count_33
	.quad	_L_1+2930
	.quad	_extract_constant_count__Word_extract_constant_count_32
	.quad	_L_1+2961
	.quad	_extract_constant_count__Word_extract_constant_count_31
	.quad	_L_1+2992
	.quad	_extract_constant_count__Word_extract_constant_count_30
	.quad	_L_1+3023
	.quad	_extract_constant_count__Word_extract_constant_count_29
	.quad	_L_1+3054
	.quad	_extract_constant_count__Word_extract_constant_count_28
	.quad	_L_1+3085
	.quad	_extract_constant_count__Word_extract_constant_count_27
	.quad	_L_1+3116
	.quad	_extract_constant_count__Word_extract_constant_count_26
	.quad	_L_1+3147
	.quad	_extract_constant_count__Word_extract_constant_count_25
	.quad	_L_1+3178
	.quad	_extract_constant_count__Word_extract_constant_count_24
	.quad	_L_1+3209
	.quad	_extract_constant_count__Word_extract_constant_count_23
	.quad	_L_1+3240
	.quad	_extract_constant_count__Word_extract_constant_count_22
	.quad	_L_1+3271
	.quad	_extract_constant_count__Word_extract_constant_count_21
	.quad	_L_1+3302
	.quad	_extract_constant_count__Word_extract_constant_count_20
	.quad	_L_1+3333
	.quad	_extract_constant_count__Word_extract_constant_count_19
	.quad	_L_1+3364
	.quad	_extract_constant_count__Word_extract_constant_count_18
	.quad	_L_1+3395
	.quad	_extract_constant_count__Word_extract_constant_count_17
	.quad	_L_1+3426
	.quad	_extract_constant_count__Word_extract_constant_count_16
	.quad	_L_1+3457
	.quad	_extract_constant_count__Word_extract_constant_count_15
	.quad	_L_1+3488
	.quad	_extract_constant_count__Word_extract_constant_count_14
	.quad	_L_1+3519
	.quad	_extract_constant_count__Word_extract_constant_count_13
	.quad	_L_1+3550
	.quad	_extract_constant_count__Word_extract_constant_count_12
	.quad	_L_1+3581
	.quad	_extract_constant_count__Word_extract_constant_count_11
	.quad	_L_1+3612
	.quad	_extract_constant_count__Word_extract_constant_count_10
	.quad	_L_1+3643
	.quad	_extract_constant_count__Word_extract_constant_count_9
	.quad	_L_1+3674
	.quad	_extract_constant_count__Word_extract_constant_count_8
	.quad	_L_1+3704
	.quad	_extract_constant_count__Word_extract_constant_count_7
	.quad	_L_1+3734
	.quad	_extract_constant_count__Word_extract_constant_count_6
	.quad	_L_1+3764
	.quad	_extract_constant_count__Word_extract_constant_count_5
	.quad	_L_1+3794
	.quad	_extract_constant_count__Word_extract_constant_count_4
	.quad	_L_1+3824
	.quad	_extract_constant_count__Word_extract_constant_count_3
	.quad	_L_1+3854
	.quad	_extract_constant_count__Word_extract_constant_count_2
	.quad	_L_1+3884
	.quad	_extract_constant_count__Word_extract_constant_count_1
	.quad	_L_1+3914
	.quad	_extract_constant_count__Word_extract_constant_count_0
	.quad	_L_1+3944
	.space 8
	.ascii "../AMD64_DARWIN/extract_constant_count.m3"
	.space 7
	.data
	.align 5
_MM_extract_constant_count:
	.quad	_L_1+6048
	.space 32
	.quad	_L_1+3976
	.space 24
	.quad	_MM_extract_constant_count+208
	.space 8
	.quad	_extract_constant_count_M3
	.quad	3
	.quad	1105
	.quad	1106
	.long	1149920140
	.byte	85
	.space 3
	.quad	1110
	.byte	87
	.space 3
	.long	1112
	.quad	1113
	.word	1114
	.space 6
	.quad	1115
	.long	1635523546
	.long	1083273330
	.quad	1118
	.word	1119
	.space 2
	.long	1120
	.space 24
	.quad	_extract_constant_count_I3
	.quad	_MM_extract_constant_count+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_extract_constant_count+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_extract_constant_count+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_extract_constant_count+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
