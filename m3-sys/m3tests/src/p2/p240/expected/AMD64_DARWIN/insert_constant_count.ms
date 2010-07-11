	.text
.globl _insert_constant_count__Word_insert_constant_count_0
	.private_extern _insert_constant_count__Word_insert_constant_count_0
_insert_constant_count__Word_insert_constant_count_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L2
	movl	$1761, %edi
	call	__m3_fault
L2:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_1
	.private_extern _insert_constant_count__Word_insert_constant_count_1
_insert_constant_count__Word_insert_constant_count_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L5
	movl	$1825, %edi
	call	__m3_fault
L5:
	movq	-16(%rbp), %rcx
	movl	$1, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_2
	.private_extern _insert_constant_count__Word_insert_constant_count_2
_insert_constant_count__Word_insert_constant_count_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$62, -16(%rbp)
	jle	L8
	movl	$1889, %edi
	call	__m3_fault
L8:
	movq	-16(%rbp), %rcx
	movl	$3, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$3, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_3
	.private_extern _insert_constant_count__Word_insert_constant_count_3
_insert_constant_count__Word_insert_constant_count_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$61, -16(%rbp)
	jle	L11
	movl	$1953, %edi
	call	__m3_fault
L11:
	movq	-16(%rbp), %rcx
	movl	$7, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$7, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_4
	.private_extern _insert_constant_count__Word_insert_constant_count_4
_insert_constant_count__Word_insert_constant_count_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$60, -16(%rbp)
	jle	L14
	movl	$2017, %edi
	call	__m3_fault
L14:
	movq	-16(%rbp), %rcx
	movl	$15, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$15, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_5
	.private_extern _insert_constant_count__Word_insert_constant_count_5
_insert_constant_count__Word_insert_constant_count_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$59, -16(%rbp)
	jle	L17
	movl	$2081, %edi
	call	__m3_fault
L17:
	movq	-16(%rbp), %rcx
	movl	$31, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$31, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_6
	.private_extern _insert_constant_count__Word_insert_constant_count_6
_insert_constant_count__Word_insert_constant_count_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$58, -16(%rbp)
	jle	L20
	movl	$2145, %edi
	call	__m3_fault
L20:
	movq	-16(%rbp), %rcx
	movl	$63, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_7
	.private_extern _insert_constant_count__Word_insert_constant_count_7
_insert_constant_count__Word_insert_constant_count_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$57, -16(%rbp)
	jle	L23
	movl	$2209, %edi
	call	__m3_fault
L23:
	movq	-16(%rbp), %rcx
	movl	$127, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$127, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_8
	.private_extern _insert_constant_count__Word_insert_constant_count_8
_insert_constant_count__Word_insert_constant_count_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$56, -16(%rbp)
	jle	L26
	movl	$2273, %edi
	call	__m3_fault
L26:
	movq	-16(%rbp), %rcx
	movl	$255, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$255, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_9
	.private_extern _insert_constant_count__Word_insert_constant_count_9
_insert_constant_count__Word_insert_constant_count_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$55, -16(%rbp)
	jle	L29
	movl	$2337, %edi
	call	__m3_fault
L29:
	movq	-16(%rbp), %rcx
	movl	$511, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$511, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_10
	.private_extern _insert_constant_count__Word_insert_constant_count_10
_insert_constant_count__Word_insert_constant_count_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$54, -16(%rbp)
	jle	L32
	movl	$2401, %edi
	call	__m3_fault
L32:
	movq	-16(%rbp), %rcx
	movl	$1023, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1023, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_11
	.private_extern _insert_constant_count__Word_insert_constant_count_11
_insert_constant_count__Word_insert_constant_count_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$53, -16(%rbp)
	jle	L35
	movl	$2465, %edi
	call	__m3_fault
L35:
	movq	-16(%rbp), %rcx
	movl	$2047, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$2047, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_12
	.private_extern _insert_constant_count__Word_insert_constant_count_12
_insert_constant_count__Word_insert_constant_count_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$52, -16(%rbp)
	jle	L38
	movl	$2529, %edi
	call	__m3_fault
L38:
	movq	-16(%rbp), %rcx
	movl	$4095, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$4095, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_13
	.private_extern _insert_constant_count__Word_insert_constant_count_13
_insert_constant_count__Word_insert_constant_count_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$51, -16(%rbp)
	jle	L41
	movl	$2593, %edi
	call	__m3_fault
L41:
	movq	-16(%rbp), %rcx
	movl	$8191, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$8191, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_14
	.private_extern _insert_constant_count__Word_insert_constant_count_14
_insert_constant_count__Word_insert_constant_count_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$50, -16(%rbp)
	jle	L44
	movl	$2657, %edi
	call	__m3_fault
L44:
	movq	-16(%rbp), %rcx
	movl	$16383, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$16383, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_15
	.private_extern _insert_constant_count__Word_insert_constant_count_15
_insert_constant_count__Word_insert_constant_count_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$49, -16(%rbp)
	jle	L47
	movl	$2721, %edi
	call	__m3_fault
L47:
	movq	-16(%rbp), %rcx
	movl	$32767, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$32767, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_16
	.private_extern _insert_constant_count__Word_insert_constant_count_16
_insert_constant_count__Word_insert_constant_count_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$48, -16(%rbp)
	jle	L50
	movl	$2785, %edi
	call	__m3_fault
L50:
	movq	-16(%rbp), %rcx
	movl	$65535, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$65535, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_17
	.private_extern _insert_constant_count__Word_insert_constant_count_17
_insert_constant_count__Word_insert_constant_count_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$47, -16(%rbp)
	jle	L53
	movl	$2849, %edi
	call	__m3_fault
L53:
	movq	-16(%rbp), %rcx
	movl	$131071, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$131071, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_18
	.private_extern _insert_constant_count__Word_insert_constant_count_18
_insert_constant_count__Word_insert_constant_count_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$46, -16(%rbp)
	jle	L56
	movl	$2913, %edi
	call	__m3_fault
L56:
	movq	-16(%rbp), %rcx
	movl	$262143, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$262143, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_19
	.private_extern _insert_constant_count__Word_insert_constant_count_19
_insert_constant_count__Word_insert_constant_count_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$45, -16(%rbp)
	jle	L59
	movl	$2977, %edi
	call	__m3_fault
L59:
	movq	-16(%rbp), %rcx
	movl	$524287, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$524287, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_20
	.private_extern _insert_constant_count__Word_insert_constant_count_20
_insert_constant_count__Word_insert_constant_count_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$44, -16(%rbp)
	jle	L62
	movl	$3041, %edi
	call	__m3_fault
L62:
	movq	-16(%rbp), %rcx
	movl	$1048575, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1048575, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_21
	.private_extern _insert_constant_count__Word_insert_constant_count_21
_insert_constant_count__Word_insert_constant_count_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$43, -16(%rbp)
	jle	L65
	movl	$3105, %edi
	call	__m3_fault
L65:
	movq	-16(%rbp), %rcx
	movl	$2097151, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$2097151, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_22
	.private_extern _insert_constant_count__Word_insert_constant_count_22
_insert_constant_count__Word_insert_constant_count_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$42, -16(%rbp)
	jle	L68
	movl	$3169, %edi
	call	__m3_fault
L68:
	movq	-16(%rbp), %rcx
	movl	$4194303, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$4194303, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_23
	.private_extern _insert_constant_count__Word_insert_constant_count_23
_insert_constant_count__Word_insert_constant_count_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$41, -16(%rbp)
	jle	L71
	movl	$3233, %edi
	call	__m3_fault
L71:
	movq	-16(%rbp), %rcx
	movl	$8388607, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$8388607, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_24
	.private_extern _insert_constant_count__Word_insert_constant_count_24
_insert_constant_count__Word_insert_constant_count_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$40, -16(%rbp)
	jle	L74
	movl	$3297, %edi
	call	__m3_fault
L74:
	movq	-16(%rbp), %rcx
	movl	$16777215, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$16777215, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_25
	.private_extern _insert_constant_count__Word_insert_constant_count_25
_insert_constant_count__Word_insert_constant_count_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$39, -16(%rbp)
	jle	L77
	movl	$3361, %edi
	call	__m3_fault
L77:
	movq	-16(%rbp), %rcx
	movl	$33554431, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$33554431, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_26
	.private_extern _insert_constant_count__Word_insert_constant_count_26
_insert_constant_count__Word_insert_constant_count_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$38, -16(%rbp)
	jle	L80
	movl	$3425, %edi
	call	__m3_fault
L80:
	movq	-16(%rbp), %rcx
	movl	$67108863, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$67108863, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_27
	.private_extern _insert_constant_count__Word_insert_constant_count_27
_insert_constant_count__Word_insert_constant_count_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$37, -16(%rbp)
	jle	L83
	movl	$3489, %edi
	call	__m3_fault
L83:
	movq	-16(%rbp), %rcx
	movl	$134217727, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$134217727, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_28
	.private_extern _insert_constant_count__Word_insert_constant_count_28
_insert_constant_count__Word_insert_constant_count_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$36, -16(%rbp)
	jle	L86
	movl	$3553, %edi
	call	__m3_fault
L86:
	movq	-16(%rbp), %rcx
	movl	$268435455, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$268435455, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_29
	.private_extern _insert_constant_count__Word_insert_constant_count_29
_insert_constant_count__Word_insert_constant_count_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$35, -16(%rbp)
	jle	L89
	movl	$3617, %edi
	call	__m3_fault
L89:
	movq	-16(%rbp), %rcx
	movl	$536870911, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$536870911, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_30
	.private_extern _insert_constant_count__Word_insert_constant_count_30
_insert_constant_count__Word_insert_constant_count_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$34, -16(%rbp)
	jle	L92
	movl	$3681, %edi
	call	__m3_fault
L92:
	movq	-16(%rbp), %rcx
	movl	$1073741823, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1073741823, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_31
	.private_extern _insert_constant_count__Word_insert_constant_count_31
_insert_constant_count__Word_insert_constant_count_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$33, -16(%rbp)
	jle	L95
	movl	$3745, %edi
	call	__m3_fault
L95:
	movq	-16(%rbp), %rcx
	movl	$2147483647, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$2147483647, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_32
	.private_extern _insert_constant_count__Word_insert_constant_count_32
_insert_constant_count__Word_insert_constant_count_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32, -16(%rbp)
	jle	L98
	movl	$3809, %edi
	call	__m3_fault
L98:
	movq	-16(%rbp), %rcx
	movl	$4294967295, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$4294967295, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_33
	.private_extern _insert_constant_count__Word_insert_constant_count_33
_insert_constant_count__Word_insert_constant_count_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$31, -16(%rbp)
	jle	L101
	movl	$3873, %edi
	call	__m3_fault
L101:
	movq	-16(%rbp), %rcx
	movabsq	$8589934591, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$8589934591, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_34
	.private_extern _insert_constant_count__Word_insert_constant_count_34
_insert_constant_count__Word_insert_constant_count_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$30, -16(%rbp)
	jle	L104
	movl	$3937, %edi
	call	__m3_fault
L104:
	movq	-16(%rbp), %rcx
	movabsq	$17179869183, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$17179869183, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_35
	.private_extern _insert_constant_count__Word_insert_constant_count_35
_insert_constant_count__Word_insert_constant_count_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$29, -16(%rbp)
	jle	L107
	movl	$4001, %edi
	call	__m3_fault
L107:
	movq	-16(%rbp), %rcx
	movabsq	$34359738367, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$34359738367, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_36
	.private_extern _insert_constant_count__Word_insert_constant_count_36
_insert_constant_count__Word_insert_constant_count_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$28, -16(%rbp)
	jle	L110
	movl	$4065, %edi
	call	__m3_fault
L110:
	movq	-16(%rbp), %rcx
	movabsq	$68719476735, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$68719476735, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_37
	.private_extern _insert_constant_count__Word_insert_constant_count_37
_insert_constant_count__Word_insert_constant_count_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$27, -16(%rbp)
	jle	L113
	movl	$4129, %edi
	call	__m3_fault
L113:
	movq	-16(%rbp), %rcx
	movabsq	$137438953471, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$137438953471, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_38
	.private_extern _insert_constant_count__Word_insert_constant_count_38
_insert_constant_count__Word_insert_constant_count_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$26, -16(%rbp)
	jle	L116
	movl	$4193, %edi
	call	__m3_fault
L116:
	movq	-16(%rbp), %rcx
	movabsq	$274877906943, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$274877906943, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_39
	.private_extern _insert_constant_count__Word_insert_constant_count_39
_insert_constant_count__Word_insert_constant_count_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$25, -16(%rbp)
	jle	L119
	movl	$4257, %edi
	call	__m3_fault
L119:
	movq	-16(%rbp), %rcx
	movabsq	$549755813887, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$549755813887, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_40
	.private_extern _insert_constant_count__Word_insert_constant_count_40
_insert_constant_count__Word_insert_constant_count_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$24, -16(%rbp)
	jle	L122
	movl	$4321, %edi
	call	__m3_fault
L122:
	movq	-16(%rbp), %rcx
	movabsq	$1099511627775, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$1099511627775, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_41
	.private_extern _insert_constant_count__Word_insert_constant_count_41
_insert_constant_count__Word_insert_constant_count_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$23, -16(%rbp)
	jle	L125
	movl	$4385, %edi
	call	__m3_fault
L125:
	movq	-16(%rbp), %rcx
	movabsq	$2199023255551, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$2199023255551, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_42
	.private_extern _insert_constant_count__Word_insert_constant_count_42
_insert_constant_count__Word_insert_constant_count_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$22, -16(%rbp)
	jle	L128
	movl	$4449, %edi
	call	__m3_fault
L128:
	movq	-16(%rbp), %rcx
	movabsq	$4398046511103, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$4398046511103, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_43
	.private_extern _insert_constant_count__Word_insert_constant_count_43
_insert_constant_count__Word_insert_constant_count_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$21, -16(%rbp)
	jle	L131
	movl	$4513, %edi
	call	__m3_fault
L131:
	movq	-16(%rbp), %rcx
	movabsq	$8796093022207, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$8796093022207, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_44
	.private_extern _insert_constant_count__Word_insert_constant_count_44
_insert_constant_count__Word_insert_constant_count_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$20, -16(%rbp)
	jle	L134
	movl	$4577, %edi
	call	__m3_fault
L134:
	movq	-16(%rbp), %rcx
	movabsq	$17592186044415, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$17592186044415, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_45
	.private_extern _insert_constant_count__Word_insert_constant_count_45
_insert_constant_count__Word_insert_constant_count_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$19, -16(%rbp)
	jle	L137
	movl	$4641, %edi
	call	__m3_fault
L137:
	movq	-16(%rbp), %rcx
	movabsq	$35184372088831, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$35184372088831, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_46
	.private_extern _insert_constant_count__Word_insert_constant_count_46
_insert_constant_count__Word_insert_constant_count_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$18, -16(%rbp)
	jle	L140
	movl	$4705, %edi
	call	__m3_fault
L140:
	movq	-16(%rbp), %rcx
	movabsq	$70368744177663, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$70368744177663, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_47
	.private_extern _insert_constant_count__Word_insert_constant_count_47
_insert_constant_count__Word_insert_constant_count_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$17, -16(%rbp)
	jle	L143
	movl	$4769, %edi
	call	__m3_fault
L143:
	movq	-16(%rbp), %rcx
	movabsq	$140737488355327, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$140737488355327, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_48
	.private_extern _insert_constant_count__Word_insert_constant_count_48
_insert_constant_count__Word_insert_constant_count_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$16, -16(%rbp)
	jle	L146
	movl	$4833, %edi
	call	__m3_fault
L146:
	movq	-16(%rbp), %rcx
	movabsq	$281474976710655, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$281474976710655, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_49
	.private_extern _insert_constant_count__Word_insert_constant_count_49
_insert_constant_count__Word_insert_constant_count_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$15, -16(%rbp)
	jle	L149
	movl	$4897, %edi
	call	__m3_fault
L149:
	movq	-16(%rbp), %rcx
	movabsq	$562949953421311, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$562949953421311, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_50
	.private_extern _insert_constant_count__Word_insert_constant_count_50
_insert_constant_count__Word_insert_constant_count_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$14, -16(%rbp)
	jle	L152
	movl	$4961, %edi
	call	__m3_fault
L152:
	movq	-16(%rbp), %rcx
	movabsq	$1125899906842623, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$1125899906842623, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_51
	.private_extern _insert_constant_count__Word_insert_constant_count_51
_insert_constant_count__Word_insert_constant_count_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$13, -16(%rbp)
	jle	L155
	movl	$5025, %edi
	call	__m3_fault
L155:
	movq	-16(%rbp), %rcx
	movabsq	$2251799813685247, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$2251799813685247, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_52
	.private_extern _insert_constant_count__Word_insert_constant_count_52
_insert_constant_count__Word_insert_constant_count_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$12, -16(%rbp)
	jle	L158
	movl	$5089, %edi
	call	__m3_fault
L158:
	movq	-16(%rbp), %rcx
	movabsq	$4503599627370495, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$4503599627370495, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_53
	.private_extern _insert_constant_count__Word_insert_constant_count_53
_insert_constant_count__Word_insert_constant_count_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$11, -16(%rbp)
	jle	L161
	movl	$5153, %edi
	call	__m3_fault
L161:
	movq	-16(%rbp), %rcx
	movabsq	$9007199254740991, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$9007199254740991, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_54
	.private_extern _insert_constant_count__Word_insert_constant_count_54
_insert_constant_count__Word_insert_constant_count_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$10, -16(%rbp)
	jle	L164
	movl	$5217, %edi
	call	__m3_fault
L164:
	movq	-16(%rbp), %rcx
	movabsq	$18014398509481983, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$18014398509481983, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_55
	.private_extern _insert_constant_count__Word_insert_constant_count_55
_insert_constant_count__Word_insert_constant_count_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$9, -16(%rbp)
	jle	L167
	movl	$5281, %edi
	call	__m3_fault
L167:
	movq	-16(%rbp), %rcx
	movabsq	$36028797018963967, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$36028797018963967, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_56
	.private_extern _insert_constant_count__Word_insert_constant_count_56
_insert_constant_count__Word_insert_constant_count_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$8, -16(%rbp)
	jle	L170
	movl	$5345, %edi
	call	__m3_fault
L170:
	movq	-16(%rbp), %rcx
	movabsq	$72057594037927935, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$72057594037927935, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_57
	.private_extern _insert_constant_count__Word_insert_constant_count_57
_insert_constant_count__Word_insert_constant_count_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$7, -16(%rbp)
	jle	L173
	movl	$5409, %edi
	call	__m3_fault
L173:
	movq	-16(%rbp), %rcx
	movabsq	$144115188075855871, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$144115188075855871, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_58
	.private_extern _insert_constant_count__Word_insert_constant_count_58
_insert_constant_count__Word_insert_constant_count_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$6, -16(%rbp)
	jle	L176
	movl	$5473, %edi
	call	__m3_fault
L176:
	movq	-16(%rbp), %rcx
	movabsq	$288230376151711743, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$288230376151711743, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_59
	.private_extern _insert_constant_count__Word_insert_constant_count_59
_insert_constant_count__Word_insert_constant_count_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$5, -16(%rbp)
	jle	L179
	movl	$5537, %edi
	call	__m3_fault
L179:
	movq	-16(%rbp), %rcx
	movabsq	$576460752303423487, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$576460752303423487, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_60
	.private_extern _insert_constant_count__Word_insert_constant_count_60
_insert_constant_count__Word_insert_constant_count_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$4, -16(%rbp)
	jle	L182
	movl	$5601, %edi
	call	__m3_fault
L182:
	movq	-16(%rbp), %rcx
	movabsq	$1152921504606846975, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$1152921504606846975, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_61
	.private_extern _insert_constant_count__Word_insert_constant_count_61
_insert_constant_count__Word_insert_constant_count_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$3, -16(%rbp)
	jle	L185
	movl	$5665, %edi
	call	__m3_fault
L185:
	movq	-16(%rbp), %rcx
	movabsq	$2305843009213693951, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$2305843009213693951, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_62
	.private_extern _insert_constant_count__Word_insert_constant_count_62
_insert_constant_count__Word_insert_constant_count_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2, -16(%rbp)
	jle	L188
	movl	$5729, %edi
	call	__m3_fault
L188:
	movq	-16(%rbp), %rcx
	movabsq	$4611686018427387903, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$4611686018427387903, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Word_insert_constant_count_63
	.private_extern _insert_constant_count__Word_insert_constant_count_63
_insert_constant_count__Word_insert_constant_count_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$1, -16(%rbp)
	jle	L191
	movl	$5793, %edi
	call	__m3_fault
L191:
	movq	-16(%rbp), %rcx
	movabsq	$9223372036854775807, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$9223372036854775807, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_0
	.private_extern _insert_constant_count__Long_insert_constant_count_0
_insert_constant_count__Long_insert_constant_count_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L194
	movl	$5857, %edi
	call	__m3_fault
L194:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_1
	.private_extern _insert_constant_count__Long_insert_constant_count_1
_insert_constant_count__Long_insert_constant_count_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L197
	movl	$5921, %edi
	call	__m3_fault
L197:
	movq	-16(%rbp), %rcx
	movl	$1, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_2
	.private_extern _insert_constant_count__Long_insert_constant_count_2
_insert_constant_count__Long_insert_constant_count_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$62, -16(%rbp)
	jle	L200
	movl	$5985, %edi
	call	__m3_fault
L200:
	movq	-16(%rbp), %rcx
	movl	$3, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$3, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_3
	.private_extern _insert_constant_count__Long_insert_constant_count_3
_insert_constant_count__Long_insert_constant_count_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$61, -16(%rbp)
	jle	L203
	movl	$6049, %edi
	call	__m3_fault
L203:
	movq	-16(%rbp), %rcx
	movl	$7, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$7, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_4
	.private_extern _insert_constant_count__Long_insert_constant_count_4
_insert_constant_count__Long_insert_constant_count_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$60, -16(%rbp)
	jle	L206
	movl	$6113, %edi
	call	__m3_fault
L206:
	movq	-16(%rbp), %rcx
	movl	$15, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$15, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_5
	.private_extern _insert_constant_count__Long_insert_constant_count_5
_insert_constant_count__Long_insert_constant_count_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$59, -16(%rbp)
	jle	L209
	movl	$6177, %edi
	call	__m3_fault
L209:
	movq	-16(%rbp), %rcx
	movl	$31, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$31, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_6
	.private_extern _insert_constant_count__Long_insert_constant_count_6
_insert_constant_count__Long_insert_constant_count_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$58, -16(%rbp)
	jle	L212
	movl	$6241, %edi
	call	__m3_fault
L212:
	movq	-16(%rbp), %rcx
	movl	$63, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_7
	.private_extern _insert_constant_count__Long_insert_constant_count_7
_insert_constant_count__Long_insert_constant_count_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$57, -16(%rbp)
	jle	L215
	movl	$6305, %edi
	call	__m3_fault
L215:
	movq	-16(%rbp), %rcx
	movl	$127, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$127, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_8
	.private_extern _insert_constant_count__Long_insert_constant_count_8
_insert_constant_count__Long_insert_constant_count_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$56, -16(%rbp)
	jle	L218
	movl	$6369, %edi
	call	__m3_fault
L218:
	movq	-16(%rbp), %rcx
	movl	$255, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$255, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_9
	.private_extern _insert_constant_count__Long_insert_constant_count_9
_insert_constant_count__Long_insert_constant_count_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$55, -16(%rbp)
	jle	L221
	movl	$6433, %edi
	call	__m3_fault
L221:
	movq	-16(%rbp), %rcx
	movl	$511, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$511, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_10
	.private_extern _insert_constant_count__Long_insert_constant_count_10
_insert_constant_count__Long_insert_constant_count_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$54, -16(%rbp)
	jle	L224
	movl	$6497, %edi
	call	__m3_fault
L224:
	movq	-16(%rbp), %rcx
	movl	$1023, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1023, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_11
	.private_extern _insert_constant_count__Long_insert_constant_count_11
_insert_constant_count__Long_insert_constant_count_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$53, -16(%rbp)
	jle	L227
	movl	$6561, %edi
	call	__m3_fault
L227:
	movq	-16(%rbp), %rcx
	movl	$2047, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$2047, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_12
	.private_extern _insert_constant_count__Long_insert_constant_count_12
_insert_constant_count__Long_insert_constant_count_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$52, -16(%rbp)
	jle	L230
	movl	$6625, %edi
	call	__m3_fault
L230:
	movq	-16(%rbp), %rcx
	movl	$4095, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$4095, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_13
	.private_extern _insert_constant_count__Long_insert_constant_count_13
_insert_constant_count__Long_insert_constant_count_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$51, -16(%rbp)
	jle	L233
	movl	$6689, %edi
	call	__m3_fault
L233:
	movq	-16(%rbp), %rcx
	movl	$8191, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$8191, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_14
	.private_extern _insert_constant_count__Long_insert_constant_count_14
_insert_constant_count__Long_insert_constant_count_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$50, -16(%rbp)
	jle	L236
	movl	$6753, %edi
	call	__m3_fault
L236:
	movq	-16(%rbp), %rcx
	movl	$16383, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$16383, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_15
	.private_extern _insert_constant_count__Long_insert_constant_count_15
_insert_constant_count__Long_insert_constant_count_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$49, -16(%rbp)
	jle	L239
	movl	$6817, %edi
	call	__m3_fault
L239:
	movq	-16(%rbp), %rcx
	movl	$32767, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$32767, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_16
	.private_extern _insert_constant_count__Long_insert_constant_count_16
_insert_constant_count__Long_insert_constant_count_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$48, -16(%rbp)
	jle	L242
	movl	$6881, %edi
	call	__m3_fault
L242:
	movq	-16(%rbp), %rcx
	movl	$65535, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$65535, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_17
	.private_extern _insert_constant_count__Long_insert_constant_count_17
_insert_constant_count__Long_insert_constant_count_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$47, -16(%rbp)
	jle	L245
	movl	$6945, %edi
	call	__m3_fault
L245:
	movq	-16(%rbp), %rcx
	movl	$131071, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$131071, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_18
	.private_extern _insert_constant_count__Long_insert_constant_count_18
_insert_constant_count__Long_insert_constant_count_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$46, -16(%rbp)
	jle	L248
	movl	$7009, %edi
	call	__m3_fault
L248:
	movq	-16(%rbp), %rcx
	movl	$262143, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$262143, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_19
	.private_extern _insert_constant_count__Long_insert_constant_count_19
_insert_constant_count__Long_insert_constant_count_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$45, -16(%rbp)
	jle	L251
	movl	$7073, %edi
	call	__m3_fault
L251:
	movq	-16(%rbp), %rcx
	movl	$524287, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$524287, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_20
	.private_extern _insert_constant_count__Long_insert_constant_count_20
_insert_constant_count__Long_insert_constant_count_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$44, -16(%rbp)
	jle	L254
	movl	$7137, %edi
	call	__m3_fault
L254:
	movq	-16(%rbp), %rcx
	movl	$1048575, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1048575, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_21
	.private_extern _insert_constant_count__Long_insert_constant_count_21
_insert_constant_count__Long_insert_constant_count_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$43, -16(%rbp)
	jle	L257
	movl	$7201, %edi
	call	__m3_fault
L257:
	movq	-16(%rbp), %rcx
	movl	$2097151, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$2097151, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_22
	.private_extern _insert_constant_count__Long_insert_constant_count_22
_insert_constant_count__Long_insert_constant_count_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$42, -16(%rbp)
	jle	L260
	movl	$7265, %edi
	call	__m3_fault
L260:
	movq	-16(%rbp), %rcx
	movl	$4194303, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$4194303, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_23
	.private_extern _insert_constant_count__Long_insert_constant_count_23
_insert_constant_count__Long_insert_constant_count_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$41, -16(%rbp)
	jle	L263
	movl	$7329, %edi
	call	__m3_fault
L263:
	movq	-16(%rbp), %rcx
	movl	$8388607, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$8388607, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_24
	.private_extern _insert_constant_count__Long_insert_constant_count_24
_insert_constant_count__Long_insert_constant_count_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$40, -16(%rbp)
	jle	L266
	movl	$7393, %edi
	call	__m3_fault
L266:
	movq	-16(%rbp), %rcx
	movl	$16777215, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$16777215, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_25
	.private_extern _insert_constant_count__Long_insert_constant_count_25
_insert_constant_count__Long_insert_constant_count_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$39, -16(%rbp)
	jle	L269
	movl	$7457, %edi
	call	__m3_fault
L269:
	movq	-16(%rbp), %rcx
	movl	$33554431, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$33554431, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_26
	.private_extern _insert_constant_count__Long_insert_constant_count_26
_insert_constant_count__Long_insert_constant_count_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$38, -16(%rbp)
	jle	L272
	movl	$7521, %edi
	call	__m3_fault
L272:
	movq	-16(%rbp), %rcx
	movl	$67108863, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$67108863, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_27
	.private_extern _insert_constant_count__Long_insert_constant_count_27
_insert_constant_count__Long_insert_constant_count_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$37, -16(%rbp)
	jle	L275
	movl	$7585, %edi
	call	__m3_fault
L275:
	movq	-16(%rbp), %rcx
	movl	$134217727, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$134217727, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_28
	.private_extern _insert_constant_count__Long_insert_constant_count_28
_insert_constant_count__Long_insert_constant_count_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$36, -16(%rbp)
	jle	L278
	movl	$7649, %edi
	call	__m3_fault
L278:
	movq	-16(%rbp), %rcx
	movl	$268435455, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$268435455, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_29
	.private_extern _insert_constant_count__Long_insert_constant_count_29
_insert_constant_count__Long_insert_constant_count_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$35, -16(%rbp)
	jle	L281
	movl	$7713, %edi
	call	__m3_fault
L281:
	movq	-16(%rbp), %rcx
	movl	$536870911, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$536870911, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_30
	.private_extern _insert_constant_count__Long_insert_constant_count_30
_insert_constant_count__Long_insert_constant_count_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$34, -16(%rbp)
	jle	L284
	movl	$7777, %edi
	call	__m3_fault
L284:
	movq	-16(%rbp), %rcx
	movl	$1073741823, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$1073741823, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_31
	.private_extern _insert_constant_count__Long_insert_constant_count_31
_insert_constant_count__Long_insert_constant_count_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$33, -16(%rbp)
	jle	L287
	movl	$7841, %edi
	call	__m3_fault
L287:
	movq	-16(%rbp), %rcx
	movl	$2147483647, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$2147483647, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_32
	.private_extern _insert_constant_count__Long_insert_constant_count_32
_insert_constant_count__Long_insert_constant_count_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32, -16(%rbp)
	jle	L290
	movl	$7905, %edi
	call	__m3_fault
L290:
	movq	-16(%rbp), %rcx
	movl	$4294967295, %eax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$4294967295, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_33
	.private_extern _insert_constant_count__Long_insert_constant_count_33
_insert_constant_count__Long_insert_constant_count_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$31, -16(%rbp)
	jle	L293
	movl	$7969, %edi
	call	__m3_fault
L293:
	movq	-16(%rbp), %rcx
	movabsq	$8589934591, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$8589934591, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_34
	.private_extern _insert_constant_count__Long_insert_constant_count_34
_insert_constant_count__Long_insert_constant_count_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$30, -16(%rbp)
	jle	L296
	movl	$8033, %edi
	call	__m3_fault
L296:
	movq	-16(%rbp), %rcx
	movabsq	$17179869183, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$17179869183, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_35
	.private_extern _insert_constant_count__Long_insert_constant_count_35
_insert_constant_count__Long_insert_constant_count_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$29, -16(%rbp)
	jle	L299
	movl	$8097, %edi
	call	__m3_fault
L299:
	movq	-16(%rbp), %rcx
	movabsq	$34359738367, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$34359738367, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_36
	.private_extern _insert_constant_count__Long_insert_constant_count_36
_insert_constant_count__Long_insert_constant_count_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$28, -16(%rbp)
	jle	L302
	movl	$8161, %edi
	call	__m3_fault
L302:
	movq	-16(%rbp), %rcx
	movabsq	$68719476735, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$68719476735, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_37
	.private_extern _insert_constant_count__Long_insert_constant_count_37
_insert_constant_count__Long_insert_constant_count_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$27, -16(%rbp)
	jle	L305
	movl	$8225, %edi
	call	__m3_fault
L305:
	movq	-16(%rbp), %rcx
	movabsq	$137438953471, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$137438953471, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_38
	.private_extern _insert_constant_count__Long_insert_constant_count_38
_insert_constant_count__Long_insert_constant_count_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$26, -16(%rbp)
	jle	L308
	movl	$8289, %edi
	call	__m3_fault
L308:
	movq	-16(%rbp), %rcx
	movabsq	$274877906943, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$274877906943, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_39
	.private_extern _insert_constant_count__Long_insert_constant_count_39
_insert_constant_count__Long_insert_constant_count_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$25, -16(%rbp)
	jle	L311
	movl	$8353, %edi
	call	__m3_fault
L311:
	movq	-16(%rbp), %rcx
	movabsq	$549755813887, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$549755813887, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_40
	.private_extern _insert_constant_count__Long_insert_constant_count_40
_insert_constant_count__Long_insert_constant_count_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$24, -16(%rbp)
	jle	L314
	movl	$8417, %edi
	call	__m3_fault
L314:
	movq	-16(%rbp), %rcx
	movabsq	$1099511627775, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$1099511627775, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_41
	.private_extern _insert_constant_count__Long_insert_constant_count_41
_insert_constant_count__Long_insert_constant_count_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$23, -16(%rbp)
	jle	L317
	movl	$8481, %edi
	call	__m3_fault
L317:
	movq	-16(%rbp), %rcx
	movabsq	$2199023255551, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$2199023255551, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_42
	.private_extern _insert_constant_count__Long_insert_constant_count_42
_insert_constant_count__Long_insert_constant_count_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$22, -16(%rbp)
	jle	L320
	movl	$8545, %edi
	call	__m3_fault
L320:
	movq	-16(%rbp), %rcx
	movabsq	$4398046511103, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$4398046511103, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_43
	.private_extern _insert_constant_count__Long_insert_constant_count_43
_insert_constant_count__Long_insert_constant_count_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$21, -16(%rbp)
	jle	L323
	movl	$8609, %edi
	call	__m3_fault
L323:
	movq	-16(%rbp), %rcx
	movabsq	$8796093022207, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$8796093022207, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_44
	.private_extern _insert_constant_count__Long_insert_constant_count_44
_insert_constant_count__Long_insert_constant_count_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$20, -16(%rbp)
	jle	L326
	movl	$8673, %edi
	call	__m3_fault
L326:
	movq	-16(%rbp), %rcx
	movabsq	$17592186044415, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$17592186044415, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_45
	.private_extern _insert_constant_count__Long_insert_constant_count_45
_insert_constant_count__Long_insert_constant_count_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$19, -16(%rbp)
	jle	L329
	movl	$8737, %edi
	call	__m3_fault
L329:
	movq	-16(%rbp), %rcx
	movabsq	$35184372088831, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$35184372088831, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_46
	.private_extern _insert_constant_count__Long_insert_constant_count_46
_insert_constant_count__Long_insert_constant_count_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$18, -16(%rbp)
	jle	L332
	movl	$8801, %edi
	call	__m3_fault
L332:
	movq	-16(%rbp), %rcx
	movabsq	$70368744177663, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$70368744177663, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_47
	.private_extern _insert_constant_count__Long_insert_constant_count_47
_insert_constant_count__Long_insert_constant_count_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$17, -16(%rbp)
	jle	L335
	movl	$8865, %edi
	call	__m3_fault
L335:
	movq	-16(%rbp), %rcx
	movabsq	$140737488355327, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$140737488355327, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_48
	.private_extern _insert_constant_count__Long_insert_constant_count_48
_insert_constant_count__Long_insert_constant_count_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$16, -16(%rbp)
	jle	L338
	movl	$8929, %edi
	call	__m3_fault
L338:
	movq	-16(%rbp), %rcx
	movabsq	$281474976710655, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$281474976710655, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_49
	.private_extern _insert_constant_count__Long_insert_constant_count_49
_insert_constant_count__Long_insert_constant_count_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$15, -16(%rbp)
	jle	L341
	movl	$8993, %edi
	call	__m3_fault
L341:
	movq	-16(%rbp), %rcx
	movabsq	$562949953421311, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$562949953421311, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_50
	.private_extern _insert_constant_count__Long_insert_constant_count_50
_insert_constant_count__Long_insert_constant_count_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$14, -16(%rbp)
	jle	L344
	movl	$9057, %edi
	call	__m3_fault
L344:
	movq	-16(%rbp), %rcx
	movabsq	$1125899906842623, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$1125899906842623, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_51
	.private_extern _insert_constant_count__Long_insert_constant_count_51
_insert_constant_count__Long_insert_constant_count_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$13, -16(%rbp)
	jle	L347
	movl	$9121, %edi
	call	__m3_fault
L347:
	movq	-16(%rbp), %rcx
	movabsq	$2251799813685247, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$2251799813685247, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_52
	.private_extern _insert_constant_count__Long_insert_constant_count_52
_insert_constant_count__Long_insert_constant_count_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$12, -16(%rbp)
	jle	L350
	movl	$9185, %edi
	call	__m3_fault
L350:
	movq	-16(%rbp), %rcx
	movabsq	$4503599627370495, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$4503599627370495, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_53
	.private_extern _insert_constant_count__Long_insert_constant_count_53
_insert_constant_count__Long_insert_constant_count_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$11, -16(%rbp)
	jle	L353
	movl	$9249, %edi
	call	__m3_fault
L353:
	movq	-16(%rbp), %rcx
	movabsq	$9007199254740991, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$9007199254740991, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_54
	.private_extern _insert_constant_count__Long_insert_constant_count_54
_insert_constant_count__Long_insert_constant_count_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$10, -16(%rbp)
	jle	L356
	movl	$9313, %edi
	call	__m3_fault
L356:
	movq	-16(%rbp), %rcx
	movabsq	$18014398509481983, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$18014398509481983, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_55
	.private_extern _insert_constant_count__Long_insert_constant_count_55
_insert_constant_count__Long_insert_constant_count_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$9, -16(%rbp)
	jle	L359
	movl	$9377, %edi
	call	__m3_fault
L359:
	movq	-16(%rbp), %rcx
	movabsq	$36028797018963967, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$36028797018963967, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_56
	.private_extern _insert_constant_count__Long_insert_constant_count_56
_insert_constant_count__Long_insert_constant_count_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$8, -16(%rbp)
	jle	L362
	movl	$9441, %edi
	call	__m3_fault
L362:
	movq	-16(%rbp), %rcx
	movabsq	$72057594037927935, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$72057594037927935, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_57
	.private_extern _insert_constant_count__Long_insert_constant_count_57
_insert_constant_count__Long_insert_constant_count_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$7, -16(%rbp)
	jle	L365
	movl	$9505, %edi
	call	__m3_fault
L365:
	movq	-16(%rbp), %rcx
	movabsq	$144115188075855871, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$144115188075855871, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_58
	.private_extern _insert_constant_count__Long_insert_constant_count_58
_insert_constant_count__Long_insert_constant_count_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$6, -16(%rbp)
	jle	L368
	movl	$9569, %edi
	call	__m3_fault
L368:
	movq	-16(%rbp), %rcx
	movabsq	$288230376151711743, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$288230376151711743, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_59
	.private_extern _insert_constant_count__Long_insert_constant_count_59
_insert_constant_count__Long_insert_constant_count_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$5, -16(%rbp)
	jle	L371
	movl	$9633, %edi
	call	__m3_fault
L371:
	movq	-16(%rbp), %rcx
	movabsq	$576460752303423487, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$576460752303423487, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_60
	.private_extern _insert_constant_count__Long_insert_constant_count_60
_insert_constant_count__Long_insert_constant_count_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$4, -16(%rbp)
	jle	L374
	movl	$9697, %edi
	call	__m3_fault
L374:
	movq	-16(%rbp), %rcx
	movabsq	$1152921504606846975, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$1152921504606846975, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_61
	.private_extern _insert_constant_count__Long_insert_constant_count_61
_insert_constant_count__Long_insert_constant_count_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$3, -16(%rbp)
	jle	L377
	movl	$9761, %edi
	call	__m3_fault
L377:
	movq	-16(%rbp), %rcx
	movabsq	$2305843009213693951, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$2305843009213693951, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_62
	.private_extern _insert_constant_count__Long_insert_constant_count_62
_insert_constant_count__Long_insert_constant_count_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2, -16(%rbp)
	jle	L380
	movl	$9825, %edi
	call	__m3_fault
L380:
	movq	-16(%rbp), %rcx
	movabsq	$4611686018427387903, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$4611686018427387903, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count__Long_insert_constant_count_63
	.private_extern _insert_constant_count__Long_insert_constant_count_63
_insert_constant_count__Long_insert_constant_count_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	192+_MM_insert_constant_count(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$1, -16(%rbp)
	jle	L383
	movl	$9889, %edi
	call	__m3_fault
L383:
	movq	-16(%rbp), %rcx
	movabsq	$9223372036854775807, %rax
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movabsq	$9223372036854775807, %rax
	andq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	leave
	ret
.globl _insert_constant_count_M3
_insert_constant_count_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_insert_constant_count(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_insert_constant_count(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "insert_constant_count_M3"
	.space 1
	.ascii "Long_insert_constant_count_63"
	.space 1
	.ascii "Long_insert_constant_count_62"
	.space 1
	.ascii "Long_insert_constant_count_61"
	.space 1
	.ascii "Long_insert_constant_count_60"
	.space 1
	.ascii "Long_insert_constant_count_59"
	.space 1
	.ascii "Long_insert_constant_count_58"
	.space 1
	.ascii "Long_insert_constant_count_57"
	.space 1
	.ascii "Long_insert_constant_count_56"
	.space 1
	.ascii "Long_insert_constant_count_55"
	.space 1
	.ascii "Long_insert_constant_count_54"
	.space 1
	.ascii "Long_insert_constant_count_53"
	.space 1
	.ascii "Long_insert_constant_count_52"
	.space 1
	.ascii "Long_insert_constant_count_51"
	.space 1
	.ascii "Long_insert_constant_count_50"
	.space 1
	.ascii "Long_insert_constant_count_49"
	.space 1
	.ascii "Long_insert_constant_count_48"
	.space 1
	.ascii "Long_insert_constant_count_47"
	.space 1
	.ascii "Long_insert_constant_count_46"
	.space 1
	.ascii "Long_insert_constant_count_45"
	.space 1
	.ascii "Long_insert_constant_count_44"
	.space 1
	.ascii "Long_insert_constant_count_43"
	.space 1
	.ascii "Long_insert_constant_count_42"
	.space 1
	.ascii "Long_insert_constant_count_41"
	.space 1
	.ascii "Long_insert_constant_count_40"
	.space 1
	.ascii "Long_insert_constant_count_39"
	.space 1
	.ascii "Long_insert_constant_count_38"
	.space 1
	.ascii "Long_insert_constant_count_37"
	.space 1
	.ascii "Long_insert_constant_count_36"
	.space 1
	.ascii "Long_insert_constant_count_35"
	.space 1
	.ascii "Long_insert_constant_count_34"
	.space 1
	.ascii "Long_insert_constant_count_33"
	.space 1
	.ascii "Long_insert_constant_count_32"
	.space 1
	.ascii "Long_insert_constant_count_31"
	.space 1
	.ascii "Long_insert_constant_count_30"
	.space 1
	.ascii "Long_insert_constant_count_29"
	.space 1
	.ascii "Long_insert_constant_count_28"
	.space 1
	.ascii "Long_insert_constant_count_27"
	.space 1
	.ascii "Long_insert_constant_count_26"
	.space 1
	.ascii "Long_insert_constant_count_25"
	.space 1
	.ascii "Long_insert_constant_count_24"
	.space 1
	.ascii "Long_insert_constant_count_23"
	.space 1
	.ascii "Long_insert_constant_count_22"
	.space 1
	.ascii "Long_insert_constant_count_21"
	.space 1
	.ascii "Long_insert_constant_count_20"
	.space 1
	.ascii "Long_insert_constant_count_19"
	.space 1
	.ascii "Long_insert_constant_count_18"
	.space 1
	.ascii "Long_insert_constant_count_17"
	.space 1
	.ascii "Long_insert_constant_count_16"
	.space 1
	.ascii "Long_insert_constant_count_15"
	.space 1
	.ascii "Long_insert_constant_count_14"
	.space 1
	.ascii "Long_insert_constant_count_13"
	.space 1
	.ascii "Long_insert_constant_count_12"
	.space 1
	.ascii "Long_insert_constant_count_11"
	.space 1
	.ascii "Long_insert_constant_count_10"
	.space 1
	.ascii "Long_insert_constant_count_9"
	.space 1
	.ascii "Long_insert_constant_count_8"
	.space 1
	.ascii "Long_insert_constant_count_7"
	.space 1
	.ascii "Long_insert_constant_count_6"
	.space 1
	.ascii "Long_insert_constant_count_5"
	.space 1
	.ascii "Long_insert_constant_count_4"
	.space 1
	.ascii "Long_insert_constant_count_3"
	.space 1
	.ascii "Long_insert_constant_count_2"
	.space 1
	.ascii "Long_insert_constant_count_1"
	.space 1
	.ascii "Long_insert_constant_count_0"
	.space 1
	.ascii "Word_insert_constant_count_63"
	.space 1
	.ascii "Word_insert_constant_count_62"
	.space 1
	.ascii "Word_insert_constant_count_61"
	.space 1
	.ascii "Word_insert_constant_count_60"
	.space 1
	.ascii "Word_insert_constant_count_59"
	.space 1
	.ascii "Word_insert_constant_count_58"
	.space 1
	.ascii "Word_insert_constant_count_57"
	.space 1
	.ascii "Word_insert_constant_count_56"
	.space 1
	.ascii "Word_insert_constant_count_55"
	.space 1
	.ascii "Word_insert_constant_count_54"
	.space 1
	.ascii "Word_insert_constant_count_53"
	.space 1
	.ascii "Word_insert_constant_count_52"
	.space 1
	.ascii "Word_insert_constant_count_51"
	.space 1
	.ascii "Word_insert_constant_count_50"
	.space 1
	.ascii "Word_insert_constant_count_49"
	.space 1
	.ascii "Word_insert_constant_count_48"
	.space 1
	.ascii "Word_insert_constant_count_47"
	.space 1
	.ascii "Word_insert_constant_count_46"
	.space 1
	.ascii "Word_insert_constant_count_45"
	.space 1
	.ascii "Word_insert_constant_count_44"
	.space 1
	.ascii "Word_insert_constant_count_43"
	.space 1
	.ascii "Word_insert_constant_count_42"
	.space 1
	.ascii "Word_insert_constant_count_41"
	.space 1
	.ascii "Word_insert_constant_count_40"
	.space 1
	.ascii "Word_insert_constant_count_39"
	.space 1
	.ascii "Word_insert_constant_count_38"
	.space 1
	.ascii "Word_insert_constant_count_37"
	.space 1
	.ascii "Word_insert_constant_count_36"
	.space 1
	.ascii "Word_insert_constant_count_35"
	.space 1
	.ascii "Word_insert_constant_count_34"
	.space 1
	.ascii "Word_insert_constant_count_33"
	.space 1
	.ascii "Word_insert_constant_count_32"
	.space 1
	.ascii "Word_insert_constant_count_31"
	.space 1
	.ascii "Word_insert_constant_count_30"
	.space 1
	.ascii "Word_insert_constant_count_29"
	.space 1
	.ascii "Word_insert_constant_count_28"
	.space 1
	.ascii "Word_insert_constant_count_27"
	.space 1
	.ascii "Word_insert_constant_count_26"
	.space 1
	.ascii "Word_insert_constant_count_25"
	.space 1
	.ascii "Word_insert_constant_count_24"
	.space 1
	.ascii "Word_insert_constant_count_23"
	.space 1
	.ascii "Word_insert_constant_count_22"
	.space 1
	.ascii "Word_insert_constant_count_21"
	.space 1
	.ascii "Word_insert_constant_count_20"
	.space 1
	.ascii "Word_insert_constant_count_19"
	.space 1
	.ascii "Word_insert_constant_count_18"
	.space 1
	.ascii "Word_insert_constant_count_17"
	.space 1
	.ascii "Word_insert_constant_count_16"
	.space 1
	.ascii "Word_insert_constant_count_15"
	.space 1
	.ascii "Word_insert_constant_count_14"
	.space 1
	.ascii "Word_insert_constant_count_13"
	.space 1
	.ascii "Word_insert_constant_count_12"
	.space 1
	.ascii "Word_insert_constant_count_11"
	.space 1
	.ascii "Word_insert_constant_count_10"
	.space 1
	.ascii "Word_insert_constant_count_9"
	.space 1
	.ascii "Word_insert_constant_count_8"
	.space 1
	.ascii "Word_insert_constant_count_7"
	.space 1
	.ascii "Word_insert_constant_count_6"
	.space 1
	.ascii "Word_insert_constant_count_5"
	.space 1
	.ascii "Word_insert_constant_count_4"
	.space 1
	.ascii "Word_insert_constant_count_3"
	.space 1
	.ascii "Word_insert_constant_count_2"
	.space 1
	.ascii "Word_insert_constant_count_1"
	.space 1
	.ascii "Word_insert_constant_count_0"
	.space 4
	.quad	_insert_constant_count_M3
	.quad	_L_1
	.quad	_insert_constant_count__Long_insert_constant_count_63
	.quad	_L_1+25
	.quad	_insert_constant_count__Long_insert_constant_count_62
	.quad	_L_1+55
	.quad	_insert_constant_count__Long_insert_constant_count_61
	.quad	_L_1+85
	.quad	_insert_constant_count__Long_insert_constant_count_60
	.quad	_L_1+115
	.quad	_insert_constant_count__Long_insert_constant_count_59
	.quad	_L_1+145
	.quad	_insert_constant_count__Long_insert_constant_count_58
	.quad	_L_1+175
	.quad	_insert_constant_count__Long_insert_constant_count_57
	.quad	_L_1+205
	.quad	_insert_constant_count__Long_insert_constant_count_56
	.quad	_L_1+235
	.quad	_insert_constant_count__Long_insert_constant_count_55
	.quad	_L_1+265
	.quad	_insert_constant_count__Long_insert_constant_count_54
	.quad	_L_1+295
	.quad	_insert_constant_count__Long_insert_constant_count_53
	.quad	_L_1+325
	.quad	_insert_constant_count__Long_insert_constant_count_52
	.quad	_L_1+355
	.quad	_insert_constant_count__Long_insert_constant_count_51
	.quad	_L_1+385
	.quad	_insert_constant_count__Long_insert_constant_count_50
	.quad	_L_1+415
	.quad	_insert_constant_count__Long_insert_constant_count_49
	.quad	_L_1+445
	.quad	_insert_constant_count__Long_insert_constant_count_48
	.quad	_L_1+475
	.quad	_insert_constant_count__Long_insert_constant_count_47
	.quad	_L_1+505
	.quad	_insert_constant_count__Long_insert_constant_count_46
	.quad	_L_1+535
	.quad	_insert_constant_count__Long_insert_constant_count_45
	.quad	_L_1+565
	.quad	_insert_constant_count__Long_insert_constant_count_44
	.quad	_L_1+595
	.quad	_insert_constant_count__Long_insert_constant_count_43
	.quad	_L_1+625
	.quad	_insert_constant_count__Long_insert_constant_count_42
	.quad	_L_1+655
	.quad	_insert_constant_count__Long_insert_constant_count_41
	.quad	_L_1+685
	.quad	_insert_constant_count__Long_insert_constant_count_40
	.quad	_L_1+715
	.quad	_insert_constant_count__Long_insert_constant_count_39
	.quad	_L_1+745
	.quad	_insert_constant_count__Long_insert_constant_count_38
	.quad	_L_1+775
	.quad	_insert_constant_count__Long_insert_constant_count_37
	.quad	_L_1+805
	.quad	_insert_constant_count__Long_insert_constant_count_36
	.quad	_L_1+835
	.quad	_insert_constant_count__Long_insert_constant_count_35
	.quad	_L_1+865
	.quad	_insert_constant_count__Long_insert_constant_count_34
	.quad	_L_1+895
	.quad	_insert_constant_count__Long_insert_constant_count_33
	.quad	_L_1+925
	.quad	_insert_constant_count__Long_insert_constant_count_32
	.quad	_L_1+955
	.quad	_insert_constant_count__Long_insert_constant_count_31
	.quad	_L_1+985
	.quad	_insert_constant_count__Long_insert_constant_count_30
	.quad	_L_1+1015
	.quad	_insert_constant_count__Long_insert_constant_count_29
	.quad	_L_1+1045
	.quad	_insert_constant_count__Long_insert_constant_count_28
	.quad	_L_1+1075
	.quad	_insert_constant_count__Long_insert_constant_count_27
	.quad	_L_1+1105
	.quad	_insert_constant_count__Long_insert_constant_count_26
	.quad	_L_1+1135
	.quad	_insert_constant_count__Long_insert_constant_count_25
	.quad	_L_1+1165
	.quad	_insert_constant_count__Long_insert_constant_count_24
	.quad	_L_1+1195
	.quad	_insert_constant_count__Long_insert_constant_count_23
	.quad	_L_1+1225
	.quad	_insert_constant_count__Long_insert_constant_count_22
	.quad	_L_1+1255
	.quad	_insert_constant_count__Long_insert_constant_count_21
	.quad	_L_1+1285
	.quad	_insert_constant_count__Long_insert_constant_count_20
	.quad	_L_1+1315
	.quad	_insert_constant_count__Long_insert_constant_count_19
	.quad	_L_1+1345
	.quad	_insert_constant_count__Long_insert_constant_count_18
	.quad	_L_1+1375
	.quad	_insert_constant_count__Long_insert_constant_count_17
	.quad	_L_1+1405
	.quad	_insert_constant_count__Long_insert_constant_count_16
	.quad	_L_1+1435
	.quad	_insert_constant_count__Long_insert_constant_count_15
	.quad	_L_1+1465
	.quad	_insert_constant_count__Long_insert_constant_count_14
	.quad	_L_1+1495
	.quad	_insert_constant_count__Long_insert_constant_count_13
	.quad	_L_1+1525
	.quad	_insert_constant_count__Long_insert_constant_count_12
	.quad	_L_1+1555
	.quad	_insert_constant_count__Long_insert_constant_count_11
	.quad	_L_1+1585
	.quad	_insert_constant_count__Long_insert_constant_count_10
	.quad	_L_1+1615
	.quad	_insert_constant_count__Long_insert_constant_count_9
	.quad	_L_1+1645
	.quad	_insert_constant_count__Long_insert_constant_count_8
	.quad	_L_1+1674
	.quad	_insert_constant_count__Long_insert_constant_count_7
	.quad	_L_1+1703
	.quad	_insert_constant_count__Long_insert_constant_count_6
	.quad	_L_1+1732
	.quad	_insert_constant_count__Long_insert_constant_count_5
	.quad	_L_1+1761
	.quad	_insert_constant_count__Long_insert_constant_count_4
	.quad	_L_1+1790
	.quad	_insert_constant_count__Long_insert_constant_count_3
	.quad	_L_1+1819
	.quad	_insert_constant_count__Long_insert_constant_count_2
	.quad	_L_1+1848
	.quad	_insert_constant_count__Long_insert_constant_count_1
	.quad	_L_1+1877
	.quad	_insert_constant_count__Long_insert_constant_count_0
	.quad	_L_1+1906
	.quad	_insert_constant_count__Word_insert_constant_count_63
	.quad	_L_1+1935
	.quad	_insert_constant_count__Word_insert_constant_count_62
	.quad	_L_1+1965
	.quad	_insert_constant_count__Word_insert_constant_count_61
	.quad	_L_1+1995
	.quad	_insert_constant_count__Word_insert_constant_count_60
	.quad	_L_1+2025
	.quad	_insert_constant_count__Word_insert_constant_count_59
	.quad	_L_1+2055
	.quad	_insert_constant_count__Word_insert_constant_count_58
	.quad	_L_1+2085
	.quad	_insert_constant_count__Word_insert_constant_count_57
	.quad	_L_1+2115
	.quad	_insert_constant_count__Word_insert_constant_count_56
	.quad	_L_1+2145
	.quad	_insert_constant_count__Word_insert_constant_count_55
	.quad	_L_1+2175
	.quad	_insert_constant_count__Word_insert_constant_count_54
	.quad	_L_1+2205
	.quad	_insert_constant_count__Word_insert_constant_count_53
	.quad	_L_1+2235
	.quad	_insert_constant_count__Word_insert_constant_count_52
	.quad	_L_1+2265
	.quad	_insert_constant_count__Word_insert_constant_count_51
	.quad	_L_1+2295
	.quad	_insert_constant_count__Word_insert_constant_count_50
	.quad	_L_1+2325
	.quad	_insert_constant_count__Word_insert_constant_count_49
	.quad	_L_1+2355
	.quad	_insert_constant_count__Word_insert_constant_count_48
	.quad	_L_1+2385
	.quad	_insert_constant_count__Word_insert_constant_count_47
	.quad	_L_1+2415
	.quad	_insert_constant_count__Word_insert_constant_count_46
	.quad	_L_1+2445
	.quad	_insert_constant_count__Word_insert_constant_count_45
	.quad	_L_1+2475
	.quad	_insert_constant_count__Word_insert_constant_count_44
	.quad	_L_1+2505
	.quad	_insert_constant_count__Word_insert_constant_count_43
	.quad	_L_1+2535
	.quad	_insert_constant_count__Word_insert_constant_count_42
	.quad	_L_1+2565
	.quad	_insert_constant_count__Word_insert_constant_count_41
	.quad	_L_1+2595
	.quad	_insert_constant_count__Word_insert_constant_count_40
	.quad	_L_1+2625
	.quad	_insert_constant_count__Word_insert_constant_count_39
	.quad	_L_1+2655
	.quad	_insert_constant_count__Word_insert_constant_count_38
	.quad	_L_1+2685
	.quad	_insert_constant_count__Word_insert_constant_count_37
	.quad	_L_1+2715
	.quad	_insert_constant_count__Word_insert_constant_count_36
	.quad	_L_1+2745
	.quad	_insert_constant_count__Word_insert_constant_count_35
	.quad	_L_1+2775
	.quad	_insert_constant_count__Word_insert_constant_count_34
	.quad	_L_1+2805
	.quad	_insert_constant_count__Word_insert_constant_count_33
	.quad	_L_1+2835
	.quad	_insert_constant_count__Word_insert_constant_count_32
	.quad	_L_1+2865
	.quad	_insert_constant_count__Word_insert_constant_count_31
	.quad	_L_1+2895
	.quad	_insert_constant_count__Word_insert_constant_count_30
	.quad	_L_1+2925
	.quad	_insert_constant_count__Word_insert_constant_count_29
	.quad	_L_1+2955
	.quad	_insert_constant_count__Word_insert_constant_count_28
	.quad	_L_1+2985
	.quad	_insert_constant_count__Word_insert_constant_count_27
	.quad	_L_1+3015
	.quad	_insert_constant_count__Word_insert_constant_count_26
	.quad	_L_1+3045
	.quad	_insert_constant_count__Word_insert_constant_count_25
	.quad	_L_1+3075
	.quad	_insert_constant_count__Word_insert_constant_count_24
	.quad	_L_1+3105
	.quad	_insert_constant_count__Word_insert_constant_count_23
	.quad	_L_1+3135
	.quad	_insert_constant_count__Word_insert_constant_count_22
	.quad	_L_1+3165
	.quad	_insert_constant_count__Word_insert_constant_count_21
	.quad	_L_1+3195
	.quad	_insert_constant_count__Word_insert_constant_count_20
	.quad	_L_1+3225
	.quad	_insert_constant_count__Word_insert_constant_count_19
	.quad	_L_1+3255
	.quad	_insert_constant_count__Word_insert_constant_count_18
	.quad	_L_1+3285
	.quad	_insert_constant_count__Word_insert_constant_count_17
	.quad	_L_1+3315
	.quad	_insert_constant_count__Word_insert_constant_count_16
	.quad	_L_1+3345
	.quad	_insert_constant_count__Word_insert_constant_count_15
	.quad	_L_1+3375
	.quad	_insert_constant_count__Word_insert_constant_count_14
	.quad	_L_1+3405
	.quad	_insert_constant_count__Word_insert_constant_count_13
	.quad	_L_1+3435
	.quad	_insert_constant_count__Word_insert_constant_count_12
	.quad	_L_1+3465
	.quad	_insert_constant_count__Word_insert_constant_count_11
	.quad	_L_1+3495
	.quad	_insert_constant_count__Word_insert_constant_count_10
	.quad	_L_1+3525
	.quad	_insert_constant_count__Word_insert_constant_count_9
	.quad	_L_1+3555
	.quad	_insert_constant_count__Word_insert_constant_count_8
	.quad	_L_1+3584
	.quad	_insert_constant_count__Word_insert_constant_count_7
	.quad	_L_1+3613
	.quad	_insert_constant_count__Word_insert_constant_count_6
	.quad	_L_1+3642
	.quad	_insert_constant_count__Word_insert_constant_count_5
	.quad	_L_1+3671
	.quad	_insert_constant_count__Word_insert_constant_count_4
	.quad	_L_1+3700
	.quad	_insert_constant_count__Word_insert_constant_count_3
	.quad	_L_1+3729
	.quad	_insert_constant_count__Word_insert_constant_count_2
	.quad	_L_1+3758
	.quad	_insert_constant_count__Word_insert_constant_count_1
	.quad	_L_1+3787
	.quad	_insert_constant_count__Word_insert_constant_count_0
	.quad	_L_1+3816
	.space 8
	.ascii "../AMD64_DARWIN/insert_constant_count.m3"
	.space 8
	.data
	.align 5
_MM_insert_constant_count:
	.quad	_L_1+5920
	.space 32
	.quad	_L_1+3848
	.space 24
	.quad	_MM_insert_constant_count+208
	.space 8
	.quad	_insert_constant_count_M3
	.quad	3
	.quad	1009
	.quad	1010
	.long	1149027962
	.byte	-11
	.space 3
	.quad	1014
	.byte	-9
	.space 3
	.long	1016
	.quad	1017
	.word	1018
	.space 6
	.quad	1019
	.long	432932703
	.long	1083171025
	.quad	1022
	.word	1023
	.space 2
	.long	1024
	.space 24
	.quad	_insert_constant_count_I3
	.quad	_MM_insert_constant_count+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_insert_constant_count+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_insert_constant_count+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_insert_constant_count+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
