	.text
.globl _LeftRotate__uLeftRotate_var_u16_I
	.private_extern _LeftRotate__uLeftRotate_var_u16_I
_LeftRotate__uLeftRotate_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L2
	cmpq	$63, -16(%rbp)
	jle	L3
L2:
	movl	$1729, %edi
	call	__m3_fault
L3:
	movzwl	104+_MM_LeftRotate(%rip), %eax
	movswq	%ax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u16_I
	.private_extern _LeftRotate__uLeftRotate_param_u16_I
_LeftRotate__uLeftRotate_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L6
	cmpq	$63, -16(%rbp)
	jle	L7
L6:
	movl	$1761, %edi
	call	__m3_fault
L7:
	movswq	-18(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u16_C
	.private_extern _LeftRotate__uLeftRotate_var_u16_C
_LeftRotate__uLeftRotate_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L10
	movl	$1793, %edi
	call	__m3_fault
L10:
	movzwl	104+_MM_LeftRotate(%rip), %eax
	movswq	%ax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u16_C
	.private_extern _LeftRotate__uLeftRotate_param_u16_C
_LeftRotate__uLeftRotate_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L13
	movl	$1825, %edi
	call	__m3_fault
L13:
	movswq	-18(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u64_I
	.private_extern _LeftRotate__uLeftRotate_var_u64_I
_LeftRotate__uLeftRotate_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L16
	cmpq	$63, -16(%rbp)
	jle	L17
L16:
	movl	$1857, %edi
	call	__m3_fault
L17:
	movq	112+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u64_I
	.private_extern _LeftRotate__uLeftRotate_param_u64_I
_LeftRotate__uLeftRotate_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L20
	cmpq	$63, -16(%rbp)
	jle	L21
L20:
	movl	$1889, %edi
	call	__m3_fault
L21:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u64_C
	.private_extern _LeftRotate__uLeftRotate_var_u64_C
_LeftRotate__uLeftRotate_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L24
	movl	$1921, %edi
	call	__m3_fault
L24:
	movq	112+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u64_C
	.private_extern _LeftRotate__uLeftRotate_param_u64_C
_LeftRotate__uLeftRotate_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L27
	movl	$1953, %edi
	call	__m3_fault
L27:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_LC_I
	.private_extern _LeftRotate__uLeftRotate_var_LC_I
_LeftRotate__uLeftRotate_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L30
	cmpq	$63, -16(%rbp)
	jle	L31
L30:
	movl	$1985, %edi
	call	__m3_fault
L31:
	movq	120+_MM_LeftRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_LC_I
	.private_extern _LeftRotate__uLeftRotate_param_LC_I
_LeftRotate__uLeftRotate_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L34
	cmpq	$63, -16(%rbp)
	jle	L35
L34:
	movl	$2017, %edi
	call	__m3_fault
L35:
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_LC_C
	.private_extern _LeftRotate__uLeftRotate_var_LC_C
_LeftRotate__uLeftRotate_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L38
	movl	$2049, %edi
	call	__m3_fault
L38:
	movq	120+_MM_LeftRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_LC_C
	.private_extern _LeftRotate__uLeftRotate_param_LC_C
_LeftRotate__uLeftRotate_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L41
	movl	$2081, %edi
	call	__m3_fault
L41:
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i16_I
	.private_extern _LeftRotate__uLeftRotate_var_i16_I
_LeftRotate__uLeftRotate_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L44
	cmpq	$63, -16(%rbp)
	jle	L45
L44:
	movl	$2113, %edi
	call	__m3_fault
L45:
	movzwl	136+_MM_LeftRotate(%rip), %eax
	movzwl	%ax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i16_I
	.private_extern _LeftRotate__uLeftRotate_param_i16_I
_LeftRotate__uLeftRotate_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L48
	cmpq	$63, -16(%rbp)
	jle	L49
L48:
	movl	$2145, %edi
	call	__m3_fault
L49:
	movzwl	-18(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i16_C
	.private_extern _LeftRotate__uLeftRotate_var_i16_C
_LeftRotate__uLeftRotate_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L52
	movl	$2177, %edi
	call	__m3_fault
L52:
	movzwl	136+_MM_LeftRotate(%rip), %eax
	movzwl	%ax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i16_C
	.private_extern _LeftRotate__uLeftRotate_param_i16_C
_LeftRotate__uLeftRotate_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L55
	movl	$2209, %edi
	call	__m3_fault
L55:
	movzwl	-18(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i32_I
	.private_extern _LeftRotate__uLeftRotate_var_i32_I
_LeftRotate__uLeftRotate_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L58
	cmpq	$63, -16(%rbp)
	jle	L59
L58:
	movl	$2241, %edi
	call	__m3_fault
L59:
	movl	140+_MM_LeftRotate(%rip), %eax
	mov	%eax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i32_I
	.private_extern _LeftRotate__uLeftRotate_param_i32_I
_LeftRotate__uLeftRotate_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L62
	cmpq	$63, -16(%rbp)
	jle	L63
L62:
	movl	$2273, %edi
	call	__m3_fault
L63:
	mov	-20(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i32_C
	.private_extern _LeftRotate__uLeftRotate_var_i32_C
_LeftRotate__uLeftRotate_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L66
	movl	$2305, %edi
	call	__m3_fault
L66:
	movl	140+_MM_LeftRotate(%rip), %eax
	mov	%eax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i32_C
	.private_extern _LeftRotate__uLeftRotate_param_i32_C
_LeftRotate__uLeftRotate_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L69
	movl	$2337, %edi
	call	__m3_fault
L69:
	mov	-20(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_I_I
	.private_extern _LeftRotate__uLeftRotate_var_I_I
_LeftRotate__uLeftRotate_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L72
	cmpq	$63, -16(%rbp)
	jle	L73
L72:
	movl	$2369, %edi
	call	__m3_fault
L73:
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_I_I
	.private_extern _LeftRotate__uLeftRotate_param_I_I
_LeftRotate__uLeftRotate_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L76
	cmpq	$63, -16(%rbp)
	jle	L77
L76:
	movl	$2401, %edi
	call	__m3_fault
L77:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_I_C
	.private_extern _LeftRotate__uLeftRotate_var_I_C
_LeftRotate__uLeftRotate_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L80
	movl	$2433, %edi
	call	__m3_fault
L80:
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_I_C
	.private_extern _LeftRotate__uLeftRotate_param_I_C
_LeftRotate__uLeftRotate_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L83
	movl	$2465, %edi
	call	__m3_fault
L83:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i64_I
	.private_extern _LeftRotate__uLeftRotate_var_i64_I
_LeftRotate__uLeftRotate_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L86
	cmpq	$63, -16(%rbp)
	jle	L87
L86:
	movl	$2497, %edi
	call	__m3_fault
L87:
	movq	152+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i64_I
	.private_extern _LeftRotate__uLeftRotate_param_i64_I
_LeftRotate__uLeftRotate_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L90
	cmpq	$63, -16(%rbp)
	jle	L91
L90:
	movl	$2529, %edi
	call	__m3_fault
L91:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i64_C
	.private_extern _LeftRotate__uLeftRotate_var_i64_C
_LeftRotate__uLeftRotate_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L94
	movl	$2561, %edi
	call	__m3_fault
L94:
	movq	152+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i64_C
	.private_extern _LeftRotate__uLeftRotate_param_i64_C
_LeftRotate__uLeftRotate_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L97
	movl	$2593, %edi
	call	__m3_fault
L97:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_C_I
	.private_extern _LeftRotate__uLeftRotate_var_C_I
_LeftRotate__uLeftRotate_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L100
	cmpq	$63, -16(%rbp)
	jle	L101
L100:
	movl	$2625, %edi
	call	__m3_fault
L101:
	movq	160+_MM_LeftRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_C_I
	.private_extern _LeftRotate__uLeftRotate_param_C_I
_LeftRotate__uLeftRotate_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L104
	cmpq	$63, -16(%rbp)
	jle	L105
L104:
	movl	$2657, %edi
	call	__m3_fault
L105:
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_C_C
	.private_extern _LeftRotate__uLeftRotate_var_C_C
_LeftRotate__uLeftRotate_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L108
	movl	$2689, %edi
	call	__m3_fault
L108:
	movq	160+_MM_LeftRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_C_C
	.private_extern _LeftRotate__uLeftRotate_param_C_C
_LeftRotate__uLeftRotate_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L111
	movl	$2721, %edi
	call	__m3_fault
L111:
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u8_I
	.private_extern _LeftRotate__uLeftRotate_var_u8_I
_LeftRotate__uLeftRotate_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L114
	cmpq	$63, -16(%rbp)
	jle	L115
L114:
	movl	$2753, %edi
	call	__m3_fault
L115:
	movzbl	172+_MM_LeftRotate(%rip), %eax
	movsbq	%al,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u8_I
	.private_extern _LeftRotate__uLeftRotate_param_u8_I
_LeftRotate__uLeftRotate_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L118
	cmpq	$63, -16(%rbp)
	jle	L119
L118:
	movl	$2785, %edi
	call	__m3_fault
L119:
	movsbq	-17(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u8_C
	.private_extern _LeftRotate__uLeftRotate_var_u8_C
_LeftRotate__uLeftRotate_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L122
	movl	$2817, %edi
	call	__m3_fault
L122:
	movzbl	172+_MM_LeftRotate(%rip), %eax
	movsbq	%al,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u8_C
	.private_extern _LeftRotate__uLeftRotate_param_u8_C
_LeftRotate__uLeftRotate_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L125
	movl	$2849, %edi
	call	__m3_fault
L125:
	movsbq	-17(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i8_I
	.private_extern _LeftRotate__uLeftRotate_var_i8_I
_LeftRotate__uLeftRotate_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L128
	cmpq	$63, -16(%rbp)
	jle	L129
L128:
	movl	$2881, %edi
	call	__m3_fault
L129:
	movzbl	173+_MM_LeftRotate(%rip), %eax
	movzbl	%al, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i8_I
	.private_extern _LeftRotate__uLeftRotate_param_i8_I
_LeftRotate__uLeftRotate_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L132
	cmpq	$63, -16(%rbp)
	jle	L133
L132:
	movl	$2913, %edi
	call	__m3_fault
L133:
	movzbl	-17(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_i8_C
	.private_extern _LeftRotate__uLeftRotate_var_i8_C
_LeftRotate__uLeftRotate_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L136
	movl	$2945, %edi
	call	__m3_fault
L136:
	movzbl	173+_MM_LeftRotate(%rip), %eax
	movzbl	%al, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_i8_C
	.private_extern _LeftRotate__uLeftRotate_param_i8_C
_LeftRotate__uLeftRotate_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L139
	movl	$2977, %edi
	call	__m3_fault
L139:
	movzbl	-17(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_L_I
	.private_extern _LeftRotate__uLeftRotate_var_L_I
_LeftRotate__uLeftRotate_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L142
	cmpq	$63, -16(%rbp)
	jle	L143
L142:
	movl	$3009, %edi
	call	__m3_fault
L143:
	movq	176+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_L_I
	.private_extern _LeftRotate__uLeftRotate_param_L_I
_LeftRotate__uLeftRotate_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L146
	cmpq	$63, -16(%rbp)
	jle	L147
L146:
	movl	$3041, %edi
	call	__m3_fault
L147:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_L_C
	.private_extern _LeftRotate__uLeftRotate_var_L_C
_LeftRotate__uLeftRotate_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L150
	movl	$3073, %edi
	call	__m3_fault
L150:
	movq	176+_MM_LeftRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_L_C
	.private_extern _LeftRotate__uLeftRotate_param_L_C
_LeftRotate__uLeftRotate_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L153
	movl	$3105, %edi
	call	__m3_fault
L153:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u32_I
	.private_extern _LeftRotate__uLeftRotate_var_u32_I
_LeftRotate__uLeftRotate_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L156
	cmpq	$63, -16(%rbp)
	jle	L157
L156:
	movl	$3137, %edi
	call	__m3_fault
L157:
	movl	184+_MM_LeftRotate(%rip), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u32_I
	.private_extern _LeftRotate__uLeftRotate_param_u32_I
_LeftRotate__uLeftRotate_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L160
	cmpq	$63, -16(%rbp)
	jle	L161
L160:
	movl	$3169, %edi
	call	__m3_fault
L161:
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_var_u32_C
	.private_extern _LeftRotate__uLeftRotate_var_u32_C
_LeftRotate__uLeftRotate_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_LeftRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L164
	movl	$3201, %edi
	call	__m3_fault
L164:
	movl	184+_MM_LeftRotate(%rip), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate__uLeftRotate_param_u32_C
	.private_extern _LeftRotate__uLeftRotate_param_u32_C
_LeftRotate__uLeftRotate_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L167
	movl	$3233, %edi
	call	__m3_fault
L167:
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rolq	%cl, %rax
	leave
	ret
.globl _LeftRotate_M3
_LeftRotate_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_LeftRotate(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_LeftRotate(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "LeftRotate_M3"
	.space 1
	.ascii "uLeftRotate_param_u32_C"
	.space 1
	.ascii "uLeftRotate_var_u32_C"
	.space 1
	.ascii "uLeftRotate_param_u32_I"
	.space 1
	.ascii "uLeftRotate_var_u32_I"
	.space 1
	.ascii "uLeftRotate_param_L_C"
	.space 1
	.ascii "uLeftRotate_var_L_C"
	.space 1
	.ascii "uLeftRotate_param_L_I"
	.space 1
	.ascii "uLeftRotate_var_L_I"
	.space 1
	.ascii "uLeftRotate_param_i8_C"
	.space 1
	.ascii "uLeftRotate_var_i8_C"
	.space 1
	.ascii "uLeftRotate_param_i8_I"
	.space 1
	.ascii "uLeftRotate_var_i8_I"
	.space 1
	.ascii "uLeftRotate_param_u8_C"
	.space 1
	.ascii "uLeftRotate_var_u8_C"
	.space 1
	.ascii "uLeftRotate_param_u8_I"
	.space 1
	.ascii "uLeftRotate_var_u8_I"
	.space 1
	.ascii "uLeftRotate_param_C_C"
	.space 1
	.ascii "uLeftRotate_var_C_C"
	.space 1
	.ascii "uLeftRotate_param_C_I"
	.space 1
	.ascii "uLeftRotate_var_C_I"
	.space 1
	.ascii "uLeftRotate_param_i64_C"
	.space 1
	.ascii "uLeftRotate_var_i64_C"
	.space 1
	.ascii "uLeftRotate_param_i64_I"
	.space 1
	.ascii "uLeftRotate_var_i64_I"
	.space 1
	.ascii "uLeftRotate_param_I_C"
	.space 1
	.ascii "uLeftRotate_var_I_C"
	.space 1
	.ascii "uLeftRotate_param_I_I"
	.space 1
	.ascii "uLeftRotate_var_I_I"
	.space 1
	.ascii "uLeftRotate_param_i32_C"
	.space 1
	.ascii "uLeftRotate_var_i32_C"
	.space 1
	.ascii "uLeftRotate_param_i32_I"
	.space 1
	.ascii "uLeftRotate_var_i32_I"
	.space 1
	.ascii "uLeftRotate_param_i16_C"
	.space 1
	.ascii "uLeftRotate_var_i16_C"
	.space 1
	.ascii "uLeftRotate_param_i16_I"
	.space 1
	.ascii "uLeftRotate_var_i16_I"
	.space 1
	.ascii "uLeftRotate_param_LC_C"
	.space 1
	.ascii "uLeftRotate_var_LC_C"
	.space 1
	.ascii "uLeftRotate_param_LC_I"
	.space 1
	.ascii "uLeftRotate_var_LC_I"
	.space 1
	.ascii "uLeftRotate_param_u64_C"
	.space 1
	.ascii "uLeftRotate_var_u64_C"
	.space 1
	.ascii "uLeftRotate_param_u64_I"
	.space 1
	.ascii "uLeftRotate_var_u64_I"
	.space 1
	.ascii "uLeftRotate_param_u16_C"
	.space 1
	.ascii "uLeftRotate_var_u16_C"
	.space 1
	.ascii "uLeftRotate_param_u16_I"
	.space 1
	.ascii "uLeftRotate_var_u16_I"
	.space 7
	.quad	_LeftRotate_M3
	.quad	_L_1
	.quad	_LeftRotate__uLeftRotate_param_u32_C
	.quad	_L_1+14
	.quad	_LeftRotate__uLeftRotate_var_u32_C
	.quad	_L_1+38
	.quad	_LeftRotate__uLeftRotate_param_u32_I
	.quad	_L_1+60
	.quad	_LeftRotate__uLeftRotate_var_u32_I
	.quad	_L_1+84
	.quad	_LeftRotate__uLeftRotate_param_L_C
	.quad	_L_1+106
	.quad	_LeftRotate__uLeftRotate_var_L_C
	.quad	_L_1+128
	.quad	_LeftRotate__uLeftRotate_param_L_I
	.quad	_L_1+148
	.quad	_LeftRotate__uLeftRotate_var_L_I
	.quad	_L_1+170
	.quad	_LeftRotate__uLeftRotate_param_i8_C
	.quad	_L_1+190
	.quad	_LeftRotate__uLeftRotate_var_i8_C
	.quad	_L_1+213
	.quad	_LeftRotate__uLeftRotate_param_i8_I
	.quad	_L_1+234
	.quad	_LeftRotate__uLeftRotate_var_i8_I
	.quad	_L_1+257
	.quad	_LeftRotate__uLeftRotate_param_u8_C
	.quad	_L_1+278
	.quad	_LeftRotate__uLeftRotate_var_u8_C
	.quad	_L_1+301
	.quad	_LeftRotate__uLeftRotate_param_u8_I
	.quad	_L_1+322
	.quad	_LeftRotate__uLeftRotate_var_u8_I
	.quad	_L_1+345
	.quad	_LeftRotate__uLeftRotate_param_C_C
	.quad	_L_1+366
	.quad	_LeftRotate__uLeftRotate_var_C_C
	.quad	_L_1+388
	.quad	_LeftRotate__uLeftRotate_param_C_I
	.quad	_L_1+408
	.quad	_LeftRotate__uLeftRotate_var_C_I
	.quad	_L_1+430
	.quad	_LeftRotate__uLeftRotate_param_i64_C
	.quad	_L_1+450
	.quad	_LeftRotate__uLeftRotate_var_i64_C
	.quad	_L_1+474
	.quad	_LeftRotate__uLeftRotate_param_i64_I
	.quad	_L_1+496
	.quad	_LeftRotate__uLeftRotate_var_i64_I
	.quad	_L_1+520
	.quad	_LeftRotate__uLeftRotate_param_I_C
	.quad	_L_1+542
	.quad	_LeftRotate__uLeftRotate_var_I_C
	.quad	_L_1+564
	.quad	_LeftRotate__uLeftRotate_param_I_I
	.quad	_L_1+584
	.quad	_LeftRotate__uLeftRotate_var_I_I
	.quad	_L_1+606
	.quad	_LeftRotate__uLeftRotate_param_i32_C
	.quad	_L_1+626
	.quad	_LeftRotate__uLeftRotate_var_i32_C
	.quad	_L_1+650
	.quad	_LeftRotate__uLeftRotate_param_i32_I
	.quad	_L_1+672
	.quad	_LeftRotate__uLeftRotate_var_i32_I
	.quad	_L_1+696
	.quad	_LeftRotate__uLeftRotate_param_i16_C
	.quad	_L_1+718
	.quad	_LeftRotate__uLeftRotate_var_i16_C
	.quad	_L_1+742
	.quad	_LeftRotate__uLeftRotate_param_i16_I
	.quad	_L_1+764
	.quad	_LeftRotate__uLeftRotate_var_i16_I
	.quad	_L_1+788
	.quad	_LeftRotate__uLeftRotate_param_LC_C
	.quad	_L_1+810
	.quad	_LeftRotate__uLeftRotate_var_LC_C
	.quad	_L_1+833
	.quad	_LeftRotate__uLeftRotate_param_LC_I
	.quad	_L_1+854
	.quad	_LeftRotate__uLeftRotate_var_LC_I
	.quad	_L_1+877
	.quad	_LeftRotate__uLeftRotate_param_u64_C
	.quad	_L_1+898
	.quad	_LeftRotate__uLeftRotate_var_u64_C
	.quad	_L_1+922
	.quad	_LeftRotate__uLeftRotate_param_u64_I
	.quad	_L_1+944
	.quad	_LeftRotate__uLeftRotate_var_u64_I
	.quad	_L_1+968
	.quad	_LeftRotate__uLeftRotate_param_u16_C
	.quad	_L_1+990
	.quad	_LeftRotate__uLeftRotate_var_u16_C
	.quad	_L_1+1014
	.quad	_LeftRotate__uLeftRotate_param_u16_I
	.quad	_L_1+1036
	.quad	_LeftRotate__uLeftRotate_var_u16_I
	.quad	_L_1+1060
	.space 8
	.ascii "../AMD64_DARWIN/LeftRotate.m3"
	.space 3
	.data
	.align 5
_MM_LeftRotate:
	.quad	_L_1+1880
	.space 32
	.quad	_L_1+1088
	.space 24
	.quad	_MM_LeftRotate+208
	.space 8
	.quad	_LeftRotate_M3
	.quad	3
	.word	721
	.space 6
	.quad	722
	.quad	723
	.long	3435973837
	.long	1082566092
	.word	726
	.space 2
	.long	727
	.quad	728
	.quad	729
	.quad	730
	.long	1144450777
	.byte	-35
	.byte	-34
	.space 2
	.quad	735
	.long	736
	.space 28
	.quad	_LeftRotate_I3
	.quad	_MM_LeftRotate+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_LeftRotate+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_LeftRotate+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_LeftRotate+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
