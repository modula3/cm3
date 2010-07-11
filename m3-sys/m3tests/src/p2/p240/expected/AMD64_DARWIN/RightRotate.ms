	.text
.globl _RightRotate__uRightRotate_var_i64_I
	.private_extern _RightRotate__uRightRotate_var_i64_I
_RightRotate__uRightRotate_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L2
	cmpq	$63, -16(%rbp)
	jle	L3
L2:
	movl	$1729, %edi
	call	__m3_fault
L3:
	movq	104+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i64_I
	.private_extern _RightRotate__uRightRotate_param_i64_I
_RightRotate__uRightRotate_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
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
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i64_C
	.private_extern _RightRotate__uRightRotate_var_i64_C
_RightRotate__uRightRotate_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L10
	movl	$1793, %edi
	call	__m3_fault
L10:
	movq	104+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i64_C
	.private_extern _RightRotate__uRightRotate_param_i64_C
_RightRotate__uRightRotate_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L13
	movl	$1825, %edi
	call	__m3_fault
L13:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_LC_I
	.private_extern _RightRotate__uRightRotate_var_LC_I
_RightRotate__uRightRotate_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L16
	cmpq	$63, -16(%rbp)
	jle	L17
L16:
	movl	$1857, %edi
	call	__m3_fault
L17:
	movq	112+_MM_RightRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_LC_I
	.private_extern _RightRotate__uRightRotate_param_LC_I
_RightRotate__uRightRotate_param_LC_I:
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
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_LC_C
	.private_extern _RightRotate__uRightRotate_var_LC_C
_RightRotate__uRightRotate_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L24
	movl	$1921, %edi
	call	__m3_fault
L24:
	movq	112+_MM_RightRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_LC_C
	.private_extern _RightRotate__uRightRotate_param_LC_C
_RightRotate__uRightRotate_param_LC_C:
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
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u8_I
	.private_extern _RightRotate__uRightRotate_var_u8_I
_RightRotate__uRightRotate_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L30
	cmpq	$63, -16(%rbp)
	jle	L31
L30:
	movl	$1985, %edi
	call	__m3_fault
L31:
	movzbl	124+_MM_RightRotate(%rip), %eax
	movsbq	%al,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u8_I
	.private_extern _RightRotate__uRightRotate_param_u8_I
_RightRotate__uRightRotate_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
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
	movsbq	-17(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u8_C
	.private_extern _RightRotate__uRightRotate_var_u8_C
_RightRotate__uRightRotate_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L38
	movl	$2049, %edi
	call	__m3_fault
L38:
	movzbl	124+_MM_RightRotate(%rip), %eax
	movsbq	%al,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u8_C
	.private_extern _RightRotate__uRightRotate_param_u8_C
_RightRotate__uRightRotate_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L41
	movl	$2081, %edi
	call	__m3_fault
L41:
	movsbq	-17(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_I_I
	.private_extern _RightRotate__uRightRotate_var_I_I
_RightRotate__uRightRotate_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L44
	cmpq	$63, -16(%rbp)
	jle	L45
L44:
	movl	$2113, %edi
	call	__m3_fault
L45:
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_I_I
	.private_extern _RightRotate__uRightRotate_param_I_I
_RightRotate__uRightRotate_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
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
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_I_C
	.private_extern _RightRotate__uRightRotate_var_I_C
_RightRotate__uRightRotate_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L52
	movl	$2177, %edi
	call	__m3_fault
L52:
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_I_C
	.private_extern _RightRotate__uRightRotate_param_I_C
_RightRotate__uRightRotate_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L55
	movl	$2209, %edi
	call	__m3_fault
L55:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i8_I
	.private_extern _RightRotate__uRightRotate_var_i8_I
_RightRotate__uRightRotate_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L58
	cmpq	$63, -16(%rbp)
	jle	L59
L58:
	movl	$2241, %edi
	call	__m3_fault
L59:
	movzbl	136+_MM_RightRotate(%rip), %eax
	movzbl	%al, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i8_I
	.private_extern _RightRotate__uRightRotate_param_i8_I
_RightRotate__uRightRotate_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
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
	movzbl	-17(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i8_C
	.private_extern _RightRotate__uRightRotate_var_i8_C
_RightRotate__uRightRotate_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L66
	movl	$2305, %edi
	call	__m3_fault
L66:
	movzbl	136+_MM_RightRotate(%rip), %eax
	movzbl	%al, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i8_C
	.private_extern _RightRotate__uRightRotate_param_i8_C
_RightRotate__uRightRotate_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L69
	movl	$2337, %edi
	call	__m3_fault
L69:
	movzbl	-17(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u32_I
	.private_extern _RightRotate__uRightRotate_var_u32_I
_RightRotate__uRightRotate_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L72
	cmpq	$63, -16(%rbp)
	jle	L73
L72:
	movl	$2369, %edi
	call	__m3_fault
L73:
	movl	140+_MM_RightRotate(%rip), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u32_I
	.private_extern _RightRotate__uRightRotate_param_u32_I
_RightRotate__uRightRotate_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
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
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u32_C
	.private_extern _RightRotate__uRightRotate_var_u32_C
_RightRotate__uRightRotate_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L80
	movl	$2433, %edi
	call	__m3_fault
L80:
	movl	140+_MM_RightRotate(%rip), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u32_C
	.private_extern _RightRotate__uRightRotate_param_u32_C
_RightRotate__uRightRotate_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L83
	movl	$2465, %edi
	call	__m3_fault
L83:
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_C_I
	.private_extern _RightRotate__uRightRotate_var_C_I
_RightRotate__uRightRotate_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L86
	cmpq	$63, -16(%rbp)
	jle	L87
L86:
	movl	$2497, %edi
	call	__m3_fault
L87:
	movq	144+_MM_RightRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_C_I
	.private_extern _RightRotate__uRightRotate_param_C_I
_RightRotate__uRightRotate_param_C_I:
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
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_C_C
	.private_extern _RightRotate__uRightRotate_var_C_C
_RightRotate__uRightRotate_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L94
	movl	$2561, %edi
	call	__m3_fault
L94:
	movq	144+_MM_RightRotate(%rip), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_C_C
	.private_extern _RightRotate__uRightRotate_param_C_C
_RightRotate__uRightRotate_param_C_C:
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
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u16_I
	.private_extern _RightRotate__uRightRotate_var_u16_I
_RightRotate__uRightRotate_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L100
	cmpq	$63, -16(%rbp)
	jle	L101
L100:
	movl	$2625, %edi
	call	__m3_fault
L101:
	movzwl	152+_MM_RightRotate(%rip), %eax
	movswq	%ax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u16_I
	.private_extern _RightRotate__uRightRotate_param_u16_I
_RightRotate__uRightRotate_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
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
	movswq	-18(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u16_C
	.private_extern _RightRotate__uRightRotate_var_u16_C
_RightRotate__uRightRotate_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L108
	movl	$2689, %edi
	call	__m3_fault
L108:
	movzwl	152+_MM_RightRotate(%rip), %eax
	movswq	%ax,%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u16_C
	.private_extern _RightRotate__uRightRotate_param_u16_C
_RightRotate__uRightRotate_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L111
	movl	$2721, %edi
	call	__m3_fault
L111:
	movswq	-18(%rbp),%rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u64_I
	.private_extern _RightRotate__uRightRotate_var_u64_I
_RightRotate__uRightRotate_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L114
	cmpq	$63, -16(%rbp)
	jle	L115
L114:
	movl	$2753, %edi
	call	__m3_fault
L115:
	movq	160+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u64_I
	.private_extern _RightRotate__uRightRotate_param_u64_I
_RightRotate__uRightRotate_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
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
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_u64_C
	.private_extern _RightRotate__uRightRotate_var_u64_C
_RightRotate__uRightRotate_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L122
	movl	$2817, %edi
	call	__m3_fault
L122:
	movq	160+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_u64_C
	.private_extern _RightRotate__uRightRotate_param_u64_C
_RightRotate__uRightRotate_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L125
	movl	$2849, %edi
	call	__m3_fault
L125:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_L_I
	.private_extern _RightRotate__uRightRotate_var_L_I
_RightRotate__uRightRotate_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L128
	cmpq	$63, -16(%rbp)
	jle	L129
L128:
	movl	$2881, %edi
	call	__m3_fault
L129:
	movq	176+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_L_I
	.private_extern _RightRotate__uRightRotate_param_L_I
_RightRotate__uRightRotate_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
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
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_L_C
	.private_extern _RightRotate__uRightRotate_var_L_C
_RightRotate__uRightRotate_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L136
	movl	$2945, %edi
	call	__m3_fault
L136:
	movq	176+_MM_RightRotate(%rip), %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_L_C
	.private_extern _RightRotate__uRightRotate_param_L_C
_RightRotate__uRightRotate_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L139
	movl	$2977, %edi
	call	__m3_fault
L139:
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i16_I
	.private_extern _RightRotate__uRightRotate_var_i16_I
_RightRotate__uRightRotate_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L142
	cmpq	$63, -16(%rbp)
	jle	L143
L142:
	movl	$3009, %edi
	call	__m3_fault
L143:
	movzwl	184+_MM_RightRotate(%rip), %eax
	movzwl	%ax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i16_I
	.private_extern _RightRotate__uRightRotate_param_i16_I
_RightRotate__uRightRotate_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
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
	movzwl	-18(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i16_C
	.private_extern _RightRotate__uRightRotate_var_i16_C
_RightRotate__uRightRotate_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L150
	movl	$3073, %edi
	call	__m3_fault
L150:
	movzwl	184+_MM_RightRotate(%rip), %eax
	movzwl	%ax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i16_C
	.private_extern _RightRotate__uRightRotate_param_i16_C
_RightRotate__uRightRotate_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L153
	movl	$3105, %edi
	call	__m3_fault
L153:
	movzwl	-18(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i32_I
	.private_extern _RightRotate__uRightRotate_var_i32_I
_RightRotate__uRightRotate_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L156
	cmpq	$63, -16(%rbp)
	jle	L157
L156:
	movl	$3137, %edi
	call	__m3_fault
L157:
	movl	188+_MM_RightRotate(%rip), %eax
	mov	%eax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i32_I
	.private_extern _RightRotate__uRightRotate_param_i32_I
_RightRotate__uRightRotate_param_i32_I:
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
	mov	-20(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_var_i32_C
	.private_extern _RightRotate__uRightRotate_var_i32_C
_RightRotate__uRightRotate_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_RightRotate(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L164
	movl	$3201, %edi
	call	__m3_fault
L164:
	movl	188+_MM_RightRotate(%rip), %eax
	mov	%eax, %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate__uRightRotate_param_i32_C
	.private_extern _RightRotate__uRightRotate_param_i32_C
_RightRotate__uRightRotate_param_i32_C:
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
	mov	-20(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	%rdx, %rax
	rorq	%cl, %rax
	leave
	ret
.globl _RightRotate_M3
_RightRotate_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_RightRotate(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_RightRotate(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "RightRotate_M3"
	.space 1
	.ascii "uRightRotate_param_i32_C"
	.space 1
	.ascii "uRightRotate_var_i32_C"
	.space 1
	.ascii "uRightRotate_param_i32_I"
	.space 1
	.ascii "uRightRotate_var_i32_I"
	.space 1
	.ascii "uRightRotate_param_i16_C"
	.space 1
	.ascii "uRightRotate_var_i16_C"
	.space 1
	.ascii "uRightRotate_param_i16_I"
	.space 1
	.ascii "uRightRotate_var_i16_I"
	.space 1
	.ascii "uRightRotate_param_L_C"
	.space 1
	.ascii "uRightRotate_var_L_C"
	.space 1
	.ascii "uRightRotate_param_L_I"
	.space 1
	.ascii "uRightRotate_var_L_I"
	.space 1
	.ascii "uRightRotate_param_u64_C"
	.space 1
	.ascii "uRightRotate_var_u64_C"
	.space 1
	.ascii "uRightRotate_param_u64_I"
	.space 1
	.ascii "uRightRotate_var_u64_I"
	.space 1
	.ascii "uRightRotate_param_u16_C"
	.space 1
	.ascii "uRightRotate_var_u16_C"
	.space 1
	.ascii "uRightRotate_param_u16_I"
	.space 1
	.ascii "uRightRotate_var_u16_I"
	.space 1
	.ascii "uRightRotate_param_C_C"
	.space 1
	.ascii "uRightRotate_var_C_C"
	.space 1
	.ascii "uRightRotate_param_C_I"
	.space 1
	.ascii "uRightRotate_var_C_I"
	.space 1
	.ascii "uRightRotate_param_u32_C"
	.space 1
	.ascii "uRightRotate_var_u32_C"
	.space 1
	.ascii "uRightRotate_param_u32_I"
	.space 1
	.ascii "uRightRotate_var_u32_I"
	.space 1
	.ascii "uRightRotate_param_i8_C"
	.space 1
	.ascii "uRightRotate_var_i8_C"
	.space 1
	.ascii "uRightRotate_param_i8_I"
	.space 1
	.ascii "uRightRotate_var_i8_I"
	.space 1
	.ascii "uRightRotate_param_I_C"
	.space 1
	.ascii "uRightRotate_var_I_C"
	.space 1
	.ascii "uRightRotate_param_I_I"
	.space 1
	.ascii "uRightRotate_var_I_I"
	.space 1
	.ascii "uRightRotate_param_u8_C"
	.space 1
	.ascii "uRightRotate_var_u8_C"
	.space 1
	.ascii "uRightRotate_param_u8_I"
	.space 1
	.ascii "uRightRotate_var_u8_I"
	.space 1
	.ascii "uRightRotate_param_LC_C"
	.space 1
	.ascii "uRightRotate_var_LC_C"
	.space 1
	.ascii "uRightRotate_param_LC_I"
	.space 1
	.ascii "uRightRotate_var_LC_I"
	.space 1
	.ascii "uRightRotate_param_i64_C"
	.space 1
	.ascii "uRightRotate_var_i64_C"
	.space 1
	.ascii "uRightRotate_param_i64_I"
	.space 1
	.ascii "uRightRotate_var_i64_I"
	.space 6
	.quad	_RightRotate_M3
	.quad	_L_1
	.quad	_RightRotate__uRightRotate_param_i32_C
	.quad	_L_1+15
	.quad	_RightRotate__uRightRotate_var_i32_C
	.quad	_L_1+40
	.quad	_RightRotate__uRightRotate_param_i32_I
	.quad	_L_1+63
	.quad	_RightRotate__uRightRotate_var_i32_I
	.quad	_L_1+88
	.quad	_RightRotate__uRightRotate_param_i16_C
	.quad	_L_1+111
	.quad	_RightRotate__uRightRotate_var_i16_C
	.quad	_L_1+136
	.quad	_RightRotate__uRightRotate_param_i16_I
	.quad	_L_1+159
	.quad	_RightRotate__uRightRotate_var_i16_I
	.quad	_L_1+184
	.quad	_RightRotate__uRightRotate_param_L_C
	.quad	_L_1+207
	.quad	_RightRotate__uRightRotate_var_L_C
	.quad	_L_1+230
	.quad	_RightRotate__uRightRotate_param_L_I
	.quad	_L_1+251
	.quad	_RightRotate__uRightRotate_var_L_I
	.quad	_L_1+274
	.quad	_RightRotate__uRightRotate_param_u64_C
	.quad	_L_1+295
	.quad	_RightRotate__uRightRotate_var_u64_C
	.quad	_L_1+320
	.quad	_RightRotate__uRightRotate_param_u64_I
	.quad	_L_1+343
	.quad	_RightRotate__uRightRotate_var_u64_I
	.quad	_L_1+368
	.quad	_RightRotate__uRightRotate_param_u16_C
	.quad	_L_1+391
	.quad	_RightRotate__uRightRotate_var_u16_C
	.quad	_L_1+416
	.quad	_RightRotate__uRightRotate_param_u16_I
	.quad	_L_1+439
	.quad	_RightRotate__uRightRotate_var_u16_I
	.quad	_L_1+464
	.quad	_RightRotate__uRightRotate_param_C_C
	.quad	_L_1+487
	.quad	_RightRotate__uRightRotate_var_C_C
	.quad	_L_1+510
	.quad	_RightRotate__uRightRotate_param_C_I
	.quad	_L_1+531
	.quad	_RightRotate__uRightRotate_var_C_I
	.quad	_L_1+554
	.quad	_RightRotate__uRightRotate_param_u32_C
	.quad	_L_1+575
	.quad	_RightRotate__uRightRotate_var_u32_C
	.quad	_L_1+600
	.quad	_RightRotate__uRightRotate_param_u32_I
	.quad	_L_1+623
	.quad	_RightRotate__uRightRotate_var_u32_I
	.quad	_L_1+648
	.quad	_RightRotate__uRightRotate_param_i8_C
	.quad	_L_1+671
	.quad	_RightRotate__uRightRotate_var_i8_C
	.quad	_L_1+695
	.quad	_RightRotate__uRightRotate_param_i8_I
	.quad	_L_1+717
	.quad	_RightRotate__uRightRotate_var_i8_I
	.quad	_L_1+741
	.quad	_RightRotate__uRightRotate_param_I_C
	.quad	_L_1+763
	.quad	_RightRotate__uRightRotate_var_I_C
	.quad	_L_1+786
	.quad	_RightRotate__uRightRotate_param_I_I
	.quad	_L_1+807
	.quad	_RightRotate__uRightRotate_var_I_I
	.quad	_L_1+830
	.quad	_RightRotate__uRightRotate_param_u8_C
	.quad	_L_1+851
	.quad	_RightRotate__uRightRotate_var_u8_C
	.quad	_L_1+875
	.quad	_RightRotate__uRightRotate_param_u8_I
	.quad	_L_1+897
	.quad	_RightRotate__uRightRotate_var_u8_I
	.quad	_L_1+921
	.quad	_RightRotate__uRightRotate_param_LC_C
	.quad	_L_1+943
	.quad	_RightRotate__uRightRotate_var_LC_C
	.quad	_L_1+967
	.quad	_RightRotate__uRightRotate_param_LC_I
	.quad	_L_1+989
	.quad	_RightRotate__uRightRotate_var_LC_I
	.quad	_L_1+1013
	.quad	_RightRotate__uRightRotate_param_i64_C
	.quad	_L_1+1035
	.quad	_RightRotate__uRightRotate_var_i64_C
	.quad	_L_1+1060
	.quad	_RightRotate__uRightRotate_param_i64_I
	.quad	_L_1+1083
	.quad	_RightRotate__uRightRotate_var_i64_I
	.quad	_L_1+1108
	.space 8
	.ascii "../AMD64_DARWIN/RightRotate.m3"
	.space 2
	.data
	.align 5
_MM_RightRotate:
	.quad	_L_1+1928
	.space 32
	.quad	_L_1+1136
	.space 24
	.quad	_MM_RightRotate+208
	.space 8
	.quad	_RightRotate_M3
	.quad	3
	.quad	753
	.quad	754
	.long	1144844386
	.byte	-11
	.space 3
	.quad	758
	.byte	-9
	.space 3
	.long	760
	.quad	761
	.word	762
	.space 6
	.quad	763
	.long	3092376453
	.long	1082648094
	.quad	766
	.word	767
	.space 2
	.long	768
	.space 24
	.quad	_RightRotate_I3
	.quad	_MM_RightRotate+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_RightRotate+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_RightRotate+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_RightRotate+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
