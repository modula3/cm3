	.text
.globl _Minus__uMinus_var_u16_u16
	.private_extern _Minus__uMinus_var_u16_u16
_Minus__uMinus_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_u16_u16
	.private_extern _Minus__Minus_var_u16_u16
_Minus__Minus_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L4
	cmpq	$32767, -16(%rbp)
	jle	L5
L4:
	movl	$1761, %edi
	call	__m3_fault
L5:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_u16
	.private_extern _Minus__uMinus_param_u16_u16
_Minus__uMinus_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_u16
	.private_extern _Minus__Minus_param_u16_u16
_Minus__Minus_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movswq	-20(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L10
	cmpq	$32767, -16(%rbp)
	jle	L11
L10:
	movl	$1825, %edi
	call	__m3_fault
L11:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_u64
	.private_extern _Minus__uMinus_var_u16_u64
_Minus__uMinus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_u64
	.private_extern _Minus__Minus_var_u16_u64
_Minus__Minus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_u64
	.private_extern _Minus__uMinus_param_u16_u64
_Minus__uMinus_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_u64
	.private_extern _Minus__Minus_param_u16_u64
_Minus__Minus_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_LC
	.private_extern _Minus__uMinus_var_u16_LC
_Minus__uMinus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_LC
	.private_extern _Minus__Minus_var_u16_LC
_Minus__Minus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_LC
	.private_extern _Minus__uMinus_param_u16_LC
_Minus__uMinus_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u16_LC
	.private_extern _Minus__Minus_param_u16_LC
_Minus__Minus_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i16
	.private_extern _Minus__uMinus_var_u16_i16
_Minus__uMinus_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_i16
	.private_extern _Minus__Minus_var_u16_i16
_Minus__Minus_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L32
	movl	$2145, %edi
	call	__m3_fault
L32:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_i16
	.private_extern _Minus__uMinus_param_u16_i16
_Minus__uMinus_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_i16
	.private_extern _Minus__Minus_param_u16_i16
_Minus__Minus_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movzwl	-20(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L37
	movl	$2209, %edi
	call	__m3_fault
L37:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i32
	.private_extern _Minus__uMinus_var_u16_i32
_Minus__uMinus_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_i32
	.private_extern _Minus__Minus_var_u16_i32
_Minus__Minus_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L42
	movl	$2273, %edi
	call	__m3_fault
L42:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_i32
	.private_extern _Minus__uMinus_param_u16_i32
_Minus__uMinus_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_i32
	.private_extern _Minus__Minus_param_u16_i32
_Minus__Minus_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	mov	-24(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L47
	movl	$2337, %edi
	call	__m3_fault
L47:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_I
	.private_extern _Minus__uMinus_var_u16_I
_Minus__uMinus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_I
	.private_extern _Minus__Minus_var_u16_I
_Minus__Minus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L52
	cmpq	$32767, -16(%rbp)
	jle	L53
L52:
	movl	$2401, %edi
	call	__m3_fault
L53:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_I
	.private_extern _Minus__uMinus_param_u16_I
_Minus__uMinus_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_I
	.private_extern _Minus__Minus_param_u16_I
_Minus__Minus_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L58
	cmpq	$32767, -16(%rbp)
	jle	L59
L58:
	movl	$2465, %edi
	call	__m3_fault
L59:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i64
	.private_extern _Minus__uMinus_var_u16_i64
_Minus__uMinus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_i64
	.private_extern _Minus__Minus_var_u16_i64
_Minus__Minus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_i64
	.private_extern _Minus__uMinus_param_u16_i64
_Minus__uMinus_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_i64
	.private_extern _Minus__Minus_param_u16_i64
_Minus__Minus_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_C
	.private_extern _Minus__uMinus_var_u16_C
_Minus__uMinus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_C
	.private_extern _Minus__Minus_var_u16_C
_Minus__Minus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L72
	cmpq	$32767, -16(%rbp)
	jle	L73
L72:
	movl	$2657, %edi
	call	__m3_fault
L73:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_C
	.private_extern _Minus__uMinus_param_u16_C
_Minus__uMinus_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u16_C
	.private_extern _Minus__Minus_param_u16_C
_Minus__Minus_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	-32(%rbp), %rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L78
	cmpq	$32767, -16(%rbp)
	jle	L79
L78:
	movl	$2721, %edi
	call	__m3_fault
L79:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_u8
	.private_extern _Minus__uMinus_var_u16_u8
_Minus__uMinus_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_u8
	.private_extern _Minus__Minus_var_u16_u8
_Minus__Minus_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L84
	cmpq	$32767, -16(%rbp)
	jle	L85
L84:
	movl	$2785, %edi
	call	__m3_fault
L85:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_u8
	.private_extern _Minus__uMinus_param_u16_u8
_Minus__uMinus_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_u8
	.private_extern _Minus__Minus_param_u16_u8
_Minus__Minus_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movsbq	-19(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L90
	cmpq	$32767, -16(%rbp)
	jle	L91
L90:
	movl	$2849, %edi
	call	__m3_fault
L91:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i8
	.private_extern _Minus__uMinus_var_u16_i8
_Minus__uMinus_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_i8
	.private_extern _Minus__Minus_var_u16_i8
_Minus__Minus_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L96
	movl	$2913, %edi
	call	__m3_fault
L96:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_i8
	.private_extern _Minus__uMinus_param_u16_i8
_Minus__uMinus_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_i8
	.private_extern _Minus__Minus_param_u16_i8
_Minus__Minus_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movzbl	-19(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L101
	movl	$2977, %edi
	call	__m3_fault
L101:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_L
	.private_extern _Minus__uMinus_var_u16_L
_Minus__uMinus_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_L
	.private_extern _Minus__Minus_var_u16_L
_Minus__Minus_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_L
	.private_extern _Minus__uMinus_param_u16_L
_Minus__uMinus_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_L
	.private_extern _Minus__Minus_param_u16_L
_Minus__Minus_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_u32
	.private_extern _Minus__uMinus_var_u16_u32
_Minus__uMinus_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u16_u32
	.private_extern _Minus__Minus_var_u16_u32
_Minus__Minus_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L114
	cmpq	$32767, -16(%rbp)
	jle	L115
L114:
	movl	$3169, %edi
	call	__m3_fault
L115:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u16_u32
	.private_extern _Minus__uMinus_param_u16_u32
_Minus__uMinus_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u16_u32
	.private_extern _Minus__Minus_param_u16_u32
_Minus__Minus_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L120
	cmpq	$32767, -16(%rbp)
	jle	L121
L120:
	movl	$3233, %edi
	call	__m3_fault
L121:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_u16
	.private_extern _Minus__uMinus_var_u64_u16
_Minus__uMinus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_u16
	.private_extern _Minus__Minus_var_u64_u16
_Minus__Minus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_u16
	.private_extern _Minus__uMinus_param_u64_u16
_Minus__uMinus_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_u16
	.private_extern _Minus__Minus_param_u64_u16
_Minus__Minus_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_u64
	.private_extern _Minus__uMinus_var_u64_u64
_Minus__uMinus_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_u64_u64
	.private_extern _Minus__Minus_var_u64_u64
_Minus__Minus_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__uMinus_param_u64_u64
	.private_extern _Minus__uMinus_param_u64_u64
_Minus__uMinus_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_u64
	.private_extern _Minus__Minus_param_u64_u64
_Minus__Minus_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_LC
	.private_extern _Minus__uMinus_var_u64_LC
_Minus__uMinus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_LC
	.private_extern _Minus__Minus_var_u64_LC
_Minus__Minus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_LC
	.private_extern _Minus__uMinus_param_u64_LC
_Minus__uMinus_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u64_LC
	.private_extern _Minus__Minus_param_u64_LC
_Minus__Minus_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_i16
	.private_extern _Minus__uMinus_var_u64_i16
_Minus__uMinus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_i16
	.private_extern _Minus__Minus_var_u64_i16
_Minus__Minus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_i16
	.private_extern _Minus__uMinus_param_u64_i16
_Minus__uMinus_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_i16
	.private_extern _Minus__Minus_param_u64_i16
_Minus__Minus_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_i32
	.private_extern _Minus__uMinus_var_u64_i32
_Minus__uMinus_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_i32
	.private_extern _Minus__Minus_var_u64_i32
_Minus__Minus_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_i32
	.private_extern _Minus__uMinus_param_u64_i32
_Minus__uMinus_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_i32
	.private_extern _Minus__Minus_param_u64_i32
_Minus__Minus_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_I
	.private_extern _Minus__uMinus_var_u64_I
_Minus__uMinus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_I
	.private_extern _Minus__Minus_var_u64_I
_Minus__Minus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_I
	.private_extern _Minus__uMinus_param_u64_I
_Minus__uMinus_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_I
	.private_extern _Minus__Minus_param_u64_I
_Minus__Minus_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_i64
	.private_extern _Minus__uMinus_var_u64_i64
_Minus__uMinus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_i64
	.private_extern _Minus__Minus_var_u64_i64
_Minus__Minus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_i64
	.private_extern _Minus__uMinus_param_u64_i64
_Minus__uMinus_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_i64
	.private_extern _Minus__Minus_param_u64_i64
_Minus__Minus_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_C
	.private_extern _Minus__uMinus_var_u64_C
_Minus__uMinus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_C
	.private_extern _Minus__Minus_var_u64_C
_Minus__Minus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_C
	.private_extern _Minus__uMinus_param_u64_C
_Minus__uMinus_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u64_C
	.private_extern _Minus__Minus_param_u64_C
_Minus__Minus_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_u8
	.private_extern _Minus__uMinus_var_u64_u8
_Minus__uMinus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_u8
	.private_extern _Minus__Minus_var_u64_u8
_Minus__Minus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_u8
	.private_extern _Minus__uMinus_param_u64_u8
_Minus__uMinus_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_u8
	.private_extern _Minus__Minus_param_u64_u8
_Minus__Minus_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_i8
	.private_extern _Minus__uMinus_var_u64_i8
_Minus__uMinus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_i8
	.private_extern _Minus__Minus_var_u64_i8
_Minus__Minus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_i8
	.private_extern _Minus__uMinus_param_u64_i8
_Minus__uMinus_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_i8
	.private_extern _Minus__Minus_param_u64_i8
_Minus__Minus_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_L
	.private_extern _Minus__uMinus_var_u64_L
_Minus__uMinus_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_L
	.private_extern _Minus__Minus_var_u64_L
_Minus__Minus_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_L
	.private_extern _Minus__uMinus_param_u64_L
_Minus__uMinus_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_L
	.private_extern _Minus__Minus_param_u64_L
_Minus__Minus_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_u32
	.private_extern _Minus__uMinus_var_u64_u32
_Minus__uMinus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u64_u32
	.private_extern _Minus__Minus_var_u64_u32
_Minus__Minus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u64_u32
	.private_extern _Minus__uMinus_param_u64_u32
_Minus__uMinus_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u64_u32
	.private_extern _Minus__Minus_param_u64_u32
_Minus__Minus_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_u16
	.private_extern _Minus__uMinus_var_LC_u16
_Minus__uMinus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_u16
	.private_extern _Minus__Minus_var_LC_u16
_Minus__Minus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_u16
	.private_extern _Minus__uMinus_param_LC_u16
_Minus__uMinus_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_u16
	.private_extern _Minus__Minus_param_LC_u16
_Minus__Minus_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_u64
	.private_extern _Minus__uMinus_var_LC_u64
_Minus__uMinus_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_u64
	.private_extern _Minus__Minus_var_LC_u64
_Minus__Minus_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_u64
	.private_extern _Minus__uMinus_param_LC_u64
_Minus__uMinus_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_u64
	.private_extern _Minus__Minus_param_LC_u64
_Minus__Minus_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_LC
	.private_extern _Minus__uMinus_var_LC_LC
_Minus__uMinus_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_LC_LC
	.private_extern _Minus__Minus_var_LC_LC
_Minus__Minus_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__uMinus_param_LC_LC
	.private_extern _Minus__uMinus_param_LC_LC
_Minus__uMinus_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_LC
	.private_extern _Minus__Minus_param_LC_LC
_Minus__Minus_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_i16
	.private_extern _Minus__uMinus_var_LC_i16
_Minus__uMinus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_i16
	.private_extern _Minus__Minus_var_LC_i16
_Minus__Minus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_i16
	.private_extern _Minus__uMinus_param_LC_i16
_Minus__uMinus_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_i16
	.private_extern _Minus__Minus_param_LC_i16
_Minus__Minus_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_i32
	.private_extern _Minus__uMinus_var_LC_i32
_Minus__uMinus_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_i32
	.private_extern _Minus__Minus_var_LC_i32
_Minus__Minus_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_i32
	.private_extern _Minus__uMinus_param_LC_i32
_Minus__uMinus_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_i32
	.private_extern _Minus__Minus_param_LC_i32
_Minus__Minus_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_I
	.private_extern _Minus__uMinus_var_LC_I
_Minus__uMinus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_I
	.private_extern _Minus__Minus_var_LC_I
_Minus__Minus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_I
	.private_extern _Minus__uMinus_param_LC_I
_Minus__uMinus_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_I
	.private_extern _Minus__Minus_param_LC_I
_Minus__Minus_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_i64
	.private_extern _Minus__uMinus_var_LC_i64
_Minus__uMinus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_i64
	.private_extern _Minus__Minus_var_LC_i64
_Minus__Minus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_i64
	.private_extern _Minus__uMinus_param_LC_i64
_Minus__uMinus_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_i64
	.private_extern _Minus__Minus_param_LC_i64
_Minus__Minus_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_C
	.private_extern _Minus__uMinus_var_LC_C
_Minus__uMinus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_C
	.private_extern _Minus__Minus_var_LC_C
_Minus__Minus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_C
	.private_extern _Minus__uMinus_param_LC_C
_Minus__uMinus_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_C
	.private_extern _Minus__Minus_param_LC_C
_Minus__Minus_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_u8
	.private_extern _Minus__uMinus_var_LC_u8
_Minus__uMinus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_u8
	.private_extern _Minus__Minus_var_LC_u8
_Minus__Minus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_u8
	.private_extern _Minus__uMinus_param_LC_u8
_Minus__uMinus_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_u8
	.private_extern _Minus__Minus_param_LC_u8
_Minus__Minus_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_i8
	.private_extern _Minus__uMinus_var_LC_i8
_Minus__uMinus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_i8
	.private_extern _Minus__Minus_var_LC_i8
_Minus__Minus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_i8
	.private_extern _Minus__uMinus_param_LC_i8
_Minus__uMinus_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_i8
	.private_extern _Minus__Minus_param_LC_i8
_Minus__Minus_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_L
	.private_extern _Minus__uMinus_var_LC_L
_Minus__uMinus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_L
	.private_extern _Minus__Minus_var_LC_L
_Minus__Minus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_L
	.private_extern _Minus__uMinus_param_LC_L
_Minus__uMinus_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_L
	.private_extern _Minus__Minus_param_LC_L
_Minus__Minus_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_LC_u32
	.private_extern _Minus__uMinus_var_LC_u32
_Minus__uMinus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_LC_u32
	.private_extern _Minus__Minus_var_LC_u32
_Minus__Minus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_LC_u32
	.private_extern _Minus__uMinus_param_LC_u32
_Minus__uMinus_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_LC_u32
	.private_extern _Minus__Minus_param_LC_u32
_Minus__Minus_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_f64_f64
	.private_extern _Minus__Minus_var_f64_f64
_Minus__Minus_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_Minus(%rip), %xmm1
	movsd	128+_MM_Minus(%rip), %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
	ret
.globl _Minus__Minus_param_f64_f64
	.private_extern _Minus__Minus_param_f64_f64
_Minus__Minus_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm0
	subsd	-32(%rbp), %xmm0
	leave
	ret
.globl _Minus__uMinus_var_i16_u16
	.private_extern _Minus__uMinus_var_i16_u16
_Minus__uMinus_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_u16
	.private_extern _Minus__Minus_var_i16_u16
_Minus__Minus_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L322
	cmpq	$65535, -16(%rbp)
	jle	L323
L322:
	movl	$6433, %edi
	call	__m3_fault
L323:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_u16
	.private_extern _Minus__uMinus_param_i16_u16
_Minus__uMinus_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_u16
	.private_extern _Minus__Minus_param_i16_u16
_Minus__Minus_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	-20(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L328
	cmpq	$65535, -16(%rbp)
	jle	L329
L328:
	movl	$6497, %edi
	call	__m3_fault
L329:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_u64
	.private_extern _Minus__uMinus_var_i16_u64
_Minus__uMinus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_u64
	.private_extern _Minus__Minus_var_i16_u64
_Minus__Minus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_u64
	.private_extern _Minus__uMinus_param_i16_u64
_Minus__uMinus_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_u64
	.private_extern _Minus__Minus_param_i16_u64
_Minus__Minus_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_LC
	.private_extern _Minus__uMinus_var_i16_LC
_Minus__uMinus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_LC
	.private_extern _Minus__Minus_var_i16_LC
_Minus__Minus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_LC
	.private_extern _Minus__uMinus_param_i16_LC
_Minus__uMinus_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i16_LC
	.private_extern _Minus__Minus_param_i16_LC
_Minus__Minus_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_i16
	.private_extern _Minus__uMinus_var_i16_i16
_Minus__uMinus_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_i16_i16
	.private_extern _Minus__Minus_var_i16_i16
_Minus__Minus_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L350
	movl	$6817, %edi
	call	__m3_fault
L350:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_i16
	.private_extern _Minus__uMinus_param_i16_i16
_Minus__uMinus_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_i16
	.private_extern _Minus__Minus_param_i16_i16
_Minus__Minus_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movzwl	-20(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L355
	movl	$6881, %edi
	call	__m3_fault
L355:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_i32
	.private_extern _Minus__uMinus_var_i16_i32
_Minus__uMinus_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_i32
	.private_extern _Minus__Minus_var_i16_i32
_Minus__Minus_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L360
	movl	$6945, %edi
	call	__m3_fault
L360:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_i32
	.private_extern _Minus__uMinus_param_i16_i32
_Minus__uMinus_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_i32
	.private_extern _Minus__Minus_param_i16_i32
_Minus__Minus_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	mov	-24(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L365
	movl	$7009, %edi
	call	__m3_fault
L365:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_I
	.private_extern _Minus__uMinus_var_i16_I
_Minus__uMinus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_I
	.private_extern _Minus__Minus_var_i16_I
_Minus__Minus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L370
	cmpq	$65535, -16(%rbp)
	jle	L371
L370:
	movl	$7073, %edi
	call	__m3_fault
L371:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_I
	.private_extern _Minus__uMinus_param_i16_I
_Minus__uMinus_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_I
	.private_extern _Minus__Minus_param_i16_I
_Minus__Minus_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L376
	cmpq	$65535, -16(%rbp)
	jle	L377
L376:
	movl	$7137, %edi
	call	__m3_fault
L377:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_i64
	.private_extern _Minus__uMinus_var_i16_i64
_Minus__uMinus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_i64
	.private_extern _Minus__Minus_var_i16_i64
_Minus__Minus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_i64
	.private_extern _Minus__uMinus_param_i16_i64
_Minus__uMinus_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_i64
	.private_extern _Minus__Minus_param_i16_i64
_Minus__Minus_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_C
	.private_extern _Minus__uMinus_var_i16_C
_Minus__uMinus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_C
	.private_extern _Minus__Minus_var_i16_C
_Minus__Minus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L390
	movl	$7329, %edi
	call	__m3_fault
L390:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_C
	.private_extern _Minus__uMinus_param_i16_C
_Minus__uMinus_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i16_C
	.private_extern _Minus__Minus_param_i16_C
_Minus__Minus_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	-32(%rbp), %rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L395
	movl	$7393, %edi
	call	__m3_fault
L395:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_u8
	.private_extern _Minus__uMinus_var_i16_u8
_Minus__uMinus_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_u8
	.private_extern _Minus__Minus_var_i16_u8
_Minus__Minus_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L400
	cmpq	$65535, -16(%rbp)
	jle	L401
L400:
	movl	$7457, %edi
	call	__m3_fault
L401:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_u8
	.private_extern _Minus__uMinus_param_i16_u8
_Minus__uMinus_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_u8
	.private_extern _Minus__Minus_param_i16_u8
_Minus__Minus_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movsbq	-19(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L406
	cmpq	$65535, -16(%rbp)
	jle	L407
L406:
	movl	$7521, %edi
	call	__m3_fault
L407:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_i8
	.private_extern _Minus__uMinus_var_i16_i8
_Minus__uMinus_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_i8
	.private_extern _Minus__Minus_var_i16_i8
_Minus__Minus_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L412
	movl	$7585, %edi
	call	__m3_fault
L412:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_i8
	.private_extern _Minus__uMinus_param_i16_i8
_Minus__uMinus_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_i8
	.private_extern _Minus__Minus_param_i16_i8
_Minus__Minus_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movzbl	-19(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L417
	movl	$7649, %edi
	call	__m3_fault
L417:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_L
	.private_extern _Minus__uMinus_var_i16_L
_Minus__uMinus_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_L
	.private_extern _Minus__Minus_var_i16_L
_Minus__Minus_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_L
	.private_extern _Minus__uMinus_param_i16_L
_Minus__uMinus_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_L
	.private_extern _Minus__Minus_param_i16_L
_Minus__Minus_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_u32
	.private_extern _Minus__uMinus_var_i16_u32
_Minus__uMinus_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i16_u32
	.private_extern _Minus__Minus_var_i16_u32
_Minus__Minus_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L430
	cmpq	$65535, -16(%rbp)
	jle	L431
L430:
	movl	$7841, %edi
	call	__m3_fault
L431:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i16_u32
	.private_extern _Minus__uMinus_param_i16_u32
_Minus__uMinus_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i16_u32
	.private_extern _Minus__Minus_param_i16_u32
_Minus__Minus_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L436
	cmpq	$65535, -16(%rbp)
	jle	L437
L436:
	movl	$7905, %edi
	call	__m3_fault
L437:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u16
	.private_extern _Minus__uMinus_var_i32_u16
_Minus__uMinus_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_u16
	.private_extern _Minus__Minus_var_i32_u16
_Minus__Minus_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L442
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L443
L442:
	movl	$7969, %edi
	call	__m3_fault
L443:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_u16
	.private_extern _Minus__uMinus_param_i32_u16
_Minus__uMinus_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_u16
	.private_extern _Minus__Minus_param_i32_u16
_Minus__Minus_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movswq	-22(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L448
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L449
L448:
	movl	$8033, %edi
	call	__m3_fault
L449:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u64
	.private_extern _Minus__uMinus_var_i32_u64
_Minus__uMinus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_u64
	.private_extern _Minus__Minus_var_i32_u64
_Minus__Minus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_u64
	.private_extern _Minus__uMinus_param_i32_u64
_Minus__uMinus_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_u64
	.private_extern _Minus__Minus_param_i32_u64
_Minus__Minus_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_LC
	.private_extern _Minus__uMinus_var_i32_LC
_Minus__uMinus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_LC
	.private_extern _Minus__Minus_var_i32_LC
_Minus__Minus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_LC
	.private_extern _Minus__uMinus_param_i32_LC
_Minus__uMinus_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i32_LC
	.private_extern _Minus__Minus_param_i32_LC
_Minus__Minus_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_i16
	.private_extern _Minus__uMinus_var_i32_i16
_Minus__uMinus_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_i16
	.private_extern _Minus__Minus_var_i32_i16
_Minus__Minus_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L470
	movl	$8353, %edi
	call	__m3_fault
L470:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_i16
	.private_extern _Minus__uMinus_param_i32_i16
_Minus__uMinus_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_i16
	.private_extern _Minus__Minus_param_i32_i16
_Minus__Minus_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movzwl	-22(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L475
	movl	$8417, %edi
	call	__m3_fault
L475:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_i32
	.private_extern _Minus__uMinus_var_i32_i32
_Minus__uMinus_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_i32_i32
	.private_extern _Minus__Minus_var_i32_i32
_Minus__Minus_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L480
	movl	$8481, %edi
	call	__m3_fault
L480:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_i32
	.private_extern _Minus__uMinus_param_i32_i32
_Minus__uMinus_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_i32
	.private_extern _Minus__Minus_param_i32_i32
_Minus__Minus_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	mov	-24(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L485
	movl	$8545, %edi
	call	__m3_fault
L485:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_I
	.private_extern _Minus__uMinus_var_i32_I
_Minus__uMinus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_I
	.private_extern _Minus__Minus_var_i32_I
_Minus__Minus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L490
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L491
L490:
	movl	$8609, %edi
	call	__m3_fault
L491:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_I
	.private_extern _Minus__uMinus_param_i32_I
_Minus__uMinus_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_I
	.private_extern _Minus__Minus_param_i32_I
_Minus__Minus_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L496
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L497
L496:
	movl	$8673, %edi
	call	__m3_fault
L497:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_i64
	.private_extern _Minus__uMinus_var_i32_i64
_Minus__uMinus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_i64
	.private_extern _Minus__Minus_var_i32_i64
_Minus__Minus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_i64
	.private_extern _Minus__uMinus_param_i32_i64
_Minus__uMinus_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_i64
	.private_extern _Minus__Minus_param_i32_i64
_Minus__Minus_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_C
	.private_extern _Minus__uMinus_var_i32_C
_Minus__uMinus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_C
	.private_extern _Minus__Minus_var_i32_C
_Minus__Minus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L510
	movl	$8865, %edi
	call	__m3_fault
L510:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_C
	.private_extern _Minus__uMinus_param_i32_C
_Minus__uMinus_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i32_C
	.private_extern _Minus__Minus_param_i32_C
_Minus__Minus_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	-32(%rbp), %rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L515
	movl	$8929, %edi
	call	__m3_fault
L515:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u8
	.private_extern _Minus__uMinus_var_i32_u8
_Minus__uMinus_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_u8
	.private_extern _Minus__Minus_var_i32_u8
_Minus__Minus_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L520
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L521
L520:
	movl	$8993, %edi
	call	__m3_fault
L521:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_u8
	.private_extern _Minus__uMinus_param_i32_u8
_Minus__uMinus_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_u8
	.private_extern _Minus__Minus_param_i32_u8
_Minus__Minus_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movsbq	-21(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L526
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L527
L526:
	movl	$9057, %edi
	call	__m3_fault
L527:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_i8
	.private_extern _Minus__uMinus_var_i32_i8
_Minus__uMinus_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_i8
	.private_extern _Minus__Minus_var_i32_i8
_Minus__Minus_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L532
	movl	$9121, %edi
	call	__m3_fault
L532:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_i8
	.private_extern _Minus__uMinus_param_i32_i8
_Minus__uMinus_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_i8
	.private_extern _Minus__Minus_param_i32_i8
_Minus__Minus_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movzbl	-21(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L537
	movl	$9185, %edi
	call	__m3_fault
L537:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_L
	.private_extern _Minus__uMinus_var_i32_L
_Minus__uMinus_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_L
	.private_extern _Minus__Minus_var_i32_L
_Minus__Minus_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_L
	.private_extern _Minus__uMinus_param_i32_L
_Minus__uMinus_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_L
	.private_extern _Minus__Minus_param_i32_L
_Minus__Minus_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u32
	.private_extern _Minus__uMinus_var_i32_u32
_Minus__uMinus_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i32_u32
	.private_extern _Minus__Minus_var_i32_u32
_Minus__Minus_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L550
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L551
L550:
	movl	$9377, %edi
	call	__m3_fault
L551:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i32_u32
	.private_extern _Minus__uMinus_param_i32_u32
_Minus__uMinus_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i32_u32
	.private_extern _Minus__Minus_param_i32_u32
_Minus__Minus_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L556
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L557
L556:
	movl	$9441, %edi
	call	__m3_fault
L557:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_I_u16
	.private_extern _Minus__uMinus_var_I_u16
_Minus__uMinus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_u16
	.private_extern _Minus__Minus_var_I_u16
_Minus__Minus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_u16
	.private_extern _Minus__uMinus_param_I_u16
_Minus__uMinus_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_u16
	.private_extern _Minus__Minus_param_I_u16
_Minus__Minus_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_u64
	.private_extern _Minus__uMinus_var_I_u64
_Minus__uMinus_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_u64
	.private_extern _Minus__Minus_var_I_u64
_Minus__Minus_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_u64
	.private_extern _Minus__uMinus_param_I_u64
_Minus__uMinus_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_u64
	.private_extern _Minus__Minus_param_I_u64
_Minus__Minus_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_LC
	.private_extern _Minus__uMinus_var_I_LC
_Minus__uMinus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_LC
	.private_extern _Minus__Minus_var_I_LC
_Minus__Minus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_LC
	.private_extern _Minus__uMinus_param_I_LC
_Minus__uMinus_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_I_LC
	.private_extern _Minus__Minus_param_I_LC
_Minus__Minus_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_i16
	.private_extern _Minus__uMinus_var_I_i16
_Minus__uMinus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_i16
	.private_extern _Minus__Minus_var_I_i16
_Minus__Minus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_i16
	.private_extern _Minus__uMinus_param_I_i16
_Minus__uMinus_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_i16
	.private_extern _Minus__Minus_param_I_i16
_Minus__Minus_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_i32
	.private_extern _Minus__uMinus_var_I_i32
_Minus__uMinus_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_i32
	.private_extern _Minus__Minus_var_I_i32
_Minus__Minus_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_i32
	.private_extern _Minus__uMinus_param_I_i32
_Minus__uMinus_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_i32
	.private_extern _Minus__Minus_param_I_i32
_Minus__Minus_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_I
	.private_extern _Minus__uMinus_var_I_I
_Minus__uMinus_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_I_I
	.private_extern _Minus__Minus_var_I_I
_Minus__Minus_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__uMinus_param_I_I
	.private_extern _Minus__uMinus_param_I_I
_Minus__uMinus_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_I
	.private_extern _Minus__Minus_param_I_I
_Minus__Minus_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_i64
	.private_extern _Minus__uMinus_var_I_i64
_Minus__uMinus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_i64
	.private_extern _Minus__Minus_var_I_i64
_Minus__Minus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_i64
	.private_extern _Minus__uMinus_param_I_i64
_Minus__uMinus_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_i64
	.private_extern _Minus__Minus_param_I_i64
_Minus__Minus_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_C
	.private_extern _Minus__uMinus_var_I_C
_Minus__uMinus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_C
	.private_extern _Minus__Minus_var_I_C
_Minus__Minus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_C
	.private_extern _Minus__uMinus_param_I_C
_Minus__uMinus_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_I_C
	.private_extern _Minus__Minus_param_I_C
_Minus__Minus_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_u8
	.private_extern _Minus__uMinus_var_I_u8
_Minus__uMinus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_u8
	.private_extern _Minus__Minus_var_I_u8
_Minus__Minus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_u8
	.private_extern _Minus__uMinus_param_I_u8
_Minus__uMinus_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_u8
	.private_extern _Minus__Minus_param_I_u8
_Minus__Minus_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_i8
	.private_extern _Minus__uMinus_var_I_i8
_Minus__uMinus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_i8
	.private_extern _Minus__Minus_var_I_i8
_Minus__Minus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_i8
	.private_extern _Minus__uMinus_param_I_i8
_Minus__uMinus_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_i8
	.private_extern _Minus__Minus_param_I_i8
_Minus__Minus_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_L
	.private_extern _Minus__uMinus_var_I_L
_Minus__uMinus_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_L
	.private_extern _Minus__Minus_var_I_L
_Minus__Minus_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_L
	.private_extern _Minus__uMinus_param_I_L
_Minus__uMinus_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_L
	.private_extern _Minus__Minus_param_I_L
_Minus__Minus_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_I_u32
	.private_extern _Minus__uMinus_var_I_u32
_Minus__uMinus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_I_u32
	.private_extern _Minus__Minus_var_I_u32
_Minus__Minus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_I_u32
	.private_extern _Minus__uMinus_param_I_u32
_Minus__uMinus_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_I_u32
	.private_extern _Minus__Minus_param_I_u32
_Minus__Minus_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_u16
	.private_extern _Minus__uMinus_var_i64_u16
_Minus__uMinus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_u16
	.private_extern _Minus__Minus_var_i64_u16
_Minus__Minus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_u16
	.private_extern _Minus__uMinus_param_i64_u16
_Minus__uMinus_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_u16
	.private_extern _Minus__Minus_param_i64_u16
_Minus__Minus_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_u64
	.private_extern _Minus__uMinus_var_i64_u64
_Minus__uMinus_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_u64
	.private_extern _Minus__Minus_var_i64_u64
_Minus__Minus_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_u64
	.private_extern _Minus__uMinus_param_i64_u64
_Minus__uMinus_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_u64
	.private_extern _Minus__Minus_param_i64_u64
_Minus__Minus_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_LC
	.private_extern _Minus__uMinus_var_i64_LC
_Minus__uMinus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_LC
	.private_extern _Minus__Minus_var_i64_LC
_Minus__Minus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_LC
	.private_extern _Minus__uMinus_param_i64_LC
_Minus__uMinus_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i64_LC
	.private_extern _Minus__Minus_param_i64_LC
_Minus__Minus_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_i16
	.private_extern _Minus__uMinus_var_i64_i16
_Minus__uMinus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_i16
	.private_extern _Minus__Minus_var_i64_i16
_Minus__Minus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_i16
	.private_extern _Minus__uMinus_param_i64_i16
_Minus__uMinus_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_i16
	.private_extern _Minus__Minus_param_i64_i16
_Minus__Minus_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_i32
	.private_extern _Minus__uMinus_var_i64_i32
_Minus__uMinus_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_i32
	.private_extern _Minus__Minus_var_i64_i32
_Minus__Minus_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_i32
	.private_extern _Minus__uMinus_param_i64_i32
_Minus__uMinus_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_i32
	.private_extern _Minus__Minus_param_i64_i32
_Minus__Minus_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_I
	.private_extern _Minus__uMinus_var_i64_I
_Minus__uMinus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_I
	.private_extern _Minus__Minus_var_i64_I
_Minus__Minus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_I
	.private_extern _Minus__uMinus_param_i64_I
_Minus__uMinus_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_I
	.private_extern _Minus__Minus_param_i64_I
_Minus__Minus_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_i64
	.private_extern _Minus__uMinus_var_i64_i64
_Minus__uMinus_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_i64_i64
	.private_extern _Minus__Minus_var_i64_i64
_Minus__Minus_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__uMinus_param_i64_i64
	.private_extern _Minus__uMinus_param_i64_i64
_Minus__uMinus_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_i64
	.private_extern _Minus__Minus_param_i64_i64
_Minus__Minus_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_C
	.private_extern _Minus__uMinus_var_i64_C
_Minus__uMinus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_C
	.private_extern _Minus__Minus_var_i64_C
_Minus__Minus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_C
	.private_extern _Minus__uMinus_param_i64_C
_Minus__uMinus_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i64_C
	.private_extern _Minus__Minus_param_i64_C
_Minus__Minus_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_u8
	.private_extern _Minus__uMinus_var_i64_u8
_Minus__uMinus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_u8
	.private_extern _Minus__Minus_var_i64_u8
_Minus__Minus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_u8
	.private_extern _Minus__uMinus_param_i64_u8
_Minus__uMinus_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_u8
	.private_extern _Minus__Minus_param_i64_u8
_Minus__Minus_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_i8
	.private_extern _Minus__uMinus_var_i64_i8
_Minus__uMinus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_i8
	.private_extern _Minus__Minus_var_i64_i8
_Minus__Minus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_i8
	.private_extern _Minus__uMinus_param_i64_i8
_Minus__uMinus_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_i8
	.private_extern _Minus__Minus_param_i64_i8
_Minus__Minus_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_L
	.private_extern _Minus__uMinus_var_i64_L
_Minus__uMinus_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_L
	.private_extern _Minus__Minus_var_i64_L
_Minus__Minus_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_L
	.private_extern _Minus__uMinus_param_i64_L
_Minus__uMinus_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_L
	.private_extern _Minus__Minus_param_i64_L
_Minus__Minus_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i64_u32
	.private_extern _Minus__uMinus_var_i64_u32
_Minus__uMinus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i64_u32
	.private_extern _Minus__Minus_var_i64_u32
_Minus__Minus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Minus(%rip), %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i64_u32
	.private_extern _Minus__uMinus_param_i64_u32
_Minus__uMinus_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i64_u32
	.private_extern _Minus__Minus_param_i64_u32
_Minus__Minus_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u16
	.private_extern _Minus__uMinus_var_C_u16
_Minus__uMinus_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_u16
	.private_extern _Minus__Minus_var_C_u16
_Minus__Minus_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L754
	movl	$12577, %edi
	call	__m3_fault
L754:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_u16
	.private_extern _Minus__uMinus_param_C_u16
_Minus__uMinus_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_u16
	.private_extern _Minus__Minus_param_C_u16
_Minus__Minus_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movswq	-26(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L759
	movl	$12641, %edi
	call	__m3_fault
L759:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u64
	.private_extern _Minus__uMinus_var_C_u64
_Minus__uMinus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_u64
	.private_extern _Minus__Minus_var_C_u64
_Minus__Minus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_C_u64
	.private_extern _Minus__uMinus_param_C_u64
_Minus__uMinus_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_u64
	.private_extern _Minus__Minus_param_C_u64
_Minus__Minus_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_LC
	.private_extern _Minus__uMinus_var_C_LC
_Minus__uMinus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_LC
	.private_extern _Minus__Minus_var_C_LC
_Minus__Minus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_C_LC
	.private_extern _Minus__uMinus_param_C_LC
_Minus__uMinus_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_LC
	.private_extern _Minus__Minus_param_C_LC
_Minus__Minus_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i16
	.private_extern _Minus__uMinus_var_C_i16
_Minus__uMinus_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_i16
	.private_extern _Minus__Minus_var_C_i16
_Minus__Minus_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L780
	movl	$12961, %edi
	call	__m3_fault
L780:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_i16
	.private_extern _Minus__uMinus_param_C_i16
_Minus__uMinus_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_i16
	.private_extern _Minus__Minus_param_C_i16
_Minus__Minus_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movzwl	-26(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L785
	movl	$13025, %edi
	call	__m3_fault
L785:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i32
	.private_extern _Minus__uMinus_var_C_i32
_Minus__uMinus_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_i32
	.private_extern _Minus__Minus_var_C_i32
_Minus__Minus_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L790
	movl	$13089, %edi
	call	__m3_fault
L790:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_i32
	.private_extern _Minus__uMinus_param_C_i32
_Minus__uMinus_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_i32
	.private_extern _Minus__Minus_param_C_i32
_Minus__Minus_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	mov	-28(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L795
	movl	$13153, %edi
	call	__m3_fault
L795:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_I
	.private_extern _Minus__uMinus_var_C_I
_Minus__uMinus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_I
	.private_extern _Minus__Minus_var_C_I
_Minus__Minus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L800
	movl	$13217, %edi
	call	__m3_fault
L800:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_I
	.private_extern _Minus__uMinus_param_C_I
_Minus__uMinus_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_I
	.private_extern _Minus__Minus_param_C_I
_Minus__Minus_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L805
	movl	$13281, %edi
	call	__m3_fault
L805:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i64
	.private_extern _Minus__uMinus_var_C_i64
_Minus__uMinus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_i64
	.private_extern _Minus__Minus_var_C_i64
_Minus__Minus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_C_i64
	.private_extern _Minus__uMinus_param_C_i64
_Minus__uMinus_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_i64
	.private_extern _Minus__Minus_param_C_i64
_Minus__Minus_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_C
	.private_extern _Minus__uMinus_var_C_C
_Minus__uMinus_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_C_C
	.private_extern _Minus__Minus_var_C_C
_Minus__Minus_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L818
	movl	$13473, %edi
	call	__m3_fault
L818:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_C
	.private_extern _Minus__uMinus_param_C_C
_Minus__uMinus_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_C
	.private_extern _Minus__Minus_param_C_C
_Minus__Minus_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L823
	movl	$13537, %edi
	call	__m3_fault
L823:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u8
	.private_extern _Minus__uMinus_var_C_u8
_Minus__uMinus_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_u8
	.private_extern _Minus__Minus_var_C_u8
_Minus__Minus_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L828
	movl	$13601, %edi
	call	__m3_fault
L828:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_u8
	.private_extern _Minus__uMinus_param_C_u8
_Minus__uMinus_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_u8
	.private_extern _Minus__Minus_param_C_u8
_Minus__Minus_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movsbq	-25(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L833
	movl	$13665, %edi
	call	__m3_fault
L833:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i8
	.private_extern _Minus__uMinus_var_C_i8
_Minus__uMinus_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_i8
	.private_extern _Minus__Minus_var_C_i8
_Minus__Minus_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L838
	movl	$13729, %edi
	call	__m3_fault
L838:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_i8
	.private_extern _Minus__uMinus_param_C_i8
_Minus__uMinus_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_i8
	.private_extern _Minus__Minus_param_C_i8
_Minus__Minus_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movzbl	-25(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L843
	movl	$13793, %edi
	call	__m3_fault
L843:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_L
	.private_extern _Minus__uMinus_var_C_L
_Minus__uMinus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_L
	.private_extern _Minus__Minus_var_C_L
_Minus__Minus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_C_L
	.private_extern _Minus__uMinus_param_C_L
_Minus__uMinus_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_L
	.private_extern _Minus__Minus_param_C_L
_Minus__Minus_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u32
	.private_extern _Minus__uMinus_var_C_u32
_Minus__uMinus_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_C_u32
	.private_extern _Minus__Minus_var_C_u32
_Minus__Minus_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L856
	movl	$13985, %edi
	call	__m3_fault
L856:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_C_u32
	.private_extern _Minus__uMinus_param_C_u32
_Minus__uMinus_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__Minus_param_C_u32
	.private_extern _Minus__Minus_param_C_u32
_Minus__Minus_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L861
	movl	$14049, %edi
	call	__m3_fault
L861:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_var_f32_f32
	.private_extern _Minus__Minus_var_f32_f32
_Minus__Minus_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_Minus(%rip), %xmm1
	movss	168+_MM_Minus(%rip), %xmm0
	movaps	%xmm1, %xmm2
	subss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	leave
	ret
.globl _Minus__Minus_param_f32_f32
	.private_extern _Minus__Minus_param_f32_f32
_Minus__Minus_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm0
	subss	-24(%rbp), %xmm0
	leave
	ret
.globl _Minus__uMinus_var_u8_u16
	.private_extern _Minus__uMinus_var_u8_u16
_Minus__uMinus_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_u16
	.private_extern _Minus__Minus_var_u8_u16
_Minus__Minus_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L870
	cmpq	$127, -16(%rbp)
	jle	L871
L870:
	movl	$14177, %edi
	call	__m3_fault
L871:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_u16
	.private_extern _Minus__uMinus_param_u8_u16
_Minus__uMinus_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_u16
	.private_extern _Minus__Minus_param_u8_u16
_Minus__Minus_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movswq	-20(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L876
	cmpq	$127, -16(%rbp)
	jle	L877
L876:
	movl	$14241, %edi
	call	__m3_fault
L877:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_u64
	.private_extern _Minus__uMinus_var_u8_u64
_Minus__uMinus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_u64
	.private_extern _Minus__Minus_var_u8_u64
_Minus__Minus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_u64
	.private_extern _Minus__uMinus_param_u8_u64
_Minus__uMinus_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_u64
	.private_extern _Minus__Minus_param_u8_u64
_Minus__Minus_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_LC
	.private_extern _Minus__uMinus_var_u8_LC
_Minus__uMinus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_LC
	.private_extern _Minus__Minus_var_u8_LC
_Minus__Minus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_LC
	.private_extern _Minus__uMinus_param_u8_LC
_Minus__uMinus_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u8_LC
	.private_extern _Minus__Minus_param_u8_LC
_Minus__Minus_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_i16
	.private_extern _Minus__uMinus_var_u8_i16
_Minus__uMinus_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_i16
	.private_extern _Minus__Minus_var_u8_i16
_Minus__Minus_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L898
	movl	$14561, %edi
	call	__m3_fault
L898:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_i16
	.private_extern _Minus__uMinus_param_u8_i16
_Minus__uMinus_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_i16
	.private_extern _Minus__Minus_param_u8_i16
_Minus__Minus_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movzwl	-20(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L903
	movl	$14625, %edi
	call	__m3_fault
L903:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_i32
	.private_extern _Minus__uMinus_var_u8_i32
_Minus__uMinus_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_i32
	.private_extern _Minus__Minus_var_u8_i32
_Minus__Minus_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L908
	movl	$14689, %edi
	call	__m3_fault
L908:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_i32
	.private_extern _Minus__uMinus_param_u8_i32
_Minus__uMinus_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_i32
	.private_extern _Minus__Minus_param_u8_i32
_Minus__Minus_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	mov	-24(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L913
	movl	$14753, %edi
	call	__m3_fault
L913:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_I
	.private_extern _Minus__uMinus_var_u8_I
_Minus__uMinus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_I
	.private_extern _Minus__Minus_var_u8_I
_Minus__Minus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L918
	cmpq	$127, -16(%rbp)
	jle	L919
L918:
	movl	$14817, %edi
	call	__m3_fault
L919:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_I
	.private_extern _Minus__uMinus_param_u8_I
_Minus__uMinus_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_I
	.private_extern _Minus__Minus_param_u8_I
_Minus__Minus_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L924
	cmpq	$127, -16(%rbp)
	jle	L925
L924:
	movl	$14881, %edi
	call	__m3_fault
L925:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_i64
	.private_extern _Minus__uMinus_var_u8_i64
_Minus__uMinus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_i64
	.private_extern _Minus__Minus_var_u8_i64
_Minus__Minus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_i64
	.private_extern _Minus__uMinus_param_u8_i64
_Minus__uMinus_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_i64
	.private_extern _Minus__Minus_param_u8_i64
_Minus__Minus_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_C
	.private_extern _Minus__uMinus_var_u8_C
_Minus__uMinus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_C
	.private_extern _Minus__Minus_var_u8_C
_Minus__Minus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L938
	cmpq	$127, -16(%rbp)
	jle	L939
L938:
	movl	$15073, %edi
	call	__m3_fault
L939:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_C
	.private_extern _Minus__uMinus_param_u8_C
_Minus__uMinus_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u8_C
	.private_extern _Minus__Minus_param_u8_C
_Minus__Minus_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	-32(%rbp), %rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L944
	cmpq	$127, -16(%rbp)
	jle	L945
L944:
	movl	$15137, %edi
	call	__m3_fault
L945:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_u8
	.private_extern _Minus__uMinus_var_u8_u8
_Minus__uMinus_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_u8_u8
	.private_extern _Minus__Minus_var_u8_u8
_Minus__Minus_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L950
	cmpq	$127, -16(%rbp)
	jle	L951
L950:
	movl	$15201, %edi
	call	__m3_fault
L951:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_u8
	.private_extern _Minus__uMinus_param_u8_u8
_Minus__uMinus_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_u8
	.private_extern _Minus__Minus_param_u8_u8
_Minus__Minus_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movsbq	-18(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L956
	cmpq	$127, -16(%rbp)
	jle	L957
L956:
	movl	$15265, %edi
	call	__m3_fault
L957:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_i8
	.private_extern _Minus__uMinus_var_u8_i8
_Minus__uMinus_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_i8
	.private_extern _Minus__Minus_var_u8_i8
_Minus__Minus_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L962
	movl	$15329, %edi
	call	__m3_fault
L962:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_i8
	.private_extern _Minus__uMinus_param_u8_i8
_Minus__uMinus_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_i8
	.private_extern _Minus__Minus_param_u8_i8
_Minus__Minus_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movzbl	-18(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L967
	movl	$15393, %edi
	call	__m3_fault
L967:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_L
	.private_extern _Minus__uMinus_var_u8_L
_Minus__uMinus_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_L
	.private_extern _Minus__Minus_var_u8_L
_Minus__Minus_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_L
	.private_extern _Minus__uMinus_param_u8_L
_Minus__uMinus_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_L
	.private_extern _Minus__Minus_param_u8_L
_Minus__Minus_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_u32
	.private_extern _Minus__uMinus_var_u8_u32
_Minus__uMinus_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u8_u32
	.private_extern _Minus__Minus_var_u8_u32
_Minus__Minus_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L980
	cmpq	$127, -16(%rbp)
	jle	L981
L980:
	movl	$15585, %edi
	call	__m3_fault
L981:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u8_u32
	.private_extern _Minus__uMinus_param_u8_u32
_Minus__uMinus_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u8_u32
	.private_extern _Minus__Minus_param_u8_u32
_Minus__Minus_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L986
	cmpq	$127, -16(%rbp)
	jle	L987
L986:
	movl	$15649, %edi
	call	__m3_fault
L987:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u16
	.private_extern _Minus__uMinus_var_i8_u16
_Minus__uMinus_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_u16
	.private_extern _Minus__Minus_var_i8_u16
_Minus__Minus_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L992
	cmpq	$255, -16(%rbp)
	jle	L993
L992:
	movl	$15713, %edi
	call	__m3_fault
L993:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_u16
	.private_extern _Minus__uMinus_param_i8_u16
_Minus__uMinus_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_u16
	.private_extern _Minus__Minus_param_i8_u16
_Minus__Minus_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movswq	-20(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L998
	cmpq	$255, -16(%rbp)
	jle	L999
L998:
	movl	$15777, %edi
	call	__m3_fault
L999:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u64
	.private_extern _Minus__uMinus_var_i8_u64
_Minus__uMinus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_u64
	.private_extern _Minus__Minus_var_i8_u64
_Minus__Minus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_u64
	.private_extern _Minus__uMinus_param_i8_u64
_Minus__uMinus_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_u64
	.private_extern _Minus__Minus_param_i8_u64
_Minus__Minus_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_LC
	.private_extern _Minus__uMinus_var_i8_LC
_Minus__uMinus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_LC
	.private_extern _Minus__Minus_var_i8_LC
_Minus__Minus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_LC
	.private_extern _Minus__uMinus_param_i8_LC
_Minus__uMinus_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i8_LC
	.private_extern _Minus__Minus_param_i8_LC
_Minus__Minus_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_i16
	.private_extern _Minus__uMinus_var_i8_i16
_Minus__uMinus_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_i16
	.private_extern _Minus__Minus_var_i8_i16
_Minus__Minus_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1020
	movl	$16097, %edi
	call	__m3_fault
L1020:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_i16
	.private_extern _Minus__uMinus_param_i8_i16
_Minus__uMinus_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_i16
	.private_extern _Minus__Minus_param_i8_i16
_Minus__Minus_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movzwl	-20(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1025
	movl	$16161, %edi
	call	__m3_fault
L1025:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_i32
	.private_extern _Minus__uMinus_var_i8_i32
_Minus__uMinus_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_i32
	.private_extern _Minus__Minus_var_i8_i32
_Minus__Minus_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1030
	movl	$16225, %edi
	call	__m3_fault
L1030:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_i32
	.private_extern _Minus__uMinus_param_i8_i32
_Minus__uMinus_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_i32
	.private_extern _Minus__Minus_param_i8_i32
_Minus__Minus_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	mov	-24(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1035
	movl	$16289, %edi
	call	__m3_fault
L1035:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_I
	.private_extern _Minus__uMinus_var_i8_I
_Minus__uMinus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_I
	.private_extern _Minus__Minus_var_i8_I
_Minus__Minus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1040
	cmpq	$255, -16(%rbp)
	jle	L1041
L1040:
	movl	$16353, %edi
	call	__m3_fault
L1041:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_I
	.private_extern _Minus__uMinus_param_i8_I
_Minus__uMinus_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_I
	.private_extern _Minus__Minus_param_i8_I
_Minus__Minus_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1046
	cmpq	$255, -16(%rbp)
	jle	L1047
L1046:
	movl	$16417, %edi
	call	__m3_fault
L1047:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_i64
	.private_extern _Minus__uMinus_var_i8_i64
_Minus__uMinus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_i64
	.private_extern _Minus__Minus_var_i8_i64
_Minus__Minus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_i64
	.private_extern _Minus__uMinus_param_i8_i64
_Minus__uMinus_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_i64
	.private_extern _Minus__Minus_param_i8_i64
_Minus__Minus_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_C
	.private_extern _Minus__uMinus_var_i8_C
_Minus__uMinus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_C
	.private_extern _Minus__Minus_var_i8_C
_Minus__Minus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1060
	movl	$16609, %edi
	call	__m3_fault
L1060:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_C
	.private_extern _Minus__uMinus_param_i8_C
_Minus__uMinus_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_i8_C
	.private_extern _Minus__Minus_param_i8_C
_Minus__Minus_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	-32(%rbp), %rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1065
	movl	$16673, %edi
	call	__m3_fault
L1065:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u8
	.private_extern _Minus__uMinus_var_i8_u8
_Minus__uMinus_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_u8
	.private_extern _Minus__Minus_var_i8_u8
_Minus__Minus_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1070
	cmpq	$255, -16(%rbp)
	jle	L1071
L1070:
	movl	$16737, %edi
	call	__m3_fault
L1071:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_u8
	.private_extern _Minus__uMinus_param_i8_u8
_Minus__uMinus_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_u8
	.private_extern _Minus__Minus_param_i8_u8
_Minus__Minus_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	-18(%rbp),%rdx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1076
	cmpq	$255, -16(%rbp)
	jle	L1077
L1076:
	movl	$16801, %edi
	call	__m3_fault
L1077:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_i8
	.private_extern _Minus__uMinus_var_i8_i8
_Minus__uMinus_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_i8_i8
	.private_extern _Minus__Minus_var_i8_i8
_Minus__Minus_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1082
	movl	$16865, %edi
	call	__m3_fault
L1082:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_i8
	.private_extern _Minus__uMinus_param_i8_i8
_Minus__uMinus_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_i8
	.private_extern _Minus__Minus_param_i8_i8
_Minus__Minus_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movzbl	-18(%rbp), %edx
	subq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1087
	movl	$16929, %edi
	call	__m3_fault
L1087:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_L
	.private_extern _Minus__uMinus_var_i8_L
_Minus__uMinus_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_L
	.private_extern _Minus__Minus_var_i8_L
_Minus__Minus_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_L
	.private_extern _Minus__uMinus_param_i8_L
_Minus__uMinus_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_L
	.private_extern _Minus__Minus_param_i8_L
_Minus__Minus_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u32
	.private_extern _Minus__uMinus_var_i8_u32
_Minus__uMinus_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_i8_u32
	.private_extern _Minus__Minus_var_i8_u32
_Minus__Minus_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1100
	cmpq	$255, -16(%rbp)
	jle	L1101
L1100:
	movl	$17121, %edi
	call	__m3_fault
L1101:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_i8_u32
	.private_extern _Minus__uMinus_param_i8_u32
_Minus__uMinus_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_i8_u32
	.private_extern _Minus__Minus_param_i8_u32
_Minus__Minus_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1106
	cmpq	$255, -16(%rbp)
	jle	L1107
L1106:
	movl	$17185, %edi
	call	__m3_fault
L1107:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_L_u16
	.private_extern _Minus__uMinus_var_L_u16
_Minus__uMinus_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_u16
	.private_extern _Minus__Minus_var_L_u16
_Minus__Minus_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_u16
	.private_extern _Minus__uMinus_param_L_u16
_Minus__uMinus_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_u16
	.private_extern _Minus__Minus_param_L_u16
_Minus__Minus_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_u64
	.private_extern _Minus__uMinus_var_L_u64
_Minus__uMinus_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_u64
	.private_extern _Minus__Minus_var_L_u64
_Minus__Minus_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_u64
	.private_extern _Minus__uMinus_param_L_u64
_Minus__uMinus_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_u64
	.private_extern _Minus__Minus_param_L_u64
_Minus__Minus_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_LC
	.private_extern _Minus__uMinus_var_L_LC
_Minus__uMinus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_LC
	.private_extern _Minus__Minus_var_L_LC
_Minus__Minus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_LC
	.private_extern _Minus__uMinus_param_L_LC
_Minus__uMinus_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_L_LC
	.private_extern _Minus__Minus_param_L_LC
_Minus__Minus_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_i16
	.private_extern _Minus__uMinus_var_L_i16
_Minus__uMinus_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_i16
	.private_extern _Minus__Minus_var_L_i16
_Minus__Minus_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_i16
	.private_extern _Minus__uMinus_param_L_i16
_Minus__uMinus_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_i16
	.private_extern _Minus__Minus_param_L_i16
_Minus__Minus_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_i32
	.private_extern _Minus__uMinus_var_L_i32
_Minus__uMinus_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_i32
	.private_extern _Minus__Minus_var_L_i32
_Minus__Minus_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_i32
	.private_extern _Minus__uMinus_param_L_i32
_Minus__uMinus_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_i32
	.private_extern _Minus__Minus_param_L_i32
_Minus__Minus_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_I
	.private_extern _Minus__uMinus_var_L_I
_Minus__uMinus_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_I
	.private_extern _Minus__Minus_var_L_I
_Minus__Minus_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_I
	.private_extern _Minus__uMinus_param_L_I
_Minus__uMinus_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_I
	.private_extern _Minus__Minus_param_L_I
_Minus__Minus_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_i64
	.private_extern _Minus__uMinus_var_L_i64
_Minus__uMinus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_i64
	.private_extern _Minus__Minus_var_L_i64
_Minus__Minus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_i64
	.private_extern _Minus__uMinus_param_L_i64
_Minus__uMinus_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_i64
	.private_extern _Minus__Minus_param_L_i64
_Minus__Minus_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_C
	.private_extern _Minus__uMinus_var_L_C
_Minus__uMinus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_C
	.private_extern _Minus__Minus_var_L_C
_Minus__Minus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_C
	.private_extern _Minus__uMinus_param_L_C
_Minus__uMinus_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_L_C
	.private_extern _Minus__Minus_param_L_C
_Minus__Minus_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_u8
	.private_extern _Minus__uMinus_var_L_u8
_Minus__uMinus_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_u8
	.private_extern _Minus__Minus_var_L_u8
_Minus__Minus_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_u8
	.private_extern _Minus__uMinus_param_L_u8
_Minus__uMinus_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_u8
	.private_extern _Minus__Minus_param_L_u8
_Minus__Minus_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_i8
	.private_extern _Minus__uMinus_var_L_i8
_Minus__uMinus_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_i8
	.private_extern _Minus__Minus_var_L_i8
_Minus__Minus_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_i8
	.private_extern _Minus__uMinus_param_L_i8
_Minus__uMinus_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_i8
	.private_extern _Minus__Minus_param_L_i8
_Minus__Minus_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_L
	.private_extern _Minus__uMinus_var_L_L
_Minus__uMinus_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_L_L
	.private_extern _Minus__Minus_var_L_L
_Minus__Minus_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__uMinus_param_L_L
	.private_extern _Minus__uMinus_param_L_L
_Minus__uMinus_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_L
	.private_extern _Minus__Minus_param_L_L
_Minus__Minus_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_L_u32
	.private_extern _Minus__uMinus_var_L_u32
_Minus__uMinus_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_L_u32
	.private_extern _Minus__Minus_var_L_u32
_Minus__Minus_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movl	184+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_L_u32
	.private_extern _Minus__uMinus_param_L_u32
_Minus__uMinus_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_L_u32
	.private_extern _Minus__Minus_param_L_u32
_Minus__Minus_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_u16
	.private_extern _Minus__uMinus_var_u32_u16
_Minus__uMinus_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_u16
	.private_extern _Minus__Minus_var_u32_u16
_Minus__Minus_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1208
	cmpq	$2147483647, -16(%rbp)
	jle	L1209
L1208:
	movl	$18785, %edi
	call	__m3_fault
L1209:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_u16
	.private_extern _Minus__uMinus_param_u32_u16
_Minus__uMinus_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_u16
	.private_extern _Minus__Minus_param_u32_u16
_Minus__Minus_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1214
	cmpq	$2147483647, -16(%rbp)
	jle	L1215
L1214:
	movl	$18849, %edi
	call	__m3_fault
L1215:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_u64
	.private_extern _Minus__uMinus_var_u32_u64
_Minus__uMinus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_u64
	.private_extern _Minus__Minus_var_u32_u64
_Minus__Minus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_u64
	.private_extern _Minus__uMinus_param_u32_u64
_Minus__uMinus_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_u64
	.private_extern _Minus__Minus_param_u32_u64
_Minus__Minus_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_LC
	.private_extern _Minus__uMinus_var_u32_LC
_Minus__uMinus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_LC
	.private_extern _Minus__Minus_var_u32_LC
_Minus__Minus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_LC
	.private_extern _Minus__uMinus_param_u32_LC
_Minus__uMinus_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u32_LC
	.private_extern _Minus__Minus_param_u32_LC
_Minus__Minus_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i16
	.private_extern _Minus__uMinus_var_u32_i16
_Minus__uMinus_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_i16
	.private_extern _Minus__Minus_var_u32_i16
_Minus__Minus_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L1236
	movl	$19169, %edi
	call	__m3_fault
L1236:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_i16
	.private_extern _Minus__uMinus_param_u32_i16
_Minus__uMinus_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_i16
	.private_extern _Minus__Minus_param_u32_i16
_Minus__Minus_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L1241
	movl	$19233, %edi
	call	__m3_fault
L1241:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i32
	.private_extern _Minus__uMinus_var_u32_i32
_Minus__uMinus_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_i32
	.private_extern _Minus__Minus_var_u32_i32
_Minus__Minus_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L1246
	movl	$19297, %edi
	call	__m3_fault
L1246:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_i32
	.private_extern _Minus__uMinus_param_u32_i32
_Minus__uMinus_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_i32
	.private_extern _Minus__Minus_param_u32_i32
_Minus__Minus_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L1251
	movl	$19361, %edi
	call	__m3_fault
L1251:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_I
	.private_extern _Minus__uMinus_var_u32_I
_Minus__uMinus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_I
	.private_extern _Minus__Minus_var_u32_I
_Minus__Minus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1256
	cmpq	$2147483647, -16(%rbp)
	jle	L1257
L1256:
	movl	$19425, %edi
	call	__m3_fault
L1257:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_I
	.private_extern _Minus__uMinus_param_u32_I
_Minus__uMinus_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_I
	.private_extern _Minus__Minus_param_u32_I
_Minus__Minus_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1262
	cmpq	$2147483647, -16(%rbp)
	jle	L1263
L1262:
	movl	$19489, %edi
	call	__m3_fault
L1263:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i64
	.private_extern _Minus__uMinus_var_u32_i64
_Minus__uMinus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_i64
	.private_extern _Minus__Minus_var_u32_i64
_Minus__Minus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_i64
	.private_extern _Minus__uMinus_param_u32_i64
_Minus__uMinus_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_i64
	.private_extern _Minus__Minus_param_u32_i64
_Minus__Minus_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_C
	.private_extern _Minus__uMinus_var_u32_C
_Minus__uMinus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_C
	.private_extern _Minus__Minus_var_u32_C
_Minus__Minus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1276
	cmpq	$2147483647, -16(%rbp)
	jle	L1277
L1276:
	movl	$19681, %edi
	call	__m3_fault
L1277:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_C
	.private_extern _Minus__uMinus_param_u32_C
_Minus__uMinus_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__Minus_param_u32_C
	.private_extern _Minus__Minus_param_u32_C
_Minus__Minus_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1282
	cmpq	$2147483647, -16(%rbp)
	jle	L1283
L1282:
	movl	$19745, %edi
	call	__m3_fault
L1283:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_u8
	.private_extern _Minus__uMinus_var_u32_u8
_Minus__uMinus_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_u8
	.private_extern _Minus__Minus_var_u32_u8
_Minus__Minus_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1288
	cmpq	$2147483647, -16(%rbp)
	jle	L1289
L1288:
	movl	$19809, %edi
	call	__m3_fault
L1289:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_u8
	.private_extern _Minus__uMinus_param_u32_u8
_Minus__uMinus_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_u8
	.private_extern _Minus__Minus_param_u32_u8
_Minus__Minus_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1294
	cmpq	$2147483647, -16(%rbp)
	jle	L1295
L1294:
	movl	$19873, %edi
	call	__m3_fault
L1295:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i8
	.private_extern _Minus__uMinus_var_u32_i8
_Minus__uMinus_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_i8
	.private_extern _Minus__Minus_var_u32_i8
_Minus__Minus_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L1300
	movl	$19937, %edi
	call	__m3_fault
L1300:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_i8
	.private_extern _Minus__uMinus_param_u32_i8
_Minus__uMinus_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_i8
	.private_extern _Minus__Minus_param_u32_i8
_Minus__Minus_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L1305
	movl	$20001, %edi
	call	__m3_fault
L1305:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_L
	.private_extern _Minus__uMinus_var_u32_L
_Minus__uMinus_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_var_u32_L
	.private_extern _Minus__Minus_var_u32_L
_Minus__Minus_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_L
	.private_extern _Minus__uMinus_param_u32_L
_Minus__uMinus_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_L
	.private_extern _Minus__Minus_param_u32_L
_Minus__Minus_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_u32
	.private_extern _Minus__uMinus_var_u32_u32
_Minus__uMinus_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Minus__Minus_var_u32_u32
	.private_extern _Minus__Minus_var_u32_u32
_Minus__Minus_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$0, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1318
	cmpq	$2147483647, -16(%rbp)
	jle	L1319
L1318:
	movl	$20193, %edi
	call	__m3_fault
L1319:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_param_u32_u32
	.private_extern _Minus__uMinus_param_u32_u32
_Minus__uMinus_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _Minus__Minus_param_u32_u32
	.private_extern _Minus__Minus_param_u32_u32
_Minus__Minus_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1324
	cmpq	$2147483647, -16(%rbp)
	jle	L1325
L1324:
	movl	$20257, %edi
	call	__m3_fault
L1325:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus_M3
_Minus_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Minus(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Minus(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Minus_M3"
	.space 1
	.ascii "Minus_param_u32_u32"
	.space 1
	.ascii "uMinus_param_u32_u32"
	.space 1
	.ascii "Minus_var_u32_u32"
	.space 1
	.ascii "uMinus_var_u32_u32"
	.space 1
	.ascii "Minus_param_u32_L"
	.space 1
	.ascii "uMinus_param_u32_L"
	.space 1
	.ascii "Minus_var_u32_L"
	.space 1
	.ascii "uMinus_var_u32_L"
	.space 1
	.ascii "Minus_param_u32_i8"
	.space 1
	.ascii "uMinus_param_u32_i8"
	.space 1
	.ascii "Minus_var_u32_i8"
	.space 1
	.ascii "uMinus_var_u32_i8"
	.space 1
	.ascii "Minus_param_u32_u8"
	.space 1
	.ascii "uMinus_param_u32_u8"
	.space 1
	.ascii "Minus_var_u32_u8"
	.space 1
	.ascii "uMinus_var_u32_u8"
	.space 1
	.ascii "Minus_param_u32_C"
	.space 1
	.ascii "uMinus_param_u32_C"
	.space 1
	.ascii "Minus_var_u32_C"
	.space 1
	.ascii "uMinus_var_u32_C"
	.space 1
	.ascii "Minus_param_u32_i64"
	.space 1
	.ascii "uMinus_param_u32_i64"
	.space 1
	.ascii "Minus_var_u32_i64"
	.space 1
	.ascii "uMinus_var_u32_i64"
	.space 1
	.ascii "Minus_param_u32_I"
	.space 1
	.ascii "uMinus_param_u32_I"
	.space 1
	.ascii "Minus_var_u32_I"
	.space 1
	.ascii "uMinus_var_u32_I"
	.space 1
	.ascii "Minus_param_u32_i32"
	.space 1
	.ascii "uMinus_param_u32_i32"
	.space 1
	.ascii "Minus_var_u32_i32"
	.space 1
	.ascii "uMinus_var_u32_i32"
	.space 1
	.ascii "Minus_param_u32_i16"
	.space 1
	.ascii "uMinus_param_u32_i16"
	.space 1
	.ascii "Minus_var_u32_i16"
	.space 1
	.ascii "uMinus_var_u32_i16"
	.space 1
	.ascii "Minus_param_u32_LC"
	.space 1
	.ascii "uMinus_param_u32_LC"
	.space 1
	.ascii "Minus_var_u32_LC"
	.space 1
	.ascii "uMinus_var_u32_LC"
	.space 1
	.ascii "Minus_param_u32_u64"
	.space 1
	.ascii "uMinus_param_u32_u64"
	.space 1
	.ascii "Minus_var_u32_u64"
	.space 1
	.ascii "uMinus_var_u32_u64"
	.space 1
	.ascii "Minus_param_u32_u16"
	.space 1
	.ascii "uMinus_param_u32_u16"
	.space 1
	.ascii "Minus_var_u32_u16"
	.space 1
	.ascii "uMinus_var_u32_u16"
	.space 1
	.ascii "Minus_param_L_u32"
	.space 1
	.ascii "uMinus_param_L_u32"
	.space 1
	.ascii "Minus_var_L_u32"
	.space 1
	.ascii "uMinus_var_L_u32"
	.space 1
	.ascii "Minus_param_L_L"
	.space 1
	.ascii "uMinus_param_L_L"
	.space 1
	.ascii "Minus_var_L_L"
	.space 1
	.ascii "uMinus_var_L_L"
	.space 1
	.ascii "Minus_param_L_i8"
	.space 1
	.ascii "uMinus_param_L_i8"
	.space 1
	.ascii "Minus_var_L_i8"
	.space 1
	.ascii "uMinus_var_L_i8"
	.space 1
	.ascii "Minus_param_L_u8"
	.space 1
	.ascii "uMinus_param_L_u8"
	.space 1
	.ascii "Minus_var_L_u8"
	.space 1
	.ascii "uMinus_var_L_u8"
	.space 1
	.ascii "Minus_param_L_C"
	.space 1
	.ascii "uMinus_param_L_C"
	.space 1
	.ascii "Minus_var_L_C"
	.space 1
	.ascii "uMinus_var_L_C"
	.space 1
	.ascii "Minus_param_L_i64"
	.space 1
	.ascii "uMinus_param_L_i64"
	.space 1
	.ascii "Minus_var_L_i64"
	.space 1
	.ascii "uMinus_var_L_i64"
	.space 1
	.ascii "Minus_param_L_I"
	.space 1
	.ascii "uMinus_param_L_I"
	.space 1
	.ascii "Minus_var_L_I"
	.space 1
	.ascii "uMinus_var_L_I"
	.space 1
	.ascii "Minus_param_L_i32"
	.space 1
	.ascii "uMinus_param_L_i32"
	.space 1
	.ascii "Minus_var_L_i32"
	.space 1
	.ascii "uMinus_var_L_i32"
	.space 1
	.ascii "Minus_param_L_i16"
	.space 1
	.ascii "uMinus_param_L_i16"
	.space 1
	.ascii "Minus_var_L_i16"
	.space 1
	.ascii "uMinus_var_L_i16"
	.space 1
	.ascii "Minus_param_L_LC"
	.space 1
	.ascii "uMinus_param_L_LC"
	.space 1
	.ascii "Minus_var_L_LC"
	.space 1
	.ascii "uMinus_var_L_LC"
	.space 1
	.ascii "Minus_param_L_u64"
	.space 1
	.ascii "uMinus_param_L_u64"
	.space 1
	.ascii "Minus_var_L_u64"
	.space 1
	.ascii "uMinus_var_L_u64"
	.space 1
	.ascii "Minus_param_L_u16"
	.space 1
	.ascii "uMinus_param_L_u16"
	.space 1
	.ascii "Minus_var_L_u16"
	.space 1
	.ascii "uMinus_var_L_u16"
	.space 1
	.ascii "Minus_param_i8_u32"
	.space 1
	.ascii "uMinus_param_i8_u32"
	.space 1
	.ascii "Minus_var_i8_u32"
	.space 1
	.ascii "uMinus_var_i8_u32"
	.space 1
	.ascii "Minus_param_i8_L"
	.space 1
	.ascii "uMinus_param_i8_L"
	.space 1
	.ascii "Minus_var_i8_L"
	.space 1
	.ascii "uMinus_var_i8_L"
	.space 1
	.ascii "Minus_param_i8_i8"
	.space 1
	.ascii "uMinus_param_i8_i8"
	.space 1
	.ascii "Minus_var_i8_i8"
	.space 1
	.ascii "uMinus_var_i8_i8"
	.space 1
	.ascii "Minus_param_i8_u8"
	.space 1
	.ascii "uMinus_param_i8_u8"
	.space 1
	.ascii "Minus_var_i8_u8"
	.space 1
	.ascii "uMinus_var_i8_u8"
	.space 1
	.ascii "Minus_param_i8_C"
	.space 1
	.ascii "uMinus_param_i8_C"
	.space 1
	.ascii "Minus_var_i8_C"
	.space 1
	.ascii "uMinus_var_i8_C"
	.space 1
	.ascii "Minus_param_i8_i64"
	.space 1
	.ascii "uMinus_param_i8_i64"
	.space 1
	.ascii "Minus_var_i8_i64"
	.space 1
	.ascii "uMinus_var_i8_i64"
	.space 1
	.ascii "Minus_param_i8_I"
	.space 1
	.ascii "uMinus_param_i8_I"
	.space 1
	.ascii "Minus_var_i8_I"
	.space 1
	.ascii "uMinus_var_i8_I"
	.space 1
	.ascii "Minus_param_i8_i32"
	.space 1
	.ascii "uMinus_param_i8_i32"
	.space 1
	.ascii "Minus_var_i8_i32"
	.space 1
	.ascii "uMinus_var_i8_i32"
	.space 1
	.ascii "Minus_param_i8_i16"
	.space 1
	.ascii "uMinus_param_i8_i16"
	.space 1
	.ascii "Minus_var_i8_i16"
	.space 1
	.ascii "uMinus_var_i8_i16"
	.space 1
	.ascii "Minus_param_i8_LC"
	.space 1
	.ascii "uMinus_param_i8_LC"
	.space 1
	.ascii "Minus_var_i8_LC"
	.space 1
	.ascii "uMinus_var_i8_LC"
	.space 1
	.ascii "Minus_param_i8_u64"
	.space 1
	.ascii "uMinus_param_i8_u64"
	.space 1
	.ascii "Minus_var_i8_u64"
	.space 1
	.ascii "uMinus_var_i8_u64"
	.space 1
	.ascii "Minus_param_i8_u16"
	.space 1
	.ascii "uMinus_param_i8_u16"
	.space 1
	.ascii "Minus_var_i8_u16"
	.space 1
	.ascii "uMinus_var_i8_u16"
	.space 1
	.ascii "Minus_param_u8_u32"
	.space 1
	.ascii "uMinus_param_u8_u32"
	.space 1
	.ascii "Minus_var_u8_u32"
	.space 1
	.ascii "uMinus_var_u8_u32"
	.space 1
	.ascii "Minus_param_u8_L"
	.space 1
	.ascii "uMinus_param_u8_L"
	.space 1
	.ascii "Minus_var_u8_L"
	.space 1
	.ascii "uMinus_var_u8_L"
	.space 1
	.ascii "Minus_param_u8_i8"
	.space 1
	.ascii "uMinus_param_u8_i8"
	.space 1
	.ascii "Minus_var_u8_i8"
	.space 1
	.ascii "uMinus_var_u8_i8"
	.space 1
	.ascii "Minus_param_u8_u8"
	.space 1
	.ascii "uMinus_param_u8_u8"
	.space 1
	.ascii "Minus_var_u8_u8"
	.space 1
	.ascii "uMinus_var_u8_u8"
	.space 1
	.ascii "Minus_param_u8_C"
	.space 1
	.ascii "uMinus_param_u8_C"
	.space 1
	.ascii "Minus_var_u8_C"
	.space 1
	.ascii "uMinus_var_u8_C"
	.space 1
	.ascii "Minus_param_u8_i64"
	.space 1
	.ascii "uMinus_param_u8_i64"
	.space 1
	.ascii "Minus_var_u8_i64"
	.space 1
	.ascii "uMinus_var_u8_i64"
	.space 1
	.ascii "Minus_param_u8_I"
	.space 1
	.ascii "uMinus_param_u8_I"
	.space 1
	.ascii "Minus_var_u8_I"
	.space 1
	.ascii "uMinus_var_u8_I"
	.space 1
	.ascii "Minus_param_u8_i32"
	.space 1
	.ascii "uMinus_param_u8_i32"
	.space 1
	.ascii "Minus_var_u8_i32"
	.space 1
	.ascii "uMinus_var_u8_i32"
	.space 1
	.ascii "Minus_param_u8_i16"
	.space 1
	.ascii "uMinus_param_u8_i16"
	.space 1
	.ascii "Minus_var_u8_i16"
	.space 1
	.ascii "uMinus_var_u8_i16"
	.space 1
	.ascii "Minus_param_u8_LC"
	.space 1
	.ascii "uMinus_param_u8_LC"
	.space 1
	.ascii "Minus_var_u8_LC"
	.space 1
	.ascii "uMinus_var_u8_LC"
	.space 1
	.ascii "Minus_param_u8_u64"
	.space 1
	.ascii "uMinus_param_u8_u64"
	.space 1
	.ascii "Minus_var_u8_u64"
	.space 1
	.ascii "uMinus_var_u8_u64"
	.space 1
	.ascii "Minus_param_u8_u16"
	.space 1
	.ascii "uMinus_param_u8_u16"
	.space 1
	.ascii "Minus_var_u8_u16"
	.space 1
	.ascii "uMinus_var_u8_u16"
	.space 1
	.ascii "Minus_param_f32_f32"
	.space 1
	.ascii "Minus_var_f32_f32"
	.space 1
	.ascii "Minus_param_C_u32"
	.space 1
	.ascii "uMinus_param_C_u32"
	.space 1
	.ascii "Minus_var_C_u32"
	.space 1
	.ascii "uMinus_var_C_u32"
	.space 1
	.ascii "Minus_param_C_L"
	.space 1
	.ascii "uMinus_param_C_L"
	.space 1
	.ascii "Minus_var_C_L"
	.space 1
	.ascii "uMinus_var_C_L"
	.space 1
	.ascii "Minus_param_C_i8"
	.space 1
	.ascii "uMinus_param_C_i8"
	.space 1
	.ascii "Minus_var_C_i8"
	.space 1
	.ascii "uMinus_var_C_i8"
	.space 1
	.ascii "Minus_param_C_u8"
	.space 1
	.ascii "uMinus_param_C_u8"
	.space 1
	.ascii "Minus_var_C_u8"
	.space 1
	.ascii "uMinus_var_C_u8"
	.space 1
	.ascii "Minus_param_C_C"
	.space 1
	.ascii "uMinus_param_C_C"
	.space 1
	.ascii "Minus_var_C_C"
	.space 1
	.ascii "uMinus_var_C_C"
	.space 1
	.ascii "Minus_param_C_i64"
	.space 1
	.ascii "uMinus_param_C_i64"
	.space 1
	.ascii "Minus_var_C_i64"
	.space 1
	.ascii "uMinus_var_C_i64"
	.space 1
	.ascii "Minus_param_C_I"
	.space 1
	.ascii "uMinus_param_C_I"
	.space 1
	.ascii "Minus_var_C_I"
	.space 1
	.ascii "uMinus_var_C_I"
	.space 1
	.ascii "Minus_param_C_i32"
	.space 1
	.ascii "uMinus_param_C_i32"
	.space 1
	.ascii "Minus_var_C_i32"
	.space 1
	.ascii "uMinus_var_C_i32"
	.space 1
	.ascii "Minus_param_C_i16"
	.space 1
	.ascii "uMinus_param_C_i16"
	.space 1
	.ascii "Minus_var_C_i16"
	.space 1
	.ascii "uMinus_var_C_i16"
	.space 1
	.ascii "Minus_param_C_LC"
	.space 1
	.ascii "uMinus_param_C_LC"
	.space 1
	.ascii "Minus_var_C_LC"
	.space 1
	.ascii "uMinus_var_C_LC"
	.space 1
	.ascii "Minus_param_C_u64"
	.space 1
	.ascii "uMinus_param_C_u64"
	.space 1
	.ascii "Minus_var_C_u64"
	.space 1
	.ascii "uMinus_var_C_u64"
	.space 1
	.ascii "Minus_param_C_u16"
	.space 1
	.ascii "uMinus_param_C_u16"
	.space 1
	.ascii "Minus_var_C_u16"
	.space 1
	.ascii "uMinus_var_C_u16"
	.space 1
	.ascii "Minus_param_i64_u32"
	.space 1
	.ascii "uMinus_param_i64_u32"
	.space 1
	.ascii "Minus_var_i64_u32"
	.space 1
	.ascii "uMinus_var_i64_u32"
	.space 1
	.ascii "Minus_param_i64_L"
	.space 1
	.ascii "uMinus_param_i64_L"
	.space 1
	.ascii "Minus_var_i64_L"
	.space 1
	.ascii "uMinus_var_i64_L"
	.space 1
	.ascii "Minus_param_i64_i8"
	.space 1
	.ascii "uMinus_param_i64_i8"
	.space 1
	.ascii "Minus_var_i64_i8"
	.space 1
	.ascii "uMinus_var_i64_i8"
	.space 1
	.ascii "Minus_param_i64_u8"
	.space 1
	.ascii "uMinus_param_i64_u8"
	.space 1
	.ascii "Minus_var_i64_u8"
	.space 1
	.ascii "uMinus_var_i64_u8"
	.space 1
	.ascii "Minus_param_i64_C"
	.space 1
	.ascii "uMinus_param_i64_C"
	.space 1
	.ascii "Minus_var_i64_C"
	.space 1
	.ascii "uMinus_var_i64_C"
	.space 1
	.ascii "Minus_param_i64_i64"
	.space 1
	.ascii "uMinus_param_i64_i64"
	.space 1
	.ascii "Minus_var_i64_i64"
	.space 1
	.ascii "uMinus_var_i64_i64"
	.space 1
	.ascii "Minus_param_i64_I"
	.space 1
	.ascii "uMinus_param_i64_I"
	.space 1
	.ascii "Minus_var_i64_I"
	.space 1
	.ascii "uMinus_var_i64_I"
	.space 1
	.ascii "Minus_param_i64_i32"
	.space 1
	.ascii "uMinus_param_i64_i32"
	.space 1
	.ascii "Minus_var_i64_i32"
	.space 1
	.ascii "uMinus_var_i64_i32"
	.space 1
	.ascii "Minus_param_i64_i16"
	.space 1
	.ascii "uMinus_param_i64_i16"
	.space 1
	.ascii "Minus_var_i64_i16"
	.space 1
	.ascii "uMinus_var_i64_i16"
	.space 1
	.ascii "Minus_param_i64_LC"
	.space 1
	.ascii "uMinus_param_i64_LC"
	.space 1
	.ascii "Minus_var_i64_LC"
	.space 1
	.ascii "uMinus_var_i64_LC"
	.space 1
	.ascii "Minus_param_i64_u64"
	.space 1
	.ascii "uMinus_param_i64_u64"
	.space 1
	.ascii "Minus_var_i64_u64"
	.space 1
	.ascii "uMinus_var_i64_u64"
	.space 1
	.ascii "Minus_param_i64_u16"
	.space 1
	.ascii "uMinus_param_i64_u16"
	.space 1
	.ascii "Minus_var_i64_u16"
	.space 1
	.ascii "uMinus_var_i64_u16"
	.space 1
	.ascii "Minus_param_I_u32"
	.space 1
	.ascii "uMinus_param_I_u32"
	.space 1
	.ascii "Minus_var_I_u32"
	.space 1
	.ascii "uMinus_var_I_u32"
	.space 1
	.ascii "Minus_param_I_L"
	.space 1
	.ascii "uMinus_param_I_L"
	.space 1
	.ascii "Minus_var_I_L"
	.space 1
	.ascii "uMinus_var_I_L"
	.space 1
	.ascii "Minus_param_I_i8"
	.space 1
	.ascii "uMinus_param_I_i8"
	.space 1
	.ascii "Minus_var_I_i8"
	.space 1
	.ascii "uMinus_var_I_i8"
	.space 1
	.ascii "Minus_param_I_u8"
	.space 1
	.ascii "uMinus_param_I_u8"
	.space 1
	.ascii "Minus_var_I_u8"
	.space 1
	.ascii "uMinus_var_I_u8"
	.space 1
	.ascii "Minus_param_I_C"
	.space 1
	.ascii "uMinus_param_I_C"
	.space 1
	.ascii "Minus_var_I_C"
	.space 1
	.ascii "uMinus_var_I_C"
	.space 1
	.ascii "Minus_param_I_i64"
	.space 1
	.ascii "uMinus_param_I_i64"
	.space 1
	.ascii "Minus_var_I_i64"
	.space 1
	.ascii "uMinus_var_I_i64"
	.space 1
	.ascii "Minus_param_I_I"
	.space 1
	.ascii "uMinus_param_I_I"
	.space 1
	.ascii "Minus_var_I_I"
	.space 1
	.ascii "uMinus_var_I_I"
	.space 1
	.ascii "Minus_param_I_i32"
	.space 1
	.ascii "uMinus_param_I_i32"
	.space 1
	.ascii "Minus_var_I_i32"
	.space 1
	.ascii "uMinus_var_I_i32"
	.space 1
	.ascii "Minus_param_I_i16"
	.space 1
	.ascii "uMinus_param_I_i16"
	.space 1
	.ascii "Minus_var_I_i16"
	.space 1
	.ascii "uMinus_var_I_i16"
	.space 1
	.ascii "Minus_param_I_LC"
	.space 1
	.ascii "uMinus_param_I_LC"
	.space 1
	.ascii "Minus_var_I_LC"
	.space 1
	.ascii "uMinus_var_I_LC"
	.space 1
	.ascii "Minus_param_I_u64"
	.space 1
	.ascii "uMinus_param_I_u64"
	.space 1
	.ascii "Minus_var_I_u64"
	.space 1
	.ascii "uMinus_var_I_u64"
	.space 1
	.ascii "Minus_param_I_u16"
	.space 1
	.ascii "uMinus_param_I_u16"
	.space 1
	.ascii "Minus_var_I_u16"
	.space 1
	.ascii "uMinus_var_I_u16"
	.space 1
	.ascii "Minus_param_i32_u32"
	.space 1
	.ascii "uMinus_param_i32_u32"
	.space 1
	.ascii "Minus_var_i32_u32"
	.space 1
	.ascii "uMinus_var_i32_u32"
	.space 1
	.ascii "Minus_param_i32_L"
	.space 1
	.ascii "uMinus_param_i32_L"
	.space 1
	.ascii "Minus_var_i32_L"
	.space 1
	.ascii "uMinus_var_i32_L"
	.space 1
	.ascii "Minus_param_i32_i8"
	.space 1
	.ascii "uMinus_param_i32_i8"
	.space 1
	.ascii "Minus_var_i32_i8"
	.space 1
	.ascii "uMinus_var_i32_i8"
	.space 1
	.ascii "Minus_param_i32_u8"
	.space 1
	.ascii "uMinus_param_i32_u8"
	.space 1
	.ascii "Minus_var_i32_u8"
	.space 1
	.ascii "uMinus_var_i32_u8"
	.space 1
	.ascii "Minus_param_i32_C"
	.space 1
	.ascii "uMinus_param_i32_C"
	.space 1
	.ascii "Minus_var_i32_C"
	.space 1
	.ascii "uMinus_var_i32_C"
	.space 1
	.ascii "Minus_param_i32_i64"
	.space 1
	.ascii "uMinus_param_i32_i64"
	.space 1
	.ascii "Minus_var_i32_i64"
	.space 1
	.ascii "uMinus_var_i32_i64"
	.space 1
	.ascii "Minus_param_i32_I"
	.space 1
	.ascii "uMinus_param_i32_I"
	.space 1
	.ascii "Minus_var_i32_I"
	.space 1
	.ascii "uMinus_var_i32_I"
	.space 1
	.ascii "Minus_param_i32_i32"
	.space 1
	.ascii "uMinus_param_i32_i32"
	.space 1
	.ascii "Minus_var_i32_i32"
	.space 1
	.ascii "uMinus_var_i32_i32"
	.space 1
	.ascii "Minus_param_i32_i16"
	.space 1
	.ascii "uMinus_param_i32_i16"
	.space 1
	.ascii "Minus_var_i32_i16"
	.space 1
	.ascii "uMinus_var_i32_i16"
	.space 1
	.ascii "Minus_param_i32_LC"
	.space 1
	.ascii "uMinus_param_i32_LC"
	.space 1
	.ascii "Minus_var_i32_LC"
	.space 1
	.ascii "uMinus_var_i32_LC"
	.space 1
	.ascii "Minus_param_i32_u64"
	.space 1
	.ascii "uMinus_param_i32_u64"
	.space 1
	.ascii "Minus_var_i32_u64"
	.space 1
	.ascii "uMinus_var_i32_u64"
	.space 1
	.ascii "Minus_param_i32_u16"
	.space 1
	.ascii "uMinus_param_i32_u16"
	.space 1
	.ascii "Minus_var_i32_u16"
	.space 1
	.ascii "uMinus_var_i32_u16"
	.space 1
	.ascii "Minus_param_i16_u32"
	.space 1
	.ascii "uMinus_param_i16_u32"
	.space 1
	.ascii "Minus_var_i16_u32"
	.space 1
	.ascii "uMinus_var_i16_u32"
	.space 1
	.ascii "Minus_param_i16_L"
	.space 1
	.ascii "uMinus_param_i16_L"
	.space 1
	.ascii "Minus_var_i16_L"
	.space 1
	.ascii "uMinus_var_i16_L"
	.space 1
	.ascii "Minus_param_i16_i8"
	.space 1
	.ascii "uMinus_param_i16_i8"
	.space 1
	.ascii "Minus_var_i16_i8"
	.space 1
	.ascii "uMinus_var_i16_i8"
	.space 1
	.ascii "Minus_param_i16_u8"
	.space 1
	.ascii "uMinus_param_i16_u8"
	.space 1
	.ascii "Minus_var_i16_u8"
	.space 1
	.ascii "uMinus_var_i16_u8"
	.space 1
	.ascii "Minus_param_i16_C"
	.space 1
	.ascii "uMinus_param_i16_C"
	.space 1
	.ascii "Minus_var_i16_C"
	.space 1
	.ascii "uMinus_var_i16_C"
	.space 1
	.ascii "Minus_param_i16_i64"
	.space 1
	.ascii "uMinus_param_i16_i64"
	.space 1
	.ascii "Minus_var_i16_i64"
	.space 1
	.ascii "uMinus_var_i16_i64"
	.space 1
	.ascii "Minus_param_i16_I"
	.space 1
	.ascii "uMinus_param_i16_I"
	.space 1
	.ascii "Minus_var_i16_I"
	.space 1
	.ascii "uMinus_var_i16_I"
	.space 1
	.ascii "Minus_param_i16_i32"
	.space 1
	.ascii "uMinus_param_i16_i32"
	.space 1
	.ascii "Minus_var_i16_i32"
	.space 1
	.ascii "uMinus_var_i16_i32"
	.space 1
	.ascii "Minus_param_i16_i16"
	.space 1
	.ascii "uMinus_param_i16_i16"
	.space 1
	.ascii "Minus_var_i16_i16"
	.space 1
	.ascii "uMinus_var_i16_i16"
	.space 1
	.ascii "Minus_param_i16_LC"
	.space 1
	.ascii "uMinus_param_i16_LC"
	.space 1
	.ascii "Minus_var_i16_LC"
	.space 1
	.ascii "uMinus_var_i16_LC"
	.space 1
	.ascii "Minus_param_i16_u64"
	.space 1
	.ascii "uMinus_param_i16_u64"
	.space 1
	.ascii "Minus_var_i16_u64"
	.space 1
	.ascii "uMinus_var_i16_u64"
	.space 1
	.ascii "Minus_param_i16_u16"
	.space 1
	.ascii "uMinus_param_i16_u16"
	.space 1
	.ascii "Minus_var_i16_u16"
	.space 1
	.ascii "uMinus_var_i16_u16"
	.space 1
	.ascii "Minus_param_f64_f64"
	.space 1
	.ascii "Minus_var_f64_f64"
	.space 1
	.ascii "Minus_param_LC_u32"
	.space 1
	.ascii "uMinus_param_LC_u32"
	.space 1
	.ascii "Minus_var_LC_u32"
	.space 1
	.ascii "uMinus_var_LC_u32"
	.space 1
	.ascii "Minus_param_LC_L"
	.space 1
	.ascii "uMinus_param_LC_L"
	.space 1
	.ascii "Minus_var_LC_L"
	.space 1
	.ascii "uMinus_var_LC_L"
	.space 1
	.ascii "Minus_param_LC_i8"
	.space 1
	.ascii "uMinus_param_LC_i8"
	.space 1
	.ascii "Minus_var_LC_i8"
	.space 1
	.ascii "uMinus_var_LC_i8"
	.space 1
	.ascii "Minus_param_LC_u8"
	.space 1
	.ascii "uMinus_param_LC_u8"
	.space 1
	.ascii "Minus_var_LC_u8"
	.space 1
	.ascii "uMinus_var_LC_u8"
	.space 1
	.ascii "Minus_param_LC_C"
	.space 1
	.ascii "uMinus_param_LC_C"
	.space 1
	.ascii "Minus_var_LC_C"
	.space 1
	.ascii "uMinus_var_LC_C"
	.space 1
	.ascii "Minus_param_LC_i64"
	.space 1
	.ascii "uMinus_param_LC_i64"
	.space 1
	.ascii "Minus_var_LC_i64"
	.space 1
	.ascii "uMinus_var_LC_i64"
	.space 1
	.ascii "Minus_param_LC_I"
	.space 1
	.ascii "uMinus_param_LC_I"
	.space 1
	.ascii "Minus_var_LC_I"
	.space 1
	.ascii "uMinus_var_LC_I"
	.space 1
	.ascii "Minus_param_LC_i32"
	.space 1
	.ascii "uMinus_param_LC_i32"
	.space 1
	.ascii "Minus_var_LC_i32"
	.space 1
	.ascii "uMinus_var_LC_i32"
	.space 1
	.ascii "Minus_param_LC_i16"
	.space 1
	.ascii "uMinus_param_LC_i16"
	.space 1
	.ascii "Minus_var_LC_i16"
	.space 1
	.ascii "uMinus_var_LC_i16"
	.space 1
	.ascii "Minus_param_LC_LC"
	.space 1
	.ascii "uMinus_param_LC_LC"
	.space 1
	.ascii "Minus_var_LC_LC"
	.space 1
	.ascii "uMinus_var_LC_LC"
	.space 1
	.ascii "Minus_param_LC_u64"
	.space 1
	.ascii "uMinus_param_LC_u64"
	.space 1
	.ascii "Minus_var_LC_u64"
	.space 1
	.ascii "uMinus_var_LC_u64"
	.space 1
	.ascii "Minus_param_LC_u16"
	.space 1
	.ascii "uMinus_param_LC_u16"
	.space 1
	.ascii "Minus_var_LC_u16"
	.space 1
	.ascii "uMinus_var_LC_u16"
	.space 1
	.ascii "Minus_param_u64_u32"
	.space 1
	.ascii "uMinus_param_u64_u32"
	.space 1
	.ascii "Minus_var_u64_u32"
	.space 1
	.ascii "uMinus_var_u64_u32"
	.space 1
	.ascii "Minus_param_u64_L"
	.space 1
	.ascii "uMinus_param_u64_L"
	.space 1
	.ascii "Minus_var_u64_L"
	.space 1
	.ascii "uMinus_var_u64_L"
	.space 1
	.ascii "Minus_param_u64_i8"
	.space 1
	.ascii "uMinus_param_u64_i8"
	.space 1
	.ascii "Minus_var_u64_i8"
	.space 1
	.ascii "uMinus_var_u64_i8"
	.space 1
	.ascii "Minus_param_u64_u8"
	.space 1
	.ascii "uMinus_param_u64_u8"
	.space 1
	.ascii "Minus_var_u64_u8"
	.space 1
	.ascii "uMinus_var_u64_u8"
	.space 1
	.ascii "Minus_param_u64_C"
	.space 1
	.ascii "uMinus_param_u64_C"
	.space 1
	.ascii "Minus_var_u64_C"
	.space 1
	.ascii "uMinus_var_u64_C"
	.space 1
	.ascii "Minus_param_u64_i64"
	.space 1
	.ascii "uMinus_param_u64_i64"
	.space 1
	.ascii "Minus_var_u64_i64"
	.space 1
	.ascii "uMinus_var_u64_i64"
	.space 1
	.ascii "Minus_param_u64_I"
	.space 1
	.ascii "uMinus_param_u64_I"
	.space 1
	.ascii "Minus_var_u64_I"
	.space 1
	.ascii "uMinus_var_u64_I"
	.space 1
	.ascii "Minus_param_u64_i32"
	.space 1
	.ascii "uMinus_param_u64_i32"
	.space 1
	.ascii "Minus_var_u64_i32"
	.space 1
	.ascii "uMinus_var_u64_i32"
	.space 1
	.ascii "Minus_param_u64_i16"
	.space 1
	.ascii "uMinus_param_u64_i16"
	.space 1
	.ascii "Minus_var_u64_i16"
	.space 1
	.ascii "uMinus_var_u64_i16"
	.space 1
	.ascii "Minus_param_u64_LC"
	.space 1
	.ascii "uMinus_param_u64_LC"
	.space 1
	.ascii "Minus_var_u64_LC"
	.space 1
	.ascii "uMinus_var_u64_LC"
	.space 1
	.ascii "Minus_param_u64_u64"
	.space 1
	.ascii "uMinus_param_u64_u64"
	.space 1
	.ascii "Minus_var_u64_u64"
	.space 1
	.ascii "uMinus_var_u64_u64"
	.space 1
	.ascii "Minus_param_u64_u16"
	.space 1
	.ascii "uMinus_param_u64_u16"
	.space 1
	.ascii "Minus_var_u64_u16"
	.space 1
	.ascii "uMinus_var_u64_u16"
	.space 1
	.ascii "Minus_param_u16_u32"
	.space 1
	.ascii "uMinus_param_u16_u32"
	.space 1
	.ascii "Minus_var_u16_u32"
	.space 1
	.ascii "uMinus_var_u16_u32"
	.space 1
	.ascii "Minus_param_u16_L"
	.space 1
	.ascii "uMinus_param_u16_L"
	.space 1
	.ascii "Minus_var_u16_L"
	.space 1
	.ascii "uMinus_var_u16_L"
	.space 1
	.ascii "Minus_param_u16_i8"
	.space 1
	.ascii "uMinus_param_u16_i8"
	.space 1
	.ascii "Minus_var_u16_i8"
	.space 1
	.ascii "uMinus_var_u16_i8"
	.space 1
	.ascii "Minus_param_u16_u8"
	.space 1
	.ascii "uMinus_param_u16_u8"
	.space 1
	.ascii "Minus_var_u16_u8"
	.space 1
	.ascii "uMinus_var_u16_u8"
	.space 1
	.ascii "Minus_param_u16_C"
	.space 1
	.ascii "uMinus_param_u16_C"
	.space 1
	.ascii "Minus_var_u16_C"
	.space 1
	.ascii "uMinus_var_u16_C"
	.space 1
	.ascii "Minus_param_u16_i64"
	.space 1
	.ascii "uMinus_param_u16_i64"
	.space 1
	.ascii "Minus_var_u16_i64"
	.space 1
	.ascii "uMinus_var_u16_i64"
	.space 1
	.ascii "Minus_param_u16_I"
	.space 1
	.ascii "uMinus_param_u16_I"
	.space 1
	.ascii "Minus_var_u16_I"
	.space 1
	.ascii "uMinus_var_u16_I"
	.space 1
	.ascii "Minus_param_u16_i32"
	.space 1
	.ascii "uMinus_param_u16_i32"
	.space 1
	.ascii "Minus_var_u16_i32"
	.space 1
	.ascii "uMinus_var_u16_i32"
	.space 1
	.ascii "Minus_param_u16_i16"
	.space 1
	.ascii "uMinus_param_u16_i16"
	.space 1
	.ascii "Minus_var_u16_i16"
	.space 1
	.ascii "uMinus_var_u16_i16"
	.space 1
	.ascii "Minus_param_u16_LC"
	.space 1
	.ascii "uMinus_param_u16_LC"
	.space 1
	.ascii "Minus_var_u16_LC"
	.space 1
	.ascii "uMinus_var_u16_LC"
	.space 1
	.ascii "Minus_param_u16_u64"
	.space 1
	.ascii "uMinus_param_u16_u64"
	.space 1
	.ascii "Minus_var_u16_u64"
	.space 1
	.ascii "uMinus_var_u16_u64"
	.space 1
	.ascii "Minus_param_u16_u16"
	.space 1
	.ascii "uMinus_param_u16_u16"
	.space 1
	.ascii "Minus_var_u16_u16"
	.space 1
	.ascii "uMinus_var_u16_u16"
	.space 4
	.quad	_Minus_M3
	.quad	_L_1
	.quad	_Minus__Minus_param_u32_u32
	.quad	_L_1+9
	.quad	_Minus__uMinus_param_u32_u32
	.quad	_L_1+29
	.quad	_Minus__Minus_var_u32_u32
	.quad	_L_1+50
	.quad	_Minus__uMinus_var_u32_u32
	.quad	_L_1+68
	.quad	_Minus__Minus_param_u32_L
	.quad	_L_1+87
	.quad	_Minus__uMinus_param_u32_L
	.quad	_L_1+105
	.quad	_Minus__Minus_var_u32_L
	.quad	_L_1+124
	.quad	_Minus__uMinus_var_u32_L
	.quad	_L_1+140
	.quad	_Minus__Minus_param_u32_i8
	.quad	_L_1+157
	.quad	_Minus__uMinus_param_u32_i8
	.quad	_L_1+176
	.quad	_Minus__Minus_var_u32_i8
	.quad	_L_1+196
	.quad	_Minus__uMinus_var_u32_i8
	.quad	_L_1+213
	.quad	_Minus__Minus_param_u32_u8
	.quad	_L_1+231
	.quad	_Minus__uMinus_param_u32_u8
	.quad	_L_1+250
	.quad	_Minus__Minus_var_u32_u8
	.quad	_L_1+270
	.quad	_Minus__uMinus_var_u32_u8
	.quad	_L_1+287
	.quad	_Minus__Minus_param_u32_C
	.quad	_L_1+305
	.quad	_Minus__uMinus_param_u32_C
	.quad	_L_1+323
	.quad	_Minus__Minus_var_u32_C
	.quad	_L_1+342
	.quad	_Minus__uMinus_var_u32_C
	.quad	_L_1+358
	.quad	_Minus__Minus_param_u32_i64
	.quad	_L_1+375
	.quad	_Minus__uMinus_param_u32_i64
	.quad	_L_1+395
	.quad	_Minus__Minus_var_u32_i64
	.quad	_L_1+416
	.quad	_Minus__uMinus_var_u32_i64
	.quad	_L_1+434
	.quad	_Minus__Minus_param_u32_I
	.quad	_L_1+453
	.quad	_Minus__uMinus_param_u32_I
	.quad	_L_1+471
	.quad	_Minus__Minus_var_u32_I
	.quad	_L_1+490
	.quad	_Minus__uMinus_var_u32_I
	.quad	_L_1+506
	.quad	_Minus__Minus_param_u32_i32
	.quad	_L_1+523
	.quad	_Minus__uMinus_param_u32_i32
	.quad	_L_1+543
	.quad	_Minus__Minus_var_u32_i32
	.quad	_L_1+564
	.quad	_Minus__uMinus_var_u32_i32
	.quad	_L_1+582
	.quad	_Minus__Minus_param_u32_i16
	.quad	_L_1+601
	.quad	_Minus__uMinus_param_u32_i16
	.quad	_L_1+621
	.quad	_Minus__Minus_var_u32_i16
	.quad	_L_1+642
	.quad	_Minus__uMinus_var_u32_i16
	.quad	_L_1+660
	.quad	_Minus__Minus_param_u32_LC
	.quad	_L_1+679
	.quad	_Minus__uMinus_param_u32_LC
	.quad	_L_1+698
	.quad	_Minus__Minus_var_u32_LC
	.quad	_L_1+718
	.quad	_Minus__uMinus_var_u32_LC
	.quad	_L_1+735
	.quad	_Minus__Minus_param_u32_u64
	.quad	_L_1+753
	.quad	_Minus__uMinus_param_u32_u64
	.quad	_L_1+773
	.quad	_Minus__Minus_var_u32_u64
	.quad	_L_1+794
	.quad	_Minus__uMinus_var_u32_u64
	.quad	_L_1+812
	.quad	_Minus__Minus_param_u32_u16
	.quad	_L_1+831
	.quad	_Minus__uMinus_param_u32_u16
	.quad	_L_1+851
	.quad	_Minus__Minus_var_u32_u16
	.quad	_L_1+872
	.quad	_Minus__uMinus_var_u32_u16
	.quad	_L_1+890
	.quad	_Minus__Minus_param_L_u32
	.quad	_L_1+909
	.quad	_Minus__uMinus_param_L_u32
	.quad	_L_1+927
	.quad	_Minus__Minus_var_L_u32
	.quad	_L_1+946
	.quad	_Minus__uMinus_var_L_u32
	.quad	_L_1+962
	.quad	_Minus__Minus_param_L_L
	.quad	_L_1+979
	.quad	_Minus__uMinus_param_L_L
	.quad	_L_1+995
	.quad	_Minus__Minus_var_L_L
	.quad	_L_1+1012
	.quad	_Minus__uMinus_var_L_L
	.quad	_L_1+1026
	.quad	_Minus__Minus_param_L_i8
	.quad	_L_1+1041
	.quad	_Minus__uMinus_param_L_i8
	.quad	_L_1+1058
	.quad	_Minus__Minus_var_L_i8
	.quad	_L_1+1076
	.quad	_Minus__uMinus_var_L_i8
	.quad	_L_1+1091
	.quad	_Minus__Minus_param_L_u8
	.quad	_L_1+1107
	.quad	_Minus__uMinus_param_L_u8
	.quad	_L_1+1124
	.quad	_Minus__Minus_var_L_u8
	.quad	_L_1+1142
	.quad	_Minus__uMinus_var_L_u8
	.quad	_L_1+1157
	.quad	_Minus__Minus_param_L_C
	.quad	_L_1+1173
	.quad	_Minus__uMinus_param_L_C
	.quad	_L_1+1189
	.quad	_Minus__Minus_var_L_C
	.quad	_L_1+1206
	.quad	_Minus__uMinus_var_L_C
	.quad	_L_1+1220
	.quad	_Minus__Minus_param_L_i64
	.quad	_L_1+1235
	.quad	_Minus__uMinus_param_L_i64
	.quad	_L_1+1253
	.quad	_Minus__Minus_var_L_i64
	.quad	_L_1+1272
	.quad	_Minus__uMinus_var_L_i64
	.quad	_L_1+1288
	.quad	_Minus__Minus_param_L_I
	.quad	_L_1+1305
	.quad	_Minus__uMinus_param_L_I
	.quad	_L_1+1321
	.quad	_Minus__Minus_var_L_I
	.quad	_L_1+1338
	.quad	_Minus__uMinus_var_L_I
	.quad	_L_1+1352
	.quad	_Minus__Minus_param_L_i32
	.quad	_L_1+1367
	.quad	_Minus__uMinus_param_L_i32
	.quad	_L_1+1385
	.quad	_Minus__Minus_var_L_i32
	.quad	_L_1+1404
	.quad	_Minus__uMinus_var_L_i32
	.quad	_L_1+1420
	.quad	_Minus__Minus_param_L_i16
	.quad	_L_1+1437
	.quad	_Minus__uMinus_param_L_i16
	.quad	_L_1+1455
	.quad	_Minus__Minus_var_L_i16
	.quad	_L_1+1474
	.quad	_Minus__uMinus_var_L_i16
	.quad	_L_1+1490
	.quad	_Minus__Minus_param_L_LC
	.quad	_L_1+1507
	.quad	_Minus__uMinus_param_L_LC
	.quad	_L_1+1524
	.quad	_Minus__Minus_var_L_LC
	.quad	_L_1+1542
	.quad	_Minus__uMinus_var_L_LC
	.quad	_L_1+1557
	.quad	_Minus__Minus_param_L_u64
	.quad	_L_1+1573
	.quad	_Minus__uMinus_param_L_u64
	.quad	_L_1+1591
	.quad	_Minus__Minus_var_L_u64
	.quad	_L_1+1610
	.quad	_Minus__uMinus_var_L_u64
	.quad	_L_1+1626
	.quad	_Minus__Minus_param_L_u16
	.quad	_L_1+1643
	.quad	_Minus__uMinus_param_L_u16
	.quad	_L_1+1661
	.quad	_Minus__Minus_var_L_u16
	.quad	_L_1+1680
	.quad	_Minus__uMinus_var_L_u16
	.quad	_L_1+1696
	.quad	_Minus__Minus_param_i8_u32
	.quad	_L_1+1713
	.quad	_Minus__uMinus_param_i8_u32
	.quad	_L_1+1732
	.quad	_Minus__Minus_var_i8_u32
	.quad	_L_1+1752
	.quad	_Minus__uMinus_var_i8_u32
	.quad	_L_1+1769
	.quad	_Minus__Minus_param_i8_L
	.quad	_L_1+1787
	.quad	_Minus__uMinus_param_i8_L
	.quad	_L_1+1804
	.quad	_Minus__Minus_var_i8_L
	.quad	_L_1+1822
	.quad	_Minus__uMinus_var_i8_L
	.quad	_L_1+1837
	.quad	_Minus__Minus_param_i8_i8
	.quad	_L_1+1853
	.quad	_Minus__uMinus_param_i8_i8
	.quad	_L_1+1871
	.quad	_Minus__Minus_var_i8_i8
	.quad	_L_1+1890
	.quad	_Minus__uMinus_var_i8_i8
	.quad	_L_1+1906
	.quad	_Minus__Minus_param_i8_u8
	.quad	_L_1+1923
	.quad	_Minus__uMinus_param_i8_u8
	.quad	_L_1+1941
	.quad	_Minus__Minus_var_i8_u8
	.quad	_L_1+1960
	.quad	_Minus__uMinus_var_i8_u8
	.quad	_L_1+1976
	.quad	_Minus__Minus_param_i8_C
	.quad	_L_1+1993
	.quad	_Minus__uMinus_param_i8_C
	.quad	_L_1+2010
	.quad	_Minus__Minus_var_i8_C
	.quad	_L_1+2028
	.quad	_Minus__uMinus_var_i8_C
	.quad	_L_1+2043
	.quad	_Minus__Minus_param_i8_i64
	.quad	_L_1+2059
	.quad	_Minus__uMinus_param_i8_i64
	.quad	_L_1+2078
	.quad	_Minus__Minus_var_i8_i64
	.quad	_L_1+2098
	.quad	_Minus__uMinus_var_i8_i64
	.quad	_L_1+2115
	.quad	_Minus__Minus_param_i8_I
	.quad	_L_1+2133
	.quad	_Minus__uMinus_param_i8_I
	.quad	_L_1+2150
	.quad	_Minus__Minus_var_i8_I
	.quad	_L_1+2168
	.quad	_Minus__uMinus_var_i8_I
	.quad	_L_1+2183
	.quad	_Minus__Minus_param_i8_i32
	.quad	_L_1+2199
	.quad	_Minus__uMinus_param_i8_i32
	.quad	_L_1+2218
	.quad	_Minus__Minus_var_i8_i32
	.quad	_L_1+2238
	.quad	_Minus__uMinus_var_i8_i32
	.quad	_L_1+2255
	.quad	_Minus__Minus_param_i8_i16
	.quad	_L_1+2273
	.quad	_Minus__uMinus_param_i8_i16
	.quad	_L_1+2292
	.quad	_Minus__Minus_var_i8_i16
	.quad	_L_1+2312
	.quad	_Minus__uMinus_var_i8_i16
	.quad	_L_1+2329
	.quad	_Minus__Minus_param_i8_LC
	.quad	_L_1+2347
	.quad	_Minus__uMinus_param_i8_LC
	.quad	_L_1+2365
	.quad	_Minus__Minus_var_i8_LC
	.quad	_L_1+2384
	.quad	_Minus__uMinus_var_i8_LC
	.quad	_L_1+2400
	.quad	_Minus__Minus_param_i8_u64
	.quad	_L_1+2417
	.quad	_Minus__uMinus_param_i8_u64
	.quad	_L_1+2436
	.quad	_Minus__Minus_var_i8_u64
	.quad	_L_1+2456
	.quad	_Minus__uMinus_var_i8_u64
	.quad	_L_1+2473
	.quad	_Minus__Minus_param_i8_u16
	.quad	_L_1+2491
	.quad	_Minus__uMinus_param_i8_u16
	.quad	_L_1+2510
	.quad	_Minus__Minus_var_i8_u16
	.quad	_L_1+2530
	.quad	_Minus__uMinus_var_i8_u16
	.quad	_L_1+2547
	.quad	_Minus__Minus_param_u8_u32
	.quad	_L_1+2565
	.quad	_Minus__uMinus_param_u8_u32
	.quad	_L_1+2584
	.quad	_Minus__Minus_var_u8_u32
	.quad	_L_1+2604
	.quad	_Minus__uMinus_var_u8_u32
	.quad	_L_1+2621
	.quad	_Minus__Minus_param_u8_L
	.quad	_L_1+2639
	.quad	_Minus__uMinus_param_u8_L
	.quad	_L_1+2656
	.quad	_Minus__Minus_var_u8_L
	.quad	_L_1+2674
	.quad	_Minus__uMinus_var_u8_L
	.quad	_L_1+2689
	.quad	_Minus__Minus_param_u8_i8
	.quad	_L_1+2705
	.quad	_Minus__uMinus_param_u8_i8
	.quad	_L_1+2723
	.quad	_Minus__Minus_var_u8_i8
	.quad	_L_1+2742
	.quad	_Minus__uMinus_var_u8_i8
	.quad	_L_1+2758
	.quad	_Minus__Minus_param_u8_u8
	.quad	_L_1+2775
	.quad	_Minus__uMinus_param_u8_u8
	.quad	_L_1+2793
	.quad	_Minus__Minus_var_u8_u8
	.quad	_L_1+2812
	.quad	_Minus__uMinus_var_u8_u8
	.quad	_L_1+2828
	.quad	_Minus__Minus_param_u8_C
	.quad	_L_1+2845
	.quad	_Minus__uMinus_param_u8_C
	.quad	_L_1+2862
	.quad	_Minus__Minus_var_u8_C
	.quad	_L_1+2880
	.quad	_Minus__uMinus_var_u8_C
	.quad	_L_1+2895
	.quad	_Minus__Minus_param_u8_i64
	.quad	_L_1+2911
	.quad	_Minus__uMinus_param_u8_i64
	.quad	_L_1+2930
	.quad	_Minus__Minus_var_u8_i64
	.quad	_L_1+2950
	.quad	_Minus__uMinus_var_u8_i64
	.quad	_L_1+2967
	.quad	_Minus__Minus_param_u8_I
	.quad	_L_1+2985
	.quad	_Minus__uMinus_param_u8_I
	.quad	_L_1+3002
	.quad	_Minus__Minus_var_u8_I
	.quad	_L_1+3020
	.quad	_Minus__uMinus_var_u8_I
	.quad	_L_1+3035
	.quad	_Minus__Minus_param_u8_i32
	.quad	_L_1+3051
	.quad	_Minus__uMinus_param_u8_i32
	.quad	_L_1+3070
	.quad	_Minus__Minus_var_u8_i32
	.quad	_L_1+3090
	.quad	_Minus__uMinus_var_u8_i32
	.quad	_L_1+3107
	.quad	_Minus__Minus_param_u8_i16
	.quad	_L_1+3125
	.quad	_Minus__uMinus_param_u8_i16
	.quad	_L_1+3144
	.quad	_Minus__Minus_var_u8_i16
	.quad	_L_1+3164
	.quad	_Minus__uMinus_var_u8_i16
	.quad	_L_1+3181
	.quad	_Minus__Minus_param_u8_LC
	.quad	_L_1+3199
	.quad	_Minus__uMinus_param_u8_LC
	.quad	_L_1+3217
	.quad	_Minus__Minus_var_u8_LC
	.quad	_L_1+3236
	.quad	_Minus__uMinus_var_u8_LC
	.quad	_L_1+3252
	.quad	_Minus__Minus_param_u8_u64
	.quad	_L_1+3269
	.quad	_Minus__uMinus_param_u8_u64
	.quad	_L_1+3288
	.quad	_Minus__Minus_var_u8_u64
	.quad	_L_1+3308
	.quad	_Minus__uMinus_var_u8_u64
	.quad	_L_1+3325
	.quad	_Minus__Minus_param_u8_u16
	.quad	_L_1+3343
	.quad	_Minus__uMinus_param_u8_u16
	.quad	_L_1+3362
	.quad	_Minus__Minus_var_u8_u16
	.quad	_L_1+3382
	.quad	_Minus__uMinus_var_u8_u16
	.quad	_L_1+3399
	.quad	_Minus__Minus_param_f32_f32
	.quad	_L_1+3417
	.quad	_Minus__Minus_var_f32_f32
	.quad	_L_1+3437
	.quad	_Minus__Minus_param_C_u32
	.quad	_L_1+3455
	.quad	_Minus__uMinus_param_C_u32
	.quad	_L_1+3473
	.quad	_Minus__Minus_var_C_u32
	.quad	_L_1+3492
	.quad	_Minus__uMinus_var_C_u32
	.quad	_L_1+3508
	.quad	_Minus__Minus_param_C_L
	.quad	_L_1+3525
	.quad	_Minus__uMinus_param_C_L
	.quad	_L_1+3541
	.quad	_Minus__Minus_var_C_L
	.quad	_L_1+3558
	.quad	_Minus__uMinus_var_C_L
	.quad	_L_1+3572
	.quad	_Minus__Minus_param_C_i8
	.quad	_L_1+3587
	.quad	_Minus__uMinus_param_C_i8
	.quad	_L_1+3604
	.quad	_Minus__Minus_var_C_i8
	.quad	_L_1+3622
	.quad	_Minus__uMinus_var_C_i8
	.quad	_L_1+3637
	.quad	_Minus__Minus_param_C_u8
	.quad	_L_1+3653
	.quad	_Minus__uMinus_param_C_u8
	.quad	_L_1+3670
	.quad	_Minus__Minus_var_C_u8
	.quad	_L_1+3688
	.quad	_Minus__uMinus_var_C_u8
	.quad	_L_1+3703
	.quad	_Minus__Minus_param_C_C
	.quad	_L_1+3719
	.quad	_Minus__uMinus_param_C_C
	.quad	_L_1+3735
	.quad	_Minus__Minus_var_C_C
	.quad	_L_1+3752
	.quad	_Minus__uMinus_var_C_C
	.quad	_L_1+3766
	.quad	_Minus__Minus_param_C_i64
	.quad	_L_1+3781
	.quad	_Minus__uMinus_param_C_i64
	.quad	_L_1+3799
	.quad	_Minus__Minus_var_C_i64
	.quad	_L_1+3818
	.quad	_Minus__uMinus_var_C_i64
	.quad	_L_1+3834
	.quad	_Minus__Minus_param_C_I
	.quad	_L_1+3851
	.quad	_Minus__uMinus_param_C_I
	.quad	_L_1+3867
	.quad	_Minus__Minus_var_C_I
	.quad	_L_1+3884
	.quad	_Minus__uMinus_var_C_I
	.quad	_L_1+3898
	.quad	_Minus__Minus_param_C_i32
	.quad	_L_1+3913
	.quad	_Minus__uMinus_param_C_i32
	.quad	_L_1+3931
	.quad	_Minus__Minus_var_C_i32
	.quad	_L_1+3950
	.quad	_Minus__uMinus_var_C_i32
	.quad	_L_1+3966
	.quad	_Minus__Minus_param_C_i16
	.quad	_L_1+3983
	.quad	_Minus__uMinus_param_C_i16
	.quad	_L_1+4001
	.quad	_Minus__Minus_var_C_i16
	.quad	_L_1+4020
	.quad	_Minus__uMinus_var_C_i16
	.quad	_L_1+4036
	.quad	_Minus__Minus_param_C_LC
	.quad	_L_1+4053
	.quad	_Minus__uMinus_param_C_LC
	.quad	_L_1+4070
	.quad	_Minus__Minus_var_C_LC
	.quad	_L_1+4088
	.quad	_Minus__uMinus_var_C_LC
	.quad	_L_1+4103
	.quad	_Minus__Minus_param_C_u64
	.quad	_L_1+4119
	.quad	_Minus__uMinus_param_C_u64
	.quad	_L_1+4137
	.quad	_Minus__Minus_var_C_u64
	.quad	_L_1+4156
	.quad	_Minus__uMinus_var_C_u64
	.quad	_L_1+4172
	.quad	_Minus__Minus_param_C_u16
	.quad	_L_1+4189
	.quad	_Minus__uMinus_param_C_u16
	.quad	_L_1+4207
	.quad	_Minus__Minus_var_C_u16
	.quad	_L_1+4226
	.quad	_Minus__uMinus_var_C_u16
	.quad	_L_1+4242
	.quad	_Minus__Minus_param_i64_u32
	.quad	_L_1+4259
	.quad	_Minus__uMinus_param_i64_u32
	.quad	_L_1+4279
	.quad	_Minus__Minus_var_i64_u32
	.quad	_L_1+4300
	.quad	_Minus__uMinus_var_i64_u32
	.quad	_L_1+4318
	.quad	_Minus__Minus_param_i64_L
	.quad	_L_1+4337
	.quad	_Minus__uMinus_param_i64_L
	.quad	_L_1+4355
	.quad	_Minus__Minus_var_i64_L
	.quad	_L_1+4374
	.quad	_Minus__uMinus_var_i64_L
	.quad	_L_1+4390
	.quad	_Minus__Minus_param_i64_i8
	.quad	_L_1+4407
	.quad	_Minus__uMinus_param_i64_i8
	.quad	_L_1+4426
	.quad	_Minus__Minus_var_i64_i8
	.quad	_L_1+4446
	.quad	_Minus__uMinus_var_i64_i8
	.quad	_L_1+4463
	.quad	_Minus__Minus_param_i64_u8
	.quad	_L_1+4481
	.quad	_Minus__uMinus_param_i64_u8
	.quad	_L_1+4500
	.quad	_Minus__Minus_var_i64_u8
	.quad	_L_1+4520
	.quad	_Minus__uMinus_var_i64_u8
	.quad	_L_1+4537
	.quad	_Minus__Minus_param_i64_C
	.quad	_L_1+4555
	.quad	_Minus__uMinus_param_i64_C
	.quad	_L_1+4573
	.quad	_Minus__Minus_var_i64_C
	.quad	_L_1+4592
	.quad	_Minus__uMinus_var_i64_C
	.quad	_L_1+4608
	.quad	_Minus__Minus_param_i64_i64
	.quad	_L_1+4625
	.quad	_Minus__uMinus_param_i64_i64
	.quad	_L_1+4645
	.quad	_Minus__Minus_var_i64_i64
	.quad	_L_1+4666
	.quad	_Minus__uMinus_var_i64_i64
	.quad	_L_1+4684
	.quad	_Minus__Minus_param_i64_I
	.quad	_L_1+4703
	.quad	_Minus__uMinus_param_i64_I
	.quad	_L_1+4721
	.quad	_Minus__Minus_var_i64_I
	.quad	_L_1+4740
	.quad	_Minus__uMinus_var_i64_I
	.quad	_L_1+4756
	.quad	_Minus__Minus_param_i64_i32
	.quad	_L_1+4773
	.quad	_Minus__uMinus_param_i64_i32
	.quad	_L_1+4793
	.quad	_Minus__Minus_var_i64_i32
	.quad	_L_1+4814
	.quad	_Minus__uMinus_var_i64_i32
	.quad	_L_1+4832
	.quad	_Minus__Minus_param_i64_i16
	.quad	_L_1+4851
	.quad	_Minus__uMinus_param_i64_i16
	.quad	_L_1+4871
	.quad	_Minus__Minus_var_i64_i16
	.quad	_L_1+4892
	.quad	_Minus__uMinus_var_i64_i16
	.quad	_L_1+4910
	.quad	_Minus__Minus_param_i64_LC
	.quad	_L_1+4929
	.quad	_Minus__uMinus_param_i64_LC
	.quad	_L_1+4948
	.quad	_Minus__Minus_var_i64_LC
	.quad	_L_1+4968
	.quad	_Minus__uMinus_var_i64_LC
	.quad	_L_1+4985
	.quad	_Minus__Minus_param_i64_u64
	.quad	_L_1+5003
	.quad	_Minus__uMinus_param_i64_u64
	.quad	_L_1+5023
	.quad	_Minus__Minus_var_i64_u64
	.quad	_L_1+5044
	.quad	_Minus__uMinus_var_i64_u64
	.quad	_L_1+5062
	.quad	_Minus__Minus_param_i64_u16
	.quad	_L_1+5081
	.quad	_Minus__uMinus_param_i64_u16
	.quad	_L_1+5101
	.quad	_Minus__Minus_var_i64_u16
	.quad	_L_1+5122
	.quad	_Minus__uMinus_var_i64_u16
	.quad	_L_1+5140
	.quad	_Minus__Minus_param_I_u32
	.quad	_L_1+5159
	.quad	_Minus__uMinus_param_I_u32
	.quad	_L_1+5177
	.quad	_Minus__Minus_var_I_u32
	.quad	_L_1+5196
	.quad	_Minus__uMinus_var_I_u32
	.quad	_L_1+5212
	.quad	_Minus__Minus_param_I_L
	.quad	_L_1+5229
	.quad	_Minus__uMinus_param_I_L
	.quad	_L_1+5245
	.quad	_Minus__Minus_var_I_L
	.quad	_L_1+5262
	.quad	_Minus__uMinus_var_I_L
	.quad	_L_1+5276
	.quad	_Minus__Minus_param_I_i8
	.quad	_L_1+5291
	.quad	_Minus__uMinus_param_I_i8
	.quad	_L_1+5308
	.quad	_Minus__Minus_var_I_i8
	.quad	_L_1+5326
	.quad	_Minus__uMinus_var_I_i8
	.quad	_L_1+5341
	.quad	_Minus__Minus_param_I_u8
	.quad	_L_1+5357
	.quad	_Minus__uMinus_param_I_u8
	.quad	_L_1+5374
	.quad	_Minus__Minus_var_I_u8
	.quad	_L_1+5392
	.quad	_Minus__uMinus_var_I_u8
	.quad	_L_1+5407
	.quad	_Minus__Minus_param_I_C
	.quad	_L_1+5423
	.quad	_Minus__uMinus_param_I_C
	.quad	_L_1+5439
	.quad	_Minus__Minus_var_I_C
	.quad	_L_1+5456
	.quad	_Minus__uMinus_var_I_C
	.quad	_L_1+5470
	.quad	_Minus__Minus_param_I_i64
	.quad	_L_1+5485
	.quad	_Minus__uMinus_param_I_i64
	.quad	_L_1+5503
	.quad	_Minus__Minus_var_I_i64
	.quad	_L_1+5522
	.quad	_Minus__uMinus_var_I_i64
	.quad	_L_1+5538
	.quad	_Minus__Minus_param_I_I
	.quad	_L_1+5555
	.quad	_Minus__uMinus_param_I_I
	.quad	_L_1+5571
	.quad	_Minus__Minus_var_I_I
	.quad	_L_1+5588
	.quad	_Minus__uMinus_var_I_I
	.quad	_L_1+5602
	.quad	_Minus__Minus_param_I_i32
	.quad	_L_1+5617
	.quad	_Minus__uMinus_param_I_i32
	.quad	_L_1+5635
	.quad	_Minus__Minus_var_I_i32
	.quad	_L_1+5654
	.quad	_Minus__uMinus_var_I_i32
	.quad	_L_1+5670
	.quad	_Minus__Minus_param_I_i16
	.quad	_L_1+5687
	.quad	_Minus__uMinus_param_I_i16
	.quad	_L_1+5705
	.quad	_Minus__Minus_var_I_i16
	.quad	_L_1+5724
	.quad	_Minus__uMinus_var_I_i16
	.quad	_L_1+5740
	.quad	_Minus__Minus_param_I_LC
	.quad	_L_1+5757
	.quad	_Minus__uMinus_param_I_LC
	.quad	_L_1+5774
	.quad	_Minus__Minus_var_I_LC
	.quad	_L_1+5792
	.quad	_Minus__uMinus_var_I_LC
	.quad	_L_1+5807
	.quad	_Minus__Minus_param_I_u64
	.quad	_L_1+5823
	.quad	_Minus__uMinus_param_I_u64
	.quad	_L_1+5841
	.quad	_Minus__Minus_var_I_u64
	.quad	_L_1+5860
	.quad	_Minus__uMinus_var_I_u64
	.quad	_L_1+5876
	.quad	_Minus__Minus_param_I_u16
	.quad	_L_1+5893
	.quad	_Minus__uMinus_param_I_u16
	.quad	_L_1+5911
	.quad	_Minus__Minus_var_I_u16
	.quad	_L_1+5930
	.quad	_Minus__uMinus_var_I_u16
	.quad	_L_1+5946
	.quad	_Minus__Minus_param_i32_u32
	.quad	_L_1+5963
	.quad	_Minus__uMinus_param_i32_u32
	.quad	_L_1+5983
	.quad	_Minus__Minus_var_i32_u32
	.quad	_L_1+6004
	.quad	_Minus__uMinus_var_i32_u32
	.quad	_L_1+6022
	.quad	_Minus__Minus_param_i32_L
	.quad	_L_1+6041
	.quad	_Minus__uMinus_param_i32_L
	.quad	_L_1+6059
	.quad	_Minus__Minus_var_i32_L
	.quad	_L_1+6078
	.quad	_Minus__uMinus_var_i32_L
	.quad	_L_1+6094
	.quad	_Minus__Minus_param_i32_i8
	.quad	_L_1+6111
	.quad	_Minus__uMinus_param_i32_i8
	.quad	_L_1+6130
	.quad	_Minus__Minus_var_i32_i8
	.quad	_L_1+6150
	.quad	_Minus__uMinus_var_i32_i8
	.quad	_L_1+6167
	.quad	_Minus__Minus_param_i32_u8
	.quad	_L_1+6185
	.quad	_Minus__uMinus_param_i32_u8
	.quad	_L_1+6204
	.quad	_Minus__Minus_var_i32_u8
	.quad	_L_1+6224
	.quad	_Minus__uMinus_var_i32_u8
	.quad	_L_1+6241
	.quad	_Minus__Minus_param_i32_C
	.quad	_L_1+6259
	.quad	_Minus__uMinus_param_i32_C
	.quad	_L_1+6277
	.quad	_Minus__Minus_var_i32_C
	.quad	_L_1+6296
	.quad	_Minus__uMinus_var_i32_C
	.quad	_L_1+6312
	.quad	_Minus__Minus_param_i32_i64
	.quad	_L_1+6329
	.quad	_Minus__uMinus_param_i32_i64
	.quad	_L_1+6349
	.quad	_Minus__Minus_var_i32_i64
	.quad	_L_1+6370
	.quad	_Minus__uMinus_var_i32_i64
	.quad	_L_1+6388
	.quad	_Minus__Minus_param_i32_I
	.quad	_L_1+6407
	.quad	_Minus__uMinus_param_i32_I
	.quad	_L_1+6425
	.quad	_Minus__Minus_var_i32_I
	.quad	_L_1+6444
	.quad	_Minus__uMinus_var_i32_I
	.quad	_L_1+6460
	.quad	_Minus__Minus_param_i32_i32
	.quad	_L_1+6477
	.quad	_Minus__uMinus_param_i32_i32
	.quad	_L_1+6497
	.quad	_Minus__Minus_var_i32_i32
	.quad	_L_1+6518
	.quad	_Minus__uMinus_var_i32_i32
	.quad	_L_1+6536
	.quad	_Minus__Minus_param_i32_i16
	.quad	_L_1+6555
	.quad	_Minus__uMinus_param_i32_i16
	.quad	_L_1+6575
	.quad	_Minus__Minus_var_i32_i16
	.quad	_L_1+6596
	.quad	_Minus__uMinus_var_i32_i16
	.quad	_L_1+6614
	.quad	_Minus__Minus_param_i32_LC
	.quad	_L_1+6633
	.quad	_Minus__uMinus_param_i32_LC
	.quad	_L_1+6652
	.quad	_Minus__Minus_var_i32_LC
	.quad	_L_1+6672
	.quad	_Minus__uMinus_var_i32_LC
	.quad	_L_1+6689
	.quad	_Minus__Minus_param_i32_u64
	.quad	_L_1+6707
	.quad	_Minus__uMinus_param_i32_u64
	.quad	_L_1+6727
	.quad	_Minus__Minus_var_i32_u64
	.quad	_L_1+6748
	.quad	_Minus__uMinus_var_i32_u64
	.quad	_L_1+6766
	.quad	_Minus__Minus_param_i32_u16
	.quad	_L_1+6785
	.quad	_Minus__uMinus_param_i32_u16
	.quad	_L_1+6805
	.quad	_Minus__Minus_var_i32_u16
	.quad	_L_1+6826
	.quad	_Minus__uMinus_var_i32_u16
	.quad	_L_1+6844
	.quad	_Minus__Minus_param_i16_u32
	.quad	_L_1+6863
	.quad	_Minus__uMinus_param_i16_u32
	.quad	_L_1+6883
	.quad	_Minus__Minus_var_i16_u32
	.quad	_L_1+6904
	.quad	_Minus__uMinus_var_i16_u32
	.quad	_L_1+6922
	.quad	_Minus__Minus_param_i16_L
	.quad	_L_1+6941
	.quad	_Minus__uMinus_param_i16_L
	.quad	_L_1+6959
	.quad	_Minus__Minus_var_i16_L
	.quad	_L_1+6978
	.quad	_Minus__uMinus_var_i16_L
	.quad	_L_1+6994
	.quad	_Minus__Minus_param_i16_i8
	.quad	_L_1+7011
	.quad	_Minus__uMinus_param_i16_i8
	.quad	_L_1+7030
	.quad	_Minus__Minus_var_i16_i8
	.quad	_L_1+7050
	.quad	_Minus__uMinus_var_i16_i8
	.quad	_L_1+7067
	.quad	_Minus__Minus_param_i16_u8
	.quad	_L_1+7085
	.quad	_Minus__uMinus_param_i16_u8
	.quad	_L_1+7104
	.quad	_Minus__Minus_var_i16_u8
	.quad	_L_1+7124
	.quad	_Minus__uMinus_var_i16_u8
	.quad	_L_1+7141
	.quad	_Minus__Minus_param_i16_C
	.quad	_L_1+7159
	.quad	_Minus__uMinus_param_i16_C
	.quad	_L_1+7177
	.quad	_Minus__Minus_var_i16_C
	.quad	_L_1+7196
	.quad	_Minus__uMinus_var_i16_C
	.quad	_L_1+7212
	.quad	_Minus__Minus_param_i16_i64
	.quad	_L_1+7229
	.quad	_Minus__uMinus_param_i16_i64
	.quad	_L_1+7249
	.quad	_Minus__Minus_var_i16_i64
	.quad	_L_1+7270
	.quad	_Minus__uMinus_var_i16_i64
	.quad	_L_1+7288
	.quad	_Minus__Minus_param_i16_I
	.quad	_L_1+7307
	.quad	_Minus__uMinus_param_i16_I
	.quad	_L_1+7325
	.quad	_Minus__Minus_var_i16_I
	.quad	_L_1+7344
	.quad	_Minus__uMinus_var_i16_I
	.quad	_L_1+7360
	.quad	_Minus__Minus_param_i16_i32
	.quad	_L_1+7377
	.quad	_Minus__uMinus_param_i16_i32
	.quad	_L_1+7397
	.quad	_Minus__Minus_var_i16_i32
	.quad	_L_1+7418
	.quad	_Minus__uMinus_var_i16_i32
	.quad	_L_1+7436
	.quad	_Minus__Minus_param_i16_i16
	.quad	_L_1+7455
	.quad	_Minus__uMinus_param_i16_i16
	.quad	_L_1+7475
	.quad	_Minus__Minus_var_i16_i16
	.quad	_L_1+7496
	.quad	_Minus__uMinus_var_i16_i16
	.quad	_L_1+7514
	.quad	_Minus__Minus_param_i16_LC
	.quad	_L_1+7533
	.quad	_Minus__uMinus_param_i16_LC
	.quad	_L_1+7552
	.quad	_Minus__Minus_var_i16_LC
	.quad	_L_1+7572
	.quad	_Minus__uMinus_var_i16_LC
	.quad	_L_1+7589
	.quad	_Minus__Minus_param_i16_u64
	.quad	_L_1+7607
	.quad	_Minus__uMinus_param_i16_u64
	.quad	_L_1+7627
	.quad	_Minus__Minus_var_i16_u64
	.quad	_L_1+7648
	.quad	_Minus__uMinus_var_i16_u64
	.quad	_L_1+7666
	.quad	_Minus__Minus_param_i16_u16
	.quad	_L_1+7685
	.quad	_Minus__uMinus_param_i16_u16
	.quad	_L_1+7705
	.quad	_Minus__Minus_var_i16_u16
	.quad	_L_1+7726
	.quad	_Minus__uMinus_var_i16_u16
	.quad	_L_1+7744
	.quad	_Minus__Minus_param_f64_f64
	.quad	_L_1+7763
	.quad	_Minus__Minus_var_f64_f64
	.quad	_L_1+7783
	.quad	_Minus__Minus_param_LC_u32
	.quad	_L_1+7801
	.quad	_Minus__uMinus_param_LC_u32
	.quad	_L_1+7820
	.quad	_Minus__Minus_var_LC_u32
	.quad	_L_1+7840
	.quad	_Minus__uMinus_var_LC_u32
	.quad	_L_1+7857
	.quad	_Minus__Minus_param_LC_L
	.quad	_L_1+7875
	.quad	_Minus__uMinus_param_LC_L
	.quad	_L_1+7892
	.quad	_Minus__Minus_var_LC_L
	.quad	_L_1+7910
	.quad	_Minus__uMinus_var_LC_L
	.quad	_L_1+7925
	.quad	_Minus__Minus_param_LC_i8
	.quad	_L_1+7941
	.quad	_Minus__uMinus_param_LC_i8
	.quad	_L_1+7959
	.quad	_Minus__Minus_var_LC_i8
	.quad	_L_1+7978
	.quad	_Minus__uMinus_var_LC_i8
	.quad	_L_1+7994
	.quad	_Minus__Minus_param_LC_u8
	.quad	_L_1+8011
	.quad	_Minus__uMinus_param_LC_u8
	.quad	_L_1+8029
	.quad	_Minus__Minus_var_LC_u8
	.quad	_L_1+8048
	.quad	_Minus__uMinus_var_LC_u8
	.quad	_L_1+8064
	.quad	_Minus__Minus_param_LC_C
	.quad	_L_1+8081
	.quad	_Minus__uMinus_param_LC_C
	.quad	_L_1+8098
	.quad	_Minus__Minus_var_LC_C
	.quad	_L_1+8116
	.quad	_Minus__uMinus_var_LC_C
	.quad	_L_1+8131
	.quad	_Minus__Minus_param_LC_i64
	.quad	_L_1+8147
	.quad	_Minus__uMinus_param_LC_i64
	.quad	_L_1+8166
	.quad	_Minus__Minus_var_LC_i64
	.quad	_L_1+8186
	.quad	_Minus__uMinus_var_LC_i64
	.quad	_L_1+8203
	.quad	_Minus__Minus_param_LC_I
	.quad	_L_1+8221
	.quad	_Minus__uMinus_param_LC_I
	.quad	_L_1+8238
	.quad	_Minus__Minus_var_LC_I
	.quad	_L_1+8256
	.quad	_Minus__uMinus_var_LC_I
	.quad	_L_1+8271
	.quad	_Minus__Minus_param_LC_i32
	.quad	_L_1+8287
	.quad	_Minus__uMinus_param_LC_i32
	.quad	_L_1+8306
	.quad	_Minus__Minus_var_LC_i32
	.quad	_L_1+8326
	.quad	_Minus__uMinus_var_LC_i32
	.quad	_L_1+8343
	.quad	_Minus__Minus_param_LC_i16
	.quad	_L_1+8361
	.quad	_Minus__uMinus_param_LC_i16
	.quad	_L_1+8380
	.quad	_Minus__Minus_var_LC_i16
	.quad	_L_1+8400
	.quad	_Minus__uMinus_var_LC_i16
	.quad	_L_1+8417
	.quad	_Minus__Minus_param_LC_LC
	.quad	_L_1+8435
	.quad	_Minus__uMinus_param_LC_LC
	.quad	_L_1+8453
	.quad	_Minus__Minus_var_LC_LC
	.quad	_L_1+8472
	.quad	_Minus__uMinus_var_LC_LC
	.quad	_L_1+8488
	.quad	_Minus__Minus_param_LC_u64
	.quad	_L_1+8505
	.quad	_Minus__uMinus_param_LC_u64
	.quad	_L_1+8524
	.quad	_Minus__Minus_var_LC_u64
	.quad	_L_1+8544
	.quad	_Minus__uMinus_var_LC_u64
	.quad	_L_1+8561
	.quad	_Minus__Minus_param_LC_u16
	.quad	_L_1+8579
	.quad	_Minus__uMinus_param_LC_u16
	.quad	_L_1+8598
	.quad	_Minus__Minus_var_LC_u16
	.quad	_L_1+8618
	.quad	_Minus__uMinus_var_LC_u16
	.quad	_L_1+8635
	.quad	_Minus__Minus_param_u64_u32
	.quad	_L_1+8653
	.quad	_Minus__uMinus_param_u64_u32
	.quad	_L_1+8673
	.quad	_Minus__Minus_var_u64_u32
	.quad	_L_1+8694
	.quad	_Minus__uMinus_var_u64_u32
	.quad	_L_1+8712
	.quad	_Minus__Minus_param_u64_L
	.quad	_L_1+8731
	.quad	_Minus__uMinus_param_u64_L
	.quad	_L_1+8749
	.quad	_Minus__Minus_var_u64_L
	.quad	_L_1+8768
	.quad	_Minus__uMinus_var_u64_L
	.quad	_L_1+8784
	.quad	_Minus__Minus_param_u64_i8
	.quad	_L_1+8801
	.quad	_Minus__uMinus_param_u64_i8
	.quad	_L_1+8820
	.quad	_Minus__Minus_var_u64_i8
	.quad	_L_1+8840
	.quad	_Minus__uMinus_var_u64_i8
	.quad	_L_1+8857
	.quad	_Minus__Minus_param_u64_u8
	.quad	_L_1+8875
	.quad	_Minus__uMinus_param_u64_u8
	.quad	_L_1+8894
	.quad	_Minus__Minus_var_u64_u8
	.quad	_L_1+8914
	.quad	_Minus__uMinus_var_u64_u8
	.quad	_L_1+8931
	.quad	_Minus__Minus_param_u64_C
	.quad	_L_1+8949
	.quad	_Minus__uMinus_param_u64_C
	.quad	_L_1+8967
	.quad	_Minus__Minus_var_u64_C
	.quad	_L_1+8986
	.quad	_Minus__uMinus_var_u64_C
	.quad	_L_1+9002
	.quad	_Minus__Minus_param_u64_i64
	.quad	_L_1+9019
	.quad	_Minus__uMinus_param_u64_i64
	.quad	_L_1+9039
	.quad	_Minus__Minus_var_u64_i64
	.quad	_L_1+9060
	.quad	_Minus__uMinus_var_u64_i64
	.quad	_L_1+9078
	.quad	_Minus__Minus_param_u64_I
	.quad	_L_1+9097
	.quad	_Minus__uMinus_param_u64_I
	.quad	_L_1+9115
	.quad	_Minus__Minus_var_u64_I
	.quad	_L_1+9134
	.quad	_Minus__uMinus_var_u64_I
	.quad	_L_1+9150
	.quad	_Minus__Minus_param_u64_i32
	.quad	_L_1+9167
	.quad	_Minus__uMinus_param_u64_i32
	.quad	_L_1+9187
	.quad	_Minus__Minus_var_u64_i32
	.quad	_L_1+9208
	.quad	_Minus__uMinus_var_u64_i32
	.quad	_L_1+9226
	.quad	_Minus__Minus_param_u64_i16
	.quad	_L_1+9245
	.quad	_Minus__uMinus_param_u64_i16
	.quad	_L_1+9265
	.quad	_Minus__Minus_var_u64_i16
	.quad	_L_1+9286
	.quad	_Minus__uMinus_var_u64_i16
	.quad	_L_1+9304
	.quad	_Minus__Minus_param_u64_LC
	.quad	_L_1+9323
	.quad	_Minus__uMinus_param_u64_LC
	.quad	_L_1+9342
	.quad	_Minus__Minus_var_u64_LC
	.quad	_L_1+9362
	.quad	_Minus__uMinus_var_u64_LC
	.quad	_L_1+9379
	.quad	_Minus__Minus_param_u64_u64
	.quad	_L_1+9397
	.quad	_Minus__uMinus_param_u64_u64
	.quad	_L_1+9417
	.quad	_Minus__Minus_var_u64_u64
	.quad	_L_1+9438
	.quad	_Minus__uMinus_var_u64_u64
	.quad	_L_1+9456
	.quad	_Minus__Minus_param_u64_u16
	.quad	_L_1+9475
	.quad	_Minus__uMinus_param_u64_u16
	.quad	_L_1+9495
	.quad	_Minus__Minus_var_u64_u16
	.quad	_L_1+9516
	.quad	_Minus__uMinus_var_u64_u16
	.quad	_L_1+9534
	.quad	_Minus__Minus_param_u16_u32
	.quad	_L_1+9553
	.quad	_Minus__uMinus_param_u16_u32
	.quad	_L_1+9573
	.quad	_Minus__Minus_var_u16_u32
	.quad	_L_1+9594
	.quad	_Minus__uMinus_var_u16_u32
	.quad	_L_1+9612
	.quad	_Minus__Minus_param_u16_L
	.quad	_L_1+9631
	.quad	_Minus__uMinus_param_u16_L
	.quad	_L_1+9649
	.quad	_Minus__Minus_var_u16_L
	.quad	_L_1+9668
	.quad	_Minus__uMinus_var_u16_L
	.quad	_L_1+9684
	.quad	_Minus__Minus_param_u16_i8
	.quad	_L_1+9701
	.quad	_Minus__uMinus_param_u16_i8
	.quad	_L_1+9720
	.quad	_Minus__Minus_var_u16_i8
	.quad	_L_1+9740
	.quad	_Minus__uMinus_var_u16_i8
	.quad	_L_1+9757
	.quad	_Minus__Minus_param_u16_u8
	.quad	_L_1+9775
	.quad	_Minus__uMinus_param_u16_u8
	.quad	_L_1+9794
	.quad	_Minus__Minus_var_u16_u8
	.quad	_L_1+9814
	.quad	_Minus__uMinus_var_u16_u8
	.quad	_L_1+9831
	.quad	_Minus__Minus_param_u16_C
	.quad	_L_1+9849
	.quad	_Minus__uMinus_param_u16_C
	.quad	_L_1+9867
	.quad	_Minus__Minus_var_u16_C
	.quad	_L_1+9886
	.quad	_Minus__uMinus_var_u16_C
	.quad	_L_1+9902
	.quad	_Minus__Minus_param_u16_i64
	.quad	_L_1+9919
	.quad	_Minus__uMinus_param_u16_i64
	.quad	_L_1+9939
	.quad	_Minus__Minus_var_u16_i64
	.quad	_L_1+9960
	.quad	_Minus__uMinus_var_u16_i64
	.quad	_L_1+9978
	.quad	_Minus__Minus_param_u16_I
	.quad	_L_1+9997
	.quad	_Minus__uMinus_param_u16_I
	.quad	_L_1+10015
	.quad	_Minus__Minus_var_u16_I
	.quad	_L_1+10034
	.quad	_Minus__uMinus_var_u16_I
	.quad	_L_1+10050
	.quad	_Minus__Minus_param_u16_i32
	.quad	_L_1+10067
	.quad	_Minus__uMinus_param_u16_i32
	.quad	_L_1+10087
	.quad	_Minus__Minus_var_u16_i32
	.quad	_L_1+10108
	.quad	_Minus__uMinus_var_u16_i32
	.quad	_L_1+10126
	.quad	_Minus__Minus_param_u16_i16
	.quad	_L_1+10145
	.quad	_Minus__uMinus_param_u16_i16
	.quad	_L_1+10165
	.quad	_Minus__Minus_var_u16_i16
	.quad	_L_1+10186
	.quad	_Minus__uMinus_var_u16_i16
	.quad	_L_1+10204
	.quad	_Minus__Minus_param_u16_LC
	.quad	_L_1+10223
	.quad	_Minus__uMinus_param_u16_LC
	.quad	_L_1+10242
	.quad	_Minus__Minus_var_u16_LC
	.quad	_L_1+10262
	.quad	_Minus__uMinus_var_u16_LC
	.quad	_L_1+10279
	.quad	_Minus__Minus_param_u16_u64
	.quad	_L_1+10297
	.quad	_Minus__uMinus_param_u16_u64
	.quad	_L_1+10317
	.quad	_Minus__Minus_var_u16_u64
	.quad	_L_1+10338
	.quad	_Minus__uMinus_var_u16_u64
	.quad	_L_1+10356
	.quad	_Minus__Minus_param_u16_u16
	.quad	_L_1+10375
	.quad	_Minus__uMinus_param_u16_u16
	.quad	_L_1+10395
	.quad	_Minus__Minus_var_u16_u16
	.quad	_L_1+10416
	.quad	_Minus__uMinus_var_u16_u16
	.quad	_L_1+10434
	.space 8
	.ascii "../AMD64_DARWIN/Minus.m3"
	.space 8
	.data
	.align 5
_MM_Minus:
	.quad	_L_1+19760
	.space 32
	.quad	_L_1+10456
	.space 24
	.quad	_MM_Minus+208
	.space 8
	.quad	_Minus_M3
	.quad	3
	.word	209
	.space 6
	.quad	210
	.quad	211
	.long	3848290697
	.long	1080723152
	.word	214
	.space 2
	.long	215
	.quad	216
	.quad	217
	.quad	218
	.long	1130051666
	.byte	-35
	.byte	-34
	.space 2
	.quad	223
	.long	224
	.space 28
	.quad	_Minus_I3
	.quad	_MM_Minus+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Minus+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Minus+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Minus+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
