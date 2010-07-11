	.text
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
.globl _Minus__uMinus_var_i64_LC
	.private_extern _Minus__uMinus_var_i64_LC
_Minus__uMinus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
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
	movq	104+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i64_u8
	.private_extern _Minus__uMinus_var_i64_u8
_Minus__uMinus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i64_I
	.private_extern _Minus__uMinus_var_i64_I
_Minus__uMinus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movq	104+_MM_Minus(%rip), %rdx
	movq	128+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i64_i8
	.private_extern _Minus__uMinus_var_i64_i8
_Minus__uMinus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i64_u32
	.private_extern _Minus__uMinus_var_i64_u32
_Minus__uMinus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i64_C
	.private_extern _Minus__uMinus_var_i64_C
_Minus__uMinus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movq	104+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i64_u16
	.private_extern _Minus__uMinus_var_i64_u16
_Minus__uMinus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movq	104+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i64_L
	.private_extern _Minus__uMinus_var_i64_L
_Minus__uMinus_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
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
	movq	104+_MM_Minus(%rip), %rdx
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
.globl _Minus__uMinus_var_i64_i16
	.private_extern _Minus__uMinus_var_i64_i16
_Minus__uMinus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movq	104+_MM_Minus(%rip), %rdx
	movl	188+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_LC_i64
	.private_extern _Minus__uMinus_var_LC_i64
_Minus__uMinus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_LC_u8
	.private_extern _Minus__uMinus_var_LC_u8
_Minus__uMinus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_LC_I
	.private_extern _Minus__uMinus_var_LC_I
_Minus__uMinus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_LC_i8
	.private_extern _Minus__uMinus_var_LC_i8
_Minus__uMinus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_LC_u32
	.private_extern _Minus__uMinus_var_LC_u32
_Minus__uMinus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_LC_C
	.private_extern _Minus__uMinus_var_LC_C
_Minus__uMinus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_LC_u16
	.private_extern _Minus__uMinus_var_LC_u16
_Minus__uMinus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_LC_L
	.private_extern _Minus__uMinus_var_LC_L
_Minus__uMinus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
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
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_LC_i16
	.private_extern _Minus__uMinus_var_LC_i16
_Minus__uMinus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Minus(%rip), %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Minus(%rip), %eax
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
.globl _Minus__Minus_var_f32_f32
	.private_extern _Minus__Minus_var_f32_f32
_Minus__Minus_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	120+_MM_Minus(%rip), %xmm1
	movss	120+_MM_Minus(%rip), %xmm0
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
.globl _Minus__uMinus_var_u8_i64
	.private_extern _Minus__uMinus_var_u8_i64
_Minus__uMinus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u8_LC
	.private_extern _Minus__uMinus_var_u8_LC
_Minus__uMinus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Minus(%rip), %rax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Minus(%rip), %rax
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
	jl	L216
	cmpq	$127, -16(%rbp)
	jle	L217
L216:
	movl	$5153, %edi
	call	__m3_fault
L217:
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
	jl	L222
	cmpq	$127, -16(%rbp)
	jle	L223
L222:
	movl	$5217, %edi
	call	__m3_fault
L223:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_I
	.private_extern _Minus__uMinus_var_u8_I
_Minus__uMinus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L228
	cmpq	$127, -16(%rbp)
	jle	L229
L228:
	movl	$5281, %edi
	call	__m3_fault
L229:
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
	jl	L234
	cmpq	$127, -16(%rbp)
	jle	L235
L234:
	movl	$5345, %edi
	call	__m3_fault
L235:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_i8
	.private_extern _Minus__uMinus_var_u8_i8
_Minus__uMinus_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L240
	movl	$5409, %edi
	call	__m3_fault
L240:
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
	jge	L245
	movl	$5473, %edi
	call	__m3_fault
L245:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_u32
	.private_extern _Minus__uMinus_var_u8_u32
_Minus__uMinus_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L250
	cmpq	$127, -16(%rbp)
	jle	L251
L250:
	movl	$5537, %edi
	call	__m3_fault
L251:
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
	jl	L256
	cmpq	$127, -16(%rbp)
	jle	L257
L256:
	movl	$5601, %edi
	call	__m3_fault
L257:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_C
	.private_extern _Minus__uMinus_var_u8_C
_Minus__uMinus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L262
	cmpq	$127, -16(%rbp)
	jle	L263
L262:
	movl	$5665, %edi
	call	__m3_fault
L263:
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
	jl	L268
	cmpq	$127, -16(%rbp)
	jle	L269
L268:
	movl	$5729, %edi
	call	__m3_fault
L269:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_u16
	.private_extern _Minus__uMinus_var_u8_u16
_Minus__uMinus_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L274
	cmpq	$127, -16(%rbp)
	jle	L275
L274:
	movl	$5793, %edi
	call	__m3_fault
L275:
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
	jl	L280
	cmpq	$127, -16(%rbp)
	jle	L281
L280:
	movl	$5857, %edi
	call	__m3_fault
L281:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_u64
	.private_extern _Minus__uMinus_var_u8_u64
_Minus__uMinus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u8_L
	.private_extern _Minus__uMinus_var_u8_L
_Minus__uMinus_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u8_i16
	.private_extern _Minus__uMinus_var_u8_i16
_Minus__uMinus_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L302
	movl	$6177, %edi
	call	__m3_fault
L302:
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
	jge	L307
	movl	$6241, %edi
	call	__m3_fault
L307:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u8_i32
	.private_extern _Minus__uMinus_var_u8_i32
_Minus__uMinus_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jge	L312
	movl	$6305, %edi
	call	__m3_fault
L312:
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
	jge	L317
	movl	$6369, %edi
	call	__m3_fault
L317:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_I_i64
	.private_extern _Minus__uMinus_var_I_i64
_Minus__uMinus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movq	128+_MM_Minus(%rip), %rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_I_LC
	.private_extern _Minus__uMinus_var_I_LC
_Minus__uMinus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
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
	movq	128+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_I_u8
	.private_extern _Minus__uMinus_var_I_u8
_Minus__uMinus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_I_i8
	.private_extern _Minus__uMinus_var_I_i8
_Minus__uMinus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_I_u32
	.private_extern _Minus__uMinus_var_I_u32
_Minus__uMinus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_I_C
	.private_extern _Minus__uMinus_var_I_C
_Minus__uMinus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movq	128+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_I_u16
	.private_extern _Minus__uMinus_var_I_u16
_Minus__uMinus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movq	128+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_I_L
	.private_extern _Minus__uMinus_var_I_L
_Minus__uMinus_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
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
	movq	128+_MM_Minus(%rip), %rdx
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
.globl _Minus__uMinus_var_I_i16
	.private_extern _Minus__uMinus_var_I_i16
_Minus__uMinus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movq	128+_MM_Minus(%rip), %rdx
	movl	188+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i8_i64
	.private_extern _Minus__uMinus_var_i8_i64
_Minus__uMinus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Minus(%rip), %rax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i8_LC
	.private_extern _Minus__uMinus_var_i8_LC
_Minus__uMinus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Minus(%rip), %rax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i8_u8
	.private_extern _Minus__uMinus_var_i8_u8
_Minus__uMinus_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L434
	cmpq	$255, -16(%rbp)
	jle	L435
L434:
	movl	$8225, %edi
	call	__m3_fault
L435:
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
	js	L440
	cmpq	$255, -16(%rbp)
	jle	L441
L440:
	movl	$8289, %edi
	call	__m3_fault
L441:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_I
	.private_extern _Minus__uMinus_var_i8_I
_Minus__uMinus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Minus(%rip), %rax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L446
	cmpq	$255, -16(%rbp)
	jle	L447
L446:
	movl	$8353, %edi
	call	__m3_fault
L447:
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
	js	L452
	cmpq	$255, -16(%rbp)
	jle	L453
L452:
	movl	$8417, %edi
	call	__m3_fault
L453:
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
	jns	L458
	movl	$8481, %edi
	call	__m3_fault
L458:
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
	jns	L463
	movl	$8545, %edi
	call	__m3_fault
L463:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u32
	.private_extern _Minus__uMinus_var_i8_u32
_Minus__uMinus_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L468
	cmpq	$255, -16(%rbp)
	jle	L469
L468:
	movl	$8609, %edi
	call	__m3_fault
L469:
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
	js	L474
	cmpq	$255, -16(%rbp)
	jle	L475
L474:
	movl	$8673, %edi
	call	__m3_fault
L475:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_C
	.private_extern _Minus__uMinus_var_i8_C
_Minus__uMinus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Minus(%rip), %rax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L480
	movl	$8737, %edi
	call	__m3_fault
L480:
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
	jns	L485
	movl	$8801, %edi
	call	__m3_fault
L485:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u16
	.private_extern _Minus__uMinus_var_i8_u16
_Minus__uMinus_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L490
	cmpq	$255, -16(%rbp)
	jle	L491
L490:
	movl	$8865, %edi
	call	__m3_fault
L491:
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
	js	L496
	cmpq	$255, -16(%rbp)
	jle	L497
L496:
	movl	$8929, %edi
	call	__m3_fault
L497:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_u64
	.private_extern _Minus__uMinus_var_i8_u64
_Minus__uMinus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i8_L
	.private_extern _Minus__uMinus_var_i8_L
_Minus__uMinus_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i8_i16
	.private_extern _Minus__uMinus_var_i8_i16
_Minus__uMinus_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L518
	movl	$9249, %edi
	call	__m3_fault
L518:
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
	jns	L523
	movl	$9313, %edi
	call	__m3_fault
L523:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i8_i32
	.private_extern _Minus__uMinus_var_i8_i32
_Minus__uMinus_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L528
	movl	$9377, %edi
	call	__m3_fault
L528:
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
	jns	L533
	movl	$9441, %edi
	call	__m3_fault
L533:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i64
	.private_extern _Minus__uMinus_var_u32_i64
_Minus__uMinus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u32_LC
	.private_extern _Minus__uMinus_var_u32_LC
_Minus__uMinus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Minus(%rip), %rax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u32_u8
	.private_extern _Minus__uMinus_var_u32_u8
_Minus__uMinus_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L554
	cmpq	$2147483647, -16(%rbp)
	jle	L555
L554:
	movl	$9761, %edi
	call	__m3_fault
L555:
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
	jl	L560
	cmpq	$2147483647, -16(%rbp)
	jle	L561
L560:
	movl	$9825, %edi
	call	__m3_fault
L561:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_I
	.private_extern _Minus__uMinus_var_u32_I
_Minus__uMinus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L566
	cmpq	$2147483647, -16(%rbp)
	jle	L567
L566:
	movl	$9889, %edi
	call	__m3_fault
L567:
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
	jl	L572
	cmpq	$2147483647, -16(%rbp)
	jle	L573
L572:
	movl	$9953, %edi
	call	__m3_fault
L573:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i8
	.private_extern _Minus__uMinus_var_u32_i8
_Minus__uMinus_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L578
	movl	$10017, %edi
	call	__m3_fault
L578:
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
	jge	L583
	movl	$10081, %edi
	call	__m3_fault
L583:
	movq	-16(%rbp), %rax
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
	jl	L588
	cmpq	$2147483647, -16(%rbp)
	jle	L589
L588:
	movl	$10145, %edi
	call	__m3_fault
L589:
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
	jl	L594
	cmpq	$2147483647, -16(%rbp)
	jle	L595
L594:
	movl	$10209, %edi
	call	__m3_fault
L595:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_C
	.private_extern _Minus__uMinus_var_u32_C
_Minus__uMinus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L600
	cmpq	$2147483647, -16(%rbp)
	jle	L601
L600:
	movl	$10273, %edi
	call	__m3_fault
L601:
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
	jl	L606
	cmpq	$2147483647, -16(%rbp)
	jle	L607
L606:
	movl	$10337, %edi
	call	__m3_fault
L607:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_u16
	.private_extern _Minus__uMinus_var_u32_u16
_Minus__uMinus_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L612
	cmpq	$2147483647, -16(%rbp)
	jle	L613
L612:
	movl	$10401, %edi
	call	__m3_fault
L613:
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
	jl	L618
	cmpq	$2147483647, -16(%rbp)
	jle	L619
L618:
	movl	$10465, %edi
	call	__m3_fault
L619:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_u64
	.private_extern _Minus__uMinus_var_u32_u64
_Minus__uMinus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u32_L
	.private_extern _Minus__uMinus_var_u32_L
_Minus__uMinus_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u32_i16
	.private_extern _Minus__uMinus_var_u32_i16
_Minus__uMinus_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L640
	movl	$10785, %edi
	call	__m3_fault
L640:
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
	jge	L645
	movl	$10849, %edi
	call	__m3_fault
L645:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u32_i32
	.private_extern _Minus__uMinus_var_u32_i32
_Minus__uMinus_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jge	L650
	movl	$10913, %edi
	call	__m3_fault
L650:
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
	jge	L655
	movl	$10977, %edi
	call	__m3_fault
L655:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i64
	.private_extern _Minus__uMinus_var_C_i64
_Minus__uMinus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_C_LC
	.private_extern _Minus__uMinus_var_C_LC
_Minus__uMinus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_C_u8
	.private_extern _Minus__uMinus_var_C_u8
_Minus__uMinus_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L676
	movl	$11297, %edi
	call	__m3_fault
L676:
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
	jns	L681
	movl	$11361, %edi
	call	__m3_fault
L681:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_I
	.private_extern _Minus__uMinus_var_C_I
_Minus__uMinus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L686
	movl	$11425, %edi
	call	__m3_fault
L686:
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
	jns	L691
	movl	$11489, %edi
	call	__m3_fault
L691:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i8
	.private_extern _Minus__uMinus_var_C_i8
_Minus__uMinus_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L696
	movl	$11553, %edi
	call	__m3_fault
L696:
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
	jns	L701
	movl	$11617, %edi
	call	__m3_fault
L701:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u32
	.private_extern _Minus__uMinus_var_C_u32
_Minus__uMinus_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L706
	movl	$11681, %edi
	call	__m3_fault
L706:
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
	jns	L711
	movl	$11745, %edi
	call	__m3_fault
L711:
	movq	-16(%rbp), %rax
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
	jns	L716
	movl	$11809, %edi
	call	__m3_fault
L716:
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
	jns	L721
	movl	$11873, %edi
	call	__m3_fault
L721:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u16
	.private_extern _Minus__uMinus_var_C_u16
_Minus__uMinus_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L726
	movl	$11937, %edi
	call	__m3_fault
L726:
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
	jns	L731
	movl	$12001, %edi
	call	__m3_fault
L731:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_u64
	.private_extern _Minus__uMinus_var_C_u64
_Minus__uMinus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_C_L
	.private_extern _Minus__uMinus_var_C_L
_Minus__uMinus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
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
	movq	144+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_C_i16
	.private_extern _Minus__uMinus_var_C_i16
_Minus__uMinus_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L752
	movl	$12321, %edi
	call	__m3_fault
L752:
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
	jns	L757
	movl	$12385, %edi
	call	__m3_fault
L757:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_C_i32
	.private_extern _Minus__uMinus_var_C_i32
_Minus__uMinus_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Minus(%rip), %rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movq	144+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L762
	movl	$12449, %edi
	call	__m3_fault
L762:
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
	jns	L767
	movl	$12513, %edi
	call	__m3_fault
L767:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i64
	.private_extern _Minus__uMinus_var_u16_i64
_Minus__uMinus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u16_LC
	.private_extern _Minus__uMinus_var_u16_LC
_Minus__uMinus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Minus(%rip), %rax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u16_u8
	.private_extern _Minus__uMinus_var_u16_u8
_Minus__uMinus_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L788
	cmpq	$32767, -16(%rbp)
	jle	L789
L788:
	movl	$12833, %edi
	call	__m3_fault
L789:
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
	jl	L794
	cmpq	$32767, -16(%rbp)
	jle	L795
L794:
	movl	$12897, %edi
	call	__m3_fault
L795:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_I
	.private_extern _Minus__uMinus_var_u16_I
_Minus__uMinus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L800
	cmpq	$32767, -16(%rbp)
	jle	L801
L800:
	movl	$12961, %edi
	call	__m3_fault
L801:
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
	jl	L806
	cmpq	$32767, -16(%rbp)
	jle	L807
L806:
	movl	$13025, %edi
	call	__m3_fault
L807:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i8
	.private_extern _Minus__uMinus_var_u16_i8
_Minus__uMinus_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L812
	movl	$13089, %edi
	call	__m3_fault
L812:
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
	jge	L817
	movl	$13153, %edi
	call	__m3_fault
L817:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_u32
	.private_extern _Minus__uMinus_var_u16_u32
_Minus__uMinus_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L822
	cmpq	$32767, -16(%rbp)
	jle	L823
L822:
	movl	$13217, %edi
	call	__m3_fault
L823:
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
	jl	L828
	cmpq	$32767, -16(%rbp)
	jle	L829
L828:
	movl	$13281, %edi
	call	__m3_fault
L829:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_C
	.private_extern _Minus__uMinus_var_u16_C
_Minus__uMinus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L834
	cmpq	$32767, -16(%rbp)
	jle	L835
L834:
	movl	$13345, %edi
	call	__m3_fault
L835:
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
	jl	L840
	cmpq	$32767, -16(%rbp)
	jle	L841
L840:
	movl	$13409, %edi
	call	__m3_fault
L841:
	movq	-16(%rbp), %rax
	leave
	ret
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
	jl	L846
	cmpq	$32767, -16(%rbp)
	jle	L847
L846:
	movl	$13473, %edi
	call	__m3_fault
L847:
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
	jl	L852
	cmpq	$32767, -16(%rbp)
	jle	L853
L852:
	movl	$13537, %edi
	call	__m3_fault
L853:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_u64
	.private_extern _Minus__uMinus_var_u16_u64
_Minus__uMinus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Minus(%rip), %rax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u16_L
	.private_extern _Minus__uMinus_var_u16_L
_Minus__uMinus_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u16_i16
	.private_extern _Minus__uMinus_var_u16_i16
_Minus__uMinus_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L874
	movl	$13857, %edi
	call	__m3_fault
L874:
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
	jge	L879
	movl	$13921, %edi
	call	__m3_fault
L879:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u16_i32
	.private_extern _Minus__uMinus_var_u16_i32
_Minus__uMinus_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jge	L884
	movl	$13985, %edi
	call	__m3_fault
L884:
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
	jge	L889
	movl	$14049, %edi
	call	__m3_fault
L889:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_u64_i64
	.private_extern _Minus__uMinus_var_u64_i64
_Minus__uMinus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movq	160+_MM_Minus(%rip), %rdx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u64_LC
	.private_extern _Minus__uMinus_var_u64_LC
_Minus__uMinus_var_u64_LC:
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
.globl _Minus__Minus_var_u64_LC
	.private_extern _Minus__Minus_var_u64_LC
_Minus__Minus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u64_u8
	.private_extern _Minus__uMinus_var_u64_u8
_Minus__uMinus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u64_I
	.private_extern _Minus__uMinus_var_u64_I
_Minus__uMinus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movq	160+_MM_Minus(%rip), %rdx
	movq	128+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u64_i8
	.private_extern _Minus__uMinus_var_u64_i8
_Minus__uMinus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u64_u32
	.private_extern _Minus__uMinus_var_u64_u32
_Minus__uMinus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rdx
	movl	140+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u64_C
	.private_extern _Minus__uMinus_var_u64_C
_Minus__uMinus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Minus(%rip), %rax
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
	movq	160+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_u64_u16
	.private_extern _Minus__uMinus_var_u64_u16
_Minus__uMinus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_u64_L
	.private_extern _Minus__uMinus_var_u64_L
_Minus__uMinus_var_u64_L:
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
.globl _Minus__Minus_var_u64_L
	.private_extern _Minus__Minus_var_u64_L
_Minus__Minus_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rdx
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
.globl _Minus__uMinus_var_u64_i16
	.private_extern _Minus__uMinus_var_u64_i16
_Minus__uMinus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rdx
	movl	188+_MM_Minus(%rip), %eax
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
.globl _Minus__Minus_var_f64_f64
	.private_extern _Minus__Minus_var_f64_f64
_Minus__Minus_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	168+_MM_Minus(%rip), %xmm1
	movsd	168+_MM_Minus(%rip), %xmm0
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
.globl _Minus__uMinus_var_L_i64
	.private_extern _Minus__uMinus_var_L_i64
_Minus__uMinus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Minus(%rip), %rax
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
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_L_LC
	.private_extern _Minus__uMinus_var_L_LC
_Minus__uMinus_var_L_LC:
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
.globl _Minus__Minus_var_L_LC
	.private_extern _Minus__Minus_var_L_LC
_Minus__Minus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_L_u8
	.private_extern _Minus__uMinus_var_L_u8
_Minus__uMinus_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movzbl	124+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_L_I
	.private_extern _Minus__uMinus_var_L_I
_Minus__uMinus_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Minus(%rip), %rax
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
	movq	128+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_L_i8
	.private_extern _Minus__uMinus_var_L_i8
_Minus__uMinus_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movzbl	136+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_L_u32
	.private_extern _Minus__uMinus_var_L_u32
_Minus__uMinus_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Minus(%rip), %eax
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
	movl	140+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_L_C
	.private_extern _Minus__uMinus_var_L_C
_Minus__uMinus_var_L_C:
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
.globl _Minus__Minus_var_L_C
	.private_extern _Minus__Minus_var_L_C
_Minus__Minus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rdx
	movq	144+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_L_u16
	.private_extern _Minus__uMinus_var_L_u16
_Minus__uMinus_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movzwl	152+_MM_Minus(%rip), %eax
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
	movq	160+_MM_Minus(%rip), %rax
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
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_L_i16
	.private_extern _Minus__uMinus_var_L_i16
_Minus__uMinus_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i16_i64
	.private_extern _Minus__uMinus_var_i16_i64
_Minus__uMinus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Minus(%rip), %rax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i16_LC
	.private_extern _Minus__uMinus_var_i16_LC
_Minus__uMinus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Minus(%rip), %rax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i16_u8
	.private_extern _Minus__uMinus_var_i16_u8
_Minus__uMinus_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1106
	cmpq	$65535, -16(%rbp)
	jle	L1107
L1106:
	movl	$17505, %edi
	call	__m3_fault
L1107:
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
	js	L1112
	cmpq	$65535, -16(%rbp)
	jle	L1113
L1112:
	movl	$17569, %edi
	call	__m3_fault
L1113:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_I
	.private_extern _Minus__uMinus_var_i16_I
_Minus__uMinus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Minus(%rip), %rax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1118
	cmpq	$65535, -16(%rbp)
	jle	L1119
L1118:
	movl	$17633, %edi
	call	__m3_fault
L1119:
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
	js	L1124
	cmpq	$65535, -16(%rbp)
	jle	L1125
L1124:
	movl	$17697, %edi
	call	__m3_fault
L1125:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_i8
	.private_extern _Minus__uMinus_var_i16_i8
_Minus__uMinus_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1130
	movl	$17761, %edi
	call	__m3_fault
L1130:
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
	jns	L1135
	movl	$17825, %edi
	call	__m3_fault
L1135:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_u32
	.private_extern _Minus__uMinus_var_i16_u32
_Minus__uMinus_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1140
	cmpq	$65535, -16(%rbp)
	jle	L1141
L1140:
	movl	$17889, %edi
	call	__m3_fault
L1141:
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
	js	L1146
	cmpq	$65535, -16(%rbp)
	jle	L1147
L1146:
	movl	$17953, %edi
	call	__m3_fault
L1147:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_C
	.private_extern _Minus__uMinus_var_i16_C
_Minus__uMinus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Minus(%rip), %rax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1152
	movl	$18017, %edi
	call	__m3_fault
L1152:
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
	jns	L1157
	movl	$18081, %edi
	call	__m3_fault
L1157:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_u16
	.private_extern _Minus__uMinus_var_i16_u16
_Minus__uMinus_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1162
	cmpq	$65535, -16(%rbp)
	jle	L1163
L1162:
	movl	$18145, %edi
	call	__m3_fault
L1163:
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
	js	L1168
	cmpq	$65535, -16(%rbp)
	jle	L1169
L1168:
	movl	$18209, %edi
	call	__m3_fault
L1169:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_u64
	.private_extern _Minus__uMinus_var_i16_u64
_Minus__uMinus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i16_L
	.private_extern _Minus__uMinus_var_i16_L
_Minus__uMinus_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
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
	jns	L1190
	movl	$18529, %edi
	call	__m3_fault
L1190:
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
	jns	L1195
	movl	$18593, %edi
	call	__m3_fault
L1195:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i16_i32
	.private_extern _Minus__uMinus_var_i16_i32
_Minus__uMinus_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Minus(%rip), %eax
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
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1200
	movl	$18657, %edi
	call	__m3_fault
L1200:
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
	jns	L1205
	movl	$18721, %edi
	call	__m3_fault
L1205:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_i64
	.private_extern _Minus__uMinus_var_i32_i64
_Minus__uMinus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Minus(%rip), %rax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i32_LC
	.private_extern _Minus__uMinus_var_i32_LC
_Minus__uMinus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Minus(%rip), %rax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i32_u8
	.private_extern _Minus__uMinus_var_i32_u8
_Minus__uMinus_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Minus(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1226
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1227
L1226:
	movl	$19041, %edi
	call	__m3_fault
L1227:
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
	js	L1232
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1233
L1232:
	movl	$19105, %edi
	call	__m3_fault
L1233:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_I
	.private_extern _Minus__uMinus_var_i32_I
_Minus__uMinus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Minus(%rip), %rax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1238
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1239
L1238:
	movl	$19169, %edi
	call	__m3_fault
L1239:
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
	js	L1244
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1245
L1244:
	movl	$19233, %edi
	call	__m3_fault
L1245:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_i8
	.private_extern _Minus__uMinus_var_i32_i8
_Minus__uMinus_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1250
	movl	$19297, %edi
	call	__m3_fault
L1250:
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
	jns	L1255
	movl	$19361, %edi
	call	__m3_fault
L1255:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u32
	.private_extern _Minus__uMinus_var_i32_u32
_Minus__uMinus_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1260
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1261
L1260:
	movl	$19425, %edi
	call	__m3_fault
L1261:
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
	js	L1266
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1267
L1266:
	movl	$19489, %edi
	call	__m3_fault
L1267:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_C
	.private_extern _Minus__uMinus_var_i32_C
_Minus__uMinus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Minus(%rip), %rax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1272
	movl	$19553, %edi
	call	__m3_fault
L1272:
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
	jns	L1277
	movl	$19617, %edi
	call	__m3_fault
L1277:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u16
	.private_extern _Minus__uMinus_var_i32_u16
_Minus__uMinus_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Minus(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1282
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1283
L1282:
	movl	$19681, %edi
	call	__m3_fault
L1283:
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
	js	L1288
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1289
L1288:
	movl	$19745, %edi
	call	__m3_fault
L1289:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Minus__uMinus_var_i32_u64
	.private_extern _Minus__uMinus_var_i32_u64
_Minus__uMinus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
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
.globl _Minus__uMinus_var_i32_L
	.private_extern _Minus__uMinus_var_i32_L
_Minus__uMinus_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
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
.globl _Minus__uMinus_var_i32_i16
	.private_extern _Minus__uMinus_var_i32_i16
_Minus__uMinus_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Minus(%rip), %eax
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
	movl	188+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1310
	movl	$20065, %edi
	call	__m3_fault
L1310:
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
	jns	L1315
	movl	$20129, %edi
	call	__m3_fault
L1315:
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
	jns	L1320
	movl	$20193, %edi
	call	__m3_fault
L1320:
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
	jns	L1325
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
	.ascii "Minus_param_i32_L"
	.space 1
	.ascii "uMinus_param_i32_L"
	.space 1
	.ascii "Minus_var_i32_L"
	.space 1
	.ascii "uMinus_var_i32_L"
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
	.ascii "Minus_param_i32_C"
	.space 1
	.ascii "uMinus_param_i32_C"
	.space 1
	.ascii "Minus_var_i32_C"
	.space 1
	.ascii "uMinus_var_i32_C"
	.space 1
	.ascii "Minus_param_i32_u32"
	.space 1
	.ascii "uMinus_param_i32_u32"
	.space 1
	.ascii "Minus_var_i32_u32"
	.space 1
	.ascii "uMinus_var_i32_u32"
	.space 1
	.ascii "Minus_param_i32_i8"
	.space 1
	.ascii "uMinus_param_i32_i8"
	.space 1
	.ascii "Minus_var_i32_i8"
	.space 1
	.ascii "uMinus_var_i32_i8"
	.space 1
	.ascii "Minus_param_i32_I"
	.space 1
	.ascii "uMinus_param_i32_I"
	.space 1
	.ascii "Minus_var_i32_I"
	.space 1
	.ascii "uMinus_var_i32_I"
	.space 1
	.ascii "Minus_param_i32_u8"
	.space 1
	.ascii "uMinus_param_i32_u8"
	.space 1
	.ascii "Minus_var_i32_u8"
	.space 1
	.ascii "uMinus_var_i32_u8"
	.space 1
	.ascii "Minus_param_i32_LC"
	.space 1
	.ascii "uMinus_param_i32_LC"
	.space 1
	.ascii "Minus_var_i32_LC"
	.space 1
	.ascii "uMinus_var_i32_LC"
	.space 1
	.ascii "Minus_param_i32_i64"
	.space 1
	.ascii "uMinus_param_i32_i64"
	.space 1
	.ascii "Minus_var_i32_i64"
	.space 1
	.ascii "uMinus_var_i32_i64"
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
	.ascii "Minus_param_i16_L"
	.space 1
	.ascii "uMinus_param_i16_L"
	.space 1
	.ascii "Minus_var_i16_L"
	.space 1
	.ascii "uMinus_var_i16_L"
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
	.ascii "Minus_param_i16_C"
	.space 1
	.ascii "uMinus_param_i16_C"
	.space 1
	.ascii "Minus_var_i16_C"
	.space 1
	.ascii "uMinus_var_i16_C"
	.space 1
	.ascii "Minus_param_i16_u32"
	.space 1
	.ascii "uMinus_param_i16_u32"
	.space 1
	.ascii "Minus_var_i16_u32"
	.space 1
	.ascii "uMinus_var_i16_u32"
	.space 1
	.ascii "Minus_param_i16_i8"
	.space 1
	.ascii "uMinus_param_i16_i8"
	.space 1
	.ascii "Minus_var_i16_i8"
	.space 1
	.ascii "uMinus_var_i16_i8"
	.space 1
	.ascii "Minus_param_i16_I"
	.space 1
	.ascii "uMinus_param_i16_I"
	.space 1
	.ascii "Minus_var_i16_I"
	.space 1
	.ascii "uMinus_var_i16_I"
	.space 1
	.ascii "Minus_param_i16_u8"
	.space 1
	.ascii "uMinus_param_i16_u8"
	.space 1
	.ascii "Minus_var_i16_u8"
	.space 1
	.ascii "uMinus_var_i16_u8"
	.space 1
	.ascii "Minus_param_i16_LC"
	.space 1
	.ascii "uMinus_param_i16_LC"
	.space 1
	.ascii "Minus_var_i16_LC"
	.space 1
	.ascii "uMinus_var_i16_LC"
	.space 1
	.ascii "Minus_param_i16_i64"
	.space 1
	.ascii "uMinus_param_i16_i64"
	.space 1
	.ascii "Minus_var_i16_i64"
	.space 1
	.ascii "uMinus_var_i16_i64"
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
	.ascii "Minus_param_L_L"
	.space 1
	.ascii "uMinus_param_L_L"
	.space 1
	.ascii "Minus_var_L_L"
	.space 1
	.ascii "uMinus_var_L_L"
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
	.ascii "Minus_param_L_C"
	.space 1
	.ascii "uMinus_param_L_C"
	.space 1
	.ascii "Minus_var_L_C"
	.space 1
	.ascii "uMinus_var_L_C"
	.space 1
	.ascii "Minus_param_L_u32"
	.space 1
	.ascii "uMinus_param_L_u32"
	.space 1
	.ascii "Minus_var_L_u32"
	.space 1
	.ascii "uMinus_var_L_u32"
	.space 1
	.ascii "Minus_param_L_i8"
	.space 1
	.ascii "uMinus_param_L_i8"
	.space 1
	.ascii "Minus_var_L_i8"
	.space 1
	.ascii "uMinus_var_L_i8"
	.space 1
	.ascii "Minus_param_L_I"
	.space 1
	.ascii "uMinus_param_L_I"
	.space 1
	.ascii "Minus_var_L_I"
	.space 1
	.ascii "uMinus_var_L_I"
	.space 1
	.ascii "Minus_param_L_u8"
	.space 1
	.ascii "uMinus_param_L_u8"
	.space 1
	.ascii "Minus_var_L_u8"
	.space 1
	.ascii "uMinus_var_L_u8"
	.space 1
	.ascii "Minus_param_L_LC"
	.space 1
	.ascii "uMinus_param_L_LC"
	.space 1
	.ascii "Minus_var_L_LC"
	.space 1
	.ascii "uMinus_var_L_LC"
	.space 1
	.ascii "Minus_param_L_i64"
	.space 1
	.ascii "uMinus_param_L_i64"
	.space 1
	.ascii "Minus_var_L_i64"
	.space 1
	.ascii "uMinus_var_L_i64"
	.space 1
	.ascii "Minus_param_f64_f64"
	.space 1
	.ascii "Minus_var_f64_f64"
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
	.ascii "Minus_param_u64_L"
	.space 1
	.ascii "uMinus_param_u64_L"
	.space 1
	.ascii "Minus_var_u64_L"
	.space 1
	.ascii "uMinus_var_u64_L"
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
	.ascii "Minus_param_u64_C"
	.space 1
	.ascii "uMinus_param_u64_C"
	.space 1
	.ascii "Minus_var_u64_C"
	.space 1
	.ascii "uMinus_var_u64_C"
	.space 1
	.ascii "Minus_param_u64_u32"
	.space 1
	.ascii "uMinus_param_u64_u32"
	.space 1
	.ascii "Minus_var_u64_u32"
	.space 1
	.ascii "uMinus_var_u64_u32"
	.space 1
	.ascii "Minus_param_u64_i8"
	.space 1
	.ascii "uMinus_param_u64_i8"
	.space 1
	.ascii "Minus_var_u64_i8"
	.space 1
	.ascii "uMinus_var_u64_i8"
	.space 1
	.ascii "Minus_param_u64_I"
	.space 1
	.ascii "uMinus_param_u64_I"
	.space 1
	.ascii "Minus_var_u64_I"
	.space 1
	.ascii "uMinus_var_u64_I"
	.space 1
	.ascii "Minus_param_u64_u8"
	.space 1
	.ascii "uMinus_param_u64_u8"
	.space 1
	.ascii "Minus_var_u64_u8"
	.space 1
	.ascii "uMinus_var_u64_u8"
	.space 1
	.ascii "Minus_param_u64_LC"
	.space 1
	.ascii "uMinus_param_u64_LC"
	.space 1
	.ascii "Minus_var_u64_LC"
	.space 1
	.ascii "uMinus_var_u64_LC"
	.space 1
	.ascii "Minus_param_u64_i64"
	.space 1
	.ascii "uMinus_param_u64_i64"
	.space 1
	.ascii "Minus_var_u64_i64"
	.space 1
	.ascii "uMinus_var_u64_i64"
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
	.ascii "Minus_param_u16_L"
	.space 1
	.ascii "uMinus_param_u16_L"
	.space 1
	.ascii "Minus_var_u16_L"
	.space 1
	.ascii "uMinus_var_u16_L"
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
	.space 1
	.ascii "Minus_param_u16_C"
	.space 1
	.ascii "uMinus_param_u16_C"
	.space 1
	.ascii "Minus_var_u16_C"
	.space 1
	.ascii "uMinus_var_u16_C"
	.space 1
	.ascii "Minus_param_u16_u32"
	.space 1
	.ascii "uMinus_param_u16_u32"
	.space 1
	.ascii "Minus_var_u16_u32"
	.space 1
	.ascii "uMinus_var_u16_u32"
	.space 1
	.ascii "Minus_param_u16_i8"
	.space 1
	.ascii "uMinus_param_u16_i8"
	.space 1
	.ascii "Minus_var_u16_i8"
	.space 1
	.ascii "uMinus_var_u16_i8"
	.space 1
	.ascii "Minus_param_u16_I"
	.space 1
	.ascii "uMinus_param_u16_I"
	.space 1
	.ascii "Minus_var_u16_I"
	.space 1
	.ascii "uMinus_var_u16_I"
	.space 1
	.ascii "Minus_param_u16_u8"
	.space 1
	.ascii "uMinus_param_u16_u8"
	.space 1
	.ascii "Minus_var_u16_u8"
	.space 1
	.ascii "uMinus_var_u16_u8"
	.space 1
	.ascii "Minus_param_u16_LC"
	.space 1
	.ascii "uMinus_param_u16_LC"
	.space 1
	.ascii "Minus_var_u16_LC"
	.space 1
	.ascii "uMinus_var_u16_LC"
	.space 1
	.ascii "Minus_param_u16_i64"
	.space 1
	.ascii "uMinus_param_u16_i64"
	.space 1
	.ascii "Minus_var_u16_i64"
	.space 1
	.ascii "uMinus_var_u16_i64"
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
	.ascii "Minus_param_C_L"
	.space 1
	.ascii "uMinus_param_C_L"
	.space 1
	.ascii "Minus_var_C_L"
	.space 1
	.ascii "uMinus_var_C_L"
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
	.ascii "Minus_param_C_C"
	.space 1
	.ascii "uMinus_param_C_C"
	.space 1
	.ascii "Minus_var_C_C"
	.space 1
	.ascii "uMinus_var_C_C"
	.space 1
	.ascii "Minus_param_C_u32"
	.space 1
	.ascii "uMinus_param_C_u32"
	.space 1
	.ascii "Minus_var_C_u32"
	.space 1
	.ascii "uMinus_var_C_u32"
	.space 1
	.ascii "Minus_param_C_i8"
	.space 1
	.ascii "uMinus_param_C_i8"
	.space 1
	.ascii "Minus_var_C_i8"
	.space 1
	.ascii "uMinus_var_C_i8"
	.space 1
	.ascii "Minus_param_C_I"
	.space 1
	.ascii "uMinus_param_C_I"
	.space 1
	.ascii "Minus_var_C_I"
	.space 1
	.ascii "uMinus_var_C_I"
	.space 1
	.ascii "Minus_param_C_u8"
	.space 1
	.ascii "uMinus_param_C_u8"
	.space 1
	.ascii "Minus_var_C_u8"
	.space 1
	.ascii "uMinus_var_C_u8"
	.space 1
	.ascii "Minus_param_C_LC"
	.space 1
	.ascii "uMinus_param_C_LC"
	.space 1
	.ascii "Minus_var_C_LC"
	.space 1
	.ascii "uMinus_var_C_LC"
	.space 1
	.ascii "Minus_param_C_i64"
	.space 1
	.ascii "uMinus_param_C_i64"
	.space 1
	.ascii "Minus_var_C_i64"
	.space 1
	.ascii "uMinus_var_C_i64"
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
	.ascii "Minus_param_u32_L"
	.space 1
	.ascii "uMinus_param_u32_L"
	.space 1
	.ascii "Minus_var_u32_L"
	.space 1
	.ascii "uMinus_var_u32_L"
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
	.ascii "Minus_param_u32_C"
	.space 1
	.ascii "uMinus_param_u32_C"
	.space 1
	.ascii "Minus_var_u32_C"
	.space 1
	.ascii "uMinus_var_u32_C"
	.space 1
	.ascii "Minus_param_u32_u32"
	.space 1
	.ascii "uMinus_param_u32_u32"
	.space 1
	.ascii "Minus_var_u32_u32"
	.space 1
	.ascii "uMinus_var_u32_u32"
	.space 1
	.ascii "Minus_param_u32_i8"
	.space 1
	.ascii "uMinus_param_u32_i8"
	.space 1
	.ascii "Minus_var_u32_i8"
	.space 1
	.ascii "uMinus_var_u32_i8"
	.space 1
	.ascii "Minus_param_u32_I"
	.space 1
	.ascii "uMinus_param_u32_I"
	.space 1
	.ascii "Minus_var_u32_I"
	.space 1
	.ascii "uMinus_var_u32_I"
	.space 1
	.ascii "Minus_param_u32_u8"
	.space 1
	.ascii "uMinus_param_u32_u8"
	.space 1
	.ascii "Minus_var_u32_u8"
	.space 1
	.ascii "uMinus_var_u32_u8"
	.space 1
	.ascii "Minus_param_u32_LC"
	.space 1
	.ascii "uMinus_param_u32_LC"
	.space 1
	.ascii "Minus_var_u32_LC"
	.space 1
	.ascii "uMinus_var_u32_LC"
	.space 1
	.ascii "Minus_param_u32_i64"
	.space 1
	.ascii "uMinus_param_u32_i64"
	.space 1
	.ascii "Minus_var_u32_i64"
	.space 1
	.ascii "uMinus_var_u32_i64"
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
	.ascii "Minus_param_i8_L"
	.space 1
	.ascii "uMinus_param_i8_L"
	.space 1
	.ascii "Minus_var_i8_L"
	.space 1
	.ascii "uMinus_var_i8_L"
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
	.ascii "Minus_param_i8_C"
	.space 1
	.ascii "uMinus_param_i8_C"
	.space 1
	.ascii "Minus_var_i8_C"
	.space 1
	.ascii "uMinus_var_i8_C"
	.space 1
	.ascii "Minus_param_i8_u32"
	.space 1
	.ascii "uMinus_param_i8_u32"
	.space 1
	.ascii "Minus_var_i8_u32"
	.space 1
	.ascii "uMinus_var_i8_u32"
	.space 1
	.ascii "Minus_param_i8_i8"
	.space 1
	.ascii "uMinus_param_i8_i8"
	.space 1
	.ascii "Minus_var_i8_i8"
	.space 1
	.ascii "uMinus_var_i8_i8"
	.space 1
	.ascii "Minus_param_i8_I"
	.space 1
	.ascii "uMinus_param_i8_I"
	.space 1
	.ascii "Minus_var_i8_I"
	.space 1
	.ascii "uMinus_var_i8_I"
	.space 1
	.ascii "Minus_param_i8_u8"
	.space 1
	.ascii "uMinus_param_i8_u8"
	.space 1
	.ascii "Minus_var_i8_u8"
	.space 1
	.ascii "uMinus_var_i8_u8"
	.space 1
	.ascii "Minus_param_i8_LC"
	.space 1
	.ascii "uMinus_param_i8_LC"
	.space 1
	.ascii "Minus_var_i8_LC"
	.space 1
	.ascii "uMinus_var_i8_LC"
	.space 1
	.ascii "Minus_param_i8_i64"
	.space 1
	.ascii "uMinus_param_i8_i64"
	.space 1
	.ascii "Minus_var_i8_i64"
	.space 1
	.ascii "uMinus_var_i8_i64"
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
	.ascii "Minus_param_I_L"
	.space 1
	.ascii "uMinus_param_I_L"
	.space 1
	.ascii "Minus_var_I_L"
	.space 1
	.ascii "uMinus_var_I_L"
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
	.ascii "Minus_param_I_C"
	.space 1
	.ascii "uMinus_param_I_C"
	.space 1
	.ascii "Minus_var_I_C"
	.space 1
	.ascii "uMinus_var_I_C"
	.space 1
	.ascii "Minus_param_I_u32"
	.space 1
	.ascii "uMinus_param_I_u32"
	.space 1
	.ascii "Minus_var_I_u32"
	.space 1
	.ascii "uMinus_var_I_u32"
	.space 1
	.ascii "Minus_param_I_i8"
	.space 1
	.ascii "uMinus_param_I_i8"
	.space 1
	.ascii "Minus_var_I_i8"
	.space 1
	.ascii "uMinus_var_I_i8"
	.space 1
	.ascii "Minus_param_I_I"
	.space 1
	.ascii "uMinus_param_I_I"
	.space 1
	.ascii "Minus_var_I_I"
	.space 1
	.ascii "uMinus_var_I_I"
	.space 1
	.ascii "Minus_param_I_u8"
	.space 1
	.ascii "uMinus_param_I_u8"
	.space 1
	.ascii "Minus_var_I_u8"
	.space 1
	.ascii "uMinus_var_I_u8"
	.space 1
	.ascii "Minus_param_I_LC"
	.space 1
	.ascii "uMinus_param_I_LC"
	.space 1
	.ascii "Minus_var_I_LC"
	.space 1
	.ascii "uMinus_var_I_LC"
	.space 1
	.ascii "Minus_param_I_i64"
	.space 1
	.ascii "uMinus_param_I_i64"
	.space 1
	.ascii "Minus_var_I_i64"
	.space 1
	.ascii "uMinus_var_I_i64"
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
	.ascii "Minus_param_u8_L"
	.space 1
	.ascii "uMinus_param_u8_L"
	.space 1
	.ascii "Minus_var_u8_L"
	.space 1
	.ascii "uMinus_var_u8_L"
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
	.ascii "Minus_param_u8_C"
	.space 1
	.ascii "uMinus_param_u8_C"
	.space 1
	.ascii "Minus_var_u8_C"
	.space 1
	.ascii "uMinus_var_u8_C"
	.space 1
	.ascii "Minus_param_u8_u32"
	.space 1
	.ascii "uMinus_param_u8_u32"
	.space 1
	.ascii "Minus_var_u8_u32"
	.space 1
	.ascii "uMinus_var_u8_u32"
	.space 1
	.ascii "Minus_param_u8_i8"
	.space 1
	.ascii "uMinus_param_u8_i8"
	.space 1
	.ascii "Minus_var_u8_i8"
	.space 1
	.ascii "uMinus_var_u8_i8"
	.space 1
	.ascii "Minus_param_u8_I"
	.space 1
	.ascii "uMinus_param_u8_I"
	.space 1
	.ascii "Minus_var_u8_I"
	.space 1
	.ascii "uMinus_var_u8_I"
	.space 1
	.ascii "Minus_param_u8_u8"
	.space 1
	.ascii "uMinus_param_u8_u8"
	.space 1
	.ascii "Minus_var_u8_u8"
	.space 1
	.ascii "uMinus_var_u8_u8"
	.space 1
	.ascii "Minus_param_u8_LC"
	.space 1
	.ascii "uMinus_param_u8_LC"
	.space 1
	.ascii "Minus_var_u8_LC"
	.space 1
	.ascii "uMinus_var_u8_LC"
	.space 1
	.ascii "Minus_param_u8_i64"
	.space 1
	.ascii "uMinus_param_u8_i64"
	.space 1
	.ascii "Minus_var_u8_i64"
	.space 1
	.ascii "uMinus_var_u8_i64"
	.space 1
	.ascii "Minus_param_f32_f32"
	.space 1
	.ascii "Minus_var_f32_f32"
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
	.ascii "Minus_param_LC_L"
	.space 1
	.ascii "uMinus_param_LC_L"
	.space 1
	.ascii "Minus_var_LC_L"
	.space 1
	.ascii "uMinus_var_LC_L"
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
	.ascii "Minus_param_LC_C"
	.space 1
	.ascii "uMinus_param_LC_C"
	.space 1
	.ascii "Minus_var_LC_C"
	.space 1
	.ascii "uMinus_var_LC_C"
	.space 1
	.ascii "Minus_param_LC_u32"
	.space 1
	.ascii "uMinus_param_LC_u32"
	.space 1
	.ascii "Minus_var_LC_u32"
	.space 1
	.ascii "uMinus_var_LC_u32"
	.space 1
	.ascii "Minus_param_LC_i8"
	.space 1
	.ascii "uMinus_param_LC_i8"
	.space 1
	.ascii "Minus_var_LC_i8"
	.space 1
	.ascii "uMinus_var_LC_i8"
	.space 1
	.ascii "Minus_param_LC_I"
	.space 1
	.ascii "uMinus_param_LC_I"
	.space 1
	.ascii "Minus_var_LC_I"
	.space 1
	.ascii "uMinus_var_LC_I"
	.space 1
	.ascii "Minus_param_LC_u8"
	.space 1
	.ascii "uMinus_param_LC_u8"
	.space 1
	.ascii "Minus_var_LC_u8"
	.space 1
	.ascii "uMinus_var_LC_u8"
	.space 1
	.ascii "Minus_param_LC_LC"
	.space 1
	.ascii "uMinus_param_LC_LC"
	.space 1
	.ascii "Minus_var_LC_LC"
	.space 1
	.ascii "uMinus_var_LC_LC"
	.space 1
	.ascii "Minus_param_LC_i64"
	.space 1
	.ascii "uMinus_param_LC_i64"
	.space 1
	.ascii "Minus_var_LC_i64"
	.space 1
	.ascii "uMinus_var_LC_i64"
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
	.ascii "Minus_param_i64_L"
	.space 1
	.ascii "uMinus_param_i64_L"
	.space 1
	.ascii "Minus_var_i64_L"
	.space 1
	.ascii "uMinus_var_i64_L"
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
	.ascii "Minus_param_i64_C"
	.space 1
	.ascii "uMinus_param_i64_C"
	.space 1
	.ascii "Minus_var_i64_C"
	.space 1
	.ascii "uMinus_var_i64_C"
	.space 1
	.ascii "Minus_param_i64_u32"
	.space 1
	.ascii "uMinus_param_i64_u32"
	.space 1
	.ascii "Minus_var_i64_u32"
	.space 1
	.ascii "uMinus_var_i64_u32"
	.space 1
	.ascii "Minus_param_i64_i8"
	.space 1
	.ascii "uMinus_param_i64_i8"
	.space 1
	.ascii "Minus_var_i64_i8"
	.space 1
	.ascii "uMinus_var_i64_i8"
	.space 1
	.ascii "Minus_param_i64_I"
	.space 1
	.ascii "uMinus_param_i64_I"
	.space 1
	.ascii "Minus_var_i64_I"
	.space 1
	.ascii "uMinus_var_i64_I"
	.space 1
	.ascii "Minus_param_i64_u8"
	.space 1
	.ascii "uMinus_param_i64_u8"
	.space 1
	.ascii "Minus_var_i64_u8"
	.space 1
	.ascii "uMinus_var_i64_u8"
	.space 1
	.ascii "Minus_param_i64_LC"
	.space 1
	.ascii "uMinus_param_i64_LC"
	.space 1
	.ascii "Minus_var_i64_LC"
	.space 1
	.ascii "uMinus_var_i64_LC"
	.space 1
	.ascii "Minus_param_i64_i64"
	.space 1
	.ascii "uMinus_param_i64_i64"
	.space 1
	.ascii "Minus_var_i64_i64"
	.space 1
	.ascii "uMinus_var_i64_i64"
	.space 4
	.quad	_Minus_M3
	.quad	_L_1
	.quad	_Minus__Minus_param_i32_i32
	.quad	_L_1+9
	.quad	_Minus__uMinus_param_i32_i32
	.quad	_L_1+29
	.quad	_Minus__Minus_var_i32_i32
	.quad	_L_1+50
	.quad	_Minus__uMinus_var_i32_i32
	.quad	_L_1+68
	.quad	_Minus__Minus_param_i32_i16
	.quad	_L_1+87
	.quad	_Minus__uMinus_param_i32_i16
	.quad	_L_1+107
	.quad	_Minus__Minus_var_i32_i16
	.quad	_L_1+128
	.quad	_Minus__uMinus_var_i32_i16
	.quad	_L_1+146
	.quad	_Minus__Minus_param_i32_L
	.quad	_L_1+165
	.quad	_Minus__uMinus_param_i32_L
	.quad	_L_1+183
	.quad	_Minus__Minus_var_i32_L
	.quad	_L_1+202
	.quad	_Minus__uMinus_var_i32_L
	.quad	_L_1+218
	.quad	_Minus__Minus_param_i32_u64
	.quad	_L_1+235
	.quad	_Minus__uMinus_param_i32_u64
	.quad	_L_1+255
	.quad	_Minus__Minus_var_i32_u64
	.quad	_L_1+276
	.quad	_Minus__uMinus_var_i32_u64
	.quad	_L_1+294
	.quad	_Minus__Minus_param_i32_u16
	.quad	_L_1+313
	.quad	_Minus__uMinus_param_i32_u16
	.quad	_L_1+333
	.quad	_Minus__Minus_var_i32_u16
	.quad	_L_1+354
	.quad	_Minus__uMinus_var_i32_u16
	.quad	_L_1+372
	.quad	_Minus__Minus_param_i32_C
	.quad	_L_1+391
	.quad	_Minus__uMinus_param_i32_C
	.quad	_L_1+409
	.quad	_Minus__Minus_var_i32_C
	.quad	_L_1+428
	.quad	_Minus__uMinus_var_i32_C
	.quad	_L_1+444
	.quad	_Minus__Minus_param_i32_u32
	.quad	_L_1+461
	.quad	_Minus__uMinus_param_i32_u32
	.quad	_L_1+481
	.quad	_Minus__Minus_var_i32_u32
	.quad	_L_1+502
	.quad	_Minus__uMinus_var_i32_u32
	.quad	_L_1+520
	.quad	_Minus__Minus_param_i32_i8
	.quad	_L_1+539
	.quad	_Minus__uMinus_param_i32_i8
	.quad	_L_1+558
	.quad	_Minus__Minus_var_i32_i8
	.quad	_L_1+578
	.quad	_Minus__uMinus_var_i32_i8
	.quad	_L_1+595
	.quad	_Minus__Minus_param_i32_I
	.quad	_L_1+613
	.quad	_Minus__uMinus_param_i32_I
	.quad	_L_1+631
	.quad	_Minus__Minus_var_i32_I
	.quad	_L_1+650
	.quad	_Minus__uMinus_var_i32_I
	.quad	_L_1+666
	.quad	_Minus__Minus_param_i32_u8
	.quad	_L_1+683
	.quad	_Minus__uMinus_param_i32_u8
	.quad	_L_1+702
	.quad	_Minus__Minus_var_i32_u8
	.quad	_L_1+722
	.quad	_Minus__uMinus_var_i32_u8
	.quad	_L_1+739
	.quad	_Minus__Minus_param_i32_LC
	.quad	_L_1+757
	.quad	_Minus__uMinus_param_i32_LC
	.quad	_L_1+776
	.quad	_Minus__Minus_var_i32_LC
	.quad	_L_1+796
	.quad	_Minus__uMinus_var_i32_LC
	.quad	_L_1+813
	.quad	_Minus__Minus_param_i32_i64
	.quad	_L_1+831
	.quad	_Minus__uMinus_param_i32_i64
	.quad	_L_1+851
	.quad	_Minus__Minus_var_i32_i64
	.quad	_L_1+872
	.quad	_Minus__uMinus_var_i32_i64
	.quad	_L_1+890
	.quad	_Minus__Minus_param_i16_i32
	.quad	_L_1+909
	.quad	_Minus__uMinus_param_i16_i32
	.quad	_L_1+929
	.quad	_Minus__Minus_var_i16_i32
	.quad	_L_1+950
	.quad	_Minus__uMinus_var_i16_i32
	.quad	_L_1+968
	.quad	_Minus__Minus_param_i16_i16
	.quad	_L_1+987
	.quad	_Minus__uMinus_param_i16_i16
	.quad	_L_1+1007
	.quad	_Minus__Minus_var_i16_i16
	.quad	_L_1+1028
	.quad	_Minus__uMinus_var_i16_i16
	.quad	_L_1+1046
	.quad	_Minus__Minus_param_i16_L
	.quad	_L_1+1065
	.quad	_Minus__uMinus_param_i16_L
	.quad	_L_1+1083
	.quad	_Minus__Minus_var_i16_L
	.quad	_L_1+1102
	.quad	_Minus__uMinus_var_i16_L
	.quad	_L_1+1118
	.quad	_Minus__Minus_param_i16_u64
	.quad	_L_1+1135
	.quad	_Minus__uMinus_param_i16_u64
	.quad	_L_1+1155
	.quad	_Minus__Minus_var_i16_u64
	.quad	_L_1+1176
	.quad	_Minus__uMinus_var_i16_u64
	.quad	_L_1+1194
	.quad	_Minus__Minus_param_i16_u16
	.quad	_L_1+1213
	.quad	_Minus__uMinus_param_i16_u16
	.quad	_L_1+1233
	.quad	_Minus__Minus_var_i16_u16
	.quad	_L_1+1254
	.quad	_Minus__uMinus_var_i16_u16
	.quad	_L_1+1272
	.quad	_Minus__Minus_param_i16_C
	.quad	_L_1+1291
	.quad	_Minus__uMinus_param_i16_C
	.quad	_L_1+1309
	.quad	_Minus__Minus_var_i16_C
	.quad	_L_1+1328
	.quad	_Minus__uMinus_var_i16_C
	.quad	_L_1+1344
	.quad	_Minus__Minus_param_i16_u32
	.quad	_L_1+1361
	.quad	_Minus__uMinus_param_i16_u32
	.quad	_L_1+1381
	.quad	_Minus__Minus_var_i16_u32
	.quad	_L_1+1402
	.quad	_Minus__uMinus_var_i16_u32
	.quad	_L_1+1420
	.quad	_Minus__Minus_param_i16_i8
	.quad	_L_1+1439
	.quad	_Minus__uMinus_param_i16_i8
	.quad	_L_1+1458
	.quad	_Minus__Minus_var_i16_i8
	.quad	_L_1+1478
	.quad	_Minus__uMinus_var_i16_i8
	.quad	_L_1+1495
	.quad	_Minus__Minus_param_i16_I
	.quad	_L_1+1513
	.quad	_Minus__uMinus_param_i16_I
	.quad	_L_1+1531
	.quad	_Minus__Minus_var_i16_I
	.quad	_L_1+1550
	.quad	_Minus__uMinus_var_i16_I
	.quad	_L_1+1566
	.quad	_Minus__Minus_param_i16_u8
	.quad	_L_1+1583
	.quad	_Minus__uMinus_param_i16_u8
	.quad	_L_1+1602
	.quad	_Minus__Minus_var_i16_u8
	.quad	_L_1+1622
	.quad	_Minus__uMinus_var_i16_u8
	.quad	_L_1+1639
	.quad	_Minus__Minus_param_i16_LC
	.quad	_L_1+1657
	.quad	_Minus__uMinus_param_i16_LC
	.quad	_L_1+1676
	.quad	_Minus__Minus_var_i16_LC
	.quad	_L_1+1696
	.quad	_Minus__uMinus_var_i16_LC
	.quad	_L_1+1713
	.quad	_Minus__Minus_param_i16_i64
	.quad	_L_1+1731
	.quad	_Minus__uMinus_param_i16_i64
	.quad	_L_1+1751
	.quad	_Minus__Minus_var_i16_i64
	.quad	_L_1+1772
	.quad	_Minus__uMinus_var_i16_i64
	.quad	_L_1+1790
	.quad	_Minus__Minus_param_L_i32
	.quad	_L_1+1809
	.quad	_Minus__uMinus_param_L_i32
	.quad	_L_1+1827
	.quad	_Minus__Minus_var_L_i32
	.quad	_L_1+1846
	.quad	_Minus__uMinus_var_L_i32
	.quad	_L_1+1862
	.quad	_Minus__Minus_param_L_i16
	.quad	_L_1+1879
	.quad	_Minus__uMinus_param_L_i16
	.quad	_L_1+1897
	.quad	_Minus__Minus_var_L_i16
	.quad	_L_1+1916
	.quad	_Minus__uMinus_var_L_i16
	.quad	_L_1+1932
	.quad	_Minus__Minus_param_L_L
	.quad	_L_1+1949
	.quad	_Minus__uMinus_param_L_L
	.quad	_L_1+1965
	.quad	_Minus__Minus_var_L_L
	.quad	_L_1+1982
	.quad	_Minus__uMinus_var_L_L
	.quad	_L_1+1996
	.quad	_Minus__Minus_param_L_u64
	.quad	_L_1+2011
	.quad	_Minus__uMinus_param_L_u64
	.quad	_L_1+2029
	.quad	_Minus__Minus_var_L_u64
	.quad	_L_1+2048
	.quad	_Minus__uMinus_var_L_u64
	.quad	_L_1+2064
	.quad	_Minus__Minus_param_L_u16
	.quad	_L_1+2081
	.quad	_Minus__uMinus_param_L_u16
	.quad	_L_1+2099
	.quad	_Minus__Minus_var_L_u16
	.quad	_L_1+2118
	.quad	_Minus__uMinus_var_L_u16
	.quad	_L_1+2134
	.quad	_Minus__Minus_param_L_C
	.quad	_L_1+2151
	.quad	_Minus__uMinus_param_L_C
	.quad	_L_1+2167
	.quad	_Minus__Minus_var_L_C
	.quad	_L_1+2184
	.quad	_Minus__uMinus_var_L_C
	.quad	_L_1+2198
	.quad	_Minus__Minus_param_L_u32
	.quad	_L_1+2213
	.quad	_Minus__uMinus_param_L_u32
	.quad	_L_1+2231
	.quad	_Minus__Minus_var_L_u32
	.quad	_L_1+2250
	.quad	_Minus__uMinus_var_L_u32
	.quad	_L_1+2266
	.quad	_Minus__Minus_param_L_i8
	.quad	_L_1+2283
	.quad	_Minus__uMinus_param_L_i8
	.quad	_L_1+2300
	.quad	_Minus__Minus_var_L_i8
	.quad	_L_1+2318
	.quad	_Minus__uMinus_var_L_i8
	.quad	_L_1+2333
	.quad	_Minus__Minus_param_L_I
	.quad	_L_1+2349
	.quad	_Minus__uMinus_param_L_I
	.quad	_L_1+2365
	.quad	_Minus__Minus_var_L_I
	.quad	_L_1+2382
	.quad	_Minus__uMinus_var_L_I
	.quad	_L_1+2396
	.quad	_Minus__Minus_param_L_u8
	.quad	_L_1+2411
	.quad	_Minus__uMinus_param_L_u8
	.quad	_L_1+2428
	.quad	_Minus__Minus_var_L_u8
	.quad	_L_1+2446
	.quad	_Minus__uMinus_var_L_u8
	.quad	_L_1+2461
	.quad	_Minus__Minus_param_L_LC
	.quad	_L_1+2477
	.quad	_Minus__uMinus_param_L_LC
	.quad	_L_1+2494
	.quad	_Minus__Minus_var_L_LC
	.quad	_L_1+2512
	.quad	_Minus__uMinus_var_L_LC
	.quad	_L_1+2527
	.quad	_Minus__Minus_param_L_i64
	.quad	_L_1+2543
	.quad	_Minus__uMinus_param_L_i64
	.quad	_L_1+2561
	.quad	_Minus__Minus_var_L_i64
	.quad	_L_1+2580
	.quad	_Minus__uMinus_var_L_i64
	.quad	_L_1+2596
	.quad	_Minus__Minus_param_f64_f64
	.quad	_L_1+2613
	.quad	_Minus__Minus_var_f64_f64
	.quad	_L_1+2633
	.quad	_Minus__Minus_param_u64_i32
	.quad	_L_1+2651
	.quad	_Minus__uMinus_param_u64_i32
	.quad	_L_1+2671
	.quad	_Minus__Minus_var_u64_i32
	.quad	_L_1+2692
	.quad	_Minus__uMinus_var_u64_i32
	.quad	_L_1+2710
	.quad	_Minus__Minus_param_u64_i16
	.quad	_L_1+2729
	.quad	_Minus__uMinus_param_u64_i16
	.quad	_L_1+2749
	.quad	_Minus__Minus_var_u64_i16
	.quad	_L_1+2770
	.quad	_Minus__uMinus_var_u64_i16
	.quad	_L_1+2788
	.quad	_Minus__Minus_param_u64_L
	.quad	_L_1+2807
	.quad	_Minus__uMinus_param_u64_L
	.quad	_L_1+2825
	.quad	_Minus__Minus_var_u64_L
	.quad	_L_1+2844
	.quad	_Minus__uMinus_var_u64_L
	.quad	_L_1+2860
	.quad	_Minus__Minus_param_u64_u64
	.quad	_L_1+2877
	.quad	_Minus__uMinus_param_u64_u64
	.quad	_L_1+2897
	.quad	_Minus__Minus_var_u64_u64
	.quad	_L_1+2918
	.quad	_Minus__uMinus_var_u64_u64
	.quad	_L_1+2936
	.quad	_Minus__Minus_param_u64_u16
	.quad	_L_1+2955
	.quad	_Minus__uMinus_param_u64_u16
	.quad	_L_1+2975
	.quad	_Minus__Minus_var_u64_u16
	.quad	_L_1+2996
	.quad	_Minus__uMinus_var_u64_u16
	.quad	_L_1+3014
	.quad	_Minus__Minus_param_u64_C
	.quad	_L_1+3033
	.quad	_Minus__uMinus_param_u64_C
	.quad	_L_1+3051
	.quad	_Minus__Minus_var_u64_C
	.quad	_L_1+3070
	.quad	_Minus__uMinus_var_u64_C
	.quad	_L_1+3086
	.quad	_Minus__Minus_param_u64_u32
	.quad	_L_1+3103
	.quad	_Minus__uMinus_param_u64_u32
	.quad	_L_1+3123
	.quad	_Minus__Minus_var_u64_u32
	.quad	_L_1+3144
	.quad	_Minus__uMinus_var_u64_u32
	.quad	_L_1+3162
	.quad	_Minus__Minus_param_u64_i8
	.quad	_L_1+3181
	.quad	_Minus__uMinus_param_u64_i8
	.quad	_L_1+3200
	.quad	_Minus__Minus_var_u64_i8
	.quad	_L_1+3220
	.quad	_Minus__uMinus_var_u64_i8
	.quad	_L_1+3237
	.quad	_Minus__Minus_param_u64_I
	.quad	_L_1+3255
	.quad	_Minus__uMinus_param_u64_I
	.quad	_L_1+3273
	.quad	_Minus__Minus_var_u64_I
	.quad	_L_1+3292
	.quad	_Minus__uMinus_var_u64_I
	.quad	_L_1+3308
	.quad	_Minus__Minus_param_u64_u8
	.quad	_L_1+3325
	.quad	_Minus__uMinus_param_u64_u8
	.quad	_L_1+3344
	.quad	_Minus__Minus_var_u64_u8
	.quad	_L_1+3364
	.quad	_Minus__uMinus_var_u64_u8
	.quad	_L_1+3381
	.quad	_Minus__Minus_param_u64_LC
	.quad	_L_1+3399
	.quad	_Minus__uMinus_param_u64_LC
	.quad	_L_1+3418
	.quad	_Minus__Minus_var_u64_LC
	.quad	_L_1+3438
	.quad	_Minus__uMinus_var_u64_LC
	.quad	_L_1+3455
	.quad	_Minus__Minus_param_u64_i64
	.quad	_L_1+3473
	.quad	_Minus__uMinus_param_u64_i64
	.quad	_L_1+3493
	.quad	_Minus__Minus_var_u64_i64
	.quad	_L_1+3514
	.quad	_Minus__uMinus_var_u64_i64
	.quad	_L_1+3532
	.quad	_Minus__Minus_param_u16_i32
	.quad	_L_1+3551
	.quad	_Minus__uMinus_param_u16_i32
	.quad	_L_1+3571
	.quad	_Minus__Minus_var_u16_i32
	.quad	_L_1+3592
	.quad	_Minus__uMinus_var_u16_i32
	.quad	_L_1+3610
	.quad	_Minus__Minus_param_u16_i16
	.quad	_L_1+3629
	.quad	_Minus__uMinus_param_u16_i16
	.quad	_L_1+3649
	.quad	_Minus__Minus_var_u16_i16
	.quad	_L_1+3670
	.quad	_Minus__uMinus_var_u16_i16
	.quad	_L_1+3688
	.quad	_Minus__Minus_param_u16_L
	.quad	_L_1+3707
	.quad	_Minus__uMinus_param_u16_L
	.quad	_L_1+3725
	.quad	_Minus__Minus_var_u16_L
	.quad	_L_1+3744
	.quad	_Minus__uMinus_var_u16_L
	.quad	_L_1+3760
	.quad	_Minus__Minus_param_u16_u64
	.quad	_L_1+3777
	.quad	_Minus__uMinus_param_u16_u64
	.quad	_L_1+3797
	.quad	_Minus__Minus_var_u16_u64
	.quad	_L_1+3818
	.quad	_Minus__uMinus_var_u16_u64
	.quad	_L_1+3836
	.quad	_Minus__Minus_param_u16_u16
	.quad	_L_1+3855
	.quad	_Minus__uMinus_param_u16_u16
	.quad	_L_1+3875
	.quad	_Minus__Minus_var_u16_u16
	.quad	_L_1+3896
	.quad	_Minus__uMinus_var_u16_u16
	.quad	_L_1+3914
	.quad	_Minus__Minus_param_u16_C
	.quad	_L_1+3933
	.quad	_Minus__uMinus_param_u16_C
	.quad	_L_1+3951
	.quad	_Minus__Minus_var_u16_C
	.quad	_L_1+3970
	.quad	_Minus__uMinus_var_u16_C
	.quad	_L_1+3986
	.quad	_Minus__Minus_param_u16_u32
	.quad	_L_1+4003
	.quad	_Minus__uMinus_param_u16_u32
	.quad	_L_1+4023
	.quad	_Minus__Minus_var_u16_u32
	.quad	_L_1+4044
	.quad	_Minus__uMinus_var_u16_u32
	.quad	_L_1+4062
	.quad	_Minus__Minus_param_u16_i8
	.quad	_L_1+4081
	.quad	_Minus__uMinus_param_u16_i8
	.quad	_L_1+4100
	.quad	_Minus__Minus_var_u16_i8
	.quad	_L_1+4120
	.quad	_Minus__uMinus_var_u16_i8
	.quad	_L_1+4137
	.quad	_Minus__Minus_param_u16_I
	.quad	_L_1+4155
	.quad	_Minus__uMinus_param_u16_I
	.quad	_L_1+4173
	.quad	_Minus__Minus_var_u16_I
	.quad	_L_1+4192
	.quad	_Minus__uMinus_var_u16_I
	.quad	_L_1+4208
	.quad	_Minus__Minus_param_u16_u8
	.quad	_L_1+4225
	.quad	_Minus__uMinus_param_u16_u8
	.quad	_L_1+4244
	.quad	_Minus__Minus_var_u16_u8
	.quad	_L_1+4264
	.quad	_Minus__uMinus_var_u16_u8
	.quad	_L_1+4281
	.quad	_Minus__Minus_param_u16_LC
	.quad	_L_1+4299
	.quad	_Minus__uMinus_param_u16_LC
	.quad	_L_1+4318
	.quad	_Minus__Minus_var_u16_LC
	.quad	_L_1+4338
	.quad	_Minus__uMinus_var_u16_LC
	.quad	_L_1+4355
	.quad	_Minus__Minus_param_u16_i64
	.quad	_L_1+4373
	.quad	_Minus__uMinus_param_u16_i64
	.quad	_L_1+4393
	.quad	_Minus__Minus_var_u16_i64
	.quad	_L_1+4414
	.quad	_Minus__uMinus_var_u16_i64
	.quad	_L_1+4432
	.quad	_Minus__Minus_param_C_i32
	.quad	_L_1+4451
	.quad	_Minus__uMinus_param_C_i32
	.quad	_L_1+4469
	.quad	_Minus__Minus_var_C_i32
	.quad	_L_1+4488
	.quad	_Minus__uMinus_var_C_i32
	.quad	_L_1+4504
	.quad	_Minus__Minus_param_C_i16
	.quad	_L_1+4521
	.quad	_Minus__uMinus_param_C_i16
	.quad	_L_1+4539
	.quad	_Minus__Minus_var_C_i16
	.quad	_L_1+4558
	.quad	_Minus__uMinus_var_C_i16
	.quad	_L_1+4574
	.quad	_Minus__Minus_param_C_L
	.quad	_L_1+4591
	.quad	_Minus__uMinus_param_C_L
	.quad	_L_1+4607
	.quad	_Minus__Minus_var_C_L
	.quad	_L_1+4624
	.quad	_Minus__uMinus_var_C_L
	.quad	_L_1+4638
	.quad	_Minus__Minus_param_C_u64
	.quad	_L_1+4653
	.quad	_Minus__uMinus_param_C_u64
	.quad	_L_1+4671
	.quad	_Minus__Minus_var_C_u64
	.quad	_L_1+4690
	.quad	_Minus__uMinus_var_C_u64
	.quad	_L_1+4706
	.quad	_Minus__Minus_param_C_u16
	.quad	_L_1+4723
	.quad	_Minus__uMinus_param_C_u16
	.quad	_L_1+4741
	.quad	_Minus__Minus_var_C_u16
	.quad	_L_1+4760
	.quad	_Minus__uMinus_var_C_u16
	.quad	_L_1+4776
	.quad	_Minus__Minus_param_C_C
	.quad	_L_1+4793
	.quad	_Minus__uMinus_param_C_C
	.quad	_L_1+4809
	.quad	_Minus__Minus_var_C_C
	.quad	_L_1+4826
	.quad	_Minus__uMinus_var_C_C
	.quad	_L_1+4840
	.quad	_Minus__Minus_param_C_u32
	.quad	_L_1+4855
	.quad	_Minus__uMinus_param_C_u32
	.quad	_L_1+4873
	.quad	_Minus__Minus_var_C_u32
	.quad	_L_1+4892
	.quad	_Minus__uMinus_var_C_u32
	.quad	_L_1+4908
	.quad	_Minus__Minus_param_C_i8
	.quad	_L_1+4925
	.quad	_Minus__uMinus_param_C_i8
	.quad	_L_1+4942
	.quad	_Minus__Minus_var_C_i8
	.quad	_L_1+4960
	.quad	_Minus__uMinus_var_C_i8
	.quad	_L_1+4975
	.quad	_Minus__Minus_param_C_I
	.quad	_L_1+4991
	.quad	_Minus__uMinus_param_C_I
	.quad	_L_1+5007
	.quad	_Minus__Minus_var_C_I
	.quad	_L_1+5024
	.quad	_Minus__uMinus_var_C_I
	.quad	_L_1+5038
	.quad	_Minus__Minus_param_C_u8
	.quad	_L_1+5053
	.quad	_Minus__uMinus_param_C_u8
	.quad	_L_1+5070
	.quad	_Minus__Minus_var_C_u8
	.quad	_L_1+5088
	.quad	_Minus__uMinus_var_C_u8
	.quad	_L_1+5103
	.quad	_Minus__Minus_param_C_LC
	.quad	_L_1+5119
	.quad	_Minus__uMinus_param_C_LC
	.quad	_L_1+5136
	.quad	_Minus__Minus_var_C_LC
	.quad	_L_1+5154
	.quad	_Minus__uMinus_var_C_LC
	.quad	_L_1+5169
	.quad	_Minus__Minus_param_C_i64
	.quad	_L_1+5185
	.quad	_Minus__uMinus_param_C_i64
	.quad	_L_1+5203
	.quad	_Minus__Minus_var_C_i64
	.quad	_L_1+5222
	.quad	_Minus__uMinus_var_C_i64
	.quad	_L_1+5238
	.quad	_Minus__Minus_param_u32_i32
	.quad	_L_1+5255
	.quad	_Minus__uMinus_param_u32_i32
	.quad	_L_1+5275
	.quad	_Minus__Minus_var_u32_i32
	.quad	_L_1+5296
	.quad	_Minus__uMinus_var_u32_i32
	.quad	_L_1+5314
	.quad	_Minus__Minus_param_u32_i16
	.quad	_L_1+5333
	.quad	_Minus__uMinus_param_u32_i16
	.quad	_L_1+5353
	.quad	_Minus__Minus_var_u32_i16
	.quad	_L_1+5374
	.quad	_Minus__uMinus_var_u32_i16
	.quad	_L_1+5392
	.quad	_Minus__Minus_param_u32_L
	.quad	_L_1+5411
	.quad	_Minus__uMinus_param_u32_L
	.quad	_L_1+5429
	.quad	_Minus__Minus_var_u32_L
	.quad	_L_1+5448
	.quad	_Minus__uMinus_var_u32_L
	.quad	_L_1+5464
	.quad	_Minus__Minus_param_u32_u64
	.quad	_L_1+5481
	.quad	_Minus__uMinus_param_u32_u64
	.quad	_L_1+5501
	.quad	_Minus__Minus_var_u32_u64
	.quad	_L_1+5522
	.quad	_Minus__uMinus_var_u32_u64
	.quad	_L_1+5540
	.quad	_Minus__Minus_param_u32_u16
	.quad	_L_1+5559
	.quad	_Minus__uMinus_param_u32_u16
	.quad	_L_1+5579
	.quad	_Minus__Minus_var_u32_u16
	.quad	_L_1+5600
	.quad	_Minus__uMinus_var_u32_u16
	.quad	_L_1+5618
	.quad	_Minus__Minus_param_u32_C
	.quad	_L_1+5637
	.quad	_Minus__uMinus_param_u32_C
	.quad	_L_1+5655
	.quad	_Minus__Minus_var_u32_C
	.quad	_L_1+5674
	.quad	_Minus__uMinus_var_u32_C
	.quad	_L_1+5690
	.quad	_Minus__Minus_param_u32_u32
	.quad	_L_1+5707
	.quad	_Minus__uMinus_param_u32_u32
	.quad	_L_1+5727
	.quad	_Minus__Minus_var_u32_u32
	.quad	_L_1+5748
	.quad	_Minus__uMinus_var_u32_u32
	.quad	_L_1+5766
	.quad	_Minus__Minus_param_u32_i8
	.quad	_L_1+5785
	.quad	_Minus__uMinus_param_u32_i8
	.quad	_L_1+5804
	.quad	_Minus__Minus_var_u32_i8
	.quad	_L_1+5824
	.quad	_Minus__uMinus_var_u32_i8
	.quad	_L_1+5841
	.quad	_Minus__Minus_param_u32_I
	.quad	_L_1+5859
	.quad	_Minus__uMinus_param_u32_I
	.quad	_L_1+5877
	.quad	_Minus__Minus_var_u32_I
	.quad	_L_1+5896
	.quad	_Minus__uMinus_var_u32_I
	.quad	_L_1+5912
	.quad	_Minus__Minus_param_u32_u8
	.quad	_L_1+5929
	.quad	_Minus__uMinus_param_u32_u8
	.quad	_L_1+5948
	.quad	_Minus__Minus_var_u32_u8
	.quad	_L_1+5968
	.quad	_Minus__uMinus_var_u32_u8
	.quad	_L_1+5985
	.quad	_Minus__Minus_param_u32_LC
	.quad	_L_1+6003
	.quad	_Minus__uMinus_param_u32_LC
	.quad	_L_1+6022
	.quad	_Minus__Minus_var_u32_LC
	.quad	_L_1+6042
	.quad	_Minus__uMinus_var_u32_LC
	.quad	_L_1+6059
	.quad	_Minus__Minus_param_u32_i64
	.quad	_L_1+6077
	.quad	_Minus__uMinus_param_u32_i64
	.quad	_L_1+6097
	.quad	_Minus__Minus_var_u32_i64
	.quad	_L_1+6118
	.quad	_Minus__uMinus_var_u32_i64
	.quad	_L_1+6136
	.quad	_Minus__Minus_param_i8_i32
	.quad	_L_1+6155
	.quad	_Minus__uMinus_param_i8_i32
	.quad	_L_1+6174
	.quad	_Minus__Minus_var_i8_i32
	.quad	_L_1+6194
	.quad	_Minus__uMinus_var_i8_i32
	.quad	_L_1+6211
	.quad	_Minus__Minus_param_i8_i16
	.quad	_L_1+6229
	.quad	_Minus__uMinus_param_i8_i16
	.quad	_L_1+6248
	.quad	_Minus__Minus_var_i8_i16
	.quad	_L_1+6268
	.quad	_Minus__uMinus_var_i8_i16
	.quad	_L_1+6285
	.quad	_Minus__Minus_param_i8_L
	.quad	_L_1+6303
	.quad	_Minus__uMinus_param_i8_L
	.quad	_L_1+6320
	.quad	_Minus__Minus_var_i8_L
	.quad	_L_1+6338
	.quad	_Minus__uMinus_var_i8_L
	.quad	_L_1+6353
	.quad	_Minus__Minus_param_i8_u64
	.quad	_L_1+6369
	.quad	_Minus__uMinus_param_i8_u64
	.quad	_L_1+6388
	.quad	_Minus__Minus_var_i8_u64
	.quad	_L_1+6408
	.quad	_Minus__uMinus_var_i8_u64
	.quad	_L_1+6425
	.quad	_Minus__Minus_param_i8_u16
	.quad	_L_1+6443
	.quad	_Minus__uMinus_param_i8_u16
	.quad	_L_1+6462
	.quad	_Minus__Minus_var_i8_u16
	.quad	_L_1+6482
	.quad	_Minus__uMinus_var_i8_u16
	.quad	_L_1+6499
	.quad	_Minus__Minus_param_i8_C
	.quad	_L_1+6517
	.quad	_Minus__uMinus_param_i8_C
	.quad	_L_1+6534
	.quad	_Minus__Minus_var_i8_C
	.quad	_L_1+6552
	.quad	_Minus__uMinus_var_i8_C
	.quad	_L_1+6567
	.quad	_Minus__Minus_param_i8_u32
	.quad	_L_1+6583
	.quad	_Minus__uMinus_param_i8_u32
	.quad	_L_1+6602
	.quad	_Minus__Minus_var_i8_u32
	.quad	_L_1+6622
	.quad	_Minus__uMinus_var_i8_u32
	.quad	_L_1+6639
	.quad	_Minus__Minus_param_i8_i8
	.quad	_L_1+6657
	.quad	_Minus__uMinus_param_i8_i8
	.quad	_L_1+6675
	.quad	_Minus__Minus_var_i8_i8
	.quad	_L_1+6694
	.quad	_Minus__uMinus_var_i8_i8
	.quad	_L_1+6710
	.quad	_Minus__Minus_param_i8_I
	.quad	_L_1+6727
	.quad	_Minus__uMinus_param_i8_I
	.quad	_L_1+6744
	.quad	_Minus__Minus_var_i8_I
	.quad	_L_1+6762
	.quad	_Minus__uMinus_var_i8_I
	.quad	_L_1+6777
	.quad	_Minus__Minus_param_i8_u8
	.quad	_L_1+6793
	.quad	_Minus__uMinus_param_i8_u8
	.quad	_L_1+6811
	.quad	_Minus__Minus_var_i8_u8
	.quad	_L_1+6830
	.quad	_Minus__uMinus_var_i8_u8
	.quad	_L_1+6846
	.quad	_Minus__Minus_param_i8_LC
	.quad	_L_1+6863
	.quad	_Minus__uMinus_param_i8_LC
	.quad	_L_1+6881
	.quad	_Minus__Minus_var_i8_LC
	.quad	_L_1+6900
	.quad	_Minus__uMinus_var_i8_LC
	.quad	_L_1+6916
	.quad	_Minus__Minus_param_i8_i64
	.quad	_L_1+6933
	.quad	_Minus__uMinus_param_i8_i64
	.quad	_L_1+6952
	.quad	_Minus__Minus_var_i8_i64
	.quad	_L_1+6972
	.quad	_Minus__uMinus_var_i8_i64
	.quad	_L_1+6989
	.quad	_Minus__Minus_param_I_i32
	.quad	_L_1+7007
	.quad	_Minus__uMinus_param_I_i32
	.quad	_L_1+7025
	.quad	_Minus__Minus_var_I_i32
	.quad	_L_1+7044
	.quad	_Minus__uMinus_var_I_i32
	.quad	_L_1+7060
	.quad	_Minus__Minus_param_I_i16
	.quad	_L_1+7077
	.quad	_Minus__uMinus_param_I_i16
	.quad	_L_1+7095
	.quad	_Minus__Minus_var_I_i16
	.quad	_L_1+7114
	.quad	_Minus__uMinus_var_I_i16
	.quad	_L_1+7130
	.quad	_Minus__Minus_param_I_L
	.quad	_L_1+7147
	.quad	_Minus__uMinus_param_I_L
	.quad	_L_1+7163
	.quad	_Minus__Minus_var_I_L
	.quad	_L_1+7180
	.quad	_Minus__uMinus_var_I_L
	.quad	_L_1+7194
	.quad	_Minus__Minus_param_I_u64
	.quad	_L_1+7209
	.quad	_Minus__uMinus_param_I_u64
	.quad	_L_1+7227
	.quad	_Minus__Minus_var_I_u64
	.quad	_L_1+7246
	.quad	_Minus__uMinus_var_I_u64
	.quad	_L_1+7262
	.quad	_Minus__Minus_param_I_u16
	.quad	_L_1+7279
	.quad	_Minus__uMinus_param_I_u16
	.quad	_L_1+7297
	.quad	_Minus__Minus_var_I_u16
	.quad	_L_1+7316
	.quad	_Minus__uMinus_var_I_u16
	.quad	_L_1+7332
	.quad	_Minus__Minus_param_I_C
	.quad	_L_1+7349
	.quad	_Minus__uMinus_param_I_C
	.quad	_L_1+7365
	.quad	_Minus__Minus_var_I_C
	.quad	_L_1+7382
	.quad	_Minus__uMinus_var_I_C
	.quad	_L_1+7396
	.quad	_Minus__Minus_param_I_u32
	.quad	_L_1+7411
	.quad	_Minus__uMinus_param_I_u32
	.quad	_L_1+7429
	.quad	_Minus__Minus_var_I_u32
	.quad	_L_1+7448
	.quad	_Minus__uMinus_var_I_u32
	.quad	_L_1+7464
	.quad	_Minus__Minus_param_I_i8
	.quad	_L_1+7481
	.quad	_Minus__uMinus_param_I_i8
	.quad	_L_1+7498
	.quad	_Minus__Minus_var_I_i8
	.quad	_L_1+7516
	.quad	_Minus__uMinus_var_I_i8
	.quad	_L_1+7531
	.quad	_Minus__Minus_param_I_I
	.quad	_L_1+7547
	.quad	_Minus__uMinus_param_I_I
	.quad	_L_1+7563
	.quad	_Minus__Minus_var_I_I
	.quad	_L_1+7580
	.quad	_Minus__uMinus_var_I_I
	.quad	_L_1+7594
	.quad	_Minus__Minus_param_I_u8
	.quad	_L_1+7609
	.quad	_Minus__uMinus_param_I_u8
	.quad	_L_1+7626
	.quad	_Minus__Minus_var_I_u8
	.quad	_L_1+7644
	.quad	_Minus__uMinus_var_I_u8
	.quad	_L_1+7659
	.quad	_Minus__Minus_param_I_LC
	.quad	_L_1+7675
	.quad	_Minus__uMinus_param_I_LC
	.quad	_L_1+7692
	.quad	_Minus__Minus_var_I_LC
	.quad	_L_1+7710
	.quad	_Minus__uMinus_var_I_LC
	.quad	_L_1+7725
	.quad	_Minus__Minus_param_I_i64
	.quad	_L_1+7741
	.quad	_Minus__uMinus_param_I_i64
	.quad	_L_1+7759
	.quad	_Minus__Minus_var_I_i64
	.quad	_L_1+7778
	.quad	_Minus__uMinus_var_I_i64
	.quad	_L_1+7794
	.quad	_Minus__Minus_param_u8_i32
	.quad	_L_1+7811
	.quad	_Minus__uMinus_param_u8_i32
	.quad	_L_1+7830
	.quad	_Minus__Minus_var_u8_i32
	.quad	_L_1+7850
	.quad	_Minus__uMinus_var_u8_i32
	.quad	_L_1+7867
	.quad	_Minus__Minus_param_u8_i16
	.quad	_L_1+7885
	.quad	_Minus__uMinus_param_u8_i16
	.quad	_L_1+7904
	.quad	_Minus__Minus_var_u8_i16
	.quad	_L_1+7924
	.quad	_Minus__uMinus_var_u8_i16
	.quad	_L_1+7941
	.quad	_Minus__Minus_param_u8_L
	.quad	_L_1+7959
	.quad	_Minus__uMinus_param_u8_L
	.quad	_L_1+7976
	.quad	_Minus__Minus_var_u8_L
	.quad	_L_1+7994
	.quad	_Minus__uMinus_var_u8_L
	.quad	_L_1+8009
	.quad	_Minus__Minus_param_u8_u64
	.quad	_L_1+8025
	.quad	_Minus__uMinus_param_u8_u64
	.quad	_L_1+8044
	.quad	_Minus__Minus_var_u8_u64
	.quad	_L_1+8064
	.quad	_Minus__uMinus_var_u8_u64
	.quad	_L_1+8081
	.quad	_Minus__Minus_param_u8_u16
	.quad	_L_1+8099
	.quad	_Minus__uMinus_param_u8_u16
	.quad	_L_1+8118
	.quad	_Minus__Minus_var_u8_u16
	.quad	_L_1+8138
	.quad	_Minus__uMinus_var_u8_u16
	.quad	_L_1+8155
	.quad	_Minus__Minus_param_u8_C
	.quad	_L_1+8173
	.quad	_Minus__uMinus_param_u8_C
	.quad	_L_1+8190
	.quad	_Minus__Minus_var_u8_C
	.quad	_L_1+8208
	.quad	_Minus__uMinus_var_u8_C
	.quad	_L_1+8223
	.quad	_Minus__Minus_param_u8_u32
	.quad	_L_1+8239
	.quad	_Minus__uMinus_param_u8_u32
	.quad	_L_1+8258
	.quad	_Minus__Minus_var_u8_u32
	.quad	_L_1+8278
	.quad	_Minus__uMinus_var_u8_u32
	.quad	_L_1+8295
	.quad	_Minus__Minus_param_u8_i8
	.quad	_L_1+8313
	.quad	_Minus__uMinus_param_u8_i8
	.quad	_L_1+8331
	.quad	_Minus__Minus_var_u8_i8
	.quad	_L_1+8350
	.quad	_Minus__uMinus_var_u8_i8
	.quad	_L_1+8366
	.quad	_Minus__Minus_param_u8_I
	.quad	_L_1+8383
	.quad	_Minus__uMinus_param_u8_I
	.quad	_L_1+8400
	.quad	_Minus__Minus_var_u8_I
	.quad	_L_1+8418
	.quad	_Minus__uMinus_var_u8_I
	.quad	_L_1+8433
	.quad	_Minus__Minus_param_u8_u8
	.quad	_L_1+8449
	.quad	_Minus__uMinus_param_u8_u8
	.quad	_L_1+8467
	.quad	_Minus__Minus_var_u8_u8
	.quad	_L_1+8486
	.quad	_Minus__uMinus_var_u8_u8
	.quad	_L_1+8502
	.quad	_Minus__Minus_param_u8_LC
	.quad	_L_1+8519
	.quad	_Minus__uMinus_param_u8_LC
	.quad	_L_1+8537
	.quad	_Minus__Minus_var_u8_LC
	.quad	_L_1+8556
	.quad	_Minus__uMinus_var_u8_LC
	.quad	_L_1+8572
	.quad	_Minus__Minus_param_u8_i64
	.quad	_L_1+8589
	.quad	_Minus__uMinus_param_u8_i64
	.quad	_L_1+8608
	.quad	_Minus__Minus_var_u8_i64
	.quad	_L_1+8628
	.quad	_Minus__uMinus_var_u8_i64
	.quad	_L_1+8645
	.quad	_Minus__Minus_param_f32_f32
	.quad	_L_1+8663
	.quad	_Minus__Minus_var_f32_f32
	.quad	_L_1+8683
	.quad	_Minus__Minus_param_LC_i32
	.quad	_L_1+8701
	.quad	_Minus__uMinus_param_LC_i32
	.quad	_L_1+8720
	.quad	_Minus__Minus_var_LC_i32
	.quad	_L_1+8740
	.quad	_Minus__uMinus_var_LC_i32
	.quad	_L_1+8757
	.quad	_Minus__Minus_param_LC_i16
	.quad	_L_1+8775
	.quad	_Minus__uMinus_param_LC_i16
	.quad	_L_1+8794
	.quad	_Minus__Minus_var_LC_i16
	.quad	_L_1+8814
	.quad	_Minus__uMinus_var_LC_i16
	.quad	_L_1+8831
	.quad	_Minus__Minus_param_LC_L
	.quad	_L_1+8849
	.quad	_Minus__uMinus_param_LC_L
	.quad	_L_1+8866
	.quad	_Minus__Minus_var_LC_L
	.quad	_L_1+8884
	.quad	_Minus__uMinus_var_LC_L
	.quad	_L_1+8899
	.quad	_Minus__Minus_param_LC_u64
	.quad	_L_1+8915
	.quad	_Minus__uMinus_param_LC_u64
	.quad	_L_1+8934
	.quad	_Minus__Minus_var_LC_u64
	.quad	_L_1+8954
	.quad	_Minus__uMinus_var_LC_u64
	.quad	_L_1+8971
	.quad	_Minus__Minus_param_LC_u16
	.quad	_L_1+8989
	.quad	_Minus__uMinus_param_LC_u16
	.quad	_L_1+9008
	.quad	_Minus__Minus_var_LC_u16
	.quad	_L_1+9028
	.quad	_Minus__uMinus_var_LC_u16
	.quad	_L_1+9045
	.quad	_Minus__Minus_param_LC_C
	.quad	_L_1+9063
	.quad	_Minus__uMinus_param_LC_C
	.quad	_L_1+9080
	.quad	_Minus__Minus_var_LC_C
	.quad	_L_1+9098
	.quad	_Minus__uMinus_var_LC_C
	.quad	_L_1+9113
	.quad	_Minus__Minus_param_LC_u32
	.quad	_L_1+9129
	.quad	_Minus__uMinus_param_LC_u32
	.quad	_L_1+9148
	.quad	_Minus__Minus_var_LC_u32
	.quad	_L_1+9168
	.quad	_Minus__uMinus_var_LC_u32
	.quad	_L_1+9185
	.quad	_Minus__Minus_param_LC_i8
	.quad	_L_1+9203
	.quad	_Minus__uMinus_param_LC_i8
	.quad	_L_1+9221
	.quad	_Minus__Minus_var_LC_i8
	.quad	_L_1+9240
	.quad	_Minus__uMinus_var_LC_i8
	.quad	_L_1+9256
	.quad	_Minus__Minus_param_LC_I
	.quad	_L_1+9273
	.quad	_Minus__uMinus_param_LC_I
	.quad	_L_1+9290
	.quad	_Minus__Minus_var_LC_I
	.quad	_L_1+9308
	.quad	_Minus__uMinus_var_LC_I
	.quad	_L_1+9323
	.quad	_Minus__Minus_param_LC_u8
	.quad	_L_1+9339
	.quad	_Minus__uMinus_param_LC_u8
	.quad	_L_1+9357
	.quad	_Minus__Minus_var_LC_u8
	.quad	_L_1+9376
	.quad	_Minus__uMinus_var_LC_u8
	.quad	_L_1+9392
	.quad	_Minus__Minus_param_LC_LC
	.quad	_L_1+9409
	.quad	_Minus__uMinus_param_LC_LC
	.quad	_L_1+9427
	.quad	_Minus__Minus_var_LC_LC
	.quad	_L_1+9446
	.quad	_Minus__uMinus_var_LC_LC
	.quad	_L_1+9462
	.quad	_Minus__Minus_param_LC_i64
	.quad	_L_1+9479
	.quad	_Minus__uMinus_param_LC_i64
	.quad	_L_1+9498
	.quad	_Minus__Minus_var_LC_i64
	.quad	_L_1+9518
	.quad	_Minus__uMinus_var_LC_i64
	.quad	_L_1+9535
	.quad	_Minus__Minus_param_i64_i32
	.quad	_L_1+9553
	.quad	_Minus__uMinus_param_i64_i32
	.quad	_L_1+9573
	.quad	_Minus__Minus_var_i64_i32
	.quad	_L_1+9594
	.quad	_Minus__uMinus_var_i64_i32
	.quad	_L_1+9612
	.quad	_Minus__Minus_param_i64_i16
	.quad	_L_1+9631
	.quad	_Minus__uMinus_param_i64_i16
	.quad	_L_1+9651
	.quad	_Minus__Minus_var_i64_i16
	.quad	_L_1+9672
	.quad	_Minus__uMinus_var_i64_i16
	.quad	_L_1+9690
	.quad	_Minus__Minus_param_i64_L
	.quad	_L_1+9709
	.quad	_Minus__uMinus_param_i64_L
	.quad	_L_1+9727
	.quad	_Minus__Minus_var_i64_L
	.quad	_L_1+9746
	.quad	_Minus__uMinus_var_i64_L
	.quad	_L_1+9762
	.quad	_Minus__Minus_param_i64_u64
	.quad	_L_1+9779
	.quad	_Minus__uMinus_param_i64_u64
	.quad	_L_1+9799
	.quad	_Minus__Minus_var_i64_u64
	.quad	_L_1+9820
	.quad	_Minus__uMinus_var_i64_u64
	.quad	_L_1+9838
	.quad	_Minus__Minus_param_i64_u16
	.quad	_L_1+9857
	.quad	_Minus__uMinus_param_i64_u16
	.quad	_L_1+9877
	.quad	_Minus__Minus_var_i64_u16
	.quad	_L_1+9898
	.quad	_Minus__uMinus_var_i64_u16
	.quad	_L_1+9916
	.quad	_Minus__Minus_param_i64_C
	.quad	_L_1+9935
	.quad	_Minus__uMinus_param_i64_C
	.quad	_L_1+9953
	.quad	_Minus__Minus_var_i64_C
	.quad	_L_1+9972
	.quad	_Minus__uMinus_var_i64_C
	.quad	_L_1+9988
	.quad	_Minus__Minus_param_i64_u32
	.quad	_L_1+10005
	.quad	_Minus__uMinus_param_i64_u32
	.quad	_L_1+10025
	.quad	_Minus__Minus_var_i64_u32
	.quad	_L_1+10046
	.quad	_Minus__uMinus_var_i64_u32
	.quad	_L_1+10064
	.quad	_Minus__Minus_param_i64_i8
	.quad	_L_1+10083
	.quad	_Minus__uMinus_param_i64_i8
	.quad	_L_1+10102
	.quad	_Minus__Minus_var_i64_i8
	.quad	_L_1+10122
	.quad	_Minus__uMinus_var_i64_i8
	.quad	_L_1+10139
	.quad	_Minus__Minus_param_i64_I
	.quad	_L_1+10157
	.quad	_Minus__uMinus_param_i64_I
	.quad	_L_1+10175
	.quad	_Minus__Minus_var_i64_I
	.quad	_L_1+10194
	.quad	_Minus__uMinus_var_i64_I
	.quad	_L_1+10210
	.quad	_Minus__Minus_param_i64_u8
	.quad	_L_1+10227
	.quad	_Minus__uMinus_param_i64_u8
	.quad	_L_1+10246
	.quad	_Minus__Minus_var_i64_u8
	.quad	_L_1+10266
	.quad	_Minus__uMinus_var_i64_u8
	.quad	_L_1+10283
	.quad	_Minus__Minus_param_i64_LC
	.quad	_L_1+10301
	.quad	_Minus__uMinus_param_i64_LC
	.quad	_L_1+10320
	.quad	_Minus__Minus_var_i64_LC
	.quad	_L_1+10340
	.quad	_Minus__uMinus_var_i64_LC
	.quad	_L_1+10357
	.quad	_Minus__Minus_param_i64_i64
	.quad	_L_1+10375
	.quad	_Minus__uMinus_param_i64_i64
	.quad	_L_1+10395
	.quad	_Minus__Minus_var_i64_i64
	.quad	_L_1+10416
	.quad	_Minus__uMinus_var_i64_i64
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
	.quad	209
	.quad	210
	.long	1129526854
	.byte	-43
	.space 3
	.quad	214
	.byte	-41
	.space 3
	.long	216
	.quad	217
	.word	218
	.space 6
	.quad	219
	.long	1855425872
	.long	1080788754
	.quad	222
	.word	223
	.space 2
	.long	224
	.space 24
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
