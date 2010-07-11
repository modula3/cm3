	.text
.globl _Times__uTimes_var_i64_i64
	.private_extern _Times__uTimes_var_i64_i64
_Times__uTimes_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i64
	.private_extern _Times__Times_var_i64_i64
_Times__Times_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i64
	.private_extern _Times__uTimes_param_i64_i64
_Times__uTimes_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i64
	.private_extern _Times__Times_param_i64_i64
_Times__Times_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_LC
	.private_extern _Times__uTimes_var_i64_LC
_Times__uTimes_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_LC
	.private_extern _Times__Times_var_i64_LC
_Times__Times_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_LC
	.private_extern _Times__uTimes_param_i64_LC
_Times__uTimes_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i64_LC
	.private_extern _Times__Times_param_i64_LC
_Times__Times_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u8
	.private_extern _Times__uTimes_var_i64_u8
_Times__uTimes_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u8
	.private_extern _Times__Times_var_i64_u8
_Times__Times_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u8
	.private_extern _Times__uTimes_param_i64_u8
_Times__uTimes_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u8
	.private_extern _Times__Times_param_i64_u8
_Times__Times_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_I
	.private_extern _Times__uTimes_var_i64_I
_Times__uTimes_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_I
	.private_extern _Times__Times_var_i64_I
_Times__Times_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_I
	.private_extern _Times__uTimes_param_i64_I
_Times__uTimes_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_I
	.private_extern _Times__Times_param_i64_I
_Times__Times_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i8
	.private_extern _Times__uTimes_var_i64_i8
_Times__uTimes_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i8
	.private_extern _Times__Times_var_i64_i8
_Times__Times_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i8
	.private_extern _Times__uTimes_param_i64_i8
_Times__uTimes_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i8
	.private_extern _Times__Times_param_i64_i8
_Times__Times_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u32
	.private_extern _Times__uTimes_var_i64_u32
_Times__uTimes_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u32
	.private_extern _Times__Times_var_i64_u32
_Times__Times_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u32
	.private_extern _Times__uTimes_param_i64_u32
_Times__uTimes_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u32
	.private_extern _Times__Times_param_i64_u32
_Times__Times_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_C
	.private_extern _Times__uTimes_var_i64_C
_Times__uTimes_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_C
	.private_extern _Times__Times_var_i64_C
_Times__Times_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_C
	.private_extern _Times__uTimes_param_i64_C
_Times__uTimes_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i64_C
	.private_extern _Times__Times_param_i64_C
_Times__Times_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u16
	.private_extern _Times__uTimes_var_i64_u16
_Times__uTimes_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u16
	.private_extern _Times__Times_var_i64_u16
_Times__Times_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u16
	.private_extern _Times__uTimes_param_i64_u16
_Times__uTimes_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u16
	.private_extern _Times__Times_param_i64_u16
_Times__Times_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u64
	.private_extern _Times__uTimes_var_i64_u64
_Times__uTimes_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u64
	.private_extern _Times__Times_var_i64_u64
_Times__Times_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u64
	.private_extern _Times__uTimes_param_i64_u64
_Times__uTimes_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u64
	.private_extern _Times__Times_param_i64_u64
_Times__Times_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_L
	.private_extern _Times__uTimes_var_i64_L
_Times__uTimes_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_L
	.private_extern _Times__Times_var_i64_L
_Times__Times_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_L
	.private_extern _Times__uTimes_param_i64_L
_Times__uTimes_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_L
	.private_extern _Times__Times_param_i64_L
_Times__Times_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i16
	.private_extern _Times__uTimes_var_i64_i16
_Times__uTimes_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i16
	.private_extern _Times__Times_var_i64_i16
_Times__Times_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i16
	.private_extern _Times__uTimes_param_i64_i16
_Times__uTimes_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i16
	.private_extern _Times__Times_param_i64_i16
_Times__Times_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i32
	.private_extern _Times__uTimes_var_i64_i32
_Times__uTimes_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i32
	.private_extern _Times__Times_var_i64_i32
_Times__Times_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i32
	.private_extern _Times__uTimes_param_i64_i32
_Times__uTimes_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i32
	.private_extern _Times__Times_param_i64_i32
_Times__Times_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i64
	.private_extern _Times__uTimes_var_LC_i64
_Times__uTimes_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i64
	.private_extern _Times__Times_var_LC_i64
_Times__Times_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i64
	.private_extern _Times__uTimes_param_LC_i64
_Times__uTimes_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i64
	.private_extern _Times__Times_param_LC_i64
_Times__Times_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_LC
	.private_extern _Times__uTimes_var_LC_LC
_Times__uTimes_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_LC
	.private_extern _Times__Times_var_LC_LC
_Times__Times_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_LC
	.private_extern _Times__uTimes_param_LC_LC
_Times__uTimes_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_LC
	.private_extern _Times__Times_param_LC_LC
_Times__Times_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u8
	.private_extern _Times__uTimes_var_LC_u8
_Times__uTimes_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u8
	.private_extern _Times__Times_var_LC_u8
_Times__Times_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u8
	.private_extern _Times__uTimes_param_LC_u8
_Times__uTimes_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u8
	.private_extern _Times__Times_param_LC_u8
_Times__Times_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_I
	.private_extern _Times__uTimes_var_LC_I
_Times__uTimes_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_I
	.private_extern _Times__Times_var_LC_I
_Times__Times_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_I
	.private_extern _Times__uTimes_param_LC_I
_Times__uTimes_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_I
	.private_extern _Times__Times_param_LC_I
_Times__Times_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i8
	.private_extern _Times__uTimes_var_LC_i8
_Times__uTimes_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i8
	.private_extern _Times__Times_var_LC_i8
_Times__Times_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i8
	.private_extern _Times__uTimes_param_LC_i8
_Times__uTimes_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i8
	.private_extern _Times__Times_param_LC_i8
_Times__Times_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u32
	.private_extern _Times__uTimes_var_LC_u32
_Times__uTimes_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u32
	.private_extern _Times__Times_var_LC_u32
_Times__Times_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u32
	.private_extern _Times__uTimes_param_LC_u32
_Times__uTimes_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u32
	.private_extern _Times__Times_param_LC_u32
_Times__Times_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_C
	.private_extern _Times__uTimes_var_LC_C
_Times__uTimes_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_C
	.private_extern _Times__Times_var_LC_C
_Times__Times_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_C
	.private_extern _Times__uTimes_param_LC_C
_Times__uTimes_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_C
	.private_extern _Times__Times_param_LC_C
_Times__Times_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u16
	.private_extern _Times__uTimes_var_LC_u16
_Times__uTimes_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u16
	.private_extern _Times__Times_var_LC_u16
_Times__Times_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u16
	.private_extern _Times__uTimes_param_LC_u16
_Times__uTimes_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u16
	.private_extern _Times__Times_param_LC_u16
_Times__Times_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u64
	.private_extern _Times__uTimes_var_LC_u64
_Times__uTimes_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u64
	.private_extern _Times__Times_var_LC_u64
_Times__Times_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u64
	.private_extern _Times__uTimes_param_LC_u64
_Times__uTimes_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u64
	.private_extern _Times__Times_param_LC_u64
_Times__Times_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_L
	.private_extern _Times__uTimes_var_LC_L
_Times__uTimes_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_L
	.private_extern _Times__Times_var_LC_L
_Times__Times_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_L
	.private_extern _Times__uTimes_param_LC_L
_Times__uTimes_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_L
	.private_extern _Times__Times_param_LC_L
_Times__Times_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i16
	.private_extern _Times__uTimes_var_LC_i16
_Times__uTimes_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i16
	.private_extern _Times__Times_var_LC_i16
_Times__Times_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i16
	.private_extern _Times__uTimes_param_LC_i16
_Times__uTimes_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i16
	.private_extern _Times__Times_param_LC_i16
_Times__Times_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i32
	.private_extern _Times__uTimes_var_LC_i32
_Times__uTimes_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i32
	.private_extern _Times__Times_var_LC_i32
_Times__Times_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i32
	.private_extern _Times__uTimes_param_LC_i32
_Times__uTimes_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i32
	.private_extern _Times__Times_param_LC_i32
_Times__Times_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_f32_f32
	.private_extern _Times__Times_var_f32_f32
_Times__Times_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	120+_MM_Times(%rip), %xmm1
	movss	120+_MM_Times(%rip), %xmm0
	mulss	%xmm1, %xmm0
	leave
	ret
.globl _Times__Times_param_f32_f32
	.private_extern _Times__Times_param_f32_f32
_Times__Times_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm0
	mulss	-24(%rbp), %xmm0
	leave
	ret
.globl _Times__uTimes_var_u8_i64
	.private_extern _Times__uTimes_var_u8_i64
_Times__uTimes_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i64
	.private_extern _Times__Times_var_u8_i64
_Times__Times_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i64
	.private_extern _Times__uTimes_param_u8_i64
_Times__uTimes_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i64
	.private_extern _Times__Times_param_u8_i64
_Times__Times_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_LC
	.private_extern _Times__uTimes_var_u8_LC
_Times__uTimes_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_LC
	.private_extern _Times__Times_var_u8_LC
_Times__Times_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_LC
	.private_extern _Times__uTimes_param_u8_LC
_Times__uTimes_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u8_LC
	.private_extern _Times__Times_param_u8_LC
_Times__Times_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u8
	.private_extern _Times__uTimes_var_u8_u8
_Times__uTimes_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u8
	.private_extern _Times__Times_var_u8_u8
_Times__Times_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
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
.globl _Times__uTimes_param_u8_u8
	.private_extern _Times__uTimes_param_u8_u8
_Times__uTimes_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u8
	.private_extern _Times__Times_param_u8_u8
_Times__Times_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movsbq	-18(%rbp),%rdx
	imulq	%rdx, %rax
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
.globl _Times__uTimes_var_u8_I
	.private_extern _Times__uTimes_var_u8_I
_Times__uTimes_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_I
	.private_extern _Times__Times_var_u8_I
_Times__Times_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
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
.globl _Times__uTimes_param_u8_I
	.private_extern _Times__uTimes_param_u8_I
_Times__uTimes_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_I
	.private_extern _Times__Times_param_u8_I
_Times__Times_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
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
.globl _Times__uTimes_var_u8_i8
	.private_extern _Times__uTimes_var_u8_i8
_Times__uTimes_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i8
	.private_extern _Times__Times_var_u8_i8
_Times__Times_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L240
	cmpq	$127, -16(%rbp)
	jle	L241
L240:
	movl	$5409, %edi
	call	__m3_fault
L241:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i8
	.private_extern _Times__uTimes_param_u8_i8
_Times__uTimes_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i8
	.private_extern _Times__Times_param_u8_i8
_Times__Times_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movzbl	-18(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L246
	cmpq	$127, -16(%rbp)
	jle	L247
L246:
	movl	$5473, %edi
	call	__m3_fault
L247:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u32
	.private_extern _Times__uTimes_var_u8_u32
_Times__uTimes_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u32
	.private_extern _Times__Times_var_u8_u32
_Times__Times_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L252
	cmpq	$127, -16(%rbp)
	jle	L253
L252:
	movl	$5537, %edi
	call	__m3_fault
L253:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u32
	.private_extern _Times__uTimes_param_u8_u32
_Times__uTimes_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u32
	.private_extern _Times__Times_param_u8_u32
_Times__Times_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L258
	cmpq	$127, -16(%rbp)
	jle	L259
L258:
	movl	$5601, %edi
	call	__m3_fault
L259:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_C
	.private_extern _Times__uTimes_var_u8_C
_Times__uTimes_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_C
	.private_extern _Times__Times_var_u8_C
_Times__Times_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L264
	cmpq	$127, -16(%rbp)
	jle	L265
L264:
	movl	$5665, %edi
	call	__m3_fault
L265:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_C
	.private_extern _Times__uTimes_param_u8_C
_Times__uTimes_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u8_C
	.private_extern _Times__Times_param_u8_C
_Times__Times_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L270
	cmpq	$127, -16(%rbp)
	jle	L271
L270:
	movl	$5729, %edi
	call	__m3_fault
L271:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u16
	.private_extern _Times__uTimes_var_u8_u16
_Times__uTimes_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u16
	.private_extern _Times__Times_var_u8_u16
_Times__Times_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L276
	cmpq	$127, -16(%rbp)
	jle	L277
L276:
	movl	$5793, %edi
	call	__m3_fault
L277:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u16
	.private_extern _Times__uTimes_param_u8_u16
_Times__uTimes_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u16
	.private_extern _Times__Times_param_u8_u16
_Times__Times_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L282
	cmpq	$127, -16(%rbp)
	jle	L283
L282:
	movl	$5857, %edi
	call	__m3_fault
L283:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u64
	.private_extern _Times__uTimes_var_u8_u64
_Times__uTimes_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u64
	.private_extern _Times__Times_var_u8_u64
_Times__Times_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u64
	.private_extern _Times__uTimes_param_u8_u64
_Times__uTimes_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u64
	.private_extern _Times__Times_param_u8_u64
_Times__Times_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_L
	.private_extern _Times__uTimes_var_u8_L
_Times__uTimes_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_L
	.private_extern _Times__Times_var_u8_L
_Times__Times_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_L
	.private_extern _Times__uTimes_param_u8_L
_Times__uTimes_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_L
	.private_extern _Times__Times_param_u8_L
_Times__Times_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_i16
	.private_extern _Times__uTimes_var_u8_i16
_Times__uTimes_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i16
	.private_extern _Times__Times_var_u8_i16
_Times__Times_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L304
	cmpq	$127, -16(%rbp)
	jle	L305
L304:
	movl	$6177, %edi
	call	__m3_fault
L305:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i16
	.private_extern _Times__uTimes_param_u8_i16
_Times__uTimes_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i16
	.private_extern _Times__Times_param_u8_i16
_Times__Times_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L310
	cmpq	$127, -16(%rbp)
	jle	L311
L310:
	movl	$6241, %edi
	call	__m3_fault
L311:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_i32
	.private_extern _Times__uTimes_var_u8_i32
_Times__uTimes_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i32
	.private_extern _Times__Times_var_u8_i32
_Times__Times_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L316
	cmpq	$127, -16(%rbp)
	jle	L317
L316:
	movl	$6305, %edi
	call	__m3_fault
L317:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i32
	.private_extern _Times__uTimes_param_u8_i32
_Times__uTimes_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i32
	.private_extern _Times__Times_param_u8_i32
_Times__Times_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L322
	cmpq	$127, -16(%rbp)
	jle	L323
L322:
	movl	$6369, %edi
	call	__m3_fault
L323:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i64
	.private_extern _Times__uTimes_var_I_i64
_Times__uTimes_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i64
	.private_extern _Times__Times_var_I_i64
_Times__Times_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i64
	.private_extern _Times__uTimes_param_I_i64
_Times__uTimes_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i64
	.private_extern _Times__Times_param_I_i64
_Times__Times_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_LC
	.private_extern _Times__uTimes_var_I_LC
_Times__uTimes_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_LC
	.private_extern _Times__Times_var_I_LC
_Times__Times_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_LC
	.private_extern _Times__uTimes_param_I_LC
_Times__uTimes_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_I_LC
	.private_extern _Times__Times_param_I_LC
_Times__Times_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u8
	.private_extern _Times__uTimes_var_I_u8
_Times__uTimes_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u8
	.private_extern _Times__Times_var_I_u8
_Times__Times_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u8
	.private_extern _Times__uTimes_param_I_u8
_Times__uTimes_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u8
	.private_extern _Times__Times_param_I_u8
_Times__Times_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_I
	.private_extern _Times__uTimes_var_I_I
_Times__uTimes_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_I
	.private_extern _Times__Times_var_I_I
_Times__Times_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_I
	.private_extern _Times__uTimes_param_I_I
_Times__uTimes_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_I
	.private_extern _Times__Times_param_I_I
_Times__Times_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i8
	.private_extern _Times__uTimes_var_I_i8
_Times__uTimes_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i8
	.private_extern _Times__Times_var_I_i8
_Times__Times_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i8
	.private_extern _Times__uTimes_param_I_i8
_Times__uTimes_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i8
	.private_extern _Times__Times_param_I_i8
_Times__Times_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u32
	.private_extern _Times__uTimes_var_I_u32
_Times__uTimes_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u32
	.private_extern _Times__Times_var_I_u32
_Times__Times_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u32
	.private_extern _Times__uTimes_param_I_u32
_Times__uTimes_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u32
	.private_extern _Times__Times_param_I_u32
_Times__Times_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_C
	.private_extern _Times__uTimes_var_I_C
_Times__uTimes_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_C
	.private_extern _Times__Times_var_I_C
_Times__Times_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_C
	.private_extern _Times__uTimes_param_I_C
_Times__uTimes_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_I_C
	.private_extern _Times__Times_param_I_C
_Times__Times_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u16
	.private_extern _Times__uTimes_var_I_u16
_Times__uTimes_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u16
	.private_extern _Times__Times_var_I_u16
_Times__Times_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u16
	.private_extern _Times__uTimes_param_I_u16
_Times__uTimes_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u16
	.private_extern _Times__Times_param_I_u16
_Times__Times_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u64
	.private_extern _Times__uTimes_var_I_u64
_Times__uTimes_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u64
	.private_extern _Times__Times_var_I_u64
_Times__Times_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u64
	.private_extern _Times__uTimes_param_I_u64
_Times__uTimes_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u64
	.private_extern _Times__Times_param_I_u64
_Times__Times_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_L
	.private_extern _Times__uTimes_var_I_L
_Times__uTimes_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_L
	.private_extern _Times__Times_var_I_L
_Times__Times_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_L
	.private_extern _Times__uTimes_param_I_L
_Times__uTimes_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_L
	.private_extern _Times__Times_param_I_L
_Times__Times_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i16
	.private_extern _Times__uTimes_var_I_i16
_Times__uTimes_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i16
	.private_extern _Times__Times_var_I_i16
_Times__Times_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i16
	.private_extern _Times__uTimes_param_I_i16
_Times__uTimes_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i16
	.private_extern _Times__Times_param_I_i16
_Times__Times_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i32
	.private_extern _Times__uTimes_var_I_i32
_Times__uTimes_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i32
	.private_extern _Times__Times_var_I_i32
_Times__Times_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i32
	.private_extern _Times__uTimes_param_I_i32
_Times__uTimes_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i32
	.private_extern _Times__Times_param_I_i32
_Times__Times_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i64
	.private_extern _Times__uTimes_var_i8_i64
_Times__uTimes_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i64
	.private_extern _Times__Times_var_i8_i64
_Times__Times_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i64
	.private_extern _Times__uTimes_param_i8_i64
_Times__uTimes_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i64
	.private_extern _Times__Times_param_i8_i64
_Times__Times_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_LC
	.private_extern _Times__uTimes_var_i8_LC
_Times__uTimes_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_LC
	.private_extern _Times__Times_var_i8_LC
_Times__Times_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_LC
	.private_extern _Times__uTimes_param_i8_LC
_Times__uTimes_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i8_LC
	.private_extern _Times__Times_param_i8_LC
_Times__Times_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u8
	.private_extern _Times__uTimes_var_i8_u8
_Times__uTimes_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u8
	.private_extern _Times__Times_var_i8_u8
_Times__Times_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L440
	cmpq	$255, -16(%rbp)
	jle	L441
L440:
	movl	$8225, %edi
	call	__m3_fault
L441:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u8
	.private_extern _Times__uTimes_param_i8_u8
_Times__uTimes_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u8
	.private_extern _Times__Times_param_i8_u8
_Times__Times_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	-18(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L446
	cmpq	$255, -16(%rbp)
	jle	L447
L446:
	movl	$8289, %edi
	call	__m3_fault
L447:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_I
	.private_extern _Times__uTimes_var_i8_I
_Times__uTimes_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_I
	.private_extern _Times__Times_var_i8_I
_Times__Times_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L452
	cmpq	$255, -16(%rbp)
	jle	L453
L452:
	movl	$8353, %edi
	call	__m3_fault
L453:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_I
	.private_extern _Times__uTimes_param_i8_I
_Times__uTimes_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_I
	.private_extern _Times__Times_param_i8_I
_Times__Times_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L458
	cmpq	$255, -16(%rbp)
	jle	L459
L458:
	movl	$8417, %edi
	call	__m3_fault
L459:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i8
	.private_extern _Times__uTimes_var_i8_i8
_Times__uTimes_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i8
	.private_extern _Times__Times_var_i8_i8
_Times__Times_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L464
	cmpq	$255, -16(%rbp)
	jle	L465
L464:
	movl	$8481, %edi
	call	__m3_fault
L465:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i8
	.private_extern _Times__uTimes_param_i8_i8
_Times__uTimes_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i8
	.private_extern _Times__Times_param_i8_i8
_Times__Times_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movzbl	-18(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L470
	cmpq	$255, -16(%rbp)
	jle	L471
L470:
	movl	$8545, %edi
	call	__m3_fault
L471:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u32
	.private_extern _Times__uTimes_var_i8_u32
_Times__uTimes_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u32
	.private_extern _Times__Times_var_i8_u32
_Times__Times_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L476
	cmpq	$255, -16(%rbp)
	jle	L477
L476:
	movl	$8609, %edi
	call	__m3_fault
L477:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u32
	.private_extern _Times__uTimes_param_i8_u32
_Times__uTimes_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u32
	.private_extern _Times__Times_param_i8_u32
_Times__Times_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L482
	cmpq	$255, -16(%rbp)
	jle	L483
L482:
	movl	$8673, %edi
	call	__m3_fault
L483:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_C
	.private_extern _Times__uTimes_var_i8_C
_Times__uTimes_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_C
	.private_extern _Times__Times_var_i8_C
_Times__Times_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L488
	cmpq	$255, -16(%rbp)
	jle	L489
L488:
	movl	$8737, %edi
	call	__m3_fault
L489:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_C
	.private_extern _Times__uTimes_param_i8_C
_Times__uTimes_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i8_C
	.private_extern _Times__Times_param_i8_C
_Times__Times_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L494
	cmpq	$255, -16(%rbp)
	jle	L495
L494:
	movl	$8801, %edi
	call	__m3_fault
L495:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u16
	.private_extern _Times__uTimes_var_i8_u16
_Times__uTimes_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u16
	.private_extern _Times__Times_var_i8_u16
_Times__Times_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L500
	cmpq	$255, -16(%rbp)
	jle	L501
L500:
	movl	$8865, %edi
	call	__m3_fault
L501:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u16
	.private_extern _Times__uTimes_param_i8_u16
_Times__uTimes_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u16
	.private_extern _Times__Times_param_i8_u16
_Times__Times_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L506
	cmpq	$255, -16(%rbp)
	jle	L507
L506:
	movl	$8929, %edi
	call	__m3_fault
L507:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u64
	.private_extern _Times__uTimes_var_i8_u64
_Times__uTimes_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u64
	.private_extern _Times__Times_var_i8_u64
_Times__Times_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u64
	.private_extern _Times__uTimes_param_i8_u64
_Times__uTimes_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u64
	.private_extern _Times__Times_param_i8_u64
_Times__Times_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_L
	.private_extern _Times__uTimes_var_i8_L
_Times__uTimes_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_L
	.private_extern _Times__Times_var_i8_L
_Times__Times_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_L
	.private_extern _Times__uTimes_param_i8_L
_Times__uTimes_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_L
	.private_extern _Times__Times_param_i8_L
_Times__Times_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i16
	.private_extern _Times__uTimes_var_i8_i16
_Times__uTimes_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i16
	.private_extern _Times__Times_var_i8_i16
_Times__Times_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L528
	cmpq	$255, -16(%rbp)
	jle	L529
L528:
	movl	$9249, %edi
	call	__m3_fault
L529:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i16
	.private_extern _Times__uTimes_param_i8_i16
_Times__uTimes_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i16
	.private_extern _Times__Times_param_i8_i16
_Times__Times_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L534
	cmpq	$255, -16(%rbp)
	jle	L535
L534:
	movl	$9313, %edi
	call	__m3_fault
L535:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i32
	.private_extern _Times__uTimes_var_i8_i32
_Times__uTimes_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i32
	.private_extern _Times__Times_var_i8_i32
_Times__Times_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L540
	cmpq	$255, -16(%rbp)
	jle	L541
L540:
	movl	$9377, %edi
	call	__m3_fault
L541:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i32
	.private_extern _Times__uTimes_param_i8_i32
_Times__uTimes_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i32
	.private_extern _Times__Times_param_i8_i32
_Times__Times_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L546
	cmpq	$255, -16(%rbp)
	jle	L547
L546:
	movl	$9441, %edi
	call	__m3_fault
L547:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i64
	.private_extern _Times__uTimes_var_u32_i64
_Times__uTimes_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i64
	.private_extern _Times__Times_var_u32_i64
_Times__Times_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i64
	.private_extern _Times__uTimes_param_u32_i64
_Times__uTimes_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_i64
	.private_extern _Times__Times_param_u32_i64
_Times__Times_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_LC
	.private_extern _Times__uTimes_var_u32_LC
_Times__uTimes_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_LC
	.private_extern _Times__Times_var_u32_LC
_Times__Times_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_LC
	.private_extern _Times__uTimes_param_u32_LC
_Times__uTimes_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u32_LC
	.private_extern _Times__Times_param_u32_LC
_Times__Times_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u8
	.private_extern _Times__uTimes_var_u32_u8
_Times__uTimes_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u8
	.private_extern _Times__Times_var_u32_u8
_Times__Times_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L568
	cmpq	$2147483647, -16(%rbp)
	jle	L569
L568:
	movl	$9761, %edi
	call	__m3_fault
L569:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u8
	.private_extern _Times__uTimes_param_u32_u8
_Times__uTimes_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_u8
	.private_extern _Times__Times_param_u32_u8
_Times__Times_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L574
	cmpq	$2147483647, -16(%rbp)
	jle	L575
L574:
	movl	$9825, %edi
	call	__m3_fault
L575:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_I
	.private_extern _Times__uTimes_var_u32_I
_Times__uTimes_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_I
	.private_extern _Times__Times_var_u32_I
_Times__Times_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L580
	cmpq	$2147483647, -16(%rbp)
	jle	L581
L580:
	movl	$9889, %edi
	call	__m3_fault
L581:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_I
	.private_extern _Times__uTimes_param_u32_I
_Times__uTimes_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_I
	.private_extern _Times__Times_param_u32_I
_Times__Times_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L586
	cmpq	$2147483647, -16(%rbp)
	jle	L587
L586:
	movl	$9953, %edi
	call	__m3_fault
L587:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i8
	.private_extern _Times__uTimes_var_u32_i8
_Times__uTimes_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i8
	.private_extern _Times__Times_var_u32_i8
_Times__Times_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L592
	cmpq	$2147483647, -16(%rbp)
	jle	L593
L592:
	movl	$10017, %edi
	call	__m3_fault
L593:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i8
	.private_extern _Times__uTimes_param_u32_i8
_Times__uTimes_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_i8
	.private_extern _Times__Times_param_u32_i8
_Times__Times_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L598
	cmpq	$2147483647, -16(%rbp)
	jle	L599
L598:
	movl	$10081, %edi
	call	__m3_fault
L599:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u32
	.private_extern _Times__uTimes_var_u32_u32
_Times__uTimes_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u32
	.private_extern _Times__Times_var_u32_u32
_Times__Times_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L604
	cmpq	$2147483647, -16(%rbp)
	jle	L605
L604:
	movl	$10145, %edi
	call	__m3_fault
L605:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u32
	.private_extern _Times__uTimes_param_u32_u32
_Times__uTimes_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_u32
	.private_extern _Times__Times_param_u32_u32
_Times__Times_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L610
	cmpq	$2147483647, -16(%rbp)
	jle	L611
L610:
	movl	$10209, %edi
	call	__m3_fault
L611:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_C
	.private_extern _Times__uTimes_var_u32_C
_Times__uTimes_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_C
	.private_extern _Times__Times_var_u32_C
_Times__Times_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L616
	cmpq	$2147483647, -16(%rbp)
	jle	L617
L616:
	movl	$10273, %edi
	call	__m3_fault
L617:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_C
	.private_extern _Times__uTimes_param_u32_C
_Times__uTimes_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u32_C
	.private_extern _Times__Times_param_u32_C
_Times__Times_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L622
	cmpq	$2147483647, -16(%rbp)
	jle	L623
L622:
	movl	$10337, %edi
	call	__m3_fault
L623:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u16
	.private_extern _Times__uTimes_var_u32_u16
_Times__uTimes_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u16
	.private_extern _Times__Times_var_u32_u16
_Times__Times_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L628
	cmpq	$2147483647, -16(%rbp)
	jle	L629
L628:
	movl	$10401, %edi
	call	__m3_fault
L629:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u16
	.private_extern _Times__uTimes_param_u32_u16
_Times__uTimes_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_u16
	.private_extern _Times__Times_param_u32_u16
_Times__Times_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L634
	cmpq	$2147483647, -16(%rbp)
	jle	L635
L634:
	movl	$10465, %edi
	call	__m3_fault
L635:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u64
	.private_extern _Times__uTimes_var_u32_u64
_Times__uTimes_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u64
	.private_extern _Times__Times_var_u32_u64
_Times__Times_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u64
	.private_extern _Times__uTimes_param_u32_u64
_Times__uTimes_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_u64
	.private_extern _Times__Times_param_u32_u64
_Times__Times_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_L
	.private_extern _Times__uTimes_var_u32_L
_Times__uTimes_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_L
	.private_extern _Times__Times_var_u32_L
_Times__Times_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_L
	.private_extern _Times__uTimes_param_u32_L
_Times__uTimes_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_L
	.private_extern _Times__Times_param_u32_L
_Times__Times_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i16
	.private_extern _Times__uTimes_var_u32_i16
_Times__uTimes_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i16
	.private_extern _Times__Times_var_u32_i16
_Times__Times_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L656
	cmpq	$2147483647, -16(%rbp)
	jle	L657
L656:
	movl	$10785, %edi
	call	__m3_fault
L657:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i16
	.private_extern _Times__uTimes_param_u32_i16
_Times__uTimes_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_i16
	.private_extern _Times__Times_param_u32_i16
_Times__Times_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L662
	cmpq	$2147483647, -16(%rbp)
	jle	L663
L662:
	movl	$10849, %edi
	call	__m3_fault
L663:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i32
	.private_extern _Times__uTimes_var_u32_i32
_Times__uTimes_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i32
	.private_extern _Times__Times_var_u32_i32
_Times__Times_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L668
	cmpq	$2147483647, -16(%rbp)
	jle	L669
L668:
	movl	$10913, %edi
	call	__m3_fault
L669:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i32
	.private_extern _Times__uTimes_param_u32_i32
_Times__uTimes_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_i32
	.private_extern _Times__Times_param_u32_i32
_Times__Times_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L674
	cmpq	$2147483647, -16(%rbp)
	jle	L675
L674:
	movl	$10977, %edi
	call	__m3_fault
L675:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i64
	.private_extern _Times__uTimes_var_C_i64
_Times__uTimes_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i64
	.private_extern _Times__Times_var_C_i64
_Times__Times_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_i64
	.private_extern _Times__uTimes_param_C_i64
_Times__uTimes_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i64
	.private_extern _Times__Times_param_C_i64
_Times__Times_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_LC
	.private_extern _Times__uTimes_var_C_LC
_Times__uTimes_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_LC
	.private_extern _Times__Times_var_C_LC
_Times__Times_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_LC
	.private_extern _Times__uTimes_param_C_LC
_Times__uTimes_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_LC
	.private_extern _Times__Times_param_C_LC
_Times__Times_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_C_u8
	.private_extern _Times__uTimes_var_C_u8
_Times__uTimes_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u8
	.private_extern _Times__Times_var_C_u8
_Times__Times_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L696
	movl	$11297, %edi
	call	__m3_fault
L696:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_u8
	.private_extern _Times__uTimes_param_C_u8
_Times__uTimes_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u8
	.private_extern _Times__Times_param_C_u8
_Times__Times_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L701
	movl	$11361, %edi
	call	__m3_fault
L701:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_I
	.private_extern _Times__uTimes_var_C_I
_Times__uTimes_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_I
	.private_extern _Times__Times_var_C_I
_Times__Times_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L706
	movl	$11425, %edi
	call	__m3_fault
L706:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_I
	.private_extern _Times__uTimes_param_C_I
_Times__uTimes_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_I
	.private_extern _Times__Times_param_C_I
_Times__Times_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L711
	movl	$11489, %edi
	call	__m3_fault
L711:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i8
	.private_extern _Times__uTimes_var_C_i8
_Times__uTimes_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i8
	.private_extern _Times__Times_var_C_i8
_Times__Times_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L716
	movl	$11553, %edi
	call	__m3_fault
L716:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_i8
	.private_extern _Times__uTimes_param_C_i8
_Times__uTimes_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i8
	.private_extern _Times__Times_param_C_i8
_Times__Times_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L721
	movl	$11617, %edi
	call	__m3_fault
L721:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u32
	.private_extern _Times__uTimes_var_C_u32
_Times__uTimes_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u32
	.private_extern _Times__Times_var_C_u32
_Times__Times_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L726
	movl	$11681, %edi
	call	__m3_fault
L726:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_u32
	.private_extern _Times__uTimes_param_C_u32
_Times__uTimes_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u32
	.private_extern _Times__Times_param_C_u32
_Times__Times_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L731
	movl	$11745, %edi
	call	__m3_fault
L731:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_C
	.private_extern _Times__uTimes_var_C_C
_Times__uTimes_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_C
	.private_extern _Times__Times_var_C_C
_Times__Times_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L736
	movl	$11809, %edi
	call	__m3_fault
L736:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_C
	.private_extern _Times__uTimes_param_C_C
_Times__uTimes_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_C
	.private_extern _Times__Times_param_C_C
_Times__Times_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L741
	movl	$11873, %edi
	call	__m3_fault
L741:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u16
	.private_extern _Times__uTimes_var_C_u16
_Times__uTimes_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u16
	.private_extern _Times__Times_var_C_u16
_Times__Times_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L746
	movl	$11937, %edi
	call	__m3_fault
L746:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_u16
	.private_extern _Times__uTimes_param_C_u16
_Times__uTimes_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u16
	.private_extern _Times__Times_param_C_u16
_Times__Times_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L751
	movl	$12001, %edi
	call	__m3_fault
L751:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u64
	.private_extern _Times__uTimes_var_C_u64
_Times__uTimes_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u64
	.private_extern _Times__Times_var_C_u64
_Times__Times_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_u64
	.private_extern _Times__uTimes_param_C_u64
_Times__uTimes_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u64
	.private_extern _Times__Times_param_C_u64
_Times__Times_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_L
	.private_extern _Times__uTimes_var_C_L
_Times__uTimes_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_L
	.private_extern _Times__Times_var_C_L
_Times__Times_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_L
	.private_extern _Times__uTimes_param_C_L
_Times__uTimes_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_L
	.private_extern _Times__Times_param_C_L
_Times__Times_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i16
	.private_extern _Times__uTimes_var_C_i16
_Times__uTimes_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i16
	.private_extern _Times__Times_var_C_i16
_Times__Times_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L772
	movl	$12321, %edi
	call	__m3_fault
L772:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_i16
	.private_extern _Times__uTimes_param_C_i16
_Times__uTimes_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i16
	.private_extern _Times__Times_param_C_i16
_Times__Times_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L777
	movl	$12385, %edi
	call	__m3_fault
L777:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i32
	.private_extern _Times__uTimes_var_C_i32
_Times__uTimes_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i32
	.private_extern _Times__Times_var_C_i32
_Times__Times_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L782
	movl	$12449, %edi
	call	__m3_fault
L782:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_i32
	.private_extern _Times__uTimes_param_C_i32
_Times__uTimes_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i32
	.private_extern _Times__Times_param_C_i32
_Times__Times_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L787
	movl	$12513, %edi
	call	__m3_fault
L787:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i64
	.private_extern _Times__uTimes_var_u16_i64
_Times__uTimes_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i64
	.private_extern _Times__Times_var_u16_i64
_Times__Times_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i64
	.private_extern _Times__uTimes_param_u16_i64
_Times__uTimes_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i64
	.private_extern _Times__Times_param_u16_i64
_Times__Times_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_LC
	.private_extern _Times__uTimes_var_u16_LC
_Times__uTimes_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_LC
	.private_extern _Times__Times_var_u16_LC
_Times__Times_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_LC
	.private_extern _Times__uTimes_param_u16_LC
_Times__uTimes_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u16_LC
	.private_extern _Times__Times_param_u16_LC
_Times__Times_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_u16_u8
	.private_extern _Times__uTimes_var_u16_u8
_Times__uTimes_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u8
	.private_extern _Times__Times_var_u16_u8
_Times__Times_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L808
	cmpq	$32767, -16(%rbp)
	jle	L809
L808:
	movl	$12833, %edi
	call	__m3_fault
L809:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u8
	.private_extern _Times__uTimes_param_u16_u8
_Times__uTimes_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u8
	.private_extern _Times__Times_param_u16_u8
_Times__Times_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movsbq	-19(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L814
	cmpq	$32767, -16(%rbp)
	jle	L815
L814:
	movl	$12897, %edi
	call	__m3_fault
L815:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_I
	.private_extern _Times__uTimes_var_u16_I
_Times__uTimes_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_I
	.private_extern _Times__Times_var_u16_I
_Times__Times_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L820
	cmpq	$32767, -16(%rbp)
	jle	L821
L820:
	movl	$12961, %edi
	call	__m3_fault
L821:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_I
	.private_extern _Times__uTimes_param_u16_I
_Times__uTimes_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_I
	.private_extern _Times__Times_param_u16_I
_Times__Times_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L826
	cmpq	$32767, -16(%rbp)
	jle	L827
L826:
	movl	$13025, %edi
	call	__m3_fault
L827:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i8
	.private_extern _Times__uTimes_var_u16_i8
_Times__uTimes_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i8
	.private_extern _Times__Times_var_u16_i8
_Times__Times_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L832
	cmpq	$32767, -16(%rbp)
	jle	L833
L832:
	movl	$13089, %edi
	call	__m3_fault
L833:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i8
	.private_extern _Times__uTimes_param_u16_i8
_Times__uTimes_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i8
	.private_extern _Times__Times_param_u16_i8
_Times__Times_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movzbl	-19(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L838
	cmpq	$32767, -16(%rbp)
	jle	L839
L838:
	movl	$13153, %edi
	call	__m3_fault
L839:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_u32
	.private_extern _Times__uTimes_var_u16_u32
_Times__uTimes_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u32
	.private_extern _Times__Times_var_u16_u32
_Times__Times_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L844
	cmpq	$32767, -16(%rbp)
	jle	L845
L844:
	movl	$13217, %edi
	call	__m3_fault
L845:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u32
	.private_extern _Times__uTimes_param_u16_u32
_Times__uTimes_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u32
	.private_extern _Times__Times_param_u16_u32
_Times__Times_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L850
	cmpq	$32767, -16(%rbp)
	jle	L851
L850:
	movl	$13281, %edi
	call	__m3_fault
L851:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_C
	.private_extern _Times__uTimes_var_u16_C
_Times__uTimes_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_C
	.private_extern _Times__Times_var_u16_C
_Times__Times_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L856
	cmpq	$32767, -16(%rbp)
	jle	L857
L856:
	movl	$13345, %edi
	call	__m3_fault
L857:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_C
	.private_extern _Times__uTimes_param_u16_C
_Times__uTimes_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u16_C
	.private_extern _Times__Times_param_u16_C
_Times__Times_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L862
	cmpq	$32767, -16(%rbp)
	jle	L863
L862:
	movl	$13409, %edi
	call	__m3_fault
L863:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_u16
	.private_extern _Times__uTimes_var_u16_u16
_Times__uTimes_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u16
	.private_extern _Times__Times_var_u16_u16
_Times__Times_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L868
	cmpq	$32767, -16(%rbp)
	jle	L869
L868:
	movl	$13473, %edi
	call	__m3_fault
L869:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u16
	.private_extern _Times__uTimes_param_u16_u16
_Times__uTimes_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u16
	.private_extern _Times__Times_param_u16_u16
_Times__Times_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L874
	cmpq	$32767, -16(%rbp)
	jle	L875
L874:
	movl	$13537, %edi
	call	__m3_fault
L875:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_u64
	.private_extern _Times__uTimes_var_u16_u64
_Times__uTimes_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u64
	.private_extern _Times__Times_var_u16_u64
_Times__Times_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u64
	.private_extern _Times__uTimes_param_u16_u64
_Times__uTimes_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u64
	.private_extern _Times__Times_param_u16_u64
_Times__Times_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_L
	.private_extern _Times__uTimes_var_u16_L
_Times__uTimes_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_L
	.private_extern _Times__Times_var_u16_L
_Times__Times_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_L
	.private_extern _Times__uTimes_param_u16_L
_Times__uTimes_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_L
	.private_extern _Times__Times_param_u16_L
_Times__Times_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i16
	.private_extern _Times__uTimes_var_u16_i16
_Times__uTimes_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i16
	.private_extern _Times__Times_var_u16_i16
_Times__Times_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L896
	cmpq	$32767, -16(%rbp)
	jle	L897
L896:
	movl	$13857, %edi
	call	__m3_fault
L897:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i16
	.private_extern _Times__uTimes_param_u16_i16
_Times__uTimes_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i16
	.private_extern _Times__Times_param_u16_i16
_Times__Times_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L902
	cmpq	$32767, -16(%rbp)
	jle	L903
L902:
	movl	$13921, %edi
	call	__m3_fault
L903:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i32
	.private_extern _Times__uTimes_var_u16_i32
_Times__uTimes_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i32
	.private_extern _Times__Times_var_u16_i32
_Times__Times_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L908
	cmpq	$32767, -16(%rbp)
	jle	L909
L908:
	movl	$13985, %edi
	call	__m3_fault
L909:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i32
	.private_extern _Times__uTimes_param_u16_i32
_Times__uTimes_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i32
	.private_extern _Times__Times_param_u16_i32
_Times__Times_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L914
	cmpq	$32767, -16(%rbp)
	jle	L915
L914:
	movl	$14049, %edi
	call	__m3_fault
L915:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i64
	.private_extern _Times__uTimes_var_u64_i64
_Times__uTimes_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i64
	.private_extern _Times__Times_var_u64_i64
_Times__Times_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i64
	.private_extern _Times__uTimes_param_u64_i64
_Times__uTimes_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i64
	.private_extern _Times__Times_param_u64_i64
_Times__Times_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_LC
	.private_extern _Times__uTimes_var_u64_LC
_Times__uTimes_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_LC
	.private_extern _Times__Times_var_u64_LC
_Times__Times_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_LC
	.private_extern _Times__uTimes_param_u64_LC
_Times__uTimes_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u64_LC
	.private_extern _Times__Times_param_u64_LC
_Times__Times_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u8
	.private_extern _Times__uTimes_var_u64_u8
_Times__uTimes_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u8
	.private_extern _Times__Times_var_u64_u8
_Times__Times_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u8
	.private_extern _Times__uTimes_param_u64_u8
_Times__uTimes_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u8
	.private_extern _Times__Times_param_u64_u8
_Times__Times_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_I
	.private_extern _Times__uTimes_var_u64_I
_Times__uTimes_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_I
	.private_extern _Times__Times_var_u64_I
_Times__Times_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_I
	.private_extern _Times__uTimes_param_u64_I
_Times__uTimes_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_I
	.private_extern _Times__Times_param_u64_I
_Times__Times_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i8
	.private_extern _Times__uTimes_var_u64_i8
_Times__uTimes_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i8
	.private_extern _Times__Times_var_u64_i8
_Times__Times_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i8
	.private_extern _Times__uTimes_param_u64_i8
_Times__uTimes_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i8
	.private_extern _Times__Times_param_u64_i8
_Times__Times_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u32
	.private_extern _Times__uTimes_var_u64_u32
_Times__uTimes_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u32
	.private_extern _Times__Times_var_u64_u32
_Times__Times_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u32
	.private_extern _Times__uTimes_param_u64_u32
_Times__uTimes_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u32
	.private_extern _Times__Times_param_u64_u32
_Times__Times_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_C
	.private_extern _Times__uTimes_var_u64_C
_Times__uTimes_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_C
	.private_extern _Times__Times_var_u64_C
_Times__Times_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_C
	.private_extern _Times__uTimes_param_u64_C
_Times__uTimes_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u64_C
	.private_extern _Times__Times_param_u64_C
_Times__Times_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u16
	.private_extern _Times__uTimes_var_u64_u16
_Times__uTimes_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u16
	.private_extern _Times__Times_var_u64_u16
_Times__Times_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u16
	.private_extern _Times__uTimes_param_u64_u16
_Times__uTimes_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u16
	.private_extern _Times__Times_param_u64_u16
_Times__Times_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u64
	.private_extern _Times__uTimes_var_u64_u64
_Times__uTimes_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u64
	.private_extern _Times__Times_var_u64_u64
_Times__Times_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u64
	.private_extern _Times__uTimes_param_u64_u64
_Times__uTimes_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u64
	.private_extern _Times__Times_param_u64_u64
_Times__Times_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_L
	.private_extern _Times__uTimes_var_u64_L
_Times__uTimes_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_L
	.private_extern _Times__Times_var_u64_L
_Times__Times_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_L
	.private_extern _Times__uTimes_param_u64_L
_Times__uTimes_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_L
	.private_extern _Times__Times_param_u64_L
_Times__Times_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i16
	.private_extern _Times__uTimes_var_u64_i16
_Times__uTimes_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i16
	.private_extern _Times__Times_var_u64_i16
_Times__Times_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i16
	.private_extern _Times__uTimes_param_u64_i16
_Times__uTimes_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i16
	.private_extern _Times__Times_param_u64_i16
_Times__Times_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i32
	.private_extern _Times__uTimes_var_u64_i32
_Times__uTimes_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i32
	.private_extern _Times__Times_var_u64_i32
_Times__Times_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i32
	.private_extern _Times__uTimes_param_u64_i32
_Times__uTimes_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i32
	.private_extern _Times__Times_param_u64_i32
_Times__Times_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_var_f64_f64
	.private_extern _Times__Times_var_f64_f64
_Times__Times_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	168+_MM_Times(%rip), %xmm1
	movsd	168+_MM_Times(%rip), %xmm0
	mulsd	%xmm1, %xmm0
	leave
	ret
.globl _Times__Times_param_f64_f64
	.private_extern _Times__Times_param_f64_f64
_Times__Times_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm0
	mulsd	-32(%rbp), %xmm0
	leave
	ret
.globl _Times__uTimes_var_L_i64
	.private_extern _Times__uTimes_var_L_i64
_Times__uTimes_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i64
	.private_extern _Times__Times_var_L_i64
_Times__Times_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i64
	.private_extern _Times__uTimes_param_L_i64
_Times__uTimes_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i64
	.private_extern _Times__Times_param_L_i64
_Times__Times_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_LC
	.private_extern _Times__uTimes_var_L_LC
_Times__uTimes_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_LC
	.private_extern _Times__Times_var_L_LC
_Times__Times_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_LC
	.private_extern _Times__uTimes_param_L_LC
_Times__uTimes_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_L_LC
	.private_extern _Times__Times_param_L_LC
_Times__Times_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u8
	.private_extern _Times__uTimes_var_L_u8
_Times__uTimes_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u8
	.private_extern _Times__Times_var_L_u8
_Times__Times_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u8
	.private_extern _Times__uTimes_param_L_u8
_Times__uTimes_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u8
	.private_extern _Times__Times_param_L_u8
_Times__Times_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_I
	.private_extern _Times__uTimes_var_L_I
_Times__uTimes_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_I
	.private_extern _Times__Times_var_L_I
_Times__Times_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_I
	.private_extern _Times__uTimes_param_L_I
_Times__uTimes_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_I
	.private_extern _Times__Times_param_L_I
_Times__Times_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i8
	.private_extern _Times__uTimes_var_L_i8
_Times__uTimes_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i8
	.private_extern _Times__Times_var_L_i8
_Times__Times_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i8
	.private_extern _Times__uTimes_param_L_i8
_Times__uTimes_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i8
	.private_extern _Times__Times_param_L_i8
_Times__Times_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u32
	.private_extern _Times__uTimes_var_L_u32
_Times__uTimes_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u32
	.private_extern _Times__Times_var_L_u32
_Times__Times_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u32
	.private_extern _Times__uTimes_param_L_u32
_Times__uTimes_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u32
	.private_extern _Times__Times_param_L_u32
_Times__Times_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_C
	.private_extern _Times__uTimes_var_L_C
_Times__uTimes_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_C
	.private_extern _Times__Times_var_L_C
_Times__Times_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_C
	.private_extern _Times__uTimes_param_L_C
_Times__uTimes_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_L_C
	.private_extern _Times__Times_param_L_C
_Times__Times_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u16
	.private_extern _Times__uTimes_var_L_u16
_Times__uTimes_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u16
	.private_extern _Times__Times_var_L_u16
_Times__Times_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u16
	.private_extern _Times__uTimes_param_L_u16
_Times__uTimes_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u16
	.private_extern _Times__Times_param_L_u16
_Times__Times_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u64
	.private_extern _Times__uTimes_var_L_u64
_Times__uTimes_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u64
	.private_extern _Times__Times_var_L_u64
_Times__Times_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u64
	.private_extern _Times__uTimes_param_L_u64
_Times__uTimes_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u64
	.private_extern _Times__Times_param_L_u64
_Times__Times_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_L
	.private_extern _Times__uTimes_var_L_L
_Times__uTimes_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_L
	.private_extern _Times__Times_var_L_L
_Times__Times_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_L
	.private_extern _Times__uTimes_param_L_L
_Times__uTimes_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_L
	.private_extern _Times__Times_param_L_L
_Times__Times_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i16
	.private_extern _Times__uTimes_var_L_i16
_Times__uTimes_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i16
	.private_extern _Times__Times_var_L_i16
_Times__Times_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i16
	.private_extern _Times__uTimes_param_L_i16
_Times__uTimes_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i16
	.private_extern _Times__Times_param_L_i16
_Times__Times_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i32
	.private_extern _Times__uTimes_var_L_i32
_Times__uTimes_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i32
	.private_extern _Times__Times_var_L_i32
_Times__Times_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i32
	.private_extern _Times__uTimes_param_L_i32
_Times__uTimes_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i32
	.private_extern _Times__Times_param_L_i32
_Times__Times_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i64
	.private_extern _Times__uTimes_var_i16_i64
_Times__uTimes_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i64
	.private_extern _Times__Times_var_i16_i64
_Times__Times_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i64
	.private_extern _Times__uTimes_param_i16_i64
_Times__uTimes_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i64
	.private_extern _Times__Times_param_i16_i64
_Times__Times_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_LC
	.private_extern _Times__uTimes_var_i16_LC
_Times__uTimes_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_LC
	.private_extern _Times__Times_var_i16_LC
_Times__Times_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_LC
	.private_extern _Times__uTimes_param_i16_LC
_Times__uTimes_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i16_LC
	.private_extern _Times__Times_param_i16_LC
_Times__Times_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u8
	.private_extern _Times__uTimes_var_i16_u8
_Times__uTimes_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u8
	.private_extern _Times__Times_var_i16_u8
_Times__Times_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1132
	cmpq	$65535, -16(%rbp)
	jle	L1133
L1132:
	movl	$17505, %edi
	call	__m3_fault
L1133:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u8
	.private_extern _Times__uTimes_param_i16_u8
_Times__uTimes_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u8
	.private_extern _Times__Times_param_i16_u8
_Times__Times_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movsbq	-19(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1138
	cmpq	$65535, -16(%rbp)
	jle	L1139
L1138:
	movl	$17569, %edi
	call	__m3_fault
L1139:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_I
	.private_extern _Times__uTimes_var_i16_I
_Times__uTimes_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_I
	.private_extern _Times__Times_var_i16_I
_Times__Times_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1144
	cmpq	$65535, -16(%rbp)
	jle	L1145
L1144:
	movl	$17633, %edi
	call	__m3_fault
L1145:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_I
	.private_extern _Times__uTimes_param_i16_I
_Times__uTimes_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_I
	.private_extern _Times__Times_param_i16_I
_Times__Times_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1150
	cmpq	$65535, -16(%rbp)
	jle	L1151
L1150:
	movl	$17697, %edi
	call	__m3_fault
L1151:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i8
	.private_extern _Times__uTimes_var_i16_i8
_Times__uTimes_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i8
	.private_extern _Times__Times_var_i16_i8
_Times__Times_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1156
	cmpq	$65535, -16(%rbp)
	jle	L1157
L1156:
	movl	$17761, %edi
	call	__m3_fault
L1157:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i8
	.private_extern _Times__uTimes_param_i16_i8
_Times__uTimes_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i8
	.private_extern _Times__Times_param_i16_i8
_Times__Times_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movzbl	-19(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1162
	cmpq	$65535, -16(%rbp)
	jle	L1163
L1162:
	movl	$17825, %edi
	call	__m3_fault
L1163:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u32
	.private_extern _Times__uTimes_var_i16_u32
_Times__uTimes_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u32
	.private_extern _Times__Times_var_i16_u32
_Times__Times_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1168
	cmpq	$65535, -16(%rbp)
	jle	L1169
L1168:
	movl	$17889, %edi
	call	__m3_fault
L1169:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u32
	.private_extern _Times__uTimes_param_i16_u32
_Times__uTimes_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u32
	.private_extern _Times__Times_param_i16_u32
_Times__Times_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1174
	cmpq	$65535, -16(%rbp)
	jle	L1175
L1174:
	movl	$17953, %edi
	call	__m3_fault
L1175:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_C
	.private_extern _Times__uTimes_var_i16_C
_Times__uTimes_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_C
	.private_extern _Times__Times_var_i16_C
_Times__Times_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1180
	cmpq	$65535, -16(%rbp)
	jle	L1181
L1180:
	movl	$18017, %edi
	call	__m3_fault
L1181:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_C
	.private_extern _Times__uTimes_param_i16_C
_Times__uTimes_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i16_C
	.private_extern _Times__Times_param_i16_C
_Times__Times_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1186
	cmpq	$65535, -16(%rbp)
	jle	L1187
L1186:
	movl	$18081, %edi
	call	__m3_fault
L1187:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u16
	.private_extern _Times__uTimes_var_i16_u16
_Times__uTimes_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u16
	.private_extern _Times__Times_var_i16_u16
_Times__Times_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1192
	cmpq	$65535, -16(%rbp)
	jle	L1193
L1192:
	movl	$18145, %edi
	call	__m3_fault
L1193:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u16
	.private_extern _Times__uTimes_param_i16_u16
_Times__uTimes_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u16
	.private_extern _Times__Times_param_i16_u16
_Times__Times_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1198
	cmpq	$65535, -16(%rbp)
	jle	L1199
L1198:
	movl	$18209, %edi
	call	__m3_fault
L1199:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u64
	.private_extern _Times__uTimes_var_i16_u64
_Times__uTimes_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u64
	.private_extern _Times__Times_var_i16_u64
_Times__Times_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u64
	.private_extern _Times__uTimes_param_i16_u64
_Times__uTimes_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u64
	.private_extern _Times__Times_param_i16_u64
_Times__Times_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_L
	.private_extern _Times__uTimes_var_i16_L
_Times__uTimes_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_L
	.private_extern _Times__Times_var_i16_L
_Times__Times_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_L
	.private_extern _Times__uTimes_param_i16_L
_Times__uTimes_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_L
	.private_extern _Times__Times_param_i16_L
_Times__Times_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i16
	.private_extern _Times__uTimes_var_i16_i16
_Times__uTimes_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i16
	.private_extern _Times__Times_var_i16_i16
_Times__Times_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1220
	cmpq	$65535, -16(%rbp)
	jle	L1221
L1220:
	movl	$18529, %edi
	call	__m3_fault
L1221:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i16
	.private_extern _Times__uTimes_param_i16_i16
_Times__uTimes_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i16
	.private_extern _Times__Times_param_i16_i16
_Times__Times_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1226
	cmpq	$65535, -16(%rbp)
	jle	L1227
L1226:
	movl	$18593, %edi
	call	__m3_fault
L1227:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i32
	.private_extern _Times__uTimes_var_i16_i32
_Times__uTimes_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i32
	.private_extern _Times__Times_var_i16_i32
_Times__Times_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1232
	cmpq	$65535, -16(%rbp)
	jle	L1233
L1232:
	movl	$18657, %edi
	call	__m3_fault
L1233:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i32
	.private_extern _Times__uTimes_param_i16_i32
_Times__uTimes_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i32
	.private_extern _Times__Times_param_i16_i32
_Times__Times_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1238
	cmpq	$65535, -16(%rbp)
	jle	L1239
L1238:
	movl	$18721, %edi
	call	__m3_fault
L1239:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i64
	.private_extern _Times__uTimes_var_i32_i64
_Times__uTimes_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i64
	.private_extern _Times__Times_var_i32_i64
_Times__Times_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i64
	.private_extern _Times__uTimes_param_i32_i64
_Times__uTimes_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i64
	.private_extern _Times__Times_param_i32_i64
_Times__Times_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_LC
	.private_extern _Times__uTimes_var_i32_LC
_Times__uTimes_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_LC
	.private_extern _Times__Times_var_i32_LC
_Times__Times_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_LC
	.private_extern _Times__uTimes_param_i32_LC
_Times__uTimes_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i32_LC
	.private_extern _Times__Times_param_i32_LC
_Times__Times_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u8
	.private_extern _Times__uTimes_var_i32_u8
_Times__uTimes_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u8
	.private_extern _Times__Times_var_i32_u8
_Times__Times_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1260
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1261
L1260:
	movl	$19041, %edi
	call	__m3_fault
L1261:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u8
	.private_extern _Times__uTimes_param_i32_u8
_Times__uTimes_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u8
	.private_extern _Times__Times_param_i32_u8
_Times__Times_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movsbq	-21(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1266
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1267
L1266:
	movl	$19105, %edi
	call	__m3_fault
L1267:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_I
	.private_extern _Times__uTimes_var_i32_I
_Times__uTimes_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_I
	.private_extern _Times__Times_var_i32_I
_Times__Times_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1272
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1273
L1272:
	movl	$19169, %edi
	call	__m3_fault
L1273:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_I
	.private_extern _Times__uTimes_param_i32_I
_Times__uTimes_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_I
	.private_extern _Times__Times_param_i32_I
_Times__Times_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1278
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1279
L1278:
	movl	$19233, %edi
	call	__m3_fault
L1279:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i8
	.private_extern _Times__uTimes_var_i32_i8
_Times__uTimes_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i8
	.private_extern _Times__Times_var_i32_i8
_Times__Times_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1284
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1285
L1284:
	movl	$19297, %edi
	call	__m3_fault
L1285:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i8
	.private_extern _Times__uTimes_param_i32_i8
_Times__uTimes_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i8
	.private_extern _Times__Times_param_i32_i8
_Times__Times_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movzbl	-21(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1290
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1291
L1290:
	movl	$19361, %edi
	call	__m3_fault
L1291:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u32
	.private_extern _Times__uTimes_var_i32_u32
_Times__uTimes_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u32
	.private_extern _Times__Times_var_i32_u32
_Times__Times_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1296
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1297
L1296:
	movl	$19425, %edi
	call	__m3_fault
L1297:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u32
	.private_extern _Times__uTimes_param_i32_u32
_Times__uTimes_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u32
	.private_extern _Times__Times_param_i32_u32
_Times__Times_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1302
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1303
L1302:
	movl	$19489, %edi
	call	__m3_fault
L1303:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_C
	.private_extern _Times__uTimes_var_i32_C
_Times__uTimes_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_C
	.private_extern _Times__Times_var_i32_C
_Times__Times_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1308
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1309
L1308:
	movl	$19553, %edi
	call	__m3_fault
L1309:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_C
	.private_extern _Times__uTimes_param_i32_C
_Times__uTimes_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i32_C
	.private_extern _Times__Times_param_i32_C
_Times__Times_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1314
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1315
L1314:
	movl	$19617, %edi
	call	__m3_fault
L1315:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u16
	.private_extern _Times__uTimes_var_i32_u16
_Times__uTimes_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u16
	.private_extern _Times__Times_var_i32_u16
_Times__Times_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1320
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1321
L1320:
	movl	$19681, %edi
	call	__m3_fault
L1321:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u16
	.private_extern _Times__uTimes_param_i32_u16
_Times__uTimes_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u16
	.private_extern _Times__Times_param_i32_u16
_Times__Times_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movswq	-22(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1326
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1327
L1326:
	movl	$19745, %edi
	call	__m3_fault
L1327:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u64
	.private_extern _Times__uTimes_var_i32_u64
_Times__uTimes_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u64
	.private_extern _Times__Times_var_i32_u64
_Times__Times_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u64
	.private_extern _Times__uTimes_param_i32_u64
_Times__uTimes_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u64
	.private_extern _Times__Times_param_i32_u64
_Times__Times_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_L
	.private_extern _Times__uTimes_var_i32_L
_Times__uTimes_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_L
	.private_extern _Times__Times_var_i32_L
_Times__Times_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_L
	.private_extern _Times__uTimes_param_i32_L
_Times__uTimes_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_L
	.private_extern _Times__Times_param_i32_L
_Times__Times_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i16
	.private_extern _Times__uTimes_var_i32_i16
_Times__uTimes_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i16
	.private_extern _Times__Times_var_i32_i16
_Times__Times_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1348
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1349
L1348:
	movl	$20065, %edi
	call	__m3_fault
L1349:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i16
	.private_extern _Times__uTimes_param_i32_i16
_Times__uTimes_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i16
	.private_extern _Times__Times_param_i32_i16
_Times__Times_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movzwl	-22(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1354
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1355
L1354:
	movl	$20129, %edi
	call	__m3_fault
L1355:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i32
	.private_extern _Times__uTimes_var_i32_i32
_Times__uTimes_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i32
	.private_extern _Times__Times_var_i32_i32
_Times__Times_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	188+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1360
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1361
L1360:
	movl	$20193, %edi
	call	__m3_fault
L1361:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i32
	.private_extern _Times__uTimes_param_i32_i32
_Times__uTimes_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i32
	.private_extern _Times__Times_param_i32_i32
_Times__Times_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1366
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1367
L1366:
	movl	$20257, %edi
	call	__m3_fault
L1367:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times_M3
_Times_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Times(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Times(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Times_M3"
	.space 1
	.ascii "Times_param_i32_i32"
	.space 1
	.ascii "uTimes_param_i32_i32"
	.space 1
	.ascii "Times_var_i32_i32"
	.space 1
	.ascii "uTimes_var_i32_i32"
	.space 1
	.ascii "Times_param_i32_i16"
	.space 1
	.ascii "uTimes_param_i32_i16"
	.space 1
	.ascii "Times_var_i32_i16"
	.space 1
	.ascii "uTimes_var_i32_i16"
	.space 1
	.ascii "Times_param_i32_L"
	.space 1
	.ascii "uTimes_param_i32_L"
	.space 1
	.ascii "Times_var_i32_L"
	.space 1
	.ascii "uTimes_var_i32_L"
	.space 1
	.ascii "Times_param_i32_u64"
	.space 1
	.ascii "uTimes_param_i32_u64"
	.space 1
	.ascii "Times_var_i32_u64"
	.space 1
	.ascii "uTimes_var_i32_u64"
	.space 1
	.ascii "Times_param_i32_u16"
	.space 1
	.ascii "uTimes_param_i32_u16"
	.space 1
	.ascii "Times_var_i32_u16"
	.space 1
	.ascii "uTimes_var_i32_u16"
	.space 1
	.ascii "Times_param_i32_C"
	.space 1
	.ascii "uTimes_param_i32_C"
	.space 1
	.ascii "Times_var_i32_C"
	.space 1
	.ascii "uTimes_var_i32_C"
	.space 1
	.ascii "Times_param_i32_u32"
	.space 1
	.ascii "uTimes_param_i32_u32"
	.space 1
	.ascii "Times_var_i32_u32"
	.space 1
	.ascii "uTimes_var_i32_u32"
	.space 1
	.ascii "Times_param_i32_i8"
	.space 1
	.ascii "uTimes_param_i32_i8"
	.space 1
	.ascii "Times_var_i32_i8"
	.space 1
	.ascii "uTimes_var_i32_i8"
	.space 1
	.ascii "Times_param_i32_I"
	.space 1
	.ascii "uTimes_param_i32_I"
	.space 1
	.ascii "Times_var_i32_I"
	.space 1
	.ascii "uTimes_var_i32_I"
	.space 1
	.ascii "Times_param_i32_u8"
	.space 1
	.ascii "uTimes_param_i32_u8"
	.space 1
	.ascii "Times_var_i32_u8"
	.space 1
	.ascii "uTimes_var_i32_u8"
	.space 1
	.ascii "Times_param_i32_LC"
	.space 1
	.ascii "uTimes_param_i32_LC"
	.space 1
	.ascii "Times_var_i32_LC"
	.space 1
	.ascii "uTimes_var_i32_LC"
	.space 1
	.ascii "Times_param_i32_i64"
	.space 1
	.ascii "uTimes_param_i32_i64"
	.space 1
	.ascii "Times_var_i32_i64"
	.space 1
	.ascii "uTimes_var_i32_i64"
	.space 1
	.ascii "Times_param_i16_i32"
	.space 1
	.ascii "uTimes_param_i16_i32"
	.space 1
	.ascii "Times_var_i16_i32"
	.space 1
	.ascii "uTimes_var_i16_i32"
	.space 1
	.ascii "Times_param_i16_i16"
	.space 1
	.ascii "uTimes_param_i16_i16"
	.space 1
	.ascii "Times_var_i16_i16"
	.space 1
	.ascii "uTimes_var_i16_i16"
	.space 1
	.ascii "Times_param_i16_L"
	.space 1
	.ascii "uTimes_param_i16_L"
	.space 1
	.ascii "Times_var_i16_L"
	.space 1
	.ascii "uTimes_var_i16_L"
	.space 1
	.ascii "Times_param_i16_u64"
	.space 1
	.ascii "uTimes_param_i16_u64"
	.space 1
	.ascii "Times_var_i16_u64"
	.space 1
	.ascii "uTimes_var_i16_u64"
	.space 1
	.ascii "Times_param_i16_u16"
	.space 1
	.ascii "uTimes_param_i16_u16"
	.space 1
	.ascii "Times_var_i16_u16"
	.space 1
	.ascii "uTimes_var_i16_u16"
	.space 1
	.ascii "Times_param_i16_C"
	.space 1
	.ascii "uTimes_param_i16_C"
	.space 1
	.ascii "Times_var_i16_C"
	.space 1
	.ascii "uTimes_var_i16_C"
	.space 1
	.ascii "Times_param_i16_u32"
	.space 1
	.ascii "uTimes_param_i16_u32"
	.space 1
	.ascii "Times_var_i16_u32"
	.space 1
	.ascii "uTimes_var_i16_u32"
	.space 1
	.ascii "Times_param_i16_i8"
	.space 1
	.ascii "uTimes_param_i16_i8"
	.space 1
	.ascii "Times_var_i16_i8"
	.space 1
	.ascii "uTimes_var_i16_i8"
	.space 1
	.ascii "Times_param_i16_I"
	.space 1
	.ascii "uTimes_param_i16_I"
	.space 1
	.ascii "Times_var_i16_I"
	.space 1
	.ascii "uTimes_var_i16_I"
	.space 1
	.ascii "Times_param_i16_u8"
	.space 1
	.ascii "uTimes_param_i16_u8"
	.space 1
	.ascii "Times_var_i16_u8"
	.space 1
	.ascii "uTimes_var_i16_u8"
	.space 1
	.ascii "Times_param_i16_LC"
	.space 1
	.ascii "uTimes_param_i16_LC"
	.space 1
	.ascii "Times_var_i16_LC"
	.space 1
	.ascii "uTimes_var_i16_LC"
	.space 1
	.ascii "Times_param_i16_i64"
	.space 1
	.ascii "uTimes_param_i16_i64"
	.space 1
	.ascii "Times_var_i16_i64"
	.space 1
	.ascii "uTimes_var_i16_i64"
	.space 1
	.ascii "Times_param_L_i32"
	.space 1
	.ascii "uTimes_param_L_i32"
	.space 1
	.ascii "Times_var_L_i32"
	.space 1
	.ascii "uTimes_var_L_i32"
	.space 1
	.ascii "Times_param_L_i16"
	.space 1
	.ascii "uTimes_param_L_i16"
	.space 1
	.ascii "Times_var_L_i16"
	.space 1
	.ascii "uTimes_var_L_i16"
	.space 1
	.ascii "Times_param_L_L"
	.space 1
	.ascii "uTimes_param_L_L"
	.space 1
	.ascii "Times_var_L_L"
	.space 1
	.ascii "uTimes_var_L_L"
	.space 1
	.ascii "Times_param_L_u64"
	.space 1
	.ascii "uTimes_param_L_u64"
	.space 1
	.ascii "Times_var_L_u64"
	.space 1
	.ascii "uTimes_var_L_u64"
	.space 1
	.ascii "Times_param_L_u16"
	.space 1
	.ascii "uTimes_param_L_u16"
	.space 1
	.ascii "Times_var_L_u16"
	.space 1
	.ascii "uTimes_var_L_u16"
	.space 1
	.ascii "Times_param_L_C"
	.space 1
	.ascii "uTimes_param_L_C"
	.space 1
	.ascii "Times_var_L_C"
	.space 1
	.ascii "uTimes_var_L_C"
	.space 1
	.ascii "Times_param_L_u32"
	.space 1
	.ascii "uTimes_param_L_u32"
	.space 1
	.ascii "Times_var_L_u32"
	.space 1
	.ascii "uTimes_var_L_u32"
	.space 1
	.ascii "Times_param_L_i8"
	.space 1
	.ascii "uTimes_param_L_i8"
	.space 1
	.ascii "Times_var_L_i8"
	.space 1
	.ascii "uTimes_var_L_i8"
	.space 1
	.ascii "Times_param_L_I"
	.space 1
	.ascii "uTimes_param_L_I"
	.space 1
	.ascii "Times_var_L_I"
	.space 1
	.ascii "uTimes_var_L_I"
	.space 1
	.ascii "Times_param_L_u8"
	.space 1
	.ascii "uTimes_param_L_u8"
	.space 1
	.ascii "Times_var_L_u8"
	.space 1
	.ascii "uTimes_var_L_u8"
	.space 1
	.ascii "Times_param_L_LC"
	.space 1
	.ascii "uTimes_param_L_LC"
	.space 1
	.ascii "Times_var_L_LC"
	.space 1
	.ascii "uTimes_var_L_LC"
	.space 1
	.ascii "Times_param_L_i64"
	.space 1
	.ascii "uTimes_param_L_i64"
	.space 1
	.ascii "Times_var_L_i64"
	.space 1
	.ascii "uTimes_var_L_i64"
	.space 1
	.ascii "Times_param_f64_f64"
	.space 1
	.ascii "Times_var_f64_f64"
	.space 1
	.ascii "Times_param_u64_i32"
	.space 1
	.ascii "uTimes_param_u64_i32"
	.space 1
	.ascii "Times_var_u64_i32"
	.space 1
	.ascii "uTimes_var_u64_i32"
	.space 1
	.ascii "Times_param_u64_i16"
	.space 1
	.ascii "uTimes_param_u64_i16"
	.space 1
	.ascii "Times_var_u64_i16"
	.space 1
	.ascii "uTimes_var_u64_i16"
	.space 1
	.ascii "Times_param_u64_L"
	.space 1
	.ascii "uTimes_param_u64_L"
	.space 1
	.ascii "Times_var_u64_L"
	.space 1
	.ascii "uTimes_var_u64_L"
	.space 1
	.ascii "Times_param_u64_u64"
	.space 1
	.ascii "uTimes_param_u64_u64"
	.space 1
	.ascii "Times_var_u64_u64"
	.space 1
	.ascii "uTimes_var_u64_u64"
	.space 1
	.ascii "Times_param_u64_u16"
	.space 1
	.ascii "uTimes_param_u64_u16"
	.space 1
	.ascii "Times_var_u64_u16"
	.space 1
	.ascii "uTimes_var_u64_u16"
	.space 1
	.ascii "Times_param_u64_C"
	.space 1
	.ascii "uTimes_param_u64_C"
	.space 1
	.ascii "Times_var_u64_C"
	.space 1
	.ascii "uTimes_var_u64_C"
	.space 1
	.ascii "Times_param_u64_u32"
	.space 1
	.ascii "uTimes_param_u64_u32"
	.space 1
	.ascii "Times_var_u64_u32"
	.space 1
	.ascii "uTimes_var_u64_u32"
	.space 1
	.ascii "Times_param_u64_i8"
	.space 1
	.ascii "uTimes_param_u64_i8"
	.space 1
	.ascii "Times_var_u64_i8"
	.space 1
	.ascii "uTimes_var_u64_i8"
	.space 1
	.ascii "Times_param_u64_I"
	.space 1
	.ascii "uTimes_param_u64_I"
	.space 1
	.ascii "Times_var_u64_I"
	.space 1
	.ascii "uTimes_var_u64_I"
	.space 1
	.ascii "Times_param_u64_u8"
	.space 1
	.ascii "uTimes_param_u64_u8"
	.space 1
	.ascii "Times_var_u64_u8"
	.space 1
	.ascii "uTimes_var_u64_u8"
	.space 1
	.ascii "Times_param_u64_LC"
	.space 1
	.ascii "uTimes_param_u64_LC"
	.space 1
	.ascii "Times_var_u64_LC"
	.space 1
	.ascii "uTimes_var_u64_LC"
	.space 1
	.ascii "Times_param_u64_i64"
	.space 1
	.ascii "uTimes_param_u64_i64"
	.space 1
	.ascii "Times_var_u64_i64"
	.space 1
	.ascii "uTimes_var_u64_i64"
	.space 1
	.ascii "Times_param_u16_i32"
	.space 1
	.ascii "uTimes_param_u16_i32"
	.space 1
	.ascii "Times_var_u16_i32"
	.space 1
	.ascii "uTimes_var_u16_i32"
	.space 1
	.ascii "Times_param_u16_i16"
	.space 1
	.ascii "uTimes_param_u16_i16"
	.space 1
	.ascii "Times_var_u16_i16"
	.space 1
	.ascii "uTimes_var_u16_i16"
	.space 1
	.ascii "Times_param_u16_L"
	.space 1
	.ascii "uTimes_param_u16_L"
	.space 1
	.ascii "Times_var_u16_L"
	.space 1
	.ascii "uTimes_var_u16_L"
	.space 1
	.ascii "Times_param_u16_u64"
	.space 1
	.ascii "uTimes_param_u16_u64"
	.space 1
	.ascii "Times_var_u16_u64"
	.space 1
	.ascii "uTimes_var_u16_u64"
	.space 1
	.ascii "Times_param_u16_u16"
	.space 1
	.ascii "uTimes_param_u16_u16"
	.space 1
	.ascii "Times_var_u16_u16"
	.space 1
	.ascii "uTimes_var_u16_u16"
	.space 1
	.ascii "Times_param_u16_C"
	.space 1
	.ascii "uTimes_param_u16_C"
	.space 1
	.ascii "Times_var_u16_C"
	.space 1
	.ascii "uTimes_var_u16_C"
	.space 1
	.ascii "Times_param_u16_u32"
	.space 1
	.ascii "uTimes_param_u16_u32"
	.space 1
	.ascii "Times_var_u16_u32"
	.space 1
	.ascii "uTimes_var_u16_u32"
	.space 1
	.ascii "Times_param_u16_i8"
	.space 1
	.ascii "uTimes_param_u16_i8"
	.space 1
	.ascii "Times_var_u16_i8"
	.space 1
	.ascii "uTimes_var_u16_i8"
	.space 1
	.ascii "Times_param_u16_I"
	.space 1
	.ascii "uTimes_param_u16_I"
	.space 1
	.ascii "Times_var_u16_I"
	.space 1
	.ascii "uTimes_var_u16_I"
	.space 1
	.ascii "Times_param_u16_u8"
	.space 1
	.ascii "uTimes_param_u16_u8"
	.space 1
	.ascii "Times_var_u16_u8"
	.space 1
	.ascii "uTimes_var_u16_u8"
	.space 1
	.ascii "Times_param_u16_LC"
	.space 1
	.ascii "uTimes_param_u16_LC"
	.space 1
	.ascii "Times_var_u16_LC"
	.space 1
	.ascii "uTimes_var_u16_LC"
	.space 1
	.ascii "Times_param_u16_i64"
	.space 1
	.ascii "uTimes_param_u16_i64"
	.space 1
	.ascii "Times_var_u16_i64"
	.space 1
	.ascii "uTimes_var_u16_i64"
	.space 1
	.ascii "Times_param_C_i32"
	.space 1
	.ascii "uTimes_param_C_i32"
	.space 1
	.ascii "Times_var_C_i32"
	.space 1
	.ascii "uTimes_var_C_i32"
	.space 1
	.ascii "Times_param_C_i16"
	.space 1
	.ascii "uTimes_param_C_i16"
	.space 1
	.ascii "Times_var_C_i16"
	.space 1
	.ascii "uTimes_var_C_i16"
	.space 1
	.ascii "Times_param_C_L"
	.space 1
	.ascii "uTimes_param_C_L"
	.space 1
	.ascii "Times_var_C_L"
	.space 1
	.ascii "uTimes_var_C_L"
	.space 1
	.ascii "Times_param_C_u64"
	.space 1
	.ascii "uTimes_param_C_u64"
	.space 1
	.ascii "Times_var_C_u64"
	.space 1
	.ascii "uTimes_var_C_u64"
	.space 1
	.ascii "Times_param_C_u16"
	.space 1
	.ascii "uTimes_param_C_u16"
	.space 1
	.ascii "Times_var_C_u16"
	.space 1
	.ascii "uTimes_var_C_u16"
	.space 1
	.ascii "Times_param_C_C"
	.space 1
	.ascii "uTimes_param_C_C"
	.space 1
	.ascii "Times_var_C_C"
	.space 1
	.ascii "uTimes_var_C_C"
	.space 1
	.ascii "Times_param_C_u32"
	.space 1
	.ascii "uTimes_param_C_u32"
	.space 1
	.ascii "Times_var_C_u32"
	.space 1
	.ascii "uTimes_var_C_u32"
	.space 1
	.ascii "Times_param_C_i8"
	.space 1
	.ascii "uTimes_param_C_i8"
	.space 1
	.ascii "Times_var_C_i8"
	.space 1
	.ascii "uTimes_var_C_i8"
	.space 1
	.ascii "Times_param_C_I"
	.space 1
	.ascii "uTimes_param_C_I"
	.space 1
	.ascii "Times_var_C_I"
	.space 1
	.ascii "uTimes_var_C_I"
	.space 1
	.ascii "Times_param_C_u8"
	.space 1
	.ascii "uTimes_param_C_u8"
	.space 1
	.ascii "Times_var_C_u8"
	.space 1
	.ascii "uTimes_var_C_u8"
	.space 1
	.ascii "Times_param_C_LC"
	.space 1
	.ascii "uTimes_param_C_LC"
	.space 1
	.ascii "Times_var_C_LC"
	.space 1
	.ascii "uTimes_var_C_LC"
	.space 1
	.ascii "Times_param_C_i64"
	.space 1
	.ascii "uTimes_param_C_i64"
	.space 1
	.ascii "Times_var_C_i64"
	.space 1
	.ascii "uTimes_var_C_i64"
	.space 1
	.ascii "Times_param_u32_i32"
	.space 1
	.ascii "uTimes_param_u32_i32"
	.space 1
	.ascii "Times_var_u32_i32"
	.space 1
	.ascii "uTimes_var_u32_i32"
	.space 1
	.ascii "Times_param_u32_i16"
	.space 1
	.ascii "uTimes_param_u32_i16"
	.space 1
	.ascii "Times_var_u32_i16"
	.space 1
	.ascii "uTimes_var_u32_i16"
	.space 1
	.ascii "Times_param_u32_L"
	.space 1
	.ascii "uTimes_param_u32_L"
	.space 1
	.ascii "Times_var_u32_L"
	.space 1
	.ascii "uTimes_var_u32_L"
	.space 1
	.ascii "Times_param_u32_u64"
	.space 1
	.ascii "uTimes_param_u32_u64"
	.space 1
	.ascii "Times_var_u32_u64"
	.space 1
	.ascii "uTimes_var_u32_u64"
	.space 1
	.ascii "Times_param_u32_u16"
	.space 1
	.ascii "uTimes_param_u32_u16"
	.space 1
	.ascii "Times_var_u32_u16"
	.space 1
	.ascii "uTimes_var_u32_u16"
	.space 1
	.ascii "Times_param_u32_C"
	.space 1
	.ascii "uTimes_param_u32_C"
	.space 1
	.ascii "Times_var_u32_C"
	.space 1
	.ascii "uTimes_var_u32_C"
	.space 1
	.ascii "Times_param_u32_u32"
	.space 1
	.ascii "uTimes_param_u32_u32"
	.space 1
	.ascii "Times_var_u32_u32"
	.space 1
	.ascii "uTimes_var_u32_u32"
	.space 1
	.ascii "Times_param_u32_i8"
	.space 1
	.ascii "uTimes_param_u32_i8"
	.space 1
	.ascii "Times_var_u32_i8"
	.space 1
	.ascii "uTimes_var_u32_i8"
	.space 1
	.ascii "Times_param_u32_I"
	.space 1
	.ascii "uTimes_param_u32_I"
	.space 1
	.ascii "Times_var_u32_I"
	.space 1
	.ascii "uTimes_var_u32_I"
	.space 1
	.ascii "Times_param_u32_u8"
	.space 1
	.ascii "uTimes_param_u32_u8"
	.space 1
	.ascii "Times_var_u32_u8"
	.space 1
	.ascii "uTimes_var_u32_u8"
	.space 1
	.ascii "Times_param_u32_LC"
	.space 1
	.ascii "uTimes_param_u32_LC"
	.space 1
	.ascii "Times_var_u32_LC"
	.space 1
	.ascii "uTimes_var_u32_LC"
	.space 1
	.ascii "Times_param_u32_i64"
	.space 1
	.ascii "uTimes_param_u32_i64"
	.space 1
	.ascii "Times_var_u32_i64"
	.space 1
	.ascii "uTimes_var_u32_i64"
	.space 1
	.ascii "Times_param_i8_i32"
	.space 1
	.ascii "uTimes_param_i8_i32"
	.space 1
	.ascii "Times_var_i8_i32"
	.space 1
	.ascii "uTimes_var_i8_i32"
	.space 1
	.ascii "Times_param_i8_i16"
	.space 1
	.ascii "uTimes_param_i8_i16"
	.space 1
	.ascii "Times_var_i8_i16"
	.space 1
	.ascii "uTimes_var_i8_i16"
	.space 1
	.ascii "Times_param_i8_L"
	.space 1
	.ascii "uTimes_param_i8_L"
	.space 1
	.ascii "Times_var_i8_L"
	.space 1
	.ascii "uTimes_var_i8_L"
	.space 1
	.ascii "Times_param_i8_u64"
	.space 1
	.ascii "uTimes_param_i8_u64"
	.space 1
	.ascii "Times_var_i8_u64"
	.space 1
	.ascii "uTimes_var_i8_u64"
	.space 1
	.ascii "Times_param_i8_u16"
	.space 1
	.ascii "uTimes_param_i8_u16"
	.space 1
	.ascii "Times_var_i8_u16"
	.space 1
	.ascii "uTimes_var_i8_u16"
	.space 1
	.ascii "Times_param_i8_C"
	.space 1
	.ascii "uTimes_param_i8_C"
	.space 1
	.ascii "Times_var_i8_C"
	.space 1
	.ascii "uTimes_var_i8_C"
	.space 1
	.ascii "Times_param_i8_u32"
	.space 1
	.ascii "uTimes_param_i8_u32"
	.space 1
	.ascii "Times_var_i8_u32"
	.space 1
	.ascii "uTimes_var_i8_u32"
	.space 1
	.ascii "Times_param_i8_i8"
	.space 1
	.ascii "uTimes_param_i8_i8"
	.space 1
	.ascii "Times_var_i8_i8"
	.space 1
	.ascii "uTimes_var_i8_i8"
	.space 1
	.ascii "Times_param_i8_I"
	.space 1
	.ascii "uTimes_param_i8_I"
	.space 1
	.ascii "Times_var_i8_I"
	.space 1
	.ascii "uTimes_var_i8_I"
	.space 1
	.ascii "Times_param_i8_u8"
	.space 1
	.ascii "uTimes_param_i8_u8"
	.space 1
	.ascii "Times_var_i8_u8"
	.space 1
	.ascii "uTimes_var_i8_u8"
	.space 1
	.ascii "Times_param_i8_LC"
	.space 1
	.ascii "uTimes_param_i8_LC"
	.space 1
	.ascii "Times_var_i8_LC"
	.space 1
	.ascii "uTimes_var_i8_LC"
	.space 1
	.ascii "Times_param_i8_i64"
	.space 1
	.ascii "uTimes_param_i8_i64"
	.space 1
	.ascii "Times_var_i8_i64"
	.space 1
	.ascii "uTimes_var_i8_i64"
	.space 1
	.ascii "Times_param_I_i32"
	.space 1
	.ascii "uTimes_param_I_i32"
	.space 1
	.ascii "Times_var_I_i32"
	.space 1
	.ascii "uTimes_var_I_i32"
	.space 1
	.ascii "Times_param_I_i16"
	.space 1
	.ascii "uTimes_param_I_i16"
	.space 1
	.ascii "Times_var_I_i16"
	.space 1
	.ascii "uTimes_var_I_i16"
	.space 1
	.ascii "Times_param_I_L"
	.space 1
	.ascii "uTimes_param_I_L"
	.space 1
	.ascii "Times_var_I_L"
	.space 1
	.ascii "uTimes_var_I_L"
	.space 1
	.ascii "Times_param_I_u64"
	.space 1
	.ascii "uTimes_param_I_u64"
	.space 1
	.ascii "Times_var_I_u64"
	.space 1
	.ascii "uTimes_var_I_u64"
	.space 1
	.ascii "Times_param_I_u16"
	.space 1
	.ascii "uTimes_param_I_u16"
	.space 1
	.ascii "Times_var_I_u16"
	.space 1
	.ascii "uTimes_var_I_u16"
	.space 1
	.ascii "Times_param_I_C"
	.space 1
	.ascii "uTimes_param_I_C"
	.space 1
	.ascii "Times_var_I_C"
	.space 1
	.ascii "uTimes_var_I_C"
	.space 1
	.ascii "Times_param_I_u32"
	.space 1
	.ascii "uTimes_param_I_u32"
	.space 1
	.ascii "Times_var_I_u32"
	.space 1
	.ascii "uTimes_var_I_u32"
	.space 1
	.ascii "Times_param_I_i8"
	.space 1
	.ascii "uTimes_param_I_i8"
	.space 1
	.ascii "Times_var_I_i8"
	.space 1
	.ascii "uTimes_var_I_i8"
	.space 1
	.ascii "Times_param_I_I"
	.space 1
	.ascii "uTimes_param_I_I"
	.space 1
	.ascii "Times_var_I_I"
	.space 1
	.ascii "uTimes_var_I_I"
	.space 1
	.ascii "Times_param_I_u8"
	.space 1
	.ascii "uTimes_param_I_u8"
	.space 1
	.ascii "Times_var_I_u8"
	.space 1
	.ascii "uTimes_var_I_u8"
	.space 1
	.ascii "Times_param_I_LC"
	.space 1
	.ascii "uTimes_param_I_LC"
	.space 1
	.ascii "Times_var_I_LC"
	.space 1
	.ascii "uTimes_var_I_LC"
	.space 1
	.ascii "Times_param_I_i64"
	.space 1
	.ascii "uTimes_param_I_i64"
	.space 1
	.ascii "Times_var_I_i64"
	.space 1
	.ascii "uTimes_var_I_i64"
	.space 1
	.ascii "Times_param_u8_i32"
	.space 1
	.ascii "uTimes_param_u8_i32"
	.space 1
	.ascii "Times_var_u8_i32"
	.space 1
	.ascii "uTimes_var_u8_i32"
	.space 1
	.ascii "Times_param_u8_i16"
	.space 1
	.ascii "uTimes_param_u8_i16"
	.space 1
	.ascii "Times_var_u8_i16"
	.space 1
	.ascii "uTimes_var_u8_i16"
	.space 1
	.ascii "Times_param_u8_L"
	.space 1
	.ascii "uTimes_param_u8_L"
	.space 1
	.ascii "Times_var_u8_L"
	.space 1
	.ascii "uTimes_var_u8_L"
	.space 1
	.ascii "Times_param_u8_u64"
	.space 1
	.ascii "uTimes_param_u8_u64"
	.space 1
	.ascii "Times_var_u8_u64"
	.space 1
	.ascii "uTimes_var_u8_u64"
	.space 1
	.ascii "Times_param_u8_u16"
	.space 1
	.ascii "uTimes_param_u8_u16"
	.space 1
	.ascii "Times_var_u8_u16"
	.space 1
	.ascii "uTimes_var_u8_u16"
	.space 1
	.ascii "Times_param_u8_C"
	.space 1
	.ascii "uTimes_param_u8_C"
	.space 1
	.ascii "Times_var_u8_C"
	.space 1
	.ascii "uTimes_var_u8_C"
	.space 1
	.ascii "Times_param_u8_u32"
	.space 1
	.ascii "uTimes_param_u8_u32"
	.space 1
	.ascii "Times_var_u8_u32"
	.space 1
	.ascii "uTimes_var_u8_u32"
	.space 1
	.ascii "Times_param_u8_i8"
	.space 1
	.ascii "uTimes_param_u8_i8"
	.space 1
	.ascii "Times_var_u8_i8"
	.space 1
	.ascii "uTimes_var_u8_i8"
	.space 1
	.ascii "Times_param_u8_I"
	.space 1
	.ascii "uTimes_param_u8_I"
	.space 1
	.ascii "Times_var_u8_I"
	.space 1
	.ascii "uTimes_var_u8_I"
	.space 1
	.ascii "Times_param_u8_u8"
	.space 1
	.ascii "uTimes_param_u8_u8"
	.space 1
	.ascii "Times_var_u8_u8"
	.space 1
	.ascii "uTimes_var_u8_u8"
	.space 1
	.ascii "Times_param_u8_LC"
	.space 1
	.ascii "uTimes_param_u8_LC"
	.space 1
	.ascii "Times_var_u8_LC"
	.space 1
	.ascii "uTimes_var_u8_LC"
	.space 1
	.ascii "Times_param_u8_i64"
	.space 1
	.ascii "uTimes_param_u8_i64"
	.space 1
	.ascii "Times_var_u8_i64"
	.space 1
	.ascii "uTimes_var_u8_i64"
	.space 1
	.ascii "Times_param_f32_f32"
	.space 1
	.ascii "Times_var_f32_f32"
	.space 1
	.ascii "Times_param_LC_i32"
	.space 1
	.ascii "uTimes_param_LC_i32"
	.space 1
	.ascii "Times_var_LC_i32"
	.space 1
	.ascii "uTimes_var_LC_i32"
	.space 1
	.ascii "Times_param_LC_i16"
	.space 1
	.ascii "uTimes_param_LC_i16"
	.space 1
	.ascii "Times_var_LC_i16"
	.space 1
	.ascii "uTimes_var_LC_i16"
	.space 1
	.ascii "Times_param_LC_L"
	.space 1
	.ascii "uTimes_param_LC_L"
	.space 1
	.ascii "Times_var_LC_L"
	.space 1
	.ascii "uTimes_var_LC_L"
	.space 1
	.ascii "Times_param_LC_u64"
	.space 1
	.ascii "uTimes_param_LC_u64"
	.space 1
	.ascii "Times_var_LC_u64"
	.space 1
	.ascii "uTimes_var_LC_u64"
	.space 1
	.ascii "Times_param_LC_u16"
	.space 1
	.ascii "uTimes_param_LC_u16"
	.space 1
	.ascii "Times_var_LC_u16"
	.space 1
	.ascii "uTimes_var_LC_u16"
	.space 1
	.ascii "Times_param_LC_C"
	.space 1
	.ascii "uTimes_param_LC_C"
	.space 1
	.ascii "Times_var_LC_C"
	.space 1
	.ascii "uTimes_var_LC_C"
	.space 1
	.ascii "Times_param_LC_u32"
	.space 1
	.ascii "uTimes_param_LC_u32"
	.space 1
	.ascii "Times_var_LC_u32"
	.space 1
	.ascii "uTimes_var_LC_u32"
	.space 1
	.ascii "Times_param_LC_i8"
	.space 1
	.ascii "uTimes_param_LC_i8"
	.space 1
	.ascii "Times_var_LC_i8"
	.space 1
	.ascii "uTimes_var_LC_i8"
	.space 1
	.ascii "Times_param_LC_I"
	.space 1
	.ascii "uTimes_param_LC_I"
	.space 1
	.ascii "Times_var_LC_I"
	.space 1
	.ascii "uTimes_var_LC_I"
	.space 1
	.ascii "Times_param_LC_u8"
	.space 1
	.ascii "uTimes_param_LC_u8"
	.space 1
	.ascii "Times_var_LC_u8"
	.space 1
	.ascii "uTimes_var_LC_u8"
	.space 1
	.ascii "Times_param_LC_LC"
	.space 1
	.ascii "uTimes_param_LC_LC"
	.space 1
	.ascii "Times_var_LC_LC"
	.space 1
	.ascii "uTimes_var_LC_LC"
	.space 1
	.ascii "Times_param_LC_i64"
	.space 1
	.ascii "uTimes_param_LC_i64"
	.space 1
	.ascii "Times_var_LC_i64"
	.space 1
	.ascii "uTimes_var_LC_i64"
	.space 1
	.ascii "Times_param_i64_i32"
	.space 1
	.ascii "uTimes_param_i64_i32"
	.space 1
	.ascii "Times_var_i64_i32"
	.space 1
	.ascii "uTimes_var_i64_i32"
	.space 1
	.ascii "Times_param_i64_i16"
	.space 1
	.ascii "uTimes_param_i64_i16"
	.space 1
	.ascii "Times_var_i64_i16"
	.space 1
	.ascii "uTimes_var_i64_i16"
	.space 1
	.ascii "Times_param_i64_L"
	.space 1
	.ascii "uTimes_param_i64_L"
	.space 1
	.ascii "Times_var_i64_L"
	.space 1
	.ascii "uTimes_var_i64_L"
	.space 1
	.ascii "Times_param_i64_u64"
	.space 1
	.ascii "uTimes_param_i64_u64"
	.space 1
	.ascii "Times_var_i64_u64"
	.space 1
	.ascii "uTimes_var_i64_u64"
	.space 1
	.ascii "Times_param_i64_u16"
	.space 1
	.ascii "uTimes_param_i64_u16"
	.space 1
	.ascii "Times_var_i64_u16"
	.space 1
	.ascii "uTimes_var_i64_u16"
	.space 1
	.ascii "Times_param_i64_C"
	.space 1
	.ascii "uTimes_param_i64_C"
	.space 1
	.ascii "Times_var_i64_C"
	.space 1
	.ascii "uTimes_var_i64_C"
	.space 1
	.ascii "Times_param_i64_u32"
	.space 1
	.ascii "uTimes_param_i64_u32"
	.space 1
	.ascii "Times_var_i64_u32"
	.space 1
	.ascii "uTimes_var_i64_u32"
	.space 1
	.ascii "Times_param_i64_i8"
	.space 1
	.ascii "uTimes_param_i64_i8"
	.space 1
	.ascii "Times_var_i64_i8"
	.space 1
	.ascii "uTimes_var_i64_i8"
	.space 1
	.ascii "Times_param_i64_I"
	.space 1
	.ascii "uTimes_param_i64_I"
	.space 1
	.ascii "Times_var_i64_I"
	.space 1
	.ascii "uTimes_var_i64_I"
	.space 1
	.ascii "Times_param_i64_u8"
	.space 1
	.ascii "uTimes_param_i64_u8"
	.space 1
	.ascii "Times_var_i64_u8"
	.space 1
	.ascii "uTimes_var_i64_u8"
	.space 1
	.ascii "Times_param_i64_LC"
	.space 1
	.ascii "uTimes_param_i64_LC"
	.space 1
	.ascii "Times_var_i64_LC"
	.space 1
	.ascii "uTimes_var_i64_LC"
	.space 1
	.ascii "Times_param_i64_i64"
	.space 1
	.ascii "uTimes_param_i64_i64"
	.space 1
	.ascii "Times_var_i64_i64"
	.space 1
	.ascii "uTimes_var_i64_i64"
	.space 4
	.quad	_Times_M3
	.quad	_L_1
	.quad	_Times__Times_param_i32_i32
	.quad	_L_1+9
	.quad	_Times__uTimes_param_i32_i32
	.quad	_L_1+29
	.quad	_Times__Times_var_i32_i32
	.quad	_L_1+50
	.quad	_Times__uTimes_var_i32_i32
	.quad	_L_1+68
	.quad	_Times__Times_param_i32_i16
	.quad	_L_1+87
	.quad	_Times__uTimes_param_i32_i16
	.quad	_L_1+107
	.quad	_Times__Times_var_i32_i16
	.quad	_L_1+128
	.quad	_Times__uTimes_var_i32_i16
	.quad	_L_1+146
	.quad	_Times__Times_param_i32_L
	.quad	_L_1+165
	.quad	_Times__uTimes_param_i32_L
	.quad	_L_1+183
	.quad	_Times__Times_var_i32_L
	.quad	_L_1+202
	.quad	_Times__uTimes_var_i32_L
	.quad	_L_1+218
	.quad	_Times__Times_param_i32_u64
	.quad	_L_1+235
	.quad	_Times__uTimes_param_i32_u64
	.quad	_L_1+255
	.quad	_Times__Times_var_i32_u64
	.quad	_L_1+276
	.quad	_Times__uTimes_var_i32_u64
	.quad	_L_1+294
	.quad	_Times__Times_param_i32_u16
	.quad	_L_1+313
	.quad	_Times__uTimes_param_i32_u16
	.quad	_L_1+333
	.quad	_Times__Times_var_i32_u16
	.quad	_L_1+354
	.quad	_Times__uTimes_var_i32_u16
	.quad	_L_1+372
	.quad	_Times__Times_param_i32_C
	.quad	_L_1+391
	.quad	_Times__uTimes_param_i32_C
	.quad	_L_1+409
	.quad	_Times__Times_var_i32_C
	.quad	_L_1+428
	.quad	_Times__uTimes_var_i32_C
	.quad	_L_1+444
	.quad	_Times__Times_param_i32_u32
	.quad	_L_1+461
	.quad	_Times__uTimes_param_i32_u32
	.quad	_L_1+481
	.quad	_Times__Times_var_i32_u32
	.quad	_L_1+502
	.quad	_Times__uTimes_var_i32_u32
	.quad	_L_1+520
	.quad	_Times__Times_param_i32_i8
	.quad	_L_1+539
	.quad	_Times__uTimes_param_i32_i8
	.quad	_L_1+558
	.quad	_Times__Times_var_i32_i8
	.quad	_L_1+578
	.quad	_Times__uTimes_var_i32_i8
	.quad	_L_1+595
	.quad	_Times__Times_param_i32_I
	.quad	_L_1+613
	.quad	_Times__uTimes_param_i32_I
	.quad	_L_1+631
	.quad	_Times__Times_var_i32_I
	.quad	_L_1+650
	.quad	_Times__uTimes_var_i32_I
	.quad	_L_1+666
	.quad	_Times__Times_param_i32_u8
	.quad	_L_1+683
	.quad	_Times__uTimes_param_i32_u8
	.quad	_L_1+702
	.quad	_Times__Times_var_i32_u8
	.quad	_L_1+722
	.quad	_Times__uTimes_var_i32_u8
	.quad	_L_1+739
	.quad	_Times__Times_param_i32_LC
	.quad	_L_1+757
	.quad	_Times__uTimes_param_i32_LC
	.quad	_L_1+776
	.quad	_Times__Times_var_i32_LC
	.quad	_L_1+796
	.quad	_Times__uTimes_var_i32_LC
	.quad	_L_1+813
	.quad	_Times__Times_param_i32_i64
	.quad	_L_1+831
	.quad	_Times__uTimes_param_i32_i64
	.quad	_L_1+851
	.quad	_Times__Times_var_i32_i64
	.quad	_L_1+872
	.quad	_Times__uTimes_var_i32_i64
	.quad	_L_1+890
	.quad	_Times__Times_param_i16_i32
	.quad	_L_1+909
	.quad	_Times__uTimes_param_i16_i32
	.quad	_L_1+929
	.quad	_Times__Times_var_i16_i32
	.quad	_L_1+950
	.quad	_Times__uTimes_var_i16_i32
	.quad	_L_1+968
	.quad	_Times__Times_param_i16_i16
	.quad	_L_1+987
	.quad	_Times__uTimes_param_i16_i16
	.quad	_L_1+1007
	.quad	_Times__Times_var_i16_i16
	.quad	_L_1+1028
	.quad	_Times__uTimes_var_i16_i16
	.quad	_L_1+1046
	.quad	_Times__Times_param_i16_L
	.quad	_L_1+1065
	.quad	_Times__uTimes_param_i16_L
	.quad	_L_1+1083
	.quad	_Times__Times_var_i16_L
	.quad	_L_1+1102
	.quad	_Times__uTimes_var_i16_L
	.quad	_L_1+1118
	.quad	_Times__Times_param_i16_u64
	.quad	_L_1+1135
	.quad	_Times__uTimes_param_i16_u64
	.quad	_L_1+1155
	.quad	_Times__Times_var_i16_u64
	.quad	_L_1+1176
	.quad	_Times__uTimes_var_i16_u64
	.quad	_L_1+1194
	.quad	_Times__Times_param_i16_u16
	.quad	_L_1+1213
	.quad	_Times__uTimes_param_i16_u16
	.quad	_L_1+1233
	.quad	_Times__Times_var_i16_u16
	.quad	_L_1+1254
	.quad	_Times__uTimes_var_i16_u16
	.quad	_L_1+1272
	.quad	_Times__Times_param_i16_C
	.quad	_L_1+1291
	.quad	_Times__uTimes_param_i16_C
	.quad	_L_1+1309
	.quad	_Times__Times_var_i16_C
	.quad	_L_1+1328
	.quad	_Times__uTimes_var_i16_C
	.quad	_L_1+1344
	.quad	_Times__Times_param_i16_u32
	.quad	_L_1+1361
	.quad	_Times__uTimes_param_i16_u32
	.quad	_L_1+1381
	.quad	_Times__Times_var_i16_u32
	.quad	_L_1+1402
	.quad	_Times__uTimes_var_i16_u32
	.quad	_L_1+1420
	.quad	_Times__Times_param_i16_i8
	.quad	_L_1+1439
	.quad	_Times__uTimes_param_i16_i8
	.quad	_L_1+1458
	.quad	_Times__Times_var_i16_i8
	.quad	_L_1+1478
	.quad	_Times__uTimes_var_i16_i8
	.quad	_L_1+1495
	.quad	_Times__Times_param_i16_I
	.quad	_L_1+1513
	.quad	_Times__uTimes_param_i16_I
	.quad	_L_1+1531
	.quad	_Times__Times_var_i16_I
	.quad	_L_1+1550
	.quad	_Times__uTimes_var_i16_I
	.quad	_L_1+1566
	.quad	_Times__Times_param_i16_u8
	.quad	_L_1+1583
	.quad	_Times__uTimes_param_i16_u8
	.quad	_L_1+1602
	.quad	_Times__Times_var_i16_u8
	.quad	_L_1+1622
	.quad	_Times__uTimes_var_i16_u8
	.quad	_L_1+1639
	.quad	_Times__Times_param_i16_LC
	.quad	_L_1+1657
	.quad	_Times__uTimes_param_i16_LC
	.quad	_L_1+1676
	.quad	_Times__Times_var_i16_LC
	.quad	_L_1+1696
	.quad	_Times__uTimes_var_i16_LC
	.quad	_L_1+1713
	.quad	_Times__Times_param_i16_i64
	.quad	_L_1+1731
	.quad	_Times__uTimes_param_i16_i64
	.quad	_L_1+1751
	.quad	_Times__Times_var_i16_i64
	.quad	_L_1+1772
	.quad	_Times__uTimes_var_i16_i64
	.quad	_L_1+1790
	.quad	_Times__Times_param_L_i32
	.quad	_L_1+1809
	.quad	_Times__uTimes_param_L_i32
	.quad	_L_1+1827
	.quad	_Times__Times_var_L_i32
	.quad	_L_1+1846
	.quad	_Times__uTimes_var_L_i32
	.quad	_L_1+1862
	.quad	_Times__Times_param_L_i16
	.quad	_L_1+1879
	.quad	_Times__uTimes_param_L_i16
	.quad	_L_1+1897
	.quad	_Times__Times_var_L_i16
	.quad	_L_1+1916
	.quad	_Times__uTimes_var_L_i16
	.quad	_L_1+1932
	.quad	_Times__Times_param_L_L
	.quad	_L_1+1949
	.quad	_Times__uTimes_param_L_L
	.quad	_L_1+1965
	.quad	_Times__Times_var_L_L
	.quad	_L_1+1982
	.quad	_Times__uTimes_var_L_L
	.quad	_L_1+1996
	.quad	_Times__Times_param_L_u64
	.quad	_L_1+2011
	.quad	_Times__uTimes_param_L_u64
	.quad	_L_1+2029
	.quad	_Times__Times_var_L_u64
	.quad	_L_1+2048
	.quad	_Times__uTimes_var_L_u64
	.quad	_L_1+2064
	.quad	_Times__Times_param_L_u16
	.quad	_L_1+2081
	.quad	_Times__uTimes_param_L_u16
	.quad	_L_1+2099
	.quad	_Times__Times_var_L_u16
	.quad	_L_1+2118
	.quad	_Times__uTimes_var_L_u16
	.quad	_L_1+2134
	.quad	_Times__Times_param_L_C
	.quad	_L_1+2151
	.quad	_Times__uTimes_param_L_C
	.quad	_L_1+2167
	.quad	_Times__Times_var_L_C
	.quad	_L_1+2184
	.quad	_Times__uTimes_var_L_C
	.quad	_L_1+2198
	.quad	_Times__Times_param_L_u32
	.quad	_L_1+2213
	.quad	_Times__uTimes_param_L_u32
	.quad	_L_1+2231
	.quad	_Times__Times_var_L_u32
	.quad	_L_1+2250
	.quad	_Times__uTimes_var_L_u32
	.quad	_L_1+2266
	.quad	_Times__Times_param_L_i8
	.quad	_L_1+2283
	.quad	_Times__uTimes_param_L_i8
	.quad	_L_1+2300
	.quad	_Times__Times_var_L_i8
	.quad	_L_1+2318
	.quad	_Times__uTimes_var_L_i8
	.quad	_L_1+2333
	.quad	_Times__Times_param_L_I
	.quad	_L_1+2349
	.quad	_Times__uTimes_param_L_I
	.quad	_L_1+2365
	.quad	_Times__Times_var_L_I
	.quad	_L_1+2382
	.quad	_Times__uTimes_var_L_I
	.quad	_L_1+2396
	.quad	_Times__Times_param_L_u8
	.quad	_L_1+2411
	.quad	_Times__uTimes_param_L_u8
	.quad	_L_1+2428
	.quad	_Times__Times_var_L_u8
	.quad	_L_1+2446
	.quad	_Times__uTimes_var_L_u8
	.quad	_L_1+2461
	.quad	_Times__Times_param_L_LC
	.quad	_L_1+2477
	.quad	_Times__uTimes_param_L_LC
	.quad	_L_1+2494
	.quad	_Times__Times_var_L_LC
	.quad	_L_1+2512
	.quad	_Times__uTimes_var_L_LC
	.quad	_L_1+2527
	.quad	_Times__Times_param_L_i64
	.quad	_L_1+2543
	.quad	_Times__uTimes_param_L_i64
	.quad	_L_1+2561
	.quad	_Times__Times_var_L_i64
	.quad	_L_1+2580
	.quad	_Times__uTimes_var_L_i64
	.quad	_L_1+2596
	.quad	_Times__Times_param_f64_f64
	.quad	_L_1+2613
	.quad	_Times__Times_var_f64_f64
	.quad	_L_1+2633
	.quad	_Times__Times_param_u64_i32
	.quad	_L_1+2651
	.quad	_Times__uTimes_param_u64_i32
	.quad	_L_1+2671
	.quad	_Times__Times_var_u64_i32
	.quad	_L_1+2692
	.quad	_Times__uTimes_var_u64_i32
	.quad	_L_1+2710
	.quad	_Times__Times_param_u64_i16
	.quad	_L_1+2729
	.quad	_Times__uTimes_param_u64_i16
	.quad	_L_1+2749
	.quad	_Times__Times_var_u64_i16
	.quad	_L_1+2770
	.quad	_Times__uTimes_var_u64_i16
	.quad	_L_1+2788
	.quad	_Times__Times_param_u64_L
	.quad	_L_1+2807
	.quad	_Times__uTimes_param_u64_L
	.quad	_L_1+2825
	.quad	_Times__Times_var_u64_L
	.quad	_L_1+2844
	.quad	_Times__uTimes_var_u64_L
	.quad	_L_1+2860
	.quad	_Times__Times_param_u64_u64
	.quad	_L_1+2877
	.quad	_Times__uTimes_param_u64_u64
	.quad	_L_1+2897
	.quad	_Times__Times_var_u64_u64
	.quad	_L_1+2918
	.quad	_Times__uTimes_var_u64_u64
	.quad	_L_1+2936
	.quad	_Times__Times_param_u64_u16
	.quad	_L_1+2955
	.quad	_Times__uTimes_param_u64_u16
	.quad	_L_1+2975
	.quad	_Times__Times_var_u64_u16
	.quad	_L_1+2996
	.quad	_Times__uTimes_var_u64_u16
	.quad	_L_1+3014
	.quad	_Times__Times_param_u64_C
	.quad	_L_1+3033
	.quad	_Times__uTimes_param_u64_C
	.quad	_L_1+3051
	.quad	_Times__Times_var_u64_C
	.quad	_L_1+3070
	.quad	_Times__uTimes_var_u64_C
	.quad	_L_1+3086
	.quad	_Times__Times_param_u64_u32
	.quad	_L_1+3103
	.quad	_Times__uTimes_param_u64_u32
	.quad	_L_1+3123
	.quad	_Times__Times_var_u64_u32
	.quad	_L_1+3144
	.quad	_Times__uTimes_var_u64_u32
	.quad	_L_1+3162
	.quad	_Times__Times_param_u64_i8
	.quad	_L_1+3181
	.quad	_Times__uTimes_param_u64_i8
	.quad	_L_1+3200
	.quad	_Times__Times_var_u64_i8
	.quad	_L_1+3220
	.quad	_Times__uTimes_var_u64_i8
	.quad	_L_1+3237
	.quad	_Times__Times_param_u64_I
	.quad	_L_1+3255
	.quad	_Times__uTimes_param_u64_I
	.quad	_L_1+3273
	.quad	_Times__Times_var_u64_I
	.quad	_L_1+3292
	.quad	_Times__uTimes_var_u64_I
	.quad	_L_1+3308
	.quad	_Times__Times_param_u64_u8
	.quad	_L_1+3325
	.quad	_Times__uTimes_param_u64_u8
	.quad	_L_1+3344
	.quad	_Times__Times_var_u64_u8
	.quad	_L_1+3364
	.quad	_Times__uTimes_var_u64_u8
	.quad	_L_1+3381
	.quad	_Times__Times_param_u64_LC
	.quad	_L_1+3399
	.quad	_Times__uTimes_param_u64_LC
	.quad	_L_1+3418
	.quad	_Times__Times_var_u64_LC
	.quad	_L_1+3438
	.quad	_Times__uTimes_var_u64_LC
	.quad	_L_1+3455
	.quad	_Times__Times_param_u64_i64
	.quad	_L_1+3473
	.quad	_Times__uTimes_param_u64_i64
	.quad	_L_1+3493
	.quad	_Times__Times_var_u64_i64
	.quad	_L_1+3514
	.quad	_Times__uTimes_var_u64_i64
	.quad	_L_1+3532
	.quad	_Times__Times_param_u16_i32
	.quad	_L_1+3551
	.quad	_Times__uTimes_param_u16_i32
	.quad	_L_1+3571
	.quad	_Times__Times_var_u16_i32
	.quad	_L_1+3592
	.quad	_Times__uTimes_var_u16_i32
	.quad	_L_1+3610
	.quad	_Times__Times_param_u16_i16
	.quad	_L_1+3629
	.quad	_Times__uTimes_param_u16_i16
	.quad	_L_1+3649
	.quad	_Times__Times_var_u16_i16
	.quad	_L_1+3670
	.quad	_Times__uTimes_var_u16_i16
	.quad	_L_1+3688
	.quad	_Times__Times_param_u16_L
	.quad	_L_1+3707
	.quad	_Times__uTimes_param_u16_L
	.quad	_L_1+3725
	.quad	_Times__Times_var_u16_L
	.quad	_L_1+3744
	.quad	_Times__uTimes_var_u16_L
	.quad	_L_1+3760
	.quad	_Times__Times_param_u16_u64
	.quad	_L_1+3777
	.quad	_Times__uTimes_param_u16_u64
	.quad	_L_1+3797
	.quad	_Times__Times_var_u16_u64
	.quad	_L_1+3818
	.quad	_Times__uTimes_var_u16_u64
	.quad	_L_1+3836
	.quad	_Times__Times_param_u16_u16
	.quad	_L_1+3855
	.quad	_Times__uTimes_param_u16_u16
	.quad	_L_1+3875
	.quad	_Times__Times_var_u16_u16
	.quad	_L_1+3896
	.quad	_Times__uTimes_var_u16_u16
	.quad	_L_1+3914
	.quad	_Times__Times_param_u16_C
	.quad	_L_1+3933
	.quad	_Times__uTimes_param_u16_C
	.quad	_L_1+3951
	.quad	_Times__Times_var_u16_C
	.quad	_L_1+3970
	.quad	_Times__uTimes_var_u16_C
	.quad	_L_1+3986
	.quad	_Times__Times_param_u16_u32
	.quad	_L_1+4003
	.quad	_Times__uTimes_param_u16_u32
	.quad	_L_1+4023
	.quad	_Times__Times_var_u16_u32
	.quad	_L_1+4044
	.quad	_Times__uTimes_var_u16_u32
	.quad	_L_1+4062
	.quad	_Times__Times_param_u16_i8
	.quad	_L_1+4081
	.quad	_Times__uTimes_param_u16_i8
	.quad	_L_1+4100
	.quad	_Times__Times_var_u16_i8
	.quad	_L_1+4120
	.quad	_Times__uTimes_var_u16_i8
	.quad	_L_1+4137
	.quad	_Times__Times_param_u16_I
	.quad	_L_1+4155
	.quad	_Times__uTimes_param_u16_I
	.quad	_L_1+4173
	.quad	_Times__Times_var_u16_I
	.quad	_L_1+4192
	.quad	_Times__uTimes_var_u16_I
	.quad	_L_1+4208
	.quad	_Times__Times_param_u16_u8
	.quad	_L_1+4225
	.quad	_Times__uTimes_param_u16_u8
	.quad	_L_1+4244
	.quad	_Times__Times_var_u16_u8
	.quad	_L_1+4264
	.quad	_Times__uTimes_var_u16_u8
	.quad	_L_1+4281
	.quad	_Times__Times_param_u16_LC
	.quad	_L_1+4299
	.quad	_Times__uTimes_param_u16_LC
	.quad	_L_1+4318
	.quad	_Times__Times_var_u16_LC
	.quad	_L_1+4338
	.quad	_Times__uTimes_var_u16_LC
	.quad	_L_1+4355
	.quad	_Times__Times_param_u16_i64
	.quad	_L_1+4373
	.quad	_Times__uTimes_param_u16_i64
	.quad	_L_1+4393
	.quad	_Times__Times_var_u16_i64
	.quad	_L_1+4414
	.quad	_Times__uTimes_var_u16_i64
	.quad	_L_1+4432
	.quad	_Times__Times_param_C_i32
	.quad	_L_1+4451
	.quad	_Times__uTimes_param_C_i32
	.quad	_L_1+4469
	.quad	_Times__Times_var_C_i32
	.quad	_L_1+4488
	.quad	_Times__uTimes_var_C_i32
	.quad	_L_1+4504
	.quad	_Times__Times_param_C_i16
	.quad	_L_1+4521
	.quad	_Times__uTimes_param_C_i16
	.quad	_L_1+4539
	.quad	_Times__Times_var_C_i16
	.quad	_L_1+4558
	.quad	_Times__uTimes_var_C_i16
	.quad	_L_1+4574
	.quad	_Times__Times_param_C_L
	.quad	_L_1+4591
	.quad	_Times__uTimes_param_C_L
	.quad	_L_1+4607
	.quad	_Times__Times_var_C_L
	.quad	_L_1+4624
	.quad	_Times__uTimes_var_C_L
	.quad	_L_1+4638
	.quad	_Times__Times_param_C_u64
	.quad	_L_1+4653
	.quad	_Times__uTimes_param_C_u64
	.quad	_L_1+4671
	.quad	_Times__Times_var_C_u64
	.quad	_L_1+4690
	.quad	_Times__uTimes_var_C_u64
	.quad	_L_1+4706
	.quad	_Times__Times_param_C_u16
	.quad	_L_1+4723
	.quad	_Times__uTimes_param_C_u16
	.quad	_L_1+4741
	.quad	_Times__Times_var_C_u16
	.quad	_L_1+4760
	.quad	_Times__uTimes_var_C_u16
	.quad	_L_1+4776
	.quad	_Times__Times_param_C_C
	.quad	_L_1+4793
	.quad	_Times__uTimes_param_C_C
	.quad	_L_1+4809
	.quad	_Times__Times_var_C_C
	.quad	_L_1+4826
	.quad	_Times__uTimes_var_C_C
	.quad	_L_1+4840
	.quad	_Times__Times_param_C_u32
	.quad	_L_1+4855
	.quad	_Times__uTimes_param_C_u32
	.quad	_L_1+4873
	.quad	_Times__Times_var_C_u32
	.quad	_L_1+4892
	.quad	_Times__uTimes_var_C_u32
	.quad	_L_1+4908
	.quad	_Times__Times_param_C_i8
	.quad	_L_1+4925
	.quad	_Times__uTimes_param_C_i8
	.quad	_L_1+4942
	.quad	_Times__Times_var_C_i8
	.quad	_L_1+4960
	.quad	_Times__uTimes_var_C_i8
	.quad	_L_1+4975
	.quad	_Times__Times_param_C_I
	.quad	_L_1+4991
	.quad	_Times__uTimes_param_C_I
	.quad	_L_1+5007
	.quad	_Times__Times_var_C_I
	.quad	_L_1+5024
	.quad	_Times__uTimes_var_C_I
	.quad	_L_1+5038
	.quad	_Times__Times_param_C_u8
	.quad	_L_1+5053
	.quad	_Times__uTimes_param_C_u8
	.quad	_L_1+5070
	.quad	_Times__Times_var_C_u8
	.quad	_L_1+5088
	.quad	_Times__uTimes_var_C_u8
	.quad	_L_1+5103
	.quad	_Times__Times_param_C_LC
	.quad	_L_1+5119
	.quad	_Times__uTimes_param_C_LC
	.quad	_L_1+5136
	.quad	_Times__Times_var_C_LC
	.quad	_L_1+5154
	.quad	_Times__uTimes_var_C_LC
	.quad	_L_1+5169
	.quad	_Times__Times_param_C_i64
	.quad	_L_1+5185
	.quad	_Times__uTimes_param_C_i64
	.quad	_L_1+5203
	.quad	_Times__Times_var_C_i64
	.quad	_L_1+5222
	.quad	_Times__uTimes_var_C_i64
	.quad	_L_1+5238
	.quad	_Times__Times_param_u32_i32
	.quad	_L_1+5255
	.quad	_Times__uTimes_param_u32_i32
	.quad	_L_1+5275
	.quad	_Times__Times_var_u32_i32
	.quad	_L_1+5296
	.quad	_Times__uTimes_var_u32_i32
	.quad	_L_1+5314
	.quad	_Times__Times_param_u32_i16
	.quad	_L_1+5333
	.quad	_Times__uTimes_param_u32_i16
	.quad	_L_1+5353
	.quad	_Times__Times_var_u32_i16
	.quad	_L_1+5374
	.quad	_Times__uTimes_var_u32_i16
	.quad	_L_1+5392
	.quad	_Times__Times_param_u32_L
	.quad	_L_1+5411
	.quad	_Times__uTimes_param_u32_L
	.quad	_L_1+5429
	.quad	_Times__Times_var_u32_L
	.quad	_L_1+5448
	.quad	_Times__uTimes_var_u32_L
	.quad	_L_1+5464
	.quad	_Times__Times_param_u32_u64
	.quad	_L_1+5481
	.quad	_Times__uTimes_param_u32_u64
	.quad	_L_1+5501
	.quad	_Times__Times_var_u32_u64
	.quad	_L_1+5522
	.quad	_Times__uTimes_var_u32_u64
	.quad	_L_1+5540
	.quad	_Times__Times_param_u32_u16
	.quad	_L_1+5559
	.quad	_Times__uTimes_param_u32_u16
	.quad	_L_1+5579
	.quad	_Times__Times_var_u32_u16
	.quad	_L_1+5600
	.quad	_Times__uTimes_var_u32_u16
	.quad	_L_1+5618
	.quad	_Times__Times_param_u32_C
	.quad	_L_1+5637
	.quad	_Times__uTimes_param_u32_C
	.quad	_L_1+5655
	.quad	_Times__Times_var_u32_C
	.quad	_L_1+5674
	.quad	_Times__uTimes_var_u32_C
	.quad	_L_1+5690
	.quad	_Times__Times_param_u32_u32
	.quad	_L_1+5707
	.quad	_Times__uTimes_param_u32_u32
	.quad	_L_1+5727
	.quad	_Times__Times_var_u32_u32
	.quad	_L_1+5748
	.quad	_Times__uTimes_var_u32_u32
	.quad	_L_1+5766
	.quad	_Times__Times_param_u32_i8
	.quad	_L_1+5785
	.quad	_Times__uTimes_param_u32_i8
	.quad	_L_1+5804
	.quad	_Times__Times_var_u32_i8
	.quad	_L_1+5824
	.quad	_Times__uTimes_var_u32_i8
	.quad	_L_1+5841
	.quad	_Times__Times_param_u32_I
	.quad	_L_1+5859
	.quad	_Times__uTimes_param_u32_I
	.quad	_L_1+5877
	.quad	_Times__Times_var_u32_I
	.quad	_L_1+5896
	.quad	_Times__uTimes_var_u32_I
	.quad	_L_1+5912
	.quad	_Times__Times_param_u32_u8
	.quad	_L_1+5929
	.quad	_Times__uTimes_param_u32_u8
	.quad	_L_1+5948
	.quad	_Times__Times_var_u32_u8
	.quad	_L_1+5968
	.quad	_Times__uTimes_var_u32_u8
	.quad	_L_1+5985
	.quad	_Times__Times_param_u32_LC
	.quad	_L_1+6003
	.quad	_Times__uTimes_param_u32_LC
	.quad	_L_1+6022
	.quad	_Times__Times_var_u32_LC
	.quad	_L_1+6042
	.quad	_Times__uTimes_var_u32_LC
	.quad	_L_1+6059
	.quad	_Times__Times_param_u32_i64
	.quad	_L_1+6077
	.quad	_Times__uTimes_param_u32_i64
	.quad	_L_1+6097
	.quad	_Times__Times_var_u32_i64
	.quad	_L_1+6118
	.quad	_Times__uTimes_var_u32_i64
	.quad	_L_1+6136
	.quad	_Times__Times_param_i8_i32
	.quad	_L_1+6155
	.quad	_Times__uTimes_param_i8_i32
	.quad	_L_1+6174
	.quad	_Times__Times_var_i8_i32
	.quad	_L_1+6194
	.quad	_Times__uTimes_var_i8_i32
	.quad	_L_1+6211
	.quad	_Times__Times_param_i8_i16
	.quad	_L_1+6229
	.quad	_Times__uTimes_param_i8_i16
	.quad	_L_1+6248
	.quad	_Times__Times_var_i8_i16
	.quad	_L_1+6268
	.quad	_Times__uTimes_var_i8_i16
	.quad	_L_1+6285
	.quad	_Times__Times_param_i8_L
	.quad	_L_1+6303
	.quad	_Times__uTimes_param_i8_L
	.quad	_L_1+6320
	.quad	_Times__Times_var_i8_L
	.quad	_L_1+6338
	.quad	_Times__uTimes_var_i8_L
	.quad	_L_1+6353
	.quad	_Times__Times_param_i8_u64
	.quad	_L_1+6369
	.quad	_Times__uTimes_param_i8_u64
	.quad	_L_1+6388
	.quad	_Times__Times_var_i8_u64
	.quad	_L_1+6408
	.quad	_Times__uTimes_var_i8_u64
	.quad	_L_1+6425
	.quad	_Times__Times_param_i8_u16
	.quad	_L_1+6443
	.quad	_Times__uTimes_param_i8_u16
	.quad	_L_1+6462
	.quad	_Times__Times_var_i8_u16
	.quad	_L_1+6482
	.quad	_Times__uTimes_var_i8_u16
	.quad	_L_1+6499
	.quad	_Times__Times_param_i8_C
	.quad	_L_1+6517
	.quad	_Times__uTimes_param_i8_C
	.quad	_L_1+6534
	.quad	_Times__Times_var_i8_C
	.quad	_L_1+6552
	.quad	_Times__uTimes_var_i8_C
	.quad	_L_1+6567
	.quad	_Times__Times_param_i8_u32
	.quad	_L_1+6583
	.quad	_Times__uTimes_param_i8_u32
	.quad	_L_1+6602
	.quad	_Times__Times_var_i8_u32
	.quad	_L_1+6622
	.quad	_Times__uTimes_var_i8_u32
	.quad	_L_1+6639
	.quad	_Times__Times_param_i8_i8
	.quad	_L_1+6657
	.quad	_Times__uTimes_param_i8_i8
	.quad	_L_1+6675
	.quad	_Times__Times_var_i8_i8
	.quad	_L_1+6694
	.quad	_Times__uTimes_var_i8_i8
	.quad	_L_1+6710
	.quad	_Times__Times_param_i8_I
	.quad	_L_1+6727
	.quad	_Times__uTimes_param_i8_I
	.quad	_L_1+6744
	.quad	_Times__Times_var_i8_I
	.quad	_L_1+6762
	.quad	_Times__uTimes_var_i8_I
	.quad	_L_1+6777
	.quad	_Times__Times_param_i8_u8
	.quad	_L_1+6793
	.quad	_Times__uTimes_param_i8_u8
	.quad	_L_1+6811
	.quad	_Times__Times_var_i8_u8
	.quad	_L_1+6830
	.quad	_Times__uTimes_var_i8_u8
	.quad	_L_1+6846
	.quad	_Times__Times_param_i8_LC
	.quad	_L_1+6863
	.quad	_Times__uTimes_param_i8_LC
	.quad	_L_1+6881
	.quad	_Times__Times_var_i8_LC
	.quad	_L_1+6900
	.quad	_Times__uTimes_var_i8_LC
	.quad	_L_1+6916
	.quad	_Times__Times_param_i8_i64
	.quad	_L_1+6933
	.quad	_Times__uTimes_param_i8_i64
	.quad	_L_1+6952
	.quad	_Times__Times_var_i8_i64
	.quad	_L_1+6972
	.quad	_Times__uTimes_var_i8_i64
	.quad	_L_1+6989
	.quad	_Times__Times_param_I_i32
	.quad	_L_1+7007
	.quad	_Times__uTimes_param_I_i32
	.quad	_L_1+7025
	.quad	_Times__Times_var_I_i32
	.quad	_L_1+7044
	.quad	_Times__uTimes_var_I_i32
	.quad	_L_1+7060
	.quad	_Times__Times_param_I_i16
	.quad	_L_1+7077
	.quad	_Times__uTimes_param_I_i16
	.quad	_L_1+7095
	.quad	_Times__Times_var_I_i16
	.quad	_L_1+7114
	.quad	_Times__uTimes_var_I_i16
	.quad	_L_1+7130
	.quad	_Times__Times_param_I_L
	.quad	_L_1+7147
	.quad	_Times__uTimes_param_I_L
	.quad	_L_1+7163
	.quad	_Times__Times_var_I_L
	.quad	_L_1+7180
	.quad	_Times__uTimes_var_I_L
	.quad	_L_1+7194
	.quad	_Times__Times_param_I_u64
	.quad	_L_1+7209
	.quad	_Times__uTimes_param_I_u64
	.quad	_L_1+7227
	.quad	_Times__Times_var_I_u64
	.quad	_L_1+7246
	.quad	_Times__uTimes_var_I_u64
	.quad	_L_1+7262
	.quad	_Times__Times_param_I_u16
	.quad	_L_1+7279
	.quad	_Times__uTimes_param_I_u16
	.quad	_L_1+7297
	.quad	_Times__Times_var_I_u16
	.quad	_L_1+7316
	.quad	_Times__uTimes_var_I_u16
	.quad	_L_1+7332
	.quad	_Times__Times_param_I_C
	.quad	_L_1+7349
	.quad	_Times__uTimes_param_I_C
	.quad	_L_1+7365
	.quad	_Times__Times_var_I_C
	.quad	_L_1+7382
	.quad	_Times__uTimes_var_I_C
	.quad	_L_1+7396
	.quad	_Times__Times_param_I_u32
	.quad	_L_1+7411
	.quad	_Times__uTimes_param_I_u32
	.quad	_L_1+7429
	.quad	_Times__Times_var_I_u32
	.quad	_L_1+7448
	.quad	_Times__uTimes_var_I_u32
	.quad	_L_1+7464
	.quad	_Times__Times_param_I_i8
	.quad	_L_1+7481
	.quad	_Times__uTimes_param_I_i8
	.quad	_L_1+7498
	.quad	_Times__Times_var_I_i8
	.quad	_L_1+7516
	.quad	_Times__uTimes_var_I_i8
	.quad	_L_1+7531
	.quad	_Times__Times_param_I_I
	.quad	_L_1+7547
	.quad	_Times__uTimes_param_I_I
	.quad	_L_1+7563
	.quad	_Times__Times_var_I_I
	.quad	_L_1+7580
	.quad	_Times__uTimes_var_I_I
	.quad	_L_1+7594
	.quad	_Times__Times_param_I_u8
	.quad	_L_1+7609
	.quad	_Times__uTimes_param_I_u8
	.quad	_L_1+7626
	.quad	_Times__Times_var_I_u8
	.quad	_L_1+7644
	.quad	_Times__uTimes_var_I_u8
	.quad	_L_1+7659
	.quad	_Times__Times_param_I_LC
	.quad	_L_1+7675
	.quad	_Times__uTimes_param_I_LC
	.quad	_L_1+7692
	.quad	_Times__Times_var_I_LC
	.quad	_L_1+7710
	.quad	_Times__uTimes_var_I_LC
	.quad	_L_1+7725
	.quad	_Times__Times_param_I_i64
	.quad	_L_1+7741
	.quad	_Times__uTimes_param_I_i64
	.quad	_L_1+7759
	.quad	_Times__Times_var_I_i64
	.quad	_L_1+7778
	.quad	_Times__uTimes_var_I_i64
	.quad	_L_1+7794
	.quad	_Times__Times_param_u8_i32
	.quad	_L_1+7811
	.quad	_Times__uTimes_param_u8_i32
	.quad	_L_1+7830
	.quad	_Times__Times_var_u8_i32
	.quad	_L_1+7850
	.quad	_Times__uTimes_var_u8_i32
	.quad	_L_1+7867
	.quad	_Times__Times_param_u8_i16
	.quad	_L_1+7885
	.quad	_Times__uTimes_param_u8_i16
	.quad	_L_1+7904
	.quad	_Times__Times_var_u8_i16
	.quad	_L_1+7924
	.quad	_Times__uTimes_var_u8_i16
	.quad	_L_1+7941
	.quad	_Times__Times_param_u8_L
	.quad	_L_1+7959
	.quad	_Times__uTimes_param_u8_L
	.quad	_L_1+7976
	.quad	_Times__Times_var_u8_L
	.quad	_L_1+7994
	.quad	_Times__uTimes_var_u8_L
	.quad	_L_1+8009
	.quad	_Times__Times_param_u8_u64
	.quad	_L_1+8025
	.quad	_Times__uTimes_param_u8_u64
	.quad	_L_1+8044
	.quad	_Times__Times_var_u8_u64
	.quad	_L_1+8064
	.quad	_Times__uTimes_var_u8_u64
	.quad	_L_1+8081
	.quad	_Times__Times_param_u8_u16
	.quad	_L_1+8099
	.quad	_Times__uTimes_param_u8_u16
	.quad	_L_1+8118
	.quad	_Times__Times_var_u8_u16
	.quad	_L_1+8138
	.quad	_Times__uTimes_var_u8_u16
	.quad	_L_1+8155
	.quad	_Times__Times_param_u8_C
	.quad	_L_1+8173
	.quad	_Times__uTimes_param_u8_C
	.quad	_L_1+8190
	.quad	_Times__Times_var_u8_C
	.quad	_L_1+8208
	.quad	_Times__uTimes_var_u8_C
	.quad	_L_1+8223
	.quad	_Times__Times_param_u8_u32
	.quad	_L_1+8239
	.quad	_Times__uTimes_param_u8_u32
	.quad	_L_1+8258
	.quad	_Times__Times_var_u8_u32
	.quad	_L_1+8278
	.quad	_Times__uTimes_var_u8_u32
	.quad	_L_1+8295
	.quad	_Times__Times_param_u8_i8
	.quad	_L_1+8313
	.quad	_Times__uTimes_param_u8_i8
	.quad	_L_1+8331
	.quad	_Times__Times_var_u8_i8
	.quad	_L_1+8350
	.quad	_Times__uTimes_var_u8_i8
	.quad	_L_1+8366
	.quad	_Times__Times_param_u8_I
	.quad	_L_1+8383
	.quad	_Times__uTimes_param_u8_I
	.quad	_L_1+8400
	.quad	_Times__Times_var_u8_I
	.quad	_L_1+8418
	.quad	_Times__uTimes_var_u8_I
	.quad	_L_1+8433
	.quad	_Times__Times_param_u8_u8
	.quad	_L_1+8449
	.quad	_Times__uTimes_param_u8_u8
	.quad	_L_1+8467
	.quad	_Times__Times_var_u8_u8
	.quad	_L_1+8486
	.quad	_Times__uTimes_var_u8_u8
	.quad	_L_1+8502
	.quad	_Times__Times_param_u8_LC
	.quad	_L_1+8519
	.quad	_Times__uTimes_param_u8_LC
	.quad	_L_1+8537
	.quad	_Times__Times_var_u8_LC
	.quad	_L_1+8556
	.quad	_Times__uTimes_var_u8_LC
	.quad	_L_1+8572
	.quad	_Times__Times_param_u8_i64
	.quad	_L_1+8589
	.quad	_Times__uTimes_param_u8_i64
	.quad	_L_1+8608
	.quad	_Times__Times_var_u8_i64
	.quad	_L_1+8628
	.quad	_Times__uTimes_var_u8_i64
	.quad	_L_1+8645
	.quad	_Times__Times_param_f32_f32
	.quad	_L_1+8663
	.quad	_Times__Times_var_f32_f32
	.quad	_L_1+8683
	.quad	_Times__Times_param_LC_i32
	.quad	_L_1+8701
	.quad	_Times__uTimes_param_LC_i32
	.quad	_L_1+8720
	.quad	_Times__Times_var_LC_i32
	.quad	_L_1+8740
	.quad	_Times__uTimes_var_LC_i32
	.quad	_L_1+8757
	.quad	_Times__Times_param_LC_i16
	.quad	_L_1+8775
	.quad	_Times__uTimes_param_LC_i16
	.quad	_L_1+8794
	.quad	_Times__Times_var_LC_i16
	.quad	_L_1+8814
	.quad	_Times__uTimes_var_LC_i16
	.quad	_L_1+8831
	.quad	_Times__Times_param_LC_L
	.quad	_L_1+8849
	.quad	_Times__uTimes_param_LC_L
	.quad	_L_1+8866
	.quad	_Times__Times_var_LC_L
	.quad	_L_1+8884
	.quad	_Times__uTimes_var_LC_L
	.quad	_L_1+8899
	.quad	_Times__Times_param_LC_u64
	.quad	_L_1+8915
	.quad	_Times__uTimes_param_LC_u64
	.quad	_L_1+8934
	.quad	_Times__Times_var_LC_u64
	.quad	_L_1+8954
	.quad	_Times__uTimes_var_LC_u64
	.quad	_L_1+8971
	.quad	_Times__Times_param_LC_u16
	.quad	_L_1+8989
	.quad	_Times__uTimes_param_LC_u16
	.quad	_L_1+9008
	.quad	_Times__Times_var_LC_u16
	.quad	_L_1+9028
	.quad	_Times__uTimes_var_LC_u16
	.quad	_L_1+9045
	.quad	_Times__Times_param_LC_C
	.quad	_L_1+9063
	.quad	_Times__uTimes_param_LC_C
	.quad	_L_1+9080
	.quad	_Times__Times_var_LC_C
	.quad	_L_1+9098
	.quad	_Times__uTimes_var_LC_C
	.quad	_L_1+9113
	.quad	_Times__Times_param_LC_u32
	.quad	_L_1+9129
	.quad	_Times__uTimes_param_LC_u32
	.quad	_L_1+9148
	.quad	_Times__Times_var_LC_u32
	.quad	_L_1+9168
	.quad	_Times__uTimes_var_LC_u32
	.quad	_L_1+9185
	.quad	_Times__Times_param_LC_i8
	.quad	_L_1+9203
	.quad	_Times__uTimes_param_LC_i8
	.quad	_L_1+9221
	.quad	_Times__Times_var_LC_i8
	.quad	_L_1+9240
	.quad	_Times__uTimes_var_LC_i8
	.quad	_L_1+9256
	.quad	_Times__Times_param_LC_I
	.quad	_L_1+9273
	.quad	_Times__uTimes_param_LC_I
	.quad	_L_1+9290
	.quad	_Times__Times_var_LC_I
	.quad	_L_1+9308
	.quad	_Times__uTimes_var_LC_I
	.quad	_L_1+9323
	.quad	_Times__Times_param_LC_u8
	.quad	_L_1+9339
	.quad	_Times__uTimes_param_LC_u8
	.quad	_L_1+9357
	.quad	_Times__Times_var_LC_u8
	.quad	_L_1+9376
	.quad	_Times__uTimes_var_LC_u8
	.quad	_L_1+9392
	.quad	_Times__Times_param_LC_LC
	.quad	_L_1+9409
	.quad	_Times__uTimes_param_LC_LC
	.quad	_L_1+9427
	.quad	_Times__Times_var_LC_LC
	.quad	_L_1+9446
	.quad	_Times__uTimes_var_LC_LC
	.quad	_L_1+9462
	.quad	_Times__Times_param_LC_i64
	.quad	_L_1+9479
	.quad	_Times__uTimes_param_LC_i64
	.quad	_L_1+9498
	.quad	_Times__Times_var_LC_i64
	.quad	_L_1+9518
	.quad	_Times__uTimes_var_LC_i64
	.quad	_L_1+9535
	.quad	_Times__Times_param_i64_i32
	.quad	_L_1+9553
	.quad	_Times__uTimes_param_i64_i32
	.quad	_L_1+9573
	.quad	_Times__Times_var_i64_i32
	.quad	_L_1+9594
	.quad	_Times__uTimes_var_i64_i32
	.quad	_L_1+9612
	.quad	_Times__Times_param_i64_i16
	.quad	_L_1+9631
	.quad	_Times__uTimes_param_i64_i16
	.quad	_L_1+9651
	.quad	_Times__Times_var_i64_i16
	.quad	_L_1+9672
	.quad	_Times__uTimes_var_i64_i16
	.quad	_L_1+9690
	.quad	_Times__Times_param_i64_L
	.quad	_L_1+9709
	.quad	_Times__uTimes_param_i64_L
	.quad	_L_1+9727
	.quad	_Times__Times_var_i64_L
	.quad	_L_1+9746
	.quad	_Times__uTimes_var_i64_L
	.quad	_L_1+9762
	.quad	_Times__Times_param_i64_u64
	.quad	_L_1+9779
	.quad	_Times__uTimes_param_i64_u64
	.quad	_L_1+9799
	.quad	_Times__Times_var_i64_u64
	.quad	_L_1+9820
	.quad	_Times__uTimes_var_i64_u64
	.quad	_L_1+9838
	.quad	_Times__Times_param_i64_u16
	.quad	_L_1+9857
	.quad	_Times__uTimes_param_i64_u16
	.quad	_L_1+9877
	.quad	_Times__Times_var_i64_u16
	.quad	_L_1+9898
	.quad	_Times__uTimes_var_i64_u16
	.quad	_L_1+9916
	.quad	_Times__Times_param_i64_C
	.quad	_L_1+9935
	.quad	_Times__uTimes_param_i64_C
	.quad	_L_1+9953
	.quad	_Times__Times_var_i64_C
	.quad	_L_1+9972
	.quad	_Times__uTimes_var_i64_C
	.quad	_L_1+9988
	.quad	_Times__Times_param_i64_u32
	.quad	_L_1+10005
	.quad	_Times__uTimes_param_i64_u32
	.quad	_L_1+10025
	.quad	_Times__Times_var_i64_u32
	.quad	_L_1+10046
	.quad	_Times__uTimes_var_i64_u32
	.quad	_L_1+10064
	.quad	_Times__Times_param_i64_i8
	.quad	_L_1+10083
	.quad	_Times__uTimes_param_i64_i8
	.quad	_L_1+10102
	.quad	_Times__Times_var_i64_i8
	.quad	_L_1+10122
	.quad	_Times__uTimes_var_i64_i8
	.quad	_L_1+10139
	.quad	_Times__Times_param_i64_I
	.quad	_L_1+10157
	.quad	_Times__uTimes_param_i64_I
	.quad	_L_1+10175
	.quad	_Times__Times_var_i64_I
	.quad	_L_1+10194
	.quad	_Times__uTimes_var_i64_I
	.quad	_L_1+10210
	.quad	_Times__Times_param_i64_u8
	.quad	_L_1+10227
	.quad	_Times__uTimes_param_i64_u8
	.quad	_L_1+10246
	.quad	_Times__Times_var_i64_u8
	.quad	_L_1+10266
	.quad	_Times__uTimes_var_i64_u8
	.quad	_L_1+10283
	.quad	_Times__Times_param_i64_LC
	.quad	_L_1+10301
	.quad	_Times__uTimes_param_i64_LC
	.quad	_L_1+10320
	.quad	_Times__Times_var_i64_LC
	.quad	_L_1+10340
	.quad	_Times__uTimes_var_i64_LC
	.quad	_L_1+10357
	.quad	_Times__Times_param_i64_i64
	.quad	_L_1+10375
	.quad	_Times__uTimes_param_i64_i64
	.quad	_L_1+10395
	.quad	_Times__Times_var_i64_i64
	.quad	_L_1+10416
	.quad	_Times__uTimes_var_i64_i64
	.quad	_L_1+10434
	.space 8
	.ascii "../AMD64_DARWIN/Times.m3"
	.space 8
	.data
	.align 5
_MM_Times:
	.quad	_L_1+19760
	.space 32
	.quad	_L_1+10456
	.space 24
	.quad	_MM_Times+208
	.space 8
	.quad	_Times_M3
	.quad	3
	.quad	177
	.quad	178
	.long	1127427604
	.byte	-75
	.space 3
	.quad	182
	.byte	-73
	.space 3
	.long	184
	.quad	185
	.word	186
	.space 6
	.quad	187
	.long	1236950581
	.long	1080526348
	.quad	190
	.word	191
	.space 2
	.long	192
	.space 24
	.quad	_Times_I3
	.quad	_MM_Times+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Times+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Times+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Times+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
