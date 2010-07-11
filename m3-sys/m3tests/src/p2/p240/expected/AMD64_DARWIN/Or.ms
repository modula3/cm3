	.text
.globl _Or__uOr_var_i64_i64
	.private_extern _Or__uOr_var_i64_i64
_Or__uOr_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Or(%rip), %rax
	leave
	ret
.globl _Or__uOr_param_i64_i64
	.private_extern _Or__uOr_param_i64_i64
_Or__uOr_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_LC
	.private_extern _Or__uOr_var_i64_LC
_Or__uOr_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_LC
	.private_extern _Or__uOr_param_i64_LC
_Or__uOr_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i64_u8
	.private_extern _Or__uOr_var_i64_u8
_Or__uOr_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_u8
	.private_extern _Or__uOr_param_i64_u8
_Or__uOr_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_I
	.private_extern _Or__uOr_var_i64_I
_Or__uOr_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_I
	.private_extern _Or__uOr_param_i64_I
_Or__uOr_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_i8
	.private_extern _Or__uOr_var_i64_i8
_Or__uOr_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_i8
	.private_extern _Or__uOr_param_i64_i8
_Or__uOr_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_u32
	.private_extern _Or__uOr_var_i64_u32
_Or__uOr_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_u32
	.private_extern _Or__uOr_param_i64_u32
_Or__uOr_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_C
	.private_extern _Or__uOr_var_i64_C
_Or__uOr_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_C
	.private_extern _Or__uOr_param_i64_C
_Or__uOr_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i64_u16
	.private_extern _Or__uOr_var_i64_u16
_Or__uOr_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_u16
	.private_extern _Or__uOr_param_i64_u16
_Or__uOr_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_u64
	.private_extern _Or__uOr_var_i64_u64
_Or__uOr_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_u64
	.private_extern _Or__uOr_param_i64_u64
_Or__uOr_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_L
	.private_extern _Or__uOr_var_i64_L
_Or__uOr_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_L
	.private_extern _Or__uOr_param_i64_L
_Or__uOr_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_i16
	.private_extern _Or__uOr_var_i64_i16
_Or__uOr_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_i16
	.private_extern _Or__uOr_param_i64_i16
_Or__uOr_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i64_i32
	.private_extern _Or__uOr_var_i64_i32
_Or__uOr_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i64_i32
	.private_extern _Or__uOr_param_i64_i32
_Or__uOr_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_LC_i64
	.private_extern _Or__uOr_var_LC_i64
_Or__uOr_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_i64
	.private_extern _Or__uOr_param_LC_i64
_Or__uOr_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_LC
	.private_extern _Or__uOr_var_LC_LC
_Or__uOr_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rax
	leave
	ret
.globl _Or__uOr_param_LC_LC
	.private_extern _Or__uOr_param_LC_LC
_Or__uOr_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_LC_u8
	.private_extern _Or__uOr_var_LC_u8
_Or__uOr_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_u8
	.private_extern _Or__uOr_param_LC_u8
_Or__uOr_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_I
	.private_extern _Or__uOr_var_LC_I
_Or__uOr_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_I
	.private_extern _Or__uOr_param_LC_I
_Or__uOr_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_i8
	.private_extern _Or__uOr_var_LC_i8
_Or__uOr_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_i8
	.private_extern _Or__uOr_param_LC_i8
_Or__uOr_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_u32
	.private_extern _Or__uOr_var_LC_u32
_Or__uOr_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_u32
	.private_extern _Or__uOr_param_LC_u32
_Or__uOr_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_C
	.private_extern _Or__uOr_var_LC_C
_Or__uOr_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_C
	.private_extern _Or__uOr_param_LC_C
_Or__uOr_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_LC_u16
	.private_extern _Or__uOr_var_LC_u16
_Or__uOr_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_u16
	.private_extern _Or__uOr_param_LC_u16
_Or__uOr_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_u64
	.private_extern _Or__uOr_var_LC_u64
_Or__uOr_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_u64
	.private_extern _Or__uOr_param_LC_u64
_Or__uOr_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_L
	.private_extern _Or__uOr_var_LC_L
_Or__uOr_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_L
	.private_extern _Or__uOr_param_LC_L
_Or__uOr_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_i16
	.private_extern _Or__uOr_var_LC_i16
_Or__uOr_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_i16
	.private_extern _Or__uOr_param_LC_i16
_Or__uOr_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_LC_i32
	.private_extern _Or__uOr_var_LC_i32
_Or__uOr_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_LC_i32
	.private_extern _Or__uOr_param_LC_i32
_Or__uOr_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u8_i64
	.private_extern _Or__uOr_var_u8_i64
_Or__uOr_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_i64
	.private_extern _Or__uOr_param_u8_i64
_Or__uOr_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_LC
	.private_extern _Or__uOr_var_u8_LC
_Or__uOr_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_LC
	.private_extern _Or__uOr_param_u8_LC
_Or__uOr_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u8_u8
	.private_extern _Or__uOr_var_u8_u8
_Or__uOr_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _Or__uOr_param_u8_u8
	.private_extern _Or__uOr_param_u8_u8
_Or__uOr_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_I
	.private_extern _Or__uOr_var_u8_I
_Or__uOr_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_I
	.private_extern _Or__uOr_param_u8_I
_Or__uOr_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_i8
	.private_extern _Or__uOr_var_u8_i8
_Or__uOr_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_i8
	.private_extern _Or__uOr_param_u8_i8
_Or__uOr_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_u32
	.private_extern _Or__uOr_var_u8_u32
_Or__uOr_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_u32
	.private_extern _Or__uOr_param_u8_u32
_Or__uOr_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_C
	.private_extern _Or__uOr_var_u8_C
_Or__uOr_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_C
	.private_extern _Or__uOr_param_u8_C
_Or__uOr_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u8_u16
	.private_extern _Or__uOr_var_u8_u16
_Or__uOr_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_u16
	.private_extern _Or__uOr_param_u8_u16
_Or__uOr_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_u64
	.private_extern _Or__uOr_var_u8_u64
_Or__uOr_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_u64
	.private_extern _Or__uOr_param_u8_u64
_Or__uOr_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_L
	.private_extern _Or__uOr_var_u8_L
_Or__uOr_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_L
	.private_extern _Or__uOr_param_u8_L
_Or__uOr_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_i16
	.private_extern _Or__uOr_var_u8_i16
_Or__uOr_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_i16
	.private_extern _Or__uOr_param_u8_i16
_Or__uOr_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u8_i32
	.private_extern _Or__uOr_var_u8_i32
_Or__uOr_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u8_i32
	.private_extern _Or__uOr_param_u8_i32
_Or__uOr_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_i64
	.private_extern _Or__uOr_var_I_i64
_Or__uOr_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_i64
	.private_extern _Or__uOr_param_I_i64
_Or__uOr_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_LC
	.private_extern _Or__uOr_var_I_LC
_Or__uOr_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_LC
	.private_extern _Or__uOr_param_I_LC
_Or__uOr_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_I_u8
	.private_extern _Or__uOr_var_I_u8
_Or__uOr_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_u8
	.private_extern _Or__uOr_param_I_u8
_Or__uOr_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_I
	.private_extern _Or__uOr_var_I_I
_Or__uOr_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	leave
	ret
.globl _Or__uOr_param_I_I
	.private_extern _Or__uOr_param_I_I
_Or__uOr_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_i8
	.private_extern _Or__uOr_var_I_i8
_Or__uOr_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_i8
	.private_extern _Or__uOr_param_I_i8
_Or__uOr_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_u32
	.private_extern _Or__uOr_var_I_u32
_Or__uOr_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_u32
	.private_extern _Or__uOr_param_I_u32
_Or__uOr_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_C
	.private_extern _Or__uOr_var_I_C
_Or__uOr_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_C
	.private_extern _Or__uOr_param_I_C
_Or__uOr_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_I_u16
	.private_extern _Or__uOr_var_I_u16
_Or__uOr_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_u16
	.private_extern _Or__uOr_param_I_u16
_Or__uOr_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_u64
	.private_extern _Or__uOr_var_I_u64
_Or__uOr_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_u64
	.private_extern _Or__uOr_param_I_u64
_Or__uOr_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_L
	.private_extern _Or__uOr_var_I_L
_Or__uOr_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_L
	.private_extern _Or__uOr_param_I_L
_Or__uOr_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_i16
	.private_extern _Or__uOr_var_I_i16
_Or__uOr_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_i16
	.private_extern _Or__uOr_param_I_i16
_Or__uOr_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_I_i32
	.private_extern _Or__uOr_var_I_i32
_Or__uOr_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_I_i32
	.private_extern _Or__uOr_param_I_i32
_Or__uOr_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_i64
	.private_extern _Or__uOr_var_i8_i64
_Or__uOr_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_i64
	.private_extern _Or__uOr_param_i8_i64
_Or__uOr_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_LC
	.private_extern _Or__uOr_var_i8_LC
_Or__uOr_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_LC
	.private_extern _Or__uOr_param_i8_LC
_Or__uOr_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i8_u8
	.private_extern _Or__uOr_var_i8_u8
_Or__uOr_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_u8
	.private_extern _Or__uOr_param_i8_u8
_Or__uOr_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_I
	.private_extern _Or__uOr_var_i8_I
_Or__uOr_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_I
	.private_extern _Or__uOr_param_i8_I
_Or__uOr_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_i8
	.private_extern _Or__uOr_var_i8_i8
_Or__uOr_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _Or__uOr_param_i8_i8
	.private_extern _Or__uOr_param_i8_i8
_Or__uOr_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_u32
	.private_extern _Or__uOr_var_i8_u32
_Or__uOr_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_u32
	.private_extern _Or__uOr_param_i8_u32
_Or__uOr_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_C
	.private_extern _Or__uOr_var_i8_C
_Or__uOr_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_C
	.private_extern _Or__uOr_param_i8_C
_Or__uOr_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i8_u16
	.private_extern _Or__uOr_var_i8_u16
_Or__uOr_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_u16
	.private_extern _Or__uOr_param_i8_u16
_Or__uOr_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_u64
	.private_extern _Or__uOr_var_i8_u64
_Or__uOr_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_u64
	.private_extern _Or__uOr_param_i8_u64
_Or__uOr_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_L
	.private_extern _Or__uOr_var_i8_L
_Or__uOr_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_L
	.private_extern _Or__uOr_param_i8_L
_Or__uOr_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_i16
	.private_extern _Or__uOr_var_i8_i16
_Or__uOr_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_i16
	.private_extern _Or__uOr_param_i8_i16
_Or__uOr_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i8_i32
	.private_extern _Or__uOr_var_i8_i32
_Or__uOr_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i8_i32
	.private_extern _Or__uOr_param_i8_i32
_Or__uOr_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_i64
	.private_extern _Or__uOr_var_u32_i64
_Or__uOr_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_i64
	.private_extern _Or__uOr_param_u32_i64
_Or__uOr_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_LC
	.private_extern _Or__uOr_var_u32_LC
_Or__uOr_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_LC
	.private_extern _Or__uOr_param_u32_LC
_Or__uOr_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u32_u8
	.private_extern _Or__uOr_var_u32_u8
_Or__uOr_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_u8
	.private_extern _Or__uOr_param_u32_u8
_Or__uOr_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_I
	.private_extern _Or__uOr_var_u32_I
_Or__uOr_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_I
	.private_extern _Or__uOr_param_u32_I
_Or__uOr_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_i8
	.private_extern _Or__uOr_var_u32_i8
_Or__uOr_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_i8
	.private_extern _Or__uOr_param_u32_i8
_Or__uOr_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_u32
	.private_extern _Or__uOr_var_u32_u32
_Or__uOr_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	cltq
	leave
	ret
.globl _Or__uOr_param_u32_u32
	.private_extern _Or__uOr_param_u32_u32
_Or__uOr_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_C
	.private_extern _Or__uOr_var_u32_C
_Or__uOr_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_C
	.private_extern _Or__uOr_param_u32_C
_Or__uOr_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u32_u16
	.private_extern _Or__uOr_var_u32_u16
_Or__uOr_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_u16
	.private_extern _Or__uOr_param_u32_u16
_Or__uOr_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_u64
	.private_extern _Or__uOr_var_u32_u64
_Or__uOr_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_u64
	.private_extern _Or__uOr_param_u32_u64
_Or__uOr_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_L
	.private_extern _Or__uOr_var_u32_L
_Or__uOr_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_L
	.private_extern _Or__uOr_param_u32_L
_Or__uOr_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_i16
	.private_extern _Or__uOr_var_u32_i16
_Or__uOr_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_i16
	.private_extern _Or__uOr_param_u32_i16
_Or__uOr_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u32_i32
	.private_extern _Or__uOr_var_u32_i32
_Or__uOr_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u32_i32
	.private_extern _Or__uOr_param_u32_i32
_Or__uOr_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_C_i64
	.private_extern _Or__uOr_var_C_i64
_Or__uOr_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_i64
	.private_extern _Or__uOr_param_C_i64
_Or__uOr_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_LC
	.private_extern _Or__uOr_var_C_LC
_Or__uOr_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_LC
	.private_extern _Or__uOr_param_C_LC
_Or__uOr_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_C_u8
	.private_extern _Or__uOr_var_C_u8
_Or__uOr_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_u8
	.private_extern _Or__uOr_param_C_u8
_Or__uOr_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_I
	.private_extern _Or__uOr_var_C_I
_Or__uOr_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_I
	.private_extern _Or__uOr_param_C_I
_Or__uOr_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_i8
	.private_extern _Or__uOr_var_C_i8
_Or__uOr_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_i8
	.private_extern _Or__uOr_param_C_i8
_Or__uOr_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_u32
	.private_extern _Or__uOr_var_C_u32
_Or__uOr_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_u32
	.private_extern _Or__uOr_param_C_u32
_Or__uOr_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_C
	.private_extern _Or__uOr_var_C_C
_Or__uOr_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	leave
	ret
.globl _Or__uOr_param_C_C
	.private_extern _Or__uOr_param_C_C
_Or__uOr_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_C_u16
	.private_extern _Or__uOr_var_C_u16
_Or__uOr_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_u16
	.private_extern _Or__uOr_param_C_u16
_Or__uOr_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_u64
	.private_extern _Or__uOr_var_C_u64
_Or__uOr_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_u64
	.private_extern _Or__uOr_param_C_u64
_Or__uOr_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_L
	.private_extern _Or__uOr_var_C_L
_Or__uOr_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_L
	.private_extern _Or__uOr_param_C_L
_Or__uOr_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_i16
	.private_extern _Or__uOr_var_C_i16
_Or__uOr_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_i16
	.private_extern _Or__uOr_param_C_i16
_Or__uOr_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_C_i32
	.private_extern _Or__uOr_var_C_i32
_Or__uOr_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_C_i32
	.private_extern _Or__uOr_param_C_i32
_Or__uOr_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	orq	-24(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u16_i64
	.private_extern _Or__uOr_var_u16_i64
_Or__uOr_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_i64
	.private_extern _Or__uOr_param_u16_i64
_Or__uOr_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_LC
	.private_extern _Or__uOr_var_u16_LC
_Or__uOr_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_LC
	.private_extern _Or__uOr_param_u16_LC
_Or__uOr_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u16_u8
	.private_extern _Or__uOr_var_u16_u8
_Or__uOr_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_u8
	.private_extern _Or__uOr_param_u16_u8
_Or__uOr_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_I
	.private_extern _Or__uOr_var_u16_I
_Or__uOr_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_I
	.private_extern _Or__uOr_param_u16_I
_Or__uOr_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_i8
	.private_extern _Or__uOr_var_u16_i8
_Or__uOr_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_i8
	.private_extern _Or__uOr_param_u16_i8
_Or__uOr_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_u32
	.private_extern _Or__uOr_var_u16_u32
_Or__uOr_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_u32
	.private_extern _Or__uOr_param_u16_u32
_Or__uOr_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_C
	.private_extern _Or__uOr_var_u16_C
_Or__uOr_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_C
	.private_extern _Or__uOr_param_u16_C
_Or__uOr_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u16_u16
	.private_extern _Or__uOr_var_u16_u16
_Or__uOr_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _Or__uOr_param_u16_u16
	.private_extern _Or__uOr_param_u16_u16
_Or__uOr_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_u64
	.private_extern _Or__uOr_var_u16_u64
_Or__uOr_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_u64
	.private_extern _Or__uOr_param_u16_u64
_Or__uOr_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_L
	.private_extern _Or__uOr_var_u16_L
_Or__uOr_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_L
	.private_extern _Or__uOr_param_u16_L
_Or__uOr_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_i16
	.private_extern _Or__uOr_var_u16_i16
_Or__uOr_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_i16
	.private_extern _Or__uOr_param_u16_i16
_Or__uOr_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u16_i32
	.private_extern _Or__uOr_var_u16_i32
_Or__uOr_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u16_i32
	.private_extern _Or__uOr_param_u16_i32
_Or__uOr_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_i64
	.private_extern _Or__uOr_var_u64_i64
_Or__uOr_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_i64
	.private_extern _Or__uOr_param_u64_i64
_Or__uOr_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_LC
	.private_extern _Or__uOr_var_u64_LC
_Or__uOr_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_LC
	.private_extern _Or__uOr_param_u64_LC
_Or__uOr_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u64_u8
	.private_extern _Or__uOr_var_u64_u8
_Or__uOr_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_u8
	.private_extern _Or__uOr_param_u64_u8
_Or__uOr_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_I
	.private_extern _Or__uOr_var_u64_I
_Or__uOr_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_I
	.private_extern _Or__uOr_param_u64_I
_Or__uOr_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_i8
	.private_extern _Or__uOr_var_u64_i8
_Or__uOr_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_i8
	.private_extern _Or__uOr_param_u64_i8
_Or__uOr_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_u32
	.private_extern _Or__uOr_var_u64_u32
_Or__uOr_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_u32
	.private_extern _Or__uOr_param_u64_u32
_Or__uOr_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_C
	.private_extern _Or__uOr_var_u64_C
_Or__uOr_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_C
	.private_extern _Or__uOr_param_u64_C
_Or__uOr_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_u64_u16
	.private_extern _Or__uOr_var_u64_u16
_Or__uOr_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_u16
	.private_extern _Or__uOr_param_u64_u16
_Or__uOr_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_u64
	.private_extern _Or__uOr_var_u64_u64
_Or__uOr_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rax
	leave
	ret
.globl _Or__uOr_param_u64_u64
	.private_extern _Or__uOr_param_u64_u64
_Or__uOr_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_L
	.private_extern _Or__uOr_var_u64_L
_Or__uOr_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_L
	.private_extern _Or__uOr_param_u64_L
_Or__uOr_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_i16
	.private_extern _Or__uOr_var_u64_i16
_Or__uOr_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_i16
	.private_extern _Or__uOr_param_u64_i16
_Or__uOr_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_u64_i32
	.private_extern _Or__uOr_var_u64_i32
_Or__uOr_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_u64_i32
	.private_extern _Or__uOr_param_u64_i32
_Or__uOr_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_i64
	.private_extern _Or__uOr_var_L_i64
_Or__uOr_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_i64
	.private_extern _Or__uOr_param_L_i64
_Or__uOr_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_LC
	.private_extern _Or__uOr_var_L_LC
_Or__uOr_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_LC
	.private_extern _Or__uOr_param_L_LC
_Or__uOr_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_L_u8
	.private_extern _Or__uOr_var_L_u8
_Or__uOr_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_u8
	.private_extern _Or__uOr_param_L_u8
_Or__uOr_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_I
	.private_extern _Or__uOr_var_L_I
_Or__uOr_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_I
	.private_extern _Or__uOr_param_L_I
_Or__uOr_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_i8
	.private_extern _Or__uOr_var_L_i8
_Or__uOr_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_i8
	.private_extern _Or__uOr_param_L_i8
_Or__uOr_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_u32
	.private_extern _Or__uOr_var_L_u32
_Or__uOr_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_u32
	.private_extern _Or__uOr_param_L_u32
_Or__uOr_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_C
	.private_extern _Or__uOr_var_L_C
_Or__uOr_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_C
	.private_extern _Or__uOr_param_L_C
_Or__uOr_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_L_u16
	.private_extern _Or__uOr_var_L_u16
_Or__uOr_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_u16
	.private_extern _Or__uOr_param_L_u16
_Or__uOr_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_u64
	.private_extern _Or__uOr_var_L_u64
_Or__uOr_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_u64
	.private_extern _Or__uOr_param_L_u64
_Or__uOr_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_L
	.private_extern _Or__uOr_var_L_L
_Or__uOr_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Or(%rip), %rax
	leave
	ret
.globl _Or__uOr_param_L_L
	.private_extern _Or__uOr_param_L_L
_Or__uOr_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_i16
	.private_extern _Or__uOr_var_L_i16
_Or__uOr_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_i16
	.private_extern _Or__uOr_param_L_i16
_Or__uOr_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_L_i32
	.private_extern _Or__uOr_var_L_i32
_Or__uOr_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_L_i32
	.private_extern _Or__uOr_param_L_i32
_Or__uOr_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_i64
	.private_extern _Or__uOr_var_i16_i64
_Or__uOr_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_i64
	.private_extern _Or__uOr_param_i16_i64
_Or__uOr_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_LC
	.private_extern _Or__uOr_var_i16_LC
_Or__uOr_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_LC
	.private_extern _Or__uOr_param_i16_LC
_Or__uOr_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i16_u8
	.private_extern _Or__uOr_var_i16_u8
_Or__uOr_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_u8
	.private_extern _Or__uOr_param_i16_u8
_Or__uOr_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_I
	.private_extern _Or__uOr_var_i16_I
_Or__uOr_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_I
	.private_extern _Or__uOr_param_i16_I
_Or__uOr_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_i8
	.private_extern _Or__uOr_var_i16_i8
_Or__uOr_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_i8
	.private_extern _Or__uOr_param_i16_i8
_Or__uOr_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_u32
	.private_extern _Or__uOr_var_i16_u32
_Or__uOr_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_u32
	.private_extern _Or__uOr_param_i16_u32
_Or__uOr_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_C
	.private_extern _Or__uOr_var_i16_C
_Or__uOr_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_C
	.private_extern _Or__uOr_param_i16_C
_Or__uOr_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i16_u16
	.private_extern _Or__uOr_var_i16_u16
_Or__uOr_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_u16
	.private_extern _Or__uOr_param_i16_u16
_Or__uOr_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_u64
	.private_extern _Or__uOr_var_i16_u64
_Or__uOr_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_u64
	.private_extern _Or__uOr_param_i16_u64
_Or__uOr_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_L
	.private_extern _Or__uOr_var_i16_L
_Or__uOr_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_L
	.private_extern _Or__uOr_param_i16_L
_Or__uOr_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_i16
	.private_extern _Or__uOr_var_i16_i16
_Or__uOr_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _Or__uOr_param_i16_i16
	.private_extern _Or__uOr_param_i16_i16
_Or__uOr_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i16_i32
	.private_extern _Or__uOr_var_i16_i32
_Or__uOr_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i16_i32
	.private_extern _Or__uOr_param_i16_i32
_Or__uOr_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_i64
	.private_extern _Or__uOr_var_i32_i64
_Or__uOr_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_i64
	.private_extern _Or__uOr_param_i32_i64
_Or__uOr_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_LC
	.private_extern _Or__uOr_var_i32_LC
_Or__uOr_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_LC
	.private_extern _Or__uOr_param_i32_LC
_Or__uOr_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i32_u8
	.private_extern _Or__uOr_var_i32_u8
_Or__uOr_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Or(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_u8
	.private_extern _Or__uOr_param_i32_u8
_Or__uOr_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_I
	.private_extern _Or__uOr_var_i32_I
_Or__uOr_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_I
	.private_extern _Or__uOr_param_i32_I
_Or__uOr_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_i8
	.private_extern _Or__uOr_var_i32_i8
_Or__uOr_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Or(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_i8
	.private_extern _Or__uOr_param_i32_i8
_Or__uOr_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_u32
	.private_extern _Or__uOr_var_i32_u32
_Or__uOr_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Or(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_u32
	.private_extern _Or__uOr_param_i32_u32
_Or__uOr_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_C
	.private_extern _Or__uOr_var_i32_C
_Or__uOr_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_C
	.private_extern _Or__uOr_param_i32_C
_Or__uOr_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	orq	-32(%rbp), %rax
	leave
	ret
.globl _Or__uOr_var_i32_u16
	.private_extern _Or__uOr_var_i32_u16
_Or__uOr_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Or(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_u16
	.private_extern _Or__uOr_param_i32_u16
_Or__uOr_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_u64
	.private_extern _Or__uOr_var_i32_u64
_Or__uOr_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_u64
	.private_extern _Or__uOr_param_i32_u64
_Or__uOr_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_L
	.private_extern _Or__uOr_var_i32_L
_Or__uOr_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Or(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_L
	.private_extern _Or__uOr_param_i32_L
_Or__uOr_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_i16
	.private_extern _Or__uOr_var_i32_i16
_Or__uOr_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Or(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_param_i32_i16
	.private_extern _Or__uOr_param_i32_i16
_Or__uOr_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or__uOr_var_i32_i32
	.private_extern _Or__uOr_var_i32_i32
_Or__uOr_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Or(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _Or__uOr_param_i32_i32
	.private_extern _Or__uOr_param_i32_i32
_Or__uOr_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _Or_M3
_Or_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Or(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Or_M3"
	.space 1
	.ascii "uOr_param_i32_i32"
	.space 1
	.ascii "uOr_var_i32_i32"
	.space 1
	.ascii "uOr_param_i32_i16"
	.space 1
	.ascii "uOr_var_i32_i16"
	.space 1
	.ascii "uOr_param_i32_L"
	.space 1
	.ascii "uOr_var_i32_L"
	.space 1
	.ascii "uOr_param_i32_u64"
	.space 1
	.ascii "uOr_var_i32_u64"
	.space 1
	.ascii "uOr_param_i32_u16"
	.space 1
	.ascii "uOr_var_i32_u16"
	.space 1
	.ascii "uOr_param_i32_C"
	.space 1
	.ascii "uOr_var_i32_C"
	.space 1
	.ascii "uOr_param_i32_u32"
	.space 1
	.ascii "uOr_var_i32_u32"
	.space 1
	.ascii "uOr_param_i32_i8"
	.space 1
	.ascii "uOr_var_i32_i8"
	.space 1
	.ascii "uOr_param_i32_I"
	.space 1
	.ascii "uOr_var_i32_I"
	.space 1
	.ascii "uOr_param_i32_u8"
	.space 1
	.ascii "uOr_var_i32_u8"
	.space 1
	.ascii "uOr_param_i32_LC"
	.space 1
	.ascii "uOr_var_i32_LC"
	.space 1
	.ascii "uOr_param_i32_i64"
	.space 1
	.ascii "uOr_var_i32_i64"
	.space 1
	.ascii "uOr_param_i16_i32"
	.space 1
	.ascii "uOr_var_i16_i32"
	.space 1
	.ascii "uOr_param_i16_i16"
	.space 1
	.ascii "uOr_var_i16_i16"
	.space 1
	.ascii "uOr_param_i16_L"
	.space 1
	.ascii "uOr_var_i16_L"
	.space 1
	.ascii "uOr_param_i16_u64"
	.space 1
	.ascii "uOr_var_i16_u64"
	.space 1
	.ascii "uOr_param_i16_u16"
	.space 1
	.ascii "uOr_var_i16_u16"
	.space 1
	.ascii "uOr_param_i16_C"
	.space 1
	.ascii "uOr_var_i16_C"
	.space 1
	.ascii "uOr_param_i16_u32"
	.space 1
	.ascii "uOr_var_i16_u32"
	.space 1
	.ascii "uOr_param_i16_i8"
	.space 1
	.ascii "uOr_var_i16_i8"
	.space 1
	.ascii "uOr_param_i16_I"
	.space 1
	.ascii "uOr_var_i16_I"
	.space 1
	.ascii "uOr_param_i16_u8"
	.space 1
	.ascii "uOr_var_i16_u8"
	.space 1
	.ascii "uOr_param_i16_LC"
	.space 1
	.ascii "uOr_var_i16_LC"
	.space 1
	.ascii "uOr_param_i16_i64"
	.space 1
	.ascii "uOr_var_i16_i64"
	.space 1
	.ascii "uOr_param_L_i32"
	.space 1
	.ascii "uOr_var_L_i32"
	.space 1
	.ascii "uOr_param_L_i16"
	.space 1
	.ascii "uOr_var_L_i16"
	.space 1
	.ascii "uOr_param_L_L"
	.space 1
	.ascii "uOr_var_L_L"
	.space 1
	.ascii "uOr_param_L_u64"
	.space 1
	.ascii "uOr_var_L_u64"
	.space 1
	.ascii "uOr_param_L_u16"
	.space 1
	.ascii "uOr_var_L_u16"
	.space 1
	.ascii "uOr_param_L_C"
	.space 1
	.ascii "uOr_var_L_C"
	.space 1
	.ascii "uOr_param_L_u32"
	.space 1
	.ascii "uOr_var_L_u32"
	.space 1
	.ascii "uOr_param_L_i8"
	.space 1
	.ascii "uOr_var_L_i8"
	.space 1
	.ascii "uOr_param_L_I"
	.space 1
	.ascii "uOr_var_L_I"
	.space 1
	.ascii "uOr_param_L_u8"
	.space 1
	.ascii "uOr_var_L_u8"
	.space 1
	.ascii "uOr_param_L_LC"
	.space 1
	.ascii "uOr_var_L_LC"
	.space 1
	.ascii "uOr_param_L_i64"
	.space 1
	.ascii "uOr_var_L_i64"
	.space 1
	.ascii "uOr_param_u64_i32"
	.space 1
	.ascii "uOr_var_u64_i32"
	.space 1
	.ascii "uOr_param_u64_i16"
	.space 1
	.ascii "uOr_var_u64_i16"
	.space 1
	.ascii "uOr_param_u64_L"
	.space 1
	.ascii "uOr_var_u64_L"
	.space 1
	.ascii "uOr_param_u64_u64"
	.space 1
	.ascii "uOr_var_u64_u64"
	.space 1
	.ascii "uOr_param_u64_u16"
	.space 1
	.ascii "uOr_var_u64_u16"
	.space 1
	.ascii "uOr_param_u64_C"
	.space 1
	.ascii "uOr_var_u64_C"
	.space 1
	.ascii "uOr_param_u64_u32"
	.space 1
	.ascii "uOr_var_u64_u32"
	.space 1
	.ascii "uOr_param_u64_i8"
	.space 1
	.ascii "uOr_var_u64_i8"
	.space 1
	.ascii "uOr_param_u64_I"
	.space 1
	.ascii "uOr_var_u64_I"
	.space 1
	.ascii "uOr_param_u64_u8"
	.space 1
	.ascii "uOr_var_u64_u8"
	.space 1
	.ascii "uOr_param_u64_LC"
	.space 1
	.ascii "uOr_var_u64_LC"
	.space 1
	.ascii "uOr_param_u64_i64"
	.space 1
	.ascii "uOr_var_u64_i64"
	.space 1
	.ascii "uOr_param_u16_i32"
	.space 1
	.ascii "uOr_var_u16_i32"
	.space 1
	.ascii "uOr_param_u16_i16"
	.space 1
	.ascii "uOr_var_u16_i16"
	.space 1
	.ascii "uOr_param_u16_L"
	.space 1
	.ascii "uOr_var_u16_L"
	.space 1
	.ascii "uOr_param_u16_u64"
	.space 1
	.ascii "uOr_var_u16_u64"
	.space 1
	.ascii "uOr_param_u16_u16"
	.space 1
	.ascii "uOr_var_u16_u16"
	.space 1
	.ascii "uOr_param_u16_C"
	.space 1
	.ascii "uOr_var_u16_C"
	.space 1
	.ascii "uOr_param_u16_u32"
	.space 1
	.ascii "uOr_var_u16_u32"
	.space 1
	.ascii "uOr_param_u16_i8"
	.space 1
	.ascii "uOr_var_u16_i8"
	.space 1
	.ascii "uOr_param_u16_I"
	.space 1
	.ascii "uOr_var_u16_I"
	.space 1
	.ascii "uOr_param_u16_u8"
	.space 1
	.ascii "uOr_var_u16_u8"
	.space 1
	.ascii "uOr_param_u16_LC"
	.space 1
	.ascii "uOr_var_u16_LC"
	.space 1
	.ascii "uOr_param_u16_i64"
	.space 1
	.ascii "uOr_var_u16_i64"
	.space 1
	.ascii "uOr_param_C_i32"
	.space 1
	.ascii "uOr_var_C_i32"
	.space 1
	.ascii "uOr_param_C_i16"
	.space 1
	.ascii "uOr_var_C_i16"
	.space 1
	.ascii "uOr_param_C_L"
	.space 1
	.ascii "uOr_var_C_L"
	.space 1
	.ascii "uOr_param_C_u64"
	.space 1
	.ascii "uOr_var_C_u64"
	.space 1
	.ascii "uOr_param_C_u16"
	.space 1
	.ascii "uOr_var_C_u16"
	.space 1
	.ascii "uOr_param_C_C"
	.space 1
	.ascii "uOr_var_C_C"
	.space 1
	.ascii "uOr_param_C_u32"
	.space 1
	.ascii "uOr_var_C_u32"
	.space 1
	.ascii "uOr_param_C_i8"
	.space 1
	.ascii "uOr_var_C_i8"
	.space 1
	.ascii "uOr_param_C_I"
	.space 1
	.ascii "uOr_var_C_I"
	.space 1
	.ascii "uOr_param_C_u8"
	.space 1
	.ascii "uOr_var_C_u8"
	.space 1
	.ascii "uOr_param_C_LC"
	.space 1
	.ascii "uOr_var_C_LC"
	.space 1
	.ascii "uOr_param_C_i64"
	.space 1
	.ascii "uOr_var_C_i64"
	.space 1
	.ascii "uOr_param_u32_i32"
	.space 1
	.ascii "uOr_var_u32_i32"
	.space 1
	.ascii "uOr_param_u32_i16"
	.space 1
	.ascii "uOr_var_u32_i16"
	.space 1
	.ascii "uOr_param_u32_L"
	.space 1
	.ascii "uOr_var_u32_L"
	.space 1
	.ascii "uOr_param_u32_u64"
	.space 1
	.ascii "uOr_var_u32_u64"
	.space 1
	.ascii "uOr_param_u32_u16"
	.space 1
	.ascii "uOr_var_u32_u16"
	.space 1
	.ascii "uOr_param_u32_C"
	.space 1
	.ascii "uOr_var_u32_C"
	.space 1
	.ascii "uOr_param_u32_u32"
	.space 1
	.ascii "uOr_var_u32_u32"
	.space 1
	.ascii "uOr_param_u32_i8"
	.space 1
	.ascii "uOr_var_u32_i8"
	.space 1
	.ascii "uOr_param_u32_I"
	.space 1
	.ascii "uOr_var_u32_I"
	.space 1
	.ascii "uOr_param_u32_u8"
	.space 1
	.ascii "uOr_var_u32_u8"
	.space 1
	.ascii "uOr_param_u32_LC"
	.space 1
	.ascii "uOr_var_u32_LC"
	.space 1
	.ascii "uOr_param_u32_i64"
	.space 1
	.ascii "uOr_var_u32_i64"
	.space 1
	.ascii "uOr_param_i8_i32"
	.space 1
	.ascii "uOr_var_i8_i32"
	.space 1
	.ascii "uOr_param_i8_i16"
	.space 1
	.ascii "uOr_var_i8_i16"
	.space 1
	.ascii "uOr_param_i8_L"
	.space 1
	.ascii "uOr_var_i8_L"
	.space 1
	.ascii "uOr_param_i8_u64"
	.space 1
	.ascii "uOr_var_i8_u64"
	.space 1
	.ascii "uOr_param_i8_u16"
	.space 1
	.ascii "uOr_var_i8_u16"
	.space 1
	.ascii "uOr_param_i8_C"
	.space 1
	.ascii "uOr_var_i8_C"
	.space 1
	.ascii "uOr_param_i8_u32"
	.space 1
	.ascii "uOr_var_i8_u32"
	.space 1
	.ascii "uOr_param_i8_i8"
	.space 1
	.ascii "uOr_var_i8_i8"
	.space 1
	.ascii "uOr_param_i8_I"
	.space 1
	.ascii "uOr_var_i8_I"
	.space 1
	.ascii "uOr_param_i8_u8"
	.space 1
	.ascii "uOr_var_i8_u8"
	.space 1
	.ascii "uOr_param_i8_LC"
	.space 1
	.ascii "uOr_var_i8_LC"
	.space 1
	.ascii "uOr_param_i8_i64"
	.space 1
	.ascii "uOr_var_i8_i64"
	.space 1
	.ascii "uOr_param_I_i32"
	.space 1
	.ascii "uOr_var_I_i32"
	.space 1
	.ascii "uOr_param_I_i16"
	.space 1
	.ascii "uOr_var_I_i16"
	.space 1
	.ascii "uOr_param_I_L"
	.space 1
	.ascii "uOr_var_I_L"
	.space 1
	.ascii "uOr_param_I_u64"
	.space 1
	.ascii "uOr_var_I_u64"
	.space 1
	.ascii "uOr_param_I_u16"
	.space 1
	.ascii "uOr_var_I_u16"
	.space 1
	.ascii "uOr_param_I_C"
	.space 1
	.ascii "uOr_var_I_C"
	.space 1
	.ascii "uOr_param_I_u32"
	.space 1
	.ascii "uOr_var_I_u32"
	.space 1
	.ascii "uOr_param_I_i8"
	.space 1
	.ascii "uOr_var_I_i8"
	.space 1
	.ascii "uOr_param_I_I"
	.space 1
	.ascii "uOr_var_I_I"
	.space 1
	.ascii "uOr_param_I_u8"
	.space 1
	.ascii "uOr_var_I_u8"
	.space 1
	.ascii "uOr_param_I_LC"
	.space 1
	.ascii "uOr_var_I_LC"
	.space 1
	.ascii "uOr_param_I_i64"
	.space 1
	.ascii "uOr_var_I_i64"
	.space 1
	.ascii "uOr_param_u8_i32"
	.space 1
	.ascii "uOr_var_u8_i32"
	.space 1
	.ascii "uOr_param_u8_i16"
	.space 1
	.ascii "uOr_var_u8_i16"
	.space 1
	.ascii "uOr_param_u8_L"
	.space 1
	.ascii "uOr_var_u8_L"
	.space 1
	.ascii "uOr_param_u8_u64"
	.space 1
	.ascii "uOr_var_u8_u64"
	.space 1
	.ascii "uOr_param_u8_u16"
	.space 1
	.ascii "uOr_var_u8_u16"
	.space 1
	.ascii "uOr_param_u8_C"
	.space 1
	.ascii "uOr_var_u8_C"
	.space 1
	.ascii "uOr_param_u8_u32"
	.space 1
	.ascii "uOr_var_u8_u32"
	.space 1
	.ascii "uOr_param_u8_i8"
	.space 1
	.ascii "uOr_var_u8_i8"
	.space 1
	.ascii "uOr_param_u8_I"
	.space 1
	.ascii "uOr_var_u8_I"
	.space 1
	.ascii "uOr_param_u8_u8"
	.space 1
	.ascii "uOr_var_u8_u8"
	.space 1
	.ascii "uOr_param_u8_LC"
	.space 1
	.ascii "uOr_var_u8_LC"
	.space 1
	.ascii "uOr_param_u8_i64"
	.space 1
	.ascii "uOr_var_u8_i64"
	.space 1
	.ascii "uOr_param_LC_i32"
	.space 1
	.ascii "uOr_var_LC_i32"
	.space 1
	.ascii "uOr_param_LC_i16"
	.space 1
	.ascii "uOr_var_LC_i16"
	.space 1
	.ascii "uOr_param_LC_L"
	.space 1
	.ascii "uOr_var_LC_L"
	.space 1
	.ascii "uOr_param_LC_u64"
	.space 1
	.ascii "uOr_var_LC_u64"
	.space 1
	.ascii "uOr_param_LC_u16"
	.space 1
	.ascii "uOr_var_LC_u16"
	.space 1
	.ascii "uOr_param_LC_C"
	.space 1
	.ascii "uOr_var_LC_C"
	.space 1
	.ascii "uOr_param_LC_u32"
	.space 1
	.ascii "uOr_var_LC_u32"
	.space 1
	.ascii "uOr_param_LC_i8"
	.space 1
	.ascii "uOr_var_LC_i8"
	.space 1
	.ascii "uOr_param_LC_I"
	.space 1
	.ascii "uOr_var_LC_I"
	.space 1
	.ascii "uOr_param_LC_u8"
	.space 1
	.ascii "uOr_var_LC_u8"
	.space 1
	.ascii "uOr_param_LC_LC"
	.space 1
	.ascii "uOr_var_LC_LC"
	.space 1
	.ascii "uOr_param_LC_i64"
	.space 1
	.ascii "uOr_var_LC_i64"
	.space 1
	.ascii "uOr_param_i64_i32"
	.space 1
	.ascii "uOr_var_i64_i32"
	.space 1
	.ascii "uOr_param_i64_i16"
	.space 1
	.ascii "uOr_var_i64_i16"
	.space 1
	.ascii "uOr_param_i64_L"
	.space 1
	.ascii "uOr_var_i64_L"
	.space 1
	.ascii "uOr_param_i64_u64"
	.space 1
	.ascii "uOr_var_i64_u64"
	.space 1
	.ascii "uOr_param_i64_u16"
	.space 1
	.ascii "uOr_var_i64_u16"
	.space 1
	.ascii "uOr_param_i64_C"
	.space 1
	.ascii "uOr_var_i64_C"
	.space 1
	.ascii "uOr_param_i64_u32"
	.space 1
	.ascii "uOr_var_i64_u32"
	.space 1
	.ascii "uOr_param_i64_i8"
	.space 1
	.ascii "uOr_var_i64_i8"
	.space 1
	.ascii "uOr_param_i64_I"
	.space 1
	.ascii "uOr_var_i64_I"
	.space 1
	.ascii "uOr_param_i64_u8"
	.space 1
	.ascii "uOr_var_i64_u8"
	.space 1
	.ascii "uOr_param_i64_LC"
	.space 1
	.ascii "uOr_var_i64_LC"
	.space 1
	.ascii "uOr_param_i64_i64"
	.space 1
	.ascii "uOr_var_i64_i64"
	.space 3
	.quad	_Or_M3
	.quad	_L_1
	.quad	_Or__uOr_param_i32_i32
	.quad	_L_1+6
	.quad	_Or__uOr_var_i32_i32
	.quad	_L_1+24
	.quad	_Or__uOr_param_i32_i16
	.quad	_L_1+40
	.quad	_Or__uOr_var_i32_i16
	.quad	_L_1+58
	.quad	_Or__uOr_param_i32_L
	.quad	_L_1+74
	.quad	_Or__uOr_var_i32_L
	.quad	_L_1+90
	.quad	_Or__uOr_param_i32_u64
	.quad	_L_1+104
	.quad	_Or__uOr_var_i32_u64
	.quad	_L_1+122
	.quad	_Or__uOr_param_i32_u16
	.quad	_L_1+138
	.quad	_Or__uOr_var_i32_u16
	.quad	_L_1+156
	.quad	_Or__uOr_param_i32_C
	.quad	_L_1+172
	.quad	_Or__uOr_var_i32_C
	.quad	_L_1+188
	.quad	_Or__uOr_param_i32_u32
	.quad	_L_1+202
	.quad	_Or__uOr_var_i32_u32
	.quad	_L_1+220
	.quad	_Or__uOr_param_i32_i8
	.quad	_L_1+236
	.quad	_Or__uOr_var_i32_i8
	.quad	_L_1+253
	.quad	_Or__uOr_param_i32_I
	.quad	_L_1+268
	.quad	_Or__uOr_var_i32_I
	.quad	_L_1+284
	.quad	_Or__uOr_param_i32_u8
	.quad	_L_1+298
	.quad	_Or__uOr_var_i32_u8
	.quad	_L_1+315
	.quad	_Or__uOr_param_i32_LC
	.quad	_L_1+330
	.quad	_Or__uOr_var_i32_LC
	.quad	_L_1+347
	.quad	_Or__uOr_param_i32_i64
	.quad	_L_1+362
	.quad	_Or__uOr_var_i32_i64
	.quad	_L_1+380
	.quad	_Or__uOr_param_i16_i32
	.quad	_L_1+396
	.quad	_Or__uOr_var_i16_i32
	.quad	_L_1+414
	.quad	_Or__uOr_param_i16_i16
	.quad	_L_1+430
	.quad	_Or__uOr_var_i16_i16
	.quad	_L_1+448
	.quad	_Or__uOr_param_i16_L
	.quad	_L_1+464
	.quad	_Or__uOr_var_i16_L
	.quad	_L_1+480
	.quad	_Or__uOr_param_i16_u64
	.quad	_L_1+494
	.quad	_Or__uOr_var_i16_u64
	.quad	_L_1+512
	.quad	_Or__uOr_param_i16_u16
	.quad	_L_1+528
	.quad	_Or__uOr_var_i16_u16
	.quad	_L_1+546
	.quad	_Or__uOr_param_i16_C
	.quad	_L_1+562
	.quad	_Or__uOr_var_i16_C
	.quad	_L_1+578
	.quad	_Or__uOr_param_i16_u32
	.quad	_L_1+592
	.quad	_Or__uOr_var_i16_u32
	.quad	_L_1+610
	.quad	_Or__uOr_param_i16_i8
	.quad	_L_1+626
	.quad	_Or__uOr_var_i16_i8
	.quad	_L_1+643
	.quad	_Or__uOr_param_i16_I
	.quad	_L_1+658
	.quad	_Or__uOr_var_i16_I
	.quad	_L_1+674
	.quad	_Or__uOr_param_i16_u8
	.quad	_L_1+688
	.quad	_Or__uOr_var_i16_u8
	.quad	_L_1+705
	.quad	_Or__uOr_param_i16_LC
	.quad	_L_1+720
	.quad	_Or__uOr_var_i16_LC
	.quad	_L_1+737
	.quad	_Or__uOr_param_i16_i64
	.quad	_L_1+752
	.quad	_Or__uOr_var_i16_i64
	.quad	_L_1+770
	.quad	_Or__uOr_param_L_i32
	.quad	_L_1+786
	.quad	_Or__uOr_var_L_i32
	.quad	_L_1+802
	.quad	_Or__uOr_param_L_i16
	.quad	_L_1+816
	.quad	_Or__uOr_var_L_i16
	.quad	_L_1+832
	.quad	_Or__uOr_param_L_L
	.quad	_L_1+846
	.quad	_Or__uOr_var_L_L
	.quad	_L_1+860
	.quad	_Or__uOr_param_L_u64
	.quad	_L_1+872
	.quad	_Or__uOr_var_L_u64
	.quad	_L_1+888
	.quad	_Or__uOr_param_L_u16
	.quad	_L_1+902
	.quad	_Or__uOr_var_L_u16
	.quad	_L_1+918
	.quad	_Or__uOr_param_L_C
	.quad	_L_1+932
	.quad	_Or__uOr_var_L_C
	.quad	_L_1+946
	.quad	_Or__uOr_param_L_u32
	.quad	_L_1+958
	.quad	_Or__uOr_var_L_u32
	.quad	_L_1+974
	.quad	_Or__uOr_param_L_i8
	.quad	_L_1+988
	.quad	_Or__uOr_var_L_i8
	.quad	_L_1+1003
	.quad	_Or__uOr_param_L_I
	.quad	_L_1+1016
	.quad	_Or__uOr_var_L_I
	.quad	_L_1+1030
	.quad	_Or__uOr_param_L_u8
	.quad	_L_1+1042
	.quad	_Or__uOr_var_L_u8
	.quad	_L_1+1057
	.quad	_Or__uOr_param_L_LC
	.quad	_L_1+1070
	.quad	_Or__uOr_var_L_LC
	.quad	_L_1+1085
	.quad	_Or__uOr_param_L_i64
	.quad	_L_1+1098
	.quad	_Or__uOr_var_L_i64
	.quad	_L_1+1114
	.quad	_Or__uOr_param_u64_i32
	.quad	_L_1+1128
	.quad	_Or__uOr_var_u64_i32
	.quad	_L_1+1146
	.quad	_Or__uOr_param_u64_i16
	.quad	_L_1+1162
	.quad	_Or__uOr_var_u64_i16
	.quad	_L_1+1180
	.quad	_Or__uOr_param_u64_L
	.quad	_L_1+1196
	.quad	_Or__uOr_var_u64_L
	.quad	_L_1+1212
	.quad	_Or__uOr_param_u64_u64
	.quad	_L_1+1226
	.quad	_Or__uOr_var_u64_u64
	.quad	_L_1+1244
	.quad	_Or__uOr_param_u64_u16
	.quad	_L_1+1260
	.quad	_Or__uOr_var_u64_u16
	.quad	_L_1+1278
	.quad	_Or__uOr_param_u64_C
	.quad	_L_1+1294
	.quad	_Or__uOr_var_u64_C
	.quad	_L_1+1310
	.quad	_Or__uOr_param_u64_u32
	.quad	_L_1+1324
	.quad	_Or__uOr_var_u64_u32
	.quad	_L_1+1342
	.quad	_Or__uOr_param_u64_i8
	.quad	_L_1+1358
	.quad	_Or__uOr_var_u64_i8
	.quad	_L_1+1375
	.quad	_Or__uOr_param_u64_I
	.quad	_L_1+1390
	.quad	_Or__uOr_var_u64_I
	.quad	_L_1+1406
	.quad	_Or__uOr_param_u64_u8
	.quad	_L_1+1420
	.quad	_Or__uOr_var_u64_u8
	.quad	_L_1+1437
	.quad	_Or__uOr_param_u64_LC
	.quad	_L_1+1452
	.quad	_Or__uOr_var_u64_LC
	.quad	_L_1+1469
	.quad	_Or__uOr_param_u64_i64
	.quad	_L_1+1484
	.quad	_Or__uOr_var_u64_i64
	.quad	_L_1+1502
	.quad	_Or__uOr_param_u16_i32
	.quad	_L_1+1518
	.quad	_Or__uOr_var_u16_i32
	.quad	_L_1+1536
	.quad	_Or__uOr_param_u16_i16
	.quad	_L_1+1552
	.quad	_Or__uOr_var_u16_i16
	.quad	_L_1+1570
	.quad	_Or__uOr_param_u16_L
	.quad	_L_1+1586
	.quad	_Or__uOr_var_u16_L
	.quad	_L_1+1602
	.quad	_Or__uOr_param_u16_u64
	.quad	_L_1+1616
	.quad	_Or__uOr_var_u16_u64
	.quad	_L_1+1634
	.quad	_Or__uOr_param_u16_u16
	.quad	_L_1+1650
	.quad	_Or__uOr_var_u16_u16
	.quad	_L_1+1668
	.quad	_Or__uOr_param_u16_C
	.quad	_L_1+1684
	.quad	_Or__uOr_var_u16_C
	.quad	_L_1+1700
	.quad	_Or__uOr_param_u16_u32
	.quad	_L_1+1714
	.quad	_Or__uOr_var_u16_u32
	.quad	_L_1+1732
	.quad	_Or__uOr_param_u16_i8
	.quad	_L_1+1748
	.quad	_Or__uOr_var_u16_i8
	.quad	_L_1+1765
	.quad	_Or__uOr_param_u16_I
	.quad	_L_1+1780
	.quad	_Or__uOr_var_u16_I
	.quad	_L_1+1796
	.quad	_Or__uOr_param_u16_u8
	.quad	_L_1+1810
	.quad	_Or__uOr_var_u16_u8
	.quad	_L_1+1827
	.quad	_Or__uOr_param_u16_LC
	.quad	_L_1+1842
	.quad	_Or__uOr_var_u16_LC
	.quad	_L_1+1859
	.quad	_Or__uOr_param_u16_i64
	.quad	_L_1+1874
	.quad	_Or__uOr_var_u16_i64
	.quad	_L_1+1892
	.quad	_Or__uOr_param_C_i32
	.quad	_L_1+1908
	.quad	_Or__uOr_var_C_i32
	.quad	_L_1+1924
	.quad	_Or__uOr_param_C_i16
	.quad	_L_1+1938
	.quad	_Or__uOr_var_C_i16
	.quad	_L_1+1954
	.quad	_Or__uOr_param_C_L
	.quad	_L_1+1968
	.quad	_Or__uOr_var_C_L
	.quad	_L_1+1982
	.quad	_Or__uOr_param_C_u64
	.quad	_L_1+1994
	.quad	_Or__uOr_var_C_u64
	.quad	_L_1+2010
	.quad	_Or__uOr_param_C_u16
	.quad	_L_1+2024
	.quad	_Or__uOr_var_C_u16
	.quad	_L_1+2040
	.quad	_Or__uOr_param_C_C
	.quad	_L_1+2054
	.quad	_Or__uOr_var_C_C
	.quad	_L_1+2068
	.quad	_Or__uOr_param_C_u32
	.quad	_L_1+2080
	.quad	_Or__uOr_var_C_u32
	.quad	_L_1+2096
	.quad	_Or__uOr_param_C_i8
	.quad	_L_1+2110
	.quad	_Or__uOr_var_C_i8
	.quad	_L_1+2125
	.quad	_Or__uOr_param_C_I
	.quad	_L_1+2138
	.quad	_Or__uOr_var_C_I
	.quad	_L_1+2152
	.quad	_Or__uOr_param_C_u8
	.quad	_L_1+2164
	.quad	_Or__uOr_var_C_u8
	.quad	_L_1+2179
	.quad	_Or__uOr_param_C_LC
	.quad	_L_1+2192
	.quad	_Or__uOr_var_C_LC
	.quad	_L_1+2207
	.quad	_Or__uOr_param_C_i64
	.quad	_L_1+2220
	.quad	_Or__uOr_var_C_i64
	.quad	_L_1+2236
	.quad	_Or__uOr_param_u32_i32
	.quad	_L_1+2250
	.quad	_Or__uOr_var_u32_i32
	.quad	_L_1+2268
	.quad	_Or__uOr_param_u32_i16
	.quad	_L_1+2284
	.quad	_Or__uOr_var_u32_i16
	.quad	_L_1+2302
	.quad	_Or__uOr_param_u32_L
	.quad	_L_1+2318
	.quad	_Or__uOr_var_u32_L
	.quad	_L_1+2334
	.quad	_Or__uOr_param_u32_u64
	.quad	_L_1+2348
	.quad	_Or__uOr_var_u32_u64
	.quad	_L_1+2366
	.quad	_Or__uOr_param_u32_u16
	.quad	_L_1+2382
	.quad	_Or__uOr_var_u32_u16
	.quad	_L_1+2400
	.quad	_Or__uOr_param_u32_C
	.quad	_L_1+2416
	.quad	_Or__uOr_var_u32_C
	.quad	_L_1+2432
	.quad	_Or__uOr_param_u32_u32
	.quad	_L_1+2446
	.quad	_Or__uOr_var_u32_u32
	.quad	_L_1+2464
	.quad	_Or__uOr_param_u32_i8
	.quad	_L_1+2480
	.quad	_Or__uOr_var_u32_i8
	.quad	_L_1+2497
	.quad	_Or__uOr_param_u32_I
	.quad	_L_1+2512
	.quad	_Or__uOr_var_u32_I
	.quad	_L_1+2528
	.quad	_Or__uOr_param_u32_u8
	.quad	_L_1+2542
	.quad	_Or__uOr_var_u32_u8
	.quad	_L_1+2559
	.quad	_Or__uOr_param_u32_LC
	.quad	_L_1+2574
	.quad	_Or__uOr_var_u32_LC
	.quad	_L_1+2591
	.quad	_Or__uOr_param_u32_i64
	.quad	_L_1+2606
	.quad	_Or__uOr_var_u32_i64
	.quad	_L_1+2624
	.quad	_Or__uOr_param_i8_i32
	.quad	_L_1+2640
	.quad	_Or__uOr_var_i8_i32
	.quad	_L_1+2657
	.quad	_Or__uOr_param_i8_i16
	.quad	_L_1+2672
	.quad	_Or__uOr_var_i8_i16
	.quad	_L_1+2689
	.quad	_Or__uOr_param_i8_L
	.quad	_L_1+2704
	.quad	_Or__uOr_var_i8_L
	.quad	_L_1+2719
	.quad	_Or__uOr_param_i8_u64
	.quad	_L_1+2732
	.quad	_Or__uOr_var_i8_u64
	.quad	_L_1+2749
	.quad	_Or__uOr_param_i8_u16
	.quad	_L_1+2764
	.quad	_Or__uOr_var_i8_u16
	.quad	_L_1+2781
	.quad	_Or__uOr_param_i8_C
	.quad	_L_1+2796
	.quad	_Or__uOr_var_i8_C
	.quad	_L_1+2811
	.quad	_Or__uOr_param_i8_u32
	.quad	_L_1+2824
	.quad	_Or__uOr_var_i8_u32
	.quad	_L_1+2841
	.quad	_Or__uOr_param_i8_i8
	.quad	_L_1+2856
	.quad	_Or__uOr_var_i8_i8
	.quad	_L_1+2872
	.quad	_Or__uOr_param_i8_I
	.quad	_L_1+2886
	.quad	_Or__uOr_var_i8_I
	.quad	_L_1+2901
	.quad	_Or__uOr_param_i8_u8
	.quad	_L_1+2914
	.quad	_Or__uOr_var_i8_u8
	.quad	_L_1+2930
	.quad	_Or__uOr_param_i8_LC
	.quad	_L_1+2944
	.quad	_Or__uOr_var_i8_LC
	.quad	_L_1+2960
	.quad	_Or__uOr_param_i8_i64
	.quad	_L_1+2974
	.quad	_Or__uOr_var_i8_i64
	.quad	_L_1+2991
	.quad	_Or__uOr_param_I_i32
	.quad	_L_1+3006
	.quad	_Or__uOr_var_I_i32
	.quad	_L_1+3022
	.quad	_Or__uOr_param_I_i16
	.quad	_L_1+3036
	.quad	_Or__uOr_var_I_i16
	.quad	_L_1+3052
	.quad	_Or__uOr_param_I_L
	.quad	_L_1+3066
	.quad	_Or__uOr_var_I_L
	.quad	_L_1+3080
	.quad	_Or__uOr_param_I_u64
	.quad	_L_1+3092
	.quad	_Or__uOr_var_I_u64
	.quad	_L_1+3108
	.quad	_Or__uOr_param_I_u16
	.quad	_L_1+3122
	.quad	_Or__uOr_var_I_u16
	.quad	_L_1+3138
	.quad	_Or__uOr_param_I_C
	.quad	_L_1+3152
	.quad	_Or__uOr_var_I_C
	.quad	_L_1+3166
	.quad	_Or__uOr_param_I_u32
	.quad	_L_1+3178
	.quad	_Or__uOr_var_I_u32
	.quad	_L_1+3194
	.quad	_Or__uOr_param_I_i8
	.quad	_L_1+3208
	.quad	_Or__uOr_var_I_i8
	.quad	_L_1+3223
	.quad	_Or__uOr_param_I_I
	.quad	_L_1+3236
	.quad	_Or__uOr_var_I_I
	.quad	_L_1+3250
	.quad	_Or__uOr_param_I_u8
	.quad	_L_1+3262
	.quad	_Or__uOr_var_I_u8
	.quad	_L_1+3277
	.quad	_Or__uOr_param_I_LC
	.quad	_L_1+3290
	.quad	_Or__uOr_var_I_LC
	.quad	_L_1+3305
	.quad	_Or__uOr_param_I_i64
	.quad	_L_1+3318
	.quad	_Or__uOr_var_I_i64
	.quad	_L_1+3334
	.quad	_Or__uOr_param_u8_i32
	.quad	_L_1+3348
	.quad	_Or__uOr_var_u8_i32
	.quad	_L_1+3365
	.quad	_Or__uOr_param_u8_i16
	.quad	_L_1+3380
	.quad	_Or__uOr_var_u8_i16
	.quad	_L_1+3397
	.quad	_Or__uOr_param_u8_L
	.quad	_L_1+3412
	.quad	_Or__uOr_var_u8_L
	.quad	_L_1+3427
	.quad	_Or__uOr_param_u8_u64
	.quad	_L_1+3440
	.quad	_Or__uOr_var_u8_u64
	.quad	_L_1+3457
	.quad	_Or__uOr_param_u8_u16
	.quad	_L_1+3472
	.quad	_Or__uOr_var_u8_u16
	.quad	_L_1+3489
	.quad	_Or__uOr_param_u8_C
	.quad	_L_1+3504
	.quad	_Or__uOr_var_u8_C
	.quad	_L_1+3519
	.quad	_Or__uOr_param_u8_u32
	.quad	_L_1+3532
	.quad	_Or__uOr_var_u8_u32
	.quad	_L_1+3549
	.quad	_Or__uOr_param_u8_i8
	.quad	_L_1+3564
	.quad	_Or__uOr_var_u8_i8
	.quad	_L_1+3580
	.quad	_Or__uOr_param_u8_I
	.quad	_L_1+3594
	.quad	_Or__uOr_var_u8_I
	.quad	_L_1+3609
	.quad	_Or__uOr_param_u8_u8
	.quad	_L_1+3622
	.quad	_Or__uOr_var_u8_u8
	.quad	_L_1+3638
	.quad	_Or__uOr_param_u8_LC
	.quad	_L_1+3652
	.quad	_Or__uOr_var_u8_LC
	.quad	_L_1+3668
	.quad	_Or__uOr_param_u8_i64
	.quad	_L_1+3682
	.quad	_Or__uOr_var_u8_i64
	.quad	_L_1+3699
	.quad	_Or__uOr_param_LC_i32
	.quad	_L_1+3714
	.quad	_Or__uOr_var_LC_i32
	.quad	_L_1+3731
	.quad	_Or__uOr_param_LC_i16
	.quad	_L_1+3746
	.quad	_Or__uOr_var_LC_i16
	.quad	_L_1+3763
	.quad	_Or__uOr_param_LC_L
	.quad	_L_1+3778
	.quad	_Or__uOr_var_LC_L
	.quad	_L_1+3793
	.quad	_Or__uOr_param_LC_u64
	.quad	_L_1+3806
	.quad	_Or__uOr_var_LC_u64
	.quad	_L_1+3823
	.quad	_Or__uOr_param_LC_u16
	.quad	_L_1+3838
	.quad	_Or__uOr_var_LC_u16
	.quad	_L_1+3855
	.quad	_Or__uOr_param_LC_C
	.quad	_L_1+3870
	.quad	_Or__uOr_var_LC_C
	.quad	_L_1+3885
	.quad	_Or__uOr_param_LC_u32
	.quad	_L_1+3898
	.quad	_Or__uOr_var_LC_u32
	.quad	_L_1+3915
	.quad	_Or__uOr_param_LC_i8
	.quad	_L_1+3930
	.quad	_Or__uOr_var_LC_i8
	.quad	_L_1+3946
	.quad	_Or__uOr_param_LC_I
	.quad	_L_1+3960
	.quad	_Or__uOr_var_LC_I
	.quad	_L_1+3975
	.quad	_Or__uOr_param_LC_u8
	.quad	_L_1+3988
	.quad	_Or__uOr_var_LC_u8
	.quad	_L_1+4004
	.quad	_Or__uOr_param_LC_LC
	.quad	_L_1+4018
	.quad	_Or__uOr_var_LC_LC
	.quad	_L_1+4034
	.quad	_Or__uOr_param_LC_i64
	.quad	_L_1+4048
	.quad	_Or__uOr_var_LC_i64
	.quad	_L_1+4065
	.quad	_Or__uOr_param_i64_i32
	.quad	_L_1+4080
	.quad	_Or__uOr_var_i64_i32
	.quad	_L_1+4098
	.quad	_Or__uOr_param_i64_i16
	.quad	_L_1+4114
	.quad	_Or__uOr_var_i64_i16
	.quad	_L_1+4132
	.quad	_Or__uOr_param_i64_L
	.quad	_L_1+4148
	.quad	_Or__uOr_var_i64_L
	.quad	_L_1+4164
	.quad	_Or__uOr_param_i64_u64
	.quad	_L_1+4178
	.quad	_Or__uOr_var_i64_u64
	.quad	_L_1+4196
	.quad	_Or__uOr_param_i64_u16
	.quad	_L_1+4212
	.quad	_Or__uOr_var_i64_u16
	.quad	_L_1+4230
	.quad	_Or__uOr_param_i64_C
	.quad	_L_1+4246
	.quad	_Or__uOr_var_i64_C
	.quad	_L_1+4262
	.quad	_Or__uOr_param_i64_u32
	.quad	_L_1+4276
	.quad	_Or__uOr_var_i64_u32
	.quad	_L_1+4294
	.quad	_Or__uOr_param_i64_i8
	.quad	_L_1+4310
	.quad	_Or__uOr_var_i64_i8
	.quad	_L_1+4327
	.quad	_Or__uOr_param_i64_I
	.quad	_L_1+4342
	.quad	_Or__uOr_var_i64_I
	.quad	_L_1+4358
	.quad	_Or__uOr_param_i64_u8
	.quad	_L_1+4372
	.quad	_Or__uOr_var_i64_u8
	.quad	_L_1+4389
	.quad	_Or__uOr_param_i64_LC
	.quad	_L_1+4404
	.quad	_Or__uOr_var_i64_LC
	.quad	_L_1+4421
	.quad	_Or__uOr_param_i64_i64
	.quad	_L_1+4436
	.quad	_Or__uOr_var_i64_i64
	.quad	_L_1+4454
	.space 8
	.ascii "../AMD64_DARWIN/Or.m3"
	.space 3
	.data
	.align 5
_MM_Or:
	.quad	_L_1+9104
	.space 32
	.quad	_L_1+4472
	.space 24
	.quad	_MM_Or+208
	.space 8
	.quad	_Or_M3
	.quad	3
	.quad	465
	.quad	466
	.long	1139391463
	.byte	-43
	.space 3
	.quad	470
	.byte	-41
	.space 3
	.long	472
	.quad	473
	.word	474
	.space 6
	.quad	475
	.long	3401614098
	.long	1081984929
	.quad	478
	.word	479
	.space 2
	.long	480
	.space 24
	.quad	_Or_I3
	.quad	_MM_Or+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Or+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Or+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Or+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
