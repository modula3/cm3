	.text
.globl _And__uAnd_var_i64_i64
	.private_extern _And__uAnd_var_i64_i64
_And__uAnd_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_And(%rip), %rax
	leave
	ret
.globl _And__uAnd_param_i64_i64
	.private_extern _And__uAnd_param_i64_i64
_And__uAnd_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_LC
	.private_extern _And__uAnd_var_i64_LC
_And__uAnd_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_LC
	.private_extern _And__uAnd_param_i64_LC
_And__uAnd_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i64_u8
	.private_extern _And__uAnd_var_i64_u8
_And__uAnd_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_u8
	.private_extern _And__uAnd_param_i64_u8
_And__uAnd_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_I
	.private_extern _And__uAnd_var_i64_I
_And__uAnd_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_I
	.private_extern _And__uAnd_param_i64_I
_And__uAnd_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_i8
	.private_extern _And__uAnd_var_i64_i8
_And__uAnd_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_i8
	.private_extern _And__uAnd_param_i64_i8
_And__uAnd_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_u32
	.private_extern _And__uAnd_var_i64_u32
_And__uAnd_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_u32
	.private_extern _And__uAnd_param_i64_u32
_And__uAnd_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_C
	.private_extern _And__uAnd_var_i64_C
_And__uAnd_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_C
	.private_extern _And__uAnd_param_i64_C
_And__uAnd_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i64_u16
	.private_extern _And__uAnd_var_i64_u16
_And__uAnd_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_u16
	.private_extern _And__uAnd_param_i64_u16
_And__uAnd_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_u64
	.private_extern _And__uAnd_var_i64_u64
_And__uAnd_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_u64
	.private_extern _And__uAnd_param_i64_u64
_And__uAnd_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_L
	.private_extern _And__uAnd_var_i64_L
_And__uAnd_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_L
	.private_extern _And__uAnd_param_i64_L
_And__uAnd_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_i16
	.private_extern _And__uAnd_var_i64_i16
_And__uAnd_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_i16
	.private_extern _And__uAnd_param_i64_i16
_And__uAnd_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i64_i32
	.private_extern _And__uAnd_var_i64_i32
_And__uAnd_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i64_i32
	.private_extern _And__uAnd_param_i64_i32
_And__uAnd_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_LC_i64
	.private_extern _And__uAnd_var_LC_i64
_And__uAnd_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_And(%rip), %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_i64
	.private_extern _And__uAnd_param_LC_i64
_And__uAnd_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_LC
	.private_extern _And__uAnd_var_LC_LC
_And__uAnd_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_And(%rip), %rax
	leave
	ret
.globl _And__uAnd_param_LC_LC
	.private_extern _And__uAnd_param_LC_LC
_And__uAnd_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_LC_u8
	.private_extern _And__uAnd_var_LC_u8
_And__uAnd_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_u8
	.private_extern _And__uAnd_param_LC_u8
_And__uAnd_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_I
	.private_extern _And__uAnd_var_LC_I
_And__uAnd_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_I
	.private_extern _And__uAnd_param_LC_I
_And__uAnd_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_i8
	.private_extern _And__uAnd_var_LC_i8
_And__uAnd_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_i8
	.private_extern _And__uAnd_param_LC_i8
_And__uAnd_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_u32
	.private_extern _And__uAnd_var_LC_u32
_And__uAnd_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_u32
	.private_extern _And__uAnd_param_LC_u32
_And__uAnd_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_C
	.private_extern _And__uAnd_var_LC_C
_And__uAnd_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_C
	.private_extern _And__uAnd_param_LC_C
_And__uAnd_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_LC_u16
	.private_extern _And__uAnd_var_LC_u16
_And__uAnd_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_u16
	.private_extern _And__uAnd_param_LC_u16
_And__uAnd_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_u64
	.private_extern _And__uAnd_var_LC_u64
_And__uAnd_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_And(%rip), %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_u64
	.private_extern _And__uAnd_param_LC_u64
_And__uAnd_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_L
	.private_extern _And__uAnd_var_LC_L
_And__uAnd_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_And(%rip), %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_L
	.private_extern _And__uAnd_param_LC_L
_And__uAnd_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_i16
	.private_extern _And__uAnd_var_LC_i16
_And__uAnd_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_i16
	.private_extern _And__uAnd_param_LC_i16
_And__uAnd_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_LC_i32
	.private_extern _And__uAnd_var_LC_i32
_And__uAnd_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_LC_i32
	.private_extern _And__uAnd_param_LC_i32
_And__uAnd_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u8_i64
	.private_extern _And__uAnd_var_u8_i64
_And__uAnd_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_i64
	.private_extern _And__uAnd_param_u8_i64
_And__uAnd_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_LC
	.private_extern _And__uAnd_var_u8_LC
_And__uAnd_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_LC
	.private_extern _And__uAnd_param_u8_LC
_And__uAnd_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u8_u8
	.private_extern _And__uAnd_var_u8_u8
_And__uAnd_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _And__uAnd_param_u8_u8
	.private_extern _And__uAnd_param_u8_u8
_And__uAnd_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_I
	.private_extern _And__uAnd_var_u8_I
_And__uAnd_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_I
	.private_extern _And__uAnd_param_u8_I
_And__uAnd_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_i8
	.private_extern _And__uAnd_var_u8_i8
_And__uAnd_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_i8
	.private_extern _And__uAnd_param_u8_i8
_And__uAnd_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_u32
	.private_extern _And__uAnd_var_u8_u32
_And__uAnd_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_u32
	.private_extern _And__uAnd_param_u8_u32
_And__uAnd_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_C
	.private_extern _And__uAnd_var_u8_C
_And__uAnd_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_C
	.private_extern _And__uAnd_param_u8_C
_And__uAnd_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u8_u16
	.private_extern _And__uAnd_var_u8_u16
_And__uAnd_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_u16
	.private_extern _And__uAnd_param_u8_u16
_And__uAnd_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_u64
	.private_extern _And__uAnd_var_u8_u64
_And__uAnd_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_u64
	.private_extern _And__uAnd_param_u8_u64
_And__uAnd_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_L
	.private_extern _And__uAnd_var_u8_L
_And__uAnd_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_L
	.private_extern _And__uAnd_param_u8_L
_And__uAnd_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_i16
	.private_extern _And__uAnd_var_u8_i16
_And__uAnd_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_i16
	.private_extern _And__uAnd_param_u8_i16
_And__uAnd_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u8_i32
	.private_extern _And__uAnd_var_u8_i32
_And__uAnd_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u8_i32
	.private_extern _And__uAnd_param_u8_i32
_And__uAnd_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_i64
	.private_extern _And__uAnd_var_I_i64
_And__uAnd_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_i64
	.private_extern _And__uAnd_param_I_i64
_And__uAnd_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_LC
	.private_extern _And__uAnd_var_I_LC
_And__uAnd_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_LC
	.private_extern _And__uAnd_param_I_LC
_And__uAnd_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_I_u8
	.private_extern _And__uAnd_var_I_u8
_And__uAnd_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_u8
	.private_extern _And__uAnd_param_I_u8
_And__uAnd_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_I
	.private_extern _And__uAnd_var_I_I
_And__uAnd_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	leave
	ret
.globl _And__uAnd_param_I_I
	.private_extern _And__uAnd_param_I_I
_And__uAnd_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_i8
	.private_extern _And__uAnd_var_I_i8
_And__uAnd_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_i8
	.private_extern _And__uAnd_param_I_i8
_And__uAnd_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_u32
	.private_extern _And__uAnd_var_I_u32
_And__uAnd_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_u32
	.private_extern _And__uAnd_param_I_u32
_And__uAnd_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_C
	.private_extern _And__uAnd_var_I_C
_And__uAnd_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_C
	.private_extern _And__uAnd_param_I_C
_And__uAnd_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_I_u16
	.private_extern _And__uAnd_var_I_u16
_And__uAnd_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_u16
	.private_extern _And__uAnd_param_I_u16
_And__uAnd_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_u64
	.private_extern _And__uAnd_var_I_u64
_And__uAnd_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_u64
	.private_extern _And__uAnd_param_I_u64
_And__uAnd_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_L
	.private_extern _And__uAnd_var_I_L
_And__uAnd_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_L
	.private_extern _And__uAnd_param_I_L
_And__uAnd_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_i16
	.private_extern _And__uAnd_var_I_i16
_And__uAnd_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_i16
	.private_extern _And__uAnd_param_I_i16
_And__uAnd_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_I_i32
	.private_extern _And__uAnd_var_I_i32
_And__uAnd_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_I_i32
	.private_extern _And__uAnd_param_I_i32
_And__uAnd_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_i64
	.private_extern _And__uAnd_var_i8_i64
_And__uAnd_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_i64
	.private_extern _And__uAnd_param_i8_i64
_And__uAnd_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_LC
	.private_extern _And__uAnd_var_i8_LC
_And__uAnd_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_LC
	.private_extern _And__uAnd_param_i8_LC
_And__uAnd_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i8_u8
	.private_extern _And__uAnd_var_i8_u8
_And__uAnd_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_u8
	.private_extern _And__uAnd_param_i8_u8
_And__uAnd_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_I
	.private_extern _And__uAnd_var_i8_I
_And__uAnd_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_I
	.private_extern _And__uAnd_param_i8_I
_And__uAnd_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_i8
	.private_extern _And__uAnd_var_i8_i8
_And__uAnd_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _And__uAnd_param_i8_i8
	.private_extern _And__uAnd_param_i8_i8
_And__uAnd_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_u32
	.private_extern _And__uAnd_var_i8_u32
_And__uAnd_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_u32
	.private_extern _And__uAnd_param_i8_u32
_And__uAnd_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_C
	.private_extern _And__uAnd_var_i8_C
_And__uAnd_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_C
	.private_extern _And__uAnd_param_i8_C
_And__uAnd_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i8_u16
	.private_extern _And__uAnd_var_i8_u16
_And__uAnd_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_u16
	.private_extern _And__uAnd_param_i8_u16
_And__uAnd_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_u64
	.private_extern _And__uAnd_var_i8_u64
_And__uAnd_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_u64
	.private_extern _And__uAnd_param_i8_u64
_And__uAnd_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_L
	.private_extern _And__uAnd_var_i8_L
_And__uAnd_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_L
	.private_extern _And__uAnd_param_i8_L
_And__uAnd_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_i16
	.private_extern _And__uAnd_var_i8_i16
_And__uAnd_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_i16
	.private_extern _And__uAnd_param_i8_i16
_And__uAnd_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i8_i32
	.private_extern _And__uAnd_var_i8_i32
_And__uAnd_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i8_i32
	.private_extern _And__uAnd_param_i8_i32
_And__uAnd_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_i64
	.private_extern _And__uAnd_var_u32_i64
_And__uAnd_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_i64
	.private_extern _And__uAnd_param_u32_i64
_And__uAnd_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_LC
	.private_extern _And__uAnd_var_u32_LC
_And__uAnd_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_LC
	.private_extern _And__uAnd_param_u32_LC
_And__uAnd_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u32_u8
	.private_extern _And__uAnd_var_u32_u8
_And__uAnd_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_u8
	.private_extern _And__uAnd_param_u32_u8
_And__uAnd_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_I
	.private_extern _And__uAnd_var_u32_I
_And__uAnd_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_I
	.private_extern _And__uAnd_param_u32_I
_And__uAnd_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_i8
	.private_extern _And__uAnd_var_u32_i8
_And__uAnd_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_i8
	.private_extern _And__uAnd_param_u32_i8
_And__uAnd_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_u32
	.private_extern _And__uAnd_var_u32_u32
_And__uAnd_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	cltq
	leave
	ret
.globl _And__uAnd_param_u32_u32
	.private_extern _And__uAnd_param_u32_u32
_And__uAnd_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_C
	.private_extern _And__uAnd_var_u32_C
_And__uAnd_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_C
	.private_extern _And__uAnd_param_u32_C
_And__uAnd_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u32_u16
	.private_extern _And__uAnd_var_u32_u16
_And__uAnd_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_u16
	.private_extern _And__uAnd_param_u32_u16
_And__uAnd_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_u64
	.private_extern _And__uAnd_var_u32_u64
_And__uAnd_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_u64
	.private_extern _And__uAnd_param_u32_u64
_And__uAnd_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_L
	.private_extern _And__uAnd_var_u32_L
_And__uAnd_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_L
	.private_extern _And__uAnd_param_u32_L
_And__uAnd_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_i16
	.private_extern _And__uAnd_var_u32_i16
_And__uAnd_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_i16
	.private_extern _And__uAnd_param_u32_i16
_And__uAnd_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u32_i32
	.private_extern _And__uAnd_var_u32_i32
_And__uAnd_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u32_i32
	.private_extern _And__uAnd_param_u32_i32
_And__uAnd_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_C_i64
	.private_extern _And__uAnd_var_C_i64
_And__uAnd_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_i64
	.private_extern _And__uAnd_param_C_i64
_And__uAnd_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_LC
	.private_extern _And__uAnd_var_C_LC
_And__uAnd_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_LC
	.private_extern _And__uAnd_param_C_LC
_And__uAnd_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_C_u8
	.private_extern _And__uAnd_var_C_u8
_And__uAnd_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_u8
	.private_extern _And__uAnd_param_C_u8
_And__uAnd_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_I
	.private_extern _And__uAnd_var_C_I
_And__uAnd_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_I
	.private_extern _And__uAnd_param_C_I
_And__uAnd_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_i8
	.private_extern _And__uAnd_var_C_i8
_And__uAnd_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_i8
	.private_extern _And__uAnd_param_C_i8
_And__uAnd_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_u32
	.private_extern _And__uAnd_var_C_u32
_And__uAnd_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_u32
	.private_extern _And__uAnd_param_C_u32
_And__uAnd_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_C
	.private_extern _And__uAnd_var_C_C
_And__uAnd_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rax
	leave
	ret
.globl _And__uAnd_param_C_C
	.private_extern _And__uAnd_param_C_C
_And__uAnd_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_C_u16
	.private_extern _And__uAnd_var_C_u16
_And__uAnd_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_u16
	.private_extern _And__uAnd_param_C_u16
_And__uAnd_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_u64
	.private_extern _And__uAnd_var_C_u64
_And__uAnd_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_u64
	.private_extern _And__uAnd_param_C_u64
_And__uAnd_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_L
	.private_extern _And__uAnd_var_C_L
_And__uAnd_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_L
	.private_extern _And__uAnd_param_C_L
_And__uAnd_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_i16
	.private_extern _And__uAnd_var_C_i16
_And__uAnd_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_i16
	.private_extern _And__uAnd_param_C_i16
_And__uAnd_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_C_i32
	.private_extern _And__uAnd_var_C_i32
_And__uAnd_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_C_i32
	.private_extern _And__uAnd_param_C_i32
_And__uAnd_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u16_i64
	.private_extern _And__uAnd_var_u16_i64
_And__uAnd_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_i64
	.private_extern _And__uAnd_param_u16_i64
_And__uAnd_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_LC
	.private_extern _And__uAnd_var_u16_LC
_And__uAnd_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_LC
	.private_extern _And__uAnd_param_u16_LC
_And__uAnd_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u16_u8
	.private_extern _And__uAnd_var_u16_u8
_And__uAnd_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_u8
	.private_extern _And__uAnd_param_u16_u8
_And__uAnd_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_I
	.private_extern _And__uAnd_var_u16_I
_And__uAnd_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_I
	.private_extern _And__uAnd_param_u16_I
_And__uAnd_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_i8
	.private_extern _And__uAnd_var_u16_i8
_And__uAnd_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_i8
	.private_extern _And__uAnd_param_u16_i8
_And__uAnd_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_u32
	.private_extern _And__uAnd_var_u16_u32
_And__uAnd_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_u32
	.private_extern _And__uAnd_param_u16_u32
_And__uAnd_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_C
	.private_extern _And__uAnd_var_u16_C
_And__uAnd_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_C
	.private_extern _And__uAnd_param_u16_C
_And__uAnd_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u16_u16
	.private_extern _And__uAnd_var_u16_u16
_And__uAnd_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _And__uAnd_param_u16_u16
	.private_extern _And__uAnd_param_u16_u16
_And__uAnd_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_u64
	.private_extern _And__uAnd_var_u16_u64
_And__uAnd_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_u64
	.private_extern _And__uAnd_param_u16_u64
_And__uAnd_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_L
	.private_extern _And__uAnd_var_u16_L
_And__uAnd_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_L
	.private_extern _And__uAnd_param_u16_L
_And__uAnd_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_i16
	.private_extern _And__uAnd_var_u16_i16
_And__uAnd_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_i16
	.private_extern _And__uAnd_param_u16_i16
_And__uAnd_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u16_i32
	.private_extern _And__uAnd_var_u16_i32
_And__uAnd_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u16_i32
	.private_extern _And__uAnd_param_u16_i32
_And__uAnd_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_i64
	.private_extern _And__uAnd_var_u64_i64
_And__uAnd_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_i64
	.private_extern _And__uAnd_param_u64_i64
_And__uAnd_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_LC
	.private_extern _And__uAnd_var_u64_LC
_And__uAnd_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_LC
	.private_extern _And__uAnd_param_u64_LC
_And__uAnd_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u64_u8
	.private_extern _And__uAnd_var_u64_u8
_And__uAnd_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_u8
	.private_extern _And__uAnd_param_u64_u8
_And__uAnd_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_I
	.private_extern _And__uAnd_var_u64_I
_And__uAnd_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_I
	.private_extern _And__uAnd_param_u64_I
_And__uAnd_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_i8
	.private_extern _And__uAnd_var_u64_i8
_And__uAnd_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_i8
	.private_extern _And__uAnd_param_u64_i8
_And__uAnd_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_u32
	.private_extern _And__uAnd_var_u64_u32
_And__uAnd_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_u32
	.private_extern _And__uAnd_param_u64_u32
_And__uAnd_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_C
	.private_extern _And__uAnd_var_u64_C
_And__uAnd_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_C
	.private_extern _And__uAnd_param_u64_C
_And__uAnd_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_u64_u16
	.private_extern _And__uAnd_var_u64_u16
_And__uAnd_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_u16
	.private_extern _And__uAnd_param_u64_u16
_And__uAnd_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_u64
	.private_extern _And__uAnd_var_u64_u64
_And__uAnd_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_And(%rip), %rax
	leave
	ret
.globl _And__uAnd_param_u64_u64
	.private_extern _And__uAnd_param_u64_u64
_And__uAnd_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_L
	.private_extern _And__uAnd_var_u64_L
_And__uAnd_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_L
	.private_extern _And__uAnd_param_u64_L
_And__uAnd_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_i16
	.private_extern _And__uAnd_var_u64_i16
_And__uAnd_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_i16
	.private_extern _And__uAnd_param_u64_i16
_And__uAnd_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_u64_i32
	.private_extern _And__uAnd_var_u64_i32
_And__uAnd_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_u64_i32
	.private_extern _And__uAnd_param_u64_i32
_And__uAnd_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_i64
	.private_extern _And__uAnd_var_L_i64
_And__uAnd_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_i64
	.private_extern _And__uAnd_param_L_i64
_And__uAnd_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_LC
	.private_extern _And__uAnd_var_L_LC
_And__uAnd_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_LC
	.private_extern _And__uAnd_param_L_LC
_And__uAnd_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_L_u8
	.private_extern _And__uAnd_var_L_u8
_And__uAnd_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_u8
	.private_extern _And__uAnd_param_L_u8
_And__uAnd_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_I
	.private_extern _And__uAnd_var_L_I
_And__uAnd_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_I
	.private_extern _And__uAnd_param_L_I
_And__uAnd_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_i8
	.private_extern _And__uAnd_var_L_i8
_And__uAnd_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_i8
	.private_extern _And__uAnd_param_L_i8
_And__uAnd_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_u32
	.private_extern _And__uAnd_var_L_u32
_And__uAnd_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_And(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_u32
	.private_extern _And__uAnd_param_L_u32
_And__uAnd_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_C
	.private_extern _And__uAnd_var_L_C
_And__uAnd_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_And(%rip), %rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_C
	.private_extern _And__uAnd_param_L_C
_And__uAnd_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_L_u16
	.private_extern _And__uAnd_var_L_u16
_And__uAnd_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_u16
	.private_extern _And__uAnd_param_L_u16
_And__uAnd_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_u64
	.private_extern _And__uAnd_var_L_u64
_And__uAnd_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_And(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_u64
	.private_extern _And__uAnd_param_L_u64
_And__uAnd_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_L
	.private_extern _And__uAnd_var_L_L
_And__uAnd_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_And(%rip), %rax
	leave
	ret
.globl _And__uAnd_param_L_L
	.private_extern _And__uAnd_param_L_L
_And__uAnd_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_i16
	.private_extern _And__uAnd_var_L_i16
_And__uAnd_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_i16
	.private_extern _And__uAnd_param_L_i16
_And__uAnd_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_L_i32
	.private_extern _And__uAnd_var_L_i32
_And__uAnd_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_L_i32
	.private_extern _And__uAnd_param_L_i32
_And__uAnd_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_i64
	.private_extern _And__uAnd_var_i16_i64
_And__uAnd_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_i64
	.private_extern _And__uAnd_param_i16_i64
_And__uAnd_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_LC
	.private_extern _And__uAnd_var_i16_LC
_And__uAnd_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_LC
	.private_extern _And__uAnd_param_i16_LC
_And__uAnd_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i16_u8
	.private_extern _And__uAnd_var_i16_u8
_And__uAnd_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_u8
	.private_extern _And__uAnd_param_i16_u8
_And__uAnd_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_I
	.private_extern _And__uAnd_var_i16_I
_And__uAnd_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_I
	.private_extern _And__uAnd_param_i16_I
_And__uAnd_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_i8
	.private_extern _And__uAnd_var_i16_i8
_And__uAnd_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_i8
	.private_extern _And__uAnd_param_i16_i8
_And__uAnd_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_u32
	.private_extern _And__uAnd_var_i16_u32
_And__uAnd_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_u32
	.private_extern _And__uAnd_param_i16_u32
_And__uAnd_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_C
	.private_extern _And__uAnd_var_i16_C
_And__uAnd_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_C
	.private_extern _And__uAnd_param_i16_C
_And__uAnd_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i16_u16
	.private_extern _And__uAnd_var_i16_u16
_And__uAnd_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_u16
	.private_extern _And__uAnd_param_i16_u16
_And__uAnd_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_u64
	.private_extern _And__uAnd_var_i16_u64
_And__uAnd_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_u64
	.private_extern _And__uAnd_param_i16_u64
_And__uAnd_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_L
	.private_extern _And__uAnd_var_i16_L
_And__uAnd_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_L
	.private_extern _And__uAnd_param_i16_L
_And__uAnd_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_i16
	.private_extern _And__uAnd_var_i16_i16
_And__uAnd_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _And__uAnd_param_i16_i16
	.private_extern _And__uAnd_param_i16_i16
_And__uAnd_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i16_i32
	.private_extern _And__uAnd_var_i16_i32
_And__uAnd_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i16_i32
	.private_extern _And__uAnd_param_i16_i32
_And__uAnd_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_i64
	.private_extern _And__uAnd_var_i32_i64
_And__uAnd_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_i64
	.private_extern _And__uAnd_param_i32_i64
_And__uAnd_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_LC
	.private_extern _And__uAnd_var_i32_LC
_And__uAnd_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_LC
	.private_extern _And__uAnd_param_i32_LC
_And__uAnd_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i32_u8
	.private_extern _And__uAnd_var_i32_u8
_And__uAnd_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_And(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_u8
	.private_extern _And__uAnd_param_i32_u8
_And__uAnd_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_I
	.private_extern _And__uAnd_var_i32_I
_And__uAnd_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_I
	.private_extern _And__uAnd_param_i32_I
_And__uAnd_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_i8
	.private_extern _And__uAnd_var_i32_i8
_And__uAnd_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_And(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_i8
	.private_extern _And__uAnd_param_i32_i8
_And__uAnd_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_u32
	.private_extern _And__uAnd_var_i32_u32
_And__uAnd_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_And(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_u32
	.private_extern _And__uAnd_param_i32_u32
_And__uAnd_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_C
	.private_extern _And__uAnd_var_i32_C
_And__uAnd_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_C
	.private_extern _And__uAnd_param_i32_C
_And__uAnd_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	andq	-32(%rbp), %rax
	leave
	ret
.globl _And__uAnd_var_i32_u16
	.private_extern _And__uAnd_var_i32_u16
_And__uAnd_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_And(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_u16
	.private_extern _And__uAnd_param_i32_u16
_And__uAnd_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_u64
	.private_extern _And__uAnd_var_i32_u64
_And__uAnd_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_u64
	.private_extern _And__uAnd_param_i32_u64
_And__uAnd_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_L
	.private_extern _And__uAnd_var_i32_L
_And__uAnd_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_And(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_L
	.private_extern _And__uAnd_param_i32_L
_And__uAnd_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_i16
	.private_extern _And__uAnd_var_i32_i16
_And__uAnd_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_And(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_param_i32_i16
	.private_extern _And__uAnd_param_i32_i16
_And__uAnd_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And__uAnd_var_i32_i32
	.private_extern _And__uAnd_var_i32_i32
_And__uAnd_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_And(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _And__uAnd_param_i32_i32
	.private_extern _And__uAnd_param_i32_i32
_And__uAnd_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _And_M3
_And_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_And(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "And_M3"
	.space 1
	.ascii "uAnd_param_i32_i32"
	.space 1
	.ascii "uAnd_var_i32_i32"
	.space 1
	.ascii "uAnd_param_i32_i16"
	.space 1
	.ascii "uAnd_var_i32_i16"
	.space 1
	.ascii "uAnd_param_i32_L"
	.space 1
	.ascii "uAnd_var_i32_L"
	.space 1
	.ascii "uAnd_param_i32_u64"
	.space 1
	.ascii "uAnd_var_i32_u64"
	.space 1
	.ascii "uAnd_param_i32_u16"
	.space 1
	.ascii "uAnd_var_i32_u16"
	.space 1
	.ascii "uAnd_param_i32_C"
	.space 1
	.ascii "uAnd_var_i32_C"
	.space 1
	.ascii "uAnd_param_i32_u32"
	.space 1
	.ascii "uAnd_var_i32_u32"
	.space 1
	.ascii "uAnd_param_i32_i8"
	.space 1
	.ascii "uAnd_var_i32_i8"
	.space 1
	.ascii "uAnd_param_i32_I"
	.space 1
	.ascii "uAnd_var_i32_I"
	.space 1
	.ascii "uAnd_param_i32_u8"
	.space 1
	.ascii "uAnd_var_i32_u8"
	.space 1
	.ascii "uAnd_param_i32_LC"
	.space 1
	.ascii "uAnd_var_i32_LC"
	.space 1
	.ascii "uAnd_param_i32_i64"
	.space 1
	.ascii "uAnd_var_i32_i64"
	.space 1
	.ascii "uAnd_param_i16_i32"
	.space 1
	.ascii "uAnd_var_i16_i32"
	.space 1
	.ascii "uAnd_param_i16_i16"
	.space 1
	.ascii "uAnd_var_i16_i16"
	.space 1
	.ascii "uAnd_param_i16_L"
	.space 1
	.ascii "uAnd_var_i16_L"
	.space 1
	.ascii "uAnd_param_i16_u64"
	.space 1
	.ascii "uAnd_var_i16_u64"
	.space 1
	.ascii "uAnd_param_i16_u16"
	.space 1
	.ascii "uAnd_var_i16_u16"
	.space 1
	.ascii "uAnd_param_i16_C"
	.space 1
	.ascii "uAnd_var_i16_C"
	.space 1
	.ascii "uAnd_param_i16_u32"
	.space 1
	.ascii "uAnd_var_i16_u32"
	.space 1
	.ascii "uAnd_param_i16_i8"
	.space 1
	.ascii "uAnd_var_i16_i8"
	.space 1
	.ascii "uAnd_param_i16_I"
	.space 1
	.ascii "uAnd_var_i16_I"
	.space 1
	.ascii "uAnd_param_i16_u8"
	.space 1
	.ascii "uAnd_var_i16_u8"
	.space 1
	.ascii "uAnd_param_i16_LC"
	.space 1
	.ascii "uAnd_var_i16_LC"
	.space 1
	.ascii "uAnd_param_i16_i64"
	.space 1
	.ascii "uAnd_var_i16_i64"
	.space 1
	.ascii "uAnd_param_L_i32"
	.space 1
	.ascii "uAnd_var_L_i32"
	.space 1
	.ascii "uAnd_param_L_i16"
	.space 1
	.ascii "uAnd_var_L_i16"
	.space 1
	.ascii "uAnd_param_L_L"
	.space 1
	.ascii "uAnd_var_L_L"
	.space 1
	.ascii "uAnd_param_L_u64"
	.space 1
	.ascii "uAnd_var_L_u64"
	.space 1
	.ascii "uAnd_param_L_u16"
	.space 1
	.ascii "uAnd_var_L_u16"
	.space 1
	.ascii "uAnd_param_L_C"
	.space 1
	.ascii "uAnd_var_L_C"
	.space 1
	.ascii "uAnd_param_L_u32"
	.space 1
	.ascii "uAnd_var_L_u32"
	.space 1
	.ascii "uAnd_param_L_i8"
	.space 1
	.ascii "uAnd_var_L_i8"
	.space 1
	.ascii "uAnd_param_L_I"
	.space 1
	.ascii "uAnd_var_L_I"
	.space 1
	.ascii "uAnd_param_L_u8"
	.space 1
	.ascii "uAnd_var_L_u8"
	.space 1
	.ascii "uAnd_param_L_LC"
	.space 1
	.ascii "uAnd_var_L_LC"
	.space 1
	.ascii "uAnd_param_L_i64"
	.space 1
	.ascii "uAnd_var_L_i64"
	.space 1
	.ascii "uAnd_param_u64_i32"
	.space 1
	.ascii "uAnd_var_u64_i32"
	.space 1
	.ascii "uAnd_param_u64_i16"
	.space 1
	.ascii "uAnd_var_u64_i16"
	.space 1
	.ascii "uAnd_param_u64_L"
	.space 1
	.ascii "uAnd_var_u64_L"
	.space 1
	.ascii "uAnd_param_u64_u64"
	.space 1
	.ascii "uAnd_var_u64_u64"
	.space 1
	.ascii "uAnd_param_u64_u16"
	.space 1
	.ascii "uAnd_var_u64_u16"
	.space 1
	.ascii "uAnd_param_u64_C"
	.space 1
	.ascii "uAnd_var_u64_C"
	.space 1
	.ascii "uAnd_param_u64_u32"
	.space 1
	.ascii "uAnd_var_u64_u32"
	.space 1
	.ascii "uAnd_param_u64_i8"
	.space 1
	.ascii "uAnd_var_u64_i8"
	.space 1
	.ascii "uAnd_param_u64_I"
	.space 1
	.ascii "uAnd_var_u64_I"
	.space 1
	.ascii "uAnd_param_u64_u8"
	.space 1
	.ascii "uAnd_var_u64_u8"
	.space 1
	.ascii "uAnd_param_u64_LC"
	.space 1
	.ascii "uAnd_var_u64_LC"
	.space 1
	.ascii "uAnd_param_u64_i64"
	.space 1
	.ascii "uAnd_var_u64_i64"
	.space 1
	.ascii "uAnd_param_u16_i32"
	.space 1
	.ascii "uAnd_var_u16_i32"
	.space 1
	.ascii "uAnd_param_u16_i16"
	.space 1
	.ascii "uAnd_var_u16_i16"
	.space 1
	.ascii "uAnd_param_u16_L"
	.space 1
	.ascii "uAnd_var_u16_L"
	.space 1
	.ascii "uAnd_param_u16_u64"
	.space 1
	.ascii "uAnd_var_u16_u64"
	.space 1
	.ascii "uAnd_param_u16_u16"
	.space 1
	.ascii "uAnd_var_u16_u16"
	.space 1
	.ascii "uAnd_param_u16_C"
	.space 1
	.ascii "uAnd_var_u16_C"
	.space 1
	.ascii "uAnd_param_u16_u32"
	.space 1
	.ascii "uAnd_var_u16_u32"
	.space 1
	.ascii "uAnd_param_u16_i8"
	.space 1
	.ascii "uAnd_var_u16_i8"
	.space 1
	.ascii "uAnd_param_u16_I"
	.space 1
	.ascii "uAnd_var_u16_I"
	.space 1
	.ascii "uAnd_param_u16_u8"
	.space 1
	.ascii "uAnd_var_u16_u8"
	.space 1
	.ascii "uAnd_param_u16_LC"
	.space 1
	.ascii "uAnd_var_u16_LC"
	.space 1
	.ascii "uAnd_param_u16_i64"
	.space 1
	.ascii "uAnd_var_u16_i64"
	.space 1
	.ascii "uAnd_param_C_i32"
	.space 1
	.ascii "uAnd_var_C_i32"
	.space 1
	.ascii "uAnd_param_C_i16"
	.space 1
	.ascii "uAnd_var_C_i16"
	.space 1
	.ascii "uAnd_param_C_L"
	.space 1
	.ascii "uAnd_var_C_L"
	.space 1
	.ascii "uAnd_param_C_u64"
	.space 1
	.ascii "uAnd_var_C_u64"
	.space 1
	.ascii "uAnd_param_C_u16"
	.space 1
	.ascii "uAnd_var_C_u16"
	.space 1
	.ascii "uAnd_param_C_C"
	.space 1
	.ascii "uAnd_var_C_C"
	.space 1
	.ascii "uAnd_param_C_u32"
	.space 1
	.ascii "uAnd_var_C_u32"
	.space 1
	.ascii "uAnd_param_C_i8"
	.space 1
	.ascii "uAnd_var_C_i8"
	.space 1
	.ascii "uAnd_param_C_I"
	.space 1
	.ascii "uAnd_var_C_I"
	.space 1
	.ascii "uAnd_param_C_u8"
	.space 1
	.ascii "uAnd_var_C_u8"
	.space 1
	.ascii "uAnd_param_C_LC"
	.space 1
	.ascii "uAnd_var_C_LC"
	.space 1
	.ascii "uAnd_param_C_i64"
	.space 1
	.ascii "uAnd_var_C_i64"
	.space 1
	.ascii "uAnd_param_u32_i32"
	.space 1
	.ascii "uAnd_var_u32_i32"
	.space 1
	.ascii "uAnd_param_u32_i16"
	.space 1
	.ascii "uAnd_var_u32_i16"
	.space 1
	.ascii "uAnd_param_u32_L"
	.space 1
	.ascii "uAnd_var_u32_L"
	.space 1
	.ascii "uAnd_param_u32_u64"
	.space 1
	.ascii "uAnd_var_u32_u64"
	.space 1
	.ascii "uAnd_param_u32_u16"
	.space 1
	.ascii "uAnd_var_u32_u16"
	.space 1
	.ascii "uAnd_param_u32_C"
	.space 1
	.ascii "uAnd_var_u32_C"
	.space 1
	.ascii "uAnd_param_u32_u32"
	.space 1
	.ascii "uAnd_var_u32_u32"
	.space 1
	.ascii "uAnd_param_u32_i8"
	.space 1
	.ascii "uAnd_var_u32_i8"
	.space 1
	.ascii "uAnd_param_u32_I"
	.space 1
	.ascii "uAnd_var_u32_I"
	.space 1
	.ascii "uAnd_param_u32_u8"
	.space 1
	.ascii "uAnd_var_u32_u8"
	.space 1
	.ascii "uAnd_param_u32_LC"
	.space 1
	.ascii "uAnd_var_u32_LC"
	.space 1
	.ascii "uAnd_param_u32_i64"
	.space 1
	.ascii "uAnd_var_u32_i64"
	.space 1
	.ascii "uAnd_param_i8_i32"
	.space 1
	.ascii "uAnd_var_i8_i32"
	.space 1
	.ascii "uAnd_param_i8_i16"
	.space 1
	.ascii "uAnd_var_i8_i16"
	.space 1
	.ascii "uAnd_param_i8_L"
	.space 1
	.ascii "uAnd_var_i8_L"
	.space 1
	.ascii "uAnd_param_i8_u64"
	.space 1
	.ascii "uAnd_var_i8_u64"
	.space 1
	.ascii "uAnd_param_i8_u16"
	.space 1
	.ascii "uAnd_var_i8_u16"
	.space 1
	.ascii "uAnd_param_i8_C"
	.space 1
	.ascii "uAnd_var_i8_C"
	.space 1
	.ascii "uAnd_param_i8_u32"
	.space 1
	.ascii "uAnd_var_i8_u32"
	.space 1
	.ascii "uAnd_param_i8_i8"
	.space 1
	.ascii "uAnd_var_i8_i8"
	.space 1
	.ascii "uAnd_param_i8_I"
	.space 1
	.ascii "uAnd_var_i8_I"
	.space 1
	.ascii "uAnd_param_i8_u8"
	.space 1
	.ascii "uAnd_var_i8_u8"
	.space 1
	.ascii "uAnd_param_i8_LC"
	.space 1
	.ascii "uAnd_var_i8_LC"
	.space 1
	.ascii "uAnd_param_i8_i64"
	.space 1
	.ascii "uAnd_var_i8_i64"
	.space 1
	.ascii "uAnd_param_I_i32"
	.space 1
	.ascii "uAnd_var_I_i32"
	.space 1
	.ascii "uAnd_param_I_i16"
	.space 1
	.ascii "uAnd_var_I_i16"
	.space 1
	.ascii "uAnd_param_I_L"
	.space 1
	.ascii "uAnd_var_I_L"
	.space 1
	.ascii "uAnd_param_I_u64"
	.space 1
	.ascii "uAnd_var_I_u64"
	.space 1
	.ascii "uAnd_param_I_u16"
	.space 1
	.ascii "uAnd_var_I_u16"
	.space 1
	.ascii "uAnd_param_I_C"
	.space 1
	.ascii "uAnd_var_I_C"
	.space 1
	.ascii "uAnd_param_I_u32"
	.space 1
	.ascii "uAnd_var_I_u32"
	.space 1
	.ascii "uAnd_param_I_i8"
	.space 1
	.ascii "uAnd_var_I_i8"
	.space 1
	.ascii "uAnd_param_I_I"
	.space 1
	.ascii "uAnd_var_I_I"
	.space 1
	.ascii "uAnd_param_I_u8"
	.space 1
	.ascii "uAnd_var_I_u8"
	.space 1
	.ascii "uAnd_param_I_LC"
	.space 1
	.ascii "uAnd_var_I_LC"
	.space 1
	.ascii "uAnd_param_I_i64"
	.space 1
	.ascii "uAnd_var_I_i64"
	.space 1
	.ascii "uAnd_param_u8_i32"
	.space 1
	.ascii "uAnd_var_u8_i32"
	.space 1
	.ascii "uAnd_param_u8_i16"
	.space 1
	.ascii "uAnd_var_u8_i16"
	.space 1
	.ascii "uAnd_param_u8_L"
	.space 1
	.ascii "uAnd_var_u8_L"
	.space 1
	.ascii "uAnd_param_u8_u64"
	.space 1
	.ascii "uAnd_var_u8_u64"
	.space 1
	.ascii "uAnd_param_u8_u16"
	.space 1
	.ascii "uAnd_var_u8_u16"
	.space 1
	.ascii "uAnd_param_u8_C"
	.space 1
	.ascii "uAnd_var_u8_C"
	.space 1
	.ascii "uAnd_param_u8_u32"
	.space 1
	.ascii "uAnd_var_u8_u32"
	.space 1
	.ascii "uAnd_param_u8_i8"
	.space 1
	.ascii "uAnd_var_u8_i8"
	.space 1
	.ascii "uAnd_param_u8_I"
	.space 1
	.ascii "uAnd_var_u8_I"
	.space 1
	.ascii "uAnd_param_u8_u8"
	.space 1
	.ascii "uAnd_var_u8_u8"
	.space 1
	.ascii "uAnd_param_u8_LC"
	.space 1
	.ascii "uAnd_var_u8_LC"
	.space 1
	.ascii "uAnd_param_u8_i64"
	.space 1
	.ascii "uAnd_var_u8_i64"
	.space 1
	.ascii "uAnd_param_LC_i32"
	.space 1
	.ascii "uAnd_var_LC_i32"
	.space 1
	.ascii "uAnd_param_LC_i16"
	.space 1
	.ascii "uAnd_var_LC_i16"
	.space 1
	.ascii "uAnd_param_LC_L"
	.space 1
	.ascii "uAnd_var_LC_L"
	.space 1
	.ascii "uAnd_param_LC_u64"
	.space 1
	.ascii "uAnd_var_LC_u64"
	.space 1
	.ascii "uAnd_param_LC_u16"
	.space 1
	.ascii "uAnd_var_LC_u16"
	.space 1
	.ascii "uAnd_param_LC_C"
	.space 1
	.ascii "uAnd_var_LC_C"
	.space 1
	.ascii "uAnd_param_LC_u32"
	.space 1
	.ascii "uAnd_var_LC_u32"
	.space 1
	.ascii "uAnd_param_LC_i8"
	.space 1
	.ascii "uAnd_var_LC_i8"
	.space 1
	.ascii "uAnd_param_LC_I"
	.space 1
	.ascii "uAnd_var_LC_I"
	.space 1
	.ascii "uAnd_param_LC_u8"
	.space 1
	.ascii "uAnd_var_LC_u8"
	.space 1
	.ascii "uAnd_param_LC_LC"
	.space 1
	.ascii "uAnd_var_LC_LC"
	.space 1
	.ascii "uAnd_param_LC_i64"
	.space 1
	.ascii "uAnd_var_LC_i64"
	.space 1
	.ascii "uAnd_param_i64_i32"
	.space 1
	.ascii "uAnd_var_i64_i32"
	.space 1
	.ascii "uAnd_param_i64_i16"
	.space 1
	.ascii "uAnd_var_i64_i16"
	.space 1
	.ascii "uAnd_param_i64_L"
	.space 1
	.ascii "uAnd_var_i64_L"
	.space 1
	.ascii "uAnd_param_i64_u64"
	.space 1
	.ascii "uAnd_var_i64_u64"
	.space 1
	.ascii "uAnd_param_i64_u16"
	.space 1
	.ascii "uAnd_var_i64_u16"
	.space 1
	.ascii "uAnd_param_i64_C"
	.space 1
	.ascii "uAnd_var_i64_C"
	.space 1
	.ascii "uAnd_param_i64_u32"
	.space 1
	.ascii "uAnd_var_i64_u32"
	.space 1
	.ascii "uAnd_param_i64_i8"
	.space 1
	.ascii "uAnd_var_i64_i8"
	.space 1
	.ascii "uAnd_param_i64_I"
	.space 1
	.ascii "uAnd_var_i64_I"
	.space 1
	.ascii "uAnd_param_i64_u8"
	.space 1
	.ascii "uAnd_var_i64_u8"
	.space 1
	.ascii "uAnd_param_i64_LC"
	.space 1
	.ascii "uAnd_var_i64_LC"
	.space 1
	.ascii "uAnd_param_i64_i64"
	.space 1
	.ascii "uAnd_var_i64_i64"
	.space 2
	.quad	_And_M3
	.quad	_L_1
	.quad	_And__uAnd_param_i32_i32
	.quad	_L_1+7
	.quad	_And__uAnd_var_i32_i32
	.quad	_L_1+26
	.quad	_And__uAnd_param_i32_i16
	.quad	_L_1+43
	.quad	_And__uAnd_var_i32_i16
	.quad	_L_1+62
	.quad	_And__uAnd_param_i32_L
	.quad	_L_1+79
	.quad	_And__uAnd_var_i32_L
	.quad	_L_1+96
	.quad	_And__uAnd_param_i32_u64
	.quad	_L_1+111
	.quad	_And__uAnd_var_i32_u64
	.quad	_L_1+130
	.quad	_And__uAnd_param_i32_u16
	.quad	_L_1+147
	.quad	_And__uAnd_var_i32_u16
	.quad	_L_1+166
	.quad	_And__uAnd_param_i32_C
	.quad	_L_1+183
	.quad	_And__uAnd_var_i32_C
	.quad	_L_1+200
	.quad	_And__uAnd_param_i32_u32
	.quad	_L_1+215
	.quad	_And__uAnd_var_i32_u32
	.quad	_L_1+234
	.quad	_And__uAnd_param_i32_i8
	.quad	_L_1+251
	.quad	_And__uAnd_var_i32_i8
	.quad	_L_1+269
	.quad	_And__uAnd_param_i32_I
	.quad	_L_1+285
	.quad	_And__uAnd_var_i32_I
	.quad	_L_1+302
	.quad	_And__uAnd_param_i32_u8
	.quad	_L_1+317
	.quad	_And__uAnd_var_i32_u8
	.quad	_L_1+335
	.quad	_And__uAnd_param_i32_LC
	.quad	_L_1+351
	.quad	_And__uAnd_var_i32_LC
	.quad	_L_1+369
	.quad	_And__uAnd_param_i32_i64
	.quad	_L_1+385
	.quad	_And__uAnd_var_i32_i64
	.quad	_L_1+404
	.quad	_And__uAnd_param_i16_i32
	.quad	_L_1+421
	.quad	_And__uAnd_var_i16_i32
	.quad	_L_1+440
	.quad	_And__uAnd_param_i16_i16
	.quad	_L_1+457
	.quad	_And__uAnd_var_i16_i16
	.quad	_L_1+476
	.quad	_And__uAnd_param_i16_L
	.quad	_L_1+493
	.quad	_And__uAnd_var_i16_L
	.quad	_L_1+510
	.quad	_And__uAnd_param_i16_u64
	.quad	_L_1+525
	.quad	_And__uAnd_var_i16_u64
	.quad	_L_1+544
	.quad	_And__uAnd_param_i16_u16
	.quad	_L_1+561
	.quad	_And__uAnd_var_i16_u16
	.quad	_L_1+580
	.quad	_And__uAnd_param_i16_C
	.quad	_L_1+597
	.quad	_And__uAnd_var_i16_C
	.quad	_L_1+614
	.quad	_And__uAnd_param_i16_u32
	.quad	_L_1+629
	.quad	_And__uAnd_var_i16_u32
	.quad	_L_1+648
	.quad	_And__uAnd_param_i16_i8
	.quad	_L_1+665
	.quad	_And__uAnd_var_i16_i8
	.quad	_L_1+683
	.quad	_And__uAnd_param_i16_I
	.quad	_L_1+699
	.quad	_And__uAnd_var_i16_I
	.quad	_L_1+716
	.quad	_And__uAnd_param_i16_u8
	.quad	_L_1+731
	.quad	_And__uAnd_var_i16_u8
	.quad	_L_1+749
	.quad	_And__uAnd_param_i16_LC
	.quad	_L_1+765
	.quad	_And__uAnd_var_i16_LC
	.quad	_L_1+783
	.quad	_And__uAnd_param_i16_i64
	.quad	_L_1+799
	.quad	_And__uAnd_var_i16_i64
	.quad	_L_1+818
	.quad	_And__uAnd_param_L_i32
	.quad	_L_1+835
	.quad	_And__uAnd_var_L_i32
	.quad	_L_1+852
	.quad	_And__uAnd_param_L_i16
	.quad	_L_1+867
	.quad	_And__uAnd_var_L_i16
	.quad	_L_1+884
	.quad	_And__uAnd_param_L_L
	.quad	_L_1+899
	.quad	_And__uAnd_var_L_L
	.quad	_L_1+914
	.quad	_And__uAnd_param_L_u64
	.quad	_L_1+927
	.quad	_And__uAnd_var_L_u64
	.quad	_L_1+944
	.quad	_And__uAnd_param_L_u16
	.quad	_L_1+959
	.quad	_And__uAnd_var_L_u16
	.quad	_L_1+976
	.quad	_And__uAnd_param_L_C
	.quad	_L_1+991
	.quad	_And__uAnd_var_L_C
	.quad	_L_1+1006
	.quad	_And__uAnd_param_L_u32
	.quad	_L_1+1019
	.quad	_And__uAnd_var_L_u32
	.quad	_L_1+1036
	.quad	_And__uAnd_param_L_i8
	.quad	_L_1+1051
	.quad	_And__uAnd_var_L_i8
	.quad	_L_1+1067
	.quad	_And__uAnd_param_L_I
	.quad	_L_1+1081
	.quad	_And__uAnd_var_L_I
	.quad	_L_1+1096
	.quad	_And__uAnd_param_L_u8
	.quad	_L_1+1109
	.quad	_And__uAnd_var_L_u8
	.quad	_L_1+1125
	.quad	_And__uAnd_param_L_LC
	.quad	_L_1+1139
	.quad	_And__uAnd_var_L_LC
	.quad	_L_1+1155
	.quad	_And__uAnd_param_L_i64
	.quad	_L_1+1169
	.quad	_And__uAnd_var_L_i64
	.quad	_L_1+1186
	.quad	_And__uAnd_param_u64_i32
	.quad	_L_1+1201
	.quad	_And__uAnd_var_u64_i32
	.quad	_L_1+1220
	.quad	_And__uAnd_param_u64_i16
	.quad	_L_1+1237
	.quad	_And__uAnd_var_u64_i16
	.quad	_L_1+1256
	.quad	_And__uAnd_param_u64_L
	.quad	_L_1+1273
	.quad	_And__uAnd_var_u64_L
	.quad	_L_1+1290
	.quad	_And__uAnd_param_u64_u64
	.quad	_L_1+1305
	.quad	_And__uAnd_var_u64_u64
	.quad	_L_1+1324
	.quad	_And__uAnd_param_u64_u16
	.quad	_L_1+1341
	.quad	_And__uAnd_var_u64_u16
	.quad	_L_1+1360
	.quad	_And__uAnd_param_u64_C
	.quad	_L_1+1377
	.quad	_And__uAnd_var_u64_C
	.quad	_L_1+1394
	.quad	_And__uAnd_param_u64_u32
	.quad	_L_1+1409
	.quad	_And__uAnd_var_u64_u32
	.quad	_L_1+1428
	.quad	_And__uAnd_param_u64_i8
	.quad	_L_1+1445
	.quad	_And__uAnd_var_u64_i8
	.quad	_L_1+1463
	.quad	_And__uAnd_param_u64_I
	.quad	_L_1+1479
	.quad	_And__uAnd_var_u64_I
	.quad	_L_1+1496
	.quad	_And__uAnd_param_u64_u8
	.quad	_L_1+1511
	.quad	_And__uAnd_var_u64_u8
	.quad	_L_1+1529
	.quad	_And__uAnd_param_u64_LC
	.quad	_L_1+1545
	.quad	_And__uAnd_var_u64_LC
	.quad	_L_1+1563
	.quad	_And__uAnd_param_u64_i64
	.quad	_L_1+1579
	.quad	_And__uAnd_var_u64_i64
	.quad	_L_1+1598
	.quad	_And__uAnd_param_u16_i32
	.quad	_L_1+1615
	.quad	_And__uAnd_var_u16_i32
	.quad	_L_1+1634
	.quad	_And__uAnd_param_u16_i16
	.quad	_L_1+1651
	.quad	_And__uAnd_var_u16_i16
	.quad	_L_1+1670
	.quad	_And__uAnd_param_u16_L
	.quad	_L_1+1687
	.quad	_And__uAnd_var_u16_L
	.quad	_L_1+1704
	.quad	_And__uAnd_param_u16_u64
	.quad	_L_1+1719
	.quad	_And__uAnd_var_u16_u64
	.quad	_L_1+1738
	.quad	_And__uAnd_param_u16_u16
	.quad	_L_1+1755
	.quad	_And__uAnd_var_u16_u16
	.quad	_L_1+1774
	.quad	_And__uAnd_param_u16_C
	.quad	_L_1+1791
	.quad	_And__uAnd_var_u16_C
	.quad	_L_1+1808
	.quad	_And__uAnd_param_u16_u32
	.quad	_L_1+1823
	.quad	_And__uAnd_var_u16_u32
	.quad	_L_1+1842
	.quad	_And__uAnd_param_u16_i8
	.quad	_L_1+1859
	.quad	_And__uAnd_var_u16_i8
	.quad	_L_1+1877
	.quad	_And__uAnd_param_u16_I
	.quad	_L_1+1893
	.quad	_And__uAnd_var_u16_I
	.quad	_L_1+1910
	.quad	_And__uAnd_param_u16_u8
	.quad	_L_1+1925
	.quad	_And__uAnd_var_u16_u8
	.quad	_L_1+1943
	.quad	_And__uAnd_param_u16_LC
	.quad	_L_1+1959
	.quad	_And__uAnd_var_u16_LC
	.quad	_L_1+1977
	.quad	_And__uAnd_param_u16_i64
	.quad	_L_1+1993
	.quad	_And__uAnd_var_u16_i64
	.quad	_L_1+2012
	.quad	_And__uAnd_param_C_i32
	.quad	_L_1+2029
	.quad	_And__uAnd_var_C_i32
	.quad	_L_1+2046
	.quad	_And__uAnd_param_C_i16
	.quad	_L_1+2061
	.quad	_And__uAnd_var_C_i16
	.quad	_L_1+2078
	.quad	_And__uAnd_param_C_L
	.quad	_L_1+2093
	.quad	_And__uAnd_var_C_L
	.quad	_L_1+2108
	.quad	_And__uAnd_param_C_u64
	.quad	_L_1+2121
	.quad	_And__uAnd_var_C_u64
	.quad	_L_1+2138
	.quad	_And__uAnd_param_C_u16
	.quad	_L_1+2153
	.quad	_And__uAnd_var_C_u16
	.quad	_L_1+2170
	.quad	_And__uAnd_param_C_C
	.quad	_L_1+2185
	.quad	_And__uAnd_var_C_C
	.quad	_L_1+2200
	.quad	_And__uAnd_param_C_u32
	.quad	_L_1+2213
	.quad	_And__uAnd_var_C_u32
	.quad	_L_1+2230
	.quad	_And__uAnd_param_C_i8
	.quad	_L_1+2245
	.quad	_And__uAnd_var_C_i8
	.quad	_L_1+2261
	.quad	_And__uAnd_param_C_I
	.quad	_L_1+2275
	.quad	_And__uAnd_var_C_I
	.quad	_L_1+2290
	.quad	_And__uAnd_param_C_u8
	.quad	_L_1+2303
	.quad	_And__uAnd_var_C_u8
	.quad	_L_1+2319
	.quad	_And__uAnd_param_C_LC
	.quad	_L_1+2333
	.quad	_And__uAnd_var_C_LC
	.quad	_L_1+2349
	.quad	_And__uAnd_param_C_i64
	.quad	_L_1+2363
	.quad	_And__uAnd_var_C_i64
	.quad	_L_1+2380
	.quad	_And__uAnd_param_u32_i32
	.quad	_L_1+2395
	.quad	_And__uAnd_var_u32_i32
	.quad	_L_1+2414
	.quad	_And__uAnd_param_u32_i16
	.quad	_L_1+2431
	.quad	_And__uAnd_var_u32_i16
	.quad	_L_1+2450
	.quad	_And__uAnd_param_u32_L
	.quad	_L_1+2467
	.quad	_And__uAnd_var_u32_L
	.quad	_L_1+2484
	.quad	_And__uAnd_param_u32_u64
	.quad	_L_1+2499
	.quad	_And__uAnd_var_u32_u64
	.quad	_L_1+2518
	.quad	_And__uAnd_param_u32_u16
	.quad	_L_1+2535
	.quad	_And__uAnd_var_u32_u16
	.quad	_L_1+2554
	.quad	_And__uAnd_param_u32_C
	.quad	_L_1+2571
	.quad	_And__uAnd_var_u32_C
	.quad	_L_1+2588
	.quad	_And__uAnd_param_u32_u32
	.quad	_L_1+2603
	.quad	_And__uAnd_var_u32_u32
	.quad	_L_1+2622
	.quad	_And__uAnd_param_u32_i8
	.quad	_L_1+2639
	.quad	_And__uAnd_var_u32_i8
	.quad	_L_1+2657
	.quad	_And__uAnd_param_u32_I
	.quad	_L_1+2673
	.quad	_And__uAnd_var_u32_I
	.quad	_L_1+2690
	.quad	_And__uAnd_param_u32_u8
	.quad	_L_1+2705
	.quad	_And__uAnd_var_u32_u8
	.quad	_L_1+2723
	.quad	_And__uAnd_param_u32_LC
	.quad	_L_1+2739
	.quad	_And__uAnd_var_u32_LC
	.quad	_L_1+2757
	.quad	_And__uAnd_param_u32_i64
	.quad	_L_1+2773
	.quad	_And__uAnd_var_u32_i64
	.quad	_L_1+2792
	.quad	_And__uAnd_param_i8_i32
	.quad	_L_1+2809
	.quad	_And__uAnd_var_i8_i32
	.quad	_L_1+2827
	.quad	_And__uAnd_param_i8_i16
	.quad	_L_1+2843
	.quad	_And__uAnd_var_i8_i16
	.quad	_L_1+2861
	.quad	_And__uAnd_param_i8_L
	.quad	_L_1+2877
	.quad	_And__uAnd_var_i8_L
	.quad	_L_1+2893
	.quad	_And__uAnd_param_i8_u64
	.quad	_L_1+2907
	.quad	_And__uAnd_var_i8_u64
	.quad	_L_1+2925
	.quad	_And__uAnd_param_i8_u16
	.quad	_L_1+2941
	.quad	_And__uAnd_var_i8_u16
	.quad	_L_1+2959
	.quad	_And__uAnd_param_i8_C
	.quad	_L_1+2975
	.quad	_And__uAnd_var_i8_C
	.quad	_L_1+2991
	.quad	_And__uAnd_param_i8_u32
	.quad	_L_1+3005
	.quad	_And__uAnd_var_i8_u32
	.quad	_L_1+3023
	.quad	_And__uAnd_param_i8_i8
	.quad	_L_1+3039
	.quad	_And__uAnd_var_i8_i8
	.quad	_L_1+3056
	.quad	_And__uAnd_param_i8_I
	.quad	_L_1+3071
	.quad	_And__uAnd_var_i8_I
	.quad	_L_1+3087
	.quad	_And__uAnd_param_i8_u8
	.quad	_L_1+3101
	.quad	_And__uAnd_var_i8_u8
	.quad	_L_1+3118
	.quad	_And__uAnd_param_i8_LC
	.quad	_L_1+3133
	.quad	_And__uAnd_var_i8_LC
	.quad	_L_1+3150
	.quad	_And__uAnd_param_i8_i64
	.quad	_L_1+3165
	.quad	_And__uAnd_var_i8_i64
	.quad	_L_1+3183
	.quad	_And__uAnd_param_I_i32
	.quad	_L_1+3199
	.quad	_And__uAnd_var_I_i32
	.quad	_L_1+3216
	.quad	_And__uAnd_param_I_i16
	.quad	_L_1+3231
	.quad	_And__uAnd_var_I_i16
	.quad	_L_1+3248
	.quad	_And__uAnd_param_I_L
	.quad	_L_1+3263
	.quad	_And__uAnd_var_I_L
	.quad	_L_1+3278
	.quad	_And__uAnd_param_I_u64
	.quad	_L_1+3291
	.quad	_And__uAnd_var_I_u64
	.quad	_L_1+3308
	.quad	_And__uAnd_param_I_u16
	.quad	_L_1+3323
	.quad	_And__uAnd_var_I_u16
	.quad	_L_1+3340
	.quad	_And__uAnd_param_I_C
	.quad	_L_1+3355
	.quad	_And__uAnd_var_I_C
	.quad	_L_1+3370
	.quad	_And__uAnd_param_I_u32
	.quad	_L_1+3383
	.quad	_And__uAnd_var_I_u32
	.quad	_L_1+3400
	.quad	_And__uAnd_param_I_i8
	.quad	_L_1+3415
	.quad	_And__uAnd_var_I_i8
	.quad	_L_1+3431
	.quad	_And__uAnd_param_I_I
	.quad	_L_1+3445
	.quad	_And__uAnd_var_I_I
	.quad	_L_1+3460
	.quad	_And__uAnd_param_I_u8
	.quad	_L_1+3473
	.quad	_And__uAnd_var_I_u8
	.quad	_L_1+3489
	.quad	_And__uAnd_param_I_LC
	.quad	_L_1+3503
	.quad	_And__uAnd_var_I_LC
	.quad	_L_1+3519
	.quad	_And__uAnd_param_I_i64
	.quad	_L_1+3533
	.quad	_And__uAnd_var_I_i64
	.quad	_L_1+3550
	.quad	_And__uAnd_param_u8_i32
	.quad	_L_1+3565
	.quad	_And__uAnd_var_u8_i32
	.quad	_L_1+3583
	.quad	_And__uAnd_param_u8_i16
	.quad	_L_1+3599
	.quad	_And__uAnd_var_u8_i16
	.quad	_L_1+3617
	.quad	_And__uAnd_param_u8_L
	.quad	_L_1+3633
	.quad	_And__uAnd_var_u8_L
	.quad	_L_1+3649
	.quad	_And__uAnd_param_u8_u64
	.quad	_L_1+3663
	.quad	_And__uAnd_var_u8_u64
	.quad	_L_1+3681
	.quad	_And__uAnd_param_u8_u16
	.quad	_L_1+3697
	.quad	_And__uAnd_var_u8_u16
	.quad	_L_1+3715
	.quad	_And__uAnd_param_u8_C
	.quad	_L_1+3731
	.quad	_And__uAnd_var_u8_C
	.quad	_L_1+3747
	.quad	_And__uAnd_param_u8_u32
	.quad	_L_1+3761
	.quad	_And__uAnd_var_u8_u32
	.quad	_L_1+3779
	.quad	_And__uAnd_param_u8_i8
	.quad	_L_1+3795
	.quad	_And__uAnd_var_u8_i8
	.quad	_L_1+3812
	.quad	_And__uAnd_param_u8_I
	.quad	_L_1+3827
	.quad	_And__uAnd_var_u8_I
	.quad	_L_1+3843
	.quad	_And__uAnd_param_u8_u8
	.quad	_L_1+3857
	.quad	_And__uAnd_var_u8_u8
	.quad	_L_1+3874
	.quad	_And__uAnd_param_u8_LC
	.quad	_L_1+3889
	.quad	_And__uAnd_var_u8_LC
	.quad	_L_1+3906
	.quad	_And__uAnd_param_u8_i64
	.quad	_L_1+3921
	.quad	_And__uAnd_var_u8_i64
	.quad	_L_1+3939
	.quad	_And__uAnd_param_LC_i32
	.quad	_L_1+3955
	.quad	_And__uAnd_var_LC_i32
	.quad	_L_1+3973
	.quad	_And__uAnd_param_LC_i16
	.quad	_L_1+3989
	.quad	_And__uAnd_var_LC_i16
	.quad	_L_1+4007
	.quad	_And__uAnd_param_LC_L
	.quad	_L_1+4023
	.quad	_And__uAnd_var_LC_L
	.quad	_L_1+4039
	.quad	_And__uAnd_param_LC_u64
	.quad	_L_1+4053
	.quad	_And__uAnd_var_LC_u64
	.quad	_L_1+4071
	.quad	_And__uAnd_param_LC_u16
	.quad	_L_1+4087
	.quad	_And__uAnd_var_LC_u16
	.quad	_L_1+4105
	.quad	_And__uAnd_param_LC_C
	.quad	_L_1+4121
	.quad	_And__uAnd_var_LC_C
	.quad	_L_1+4137
	.quad	_And__uAnd_param_LC_u32
	.quad	_L_1+4151
	.quad	_And__uAnd_var_LC_u32
	.quad	_L_1+4169
	.quad	_And__uAnd_param_LC_i8
	.quad	_L_1+4185
	.quad	_And__uAnd_var_LC_i8
	.quad	_L_1+4202
	.quad	_And__uAnd_param_LC_I
	.quad	_L_1+4217
	.quad	_And__uAnd_var_LC_I
	.quad	_L_1+4233
	.quad	_And__uAnd_param_LC_u8
	.quad	_L_1+4247
	.quad	_And__uAnd_var_LC_u8
	.quad	_L_1+4264
	.quad	_And__uAnd_param_LC_LC
	.quad	_L_1+4279
	.quad	_And__uAnd_var_LC_LC
	.quad	_L_1+4296
	.quad	_And__uAnd_param_LC_i64
	.quad	_L_1+4311
	.quad	_And__uAnd_var_LC_i64
	.quad	_L_1+4329
	.quad	_And__uAnd_param_i64_i32
	.quad	_L_1+4345
	.quad	_And__uAnd_var_i64_i32
	.quad	_L_1+4364
	.quad	_And__uAnd_param_i64_i16
	.quad	_L_1+4381
	.quad	_And__uAnd_var_i64_i16
	.quad	_L_1+4400
	.quad	_And__uAnd_param_i64_L
	.quad	_L_1+4417
	.quad	_And__uAnd_var_i64_L
	.quad	_L_1+4434
	.quad	_And__uAnd_param_i64_u64
	.quad	_L_1+4449
	.quad	_And__uAnd_var_i64_u64
	.quad	_L_1+4468
	.quad	_And__uAnd_param_i64_u16
	.quad	_L_1+4485
	.quad	_And__uAnd_var_i64_u16
	.quad	_L_1+4504
	.quad	_And__uAnd_param_i64_C
	.quad	_L_1+4521
	.quad	_And__uAnd_var_i64_C
	.quad	_L_1+4538
	.quad	_And__uAnd_param_i64_u32
	.quad	_L_1+4553
	.quad	_And__uAnd_var_i64_u32
	.quad	_L_1+4572
	.quad	_And__uAnd_param_i64_i8
	.quad	_L_1+4589
	.quad	_And__uAnd_var_i64_i8
	.quad	_L_1+4607
	.quad	_And__uAnd_param_i64_I
	.quad	_L_1+4623
	.quad	_And__uAnd_var_i64_I
	.quad	_L_1+4640
	.quad	_And__uAnd_param_i64_u8
	.quad	_L_1+4655
	.quad	_And__uAnd_var_i64_u8
	.quad	_L_1+4673
	.quad	_And__uAnd_param_i64_LC
	.quad	_L_1+4689
	.quad	_And__uAnd_var_i64_LC
	.quad	_L_1+4707
	.quad	_And__uAnd_param_i64_i64
	.quad	_L_1+4723
	.quad	_And__uAnd_var_i64_i64
	.quad	_L_1+4742
	.space 8
	.ascii "../AMD64_DARWIN/And.m3"
	.space 2
	.data
	.align 5
_MM_And:
	.quad	_L_1+9392
	.space 32
	.quad	_L_1+4760
	.space 24
	.quad	_MM_And+208
	.space 8
	.quad	_And_M3
	.quad	3
	.quad	433
	.quad	434
	.long	1138341839
	.byte	-75
	.space 3
	.quad	438
	.byte	-73
	.space 3
	.long	440
	.quad	441
	.word	442
	.space 6
	.quad	443
	.long	3092376453
	.long	1081853726
	.quad	446
	.word	447
	.space 2
	.long	448
	.space 24
	.quad	_And_I3
	.quad	_MM_And+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_And+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_And+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_And+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
