	.text
.globl _Plus__uPlus_var_i64_i64
	.private_extern _Plus__uPlus_var_i64_i64
_Plus__uPlus_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i64
	.private_extern _Plus__Plus_var_i64_i64
_Plus__Plus_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_i64
	.private_extern _Plus__uPlus_param_i64_i64
_Plus__uPlus_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_i64
	.private_extern _Plus__Plus_param_i64_i64
_Plus__Plus_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_LC
	.private_extern _Plus__uPlus_var_i64_LC
_Plus__uPlus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_LC
	.private_extern _Plus__Plus_var_i64_LC
_Plus__Plus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_LC
	.private_extern _Plus__uPlus_param_i64_LC
_Plus__uPlus_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_LC
	.private_extern _Plus__Plus_param_i64_LC
_Plus__Plus_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_u8
	.private_extern _Plus__uPlus_var_i64_u8
_Plus__uPlus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u8
	.private_extern _Plus__Plus_var_i64_u8
_Plus__Plus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_u8
	.private_extern _Plus__uPlus_param_i64_u8
_Plus__uPlus_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_u8
	.private_extern _Plus__Plus_param_i64_u8
_Plus__Plus_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_I
	.private_extern _Plus__uPlus_var_i64_I
_Plus__uPlus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_I
	.private_extern _Plus__Plus_var_i64_I
_Plus__Plus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_I
	.private_extern _Plus__uPlus_param_i64_I
_Plus__uPlus_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_I
	.private_extern _Plus__Plus_param_i64_I
_Plus__Plus_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_i8
	.private_extern _Plus__uPlus_var_i64_i8
_Plus__uPlus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i8
	.private_extern _Plus__Plus_var_i64_i8
_Plus__Plus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_i8
	.private_extern _Plus__uPlus_param_i64_i8
_Plus__uPlus_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_i8
	.private_extern _Plus__Plus_param_i64_i8
_Plus__Plus_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_u32
	.private_extern _Plus__uPlus_var_i64_u32
_Plus__uPlus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u32
	.private_extern _Plus__Plus_var_i64_u32
_Plus__Plus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_u32
	.private_extern _Plus__uPlus_param_i64_u32
_Plus__uPlus_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_u32
	.private_extern _Plus__Plus_param_i64_u32
_Plus__Plus_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_C
	.private_extern _Plus__uPlus_var_i64_C
_Plus__uPlus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_C
	.private_extern _Plus__Plus_var_i64_C
_Plus__Plus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_C
	.private_extern _Plus__uPlus_param_i64_C
_Plus__uPlus_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_C
	.private_extern _Plus__Plus_param_i64_C
_Plus__Plus_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_u16
	.private_extern _Plus__uPlus_var_i64_u16
_Plus__uPlus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u16
	.private_extern _Plus__Plus_var_i64_u16
_Plus__Plus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_u16
	.private_extern _Plus__uPlus_param_i64_u16
_Plus__uPlus_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_u16
	.private_extern _Plus__Plus_param_i64_u16
_Plus__Plus_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_u64
	.private_extern _Plus__uPlus_var_i64_u64
_Plus__uPlus_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u64
	.private_extern _Plus__Plus_var_i64_u64
_Plus__Plus_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_u64
	.private_extern _Plus__uPlus_param_i64_u64
_Plus__uPlus_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_u64
	.private_extern _Plus__Plus_param_i64_u64
_Plus__Plus_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_L
	.private_extern _Plus__uPlus_var_i64_L
_Plus__uPlus_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_L
	.private_extern _Plus__Plus_var_i64_L
_Plus__Plus_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_L
	.private_extern _Plus__uPlus_param_i64_L
_Plus__uPlus_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_L
	.private_extern _Plus__Plus_param_i64_L
_Plus__Plus_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_i16
	.private_extern _Plus__uPlus_var_i64_i16
_Plus__uPlus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i16
	.private_extern _Plus__Plus_var_i64_i16
_Plus__Plus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_i16
	.private_extern _Plus__uPlus_param_i64_i16
_Plus__uPlus_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_i16
	.private_extern _Plus__Plus_param_i64_i16
_Plus__Plus_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i64_i32
	.private_extern _Plus__uPlus_var_i64_i32
_Plus__uPlus_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i32
	.private_extern _Plus__Plus_var_i64_i32
_Plus__Plus_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i64_i32
	.private_extern _Plus__uPlus_param_i64_i32
_Plus__uPlus_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i64_i32
	.private_extern _Plus__Plus_param_i64_i32
_Plus__Plus_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_i64
	.private_extern _Plus__uPlus_var_LC_i64
_Plus__uPlus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i64
	.private_extern _Plus__Plus_var_LC_i64
_Plus__Plus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_i64
	.private_extern _Plus__uPlus_param_LC_i64
_Plus__uPlus_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_i64
	.private_extern _Plus__Plus_param_LC_i64
_Plus__Plus_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_LC
	.private_extern _Plus__uPlus_var_LC_LC
_Plus__uPlus_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_LC
	.private_extern _Plus__Plus_var_LC_LC
_Plus__Plus_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_LC
	.private_extern _Plus__uPlus_param_LC_LC
_Plus__uPlus_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__Plus_param_LC_LC
	.private_extern _Plus__Plus_param_LC_LC
_Plus__Plus_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_u8
	.private_extern _Plus__uPlus_var_LC_u8
_Plus__uPlus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u8
	.private_extern _Plus__Plus_var_LC_u8
_Plus__Plus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_u8
	.private_extern _Plus__uPlus_param_LC_u8
_Plus__uPlus_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_u8
	.private_extern _Plus__Plus_param_LC_u8
_Plus__Plus_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_I
	.private_extern _Plus__uPlus_var_LC_I
_Plus__uPlus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_I
	.private_extern _Plus__Plus_var_LC_I
_Plus__Plus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_I
	.private_extern _Plus__uPlus_param_LC_I
_Plus__uPlus_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_I
	.private_extern _Plus__Plus_param_LC_I
_Plus__Plus_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_i8
	.private_extern _Plus__uPlus_var_LC_i8
_Plus__uPlus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i8
	.private_extern _Plus__Plus_var_LC_i8
_Plus__Plus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_i8
	.private_extern _Plus__uPlus_param_LC_i8
_Plus__uPlus_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_i8
	.private_extern _Plus__Plus_param_LC_i8
_Plus__Plus_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_u32
	.private_extern _Plus__uPlus_var_LC_u32
_Plus__uPlus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u32
	.private_extern _Plus__Plus_var_LC_u32
_Plus__Plus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_u32
	.private_extern _Plus__uPlus_param_LC_u32
_Plus__uPlus_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_u32
	.private_extern _Plus__Plus_param_LC_u32
_Plus__Plus_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_C
	.private_extern _Plus__uPlus_var_LC_C
_Plus__uPlus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_C
	.private_extern _Plus__Plus_var_LC_C
_Plus__Plus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_C
	.private_extern _Plus__uPlus_param_LC_C
_Plus__uPlus_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__Plus_param_LC_C
	.private_extern _Plus__Plus_param_LC_C
_Plus__Plus_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_u16
	.private_extern _Plus__uPlus_var_LC_u16
_Plus__uPlus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u16
	.private_extern _Plus__Plus_var_LC_u16
_Plus__Plus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_u16
	.private_extern _Plus__uPlus_param_LC_u16
_Plus__uPlus_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_u16
	.private_extern _Plus__Plus_param_LC_u16
_Plus__Plus_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_u64
	.private_extern _Plus__uPlus_var_LC_u64
_Plus__uPlus_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u64
	.private_extern _Plus__Plus_var_LC_u64
_Plus__Plus_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_u64
	.private_extern _Plus__uPlus_param_LC_u64
_Plus__uPlus_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_u64
	.private_extern _Plus__Plus_param_LC_u64
_Plus__Plus_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_L
	.private_extern _Plus__uPlus_var_LC_L
_Plus__uPlus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_L
	.private_extern _Plus__Plus_var_LC_L
_Plus__Plus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_L
	.private_extern _Plus__uPlus_param_LC_L
_Plus__uPlus_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_L
	.private_extern _Plus__Plus_param_LC_L
_Plus__Plus_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_i16
	.private_extern _Plus__uPlus_var_LC_i16
_Plus__uPlus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i16
	.private_extern _Plus__Plus_var_LC_i16
_Plus__Plus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_i16
	.private_extern _Plus__uPlus_param_LC_i16
_Plus__uPlus_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_i16
	.private_extern _Plus__Plus_param_LC_i16
_Plus__Plus_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_LC_i32
	.private_extern _Plus__uPlus_var_LC_i32
_Plus__uPlus_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i32
	.private_extern _Plus__Plus_var_LC_i32
_Plus__Plus_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_LC_i32
	.private_extern _Plus__uPlus_param_LC_i32
_Plus__uPlus_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_LC_i32
	.private_extern _Plus__Plus_param_LC_i32
_Plus__Plus_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_f32_f32
	.private_extern _Plus__Plus_var_f32_f32
_Plus__Plus_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	120+_MM_Plus(%rip), %xmm0
	addss	%xmm0, %xmm0
	leave
	ret
.globl _Plus__Plus_param_f32_f32
	.private_extern _Plus__Plus_param_f32_f32
_Plus__Plus_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm0
	addss	-24(%rbp), %xmm0
	leave
	ret
.globl _Plus__uPlus_var_u8_i64
	.private_extern _Plus__uPlus_var_u8_i64
_Plus__uPlus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_i64
	.private_extern _Plus__Plus_var_u8_i64
_Plus__Plus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_i64
	.private_extern _Plus__uPlus_param_u8_i64
_Plus__uPlus_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_i64
	.private_extern _Plus__Plus_param_u8_i64
_Plus__Plus_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_LC
	.private_extern _Plus__uPlus_var_u8_LC
_Plus__uPlus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_LC
	.private_extern _Plus__Plus_var_u8_LC
_Plus__Plus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_LC
	.private_extern _Plus__uPlus_param_u8_LC
_Plus__uPlus_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_LC
	.private_extern _Plus__Plus_param_u8_LC
_Plus__Plus_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_u8
	.private_extern _Plus__uPlus_var_u8_u8
_Plus__uPlus_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_u8
	.private_extern _Plus__Plus_var_u8_u8
_Plus__Plus_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_u8_u8
	.private_extern _Plus__uPlus_param_u8_u8
_Plus__uPlus_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_u8
	.private_extern _Plus__Plus_param_u8_u8
_Plus__Plus_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movsbq	-18(%rbp),%rdx
	addq	%rdx, %rax
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
.globl _Plus__uPlus_var_u8_I
	.private_extern _Plus__uPlus_var_u8_I
_Plus__uPlus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_I
	.private_extern _Plus__Plus_var_u8_I
_Plus__Plus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_u8_I
	.private_extern _Plus__uPlus_param_u8_I
_Plus__uPlus_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_I
	.private_extern _Plus__Plus_param_u8_I
_Plus__Plus_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
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
.globl _Plus__uPlus_var_u8_i8
	.private_extern _Plus__uPlus_var_u8_i8
_Plus__uPlus_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_i8
	.private_extern _Plus__Plus_var_u8_i8
_Plus__Plus_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L240
	movl	$5409, %edi
	call	__m3_fault
L240:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_i8
	.private_extern _Plus__uPlus_param_u8_i8
_Plus__uPlus_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_i8
	.private_extern _Plus__Plus_param_u8_i8
_Plus__Plus_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movzbl	-18(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L245
	movl	$5473, %edi
	call	__m3_fault
L245:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_u32
	.private_extern _Plus__uPlus_var_u8_u32
_Plus__uPlus_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_u32
	.private_extern _Plus__Plus_var_u8_u32
_Plus__Plus_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_u8_u32
	.private_extern _Plus__uPlus_param_u8_u32
_Plus__uPlus_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_u32
	.private_extern _Plus__Plus_param_u8_u32
_Plus__Plus_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_var_u8_C
	.private_extern _Plus__uPlus_var_u8_C
_Plus__uPlus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_C
	.private_extern _Plus__Plus_var_u8_C
_Plus__Plus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_u8_C
	.private_extern _Plus__uPlus_param_u8_C
_Plus__uPlus_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_C
	.private_extern _Plus__Plus_param_u8_C
_Plus__Plus_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	-32(%rbp), %rdx
	addq	%rdx, %rax
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
.globl _Plus__uPlus_var_u8_u16
	.private_extern _Plus__uPlus_var_u8_u16
_Plus__uPlus_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_u16
	.private_extern _Plus__Plus_var_u8_u16
_Plus__Plus_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_u8_u16
	.private_extern _Plus__uPlus_param_u8_u16
_Plus__uPlus_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_u16
	.private_extern _Plus__Plus_param_u8_u16
_Plus__Plus_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movswq	-20(%rbp),%rdx
	addq	%rdx, %rax
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
.globl _Plus__uPlus_var_u8_u64
	.private_extern _Plus__uPlus_var_u8_u64
_Plus__uPlus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_u64
	.private_extern _Plus__Plus_var_u8_u64
_Plus__Plus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_u64
	.private_extern _Plus__uPlus_param_u8_u64
_Plus__uPlus_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_u64
	.private_extern _Plus__Plus_param_u8_u64
_Plus__Plus_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_L
	.private_extern _Plus__uPlus_var_u8_L
_Plus__uPlus_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_L
	.private_extern _Plus__Plus_var_u8_L
_Plus__Plus_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_L
	.private_extern _Plus__uPlus_param_u8_L
_Plus__uPlus_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_L
	.private_extern _Plus__Plus_param_u8_L
_Plus__Plus_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_i16
	.private_extern _Plus__uPlus_var_u8_i16
_Plus__uPlus_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_i16
	.private_extern _Plus__Plus_var_u8_i16
_Plus__Plus_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L302
	movl	$6177, %edi
	call	__m3_fault
L302:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_i16
	.private_extern _Plus__uPlus_param_u8_i16
_Plus__uPlus_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_i16
	.private_extern _Plus__Plus_param_u8_i16
_Plus__Plus_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movzwl	-20(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L307
	movl	$6241, %edi
	call	__m3_fault
L307:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_i32
	.private_extern _Plus__uPlus_var_u8_i32
_Plus__uPlus_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_i32
	.private_extern _Plus__Plus_var_u8_i32
_Plus__Plus_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L312
	movl	$6305, %edi
	call	__m3_fault
L312:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u8_i32
	.private_extern _Plus__uPlus_param_u8_i32
_Plus__uPlus_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u8_i32
	.private_extern _Plus__Plus_param_u8_i32
_Plus__Plus_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	mov	-24(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L317
	movl	$6369, %edi
	call	__m3_fault
L317:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_i64
	.private_extern _Plus__uPlus_var_I_i64
_Plus__uPlus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i64
	.private_extern _Plus__Plus_var_I_i64
_Plus__Plus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_i64
	.private_extern _Plus__uPlus_param_I_i64
_Plus__uPlus_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_i64
	.private_extern _Plus__Plus_param_I_i64
_Plus__Plus_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_I_LC
	.private_extern _Plus__uPlus_var_I_LC
_Plus__uPlus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_LC
	.private_extern _Plus__Plus_var_I_LC
_Plus__Plus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_LC
	.private_extern _Plus__uPlus_param_I_LC
_Plus__uPlus_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_I_LC
	.private_extern _Plus__Plus_param_I_LC
_Plus__Plus_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_u8
	.private_extern _Plus__uPlus_var_I_u8
_Plus__uPlus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u8
	.private_extern _Plus__Plus_var_I_u8
_Plus__Plus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_u8
	.private_extern _Plus__uPlus_param_I_u8
_Plus__uPlus_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_u8
	.private_extern _Plus__Plus_param_I_u8
_Plus__Plus_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_I
	.private_extern _Plus__uPlus_var_I_I
_Plus__uPlus_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_I
	.private_extern _Plus__Plus_var_I_I
_Plus__Plus_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_I
	.private_extern _Plus__uPlus_param_I_I
_Plus__uPlus_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_I
	.private_extern _Plus__Plus_param_I_I
_Plus__Plus_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_I_i8
	.private_extern _Plus__uPlus_var_I_i8
_Plus__uPlus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i8
	.private_extern _Plus__Plus_var_I_i8
_Plus__Plus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_i8
	.private_extern _Plus__uPlus_param_I_i8
_Plus__uPlus_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_i8
	.private_extern _Plus__Plus_param_I_i8
_Plus__Plus_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_u32
	.private_extern _Plus__uPlus_var_I_u32
_Plus__uPlus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u32
	.private_extern _Plus__Plus_var_I_u32
_Plus__Plus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_u32
	.private_extern _Plus__uPlus_param_I_u32
_Plus__uPlus_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_u32
	.private_extern _Plus__Plus_param_I_u32
_Plus__Plus_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_C
	.private_extern _Plus__uPlus_var_I_C
_Plus__uPlus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_C
	.private_extern _Plus__Plus_var_I_C
_Plus__Plus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_C
	.private_extern _Plus__uPlus_param_I_C
_Plus__uPlus_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_I_C
	.private_extern _Plus__Plus_param_I_C
_Plus__Plus_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_u16
	.private_extern _Plus__uPlus_var_I_u16
_Plus__uPlus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u16
	.private_extern _Plus__Plus_var_I_u16
_Plus__Plus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_u16
	.private_extern _Plus__uPlus_param_I_u16
_Plus__uPlus_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_u16
	.private_extern _Plus__Plus_param_I_u16
_Plus__Plus_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_u64
	.private_extern _Plus__uPlus_var_I_u64
_Plus__uPlus_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u64
	.private_extern _Plus__Plus_var_I_u64
_Plus__Plus_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_u64
	.private_extern _Plus__uPlus_param_I_u64
_Plus__uPlus_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_u64
	.private_extern _Plus__Plus_param_I_u64
_Plus__Plus_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_I_L
	.private_extern _Plus__uPlus_var_I_L
_Plus__uPlus_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_L
	.private_extern _Plus__Plus_var_I_L
_Plus__Plus_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_L
	.private_extern _Plus__uPlus_param_I_L
_Plus__uPlus_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_L
	.private_extern _Plus__Plus_param_I_L
_Plus__Plus_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_I_i16
	.private_extern _Plus__uPlus_var_I_i16
_Plus__uPlus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i16
	.private_extern _Plus__Plus_var_I_i16
_Plus__Plus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_i16
	.private_extern _Plus__uPlus_param_I_i16
_Plus__uPlus_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_i16
	.private_extern _Plus__Plus_param_I_i16
_Plus__Plus_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_i32
	.private_extern _Plus__uPlus_var_I_i32
_Plus__uPlus_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i32
	.private_extern _Plus__Plus_var_I_i32
_Plus__Plus_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_I_i32
	.private_extern _Plus__uPlus_param_I_i32
_Plus__uPlus_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_I_i32
	.private_extern _Plus__Plus_param_I_i32
_Plus__Plus_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_i64
	.private_extern _Plus__uPlus_var_i8_i64
_Plus__uPlus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_i64
	.private_extern _Plus__Plus_var_i8_i64
_Plus__Plus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_i64
	.private_extern _Plus__uPlus_param_i8_i64
_Plus__uPlus_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_i64
	.private_extern _Plus__Plus_param_i8_i64
_Plus__Plus_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_LC
	.private_extern _Plus__uPlus_var_i8_LC
_Plus__uPlus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_LC
	.private_extern _Plus__Plus_var_i8_LC
_Plus__Plus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_LC
	.private_extern _Plus__uPlus_param_i8_LC
_Plus__uPlus_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_LC
	.private_extern _Plus__Plus_param_i8_LC
_Plus__Plus_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u8
	.private_extern _Plus__uPlus_var_i8_u8
_Plus__uPlus_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_u8
	.private_extern _Plus__Plus_var_i8_u8
_Plus__Plus_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_i8_u8
	.private_extern _Plus__uPlus_param_i8_u8
_Plus__uPlus_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_u8
	.private_extern _Plus__Plus_param_i8_u8
_Plus__Plus_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	-18(%rbp),%rdx
	addq	%rdx, %rax
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
.globl _Plus__uPlus_var_i8_I
	.private_extern _Plus__uPlus_var_i8_I
_Plus__uPlus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_I
	.private_extern _Plus__Plus_var_i8_I
_Plus__Plus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_i8_I
	.private_extern _Plus__uPlus_param_i8_I
_Plus__uPlus_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_I
	.private_extern _Plus__Plus_param_i8_I
_Plus__Plus_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
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
.globl _Plus__uPlus_var_i8_i8
	.private_extern _Plus__uPlus_var_i8_i8
_Plus__uPlus_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_i8
	.private_extern _Plus__Plus_var_i8_i8
_Plus__Plus_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L458
	movl	$8481, %edi
	call	__m3_fault
L458:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_i8
	.private_extern _Plus__uPlus_param_i8_i8
_Plus__uPlus_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_i8
	.private_extern _Plus__Plus_param_i8_i8
_Plus__Plus_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movzbl	-18(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L463
	movl	$8545, %edi
	call	__m3_fault
L463:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u32
	.private_extern _Plus__uPlus_var_i8_u32
_Plus__uPlus_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_u32
	.private_extern _Plus__Plus_var_i8_u32
_Plus__Plus_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_param_i8_u32
	.private_extern _Plus__uPlus_param_i8_u32
_Plus__uPlus_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_u32
	.private_extern _Plus__Plus_param_i8_u32
_Plus__Plus_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
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
.globl _Plus__uPlus_var_i8_C
	.private_extern _Plus__uPlus_var_i8_C
_Plus__uPlus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_C
	.private_extern _Plus__Plus_var_i8_C
_Plus__Plus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L480
	cmpq	$255, -16(%rbp)
	jle	L481
L480:
	movl	$8737, %edi
	call	__m3_fault
L481:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_C
	.private_extern _Plus__uPlus_param_i8_C
_Plus__uPlus_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_C
	.private_extern _Plus__Plus_param_i8_C
_Plus__Plus_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	-32(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L486
	cmpq	$255, -16(%rbp)
	jle	L487
L486:
	movl	$8801, %edi
	call	__m3_fault
L487:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u16
	.private_extern _Plus__uPlus_var_i8_u16
_Plus__uPlus_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_u16
	.private_extern _Plus__Plus_var_i8_u16
_Plus__Plus_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L492
	cmpq	$255, -16(%rbp)
	jle	L493
L492:
	movl	$8865, %edi
	call	__m3_fault
L493:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_u16
	.private_extern _Plus__uPlus_param_i8_u16
_Plus__uPlus_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_u16
	.private_extern _Plus__Plus_param_i8_u16
_Plus__Plus_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movswq	-20(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L498
	cmpq	$255, -16(%rbp)
	jle	L499
L498:
	movl	$8929, %edi
	call	__m3_fault
L499:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u64
	.private_extern _Plus__uPlus_var_i8_u64
_Plus__uPlus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_u64
	.private_extern _Plus__Plus_var_i8_u64
_Plus__Plus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_u64
	.private_extern _Plus__uPlus_param_i8_u64
_Plus__uPlus_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_u64
	.private_extern _Plus__Plus_param_i8_u64
_Plus__Plus_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_L
	.private_extern _Plus__uPlus_var_i8_L
_Plus__uPlus_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_L
	.private_extern _Plus__Plus_var_i8_L
_Plus__Plus_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_L
	.private_extern _Plus__uPlus_param_i8_L
_Plus__uPlus_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_L
	.private_extern _Plus__Plus_param_i8_L
_Plus__Plus_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_i16
	.private_extern _Plus__uPlus_var_i8_i16
_Plus__uPlus_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_i16
	.private_extern _Plus__Plus_var_i8_i16
_Plus__Plus_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L520
	movl	$9249, %edi
	call	__m3_fault
L520:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_i16
	.private_extern _Plus__uPlus_param_i8_i16
_Plus__uPlus_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_i16
	.private_extern _Plus__Plus_param_i8_i16
_Plus__Plus_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movzwl	-20(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L525
	movl	$9313, %edi
	call	__m3_fault
L525:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_i32
	.private_extern _Plus__uPlus_var_i8_i32
_Plus__uPlus_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_i32
	.private_extern _Plus__Plus_var_i8_i32
_Plus__Plus_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L530
	movl	$9377, %edi
	call	__m3_fault
L530:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i8_i32
	.private_extern _Plus__uPlus_param_i8_i32
_Plus__uPlus_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i8_i32
	.private_extern _Plus__Plus_param_i8_i32
_Plus__Plus_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	mov	-24(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L535
	movl	$9441, %edi
	call	__m3_fault
L535:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i64
	.private_extern _Plus__uPlus_var_u32_i64
_Plus__uPlus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_i64
	.private_extern _Plus__Plus_var_u32_i64
_Plus__Plus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_i64
	.private_extern _Plus__uPlus_param_u32_i64
_Plus__uPlus_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_i64
	.private_extern _Plus__Plus_param_u32_i64
_Plus__Plus_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_LC
	.private_extern _Plus__uPlus_var_u32_LC
_Plus__uPlus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_LC
	.private_extern _Plus__Plus_var_u32_LC
_Plus__Plus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_LC
	.private_extern _Plus__uPlus_param_u32_LC
_Plus__uPlus_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_LC
	.private_extern _Plus__Plus_param_u32_LC
_Plus__Plus_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_u8
	.private_extern _Plus__uPlus_var_u32_u8
_Plus__uPlus_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_u8
	.private_extern _Plus__Plus_var_u32_u8
_Plus__Plus_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L556
	cmpq	$2147483647, -16(%rbp)
	jle	L557
L556:
	movl	$9761, %edi
	call	__m3_fault
L557:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_u8
	.private_extern _Plus__uPlus_param_u32_u8
_Plus__uPlus_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_u8
	.private_extern _Plus__Plus_param_u32_u8
_Plus__Plus_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L562
	cmpq	$2147483647, -16(%rbp)
	jle	L563
L562:
	movl	$9825, %edi
	call	__m3_fault
L563:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_I
	.private_extern _Plus__uPlus_var_u32_I
_Plus__uPlus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_I
	.private_extern _Plus__Plus_var_u32_I
_Plus__Plus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L568
	cmpq	$2147483647, -16(%rbp)
	jle	L569
L568:
	movl	$9889, %edi
	call	__m3_fault
L569:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_I
	.private_extern _Plus__uPlus_param_u32_I
_Plus__uPlus_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_I
	.private_extern _Plus__Plus_param_u32_I
_Plus__Plus_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L574
	cmpq	$2147483647, -16(%rbp)
	jle	L575
L574:
	movl	$9953, %edi
	call	__m3_fault
L575:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i8
	.private_extern _Plus__uPlus_var_u32_i8
_Plus__uPlus_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_i8
	.private_extern _Plus__Plus_var_u32_i8
_Plus__Plus_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L580
	movl	$10017, %edi
	call	__m3_fault
L580:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_i8
	.private_extern _Plus__uPlus_param_u32_i8
_Plus__uPlus_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_i8
	.private_extern _Plus__Plus_param_u32_i8
_Plus__Plus_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L585
	movl	$10081, %edi
	call	__m3_fault
L585:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_u32
	.private_extern _Plus__uPlus_var_u32_u32
_Plus__uPlus_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_u32
	.private_extern _Plus__Plus_var_u32_u32
_Plus__Plus_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L590
	cmpq	$2147483647, -16(%rbp)
	jle	L591
L590:
	movl	$10145, %edi
	call	__m3_fault
L591:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_u32
	.private_extern _Plus__uPlus_param_u32_u32
_Plus__uPlus_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_u32
	.private_extern _Plus__Plus_param_u32_u32
_Plus__Plus_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L596
	cmpq	$2147483647, -16(%rbp)
	jle	L597
L596:
	movl	$10209, %edi
	call	__m3_fault
L597:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_C
	.private_extern _Plus__uPlus_var_u32_C
_Plus__uPlus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_C
	.private_extern _Plus__Plus_var_u32_C
_Plus__Plus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L602
	cmpq	$2147483647, -16(%rbp)
	jle	L603
L602:
	movl	$10273, %edi
	call	__m3_fault
L603:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_C
	.private_extern _Plus__uPlus_param_u32_C
_Plus__uPlus_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_C
	.private_extern _Plus__Plus_param_u32_C
_Plus__Plus_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L608
	cmpq	$2147483647, -16(%rbp)
	jle	L609
L608:
	movl	$10337, %edi
	call	__m3_fault
L609:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_u16
	.private_extern _Plus__uPlus_var_u32_u16
_Plus__uPlus_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_u16
	.private_extern _Plus__Plus_var_u32_u16
_Plus__Plus_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L614
	cmpq	$2147483647, -16(%rbp)
	jle	L615
L614:
	movl	$10401, %edi
	call	__m3_fault
L615:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_u16
	.private_extern _Plus__uPlus_param_u32_u16
_Plus__uPlus_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_u16
	.private_extern _Plus__Plus_param_u32_u16
_Plus__Plus_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L620
	cmpq	$2147483647, -16(%rbp)
	jle	L621
L620:
	movl	$10465, %edi
	call	__m3_fault
L621:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_u64
	.private_extern _Plus__uPlus_var_u32_u64
_Plus__uPlus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_u64
	.private_extern _Plus__Plus_var_u32_u64
_Plus__Plus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_u64
	.private_extern _Plus__uPlus_param_u32_u64
_Plus__uPlus_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_u64
	.private_extern _Plus__Plus_param_u32_u64
_Plus__Plus_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_L
	.private_extern _Plus__uPlus_var_u32_L
_Plus__uPlus_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_L
	.private_extern _Plus__Plus_var_u32_L
_Plus__Plus_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_L
	.private_extern _Plus__uPlus_param_u32_L
_Plus__uPlus_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_L
	.private_extern _Plus__Plus_param_u32_L
_Plus__Plus_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i16
	.private_extern _Plus__uPlus_var_u32_i16
_Plus__uPlus_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_i16
	.private_extern _Plus__Plus_var_u32_i16
_Plus__Plus_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L642
	movl	$10785, %edi
	call	__m3_fault
L642:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_i16
	.private_extern _Plus__uPlus_param_u32_i16
_Plus__uPlus_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_i16
	.private_extern _Plus__Plus_param_u32_i16
_Plus__Plus_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L647
	movl	$10849, %edi
	call	__m3_fault
L647:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i32
	.private_extern _Plus__uPlus_var_u32_i32
_Plus__uPlus_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_i32
	.private_extern _Plus__Plus_var_u32_i32
_Plus__Plus_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L652
	movl	$10913, %edi
	call	__m3_fault
L652:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u32_i32
	.private_extern _Plus__uPlus_param_u32_i32
_Plus__uPlus_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u32_i32
	.private_extern _Plus__Plus_param_u32_i32
_Plus__Plus_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L657
	movl	$10977, %edi
	call	__m3_fault
L657:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i64
	.private_extern _Plus__uPlus_var_C_i64
_Plus__uPlus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_i64
	.private_extern _Plus__Plus_var_C_i64
_Plus__Plus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_i64
	.private_extern _Plus__uPlus_param_C_i64
_Plus__uPlus_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_i64
	.private_extern _Plus__Plus_param_C_i64
_Plus__Plus_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_LC
	.private_extern _Plus__uPlus_var_C_LC
_Plus__uPlus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_LC
	.private_extern _Plus__Plus_var_C_LC
_Plus__Plus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_LC
	.private_extern _Plus__uPlus_param_C_LC
_Plus__uPlus_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__Plus_param_C_LC
	.private_extern _Plus__Plus_param_C_LC
_Plus__Plus_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_u8
	.private_extern _Plus__uPlus_var_C_u8
_Plus__uPlus_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_u8
	.private_extern _Plus__Plus_var_C_u8
_Plus__Plus_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L678
	movl	$11297, %edi
	call	__m3_fault
L678:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_u8
	.private_extern _Plus__uPlus_param_C_u8
_Plus__uPlus_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_u8
	.private_extern _Plus__Plus_param_C_u8
_Plus__Plus_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	-24(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L683
	movl	$11361, %edi
	call	__m3_fault
L683:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_I
	.private_extern _Plus__uPlus_var_C_I
_Plus__uPlus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_I
	.private_extern _Plus__Plus_var_C_I
_Plus__Plus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L688
	movl	$11425, %edi
	call	__m3_fault
L688:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_I
	.private_extern _Plus__uPlus_param_C_I
_Plus__uPlus_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_I
	.private_extern _Plus__Plus_param_C_I
_Plus__Plus_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L693
	movl	$11489, %edi
	call	__m3_fault
L693:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i8
	.private_extern _Plus__uPlus_var_C_i8
_Plus__uPlus_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_i8
	.private_extern _Plus__Plus_var_C_i8
_Plus__Plus_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L698
	movl	$11553, %edi
	call	__m3_fault
L698:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_i8
	.private_extern _Plus__uPlus_param_C_i8
_Plus__uPlus_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_i8
	.private_extern _Plus__Plus_param_C_i8
_Plus__Plus_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	-24(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L703
	movl	$11617, %edi
	call	__m3_fault
L703:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_u32
	.private_extern _Plus__uPlus_var_C_u32
_Plus__uPlus_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_u32
	.private_extern _Plus__Plus_var_C_u32
_Plus__Plus_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L708
	movl	$11681, %edi
	call	__m3_fault
L708:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_u32
	.private_extern _Plus__uPlus_param_C_u32
_Plus__uPlus_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_u32
	.private_extern _Plus__Plus_param_C_u32
_Plus__Plus_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L713
	movl	$11745, %edi
	call	__m3_fault
L713:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_C
	.private_extern _Plus__uPlus_var_C_C
_Plus__uPlus_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_C
	.private_extern _Plus__Plus_var_C_C
_Plus__Plus_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L718
	movl	$11809, %edi
	call	__m3_fault
L718:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_C
	.private_extern _Plus__uPlus_param_C_C
_Plus__uPlus_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__Plus_param_C_C
	.private_extern _Plus__Plus_param_C_C
_Plus__Plus_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L723
	movl	$11873, %edi
	call	__m3_fault
L723:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_u16
	.private_extern _Plus__uPlus_var_C_u16
_Plus__uPlus_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_u16
	.private_extern _Plus__Plus_var_C_u16
_Plus__Plus_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L728
	movl	$11937, %edi
	call	__m3_fault
L728:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_u16
	.private_extern _Plus__uPlus_param_C_u16
_Plus__uPlus_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_u16
	.private_extern _Plus__Plus_param_C_u16
_Plus__Plus_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	-24(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L733
	movl	$12001, %edi
	call	__m3_fault
L733:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_u64
	.private_extern _Plus__uPlus_var_C_u64
_Plus__uPlus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_u64
	.private_extern _Plus__Plus_var_C_u64
_Plus__Plus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_u64
	.private_extern _Plus__uPlus_param_C_u64
_Plus__uPlus_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_u64
	.private_extern _Plus__Plus_param_C_u64
_Plus__Plus_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_L
	.private_extern _Plus__uPlus_var_C_L
_Plus__uPlus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_L
	.private_extern _Plus__Plus_var_C_L
_Plus__Plus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_L
	.private_extern _Plus__uPlus_param_C_L
_Plus__uPlus_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_L
	.private_extern _Plus__Plus_param_C_L
_Plus__Plus_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i16
	.private_extern _Plus__uPlus_var_C_i16
_Plus__uPlus_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_i16
	.private_extern _Plus__Plus_var_C_i16
_Plus__Plus_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L754
	movl	$12321, %edi
	call	__m3_fault
L754:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_i16
	.private_extern _Plus__uPlus_param_C_i16
_Plus__uPlus_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_i16
	.private_extern _Plus__Plus_param_C_i16
_Plus__Plus_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	-24(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L759
	movl	$12385, %edi
	call	__m3_fault
L759:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i32
	.private_extern _Plus__uPlus_var_C_i32
_Plus__uPlus_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_i32
	.private_extern _Plus__Plus_var_C_i32
_Plus__Plus_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L764
	movl	$12449, %edi
	call	__m3_fault
L764:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_C_i32
	.private_extern _Plus__uPlus_param_C_i32
_Plus__uPlus_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_C_i32
	.private_extern _Plus__Plus_param_C_i32
_Plus__Plus_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	-24(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L769
	movl	$12513, %edi
	call	__m3_fault
L769:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i64
	.private_extern _Plus__uPlus_var_u16_i64
_Plus__uPlus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_i64
	.private_extern _Plus__Plus_var_u16_i64
_Plus__Plus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_i64
	.private_extern _Plus__uPlus_param_u16_i64
_Plus__uPlus_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_i64
	.private_extern _Plus__Plus_param_u16_i64
_Plus__Plus_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_LC
	.private_extern _Plus__uPlus_var_u16_LC
_Plus__uPlus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_LC
	.private_extern _Plus__Plus_var_u16_LC
_Plus__Plus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_LC
	.private_extern _Plus__uPlus_param_u16_LC
_Plus__uPlus_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_LC
	.private_extern _Plus__Plus_param_u16_LC
_Plus__Plus_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_u8
	.private_extern _Plus__uPlus_var_u16_u8
_Plus__uPlus_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_u8
	.private_extern _Plus__Plus_var_u16_u8
_Plus__Plus_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L790
	cmpq	$32767, -16(%rbp)
	jle	L791
L790:
	movl	$12833, %edi
	call	__m3_fault
L791:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_u8
	.private_extern _Plus__uPlus_param_u16_u8
_Plus__uPlus_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_u8
	.private_extern _Plus__Plus_param_u16_u8
_Plus__Plus_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movsbq	-19(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L796
	cmpq	$32767, -16(%rbp)
	jle	L797
L796:
	movl	$12897, %edi
	call	__m3_fault
L797:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_I
	.private_extern _Plus__uPlus_var_u16_I
_Plus__uPlus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_I
	.private_extern _Plus__Plus_var_u16_I
_Plus__Plus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L802
	cmpq	$32767, -16(%rbp)
	jle	L803
L802:
	movl	$12961, %edi
	call	__m3_fault
L803:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_I
	.private_extern _Plus__uPlus_param_u16_I
_Plus__uPlus_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_I
	.private_extern _Plus__Plus_param_u16_I
_Plus__Plus_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L808
	cmpq	$32767, -16(%rbp)
	jle	L809
L808:
	movl	$13025, %edi
	call	__m3_fault
L809:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i8
	.private_extern _Plus__uPlus_var_u16_i8
_Plus__uPlus_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_i8
	.private_extern _Plus__Plus_var_u16_i8
_Plus__Plus_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L814
	movl	$13089, %edi
	call	__m3_fault
L814:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_i8
	.private_extern _Plus__uPlus_param_u16_i8
_Plus__uPlus_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_i8
	.private_extern _Plus__Plus_param_u16_i8
_Plus__Plus_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movzbl	-19(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L819
	movl	$13153, %edi
	call	__m3_fault
L819:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_u32
	.private_extern _Plus__uPlus_var_u16_u32
_Plus__uPlus_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_u32
	.private_extern _Plus__Plus_var_u16_u32
_Plus__Plus_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L824
	cmpq	$32767, -16(%rbp)
	jle	L825
L824:
	movl	$13217, %edi
	call	__m3_fault
L825:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_u32
	.private_extern _Plus__uPlus_param_u16_u32
_Plus__uPlus_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_u32
	.private_extern _Plus__Plus_param_u16_u32
_Plus__Plus_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L830
	cmpq	$32767, -16(%rbp)
	jle	L831
L830:
	movl	$13281, %edi
	call	__m3_fault
L831:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_C
	.private_extern _Plus__uPlus_var_u16_C
_Plus__uPlus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_C
	.private_extern _Plus__Plus_var_u16_C
_Plus__Plus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L836
	cmpq	$32767, -16(%rbp)
	jle	L837
L836:
	movl	$13345, %edi
	call	__m3_fault
L837:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_C
	.private_extern _Plus__uPlus_param_u16_C
_Plus__uPlus_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_C
	.private_extern _Plus__Plus_param_u16_C
_Plus__Plus_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	-32(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L842
	cmpq	$32767, -16(%rbp)
	jle	L843
L842:
	movl	$13409, %edi
	call	__m3_fault
L843:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_u16
	.private_extern _Plus__uPlus_var_u16_u16
_Plus__uPlus_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_u16
	.private_extern _Plus__Plus_var_u16_u16
_Plus__Plus_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L848
	cmpq	$32767, -16(%rbp)
	jle	L849
L848:
	movl	$13473, %edi
	call	__m3_fault
L849:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_u16
	.private_extern _Plus__uPlus_param_u16_u16
_Plus__uPlus_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_u16
	.private_extern _Plus__Plus_param_u16_u16
_Plus__Plus_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movswq	-20(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L854
	cmpq	$32767, -16(%rbp)
	jle	L855
L854:
	movl	$13537, %edi
	call	__m3_fault
L855:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_u64
	.private_extern _Plus__uPlus_var_u16_u64
_Plus__uPlus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_u64
	.private_extern _Plus__Plus_var_u16_u64
_Plus__Plus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_u64
	.private_extern _Plus__uPlus_param_u16_u64
_Plus__uPlus_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_u64
	.private_extern _Plus__Plus_param_u16_u64
_Plus__Plus_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_L
	.private_extern _Plus__uPlus_var_u16_L
_Plus__uPlus_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_L
	.private_extern _Plus__Plus_var_u16_L
_Plus__Plus_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_L
	.private_extern _Plus__uPlus_param_u16_L
_Plus__uPlus_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_L
	.private_extern _Plus__Plus_param_u16_L
_Plus__Plus_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i16
	.private_extern _Plus__uPlus_var_u16_i16
_Plus__uPlus_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_i16
	.private_extern _Plus__Plus_var_u16_i16
_Plus__Plus_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L876
	movl	$13857, %edi
	call	__m3_fault
L876:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_i16
	.private_extern _Plus__uPlus_param_u16_i16
_Plus__uPlus_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_i16
	.private_extern _Plus__Plus_param_u16_i16
_Plus__Plus_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movzwl	-20(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L881
	movl	$13921, %edi
	call	__m3_fault
L881:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i32
	.private_extern _Plus__uPlus_var_u16_i32
_Plus__uPlus_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_i32
	.private_extern _Plus__Plus_var_u16_i32
_Plus__Plus_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L886
	movl	$13985, %edi
	call	__m3_fault
L886:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_u16_i32
	.private_extern _Plus__uPlus_param_u16_i32
_Plus__uPlus_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u16_i32
	.private_extern _Plus__Plus_param_u16_i32
_Plus__Plus_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	mov	-24(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L891
	movl	$14049, %edi
	call	__m3_fault
L891:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_i64
	.private_extern _Plus__uPlus_var_u64_i64
_Plus__uPlus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i64
	.private_extern _Plus__Plus_var_u64_i64
_Plus__Plus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_i64
	.private_extern _Plus__uPlus_param_u64_i64
_Plus__uPlus_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_i64
	.private_extern _Plus__Plus_param_u64_i64
_Plus__Plus_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_LC
	.private_extern _Plus__uPlus_var_u64_LC
_Plus__uPlus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_LC
	.private_extern _Plus__Plus_var_u64_LC
_Plus__Plus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_LC
	.private_extern _Plus__uPlus_param_u64_LC
_Plus__uPlus_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_LC
	.private_extern _Plus__Plus_param_u64_LC
_Plus__Plus_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_u8
	.private_extern _Plus__uPlus_var_u64_u8
_Plus__uPlus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u8
	.private_extern _Plus__Plus_var_u64_u8
_Plus__Plus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_u8
	.private_extern _Plus__uPlus_param_u64_u8
_Plus__uPlus_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_u8
	.private_extern _Plus__Plus_param_u64_u8
_Plus__Plus_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_I
	.private_extern _Plus__uPlus_var_u64_I
_Plus__uPlus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_I
	.private_extern _Plus__Plus_var_u64_I
_Plus__Plus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_I
	.private_extern _Plus__uPlus_param_u64_I
_Plus__uPlus_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_I
	.private_extern _Plus__Plus_param_u64_I
_Plus__Plus_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_i8
	.private_extern _Plus__uPlus_var_u64_i8
_Plus__uPlus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i8
	.private_extern _Plus__Plus_var_u64_i8
_Plus__Plus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_i8
	.private_extern _Plus__uPlus_param_u64_i8
_Plus__uPlus_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_i8
	.private_extern _Plus__Plus_param_u64_i8
_Plus__Plus_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_u32
	.private_extern _Plus__uPlus_var_u64_u32
_Plus__uPlus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u32
	.private_extern _Plus__Plus_var_u64_u32
_Plus__Plus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_u32
	.private_extern _Plus__uPlus_param_u64_u32
_Plus__uPlus_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_u32
	.private_extern _Plus__Plus_param_u64_u32
_Plus__Plus_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_C
	.private_extern _Plus__uPlus_var_u64_C
_Plus__uPlus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_C
	.private_extern _Plus__Plus_var_u64_C
_Plus__Plus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_C
	.private_extern _Plus__uPlus_param_u64_C
_Plus__uPlus_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_C
	.private_extern _Plus__Plus_param_u64_C
_Plus__Plus_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_u16
	.private_extern _Plus__uPlus_var_u64_u16
_Plus__uPlus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u16
	.private_extern _Plus__Plus_var_u64_u16
_Plus__Plus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_u16
	.private_extern _Plus__uPlus_param_u64_u16
_Plus__uPlus_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_u16
	.private_extern _Plus__Plus_param_u64_u16
_Plus__Plus_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_u64
	.private_extern _Plus__uPlus_var_u64_u64
_Plus__uPlus_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u64
	.private_extern _Plus__Plus_var_u64_u64
_Plus__Plus_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_u64
	.private_extern _Plus__uPlus_param_u64_u64
_Plus__uPlus_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_u64
	.private_extern _Plus__Plus_param_u64_u64
_Plus__Plus_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_L
	.private_extern _Plus__uPlus_var_u64_L
_Plus__uPlus_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_L
	.private_extern _Plus__Plus_var_u64_L
_Plus__Plus_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_L
	.private_extern _Plus__uPlus_param_u64_L
_Plus__uPlus_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_L
	.private_extern _Plus__Plus_param_u64_L
_Plus__Plus_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_i16
	.private_extern _Plus__uPlus_var_u64_i16
_Plus__uPlus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i16
	.private_extern _Plus__Plus_var_u64_i16
_Plus__Plus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_i16
	.private_extern _Plus__uPlus_param_u64_i16
_Plus__uPlus_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_i16
	.private_extern _Plus__Plus_param_u64_i16
_Plus__Plus_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_i32
	.private_extern _Plus__uPlus_var_u64_i32
_Plus__uPlus_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i32
	.private_extern _Plus__Plus_var_u64_i32
_Plus__Plus_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_u64_i32
	.private_extern _Plus__uPlus_param_u64_i32
_Plus__uPlus_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_u64_i32
	.private_extern _Plus__Plus_param_u64_i32
_Plus__Plus_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_var_f64_f64
	.private_extern _Plus__Plus_var_f64_f64
_Plus__Plus_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	168+_MM_Plus(%rip), %xmm0
	addsd	%xmm0, %xmm0
	leave
	ret
.globl _Plus__Plus_param_f64_f64
	.private_extern _Plus__Plus_param_f64_f64
_Plus__Plus_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm0
	addsd	-32(%rbp), %xmm0
	leave
	ret
.globl _Plus__uPlus_var_L_i64
	.private_extern _Plus__uPlus_var_L_i64
_Plus__uPlus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_i64
	.private_extern _Plus__Plus_var_L_i64
_Plus__Plus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_i64
	.private_extern _Plus__uPlus_param_L_i64
_Plus__uPlus_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_i64
	.private_extern _Plus__Plus_param_L_i64
_Plus__Plus_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_L_LC
	.private_extern _Plus__uPlus_var_L_LC
_Plus__uPlus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_LC
	.private_extern _Plus__Plus_var_L_LC
_Plus__Plus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_LC
	.private_extern _Plus__uPlus_param_L_LC
_Plus__uPlus_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_L_LC
	.private_extern _Plus__Plus_param_L_LC
_Plus__Plus_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_u8
	.private_extern _Plus__uPlus_var_L_u8
_Plus__uPlus_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_u8
	.private_extern _Plus__Plus_var_L_u8
_Plus__Plus_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_u8
	.private_extern _Plus__uPlus_param_L_u8
_Plus__uPlus_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_u8
	.private_extern _Plus__Plus_param_L_u8
_Plus__Plus_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_I
	.private_extern _Plus__uPlus_var_L_I
_Plus__uPlus_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_I
	.private_extern _Plus__Plus_var_L_I
_Plus__Plus_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_I
	.private_extern _Plus__uPlus_param_L_I
_Plus__uPlus_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_I
	.private_extern _Plus__Plus_param_L_I
_Plus__Plus_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_L_i8
	.private_extern _Plus__uPlus_var_L_i8
_Plus__uPlus_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_i8
	.private_extern _Plus__Plus_var_L_i8
_Plus__Plus_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_i8
	.private_extern _Plus__uPlus_param_L_i8
_Plus__uPlus_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_i8
	.private_extern _Plus__Plus_param_L_i8
_Plus__Plus_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_u32
	.private_extern _Plus__uPlus_var_L_u32
_Plus__uPlus_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_u32
	.private_extern _Plus__Plus_var_L_u32
_Plus__Plus_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_u32
	.private_extern _Plus__uPlus_param_L_u32
_Plus__uPlus_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_u32
	.private_extern _Plus__Plus_param_L_u32
_Plus__Plus_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_C
	.private_extern _Plus__uPlus_var_L_C
_Plus__uPlus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_C
	.private_extern _Plus__Plus_var_L_C
_Plus__Plus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_C
	.private_extern _Plus__uPlus_param_L_C
_Plus__uPlus_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_L_C
	.private_extern _Plus__Plus_param_L_C
_Plus__Plus_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_u16
	.private_extern _Plus__uPlus_var_L_u16
_Plus__uPlus_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_u16
	.private_extern _Plus__Plus_var_L_u16
_Plus__Plus_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_u16
	.private_extern _Plus__uPlus_param_L_u16
_Plus__uPlus_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_u16
	.private_extern _Plus__Plus_param_L_u16
_Plus__Plus_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_u64
	.private_extern _Plus__uPlus_var_L_u64
_Plus__uPlus_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_u64
	.private_extern _Plus__Plus_var_L_u64
_Plus__Plus_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_u64
	.private_extern _Plus__uPlus_param_L_u64
_Plus__uPlus_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_u64
	.private_extern _Plus__Plus_param_L_u64
_Plus__Plus_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_L_L
	.private_extern _Plus__uPlus_var_L_L
_Plus__uPlus_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_L
	.private_extern _Plus__Plus_var_L_L
_Plus__Plus_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_L
	.private_extern _Plus__uPlus_param_L_L
_Plus__uPlus_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_L
	.private_extern _Plus__Plus_param_L_L
_Plus__Plus_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _Plus__uPlus_var_L_i16
	.private_extern _Plus__uPlus_var_L_i16
_Plus__uPlus_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_i16
	.private_extern _Plus__Plus_var_L_i16
_Plus__Plus_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_i16
	.private_extern _Plus__uPlus_param_L_i16
_Plus__uPlus_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_i16
	.private_extern _Plus__Plus_param_L_i16
_Plus__Plus_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_i32
	.private_extern _Plus__uPlus_var_L_i32
_Plus__uPlus_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_i32
	.private_extern _Plus__Plus_var_L_i32
_Plus__Plus_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_L_i32
	.private_extern _Plus__uPlus_param_L_i32
_Plus__uPlus_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_L_i32
	.private_extern _Plus__Plus_param_L_i32
_Plus__Plus_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i64
	.private_extern _Plus__uPlus_var_i16_i64
_Plus__uPlus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_i64
	.private_extern _Plus__Plus_var_i16_i64
_Plus__Plus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_i64
	.private_extern _Plus__uPlus_param_i16_i64
_Plus__uPlus_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_i64
	.private_extern _Plus__Plus_param_i16_i64
_Plus__Plus_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_LC
	.private_extern _Plus__uPlus_var_i16_LC
_Plus__uPlus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_LC
	.private_extern _Plus__Plus_var_i16_LC
_Plus__Plus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_LC
	.private_extern _Plus__uPlus_param_i16_LC
_Plus__uPlus_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_LC
	.private_extern _Plus__Plus_param_i16_LC
_Plus__Plus_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_u8
	.private_extern _Plus__uPlus_var_i16_u8
_Plus__uPlus_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_u8
	.private_extern _Plus__Plus_var_i16_u8
_Plus__Plus_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1108
	cmpq	$65535, -16(%rbp)
	jle	L1109
L1108:
	movl	$17505, %edi
	call	__m3_fault
L1109:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_u8
	.private_extern _Plus__uPlus_param_i16_u8
_Plus__uPlus_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_u8
	.private_extern _Plus__Plus_param_i16_u8
_Plus__Plus_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movsbq	-19(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1114
	cmpq	$65535, -16(%rbp)
	jle	L1115
L1114:
	movl	$17569, %edi
	call	__m3_fault
L1115:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_I
	.private_extern _Plus__uPlus_var_i16_I
_Plus__uPlus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_I
	.private_extern _Plus__Plus_var_i16_I
_Plus__Plus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1120
	cmpq	$65535, -16(%rbp)
	jle	L1121
L1120:
	movl	$17633, %edi
	call	__m3_fault
L1121:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_I
	.private_extern _Plus__uPlus_param_i16_I
_Plus__uPlus_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_I
	.private_extern _Plus__Plus_param_i16_I
_Plus__Plus_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1126
	cmpq	$65535, -16(%rbp)
	jle	L1127
L1126:
	movl	$17697, %edi
	call	__m3_fault
L1127:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i8
	.private_extern _Plus__uPlus_var_i16_i8
_Plus__uPlus_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_i8
	.private_extern _Plus__Plus_var_i16_i8
_Plus__Plus_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1132
	movl	$17761, %edi
	call	__m3_fault
L1132:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_i8
	.private_extern _Plus__uPlus_param_i16_i8
_Plus__uPlus_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_i8
	.private_extern _Plus__Plus_param_i16_i8
_Plus__Plus_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movzbl	-19(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1137
	movl	$17825, %edi
	call	__m3_fault
L1137:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_u32
	.private_extern _Plus__uPlus_var_i16_u32
_Plus__uPlus_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_u32
	.private_extern _Plus__Plus_var_i16_u32
_Plus__Plus_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1142
	cmpq	$65535, -16(%rbp)
	jle	L1143
L1142:
	movl	$17889, %edi
	call	__m3_fault
L1143:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_u32
	.private_extern _Plus__uPlus_param_i16_u32
_Plus__uPlus_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_u32
	.private_extern _Plus__Plus_param_i16_u32
_Plus__Plus_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1148
	cmpq	$65535, -16(%rbp)
	jle	L1149
L1148:
	movl	$17953, %edi
	call	__m3_fault
L1149:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_C
	.private_extern _Plus__uPlus_var_i16_C
_Plus__uPlus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_C
	.private_extern _Plus__Plus_var_i16_C
_Plus__Plus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1154
	cmpq	$65535, -16(%rbp)
	jle	L1155
L1154:
	movl	$18017, %edi
	call	__m3_fault
L1155:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_C
	.private_extern _Plus__uPlus_param_i16_C
_Plus__uPlus_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_C
	.private_extern _Plus__Plus_param_i16_C
_Plus__Plus_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	-32(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1160
	cmpq	$65535, -16(%rbp)
	jle	L1161
L1160:
	movl	$18081, %edi
	call	__m3_fault
L1161:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_u16
	.private_extern _Plus__uPlus_var_i16_u16
_Plus__uPlus_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_u16
	.private_extern _Plus__Plus_var_i16_u16
_Plus__Plus_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1166
	cmpq	$65535, -16(%rbp)
	jle	L1167
L1166:
	movl	$18145, %edi
	call	__m3_fault
L1167:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_u16
	.private_extern _Plus__uPlus_param_i16_u16
_Plus__uPlus_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_u16
	.private_extern _Plus__Plus_param_i16_u16
_Plus__Plus_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	-20(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1172
	cmpq	$65535, -16(%rbp)
	jle	L1173
L1172:
	movl	$18209, %edi
	call	__m3_fault
L1173:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_u64
	.private_extern _Plus__uPlus_var_i16_u64
_Plus__uPlus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_u64
	.private_extern _Plus__Plus_var_i16_u64
_Plus__Plus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_u64
	.private_extern _Plus__uPlus_param_i16_u64
_Plus__uPlus_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_u64
	.private_extern _Plus__Plus_param_i16_u64
_Plus__Plus_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_L
	.private_extern _Plus__uPlus_var_i16_L
_Plus__uPlus_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_L
	.private_extern _Plus__Plus_var_i16_L
_Plus__Plus_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_L
	.private_extern _Plus__uPlus_param_i16_L
_Plus__uPlus_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_L
	.private_extern _Plus__Plus_param_i16_L
_Plus__Plus_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i16
	.private_extern _Plus__uPlus_var_i16_i16
_Plus__uPlus_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_i16
	.private_extern _Plus__Plus_var_i16_i16
_Plus__Plus_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1194
	movl	$18529, %edi
	call	__m3_fault
L1194:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_i16
	.private_extern _Plus__uPlus_param_i16_i16
_Plus__uPlus_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_i16
	.private_extern _Plus__Plus_param_i16_i16
_Plus__Plus_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movzwl	-20(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1199
	movl	$18593, %edi
	call	__m3_fault
L1199:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i32
	.private_extern _Plus__uPlus_var_i16_i32
_Plus__uPlus_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_i32
	.private_extern _Plus__Plus_var_i16_i32
_Plus__Plus_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1204
	movl	$18657, %edi
	call	__m3_fault
L1204:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i16_i32
	.private_extern _Plus__uPlus_param_i16_i32
_Plus__uPlus_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i16_i32
	.private_extern _Plus__Plus_param_i16_i32
_Plus__Plus_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	mov	-24(%rbp), %edx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1209
	movl	$18721, %edi
	call	__m3_fault
L1209:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i64
	.private_extern _Plus__uPlus_var_i32_i64
_Plus__uPlus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_i64
	.private_extern _Plus__Plus_var_i32_i64
_Plus__Plus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_i64
	.private_extern _Plus__uPlus_param_i32_i64
_Plus__uPlus_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_i64
	.private_extern _Plus__Plus_param_i32_i64
_Plus__Plus_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_LC
	.private_extern _Plus__uPlus_var_i32_LC
_Plus__uPlus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_LC
	.private_extern _Plus__Plus_var_i32_LC
_Plus__Plus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_LC
	.private_extern _Plus__uPlus_param_i32_LC
_Plus__uPlus_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_LC
	.private_extern _Plus__Plus_param_i32_LC
_Plus__Plus_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u8
	.private_extern _Plus__uPlus_var_i32_u8
_Plus__uPlus_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_u8
	.private_extern _Plus__Plus_var_i32_u8
_Plus__Plus_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1230
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1231
L1230:
	movl	$19041, %edi
	call	__m3_fault
L1231:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_u8
	.private_extern _Plus__uPlus_param_i32_u8
_Plus__uPlus_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_u8
	.private_extern _Plus__Plus_param_i32_u8
_Plus__Plus_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movsbq	-21(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1236
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1237
L1236:
	movl	$19105, %edi
	call	__m3_fault
L1237:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_I
	.private_extern _Plus__uPlus_var_i32_I
_Plus__uPlus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_I
	.private_extern _Plus__Plus_var_i32_I
_Plus__Plus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1242
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1243
L1242:
	movl	$19169, %edi
	call	__m3_fault
L1243:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_I
	.private_extern _Plus__uPlus_param_i32_I
_Plus__uPlus_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_I
	.private_extern _Plus__Plus_param_i32_I
_Plus__Plus_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1248
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1249
L1248:
	movl	$19233, %edi
	call	__m3_fault
L1249:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i8
	.private_extern _Plus__uPlus_var_i32_i8
_Plus__uPlus_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_i8
	.private_extern _Plus__Plus_var_i32_i8
_Plus__Plus_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1254
	movl	$19297, %edi
	call	__m3_fault
L1254:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_i8
	.private_extern _Plus__uPlus_param_i32_i8
_Plus__uPlus_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_i8
	.private_extern _Plus__Plus_param_i32_i8
_Plus__Plus_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1259
	movl	$19361, %edi
	call	__m3_fault
L1259:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u32
	.private_extern _Plus__uPlus_var_i32_u32
_Plus__uPlus_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_u32
	.private_extern _Plus__Plus_var_i32_u32
_Plus__Plus_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1264
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1265
L1264:
	movl	$19425, %edi
	call	__m3_fault
L1265:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_u32
	.private_extern _Plus__uPlus_param_i32_u32
_Plus__uPlus_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_u32
	.private_extern _Plus__Plus_param_i32_u32
_Plus__Plus_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1270
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1271
L1270:
	movl	$19489, %edi
	call	__m3_fault
L1271:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_C
	.private_extern _Plus__uPlus_var_i32_C
_Plus__uPlus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_C
	.private_extern _Plus__Plus_var_i32_C
_Plus__Plus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1276
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1277
L1276:
	movl	$19553, %edi
	call	__m3_fault
L1277:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_C
	.private_extern _Plus__uPlus_param_i32_C
_Plus__uPlus_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_C
	.private_extern _Plus__Plus_param_i32_C
_Plus__Plus_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	-32(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1282
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1283
L1282:
	movl	$19617, %edi
	call	__m3_fault
L1283:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u16
	.private_extern _Plus__uPlus_var_i32_u16
_Plus__uPlus_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_u16
	.private_extern _Plus__Plus_var_i32_u16
_Plus__Plus_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1288
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1289
L1288:
	movl	$19681, %edi
	call	__m3_fault
L1289:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_u16
	.private_extern _Plus__uPlus_param_i32_u16
_Plus__uPlus_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_u16
	.private_extern _Plus__Plus_param_i32_u16
_Plus__Plus_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movswq	-22(%rbp),%rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1294
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1295
L1294:
	movl	$19745, %edi
	call	__m3_fault
L1295:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u64
	.private_extern _Plus__uPlus_var_i32_u64
_Plus__uPlus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_u64
	.private_extern _Plus__Plus_var_i32_u64
_Plus__Plus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_u64
	.private_extern _Plus__uPlus_param_i32_u64
_Plus__uPlus_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_u64
	.private_extern _Plus__Plus_param_i32_u64
_Plus__Plus_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_L
	.private_extern _Plus__uPlus_var_i32_L
_Plus__uPlus_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_L
	.private_extern _Plus__Plus_var_i32_L
_Plus__Plus_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_L
	.private_extern _Plus__uPlus_param_i32_L
_Plus__uPlus_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_L
	.private_extern _Plus__Plus_param_i32_L
_Plus__Plus_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i16
	.private_extern _Plus__uPlus_var_i32_i16
_Plus__uPlus_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_i16
	.private_extern _Plus__Plus_var_i32_i16
_Plus__Plus_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1316
	movl	$20065, %edi
	call	__m3_fault
L1316:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_i16
	.private_extern _Plus__uPlus_param_i32_i16
_Plus__uPlus_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_i16
	.private_extern _Plus__Plus_param_i32_i16
_Plus__Plus_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1321
	movl	$20129, %edi
	call	__m3_fault
L1321:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i32
	.private_extern _Plus__uPlus_var_i32_i32
_Plus__uPlus_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_i32
	.private_extern _Plus__Plus_var_i32_i32
_Plus__Plus_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	188+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1326
	movl	$20193, %edi
	call	__m3_fault
L1326:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_param_i32_i32
	.private_extern _Plus__uPlus_param_i32_i32
_Plus__uPlus_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_param_i32_i32
	.private_extern _Plus__Plus_param_i32_i32
_Plus__Plus_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1331
	movl	$20257, %edi
	call	__m3_fault
L1331:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus_M3
_Plus_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Plus(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Plus(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Plus_M3"
	.space 1
	.ascii "Plus_param_i32_i32"
	.space 1
	.ascii "uPlus_param_i32_i32"
	.space 1
	.ascii "Plus_var_i32_i32"
	.space 1
	.ascii "uPlus_var_i32_i32"
	.space 1
	.ascii "Plus_param_i32_i16"
	.space 1
	.ascii "uPlus_param_i32_i16"
	.space 1
	.ascii "Plus_var_i32_i16"
	.space 1
	.ascii "uPlus_var_i32_i16"
	.space 1
	.ascii "Plus_param_i32_L"
	.space 1
	.ascii "uPlus_param_i32_L"
	.space 1
	.ascii "Plus_var_i32_L"
	.space 1
	.ascii "uPlus_var_i32_L"
	.space 1
	.ascii "Plus_param_i32_u64"
	.space 1
	.ascii "uPlus_param_i32_u64"
	.space 1
	.ascii "Plus_var_i32_u64"
	.space 1
	.ascii "uPlus_var_i32_u64"
	.space 1
	.ascii "Plus_param_i32_u16"
	.space 1
	.ascii "uPlus_param_i32_u16"
	.space 1
	.ascii "Plus_var_i32_u16"
	.space 1
	.ascii "uPlus_var_i32_u16"
	.space 1
	.ascii "Plus_param_i32_C"
	.space 1
	.ascii "uPlus_param_i32_C"
	.space 1
	.ascii "Plus_var_i32_C"
	.space 1
	.ascii "uPlus_var_i32_C"
	.space 1
	.ascii "Plus_param_i32_u32"
	.space 1
	.ascii "uPlus_param_i32_u32"
	.space 1
	.ascii "Plus_var_i32_u32"
	.space 1
	.ascii "uPlus_var_i32_u32"
	.space 1
	.ascii "Plus_param_i32_i8"
	.space 1
	.ascii "uPlus_param_i32_i8"
	.space 1
	.ascii "Plus_var_i32_i8"
	.space 1
	.ascii "uPlus_var_i32_i8"
	.space 1
	.ascii "Plus_param_i32_I"
	.space 1
	.ascii "uPlus_param_i32_I"
	.space 1
	.ascii "Plus_var_i32_I"
	.space 1
	.ascii "uPlus_var_i32_I"
	.space 1
	.ascii "Plus_param_i32_u8"
	.space 1
	.ascii "uPlus_param_i32_u8"
	.space 1
	.ascii "Plus_var_i32_u8"
	.space 1
	.ascii "uPlus_var_i32_u8"
	.space 1
	.ascii "Plus_param_i32_LC"
	.space 1
	.ascii "uPlus_param_i32_LC"
	.space 1
	.ascii "Plus_var_i32_LC"
	.space 1
	.ascii "uPlus_var_i32_LC"
	.space 1
	.ascii "Plus_param_i32_i64"
	.space 1
	.ascii "uPlus_param_i32_i64"
	.space 1
	.ascii "Plus_var_i32_i64"
	.space 1
	.ascii "uPlus_var_i32_i64"
	.space 1
	.ascii "Plus_param_i16_i32"
	.space 1
	.ascii "uPlus_param_i16_i32"
	.space 1
	.ascii "Plus_var_i16_i32"
	.space 1
	.ascii "uPlus_var_i16_i32"
	.space 1
	.ascii "Plus_param_i16_i16"
	.space 1
	.ascii "uPlus_param_i16_i16"
	.space 1
	.ascii "Plus_var_i16_i16"
	.space 1
	.ascii "uPlus_var_i16_i16"
	.space 1
	.ascii "Plus_param_i16_L"
	.space 1
	.ascii "uPlus_param_i16_L"
	.space 1
	.ascii "Plus_var_i16_L"
	.space 1
	.ascii "uPlus_var_i16_L"
	.space 1
	.ascii "Plus_param_i16_u64"
	.space 1
	.ascii "uPlus_param_i16_u64"
	.space 1
	.ascii "Plus_var_i16_u64"
	.space 1
	.ascii "uPlus_var_i16_u64"
	.space 1
	.ascii "Plus_param_i16_u16"
	.space 1
	.ascii "uPlus_param_i16_u16"
	.space 1
	.ascii "Plus_var_i16_u16"
	.space 1
	.ascii "uPlus_var_i16_u16"
	.space 1
	.ascii "Plus_param_i16_C"
	.space 1
	.ascii "uPlus_param_i16_C"
	.space 1
	.ascii "Plus_var_i16_C"
	.space 1
	.ascii "uPlus_var_i16_C"
	.space 1
	.ascii "Plus_param_i16_u32"
	.space 1
	.ascii "uPlus_param_i16_u32"
	.space 1
	.ascii "Plus_var_i16_u32"
	.space 1
	.ascii "uPlus_var_i16_u32"
	.space 1
	.ascii "Plus_param_i16_i8"
	.space 1
	.ascii "uPlus_param_i16_i8"
	.space 1
	.ascii "Plus_var_i16_i8"
	.space 1
	.ascii "uPlus_var_i16_i8"
	.space 1
	.ascii "Plus_param_i16_I"
	.space 1
	.ascii "uPlus_param_i16_I"
	.space 1
	.ascii "Plus_var_i16_I"
	.space 1
	.ascii "uPlus_var_i16_I"
	.space 1
	.ascii "Plus_param_i16_u8"
	.space 1
	.ascii "uPlus_param_i16_u8"
	.space 1
	.ascii "Plus_var_i16_u8"
	.space 1
	.ascii "uPlus_var_i16_u8"
	.space 1
	.ascii "Plus_param_i16_LC"
	.space 1
	.ascii "uPlus_param_i16_LC"
	.space 1
	.ascii "Plus_var_i16_LC"
	.space 1
	.ascii "uPlus_var_i16_LC"
	.space 1
	.ascii "Plus_param_i16_i64"
	.space 1
	.ascii "uPlus_param_i16_i64"
	.space 1
	.ascii "Plus_var_i16_i64"
	.space 1
	.ascii "uPlus_var_i16_i64"
	.space 1
	.ascii "Plus_param_L_i32"
	.space 1
	.ascii "uPlus_param_L_i32"
	.space 1
	.ascii "Plus_var_L_i32"
	.space 1
	.ascii "uPlus_var_L_i32"
	.space 1
	.ascii "Plus_param_L_i16"
	.space 1
	.ascii "uPlus_param_L_i16"
	.space 1
	.ascii "Plus_var_L_i16"
	.space 1
	.ascii "uPlus_var_L_i16"
	.space 1
	.ascii "Plus_param_L_L"
	.space 1
	.ascii "uPlus_param_L_L"
	.space 1
	.ascii "Plus_var_L_L"
	.space 1
	.ascii "uPlus_var_L_L"
	.space 1
	.ascii "Plus_param_L_u64"
	.space 1
	.ascii "uPlus_param_L_u64"
	.space 1
	.ascii "Plus_var_L_u64"
	.space 1
	.ascii "uPlus_var_L_u64"
	.space 1
	.ascii "Plus_param_L_u16"
	.space 1
	.ascii "uPlus_param_L_u16"
	.space 1
	.ascii "Plus_var_L_u16"
	.space 1
	.ascii "uPlus_var_L_u16"
	.space 1
	.ascii "Plus_param_L_C"
	.space 1
	.ascii "uPlus_param_L_C"
	.space 1
	.ascii "Plus_var_L_C"
	.space 1
	.ascii "uPlus_var_L_C"
	.space 1
	.ascii "Plus_param_L_u32"
	.space 1
	.ascii "uPlus_param_L_u32"
	.space 1
	.ascii "Plus_var_L_u32"
	.space 1
	.ascii "uPlus_var_L_u32"
	.space 1
	.ascii "Plus_param_L_i8"
	.space 1
	.ascii "uPlus_param_L_i8"
	.space 1
	.ascii "Plus_var_L_i8"
	.space 1
	.ascii "uPlus_var_L_i8"
	.space 1
	.ascii "Plus_param_L_I"
	.space 1
	.ascii "uPlus_param_L_I"
	.space 1
	.ascii "Plus_var_L_I"
	.space 1
	.ascii "uPlus_var_L_I"
	.space 1
	.ascii "Plus_param_L_u8"
	.space 1
	.ascii "uPlus_param_L_u8"
	.space 1
	.ascii "Plus_var_L_u8"
	.space 1
	.ascii "uPlus_var_L_u8"
	.space 1
	.ascii "Plus_param_L_LC"
	.space 1
	.ascii "uPlus_param_L_LC"
	.space 1
	.ascii "Plus_var_L_LC"
	.space 1
	.ascii "uPlus_var_L_LC"
	.space 1
	.ascii "Plus_param_L_i64"
	.space 1
	.ascii "uPlus_param_L_i64"
	.space 1
	.ascii "Plus_var_L_i64"
	.space 1
	.ascii "uPlus_var_L_i64"
	.space 1
	.ascii "Plus_param_f64_f64"
	.space 1
	.ascii "Plus_var_f64_f64"
	.space 1
	.ascii "Plus_param_u64_i32"
	.space 1
	.ascii "uPlus_param_u64_i32"
	.space 1
	.ascii "Plus_var_u64_i32"
	.space 1
	.ascii "uPlus_var_u64_i32"
	.space 1
	.ascii "Plus_param_u64_i16"
	.space 1
	.ascii "uPlus_param_u64_i16"
	.space 1
	.ascii "Plus_var_u64_i16"
	.space 1
	.ascii "uPlus_var_u64_i16"
	.space 1
	.ascii "Plus_param_u64_L"
	.space 1
	.ascii "uPlus_param_u64_L"
	.space 1
	.ascii "Plus_var_u64_L"
	.space 1
	.ascii "uPlus_var_u64_L"
	.space 1
	.ascii "Plus_param_u64_u64"
	.space 1
	.ascii "uPlus_param_u64_u64"
	.space 1
	.ascii "Plus_var_u64_u64"
	.space 1
	.ascii "uPlus_var_u64_u64"
	.space 1
	.ascii "Plus_param_u64_u16"
	.space 1
	.ascii "uPlus_param_u64_u16"
	.space 1
	.ascii "Plus_var_u64_u16"
	.space 1
	.ascii "uPlus_var_u64_u16"
	.space 1
	.ascii "Plus_param_u64_C"
	.space 1
	.ascii "uPlus_param_u64_C"
	.space 1
	.ascii "Plus_var_u64_C"
	.space 1
	.ascii "uPlus_var_u64_C"
	.space 1
	.ascii "Plus_param_u64_u32"
	.space 1
	.ascii "uPlus_param_u64_u32"
	.space 1
	.ascii "Plus_var_u64_u32"
	.space 1
	.ascii "uPlus_var_u64_u32"
	.space 1
	.ascii "Plus_param_u64_i8"
	.space 1
	.ascii "uPlus_param_u64_i8"
	.space 1
	.ascii "Plus_var_u64_i8"
	.space 1
	.ascii "uPlus_var_u64_i8"
	.space 1
	.ascii "Plus_param_u64_I"
	.space 1
	.ascii "uPlus_param_u64_I"
	.space 1
	.ascii "Plus_var_u64_I"
	.space 1
	.ascii "uPlus_var_u64_I"
	.space 1
	.ascii "Plus_param_u64_u8"
	.space 1
	.ascii "uPlus_param_u64_u8"
	.space 1
	.ascii "Plus_var_u64_u8"
	.space 1
	.ascii "uPlus_var_u64_u8"
	.space 1
	.ascii "Plus_param_u64_LC"
	.space 1
	.ascii "uPlus_param_u64_LC"
	.space 1
	.ascii "Plus_var_u64_LC"
	.space 1
	.ascii "uPlus_var_u64_LC"
	.space 1
	.ascii "Plus_param_u64_i64"
	.space 1
	.ascii "uPlus_param_u64_i64"
	.space 1
	.ascii "Plus_var_u64_i64"
	.space 1
	.ascii "uPlus_var_u64_i64"
	.space 1
	.ascii "Plus_param_u16_i32"
	.space 1
	.ascii "uPlus_param_u16_i32"
	.space 1
	.ascii "Plus_var_u16_i32"
	.space 1
	.ascii "uPlus_var_u16_i32"
	.space 1
	.ascii "Plus_param_u16_i16"
	.space 1
	.ascii "uPlus_param_u16_i16"
	.space 1
	.ascii "Plus_var_u16_i16"
	.space 1
	.ascii "uPlus_var_u16_i16"
	.space 1
	.ascii "Plus_param_u16_L"
	.space 1
	.ascii "uPlus_param_u16_L"
	.space 1
	.ascii "Plus_var_u16_L"
	.space 1
	.ascii "uPlus_var_u16_L"
	.space 1
	.ascii "Plus_param_u16_u64"
	.space 1
	.ascii "uPlus_param_u16_u64"
	.space 1
	.ascii "Plus_var_u16_u64"
	.space 1
	.ascii "uPlus_var_u16_u64"
	.space 1
	.ascii "Plus_param_u16_u16"
	.space 1
	.ascii "uPlus_param_u16_u16"
	.space 1
	.ascii "Plus_var_u16_u16"
	.space 1
	.ascii "uPlus_var_u16_u16"
	.space 1
	.ascii "Plus_param_u16_C"
	.space 1
	.ascii "uPlus_param_u16_C"
	.space 1
	.ascii "Plus_var_u16_C"
	.space 1
	.ascii "uPlus_var_u16_C"
	.space 1
	.ascii "Plus_param_u16_u32"
	.space 1
	.ascii "uPlus_param_u16_u32"
	.space 1
	.ascii "Plus_var_u16_u32"
	.space 1
	.ascii "uPlus_var_u16_u32"
	.space 1
	.ascii "Plus_param_u16_i8"
	.space 1
	.ascii "uPlus_param_u16_i8"
	.space 1
	.ascii "Plus_var_u16_i8"
	.space 1
	.ascii "uPlus_var_u16_i8"
	.space 1
	.ascii "Plus_param_u16_I"
	.space 1
	.ascii "uPlus_param_u16_I"
	.space 1
	.ascii "Plus_var_u16_I"
	.space 1
	.ascii "uPlus_var_u16_I"
	.space 1
	.ascii "Plus_param_u16_u8"
	.space 1
	.ascii "uPlus_param_u16_u8"
	.space 1
	.ascii "Plus_var_u16_u8"
	.space 1
	.ascii "uPlus_var_u16_u8"
	.space 1
	.ascii "Plus_param_u16_LC"
	.space 1
	.ascii "uPlus_param_u16_LC"
	.space 1
	.ascii "Plus_var_u16_LC"
	.space 1
	.ascii "uPlus_var_u16_LC"
	.space 1
	.ascii "Plus_param_u16_i64"
	.space 1
	.ascii "uPlus_param_u16_i64"
	.space 1
	.ascii "Plus_var_u16_i64"
	.space 1
	.ascii "uPlus_var_u16_i64"
	.space 1
	.ascii "Plus_param_C_i32"
	.space 1
	.ascii "uPlus_param_C_i32"
	.space 1
	.ascii "Plus_var_C_i32"
	.space 1
	.ascii "uPlus_var_C_i32"
	.space 1
	.ascii "Plus_param_C_i16"
	.space 1
	.ascii "uPlus_param_C_i16"
	.space 1
	.ascii "Plus_var_C_i16"
	.space 1
	.ascii "uPlus_var_C_i16"
	.space 1
	.ascii "Plus_param_C_L"
	.space 1
	.ascii "uPlus_param_C_L"
	.space 1
	.ascii "Plus_var_C_L"
	.space 1
	.ascii "uPlus_var_C_L"
	.space 1
	.ascii "Plus_param_C_u64"
	.space 1
	.ascii "uPlus_param_C_u64"
	.space 1
	.ascii "Plus_var_C_u64"
	.space 1
	.ascii "uPlus_var_C_u64"
	.space 1
	.ascii "Plus_param_C_u16"
	.space 1
	.ascii "uPlus_param_C_u16"
	.space 1
	.ascii "Plus_var_C_u16"
	.space 1
	.ascii "uPlus_var_C_u16"
	.space 1
	.ascii "Plus_param_C_C"
	.space 1
	.ascii "uPlus_param_C_C"
	.space 1
	.ascii "Plus_var_C_C"
	.space 1
	.ascii "uPlus_var_C_C"
	.space 1
	.ascii "Plus_param_C_u32"
	.space 1
	.ascii "uPlus_param_C_u32"
	.space 1
	.ascii "Plus_var_C_u32"
	.space 1
	.ascii "uPlus_var_C_u32"
	.space 1
	.ascii "Plus_param_C_i8"
	.space 1
	.ascii "uPlus_param_C_i8"
	.space 1
	.ascii "Plus_var_C_i8"
	.space 1
	.ascii "uPlus_var_C_i8"
	.space 1
	.ascii "Plus_param_C_I"
	.space 1
	.ascii "uPlus_param_C_I"
	.space 1
	.ascii "Plus_var_C_I"
	.space 1
	.ascii "uPlus_var_C_I"
	.space 1
	.ascii "Plus_param_C_u8"
	.space 1
	.ascii "uPlus_param_C_u8"
	.space 1
	.ascii "Plus_var_C_u8"
	.space 1
	.ascii "uPlus_var_C_u8"
	.space 1
	.ascii "Plus_param_C_LC"
	.space 1
	.ascii "uPlus_param_C_LC"
	.space 1
	.ascii "Plus_var_C_LC"
	.space 1
	.ascii "uPlus_var_C_LC"
	.space 1
	.ascii "Plus_param_C_i64"
	.space 1
	.ascii "uPlus_param_C_i64"
	.space 1
	.ascii "Plus_var_C_i64"
	.space 1
	.ascii "uPlus_var_C_i64"
	.space 1
	.ascii "Plus_param_u32_i32"
	.space 1
	.ascii "uPlus_param_u32_i32"
	.space 1
	.ascii "Plus_var_u32_i32"
	.space 1
	.ascii "uPlus_var_u32_i32"
	.space 1
	.ascii "Plus_param_u32_i16"
	.space 1
	.ascii "uPlus_param_u32_i16"
	.space 1
	.ascii "Plus_var_u32_i16"
	.space 1
	.ascii "uPlus_var_u32_i16"
	.space 1
	.ascii "Plus_param_u32_L"
	.space 1
	.ascii "uPlus_param_u32_L"
	.space 1
	.ascii "Plus_var_u32_L"
	.space 1
	.ascii "uPlus_var_u32_L"
	.space 1
	.ascii "Plus_param_u32_u64"
	.space 1
	.ascii "uPlus_param_u32_u64"
	.space 1
	.ascii "Plus_var_u32_u64"
	.space 1
	.ascii "uPlus_var_u32_u64"
	.space 1
	.ascii "Plus_param_u32_u16"
	.space 1
	.ascii "uPlus_param_u32_u16"
	.space 1
	.ascii "Plus_var_u32_u16"
	.space 1
	.ascii "uPlus_var_u32_u16"
	.space 1
	.ascii "Plus_param_u32_C"
	.space 1
	.ascii "uPlus_param_u32_C"
	.space 1
	.ascii "Plus_var_u32_C"
	.space 1
	.ascii "uPlus_var_u32_C"
	.space 1
	.ascii "Plus_param_u32_u32"
	.space 1
	.ascii "uPlus_param_u32_u32"
	.space 1
	.ascii "Plus_var_u32_u32"
	.space 1
	.ascii "uPlus_var_u32_u32"
	.space 1
	.ascii "Plus_param_u32_i8"
	.space 1
	.ascii "uPlus_param_u32_i8"
	.space 1
	.ascii "Plus_var_u32_i8"
	.space 1
	.ascii "uPlus_var_u32_i8"
	.space 1
	.ascii "Plus_param_u32_I"
	.space 1
	.ascii "uPlus_param_u32_I"
	.space 1
	.ascii "Plus_var_u32_I"
	.space 1
	.ascii "uPlus_var_u32_I"
	.space 1
	.ascii "Plus_param_u32_u8"
	.space 1
	.ascii "uPlus_param_u32_u8"
	.space 1
	.ascii "Plus_var_u32_u8"
	.space 1
	.ascii "uPlus_var_u32_u8"
	.space 1
	.ascii "Plus_param_u32_LC"
	.space 1
	.ascii "uPlus_param_u32_LC"
	.space 1
	.ascii "Plus_var_u32_LC"
	.space 1
	.ascii "uPlus_var_u32_LC"
	.space 1
	.ascii "Plus_param_u32_i64"
	.space 1
	.ascii "uPlus_param_u32_i64"
	.space 1
	.ascii "Plus_var_u32_i64"
	.space 1
	.ascii "uPlus_var_u32_i64"
	.space 1
	.ascii "Plus_param_i8_i32"
	.space 1
	.ascii "uPlus_param_i8_i32"
	.space 1
	.ascii "Plus_var_i8_i32"
	.space 1
	.ascii "uPlus_var_i8_i32"
	.space 1
	.ascii "Plus_param_i8_i16"
	.space 1
	.ascii "uPlus_param_i8_i16"
	.space 1
	.ascii "Plus_var_i8_i16"
	.space 1
	.ascii "uPlus_var_i8_i16"
	.space 1
	.ascii "Plus_param_i8_L"
	.space 1
	.ascii "uPlus_param_i8_L"
	.space 1
	.ascii "Plus_var_i8_L"
	.space 1
	.ascii "uPlus_var_i8_L"
	.space 1
	.ascii "Plus_param_i8_u64"
	.space 1
	.ascii "uPlus_param_i8_u64"
	.space 1
	.ascii "Plus_var_i8_u64"
	.space 1
	.ascii "uPlus_var_i8_u64"
	.space 1
	.ascii "Plus_param_i8_u16"
	.space 1
	.ascii "uPlus_param_i8_u16"
	.space 1
	.ascii "Plus_var_i8_u16"
	.space 1
	.ascii "uPlus_var_i8_u16"
	.space 1
	.ascii "Plus_param_i8_C"
	.space 1
	.ascii "uPlus_param_i8_C"
	.space 1
	.ascii "Plus_var_i8_C"
	.space 1
	.ascii "uPlus_var_i8_C"
	.space 1
	.ascii "Plus_param_i8_u32"
	.space 1
	.ascii "uPlus_param_i8_u32"
	.space 1
	.ascii "Plus_var_i8_u32"
	.space 1
	.ascii "uPlus_var_i8_u32"
	.space 1
	.ascii "Plus_param_i8_i8"
	.space 1
	.ascii "uPlus_param_i8_i8"
	.space 1
	.ascii "Plus_var_i8_i8"
	.space 1
	.ascii "uPlus_var_i8_i8"
	.space 1
	.ascii "Plus_param_i8_I"
	.space 1
	.ascii "uPlus_param_i8_I"
	.space 1
	.ascii "Plus_var_i8_I"
	.space 1
	.ascii "uPlus_var_i8_I"
	.space 1
	.ascii "Plus_param_i8_u8"
	.space 1
	.ascii "uPlus_param_i8_u8"
	.space 1
	.ascii "Plus_var_i8_u8"
	.space 1
	.ascii "uPlus_var_i8_u8"
	.space 1
	.ascii "Plus_param_i8_LC"
	.space 1
	.ascii "uPlus_param_i8_LC"
	.space 1
	.ascii "Plus_var_i8_LC"
	.space 1
	.ascii "uPlus_var_i8_LC"
	.space 1
	.ascii "Plus_param_i8_i64"
	.space 1
	.ascii "uPlus_param_i8_i64"
	.space 1
	.ascii "Plus_var_i8_i64"
	.space 1
	.ascii "uPlus_var_i8_i64"
	.space 1
	.ascii "Plus_param_I_i32"
	.space 1
	.ascii "uPlus_param_I_i32"
	.space 1
	.ascii "Plus_var_I_i32"
	.space 1
	.ascii "uPlus_var_I_i32"
	.space 1
	.ascii "Plus_param_I_i16"
	.space 1
	.ascii "uPlus_param_I_i16"
	.space 1
	.ascii "Plus_var_I_i16"
	.space 1
	.ascii "uPlus_var_I_i16"
	.space 1
	.ascii "Plus_param_I_L"
	.space 1
	.ascii "uPlus_param_I_L"
	.space 1
	.ascii "Plus_var_I_L"
	.space 1
	.ascii "uPlus_var_I_L"
	.space 1
	.ascii "Plus_param_I_u64"
	.space 1
	.ascii "uPlus_param_I_u64"
	.space 1
	.ascii "Plus_var_I_u64"
	.space 1
	.ascii "uPlus_var_I_u64"
	.space 1
	.ascii "Plus_param_I_u16"
	.space 1
	.ascii "uPlus_param_I_u16"
	.space 1
	.ascii "Plus_var_I_u16"
	.space 1
	.ascii "uPlus_var_I_u16"
	.space 1
	.ascii "Plus_param_I_C"
	.space 1
	.ascii "uPlus_param_I_C"
	.space 1
	.ascii "Plus_var_I_C"
	.space 1
	.ascii "uPlus_var_I_C"
	.space 1
	.ascii "Plus_param_I_u32"
	.space 1
	.ascii "uPlus_param_I_u32"
	.space 1
	.ascii "Plus_var_I_u32"
	.space 1
	.ascii "uPlus_var_I_u32"
	.space 1
	.ascii "Plus_param_I_i8"
	.space 1
	.ascii "uPlus_param_I_i8"
	.space 1
	.ascii "Plus_var_I_i8"
	.space 1
	.ascii "uPlus_var_I_i8"
	.space 1
	.ascii "Plus_param_I_I"
	.space 1
	.ascii "uPlus_param_I_I"
	.space 1
	.ascii "Plus_var_I_I"
	.space 1
	.ascii "uPlus_var_I_I"
	.space 1
	.ascii "Plus_param_I_u8"
	.space 1
	.ascii "uPlus_param_I_u8"
	.space 1
	.ascii "Plus_var_I_u8"
	.space 1
	.ascii "uPlus_var_I_u8"
	.space 1
	.ascii "Plus_param_I_LC"
	.space 1
	.ascii "uPlus_param_I_LC"
	.space 1
	.ascii "Plus_var_I_LC"
	.space 1
	.ascii "uPlus_var_I_LC"
	.space 1
	.ascii "Plus_param_I_i64"
	.space 1
	.ascii "uPlus_param_I_i64"
	.space 1
	.ascii "Plus_var_I_i64"
	.space 1
	.ascii "uPlus_var_I_i64"
	.space 1
	.ascii "Plus_param_u8_i32"
	.space 1
	.ascii "uPlus_param_u8_i32"
	.space 1
	.ascii "Plus_var_u8_i32"
	.space 1
	.ascii "uPlus_var_u8_i32"
	.space 1
	.ascii "Plus_param_u8_i16"
	.space 1
	.ascii "uPlus_param_u8_i16"
	.space 1
	.ascii "Plus_var_u8_i16"
	.space 1
	.ascii "uPlus_var_u8_i16"
	.space 1
	.ascii "Plus_param_u8_L"
	.space 1
	.ascii "uPlus_param_u8_L"
	.space 1
	.ascii "Plus_var_u8_L"
	.space 1
	.ascii "uPlus_var_u8_L"
	.space 1
	.ascii "Plus_param_u8_u64"
	.space 1
	.ascii "uPlus_param_u8_u64"
	.space 1
	.ascii "Plus_var_u8_u64"
	.space 1
	.ascii "uPlus_var_u8_u64"
	.space 1
	.ascii "Plus_param_u8_u16"
	.space 1
	.ascii "uPlus_param_u8_u16"
	.space 1
	.ascii "Plus_var_u8_u16"
	.space 1
	.ascii "uPlus_var_u8_u16"
	.space 1
	.ascii "Plus_param_u8_C"
	.space 1
	.ascii "uPlus_param_u8_C"
	.space 1
	.ascii "Plus_var_u8_C"
	.space 1
	.ascii "uPlus_var_u8_C"
	.space 1
	.ascii "Plus_param_u8_u32"
	.space 1
	.ascii "uPlus_param_u8_u32"
	.space 1
	.ascii "Plus_var_u8_u32"
	.space 1
	.ascii "uPlus_var_u8_u32"
	.space 1
	.ascii "Plus_param_u8_i8"
	.space 1
	.ascii "uPlus_param_u8_i8"
	.space 1
	.ascii "Plus_var_u8_i8"
	.space 1
	.ascii "uPlus_var_u8_i8"
	.space 1
	.ascii "Plus_param_u8_I"
	.space 1
	.ascii "uPlus_param_u8_I"
	.space 1
	.ascii "Plus_var_u8_I"
	.space 1
	.ascii "uPlus_var_u8_I"
	.space 1
	.ascii "Plus_param_u8_u8"
	.space 1
	.ascii "uPlus_param_u8_u8"
	.space 1
	.ascii "Plus_var_u8_u8"
	.space 1
	.ascii "uPlus_var_u8_u8"
	.space 1
	.ascii "Plus_param_u8_LC"
	.space 1
	.ascii "uPlus_param_u8_LC"
	.space 1
	.ascii "Plus_var_u8_LC"
	.space 1
	.ascii "uPlus_var_u8_LC"
	.space 1
	.ascii "Plus_param_u8_i64"
	.space 1
	.ascii "uPlus_param_u8_i64"
	.space 1
	.ascii "Plus_var_u8_i64"
	.space 1
	.ascii "uPlus_var_u8_i64"
	.space 1
	.ascii "Plus_param_f32_f32"
	.space 1
	.ascii "Plus_var_f32_f32"
	.space 1
	.ascii "Plus_param_LC_i32"
	.space 1
	.ascii "uPlus_param_LC_i32"
	.space 1
	.ascii "Plus_var_LC_i32"
	.space 1
	.ascii "uPlus_var_LC_i32"
	.space 1
	.ascii "Plus_param_LC_i16"
	.space 1
	.ascii "uPlus_param_LC_i16"
	.space 1
	.ascii "Plus_var_LC_i16"
	.space 1
	.ascii "uPlus_var_LC_i16"
	.space 1
	.ascii "Plus_param_LC_L"
	.space 1
	.ascii "uPlus_param_LC_L"
	.space 1
	.ascii "Plus_var_LC_L"
	.space 1
	.ascii "uPlus_var_LC_L"
	.space 1
	.ascii "Plus_param_LC_u64"
	.space 1
	.ascii "uPlus_param_LC_u64"
	.space 1
	.ascii "Plus_var_LC_u64"
	.space 1
	.ascii "uPlus_var_LC_u64"
	.space 1
	.ascii "Plus_param_LC_u16"
	.space 1
	.ascii "uPlus_param_LC_u16"
	.space 1
	.ascii "Plus_var_LC_u16"
	.space 1
	.ascii "uPlus_var_LC_u16"
	.space 1
	.ascii "Plus_param_LC_C"
	.space 1
	.ascii "uPlus_param_LC_C"
	.space 1
	.ascii "Plus_var_LC_C"
	.space 1
	.ascii "uPlus_var_LC_C"
	.space 1
	.ascii "Plus_param_LC_u32"
	.space 1
	.ascii "uPlus_param_LC_u32"
	.space 1
	.ascii "Plus_var_LC_u32"
	.space 1
	.ascii "uPlus_var_LC_u32"
	.space 1
	.ascii "Plus_param_LC_i8"
	.space 1
	.ascii "uPlus_param_LC_i8"
	.space 1
	.ascii "Plus_var_LC_i8"
	.space 1
	.ascii "uPlus_var_LC_i8"
	.space 1
	.ascii "Plus_param_LC_I"
	.space 1
	.ascii "uPlus_param_LC_I"
	.space 1
	.ascii "Plus_var_LC_I"
	.space 1
	.ascii "uPlus_var_LC_I"
	.space 1
	.ascii "Plus_param_LC_u8"
	.space 1
	.ascii "uPlus_param_LC_u8"
	.space 1
	.ascii "Plus_var_LC_u8"
	.space 1
	.ascii "uPlus_var_LC_u8"
	.space 1
	.ascii "Plus_param_LC_LC"
	.space 1
	.ascii "uPlus_param_LC_LC"
	.space 1
	.ascii "Plus_var_LC_LC"
	.space 1
	.ascii "uPlus_var_LC_LC"
	.space 1
	.ascii "Plus_param_LC_i64"
	.space 1
	.ascii "uPlus_param_LC_i64"
	.space 1
	.ascii "Plus_var_LC_i64"
	.space 1
	.ascii "uPlus_var_LC_i64"
	.space 1
	.ascii "Plus_param_i64_i32"
	.space 1
	.ascii "uPlus_param_i64_i32"
	.space 1
	.ascii "Plus_var_i64_i32"
	.space 1
	.ascii "uPlus_var_i64_i32"
	.space 1
	.ascii "Plus_param_i64_i16"
	.space 1
	.ascii "uPlus_param_i64_i16"
	.space 1
	.ascii "Plus_var_i64_i16"
	.space 1
	.ascii "uPlus_var_i64_i16"
	.space 1
	.ascii "Plus_param_i64_L"
	.space 1
	.ascii "uPlus_param_i64_L"
	.space 1
	.ascii "Plus_var_i64_L"
	.space 1
	.ascii "uPlus_var_i64_L"
	.space 1
	.ascii "Plus_param_i64_u64"
	.space 1
	.ascii "uPlus_param_i64_u64"
	.space 1
	.ascii "Plus_var_i64_u64"
	.space 1
	.ascii "uPlus_var_i64_u64"
	.space 1
	.ascii "Plus_param_i64_u16"
	.space 1
	.ascii "uPlus_param_i64_u16"
	.space 1
	.ascii "Plus_var_i64_u16"
	.space 1
	.ascii "uPlus_var_i64_u16"
	.space 1
	.ascii "Plus_param_i64_C"
	.space 1
	.ascii "uPlus_param_i64_C"
	.space 1
	.ascii "Plus_var_i64_C"
	.space 1
	.ascii "uPlus_var_i64_C"
	.space 1
	.ascii "Plus_param_i64_u32"
	.space 1
	.ascii "uPlus_param_i64_u32"
	.space 1
	.ascii "Plus_var_i64_u32"
	.space 1
	.ascii "uPlus_var_i64_u32"
	.space 1
	.ascii "Plus_param_i64_i8"
	.space 1
	.ascii "uPlus_param_i64_i8"
	.space 1
	.ascii "Plus_var_i64_i8"
	.space 1
	.ascii "uPlus_var_i64_i8"
	.space 1
	.ascii "Plus_param_i64_I"
	.space 1
	.ascii "uPlus_param_i64_I"
	.space 1
	.ascii "Plus_var_i64_I"
	.space 1
	.ascii "uPlus_var_i64_I"
	.space 1
	.ascii "Plus_param_i64_u8"
	.space 1
	.ascii "uPlus_param_i64_u8"
	.space 1
	.ascii "Plus_var_i64_u8"
	.space 1
	.ascii "uPlus_var_i64_u8"
	.space 1
	.ascii "Plus_param_i64_LC"
	.space 1
	.ascii "uPlus_param_i64_LC"
	.space 1
	.ascii "Plus_var_i64_LC"
	.space 1
	.ascii "uPlus_var_i64_LC"
	.space 1
	.ascii "Plus_param_i64_i64"
	.space 1
	.ascii "uPlus_param_i64_i64"
	.space 1
	.ascii "Plus_var_i64_i64"
	.space 1
	.ascii "uPlus_var_i64_i64"
	.space 1
	.quad	_Plus_M3
	.quad	_L_1
	.quad	_Plus__Plus_param_i32_i32
	.quad	_L_1+8
	.quad	_Plus__uPlus_param_i32_i32
	.quad	_L_1+27
	.quad	_Plus__Plus_var_i32_i32
	.quad	_L_1+47
	.quad	_Plus__uPlus_var_i32_i32
	.quad	_L_1+64
	.quad	_Plus__Plus_param_i32_i16
	.quad	_L_1+82
	.quad	_Plus__uPlus_param_i32_i16
	.quad	_L_1+101
	.quad	_Plus__Plus_var_i32_i16
	.quad	_L_1+121
	.quad	_Plus__uPlus_var_i32_i16
	.quad	_L_1+138
	.quad	_Plus__Plus_param_i32_L
	.quad	_L_1+156
	.quad	_Plus__uPlus_param_i32_L
	.quad	_L_1+173
	.quad	_Plus__Plus_var_i32_L
	.quad	_L_1+191
	.quad	_Plus__uPlus_var_i32_L
	.quad	_L_1+206
	.quad	_Plus__Plus_param_i32_u64
	.quad	_L_1+222
	.quad	_Plus__uPlus_param_i32_u64
	.quad	_L_1+241
	.quad	_Plus__Plus_var_i32_u64
	.quad	_L_1+261
	.quad	_Plus__uPlus_var_i32_u64
	.quad	_L_1+278
	.quad	_Plus__Plus_param_i32_u16
	.quad	_L_1+296
	.quad	_Plus__uPlus_param_i32_u16
	.quad	_L_1+315
	.quad	_Plus__Plus_var_i32_u16
	.quad	_L_1+335
	.quad	_Plus__uPlus_var_i32_u16
	.quad	_L_1+352
	.quad	_Plus__Plus_param_i32_C
	.quad	_L_1+370
	.quad	_Plus__uPlus_param_i32_C
	.quad	_L_1+387
	.quad	_Plus__Plus_var_i32_C
	.quad	_L_1+405
	.quad	_Plus__uPlus_var_i32_C
	.quad	_L_1+420
	.quad	_Plus__Plus_param_i32_u32
	.quad	_L_1+436
	.quad	_Plus__uPlus_param_i32_u32
	.quad	_L_1+455
	.quad	_Plus__Plus_var_i32_u32
	.quad	_L_1+475
	.quad	_Plus__uPlus_var_i32_u32
	.quad	_L_1+492
	.quad	_Plus__Plus_param_i32_i8
	.quad	_L_1+510
	.quad	_Plus__uPlus_param_i32_i8
	.quad	_L_1+528
	.quad	_Plus__Plus_var_i32_i8
	.quad	_L_1+547
	.quad	_Plus__uPlus_var_i32_i8
	.quad	_L_1+563
	.quad	_Plus__Plus_param_i32_I
	.quad	_L_1+580
	.quad	_Plus__uPlus_param_i32_I
	.quad	_L_1+597
	.quad	_Plus__Plus_var_i32_I
	.quad	_L_1+615
	.quad	_Plus__uPlus_var_i32_I
	.quad	_L_1+630
	.quad	_Plus__Plus_param_i32_u8
	.quad	_L_1+646
	.quad	_Plus__uPlus_param_i32_u8
	.quad	_L_1+664
	.quad	_Plus__Plus_var_i32_u8
	.quad	_L_1+683
	.quad	_Plus__uPlus_var_i32_u8
	.quad	_L_1+699
	.quad	_Plus__Plus_param_i32_LC
	.quad	_L_1+716
	.quad	_Plus__uPlus_param_i32_LC
	.quad	_L_1+734
	.quad	_Plus__Plus_var_i32_LC
	.quad	_L_1+753
	.quad	_Plus__uPlus_var_i32_LC
	.quad	_L_1+769
	.quad	_Plus__Plus_param_i32_i64
	.quad	_L_1+786
	.quad	_Plus__uPlus_param_i32_i64
	.quad	_L_1+805
	.quad	_Plus__Plus_var_i32_i64
	.quad	_L_1+825
	.quad	_Plus__uPlus_var_i32_i64
	.quad	_L_1+842
	.quad	_Plus__Plus_param_i16_i32
	.quad	_L_1+860
	.quad	_Plus__uPlus_param_i16_i32
	.quad	_L_1+879
	.quad	_Plus__Plus_var_i16_i32
	.quad	_L_1+899
	.quad	_Plus__uPlus_var_i16_i32
	.quad	_L_1+916
	.quad	_Plus__Plus_param_i16_i16
	.quad	_L_1+934
	.quad	_Plus__uPlus_param_i16_i16
	.quad	_L_1+953
	.quad	_Plus__Plus_var_i16_i16
	.quad	_L_1+973
	.quad	_Plus__uPlus_var_i16_i16
	.quad	_L_1+990
	.quad	_Plus__Plus_param_i16_L
	.quad	_L_1+1008
	.quad	_Plus__uPlus_param_i16_L
	.quad	_L_1+1025
	.quad	_Plus__Plus_var_i16_L
	.quad	_L_1+1043
	.quad	_Plus__uPlus_var_i16_L
	.quad	_L_1+1058
	.quad	_Plus__Plus_param_i16_u64
	.quad	_L_1+1074
	.quad	_Plus__uPlus_param_i16_u64
	.quad	_L_1+1093
	.quad	_Plus__Plus_var_i16_u64
	.quad	_L_1+1113
	.quad	_Plus__uPlus_var_i16_u64
	.quad	_L_1+1130
	.quad	_Plus__Plus_param_i16_u16
	.quad	_L_1+1148
	.quad	_Plus__uPlus_param_i16_u16
	.quad	_L_1+1167
	.quad	_Plus__Plus_var_i16_u16
	.quad	_L_1+1187
	.quad	_Plus__uPlus_var_i16_u16
	.quad	_L_1+1204
	.quad	_Plus__Plus_param_i16_C
	.quad	_L_1+1222
	.quad	_Plus__uPlus_param_i16_C
	.quad	_L_1+1239
	.quad	_Plus__Plus_var_i16_C
	.quad	_L_1+1257
	.quad	_Plus__uPlus_var_i16_C
	.quad	_L_1+1272
	.quad	_Plus__Plus_param_i16_u32
	.quad	_L_1+1288
	.quad	_Plus__uPlus_param_i16_u32
	.quad	_L_1+1307
	.quad	_Plus__Plus_var_i16_u32
	.quad	_L_1+1327
	.quad	_Plus__uPlus_var_i16_u32
	.quad	_L_1+1344
	.quad	_Plus__Plus_param_i16_i8
	.quad	_L_1+1362
	.quad	_Plus__uPlus_param_i16_i8
	.quad	_L_1+1380
	.quad	_Plus__Plus_var_i16_i8
	.quad	_L_1+1399
	.quad	_Plus__uPlus_var_i16_i8
	.quad	_L_1+1415
	.quad	_Plus__Plus_param_i16_I
	.quad	_L_1+1432
	.quad	_Plus__uPlus_param_i16_I
	.quad	_L_1+1449
	.quad	_Plus__Plus_var_i16_I
	.quad	_L_1+1467
	.quad	_Plus__uPlus_var_i16_I
	.quad	_L_1+1482
	.quad	_Plus__Plus_param_i16_u8
	.quad	_L_1+1498
	.quad	_Plus__uPlus_param_i16_u8
	.quad	_L_1+1516
	.quad	_Plus__Plus_var_i16_u8
	.quad	_L_1+1535
	.quad	_Plus__uPlus_var_i16_u8
	.quad	_L_1+1551
	.quad	_Plus__Plus_param_i16_LC
	.quad	_L_1+1568
	.quad	_Plus__uPlus_param_i16_LC
	.quad	_L_1+1586
	.quad	_Plus__Plus_var_i16_LC
	.quad	_L_1+1605
	.quad	_Plus__uPlus_var_i16_LC
	.quad	_L_1+1621
	.quad	_Plus__Plus_param_i16_i64
	.quad	_L_1+1638
	.quad	_Plus__uPlus_param_i16_i64
	.quad	_L_1+1657
	.quad	_Plus__Plus_var_i16_i64
	.quad	_L_1+1677
	.quad	_Plus__uPlus_var_i16_i64
	.quad	_L_1+1694
	.quad	_Plus__Plus_param_L_i32
	.quad	_L_1+1712
	.quad	_Plus__uPlus_param_L_i32
	.quad	_L_1+1729
	.quad	_Plus__Plus_var_L_i32
	.quad	_L_1+1747
	.quad	_Plus__uPlus_var_L_i32
	.quad	_L_1+1762
	.quad	_Plus__Plus_param_L_i16
	.quad	_L_1+1778
	.quad	_Plus__uPlus_param_L_i16
	.quad	_L_1+1795
	.quad	_Plus__Plus_var_L_i16
	.quad	_L_1+1813
	.quad	_Plus__uPlus_var_L_i16
	.quad	_L_1+1828
	.quad	_Plus__Plus_param_L_L
	.quad	_L_1+1844
	.quad	_Plus__uPlus_param_L_L
	.quad	_L_1+1859
	.quad	_Plus__Plus_var_L_L
	.quad	_L_1+1875
	.quad	_Plus__uPlus_var_L_L
	.quad	_L_1+1888
	.quad	_Plus__Plus_param_L_u64
	.quad	_L_1+1902
	.quad	_Plus__uPlus_param_L_u64
	.quad	_L_1+1919
	.quad	_Plus__Plus_var_L_u64
	.quad	_L_1+1937
	.quad	_Plus__uPlus_var_L_u64
	.quad	_L_1+1952
	.quad	_Plus__Plus_param_L_u16
	.quad	_L_1+1968
	.quad	_Plus__uPlus_param_L_u16
	.quad	_L_1+1985
	.quad	_Plus__Plus_var_L_u16
	.quad	_L_1+2003
	.quad	_Plus__uPlus_var_L_u16
	.quad	_L_1+2018
	.quad	_Plus__Plus_param_L_C
	.quad	_L_1+2034
	.quad	_Plus__uPlus_param_L_C
	.quad	_L_1+2049
	.quad	_Plus__Plus_var_L_C
	.quad	_L_1+2065
	.quad	_Plus__uPlus_var_L_C
	.quad	_L_1+2078
	.quad	_Plus__Plus_param_L_u32
	.quad	_L_1+2092
	.quad	_Plus__uPlus_param_L_u32
	.quad	_L_1+2109
	.quad	_Plus__Plus_var_L_u32
	.quad	_L_1+2127
	.quad	_Plus__uPlus_var_L_u32
	.quad	_L_1+2142
	.quad	_Plus__Plus_param_L_i8
	.quad	_L_1+2158
	.quad	_Plus__uPlus_param_L_i8
	.quad	_L_1+2174
	.quad	_Plus__Plus_var_L_i8
	.quad	_L_1+2191
	.quad	_Plus__uPlus_var_L_i8
	.quad	_L_1+2205
	.quad	_Plus__Plus_param_L_I
	.quad	_L_1+2220
	.quad	_Plus__uPlus_param_L_I
	.quad	_L_1+2235
	.quad	_Plus__Plus_var_L_I
	.quad	_L_1+2251
	.quad	_Plus__uPlus_var_L_I
	.quad	_L_1+2264
	.quad	_Plus__Plus_param_L_u8
	.quad	_L_1+2278
	.quad	_Plus__uPlus_param_L_u8
	.quad	_L_1+2294
	.quad	_Plus__Plus_var_L_u8
	.quad	_L_1+2311
	.quad	_Plus__uPlus_var_L_u8
	.quad	_L_1+2325
	.quad	_Plus__Plus_param_L_LC
	.quad	_L_1+2340
	.quad	_Plus__uPlus_param_L_LC
	.quad	_L_1+2356
	.quad	_Plus__Plus_var_L_LC
	.quad	_L_1+2373
	.quad	_Plus__uPlus_var_L_LC
	.quad	_L_1+2387
	.quad	_Plus__Plus_param_L_i64
	.quad	_L_1+2402
	.quad	_Plus__uPlus_param_L_i64
	.quad	_L_1+2419
	.quad	_Plus__Plus_var_L_i64
	.quad	_L_1+2437
	.quad	_Plus__uPlus_var_L_i64
	.quad	_L_1+2452
	.quad	_Plus__Plus_param_f64_f64
	.quad	_L_1+2468
	.quad	_Plus__Plus_var_f64_f64
	.quad	_L_1+2487
	.quad	_Plus__Plus_param_u64_i32
	.quad	_L_1+2504
	.quad	_Plus__uPlus_param_u64_i32
	.quad	_L_1+2523
	.quad	_Plus__Plus_var_u64_i32
	.quad	_L_1+2543
	.quad	_Plus__uPlus_var_u64_i32
	.quad	_L_1+2560
	.quad	_Plus__Plus_param_u64_i16
	.quad	_L_1+2578
	.quad	_Plus__uPlus_param_u64_i16
	.quad	_L_1+2597
	.quad	_Plus__Plus_var_u64_i16
	.quad	_L_1+2617
	.quad	_Plus__uPlus_var_u64_i16
	.quad	_L_1+2634
	.quad	_Plus__Plus_param_u64_L
	.quad	_L_1+2652
	.quad	_Plus__uPlus_param_u64_L
	.quad	_L_1+2669
	.quad	_Plus__Plus_var_u64_L
	.quad	_L_1+2687
	.quad	_Plus__uPlus_var_u64_L
	.quad	_L_1+2702
	.quad	_Plus__Plus_param_u64_u64
	.quad	_L_1+2718
	.quad	_Plus__uPlus_param_u64_u64
	.quad	_L_1+2737
	.quad	_Plus__Plus_var_u64_u64
	.quad	_L_1+2757
	.quad	_Plus__uPlus_var_u64_u64
	.quad	_L_1+2774
	.quad	_Plus__Plus_param_u64_u16
	.quad	_L_1+2792
	.quad	_Plus__uPlus_param_u64_u16
	.quad	_L_1+2811
	.quad	_Plus__Plus_var_u64_u16
	.quad	_L_1+2831
	.quad	_Plus__uPlus_var_u64_u16
	.quad	_L_1+2848
	.quad	_Plus__Plus_param_u64_C
	.quad	_L_1+2866
	.quad	_Plus__uPlus_param_u64_C
	.quad	_L_1+2883
	.quad	_Plus__Plus_var_u64_C
	.quad	_L_1+2901
	.quad	_Plus__uPlus_var_u64_C
	.quad	_L_1+2916
	.quad	_Plus__Plus_param_u64_u32
	.quad	_L_1+2932
	.quad	_Plus__uPlus_param_u64_u32
	.quad	_L_1+2951
	.quad	_Plus__Plus_var_u64_u32
	.quad	_L_1+2971
	.quad	_Plus__uPlus_var_u64_u32
	.quad	_L_1+2988
	.quad	_Plus__Plus_param_u64_i8
	.quad	_L_1+3006
	.quad	_Plus__uPlus_param_u64_i8
	.quad	_L_1+3024
	.quad	_Plus__Plus_var_u64_i8
	.quad	_L_1+3043
	.quad	_Plus__uPlus_var_u64_i8
	.quad	_L_1+3059
	.quad	_Plus__Plus_param_u64_I
	.quad	_L_1+3076
	.quad	_Plus__uPlus_param_u64_I
	.quad	_L_1+3093
	.quad	_Plus__Plus_var_u64_I
	.quad	_L_1+3111
	.quad	_Plus__uPlus_var_u64_I
	.quad	_L_1+3126
	.quad	_Plus__Plus_param_u64_u8
	.quad	_L_1+3142
	.quad	_Plus__uPlus_param_u64_u8
	.quad	_L_1+3160
	.quad	_Plus__Plus_var_u64_u8
	.quad	_L_1+3179
	.quad	_Plus__uPlus_var_u64_u8
	.quad	_L_1+3195
	.quad	_Plus__Plus_param_u64_LC
	.quad	_L_1+3212
	.quad	_Plus__uPlus_param_u64_LC
	.quad	_L_1+3230
	.quad	_Plus__Plus_var_u64_LC
	.quad	_L_1+3249
	.quad	_Plus__uPlus_var_u64_LC
	.quad	_L_1+3265
	.quad	_Plus__Plus_param_u64_i64
	.quad	_L_1+3282
	.quad	_Plus__uPlus_param_u64_i64
	.quad	_L_1+3301
	.quad	_Plus__Plus_var_u64_i64
	.quad	_L_1+3321
	.quad	_Plus__uPlus_var_u64_i64
	.quad	_L_1+3338
	.quad	_Plus__Plus_param_u16_i32
	.quad	_L_1+3356
	.quad	_Plus__uPlus_param_u16_i32
	.quad	_L_1+3375
	.quad	_Plus__Plus_var_u16_i32
	.quad	_L_1+3395
	.quad	_Plus__uPlus_var_u16_i32
	.quad	_L_1+3412
	.quad	_Plus__Plus_param_u16_i16
	.quad	_L_1+3430
	.quad	_Plus__uPlus_param_u16_i16
	.quad	_L_1+3449
	.quad	_Plus__Plus_var_u16_i16
	.quad	_L_1+3469
	.quad	_Plus__uPlus_var_u16_i16
	.quad	_L_1+3486
	.quad	_Plus__Plus_param_u16_L
	.quad	_L_1+3504
	.quad	_Plus__uPlus_param_u16_L
	.quad	_L_1+3521
	.quad	_Plus__Plus_var_u16_L
	.quad	_L_1+3539
	.quad	_Plus__uPlus_var_u16_L
	.quad	_L_1+3554
	.quad	_Plus__Plus_param_u16_u64
	.quad	_L_1+3570
	.quad	_Plus__uPlus_param_u16_u64
	.quad	_L_1+3589
	.quad	_Plus__Plus_var_u16_u64
	.quad	_L_1+3609
	.quad	_Plus__uPlus_var_u16_u64
	.quad	_L_1+3626
	.quad	_Plus__Plus_param_u16_u16
	.quad	_L_1+3644
	.quad	_Plus__uPlus_param_u16_u16
	.quad	_L_1+3663
	.quad	_Plus__Plus_var_u16_u16
	.quad	_L_1+3683
	.quad	_Plus__uPlus_var_u16_u16
	.quad	_L_1+3700
	.quad	_Plus__Plus_param_u16_C
	.quad	_L_1+3718
	.quad	_Plus__uPlus_param_u16_C
	.quad	_L_1+3735
	.quad	_Plus__Plus_var_u16_C
	.quad	_L_1+3753
	.quad	_Plus__uPlus_var_u16_C
	.quad	_L_1+3768
	.quad	_Plus__Plus_param_u16_u32
	.quad	_L_1+3784
	.quad	_Plus__uPlus_param_u16_u32
	.quad	_L_1+3803
	.quad	_Plus__Plus_var_u16_u32
	.quad	_L_1+3823
	.quad	_Plus__uPlus_var_u16_u32
	.quad	_L_1+3840
	.quad	_Plus__Plus_param_u16_i8
	.quad	_L_1+3858
	.quad	_Plus__uPlus_param_u16_i8
	.quad	_L_1+3876
	.quad	_Plus__Plus_var_u16_i8
	.quad	_L_1+3895
	.quad	_Plus__uPlus_var_u16_i8
	.quad	_L_1+3911
	.quad	_Plus__Plus_param_u16_I
	.quad	_L_1+3928
	.quad	_Plus__uPlus_param_u16_I
	.quad	_L_1+3945
	.quad	_Plus__Plus_var_u16_I
	.quad	_L_1+3963
	.quad	_Plus__uPlus_var_u16_I
	.quad	_L_1+3978
	.quad	_Plus__Plus_param_u16_u8
	.quad	_L_1+3994
	.quad	_Plus__uPlus_param_u16_u8
	.quad	_L_1+4012
	.quad	_Plus__Plus_var_u16_u8
	.quad	_L_1+4031
	.quad	_Plus__uPlus_var_u16_u8
	.quad	_L_1+4047
	.quad	_Plus__Plus_param_u16_LC
	.quad	_L_1+4064
	.quad	_Plus__uPlus_param_u16_LC
	.quad	_L_1+4082
	.quad	_Plus__Plus_var_u16_LC
	.quad	_L_1+4101
	.quad	_Plus__uPlus_var_u16_LC
	.quad	_L_1+4117
	.quad	_Plus__Plus_param_u16_i64
	.quad	_L_1+4134
	.quad	_Plus__uPlus_param_u16_i64
	.quad	_L_1+4153
	.quad	_Plus__Plus_var_u16_i64
	.quad	_L_1+4173
	.quad	_Plus__uPlus_var_u16_i64
	.quad	_L_1+4190
	.quad	_Plus__Plus_param_C_i32
	.quad	_L_1+4208
	.quad	_Plus__uPlus_param_C_i32
	.quad	_L_1+4225
	.quad	_Plus__Plus_var_C_i32
	.quad	_L_1+4243
	.quad	_Plus__uPlus_var_C_i32
	.quad	_L_1+4258
	.quad	_Plus__Plus_param_C_i16
	.quad	_L_1+4274
	.quad	_Plus__uPlus_param_C_i16
	.quad	_L_1+4291
	.quad	_Plus__Plus_var_C_i16
	.quad	_L_1+4309
	.quad	_Plus__uPlus_var_C_i16
	.quad	_L_1+4324
	.quad	_Plus__Plus_param_C_L
	.quad	_L_1+4340
	.quad	_Plus__uPlus_param_C_L
	.quad	_L_1+4355
	.quad	_Plus__Plus_var_C_L
	.quad	_L_1+4371
	.quad	_Plus__uPlus_var_C_L
	.quad	_L_1+4384
	.quad	_Plus__Plus_param_C_u64
	.quad	_L_1+4398
	.quad	_Plus__uPlus_param_C_u64
	.quad	_L_1+4415
	.quad	_Plus__Plus_var_C_u64
	.quad	_L_1+4433
	.quad	_Plus__uPlus_var_C_u64
	.quad	_L_1+4448
	.quad	_Plus__Plus_param_C_u16
	.quad	_L_1+4464
	.quad	_Plus__uPlus_param_C_u16
	.quad	_L_1+4481
	.quad	_Plus__Plus_var_C_u16
	.quad	_L_1+4499
	.quad	_Plus__uPlus_var_C_u16
	.quad	_L_1+4514
	.quad	_Plus__Plus_param_C_C
	.quad	_L_1+4530
	.quad	_Plus__uPlus_param_C_C
	.quad	_L_1+4545
	.quad	_Plus__Plus_var_C_C
	.quad	_L_1+4561
	.quad	_Plus__uPlus_var_C_C
	.quad	_L_1+4574
	.quad	_Plus__Plus_param_C_u32
	.quad	_L_1+4588
	.quad	_Plus__uPlus_param_C_u32
	.quad	_L_1+4605
	.quad	_Plus__Plus_var_C_u32
	.quad	_L_1+4623
	.quad	_Plus__uPlus_var_C_u32
	.quad	_L_1+4638
	.quad	_Plus__Plus_param_C_i8
	.quad	_L_1+4654
	.quad	_Plus__uPlus_param_C_i8
	.quad	_L_1+4670
	.quad	_Plus__Plus_var_C_i8
	.quad	_L_1+4687
	.quad	_Plus__uPlus_var_C_i8
	.quad	_L_1+4701
	.quad	_Plus__Plus_param_C_I
	.quad	_L_1+4716
	.quad	_Plus__uPlus_param_C_I
	.quad	_L_1+4731
	.quad	_Plus__Plus_var_C_I
	.quad	_L_1+4747
	.quad	_Plus__uPlus_var_C_I
	.quad	_L_1+4760
	.quad	_Plus__Plus_param_C_u8
	.quad	_L_1+4774
	.quad	_Plus__uPlus_param_C_u8
	.quad	_L_1+4790
	.quad	_Plus__Plus_var_C_u8
	.quad	_L_1+4807
	.quad	_Plus__uPlus_var_C_u8
	.quad	_L_1+4821
	.quad	_Plus__Plus_param_C_LC
	.quad	_L_1+4836
	.quad	_Plus__uPlus_param_C_LC
	.quad	_L_1+4852
	.quad	_Plus__Plus_var_C_LC
	.quad	_L_1+4869
	.quad	_Plus__uPlus_var_C_LC
	.quad	_L_1+4883
	.quad	_Plus__Plus_param_C_i64
	.quad	_L_1+4898
	.quad	_Plus__uPlus_param_C_i64
	.quad	_L_1+4915
	.quad	_Plus__Plus_var_C_i64
	.quad	_L_1+4933
	.quad	_Plus__uPlus_var_C_i64
	.quad	_L_1+4948
	.quad	_Plus__Plus_param_u32_i32
	.quad	_L_1+4964
	.quad	_Plus__uPlus_param_u32_i32
	.quad	_L_1+4983
	.quad	_Plus__Plus_var_u32_i32
	.quad	_L_1+5003
	.quad	_Plus__uPlus_var_u32_i32
	.quad	_L_1+5020
	.quad	_Plus__Plus_param_u32_i16
	.quad	_L_1+5038
	.quad	_Plus__uPlus_param_u32_i16
	.quad	_L_1+5057
	.quad	_Plus__Plus_var_u32_i16
	.quad	_L_1+5077
	.quad	_Plus__uPlus_var_u32_i16
	.quad	_L_1+5094
	.quad	_Plus__Plus_param_u32_L
	.quad	_L_1+5112
	.quad	_Plus__uPlus_param_u32_L
	.quad	_L_1+5129
	.quad	_Plus__Plus_var_u32_L
	.quad	_L_1+5147
	.quad	_Plus__uPlus_var_u32_L
	.quad	_L_1+5162
	.quad	_Plus__Plus_param_u32_u64
	.quad	_L_1+5178
	.quad	_Plus__uPlus_param_u32_u64
	.quad	_L_1+5197
	.quad	_Plus__Plus_var_u32_u64
	.quad	_L_1+5217
	.quad	_Plus__uPlus_var_u32_u64
	.quad	_L_1+5234
	.quad	_Plus__Plus_param_u32_u16
	.quad	_L_1+5252
	.quad	_Plus__uPlus_param_u32_u16
	.quad	_L_1+5271
	.quad	_Plus__Plus_var_u32_u16
	.quad	_L_1+5291
	.quad	_Plus__uPlus_var_u32_u16
	.quad	_L_1+5308
	.quad	_Plus__Plus_param_u32_C
	.quad	_L_1+5326
	.quad	_Plus__uPlus_param_u32_C
	.quad	_L_1+5343
	.quad	_Plus__Plus_var_u32_C
	.quad	_L_1+5361
	.quad	_Plus__uPlus_var_u32_C
	.quad	_L_1+5376
	.quad	_Plus__Plus_param_u32_u32
	.quad	_L_1+5392
	.quad	_Plus__uPlus_param_u32_u32
	.quad	_L_1+5411
	.quad	_Plus__Plus_var_u32_u32
	.quad	_L_1+5431
	.quad	_Plus__uPlus_var_u32_u32
	.quad	_L_1+5448
	.quad	_Plus__Plus_param_u32_i8
	.quad	_L_1+5466
	.quad	_Plus__uPlus_param_u32_i8
	.quad	_L_1+5484
	.quad	_Plus__Plus_var_u32_i8
	.quad	_L_1+5503
	.quad	_Plus__uPlus_var_u32_i8
	.quad	_L_1+5519
	.quad	_Plus__Plus_param_u32_I
	.quad	_L_1+5536
	.quad	_Plus__uPlus_param_u32_I
	.quad	_L_1+5553
	.quad	_Plus__Plus_var_u32_I
	.quad	_L_1+5571
	.quad	_Plus__uPlus_var_u32_I
	.quad	_L_1+5586
	.quad	_Plus__Plus_param_u32_u8
	.quad	_L_1+5602
	.quad	_Plus__uPlus_param_u32_u8
	.quad	_L_1+5620
	.quad	_Plus__Plus_var_u32_u8
	.quad	_L_1+5639
	.quad	_Plus__uPlus_var_u32_u8
	.quad	_L_1+5655
	.quad	_Plus__Plus_param_u32_LC
	.quad	_L_1+5672
	.quad	_Plus__uPlus_param_u32_LC
	.quad	_L_1+5690
	.quad	_Plus__Plus_var_u32_LC
	.quad	_L_1+5709
	.quad	_Plus__uPlus_var_u32_LC
	.quad	_L_1+5725
	.quad	_Plus__Plus_param_u32_i64
	.quad	_L_1+5742
	.quad	_Plus__uPlus_param_u32_i64
	.quad	_L_1+5761
	.quad	_Plus__Plus_var_u32_i64
	.quad	_L_1+5781
	.quad	_Plus__uPlus_var_u32_i64
	.quad	_L_1+5798
	.quad	_Plus__Plus_param_i8_i32
	.quad	_L_1+5816
	.quad	_Plus__uPlus_param_i8_i32
	.quad	_L_1+5834
	.quad	_Plus__Plus_var_i8_i32
	.quad	_L_1+5853
	.quad	_Plus__uPlus_var_i8_i32
	.quad	_L_1+5869
	.quad	_Plus__Plus_param_i8_i16
	.quad	_L_1+5886
	.quad	_Plus__uPlus_param_i8_i16
	.quad	_L_1+5904
	.quad	_Plus__Plus_var_i8_i16
	.quad	_L_1+5923
	.quad	_Plus__uPlus_var_i8_i16
	.quad	_L_1+5939
	.quad	_Plus__Plus_param_i8_L
	.quad	_L_1+5956
	.quad	_Plus__uPlus_param_i8_L
	.quad	_L_1+5972
	.quad	_Plus__Plus_var_i8_L
	.quad	_L_1+5989
	.quad	_Plus__uPlus_var_i8_L
	.quad	_L_1+6003
	.quad	_Plus__Plus_param_i8_u64
	.quad	_L_1+6018
	.quad	_Plus__uPlus_param_i8_u64
	.quad	_L_1+6036
	.quad	_Plus__Plus_var_i8_u64
	.quad	_L_1+6055
	.quad	_Plus__uPlus_var_i8_u64
	.quad	_L_1+6071
	.quad	_Plus__Plus_param_i8_u16
	.quad	_L_1+6088
	.quad	_Plus__uPlus_param_i8_u16
	.quad	_L_1+6106
	.quad	_Plus__Plus_var_i8_u16
	.quad	_L_1+6125
	.quad	_Plus__uPlus_var_i8_u16
	.quad	_L_1+6141
	.quad	_Plus__Plus_param_i8_C
	.quad	_L_1+6158
	.quad	_Plus__uPlus_param_i8_C
	.quad	_L_1+6174
	.quad	_Plus__Plus_var_i8_C
	.quad	_L_1+6191
	.quad	_Plus__uPlus_var_i8_C
	.quad	_L_1+6205
	.quad	_Plus__Plus_param_i8_u32
	.quad	_L_1+6220
	.quad	_Plus__uPlus_param_i8_u32
	.quad	_L_1+6238
	.quad	_Plus__Plus_var_i8_u32
	.quad	_L_1+6257
	.quad	_Plus__uPlus_var_i8_u32
	.quad	_L_1+6273
	.quad	_Plus__Plus_param_i8_i8
	.quad	_L_1+6290
	.quad	_Plus__uPlus_param_i8_i8
	.quad	_L_1+6307
	.quad	_Plus__Plus_var_i8_i8
	.quad	_L_1+6325
	.quad	_Plus__uPlus_var_i8_i8
	.quad	_L_1+6340
	.quad	_Plus__Plus_param_i8_I
	.quad	_L_1+6356
	.quad	_Plus__uPlus_param_i8_I
	.quad	_L_1+6372
	.quad	_Plus__Plus_var_i8_I
	.quad	_L_1+6389
	.quad	_Plus__uPlus_var_i8_I
	.quad	_L_1+6403
	.quad	_Plus__Plus_param_i8_u8
	.quad	_L_1+6418
	.quad	_Plus__uPlus_param_i8_u8
	.quad	_L_1+6435
	.quad	_Plus__Plus_var_i8_u8
	.quad	_L_1+6453
	.quad	_Plus__uPlus_var_i8_u8
	.quad	_L_1+6468
	.quad	_Plus__Plus_param_i8_LC
	.quad	_L_1+6484
	.quad	_Plus__uPlus_param_i8_LC
	.quad	_L_1+6501
	.quad	_Plus__Plus_var_i8_LC
	.quad	_L_1+6519
	.quad	_Plus__uPlus_var_i8_LC
	.quad	_L_1+6534
	.quad	_Plus__Plus_param_i8_i64
	.quad	_L_1+6550
	.quad	_Plus__uPlus_param_i8_i64
	.quad	_L_1+6568
	.quad	_Plus__Plus_var_i8_i64
	.quad	_L_1+6587
	.quad	_Plus__uPlus_var_i8_i64
	.quad	_L_1+6603
	.quad	_Plus__Plus_param_I_i32
	.quad	_L_1+6620
	.quad	_Plus__uPlus_param_I_i32
	.quad	_L_1+6637
	.quad	_Plus__Plus_var_I_i32
	.quad	_L_1+6655
	.quad	_Plus__uPlus_var_I_i32
	.quad	_L_1+6670
	.quad	_Plus__Plus_param_I_i16
	.quad	_L_1+6686
	.quad	_Plus__uPlus_param_I_i16
	.quad	_L_1+6703
	.quad	_Plus__Plus_var_I_i16
	.quad	_L_1+6721
	.quad	_Plus__uPlus_var_I_i16
	.quad	_L_1+6736
	.quad	_Plus__Plus_param_I_L
	.quad	_L_1+6752
	.quad	_Plus__uPlus_param_I_L
	.quad	_L_1+6767
	.quad	_Plus__Plus_var_I_L
	.quad	_L_1+6783
	.quad	_Plus__uPlus_var_I_L
	.quad	_L_1+6796
	.quad	_Plus__Plus_param_I_u64
	.quad	_L_1+6810
	.quad	_Plus__uPlus_param_I_u64
	.quad	_L_1+6827
	.quad	_Plus__Plus_var_I_u64
	.quad	_L_1+6845
	.quad	_Plus__uPlus_var_I_u64
	.quad	_L_1+6860
	.quad	_Plus__Plus_param_I_u16
	.quad	_L_1+6876
	.quad	_Plus__uPlus_param_I_u16
	.quad	_L_1+6893
	.quad	_Plus__Plus_var_I_u16
	.quad	_L_1+6911
	.quad	_Plus__uPlus_var_I_u16
	.quad	_L_1+6926
	.quad	_Plus__Plus_param_I_C
	.quad	_L_1+6942
	.quad	_Plus__uPlus_param_I_C
	.quad	_L_1+6957
	.quad	_Plus__Plus_var_I_C
	.quad	_L_1+6973
	.quad	_Plus__uPlus_var_I_C
	.quad	_L_1+6986
	.quad	_Plus__Plus_param_I_u32
	.quad	_L_1+7000
	.quad	_Plus__uPlus_param_I_u32
	.quad	_L_1+7017
	.quad	_Plus__Plus_var_I_u32
	.quad	_L_1+7035
	.quad	_Plus__uPlus_var_I_u32
	.quad	_L_1+7050
	.quad	_Plus__Plus_param_I_i8
	.quad	_L_1+7066
	.quad	_Plus__uPlus_param_I_i8
	.quad	_L_1+7082
	.quad	_Plus__Plus_var_I_i8
	.quad	_L_1+7099
	.quad	_Plus__uPlus_var_I_i8
	.quad	_L_1+7113
	.quad	_Plus__Plus_param_I_I
	.quad	_L_1+7128
	.quad	_Plus__uPlus_param_I_I
	.quad	_L_1+7143
	.quad	_Plus__Plus_var_I_I
	.quad	_L_1+7159
	.quad	_Plus__uPlus_var_I_I
	.quad	_L_1+7172
	.quad	_Plus__Plus_param_I_u8
	.quad	_L_1+7186
	.quad	_Plus__uPlus_param_I_u8
	.quad	_L_1+7202
	.quad	_Plus__Plus_var_I_u8
	.quad	_L_1+7219
	.quad	_Plus__uPlus_var_I_u8
	.quad	_L_1+7233
	.quad	_Plus__Plus_param_I_LC
	.quad	_L_1+7248
	.quad	_Plus__uPlus_param_I_LC
	.quad	_L_1+7264
	.quad	_Plus__Plus_var_I_LC
	.quad	_L_1+7281
	.quad	_Plus__uPlus_var_I_LC
	.quad	_L_1+7295
	.quad	_Plus__Plus_param_I_i64
	.quad	_L_1+7310
	.quad	_Plus__uPlus_param_I_i64
	.quad	_L_1+7327
	.quad	_Plus__Plus_var_I_i64
	.quad	_L_1+7345
	.quad	_Plus__uPlus_var_I_i64
	.quad	_L_1+7360
	.quad	_Plus__Plus_param_u8_i32
	.quad	_L_1+7376
	.quad	_Plus__uPlus_param_u8_i32
	.quad	_L_1+7394
	.quad	_Plus__Plus_var_u8_i32
	.quad	_L_1+7413
	.quad	_Plus__uPlus_var_u8_i32
	.quad	_L_1+7429
	.quad	_Plus__Plus_param_u8_i16
	.quad	_L_1+7446
	.quad	_Plus__uPlus_param_u8_i16
	.quad	_L_1+7464
	.quad	_Plus__Plus_var_u8_i16
	.quad	_L_1+7483
	.quad	_Plus__uPlus_var_u8_i16
	.quad	_L_1+7499
	.quad	_Plus__Plus_param_u8_L
	.quad	_L_1+7516
	.quad	_Plus__uPlus_param_u8_L
	.quad	_L_1+7532
	.quad	_Plus__Plus_var_u8_L
	.quad	_L_1+7549
	.quad	_Plus__uPlus_var_u8_L
	.quad	_L_1+7563
	.quad	_Plus__Plus_param_u8_u64
	.quad	_L_1+7578
	.quad	_Plus__uPlus_param_u8_u64
	.quad	_L_1+7596
	.quad	_Plus__Plus_var_u8_u64
	.quad	_L_1+7615
	.quad	_Plus__uPlus_var_u8_u64
	.quad	_L_1+7631
	.quad	_Plus__Plus_param_u8_u16
	.quad	_L_1+7648
	.quad	_Plus__uPlus_param_u8_u16
	.quad	_L_1+7666
	.quad	_Plus__Plus_var_u8_u16
	.quad	_L_1+7685
	.quad	_Plus__uPlus_var_u8_u16
	.quad	_L_1+7701
	.quad	_Plus__Plus_param_u8_C
	.quad	_L_1+7718
	.quad	_Plus__uPlus_param_u8_C
	.quad	_L_1+7734
	.quad	_Plus__Plus_var_u8_C
	.quad	_L_1+7751
	.quad	_Plus__uPlus_var_u8_C
	.quad	_L_1+7765
	.quad	_Plus__Plus_param_u8_u32
	.quad	_L_1+7780
	.quad	_Plus__uPlus_param_u8_u32
	.quad	_L_1+7798
	.quad	_Plus__Plus_var_u8_u32
	.quad	_L_1+7817
	.quad	_Plus__uPlus_var_u8_u32
	.quad	_L_1+7833
	.quad	_Plus__Plus_param_u8_i8
	.quad	_L_1+7850
	.quad	_Plus__uPlus_param_u8_i8
	.quad	_L_1+7867
	.quad	_Plus__Plus_var_u8_i8
	.quad	_L_1+7885
	.quad	_Plus__uPlus_var_u8_i8
	.quad	_L_1+7900
	.quad	_Plus__Plus_param_u8_I
	.quad	_L_1+7916
	.quad	_Plus__uPlus_param_u8_I
	.quad	_L_1+7932
	.quad	_Plus__Plus_var_u8_I
	.quad	_L_1+7949
	.quad	_Plus__uPlus_var_u8_I
	.quad	_L_1+7963
	.quad	_Plus__Plus_param_u8_u8
	.quad	_L_1+7978
	.quad	_Plus__uPlus_param_u8_u8
	.quad	_L_1+7995
	.quad	_Plus__Plus_var_u8_u8
	.quad	_L_1+8013
	.quad	_Plus__uPlus_var_u8_u8
	.quad	_L_1+8028
	.quad	_Plus__Plus_param_u8_LC
	.quad	_L_1+8044
	.quad	_Plus__uPlus_param_u8_LC
	.quad	_L_1+8061
	.quad	_Plus__Plus_var_u8_LC
	.quad	_L_1+8079
	.quad	_Plus__uPlus_var_u8_LC
	.quad	_L_1+8094
	.quad	_Plus__Plus_param_u8_i64
	.quad	_L_1+8110
	.quad	_Plus__uPlus_param_u8_i64
	.quad	_L_1+8128
	.quad	_Plus__Plus_var_u8_i64
	.quad	_L_1+8147
	.quad	_Plus__uPlus_var_u8_i64
	.quad	_L_1+8163
	.quad	_Plus__Plus_param_f32_f32
	.quad	_L_1+8180
	.quad	_Plus__Plus_var_f32_f32
	.quad	_L_1+8199
	.quad	_Plus__Plus_param_LC_i32
	.quad	_L_1+8216
	.quad	_Plus__uPlus_param_LC_i32
	.quad	_L_1+8234
	.quad	_Plus__Plus_var_LC_i32
	.quad	_L_1+8253
	.quad	_Plus__uPlus_var_LC_i32
	.quad	_L_1+8269
	.quad	_Plus__Plus_param_LC_i16
	.quad	_L_1+8286
	.quad	_Plus__uPlus_param_LC_i16
	.quad	_L_1+8304
	.quad	_Plus__Plus_var_LC_i16
	.quad	_L_1+8323
	.quad	_Plus__uPlus_var_LC_i16
	.quad	_L_1+8339
	.quad	_Plus__Plus_param_LC_L
	.quad	_L_1+8356
	.quad	_Plus__uPlus_param_LC_L
	.quad	_L_1+8372
	.quad	_Plus__Plus_var_LC_L
	.quad	_L_1+8389
	.quad	_Plus__uPlus_var_LC_L
	.quad	_L_1+8403
	.quad	_Plus__Plus_param_LC_u64
	.quad	_L_1+8418
	.quad	_Plus__uPlus_param_LC_u64
	.quad	_L_1+8436
	.quad	_Plus__Plus_var_LC_u64
	.quad	_L_1+8455
	.quad	_Plus__uPlus_var_LC_u64
	.quad	_L_1+8471
	.quad	_Plus__Plus_param_LC_u16
	.quad	_L_1+8488
	.quad	_Plus__uPlus_param_LC_u16
	.quad	_L_1+8506
	.quad	_Plus__Plus_var_LC_u16
	.quad	_L_1+8525
	.quad	_Plus__uPlus_var_LC_u16
	.quad	_L_1+8541
	.quad	_Plus__Plus_param_LC_C
	.quad	_L_1+8558
	.quad	_Plus__uPlus_param_LC_C
	.quad	_L_1+8574
	.quad	_Plus__Plus_var_LC_C
	.quad	_L_1+8591
	.quad	_Plus__uPlus_var_LC_C
	.quad	_L_1+8605
	.quad	_Plus__Plus_param_LC_u32
	.quad	_L_1+8620
	.quad	_Plus__uPlus_param_LC_u32
	.quad	_L_1+8638
	.quad	_Plus__Plus_var_LC_u32
	.quad	_L_1+8657
	.quad	_Plus__uPlus_var_LC_u32
	.quad	_L_1+8673
	.quad	_Plus__Plus_param_LC_i8
	.quad	_L_1+8690
	.quad	_Plus__uPlus_param_LC_i8
	.quad	_L_1+8707
	.quad	_Plus__Plus_var_LC_i8
	.quad	_L_1+8725
	.quad	_Plus__uPlus_var_LC_i8
	.quad	_L_1+8740
	.quad	_Plus__Plus_param_LC_I
	.quad	_L_1+8756
	.quad	_Plus__uPlus_param_LC_I
	.quad	_L_1+8772
	.quad	_Plus__Plus_var_LC_I
	.quad	_L_1+8789
	.quad	_Plus__uPlus_var_LC_I
	.quad	_L_1+8803
	.quad	_Plus__Plus_param_LC_u8
	.quad	_L_1+8818
	.quad	_Plus__uPlus_param_LC_u8
	.quad	_L_1+8835
	.quad	_Plus__Plus_var_LC_u8
	.quad	_L_1+8853
	.quad	_Plus__uPlus_var_LC_u8
	.quad	_L_1+8868
	.quad	_Plus__Plus_param_LC_LC
	.quad	_L_1+8884
	.quad	_Plus__uPlus_param_LC_LC
	.quad	_L_1+8901
	.quad	_Plus__Plus_var_LC_LC
	.quad	_L_1+8919
	.quad	_Plus__uPlus_var_LC_LC
	.quad	_L_1+8934
	.quad	_Plus__Plus_param_LC_i64
	.quad	_L_1+8950
	.quad	_Plus__uPlus_param_LC_i64
	.quad	_L_1+8968
	.quad	_Plus__Plus_var_LC_i64
	.quad	_L_1+8987
	.quad	_Plus__uPlus_var_LC_i64
	.quad	_L_1+9003
	.quad	_Plus__Plus_param_i64_i32
	.quad	_L_1+9020
	.quad	_Plus__uPlus_param_i64_i32
	.quad	_L_1+9039
	.quad	_Plus__Plus_var_i64_i32
	.quad	_L_1+9059
	.quad	_Plus__uPlus_var_i64_i32
	.quad	_L_1+9076
	.quad	_Plus__Plus_param_i64_i16
	.quad	_L_1+9094
	.quad	_Plus__uPlus_param_i64_i16
	.quad	_L_1+9113
	.quad	_Plus__Plus_var_i64_i16
	.quad	_L_1+9133
	.quad	_Plus__uPlus_var_i64_i16
	.quad	_L_1+9150
	.quad	_Plus__Plus_param_i64_L
	.quad	_L_1+9168
	.quad	_Plus__uPlus_param_i64_L
	.quad	_L_1+9185
	.quad	_Plus__Plus_var_i64_L
	.quad	_L_1+9203
	.quad	_Plus__uPlus_var_i64_L
	.quad	_L_1+9218
	.quad	_Plus__Plus_param_i64_u64
	.quad	_L_1+9234
	.quad	_Plus__uPlus_param_i64_u64
	.quad	_L_1+9253
	.quad	_Plus__Plus_var_i64_u64
	.quad	_L_1+9273
	.quad	_Plus__uPlus_var_i64_u64
	.quad	_L_1+9290
	.quad	_Plus__Plus_param_i64_u16
	.quad	_L_1+9308
	.quad	_Plus__uPlus_param_i64_u16
	.quad	_L_1+9327
	.quad	_Plus__Plus_var_i64_u16
	.quad	_L_1+9347
	.quad	_Plus__uPlus_var_i64_u16
	.quad	_L_1+9364
	.quad	_Plus__Plus_param_i64_C
	.quad	_L_1+9382
	.quad	_Plus__uPlus_param_i64_C
	.quad	_L_1+9399
	.quad	_Plus__Plus_var_i64_C
	.quad	_L_1+9417
	.quad	_Plus__uPlus_var_i64_C
	.quad	_L_1+9432
	.quad	_Plus__Plus_param_i64_u32
	.quad	_L_1+9448
	.quad	_Plus__uPlus_param_i64_u32
	.quad	_L_1+9467
	.quad	_Plus__Plus_var_i64_u32
	.quad	_L_1+9487
	.quad	_Plus__uPlus_var_i64_u32
	.quad	_L_1+9504
	.quad	_Plus__Plus_param_i64_i8
	.quad	_L_1+9522
	.quad	_Plus__uPlus_param_i64_i8
	.quad	_L_1+9540
	.quad	_Plus__Plus_var_i64_i8
	.quad	_L_1+9559
	.quad	_Plus__uPlus_var_i64_i8
	.quad	_L_1+9575
	.quad	_Plus__Plus_param_i64_I
	.quad	_L_1+9592
	.quad	_Plus__uPlus_param_i64_I
	.quad	_L_1+9609
	.quad	_Plus__Plus_var_i64_I
	.quad	_L_1+9627
	.quad	_Plus__uPlus_var_i64_I
	.quad	_L_1+9642
	.quad	_Plus__Plus_param_i64_u8
	.quad	_L_1+9658
	.quad	_Plus__uPlus_param_i64_u8
	.quad	_L_1+9676
	.quad	_Plus__Plus_var_i64_u8
	.quad	_L_1+9695
	.quad	_Plus__uPlus_var_i64_u8
	.quad	_L_1+9711
	.quad	_Plus__Plus_param_i64_LC
	.quad	_L_1+9728
	.quad	_Plus__uPlus_param_i64_LC
	.quad	_L_1+9746
	.quad	_Plus__Plus_var_i64_LC
	.quad	_L_1+9765
	.quad	_Plus__uPlus_var_i64_LC
	.quad	_L_1+9781
	.quad	_Plus__Plus_param_i64_i64
	.quad	_L_1+9798
	.quad	_Plus__uPlus_param_i64_i64
	.quad	_L_1+9817
	.quad	_Plus__Plus_var_i64_i64
	.quad	_L_1+9837
	.quad	_Plus__uPlus_var_i64_i64
	.quad	_L_1+9854
	.space 8
	.ascii "../AMD64_DARWIN/Plus.m3"
	.space 1
	.data
	.align 5
_MM_Plus:
	.quad	_L_1+19176
	.space 32
	.quad	_L_1+9872
	.space 24
	.quad	_MM_Plus+208
	.space 8
	.quad	_Plus_M3
	.quad	3
	.quad	145
	.quad	146
	.long	1125328355
	.byte	-107
	.space 3
	.quad	150
	.byte	-105
	.space 3
	.long	152
	.quad	153
	.word	154
	.space 6
	.quad	155
	.long	618475291
	.long	1080263942
	.quad	158
	.word	159
	.space 2
	.long	160
	.space 24
	.quad	_Plus_I3
	.quad	_MM_Plus+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Plus+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Plus+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Plus+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
