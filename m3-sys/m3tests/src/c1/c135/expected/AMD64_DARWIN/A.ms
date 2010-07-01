	.text
.globl _A__ret_var_i64
	.private_extern _A__ret_var_i64
_A__ret_var_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rax
	leave
	ret
.globl _A__ret_const_i64
	.private_extern _A__ret_const_i64
_A__ret_const_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$3, %eax
	leave
	ret
.globl _A__ret_param_u64_u64
	.private_extern _A__ret_param_u64_u64
_A__ret_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u64_u16
	.private_extern _A__ret_param_u64_u16
_A__ret_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u64_i64
	.private_extern _A__ret_param_u64_i64
_A__ret_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u64_i16
	.private_extern _A__ret_param_u64_i16
_A__ret_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u64_u32
	.private_extern _A__ret_param_u64_u32
_A__ret_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_u64_u8
	.private_extern _A__ret_param_u64_u8
_A__ret_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u64_i32
	.private_extern _A__ret_param_u64_i32
_A__ret_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u64_i8
	.private_extern _A__ret_param_u64_i8
_A__ret_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_u64_u64
	.private_extern _A__add_var_u64_u64
_A__add_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_u64
	.private_extern _A__add_param_u64_u64
_A__add_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__add_var_u64_u16
	.private_extern _A__add_var_u64_u16
_A__add_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_u16
	.private_extern _A__add_param_u64_u16
_A__add_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_u64_i64
	.private_extern _A__add_var_u64_i64
_A__add_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_i64
	.private_extern _A__add_param_u64_i64
_A__add_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__add_var_u64_i16
	.private_extern _A__add_var_u64_i16
_A__add_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_i16
	.private_extern _A__add_param_u64_i16
_A__add_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_u64_u32
	.private_extern _A__add_var_u64_u32
_A__add_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_u32
	.private_extern _A__add_param_u64_u32
_A__add_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_u64_u8
	.private_extern _A__add_var_u64_u8
_A__add_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_u8
	.private_extern _A__add_param_u64_u8
_A__add_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_u64_i32
	.private_extern _A__add_var_u64_i32
_A__add_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_i32
	.private_extern _A__add_param_u64_i32
_A__add_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_u64_i8
	.private_extern _A__add_var_u64_i8
_A__add_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u64_i8
	.private_extern _A__add_param_u64_i8
_A__add_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u64_u64
	.private_extern _A__sub_var_u64_u64
_A__sub_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_u64_u64
	.private_extern _A__sub_param_u64_u64
_A__sub_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_u64_u16
	.private_extern _A__sub_var_u64_u16
_A__sub_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_u16
	.private_extern _A__sub_param_u64_u16
_A__sub_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_u64_i64
	.private_extern _A__sub_var_u64_i64
_A__sub_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_i64
	.private_extern _A__sub_param_u64_i64
_A__sub_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_u64_i16
	.private_extern _A__sub_var_u64_i16
_A__sub_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_i16
	.private_extern _A__sub_param_u64_i16
_A__sub_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_u64_u32
	.private_extern _A__sub_var_u64_u32
_A__sub_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_u32
	.private_extern _A__sub_param_u64_u32
_A__sub_param_u64_u32:
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
.globl _A__sub_var_u64_u8
	.private_extern _A__sub_var_u64_u8
_A__sub_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_u8
	.private_extern _A__sub_param_u64_u8
_A__sub_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_u64_i32
	.private_extern _A__sub_var_u64_i32
_A__sub_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_i32
	.private_extern _A__sub_param_u64_i32
_A__sub_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_u64_i8
	.private_extern _A__sub_var_u64_i8
_A__sub_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u64_i8
	.private_extern _A__sub_param_u64_i8
_A__sub_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_u64
	.private_extern _A__and_var_u64_u64
_A__and_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	leave
	ret
.globl _A__and_param_u64_u64
	.private_extern _A__and_param_u64_u64
_A__and_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_u16
	.private_extern _A__and_var_u64_u16
_A__and_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_u16
	.private_extern _A__and_param_u64_u16
_A__and_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_i64
	.private_extern _A__and_var_u64_i64
_A__and_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_i64
	.private_extern _A__and_param_u64_i64
_A__and_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_i16
	.private_extern _A__and_var_u64_i16
_A__and_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_i16
	.private_extern _A__and_param_u64_i16
_A__and_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_u32
	.private_extern _A__and_var_u64_u32
_A__and_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_u32
	.private_extern _A__and_param_u64_u32
_A__and_param_u64_u32:
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
.globl _A__and_var_u64_u8
	.private_extern _A__and_var_u64_u8
_A__and_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_u8
	.private_extern _A__and_param_u64_u8
_A__and_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_i32
	.private_extern _A__and_var_u64_i32
_A__and_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_i32
	.private_extern _A__and_param_u64_i32
_A__and_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u64_i8
	.private_extern _A__and_var_u64_i8
_A__and_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u64_i8
	.private_extern _A__and_param_u64_i8
_A__and_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_u64
	.private_extern _A__or_var_u64_u64
_A__or_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	leave
	ret
.globl _A__or_param_u64_u64
	.private_extern _A__or_param_u64_u64
_A__or_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_u16
	.private_extern _A__or_var_u64_u16
_A__or_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_u16
	.private_extern _A__or_param_u64_u16
_A__or_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_i64
	.private_extern _A__or_var_u64_i64
_A__or_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_i64
	.private_extern _A__or_param_u64_i64
_A__or_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_i16
	.private_extern _A__or_var_u64_i16
_A__or_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_i16
	.private_extern _A__or_param_u64_i16
_A__or_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_u32
	.private_extern _A__or_var_u64_u32
_A__or_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_u32
	.private_extern _A__or_param_u64_u32
_A__or_param_u64_u32:
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
.globl _A__or_var_u64_u8
	.private_extern _A__or_var_u64_u8
_A__or_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_u8
	.private_extern _A__or_param_u64_u8
_A__or_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_i32
	.private_extern _A__or_var_u64_i32
_A__or_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_i32
	.private_extern _A__or_param_u64_i32
_A__or_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u64_i8
	.private_extern _A__or_var_u64_i8
_A__or_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u64_i8
	.private_extern _A__or_param_u64_i8
_A__or_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_u64
	.private_extern _A__xor_var_u64_u64
_A__xor_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_u64_u64
	.private_extern _A__xor_param_u64_u64
_A__xor_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_u16
	.private_extern _A__xor_var_u64_u16
_A__xor_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_u16
	.private_extern _A__xor_param_u64_u16
_A__xor_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_i64
	.private_extern _A__xor_var_u64_i64
_A__xor_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_i64
	.private_extern _A__xor_param_u64_i64
_A__xor_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_i16
	.private_extern _A__xor_var_u64_i16
_A__xor_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_i16
	.private_extern _A__xor_param_u64_i16
_A__xor_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_u32
	.private_extern _A__xor_var_u64_u32
_A__xor_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_u32
	.private_extern _A__xor_param_u64_u32
_A__xor_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_u8
	.private_extern _A__xor_var_u64_u8
_A__xor_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_u8
	.private_extern _A__xor_param_u64_u8
_A__xor_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_i32
	.private_extern _A__xor_var_u64_i32
_A__xor_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_i32
	.private_extern _A__xor_param_u64_i32
_A__xor_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u64_i8
	.private_extern _A__xor_var_u64_i8
_A__xor_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u64_i8
	.private_extern _A__xor_param_u64_i8
_A__xor_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u64_u64
	.private_extern _A__mult_var_u64_u64
_A__mult_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_u64
	.private_extern _A__mult_param_u64_u64
_A__mult_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_u16
	.private_extern _A__mult_var_u64_u16
_A__mult_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_u16
	.private_extern _A__mult_param_u64_u16
_A__mult_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_i64
	.private_extern _A__mult_var_u64_i64
_A__mult_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_i64
	.private_extern _A__mult_param_u64_i64
_A__mult_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_i16
	.private_extern _A__mult_var_u64_i16
_A__mult_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_i16
	.private_extern _A__mult_param_u64_i16
_A__mult_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_u32
	.private_extern _A__mult_var_u64_u32
_A__mult_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_u32
	.private_extern _A__mult_param_u64_u32
_A__mult_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_u8
	.private_extern _A__mult_var_u64_u8
_A__mult_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_u8
	.private_extern _A__mult_param_u64_u8
_A__mult_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_i32
	.private_extern _A__mult_var_u64_i32
_A__mult_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_i32
	.private_extern _A__mult_param_u64_i32
_A__mult_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u64_i8
	.private_extern _A__mult_var_u64_i8
_A__mult_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u64_i8
	.private_extern _A__mult_param_u64_i8
_A__mult_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__div_var_u64_u64
	.private_extern _A__div_var_u64_u64
_A__div_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_u64
	.private_extern _A__div_param_u64_u64
_A__div_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_u16
	.private_extern _A__div_var_u64_u16
_A__div_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_u16
	.private_extern _A__div_param_u64_u16
_A__div_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_i64
	.private_extern _A__div_var_u64_i64
_A__div_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_i64
	.private_extern _A__div_param_u64_i64
_A__div_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_i16
	.private_extern _A__div_var_u64_i16
_A__div_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_i16
	.private_extern _A__div_param_u64_i16
_A__div_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_u32
	.private_extern _A__div_var_u64_u32
_A__div_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_u32
	.private_extern _A__div_param_u64_u32
_A__div_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_u8
	.private_extern _A__div_var_u64_u8
_A__div_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_u8
	.private_extern _A__div_param_u64_u8
_A__div_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_i32
	.private_extern _A__div_var_u64_i32
_A__div_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_i32
	.private_extern _A__div_param_u64_i32
_A__div_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u64_i8
	.private_extern _A__div_var_u64_i8
_A__div_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u64_i8
	.private_extern _A__div_param_u64_i8
_A__div_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__mod_var_u64_u64
	.private_extern _A__mod_var_u64_u64
_A__mod_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_u64
	.private_extern _A__mod_param_u64_u64
_A__mod_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_u16
	.private_extern _A__mod_var_u64_u16
_A__mod_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_u16
	.private_extern _A__mod_param_u64_u16
_A__mod_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_i64
	.private_extern _A__mod_var_u64_i64
_A__mod_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_i64
	.private_extern _A__mod_param_u64_i64
_A__mod_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_i16
	.private_extern _A__mod_var_u64_i16
_A__mod_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_i16
	.private_extern _A__mod_param_u64_i16
_A__mod_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_u32
	.private_extern _A__mod_var_u64_u32
_A__mod_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_u32
	.private_extern _A__mod_param_u64_u32
_A__mod_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_u8
	.private_extern _A__mod_var_u64_u8
_A__mod_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_u8
	.private_extern _A__mod_param_u64_u8
_A__mod_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_i32
	.private_extern _A__mod_var_u64_i32
_A__mod_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_i32
	.private_extern _A__mod_param_u64_i32
_A__mod_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u64_i8
	.private_extern _A__mod_var_u64_i8
_A__mod_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u64_i8
	.private_extern _A__mod_param_u64_i8
_A__mod_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__ret_f32
	.private_extern _A__ret_f32
_A__ret_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	112+_MM_A(%rip), %xmm0
	leave
	ret
.globl _A__add_var_f32_f32
	.private_extern _A__add_var_f32_f32
_A__add_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	112+_MM_A(%rip), %xmm0
	addss	%xmm0, %xmm0
	leave
	ret
.globl _A__add_param_f32_f32
	.private_extern _A__add_param_f32_f32
_A__add_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm1
	movss	-24(%rbp), %xmm0
	addss	%xmm1, %xmm0
	leave
	ret
.globl _A__sub_var_f32_f32
	.private_extern _A__sub_var_f32_f32
_A__sub_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	112+_MM_A(%rip), %xmm1
	movss	112+_MM_A(%rip), %xmm0
	movaps	%xmm1, %xmm2
	subss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	leave
	ret
.globl _A__sub_param_f32_f32
	.private_extern _A__sub_param_f32_f32
_A__sub_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm1
	movss	-24(%rbp), %xmm0
	movaps	%xmm1, %xmm2
	subss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	leave
	ret
.globl _A__mult_var_f32_f32
	.private_extern _A__mult_var_f32_f32
_A__mult_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	112+_MM_A(%rip), %xmm1
	movss	112+_MM_A(%rip), %xmm0
	mulss	%xmm1, %xmm0
	leave
	ret
.globl _A__mult_param_f32_f32
	.private_extern _A__mult_param_f32_f32
_A__mult_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm1
	movss	-24(%rbp), %xmm0
	mulss	%xmm1, %xmm0
	leave
	ret
.globl _A__ret_var_i16
	.private_extern _A__ret_var_i16
_A__ret_var_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L292
	movl	$5025, %edi
	call	__m3_fault
L292:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _A__ret_const_i16
	.private_extern _A__ret_const_i16
_A__ret_const_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$6, %eax
	leave
	ret
.globl _A__ret_param_u16_u64
	.private_extern _A__ret_param_u16_u64
_A__ret_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u16_u16
	.private_extern _A__ret_param_u16_u16
_A__ret_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u16_i64
	.private_extern _A__ret_param_u16_i64
_A__ret_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u16_i16
	.private_extern _A__ret_param_u16_i16
_A__ret_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u16_u32
	.private_extern _A__ret_param_u16_u32
_A__ret_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_u16_u8
	.private_extern _A__ret_param_u16_u8
_A__ret_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u16_i32
	.private_extern _A__ret_param_u16_i32
_A__ret_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u16_i8
	.private_extern _A__ret_param_u16_i8
_A__ret_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_u16_u64
	.private_extern _A__add_var_u16_u64
_A__add_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_u64
	.private_extern _A__add_param_u16_u64
_A__add_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_u16_u16
	.private_extern _A__add_var_u16_u16
_A__add_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_u16
	.private_extern _A__add_param_u16_u16
_A__add_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u16_i64
	.private_extern _A__add_var_u16_i64
_A__add_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_i64
	.private_extern _A__add_param_u16_i64
_A__add_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_u16_i16
	.private_extern _A__add_var_u16_i16
_A__add_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_i16
	.private_extern _A__add_param_u16_i16
_A__add_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u16_u32
	.private_extern _A__add_var_u16_u32
_A__add_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_u32
	.private_extern _A__add_param_u16_u32
_A__add_param_u16_u32:
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
.globl _A__add_var_u16_u8
	.private_extern _A__add_var_u16_u8
_A__add_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_u8
	.private_extern _A__add_param_u16_u8
_A__add_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u16_i32
	.private_extern _A__add_var_u16_i32
_A__add_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_i32
	.private_extern _A__add_param_u16_i32
_A__add_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u16_i8
	.private_extern _A__add_var_u16_i8
_A__add_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u16_i8
	.private_extern _A__add_param_u16_i8
_A__add_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__sub_var_u16_u64
	.private_extern _A__sub_var_u16_u64
_A__sub_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_u64
	.private_extern _A__sub_param_u16_u64
_A__sub_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u16_u16
	.private_extern _A__sub_var_u16_u16
_A__sub_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_u16_u16
	.private_extern _A__sub_param_u16_u16
_A__sub_param_u16_u16:
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
.globl _A__sub_var_u16_i64
	.private_extern _A__sub_var_u16_i64
_A__sub_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_i64
	.private_extern _A__sub_param_u16_i64
_A__sub_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u16_i16
	.private_extern _A__sub_var_u16_i16
_A__sub_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_i16
	.private_extern _A__sub_param_u16_i16
_A__sub_param_u16_i16:
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
.globl _A__sub_var_u16_u32
	.private_extern _A__sub_var_u16_u32
_A__sub_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_u32
	.private_extern _A__sub_param_u16_u32
_A__sub_param_u16_u32:
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
.globl _A__sub_var_u16_u8
	.private_extern _A__sub_var_u16_u8
_A__sub_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_u8
	.private_extern _A__sub_param_u16_u8
_A__sub_param_u16_u8:
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
.globl _A__sub_var_u16_i32
	.private_extern _A__sub_var_u16_i32
_A__sub_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_i32
	.private_extern _A__sub_param_u16_i32
_A__sub_param_u16_i32:
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
.globl _A__sub_var_u16_i8
	.private_extern _A__sub_var_u16_i8
_A__sub_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u16_i8
	.private_extern _A__sub_param_u16_i8
_A__sub_param_u16_i8:
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
.globl _A__and_var_u16_u64
	.private_extern _A__and_var_u16_u64
_A__and_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_u64
	.private_extern _A__and_param_u16_u64
_A__and_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u16_u16
	.private_extern _A__and_var_u16_u16
_A__and_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _A__and_param_u16_u16
	.private_extern _A__and_param_u16_u16
_A__and_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u16_i64
	.private_extern _A__and_var_u16_i64
_A__and_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_i64
	.private_extern _A__and_param_u16_i64
_A__and_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u16_i16
	.private_extern _A__and_var_u16_i16
_A__and_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_i16
	.private_extern _A__and_param_u16_i16
_A__and_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u16_u32
	.private_extern _A__and_var_u16_u32
_A__and_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_u32
	.private_extern _A__and_param_u16_u32
_A__and_param_u16_u32:
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
.globl _A__and_var_u16_u8
	.private_extern _A__and_var_u16_u8
_A__and_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_u8
	.private_extern _A__and_param_u16_u8
_A__and_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u16_i32
	.private_extern _A__and_var_u16_i32
_A__and_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_i32
	.private_extern _A__and_param_u16_i32
_A__and_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u16_i8
	.private_extern _A__and_var_u16_i8
_A__and_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u16_i8
	.private_extern _A__and_param_u16_i8
_A__and_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_u64
	.private_extern _A__or_var_u16_u64
_A__or_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_u64
	.private_extern _A__or_param_u16_u64
_A__or_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_u16
	.private_extern _A__or_var_u16_u16
_A__or_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _A__or_param_u16_u16
	.private_extern _A__or_param_u16_u16
_A__or_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_i64
	.private_extern _A__or_var_u16_i64
_A__or_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_i64
	.private_extern _A__or_param_u16_i64
_A__or_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_i16
	.private_extern _A__or_var_u16_i16
_A__or_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_i16
	.private_extern _A__or_param_u16_i16
_A__or_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_u32
	.private_extern _A__or_var_u16_u32
_A__or_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_u32
	.private_extern _A__or_param_u16_u32
_A__or_param_u16_u32:
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
.globl _A__or_var_u16_u8
	.private_extern _A__or_var_u16_u8
_A__or_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_u8
	.private_extern _A__or_param_u16_u8
_A__or_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_i32
	.private_extern _A__or_var_u16_i32
_A__or_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_i32
	.private_extern _A__or_param_u16_i32
_A__or_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u16_i8
	.private_extern _A__or_var_u16_i8
_A__or_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u16_i8
	.private_extern _A__or_param_u16_i8
_A__or_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_u64
	.private_extern _A__xor_var_u16_u64
_A__xor_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_u64
	.private_extern _A__xor_param_u16_u64
_A__xor_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_u16
	.private_extern _A__xor_var_u16_u16
_A__xor_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_u16_u16
	.private_extern _A__xor_param_u16_u16
_A__xor_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_i64
	.private_extern _A__xor_var_u16_i64
_A__xor_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_i64
	.private_extern _A__xor_param_u16_i64
_A__xor_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_i16
	.private_extern _A__xor_var_u16_i16
_A__xor_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_i16
	.private_extern _A__xor_param_u16_i16
_A__xor_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_u32
	.private_extern _A__xor_var_u16_u32
_A__xor_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_u32
	.private_extern _A__xor_param_u16_u32
_A__xor_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_u8
	.private_extern _A__xor_var_u16_u8
_A__xor_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_u8
	.private_extern _A__xor_param_u16_u8
_A__xor_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_i32
	.private_extern _A__xor_var_u16_i32
_A__xor_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_i32
	.private_extern _A__xor_param_u16_i32
_A__xor_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u16_i8
	.private_extern _A__xor_var_u16_i8
_A__xor_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u16_i8
	.private_extern _A__xor_param_u16_i8
_A__xor_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u16_u64
	.private_extern _A__mult_var_u16_u64
_A__mult_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_u64
	.private_extern _A__mult_param_u16_u64
_A__mult_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u16_u16
	.private_extern _A__mult_var_u16_u16
_A__mult_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_u16
	.private_extern _A__mult_param_u16_u16
_A__mult_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u16_i64
	.private_extern _A__mult_var_u16_i64
_A__mult_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_i64
	.private_extern _A__mult_param_u16_i64
_A__mult_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u16_i16
	.private_extern _A__mult_var_u16_i16
_A__mult_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_i16
	.private_extern _A__mult_param_u16_i16
_A__mult_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u16_u32
	.private_extern _A__mult_var_u16_u32
_A__mult_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_u32
	.private_extern _A__mult_param_u16_u32
_A__mult_param_u16_u32:
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
.globl _A__mult_var_u16_u8
	.private_extern _A__mult_var_u16_u8
_A__mult_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_u8
	.private_extern _A__mult_param_u16_u8
_A__mult_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u16_i32
	.private_extern _A__mult_var_u16_i32
_A__mult_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_i32
	.private_extern _A__mult_param_u16_i32
_A__mult_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u16_i8
	.private_extern _A__mult_var_u16_i8
_A__mult_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u16_i8
	.private_extern _A__mult_param_u16_i8
_A__mult_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__div_var_u16_u64
	.private_extern _A__div_var_u16_u64
_A__div_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_u64
	.private_extern _A__div_param_u16_u64
_A__div_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_u16
	.private_extern _A__div_var_u16_u16
_A__div_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_u16
	.private_extern _A__div_param_u16_u16
_A__div_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_i64
	.private_extern _A__div_var_u16_i64
_A__div_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_i64
	.private_extern _A__div_param_u16_i64
_A__div_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_i16
	.private_extern _A__div_var_u16_i16
_A__div_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_i16
	.private_extern _A__div_param_u16_i16
_A__div_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_u32
	.private_extern _A__div_var_u16_u32
_A__div_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_u32
	.private_extern _A__div_param_u16_u32
_A__div_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_u8
	.private_extern _A__div_var_u16_u8
_A__div_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_u8
	.private_extern _A__div_param_u16_u8
_A__div_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movsbq	-19(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_i32
	.private_extern _A__div_var_u16_i32
_A__div_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_i32
	.private_extern _A__div_param_u16_i32
_A__div_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u16_i8
	.private_extern _A__div_var_u16_i8
_A__div_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u16_i8
	.private_extern _A__div_param_u16_i8
_A__div_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movzbl	-19(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__mod_var_u16_u64
	.private_extern _A__mod_var_u16_u64
_A__mod_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_u64
	.private_extern _A__mod_param_u16_u64
_A__mod_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_u16
	.private_extern _A__mod_var_u16_u16
_A__mod_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_u16
	.private_extern _A__mod_param_u16_u16
_A__mod_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_i64
	.private_extern _A__mod_var_u16_i64
_A__mod_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_i64
	.private_extern _A__mod_param_u16_i64
_A__mod_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_i16
	.private_extern _A__mod_var_u16_i16
_A__mod_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_i16
	.private_extern _A__mod_param_u16_i16
_A__mod_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_u32
	.private_extern _A__mod_var_u16_u32
_A__mod_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_u32
	.private_extern _A__mod_param_u16_u32
_A__mod_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_u8
	.private_extern _A__mod_var_u16_u8
_A__mod_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_u8
	.private_extern _A__mod_param_u16_u8
_A__mod_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movsbq	-19(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_i32
	.private_extern _A__mod_var_u16_i32
_A__mod_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_i32
	.private_extern _A__mod_param_u16_i32
_A__mod_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u16_i8
	.private_extern _A__mod_var_u16_i8
_A__mod_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u16_i8
	.private_extern _A__mod_param_u16_i8
_A__mod_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movzbl	-19(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__ret_var_u64
	.private_extern _A__ret_var_u64
_A__ret_var_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	leave
	ret
.globl _A__ret_const_u64
	.private_extern _A__ret_const_u64
_A__ret_const_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$9, %eax
	leave
	ret
.globl _A__ret_param_i64_u64
	.private_extern _A__ret_param_i64_u64
_A__ret_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i64_u16
	.private_extern _A__ret_param_i64_u16
_A__ret_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i64_i64
	.private_extern _A__ret_param_i64_i64
_A__ret_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i64_i16
	.private_extern _A__ret_param_i64_i16
_A__ret_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i64_u32
	.private_extern _A__ret_param_i64_u32
_A__ret_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_i64_u8
	.private_extern _A__ret_param_i64_u8
_A__ret_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i64_i32
	.private_extern _A__ret_param_i64_i32
_A__ret_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i64_i8
	.private_extern _A__ret_param_i64_i8
_A__ret_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_i64_u64
	.private_extern _A__add_var_i64_u64
_A__add_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_u64
	.private_extern _A__add_param_i64_u64
_A__add_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__add_var_i64_u16
	.private_extern _A__add_var_i64_u16
_A__add_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_u16
	.private_extern _A__add_param_i64_u16
_A__add_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_i64_i64
	.private_extern _A__add_var_i64_i64
_A__add_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_i64
	.private_extern _A__add_param_i64_i64
_A__add_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__add_var_i64_i16
	.private_extern _A__add_var_i64_i16
_A__add_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_i16
	.private_extern _A__add_param_i64_i16
_A__add_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_i64_u32
	.private_extern _A__add_var_i64_u32
_A__add_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_u32
	.private_extern _A__add_param_i64_u32
_A__add_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_i64_u8
	.private_extern _A__add_var_i64_u8
_A__add_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_u8
	.private_extern _A__add_param_i64_u8
_A__add_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_i64_i32
	.private_extern _A__add_var_i64_i32
_A__add_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_i32
	.private_extern _A__add_param_i64_i32
_A__add_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_var_i64_i8
	.private_extern _A__add_var_i64_i8
_A__add_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i64_i8
	.private_extern _A__add_param_i64_i8
_A__add_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i64_u64
	.private_extern _A__sub_var_i64_u64
_A__sub_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_u64
	.private_extern _A__sub_param_i64_u64
_A__sub_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_i64_u16
	.private_extern _A__sub_var_i64_u16
_A__sub_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_u16
	.private_extern _A__sub_param_i64_u16
_A__sub_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_i64_i64
	.private_extern _A__sub_var_i64_i64
_A__sub_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_i64_i64
	.private_extern _A__sub_param_i64_i64
_A__sub_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_i64_i16
	.private_extern _A__sub_var_i64_i16
_A__sub_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_i16
	.private_extern _A__sub_param_i64_i16
_A__sub_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_i64_u32
	.private_extern _A__sub_var_i64_u32
_A__sub_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_u32
	.private_extern _A__sub_param_i64_u32
_A__sub_param_i64_u32:
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
.globl _A__sub_var_i64_u8
	.private_extern _A__sub_var_i64_u8
_A__sub_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_u8
	.private_extern _A__sub_param_i64_u8
_A__sub_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_i64_i32
	.private_extern _A__sub_var_i64_i32
_A__sub_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_i32
	.private_extern _A__sub_param_i64_i32
_A__sub_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_var_i64_i8
	.private_extern _A__sub_var_i64_i8
_A__sub_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i64_i8
	.private_extern _A__sub_param_i64_i8
_A__sub_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_u64
	.private_extern _A__and_var_i64_u64
_A__and_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_u64
	.private_extern _A__and_param_i64_u64
_A__and_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_u16
	.private_extern _A__and_var_i64_u16
_A__and_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_u16
	.private_extern _A__and_param_i64_u16
_A__and_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_i64
	.private_extern _A__and_var_i64_i64
_A__and_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rax
	leave
	ret
.globl _A__and_param_i64_i64
	.private_extern _A__and_param_i64_i64
_A__and_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_i16
	.private_extern _A__and_var_i64_i16
_A__and_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_i16
	.private_extern _A__and_param_i64_i16
_A__and_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_u32
	.private_extern _A__and_var_i64_u32
_A__and_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_u32
	.private_extern _A__and_param_i64_u32
_A__and_param_i64_u32:
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
.globl _A__and_var_i64_u8
	.private_extern _A__and_var_i64_u8
_A__and_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_u8
	.private_extern _A__and_param_i64_u8
_A__and_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_i32
	.private_extern _A__and_var_i64_i32
_A__and_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_i32
	.private_extern _A__and_param_i64_i32
_A__and_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i64_i8
	.private_extern _A__and_var_i64_i8
_A__and_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i64_i8
	.private_extern _A__and_param_i64_i8
_A__and_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_u64
	.private_extern _A__or_var_i64_u64
_A__or_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_u64
	.private_extern _A__or_param_i64_u64
_A__or_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_u16
	.private_extern _A__or_var_i64_u16
_A__or_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_u16
	.private_extern _A__or_param_i64_u16
_A__or_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_i64
	.private_extern _A__or_var_i64_i64
_A__or_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rax
	leave
	ret
.globl _A__or_param_i64_i64
	.private_extern _A__or_param_i64_i64
_A__or_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_i16
	.private_extern _A__or_var_i64_i16
_A__or_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_i16
	.private_extern _A__or_param_i64_i16
_A__or_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_u32
	.private_extern _A__or_var_i64_u32
_A__or_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_u32
	.private_extern _A__or_param_i64_u32
_A__or_param_i64_u32:
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
.globl _A__or_var_i64_u8
	.private_extern _A__or_var_i64_u8
_A__or_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_u8
	.private_extern _A__or_param_i64_u8
_A__or_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_i32
	.private_extern _A__or_var_i64_i32
_A__or_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_i32
	.private_extern _A__or_param_i64_i32
_A__or_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i64_i8
	.private_extern _A__or_var_i64_i8
_A__or_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i64_i8
	.private_extern _A__or_param_i64_i8
_A__or_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_u64
	.private_extern _A__xor_var_i64_u64
_A__xor_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_u64
	.private_extern _A__xor_param_i64_u64
_A__xor_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_u16
	.private_extern _A__xor_var_i64_u16
_A__xor_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_u16
	.private_extern _A__xor_param_i64_u16
_A__xor_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_i64
	.private_extern _A__xor_var_i64_i64
_A__xor_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_i64_i64
	.private_extern _A__xor_param_i64_i64
_A__xor_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_i16
	.private_extern _A__xor_var_i64_i16
_A__xor_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_i16
	.private_extern _A__xor_param_i64_i16
_A__xor_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_u32
	.private_extern _A__xor_var_i64_u32
_A__xor_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_u32
	.private_extern _A__xor_param_i64_u32
_A__xor_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_u8
	.private_extern _A__xor_var_i64_u8
_A__xor_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_u8
	.private_extern _A__xor_param_i64_u8
_A__xor_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_i32
	.private_extern _A__xor_var_i64_i32
_A__xor_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_i32
	.private_extern _A__xor_param_i64_i32
_A__xor_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i64_i8
	.private_extern _A__xor_var_i64_i8
_A__xor_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i64_i8
	.private_extern _A__xor_param_i64_i8
_A__xor_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i64_u64
	.private_extern _A__mult_var_i64_u64
_A__mult_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_u64
	.private_extern _A__mult_param_i64_u64
_A__mult_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_u16
	.private_extern _A__mult_var_i64_u16
_A__mult_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_u16
	.private_extern _A__mult_param_i64_u16
_A__mult_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_i64
	.private_extern _A__mult_var_i64_i64
_A__mult_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_A(%rip), %rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_i64
	.private_extern _A__mult_param_i64_i64
_A__mult_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_i16
	.private_extern _A__mult_var_i64_i16
_A__mult_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_i16
	.private_extern _A__mult_param_i64_i16
_A__mult_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_u32
	.private_extern _A__mult_var_i64_u32
_A__mult_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_u32
	.private_extern _A__mult_param_i64_u32
_A__mult_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_u8
	.private_extern _A__mult_var_i64_u8
_A__mult_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_u8
	.private_extern _A__mult_param_i64_u8
_A__mult_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_i32
	.private_extern _A__mult_var_i64_i32
_A__mult_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_i32
	.private_extern _A__mult_param_i64_i32
_A__mult_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i64_i8
	.private_extern _A__mult_var_i64_i8
_A__mult_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i64_i8
	.private_extern _A__mult_param_i64_i8
_A__mult_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__div_var_i64_u64
	.private_extern _A__div_var_i64_u64
_A__div_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_u64
	.private_extern _A__div_param_i64_u64
_A__div_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_u16
	.private_extern _A__div_var_i64_u16
_A__div_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_u16
	.private_extern _A__div_param_i64_u16
_A__div_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_i64
	.private_extern _A__div_var_i64_i64
_A__div_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_i64
	.private_extern _A__div_param_i64_i64
_A__div_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_i16
	.private_extern _A__div_var_i64_i16
_A__div_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_i16
	.private_extern _A__div_param_i64_i16
_A__div_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_u32
	.private_extern _A__div_var_i64_u32
_A__div_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_u32
	.private_extern _A__div_param_i64_u32
_A__div_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_u8
	.private_extern _A__div_var_i64_u8
_A__div_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_u8
	.private_extern _A__div_param_i64_u8
_A__div_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_i32
	.private_extern _A__div_var_i64_i32
_A__div_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_i32
	.private_extern _A__div_param_i64_i32
_A__div_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i64_i8
	.private_extern _A__div_var_i64_i8
_A__div_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i64_i8
	.private_extern _A__div_param_i64_i8
_A__div_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__mod_var_i64_u64
	.private_extern _A__mod_var_i64_u64
_A__mod_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_u64
	.private_extern _A__mod_param_i64_u64
_A__mod_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_u16
	.private_extern _A__mod_var_i64_u16
_A__mod_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_u16
	.private_extern _A__mod_param_i64_u16
_A__mod_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_i64
	.private_extern _A__mod_var_i64_i64
_A__mod_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_i64
	.private_extern _A__mod_param_i64_i64
_A__mod_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_i16
	.private_extern _A__mod_var_i64_i16
_A__mod_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_i16
	.private_extern _A__mod_param_i64_i16
_A__mod_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_u32
	.private_extern _A__mod_var_i64_u32
_A__mod_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_u32
	.private_extern _A__mod_param_i64_u32
_A__mod_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_u8
	.private_extern _A__mod_var_i64_u8
_A__mod_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_u8
	.private_extern _A__mod_param_i64_u8
_A__mod_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_i32
	.private_extern _A__mod_var_i64_i32
_A__mod_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_i32
	.private_extern _A__mod_param_i64_i32
_A__mod_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i64_i8
	.private_extern _A__mod_var_i64_i8
_A__mod_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	104+_MM_A(%rip), %rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i64_i8
	.private_extern _A__mod_param_i64_i8
_A__mod_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__ret_f64
	.private_extern _A__ret_f64
_A__ret_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_A(%rip), %xmm0
	leave
	ret
.globl _A__add_var_f64_f64
	.private_extern _A__add_var_f64_f64
_A__add_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_A(%rip), %xmm0
	addsd	%xmm0, %xmm0
	leave
	ret
.globl _A__add_param_f64_f64
	.private_extern _A__add_param_f64_f64
_A__add_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	addsd	%xmm1, %xmm0
	leave
	ret
.globl _A__sub_var_f64_f64
	.private_extern _A__sub_var_f64_f64
_A__sub_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_A(%rip), %xmm1
	movsd	128+_MM_A(%rip), %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
	ret
.globl _A__sub_param_f64_f64
	.private_extern _A__sub_param_f64_f64
_A__sub_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
	ret
.globl _A__mult_var_f64_f64
	.private_extern _A__mult_var_f64_f64
_A__mult_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_A(%rip), %xmm1
	movsd	128+_MM_A(%rip), %xmm0
	mulsd	%xmm1, %xmm0
	leave
	ret
.globl _A__mult_param_f64_f64
	.private_extern _A__mult_param_f64_f64
_A__mult_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	mulsd	%xmm1, %xmm0
	leave
	ret
.globl _A__ret_var_u16
	.private_extern _A__ret_var_u16
_A__ret_var_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L859
	movl	$14305, %edi
	call	__m3_fault
L859:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _A__ret_const_u16
	.private_extern _A__ret_const_u16
_A__ret_const_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$12, %eax
	leave
	ret
.globl _A__ret_param_i16_u64
	.private_extern _A__ret_param_i16_u64
_A__ret_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i16_u16
	.private_extern _A__ret_param_i16_u16
_A__ret_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i16_i64
	.private_extern _A__ret_param_i16_i64
_A__ret_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i16_i16
	.private_extern _A__ret_param_i16_i16
_A__ret_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i16_u32
	.private_extern _A__ret_param_i16_u32
_A__ret_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_i16_u8
	.private_extern _A__ret_param_i16_u8
_A__ret_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i16_i32
	.private_extern _A__ret_param_i16_i32
_A__ret_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i16_i8
	.private_extern _A__ret_param_i16_i8
_A__ret_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_i16_u64
	.private_extern _A__add_var_i16_u64
_A__add_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_u64
	.private_extern _A__add_param_i16_u64
_A__add_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_i16_u16
	.private_extern _A__add_var_i16_u16
_A__add_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_u16
	.private_extern _A__add_param_i16_u16
_A__add_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i16_i64
	.private_extern _A__add_var_i16_i64
_A__add_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_i64
	.private_extern _A__add_param_i16_i64
_A__add_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_i16_i16
	.private_extern _A__add_var_i16_i16
_A__add_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_i16
	.private_extern _A__add_param_i16_i16
_A__add_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i16_u32
	.private_extern _A__add_var_i16_u32
_A__add_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_u32
	.private_extern _A__add_param_i16_u32
_A__add_param_i16_u32:
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
.globl _A__add_var_i16_u8
	.private_extern _A__add_var_i16_u8
_A__add_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_u8
	.private_extern _A__add_param_i16_u8
_A__add_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i16_i32
	.private_extern _A__add_var_i16_i32
_A__add_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_i32
	.private_extern _A__add_param_i16_i32
_A__add_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i16_i8
	.private_extern _A__add_var_i16_i8
_A__add_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i16_i8
	.private_extern _A__add_param_i16_i8
_A__add_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__sub_var_i16_u64
	.private_extern _A__sub_var_i16_u64
_A__sub_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_u64
	.private_extern _A__sub_param_i16_u64
_A__sub_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i16_u16
	.private_extern _A__sub_var_i16_u16
_A__sub_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_u16
	.private_extern _A__sub_param_i16_u16
_A__sub_param_i16_u16:
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
.globl _A__sub_var_i16_i64
	.private_extern _A__sub_var_i16_i64
_A__sub_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_i64
	.private_extern _A__sub_param_i16_i64
_A__sub_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i16_i16
	.private_extern _A__sub_var_i16_i16
_A__sub_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_i16_i16
	.private_extern _A__sub_param_i16_i16
_A__sub_param_i16_i16:
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
.globl _A__sub_var_i16_u32
	.private_extern _A__sub_var_i16_u32
_A__sub_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_u32
	.private_extern _A__sub_param_i16_u32
_A__sub_param_i16_u32:
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
.globl _A__sub_var_i16_u8
	.private_extern _A__sub_var_i16_u8
_A__sub_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_u8
	.private_extern _A__sub_param_i16_u8
_A__sub_param_i16_u8:
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
.globl _A__sub_var_i16_i32
	.private_extern _A__sub_var_i16_i32
_A__sub_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_i32
	.private_extern _A__sub_param_i16_i32
_A__sub_param_i16_i32:
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
.globl _A__sub_var_i16_i8
	.private_extern _A__sub_var_i16_i8
_A__sub_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i16_i8
	.private_extern _A__sub_param_i16_i8
_A__sub_param_i16_i8:
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
.globl _A__and_var_i16_u64
	.private_extern _A__and_var_i16_u64
_A__and_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_u64
	.private_extern _A__and_param_i16_u64
_A__and_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i16_u16
	.private_extern _A__and_var_i16_u16
_A__and_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_u16
	.private_extern _A__and_param_i16_u16
_A__and_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i16_i64
	.private_extern _A__and_var_i16_i64
_A__and_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_i64
	.private_extern _A__and_param_i16_i64
_A__and_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i16_i16
	.private_extern _A__and_var_i16_i16
_A__and_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _A__and_param_i16_i16
	.private_extern _A__and_param_i16_i16
_A__and_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i16_u32
	.private_extern _A__and_var_i16_u32
_A__and_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_u32
	.private_extern _A__and_param_i16_u32
_A__and_param_i16_u32:
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
.globl _A__and_var_i16_u8
	.private_extern _A__and_var_i16_u8
_A__and_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_u8
	.private_extern _A__and_param_i16_u8
_A__and_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i16_i32
	.private_extern _A__and_var_i16_i32
_A__and_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_i32
	.private_extern _A__and_param_i16_i32
_A__and_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i16_i8
	.private_extern _A__and_var_i16_i8
_A__and_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i16_i8
	.private_extern _A__and_param_i16_i8
_A__and_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_u64
	.private_extern _A__or_var_i16_u64
_A__or_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_u64
	.private_extern _A__or_param_i16_u64
_A__or_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_u16
	.private_extern _A__or_var_i16_u16
_A__or_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_u16
	.private_extern _A__or_param_i16_u16
_A__or_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_i64
	.private_extern _A__or_var_i16_i64
_A__or_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_i64
	.private_extern _A__or_param_i16_i64
_A__or_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_i16
	.private_extern _A__or_var_i16_i16
_A__or_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _A__or_param_i16_i16
	.private_extern _A__or_param_i16_i16
_A__or_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_u32
	.private_extern _A__or_var_i16_u32
_A__or_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_u32
	.private_extern _A__or_param_i16_u32
_A__or_param_i16_u32:
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
.globl _A__or_var_i16_u8
	.private_extern _A__or_var_i16_u8
_A__or_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_u8
	.private_extern _A__or_param_i16_u8
_A__or_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_i32
	.private_extern _A__or_var_i16_i32
_A__or_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_i32
	.private_extern _A__or_param_i16_i32
_A__or_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i16_i8
	.private_extern _A__or_var_i16_i8
_A__or_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i16_i8
	.private_extern _A__or_param_i16_i8
_A__or_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_u64
	.private_extern _A__xor_var_i16_u64
_A__xor_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_u64
	.private_extern _A__xor_param_i16_u64
_A__xor_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_u16
	.private_extern _A__xor_var_i16_u16
_A__xor_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_u16
	.private_extern _A__xor_param_i16_u16
_A__xor_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_i64
	.private_extern _A__xor_var_i16_i64
_A__xor_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_i64
	.private_extern _A__xor_param_i16_i64
_A__xor_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_i16
	.private_extern _A__xor_var_i16_i16
_A__xor_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_i16_i16
	.private_extern _A__xor_param_i16_i16
_A__xor_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_u32
	.private_extern _A__xor_var_i16_u32
_A__xor_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_u32
	.private_extern _A__xor_param_i16_u32
_A__xor_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_u8
	.private_extern _A__xor_var_i16_u8
_A__xor_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_u8
	.private_extern _A__xor_param_i16_u8
_A__xor_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_i32
	.private_extern _A__xor_var_i16_i32
_A__xor_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_i32
	.private_extern _A__xor_param_i16_i32
_A__xor_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i16_i8
	.private_extern _A__xor_var_i16_i8
_A__xor_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i16_i8
	.private_extern _A__xor_param_i16_i8
_A__xor_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i16_u64
	.private_extern _A__mult_var_i16_u64
_A__mult_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_u64
	.private_extern _A__mult_param_i16_u64
_A__mult_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i16_u16
	.private_extern _A__mult_var_i16_u16
_A__mult_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_u16
	.private_extern _A__mult_param_i16_u16
_A__mult_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i16_i64
	.private_extern _A__mult_var_i16_i64
_A__mult_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_i64
	.private_extern _A__mult_param_i16_i64
_A__mult_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i16_i16
	.private_extern _A__mult_var_i16_i16
_A__mult_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_i16
	.private_extern _A__mult_param_i16_i16
_A__mult_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i16_u32
	.private_extern _A__mult_var_i16_u32
_A__mult_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_u32
	.private_extern _A__mult_param_i16_u32
_A__mult_param_i16_u32:
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
.globl _A__mult_var_i16_u8
	.private_extern _A__mult_var_i16_u8
_A__mult_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_u8
	.private_extern _A__mult_param_i16_u8
_A__mult_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i16_i32
	.private_extern _A__mult_var_i16_i32
_A__mult_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_i32
	.private_extern _A__mult_param_i16_i32
_A__mult_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i16_i8
	.private_extern _A__mult_var_i16_i8
_A__mult_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i16_i8
	.private_extern _A__mult_param_i16_i8
_A__mult_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__div_var_i16_u64
	.private_extern _A__div_var_i16_u64
_A__div_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i16_u64
	.private_extern _A__div_param_i16_u64
_A__div_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i16_u16
	.private_extern _A__div_var_i16_u16
_A__div_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i16_u16
	.private_extern _A__div_param_i16_u16
_A__div_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i16_i64
	.private_extern _A__div_var_i16_i64
_A__div_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i16_i64
	.private_extern _A__div_param_i16_i64
_A__div_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i16_i16
	.private_extern _A__div_var_i16_i16
_A__div_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _A__div_param_i16_i16
	.private_extern _A__div_param_i16_i16
_A__div_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1086
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1086
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1086:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_var_i16_u32
	.private_extern _A__div_var_i16_u32
_A__div_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i16_u32
	.private_extern _A__div_param_i16_u32
_A__div_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i16_u8
	.private_extern _A__div_var_i16_u8
_A__div_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i16_u8
	.private_extern _A__div_param_i16_u8
_A__div_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %esi
	movsbq	-19(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i16_i32
	.private_extern _A__div_var_i16_i32
_A__div_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1097
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1097
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1097:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_param_i16_i32
	.private_extern _A__div_param_i16_i32
_A__div_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1100
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1100
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1100:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_var_i16_i8
	.private_extern _A__div_var_i16_i8
_A__div_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1103
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1103
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1103:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_param_i16_i8
	.private_extern _A__div_param_i16_i8
_A__div_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1106
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1106
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1106:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i16_u64
	.private_extern _A__mod_var_i16_u64
_A__mod_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i16_u64
	.private_extern _A__mod_param_i16_u64
_A__mod_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i16_u16
	.private_extern _A__mod_var_i16_u16
_A__mod_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i16_u16
	.private_extern _A__mod_param_i16_u16
_A__mod_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i16_i64
	.private_extern _A__mod_var_i16_i64
_A__mod_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i16_i64
	.private_extern _A__mod_param_i16_i64
_A__mod_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i16_i16
	.private_extern _A__mod_var_i16_i16
_A__mod_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1121
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1121
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1121:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i16_i16
	.private_extern _A__mod_param_i16_i16
_A__mod_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1124
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1124
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1124:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i16_u32
	.private_extern _A__mod_var_i16_u32
_A__mod_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i16_u32
	.private_extern _A__mod_param_i16_u32
_A__mod_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i16_u8
	.private_extern _A__mod_var_i16_u8
_A__mod_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i16_u8
	.private_extern _A__mod_param_i16_u8
_A__mod_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %esi
	movsbq	-19(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i16_i32
	.private_extern _A__mod_var_i16_i32
_A__mod_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1135
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1135
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1135:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i16_i32
	.private_extern _A__mod_param_i16_i32
_A__mod_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1138
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1138
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1138:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i16_i8
	.private_extern _A__mod_var_i16_i8
_A__mod_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1141
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1141
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1141:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i16_i8
	.private_extern _A__mod_param_i16_i8
_A__mod_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1144
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1144
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1144:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__ret_fx
	.private_extern _A__ret_fx
_A__ret_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	144+_MM_A(%rip), %xmm0
	leave
	ret
.globl _A__add_var_fx_fx
	.private_extern _A__add_var_fx_fx
_A__add_var_fx_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	144+_MM_A(%rip), %xmm0
	addsd	%xmm0, %xmm0
	leave
	ret
.globl _A__add_param_fx_fx
	.private_extern _A__add_param_fx_fx
_A__add_param_fx_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	addsd	%xmm1, %xmm0
	leave
	ret
.globl _A__sub_var_fx_fx
	.private_extern _A__sub_var_fx_fx
_A__sub_var_fx_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	144+_MM_A(%rip), %xmm1
	movsd	144+_MM_A(%rip), %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
	ret
.globl _A__sub_param_fx_fx
	.private_extern _A__sub_param_fx_fx
_A__sub_param_fx_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
	ret
.globl _A__mult_var_fx_fx
	.private_extern _A__mult_var_fx_fx
_A__mult_var_fx_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	144+_MM_A(%rip), %xmm1
	movsd	144+_MM_A(%rip), %xmm0
	mulsd	%xmm1, %xmm0
	leave
	ret
.globl _A__mult_param_fx_fx
	.private_extern _A__mult_param_fx_fx
_A__mult_param_fx_fx:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	mulsd	%xmm1, %xmm0
	leave
	ret
.globl _A__ret_var_i32
	.private_extern _A__ret_var_i32
_A__ret_var_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1161
	movl	$19073, %edi
	call	__m3_fault
L1161:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _A__ret_const_i32
	.private_extern _A__ret_const_i32
_A__ret_const_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$15, %eax
	leave
	ret
.globl _A__ret_param_u32_u64
	.private_extern _A__ret_param_u32_u64
_A__ret_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u32_u16
	.private_extern _A__ret_param_u32_u16
_A__ret_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u32_i64
	.private_extern _A__ret_param_u32_i64
_A__ret_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u32_i16
	.private_extern _A__ret_param_u32_i16
_A__ret_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u32_u32
	.private_extern _A__ret_param_u32_u32
_A__ret_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_u32_u8
	.private_extern _A__ret_param_u32_u8
_A__ret_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u32_i32
	.private_extern _A__ret_param_u32_i32
_A__ret_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u32_i8
	.private_extern _A__ret_param_u32_i8
_A__ret_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_u32_u64
	.private_extern _A__add_var_u32_u64
_A__add_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_u64
	.private_extern _A__add_param_u32_u64
_A__add_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_u32_u16
	.private_extern _A__add_var_u32_u16
_A__add_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_u16
	.private_extern _A__add_param_u32_u16
_A__add_param_u32_u16:
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
.globl _A__add_var_u32_i64
	.private_extern _A__add_var_u32_i64
_A__add_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_i64
	.private_extern _A__add_param_u32_i64
_A__add_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_u32_i16
	.private_extern _A__add_var_u32_i16
_A__add_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_i16
	.private_extern _A__add_param_u32_i16
_A__add_param_u32_i16:
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
.globl _A__add_var_u32_u32
	.private_extern _A__add_var_u32_u32
_A__add_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_u32
	.private_extern _A__add_param_u32_u32
_A__add_param_u32_u32:
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
.globl _A__add_var_u32_u8
	.private_extern _A__add_var_u32_u8
_A__add_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_u8
	.private_extern _A__add_param_u32_u8
_A__add_param_u32_u8:
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
.globl _A__add_var_u32_i32
	.private_extern _A__add_var_u32_i32
_A__add_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_i32
	.private_extern _A__add_param_u32_i32
_A__add_param_u32_i32:
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
.globl _A__add_var_u32_i8
	.private_extern _A__add_var_u32_i8
_A__add_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u32_i8
	.private_extern _A__add_param_u32_i8
_A__add_param_u32_i8:
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
.globl _A__sub_var_u32_u64
	.private_extern _A__sub_var_u32_u64
_A__sub_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_u64
	.private_extern _A__sub_param_u32_u64
_A__sub_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u32_u16
	.private_extern _A__sub_var_u32_u16
_A__sub_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_u16
	.private_extern _A__sub_param_u32_u16
_A__sub_param_u32_u16:
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
.globl _A__sub_var_u32_i64
	.private_extern _A__sub_var_u32_i64
_A__sub_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_i64
	.private_extern _A__sub_param_u32_i64
_A__sub_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u32_i16
	.private_extern _A__sub_var_u32_i16
_A__sub_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_i16
	.private_extern _A__sub_param_u32_i16
_A__sub_param_u32_i16:
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
.globl _A__sub_var_u32_u32
	.private_extern _A__sub_var_u32_u32
_A__sub_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_u32_u32
	.private_extern _A__sub_param_u32_u32
_A__sub_param_u32_u32:
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
.globl _A__sub_var_u32_u8
	.private_extern _A__sub_var_u32_u8
_A__sub_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_u8
	.private_extern _A__sub_param_u32_u8
_A__sub_param_u32_u8:
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
.globl _A__sub_var_u32_i32
	.private_extern _A__sub_var_u32_i32
_A__sub_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_i32
	.private_extern _A__sub_param_u32_i32
_A__sub_param_u32_i32:
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
.globl _A__sub_var_u32_i8
	.private_extern _A__sub_var_u32_i8
_A__sub_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u32_i8
	.private_extern _A__sub_param_u32_i8
_A__sub_param_u32_i8:
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
.globl _A__and_var_u32_u64
	.private_extern _A__and_var_u32_u64
_A__and_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_u64
	.private_extern _A__and_param_u32_u64
_A__and_param_u32_u64:
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
.globl _A__and_var_u32_u16
	.private_extern _A__and_var_u32_u16
_A__and_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_u16
	.private_extern _A__and_param_u32_u16
_A__and_param_u32_u16:
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
.globl _A__and_var_u32_i64
	.private_extern _A__and_var_u32_i64
_A__and_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_i64
	.private_extern _A__and_param_u32_i64
_A__and_param_u32_i64:
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
.globl _A__and_var_u32_i16
	.private_extern _A__and_var_u32_i16
_A__and_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_i16
	.private_extern _A__and_param_u32_i16
_A__and_param_u32_i16:
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
.globl _A__and_var_u32_u32
	.private_extern _A__and_var_u32_u32
_A__and_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	cltq
	leave
	ret
.globl _A__and_param_u32_u32
	.private_extern _A__and_param_u32_u32
_A__and_param_u32_u32:
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
.globl _A__and_var_u32_u8
	.private_extern _A__and_var_u32_u8
_A__and_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_u8
	.private_extern _A__and_param_u32_u8
_A__and_param_u32_u8:
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
.globl _A__and_var_u32_i32
	.private_extern _A__and_var_u32_i32
_A__and_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_i32
	.private_extern _A__and_param_u32_i32
_A__and_param_u32_i32:
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
.globl _A__and_var_u32_i8
	.private_extern _A__and_var_u32_i8
_A__and_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u32_i8
	.private_extern _A__and_param_u32_i8
_A__and_param_u32_i8:
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
.globl _A__or_var_u32_u64
	.private_extern _A__or_var_u32_u64
_A__or_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_u64
	.private_extern _A__or_param_u32_u64
_A__or_param_u32_u64:
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
.globl _A__or_var_u32_u16
	.private_extern _A__or_var_u32_u16
_A__or_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_u16
	.private_extern _A__or_param_u32_u16
_A__or_param_u32_u16:
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
.globl _A__or_var_u32_i64
	.private_extern _A__or_var_u32_i64
_A__or_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_i64
	.private_extern _A__or_param_u32_i64
_A__or_param_u32_i64:
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
.globl _A__or_var_u32_i16
	.private_extern _A__or_var_u32_i16
_A__or_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_i16
	.private_extern _A__or_param_u32_i16
_A__or_param_u32_i16:
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
.globl _A__or_var_u32_u32
	.private_extern _A__or_var_u32_u32
_A__or_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	cltq
	leave
	ret
.globl _A__or_param_u32_u32
	.private_extern _A__or_param_u32_u32
_A__or_param_u32_u32:
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
.globl _A__or_var_u32_u8
	.private_extern _A__or_var_u32_u8
_A__or_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_u8
	.private_extern _A__or_param_u32_u8
_A__or_param_u32_u8:
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
.globl _A__or_var_u32_i32
	.private_extern _A__or_var_u32_i32
_A__or_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_i32
	.private_extern _A__or_param_u32_i32
_A__or_param_u32_i32:
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
.globl _A__or_var_u32_i8
	.private_extern _A__or_var_u32_i8
_A__or_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u32_i8
	.private_extern _A__or_param_u32_i8
_A__or_param_u32_i8:
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
.globl _A__xor_var_u32_u64
	.private_extern _A__xor_var_u32_u64
_A__xor_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_u64
	.private_extern _A__xor_param_u32_u64
_A__xor_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_u16
	.private_extern _A__xor_var_u32_u16
_A__xor_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_u16
	.private_extern _A__xor_param_u32_u16
_A__xor_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_i64
	.private_extern _A__xor_var_u32_i64
_A__xor_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_i64
	.private_extern _A__xor_param_u32_i64
_A__xor_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_i16
	.private_extern _A__xor_var_u32_i16
_A__xor_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_i16
	.private_extern _A__xor_param_u32_i16
_A__xor_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_u32
	.private_extern _A__xor_var_u32_u32
_A__xor_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_u32_u32
	.private_extern _A__xor_param_u32_u32
_A__xor_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_u8
	.private_extern _A__xor_var_u32_u8
_A__xor_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_u8
	.private_extern _A__xor_param_u32_u8
_A__xor_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_i32
	.private_extern _A__xor_var_u32_i32
_A__xor_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_i32
	.private_extern _A__xor_param_u32_i32
_A__xor_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u32_i8
	.private_extern _A__xor_var_u32_i8
_A__xor_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u32_i8
	.private_extern _A__xor_param_u32_i8
_A__xor_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u32_u64
	.private_extern _A__mult_var_u32_u64
_A__mult_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_u64
	.private_extern _A__mult_param_u32_u64
_A__mult_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u32_u16
	.private_extern _A__mult_var_u32_u16
_A__mult_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_u16
	.private_extern _A__mult_param_u32_u16
_A__mult_param_u32_u16:
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
.globl _A__mult_var_u32_i64
	.private_extern _A__mult_var_u32_i64
_A__mult_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_i64
	.private_extern _A__mult_param_u32_i64
_A__mult_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u32_i16
	.private_extern _A__mult_var_u32_i16
_A__mult_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_i16
	.private_extern _A__mult_param_u32_i16
_A__mult_param_u32_i16:
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
.globl _A__mult_var_u32_u32
	.private_extern _A__mult_var_u32_u32
_A__mult_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_u32
	.private_extern _A__mult_param_u32_u32
_A__mult_param_u32_u32:
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
.globl _A__mult_var_u32_u8
	.private_extern _A__mult_var_u32_u8
_A__mult_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_u8
	.private_extern _A__mult_param_u32_u8
_A__mult_param_u32_u8:
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
.globl _A__mult_var_u32_i32
	.private_extern _A__mult_var_u32_i32
_A__mult_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_i32
	.private_extern _A__mult_param_u32_i32
_A__mult_param_u32_i32:
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
.globl _A__mult_var_u32_i8
	.private_extern _A__mult_var_u32_i8
_A__mult_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u32_i8
	.private_extern _A__mult_param_u32_i8
_A__mult_param_u32_i8:
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
.globl _A__div_var_u32_u64
	.private_extern _A__div_var_u32_u64
_A__div_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_u64
	.private_extern _A__div_param_u32_u64
_A__div_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_u16
	.private_extern _A__div_var_u32_u16
_A__div_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_u16
	.private_extern _A__div_param_u32_u16
_A__div_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movswq	-22(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_i64
	.private_extern _A__div_var_u32_i64
_A__div_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_i64
	.private_extern _A__div_param_u32_i64
_A__div_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_i16
	.private_extern _A__div_var_u32_i16
_A__div_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_i16
	.private_extern _A__div_param_u32_i16
_A__div_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzwl	-22(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_u32
	.private_extern _A__div_var_u32_u32
_A__div_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_u32
	.private_extern _A__div_param_u32_u32
_A__div_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_u8
	.private_extern _A__div_var_u32_u8
_A__div_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_u8
	.private_extern _A__div_param_u32_u8
_A__div_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movsbq	-21(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_i32
	.private_extern _A__div_var_u32_i32
_A__div_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_i32
	.private_extern _A__div_param_u32_i32
_A__div_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	mov	-24(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u32_i8
	.private_extern _A__div_var_u32_i8
_A__div_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u32_i8
	.private_extern _A__div_param_u32_i8
_A__div_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzbl	-21(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__mod_var_u32_u64
	.private_extern _A__mod_var_u32_u64
_A__mod_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_u64
	.private_extern _A__mod_param_u32_u64
_A__mod_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_u16
	.private_extern _A__mod_var_u32_u16
_A__mod_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_u16
	.private_extern _A__mod_param_u32_u16
_A__mod_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movswq	-22(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_i64
	.private_extern _A__mod_var_u32_i64
_A__mod_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_i64
	.private_extern _A__mod_param_u32_i64
_A__mod_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_i16
	.private_extern _A__mod_var_u32_i16
_A__mod_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_i16
	.private_extern _A__mod_param_u32_i16
_A__mod_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzwl	-22(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_u32
	.private_extern _A__mod_var_u32_u32
_A__mod_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_u32
	.private_extern _A__mod_param_u32_u32
_A__mod_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_u8
	.private_extern _A__mod_var_u32_u8
_A__mod_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_u8
	.private_extern _A__mod_param_u32_u8
_A__mod_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movsbq	-21(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_i32
	.private_extern _A__mod_var_u32_i32
_A__mod_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_i32
	.private_extern _A__mod_param_u32_i32
_A__mod_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	mov	-24(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u32_i8
	.private_extern _A__mod_var_u32_i8
_A__mod_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u32_i8
	.private_extern _A__mod_param_u32_i8
_A__mod_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzbl	-21(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__ret_var_i8
	.private_extern _A__ret_var_i8
_A__ret_var_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1438
	movl	$23585, %edi
	call	__m3_fault
L1438:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _A__ret_const_i8
	.private_extern _A__ret_const_i8
_A__ret_const_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$18, %eax
	leave
	ret
.globl _A__ret_param_u8_u64
	.private_extern _A__ret_param_u8_u64
_A__ret_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u8_u16
	.private_extern _A__ret_param_u8_u16
_A__ret_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u8_i64
	.private_extern _A__ret_param_u8_i64
_A__ret_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_u8_i16
	.private_extern _A__ret_param_u8_i16
_A__ret_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u8_u32
	.private_extern _A__ret_param_u8_u32
_A__ret_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_u8_u8
	.private_extern _A__ret_param_u8_u8
_A__ret_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_u8_i32
	.private_extern _A__ret_param_u8_i32
_A__ret_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_u8_i8
	.private_extern _A__ret_param_u8_i8
_A__ret_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_u8_u64
	.private_extern _A__add_var_u8_u64
_A__add_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_u64
	.private_extern _A__add_param_u8_u64
_A__add_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_u8_u16
	.private_extern _A__add_var_u8_u16
_A__add_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_u16
	.private_extern _A__add_param_u8_u16
_A__add_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u8_i64
	.private_extern _A__add_var_u8_i64
_A__add_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_i64
	.private_extern _A__add_param_u8_i64
_A__add_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_u8_i16
	.private_extern _A__add_var_u8_i16
_A__add_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_i16
	.private_extern _A__add_param_u8_i16
_A__add_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u8_u32
	.private_extern _A__add_var_u8_u32
_A__add_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_u32
	.private_extern _A__add_param_u8_u32
_A__add_param_u8_u32:
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
.globl _A__add_var_u8_u8
	.private_extern _A__add_var_u8_u8
_A__add_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_u8
	.private_extern _A__add_param_u8_u8
_A__add_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u8_i32
	.private_extern _A__add_var_u8_i32
_A__add_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_i32
	.private_extern _A__add_param_u8_i32
_A__add_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_u8_i8
	.private_extern _A__add_var_u8_i8
_A__add_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_u8_i8
	.private_extern _A__add_param_u8_i8
_A__add_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__sub_var_u8_u64
	.private_extern _A__sub_var_u8_u64
_A__sub_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_u64
	.private_extern _A__sub_param_u8_u64
_A__sub_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u8_u16
	.private_extern _A__sub_var_u8_u16
_A__sub_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_u16
	.private_extern _A__sub_param_u8_u16
_A__sub_param_u8_u16:
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
.globl _A__sub_var_u8_i64
	.private_extern _A__sub_var_u8_i64
_A__sub_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_i64
	.private_extern _A__sub_param_u8_i64
_A__sub_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_u8_i16
	.private_extern _A__sub_var_u8_i16
_A__sub_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_i16
	.private_extern _A__sub_param_u8_i16
_A__sub_param_u8_i16:
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
.globl _A__sub_var_u8_u32
	.private_extern _A__sub_var_u8_u32
_A__sub_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_u32
	.private_extern _A__sub_param_u8_u32
_A__sub_param_u8_u32:
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
.globl _A__sub_var_u8_u8
	.private_extern _A__sub_var_u8_u8
_A__sub_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_u8_u8
	.private_extern _A__sub_param_u8_u8
_A__sub_param_u8_u8:
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
.globl _A__sub_var_u8_i32
	.private_extern _A__sub_var_u8_i32
_A__sub_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_i32
	.private_extern _A__sub_param_u8_i32
_A__sub_param_u8_i32:
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
.globl _A__sub_var_u8_i8
	.private_extern _A__sub_var_u8_i8
_A__sub_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_u8_i8
	.private_extern _A__sub_param_u8_i8
_A__sub_param_u8_i8:
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
.globl _A__and_var_u8_u64
	.private_extern _A__and_var_u8_u64
_A__and_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_u64
	.private_extern _A__and_param_u8_u64
_A__and_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u8_u16
	.private_extern _A__and_var_u8_u16
_A__and_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_u16
	.private_extern _A__and_param_u8_u16
_A__and_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u8_i64
	.private_extern _A__and_var_u8_i64
_A__and_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_i64
	.private_extern _A__and_param_u8_i64
_A__and_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u8_i16
	.private_extern _A__and_var_u8_i16
_A__and_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_i16
	.private_extern _A__and_param_u8_i16
_A__and_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u8_u32
	.private_extern _A__and_var_u8_u32
_A__and_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_u32
	.private_extern _A__and_param_u8_u32
_A__and_param_u8_u32:
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
.globl _A__and_var_u8_u8
	.private_extern _A__and_var_u8_u8
_A__and_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _A__and_param_u8_u8
	.private_extern _A__and_param_u8_u8
_A__and_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u8_i32
	.private_extern _A__and_var_u8_i32
_A__and_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_i32
	.private_extern _A__and_param_u8_i32
_A__and_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_u8_i8
	.private_extern _A__and_var_u8_i8
_A__and_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_u8_i8
	.private_extern _A__and_param_u8_i8
_A__and_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_u64
	.private_extern _A__or_var_u8_u64
_A__or_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_u64
	.private_extern _A__or_param_u8_u64
_A__or_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_u16
	.private_extern _A__or_var_u8_u16
_A__or_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_u16
	.private_extern _A__or_param_u8_u16
_A__or_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_i64
	.private_extern _A__or_var_u8_i64
_A__or_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_i64
	.private_extern _A__or_param_u8_i64
_A__or_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_i16
	.private_extern _A__or_var_u8_i16
_A__or_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_i16
	.private_extern _A__or_param_u8_i16
_A__or_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_u32
	.private_extern _A__or_var_u8_u32
_A__or_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_u32
	.private_extern _A__or_param_u8_u32
_A__or_param_u8_u32:
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
.globl _A__or_var_u8_u8
	.private_extern _A__or_var_u8_u8
_A__or_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _A__or_param_u8_u8
	.private_extern _A__or_param_u8_u8
_A__or_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_i32
	.private_extern _A__or_var_u8_i32
_A__or_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_i32
	.private_extern _A__or_param_u8_i32
_A__or_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_u8_i8
	.private_extern _A__or_var_u8_i8
_A__or_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_u8_i8
	.private_extern _A__or_param_u8_i8
_A__or_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_u64
	.private_extern _A__xor_var_u8_u64
_A__xor_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_u64
	.private_extern _A__xor_param_u8_u64
_A__xor_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_u16
	.private_extern _A__xor_var_u8_u16
_A__xor_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_u16
	.private_extern _A__xor_param_u8_u16
_A__xor_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_i64
	.private_extern _A__xor_var_u8_i64
_A__xor_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_i64
	.private_extern _A__xor_param_u8_i64
_A__xor_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_i16
	.private_extern _A__xor_var_u8_i16
_A__xor_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_i16
	.private_extern _A__xor_param_u8_i16
_A__xor_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_u32
	.private_extern _A__xor_var_u8_u32
_A__xor_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_u32
	.private_extern _A__xor_param_u8_u32
_A__xor_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_u8
	.private_extern _A__xor_var_u8_u8
_A__xor_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_u8_u8
	.private_extern _A__xor_param_u8_u8
_A__xor_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_i32
	.private_extern _A__xor_var_u8_i32
_A__xor_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_i32
	.private_extern _A__xor_param_u8_i32
_A__xor_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_u8_i8
	.private_extern _A__xor_var_u8_i8
_A__xor_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_u8_i8
	.private_extern _A__xor_param_u8_i8
_A__xor_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u8_u64
	.private_extern _A__mult_var_u8_u64
_A__mult_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_u64
	.private_extern _A__mult_param_u8_u64
_A__mult_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u8_u16
	.private_extern _A__mult_var_u8_u16
_A__mult_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_u16
	.private_extern _A__mult_param_u8_u16
_A__mult_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u8_i64
	.private_extern _A__mult_var_u8_i64
_A__mult_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_i64
	.private_extern _A__mult_param_u8_i64
_A__mult_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_u8_i16
	.private_extern _A__mult_var_u8_i16
_A__mult_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_i16
	.private_extern _A__mult_param_u8_i16
_A__mult_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u8_u32
	.private_extern _A__mult_var_u8_u32
_A__mult_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	160+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_u32
	.private_extern _A__mult_param_u8_u32
_A__mult_param_u8_u32:
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
.globl _A__mult_var_u8_u8
	.private_extern _A__mult_var_u8_u8
_A__mult_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_u8
	.private_extern _A__mult_param_u8_u8
_A__mult_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u8_i32
	.private_extern _A__mult_var_u8_i32
_A__mult_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_i32
	.private_extern _A__mult_param_u8_i32
_A__mult_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_u8_i8
	.private_extern _A__mult_var_u8_i8
_A__mult_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_u8_i8
	.private_extern _A__mult_param_u8_i8
_A__mult_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__div_var_u8_u64
	.private_extern _A__div_var_u8_u64
_A__div_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_u64
	.private_extern _A__div_param_u8_u64
_A__div_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_u16
	.private_extern _A__div_var_u8_u16
_A__div_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_u16
	.private_extern _A__div_param_u8_u16
_A__div_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_i64
	.private_extern _A__div_var_u8_i64
_A__div_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_i64
	.private_extern _A__div_param_u8_i64
_A__div_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_i16
	.private_extern _A__div_var_u8_i16
_A__div_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_i16
	.private_extern _A__div_param_u8_i16
_A__div_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_u32
	.private_extern _A__div_var_u8_u32
_A__div_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_u32
	.private_extern _A__div_param_u8_u32
_A__div_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_u8
	.private_extern _A__div_var_u8_u8
_A__div_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_u8
	.private_extern _A__div_param_u8_u8
_A__div_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movsbq	-18(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_i32
	.private_extern _A__div_var_u8_i32
_A__div_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_i32
	.private_extern _A__div_param_u8_i32
_A__div_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_u8_i8
	.private_extern _A__div_var_u8_i8
_A__div_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_u8_i8
	.private_extern _A__div_param_u8_i8
_A__div_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movzbl	-18(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__mod_var_u8_u64
	.private_extern _A__mod_var_u8_u64
_A__mod_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_u64
	.private_extern _A__mod_param_u8_u64
_A__mod_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_u16
	.private_extern _A__mod_var_u8_u16
_A__mod_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_u16
	.private_extern _A__mod_param_u8_u16
_A__mod_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_i64
	.private_extern _A__mod_var_u8_i64
_A__mod_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_i64
	.private_extern _A__mod_param_u8_i64
_A__mod_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_i16
	.private_extern _A__mod_var_u8_i16
_A__mod_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_i16
	.private_extern _A__mod_param_u8_i16
_A__mod_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_u32
	.private_extern _A__mod_var_u8_u32
_A__mod_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_u32
	.private_extern _A__mod_param_u8_u32
_A__mod_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_u8
	.private_extern _A__mod_var_u8_u8
_A__mod_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_u8
	.private_extern _A__mod_param_u8_u8
_A__mod_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movsbq	-18(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_i32
	.private_extern _A__mod_var_u8_i32
_A__mod_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_i32
	.private_extern _A__mod_param_u8_i32
_A__mod_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_u8_i8
	.private_extern _A__mod_var_u8_i8
_A__mod_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_u8_i8
	.private_extern _A__mod_param_u8_i8
_A__mod_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movzbl	-18(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__ret_var_u32
	.private_extern _A__ret_var_u32
_A__ret_var_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1715
	movl	$28097, %edi
	call	__m3_fault
L1715:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _A__ret_const_u32
	.private_extern _A__ret_const_u32
_A__ret_const_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$21, %eax
	leave
	ret
.globl _A__ret_param_i32_u64
	.private_extern _A__ret_param_i32_u64
_A__ret_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i32_u16
	.private_extern _A__ret_param_i32_u16
_A__ret_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i32_i64
	.private_extern _A__ret_param_i32_i64
_A__ret_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i32_i16
	.private_extern _A__ret_param_i32_i16
_A__ret_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i32_u32
	.private_extern _A__ret_param_i32_u32
_A__ret_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_i32_u8
	.private_extern _A__ret_param_i32_u8
_A__ret_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i32_i32
	.private_extern _A__ret_param_i32_i32
_A__ret_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i32_i8
	.private_extern _A__ret_param_i32_i8
_A__ret_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_i32_u64
	.private_extern _A__add_var_i32_u64
_A__add_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_u64
	.private_extern _A__add_param_i32_u64
_A__add_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_i32_u16
	.private_extern _A__add_var_i32_u16
_A__add_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_u16
	.private_extern _A__add_param_i32_u16
_A__add_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i32_i64
	.private_extern _A__add_var_i32_i64
_A__add_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_i64
	.private_extern _A__add_param_i32_i64
_A__add_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_i32_i16
	.private_extern _A__add_var_i32_i16
_A__add_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_i16
	.private_extern _A__add_param_i32_i16
_A__add_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i32_u32
	.private_extern _A__add_var_i32_u32
_A__add_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_u32
	.private_extern _A__add_param_i32_u32
_A__add_param_i32_u32:
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
.globl _A__add_var_i32_u8
	.private_extern _A__add_var_i32_u8
_A__add_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_u8
	.private_extern _A__add_param_i32_u8
_A__add_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i32_i32
	.private_extern _A__add_var_i32_i32
_A__add_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_i32
	.private_extern _A__add_param_i32_i32
_A__add_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i32_i8
	.private_extern _A__add_var_i32_i8
_A__add_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i32_i8
	.private_extern _A__add_param_i32_i8
_A__add_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__sub_var_i32_u64
	.private_extern _A__sub_var_i32_u64
_A__sub_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_u64
	.private_extern _A__sub_param_i32_u64
_A__sub_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i32_u16
	.private_extern _A__sub_var_i32_u16
_A__sub_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_u16
	.private_extern _A__sub_param_i32_u16
_A__sub_param_i32_u16:
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
.globl _A__sub_var_i32_i64
	.private_extern _A__sub_var_i32_i64
_A__sub_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_i64
	.private_extern _A__sub_param_i32_i64
_A__sub_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i32_i16
	.private_extern _A__sub_var_i32_i16
_A__sub_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_i16
	.private_extern _A__sub_param_i32_i16
_A__sub_param_i32_i16:
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
.globl _A__sub_var_i32_u32
	.private_extern _A__sub_var_i32_u32
_A__sub_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_u32
	.private_extern _A__sub_param_i32_u32
_A__sub_param_i32_u32:
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
.globl _A__sub_var_i32_u8
	.private_extern _A__sub_var_i32_u8
_A__sub_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_u8
	.private_extern _A__sub_param_i32_u8
_A__sub_param_i32_u8:
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
.globl _A__sub_var_i32_i32
	.private_extern _A__sub_var_i32_i32
_A__sub_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_i32_i32
	.private_extern _A__sub_param_i32_i32
_A__sub_param_i32_i32:
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
.globl _A__sub_var_i32_i8
	.private_extern _A__sub_var_i32_i8
_A__sub_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i32_i8
	.private_extern _A__sub_param_i32_i8
_A__sub_param_i32_i8:
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
.globl _A__and_var_i32_u64
	.private_extern _A__and_var_i32_u64
_A__and_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_u64
	.private_extern _A__and_param_i32_u64
_A__and_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i32_u16
	.private_extern _A__and_var_i32_u16
_A__and_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_u16
	.private_extern _A__and_param_i32_u16
_A__and_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i32_i64
	.private_extern _A__and_var_i32_i64
_A__and_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_i64
	.private_extern _A__and_param_i32_i64
_A__and_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i32_i16
	.private_extern _A__and_var_i32_i16
_A__and_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_i16
	.private_extern _A__and_param_i32_i16
_A__and_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i32_u32
	.private_extern _A__and_var_i32_u32
_A__and_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_u32
	.private_extern _A__and_param_i32_u32
_A__and_param_i32_u32:
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
.globl _A__and_var_i32_u8
	.private_extern _A__and_var_i32_u8
_A__and_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_u8
	.private_extern _A__and_param_i32_u8
_A__and_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i32_i32
	.private_extern _A__and_var_i32_i32
_A__and_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _A__and_param_i32_i32
	.private_extern _A__and_param_i32_i32
_A__and_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i32_i8
	.private_extern _A__and_var_i32_i8
_A__and_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i32_i8
	.private_extern _A__and_param_i32_i8
_A__and_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_u64
	.private_extern _A__or_var_i32_u64
_A__or_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_u64
	.private_extern _A__or_param_i32_u64
_A__or_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_u16
	.private_extern _A__or_var_i32_u16
_A__or_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_u16
	.private_extern _A__or_param_i32_u16
_A__or_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_i64
	.private_extern _A__or_var_i32_i64
_A__or_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_i64
	.private_extern _A__or_param_i32_i64
_A__or_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_i16
	.private_extern _A__or_var_i32_i16
_A__or_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_i16
	.private_extern _A__or_param_i32_i16
_A__or_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_u32
	.private_extern _A__or_var_i32_u32
_A__or_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_u32
	.private_extern _A__or_param_i32_u32
_A__or_param_i32_u32:
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
.globl _A__or_var_i32_u8
	.private_extern _A__or_var_i32_u8
_A__or_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_u8
	.private_extern _A__or_param_i32_u8
_A__or_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_i32
	.private_extern _A__or_var_i32_i32
_A__or_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _A__or_param_i32_i32
	.private_extern _A__or_param_i32_i32
_A__or_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i32_i8
	.private_extern _A__or_var_i32_i8
_A__or_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i32_i8
	.private_extern _A__or_param_i32_i8
_A__or_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_u64
	.private_extern _A__xor_var_i32_u64
_A__xor_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_u64
	.private_extern _A__xor_param_i32_u64
_A__xor_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_u16
	.private_extern _A__xor_var_i32_u16
_A__xor_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_u16
	.private_extern _A__xor_param_i32_u16
_A__xor_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_i64
	.private_extern _A__xor_var_i32_i64
_A__xor_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_i64
	.private_extern _A__xor_param_i32_i64
_A__xor_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_i16
	.private_extern _A__xor_var_i32_i16
_A__xor_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_i16
	.private_extern _A__xor_param_i32_i16
_A__xor_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_u32
	.private_extern _A__xor_var_i32_u32
_A__xor_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_u32
	.private_extern _A__xor_param_i32_u32
_A__xor_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_u8
	.private_extern _A__xor_var_i32_u8
_A__xor_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_u8
	.private_extern _A__xor_param_i32_u8
_A__xor_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_i32
	.private_extern _A__xor_var_i32_i32
_A__xor_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_i32_i32
	.private_extern _A__xor_param_i32_i32
_A__xor_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i32_i8
	.private_extern _A__xor_var_i32_i8
_A__xor_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i32_i8
	.private_extern _A__xor_param_i32_i8
_A__xor_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i32_u64
	.private_extern _A__mult_var_i32_u64
_A__mult_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_u64
	.private_extern _A__mult_param_i32_u64
_A__mult_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i32_u16
	.private_extern _A__mult_var_i32_u16
_A__mult_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_u16
	.private_extern _A__mult_param_i32_u16
_A__mult_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i32_i64
	.private_extern _A__mult_var_i32_i64
_A__mult_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_i64
	.private_extern _A__mult_param_i32_i64
_A__mult_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i32_i16
	.private_extern _A__mult_var_i32_i16
_A__mult_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_i16
	.private_extern _A__mult_param_i32_i16
_A__mult_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i32_u32
	.private_extern _A__mult_var_i32_u32
_A__mult_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_u32
	.private_extern _A__mult_param_i32_u32
_A__mult_param_i32_u32:
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
.globl _A__mult_var_i32_u8
	.private_extern _A__mult_var_i32_u8
_A__mult_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_u8
	.private_extern _A__mult_param_i32_u8
_A__mult_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i32_i32
	.private_extern _A__mult_var_i32_i32
_A__mult_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_i32
	.private_extern _A__mult_param_i32_i32
_A__mult_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i32_i8
	.private_extern _A__mult_var_i32_i8
_A__mult_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i32_i8
	.private_extern _A__mult_param_i32_i8
_A__mult_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__div_var_i32_u64
	.private_extern _A__div_var_i32_u64
_A__div_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i32_u64
	.private_extern _A__div_param_i32_u64
_A__div_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i32_u16
	.private_extern _A__div_var_i32_u16
_A__div_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i32_u16
	.private_extern _A__div_param_i32_u16
_A__div_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %esi
	movswq	-22(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i32_i64
	.private_extern _A__div_var_i32_i64
_A__div_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i32_i64
	.private_extern _A__div_param_i32_i64
_A__div_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i32_i16
	.private_extern _A__div_var_i32_i16
_A__div_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1940
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1940
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1940:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_param_i32_i16
	.private_extern _A__div_param_i32_i16
_A__div_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-22(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1943
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1943
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1943:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_var_i32_u32
	.private_extern _A__div_var_i32_u32
_A__div_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i32_u32
	.private_extern _A__div_param_i32_u32
_A__div_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i32_u8
	.private_extern _A__div_var_i32_u8
_A__div_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i32_u8
	.private_extern _A__div_param_i32_u8
_A__div_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %esi
	movsbq	-21(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i32_i32
	.private_extern _A__div_var_i32_i32
_A__div_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _A__div_param_i32_i32
	.private_extern _A__div_param_i32_i32
_A__div_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1956
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1956
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1956:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_var_i32_i8
	.private_extern _A__div_var_i32_i8
_A__div_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1959
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1959
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1959:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_param_i32_i8
	.private_extern _A__div_param_i32_i8
_A__div_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-21(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1962
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1962
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1962:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i32_u64
	.private_extern _A__mod_var_i32_u64
_A__mod_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i32_u64
	.private_extern _A__mod_param_i32_u64
_A__mod_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i32_u16
	.private_extern _A__mod_var_i32_u16
_A__mod_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i32_u16
	.private_extern _A__mod_param_i32_u16
_A__mod_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %esi
	movswq	-22(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i32_i64
	.private_extern _A__mod_var_i32_i64
_A__mod_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i32_i64
	.private_extern _A__mod_param_i32_i64
_A__mod_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i32_i16
	.private_extern _A__mod_var_i32_i16
_A__mod_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1977
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1977
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1977:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i32_i16
	.private_extern _A__mod_param_i32_i16
_A__mod_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-22(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1980
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1980
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1980:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i32_u32
	.private_extern _A__mod_var_i32_u32
_A__mod_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i32_u32
	.private_extern _A__mod_param_i32_u32
_A__mod_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i32_u8
	.private_extern _A__mod_var_i32_u8
_A__mod_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i32_u8
	.private_extern _A__mod_param_i32_u8
_A__mod_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %esi
	movsbq	-21(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i32_i32
	.private_extern _A__mod_var_i32_i32
_A__mod_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1991
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1991
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1991:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i32_i32
	.private_extern _A__mod_param_i32_i32
_A__mod_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1994
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1994
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1994:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i32_i8
	.private_extern _A__mod_var_i32_i8
_A__mod_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1997
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1997
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1997:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i32_i8
	.private_extern _A__mod_param_i32_i8
_A__mod_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-21(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L2000
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2000
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L2000:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__ret_var_u8
	.private_extern _A__ret_var_u8
_A__ret_var_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2003
	movl	$32609, %edi
	call	__m3_fault
L2003:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _A__ret_const_u8
	.private_extern _A__ret_const_u8
_A__ret_const_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$24, %eax
	leave
	ret
.globl _A__ret_param_i8_u64
	.private_extern _A__ret_param_i8_u64
_A__ret_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i8_u16
	.private_extern _A__ret_param_i8_u16
_A__ret_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i8_i64
	.private_extern _A__ret_param_i8_i64
_A__ret_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_param_i8_i16
	.private_extern _A__ret_param_i8_i16
_A__ret_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i8_u32
	.private_extern _A__ret_param_i8_u32
_A__ret_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_param_i8_u8
	.private_extern _A__ret_param_i8_u8
_A__ret_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_param_i8_i32
	.private_extern _A__ret_param_i8_i32
_A__ret_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_param_i8_i8
	.private_extern _A__ret_param_i8_i8
_A__ret_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__add_var_i8_u64
	.private_extern _A__add_var_i8_u64
_A__add_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_u64
	.private_extern _A__add_param_i8_u64
_A__add_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_i8_u16
	.private_extern _A__add_var_i8_u16
_A__add_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_u16
	.private_extern _A__add_param_i8_u16
_A__add_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i8_i64
	.private_extern _A__add_var_i8_i64
_A__add_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_i64
	.private_extern _A__add_param_i8_i64
_A__add_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_var_i8_i16
	.private_extern _A__add_var_i8_i16
_A__add_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_i16
	.private_extern _A__add_param_i8_i16
_A__add_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i8_u32
	.private_extern _A__add_var_i8_u32
_A__add_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_u32
	.private_extern _A__add_param_i8_u32
_A__add_param_i8_u32:
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
.globl _A__add_var_i8_u8
	.private_extern _A__add_var_i8_u8
_A__add_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_u8
	.private_extern _A__add_param_i8_u8
_A__add_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i8_i32
	.private_extern _A__add_var_i8_i32
_A__add_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_i32
	.private_extern _A__add_param_i8_i32
_A__add_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_var_i8_i8
	.private_extern _A__add_var_i8_i8
_A__add_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_param_i8_i8
	.private_extern _A__add_param_i8_i8
_A__add_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__sub_var_i8_u64
	.private_extern _A__sub_var_i8_u64
_A__sub_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_u64
	.private_extern _A__sub_param_i8_u64
_A__sub_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i8_u16
	.private_extern _A__sub_var_i8_u16
_A__sub_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_u16
	.private_extern _A__sub_param_i8_u16
_A__sub_param_i8_u16:
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
.globl _A__sub_var_i8_i64
	.private_extern _A__sub_var_i8_i64
_A__sub_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_i64
	.private_extern _A__sub_param_i8_i64
_A__sub_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_var_i8_i16
	.private_extern _A__sub_var_i8_i16
_A__sub_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_i16
	.private_extern _A__sub_param_i8_i16
_A__sub_param_i8_i16:
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
.globl _A__sub_var_i8_u32
	.private_extern _A__sub_var_i8_u32
_A__sub_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_u32
	.private_extern _A__sub_param_i8_u32
_A__sub_param_i8_u32:
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
.globl _A__sub_var_i8_u8
	.private_extern _A__sub_var_i8_u8
_A__sub_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_u8
	.private_extern _A__sub_param_i8_u8
_A__sub_param_i8_u8:
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
.globl _A__sub_var_i8_i32
	.private_extern _A__sub_var_i8_i32
_A__sub_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_param_i8_i32
	.private_extern _A__sub_param_i8_i32
_A__sub_param_i8_i32:
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
.globl _A__sub_var_i8_i8
	.private_extern _A__sub_var_i8_i8
_A__sub_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_param_i8_i8
	.private_extern _A__sub_param_i8_i8
_A__sub_param_i8_i8:
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
.globl _A__and_var_i8_u64
	.private_extern _A__and_var_i8_u64
_A__and_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_u64
	.private_extern _A__and_param_i8_u64
_A__and_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i8_u16
	.private_extern _A__and_var_i8_u16
_A__and_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_u16
	.private_extern _A__and_param_i8_u16
_A__and_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i8_i64
	.private_extern _A__and_var_i8_i64
_A__and_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_i64
	.private_extern _A__and_param_i8_i64
_A__and_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i8_i16
	.private_extern _A__and_var_i8_i16
_A__and_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_i16
	.private_extern _A__and_param_i8_i16
_A__and_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i8_u32
	.private_extern _A__and_var_i8_u32
_A__and_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_u32
	.private_extern _A__and_param_i8_u32
_A__and_param_i8_u32:
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
.globl _A__and_var_i8_u8
	.private_extern _A__and_var_i8_u8
_A__and_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_u8
	.private_extern _A__and_param_i8_u8
_A__and_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i8_i32
	.private_extern _A__and_var_i8_i32
_A__and_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_param_i8_i32
	.private_extern _A__and_param_i8_i32
_A__and_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_var_i8_i8
	.private_extern _A__and_var_i8_i8
_A__and_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _A__and_param_i8_i8
	.private_extern _A__and_param_i8_i8
_A__and_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_u64
	.private_extern _A__or_var_i8_u64
_A__or_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_u64
	.private_extern _A__or_param_i8_u64
_A__or_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_u16
	.private_extern _A__or_var_i8_u16
_A__or_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_u16
	.private_extern _A__or_param_i8_u16
_A__or_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_i64
	.private_extern _A__or_var_i8_i64
_A__or_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_i64
	.private_extern _A__or_param_i8_i64
_A__or_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_i16
	.private_extern _A__or_var_i8_i16
_A__or_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_i16
	.private_extern _A__or_param_i8_i16
_A__or_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_u32
	.private_extern _A__or_var_i8_u32
_A__or_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_u32
	.private_extern _A__or_param_i8_u32
_A__or_param_i8_u32:
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
.globl _A__or_var_i8_u8
	.private_extern _A__or_var_i8_u8
_A__or_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_u8
	.private_extern _A__or_param_i8_u8
_A__or_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_i32
	.private_extern _A__or_var_i8_i32
_A__or_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_param_i8_i32
	.private_extern _A__or_param_i8_i32
_A__or_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_var_i8_i8
	.private_extern _A__or_var_i8_i8
_A__or_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _A__or_param_i8_i8
	.private_extern _A__or_param_i8_i8
_A__or_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_u64
	.private_extern _A__xor_var_i8_u64
_A__xor_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_u64
	.private_extern _A__xor_param_i8_u64
_A__xor_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_u16
	.private_extern _A__xor_var_i8_u16
_A__xor_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_u16
	.private_extern _A__xor_param_i8_u16
_A__xor_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_i64
	.private_extern _A__xor_var_i8_i64
_A__xor_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_i64
	.private_extern _A__xor_param_i8_i64
_A__xor_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_i16
	.private_extern _A__xor_var_i8_i16
_A__xor_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_i16
	.private_extern _A__xor_param_i8_i16
_A__xor_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_u32
	.private_extern _A__xor_var_i8_u32
_A__xor_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_u32
	.private_extern _A__xor_param_i8_u32
_A__xor_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_u8
	.private_extern _A__xor_var_i8_u8
_A__xor_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_u8
	.private_extern _A__xor_param_i8_u8
_A__xor_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_i32
	.private_extern _A__xor_var_i8_i32
_A__xor_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_param_i8_i32
	.private_extern _A__xor_param_i8_i32
_A__xor_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_var_i8_i8
	.private_extern _A__xor_var_i8_i8
_A__xor_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_param_i8_i8
	.private_extern _A__xor_param_i8_i8
_A__xor_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i8_u64
	.private_extern _A__mult_var_i8_u64
_A__mult_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_u64
	.private_extern _A__mult_param_i8_u64
_A__mult_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i8_u16
	.private_extern _A__mult_var_i8_u16
_A__mult_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_u16
	.private_extern _A__mult_param_i8_u16
_A__mult_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i8_i64
	.private_extern _A__mult_var_i8_i64
_A__mult_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_i64
	.private_extern _A__mult_param_i8_i64
_A__mult_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_var_i8_i16
	.private_extern _A__mult_var_i8_i16
_A__mult_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_i16
	.private_extern _A__mult_param_i8_i16
_A__mult_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i8_u32
	.private_extern _A__mult_var_i8_u32
_A__mult_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	160+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_u32
	.private_extern _A__mult_param_i8_u32
_A__mult_param_i8_u32:
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
.globl _A__mult_var_i8_u8
	.private_extern _A__mult_var_i8_u8
_A__mult_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_u8
	.private_extern _A__mult_param_i8_u8
_A__mult_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i8_i32
	.private_extern _A__mult_var_i8_i32
_A__mult_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_i32
	.private_extern _A__mult_param_i8_i32
_A__mult_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_var_i8_i8
	.private_extern _A__mult_var_i8_i8
_A__mult_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_param_i8_i8
	.private_extern _A__mult_param_i8_i8
_A__mult_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__div_var_i8_u64
	.private_extern _A__div_var_i8_u64
_A__div_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i8_u64
	.private_extern _A__div_param_i8_u64
_A__div_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i8_u16
	.private_extern _A__div_var_i8_u16
_A__div_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i8_u16
	.private_extern _A__div_param_i8_u16
_A__div_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i8_i64
	.private_extern _A__div_var_i8_i64
_A__div_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i8_i64
	.private_extern _A__div_param_i8_i64
_A__div_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i8_i16
	.private_extern _A__div_var_i8_i16
_A__div_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L2228
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L2228
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L2228:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_param_i8_i16
	.private_extern _A__div_param_i8_i16
_A__div_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L2231
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2231
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L2231:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_var_i8_u32
	.private_extern _A__div_var_i8_u32
_A__div_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i8_u32
	.private_extern _A__div_param_i8_u32
_A__div_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i8_u8
	.private_extern _A__div_var_i8_u8
_A__div_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_param_i8_u8
	.private_extern _A__div_param_i8_u8
_A__div_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %esi
	movsbq	-18(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_var_i8_i32
	.private_extern _A__div_var_i8_i32
_A__div_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L2242
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L2242
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L2242:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_param_i8_i32
	.private_extern _A__div_param_i8_i32
_A__div_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L2245
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2245
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L2245:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_var_i8_i8
	.private_extern _A__div_var_i8_i8
_A__div_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _A__div_param_i8_i8
	.private_extern _A__div_param_i8_i8
_A__div_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L2250
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2250
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L2250:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i8_u64
	.private_extern _A__mod_var_i8_u64
_A__mod_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i8_u64
	.private_extern _A__mod_param_i8_u64
_A__mod_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i8_u16
	.private_extern _A__mod_var_i8_u16
_A__mod_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzwl	136+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i8_u16
	.private_extern _A__mod_param_i8_u16
_A__mod_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i8_i64
	.private_extern _A__mod_var_i8_i64
_A__mod_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	104+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i8_i64
	.private_extern _A__mod_param_i8_i64
_A__mod_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i8_i16
	.private_extern _A__mod_var_i8_i16
_A__mod_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	116+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L2265
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L2265
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L2265:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i8_i16
	.private_extern _A__mod_param_i8_i16
_A__mod_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L2268
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2268
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L2268:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i8_u32
	.private_extern _A__mod_var_i8_u32
_A__mod_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movl	160+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i8_u32
	.private_extern _A__mod_param_i8_u32
_A__mod_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i8_u8
	.private_extern _A__mod_var_i8_u8
_A__mod_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzbl	164+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_param_i8_u8
	.private_extern _A__mod_param_i8_u8
_A__mod_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %esi
	movsbq	-18(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_var_i8_i32
	.private_extern _A__mod_var_i8_i32
_A__mod_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	152+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L2279
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L2279
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L2279:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i8_i32
	.private_extern _A__mod_param_i8_i32
_A__mod_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L2282
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2282
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L2282:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_var_i8_i8
	.private_extern _A__mod_var_i8_i8
_A__mod_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzbl	156+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L2285
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L2285
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L2285:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_param_i8_i8
	.private_extern _A__mod_param_i8_i8
_A__mod_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L2288
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L2288
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L2288:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A_M3
_A_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_A(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_A(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "A_M3"
	.space 1
	.ascii "mod_param_i8_i8"
	.space 1
	.ascii "mod_var_i8_i8"
	.space 1
	.ascii "mod_param_i8_i32"
	.space 1
	.ascii "mod_var_i8_i32"
	.space 1
	.ascii "mod_param_i8_u8"
	.space 1
	.ascii "mod_var_i8_u8"
	.space 1
	.ascii "mod_param_i8_u32"
	.space 1
	.ascii "mod_var_i8_u32"
	.space 1
	.ascii "mod_param_i8_i16"
	.space 1
	.ascii "mod_var_i8_i16"
	.space 1
	.ascii "mod_param_i8_i64"
	.space 1
	.ascii "mod_var_i8_i64"
	.space 1
	.ascii "mod_param_i8_u16"
	.space 1
	.ascii "mod_var_i8_u16"
	.space 1
	.ascii "mod_param_i8_u64"
	.space 1
	.ascii "mod_var_i8_u64"
	.space 1
	.ascii "div_param_i8_i8"
	.space 1
	.ascii "div_var_i8_i8"
	.space 1
	.ascii "div_param_i8_i32"
	.space 1
	.ascii "div_var_i8_i32"
	.space 1
	.ascii "div_param_i8_u8"
	.space 1
	.ascii "div_var_i8_u8"
	.space 1
	.ascii "div_param_i8_u32"
	.space 1
	.ascii "div_var_i8_u32"
	.space 1
	.ascii "div_param_i8_i16"
	.space 1
	.ascii "div_var_i8_i16"
	.space 1
	.ascii "div_param_i8_i64"
	.space 1
	.ascii "div_var_i8_i64"
	.space 1
	.ascii "div_param_i8_u16"
	.space 1
	.ascii "div_var_i8_u16"
	.space 1
	.ascii "div_param_i8_u64"
	.space 1
	.ascii "div_var_i8_u64"
	.space 1
	.ascii "mult_param_i8_i8"
	.space 1
	.ascii "mult_var_i8_i8"
	.space 1
	.ascii "mult_param_i8_i32"
	.space 1
	.ascii "mult_var_i8_i32"
	.space 1
	.ascii "mult_param_i8_u8"
	.space 1
	.ascii "mult_var_i8_u8"
	.space 1
	.ascii "mult_param_i8_u32"
	.space 1
	.ascii "mult_var_i8_u32"
	.space 1
	.ascii "mult_param_i8_i16"
	.space 1
	.ascii "mult_var_i8_i16"
	.space 1
	.ascii "mult_param_i8_i64"
	.space 1
	.ascii "mult_var_i8_i64"
	.space 1
	.ascii "mult_param_i8_u16"
	.space 1
	.ascii "mult_var_i8_u16"
	.space 1
	.ascii "mult_param_i8_u64"
	.space 1
	.ascii "mult_var_i8_u64"
	.space 1
	.ascii "xor_param_i8_i8"
	.space 1
	.ascii "xor_var_i8_i8"
	.space 1
	.ascii "xor_param_i8_i32"
	.space 1
	.ascii "xor_var_i8_i32"
	.space 1
	.ascii "xor_param_i8_u8"
	.space 1
	.ascii "xor_var_i8_u8"
	.space 1
	.ascii "xor_param_i8_u32"
	.space 1
	.ascii "xor_var_i8_u32"
	.space 1
	.ascii "xor_param_i8_i16"
	.space 1
	.ascii "xor_var_i8_i16"
	.space 1
	.ascii "xor_param_i8_i64"
	.space 1
	.ascii "xor_var_i8_i64"
	.space 1
	.ascii "xor_param_i8_u16"
	.space 1
	.ascii "xor_var_i8_u16"
	.space 1
	.ascii "xor_param_i8_u64"
	.space 1
	.ascii "xor_var_i8_u64"
	.space 1
	.ascii "or_param_i8_i8"
	.space 1
	.ascii "or_var_i8_i8"
	.space 1
	.ascii "or_param_i8_i32"
	.space 1
	.ascii "or_var_i8_i32"
	.space 1
	.ascii "or_param_i8_u8"
	.space 1
	.ascii "or_var_i8_u8"
	.space 1
	.ascii "or_param_i8_u32"
	.space 1
	.ascii "or_var_i8_u32"
	.space 1
	.ascii "or_param_i8_i16"
	.space 1
	.ascii "or_var_i8_i16"
	.space 1
	.ascii "or_param_i8_i64"
	.space 1
	.ascii "or_var_i8_i64"
	.space 1
	.ascii "or_param_i8_u16"
	.space 1
	.ascii "or_var_i8_u16"
	.space 1
	.ascii "or_param_i8_u64"
	.space 1
	.ascii "or_var_i8_u64"
	.space 1
	.ascii "and_param_i8_i8"
	.space 1
	.ascii "and_var_i8_i8"
	.space 1
	.ascii "and_param_i8_i32"
	.space 1
	.ascii "and_var_i8_i32"
	.space 1
	.ascii "and_param_i8_u8"
	.space 1
	.ascii "and_var_i8_u8"
	.space 1
	.ascii "and_param_i8_u32"
	.space 1
	.ascii "and_var_i8_u32"
	.space 1
	.ascii "and_param_i8_i16"
	.space 1
	.ascii "and_var_i8_i16"
	.space 1
	.ascii "and_param_i8_i64"
	.space 1
	.ascii "and_var_i8_i64"
	.space 1
	.ascii "and_param_i8_u16"
	.space 1
	.ascii "and_var_i8_u16"
	.space 1
	.ascii "and_param_i8_u64"
	.space 1
	.ascii "and_var_i8_u64"
	.space 1
	.ascii "sub_param_i8_i8"
	.space 1
	.ascii "sub_var_i8_i8"
	.space 1
	.ascii "sub_param_i8_i32"
	.space 1
	.ascii "sub_var_i8_i32"
	.space 1
	.ascii "sub_param_i8_u8"
	.space 1
	.ascii "sub_var_i8_u8"
	.space 1
	.ascii "sub_param_i8_u32"
	.space 1
	.ascii "sub_var_i8_u32"
	.space 1
	.ascii "sub_param_i8_i16"
	.space 1
	.ascii "sub_var_i8_i16"
	.space 1
	.ascii "sub_param_i8_i64"
	.space 1
	.ascii "sub_var_i8_i64"
	.space 1
	.ascii "sub_param_i8_u16"
	.space 1
	.ascii "sub_var_i8_u16"
	.space 1
	.ascii "sub_param_i8_u64"
	.space 1
	.ascii "sub_var_i8_u64"
	.space 1
	.ascii "add_param_i8_i8"
	.space 1
	.ascii "add_var_i8_i8"
	.space 1
	.ascii "add_param_i8_i32"
	.space 1
	.ascii "add_var_i8_i32"
	.space 1
	.ascii "add_param_i8_u8"
	.space 1
	.ascii "add_var_i8_u8"
	.space 1
	.ascii "add_param_i8_u32"
	.space 1
	.ascii "add_var_i8_u32"
	.space 1
	.ascii "add_param_i8_i16"
	.space 1
	.ascii "add_var_i8_i16"
	.space 1
	.ascii "add_param_i8_i64"
	.space 1
	.ascii "add_var_i8_i64"
	.space 1
	.ascii "add_param_i8_u16"
	.space 1
	.ascii "add_var_i8_u16"
	.space 1
	.ascii "add_param_i8_u64"
	.space 1
	.ascii "add_var_i8_u64"
	.space 1
	.ascii "ret_param_i8_i8"
	.space 1
	.ascii "ret_param_i8_i32"
	.space 1
	.ascii "ret_param_i8_u8"
	.space 1
	.ascii "ret_param_i8_u32"
	.space 1
	.ascii "ret_param_i8_i16"
	.space 1
	.ascii "ret_param_i8_i64"
	.space 1
	.ascii "ret_param_i8_u16"
	.space 1
	.ascii "ret_param_i8_u64"
	.space 1
	.ascii "ret_const_u8"
	.space 1
	.ascii "ret_var_u8"
	.space 1
	.ascii "mod_param_i32_i8"
	.space 1
	.ascii "mod_var_i32_i8"
	.space 1
	.ascii "mod_param_i32_i32"
	.space 1
	.ascii "mod_var_i32_i32"
	.space 1
	.ascii "mod_param_i32_u8"
	.space 1
	.ascii "mod_var_i32_u8"
	.space 1
	.ascii "mod_param_i32_u32"
	.space 1
	.ascii "mod_var_i32_u32"
	.space 1
	.ascii "mod_param_i32_i16"
	.space 1
	.ascii "mod_var_i32_i16"
	.space 1
	.ascii "mod_param_i32_i64"
	.space 1
	.ascii "mod_var_i32_i64"
	.space 1
	.ascii "mod_param_i32_u16"
	.space 1
	.ascii "mod_var_i32_u16"
	.space 1
	.ascii "mod_param_i32_u64"
	.space 1
	.ascii "mod_var_i32_u64"
	.space 1
	.ascii "div_param_i32_i8"
	.space 1
	.ascii "div_var_i32_i8"
	.space 1
	.ascii "div_param_i32_i32"
	.space 1
	.ascii "div_var_i32_i32"
	.space 1
	.ascii "div_param_i32_u8"
	.space 1
	.ascii "div_var_i32_u8"
	.space 1
	.ascii "div_param_i32_u32"
	.space 1
	.ascii "div_var_i32_u32"
	.space 1
	.ascii "div_param_i32_i16"
	.space 1
	.ascii "div_var_i32_i16"
	.space 1
	.ascii "div_param_i32_i64"
	.space 1
	.ascii "div_var_i32_i64"
	.space 1
	.ascii "div_param_i32_u16"
	.space 1
	.ascii "div_var_i32_u16"
	.space 1
	.ascii "div_param_i32_u64"
	.space 1
	.ascii "div_var_i32_u64"
	.space 1
	.ascii "mult_param_i32_i8"
	.space 1
	.ascii "mult_var_i32_i8"
	.space 1
	.ascii "mult_param_i32_i32"
	.space 1
	.ascii "mult_var_i32_i32"
	.space 1
	.ascii "mult_param_i32_u8"
	.space 1
	.ascii "mult_var_i32_u8"
	.space 1
	.ascii "mult_param_i32_u32"
	.space 1
	.ascii "mult_var_i32_u32"
	.space 1
	.ascii "mult_param_i32_i16"
	.space 1
	.ascii "mult_var_i32_i16"
	.space 1
	.ascii "mult_param_i32_i64"
	.space 1
	.ascii "mult_var_i32_i64"
	.space 1
	.ascii "mult_param_i32_u16"
	.space 1
	.ascii "mult_var_i32_u16"
	.space 1
	.ascii "mult_param_i32_u64"
	.space 1
	.ascii "mult_var_i32_u64"
	.space 1
	.ascii "xor_param_i32_i8"
	.space 1
	.ascii "xor_var_i32_i8"
	.space 1
	.ascii "xor_param_i32_i32"
	.space 1
	.ascii "xor_var_i32_i32"
	.space 1
	.ascii "xor_param_i32_u8"
	.space 1
	.ascii "xor_var_i32_u8"
	.space 1
	.ascii "xor_param_i32_u32"
	.space 1
	.ascii "xor_var_i32_u32"
	.space 1
	.ascii "xor_param_i32_i16"
	.space 1
	.ascii "xor_var_i32_i16"
	.space 1
	.ascii "xor_param_i32_i64"
	.space 1
	.ascii "xor_var_i32_i64"
	.space 1
	.ascii "xor_param_i32_u16"
	.space 1
	.ascii "xor_var_i32_u16"
	.space 1
	.ascii "xor_param_i32_u64"
	.space 1
	.ascii "xor_var_i32_u64"
	.space 1
	.ascii "or_param_i32_i8"
	.space 1
	.ascii "or_var_i32_i8"
	.space 1
	.ascii "or_param_i32_i32"
	.space 1
	.ascii "or_var_i32_i32"
	.space 1
	.ascii "or_param_i32_u8"
	.space 1
	.ascii "or_var_i32_u8"
	.space 1
	.ascii "or_param_i32_u32"
	.space 1
	.ascii "or_var_i32_u32"
	.space 1
	.ascii "or_param_i32_i16"
	.space 1
	.ascii "or_var_i32_i16"
	.space 1
	.ascii "or_param_i32_i64"
	.space 1
	.ascii "or_var_i32_i64"
	.space 1
	.ascii "or_param_i32_u16"
	.space 1
	.ascii "or_var_i32_u16"
	.space 1
	.ascii "or_param_i32_u64"
	.space 1
	.ascii "or_var_i32_u64"
	.space 1
	.ascii "and_param_i32_i8"
	.space 1
	.ascii "and_var_i32_i8"
	.space 1
	.ascii "and_param_i32_i32"
	.space 1
	.ascii "and_var_i32_i32"
	.space 1
	.ascii "and_param_i32_u8"
	.space 1
	.ascii "and_var_i32_u8"
	.space 1
	.ascii "and_param_i32_u32"
	.space 1
	.ascii "and_var_i32_u32"
	.space 1
	.ascii "and_param_i32_i16"
	.space 1
	.ascii "and_var_i32_i16"
	.space 1
	.ascii "and_param_i32_i64"
	.space 1
	.ascii "and_var_i32_i64"
	.space 1
	.ascii "and_param_i32_u16"
	.space 1
	.ascii "and_var_i32_u16"
	.space 1
	.ascii "and_param_i32_u64"
	.space 1
	.ascii "and_var_i32_u64"
	.space 1
	.ascii "sub_param_i32_i8"
	.space 1
	.ascii "sub_var_i32_i8"
	.space 1
	.ascii "sub_param_i32_i32"
	.space 1
	.ascii "sub_var_i32_i32"
	.space 1
	.ascii "sub_param_i32_u8"
	.space 1
	.ascii "sub_var_i32_u8"
	.space 1
	.ascii "sub_param_i32_u32"
	.space 1
	.ascii "sub_var_i32_u32"
	.space 1
	.ascii "sub_param_i32_i16"
	.space 1
	.ascii "sub_var_i32_i16"
	.space 1
	.ascii "sub_param_i32_i64"
	.space 1
	.ascii "sub_var_i32_i64"
	.space 1
	.ascii "sub_param_i32_u16"
	.space 1
	.ascii "sub_var_i32_u16"
	.space 1
	.ascii "sub_param_i32_u64"
	.space 1
	.ascii "sub_var_i32_u64"
	.space 1
	.ascii "add_param_i32_i8"
	.space 1
	.ascii "add_var_i32_i8"
	.space 1
	.ascii "add_param_i32_i32"
	.space 1
	.ascii "add_var_i32_i32"
	.space 1
	.ascii "add_param_i32_u8"
	.space 1
	.ascii "add_var_i32_u8"
	.space 1
	.ascii "add_param_i32_u32"
	.space 1
	.ascii "add_var_i32_u32"
	.space 1
	.ascii "add_param_i32_i16"
	.space 1
	.ascii "add_var_i32_i16"
	.space 1
	.ascii "add_param_i32_i64"
	.space 1
	.ascii "add_var_i32_i64"
	.space 1
	.ascii "add_param_i32_u16"
	.space 1
	.ascii "add_var_i32_u16"
	.space 1
	.ascii "add_param_i32_u64"
	.space 1
	.ascii "add_var_i32_u64"
	.space 1
	.ascii "ret_param_i32_i8"
	.space 1
	.ascii "ret_param_i32_i32"
	.space 1
	.ascii "ret_param_i32_u8"
	.space 1
	.ascii "ret_param_i32_u32"
	.space 1
	.ascii "ret_param_i32_i16"
	.space 1
	.ascii "ret_param_i32_i64"
	.space 1
	.ascii "ret_param_i32_u16"
	.space 1
	.ascii "ret_param_i32_u64"
	.space 1
	.ascii "ret_const_u32"
	.space 1
	.ascii "ret_var_u32"
	.space 1
	.ascii "mod_param_u8_i8"
	.space 1
	.ascii "mod_var_u8_i8"
	.space 1
	.ascii "mod_param_u8_i32"
	.space 1
	.ascii "mod_var_u8_i32"
	.space 1
	.ascii "mod_param_u8_u8"
	.space 1
	.ascii "mod_var_u8_u8"
	.space 1
	.ascii "mod_param_u8_u32"
	.space 1
	.ascii "mod_var_u8_u32"
	.space 1
	.ascii "mod_param_u8_i16"
	.space 1
	.ascii "mod_var_u8_i16"
	.space 1
	.ascii "mod_param_u8_i64"
	.space 1
	.ascii "mod_var_u8_i64"
	.space 1
	.ascii "mod_param_u8_u16"
	.space 1
	.ascii "mod_var_u8_u16"
	.space 1
	.ascii "mod_param_u8_u64"
	.space 1
	.ascii "mod_var_u8_u64"
	.space 1
	.ascii "div_param_u8_i8"
	.space 1
	.ascii "div_var_u8_i8"
	.space 1
	.ascii "div_param_u8_i32"
	.space 1
	.ascii "div_var_u8_i32"
	.space 1
	.ascii "div_param_u8_u8"
	.space 1
	.ascii "div_var_u8_u8"
	.space 1
	.ascii "div_param_u8_u32"
	.space 1
	.ascii "div_var_u8_u32"
	.space 1
	.ascii "div_param_u8_i16"
	.space 1
	.ascii "div_var_u8_i16"
	.space 1
	.ascii "div_param_u8_i64"
	.space 1
	.ascii "div_var_u8_i64"
	.space 1
	.ascii "div_param_u8_u16"
	.space 1
	.ascii "div_var_u8_u16"
	.space 1
	.ascii "div_param_u8_u64"
	.space 1
	.ascii "div_var_u8_u64"
	.space 1
	.ascii "mult_param_u8_i8"
	.space 1
	.ascii "mult_var_u8_i8"
	.space 1
	.ascii "mult_param_u8_i32"
	.space 1
	.ascii "mult_var_u8_i32"
	.space 1
	.ascii "mult_param_u8_u8"
	.space 1
	.ascii "mult_var_u8_u8"
	.space 1
	.ascii "mult_param_u8_u32"
	.space 1
	.ascii "mult_var_u8_u32"
	.space 1
	.ascii "mult_param_u8_i16"
	.space 1
	.ascii "mult_var_u8_i16"
	.space 1
	.ascii "mult_param_u8_i64"
	.space 1
	.ascii "mult_var_u8_i64"
	.space 1
	.ascii "mult_param_u8_u16"
	.space 1
	.ascii "mult_var_u8_u16"
	.space 1
	.ascii "mult_param_u8_u64"
	.space 1
	.ascii "mult_var_u8_u64"
	.space 1
	.ascii "xor_param_u8_i8"
	.space 1
	.ascii "xor_var_u8_i8"
	.space 1
	.ascii "xor_param_u8_i32"
	.space 1
	.ascii "xor_var_u8_i32"
	.space 1
	.ascii "xor_param_u8_u8"
	.space 1
	.ascii "xor_var_u8_u8"
	.space 1
	.ascii "xor_param_u8_u32"
	.space 1
	.ascii "xor_var_u8_u32"
	.space 1
	.ascii "xor_param_u8_i16"
	.space 1
	.ascii "xor_var_u8_i16"
	.space 1
	.ascii "xor_param_u8_i64"
	.space 1
	.ascii "xor_var_u8_i64"
	.space 1
	.ascii "xor_param_u8_u16"
	.space 1
	.ascii "xor_var_u8_u16"
	.space 1
	.ascii "xor_param_u8_u64"
	.space 1
	.ascii "xor_var_u8_u64"
	.space 1
	.ascii "or_param_u8_i8"
	.space 1
	.ascii "or_var_u8_i8"
	.space 1
	.ascii "or_param_u8_i32"
	.space 1
	.ascii "or_var_u8_i32"
	.space 1
	.ascii "or_param_u8_u8"
	.space 1
	.ascii "or_var_u8_u8"
	.space 1
	.ascii "or_param_u8_u32"
	.space 1
	.ascii "or_var_u8_u32"
	.space 1
	.ascii "or_param_u8_i16"
	.space 1
	.ascii "or_var_u8_i16"
	.space 1
	.ascii "or_param_u8_i64"
	.space 1
	.ascii "or_var_u8_i64"
	.space 1
	.ascii "or_param_u8_u16"
	.space 1
	.ascii "or_var_u8_u16"
	.space 1
	.ascii "or_param_u8_u64"
	.space 1
	.ascii "or_var_u8_u64"
	.space 1
	.ascii "and_param_u8_i8"
	.space 1
	.ascii "and_var_u8_i8"
	.space 1
	.ascii "and_param_u8_i32"
	.space 1
	.ascii "and_var_u8_i32"
	.space 1
	.ascii "and_param_u8_u8"
	.space 1
	.ascii "and_var_u8_u8"
	.space 1
	.ascii "and_param_u8_u32"
	.space 1
	.ascii "and_var_u8_u32"
	.space 1
	.ascii "and_param_u8_i16"
	.space 1
	.ascii "and_var_u8_i16"
	.space 1
	.ascii "and_param_u8_i64"
	.space 1
	.ascii "and_var_u8_i64"
	.space 1
	.ascii "and_param_u8_u16"
	.space 1
	.ascii "and_var_u8_u16"
	.space 1
	.ascii "and_param_u8_u64"
	.space 1
	.ascii "and_var_u8_u64"
	.space 1
	.ascii "sub_param_u8_i8"
	.space 1
	.ascii "sub_var_u8_i8"
	.space 1
	.ascii "sub_param_u8_i32"
	.space 1
	.ascii "sub_var_u8_i32"
	.space 1
	.ascii "sub_param_u8_u8"
	.space 1
	.ascii "sub_var_u8_u8"
	.space 1
	.ascii "sub_param_u8_u32"
	.space 1
	.ascii "sub_var_u8_u32"
	.space 1
	.ascii "sub_param_u8_i16"
	.space 1
	.ascii "sub_var_u8_i16"
	.space 1
	.ascii "sub_param_u8_i64"
	.space 1
	.ascii "sub_var_u8_i64"
	.space 1
	.ascii "sub_param_u8_u16"
	.space 1
	.ascii "sub_var_u8_u16"
	.space 1
	.ascii "sub_param_u8_u64"
	.space 1
	.ascii "sub_var_u8_u64"
	.space 1
	.ascii "add_param_u8_i8"
	.space 1
	.ascii "add_var_u8_i8"
	.space 1
	.ascii "add_param_u8_i32"
	.space 1
	.ascii "add_var_u8_i32"
	.space 1
	.ascii "add_param_u8_u8"
	.space 1
	.ascii "add_var_u8_u8"
	.space 1
	.ascii "add_param_u8_u32"
	.space 1
	.ascii "add_var_u8_u32"
	.space 1
	.ascii "add_param_u8_i16"
	.space 1
	.ascii "add_var_u8_i16"
	.space 1
	.ascii "add_param_u8_i64"
	.space 1
	.ascii "add_var_u8_i64"
	.space 1
	.ascii "add_param_u8_u16"
	.space 1
	.ascii "add_var_u8_u16"
	.space 1
	.ascii "add_param_u8_u64"
	.space 1
	.ascii "add_var_u8_u64"
	.space 1
	.ascii "ret_param_u8_i8"
	.space 1
	.ascii "ret_param_u8_i32"
	.space 1
	.ascii "ret_param_u8_u8"
	.space 1
	.ascii "ret_param_u8_u32"
	.space 1
	.ascii "ret_param_u8_i16"
	.space 1
	.ascii "ret_param_u8_i64"
	.space 1
	.ascii "ret_param_u8_u16"
	.space 1
	.ascii "ret_param_u8_u64"
	.space 1
	.ascii "ret_const_i8"
	.space 1
	.ascii "ret_var_i8"
	.space 1
	.ascii "mod_param_u32_i8"
	.space 1
	.ascii "mod_var_u32_i8"
	.space 1
	.ascii "mod_param_u32_i32"
	.space 1
	.ascii "mod_var_u32_i32"
	.space 1
	.ascii "mod_param_u32_u8"
	.space 1
	.ascii "mod_var_u32_u8"
	.space 1
	.ascii "mod_param_u32_u32"
	.space 1
	.ascii "mod_var_u32_u32"
	.space 1
	.ascii "mod_param_u32_i16"
	.space 1
	.ascii "mod_var_u32_i16"
	.space 1
	.ascii "mod_param_u32_i64"
	.space 1
	.ascii "mod_var_u32_i64"
	.space 1
	.ascii "mod_param_u32_u16"
	.space 1
	.ascii "mod_var_u32_u16"
	.space 1
	.ascii "mod_param_u32_u64"
	.space 1
	.ascii "mod_var_u32_u64"
	.space 1
	.ascii "div_param_u32_i8"
	.space 1
	.ascii "div_var_u32_i8"
	.space 1
	.ascii "div_param_u32_i32"
	.space 1
	.ascii "div_var_u32_i32"
	.space 1
	.ascii "div_param_u32_u8"
	.space 1
	.ascii "div_var_u32_u8"
	.space 1
	.ascii "div_param_u32_u32"
	.space 1
	.ascii "div_var_u32_u32"
	.space 1
	.ascii "div_param_u32_i16"
	.space 1
	.ascii "div_var_u32_i16"
	.space 1
	.ascii "div_param_u32_i64"
	.space 1
	.ascii "div_var_u32_i64"
	.space 1
	.ascii "div_param_u32_u16"
	.space 1
	.ascii "div_var_u32_u16"
	.space 1
	.ascii "div_param_u32_u64"
	.space 1
	.ascii "div_var_u32_u64"
	.space 1
	.ascii "mult_param_u32_i8"
	.space 1
	.ascii "mult_var_u32_i8"
	.space 1
	.ascii "mult_param_u32_i32"
	.space 1
	.ascii "mult_var_u32_i32"
	.space 1
	.ascii "mult_param_u32_u8"
	.space 1
	.ascii "mult_var_u32_u8"
	.space 1
	.ascii "mult_param_u32_u32"
	.space 1
	.ascii "mult_var_u32_u32"
	.space 1
	.ascii "mult_param_u32_i16"
	.space 1
	.ascii "mult_var_u32_i16"
	.space 1
	.ascii "mult_param_u32_i64"
	.space 1
	.ascii "mult_var_u32_i64"
	.space 1
	.ascii "mult_param_u32_u16"
	.space 1
	.ascii "mult_var_u32_u16"
	.space 1
	.ascii "mult_param_u32_u64"
	.space 1
	.ascii "mult_var_u32_u64"
	.space 1
	.ascii "xor_param_u32_i8"
	.space 1
	.ascii "xor_var_u32_i8"
	.space 1
	.ascii "xor_param_u32_i32"
	.space 1
	.ascii "xor_var_u32_i32"
	.space 1
	.ascii "xor_param_u32_u8"
	.space 1
	.ascii "xor_var_u32_u8"
	.space 1
	.ascii "xor_param_u32_u32"
	.space 1
	.ascii "xor_var_u32_u32"
	.space 1
	.ascii "xor_param_u32_i16"
	.space 1
	.ascii "xor_var_u32_i16"
	.space 1
	.ascii "xor_param_u32_i64"
	.space 1
	.ascii "xor_var_u32_i64"
	.space 1
	.ascii "xor_param_u32_u16"
	.space 1
	.ascii "xor_var_u32_u16"
	.space 1
	.ascii "xor_param_u32_u64"
	.space 1
	.ascii "xor_var_u32_u64"
	.space 1
	.ascii "or_param_u32_i8"
	.space 1
	.ascii "or_var_u32_i8"
	.space 1
	.ascii "or_param_u32_i32"
	.space 1
	.ascii "or_var_u32_i32"
	.space 1
	.ascii "or_param_u32_u8"
	.space 1
	.ascii "or_var_u32_u8"
	.space 1
	.ascii "or_param_u32_u32"
	.space 1
	.ascii "or_var_u32_u32"
	.space 1
	.ascii "or_param_u32_i16"
	.space 1
	.ascii "or_var_u32_i16"
	.space 1
	.ascii "or_param_u32_i64"
	.space 1
	.ascii "or_var_u32_i64"
	.space 1
	.ascii "or_param_u32_u16"
	.space 1
	.ascii "or_var_u32_u16"
	.space 1
	.ascii "or_param_u32_u64"
	.space 1
	.ascii "or_var_u32_u64"
	.space 1
	.ascii "and_param_u32_i8"
	.space 1
	.ascii "and_var_u32_i8"
	.space 1
	.ascii "and_param_u32_i32"
	.space 1
	.ascii "and_var_u32_i32"
	.space 1
	.ascii "and_param_u32_u8"
	.space 1
	.ascii "and_var_u32_u8"
	.space 1
	.ascii "and_param_u32_u32"
	.space 1
	.ascii "and_var_u32_u32"
	.space 1
	.ascii "and_param_u32_i16"
	.space 1
	.ascii "and_var_u32_i16"
	.space 1
	.ascii "and_param_u32_i64"
	.space 1
	.ascii "and_var_u32_i64"
	.space 1
	.ascii "and_param_u32_u16"
	.space 1
	.ascii "and_var_u32_u16"
	.space 1
	.ascii "and_param_u32_u64"
	.space 1
	.ascii "and_var_u32_u64"
	.space 1
	.ascii "sub_param_u32_i8"
	.space 1
	.ascii "sub_var_u32_i8"
	.space 1
	.ascii "sub_param_u32_i32"
	.space 1
	.ascii "sub_var_u32_i32"
	.space 1
	.ascii "sub_param_u32_u8"
	.space 1
	.ascii "sub_var_u32_u8"
	.space 1
	.ascii "sub_param_u32_u32"
	.space 1
	.ascii "sub_var_u32_u32"
	.space 1
	.ascii "sub_param_u32_i16"
	.space 1
	.ascii "sub_var_u32_i16"
	.space 1
	.ascii "sub_param_u32_i64"
	.space 1
	.ascii "sub_var_u32_i64"
	.space 1
	.ascii "sub_param_u32_u16"
	.space 1
	.ascii "sub_var_u32_u16"
	.space 1
	.ascii "sub_param_u32_u64"
	.space 1
	.ascii "sub_var_u32_u64"
	.space 1
	.ascii "add_param_u32_i8"
	.space 1
	.ascii "add_var_u32_i8"
	.space 1
	.ascii "add_param_u32_i32"
	.space 1
	.ascii "add_var_u32_i32"
	.space 1
	.ascii "add_param_u32_u8"
	.space 1
	.ascii "add_var_u32_u8"
	.space 1
	.ascii "add_param_u32_u32"
	.space 1
	.ascii "add_var_u32_u32"
	.space 1
	.ascii "add_param_u32_i16"
	.space 1
	.ascii "add_var_u32_i16"
	.space 1
	.ascii "add_param_u32_i64"
	.space 1
	.ascii "add_var_u32_i64"
	.space 1
	.ascii "add_param_u32_u16"
	.space 1
	.ascii "add_var_u32_u16"
	.space 1
	.ascii "add_param_u32_u64"
	.space 1
	.ascii "add_var_u32_u64"
	.space 1
	.ascii "ret_param_u32_i8"
	.space 1
	.ascii "ret_param_u32_i32"
	.space 1
	.ascii "ret_param_u32_u8"
	.space 1
	.ascii "ret_param_u32_u32"
	.space 1
	.ascii "ret_param_u32_i16"
	.space 1
	.ascii "ret_param_u32_i64"
	.space 1
	.ascii "ret_param_u32_u16"
	.space 1
	.ascii "ret_param_u32_u64"
	.space 1
	.ascii "ret_const_i32"
	.space 1
	.ascii "ret_var_i32"
	.space 1
	.ascii "mult_param_fx_fx"
	.space 1
	.ascii "mult_var_fx_fx"
	.space 1
	.ascii "sub_param_fx_fx"
	.space 1
	.ascii "sub_var_fx_fx"
	.space 1
	.ascii "add_param_fx_fx"
	.space 1
	.ascii "add_var_fx_fx"
	.space 1
	.ascii "ret_fx"
	.space 1
	.ascii "mod_param_i16_i8"
	.space 1
	.ascii "mod_var_i16_i8"
	.space 1
	.ascii "mod_param_i16_i32"
	.space 1
	.ascii "mod_var_i16_i32"
	.space 1
	.ascii "mod_param_i16_u8"
	.space 1
	.ascii "mod_var_i16_u8"
	.space 1
	.ascii "mod_param_i16_u32"
	.space 1
	.ascii "mod_var_i16_u32"
	.space 1
	.ascii "mod_param_i16_i16"
	.space 1
	.ascii "mod_var_i16_i16"
	.space 1
	.ascii "mod_param_i16_i64"
	.space 1
	.ascii "mod_var_i16_i64"
	.space 1
	.ascii "mod_param_i16_u16"
	.space 1
	.ascii "mod_var_i16_u16"
	.space 1
	.ascii "mod_param_i16_u64"
	.space 1
	.ascii "mod_var_i16_u64"
	.space 1
	.ascii "div_param_i16_i8"
	.space 1
	.ascii "div_var_i16_i8"
	.space 1
	.ascii "div_param_i16_i32"
	.space 1
	.ascii "div_var_i16_i32"
	.space 1
	.ascii "div_param_i16_u8"
	.space 1
	.ascii "div_var_i16_u8"
	.space 1
	.ascii "div_param_i16_u32"
	.space 1
	.ascii "div_var_i16_u32"
	.space 1
	.ascii "div_param_i16_i16"
	.space 1
	.ascii "div_var_i16_i16"
	.space 1
	.ascii "div_param_i16_i64"
	.space 1
	.ascii "div_var_i16_i64"
	.space 1
	.ascii "div_param_i16_u16"
	.space 1
	.ascii "div_var_i16_u16"
	.space 1
	.ascii "div_param_i16_u64"
	.space 1
	.ascii "div_var_i16_u64"
	.space 1
	.ascii "mult_param_i16_i8"
	.space 1
	.ascii "mult_var_i16_i8"
	.space 1
	.ascii "mult_param_i16_i32"
	.space 1
	.ascii "mult_var_i16_i32"
	.space 1
	.ascii "mult_param_i16_u8"
	.space 1
	.ascii "mult_var_i16_u8"
	.space 1
	.ascii "mult_param_i16_u32"
	.space 1
	.ascii "mult_var_i16_u32"
	.space 1
	.ascii "mult_param_i16_i16"
	.space 1
	.ascii "mult_var_i16_i16"
	.space 1
	.ascii "mult_param_i16_i64"
	.space 1
	.ascii "mult_var_i16_i64"
	.space 1
	.ascii "mult_param_i16_u16"
	.space 1
	.ascii "mult_var_i16_u16"
	.space 1
	.ascii "mult_param_i16_u64"
	.space 1
	.ascii "mult_var_i16_u64"
	.space 1
	.ascii "xor_param_i16_i8"
	.space 1
	.ascii "xor_var_i16_i8"
	.space 1
	.ascii "xor_param_i16_i32"
	.space 1
	.ascii "xor_var_i16_i32"
	.space 1
	.ascii "xor_param_i16_u8"
	.space 1
	.ascii "xor_var_i16_u8"
	.space 1
	.ascii "xor_param_i16_u32"
	.space 1
	.ascii "xor_var_i16_u32"
	.space 1
	.ascii "xor_param_i16_i16"
	.space 1
	.ascii "xor_var_i16_i16"
	.space 1
	.ascii "xor_param_i16_i64"
	.space 1
	.ascii "xor_var_i16_i64"
	.space 1
	.ascii "xor_param_i16_u16"
	.space 1
	.ascii "xor_var_i16_u16"
	.space 1
	.ascii "xor_param_i16_u64"
	.space 1
	.ascii "xor_var_i16_u64"
	.space 1
	.ascii "or_param_i16_i8"
	.space 1
	.ascii "or_var_i16_i8"
	.space 1
	.ascii "or_param_i16_i32"
	.space 1
	.ascii "or_var_i16_i32"
	.space 1
	.ascii "or_param_i16_u8"
	.space 1
	.ascii "or_var_i16_u8"
	.space 1
	.ascii "or_param_i16_u32"
	.space 1
	.ascii "or_var_i16_u32"
	.space 1
	.ascii "or_param_i16_i16"
	.space 1
	.ascii "or_var_i16_i16"
	.space 1
	.ascii "or_param_i16_i64"
	.space 1
	.ascii "or_var_i16_i64"
	.space 1
	.ascii "or_param_i16_u16"
	.space 1
	.ascii "or_var_i16_u16"
	.space 1
	.ascii "or_param_i16_u64"
	.space 1
	.ascii "or_var_i16_u64"
	.space 1
	.ascii "and_param_i16_i8"
	.space 1
	.ascii "and_var_i16_i8"
	.space 1
	.ascii "and_param_i16_i32"
	.space 1
	.ascii "and_var_i16_i32"
	.space 1
	.ascii "and_param_i16_u8"
	.space 1
	.ascii "and_var_i16_u8"
	.space 1
	.ascii "and_param_i16_u32"
	.space 1
	.ascii "and_var_i16_u32"
	.space 1
	.ascii "and_param_i16_i16"
	.space 1
	.ascii "and_var_i16_i16"
	.space 1
	.ascii "and_param_i16_i64"
	.space 1
	.ascii "and_var_i16_i64"
	.space 1
	.ascii "and_param_i16_u16"
	.space 1
	.ascii "and_var_i16_u16"
	.space 1
	.ascii "and_param_i16_u64"
	.space 1
	.ascii "and_var_i16_u64"
	.space 1
	.ascii "sub_param_i16_i8"
	.space 1
	.ascii "sub_var_i16_i8"
	.space 1
	.ascii "sub_param_i16_i32"
	.space 1
	.ascii "sub_var_i16_i32"
	.space 1
	.ascii "sub_param_i16_u8"
	.space 1
	.ascii "sub_var_i16_u8"
	.space 1
	.ascii "sub_param_i16_u32"
	.space 1
	.ascii "sub_var_i16_u32"
	.space 1
	.ascii "sub_param_i16_i16"
	.space 1
	.ascii "sub_var_i16_i16"
	.space 1
	.ascii "sub_param_i16_i64"
	.space 1
	.ascii "sub_var_i16_i64"
	.space 1
	.ascii "sub_param_i16_u16"
	.space 1
	.ascii "sub_var_i16_u16"
	.space 1
	.ascii "sub_param_i16_u64"
	.space 1
	.ascii "sub_var_i16_u64"
	.space 1
	.ascii "add_param_i16_i8"
	.space 1
	.ascii "add_var_i16_i8"
	.space 1
	.ascii "add_param_i16_i32"
	.space 1
	.ascii "add_var_i16_i32"
	.space 1
	.ascii "add_param_i16_u8"
	.space 1
	.ascii "add_var_i16_u8"
	.space 1
	.ascii "add_param_i16_u32"
	.space 1
	.ascii "add_var_i16_u32"
	.space 1
	.ascii "add_param_i16_i16"
	.space 1
	.ascii "add_var_i16_i16"
	.space 1
	.ascii "add_param_i16_i64"
	.space 1
	.ascii "add_var_i16_i64"
	.space 1
	.ascii "add_param_i16_u16"
	.space 1
	.ascii "add_var_i16_u16"
	.space 1
	.ascii "add_param_i16_u64"
	.space 1
	.ascii "add_var_i16_u64"
	.space 1
	.ascii "ret_param_i16_i8"
	.space 1
	.ascii "ret_param_i16_i32"
	.space 1
	.ascii "ret_param_i16_u8"
	.space 1
	.ascii "ret_param_i16_u32"
	.space 1
	.ascii "ret_param_i16_i16"
	.space 1
	.ascii "ret_param_i16_i64"
	.space 1
	.ascii "ret_param_i16_u16"
	.space 1
	.ascii "ret_param_i16_u64"
	.space 1
	.ascii "ret_const_u16"
	.space 1
	.ascii "ret_var_u16"
	.space 1
	.ascii "mult_param_f64_f64"
	.space 1
	.ascii "mult_var_f64_f64"
	.space 1
	.ascii "sub_param_f64_f64"
	.space 1
	.ascii "sub_var_f64_f64"
	.space 1
	.ascii "add_param_f64_f64"
	.space 1
	.ascii "add_var_f64_f64"
	.space 1
	.ascii "ret_f64"
	.space 1
	.ascii "mod_param_i64_i8"
	.space 1
	.ascii "mod_var_i64_i8"
	.space 1
	.ascii "mod_param_i64_i32"
	.space 1
	.ascii "mod_var_i64_i32"
	.space 1
	.ascii "mod_param_i64_u8"
	.space 1
	.ascii "mod_var_i64_u8"
	.space 1
	.ascii "mod_param_i64_u32"
	.space 1
	.ascii "mod_var_i64_u32"
	.space 1
	.ascii "mod_param_i64_i16"
	.space 1
	.ascii "mod_var_i64_i16"
	.space 1
	.ascii "mod_param_i64_i64"
	.space 1
	.ascii "mod_var_i64_i64"
	.space 1
	.ascii "mod_param_i64_u16"
	.space 1
	.ascii "mod_var_i64_u16"
	.space 1
	.ascii "mod_param_i64_u64"
	.space 1
	.ascii "mod_var_i64_u64"
	.space 1
	.ascii "div_param_i64_i8"
	.space 1
	.ascii "div_var_i64_i8"
	.space 1
	.ascii "div_param_i64_i32"
	.space 1
	.ascii "div_var_i64_i32"
	.space 1
	.ascii "div_param_i64_u8"
	.space 1
	.ascii "div_var_i64_u8"
	.space 1
	.ascii "div_param_i64_u32"
	.space 1
	.ascii "div_var_i64_u32"
	.space 1
	.ascii "div_param_i64_i16"
	.space 1
	.ascii "div_var_i64_i16"
	.space 1
	.ascii "div_param_i64_i64"
	.space 1
	.ascii "div_var_i64_i64"
	.space 1
	.ascii "div_param_i64_u16"
	.space 1
	.ascii "div_var_i64_u16"
	.space 1
	.ascii "div_param_i64_u64"
	.space 1
	.ascii "div_var_i64_u64"
	.space 1
	.ascii "mult_param_i64_i8"
	.space 1
	.ascii "mult_var_i64_i8"
	.space 1
	.ascii "mult_param_i64_i32"
	.space 1
	.ascii "mult_var_i64_i32"
	.space 1
	.ascii "mult_param_i64_u8"
	.space 1
	.ascii "mult_var_i64_u8"
	.space 1
	.ascii "mult_param_i64_u32"
	.space 1
	.ascii "mult_var_i64_u32"
	.space 1
	.ascii "mult_param_i64_i16"
	.space 1
	.ascii "mult_var_i64_i16"
	.space 1
	.ascii "mult_param_i64_i64"
	.space 1
	.ascii "mult_var_i64_i64"
	.space 1
	.ascii "mult_param_i64_u16"
	.space 1
	.ascii "mult_var_i64_u16"
	.space 1
	.ascii "mult_param_i64_u64"
	.space 1
	.ascii "mult_var_i64_u64"
	.space 1
	.ascii "xor_param_i64_i8"
	.space 1
	.ascii "xor_var_i64_i8"
	.space 1
	.ascii "xor_param_i64_i32"
	.space 1
	.ascii "xor_var_i64_i32"
	.space 1
	.ascii "xor_param_i64_u8"
	.space 1
	.ascii "xor_var_i64_u8"
	.space 1
	.ascii "xor_param_i64_u32"
	.space 1
	.ascii "xor_var_i64_u32"
	.space 1
	.ascii "xor_param_i64_i16"
	.space 1
	.ascii "xor_var_i64_i16"
	.space 1
	.ascii "xor_param_i64_i64"
	.space 1
	.ascii "xor_var_i64_i64"
	.space 1
	.ascii "xor_param_i64_u16"
	.space 1
	.ascii "xor_var_i64_u16"
	.space 1
	.ascii "xor_param_i64_u64"
	.space 1
	.ascii "xor_var_i64_u64"
	.space 1
	.ascii "or_param_i64_i8"
	.space 1
	.ascii "or_var_i64_i8"
	.space 1
	.ascii "or_param_i64_i32"
	.space 1
	.ascii "or_var_i64_i32"
	.space 1
	.ascii "or_param_i64_u8"
	.space 1
	.ascii "or_var_i64_u8"
	.space 1
	.ascii "or_param_i64_u32"
	.space 1
	.ascii "or_var_i64_u32"
	.space 1
	.ascii "or_param_i64_i16"
	.space 1
	.ascii "or_var_i64_i16"
	.space 1
	.ascii "or_param_i64_i64"
	.space 1
	.ascii "or_var_i64_i64"
	.space 1
	.ascii "or_param_i64_u16"
	.space 1
	.ascii "or_var_i64_u16"
	.space 1
	.ascii "or_param_i64_u64"
	.space 1
	.ascii "or_var_i64_u64"
	.space 1
	.ascii "and_param_i64_i8"
	.space 1
	.ascii "and_var_i64_i8"
	.space 1
	.ascii "and_param_i64_i32"
	.space 1
	.ascii "and_var_i64_i32"
	.space 1
	.ascii "and_param_i64_u8"
	.space 1
	.ascii "and_var_i64_u8"
	.space 1
	.ascii "and_param_i64_u32"
	.space 1
	.ascii "and_var_i64_u32"
	.space 1
	.ascii "and_param_i64_i16"
	.space 1
	.ascii "and_var_i64_i16"
	.space 1
	.ascii "and_param_i64_i64"
	.space 1
	.ascii "and_var_i64_i64"
	.space 1
	.ascii "and_param_i64_u16"
	.space 1
	.ascii "and_var_i64_u16"
	.space 1
	.ascii "and_param_i64_u64"
	.space 1
	.ascii "and_var_i64_u64"
	.space 1
	.ascii "sub_param_i64_i8"
	.space 1
	.ascii "sub_var_i64_i8"
	.space 1
	.ascii "sub_param_i64_i32"
	.space 1
	.ascii "sub_var_i64_i32"
	.space 1
	.ascii "sub_param_i64_u8"
	.space 1
	.ascii "sub_var_i64_u8"
	.space 1
	.ascii "sub_param_i64_u32"
	.space 1
	.ascii "sub_var_i64_u32"
	.space 1
	.ascii "sub_param_i64_i16"
	.space 1
	.ascii "sub_var_i64_i16"
	.space 1
	.ascii "sub_param_i64_i64"
	.space 1
	.ascii "sub_var_i64_i64"
	.space 1
	.ascii "sub_param_i64_u16"
	.space 1
	.ascii "sub_var_i64_u16"
	.space 1
	.ascii "sub_param_i64_u64"
	.space 1
	.ascii "sub_var_i64_u64"
	.space 1
	.ascii "add_param_i64_i8"
	.space 1
	.ascii "add_var_i64_i8"
	.space 1
	.ascii "add_param_i64_i32"
	.space 1
	.ascii "add_var_i64_i32"
	.space 1
	.ascii "add_param_i64_u8"
	.space 1
	.ascii "add_var_i64_u8"
	.space 1
	.ascii "add_param_i64_u32"
	.space 1
	.ascii "add_var_i64_u32"
	.space 1
	.ascii "add_param_i64_i16"
	.space 1
	.ascii "add_var_i64_i16"
	.space 1
	.ascii "add_param_i64_i64"
	.space 1
	.ascii "add_var_i64_i64"
	.space 1
	.ascii "add_param_i64_u16"
	.space 1
	.ascii "add_var_i64_u16"
	.space 1
	.ascii "add_param_i64_u64"
	.space 1
	.ascii "add_var_i64_u64"
	.space 1
	.ascii "ret_param_i64_i8"
	.space 1
	.ascii "ret_param_i64_i32"
	.space 1
	.ascii "ret_param_i64_u8"
	.space 1
	.ascii "ret_param_i64_u32"
	.space 1
	.ascii "ret_param_i64_i16"
	.space 1
	.ascii "ret_param_i64_i64"
	.space 1
	.ascii "ret_param_i64_u16"
	.space 1
	.ascii "ret_param_i64_u64"
	.space 1
	.ascii "ret_const_u64"
	.space 1
	.ascii "ret_var_u64"
	.space 1
	.ascii "mod_param_u16_i8"
	.space 1
	.ascii "mod_var_u16_i8"
	.space 1
	.ascii "mod_param_u16_i32"
	.space 1
	.ascii "mod_var_u16_i32"
	.space 1
	.ascii "mod_param_u16_u8"
	.space 1
	.ascii "mod_var_u16_u8"
	.space 1
	.ascii "mod_param_u16_u32"
	.space 1
	.ascii "mod_var_u16_u32"
	.space 1
	.ascii "mod_param_u16_i16"
	.space 1
	.ascii "mod_var_u16_i16"
	.space 1
	.ascii "mod_param_u16_i64"
	.space 1
	.ascii "mod_var_u16_i64"
	.space 1
	.ascii "mod_param_u16_u16"
	.space 1
	.ascii "mod_var_u16_u16"
	.space 1
	.ascii "mod_param_u16_u64"
	.space 1
	.ascii "mod_var_u16_u64"
	.space 1
	.ascii "div_param_u16_i8"
	.space 1
	.ascii "div_var_u16_i8"
	.space 1
	.ascii "div_param_u16_i32"
	.space 1
	.ascii "div_var_u16_i32"
	.space 1
	.ascii "div_param_u16_u8"
	.space 1
	.ascii "div_var_u16_u8"
	.space 1
	.ascii "div_param_u16_u32"
	.space 1
	.ascii "div_var_u16_u32"
	.space 1
	.ascii "div_param_u16_i16"
	.space 1
	.ascii "div_var_u16_i16"
	.space 1
	.ascii "div_param_u16_i64"
	.space 1
	.ascii "div_var_u16_i64"
	.space 1
	.ascii "div_param_u16_u16"
	.space 1
	.ascii "div_var_u16_u16"
	.space 1
	.ascii "div_param_u16_u64"
	.space 1
	.ascii "div_var_u16_u64"
	.space 1
	.ascii "mult_param_u16_i8"
	.space 1
	.ascii "mult_var_u16_i8"
	.space 1
	.ascii "mult_param_u16_i32"
	.space 1
	.ascii "mult_var_u16_i32"
	.space 1
	.ascii "mult_param_u16_u8"
	.space 1
	.ascii "mult_var_u16_u8"
	.space 1
	.ascii "mult_param_u16_u32"
	.space 1
	.ascii "mult_var_u16_u32"
	.space 1
	.ascii "mult_param_u16_i16"
	.space 1
	.ascii "mult_var_u16_i16"
	.space 1
	.ascii "mult_param_u16_i64"
	.space 1
	.ascii "mult_var_u16_i64"
	.space 1
	.ascii "mult_param_u16_u16"
	.space 1
	.ascii "mult_var_u16_u16"
	.space 1
	.ascii "mult_param_u16_u64"
	.space 1
	.ascii "mult_var_u16_u64"
	.space 1
	.ascii "xor_param_u16_i8"
	.space 1
	.ascii "xor_var_u16_i8"
	.space 1
	.ascii "xor_param_u16_i32"
	.space 1
	.ascii "xor_var_u16_i32"
	.space 1
	.ascii "xor_param_u16_u8"
	.space 1
	.ascii "xor_var_u16_u8"
	.space 1
	.ascii "xor_param_u16_u32"
	.space 1
	.ascii "xor_var_u16_u32"
	.space 1
	.ascii "xor_param_u16_i16"
	.space 1
	.ascii "xor_var_u16_i16"
	.space 1
	.ascii "xor_param_u16_i64"
	.space 1
	.ascii "xor_var_u16_i64"
	.space 1
	.ascii "xor_param_u16_u16"
	.space 1
	.ascii "xor_var_u16_u16"
	.space 1
	.ascii "xor_param_u16_u64"
	.space 1
	.ascii "xor_var_u16_u64"
	.space 1
	.ascii "or_param_u16_i8"
	.space 1
	.ascii "or_var_u16_i8"
	.space 1
	.ascii "or_param_u16_i32"
	.space 1
	.ascii "or_var_u16_i32"
	.space 1
	.ascii "or_param_u16_u8"
	.space 1
	.ascii "or_var_u16_u8"
	.space 1
	.ascii "or_param_u16_u32"
	.space 1
	.ascii "or_var_u16_u32"
	.space 1
	.ascii "or_param_u16_i16"
	.space 1
	.ascii "or_var_u16_i16"
	.space 1
	.ascii "or_param_u16_i64"
	.space 1
	.ascii "or_var_u16_i64"
	.space 1
	.ascii "or_param_u16_u16"
	.space 1
	.ascii "or_var_u16_u16"
	.space 1
	.ascii "or_param_u16_u64"
	.space 1
	.ascii "or_var_u16_u64"
	.space 1
	.ascii "and_param_u16_i8"
	.space 1
	.ascii "and_var_u16_i8"
	.space 1
	.ascii "and_param_u16_i32"
	.space 1
	.ascii "and_var_u16_i32"
	.space 1
	.ascii "and_param_u16_u8"
	.space 1
	.ascii "and_var_u16_u8"
	.space 1
	.ascii "and_param_u16_u32"
	.space 1
	.ascii "and_var_u16_u32"
	.space 1
	.ascii "and_param_u16_i16"
	.space 1
	.ascii "and_var_u16_i16"
	.space 1
	.ascii "and_param_u16_i64"
	.space 1
	.ascii "and_var_u16_i64"
	.space 1
	.ascii "and_param_u16_u16"
	.space 1
	.ascii "and_var_u16_u16"
	.space 1
	.ascii "and_param_u16_u64"
	.space 1
	.ascii "and_var_u16_u64"
	.space 1
	.ascii "sub_param_u16_i8"
	.space 1
	.ascii "sub_var_u16_i8"
	.space 1
	.ascii "sub_param_u16_i32"
	.space 1
	.ascii "sub_var_u16_i32"
	.space 1
	.ascii "sub_param_u16_u8"
	.space 1
	.ascii "sub_var_u16_u8"
	.space 1
	.ascii "sub_param_u16_u32"
	.space 1
	.ascii "sub_var_u16_u32"
	.space 1
	.ascii "sub_param_u16_i16"
	.space 1
	.ascii "sub_var_u16_i16"
	.space 1
	.ascii "sub_param_u16_i64"
	.space 1
	.ascii "sub_var_u16_i64"
	.space 1
	.ascii "sub_param_u16_u16"
	.space 1
	.ascii "sub_var_u16_u16"
	.space 1
	.ascii "sub_param_u16_u64"
	.space 1
	.ascii "sub_var_u16_u64"
	.space 1
	.ascii "add_param_u16_i8"
	.space 1
	.ascii "add_var_u16_i8"
	.space 1
	.ascii "add_param_u16_i32"
	.space 1
	.ascii "add_var_u16_i32"
	.space 1
	.ascii "add_param_u16_u8"
	.space 1
	.ascii "add_var_u16_u8"
	.space 1
	.ascii "add_param_u16_u32"
	.space 1
	.ascii "add_var_u16_u32"
	.space 1
	.ascii "add_param_u16_i16"
	.space 1
	.ascii "add_var_u16_i16"
	.space 1
	.ascii "add_param_u16_i64"
	.space 1
	.ascii "add_var_u16_i64"
	.space 1
	.ascii "add_param_u16_u16"
	.space 1
	.ascii "add_var_u16_u16"
	.space 1
	.ascii "add_param_u16_u64"
	.space 1
	.ascii "add_var_u16_u64"
	.space 1
	.ascii "ret_param_u16_i8"
	.space 1
	.ascii "ret_param_u16_i32"
	.space 1
	.ascii "ret_param_u16_u8"
	.space 1
	.ascii "ret_param_u16_u32"
	.space 1
	.ascii "ret_param_u16_i16"
	.space 1
	.ascii "ret_param_u16_i64"
	.space 1
	.ascii "ret_param_u16_u16"
	.space 1
	.ascii "ret_param_u16_u64"
	.space 1
	.ascii "ret_const_i16"
	.space 1
	.ascii "ret_var_i16"
	.space 1
	.ascii "mult_param_f32_f32"
	.space 1
	.ascii "mult_var_f32_f32"
	.space 1
	.ascii "sub_param_f32_f32"
	.space 1
	.ascii "sub_var_f32_f32"
	.space 1
	.ascii "add_param_f32_f32"
	.space 1
	.ascii "add_var_f32_f32"
	.space 1
	.ascii "ret_f32"
	.space 1
	.ascii "mod_param_u64_i8"
	.space 1
	.ascii "mod_var_u64_i8"
	.space 1
	.ascii "mod_param_u64_i32"
	.space 1
	.ascii "mod_var_u64_i32"
	.space 1
	.ascii "mod_param_u64_u8"
	.space 1
	.ascii "mod_var_u64_u8"
	.space 1
	.ascii "mod_param_u64_u32"
	.space 1
	.ascii "mod_var_u64_u32"
	.space 1
	.ascii "mod_param_u64_i16"
	.space 1
	.ascii "mod_var_u64_i16"
	.space 1
	.ascii "mod_param_u64_i64"
	.space 1
	.ascii "mod_var_u64_i64"
	.space 1
	.ascii "mod_param_u64_u16"
	.space 1
	.ascii "mod_var_u64_u16"
	.space 1
	.ascii "mod_param_u64_u64"
	.space 1
	.ascii "mod_var_u64_u64"
	.space 1
	.ascii "div_param_u64_i8"
	.space 1
	.ascii "div_var_u64_i8"
	.space 1
	.ascii "div_param_u64_i32"
	.space 1
	.ascii "div_var_u64_i32"
	.space 1
	.ascii "div_param_u64_u8"
	.space 1
	.ascii "div_var_u64_u8"
	.space 1
	.ascii "div_param_u64_u32"
	.space 1
	.ascii "div_var_u64_u32"
	.space 1
	.ascii "div_param_u64_i16"
	.space 1
	.ascii "div_var_u64_i16"
	.space 1
	.ascii "div_param_u64_i64"
	.space 1
	.ascii "div_var_u64_i64"
	.space 1
	.ascii "div_param_u64_u16"
	.space 1
	.ascii "div_var_u64_u16"
	.space 1
	.ascii "div_param_u64_u64"
	.space 1
	.ascii "div_var_u64_u64"
	.space 1
	.ascii "mult_param_u64_i8"
	.space 1
	.ascii "mult_var_u64_i8"
	.space 1
	.ascii "mult_param_u64_i32"
	.space 1
	.ascii "mult_var_u64_i32"
	.space 1
	.ascii "mult_param_u64_u8"
	.space 1
	.ascii "mult_var_u64_u8"
	.space 1
	.ascii "mult_param_u64_u32"
	.space 1
	.ascii "mult_var_u64_u32"
	.space 1
	.ascii "mult_param_u64_i16"
	.space 1
	.ascii "mult_var_u64_i16"
	.space 1
	.ascii "mult_param_u64_i64"
	.space 1
	.ascii "mult_var_u64_i64"
	.space 1
	.ascii "mult_param_u64_u16"
	.space 1
	.ascii "mult_var_u64_u16"
	.space 1
	.ascii "mult_param_u64_u64"
	.space 1
	.ascii "mult_var_u64_u64"
	.space 1
	.ascii "xor_param_u64_i8"
	.space 1
	.ascii "xor_var_u64_i8"
	.space 1
	.ascii "xor_param_u64_i32"
	.space 1
	.ascii "xor_var_u64_i32"
	.space 1
	.ascii "xor_param_u64_u8"
	.space 1
	.ascii "xor_var_u64_u8"
	.space 1
	.ascii "xor_param_u64_u32"
	.space 1
	.ascii "xor_var_u64_u32"
	.space 1
	.ascii "xor_param_u64_i16"
	.space 1
	.ascii "xor_var_u64_i16"
	.space 1
	.ascii "xor_param_u64_i64"
	.space 1
	.ascii "xor_var_u64_i64"
	.space 1
	.ascii "xor_param_u64_u16"
	.space 1
	.ascii "xor_var_u64_u16"
	.space 1
	.ascii "xor_param_u64_u64"
	.space 1
	.ascii "xor_var_u64_u64"
	.space 1
	.ascii "or_param_u64_i8"
	.space 1
	.ascii "or_var_u64_i8"
	.space 1
	.ascii "or_param_u64_i32"
	.space 1
	.ascii "or_var_u64_i32"
	.space 1
	.ascii "or_param_u64_u8"
	.space 1
	.ascii "or_var_u64_u8"
	.space 1
	.ascii "or_param_u64_u32"
	.space 1
	.ascii "or_var_u64_u32"
	.space 1
	.ascii "or_param_u64_i16"
	.space 1
	.ascii "or_var_u64_i16"
	.space 1
	.ascii "or_param_u64_i64"
	.space 1
	.ascii "or_var_u64_i64"
	.space 1
	.ascii "or_param_u64_u16"
	.space 1
	.ascii "or_var_u64_u16"
	.space 1
	.ascii "or_param_u64_u64"
	.space 1
	.ascii "or_var_u64_u64"
	.space 1
	.ascii "and_param_u64_i8"
	.space 1
	.ascii "and_var_u64_i8"
	.space 1
	.ascii "and_param_u64_i32"
	.space 1
	.ascii "and_var_u64_i32"
	.space 1
	.ascii "and_param_u64_u8"
	.space 1
	.ascii "and_var_u64_u8"
	.space 1
	.ascii "and_param_u64_u32"
	.space 1
	.ascii "and_var_u64_u32"
	.space 1
	.ascii "and_param_u64_i16"
	.space 1
	.ascii "and_var_u64_i16"
	.space 1
	.ascii "and_param_u64_i64"
	.space 1
	.ascii "and_var_u64_i64"
	.space 1
	.ascii "and_param_u64_u16"
	.space 1
	.ascii "and_var_u64_u16"
	.space 1
	.ascii "and_param_u64_u64"
	.space 1
	.ascii "and_var_u64_u64"
	.space 1
	.ascii "sub_param_u64_i8"
	.space 1
	.ascii "sub_var_u64_i8"
	.space 1
	.ascii "sub_param_u64_i32"
	.space 1
	.ascii "sub_var_u64_i32"
	.space 1
	.ascii "sub_param_u64_u8"
	.space 1
	.ascii "sub_var_u64_u8"
	.space 1
	.ascii "sub_param_u64_u32"
	.space 1
	.ascii "sub_var_u64_u32"
	.space 1
	.ascii "sub_param_u64_i16"
	.space 1
	.ascii "sub_var_u64_i16"
	.space 1
	.ascii "sub_param_u64_i64"
	.space 1
	.ascii "sub_var_u64_i64"
	.space 1
	.ascii "sub_param_u64_u16"
	.space 1
	.ascii "sub_var_u64_u16"
	.space 1
	.ascii "sub_param_u64_u64"
	.space 1
	.ascii "sub_var_u64_u64"
	.space 1
	.ascii "add_param_u64_i8"
	.space 1
	.ascii "add_var_u64_i8"
	.space 1
	.ascii "add_param_u64_i32"
	.space 1
	.ascii "add_var_u64_i32"
	.space 1
	.ascii "add_param_u64_u8"
	.space 1
	.ascii "add_var_u64_u8"
	.space 1
	.ascii "add_param_u64_u32"
	.space 1
	.ascii "add_var_u64_u32"
	.space 1
	.ascii "add_param_u64_i16"
	.space 1
	.ascii "add_var_u64_i16"
	.space 1
	.ascii "add_param_u64_i64"
	.space 1
	.ascii "add_var_u64_i64"
	.space 1
	.ascii "add_param_u64_u16"
	.space 1
	.ascii "add_var_u64_u16"
	.space 1
	.ascii "add_param_u64_u64"
	.space 1
	.ascii "add_var_u64_u64"
	.space 1
	.ascii "ret_param_u64_i8"
	.space 1
	.ascii "ret_param_u64_i32"
	.space 1
	.ascii "ret_param_u64_u8"
	.space 1
	.ascii "ret_param_u64_u32"
	.space 1
	.ascii "ret_param_u64_i16"
	.space 1
	.ascii "ret_param_u64_i64"
	.space 1
	.ascii "ret_param_u64_u16"
	.space 1
	.ascii "ret_param_u64_u64"
	.space 1
	.ascii "ret_const_i64"
	.space 1
	.ascii "ret_var_i64"
	.space 5
	.quad	_A_M3
	.quad	_L_1
	.quad	_A__mod_param_i8_i8
	.quad	_L_1+5
	.quad	_A__mod_var_i8_i8
	.quad	_L_1+21
	.quad	_A__mod_param_i8_i32
	.quad	_L_1+35
	.quad	_A__mod_var_i8_i32
	.quad	_L_1+52
	.quad	_A__mod_param_i8_u8
	.quad	_L_1+67
	.quad	_A__mod_var_i8_u8
	.quad	_L_1+83
	.quad	_A__mod_param_i8_u32
	.quad	_L_1+97
	.quad	_A__mod_var_i8_u32
	.quad	_L_1+114
	.quad	_A__mod_param_i8_i16
	.quad	_L_1+129
	.quad	_A__mod_var_i8_i16
	.quad	_L_1+146
	.quad	_A__mod_param_i8_i64
	.quad	_L_1+161
	.quad	_A__mod_var_i8_i64
	.quad	_L_1+178
	.quad	_A__mod_param_i8_u16
	.quad	_L_1+193
	.quad	_A__mod_var_i8_u16
	.quad	_L_1+210
	.quad	_A__mod_param_i8_u64
	.quad	_L_1+225
	.quad	_A__mod_var_i8_u64
	.quad	_L_1+242
	.quad	_A__div_param_i8_i8
	.quad	_L_1+257
	.quad	_A__div_var_i8_i8
	.quad	_L_1+273
	.quad	_A__div_param_i8_i32
	.quad	_L_1+287
	.quad	_A__div_var_i8_i32
	.quad	_L_1+304
	.quad	_A__div_param_i8_u8
	.quad	_L_1+319
	.quad	_A__div_var_i8_u8
	.quad	_L_1+335
	.quad	_A__div_param_i8_u32
	.quad	_L_1+349
	.quad	_A__div_var_i8_u32
	.quad	_L_1+366
	.quad	_A__div_param_i8_i16
	.quad	_L_1+381
	.quad	_A__div_var_i8_i16
	.quad	_L_1+398
	.quad	_A__div_param_i8_i64
	.quad	_L_1+413
	.quad	_A__div_var_i8_i64
	.quad	_L_1+430
	.quad	_A__div_param_i8_u16
	.quad	_L_1+445
	.quad	_A__div_var_i8_u16
	.quad	_L_1+462
	.quad	_A__div_param_i8_u64
	.quad	_L_1+477
	.quad	_A__div_var_i8_u64
	.quad	_L_1+494
	.quad	_A__mult_param_i8_i8
	.quad	_L_1+509
	.quad	_A__mult_var_i8_i8
	.quad	_L_1+526
	.quad	_A__mult_param_i8_i32
	.quad	_L_1+541
	.quad	_A__mult_var_i8_i32
	.quad	_L_1+559
	.quad	_A__mult_param_i8_u8
	.quad	_L_1+575
	.quad	_A__mult_var_i8_u8
	.quad	_L_1+592
	.quad	_A__mult_param_i8_u32
	.quad	_L_1+607
	.quad	_A__mult_var_i8_u32
	.quad	_L_1+625
	.quad	_A__mult_param_i8_i16
	.quad	_L_1+641
	.quad	_A__mult_var_i8_i16
	.quad	_L_1+659
	.quad	_A__mult_param_i8_i64
	.quad	_L_1+675
	.quad	_A__mult_var_i8_i64
	.quad	_L_1+693
	.quad	_A__mult_param_i8_u16
	.quad	_L_1+709
	.quad	_A__mult_var_i8_u16
	.quad	_L_1+727
	.quad	_A__mult_param_i8_u64
	.quad	_L_1+743
	.quad	_A__mult_var_i8_u64
	.quad	_L_1+761
	.quad	_A__xor_param_i8_i8
	.quad	_L_1+777
	.quad	_A__xor_var_i8_i8
	.quad	_L_1+793
	.quad	_A__xor_param_i8_i32
	.quad	_L_1+807
	.quad	_A__xor_var_i8_i32
	.quad	_L_1+824
	.quad	_A__xor_param_i8_u8
	.quad	_L_1+839
	.quad	_A__xor_var_i8_u8
	.quad	_L_1+855
	.quad	_A__xor_param_i8_u32
	.quad	_L_1+869
	.quad	_A__xor_var_i8_u32
	.quad	_L_1+886
	.quad	_A__xor_param_i8_i16
	.quad	_L_1+901
	.quad	_A__xor_var_i8_i16
	.quad	_L_1+918
	.quad	_A__xor_param_i8_i64
	.quad	_L_1+933
	.quad	_A__xor_var_i8_i64
	.quad	_L_1+950
	.quad	_A__xor_param_i8_u16
	.quad	_L_1+965
	.quad	_A__xor_var_i8_u16
	.quad	_L_1+982
	.quad	_A__xor_param_i8_u64
	.quad	_L_1+997
	.quad	_A__xor_var_i8_u64
	.quad	_L_1+1014
	.quad	_A__or_param_i8_i8
	.quad	_L_1+1029
	.quad	_A__or_var_i8_i8
	.quad	_L_1+1044
	.quad	_A__or_param_i8_i32
	.quad	_L_1+1057
	.quad	_A__or_var_i8_i32
	.quad	_L_1+1073
	.quad	_A__or_param_i8_u8
	.quad	_L_1+1087
	.quad	_A__or_var_i8_u8
	.quad	_L_1+1102
	.quad	_A__or_param_i8_u32
	.quad	_L_1+1115
	.quad	_A__or_var_i8_u32
	.quad	_L_1+1131
	.quad	_A__or_param_i8_i16
	.quad	_L_1+1145
	.quad	_A__or_var_i8_i16
	.quad	_L_1+1161
	.quad	_A__or_param_i8_i64
	.quad	_L_1+1175
	.quad	_A__or_var_i8_i64
	.quad	_L_1+1191
	.quad	_A__or_param_i8_u16
	.quad	_L_1+1205
	.quad	_A__or_var_i8_u16
	.quad	_L_1+1221
	.quad	_A__or_param_i8_u64
	.quad	_L_1+1235
	.quad	_A__or_var_i8_u64
	.quad	_L_1+1251
	.quad	_A__and_param_i8_i8
	.quad	_L_1+1265
	.quad	_A__and_var_i8_i8
	.quad	_L_1+1281
	.quad	_A__and_param_i8_i32
	.quad	_L_1+1295
	.quad	_A__and_var_i8_i32
	.quad	_L_1+1312
	.quad	_A__and_param_i8_u8
	.quad	_L_1+1327
	.quad	_A__and_var_i8_u8
	.quad	_L_1+1343
	.quad	_A__and_param_i8_u32
	.quad	_L_1+1357
	.quad	_A__and_var_i8_u32
	.quad	_L_1+1374
	.quad	_A__and_param_i8_i16
	.quad	_L_1+1389
	.quad	_A__and_var_i8_i16
	.quad	_L_1+1406
	.quad	_A__and_param_i8_i64
	.quad	_L_1+1421
	.quad	_A__and_var_i8_i64
	.quad	_L_1+1438
	.quad	_A__and_param_i8_u16
	.quad	_L_1+1453
	.quad	_A__and_var_i8_u16
	.quad	_L_1+1470
	.quad	_A__and_param_i8_u64
	.quad	_L_1+1485
	.quad	_A__and_var_i8_u64
	.quad	_L_1+1502
	.quad	_A__sub_param_i8_i8
	.quad	_L_1+1517
	.quad	_A__sub_var_i8_i8
	.quad	_L_1+1533
	.quad	_A__sub_param_i8_i32
	.quad	_L_1+1547
	.quad	_A__sub_var_i8_i32
	.quad	_L_1+1564
	.quad	_A__sub_param_i8_u8
	.quad	_L_1+1579
	.quad	_A__sub_var_i8_u8
	.quad	_L_1+1595
	.quad	_A__sub_param_i8_u32
	.quad	_L_1+1609
	.quad	_A__sub_var_i8_u32
	.quad	_L_1+1626
	.quad	_A__sub_param_i8_i16
	.quad	_L_1+1641
	.quad	_A__sub_var_i8_i16
	.quad	_L_1+1658
	.quad	_A__sub_param_i8_i64
	.quad	_L_1+1673
	.quad	_A__sub_var_i8_i64
	.quad	_L_1+1690
	.quad	_A__sub_param_i8_u16
	.quad	_L_1+1705
	.quad	_A__sub_var_i8_u16
	.quad	_L_1+1722
	.quad	_A__sub_param_i8_u64
	.quad	_L_1+1737
	.quad	_A__sub_var_i8_u64
	.quad	_L_1+1754
	.quad	_A__add_param_i8_i8
	.quad	_L_1+1769
	.quad	_A__add_var_i8_i8
	.quad	_L_1+1785
	.quad	_A__add_param_i8_i32
	.quad	_L_1+1799
	.quad	_A__add_var_i8_i32
	.quad	_L_1+1816
	.quad	_A__add_param_i8_u8
	.quad	_L_1+1831
	.quad	_A__add_var_i8_u8
	.quad	_L_1+1847
	.quad	_A__add_param_i8_u32
	.quad	_L_1+1861
	.quad	_A__add_var_i8_u32
	.quad	_L_1+1878
	.quad	_A__add_param_i8_i16
	.quad	_L_1+1893
	.quad	_A__add_var_i8_i16
	.quad	_L_1+1910
	.quad	_A__add_param_i8_i64
	.quad	_L_1+1925
	.quad	_A__add_var_i8_i64
	.quad	_L_1+1942
	.quad	_A__add_param_i8_u16
	.quad	_L_1+1957
	.quad	_A__add_var_i8_u16
	.quad	_L_1+1974
	.quad	_A__add_param_i8_u64
	.quad	_L_1+1989
	.quad	_A__add_var_i8_u64
	.quad	_L_1+2006
	.quad	_A__ret_param_i8_i8
	.quad	_L_1+2021
	.quad	_A__ret_param_i8_i32
	.quad	_L_1+2037
	.quad	_A__ret_param_i8_u8
	.quad	_L_1+2054
	.quad	_A__ret_param_i8_u32
	.quad	_L_1+2070
	.quad	_A__ret_param_i8_i16
	.quad	_L_1+2087
	.quad	_A__ret_param_i8_i64
	.quad	_L_1+2104
	.quad	_A__ret_param_i8_u16
	.quad	_L_1+2121
	.quad	_A__ret_param_i8_u64
	.quad	_L_1+2138
	.quad	_A__ret_const_u8
	.quad	_L_1+2155
	.quad	_A__ret_var_u8
	.quad	_L_1+2168
	.quad	_A__mod_param_i32_i8
	.quad	_L_1+2179
	.quad	_A__mod_var_i32_i8
	.quad	_L_1+2196
	.quad	_A__mod_param_i32_i32
	.quad	_L_1+2211
	.quad	_A__mod_var_i32_i32
	.quad	_L_1+2229
	.quad	_A__mod_param_i32_u8
	.quad	_L_1+2245
	.quad	_A__mod_var_i32_u8
	.quad	_L_1+2262
	.quad	_A__mod_param_i32_u32
	.quad	_L_1+2277
	.quad	_A__mod_var_i32_u32
	.quad	_L_1+2295
	.quad	_A__mod_param_i32_i16
	.quad	_L_1+2311
	.quad	_A__mod_var_i32_i16
	.quad	_L_1+2329
	.quad	_A__mod_param_i32_i64
	.quad	_L_1+2345
	.quad	_A__mod_var_i32_i64
	.quad	_L_1+2363
	.quad	_A__mod_param_i32_u16
	.quad	_L_1+2379
	.quad	_A__mod_var_i32_u16
	.quad	_L_1+2397
	.quad	_A__mod_param_i32_u64
	.quad	_L_1+2413
	.quad	_A__mod_var_i32_u64
	.quad	_L_1+2431
	.quad	_A__div_param_i32_i8
	.quad	_L_1+2447
	.quad	_A__div_var_i32_i8
	.quad	_L_1+2464
	.quad	_A__div_param_i32_i32
	.quad	_L_1+2479
	.quad	_A__div_var_i32_i32
	.quad	_L_1+2497
	.quad	_A__div_param_i32_u8
	.quad	_L_1+2513
	.quad	_A__div_var_i32_u8
	.quad	_L_1+2530
	.quad	_A__div_param_i32_u32
	.quad	_L_1+2545
	.quad	_A__div_var_i32_u32
	.quad	_L_1+2563
	.quad	_A__div_param_i32_i16
	.quad	_L_1+2579
	.quad	_A__div_var_i32_i16
	.quad	_L_1+2597
	.quad	_A__div_param_i32_i64
	.quad	_L_1+2613
	.quad	_A__div_var_i32_i64
	.quad	_L_1+2631
	.quad	_A__div_param_i32_u16
	.quad	_L_1+2647
	.quad	_A__div_var_i32_u16
	.quad	_L_1+2665
	.quad	_A__div_param_i32_u64
	.quad	_L_1+2681
	.quad	_A__div_var_i32_u64
	.quad	_L_1+2699
	.quad	_A__mult_param_i32_i8
	.quad	_L_1+2715
	.quad	_A__mult_var_i32_i8
	.quad	_L_1+2733
	.quad	_A__mult_param_i32_i32
	.quad	_L_1+2749
	.quad	_A__mult_var_i32_i32
	.quad	_L_1+2768
	.quad	_A__mult_param_i32_u8
	.quad	_L_1+2785
	.quad	_A__mult_var_i32_u8
	.quad	_L_1+2803
	.quad	_A__mult_param_i32_u32
	.quad	_L_1+2819
	.quad	_A__mult_var_i32_u32
	.quad	_L_1+2838
	.quad	_A__mult_param_i32_i16
	.quad	_L_1+2855
	.quad	_A__mult_var_i32_i16
	.quad	_L_1+2874
	.quad	_A__mult_param_i32_i64
	.quad	_L_1+2891
	.quad	_A__mult_var_i32_i64
	.quad	_L_1+2910
	.quad	_A__mult_param_i32_u16
	.quad	_L_1+2927
	.quad	_A__mult_var_i32_u16
	.quad	_L_1+2946
	.quad	_A__mult_param_i32_u64
	.quad	_L_1+2963
	.quad	_A__mult_var_i32_u64
	.quad	_L_1+2982
	.quad	_A__xor_param_i32_i8
	.quad	_L_1+2999
	.quad	_A__xor_var_i32_i8
	.quad	_L_1+3016
	.quad	_A__xor_param_i32_i32
	.quad	_L_1+3031
	.quad	_A__xor_var_i32_i32
	.quad	_L_1+3049
	.quad	_A__xor_param_i32_u8
	.quad	_L_1+3065
	.quad	_A__xor_var_i32_u8
	.quad	_L_1+3082
	.quad	_A__xor_param_i32_u32
	.quad	_L_1+3097
	.quad	_A__xor_var_i32_u32
	.quad	_L_1+3115
	.quad	_A__xor_param_i32_i16
	.quad	_L_1+3131
	.quad	_A__xor_var_i32_i16
	.quad	_L_1+3149
	.quad	_A__xor_param_i32_i64
	.quad	_L_1+3165
	.quad	_A__xor_var_i32_i64
	.quad	_L_1+3183
	.quad	_A__xor_param_i32_u16
	.quad	_L_1+3199
	.quad	_A__xor_var_i32_u16
	.quad	_L_1+3217
	.quad	_A__xor_param_i32_u64
	.quad	_L_1+3233
	.quad	_A__xor_var_i32_u64
	.quad	_L_1+3251
	.quad	_A__or_param_i32_i8
	.quad	_L_1+3267
	.quad	_A__or_var_i32_i8
	.quad	_L_1+3283
	.quad	_A__or_param_i32_i32
	.quad	_L_1+3297
	.quad	_A__or_var_i32_i32
	.quad	_L_1+3314
	.quad	_A__or_param_i32_u8
	.quad	_L_1+3329
	.quad	_A__or_var_i32_u8
	.quad	_L_1+3345
	.quad	_A__or_param_i32_u32
	.quad	_L_1+3359
	.quad	_A__or_var_i32_u32
	.quad	_L_1+3376
	.quad	_A__or_param_i32_i16
	.quad	_L_1+3391
	.quad	_A__or_var_i32_i16
	.quad	_L_1+3408
	.quad	_A__or_param_i32_i64
	.quad	_L_1+3423
	.quad	_A__or_var_i32_i64
	.quad	_L_1+3440
	.quad	_A__or_param_i32_u16
	.quad	_L_1+3455
	.quad	_A__or_var_i32_u16
	.quad	_L_1+3472
	.quad	_A__or_param_i32_u64
	.quad	_L_1+3487
	.quad	_A__or_var_i32_u64
	.quad	_L_1+3504
	.quad	_A__and_param_i32_i8
	.quad	_L_1+3519
	.quad	_A__and_var_i32_i8
	.quad	_L_1+3536
	.quad	_A__and_param_i32_i32
	.quad	_L_1+3551
	.quad	_A__and_var_i32_i32
	.quad	_L_1+3569
	.quad	_A__and_param_i32_u8
	.quad	_L_1+3585
	.quad	_A__and_var_i32_u8
	.quad	_L_1+3602
	.quad	_A__and_param_i32_u32
	.quad	_L_1+3617
	.quad	_A__and_var_i32_u32
	.quad	_L_1+3635
	.quad	_A__and_param_i32_i16
	.quad	_L_1+3651
	.quad	_A__and_var_i32_i16
	.quad	_L_1+3669
	.quad	_A__and_param_i32_i64
	.quad	_L_1+3685
	.quad	_A__and_var_i32_i64
	.quad	_L_1+3703
	.quad	_A__and_param_i32_u16
	.quad	_L_1+3719
	.quad	_A__and_var_i32_u16
	.quad	_L_1+3737
	.quad	_A__and_param_i32_u64
	.quad	_L_1+3753
	.quad	_A__and_var_i32_u64
	.quad	_L_1+3771
	.quad	_A__sub_param_i32_i8
	.quad	_L_1+3787
	.quad	_A__sub_var_i32_i8
	.quad	_L_1+3804
	.quad	_A__sub_param_i32_i32
	.quad	_L_1+3819
	.quad	_A__sub_var_i32_i32
	.quad	_L_1+3837
	.quad	_A__sub_param_i32_u8
	.quad	_L_1+3853
	.quad	_A__sub_var_i32_u8
	.quad	_L_1+3870
	.quad	_A__sub_param_i32_u32
	.quad	_L_1+3885
	.quad	_A__sub_var_i32_u32
	.quad	_L_1+3903
	.quad	_A__sub_param_i32_i16
	.quad	_L_1+3919
	.quad	_A__sub_var_i32_i16
	.quad	_L_1+3937
	.quad	_A__sub_param_i32_i64
	.quad	_L_1+3953
	.quad	_A__sub_var_i32_i64
	.quad	_L_1+3971
	.quad	_A__sub_param_i32_u16
	.quad	_L_1+3987
	.quad	_A__sub_var_i32_u16
	.quad	_L_1+4005
	.quad	_A__sub_param_i32_u64
	.quad	_L_1+4021
	.quad	_A__sub_var_i32_u64
	.quad	_L_1+4039
	.quad	_A__add_param_i32_i8
	.quad	_L_1+4055
	.quad	_A__add_var_i32_i8
	.quad	_L_1+4072
	.quad	_A__add_param_i32_i32
	.quad	_L_1+4087
	.quad	_A__add_var_i32_i32
	.quad	_L_1+4105
	.quad	_A__add_param_i32_u8
	.quad	_L_1+4121
	.quad	_A__add_var_i32_u8
	.quad	_L_1+4138
	.quad	_A__add_param_i32_u32
	.quad	_L_1+4153
	.quad	_A__add_var_i32_u32
	.quad	_L_1+4171
	.quad	_A__add_param_i32_i16
	.quad	_L_1+4187
	.quad	_A__add_var_i32_i16
	.quad	_L_1+4205
	.quad	_A__add_param_i32_i64
	.quad	_L_1+4221
	.quad	_A__add_var_i32_i64
	.quad	_L_1+4239
	.quad	_A__add_param_i32_u16
	.quad	_L_1+4255
	.quad	_A__add_var_i32_u16
	.quad	_L_1+4273
	.quad	_A__add_param_i32_u64
	.quad	_L_1+4289
	.quad	_A__add_var_i32_u64
	.quad	_L_1+4307
	.quad	_A__ret_param_i32_i8
	.quad	_L_1+4323
	.quad	_A__ret_param_i32_i32
	.quad	_L_1+4340
	.quad	_A__ret_param_i32_u8
	.quad	_L_1+4358
	.quad	_A__ret_param_i32_u32
	.quad	_L_1+4375
	.quad	_A__ret_param_i32_i16
	.quad	_L_1+4393
	.quad	_A__ret_param_i32_i64
	.quad	_L_1+4411
	.quad	_A__ret_param_i32_u16
	.quad	_L_1+4429
	.quad	_A__ret_param_i32_u64
	.quad	_L_1+4447
	.quad	_A__ret_const_u32
	.quad	_L_1+4465
	.quad	_A__ret_var_u32
	.quad	_L_1+4479
	.quad	_A__mod_param_u8_i8
	.quad	_L_1+4491
	.quad	_A__mod_var_u8_i8
	.quad	_L_1+4507
	.quad	_A__mod_param_u8_i32
	.quad	_L_1+4521
	.quad	_A__mod_var_u8_i32
	.quad	_L_1+4538
	.quad	_A__mod_param_u8_u8
	.quad	_L_1+4553
	.quad	_A__mod_var_u8_u8
	.quad	_L_1+4569
	.quad	_A__mod_param_u8_u32
	.quad	_L_1+4583
	.quad	_A__mod_var_u8_u32
	.quad	_L_1+4600
	.quad	_A__mod_param_u8_i16
	.quad	_L_1+4615
	.quad	_A__mod_var_u8_i16
	.quad	_L_1+4632
	.quad	_A__mod_param_u8_i64
	.quad	_L_1+4647
	.quad	_A__mod_var_u8_i64
	.quad	_L_1+4664
	.quad	_A__mod_param_u8_u16
	.quad	_L_1+4679
	.quad	_A__mod_var_u8_u16
	.quad	_L_1+4696
	.quad	_A__mod_param_u8_u64
	.quad	_L_1+4711
	.quad	_A__mod_var_u8_u64
	.quad	_L_1+4728
	.quad	_A__div_param_u8_i8
	.quad	_L_1+4743
	.quad	_A__div_var_u8_i8
	.quad	_L_1+4759
	.quad	_A__div_param_u8_i32
	.quad	_L_1+4773
	.quad	_A__div_var_u8_i32
	.quad	_L_1+4790
	.quad	_A__div_param_u8_u8
	.quad	_L_1+4805
	.quad	_A__div_var_u8_u8
	.quad	_L_1+4821
	.quad	_A__div_param_u8_u32
	.quad	_L_1+4835
	.quad	_A__div_var_u8_u32
	.quad	_L_1+4852
	.quad	_A__div_param_u8_i16
	.quad	_L_1+4867
	.quad	_A__div_var_u8_i16
	.quad	_L_1+4884
	.quad	_A__div_param_u8_i64
	.quad	_L_1+4899
	.quad	_A__div_var_u8_i64
	.quad	_L_1+4916
	.quad	_A__div_param_u8_u16
	.quad	_L_1+4931
	.quad	_A__div_var_u8_u16
	.quad	_L_1+4948
	.quad	_A__div_param_u8_u64
	.quad	_L_1+4963
	.quad	_A__div_var_u8_u64
	.quad	_L_1+4980
	.quad	_A__mult_param_u8_i8
	.quad	_L_1+4995
	.quad	_A__mult_var_u8_i8
	.quad	_L_1+5012
	.quad	_A__mult_param_u8_i32
	.quad	_L_1+5027
	.quad	_A__mult_var_u8_i32
	.quad	_L_1+5045
	.quad	_A__mult_param_u8_u8
	.quad	_L_1+5061
	.quad	_A__mult_var_u8_u8
	.quad	_L_1+5078
	.quad	_A__mult_param_u8_u32
	.quad	_L_1+5093
	.quad	_A__mult_var_u8_u32
	.quad	_L_1+5111
	.quad	_A__mult_param_u8_i16
	.quad	_L_1+5127
	.quad	_A__mult_var_u8_i16
	.quad	_L_1+5145
	.quad	_A__mult_param_u8_i64
	.quad	_L_1+5161
	.quad	_A__mult_var_u8_i64
	.quad	_L_1+5179
	.quad	_A__mult_param_u8_u16
	.quad	_L_1+5195
	.quad	_A__mult_var_u8_u16
	.quad	_L_1+5213
	.quad	_A__mult_param_u8_u64
	.quad	_L_1+5229
	.quad	_A__mult_var_u8_u64
	.quad	_L_1+5247
	.quad	_A__xor_param_u8_i8
	.quad	_L_1+5263
	.quad	_A__xor_var_u8_i8
	.quad	_L_1+5279
	.quad	_A__xor_param_u8_i32
	.quad	_L_1+5293
	.quad	_A__xor_var_u8_i32
	.quad	_L_1+5310
	.quad	_A__xor_param_u8_u8
	.quad	_L_1+5325
	.quad	_A__xor_var_u8_u8
	.quad	_L_1+5341
	.quad	_A__xor_param_u8_u32
	.quad	_L_1+5355
	.quad	_A__xor_var_u8_u32
	.quad	_L_1+5372
	.quad	_A__xor_param_u8_i16
	.quad	_L_1+5387
	.quad	_A__xor_var_u8_i16
	.quad	_L_1+5404
	.quad	_A__xor_param_u8_i64
	.quad	_L_1+5419
	.quad	_A__xor_var_u8_i64
	.quad	_L_1+5436
	.quad	_A__xor_param_u8_u16
	.quad	_L_1+5451
	.quad	_A__xor_var_u8_u16
	.quad	_L_1+5468
	.quad	_A__xor_param_u8_u64
	.quad	_L_1+5483
	.quad	_A__xor_var_u8_u64
	.quad	_L_1+5500
	.quad	_A__or_param_u8_i8
	.quad	_L_1+5515
	.quad	_A__or_var_u8_i8
	.quad	_L_1+5530
	.quad	_A__or_param_u8_i32
	.quad	_L_1+5543
	.quad	_A__or_var_u8_i32
	.quad	_L_1+5559
	.quad	_A__or_param_u8_u8
	.quad	_L_1+5573
	.quad	_A__or_var_u8_u8
	.quad	_L_1+5588
	.quad	_A__or_param_u8_u32
	.quad	_L_1+5601
	.quad	_A__or_var_u8_u32
	.quad	_L_1+5617
	.quad	_A__or_param_u8_i16
	.quad	_L_1+5631
	.quad	_A__or_var_u8_i16
	.quad	_L_1+5647
	.quad	_A__or_param_u8_i64
	.quad	_L_1+5661
	.quad	_A__or_var_u8_i64
	.quad	_L_1+5677
	.quad	_A__or_param_u8_u16
	.quad	_L_1+5691
	.quad	_A__or_var_u8_u16
	.quad	_L_1+5707
	.quad	_A__or_param_u8_u64
	.quad	_L_1+5721
	.quad	_A__or_var_u8_u64
	.quad	_L_1+5737
	.quad	_A__and_param_u8_i8
	.quad	_L_1+5751
	.quad	_A__and_var_u8_i8
	.quad	_L_1+5767
	.quad	_A__and_param_u8_i32
	.quad	_L_1+5781
	.quad	_A__and_var_u8_i32
	.quad	_L_1+5798
	.quad	_A__and_param_u8_u8
	.quad	_L_1+5813
	.quad	_A__and_var_u8_u8
	.quad	_L_1+5829
	.quad	_A__and_param_u8_u32
	.quad	_L_1+5843
	.quad	_A__and_var_u8_u32
	.quad	_L_1+5860
	.quad	_A__and_param_u8_i16
	.quad	_L_1+5875
	.quad	_A__and_var_u8_i16
	.quad	_L_1+5892
	.quad	_A__and_param_u8_i64
	.quad	_L_1+5907
	.quad	_A__and_var_u8_i64
	.quad	_L_1+5924
	.quad	_A__and_param_u8_u16
	.quad	_L_1+5939
	.quad	_A__and_var_u8_u16
	.quad	_L_1+5956
	.quad	_A__and_param_u8_u64
	.quad	_L_1+5971
	.quad	_A__and_var_u8_u64
	.quad	_L_1+5988
	.quad	_A__sub_param_u8_i8
	.quad	_L_1+6003
	.quad	_A__sub_var_u8_i8
	.quad	_L_1+6019
	.quad	_A__sub_param_u8_i32
	.quad	_L_1+6033
	.quad	_A__sub_var_u8_i32
	.quad	_L_1+6050
	.quad	_A__sub_param_u8_u8
	.quad	_L_1+6065
	.quad	_A__sub_var_u8_u8
	.quad	_L_1+6081
	.quad	_A__sub_param_u8_u32
	.quad	_L_1+6095
	.quad	_A__sub_var_u8_u32
	.quad	_L_1+6112
	.quad	_A__sub_param_u8_i16
	.quad	_L_1+6127
	.quad	_A__sub_var_u8_i16
	.quad	_L_1+6144
	.quad	_A__sub_param_u8_i64
	.quad	_L_1+6159
	.quad	_A__sub_var_u8_i64
	.quad	_L_1+6176
	.quad	_A__sub_param_u8_u16
	.quad	_L_1+6191
	.quad	_A__sub_var_u8_u16
	.quad	_L_1+6208
	.quad	_A__sub_param_u8_u64
	.quad	_L_1+6223
	.quad	_A__sub_var_u8_u64
	.quad	_L_1+6240
	.quad	_A__add_param_u8_i8
	.quad	_L_1+6255
	.quad	_A__add_var_u8_i8
	.quad	_L_1+6271
	.quad	_A__add_param_u8_i32
	.quad	_L_1+6285
	.quad	_A__add_var_u8_i32
	.quad	_L_1+6302
	.quad	_A__add_param_u8_u8
	.quad	_L_1+6317
	.quad	_A__add_var_u8_u8
	.quad	_L_1+6333
	.quad	_A__add_param_u8_u32
	.quad	_L_1+6347
	.quad	_A__add_var_u8_u32
	.quad	_L_1+6364
	.quad	_A__add_param_u8_i16
	.quad	_L_1+6379
	.quad	_A__add_var_u8_i16
	.quad	_L_1+6396
	.quad	_A__add_param_u8_i64
	.quad	_L_1+6411
	.quad	_A__add_var_u8_i64
	.quad	_L_1+6428
	.quad	_A__add_param_u8_u16
	.quad	_L_1+6443
	.quad	_A__add_var_u8_u16
	.quad	_L_1+6460
	.quad	_A__add_param_u8_u64
	.quad	_L_1+6475
	.quad	_A__add_var_u8_u64
	.quad	_L_1+6492
	.quad	_A__ret_param_u8_i8
	.quad	_L_1+6507
	.quad	_A__ret_param_u8_i32
	.quad	_L_1+6523
	.quad	_A__ret_param_u8_u8
	.quad	_L_1+6540
	.quad	_A__ret_param_u8_u32
	.quad	_L_1+6556
	.quad	_A__ret_param_u8_i16
	.quad	_L_1+6573
	.quad	_A__ret_param_u8_i64
	.quad	_L_1+6590
	.quad	_A__ret_param_u8_u16
	.quad	_L_1+6607
	.quad	_A__ret_param_u8_u64
	.quad	_L_1+6624
	.quad	_A__ret_const_i8
	.quad	_L_1+6641
	.quad	_A__ret_var_i8
	.quad	_L_1+6654
	.quad	_A__mod_param_u32_i8
	.quad	_L_1+6665
	.quad	_A__mod_var_u32_i8
	.quad	_L_1+6682
	.quad	_A__mod_param_u32_i32
	.quad	_L_1+6697
	.quad	_A__mod_var_u32_i32
	.quad	_L_1+6715
	.quad	_A__mod_param_u32_u8
	.quad	_L_1+6731
	.quad	_A__mod_var_u32_u8
	.quad	_L_1+6748
	.quad	_A__mod_param_u32_u32
	.quad	_L_1+6763
	.quad	_A__mod_var_u32_u32
	.quad	_L_1+6781
	.quad	_A__mod_param_u32_i16
	.quad	_L_1+6797
	.quad	_A__mod_var_u32_i16
	.quad	_L_1+6815
	.quad	_A__mod_param_u32_i64
	.quad	_L_1+6831
	.quad	_A__mod_var_u32_i64
	.quad	_L_1+6849
	.quad	_A__mod_param_u32_u16
	.quad	_L_1+6865
	.quad	_A__mod_var_u32_u16
	.quad	_L_1+6883
	.quad	_A__mod_param_u32_u64
	.quad	_L_1+6899
	.quad	_A__mod_var_u32_u64
	.quad	_L_1+6917
	.quad	_A__div_param_u32_i8
	.quad	_L_1+6933
	.quad	_A__div_var_u32_i8
	.quad	_L_1+6950
	.quad	_A__div_param_u32_i32
	.quad	_L_1+6965
	.quad	_A__div_var_u32_i32
	.quad	_L_1+6983
	.quad	_A__div_param_u32_u8
	.quad	_L_1+6999
	.quad	_A__div_var_u32_u8
	.quad	_L_1+7016
	.quad	_A__div_param_u32_u32
	.quad	_L_1+7031
	.quad	_A__div_var_u32_u32
	.quad	_L_1+7049
	.quad	_A__div_param_u32_i16
	.quad	_L_1+7065
	.quad	_A__div_var_u32_i16
	.quad	_L_1+7083
	.quad	_A__div_param_u32_i64
	.quad	_L_1+7099
	.quad	_A__div_var_u32_i64
	.quad	_L_1+7117
	.quad	_A__div_param_u32_u16
	.quad	_L_1+7133
	.quad	_A__div_var_u32_u16
	.quad	_L_1+7151
	.quad	_A__div_param_u32_u64
	.quad	_L_1+7167
	.quad	_A__div_var_u32_u64
	.quad	_L_1+7185
	.quad	_A__mult_param_u32_i8
	.quad	_L_1+7201
	.quad	_A__mult_var_u32_i8
	.quad	_L_1+7219
	.quad	_A__mult_param_u32_i32
	.quad	_L_1+7235
	.quad	_A__mult_var_u32_i32
	.quad	_L_1+7254
	.quad	_A__mult_param_u32_u8
	.quad	_L_1+7271
	.quad	_A__mult_var_u32_u8
	.quad	_L_1+7289
	.quad	_A__mult_param_u32_u32
	.quad	_L_1+7305
	.quad	_A__mult_var_u32_u32
	.quad	_L_1+7324
	.quad	_A__mult_param_u32_i16
	.quad	_L_1+7341
	.quad	_A__mult_var_u32_i16
	.quad	_L_1+7360
	.quad	_A__mult_param_u32_i64
	.quad	_L_1+7377
	.quad	_A__mult_var_u32_i64
	.quad	_L_1+7396
	.quad	_A__mult_param_u32_u16
	.quad	_L_1+7413
	.quad	_A__mult_var_u32_u16
	.quad	_L_1+7432
	.quad	_A__mult_param_u32_u64
	.quad	_L_1+7449
	.quad	_A__mult_var_u32_u64
	.quad	_L_1+7468
	.quad	_A__xor_param_u32_i8
	.quad	_L_1+7485
	.quad	_A__xor_var_u32_i8
	.quad	_L_1+7502
	.quad	_A__xor_param_u32_i32
	.quad	_L_1+7517
	.quad	_A__xor_var_u32_i32
	.quad	_L_1+7535
	.quad	_A__xor_param_u32_u8
	.quad	_L_1+7551
	.quad	_A__xor_var_u32_u8
	.quad	_L_1+7568
	.quad	_A__xor_param_u32_u32
	.quad	_L_1+7583
	.quad	_A__xor_var_u32_u32
	.quad	_L_1+7601
	.quad	_A__xor_param_u32_i16
	.quad	_L_1+7617
	.quad	_A__xor_var_u32_i16
	.quad	_L_1+7635
	.quad	_A__xor_param_u32_i64
	.quad	_L_1+7651
	.quad	_A__xor_var_u32_i64
	.quad	_L_1+7669
	.quad	_A__xor_param_u32_u16
	.quad	_L_1+7685
	.quad	_A__xor_var_u32_u16
	.quad	_L_1+7703
	.quad	_A__xor_param_u32_u64
	.quad	_L_1+7719
	.quad	_A__xor_var_u32_u64
	.quad	_L_1+7737
	.quad	_A__or_param_u32_i8
	.quad	_L_1+7753
	.quad	_A__or_var_u32_i8
	.quad	_L_1+7769
	.quad	_A__or_param_u32_i32
	.quad	_L_1+7783
	.quad	_A__or_var_u32_i32
	.quad	_L_1+7800
	.quad	_A__or_param_u32_u8
	.quad	_L_1+7815
	.quad	_A__or_var_u32_u8
	.quad	_L_1+7831
	.quad	_A__or_param_u32_u32
	.quad	_L_1+7845
	.quad	_A__or_var_u32_u32
	.quad	_L_1+7862
	.quad	_A__or_param_u32_i16
	.quad	_L_1+7877
	.quad	_A__or_var_u32_i16
	.quad	_L_1+7894
	.quad	_A__or_param_u32_i64
	.quad	_L_1+7909
	.quad	_A__or_var_u32_i64
	.quad	_L_1+7926
	.quad	_A__or_param_u32_u16
	.quad	_L_1+7941
	.quad	_A__or_var_u32_u16
	.quad	_L_1+7958
	.quad	_A__or_param_u32_u64
	.quad	_L_1+7973
	.quad	_A__or_var_u32_u64
	.quad	_L_1+7990
	.quad	_A__and_param_u32_i8
	.quad	_L_1+8005
	.quad	_A__and_var_u32_i8
	.quad	_L_1+8022
	.quad	_A__and_param_u32_i32
	.quad	_L_1+8037
	.quad	_A__and_var_u32_i32
	.quad	_L_1+8055
	.quad	_A__and_param_u32_u8
	.quad	_L_1+8071
	.quad	_A__and_var_u32_u8
	.quad	_L_1+8088
	.quad	_A__and_param_u32_u32
	.quad	_L_1+8103
	.quad	_A__and_var_u32_u32
	.quad	_L_1+8121
	.quad	_A__and_param_u32_i16
	.quad	_L_1+8137
	.quad	_A__and_var_u32_i16
	.quad	_L_1+8155
	.quad	_A__and_param_u32_i64
	.quad	_L_1+8171
	.quad	_A__and_var_u32_i64
	.quad	_L_1+8189
	.quad	_A__and_param_u32_u16
	.quad	_L_1+8205
	.quad	_A__and_var_u32_u16
	.quad	_L_1+8223
	.quad	_A__and_param_u32_u64
	.quad	_L_1+8239
	.quad	_A__and_var_u32_u64
	.quad	_L_1+8257
	.quad	_A__sub_param_u32_i8
	.quad	_L_1+8273
	.quad	_A__sub_var_u32_i8
	.quad	_L_1+8290
	.quad	_A__sub_param_u32_i32
	.quad	_L_1+8305
	.quad	_A__sub_var_u32_i32
	.quad	_L_1+8323
	.quad	_A__sub_param_u32_u8
	.quad	_L_1+8339
	.quad	_A__sub_var_u32_u8
	.quad	_L_1+8356
	.quad	_A__sub_param_u32_u32
	.quad	_L_1+8371
	.quad	_A__sub_var_u32_u32
	.quad	_L_1+8389
	.quad	_A__sub_param_u32_i16
	.quad	_L_1+8405
	.quad	_A__sub_var_u32_i16
	.quad	_L_1+8423
	.quad	_A__sub_param_u32_i64
	.quad	_L_1+8439
	.quad	_A__sub_var_u32_i64
	.quad	_L_1+8457
	.quad	_A__sub_param_u32_u16
	.quad	_L_1+8473
	.quad	_A__sub_var_u32_u16
	.quad	_L_1+8491
	.quad	_A__sub_param_u32_u64
	.quad	_L_1+8507
	.quad	_A__sub_var_u32_u64
	.quad	_L_1+8525
	.quad	_A__add_param_u32_i8
	.quad	_L_1+8541
	.quad	_A__add_var_u32_i8
	.quad	_L_1+8558
	.quad	_A__add_param_u32_i32
	.quad	_L_1+8573
	.quad	_A__add_var_u32_i32
	.quad	_L_1+8591
	.quad	_A__add_param_u32_u8
	.quad	_L_1+8607
	.quad	_A__add_var_u32_u8
	.quad	_L_1+8624
	.quad	_A__add_param_u32_u32
	.quad	_L_1+8639
	.quad	_A__add_var_u32_u32
	.quad	_L_1+8657
	.quad	_A__add_param_u32_i16
	.quad	_L_1+8673
	.quad	_A__add_var_u32_i16
	.quad	_L_1+8691
	.quad	_A__add_param_u32_i64
	.quad	_L_1+8707
	.quad	_A__add_var_u32_i64
	.quad	_L_1+8725
	.quad	_A__add_param_u32_u16
	.quad	_L_1+8741
	.quad	_A__add_var_u32_u16
	.quad	_L_1+8759
	.quad	_A__add_param_u32_u64
	.quad	_L_1+8775
	.quad	_A__add_var_u32_u64
	.quad	_L_1+8793
	.quad	_A__ret_param_u32_i8
	.quad	_L_1+8809
	.quad	_A__ret_param_u32_i32
	.quad	_L_1+8826
	.quad	_A__ret_param_u32_u8
	.quad	_L_1+8844
	.quad	_A__ret_param_u32_u32
	.quad	_L_1+8861
	.quad	_A__ret_param_u32_i16
	.quad	_L_1+8879
	.quad	_A__ret_param_u32_i64
	.quad	_L_1+8897
	.quad	_A__ret_param_u32_u16
	.quad	_L_1+8915
	.quad	_A__ret_param_u32_u64
	.quad	_L_1+8933
	.quad	_A__ret_const_i32
	.quad	_L_1+8951
	.quad	_A__ret_var_i32
	.quad	_L_1+8965
	.quad	_A__mult_param_fx_fx
	.quad	_L_1+8977
	.quad	_A__mult_var_fx_fx
	.quad	_L_1+8994
	.quad	_A__sub_param_fx_fx
	.quad	_L_1+9009
	.quad	_A__sub_var_fx_fx
	.quad	_L_1+9025
	.quad	_A__add_param_fx_fx
	.quad	_L_1+9039
	.quad	_A__add_var_fx_fx
	.quad	_L_1+9055
	.quad	_A__ret_fx
	.quad	_L_1+9069
	.quad	_A__mod_param_i16_i8
	.quad	_L_1+9076
	.quad	_A__mod_var_i16_i8
	.quad	_L_1+9093
	.quad	_A__mod_param_i16_i32
	.quad	_L_1+9108
	.quad	_A__mod_var_i16_i32
	.quad	_L_1+9126
	.quad	_A__mod_param_i16_u8
	.quad	_L_1+9142
	.quad	_A__mod_var_i16_u8
	.quad	_L_1+9159
	.quad	_A__mod_param_i16_u32
	.quad	_L_1+9174
	.quad	_A__mod_var_i16_u32
	.quad	_L_1+9192
	.quad	_A__mod_param_i16_i16
	.quad	_L_1+9208
	.quad	_A__mod_var_i16_i16
	.quad	_L_1+9226
	.quad	_A__mod_param_i16_i64
	.quad	_L_1+9242
	.quad	_A__mod_var_i16_i64
	.quad	_L_1+9260
	.quad	_A__mod_param_i16_u16
	.quad	_L_1+9276
	.quad	_A__mod_var_i16_u16
	.quad	_L_1+9294
	.quad	_A__mod_param_i16_u64
	.quad	_L_1+9310
	.quad	_A__mod_var_i16_u64
	.quad	_L_1+9328
	.quad	_A__div_param_i16_i8
	.quad	_L_1+9344
	.quad	_A__div_var_i16_i8
	.quad	_L_1+9361
	.quad	_A__div_param_i16_i32
	.quad	_L_1+9376
	.quad	_A__div_var_i16_i32
	.quad	_L_1+9394
	.quad	_A__div_param_i16_u8
	.quad	_L_1+9410
	.quad	_A__div_var_i16_u8
	.quad	_L_1+9427
	.quad	_A__div_param_i16_u32
	.quad	_L_1+9442
	.quad	_A__div_var_i16_u32
	.quad	_L_1+9460
	.quad	_A__div_param_i16_i16
	.quad	_L_1+9476
	.quad	_A__div_var_i16_i16
	.quad	_L_1+9494
	.quad	_A__div_param_i16_i64
	.quad	_L_1+9510
	.quad	_A__div_var_i16_i64
	.quad	_L_1+9528
	.quad	_A__div_param_i16_u16
	.quad	_L_1+9544
	.quad	_A__div_var_i16_u16
	.quad	_L_1+9562
	.quad	_A__div_param_i16_u64
	.quad	_L_1+9578
	.quad	_A__div_var_i16_u64
	.quad	_L_1+9596
	.quad	_A__mult_param_i16_i8
	.quad	_L_1+9612
	.quad	_A__mult_var_i16_i8
	.quad	_L_1+9630
	.quad	_A__mult_param_i16_i32
	.quad	_L_1+9646
	.quad	_A__mult_var_i16_i32
	.quad	_L_1+9665
	.quad	_A__mult_param_i16_u8
	.quad	_L_1+9682
	.quad	_A__mult_var_i16_u8
	.quad	_L_1+9700
	.quad	_A__mult_param_i16_u32
	.quad	_L_1+9716
	.quad	_A__mult_var_i16_u32
	.quad	_L_1+9735
	.quad	_A__mult_param_i16_i16
	.quad	_L_1+9752
	.quad	_A__mult_var_i16_i16
	.quad	_L_1+9771
	.quad	_A__mult_param_i16_i64
	.quad	_L_1+9788
	.quad	_A__mult_var_i16_i64
	.quad	_L_1+9807
	.quad	_A__mult_param_i16_u16
	.quad	_L_1+9824
	.quad	_A__mult_var_i16_u16
	.quad	_L_1+9843
	.quad	_A__mult_param_i16_u64
	.quad	_L_1+9860
	.quad	_A__mult_var_i16_u64
	.quad	_L_1+9879
	.quad	_A__xor_param_i16_i8
	.quad	_L_1+9896
	.quad	_A__xor_var_i16_i8
	.quad	_L_1+9913
	.quad	_A__xor_param_i16_i32
	.quad	_L_1+9928
	.quad	_A__xor_var_i16_i32
	.quad	_L_1+9946
	.quad	_A__xor_param_i16_u8
	.quad	_L_1+9962
	.quad	_A__xor_var_i16_u8
	.quad	_L_1+9979
	.quad	_A__xor_param_i16_u32
	.quad	_L_1+9994
	.quad	_A__xor_var_i16_u32
	.quad	_L_1+10012
	.quad	_A__xor_param_i16_i16
	.quad	_L_1+10028
	.quad	_A__xor_var_i16_i16
	.quad	_L_1+10046
	.quad	_A__xor_param_i16_i64
	.quad	_L_1+10062
	.quad	_A__xor_var_i16_i64
	.quad	_L_1+10080
	.quad	_A__xor_param_i16_u16
	.quad	_L_1+10096
	.quad	_A__xor_var_i16_u16
	.quad	_L_1+10114
	.quad	_A__xor_param_i16_u64
	.quad	_L_1+10130
	.quad	_A__xor_var_i16_u64
	.quad	_L_1+10148
	.quad	_A__or_param_i16_i8
	.quad	_L_1+10164
	.quad	_A__or_var_i16_i8
	.quad	_L_1+10180
	.quad	_A__or_param_i16_i32
	.quad	_L_1+10194
	.quad	_A__or_var_i16_i32
	.quad	_L_1+10211
	.quad	_A__or_param_i16_u8
	.quad	_L_1+10226
	.quad	_A__or_var_i16_u8
	.quad	_L_1+10242
	.quad	_A__or_param_i16_u32
	.quad	_L_1+10256
	.quad	_A__or_var_i16_u32
	.quad	_L_1+10273
	.quad	_A__or_param_i16_i16
	.quad	_L_1+10288
	.quad	_A__or_var_i16_i16
	.quad	_L_1+10305
	.quad	_A__or_param_i16_i64
	.quad	_L_1+10320
	.quad	_A__or_var_i16_i64
	.quad	_L_1+10337
	.quad	_A__or_param_i16_u16
	.quad	_L_1+10352
	.quad	_A__or_var_i16_u16
	.quad	_L_1+10369
	.quad	_A__or_param_i16_u64
	.quad	_L_1+10384
	.quad	_A__or_var_i16_u64
	.quad	_L_1+10401
	.quad	_A__and_param_i16_i8
	.quad	_L_1+10416
	.quad	_A__and_var_i16_i8
	.quad	_L_1+10433
	.quad	_A__and_param_i16_i32
	.quad	_L_1+10448
	.quad	_A__and_var_i16_i32
	.quad	_L_1+10466
	.quad	_A__and_param_i16_u8
	.quad	_L_1+10482
	.quad	_A__and_var_i16_u8
	.quad	_L_1+10499
	.quad	_A__and_param_i16_u32
	.quad	_L_1+10514
	.quad	_A__and_var_i16_u32
	.quad	_L_1+10532
	.quad	_A__and_param_i16_i16
	.quad	_L_1+10548
	.quad	_A__and_var_i16_i16
	.quad	_L_1+10566
	.quad	_A__and_param_i16_i64
	.quad	_L_1+10582
	.quad	_A__and_var_i16_i64
	.quad	_L_1+10600
	.quad	_A__and_param_i16_u16
	.quad	_L_1+10616
	.quad	_A__and_var_i16_u16
	.quad	_L_1+10634
	.quad	_A__and_param_i16_u64
	.quad	_L_1+10650
	.quad	_A__and_var_i16_u64
	.quad	_L_1+10668
	.quad	_A__sub_param_i16_i8
	.quad	_L_1+10684
	.quad	_A__sub_var_i16_i8
	.quad	_L_1+10701
	.quad	_A__sub_param_i16_i32
	.quad	_L_1+10716
	.quad	_A__sub_var_i16_i32
	.quad	_L_1+10734
	.quad	_A__sub_param_i16_u8
	.quad	_L_1+10750
	.quad	_A__sub_var_i16_u8
	.quad	_L_1+10767
	.quad	_A__sub_param_i16_u32
	.quad	_L_1+10782
	.quad	_A__sub_var_i16_u32
	.quad	_L_1+10800
	.quad	_A__sub_param_i16_i16
	.quad	_L_1+10816
	.quad	_A__sub_var_i16_i16
	.quad	_L_1+10834
	.quad	_A__sub_param_i16_i64
	.quad	_L_1+10850
	.quad	_A__sub_var_i16_i64
	.quad	_L_1+10868
	.quad	_A__sub_param_i16_u16
	.quad	_L_1+10884
	.quad	_A__sub_var_i16_u16
	.quad	_L_1+10902
	.quad	_A__sub_param_i16_u64
	.quad	_L_1+10918
	.quad	_A__sub_var_i16_u64
	.quad	_L_1+10936
	.quad	_A__add_param_i16_i8
	.quad	_L_1+10952
	.quad	_A__add_var_i16_i8
	.quad	_L_1+10969
	.quad	_A__add_param_i16_i32
	.quad	_L_1+10984
	.quad	_A__add_var_i16_i32
	.quad	_L_1+11002
	.quad	_A__add_param_i16_u8
	.quad	_L_1+11018
	.quad	_A__add_var_i16_u8
	.quad	_L_1+11035
	.quad	_A__add_param_i16_u32
	.quad	_L_1+11050
	.quad	_A__add_var_i16_u32
	.quad	_L_1+11068
	.quad	_A__add_param_i16_i16
	.quad	_L_1+11084
	.quad	_A__add_var_i16_i16
	.quad	_L_1+11102
	.quad	_A__add_param_i16_i64
	.quad	_L_1+11118
	.quad	_A__add_var_i16_i64
	.quad	_L_1+11136
	.quad	_A__add_param_i16_u16
	.quad	_L_1+11152
	.quad	_A__add_var_i16_u16
	.quad	_L_1+11170
	.quad	_A__add_param_i16_u64
	.quad	_L_1+11186
	.quad	_A__add_var_i16_u64
	.quad	_L_1+11204
	.quad	_A__ret_param_i16_i8
	.quad	_L_1+11220
	.quad	_A__ret_param_i16_i32
	.quad	_L_1+11237
	.quad	_A__ret_param_i16_u8
	.quad	_L_1+11255
	.quad	_A__ret_param_i16_u32
	.quad	_L_1+11272
	.quad	_A__ret_param_i16_i16
	.quad	_L_1+11290
	.quad	_A__ret_param_i16_i64
	.quad	_L_1+11308
	.quad	_A__ret_param_i16_u16
	.quad	_L_1+11326
	.quad	_A__ret_param_i16_u64
	.quad	_L_1+11344
	.quad	_A__ret_const_u16
	.quad	_L_1+11362
	.quad	_A__ret_var_u16
	.quad	_L_1+11376
	.quad	_A__mult_param_f64_f64
	.quad	_L_1+11388
	.quad	_A__mult_var_f64_f64
	.quad	_L_1+11407
	.quad	_A__sub_param_f64_f64
	.quad	_L_1+11424
	.quad	_A__sub_var_f64_f64
	.quad	_L_1+11442
	.quad	_A__add_param_f64_f64
	.quad	_L_1+11458
	.quad	_A__add_var_f64_f64
	.quad	_L_1+11476
	.quad	_A__ret_f64
	.quad	_L_1+11492
	.quad	_A__mod_param_i64_i8
	.quad	_L_1+11500
	.quad	_A__mod_var_i64_i8
	.quad	_L_1+11517
	.quad	_A__mod_param_i64_i32
	.quad	_L_1+11532
	.quad	_A__mod_var_i64_i32
	.quad	_L_1+11550
	.quad	_A__mod_param_i64_u8
	.quad	_L_1+11566
	.quad	_A__mod_var_i64_u8
	.quad	_L_1+11583
	.quad	_A__mod_param_i64_u32
	.quad	_L_1+11598
	.quad	_A__mod_var_i64_u32
	.quad	_L_1+11616
	.quad	_A__mod_param_i64_i16
	.quad	_L_1+11632
	.quad	_A__mod_var_i64_i16
	.quad	_L_1+11650
	.quad	_A__mod_param_i64_i64
	.quad	_L_1+11666
	.quad	_A__mod_var_i64_i64
	.quad	_L_1+11684
	.quad	_A__mod_param_i64_u16
	.quad	_L_1+11700
	.quad	_A__mod_var_i64_u16
	.quad	_L_1+11718
	.quad	_A__mod_param_i64_u64
	.quad	_L_1+11734
	.quad	_A__mod_var_i64_u64
	.quad	_L_1+11752
	.quad	_A__div_param_i64_i8
	.quad	_L_1+11768
	.quad	_A__div_var_i64_i8
	.quad	_L_1+11785
	.quad	_A__div_param_i64_i32
	.quad	_L_1+11800
	.quad	_A__div_var_i64_i32
	.quad	_L_1+11818
	.quad	_A__div_param_i64_u8
	.quad	_L_1+11834
	.quad	_A__div_var_i64_u8
	.quad	_L_1+11851
	.quad	_A__div_param_i64_u32
	.quad	_L_1+11866
	.quad	_A__div_var_i64_u32
	.quad	_L_1+11884
	.quad	_A__div_param_i64_i16
	.quad	_L_1+11900
	.quad	_A__div_var_i64_i16
	.quad	_L_1+11918
	.quad	_A__div_param_i64_i64
	.quad	_L_1+11934
	.quad	_A__div_var_i64_i64
	.quad	_L_1+11952
	.quad	_A__div_param_i64_u16
	.quad	_L_1+11968
	.quad	_A__div_var_i64_u16
	.quad	_L_1+11986
	.quad	_A__div_param_i64_u64
	.quad	_L_1+12002
	.quad	_A__div_var_i64_u64
	.quad	_L_1+12020
	.quad	_A__mult_param_i64_i8
	.quad	_L_1+12036
	.quad	_A__mult_var_i64_i8
	.quad	_L_1+12054
	.quad	_A__mult_param_i64_i32
	.quad	_L_1+12070
	.quad	_A__mult_var_i64_i32
	.quad	_L_1+12089
	.quad	_A__mult_param_i64_u8
	.quad	_L_1+12106
	.quad	_A__mult_var_i64_u8
	.quad	_L_1+12124
	.quad	_A__mult_param_i64_u32
	.quad	_L_1+12140
	.quad	_A__mult_var_i64_u32
	.quad	_L_1+12159
	.quad	_A__mult_param_i64_i16
	.quad	_L_1+12176
	.quad	_A__mult_var_i64_i16
	.quad	_L_1+12195
	.quad	_A__mult_param_i64_i64
	.quad	_L_1+12212
	.quad	_A__mult_var_i64_i64
	.quad	_L_1+12231
	.quad	_A__mult_param_i64_u16
	.quad	_L_1+12248
	.quad	_A__mult_var_i64_u16
	.quad	_L_1+12267
	.quad	_A__mult_param_i64_u64
	.quad	_L_1+12284
	.quad	_A__mult_var_i64_u64
	.quad	_L_1+12303
	.quad	_A__xor_param_i64_i8
	.quad	_L_1+12320
	.quad	_A__xor_var_i64_i8
	.quad	_L_1+12337
	.quad	_A__xor_param_i64_i32
	.quad	_L_1+12352
	.quad	_A__xor_var_i64_i32
	.quad	_L_1+12370
	.quad	_A__xor_param_i64_u8
	.quad	_L_1+12386
	.quad	_A__xor_var_i64_u8
	.quad	_L_1+12403
	.quad	_A__xor_param_i64_u32
	.quad	_L_1+12418
	.quad	_A__xor_var_i64_u32
	.quad	_L_1+12436
	.quad	_A__xor_param_i64_i16
	.quad	_L_1+12452
	.quad	_A__xor_var_i64_i16
	.quad	_L_1+12470
	.quad	_A__xor_param_i64_i64
	.quad	_L_1+12486
	.quad	_A__xor_var_i64_i64
	.quad	_L_1+12504
	.quad	_A__xor_param_i64_u16
	.quad	_L_1+12520
	.quad	_A__xor_var_i64_u16
	.quad	_L_1+12538
	.quad	_A__xor_param_i64_u64
	.quad	_L_1+12554
	.quad	_A__xor_var_i64_u64
	.quad	_L_1+12572
	.quad	_A__or_param_i64_i8
	.quad	_L_1+12588
	.quad	_A__or_var_i64_i8
	.quad	_L_1+12604
	.quad	_A__or_param_i64_i32
	.quad	_L_1+12618
	.quad	_A__or_var_i64_i32
	.quad	_L_1+12635
	.quad	_A__or_param_i64_u8
	.quad	_L_1+12650
	.quad	_A__or_var_i64_u8
	.quad	_L_1+12666
	.quad	_A__or_param_i64_u32
	.quad	_L_1+12680
	.quad	_A__or_var_i64_u32
	.quad	_L_1+12697
	.quad	_A__or_param_i64_i16
	.quad	_L_1+12712
	.quad	_A__or_var_i64_i16
	.quad	_L_1+12729
	.quad	_A__or_param_i64_i64
	.quad	_L_1+12744
	.quad	_A__or_var_i64_i64
	.quad	_L_1+12761
	.quad	_A__or_param_i64_u16
	.quad	_L_1+12776
	.quad	_A__or_var_i64_u16
	.quad	_L_1+12793
	.quad	_A__or_param_i64_u64
	.quad	_L_1+12808
	.quad	_A__or_var_i64_u64
	.quad	_L_1+12825
	.quad	_A__and_param_i64_i8
	.quad	_L_1+12840
	.quad	_A__and_var_i64_i8
	.quad	_L_1+12857
	.quad	_A__and_param_i64_i32
	.quad	_L_1+12872
	.quad	_A__and_var_i64_i32
	.quad	_L_1+12890
	.quad	_A__and_param_i64_u8
	.quad	_L_1+12906
	.quad	_A__and_var_i64_u8
	.quad	_L_1+12923
	.quad	_A__and_param_i64_u32
	.quad	_L_1+12938
	.quad	_A__and_var_i64_u32
	.quad	_L_1+12956
	.quad	_A__and_param_i64_i16
	.quad	_L_1+12972
	.quad	_A__and_var_i64_i16
	.quad	_L_1+12990
	.quad	_A__and_param_i64_i64
	.quad	_L_1+13006
	.quad	_A__and_var_i64_i64
	.quad	_L_1+13024
	.quad	_A__and_param_i64_u16
	.quad	_L_1+13040
	.quad	_A__and_var_i64_u16
	.quad	_L_1+13058
	.quad	_A__and_param_i64_u64
	.quad	_L_1+13074
	.quad	_A__and_var_i64_u64
	.quad	_L_1+13092
	.quad	_A__sub_param_i64_i8
	.quad	_L_1+13108
	.quad	_A__sub_var_i64_i8
	.quad	_L_1+13125
	.quad	_A__sub_param_i64_i32
	.quad	_L_1+13140
	.quad	_A__sub_var_i64_i32
	.quad	_L_1+13158
	.quad	_A__sub_param_i64_u8
	.quad	_L_1+13174
	.quad	_A__sub_var_i64_u8
	.quad	_L_1+13191
	.quad	_A__sub_param_i64_u32
	.quad	_L_1+13206
	.quad	_A__sub_var_i64_u32
	.quad	_L_1+13224
	.quad	_A__sub_param_i64_i16
	.quad	_L_1+13240
	.quad	_A__sub_var_i64_i16
	.quad	_L_1+13258
	.quad	_A__sub_param_i64_i64
	.quad	_L_1+13274
	.quad	_A__sub_var_i64_i64
	.quad	_L_1+13292
	.quad	_A__sub_param_i64_u16
	.quad	_L_1+13308
	.quad	_A__sub_var_i64_u16
	.quad	_L_1+13326
	.quad	_A__sub_param_i64_u64
	.quad	_L_1+13342
	.quad	_A__sub_var_i64_u64
	.quad	_L_1+13360
	.quad	_A__add_param_i64_i8
	.quad	_L_1+13376
	.quad	_A__add_var_i64_i8
	.quad	_L_1+13393
	.quad	_A__add_param_i64_i32
	.quad	_L_1+13408
	.quad	_A__add_var_i64_i32
	.quad	_L_1+13426
	.quad	_A__add_param_i64_u8
	.quad	_L_1+13442
	.quad	_A__add_var_i64_u8
	.quad	_L_1+13459
	.quad	_A__add_param_i64_u32
	.quad	_L_1+13474
	.quad	_A__add_var_i64_u32
	.quad	_L_1+13492
	.quad	_A__add_param_i64_i16
	.quad	_L_1+13508
	.quad	_A__add_var_i64_i16
	.quad	_L_1+13526
	.quad	_A__add_param_i64_i64
	.quad	_L_1+13542
	.quad	_A__add_var_i64_i64
	.quad	_L_1+13560
	.quad	_A__add_param_i64_u16
	.quad	_L_1+13576
	.quad	_A__add_var_i64_u16
	.quad	_L_1+13594
	.quad	_A__add_param_i64_u64
	.quad	_L_1+13610
	.quad	_A__add_var_i64_u64
	.quad	_L_1+13628
	.quad	_A__ret_param_i64_i8
	.quad	_L_1+13644
	.quad	_A__ret_param_i64_i32
	.quad	_L_1+13661
	.quad	_A__ret_param_i64_u8
	.quad	_L_1+13679
	.quad	_A__ret_param_i64_u32
	.quad	_L_1+13696
	.quad	_A__ret_param_i64_i16
	.quad	_L_1+13714
	.quad	_A__ret_param_i64_i64
	.quad	_L_1+13732
	.quad	_A__ret_param_i64_u16
	.quad	_L_1+13750
	.quad	_A__ret_param_i64_u64
	.quad	_L_1+13768
	.quad	_A__ret_const_u64
	.quad	_L_1+13786
	.quad	_A__ret_var_u64
	.quad	_L_1+13800
	.quad	_A__mod_param_u16_i8
	.quad	_L_1+13812
	.quad	_A__mod_var_u16_i8
	.quad	_L_1+13829
	.quad	_A__mod_param_u16_i32
	.quad	_L_1+13844
	.quad	_A__mod_var_u16_i32
	.quad	_L_1+13862
	.quad	_A__mod_param_u16_u8
	.quad	_L_1+13878
	.quad	_A__mod_var_u16_u8
	.quad	_L_1+13895
	.quad	_A__mod_param_u16_u32
	.quad	_L_1+13910
	.quad	_A__mod_var_u16_u32
	.quad	_L_1+13928
	.quad	_A__mod_param_u16_i16
	.quad	_L_1+13944
	.quad	_A__mod_var_u16_i16
	.quad	_L_1+13962
	.quad	_A__mod_param_u16_i64
	.quad	_L_1+13978
	.quad	_A__mod_var_u16_i64
	.quad	_L_1+13996
	.quad	_A__mod_param_u16_u16
	.quad	_L_1+14012
	.quad	_A__mod_var_u16_u16
	.quad	_L_1+14030
	.quad	_A__mod_param_u16_u64
	.quad	_L_1+14046
	.quad	_A__mod_var_u16_u64
	.quad	_L_1+14064
	.quad	_A__div_param_u16_i8
	.quad	_L_1+14080
	.quad	_A__div_var_u16_i8
	.quad	_L_1+14097
	.quad	_A__div_param_u16_i32
	.quad	_L_1+14112
	.quad	_A__div_var_u16_i32
	.quad	_L_1+14130
	.quad	_A__div_param_u16_u8
	.quad	_L_1+14146
	.quad	_A__div_var_u16_u8
	.quad	_L_1+14163
	.quad	_A__div_param_u16_u32
	.quad	_L_1+14178
	.quad	_A__div_var_u16_u32
	.quad	_L_1+14196
	.quad	_A__div_param_u16_i16
	.quad	_L_1+14212
	.quad	_A__div_var_u16_i16
	.quad	_L_1+14230
	.quad	_A__div_param_u16_i64
	.quad	_L_1+14246
	.quad	_A__div_var_u16_i64
	.quad	_L_1+14264
	.quad	_A__div_param_u16_u16
	.quad	_L_1+14280
	.quad	_A__div_var_u16_u16
	.quad	_L_1+14298
	.quad	_A__div_param_u16_u64
	.quad	_L_1+14314
	.quad	_A__div_var_u16_u64
	.quad	_L_1+14332
	.quad	_A__mult_param_u16_i8
	.quad	_L_1+14348
	.quad	_A__mult_var_u16_i8
	.quad	_L_1+14366
	.quad	_A__mult_param_u16_i32
	.quad	_L_1+14382
	.quad	_A__mult_var_u16_i32
	.quad	_L_1+14401
	.quad	_A__mult_param_u16_u8
	.quad	_L_1+14418
	.quad	_A__mult_var_u16_u8
	.quad	_L_1+14436
	.quad	_A__mult_param_u16_u32
	.quad	_L_1+14452
	.quad	_A__mult_var_u16_u32
	.quad	_L_1+14471
	.quad	_A__mult_param_u16_i16
	.quad	_L_1+14488
	.quad	_A__mult_var_u16_i16
	.quad	_L_1+14507
	.quad	_A__mult_param_u16_i64
	.quad	_L_1+14524
	.quad	_A__mult_var_u16_i64
	.quad	_L_1+14543
	.quad	_A__mult_param_u16_u16
	.quad	_L_1+14560
	.quad	_A__mult_var_u16_u16
	.quad	_L_1+14579
	.quad	_A__mult_param_u16_u64
	.quad	_L_1+14596
	.quad	_A__mult_var_u16_u64
	.quad	_L_1+14615
	.quad	_A__xor_param_u16_i8
	.quad	_L_1+14632
	.quad	_A__xor_var_u16_i8
	.quad	_L_1+14649
	.quad	_A__xor_param_u16_i32
	.quad	_L_1+14664
	.quad	_A__xor_var_u16_i32
	.quad	_L_1+14682
	.quad	_A__xor_param_u16_u8
	.quad	_L_1+14698
	.quad	_A__xor_var_u16_u8
	.quad	_L_1+14715
	.quad	_A__xor_param_u16_u32
	.quad	_L_1+14730
	.quad	_A__xor_var_u16_u32
	.quad	_L_1+14748
	.quad	_A__xor_param_u16_i16
	.quad	_L_1+14764
	.quad	_A__xor_var_u16_i16
	.quad	_L_1+14782
	.quad	_A__xor_param_u16_i64
	.quad	_L_1+14798
	.quad	_A__xor_var_u16_i64
	.quad	_L_1+14816
	.quad	_A__xor_param_u16_u16
	.quad	_L_1+14832
	.quad	_A__xor_var_u16_u16
	.quad	_L_1+14850
	.quad	_A__xor_param_u16_u64
	.quad	_L_1+14866
	.quad	_A__xor_var_u16_u64
	.quad	_L_1+14884
	.quad	_A__or_param_u16_i8
	.quad	_L_1+14900
	.quad	_A__or_var_u16_i8
	.quad	_L_1+14916
	.quad	_A__or_param_u16_i32
	.quad	_L_1+14930
	.quad	_A__or_var_u16_i32
	.quad	_L_1+14947
	.quad	_A__or_param_u16_u8
	.quad	_L_1+14962
	.quad	_A__or_var_u16_u8
	.quad	_L_1+14978
	.quad	_A__or_param_u16_u32
	.quad	_L_1+14992
	.quad	_A__or_var_u16_u32
	.quad	_L_1+15009
	.quad	_A__or_param_u16_i16
	.quad	_L_1+15024
	.quad	_A__or_var_u16_i16
	.quad	_L_1+15041
	.quad	_A__or_param_u16_i64
	.quad	_L_1+15056
	.quad	_A__or_var_u16_i64
	.quad	_L_1+15073
	.quad	_A__or_param_u16_u16
	.quad	_L_1+15088
	.quad	_A__or_var_u16_u16
	.quad	_L_1+15105
	.quad	_A__or_param_u16_u64
	.quad	_L_1+15120
	.quad	_A__or_var_u16_u64
	.quad	_L_1+15137
	.quad	_A__and_param_u16_i8
	.quad	_L_1+15152
	.quad	_A__and_var_u16_i8
	.quad	_L_1+15169
	.quad	_A__and_param_u16_i32
	.quad	_L_1+15184
	.quad	_A__and_var_u16_i32
	.quad	_L_1+15202
	.quad	_A__and_param_u16_u8
	.quad	_L_1+15218
	.quad	_A__and_var_u16_u8
	.quad	_L_1+15235
	.quad	_A__and_param_u16_u32
	.quad	_L_1+15250
	.quad	_A__and_var_u16_u32
	.quad	_L_1+15268
	.quad	_A__and_param_u16_i16
	.quad	_L_1+15284
	.quad	_A__and_var_u16_i16
	.quad	_L_1+15302
	.quad	_A__and_param_u16_i64
	.quad	_L_1+15318
	.quad	_A__and_var_u16_i64
	.quad	_L_1+15336
	.quad	_A__and_param_u16_u16
	.quad	_L_1+15352
	.quad	_A__and_var_u16_u16
	.quad	_L_1+15370
	.quad	_A__and_param_u16_u64
	.quad	_L_1+15386
	.quad	_A__and_var_u16_u64
	.quad	_L_1+15404
	.quad	_A__sub_param_u16_i8
	.quad	_L_1+15420
	.quad	_A__sub_var_u16_i8
	.quad	_L_1+15437
	.quad	_A__sub_param_u16_i32
	.quad	_L_1+15452
	.quad	_A__sub_var_u16_i32
	.quad	_L_1+15470
	.quad	_A__sub_param_u16_u8
	.quad	_L_1+15486
	.quad	_A__sub_var_u16_u8
	.quad	_L_1+15503
	.quad	_A__sub_param_u16_u32
	.quad	_L_1+15518
	.quad	_A__sub_var_u16_u32
	.quad	_L_1+15536
	.quad	_A__sub_param_u16_i16
	.quad	_L_1+15552
	.quad	_A__sub_var_u16_i16
	.quad	_L_1+15570
	.quad	_A__sub_param_u16_i64
	.quad	_L_1+15586
	.quad	_A__sub_var_u16_i64
	.quad	_L_1+15604
	.quad	_A__sub_param_u16_u16
	.quad	_L_1+15620
	.quad	_A__sub_var_u16_u16
	.quad	_L_1+15638
	.quad	_A__sub_param_u16_u64
	.quad	_L_1+15654
	.quad	_A__sub_var_u16_u64
	.quad	_L_1+15672
	.quad	_A__add_param_u16_i8
	.quad	_L_1+15688
	.quad	_A__add_var_u16_i8
	.quad	_L_1+15705
	.quad	_A__add_param_u16_i32
	.quad	_L_1+15720
	.quad	_A__add_var_u16_i32
	.quad	_L_1+15738
	.quad	_A__add_param_u16_u8
	.quad	_L_1+15754
	.quad	_A__add_var_u16_u8
	.quad	_L_1+15771
	.quad	_A__add_param_u16_u32
	.quad	_L_1+15786
	.quad	_A__add_var_u16_u32
	.quad	_L_1+15804
	.quad	_A__add_param_u16_i16
	.quad	_L_1+15820
	.quad	_A__add_var_u16_i16
	.quad	_L_1+15838
	.quad	_A__add_param_u16_i64
	.quad	_L_1+15854
	.quad	_A__add_var_u16_i64
	.quad	_L_1+15872
	.quad	_A__add_param_u16_u16
	.quad	_L_1+15888
	.quad	_A__add_var_u16_u16
	.quad	_L_1+15906
	.quad	_A__add_param_u16_u64
	.quad	_L_1+15922
	.quad	_A__add_var_u16_u64
	.quad	_L_1+15940
	.quad	_A__ret_param_u16_i8
	.quad	_L_1+15956
	.quad	_A__ret_param_u16_i32
	.quad	_L_1+15973
	.quad	_A__ret_param_u16_u8
	.quad	_L_1+15991
	.quad	_A__ret_param_u16_u32
	.quad	_L_1+16008
	.quad	_A__ret_param_u16_i16
	.quad	_L_1+16026
	.quad	_A__ret_param_u16_i64
	.quad	_L_1+16044
	.quad	_A__ret_param_u16_u16
	.quad	_L_1+16062
	.quad	_A__ret_param_u16_u64
	.quad	_L_1+16080
	.quad	_A__ret_const_i16
	.quad	_L_1+16098
	.quad	_A__ret_var_i16
	.quad	_L_1+16112
	.quad	_A__mult_param_f32_f32
	.quad	_L_1+16124
	.quad	_A__mult_var_f32_f32
	.quad	_L_1+16143
	.quad	_A__sub_param_f32_f32
	.quad	_L_1+16160
	.quad	_A__sub_var_f32_f32
	.quad	_L_1+16178
	.quad	_A__add_param_f32_f32
	.quad	_L_1+16194
	.quad	_A__add_var_f32_f32
	.quad	_L_1+16212
	.quad	_A__ret_f32
	.quad	_L_1+16228
	.quad	_A__mod_param_u64_i8
	.quad	_L_1+16236
	.quad	_A__mod_var_u64_i8
	.quad	_L_1+16253
	.quad	_A__mod_param_u64_i32
	.quad	_L_1+16268
	.quad	_A__mod_var_u64_i32
	.quad	_L_1+16286
	.quad	_A__mod_param_u64_u8
	.quad	_L_1+16302
	.quad	_A__mod_var_u64_u8
	.quad	_L_1+16319
	.quad	_A__mod_param_u64_u32
	.quad	_L_1+16334
	.quad	_A__mod_var_u64_u32
	.quad	_L_1+16352
	.quad	_A__mod_param_u64_i16
	.quad	_L_1+16368
	.quad	_A__mod_var_u64_i16
	.quad	_L_1+16386
	.quad	_A__mod_param_u64_i64
	.quad	_L_1+16402
	.quad	_A__mod_var_u64_i64
	.quad	_L_1+16420
	.quad	_A__mod_param_u64_u16
	.quad	_L_1+16436
	.quad	_A__mod_var_u64_u16
	.quad	_L_1+16454
	.quad	_A__mod_param_u64_u64
	.quad	_L_1+16470
	.quad	_A__mod_var_u64_u64
	.quad	_L_1+16488
	.quad	_A__div_param_u64_i8
	.quad	_L_1+16504
	.quad	_A__div_var_u64_i8
	.quad	_L_1+16521
	.quad	_A__div_param_u64_i32
	.quad	_L_1+16536
	.quad	_A__div_var_u64_i32
	.quad	_L_1+16554
	.quad	_A__div_param_u64_u8
	.quad	_L_1+16570
	.quad	_A__div_var_u64_u8
	.quad	_L_1+16587
	.quad	_A__div_param_u64_u32
	.quad	_L_1+16602
	.quad	_A__div_var_u64_u32
	.quad	_L_1+16620
	.quad	_A__div_param_u64_i16
	.quad	_L_1+16636
	.quad	_A__div_var_u64_i16
	.quad	_L_1+16654
	.quad	_A__div_param_u64_i64
	.quad	_L_1+16670
	.quad	_A__div_var_u64_i64
	.quad	_L_1+16688
	.quad	_A__div_param_u64_u16
	.quad	_L_1+16704
	.quad	_A__div_var_u64_u16
	.quad	_L_1+16722
	.quad	_A__div_param_u64_u64
	.quad	_L_1+16738
	.quad	_A__div_var_u64_u64
	.quad	_L_1+16756
	.quad	_A__mult_param_u64_i8
	.quad	_L_1+16772
	.quad	_A__mult_var_u64_i8
	.quad	_L_1+16790
	.quad	_A__mult_param_u64_i32
	.quad	_L_1+16806
	.quad	_A__mult_var_u64_i32
	.quad	_L_1+16825
	.quad	_A__mult_param_u64_u8
	.quad	_L_1+16842
	.quad	_A__mult_var_u64_u8
	.quad	_L_1+16860
	.quad	_A__mult_param_u64_u32
	.quad	_L_1+16876
	.quad	_A__mult_var_u64_u32
	.quad	_L_1+16895
	.quad	_A__mult_param_u64_i16
	.quad	_L_1+16912
	.quad	_A__mult_var_u64_i16
	.quad	_L_1+16931
	.quad	_A__mult_param_u64_i64
	.quad	_L_1+16948
	.quad	_A__mult_var_u64_i64
	.quad	_L_1+16967
	.quad	_A__mult_param_u64_u16
	.quad	_L_1+16984
	.quad	_A__mult_var_u64_u16
	.quad	_L_1+17003
	.quad	_A__mult_param_u64_u64
	.quad	_L_1+17020
	.quad	_A__mult_var_u64_u64
	.quad	_L_1+17039
	.quad	_A__xor_param_u64_i8
	.quad	_L_1+17056
	.quad	_A__xor_var_u64_i8
	.quad	_L_1+17073
	.quad	_A__xor_param_u64_i32
	.quad	_L_1+17088
	.quad	_A__xor_var_u64_i32
	.quad	_L_1+17106
	.quad	_A__xor_param_u64_u8
	.quad	_L_1+17122
	.quad	_A__xor_var_u64_u8
	.quad	_L_1+17139
	.quad	_A__xor_param_u64_u32
	.quad	_L_1+17154
	.quad	_A__xor_var_u64_u32
	.quad	_L_1+17172
	.quad	_A__xor_param_u64_i16
	.quad	_L_1+17188
	.quad	_A__xor_var_u64_i16
	.quad	_L_1+17206
	.quad	_A__xor_param_u64_i64
	.quad	_L_1+17222
	.quad	_A__xor_var_u64_i64
	.quad	_L_1+17240
	.quad	_A__xor_param_u64_u16
	.quad	_L_1+17256
	.quad	_A__xor_var_u64_u16
	.quad	_L_1+17274
	.quad	_A__xor_param_u64_u64
	.quad	_L_1+17290
	.quad	_A__xor_var_u64_u64
	.quad	_L_1+17308
	.quad	_A__or_param_u64_i8
	.quad	_L_1+17324
	.quad	_A__or_var_u64_i8
	.quad	_L_1+17340
	.quad	_A__or_param_u64_i32
	.quad	_L_1+17354
	.quad	_A__or_var_u64_i32
	.quad	_L_1+17371
	.quad	_A__or_param_u64_u8
	.quad	_L_1+17386
	.quad	_A__or_var_u64_u8
	.quad	_L_1+17402
	.quad	_A__or_param_u64_u32
	.quad	_L_1+17416
	.quad	_A__or_var_u64_u32
	.quad	_L_1+17433
	.quad	_A__or_param_u64_i16
	.quad	_L_1+17448
	.quad	_A__or_var_u64_i16
	.quad	_L_1+17465
	.quad	_A__or_param_u64_i64
	.quad	_L_1+17480
	.quad	_A__or_var_u64_i64
	.quad	_L_1+17497
	.quad	_A__or_param_u64_u16
	.quad	_L_1+17512
	.quad	_A__or_var_u64_u16
	.quad	_L_1+17529
	.quad	_A__or_param_u64_u64
	.quad	_L_1+17544
	.quad	_A__or_var_u64_u64
	.quad	_L_1+17561
	.quad	_A__and_param_u64_i8
	.quad	_L_1+17576
	.quad	_A__and_var_u64_i8
	.quad	_L_1+17593
	.quad	_A__and_param_u64_i32
	.quad	_L_1+17608
	.quad	_A__and_var_u64_i32
	.quad	_L_1+17626
	.quad	_A__and_param_u64_u8
	.quad	_L_1+17642
	.quad	_A__and_var_u64_u8
	.quad	_L_1+17659
	.quad	_A__and_param_u64_u32
	.quad	_L_1+17674
	.quad	_A__and_var_u64_u32
	.quad	_L_1+17692
	.quad	_A__and_param_u64_i16
	.quad	_L_1+17708
	.quad	_A__and_var_u64_i16
	.quad	_L_1+17726
	.quad	_A__and_param_u64_i64
	.quad	_L_1+17742
	.quad	_A__and_var_u64_i64
	.quad	_L_1+17760
	.quad	_A__and_param_u64_u16
	.quad	_L_1+17776
	.quad	_A__and_var_u64_u16
	.quad	_L_1+17794
	.quad	_A__and_param_u64_u64
	.quad	_L_1+17810
	.quad	_A__and_var_u64_u64
	.quad	_L_1+17828
	.quad	_A__sub_param_u64_i8
	.quad	_L_1+17844
	.quad	_A__sub_var_u64_i8
	.quad	_L_1+17861
	.quad	_A__sub_param_u64_i32
	.quad	_L_1+17876
	.quad	_A__sub_var_u64_i32
	.quad	_L_1+17894
	.quad	_A__sub_param_u64_u8
	.quad	_L_1+17910
	.quad	_A__sub_var_u64_u8
	.quad	_L_1+17927
	.quad	_A__sub_param_u64_u32
	.quad	_L_1+17942
	.quad	_A__sub_var_u64_u32
	.quad	_L_1+17960
	.quad	_A__sub_param_u64_i16
	.quad	_L_1+17976
	.quad	_A__sub_var_u64_i16
	.quad	_L_1+17994
	.quad	_A__sub_param_u64_i64
	.quad	_L_1+18010
	.quad	_A__sub_var_u64_i64
	.quad	_L_1+18028
	.quad	_A__sub_param_u64_u16
	.quad	_L_1+18044
	.quad	_A__sub_var_u64_u16
	.quad	_L_1+18062
	.quad	_A__sub_param_u64_u64
	.quad	_L_1+18078
	.quad	_A__sub_var_u64_u64
	.quad	_L_1+18096
	.quad	_A__add_param_u64_i8
	.quad	_L_1+18112
	.quad	_A__add_var_u64_i8
	.quad	_L_1+18129
	.quad	_A__add_param_u64_i32
	.quad	_L_1+18144
	.quad	_A__add_var_u64_i32
	.quad	_L_1+18162
	.quad	_A__add_param_u64_u8
	.quad	_L_1+18178
	.quad	_A__add_var_u64_u8
	.quad	_L_1+18195
	.quad	_A__add_param_u64_u32
	.quad	_L_1+18210
	.quad	_A__add_var_u64_u32
	.quad	_L_1+18228
	.quad	_A__add_param_u64_i16
	.quad	_L_1+18244
	.quad	_A__add_var_u64_i16
	.quad	_L_1+18262
	.quad	_A__add_param_u64_i64
	.quad	_L_1+18278
	.quad	_A__add_var_u64_i64
	.quad	_L_1+18296
	.quad	_A__add_param_u64_u16
	.quad	_L_1+18312
	.quad	_A__add_var_u64_u16
	.quad	_L_1+18330
	.quad	_A__add_param_u64_u64
	.quad	_L_1+18346
	.quad	_A__add_var_u64_u64
	.quad	_L_1+18364
	.quad	_A__ret_param_u64_i8
	.quad	_L_1+18380
	.quad	_A__ret_param_u64_i32
	.quad	_L_1+18397
	.quad	_A__ret_param_u64_u8
	.quad	_L_1+18415
	.quad	_A__ret_param_u64_u32
	.quad	_L_1+18432
	.quad	_A__ret_param_u64_i16
	.quad	_L_1+18450
	.quad	_A__ret_param_u64_i64
	.quad	_L_1+18468
	.quad	_A__ret_param_u64_u16
	.quad	_L_1+18486
	.quad	_A__ret_param_u64_u64
	.quad	_L_1+18504
	.quad	_A__ret_const_i64
	.quad	_L_1+18522
	.quad	_A__ret_var_i64
	.quad	_L_1+18536
	.space 8
	.ascii "../AMD64_DARWIN/A.m3"
	.space 4
	.data
	.align 5
_MM_A:
	.quad	_L_1+36576
	.space 32
	.quad	_L_1+18552
	.space 24
	.quad	_MM_A+168
	.space 8
	.quad	_A_M3
	.quad	3
	.quad	1
	.space 4
	.word	4
	.space 2
	.quad	7
	.space 8
	.word	10
	.space 14
	.long	13
	.byte	16
	.space 3
	.long	19
	.byte	22
	.space 11
	.quad	_A_I3
	.quad	_MM_A+192
	.space 8
	.quad	_Long_I3
	.quad	_MM_A+216
	.space 8
	.quad	_Word_I3
	.quad	_MM_A+240
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_A+264
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
