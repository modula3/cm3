	.text
.globl _Or__uOr_var_u16_u16
	.private_extern _Or__uOr_var_u16_u16
_Or__uOr_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
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
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u16_LC
	.private_extern _Or__uOr_var_u16_LC
_Or__uOr_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u16_i16
	.private_extern _Or__uOr_var_u16_i16
_Or__uOr_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Or(%rip), %eax
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
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u16_I
	.private_extern _Or__uOr_var_u16_I
_Or__uOr_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u16_i64
	.private_extern _Or__uOr_var_u16_i64
_Or__uOr_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u16_C
	.private_extern _Or__uOr_var_u16_C
_Or__uOr_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u16_u8
	.private_extern _Or__uOr_var_u16_u8
_Or__uOr_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u16_i8
	.private_extern _Or__uOr_var_u16_i8
_Or__uOr_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u16_L
	.private_extern _Or__uOr_var_u16_L
_Or__uOr_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u16_u32
	.private_extern _Or__uOr_var_u16_u32
_Or__uOr_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u64_u16
	.private_extern _Or__uOr_var_u64_u16
_Or__uOr_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Or(%rip), %rax
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
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_LC
	.private_extern _Or__uOr_var_u64_LC
_Or__uOr_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_i16
	.private_extern _Or__uOr_var_u64_i16
_Or__uOr_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Or(%rip), %rax
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
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_I
	.private_extern _Or__uOr_var_u64_I
_Or__uOr_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_i64
	.private_extern _Or__uOr_var_u64_i64
_Or__uOr_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_C
	.private_extern _Or__uOr_var_u64_C
_Or__uOr_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_u8
	.private_extern _Or__uOr_var_u64_u8
_Or__uOr_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_i8
	.private_extern _Or__uOr_var_u64_i8
_Or__uOr_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_L
	.private_extern _Or__uOr_var_u64_L
_Or__uOr_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u64_u32
	.private_extern _Or__uOr_var_u64_u32
_Or__uOr_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_u16
	.private_extern _Or__uOr_var_LC_u16
_Or__uOr_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Or(%rip), %rax
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
	movq	120+_MM_Or(%rip), %rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_LC
	.private_extern _Or__uOr_var_LC_LC
_Or__uOr_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_i16
	.private_extern _Or__uOr_var_LC_i16
_Or__uOr_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Or(%rip), %rax
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
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_I
	.private_extern _Or__uOr_var_LC_I
_Or__uOr_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_i64
	.private_extern _Or__uOr_var_LC_i64
_Or__uOr_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Or(%rip), %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_C
	.private_extern _Or__uOr_var_LC_C
_Or__uOr_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_u8
	.private_extern _Or__uOr_var_LC_u8
_Or__uOr_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_i8
	.private_extern _Or__uOr_var_LC_i8
_Or__uOr_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_LC_L
	.private_extern _Or__uOr_var_LC_L
_Or__uOr_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Or(%rip), %rdx
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
.globl _Or__uOr_var_LC_u32
	.private_extern _Or__uOr_var_LC_u32
_Or__uOr_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i16_u16
	.private_extern _Or__uOr_var_i16_u16
_Or__uOr_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Or(%rip), %eax
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
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i16_LC
	.private_extern _Or__uOr_var_i16_LC
_Or__uOr_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i16_i16
	.private_extern _Or__uOr_var_i16_i16
_Or__uOr_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
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
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i16_I
	.private_extern _Or__uOr_var_i16_I
_Or__uOr_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i16_i64
	.private_extern _Or__uOr_var_i16_i64
_Or__uOr_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i16_C
	.private_extern _Or__uOr_var_i16_C
_Or__uOr_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i16_u8
	.private_extern _Or__uOr_var_i16_u8
_Or__uOr_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i16_i8
	.private_extern _Or__uOr_var_i16_i8
_Or__uOr_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i16_L
	.private_extern _Or__uOr_var_i16_L
_Or__uOr_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i16_u32
	.private_extern _Or__uOr_var_i16_u32
_Or__uOr_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i32_u16
	.private_extern _Or__uOr_var_i32_u16
_Or__uOr_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Or(%rip), %eax
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
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i32_LC
	.private_extern _Or__uOr_var_i32_LC
_Or__uOr_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i32_i16
	.private_extern _Or__uOr_var_i32_i16
_Or__uOr_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Or(%rip), %eax
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
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i32_I
	.private_extern _Or__uOr_var_i32_I
_Or__uOr_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i32_i64
	.private_extern _Or__uOr_var_i32_i64
_Or__uOr_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i32_C
	.private_extern _Or__uOr_var_i32_C
_Or__uOr_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i32_u8
	.private_extern _Or__uOr_var_i32_u8
_Or__uOr_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i32_i8
	.private_extern _Or__uOr_var_i32_i8
_Or__uOr_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i32_L
	.private_extern _Or__uOr_var_i32_L
_Or__uOr_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i32_u32
	.private_extern _Or__uOr_var_i32_u32
_Or__uOr_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_I_u16
	.private_extern _Or__uOr_var_I_u16
_Or__uOr_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Or(%rip), %eax
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
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_I_LC
	.private_extern _Or__uOr_var_I_LC
_Or__uOr_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_I_i16
	.private_extern _Or__uOr_var_I_i16
_Or__uOr_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Or(%rip), %eax
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
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_I_I
	.private_extern _Or__uOr_var_I_I
_Or__uOr_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_I_i64
	.private_extern _Or__uOr_var_I_i64
_Or__uOr_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_I_C
	.private_extern _Or__uOr_var_I_C
_Or__uOr_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_I_u8
	.private_extern _Or__uOr_var_I_u8
_Or__uOr_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_I_i8
	.private_extern _Or__uOr_var_I_i8
_Or__uOr_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_I_L
	.private_extern _Or__uOr_var_I_L
_Or__uOr_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_I_u32
	.private_extern _Or__uOr_var_I_u32
_Or__uOr_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i64_u16
	.private_extern _Or__uOr_var_i64_u16
_Or__uOr_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Or(%rip), %rax
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
	movq	152+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_LC
	.private_extern _Or__uOr_var_i64_LC
_Or__uOr_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_i16
	.private_extern _Or__uOr_var_i64_i16
_Or__uOr_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Or(%rip), %rax
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
	movl	140+_MM_Or(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_I
	.private_extern _Or__uOr_var_i64_I
_Or__uOr_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_i64
	.private_extern _Or__uOr_var_i64_i64
_Or__uOr_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_C
	.private_extern _Or__uOr_var_i64_C
_Or__uOr_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_u8
	.private_extern _Or__uOr_var_i64_u8
_Or__uOr_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_i8
	.private_extern _Or__uOr_var_i64_i8
_Or__uOr_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_L
	.private_extern _Or__uOr_var_i64_L
_Or__uOr_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i64_u32
	.private_extern _Or__uOr_var_i64_u32
_Or__uOr_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_C_u16
	.private_extern _Or__uOr_var_C_u16
_Or__uOr_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movzwl	104+_MM_Or(%rip), %eax
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
	movq	160+_MM_Or(%rip), %rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_C_LC
	.private_extern _Or__uOr_var_C_LC
_Or__uOr_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_C_i16
	.private_extern _Or__uOr_var_C_i16
_Or__uOr_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movzwl	136+_MM_Or(%rip), %eax
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
	movq	160+_MM_Or(%rip), %rdx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_C_I
	.private_extern _Or__uOr_var_C_I
_Or__uOr_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_C_i64
	.private_extern _Or__uOr_var_C_i64
_Or__uOr_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_C_C
	.private_extern _Or__uOr_var_C_C
_Or__uOr_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_C_u8
	.private_extern _Or__uOr_var_C_u8
_Or__uOr_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_C_i8
	.private_extern _Or__uOr_var_C_i8
_Or__uOr_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_C_L
	.private_extern _Or__uOr_var_C_L
_Or__uOr_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
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
.globl _Or__uOr_var_C_u32
	.private_extern _Or__uOr_var_C_u32
_Or__uOr_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u8_u16
	.private_extern _Or__uOr_var_u8_u16
_Or__uOr_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Or(%rip), %eax
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
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u8_LC
	.private_extern _Or__uOr_var_u8_LC
_Or__uOr_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u8_i16
	.private_extern _Or__uOr_var_u8_i16
_Or__uOr_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Or(%rip), %eax
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
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u8_I
	.private_extern _Or__uOr_var_u8_I
_Or__uOr_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u8_i64
	.private_extern _Or__uOr_var_u8_i64
_Or__uOr_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u8_C
	.private_extern _Or__uOr_var_u8_C
_Or__uOr_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u8_u8
	.private_extern _Or__uOr_var_u8_u8
_Or__uOr_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u8_i8
	.private_extern _Or__uOr_var_u8_i8
_Or__uOr_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u8_L
	.private_extern _Or__uOr_var_u8_L
_Or__uOr_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u8_u32
	.private_extern _Or__uOr_var_u8_u32
_Or__uOr_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i8_u16
	.private_extern _Or__uOr_var_i8_u16
_Or__uOr_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Or(%rip), %eax
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
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i8_LC
	.private_extern _Or__uOr_var_i8_LC
_Or__uOr_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i8_i16
	.private_extern _Or__uOr_var_i8_i16
_Or__uOr_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Or(%rip), %eax
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
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i8_I
	.private_extern _Or__uOr_var_i8_I
_Or__uOr_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i8_i64
	.private_extern _Or__uOr_var_i8_i64
_Or__uOr_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i8_C
	.private_extern _Or__uOr_var_i8_C
_Or__uOr_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_i8_u8
	.private_extern _Or__uOr_var_i8_u8
_Or__uOr_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i8_i8
	.private_extern _Or__uOr_var_i8_i8
_Or__uOr_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i8_L
	.private_extern _Or__uOr_var_i8_L
_Or__uOr_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_i8_u32
	.private_extern _Or__uOr_var_i8_u32
_Or__uOr_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_L_u16
	.private_extern _Or__uOr_var_L_u16
_Or__uOr_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Or(%rip), %eax
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
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_L_LC
	.private_extern _Or__uOr_var_L_LC
_Or__uOr_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_L_i16
	.private_extern _Or__uOr_var_L_i16
_Or__uOr_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Or(%rip), %eax
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
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_L_I
	.private_extern _Or__uOr_var_L_I
_Or__uOr_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_L_i64
	.private_extern _Or__uOr_var_L_i64
_Or__uOr_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Or(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_L_C
	.private_extern _Or__uOr_var_L_C
_Or__uOr_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Or(%rip), %rdx
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
.globl _Or__uOr_var_L_u8
	.private_extern _Or__uOr_var_L_u8
_Or__uOr_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_L_i8
	.private_extern _Or__uOr_var_L_i8
_Or__uOr_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_L_u32
	.private_extern _Or__uOr_var_L_u32
_Or__uOr_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u32_u16
	.private_extern _Or__uOr_var_u32_u16
_Or__uOr_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Or(%rip), %eax
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
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u32_LC
	.private_extern _Or__uOr_var_u32_LC
_Or__uOr_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u32_i16
	.private_extern _Or__uOr_var_u32_i16
_Or__uOr_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Or(%rip), %eax
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
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u32_I
	.private_extern _Or__uOr_var_u32_I
_Or__uOr_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u32_i64
	.private_extern _Or__uOr_var_u32_i64
_Or__uOr_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u32_C
	.private_extern _Or__uOr_var_u32_C
_Or__uOr_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Or(%rip), %rax
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
.globl _Or__uOr_var_u32_u8
	.private_extern _Or__uOr_var_u32_u8
_Or__uOr_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u32_i8
	.private_extern _Or__uOr_var_u32_i8
_Or__uOr_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u32_L
	.private_extern _Or__uOr_var_u32_L
_Or__uOr_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
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
.globl _Or__uOr_var_u32_u32
	.private_extern _Or__uOr_var_u32_u32
_Or__uOr_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Or(%rip), %eax
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
	.ascii "uOr_param_u32_u32"
	.space 1
	.ascii "uOr_var_u32_u32"
	.space 1
	.ascii "uOr_param_u32_L"
	.space 1
	.ascii "uOr_var_u32_L"
	.space 1
	.ascii "uOr_param_u32_i8"
	.space 1
	.ascii "uOr_var_u32_i8"
	.space 1
	.ascii "uOr_param_u32_u8"
	.space 1
	.ascii "uOr_var_u32_u8"
	.space 1
	.ascii "uOr_param_u32_C"
	.space 1
	.ascii "uOr_var_u32_C"
	.space 1
	.ascii "uOr_param_u32_i64"
	.space 1
	.ascii "uOr_var_u32_i64"
	.space 1
	.ascii "uOr_param_u32_I"
	.space 1
	.ascii "uOr_var_u32_I"
	.space 1
	.ascii "uOr_param_u32_i32"
	.space 1
	.ascii "uOr_var_u32_i32"
	.space 1
	.ascii "uOr_param_u32_i16"
	.space 1
	.ascii "uOr_var_u32_i16"
	.space 1
	.ascii "uOr_param_u32_LC"
	.space 1
	.ascii "uOr_var_u32_LC"
	.space 1
	.ascii "uOr_param_u32_u64"
	.space 1
	.ascii "uOr_var_u32_u64"
	.space 1
	.ascii "uOr_param_u32_u16"
	.space 1
	.ascii "uOr_var_u32_u16"
	.space 1
	.ascii "uOr_param_L_u32"
	.space 1
	.ascii "uOr_var_L_u32"
	.space 1
	.ascii "uOr_param_L_L"
	.space 1
	.ascii "uOr_var_L_L"
	.space 1
	.ascii "uOr_param_L_i8"
	.space 1
	.ascii "uOr_var_L_i8"
	.space 1
	.ascii "uOr_param_L_u8"
	.space 1
	.ascii "uOr_var_L_u8"
	.space 1
	.ascii "uOr_param_L_C"
	.space 1
	.ascii "uOr_var_L_C"
	.space 1
	.ascii "uOr_param_L_i64"
	.space 1
	.ascii "uOr_var_L_i64"
	.space 1
	.ascii "uOr_param_L_I"
	.space 1
	.ascii "uOr_var_L_I"
	.space 1
	.ascii "uOr_param_L_i32"
	.space 1
	.ascii "uOr_var_L_i32"
	.space 1
	.ascii "uOr_param_L_i16"
	.space 1
	.ascii "uOr_var_L_i16"
	.space 1
	.ascii "uOr_param_L_LC"
	.space 1
	.ascii "uOr_var_L_LC"
	.space 1
	.ascii "uOr_param_L_u64"
	.space 1
	.ascii "uOr_var_L_u64"
	.space 1
	.ascii "uOr_param_L_u16"
	.space 1
	.ascii "uOr_var_L_u16"
	.space 1
	.ascii "uOr_param_i8_u32"
	.space 1
	.ascii "uOr_var_i8_u32"
	.space 1
	.ascii "uOr_param_i8_L"
	.space 1
	.ascii "uOr_var_i8_L"
	.space 1
	.ascii "uOr_param_i8_i8"
	.space 1
	.ascii "uOr_var_i8_i8"
	.space 1
	.ascii "uOr_param_i8_u8"
	.space 1
	.ascii "uOr_var_i8_u8"
	.space 1
	.ascii "uOr_param_i8_C"
	.space 1
	.ascii "uOr_var_i8_C"
	.space 1
	.ascii "uOr_param_i8_i64"
	.space 1
	.ascii "uOr_var_i8_i64"
	.space 1
	.ascii "uOr_param_i8_I"
	.space 1
	.ascii "uOr_var_i8_I"
	.space 1
	.ascii "uOr_param_i8_i32"
	.space 1
	.ascii "uOr_var_i8_i32"
	.space 1
	.ascii "uOr_param_i8_i16"
	.space 1
	.ascii "uOr_var_i8_i16"
	.space 1
	.ascii "uOr_param_i8_LC"
	.space 1
	.ascii "uOr_var_i8_LC"
	.space 1
	.ascii "uOr_param_i8_u64"
	.space 1
	.ascii "uOr_var_i8_u64"
	.space 1
	.ascii "uOr_param_i8_u16"
	.space 1
	.ascii "uOr_var_i8_u16"
	.space 1
	.ascii "uOr_param_u8_u32"
	.space 1
	.ascii "uOr_var_u8_u32"
	.space 1
	.ascii "uOr_param_u8_L"
	.space 1
	.ascii "uOr_var_u8_L"
	.space 1
	.ascii "uOr_param_u8_i8"
	.space 1
	.ascii "uOr_var_u8_i8"
	.space 1
	.ascii "uOr_param_u8_u8"
	.space 1
	.ascii "uOr_var_u8_u8"
	.space 1
	.ascii "uOr_param_u8_C"
	.space 1
	.ascii "uOr_var_u8_C"
	.space 1
	.ascii "uOr_param_u8_i64"
	.space 1
	.ascii "uOr_var_u8_i64"
	.space 1
	.ascii "uOr_param_u8_I"
	.space 1
	.ascii "uOr_var_u8_I"
	.space 1
	.ascii "uOr_param_u8_i32"
	.space 1
	.ascii "uOr_var_u8_i32"
	.space 1
	.ascii "uOr_param_u8_i16"
	.space 1
	.ascii "uOr_var_u8_i16"
	.space 1
	.ascii "uOr_param_u8_LC"
	.space 1
	.ascii "uOr_var_u8_LC"
	.space 1
	.ascii "uOr_param_u8_u64"
	.space 1
	.ascii "uOr_var_u8_u64"
	.space 1
	.ascii "uOr_param_u8_u16"
	.space 1
	.ascii "uOr_var_u8_u16"
	.space 1
	.ascii "uOr_param_C_u32"
	.space 1
	.ascii "uOr_var_C_u32"
	.space 1
	.ascii "uOr_param_C_L"
	.space 1
	.ascii "uOr_var_C_L"
	.space 1
	.ascii "uOr_param_C_i8"
	.space 1
	.ascii "uOr_var_C_i8"
	.space 1
	.ascii "uOr_param_C_u8"
	.space 1
	.ascii "uOr_var_C_u8"
	.space 1
	.ascii "uOr_param_C_C"
	.space 1
	.ascii "uOr_var_C_C"
	.space 1
	.ascii "uOr_param_C_i64"
	.space 1
	.ascii "uOr_var_C_i64"
	.space 1
	.ascii "uOr_param_C_I"
	.space 1
	.ascii "uOr_var_C_I"
	.space 1
	.ascii "uOr_param_C_i32"
	.space 1
	.ascii "uOr_var_C_i32"
	.space 1
	.ascii "uOr_param_C_i16"
	.space 1
	.ascii "uOr_var_C_i16"
	.space 1
	.ascii "uOr_param_C_LC"
	.space 1
	.ascii "uOr_var_C_LC"
	.space 1
	.ascii "uOr_param_C_u64"
	.space 1
	.ascii "uOr_var_C_u64"
	.space 1
	.ascii "uOr_param_C_u16"
	.space 1
	.ascii "uOr_var_C_u16"
	.space 1
	.ascii "uOr_param_i64_u32"
	.space 1
	.ascii "uOr_var_i64_u32"
	.space 1
	.ascii "uOr_param_i64_L"
	.space 1
	.ascii "uOr_var_i64_L"
	.space 1
	.ascii "uOr_param_i64_i8"
	.space 1
	.ascii "uOr_var_i64_i8"
	.space 1
	.ascii "uOr_param_i64_u8"
	.space 1
	.ascii "uOr_var_i64_u8"
	.space 1
	.ascii "uOr_param_i64_C"
	.space 1
	.ascii "uOr_var_i64_C"
	.space 1
	.ascii "uOr_param_i64_i64"
	.space 1
	.ascii "uOr_var_i64_i64"
	.space 1
	.ascii "uOr_param_i64_I"
	.space 1
	.ascii "uOr_var_i64_I"
	.space 1
	.ascii "uOr_param_i64_i32"
	.space 1
	.ascii "uOr_var_i64_i32"
	.space 1
	.ascii "uOr_param_i64_i16"
	.space 1
	.ascii "uOr_var_i64_i16"
	.space 1
	.ascii "uOr_param_i64_LC"
	.space 1
	.ascii "uOr_var_i64_LC"
	.space 1
	.ascii "uOr_param_i64_u64"
	.space 1
	.ascii "uOr_var_i64_u64"
	.space 1
	.ascii "uOr_param_i64_u16"
	.space 1
	.ascii "uOr_var_i64_u16"
	.space 1
	.ascii "uOr_param_I_u32"
	.space 1
	.ascii "uOr_var_I_u32"
	.space 1
	.ascii "uOr_param_I_L"
	.space 1
	.ascii "uOr_var_I_L"
	.space 1
	.ascii "uOr_param_I_i8"
	.space 1
	.ascii "uOr_var_I_i8"
	.space 1
	.ascii "uOr_param_I_u8"
	.space 1
	.ascii "uOr_var_I_u8"
	.space 1
	.ascii "uOr_param_I_C"
	.space 1
	.ascii "uOr_var_I_C"
	.space 1
	.ascii "uOr_param_I_i64"
	.space 1
	.ascii "uOr_var_I_i64"
	.space 1
	.ascii "uOr_param_I_I"
	.space 1
	.ascii "uOr_var_I_I"
	.space 1
	.ascii "uOr_param_I_i32"
	.space 1
	.ascii "uOr_var_I_i32"
	.space 1
	.ascii "uOr_param_I_i16"
	.space 1
	.ascii "uOr_var_I_i16"
	.space 1
	.ascii "uOr_param_I_LC"
	.space 1
	.ascii "uOr_var_I_LC"
	.space 1
	.ascii "uOr_param_I_u64"
	.space 1
	.ascii "uOr_var_I_u64"
	.space 1
	.ascii "uOr_param_I_u16"
	.space 1
	.ascii "uOr_var_I_u16"
	.space 1
	.ascii "uOr_param_i32_u32"
	.space 1
	.ascii "uOr_var_i32_u32"
	.space 1
	.ascii "uOr_param_i32_L"
	.space 1
	.ascii "uOr_var_i32_L"
	.space 1
	.ascii "uOr_param_i32_i8"
	.space 1
	.ascii "uOr_var_i32_i8"
	.space 1
	.ascii "uOr_param_i32_u8"
	.space 1
	.ascii "uOr_var_i32_u8"
	.space 1
	.ascii "uOr_param_i32_C"
	.space 1
	.ascii "uOr_var_i32_C"
	.space 1
	.ascii "uOr_param_i32_i64"
	.space 1
	.ascii "uOr_var_i32_i64"
	.space 1
	.ascii "uOr_param_i32_I"
	.space 1
	.ascii "uOr_var_i32_I"
	.space 1
	.ascii "uOr_param_i32_i32"
	.space 1
	.ascii "uOr_var_i32_i32"
	.space 1
	.ascii "uOr_param_i32_i16"
	.space 1
	.ascii "uOr_var_i32_i16"
	.space 1
	.ascii "uOr_param_i32_LC"
	.space 1
	.ascii "uOr_var_i32_LC"
	.space 1
	.ascii "uOr_param_i32_u64"
	.space 1
	.ascii "uOr_var_i32_u64"
	.space 1
	.ascii "uOr_param_i32_u16"
	.space 1
	.ascii "uOr_var_i32_u16"
	.space 1
	.ascii "uOr_param_i16_u32"
	.space 1
	.ascii "uOr_var_i16_u32"
	.space 1
	.ascii "uOr_param_i16_L"
	.space 1
	.ascii "uOr_var_i16_L"
	.space 1
	.ascii "uOr_param_i16_i8"
	.space 1
	.ascii "uOr_var_i16_i8"
	.space 1
	.ascii "uOr_param_i16_u8"
	.space 1
	.ascii "uOr_var_i16_u8"
	.space 1
	.ascii "uOr_param_i16_C"
	.space 1
	.ascii "uOr_var_i16_C"
	.space 1
	.ascii "uOr_param_i16_i64"
	.space 1
	.ascii "uOr_var_i16_i64"
	.space 1
	.ascii "uOr_param_i16_I"
	.space 1
	.ascii "uOr_var_i16_I"
	.space 1
	.ascii "uOr_param_i16_i32"
	.space 1
	.ascii "uOr_var_i16_i32"
	.space 1
	.ascii "uOr_param_i16_i16"
	.space 1
	.ascii "uOr_var_i16_i16"
	.space 1
	.ascii "uOr_param_i16_LC"
	.space 1
	.ascii "uOr_var_i16_LC"
	.space 1
	.ascii "uOr_param_i16_u64"
	.space 1
	.ascii "uOr_var_i16_u64"
	.space 1
	.ascii "uOr_param_i16_u16"
	.space 1
	.ascii "uOr_var_i16_u16"
	.space 1
	.ascii "uOr_param_LC_u32"
	.space 1
	.ascii "uOr_var_LC_u32"
	.space 1
	.ascii "uOr_param_LC_L"
	.space 1
	.ascii "uOr_var_LC_L"
	.space 1
	.ascii "uOr_param_LC_i8"
	.space 1
	.ascii "uOr_var_LC_i8"
	.space 1
	.ascii "uOr_param_LC_u8"
	.space 1
	.ascii "uOr_var_LC_u8"
	.space 1
	.ascii "uOr_param_LC_C"
	.space 1
	.ascii "uOr_var_LC_C"
	.space 1
	.ascii "uOr_param_LC_i64"
	.space 1
	.ascii "uOr_var_LC_i64"
	.space 1
	.ascii "uOr_param_LC_I"
	.space 1
	.ascii "uOr_var_LC_I"
	.space 1
	.ascii "uOr_param_LC_i32"
	.space 1
	.ascii "uOr_var_LC_i32"
	.space 1
	.ascii "uOr_param_LC_i16"
	.space 1
	.ascii "uOr_var_LC_i16"
	.space 1
	.ascii "uOr_param_LC_LC"
	.space 1
	.ascii "uOr_var_LC_LC"
	.space 1
	.ascii "uOr_param_LC_u64"
	.space 1
	.ascii "uOr_var_LC_u64"
	.space 1
	.ascii "uOr_param_LC_u16"
	.space 1
	.ascii "uOr_var_LC_u16"
	.space 1
	.ascii "uOr_param_u64_u32"
	.space 1
	.ascii "uOr_var_u64_u32"
	.space 1
	.ascii "uOr_param_u64_L"
	.space 1
	.ascii "uOr_var_u64_L"
	.space 1
	.ascii "uOr_param_u64_i8"
	.space 1
	.ascii "uOr_var_u64_i8"
	.space 1
	.ascii "uOr_param_u64_u8"
	.space 1
	.ascii "uOr_var_u64_u8"
	.space 1
	.ascii "uOr_param_u64_C"
	.space 1
	.ascii "uOr_var_u64_C"
	.space 1
	.ascii "uOr_param_u64_i64"
	.space 1
	.ascii "uOr_var_u64_i64"
	.space 1
	.ascii "uOr_param_u64_I"
	.space 1
	.ascii "uOr_var_u64_I"
	.space 1
	.ascii "uOr_param_u64_i32"
	.space 1
	.ascii "uOr_var_u64_i32"
	.space 1
	.ascii "uOr_param_u64_i16"
	.space 1
	.ascii "uOr_var_u64_i16"
	.space 1
	.ascii "uOr_param_u64_LC"
	.space 1
	.ascii "uOr_var_u64_LC"
	.space 1
	.ascii "uOr_param_u64_u64"
	.space 1
	.ascii "uOr_var_u64_u64"
	.space 1
	.ascii "uOr_param_u64_u16"
	.space 1
	.ascii "uOr_var_u64_u16"
	.space 1
	.ascii "uOr_param_u16_u32"
	.space 1
	.ascii "uOr_var_u16_u32"
	.space 1
	.ascii "uOr_param_u16_L"
	.space 1
	.ascii "uOr_var_u16_L"
	.space 1
	.ascii "uOr_param_u16_i8"
	.space 1
	.ascii "uOr_var_u16_i8"
	.space 1
	.ascii "uOr_param_u16_u8"
	.space 1
	.ascii "uOr_var_u16_u8"
	.space 1
	.ascii "uOr_param_u16_C"
	.space 1
	.ascii "uOr_var_u16_C"
	.space 1
	.ascii "uOr_param_u16_i64"
	.space 1
	.ascii "uOr_var_u16_i64"
	.space 1
	.ascii "uOr_param_u16_I"
	.space 1
	.ascii "uOr_var_u16_I"
	.space 1
	.ascii "uOr_param_u16_i32"
	.space 1
	.ascii "uOr_var_u16_i32"
	.space 1
	.ascii "uOr_param_u16_i16"
	.space 1
	.ascii "uOr_var_u16_i16"
	.space 1
	.ascii "uOr_param_u16_LC"
	.space 1
	.ascii "uOr_var_u16_LC"
	.space 1
	.ascii "uOr_param_u16_u64"
	.space 1
	.ascii "uOr_var_u16_u64"
	.space 1
	.ascii "uOr_param_u16_u16"
	.space 1
	.ascii "uOr_var_u16_u16"
	.space 3
	.quad	_Or_M3
	.quad	_L_1
	.quad	_Or__uOr_param_u32_u32
	.quad	_L_1+6
	.quad	_Or__uOr_var_u32_u32
	.quad	_L_1+24
	.quad	_Or__uOr_param_u32_L
	.quad	_L_1+40
	.quad	_Or__uOr_var_u32_L
	.quad	_L_1+56
	.quad	_Or__uOr_param_u32_i8
	.quad	_L_1+70
	.quad	_Or__uOr_var_u32_i8
	.quad	_L_1+87
	.quad	_Or__uOr_param_u32_u8
	.quad	_L_1+102
	.quad	_Or__uOr_var_u32_u8
	.quad	_L_1+119
	.quad	_Or__uOr_param_u32_C
	.quad	_L_1+134
	.quad	_Or__uOr_var_u32_C
	.quad	_L_1+150
	.quad	_Or__uOr_param_u32_i64
	.quad	_L_1+164
	.quad	_Or__uOr_var_u32_i64
	.quad	_L_1+182
	.quad	_Or__uOr_param_u32_I
	.quad	_L_1+198
	.quad	_Or__uOr_var_u32_I
	.quad	_L_1+214
	.quad	_Or__uOr_param_u32_i32
	.quad	_L_1+228
	.quad	_Or__uOr_var_u32_i32
	.quad	_L_1+246
	.quad	_Or__uOr_param_u32_i16
	.quad	_L_1+262
	.quad	_Or__uOr_var_u32_i16
	.quad	_L_1+280
	.quad	_Or__uOr_param_u32_LC
	.quad	_L_1+296
	.quad	_Or__uOr_var_u32_LC
	.quad	_L_1+313
	.quad	_Or__uOr_param_u32_u64
	.quad	_L_1+328
	.quad	_Or__uOr_var_u32_u64
	.quad	_L_1+346
	.quad	_Or__uOr_param_u32_u16
	.quad	_L_1+362
	.quad	_Or__uOr_var_u32_u16
	.quad	_L_1+380
	.quad	_Or__uOr_param_L_u32
	.quad	_L_1+396
	.quad	_Or__uOr_var_L_u32
	.quad	_L_1+412
	.quad	_Or__uOr_param_L_L
	.quad	_L_1+426
	.quad	_Or__uOr_var_L_L
	.quad	_L_1+440
	.quad	_Or__uOr_param_L_i8
	.quad	_L_1+452
	.quad	_Or__uOr_var_L_i8
	.quad	_L_1+467
	.quad	_Or__uOr_param_L_u8
	.quad	_L_1+480
	.quad	_Or__uOr_var_L_u8
	.quad	_L_1+495
	.quad	_Or__uOr_param_L_C
	.quad	_L_1+508
	.quad	_Or__uOr_var_L_C
	.quad	_L_1+522
	.quad	_Or__uOr_param_L_i64
	.quad	_L_1+534
	.quad	_Or__uOr_var_L_i64
	.quad	_L_1+550
	.quad	_Or__uOr_param_L_I
	.quad	_L_1+564
	.quad	_Or__uOr_var_L_I
	.quad	_L_1+578
	.quad	_Or__uOr_param_L_i32
	.quad	_L_1+590
	.quad	_Or__uOr_var_L_i32
	.quad	_L_1+606
	.quad	_Or__uOr_param_L_i16
	.quad	_L_1+620
	.quad	_Or__uOr_var_L_i16
	.quad	_L_1+636
	.quad	_Or__uOr_param_L_LC
	.quad	_L_1+650
	.quad	_Or__uOr_var_L_LC
	.quad	_L_1+665
	.quad	_Or__uOr_param_L_u64
	.quad	_L_1+678
	.quad	_Or__uOr_var_L_u64
	.quad	_L_1+694
	.quad	_Or__uOr_param_L_u16
	.quad	_L_1+708
	.quad	_Or__uOr_var_L_u16
	.quad	_L_1+724
	.quad	_Or__uOr_param_i8_u32
	.quad	_L_1+738
	.quad	_Or__uOr_var_i8_u32
	.quad	_L_1+755
	.quad	_Or__uOr_param_i8_L
	.quad	_L_1+770
	.quad	_Or__uOr_var_i8_L
	.quad	_L_1+785
	.quad	_Or__uOr_param_i8_i8
	.quad	_L_1+798
	.quad	_Or__uOr_var_i8_i8
	.quad	_L_1+814
	.quad	_Or__uOr_param_i8_u8
	.quad	_L_1+828
	.quad	_Or__uOr_var_i8_u8
	.quad	_L_1+844
	.quad	_Or__uOr_param_i8_C
	.quad	_L_1+858
	.quad	_Or__uOr_var_i8_C
	.quad	_L_1+873
	.quad	_Or__uOr_param_i8_i64
	.quad	_L_1+886
	.quad	_Or__uOr_var_i8_i64
	.quad	_L_1+903
	.quad	_Or__uOr_param_i8_I
	.quad	_L_1+918
	.quad	_Or__uOr_var_i8_I
	.quad	_L_1+933
	.quad	_Or__uOr_param_i8_i32
	.quad	_L_1+946
	.quad	_Or__uOr_var_i8_i32
	.quad	_L_1+963
	.quad	_Or__uOr_param_i8_i16
	.quad	_L_1+978
	.quad	_Or__uOr_var_i8_i16
	.quad	_L_1+995
	.quad	_Or__uOr_param_i8_LC
	.quad	_L_1+1010
	.quad	_Or__uOr_var_i8_LC
	.quad	_L_1+1026
	.quad	_Or__uOr_param_i8_u64
	.quad	_L_1+1040
	.quad	_Or__uOr_var_i8_u64
	.quad	_L_1+1057
	.quad	_Or__uOr_param_i8_u16
	.quad	_L_1+1072
	.quad	_Or__uOr_var_i8_u16
	.quad	_L_1+1089
	.quad	_Or__uOr_param_u8_u32
	.quad	_L_1+1104
	.quad	_Or__uOr_var_u8_u32
	.quad	_L_1+1121
	.quad	_Or__uOr_param_u8_L
	.quad	_L_1+1136
	.quad	_Or__uOr_var_u8_L
	.quad	_L_1+1151
	.quad	_Or__uOr_param_u8_i8
	.quad	_L_1+1164
	.quad	_Or__uOr_var_u8_i8
	.quad	_L_1+1180
	.quad	_Or__uOr_param_u8_u8
	.quad	_L_1+1194
	.quad	_Or__uOr_var_u8_u8
	.quad	_L_1+1210
	.quad	_Or__uOr_param_u8_C
	.quad	_L_1+1224
	.quad	_Or__uOr_var_u8_C
	.quad	_L_1+1239
	.quad	_Or__uOr_param_u8_i64
	.quad	_L_1+1252
	.quad	_Or__uOr_var_u8_i64
	.quad	_L_1+1269
	.quad	_Or__uOr_param_u8_I
	.quad	_L_1+1284
	.quad	_Or__uOr_var_u8_I
	.quad	_L_1+1299
	.quad	_Or__uOr_param_u8_i32
	.quad	_L_1+1312
	.quad	_Or__uOr_var_u8_i32
	.quad	_L_1+1329
	.quad	_Or__uOr_param_u8_i16
	.quad	_L_1+1344
	.quad	_Or__uOr_var_u8_i16
	.quad	_L_1+1361
	.quad	_Or__uOr_param_u8_LC
	.quad	_L_1+1376
	.quad	_Or__uOr_var_u8_LC
	.quad	_L_1+1392
	.quad	_Or__uOr_param_u8_u64
	.quad	_L_1+1406
	.quad	_Or__uOr_var_u8_u64
	.quad	_L_1+1423
	.quad	_Or__uOr_param_u8_u16
	.quad	_L_1+1438
	.quad	_Or__uOr_var_u8_u16
	.quad	_L_1+1455
	.quad	_Or__uOr_param_C_u32
	.quad	_L_1+1470
	.quad	_Or__uOr_var_C_u32
	.quad	_L_1+1486
	.quad	_Or__uOr_param_C_L
	.quad	_L_1+1500
	.quad	_Or__uOr_var_C_L
	.quad	_L_1+1514
	.quad	_Or__uOr_param_C_i8
	.quad	_L_1+1526
	.quad	_Or__uOr_var_C_i8
	.quad	_L_1+1541
	.quad	_Or__uOr_param_C_u8
	.quad	_L_1+1554
	.quad	_Or__uOr_var_C_u8
	.quad	_L_1+1569
	.quad	_Or__uOr_param_C_C
	.quad	_L_1+1582
	.quad	_Or__uOr_var_C_C
	.quad	_L_1+1596
	.quad	_Or__uOr_param_C_i64
	.quad	_L_1+1608
	.quad	_Or__uOr_var_C_i64
	.quad	_L_1+1624
	.quad	_Or__uOr_param_C_I
	.quad	_L_1+1638
	.quad	_Or__uOr_var_C_I
	.quad	_L_1+1652
	.quad	_Or__uOr_param_C_i32
	.quad	_L_1+1664
	.quad	_Or__uOr_var_C_i32
	.quad	_L_1+1680
	.quad	_Or__uOr_param_C_i16
	.quad	_L_1+1694
	.quad	_Or__uOr_var_C_i16
	.quad	_L_1+1710
	.quad	_Or__uOr_param_C_LC
	.quad	_L_1+1724
	.quad	_Or__uOr_var_C_LC
	.quad	_L_1+1739
	.quad	_Or__uOr_param_C_u64
	.quad	_L_1+1752
	.quad	_Or__uOr_var_C_u64
	.quad	_L_1+1768
	.quad	_Or__uOr_param_C_u16
	.quad	_L_1+1782
	.quad	_Or__uOr_var_C_u16
	.quad	_L_1+1798
	.quad	_Or__uOr_param_i64_u32
	.quad	_L_1+1812
	.quad	_Or__uOr_var_i64_u32
	.quad	_L_1+1830
	.quad	_Or__uOr_param_i64_L
	.quad	_L_1+1846
	.quad	_Or__uOr_var_i64_L
	.quad	_L_1+1862
	.quad	_Or__uOr_param_i64_i8
	.quad	_L_1+1876
	.quad	_Or__uOr_var_i64_i8
	.quad	_L_1+1893
	.quad	_Or__uOr_param_i64_u8
	.quad	_L_1+1908
	.quad	_Or__uOr_var_i64_u8
	.quad	_L_1+1925
	.quad	_Or__uOr_param_i64_C
	.quad	_L_1+1940
	.quad	_Or__uOr_var_i64_C
	.quad	_L_1+1956
	.quad	_Or__uOr_param_i64_i64
	.quad	_L_1+1970
	.quad	_Or__uOr_var_i64_i64
	.quad	_L_1+1988
	.quad	_Or__uOr_param_i64_I
	.quad	_L_1+2004
	.quad	_Or__uOr_var_i64_I
	.quad	_L_1+2020
	.quad	_Or__uOr_param_i64_i32
	.quad	_L_1+2034
	.quad	_Or__uOr_var_i64_i32
	.quad	_L_1+2052
	.quad	_Or__uOr_param_i64_i16
	.quad	_L_1+2068
	.quad	_Or__uOr_var_i64_i16
	.quad	_L_1+2086
	.quad	_Or__uOr_param_i64_LC
	.quad	_L_1+2102
	.quad	_Or__uOr_var_i64_LC
	.quad	_L_1+2119
	.quad	_Or__uOr_param_i64_u64
	.quad	_L_1+2134
	.quad	_Or__uOr_var_i64_u64
	.quad	_L_1+2152
	.quad	_Or__uOr_param_i64_u16
	.quad	_L_1+2168
	.quad	_Or__uOr_var_i64_u16
	.quad	_L_1+2186
	.quad	_Or__uOr_param_I_u32
	.quad	_L_1+2202
	.quad	_Or__uOr_var_I_u32
	.quad	_L_1+2218
	.quad	_Or__uOr_param_I_L
	.quad	_L_1+2232
	.quad	_Or__uOr_var_I_L
	.quad	_L_1+2246
	.quad	_Or__uOr_param_I_i8
	.quad	_L_1+2258
	.quad	_Or__uOr_var_I_i8
	.quad	_L_1+2273
	.quad	_Or__uOr_param_I_u8
	.quad	_L_1+2286
	.quad	_Or__uOr_var_I_u8
	.quad	_L_1+2301
	.quad	_Or__uOr_param_I_C
	.quad	_L_1+2314
	.quad	_Or__uOr_var_I_C
	.quad	_L_1+2328
	.quad	_Or__uOr_param_I_i64
	.quad	_L_1+2340
	.quad	_Or__uOr_var_I_i64
	.quad	_L_1+2356
	.quad	_Or__uOr_param_I_I
	.quad	_L_1+2370
	.quad	_Or__uOr_var_I_I
	.quad	_L_1+2384
	.quad	_Or__uOr_param_I_i32
	.quad	_L_1+2396
	.quad	_Or__uOr_var_I_i32
	.quad	_L_1+2412
	.quad	_Or__uOr_param_I_i16
	.quad	_L_1+2426
	.quad	_Or__uOr_var_I_i16
	.quad	_L_1+2442
	.quad	_Or__uOr_param_I_LC
	.quad	_L_1+2456
	.quad	_Or__uOr_var_I_LC
	.quad	_L_1+2471
	.quad	_Or__uOr_param_I_u64
	.quad	_L_1+2484
	.quad	_Or__uOr_var_I_u64
	.quad	_L_1+2500
	.quad	_Or__uOr_param_I_u16
	.quad	_L_1+2514
	.quad	_Or__uOr_var_I_u16
	.quad	_L_1+2530
	.quad	_Or__uOr_param_i32_u32
	.quad	_L_1+2544
	.quad	_Or__uOr_var_i32_u32
	.quad	_L_1+2562
	.quad	_Or__uOr_param_i32_L
	.quad	_L_1+2578
	.quad	_Or__uOr_var_i32_L
	.quad	_L_1+2594
	.quad	_Or__uOr_param_i32_i8
	.quad	_L_1+2608
	.quad	_Or__uOr_var_i32_i8
	.quad	_L_1+2625
	.quad	_Or__uOr_param_i32_u8
	.quad	_L_1+2640
	.quad	_Or__uOr_var_i32_u8
	.quad	_L_1+2657
	.quad	_Or__uOr_param_i32_C
	.quad	_L_1+2672
	.quad	_Or__uOr_var_i32_C
	.quad	_L_1+2688
	.quad	_Or__uOr_param_i32_i64
	.quad	_L_1+2702
	.quad	_Or__uOr_var_i32_i64
	.quad	_L_1+2720
	.quad	_Or__uOr_param_i32_I
	.quad	_L_1+2736
	.quad	_Or__uOr_var_i32_I
	.quad	_L_1+2752
	.quad	_Or__uOr_param_i32_i32
	.quad	_L_1+2766
	.quad	_Or__uOr_var_i32_i32
	.quad	_L_1+2784
	.quad	_Or__uOr_param_i32_i16
	.quad	_L_1+2800
	.quad	_Or__uOr_var_i32_i16
	.quad	_L_1+2818
	.quad	_Or__uOr_param_i32_LC
	.quad	_L_1+2834
	.quad	_Or__uOr_var_i32_LC
	.quad	_L_1+2851
	.quad	_Or__uOr_param_i32_u64
	.quad	_L_1+2866
	.quad	_Or__uOr_var_i32_u64
	.quad	_L_1+2884
	.quad	_Or__uOr_param_i32_u16
	.quad	_L_1+2900
	.quad	_Or__uOr_var_i32_u16
	.quad	_L_1+2918
	.quad	_Or__uOr_param_i16_u32
	.quad	_L_1+2934
	.quad	_Or__uOr_var_i16_u32
	.quad	_L_1+2952
	.quad	_Or__uOr_param_i16_L
	.quad	_L_1+2968
	.quad	_Or__uOr_var_i16_L
	.quad	_L_1+2984
	.quad	_Or__uOr_param_i16_i8
	.quad	_L_1+2998
	.quad	_Or__uOr_var_i16_i8
	.quad	_L_1+3015
	.quad	_Or__uOr_param_i16_u8
	.quad	_L_1+3030
	.quad	_Or__uOr_var_i16_u8
	.quad	_L_1+3047
	.quad	_Or__uOr_param_i16_C
	.quad	_L_1+3062
	.quad	_Or__uOr_var_i16_C
	.quad	_L_1+3078
	.quad	_Or__uOr_param_i16_i64
	.quad	_L_1+3092
	.quad	_Or__uOr_var_i16_i64
	.quad	_L_1+3110
	.quad	_Or__uOr_param_i16_I
	.quad	_L_1+3126
	.quad	_Or__uOr_var_i16_I
	.quad	_L_1+3142
	.quad	_Or__uOr_param_i16_i32
	.quad	_L_1+3156
	.quad	_Or__uOr_var_i16_i32
	.quad	_L_1+3174
	.quad	_Or__uOr_param_i16_i16
	.quad	_L_1+3190
	.quad	_Or__uOr_var_i16_i16
	.quad	_L_1+3208
	.quad	_Or__uOr_param_i16_LC
	.quad	_L_1+3224
	.quad	_Or__uOr_var_i16_LC
	.quad	_L_1+3241
	.quad	_Or__uOr_param_i16_u64
	.quad	_L_1+3256
	.quad	_Or__uOr_var_i16_u64
	.quad	_L_1+3274
	.quad	_Or__uOr_param_i16_u16
	.quad	_L_1+3290
	.quad	_Or__uOr_var_i16_u16
	.quad	_L_1+3308
	.quad	_Or__uOr_param_LC_u32
	.quad	_L_1+3324
	.quad	_Or__uOr_var_LC_u32
	.quad	_L_1+3341
	.quad	_Or__uOr_param_LC_L
	.quad	_L_1+3356
	.quad	_Or__uOr_var_LC_L
	.quad	_L_1+3371
	.quad	_Or__uOr_param_LC_i8
	.quad	_L_1+3384
	.quad	_Or__uOr_var_LC_i8
	.quad	_L_1+3400
	.quad	_Or__uOr_param_LC_u8
	.quad	_L_1+3414
	.quad	_Or__uOr_var_LC_u8
	.quad	_L_1+3430
	.quad	_Or__uOr_param_LC_C
	.quad	_L_1+3444
	.quad	_Or__uOr_var_LC_C
	.quad	_L_1+3459
	.quad	_Or__uOr_param_LC_i64
	.quad	_L_1+3472
	.quad	_Or__uOr_var_LC_i64
	.quad	_L_1+3489
	.quad	_Or__uOr_param_LC_I
	.quad	_L_1+3504
	.quad	_Or__uOr_var_LC_I
	.quad	_L_1+3519
	.quad	_Or__uOr_param_LC_i32
	.quad	_L_1+3532
	.quad	_Or__uOr_var_LC_i32
	.quad	_L_1+3549
	.quad	_Or__uOr_param_LC_i16
	.quad	_L_1+3564
	.quad	_Or__uOr_var_LC_i16
	.quad	_L_1+3581
	.quad	_Or__uOr_param_LC_LC
	.quad	_L_1+3596
	.quad	_Or__uOr_var_LC_LC
	.quad	_L_1+3612
	.quad	_Or__uOr_param_LC_u64
	.quad	_L_1+3626
	.quad	_Or__uOr_var_LC_u64
	.quad	_L_1+3643
	.quad	_Or__uOr_param_LC_u16
	.quad	_L_1+3658
	.quad	_Or__uOr_var_LC_u16
	.quad	_L_1+3675
	.quad	_Or__uOr_param_u64_u32
	.quad	_L_1+3690
	.quad	_Or__uOr_var_u64_u32
	.quad	_L_1+3708
	.quad	_Or__uOr_param_u64_L
	.quad	_L_1+3724
	.quad	_Or__uOr_var_u64_L
	.quad	_L_1+3740
	.quad	_Or__uOr_param_u64_i8
	.quad	_L_1+3754
	.quad	_Or__uOr_var_u64_i8
	.quad	_L_1+3771
	.quad	_Or__uOr_param_u64_u8
	.quad	_L_1+3786
	.quad	_Or__uOr_var_u64_u8
	.quad	_L_1+3803
	.quad	_Or__uOr_param_u64_C
	.quad	_L_1+3818
	.quad	_Or__uOr_var_u64_C
	.quad	_L_1+3834
	.quad	_Or__uOr_param_u64_i64
	.quad	_L_1+3848
	.quad	_Or__uOr_var_u64_i64
	.quad	_L_1+3866
	.quad	_Or__uOr_param_u64_I
	.quad	_L_1+3882
	.quad	_Or__uOr_var_u64_I
	.quad	_L_1+3898
	.quad	_Or__uOr_param_u64_i32
	.quad	_L_1+3912
	.quad	_Or__uOr_var_u64_i32
	.quad	_L_1+3930
	.quad	_Or__uOr_param_u64_i16
	.quad	_L_1+3946
	.quad	_Or__uOr_var_u64_i16
	.quad	_L_1+3964
	.quad	_Or__uOr_param_u64_LC
	.quad	_L_1+3980
	.quad	_Or__uOr_var_u64_LC
	.quad	_L_1+3997
	.quad	_Or__uOr_param_u64_u64
	.quad	_L_1+4012
	.quad	_Or__uOr_var_u64_u64
	.quad	_L_1+4030
	.quad	_Or__uOr_param_u64_u16
	.quad	_L_1+4046
	.quad	_Or__uOr_var_u64_u16
	.quad	_L_1+4064
	.quad	_Or__uOr_param_u16_u32
	.quad	_L_1+4080
	.quad	_Or__uOr_var_u16_u32
	.quad	_L_1+4098
	.quad	_Or__uOr_param_u16_L
	.quad	_L_1+4114
	.quad	_Or__uOr_var_u16_L
	.quad	_L_1+4130
	.quad	_Or__uOr_param_u16_i8
	.quad	_L_1+4144
	.quad	_Or__uOr_var_u16_i8
	.quad	_L_1+4161
	.quad	_Or__uOr_param_u16_u8
	.quad	_L_1+4176
	.quad	_Or__uOr_var_u16_u8
	.quad	_L_1+4193
	.quad	_Or__uOr_param_u16_C
	.quad	_L_1+4208
	.quad	_Or__uOr_var_u16_C
	.quad	_L_1+4224
	.quad	_Or__uOr_param_u16_i64
	.quad	_L_1+4238
	.quad	_Or__uOr_var_u16_i64
	.quad	_L_1+4256
	.quad	_Or__uOr_param_u16_I
	.quad	_L_1+4272
	.quad	_Or__uOr_var_u16_I
	.quad	_L_1+4288
	.quad	_Or__uOr_param_u16_i32
	.quad	_L_1+4302
	.quad	_Or__uOr_var_u16_i32
	.quad	_L_1+4320
	.quad	_Or__uOr_param_u16_i16
	.quad	_L_1+4336
	.quad	_Or__uOr_var_u16_i16
	.quad	_L_1+4354
	.quad	_Or__uOr_param_u16_LC
	.quad	_L_1+4370
	.quad	_Or__uOr_var_u16_LC
	.quad	_L_1+4387
	.quad	_Or__uOr_param_u16_u64
	.quad	_L_1+4402
	.quad	_Or__uOr_var_u16_u64
	.quad	_L_1+4420
	.quad	_Or__uOr_param_u16_u16
	.quad	_L_1+4436
	.quad	_Or__uOr_var_u16_u16
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
	.word	465
	.space 6
	.quad	466
	.quad	467
	.long	103079215
	.long	1081952129
	.word	470
	.space 2
	.long	471
	.quad	472
	.quad	473
	.quad	474
	.long	1139653870
	.byte	-35
	.byte	-34
	.space 2
	.quad	479
	.long	480
	.space 28
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
