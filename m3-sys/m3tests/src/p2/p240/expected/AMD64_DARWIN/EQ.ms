	.text
.globl _EQ__EQ_var_u16_u16
	.private_extern _EQ__EQ_var_u16_u16
_EQ__EQ_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_u16
	.private_extern _EQ__EQ_param_u16_u16
_EQ__EQ_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	cmpw	-20(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_u64
	.private_extern _EQ__EQ_var_u16_u64
_EQ__EQ_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_u64
	.private_extern _EQ__EQ_param_u16_u64
_EQ__EQ_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_LC
	.private_extern _EQ__EQ_var_u16_LC
_EQ__EQ_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_LC
	.private_extern _EQ__EQ_param_u16_LC
_EQ__EQ_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_i16
	.private_extern _EQ__EQ_var_u16_i16
_EQ__EQ_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %edx
	movzwl	136+_MM_EQ(%rip), %eax
	cmpw	%ax, %dx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_i16
	.private_extern _EQ__EQ_param_u16_i16
_EQ__EQ_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	cmpw	-18(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_i32
	.private_extern _EQ__EQ_var_u16_i32
_EQ__EQ_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_i32
	.private_extern _EQ__EQ_param_u16_i32
_EQ__EQ_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_I
	.private_extern _EQ__EQ_var_u16_I
_EQ__EQ_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_I
	.private_extern _EQ__EQ_param_u16_I
_EQ__EQ_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_i64
	.private_extern _EQ__EQ_var_u16_i64
_EQ__EQ_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_i64
	.private_extern _EQ__EQ_param_u16_i64
_EQ__EQ_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_C
	.private_extern _EQ__EQ_var_u16_C
_EQ__EQ_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_C
	.private_extern _EQ__EQ_param_u16_C
_EQ__EQ_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_u8
	.private_extern _EQ__EQ_var_u16_u8
_EQ__EQ_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %edx
	movzbl	172+_MM_EQ(%rip), %eax
	cbtw
	cmpw	%ax, %dx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_u8
	.private_extern _EQ__EQ_param_u16_u8
_EQ__EQ_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movsbw	-19(%rbp),%ax
	cmpw	-18(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_i8
	.private_extern _EQ__EQ_var_u16_i8
_EQ__EQ_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %edx
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %eax
	cmpw	%ax, %dx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_i8
	.private_extern _EQ__EQ_param_u16_i8
_EQ__EQ_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzbl	-19(%rbp), %eax
	cmpw	-18(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_L
	.private_extern _EQ__EQ_var_u16_L
_EQ__EQ_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_L
	.private_extern _EQ__EQ_param_u16_L
_EQ__EQ_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u16_u32
	.private_extern _EQ__EQ_var_u16_u32
_EQ__EQ_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_EQ(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u16_u32
	.private_extern _EQ__EQ_param_u16_u32
_EQ__EQ_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_u16
	.private_extern _EQ__EQ_var_u64_u16
_EQ__EQ_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_u16
	.private_extern _EQ__EQ_param_u64_u16
_EQ__EQ_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_u64
	.private_extern _EQ__EQ_var_u64_u64
_EQ__EQ_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_u64
	.private_extern _EQ__EQ_param_u64_u64
_EQ__EQ_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_LC
	.private_extern _EQ__EQ_var_u64_LC
_EQ__EQ_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_EQ(%rip), %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_LC
	.private_extern _EQ__EQ_param_u64_LC
_EQ__EQ_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_i16
	.private_extern _EQ__EQ_var_u64_i16
_EQ__EQ_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_i16
	.private_extern _EQ__EQ_param_u64_i16
_EQ__EQ_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_i32
	.private_extern _EQ__EQ_var_u64_i32
_EQ__EQ_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_i32
	.private_extern _EQ__EQ_param_u64_i32
_EQ__EQ_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_I
	.private_extern _EQ__EQ_var_u64_I
_EQ__EQ_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_I
	.private_extern _EQ__EQ_param_u64_I
_EQ__EQ_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_i64
	.private_extern _EQ__EQ_var_u64_i64
_EQ__EQ_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_EQ(%rip), %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_i64
	.private_extern _EQ__EQ_param_u64_i64
_EQ__EQ_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_C
	.private_extern _EQ__EQ_var_u64_C
_EQ__EQ_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_C
	.private_extern _EQ__EQ_param_u64_C
_EQ__EQ_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_u8
	.private_extern _EQ__EQ_var_u64_u8
_EQ__EQ_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_u8
	.private_extern _EQ__EQ_param_u64_u8
_EQ__EQ_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_i8
	.private_extern _EQ__EQ_var_u64_i8
_EQ__EQ_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_i8
	.private_extern _EQ__EQ_param_u64_i8
_EQ__EQ_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_L
	.private_extern _EQ__EQ_var_u64_L
_EQ__EQ_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_EQ(%rip), %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_L
	.private_extern _EQ__EQ_param_u64_L
_EQ__EQ_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u64_u32
	.private_extern _EQ__EQ_var_u64_u32
_EQ__EQ_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u64_u32
	.private_extern _EQ__EQ_param_u64_u32
_EQ__EQ_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_u16
	.private_extern _EQ__EQ_var_LC_u16
_EQ__EQ_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_u16
	.private_extern _EQ__EQ_param_LC_u16
_EQ__EQ_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_u64
	.private_extern _EQ__EQ_var_LC_u64
_EQ__EQ_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_u64
	.private_extern _EQ__EQ_param_LC_u64
_EQ__EQ_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_LC
	.private_extern _EQ__EQ_var_LC_LC
_EQ__EQ_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_LC
	.private_extern _EQ__EQ_param_LC_LC
_EQ__EQ_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_i16
	.private_extern _EQ__EQ_var_LC_i16
_EQ__EQ_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_i16
	.private_extern _EQ__EQ_param_LC_i16
_EQ__EQ_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_i32
	.private_extern _EQ__EQ_var_LC_i32
_EQ__EQ_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_i32
	.private_extern _EQ__EQ_param_LC_i32
_EQ__EQ_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_I
	.private_extern _EQ__EQ_var_LC_I
_EQ__EQ_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_I
	.private_extern _EQ__EQ_param_LC_I
_EQ__EQ_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_i64
	.private_extern _EQ__EQ_var_LC_i64
_EQ__EQ_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_EQ(%rip), %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_i64
	.private_extern _EQ__EQ_param_LC_i64
_EQ__EQ_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_C
	.private_extern _EQ__EQ_var_LC_C
_EQ__EQ_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_C
	.private_extern _EQ__EQ_param_LC_C
_EQ__EQ_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_u8
	.private_extern _EQ__EQ_var_LC_u8
_EQ__EQ_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_u8
	.private_extern _EQ__EQ_param_LC_u8
_EQ__EQ_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_i8
	.private_extern _EQ__EQ_var_LC_i8
_EQ__EQ_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_i8
	.private_extern _EQ__EQ_param_LC_i8
_EQ__EQ_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_L
	.private_extern _EQ__EQ_var_LC_L
_EQ__EQ_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_EQ(%rip), %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_L
	.private_extern _EQ__EQ_param_LC_L
_EQ__EQ_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_LC_u32
	.private_extern _EQ__EQ_var_LC_u32
_EQ__EQ_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_LC_u32
	.private_extern _EQ__EQ_param_LC_u32
_EQ__EQ_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_f64_f64
	.private_extern _EQ__EQ_var_f64_f64
_EQ__EQ_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_EQ(%rip), %xmm1
	movsd	128+_MM_EQ(%rip), %xmm0
	ucomisd	%xmm0, %xmm1
	sete	%al
	setnp	%dl
	andl	%edx, %eax
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_f64_f64
	.private_extern _EQ__EQ_param_f64_f64
_EQ__EQ_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm0
	ucomisd	-32(%rbp), %xmm0
	sete	%al
	setnp	%dl
	andl	%edx, %eax
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_u16
	.private_extern _EQ__EQ_var_i16_u16
_EQ__EQ_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %edx
	movzwl	104+_MM_EQ(%rip), %eax
	cmpw	%ax, %dx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_u16
	.private_extern _EQ__EQ_param_i16_u16
_EQ__EQ_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	cmpw	-18(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_u64
	.private_extern _EQ__EQ_var_i16_u64
_EQ__EQ_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_u64
	.private_extern _EQ__EQ_param_i16_u64
_EQ__EQ_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_LC
	.private_extern _EQ__EQ_var_i16_LC
_EQ__EQ_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_LC
	.private_extern _EQ__EQ_param_i16_LC
_EQ__EQ_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_i16
	.private_extern _EQ__EQ_var_i16_i16
_EQ__EQ_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_i16
	.private_extern _EQ__EQ_param_i16_i16
_EQ__EQ_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	cmpw	-20(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_i32
	.private_extern _EQ__EQ_var_i16_i32
_EQ__EQ_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_i32
	.private_extern _EQ__EQ_param_i16_i32
_EQ__EQ_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_I
	.private_extern _EQ__EQ_var_i16_I
_EQ__EQ_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_I
	.private_extern _EQ__EQ_param_i16_I
_EQ__EQ_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_i64
	.private_extern _EQ__EQ_var_i16_i64
_EQ__EQ_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_i64
	.private_extern _EQ__EQ_param_i16_i64
_EQ__EQ_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_C
	.private_extern _EQ__EQ_var_i16_C
_EQ__EQ_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_C
	.private_extern _EQ__EQ_param_i16_C
_EQ__EQ_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_u8
	.private_extern _EQ__EQ_var_i16_u8
_EQ__EQ_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %edx
	movzbl	172+_MM_EQ(%rip), %eax
	cbtw
	cmpw	%ax, %dx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_u8
	.private_extern _EQ__EQ_param_i16_u8
_EQ__EQ_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movsbw	-19(%rbp),%ax
	cmpw	-18(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_i8
	.private_extern _EQ__EQ_var_i16_i8
_EQ__EQ_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %edx
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %eax
	cmpw	%ax, %dx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_i8
	.private_extern _EQ__EQ_param_i16_i8
_EQ__EQ_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzbl	-19(%rbp), %eax
	cmpw	-18(%rbp), %ax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_L
	.private_extern _EQ__EQ_var_i16_L
_EQ__EQ_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_L
	.private_extern _EQ__EQ_param_i16_L
_EQ__EQ_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i16_u32
	.private_extern _EQ__EQ_var_i16_u32
_EQ__EQ_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_EQ(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i16_u32
	.private_extern _EQ__EQ_param_i16_u32
_EQ__EQ_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_u16
	.private_extern _EQ__EQ_var_i32_u16
_EQ__EQ_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzwl	104+_MM_EQ(%rip), %eax
	cwtl
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_u16
	.private_extern _EQ__EQ_param_i32_u16
_EQ__EQ_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movswl	-22(%rbp),%eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_u64
	.private_extern _EQ__EQ_var_i32_u64
_EQ__EQ_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_u64
	.private_extern _EQ__EQ_param_i32_u64
_EQ__EQ_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_LC
	.private_extern _EQ__EQ_var_i32_LC
_EQ__EQ_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_LC
	.private_extern _EQ__EQ_param_i32_LC
_EQ__EQ_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_i16
	.private_extern _EQ__EQ_var_i32_i16
_EQ__EQ_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_i16
	.private_extern _EQ__EQ_param_i32_i16
_EQ__EQ_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movzwl	-22(%rbp), %eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_i32
	.private_extern _EQ__EQ_var_i32_i32
_EQ__EQ_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_i32
	.private_extern _EQ__EQ_param_i32_i32
_EQ__EQ_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cmpl	-24(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_I
	.private_extern _EQ__EQ_var_i32_I
_EQ__EQ_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_I
	.private_extern _EQ__EQ_param_i32_I
_EQ__EQ_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_i64
	.private_extern _EQ__EQ_var_i32_i64
_EQ__EQ_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_i64
	.private_extern _EQ__EQ_param_i32_i64
_EQ__EQ_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_C
	.private_extern _EQ__EQ_var_i32_C
_EQ__EQ_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_C
	.private_extern _EQ__EQ_param_i32_C
_EQ__EQ_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_u8
	.private_extern _EQ__EQ_var_i32_u8
_EQ__EQ_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzbl	172+_MM_EQ(%rip), %eax
	movsbl	%al,%eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_u8
	.private_extern _EQ__EQ_param_i32_u8
_EQ__EQ_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movsbl	-21(%rbp),%eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_i8
	.private_extern _EQ__EQ_var_i32_i8
_EQ__EQ_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_i8
	.private_extern _EQ__EQ_param_i32_i8
_EQ__EQ_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movzbl	-21(%rbp), %eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_L
	.private_extern _EQ__EQ_var_i32_L
_EQ__EQ_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_L
	.private_extern _EQ__EQ_param_i32_L
_EQ__EQ_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i32_u32
	.private_extern _EQ__EQ_var_i32_u32
_EQ__EQ_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movl	184+_MM_EQ(%rip), %eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i32_u32
	.private_extern _EQ__EQ_param_i32_u32
_EQ__EQ_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_u16
	.private_extern _EQ__EQ_var_I_u16
_EQ__EQ_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_u16
	.private_extern _EQ__EQ_param_I_u16
_EQ__EQ_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_u64
	.private_extern _EQ__EQ_var_I_u64
_EQ__EQ_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_u64
	.private_extern _EQ__EQ_param_I_u64
_EQ__EQ_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_LC
	.private_extern _EQ__EQ_var_I_LC
_EQ__EQ_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_LC
	.private_extern _EQ__EQ_param_I_LC
_EQ__EQ_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_i16
	.private_extern _EQ__EQ_var_I_i16
_EQ__EQ_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_i16
	.private_extern _EQ__EQ_param_I_i16
_EQ__EQ_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_i32
	.private_extern _EQ__EQ_var_I_i32
_EQ__EQ_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_i32
	.private_extern _EQ__EQ_param_I_i32
_EQ__EQ_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_I
	.private_extern _EQ__EQ_var_I_I
_EQ__EQ_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_I_I
	.private_extern _EQ__EQ_param_I_I
_EQ__EQ_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_i64
	.private_extern _EQ__EQ_var_I_i64
_EQ__EQ_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_i64
	.private_extern _EQ__EQ_param_I_i64
_EQ__EQ_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_C
	.private_extern _EQ__EQ_var_I_C
_EQ__EQ_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_C
	.private_extern _EQ__EQ_param_I_C
_EQ__EQ_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_u8
	.private_extern _EQ__EQ_var_I_u8
_EQ__EQ_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_u8
	.private_extern _EQ__EQ_param_I_u8
_EQ__EQ_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_i8
	.private_extern _EQ__EQ_var_I_i8
_EQ__EQ_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_i8
	.private_extern _EQ__EQ_param_I_i8
_EQ__EQ_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_L
	.private_extern _EQ__EQ_var_I_L
_EQ__EQ_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_L
	.private_extern _EQ__EQ_param_I_L
_EQ__EQ_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_I_u32
	.private_extern _EQ__EQ_var_I_u32
_EQ__EQ_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movl	184+_MM_EQ(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_I_u32
	.private_extern _EQ__EQ_param_I_u32
_EQ__EQ_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_u16
	.private_extern _EQ__EQ_var_i64_u16
_EQ__EQ_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_u16
	.private_extern _EQ__EQ_param_i64_u16
_EQ__EQ_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_u64
	.private_extern _EQ__EQ_var_i64_u64
_EQ__EQ_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_u64
	.private_extern _EQ__EQ_param_i64_u64
_EQ__EQ_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_LC
	.private_extern _EQ__EQ_var_i64_LC
_EQ__EQ_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_EQ(%rip), %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_LC
	.private_extern _EQ__EQ_param_i64_LC
_EQ__EQ_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_i16
	.private_extern _EQ__EQ_var_i64_i16
_EQ__EQ_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_i16
	.private_extern _EQ__EQ_param_i64_i16
_EQ__EQ_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_i32
	.private_extern _EQ__EQ_var_i64_i32
_EQ__EQ_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_i32
	.private_extern _EQ__EQ_param_i64_i32
_EQ__EQ_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_I
	.private_extern _EQ__EQ_var_i64_I
_EQ__EQ_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_I
	.private_extern _EQ__EQ_param_i64_I
_EQ__EQ_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_i64
	.private_extern _EQ__EQ_var_i64_i64
_EQ__EQ_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_i64
	.private_extern _EQ__EQ_param_i64_i64
_EQ__EQ_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_C
	.private_extern _EQ__EQ_var_i64_C
_EQ__EQ_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_C
	.private_extern _EQ__EQ_param_i64_C
_EQ__EQ_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_u8
	.private_extern _EQ__EQ_var_i64_u8
_EQ__EQ_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_u8
	.private_extern _EQ__EQ_param_i64_u8
_EQ__EQ_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_i8
	.private_extern _EQ__EQ_var_i64_i8
_EQ__EQ_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_i8
	.private_extern _EQ__EQ_param_i64_i8
_EQ__EQ_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_L
	.private_extern _EQ__EQ_var_i64_L
_EQ__EQ_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_EQ(%rip), %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_L
	.private_extern _EQ__EQ_param_i64_L
_EQ__EQ_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i64_u32
	.private_extern _EQ__EQ_var_i64_u32
_EQ__EQ_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i64_u32
	.private_extern _EQ__EQ_param_i64_u32
_EQ__EQ_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_u16
	.private_extern _EQ__EQ_var_C_u16
_EQ__EQ_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_u16
	.private_extern _EQ__EQ_param_C_u16
_EQ__EQ_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_u64
	.private_extern _EQ__EQ_var_C_u64
_EQ__EQ_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_u64
	.private_extern _EQ__EQ_param_C_u64
_EQ__EQ_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_LC
	.private_extern _EQ__EQ_var_C_LC
_EQ__EQ_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_LC
	.private_extern _EQ__EQ_param_C_LC
_EQ__EQ_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_i16
	.private_extern _EQ__EQ_var_C_i16
_EQ__EQ_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_i16
	.private_extern _EQ__EQ_param_C_i16
_EQ__EQ_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_i32
	.private_extern _EQ__EQ_var_C_i32
_EQ__EQ_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_i32
	.private_extern _EQ__EQ_param_C_i32
_EQ__EQ_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_I
	.private_extern _EQ__EQ_var_C_I
_EQ__EQ_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_I
	.private_extern _EQ__EQ_param_C_I
_EQ__EQ_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_i64
	.private_extern _EQ__EQ_var_C_i64
_EQ__EQ_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_i64
	.private_extern _EQ__EQ_param_C_i64
_EQ__EQ_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_C
	.private_extern _EQ__EQ_var_C_C
_EQ__EQ_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_C_C
	.private_extern _EQ__EQ_param_C_C
_EQ__EQ_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_u8
	.private_extern _EQ__EQ_var_C_u8
_EQ__EQ_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_u8
	.private_extern _EQ__EQ_param_C_u8
_EQ__EQ_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_i8
	.private_extern _EQ__EQ_var_C_i8
_EQ__EQ_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_i8
	.private_extern _EQ__EQ_param_C_i8
_EQ__EQ_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_L
	.private_extern _EQ__EQ_var_C_L
_EQ__EQ_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_L
	.private_extern _EQ__EQ_param_C_L
_EQ__EQ_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_C_u32
	.private_extern _EQ__EQ_var_C_u32
_EQ__EQ_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movl	184+_MM_EQ(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_C_u32
	.private_extern _EQ__EQ_param_C_u32
_EQ__EQ_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_f32_f32
	.private_extern _EQ__EQ_var_f32_f32
_EQ__EQ_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_EQ(%rip), %xmm1
	movss	168+_MM_EQ(%rip), %xmm0
	ucomiss	%xmm0, %xmm1
	sete	%al
	setnp	%dl
	andl	%edx, %eax
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_f32_f32
	.private_extern _EQ__EQ_param_f32_f32
_EQ__EQ_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm0
	ucomiss	-24(%rbp), %xmm0
	sete	%al
	setnp	%dl
	andl	%edx, %eax
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_u16
	.private_extern _EQ__EQ_var_u8_u16
_EQ__EQ_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_u16
	.private_extern _EQ__EQ_param_u8_u16
_EQ__EQ_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_u64
	.private_extern _EQ__EQ_var_u8_u64
_EQ__EQ_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_u64
	.private_extern _EQ__EQ_param_u8_u64
_EQ__EQ_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_LC
	.private_extern _EQ__EQ_var_u8_LC
_EQ__EQ_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_LC
	.private_extern _EQ__EQ_param_u8_LC
_EQ__EQ_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_i16
	.private_extern _EQ__EQ_var_u8_i16
_EQ__EQ_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_i16
	.private_extern _EQ__EQ_param_u8_i16
_EQ__EQ_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_i32
	.private_extern _EQ__EQ_var_u8_i32
_EQ__EQ_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_i32
	.private_extern _EQ__EQ_param_u8_i32
_EQ__EQ_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_I
	.private_extern _EQ__EQ_var_u8_I
_EQ__EQ_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_I
	.private_extern _EQ__EQ_param_u8_I
_EQ__EQ_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_i64
	.private_extern _EQ__EQ_var_u8_i64
_EQ__EQ_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_i64
	.private_extern _EQ__EQ_param_u8_i64
_EQ__EQ_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_C
	.private_extern _EQ__EQ_var_u8_C
_EQ__EQ_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_C
	.private_extern _EQ__EQ_param_u8_C
_EQ__EQ_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_u8
	.private_extern _EQ__EQ_var_u8_u8
_EQ__EQ_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_u8
	.private_extern _EQ__EQ_param_u8_u8
_EQ__EQ_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	cmpb	-18(%rbp), %al
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_i8
	.private_extern _EQ__EQ_var_u8_i8
_EQ__EQ_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %edx
	movzbl	173+_MM_EQ(%rip), %eax
	cmpb	%al, %dl
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_i8
	.private_extern _EQ__EQ_param_u8_i8
_EQ__EQ_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	cmpb	-17(%rbp), %al
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_L
	.private_extern _EQ__EQ_var_u8_L
_EQ__EQ_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_L
	.private_extern _EQ__EQ_param_u8_L
_EQ__EQ_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u8_u32
	.private_extern _EQ__EQ_var_u8_u32
_EQ__EQ_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_EQ(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u8_u32
	.private_extern _EQ__EQ_param_u8_u32
_EQ__EQ_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_u16
	.private_extern _EQ__EQ_var_i8_u16
_EQ__EQ_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_u16
	.private_extern _EQ__EQ_param_i8_u16
_EQ__EQ_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_u64
	.private_extern _EQ__EQ_var_i8_u64
_EQ__EQ_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_u64
	.private_extern _EQ__EQ_param_i8_u64
_EQ__EQ_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_LC
	.private_extern _EQ__EQ_var_i8_LC
_EQ__EQ_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_LC
	.private_extern _EQ__EQ_param_i8_LC
_EQ__EQ_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_i16
	.private_extern _EQ__EQ_var_i8_i16
_EQ__EQ_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_i16
	.private_extern _EQ__EQ_param_i8_i16
_EQ__EQ_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_i32
	.private_extern _EQ__EQ_var_i8_i32
_EQ__EQ_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_i32
	.private_extern _EQ__EQ_param_i8_i32
_EQ__EQ_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_I
	.private_extern _EQ__EQ_var_i8_I
_EQ__EQ_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_I
	.private_extern _EQ__EQ_param_i8_I
_EQ__EQ_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_i64
	.private_extern _EQ__EQ_var_i8_i64
_EQ__EQ_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_i64
	.private_extern _EQ__EQ_param_i8_i64
_EQ__EQ_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_C
	.private_extern _EQ__EQ_var_i8_C
_EQ__EQ_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_C
	.private_extern _EQ__EQ_param_i8_C
_EQ__EQ_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_u8
	.private_extern _EQ__EQ_var_i8_u8
_EQ__EQ_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %edx
	movzbl	172+_MM_EQ(%rip), %eax
	cmpb	%al, %dl
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_u8
	.private_extern _EQ__EQ_param_i8_u8
_EQ__EQ_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	cmpb	-17(%rbp), %al
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_i8
	.private_extern _EQ__EQ_var_i8_i8
_EQ__EQ_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_i8
	.private_extern _EQ__EQ_param_i8_i8
_EQ__EQ_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	cmpb	-18(%rbp), %al
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_L
	.private_extern _EQ__EQ_var_i8_L
_EQ__EQ_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_L
	.private_extern _EQ__EQ_param_i8_L
_EQ__EQ_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_i8_u32
	.private_extern _EQ__EQ_var_i8_u32
_EQ__EQ_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_EQ(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_i8_u32
	.private_extern _EQ__EQ_param_i8_u32
_EQ__EQ_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_u16
	.private_extern _EQ__EQ_var_L_u16
_EQ__EQ_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_u16
	.private_extern _EQ__EQ_param_L_u16
_EQ__EQ_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_u64
	.private_extern _EQ__EQ_var_L_u64
_EQ__EQ_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_u64
	.private_extern _EQ__EQ_param_L_u64
_EQ__EQ_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_LC
	.private_extern _EQ__EQ_var_L_LC
_EQ__EQ_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_EQ(%rip), %rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_LC
	.private_extern _EQ__EQ_param_L_LC
_EQ__EQ_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_i16
	.private_extern _EQ__EQ_var_L_i16
_EQ__EQ_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_i16
	.private_extern _EQ__EQ_param_L_i16
_EQ__EQ_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_i32
	.private_extern _EQ__EQ_var_L_i32
_EQ__EQ_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_i32
	.private_extern _EQ__EQ_param_L_i32
_EQ__EQ_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_I
	.private_extern _EQ__EQ_var_L_I
_EQ__EQ_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_I
	.private_extern _EQ__EQ_param_L_I
_EQ__EQ_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_i64
	.private_extern _EQ__EQ_var_L_i64
_EQ__EQ_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_EQ(%rip), %rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_i64
	.private_extern _EQ__EQ_param_L_i64
_EQ__EQ_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_C
	.private_extern _EQ__EQ_var_L_C
_EQ__EQ_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_C
	.private_extern _EQ__EQ_param_L_C
_EQ__EQ_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_u8
	.private_extern _EQ__EQ_var_L_u8
_EQ__EQ_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_u8
	.private_extern _EQ__EQ_param_L_u8
_EQ__EQ_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_i8
	.private_extern _EQ__EQ_var_L_i8
_EQ__EQ_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_i8
	.private_extern _EQ__EQ_param_L_i8
_EQ__EQ_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_L
	.private_extern _EQ__EQ_var_L_L
_EQ__EQ_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_L_L
	.private_extern _EQ__EQ_param_L_L
_EQ__EQ_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_L_u32
	.private_extern _EQ__EQ_var_L_u32
_EQ__EQ_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_L_u32
	.private_extern _EQ__EQ_param_L_u32
_EQ__EQ_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_u16
	.private_extern _EQ__EQ_var_u32_u16
_EQ__EQ_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %edx
	movzwl	104+_MM_EQ(%rip), %eax
	cwtl
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_u16
	.private_extern _EQ__EQ_param_u32_u16
_EQ__EQ_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movswl	-22(%rbp),%eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_u64
	.private_extern _EQ__EQ_var_u32_u64
_EQ__EQ_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_u64
	.private_extern _EQ__EQ_param_u32_u64
_EQ__EQ_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_LC
	.private_extern _EQ__EQ_var_u32_LC
_EQ__EQ_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_LC
	.private_extern _EQ__EQ_param_u32_LC
_EQ__EQ_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_i16
	.private_extern _EQ__EQ_var_u32_i16
_EQ__EQ_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %edx
	movzwl	136+_MM_EQ(%rip), %eax
	movzwl	%ax, %eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_i16
	.private_extern _EQ__EQ_param_u32_i16
_EQ__EQ_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movzwl	-22(%rbp), %eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_i32
	.private_extern _EQ__EQ_var_u32_i32
_EQ__EQ_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %edx
	movl	140+_MM_EQ(%rip), %eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_i32
	.private_extern _EQ__EQ_param_u32_i32
_EQ__EQ_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_I
	.private_extern _EQ__EQ_var_u32_I
_EQ__EQ_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_I
	.private_extern _EQ__EQ_param_u32_I
_EQ__EQ_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_i64
	.private_extern _EQ__EQ_var_u32_i64
_EQ__EQ_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_i64
	.private_extern _EQ__EQ_param_u32_i64
_EQ__EQ_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_C
	.private_extern _EQ__EQ_var_u32_C
_EQ__EQ_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_C
	.private_extern _EQ__EQ_param_u32_C
_EQ__EQ_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_u8
	.private_extern _EQ__EQ_var_u32_u8
_EQ__EQ_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %edx
	movzbl	172+_MM_EQ(%rip), %eax
	movsbl	%al,%eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_u8
	.private_extern _EQ__EQ_param_u32_u8
_EQ__EQ_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movsbl	-21(%rbp),%eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_i8
	.private_extern _EQ__EQ_var_u32_i8
_EQ__EQ_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %edx
	movzbl	173+_MM_EQ(%rip), %eax
	movzbl	%al, %eax
	cmpl	%eax, %edx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_i8
	.private_extern _EQ__EQ_param_u32_i8
_EQ__EQ_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movzbl	-21(%rbp), %eax
	cmpl	-20(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_L
	.private_extern _EQ__EQ_var_u32_L
_EQ__EQ_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_EQ(%rip), %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_L
	.private_extern _EQ__EQ_param_u32_L
_EQ__EQ_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ__EQ_var_u32_u32
	.private_extern _EQ__EQ_var_u32_u32
_EQ__EQ_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _EQ__EQ_param_u32_u32
	.private_extern _EQ__EQ_param_u32_u32
_EQ__EQ_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cmpl	-24(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	leave
	ret
.globl _EQ_M3
_EQ_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_EQ(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "EQ_M3"
	.space 1
	.ascii "EQ_param_u32_u32"
	.space 1
	.ascii "EQ_var_u32_u32"
	.space 1
	.ascii "EQ_param_u32_L"
	.space 1
	.ascii "EQ_var_u32_L"
	.space 1
	.ascii "EQ_param_u32_i8"
	.space 1
	.ascii "EQ_var_u32_i8"
	.space 1
	.ascii "EQ_param_u32_u8"
	.space 1
	.ascii "EQ_var_u32_u8"
	.space 1
	.ascii "EQ_param_u32_C"
	.space 1
	.ascii "EQ_var_u32_C"
	.space 1
	.ascii "EQ_param_u32_i64"
	.space 1
	.ascii "EQ_var_u32_i64"
	.space 1
	.ascii "EQ_param_u32_I"
	.space 1
	.ascii "EQ_var_u32_I"
	.space 1
	.ascii "EQ_param_u32_i32"
	.space 1
	.ascii "EQ_var_u32_i32"
	.space 1
	.ascii "EQ_param_u32_i16"
	.space 1
	.ascii "EQ_var_u32_i16"
	.space 1
	.ascii "EQ_param_u32_LC"
	.space 1
	.ascii "EQ_var_u32_LC"
	.space 1
	.ascii "EQ_param_u32_u64"
	.space 1
	.ascii "EQ_var_u32_u64"
	.space 1
	.ascii "EQ_param_u32_u16"
	.space 1
	.ascii "EQ_var_u32_u16"
	.space 1
	.ascii "EQ_param_L_u32"
	.space 1
	.ascii "EQ_var_L_u32"
	.space 1
	.ascii "EQ_param_L_L"
	.space 1
	.ascii "EQ_var_L_L"
	.space 1
	.ascii "EQ_param_L_i8"
	.space 1
	.ascii "EQ_var_L_i8"
	.space 1
	.ascii "EQ_param_L_u8"
	.space 1
	.ascii "EQ_var_L_u8"
	.space 1
	.ascii "EQ_param_L_C"
	.space 1
	.ascii "EQ_var_L_C"
	.space 1
	.ascii "EQ_param_L_i64"
	.space 1
	.ascii "EQ_var_L_i64"
	.space 1
	.ascii "EQ_param_L_I"
	.space 1
	.ascii "EQ_var_L_I"
	.space 1
	.ascii "EQ_param_L_i32"
	.space 1
	.ascii "EQ_var_L_i32"
	.space 1
	.ascii "EQ_param_L_i16"
	.space 1
	.ascii "EQ_var_L_i16"
	.space 1
	.ascii "EQ_param_L_LC"
	.space 1
	.ascii "EQ_var_L_LC"
	.space 1
	.ascii "EQ_param_L_u64"
	.space 1
	.ascii "EQ_var_L_u64"
	.space 1
	.ascii "EQ_param_L_u16"
	.space 1
	.ascii "EQ_var_L_u16"
	.space 1
	.ascii "EQ_param_i8_u32"
	.space 1
	.ascii "EQ_var_i8_u32"
	.space 1
	.ascii "EQ_param_i8_L"
	.space 1
	.ascii "EQ_var_i8_L"
	.space 1
	.ascii "EQ_param_i8_i8"
	.space 1
	.ascii "EQ_var_i8_i8"
	.space 1
	.ascii "EQ_param_i8_u8"
	.space 1
	.ascii "EQ_var_i8_u8"
	.space 1
	.ascii "EQ_param_i8_C"
	.space 1
	.ascii "EQ_var_i8_C"
	.space 1
	.ascii "EQ_param_i8_i64"
	.space 1
	.ascii "EQ_var_i8_i64"
	.space 1
	.ascii "EQ_param_i8_I"
	.space 1
	.ascii "EQ_var_i8_I"
	.space 1
	.ascii "EQ_param_i8_i32"
	.space 1
	.ascii "EQ_var_i8_i32"
	.space 1
	.ascii "EQ_param_i8_i16"
	.space 1
	.ascii "EQ_var_i8_i16"
	.space 1
	.ascii "EQ_param_i8_LC"
	.space 1
	.ascii "EQ_var_i8_LC"
	.space 1
	.ascii "EQ_param_i8_u64"
	.space 1
	.ascii "EQ_var_i8_u64"
	.space 1
	.ascii "EQ_param_i8_u16"
	.space 1
	.ascii "EQ_var_i8_u16"
	.space 1
	.ascii "EQ_param_u8_u32"
	.space 1
	.ascii "EQ_var_u8_u32"
	.space 1
	.ascii "EQ_param_u8_L"
	.space 1
	.ascii "EQ_var_u8_L"
	.space 1
	.ascii "EQ_param_u8_i8"
	.space 1
	.ascii "EQ_var_u8_i8"
	.space 1
	.ascii "EQ_param_u8_u8"
	.space 1
	.ascii "EQ_var_u8_u8"
	.space 1
	.ascii "EQ_param_u8_C"
	.space 1
	.ascii "EQ_var_u8_C"
	.space 1
	.ascii "EQ_param_u8_i64"
	.space 1
	.ascii "EQ_var_u8_i64"
	.space 1
	.ascii "EQ_param_u8_I"
	.space 1
	.ascii "EQ_var_u8_I"
	.space 1
	.ascii "EQ_param_u8_i32"
	.space 1
	.ascii "EQ_var_u8_i32"
	.space 1
	.ascii "EQ_param_u8_i16"
	.space 1
	.ascii "EQ_var_u8_i16"
	.space 1
	.ascii "EQ_param_u8_LC"
	.space 1
	.ascii "EQ_var_u8_LC"
	.space 1
	.ascii "EQ_param_u8_u64"
	.space 1
	.ascii "EQ_var_u8_u64"
	.space 1
	.ascii "EQ_param_u8_u16"
	.space 1
	.ascii "EQ_var_u8_u16"
	.space 1
	.ascii "EQ_param_f32_f32"
	.space 1
	.ascii "EQ_var_f32_f32"
	.space 1
	.ascii "EQ_param_C_u32"
	.space 1
	.ascii "EQ_var_C_u32"
	.space 1
	.ascii "EQ_param_C_L"
	.space 1
	.ascii "EQ_var_C_L"
	.space 1
	.ascii "EQ_param_C_i8"
	.space 1
	.ascii "EQ_var_C_i8"
	.space 1
	.ascii "EQ_param_C_u8"
	.space 1
	.ascii "EQ_var_C_u8"
	.space 1
	.ascii "EQ_param_C_C"
	.space 1
	.ascii "EQ_var_C_C"
	.space 1
	.ascii "EQ_param_C_i64"
	.space 1
	.ascii "EQ_var_C_i64"
	.space 1
	.ascii "EQ_param_C_I"
	.space 1
	.ascii "EQ_var_C_I"
	.space 1
	.ascii "EQ_param_C_i32"
	.space 1
	.ascii "EQ_var_C_i32"
	.space 1
	.ascii "EQ_param_C_i16"
	.space 1
	.ascii "EQ_var_C_i16"
	.space 1
	.ascii "EQ_param_C_LC"
	.space 1
	.ascii "EQ_var_C_LC"
	.space 1
	.ascii "EQ_param_C_u64"
	.space 1
	.ascii "EQ_var_C_u64"
	.space 1
	.ascii "EQ_param_C_u16"
	.space 1
	.ascii "EQ_var_C_u16"
	.space 1
	.ascii "EQ_param_i64_u32"
	.space 1
	.ascii "EQ_var_i64_u32"
	.space 1
	.ascii "EQ_param_i64_L"
	.space 1
	.ascii "EQ_var_i64_L"
	.space 1
	.ascii "EQ_param_i64_i8"
	.space 1
	.ascii "EQ_var_i64_i8"
	.space 1
	.ascii "EQ_param_i64_u8"
	.space 1
	.ascii "EQ_var_i64_u8"
	.space 1
	.ascii "EQ_param_i64_C"
	.space 1
	.ascii "EQ_var_i64_C"
	.space 1
	.ascii "EQ_param_i64_i64"
	.space 1
	.ascii "EQ_var_i64_i64"
	.space 1
	.ascii "EQ_param_i64_I"
	.space 1
	.ascii "EQ_var_i64_I"
	.space 1
	.ascii "EQ_param_i64_i32"
	.space 1
	.ascii "EQ_var_i64_i32"
	.space 1
	.ascii "EQ_param_i64_i16"
	.space 1
	.ascii "EQ_var_i64_i16"
	.space 1
	.ascii "EQ_param_i64_LC"
	.space 1
	.ascii "EQ_var_i64_LC"
	.space 1
	.ascii "EQ_param_i64_u64"
	.space 1
	.ascii "EQ_var_i64_u64"
	.space 1
	.ascii "EQ_param_i64_u16"
	.space 1
	.ascii "EQ_var_i64_u16"
	.space 1
	.ascii "EQ_param_I_u32"
	.space 1
	.ascii "EQ_var_I_u32"
	.space 1
	.ascii "EQ_param_I_L"
	.space 1
	.ascii "EQ_var_I_L"
	.space 1
	.ascii "EQ_param_I_i8"
	.space 1
	.ascii "EQ_var_I_i8"
	.space 1
	.ascii "EQ_param_I_u8"
	.space 1
	.ascii "EQ_var_I_u8"
	.space 1
	.ascii "EQ_param_I_C"
	.space 1
	.ascii "EQ_var_I_C"
	.space 1
	.ascii "EQ_param_I_i64"
	.space 1
	.ascii "EQ_var_I_i64"
	.space 1
	.ascii "EQ_param_I_I"
	.space 1
	.ascii "EQ_var_I_I"
	.space 1
	.ascii "EQ_param_I_i32"
	.space 1
	.ascii "EQ_var_I_i32"
	.space 1
	.ascii "EQ_param_I_i16"
	.space 1
	.ascii "EQ_var_I_i16"
	.space 1
	.ascii "EQ_param_I_LC"
	.space 1
	.ascii "EQ_var_I_LC"
	.space 1
	.ascii "EQ_param_I_u64"
	.space 1
	.ascii "EQ_var_I_u64"
	.space 1
	.ascii "EQ_param_I_u16"
	.space 1
	.ascii "EQ_var_I_u16"
	.space 1
	.ascii "EQ_param_i32_u32"
	.space 1
	.ascii "EQ_var_i32_u32"
	.space 1
	.ascii "EQ_param_i32_L"
	.space 1
	.ascii "EQ_var_i32_L"
	.space 1
	.ascii "EQ_param_i32_i8"
	.space 1
	.ascii "EQ_var_i32_i8"
	.space 1
	.ascii "EQ_param_i32_u8"
	.space 1
	.ascii "EQ_var_i32_u8"
	.space 1
	.ascii "EQ_param_i32_C"
	.space 1
	.ascii "EQ_var_i32_C"
	.space 1
	.ascii "EQ_param_i32_i64"
	.space 1
	.ascii "EQ_var_i32_i64"
	.space 1
	.ascii "EQ_param_i32_I"
	.space 1
	.ascii "EQ_var_i32_I"
	.space 1
	.ascii "EQ_param_i32_i32"
	.space 1
	.ascii "EQ_var_i32_i32"
	.space 1
	.ascii "EQ_param_i32_i16"
	.space 1
	.ascii "EQ_var_i32_i16"
	.space 1
	.ascii "EQ_param_i32_LC"
	.space 1
	.ascii "EQ_var_i32_LC"
	.space 1
	.ascii "EQ_param_i32_u64"
	.space 1
	.ascii "EQ_var_i32_u64"
	.space 1
	.ascii "EQ_param_i32_u16"
	.space 1
	.ascii "EQ_var_i32_u16"
	.space 1
	.ascii "EQ_param_i16_u32"
	.space 1
	.ascii "EQ_var_i16_u32"
	.space 1
	.ascii "EQ_param_i16_L"
	.space 1
	.ascii "EQ_var_i16_L"
	.space 1
	.ascii "EQ_param_i16_i8"
	.space 1
	.ascii "EQ_var_i16_i8"
	.space 1
	.ascii "EQ_param_i16_u8"
	.space 1
	.ascii "EQ_var_i16_u8"
	.space 1
	.ascii "EQ_param_i16_C"
	.space 1
	.ascii "EQ_var_i16_C"
	.space 1
	.ascii "EQ_param_i16_i64"
	.space 1
	.ascii "EQ_var_i16_i64"
	.space 1
	.ascii "EQ_param_i16_I"
	.space 1
	.ascii "EQ_var_i16_I"
	.space 1
	.ascii "EQ_param_i16_i32"
	.space 1
	.ascii "EQ_var_i16_i32"
	.space 1
	.ascii "EQ_param_i16_i16"
	.space 1
	.ascii "EQ_var_i16_i16"
	.space 1
	.ascii "EQ_param_i16_LC"
	.space 1
	.ascii "EQ_var_i16_LC"
	.space 1
	.ascii "EQ_param_i16_u64"
	.space 1
	.ascii "EQ_var_i16_u64"
	.space 1
	.ascii "EQ_param_i16_u16"
	.space 1
	.ascii "EQ_var_i16_u16"
	.space 1
	.ascii "EQ_param_f64_f64"
	.space 1
	.ascii "EQ_var_f64_f64"
	.space 1
	.ascii "EQ_param_LC_u32"
	.space 1
	.ascii "EQ_var_LC_u32"
	.space 1
	.ascii "EQ_param_LC_L"
	.space 1
	.ascii "EQ_var_LC_L"
	.space 1
	.ascii "EQ_param_LC_i8"
	.space 1
	.ascii "EQ_var_LC_i8"
	.space 1
	.ascii "EQ_param_LC_u8"
	.space 1
	.ascii "EQ_var_LC_u8"
	.space 1
	.ascii "EQ_param_LC_C"
	.space 1
	.ascii "EQ_var_LC_C"
	.space 1
	.ascii "EQ_param_LC_i64"
	.space 1
	.ascii "EQ_var_LC_i64"
	.space 1
	.ascii "EQ_param_LC_I"
	.space 1
	.ascii "EQ_var_LC_I"
	.space 1
	.ascii "EQ_param_LC_i32"
	.space 1
	.ascii "EQ_var_LC_i32"
	.space 1
	.ascii "EQ_param_LC_i16"
	.space 1
	.ascii "EQ_var_LC_i16"
	.space 1
	.ascii "EQ_param_LC_LC"
	.space 1
	.ascii "EQ_var_LC_LC"
	.space 1
	.ascii "EQ_param_LC_u64"
	.space 1
	.ascii "EQ_var_LC_u64"
	.space 1
	.ascii "EQ_param_LC_u16"
	.space 1
	.ascii "EQ_var_LC_u16"
	.space 1
	.ascii "EQ_param_u64_u32"
	.space 1
	.ascii "EQ_var_u64_u32"
	.space 1
	.ascii "EQ_param_u64_L"
	.space 1
	.ascii "EQ_var_u64_L"
	.space 1
	.ascii "EQ_param_u64_i8"
	.space 1
	.ascii "EQ_var_u64_i8"
	.space 1
	.ascii "EQ_param_u64_u8"
	.space 1
	.ascii "EQ_var_u64_u8"
	.space 1
	.ascii "EQ_param_u64_C"
	.space 1
	.ascii "EQ_var_u64_C"
	.space 1
	.ascii "EQ_param_u64_i64"
	.space 1
	.ascii "EQ_var_u64_i64"
	.space 1
	.ascii "EQ_param_u64_I"
	.space 1
	.ascii "EQ_var_u64_I"
	.space 1
	.ascii "EQ_param_u64_i32"
	.space 1
	.ascii "EQ_var_u64_i32"
	.space 1
	.ascii "EQ_param_u64_i16"
	.space 1
	.ascii "EQ_var_u64_i16"
	.space 1
	.ascii "EQ_param_u64_LC"
	.space 1
	.ascii "EQ_var_u64_LC"
	.space 1
	.ascii "EQ_param_u64_u64"
	.space 1
	.ascii "EQ_var_u64_u64"
	.space 1
	.ascii "EQ_param_u64_u16"
	.space 1
	.ascii "EQ_var_u64_u16"
	.space 1
	.ascii "EQ_param_u16_u32"
	.space 1
	.ascii "EQ_var_u16_u32"
	.space 1
	.ascii "EQ_param_u16_L"
	.space 1
	.ascii "EQ_var_u16_L"
	.space 1
	.ascii "EQ_param_u16_i8"
	.space 1
	.ascii "EQ_var_u16_i8"
	.space 1
	.ascii "EQ_param_u16_u8"
	.space 1
	.ascii "EQ_var_u16_u8"
	.space 1
	.ascii "EQ_param_u16_C"
	.space 1
	.ascii "EQ_var_u16_C"
	.space 1
	.ascii "EQ_param_u16_i64"
	.space 1
	.ascii "EQ_var_u16_i64"
	.space 1
	.ascii "EQ_param_u16_I"
	.space 1
	.ascii "EQ_var_u16_I"
	.space 1
	.ascii "EQ_param_u16_i32"
	.space 1
	.ascii "EQ_var_u16_i32"
	.space 1
	.ascii "EQ_param_u16_i16"
	.space 1
	.ascii "EQ_var_u16_i16"
	.space 1
	.ascii "EQ_param_u16_LC"
	.space 1
	.ascii "EQ_var_u16_LC"
	.space 1
	.ascii "EQ_param_u16_u64"
	.space 1
	.ascii "EQ_var_u16_u64"
	.space 1
	.ascii "EQ_param_u16_u16"
	.space 1
	.ascii "EQ_var_u16_u16"
	.space 3
	.quad	_EQ_M3
	.quad	_L_1
	.quad	_EQ__EQ_param_u32_u32
	.quad	_L_1+6
	.quad	_EQ__EQ_var_u32_u32
	.quad	_L_1+23
	.quad	_EQ__EQ_param_u32_L
	.quad	_L_1+38
	.quad	_EQ__EQ_var_u32_L
	.quad	_L_1+53
	.quad	_EQ__EQ_param_u32_i8
	.quad	_L_1+66
	.quad	_EQ__EQ_var_u32_i8
	.quad	_L_1+82
	.quad	_EQ__EQ_param_u32_u8
	.quad	_L_1+96
	.quad	_EQ__EQ_var_u32_u8
	.quad	_L_1+112
	.quad	_EQ__EQ_param_u32_C
	.quad	_L_1+126
	.quad	_EQ__EQ_var_u32_C
	.quad	_L_1+141
	.quad	_EQ__EQ_param_u32_i64
	.quad	_L_1+154
	.quad	_EQ__EQ_var_u32_i64
	.quad	_L_1+171
	.quad	_EQ__EQ_param_u32_I
	.quad	_L_1+186
	.quad	_EQ__EQ_var_u32_I
	.quad	_L_1+201
	.quad	_EQ__EQ_param_u32_i32
	.quad	_L_1+214
	.quad	_EQ__EQ_var_u32_i32
	.quad	_L_1+231
	.quad	_EQ__EQ_param_u32_i16
	.quad	_L_1+246
	.quad	_EQ__EQ_var_u32_i16
	.quad	_L_1+263
	.quad	_EQ__EQ_param_u32_LC
	.quad	_L_1+278
	.quad	_EQ__EQ_var_u32_LC
	.quad	_L_1+294
	.quad	_EQ__EQ_param_u32_u64
	.quad	_L_1+308
	.quad	_EQ__EQ_var_u32_u64
	.quad	_L_1+325
	.quad	_EQ__EQ_param_u32_u16
	.quad	_L_1+340
	.quad	_EQ__EQ_var_u32_u16
	.quad	_L_1+357
	.quad	_EQ__EQ_param_L_u32
	.quad	_L_1+372
	.quad	_EQ__EQ_var_L_u32
	.quad	_L_1+387
	.quad	_EQ__EQ_param_L_L
	.quad	_L_1+400
	.quad	_EQ__EQ_var_L_L
	.quad	_L_1+413
	.quad	_EQ__EQ_param_L_i8
	.quad	_L_1+424
	.quad	_EQ__EQ_var_L_i8
	.quad	_L_1+438
	.quad	_EQ__EQ_param_L_u8
	.quad	_L_1+450
	.quad	_EQ__EQ_var_L_u8
	.quad	_L_1+464
	.quad	_EQ__EQ_param_L_C
	.quad	_L_1+476
	.quad	_EQ__EQ_var_L_C
	.quad	_L_1+489
	.quad	_EQ__EQ_param_L_i64
	.quad	_L_1+500
	.quad	_EQ__EQ_var_L_i64
	.quad	_L_1+515
	.quad	_EQ__EQ_param_L_I
	.quad	_L_1+528
	.quad	_EQ__EQ_var_L_I
	.quad	_L_1+541
	.quad	_EQ__EQ_param_L_i32
	.quad	_L_1+552
	.quad	_EQ__EQ_var_L_i32
	.quad	_L_1+567
	.quad	_EQ__EQ_param_L_i16
	.quad	_L_1+580
	.quad	_EQ__EQ_var_L_i16
	.quad	_L_1+595
	.quad	_EQ__EQ_param_L_LC
	.quad	_L_1+608
	.quad	_EQ__EQ_var_L_LC
	.quad	_L_1+622
	.quad	_EQ__EQ_param_L_u64
	.quad	_L_1+634
	.quad	_EQ__EQ_var_L_u64
	.quad	_L_1+649
	.quad	_EQ__EQ_param_L_u16
	.quad	_L_1+662
	.quad	_EQ__EQ_var_L_u16
	.quad	_L_1+677
	.quad	_EQ__EQ_param_i8_u32
	.quad	_L_1+690
	.quad	_EQ__EQ_var_i8_u32
	.quad	_L_1+706
	.quad	_EQ__EQ_param_i8_L
	.quad	_L_1+720
	.quad	_EQ__EQ_var_i8_L
	.quad	_L_1+734
	.quad	_EQ__EQ_param_i8_i8
	.quad	_L_1+746
	.quad	_EQ__EQ_var_i8_i8
	.quad	_L_1+761
	.quad	_EQ__EQ_param_i8_u8
	.quad	_L_1+774
	.quad	_EQ__EQ_var_i8_u8
	.quad	_L_1+789
	.quad	_EQ__EQ_param_i8_C
	.quad	_L_1+802
	.quad	_EQ__EQ_var_i8_C
	.quad	_L_1+816
	.quad	_EQ__EQ_param_i8_i64
	.quad	_L_1+828
	.quad	_EQ__EQ_var_i8_i64
	.quad	_L_1+844
	.quad	_EQ__EQ_param_i8_I
	.quad	_L_1+858
	.quad	_EQ__EQ_var_i8_I
	.quad	_L_1+872
	.quad	_EQ__EQ_param_i8_i32
	.quad	_L_1+884
	.quad	_EQ__EQ_var_i8_i32
	.quad	_L_1+900
	.quad	_EQ__EQ_param_i8_i16
	.quad	_L_1+914
	.quad	_EQ__EQ_var_i8_i16
	.quad	_L_1+930
	.quad	_EQ__EQ_param_i8_LC
	.quad	_L_1+944
	.quad	_EQ__EQ_var_i8_LC
	.quad	_L_1+959
	.quad	_EQ__EQ_param_i8_u64
	.quad	_L_1+972
	.quad	_EQ__EQ_var_i8_u64
	.quad	_L_1+988
	.quad	_EQ__EQ_param_i8_u16
	.quad	_L_1+1002
	.quad	_EQ__EQ_var_i8_u16
	.quad	_L_1+1018
	.quad	_EQ__EQ_param_u8_u32
	.quad	_L_1+1032
	.quad	_EQ__EQ_var_u8_u32
	.quad	_L_1+1048
	.quad	_EQ__EQ_param_u8_L
	.quad	_L_1+1062
	.quad	_EQ__EQ_var_u8_L
	.quad	_L_1+1076
	.quad	_EQ__EQ_param_u8_i8
	.quad	_L_1+1088
	.quad	_EQ__EQ_var_u8_i8
	.quad	_L_1+1103
	.quad	_EQ__EQ_param_u8_u8
	.quad	_L_1+1116
	.quad	_EQ__EQ_var_u8_u8
	.quad	_L_1+1131
	.quad	_EQ__EQ_param_u8_C
	.quad	_L_1+1144
	.quad	_EQ__EQ_var_u8_C
	.quad	_L_1+1158
	.quad	_EQ__EQ_param_u8_i64
	.quad	_L_1+1170
	.quad	_EQ__EQ_var_u8_i64
	.quad	_L_1+1186
	.quad	_EQ__EQ_param_u8_I
	.quad	_L_1+1200
	.quad	_EQ__EQ_var_u8_I
	.quad	_L_1+1214
	.quad	_EQ__EQ_param_u8_i32
	.quad	_L_1+1226
	.quad	_EQ__EQ_var_u8_i32
	.quad	_L_1+1242
	.quad	_EQ__EQ_param_u8_i16
	.quad	_L_1+1256
	.quad	_EQ__EQ_var_u8_i16
	.quad	_L_1+1272
	.quad	_EQ__EQ_param_u8_LC
	.quad	_L_1+1286
	.quad	_EQ__EQ_var_u8_LC
	.quad	_L_1+1301
	.quad	_EQ__EQ_param_u8_u64
	.quad	_L_1+1314
	.quad	_EQ__EQ_var_u8_u64
	.quad	_L_1+1330
	.quad	_EQ__EQ_param_u8_u16
	.quad	_L_1+1344
	.quad	_EQ__EQ_var_u8_u16
	.quad	_L_1+1360
	.quad	_EQ__EQ_param_f32_f32
	.quad	_L_1+1374
	.quad	_EQ__EQ_var_f32_f32
	.quad	_L_1+1391
	.quad	_EQ__EQ_param_C_u32
	.quad	_L_1+1406
	.quad	_EQ__EQ_var_C_u32
	.quad	_L_1+1421
	.quad	_EQ__EQ_param_C_L
	.quad	_L_1+1434
	.quad	_EQ__EQ_var_C_L
	.quad	_L_1+1447
	.quad	_EQ__EQ_param_C_i8
	.quad	_L_1+1458
	.quad	_EQ__EQ_var_C_i8
	.quad	_L_1+1472
	.quad	_EQ__EQ_param_C_u8
	.quad	_L_1+1484
	.quad	_EQ__EQ_var_C_u8
	.quad	_L_1+1498
	.quad	_EQ__EQ_param_C_C
	.quad	_L_1+1510
	.quad	_EQ__EQ_var_C_C
	.quad	_L_1+1523
	.quad	_EQ__EQ_param_C_i64
	.quad	_L_1+1534
	.quad	_EQ__EQ_var_C_i64
	.quad	_L_1+1549
	.quad	_EQ__EQ_param_C_I
	.quad	_L_1+1562
	.quad	_EQ__EQ_var_C_I
	.quad	_L_1+1575
	.quad	_EQ__EQ_param_C_i32
	.quad	_L_1+1586
	.quad	_EQ__EQ_var_C_i32
	.quad	_L_1+1601
	.quad	_EQ__EQ_param_C_i16
	.quad	_L_1+1614
	.quad	_EQ__EQ_var_C_i16
	.quad	_L_1+1629
	.quad	_EQ__EQ_param_C_LC
	.quad	_L_1+1642
	.quad	_EQ__EQ_var_C_LC
	.quad	_L_1+1656
	.quad	_EQ__EQ_param_C_u64
	.quad	_L_1+1668
	.quad	_EQ__EQ_var_C_u64
	.quad	_L_1+1683
	.quad	_EQ__EQ_param_C_u16
	.quad	_L_1+1696
	.quad	_EQ__EQ_var_C_u16
	.quad	_L_1+1711
	.quad	_EQ__EQ_param_i64_u32
	.quad	_L_1+1724
	.quad	_EQ__EQ_var_i64_u32
	.quad	_L_1+1741
	.quad	_EQ__EQ_param_i64_L
	.quad	_L_1+1756
	.quad	_EQ__EQ_var_i64_L
	.quad	_L_1+1771
	.quad	_EQ__EQ_param_i64_i8
	.quad	_L_1+1784
	.quad	_EQ__EQ_var_i64_i8
	.quad	_L_1+1800
	.quad	_EQ__EQ_param_i64_u8
	.quad	_L_1+1814
	.quad	_EQ__EQ_var_i64_u8
	.quad	_L_1+1830
	.quad	_EQ__EQ_param_i64_C
	.quad	_L_1+1844
	.quad	_EQ__EQ_var_i64_C
	.quad	_L_1+1859
	.quad	_EQ__EQ_param_i64_i64
	.quad	_L_1+1872
	.quad	_EQ__EQ_var_i64_i64
	.quad	_L_1+1889
	.quad	_EQ__EQ_param_i64_I
	.quad	_L_1+1904
	.quad	_EQ__EQ_var_i64_I
	.quad	_L_1+1919
	.quad	_EQ__EQ_param_i64_i32
	.quad	_L_1+1932
	.quad	_EQ__EQ_var_i64_i32
	.quad	_L_1+1949
	.quad	_EQ__EQ_param_i64_i16
	.quad	_L_1+1964
	.quad	_EQ__EQ_var_i64_i16
	.quad	_L_1+1981
	.quad	_EQ__EQ_param_i64_LC
	.quad	_L_1+1996
	.quad	_EQ__EQ_var_i64_LC
	.quad	_L_1+2012
	.quad	_EQ__EQ_param_i64_u64
	.quad	_L_1+2026
	.quad	_EQ__EQ_var_i64_u64
	.quad	_L_1+2043
	.quad	_EQ__EQ_param_i64_u16
	.quad	_L_1+2058
	.quad	_EQ__EQ_var_i64_u16
	.quad	_L_1+2075
	.quad	_EQ__EQ_param_I_u32
	.quad	_L_1+2090
	.quad	_EQ__EQ_var_I_u32
	.quad	_L_1+2105
	.quad	_EQ__EQ_param_I_L
	.quad	_L_1+2118
	.quad	_EQ__EQ_var_I_L
	.quad	_L_1+2131
	.quad	_EQ__EQ_param_I_i8
	.quad	_L_1+2142
	.quad	_EQ__EQ_var_I_i8
	.quad	_L_1+2156
	.quad	_EQ__EQ_param_I_u8
	.quad	_L_1+2168
	.quad	_EQ__EQ_var_I_u8
	.quad	_L_1+2182
	.quad	_EQ__EQ_param_I_C
	.quad	_L_1+2194
	.quad	_EQ__EQ_var_I_C
	.quad	_L_1+2207
	.quad	_EQ__EQ_param_I_i64
	.quad	_L_1+2218
	.quad	_EQ__EQ_var_I_i64
	.quad	_L_1+2233
	.quad	_EQ__EQ_param_I_I
	.quad	_L_1+2246
	.quad	_EQ__EQ_var_I_I
	.quad	_L_1+2259
	.quad	_EQ__EQ_param_I_i32
	.quad	_L_1+2270
	.quad	_EQ__EQ_var_I_i32
	.quad	_L_1+2285
	.quad	_EQ__EQ_param_I_i16
	.quad	_L_1+2298
	.quad	_EQ__EQ_var_I_i16
	.quad	_L_1+2313
	.quad	_EQ__EQ_param_I_LC
	.quad	_L_1+2326
	.quad	_EQ__EQ_var_I_LC
	.quad	_L_1+2340
	.quad	_EQ__EQ_param_I_u64
	.quad	_L_1+2352
	.quad	_EQ__EQ_var_I_u64
	.quad	_L_1+2367
	.quad	_EQ__EQ_param_I_u16
	.quad	_L_1+2380
	.quad	_EQ__EQ_var_I_u16
	.quad	_L_1+2395
	.quad	_EQ__EQ_param_i32_u32
	.quad	_L_1+2408
	.quad	_EQ__EQ_var_i32_u32
	.quad	_L_1+2425
	.quad	_EQ__EQ_param_i32_L
	.quad	_L_1+2440
	.quad	_EQ__EQ_var_i32_L
	.quad	_L_1+2455
	.quad	_EQ__EQ_param_i32_i8
	.quad	_L_1+2468
	.quad	_EQ__EQ_var_i32_i8
	.quad	_L_1+2484
	.quad	_EQ__EQ_param_i32_u8
	.quad	_L_1+2498
	.quad	_EQ__EQ_var_i32_u8
	.quad	_L_1+2514
	.quad	_EQ__EQ_param_i32_C
	.quad	_L_1+2528
	.quad	_EQ__EQ_var_i32_C
	.quad	_L_1+2543
	.quad	_EQ__EQ_param_i32_i64
	.quad	_L_1+2556
	.quad	_EQ__EQ_var_i32_i64
	.quad	_L_1+2573
	.quad	_EQ__EQ_param_i32_I
	.quad	_L_1+2588
	.quad	_EQ__EQ_var_i32_I
	.quad	_L_1+2603
	.quad	_EQ__EQ_param_i32_i32
	.quad	_L_1+2616
	.quad	_EQ__EQ_var_i32_i32
	.quad	_L_1+2633
	.quad	_EQ__EQ_param_i32_i16
	.quad	_L_1+2648
	.quad	_EQ__EQ_var_i32_i16
	.quad	_L_1+2665
	.quad	_EQ__EQ_param_i32_LC
	.quad	_L_1+2680
	.quad	_EQ__EQ_var_i32_LC
	.quad	_L_1+2696
	.quad	_EQ__EQ_param_i32_u64
	.quad	_L_1+2710
	.quad	_EQ__EQ_var_i32_u64
	.quad	_L_1+2727
	.quad	_EQ__EQ_param_i32_u16
	.quad	_L_1+2742
	.quad	_EQ__EQ_var_i32_u16
	.quad	_L_1+2759
	.quad	_EQ__EQ_param_i16_u32
	.quad	_L_1+2774
	.quad	_EQ__EQ_var_i16_u32
	.quad	_L_1+2791
	.quad	_EQ__EQ_param_i16_L
	.quad	_L_1+2806
	.quad	_EQ__EQ_var_i16_L
	.quad	_L_1+2821
	.quad	_EQ__EQ_param_i16_i8
	.quad	_L_1+2834
	.quad	_EQ__EQ_var_i16_i8
	.quad	_L_1+2850
	.quad	_EQ__EQ_param_i16_u8
	.quad	_L_1+2864
	.quad	_EQ__EQ_var_i16_u8
	.quad	_L_1+2880
	.quad	_EQ__EQ_param_i16_C
	.quad	_L_1+2894
	.quad	_EQ__EQ_var_i16_C
	.quad	_L_1+2909
	.quad	_EQ__EQ_param_i16_i64
	.quad	_L_1+2922
	.quad	_EQ__EQ_var_i16_i64
	.quad	_L_1+2939
	.quad	_EQ__EQ_param_i16_I
	.quad	_L_1+2954
	.quad	_EQ__EQ_var_i16_I
	.quad	_L_1+2969
	.quad	_EQ__EQ_param_i16_i32
	.quad	_L_1+2982
	.quad	_EQ__EQ_var_i16_i32
	.quad	_L_1+2999
	.quad	_EQ__EQ_param_i16_i16
	.quad	_L_1+3014
	.quad	_EQ__EQ_var_i16_i16
	.quad	_L_1+3031
	.quad	_EQ__EQ_param_i16_LC
	.quad	_L_1+3046
	.quad	_EQ__EQ_var_i16_LC
	.quad	_L_1+3062
	.quad	_EQ__EQ_param_i16_u64
	.quad	_L_1+3076
	.quad	_EQ__EQ_var_i16_u64
	.quad	_L_1+3093
	.quad	_EQ__EQ_param_i16_u16
	.quad	_L_1+3108
	.quad	_EQ__EQ_var_i16_u16
	.quad	_L_1+3125
	.quad	_EQ__EQ_param_f64_f64
	.quad	_L_1+3140
	.quad	_EQ__EQ_var_f64_f64
	.quad	_L_1+3157
	.quad	_EQ__EQ_param_LC_u32
	.quad	_L_1+3172
	.quad	_EQ__EQ_var_LC_u32
	.quad	_L_1+3188
	.quad	_EQ__EQ_param_LC_L
	.quad	_L_1+3202
	.quad	_EQ__EQ_var_LC_L
	.quad	_L_1+3216
	.quad	_EQ__EQ_param_LC_i8
	.quad	_L_1+3228
	.quad	_EQ__EQ_var_LC_i8
	.quad	_L_1+3243
	.quad	_EQ__EQ_param_LC_u8
	.quad	_L_1+3256
	.quad	_EQ__EQ_var_LC_u8
	.quad	_L_1+3271
	.quad	_EQ__EQ_param_LC_C
	.quad	_L_1+3284
	.quad	_EQ__EQ_var_LC_C
	.quad	_L_1+3298
	.quad	_EQ__EQ_param_LC_i64
	.quad	_L_1+3310
	.quad	_EQ__EQ_var_LC_i64
	.quad	_L_1+3326
	.quad	_EQ__EQ_param_LC_I
	.quad	_L_1+3340
	.quad	_EQ__EQ_var_LC_I
	.quad	_L_1+3354
	.quad	_EQ__EQ_param_LC_i32
	.quad	_L_1+3366
	.quad	_EQ__EQ_var_LC_i32
	.quad	_L_1+3382
	.quad	_EQ__EQ_param_LC_i16
	.quad	_L_1+3396
	.quad	_EQ__EQ_var_LC_i16
	.quad	_L_1+3412
	.quad	_EQ__EQ_param_LC_LC
	.quad	_L_1+3426
	.quad	_EQ__EQ_var_LC_LC
	.quad	_L_1+3441
	.quad	_EQ__EQ_param_LC_u64
	.quad	_L_1+3454
	.quad	_EQ__EQ_var_LC_u64
	.quad	_L_1+3470
	.quad	_EQ__EQ_param_LC_u16
	.quad	_L_1+3484
	.quad	_EQ__EQ_var_LC_u16
	.quad	_L_1+3500
	.quad	_EQ__EQ_param_u64_u32
	.quad	_L_1+3514
	.quad	_EQ__EQ_var_u64_u32
	.quad	_L_1+3531
	.quad	_EQ__EQ_param_u64_L
	.quad	_L_1+3546
	.quad	_EQ__EQ_var_u64_L
	.quad	_L_1+3561
	.quad	_EQ__EQ_param_u64_i8
	.quad	_L_1+3574
	.quad	_EQ__EQ_var_u64_i8
	.quad	_L_1+3590
	.quad	_EQ__EQ_param_u64_u8
	.quad	_L_1+3604
	.quad	_EQ__EQ_var_u64_u8
	.quad	_L_1+3620
	.quad	_EQ__EQ_param_u64_C
	.quad	_L_1+3634
	.quad	_EQ__EQ_var_u64_C
	.quad	_L_1+3649
	.quad	_EQ__EQ_param_u64_i64
	.quad	_L_1+3662
	.quad	_EQ__EQ_var_u64_i64
	.quad	_L_1+3679
	.quad	_EQ__EQ_param_u64_I
	.quad	_L_1+3694
	.quad	_EQ__EQ_var_u64_I
	.quad	_L_1+3709
	.quad	_EQ__EQ_param_u64_i32
	.quad	_L_1+3722
	.quad	_EQ__EQ_var_u64_i32
	.quad	_L_1+3739
	.quad	_EQ__EQ_param_u64_i16
	.quad	_L_1+3754
	.quad	_EQ__EQ_var_u64_i16
	.quad	_L_1+3771
	.quad	_EQ__EQ_param_u64_LC
	.quad	_L_1+3786
	.quad	_EQ__EQ_var_u64_LC
	.quad	_L_1+3802
	.quad	_EQ__EQ_param_u64_u64
	.quad	_L_1+3816
	.quad	_EQ__EQ_var_u64_u64
	.quad	_L_1+3833
	.quad	_EQ__EQ_param_u64_u16
	.quad	_L_1+3848
	.quad	_EQ__EQ_var_u64_u16
	.quad	_L_1+3865
	.quad	_EQ__EQ_param_u16_u32
	.quad	_L_1+3880
	.quad	_EQ__EQ_var_u16_u32
	.quad	_L_1+3897
	.quad	_EQ__EQ_param_u16_L
	.quad	_L_1+3912
	.quad	_EQ__EQ_var_u16_L
	.quad	_L_1+3927
	.quad	_EQ__EQ_param_u16_i8
	.quad	_L_1+3940
	.quad	_EQ__EQ_var_u16_i8
	.quad	_L_1+3956
	.quad	_EQ__EQ_param_u16_u8
	.quad	_L_1+3970
	.quad	_EQ__EQ_var_u16_u8
	.quad	_L_1+3986
	.quad	_EQ__EQ_param_u16_C
	.quad	_L_1+4000
	.quad	_EQ__EQ_var_u16_C
	.quad	_L_1+4015
	.quad	_EQ__EQ_param_u16_i64
	.quad	_L_1+4028
	.quad	_EQ__EQ_var_u16_i64
	.quad	_L_1+4045
	.quad	_EQ__EQ_param_u16_I
	.quad	_L_1+4060
	.quad	_EQ__EQ_var_u16_I
	.quad	_L_1+4075
	.quad	_EQ__EQ_param_u16_i32
	.quad	_L_1+4088
	.quad	_EQ__EQ_var_u16_i32
	.quad	_L_1+4105
	.quad	_EQ__EQ_param_u16_i16
	.quad	_L_1+4120
	.quad	_EQ__EQ_var_u16_i16
	.quad	_L_1+4137
	.quad	_EQ__EQ_param_u16_LC
	.quad	_L_1+4152
	.quad	_EQ__EQ_var_u16_LC
	.quad	_L_1+4168
	.quad	_EQ__EQ_param_u16_u64
	.quad	_L_1+4182
	.quad	_EQ__EQ_var_u16_u64
	.quad	_L_1+4199
	.quad	_EQ__EQ_param_u16_u16
	.quad	_L_1+4214
	.quad	_EQ__EQ_var_u16_u16
	.quad	_L_1+4231
	.space 8
	.ascii "../AMD64_DARWIN/EQ.m3"
	.space 3
	.data
	.align 5
_MM_EQ:
	.quad	_L_1+8944
	.space 32
	.quad	_L_1+4248
	.space 24
	.quad	_MM_EQ+208
	.space 8
	.quad	_EQ_M3
	.quad	3
	.word	529
	.space 6
	.quad	530
	.quad	531
	.long	2508260901
	.long	1082172483
	.word	534
	.space 2
	.long	535
	.quad	536
	.quad	537
	.quad	538
	.long	1141301903
	.byte	29
	.byte	30
	.space 2
	.quad	543
	.long	544
	.space 28
	.quad	_EQ_I3
	.quad	_MM_EQ+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_EQ+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_EQ+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_EQ+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
