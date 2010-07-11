	.text
.globl _Xor__uXor_var_u16_u16
	.private_extern _Xor__uXor_var_u16_u16
_Xor__uXor_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_u16_u16
	.private_extern _Xor__uXor_param_u16_u16
_Xor__uXor_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_u64
	.private_extern _Xor__uXor_var_u16_u64
_Xor__uXor_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_u64
	.private_extern _Xor__uXor_param_u16_u64
_Xor__uXor_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_LC
	.private_extern _Xor__uXor_var_u16_LC
_Xor__uXor_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_LC
	.private_extern _Xor__uXor_param_u16_LC
_Xor__uXor_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u16_i16
	.private_extern _Xor__uXor_var_u16_i16
_Xor__uXor_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_i16
	.private_extern _Xor__uXor_param_u16_i16
_Xor__uXor_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_i32
	.private_extern _Xor__uXor_var_u16_i32
_Xor__uXor_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_i32
	.private_extern _Xor__uXor_param_u16_i32
_Xor__uXor_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_I
	.private_extern _Xor__uXor_var_u16_I
_Xor__uXor_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_I
	.private_extern _Xor__uXor_param_u16_I
_Xor__uXor_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_i64
	.private_extern _Xor__uXor_var_u16_i64
_Xor__uXor_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_i64
	.private_extern _Xor__uXor_param_u16_i64
_Xor__uXor_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_C
	.private_extern _Xor__uXor_var_u16_C
_Xor__uXor_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_C
	.private_extern _Xor__uXor_param_u16_C
_Xor__uXor_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u16_u8
	.private_extern _Xor__uXor_var_u16_u8
_Xor__uXor_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_u8
	.private_extern _Xor__uXor_param_u16_u8
_Xor__uXor_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_i8
	.private_extern _Xor__uXor_var_u16_i8
_Xor__uXor_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_i8
	.private_extern _Xor__uXor_param_u16_i8
_Xor__uXor_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_L
	.private_extern _Xor__uXor_var_u16_L
_Xor__uXor_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_L
	.private_extern _Xor__uXor_param_u16_L
_Xor__uXor_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u16_u32
	.private_extern _Xor__uXor_var_u16_u32
_Xor__uXor_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u16_u32
	.private_extern _Xor__uXor_param_u16_u32
_Xor__uXor_param_u16_u32:
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
.globl _Xor__uXor_var_u64_u16
	.private_extern _Xor__uXor_var_u64_u16
_Xor__uXor_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_u16
	.private_extern _Xor__uXor_param_u64_u16
_Xor__uXor_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_u64
	.private_extern _Xor__uXor_var_u64_u64
_Xor__uXor_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_u64_u64
	.private_extern _Xor__uXor_param_u64_u64
_Xor__uXor_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_LC
	.private_extern _Xor__uXor_var_u64_LC
_Xor__uXor_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_LC
	.private_extern _Xor__uXor_param_u64_LC
_Xor__uXor_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u64_i16
	.private_extern _Xor__uXor_var_u64_i16
_Xor__uXor_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_i16
	.private_extern _Xor__uXor_param_u64_i16
_Xor__uXor_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_i32
	.private_extern _Xor__uXor_var_u64_i32
_Xor__uXor_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_i32
	.private_extern _Xor__uXor_param_u64_i32
_Xor__uXor_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_I
	.private_extern _Xor__uXor_var_u64_I
_Xor__uXor_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_I
	.private_extern _Xor__uXor_param_u64_I
_Xor__uXor_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_i64
	.private_extern _Xor__uXor_var_u64_i64
_Xor__uXor_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_i64
	.private_extern _Xor__uXor_param_u64_i64
_Xor__uXor_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_C
	.private_extern _Xor__uXor_var_u64_C
_Xor__uXor_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_C
	.private_extern _Xor__uXor_param_u64_C
_Xor__uXor_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u64_u8
	.private_extern _Xor__uXor_var_u64_u8
_Xor__uXor_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_u8
	.private_extern _Xor__uXor_param_u64_u8
_Xor__uXor_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_i8
	.private_extern _Xor__uXor_var_u64_i8
_Xor__uXor_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_i8
	.private_extern _Xor__uXor_param_u64_i8
_Xor__uXor_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_L
	.private_extern _Xor__uXor_var_u64_L
_Xor__uXor_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_L
	.private_extern _Xor__uXor_param_u64_L
_Xor__uXor_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u64_u32
	.private_extern _Xor__uXor_var_u64_u32
_Xor__uXor_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u64_u32
	.private_extern _Xor__uXor_param_u64_u32
_Xor__uXor_param_u64_u32:
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
.globl _Xor__uXor_var_LC_u16
	.private_extern _Xor__uXor_var_LC_u16
_Xor__uXor_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_u16
	.private_extern _Xor__uXor_param_LC_u16
_Xor__uXor_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_u64
	.private_extern _Xor__uXor_var_LC_u64
_Xor__uXor_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Xor(%rip), %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_u64
	.private_extern _Xor__uXor_param_LC_u64
_Xor__uXor_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_LC
	.private_extern _Xor__uXor_var_LC_LC
_Xor__uXor_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_LC_LC
	.private_extern _Xor__uXor_param_LC_LC
_Xor__uXor_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_LC_i16
	.private_extern _Xor__uXor_var_LC_i16
_Xor__uXor_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_i16
	.private_extern _Xor__uXor_param_LC_i16
_Xor__uXor_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_i32
	.private_extern _Xor__uXor_var_LC_i32
_Xor__uXor_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_i32
	.private_extern _Xor__uXor_param_LC_i32
_Xor__uXor_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_I
	.private_extern _Xor__uXor_var_LC_I
_Xor__uXor_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_I
	.private_extern _Xor__uXor_param_LC_I
_Xor__uXor_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_i64
	.private_extern _Xor__uXor_var_LC_i64
_Xor__uXor_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Xor(%rip), %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_i64
	.private_extern _Xor__uXor_param_LC_i64
_Xor__uXor_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_C
	.private_extern _Xor__uXor_var_LC_C
_Xor__uXor_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_C
	.private_extern _Xor__uXor_param_LC_C
_Xor__uXor_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_LC_u8
	.private_extern _Xor__uXor_var_LC_u8
_Xor__uXor_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_u8
	.private_extern _Xor__uXor_param_LC_u8
_Xor__uXor_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_i8
	.private_extern _Xor__uXor_var_LC_i8
_Xor__uXor_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_i8
	.private_extern _Xor__uXor_param_LC_i8
_Xor__uXor_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_L
	.private_extern _Xor__uXor_var_LC_L
_Xor__uXor_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Xor(%rip), %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_L
	.private_extern _Xor__uXor_param_LC_L
_Xor__uXor_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_LC_u32
	.private_extern _Xor__uXor_var_LC_u32
_Xor__uXor_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_LC_u32
	.private_extern _Xor__uXor_param_LC_u32
_Xor__uXor_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i16_u16
	.private_extern _Xor__uXor_var_i16_u16
_Xor__uXor_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_u16
	.private_extern _Xor__uXor_param_i16_u16
_Xor__uXor_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_u64
	.private_extern _Xor__uXor_var_i16_u64
_Xor__uXor_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_u64
	.private_extern _Xor__uXor_param_i16_u64
_Xor__uXor_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_LC
	.private_extern _Xor__uXor_var_i16_LC
_Xor__uXor_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_LC
	.private_extern _Xor__uXor_param_i16_LC
_Xor__uXor_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i16_i16
	.private_extern _Xor__uXor_var_i16_i16
_Xor__uXor_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_i16_i16
	.private_extern _Xor__uXor_param_i16_i16
_Xor__uXor_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_i32
	.private_extern _Xor__uXor_var_i16_i32
_Xor__uXor_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_i32
	.private_extern _Xor__uXor_param_i16_i32
_Xor__uXor_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_I
	.private_extern _Xor__uXor_var_i16_I
_Xor__uXor_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_I
	.private_extern _Xor__uXor_param_i16_I
_Xor__uXor_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_i64
	.private_extern _Xor__uXor_var_i16_i64
_Xor__uXor_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_i64
	.private_extern _Xor__uXor_param_i16_i64
_Xor__uXor_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_C
	.private_extern _Xor__uXor_var_i16_C
_Xor__uXor_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_C
	.private_extern _Xor__uXor_param_i16_C
_Xor__uXor_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i16_u8
	.private_extern _Xor__uXor_var_i16_u8
_Xor__uXor_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_u8
	.private_extern _Xor__uXor_param_i16_u8
_Xor__uXor_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_i8
	.private_extern _Xor__uXor_var_i16_i8
_Xor__uXor_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_i8
	.private_extern _Xor__uXor_param_i16_i8
_Xor__uXor_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_L
	.private_extern _Xor__uXor_var_i16_L
_Xor__uXor_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_L
	.private_extern _Xor__uXor_param_i16_L
_Xor__uXor_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i16_u32
	.private_extern _Xor__uXor_var_i16_u32
_Xor__uXor_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i16_u32
	.private_extern _Xor__uXor_param_i16_u32
_Xor__uXor_param_i16_u32:
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
.globl _Xor__uXor_var_i32_u16
	.private_extern _Xor__uXor_var_i32_u16
_Xor__uXor_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_u16
	.private_extern _Xor__uXor_param_i32_u16
_Xor__uXor_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_u64
	.private_extern _Xor__uXor_var_i32_u64
_Xor__uXor_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_u64
	.private_extern _Xor__uXor_param_i32_u64
_Xor__uXor_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_LC
	.private_extern _Xor__uXor_var_i32_LC
_Xor__uXor_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_LC
	.private_extern _Xor__uXor_param_i32_LC
_Xor__uXor_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i32_i16
	.private_extern _Xor__uXor_var_i32_i16
_Xor__uXor_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_i16
	.private_extern _Xor__uXor_param_i32_i16
_Xor__uXor_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_i32
	.private_extern _Xor__uXor_var_i32_i32
_Xor__uXor_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_i32_i32
	.private_extern _Xor__uXor_param_i32_i32
_Xor__uXor_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_I
	.private_extern _Xor__uXor_var_i32_I
_Xor__uXor_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_I
	.private_extern _Xor__uXor_param_i32_I
_Xor__uXor_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_i64
	.private_extern _Xor__uXor_var_i32_i64
_Xor__uXor_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_i64
	.private_extern _Xor__uXor_param_i32_i64
_Xor__uXor_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_C
	.private_extern _Xor__uXor_var_i32_C
_Xor__uXor_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_C
	.private_extern _Xor__uXor_param_i32_C
_Xor__uXor_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i32_u8
	.private_extern _Xor__uXor_var_i32_u8
_Xor__uXor_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_u8
	.private_extern _Xor__uXor_param_i32_u8
_Xor__uXor_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_i8
	.private_extern _Xor__uXor_var_i32_i8
_Xor__uXor_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_i8
	.private_extern _Xor__uXor_param_i32_i8
_Xor__uXor_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_L
	.private_extern _Xor__uXor_var_i32_L
_Xor__uXor_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_L
	.private_extern _Xor__uXor_param_i32_L
_Xor__uXor_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i32_u32
	.private_extern _Xor__uXor_var_i32_u32
_Xor__uXor_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i32_u32
	.private_extern _Xor__uXor_param_i32_u32
_Xor__uXor_param_i32_u32:
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
.globl _Xor__uXor_var_I_u16
	.private_extern _Xor__uXor_var_I_u16
_Xor__uXor_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_u16
	.private_extern _Xor__uXor_param_I_u16
_Xor__uXor_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_u64
	.private_extern _Xor__uXor_var_I_u64
_Xor__uXor_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_u64
	.private_extern _Xor__uXor_param_I_u64
_Xor__uXor_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_LC
	.private_extern _Xor__uXor_var_I_LC
_Xor__uXor_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_LC
	.private_extern _Xor__uXor_param_I_LC
_Xor__uXor_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_I_i16
	.private_extern _Xor__uXor_var_I_i16
_Xor__uXor_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_i16
	.private_extern _Xor__uXor_param_I_i16
_Xor__uXor_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_i32
	.private_extern _Xor__uXor_var_I_i32
_Xor__uXor_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_i32
	.private_extern _Xor__uXor_param_I_i32
_Xor__uXor_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_I
	.private_extern _Xor__uXor_var_I_I
_Xor__uXor_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_I_I
	.private_extern _Xor__uXor_param_I_I
_Xor__uXor_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_i64
	.private_extern _Xor__uXor_var_I_i64
_Xor__uXor_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_i64
	.private_extern _Xor__uXor_param_I_i64
_Xor__uXor_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_C
	.private_extern _Xor__uXor_var_I_C
_Xor__uXor_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_C
	.private_extern _Xor__uXor_param_I_C
_Xor__uXor_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_I_u8
	.private_extern _Xor__uXor_var_I_u8
_Xor__uXor_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_u8
	.private_extern _Xor__uXor_param_I_u8
_Xor__uXor_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_i8
	.private_extern _Xor__uXor_var_I_i8
_Xor__uXor_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_i8
	.private_extern _Xor__uXor_param_I_i8
_Xor__uXor_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_L
	.private_extern _Xor__uXor_var_I_L
_Xor__uXor_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_L
	.private_extern _Xor__uXor_param_I_L
_Xor__uXor_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_I_u32
	.private_extern _Xor__uXor_var_I_u32
_Xor__uXor_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_I_u32
	.private_extern _Xor__uXor_param_I_u32
_Xor__uXor_param_I_u32:
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
.globl _Xor__uXor_var_i64_u16
	.private_extern _Xor__uXor_var_i64_u16
_Xor__uXor_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_u16
	.private_extern _Xor__uXor_param_i64_u16
_Xor__uXor_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_u64
	.private_extern _Xor__uXor_var_i64_u64
_Xor__uXor_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_u64
	.private_extern _Xor__uXor_param_i64_u64
_Xor__uXor_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_LC
	.private_extern _Xor__uXor_var_i64_LC
_Xor__uXor_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_LC
	.private_extern _Xor__uXor_param_i64_LC
_Xor__uXor_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i64_i16
	.private_extern _Xor__uXor_var_i64_i16
_Xor__uXor_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_i16
	.private_extern _Xor__uXor_param_i64_i16
_Xor__uXor_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_i32
	.private_extern _Xor__uXor_var_i64_i32
_Xor__uXor_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_i32
	.private_extern _Xor__uXor_param_i64_i32
_Xor__uXor_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_I
	.private_extern _Xor__uXor_var_i64_I
_Xor__uXor_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_I
	.private_extern _Xor__uXor_param_i64_I
_Xor__uXor_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_i64
	.private_extern _Xor__uXor_var_i64_i64
_Xor__uXor_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_i64_i64
	.private_extern _Xor__uXor_param_i64_i64
_Xor__uXor_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_C
	.private_extern _Xor__uXor_var_i64_C
_Xor__uXor_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_C
	.private_extern _Xor__uXor_param_i64_C
_Xor__uXor_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i64_u8
	.private_extern _Xor__uXor_var_i64_u8
_Xor__uXor_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_u8
	.private_extern _Xor__uXor_param_i64_u8
_Xor__uXor_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_i8
	.private_extern _Xor__uXor_var_i64_i8
_Xor__uXor_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_i8
	.private_extern _Xor__uXor_param_i64_i8
_Xor__uXor_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_L
	.private_extern _Xor__uXor_var_i64_L
_Xor__uXor_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_L
	.private_extern _Xor__uXor_param_i64_L
_Xor__uXor_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i64_u32
	.private_extern _Xor__uXor_var_i64_u32
_Xor__uXor_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i64_u32
	.private_extern _Xor__uXor_param_i64_u32
_Xor__uXor_param_i64_u32:
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
.globl _Xor__uXor_var_C_u16
	.private_extern _Xor__uXor_var_C_u16
_Xor__uXor_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_u16
	.private_extern _Xor__uXor_param_C_u16
_Xor__uXor_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_u64
	.private_extern _Xor__uXor_var_C_u64
_Xor__uXor_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_u64
	.private_extern _Xor__uXor_param_C_u64
_Xor__uXor_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_LC
	.private_extern _Xor__uXor_var_C_LC
_Xor__uXor_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_LC
	.private_extern _Xor__uXor_param_C_LC
_Xor__uXor_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_C_i16
	.private_extern _Xor__uXor_var_C_i16
_Xor__uXor_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_i16
	.private_extern _Xor__uXor_param_C_i16
_Xor__uXor_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_i32
	.private_extern _Xor__uXor_var_C_i32
_Xor__uXor_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_i32
	.private_extern _Xor__uXor_param_C_i32
_Xor__uXor_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_I
	.private_extern _Xor__uXor_var_C_I
_Xor__uXor_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_I
	.private_extern _Xor__uXor_param_C_I
_Xor__uXor_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_i64
	.private_extern _Xor__uXor_var_C_i64
_Xor__uXor_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_i64
	.private_extern _Xor__uXor_param_C_i64
_Xor__uXor_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_C
	.private_extern _Xor__uXor_var_C_C
_Xor__uXor_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_C_C
	.private_extern _Xor__uXor_param_C_C
_Xor__uXor_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_C_u8
	.private_extern _Xor__uXor_var_C_u8
_Xor__uXor_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_u8
	.private_extern _Xor__uXor_param_C_u8
_Xor__uXor_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_i8
	.private_extern _Xor__uXor_var_C_i8
_Xor__uXor_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_i8
	.private_extern _Xor__uXor_param_C_i8
_Xor__uXor_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_L
	.private_extern _Xor__uXor_var_C_L
_Xor__uXor_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_L
	.private_extern _Xor__uXor_param_C_L
_Xor__uXor_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_C_u32
	.private_extern _Xor__uXor_var_C_u32
_Xor__uXor_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_C_u32
	.private_extern _Xor__uXor_param_C_u32
_Xor__uXor_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	xorq	-24(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u8_u16
	.private_extern _Xor__uXor_var_u8_u16
_Xor__uXor_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_u16
	.private_extern _Xor__uXor_param_u8_u16
_Xor__uXor_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_u64
	.private_extern _Xor__uXor_var_u8_u64
_Xor__uXor_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_u64
	.private_extern _Xor__uXor_param_u8_u64
_Xor__uXor_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_LC
	.private_extern _Xor__uXor_var_u8_LC
_Xor__uXor_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_LC
	.private_extern _Xor__uXor_param_u8_LC
_Xor__uXor_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u8_i16
	.private_extern _Xor__uXor_var_u8_i16
_Xor__uXor_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_i16
	.private_extern _Xor__uXor_param_u8_i16
_Xor__uXor_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_i32
	.private_extern _Xor__uXor_var_u8_i32
_Xor__uXor_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_i32
	.private_extern _Xor__uXor_param_u8_i32
_Xor__uXor_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_I
	.private_extern _Xor__uXor_var_u8_I
_Xor__uXor_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_I
	.private_extern _Xor__uXor_param_u8_I
_Xor__uXor_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_i64
	.private_extern _Xor__uXor_var_u8_i64
_Xor__uXor_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_i64
	.private_extern _Xor__uXor_param_u8_i64
_Xor__uXor_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_C
	.private_extern _Xor__uXor_var_u8_C
_Xor__uXor_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_C
	.private_extern _Xor__uXor_param_u8_C
_Xor__uXor_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u8_u8
	.private_extern _Xor__uXor_var_u8_u8
_Xor__uXor_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_u8_u8
	.private_extern _Xor__uXor_param_u8_u8
_Xor__uXor_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_i8
	.private_extern _Xor__uXor_var_u8_i8
_Xor__uXor_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_i8
	.private_extern _Xor__uXor_param_u8_i8
_Xor__uXor_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_L
	.private_extern _Xor__uXor_var_u8_L
_Xor__uXor_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_L
	.private_extern _Xor__uXor_param_u8_L
_Xor__uXor_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_u8_u32
	.private_extern _Xor__uXor_var_u8_u32
_Xor__uXor_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u8_u32
	.private_extern _Xor__uXor_param_u8_u32
_Xor__uXor_param_u8_u32:
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
.globl _Xor__uXor_var_i8_u16
	.private_extern _Xor__uXor_var_i8_u16
_Xor__uXor_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_u16
	.private_extern _Xor__uXor_param_i8_u16
_Xor__uXor_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_u64
	.private_extern _Xor__uXor_var_i8_u64
_Xor__uXor_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_u64
	.private_extern _Xor__uXor_param_i8_u64
_Xor__uXor_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_LC
	.private_extern _Xor__uXor_var_i8_LC
_Xor__uXor_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_LC
	.private_extern _Xor__uXor_param_i8_LC
_Xor__uXor_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i8_i16
	.private_extern _Xor__uXor_var_i8_i16
_Xor__uXor_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_i16
	.private_extern _Xor__uXor_param_i8_i16
_Xor__uXor_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_i32
	.private_extern _Xor__uXor_var_i8_i32
_Xor__uXor_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_i32
	.private_extern _Xor__uXor_param_i8_i32
_Xor__uXor_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_I
	.private_extern _Xor__uXor_var_i8_I
_Xor__uXor_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_I
	.private_extern _Xor__uXor_param_i8_I
_Xor__uXor_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_i64
	.private_extern _Xor__uXor_var_i8_i64
_Xor__uXor_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_i64
	.private_extern _Xor__uXor_param_i8_i64
_Xor__uXor_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_C
	.private_extern _Xor__uXor_var_i8_C
_Xor__uXor_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_C
	.private_extern _Xor__uXor_param_i8_C
_Xor__uXor_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_i8_u8
	.private_extern _Xor__uXor_var_i8_u8
_Xor__uXor_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_u8
	.private_extern _Xor__uXor_param_i8_u8
_Xor__uXor_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_i8
	.private_extern _Xor__uXor_var_i8_i8
_Xor__uXor_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_i8_i8
	.private_extern _Xor__uXor_param_i8_i8
_Xor__uXor_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_L
	.private_extern _Xor__uXor_var_i8_L
_Xor__uXor_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_L
	.private_extern _Xor__uXor_param_i8_L
_Xor__uXor_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_i8_u32
	.private_extern _Xor__uXor_var_i8_u32
_Xor__uXor_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_i8_u32
	.private_extern _Xor__uXor_param_i8_u32
_Xor__uXor_param_i8_u32:
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
.globl _Xor__uXor_var_L_u16
	.private_extern _Xor__uXor_var_L_u16
_Xor__uXor_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_u16
	.private_extern _Xor__uXor_param_L_u16
_Xor__uXor_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_u64
	.private_extern _Xor__uXor_var_L_u64
_Xor__uXor_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_u64
	.private_extern _Xor__uXor_param_L_u64
_Xor__uXor_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_LC
	.private_extern _Xor__uXor_var_L_LC
_Xor__uXor_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_LC
	.private_extern _Xor__uXor_param_L_LC
_Xor__uXor_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_L_i16
	.private_extern _Xor__uXor_var_L_i16
_Xor__uXor_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_i16
	.private_extern _Xor__uXor_param_L_i16
_Xor__uXor_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_i32
	.private_extern _Xor__uXor_var_L_i32
_Xor__uXor_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_i32
	.private_extern _Xor__uXor_param_L_i32
_Xor__uXor_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_I
	.private_extern _Xor__uXor_var_L_I
_Xor__uXor_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_I
	.private_extern _Xor__uXor_param_L_I
_Xor__uXor_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_i64
	.private_extern _Xor__uXor_var_L_i64
_Xor__uXor_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_i64
	.private_extern _Xor__uXor_param_L_i64
_Xor__uXor_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_C
	.private_extern _Xor__uXor_var_L_C
_Xor__uXor_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Xor(%rip), %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_C
	.private_extern _Xor__uXor_param_L_C
_Xor__uXor_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_L_u8
	.private_extern _Xor__uXor_var_L_u8
_Xor__uXor_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_u8
	.private_extern _Xor__uXor_param_L_u8
_Xor__uXor_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_i8
	.private_extern _Xor__uXor_var_L_i8
_Xor__uXor_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_i8
	.private_extern _Xor__uXor_param_L_i8
_Xor__uXor_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_L
	.private_extern _Xor__uXor_var_L_L
_Xor__uXor_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_L_L
	.private_extern _Xor__uXor_param_L_L
_Xor__uXor_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_var_L_u32
	.private_extern _Xor__uXor_var_L_u32
_Xor__uXor_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_L_u32
	.private_extern _Xor__uXor_param_L_u32
_Xor__uXor_param_L_u32:
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
.globl _Xor__uXor_var_u32_u16
	.private_extern _Xor__uXor_var_u32_u16
_Xor__uXor_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_u16
	.private_extern _Xor__uXor_param_u32_u16
_Xor__uXor_param_u32_u16:
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
.globl _Xor__uXor_var_u32_u64
	.private_extern _Xor__uXor_var_u32_u64
_Xor__uXor_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_u64
	.private_extern _Xor__uXor_param_u32_u64
_Xor__uXor_param_u32_u64:
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
.globl _Xor__uXor_var_u32_LC
	.private_extern _Xor__uXor_var_u32_LC
_Xor__uXor_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_LC
	.private_extern _Xor__uXor_param_u32_LC
_Xor__uXor_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u32_i16
	.private_extern _Xor__uXor_var_u32_i16
_Xor__uXor_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_i16
	.private_extern _Xor__uXor_param_u32_i16
_Xor__uXor_param_u32_i16:
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
.globl _Xor__uXor_var_u32_i32
	.private_extern _Xor__uXor_var_u32_i32
_Xor__uXor_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_i32
	.private_extern _Xor__uXor_param_u32_i32
_Xor__uXor_param_u32_i32:
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
.globl _Xor__uXor_var_u32_I
	.private_extern _Xor__uXor_var_u32_I
_Xor__uXor_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_I
	.private_extern _Xor__uXor_param_u32_I
_Xor__uXor_param_u32_I:
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
.globl _Xor__uXor_var_u32_i64
	.private_extern _Xor__uXor_var_u32_i64
_Xor__uXor_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_i64
	.private_extern _Xor__uXor_param_u32_i64
_Xor__uXor_param_u32_i64:
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
.globl _Xor__uXor_var_u32_C
	.private_extern _Xor__uXor_var_u32_C
_Xor__uXor_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_C
	.private_extern _Xor__uXor_param_u32_C
_Xor__uXor_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	xorq	-32(%rbp), %rax
	leave
	ret
.globl _Xor__uXor_var_u32_u8
	.private_extern _Xor__uXor_var_u32_u8
_Xor__uXor_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Xor(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_u8
	.private_extern _Xor__uXor_param_u32_u8
_Xor__uXor_param_u32_u8:
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
.globl _Xor__uXor_var_u32_i8
	.private_extern _Xor__uXor_var_u32_i8
_Xor__uXor_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_i8
	.private_extern _Xor__uXor_param_u32_i8
_Xor__uXor_param_u32_i8:
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
.globl _Xor__uXor_var_u32_L
	.private_extern _Xor__uXor_var_u32_L
_Xor__uXor_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Xor(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _Xor__uXor_param_u32_L
	.private_extern _Xor__uXor_param_u32_L
_Xor__uXor_param_u32_L:
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
.globl _Xor__uXor_var_u32_u32
	.private_extern _Xor__uXor_var_u32_u32
_Xor__uXor_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _Xor__uXor_param_u32_u32
	.private_extern _Xor__uXor_param_u32_u32
_Xor__uXor_param_u32_u32:
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
.globl _Xor_M3
_Xor_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Xor(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Xor_M3"
	.space 1
	.ascii "uXor_param_u32_u32"
	.space 1
	.ascii "uXor_var_u32_u32"
	.space 1
	.ascii "uXor_param_u32_L"
	.space 1
	.ascii "uXor_var_u32_L"
	.space 1
	.ascii "uXor_param_u32_i8"
	.space 1
	.ascii "uXor_var_u32_i8"
	.space 1
	.ascii "uXor_param_u32_u8"
	.space 1
	.ascii "uXor_var_u32_u8"
	.space 1
	.ascii "uXor_param_u32_C"
	.space 1
	.ascii "uXor_var_u32_C"
	.space 1
	.ascii "uXor_param_u32_i64"
	.space 1
	.ascii "uXor_var_u32_i64"
	.space 1
	.ascii "uXor_param_u32_I"
	.space 1
	.ascii "uXor_var_u32_I"
	.space 1
	.ascii "uXor_param_u32_i32"
	.space 1
	.ascii "uXor_var_u32_i32"
	.space 1
	.ascii "uXor_param_u32_i16"
	.space 1
	.ascii "uXor_var_u32_i16"
	.space 1
	.ascii "uXor_param_u32_LC"
	.space 1
	.ascii "uXor_var_u32_LC"
	.space 1
	.ascii "uXor_param_u32_u64"
	.space 1
	.ascii "uXor_var_u32_u64"
	.space 1
	.ascii "uXor_param_u32_u16"
	.space 1
	.ascii "uXor_var_u32_u16"
	.space 1
	.ascii "uXor_param_L_u32"
	.space 1
	.ascii "uXor_var_L_u32"
	.space 1
	.ascii "uXor_param_L_L"
	.space 1
	.ascii "uXor_var_L_L"
	.space 1
	.ascii "uXor_param_L_i8"
	.space 1
	.ascii "uXor_var_L_i8"
	.space 1
	.ascii "uXor_param_L_u8"
	.space 1
	.ascii "uXor_var_L_u8"
	.space 1
	.ascii "uXor_param_L_C"
	.space 1
	.ascii "uXor_var_L_C"
	.space 1
	.ascii "uXor_param_L_i64"
	.space 1
	.ascii "uXor_var_L_i64"
	.space 1
	.ascii "uXor_param_L_I"
	.space 1
	.ascii "uXor_var_L_I"
	.space 1
	.ascii "uXor_param_L_i32"
	.space 1
	.ascii "uXor_var_L_i32"
	.space 1
	.ascii "uXor_param_L_i16"
	.space 1
	.ascii "uXor_var_L_i16"
	.space 1
	.ascii "uXor_param_L_LC"
	.space 1
	.ascii "uXor_var_L_LC"
	.space 1
	.ascii "uXor_param_L_u64"
	.space 1
	.ascii "uXor_var_L_u64"
	.space 1
	.ascii "uXor_param_L_u16"
	.space 1
	.ascii "uXor_var_L_u16"
	.space 1
	.ascii "uXor_param_i8_u32"
	.space 1
	.ascii "uXor_var_i8_u32"
	.space 1
	.ascii "uXor_param_i8_L"
	.space 1
	.ascii "uXor_var_i8_L"
	.space 1
	.ascii "uXor_param_i8_i8"
	.space 1
	.ascii "uXor_var_i8_i8"
	.space 1
	.ascii "uXor_param_i8_u8"
	.space 1
	.ascii "uXor_var_i8_u8"
	.space 1
	.ascii "uXor_param_i8_C"
	.space 1
	.ascii "uXor_var_i8_C"
	.space 1
	.ascii "uXor_param_i8_i64"
	.space 1
	.ascii "uXor_var_i8_i64"
	.space 1
	.ascii "uXor_param_i8_I"
	.space 1
	.ascii "uXor_var_i8_I"
	.space 1
	.ascii "uXor_param_i8_i32"
	.space 1
	.ascii "uXor_var_i8_i32"
	.space 1
	.ascii "uXor_param_i8_i16"
	.space 1
	.ascii "uXor_var_i8_i16"
	.space 1
	.ascii "uXor_param_i8_LC"
	.space 1
	.ascii "uXor_var_i8_LC"
	.space 1
	.ascii "uXor_param_i8_u64"
	.space 1
	.ascii "uXor_var_i8_u64"
	.space 1
	.ascii "uXor_param_i8_u16"
	.space 1
	.ascii "uXor_var_i8_u16"
	.space 1
	.ascii "uXor_param_u8_u32"
	.space 1
	.ascii "uXor_var_u8_u32"
	.space 1
	.ascii "uXor_param_u8_L"
	.space 1
	.ascii "uXor_var_u8_L"
	.space 1
	.ascii "uXor_param_u8_i8"
	.space 1
	.ascii "uXor_var_u8_i8"
	.space 1
	.ascii "uXor_param_u8_u8"
	.space 1
	.ascii "uXor_var_u8_u8"
	.space 1
	.ascii "uXor_param_u8_C"
	.space 1
	.ascii "uXor_var_u8_C"
	.space 1
	.ascii "uXor_param_u8_i64"
	.space 1
	.ascii "uXor_var_u8_i64"
	.space 1
	.ascii "uXor_param_u8_I"
	.space 1
	.ascii "uXor_var_u8_I"
	.space 1
	.ascii "uXor_param_u8_i32"
	.space 1
	.ascii "uXor_var_u8_i32"
	.space 1
	.ascii "uXor_param_u8_i16"
	.space 1
	.ascii "uXor_var_u8_i16"
	.space 1
	.ascii "uXor_param_u8_LC"
	.space 1
	.ascii "uXor_var_u8_LC"
	.space 1
	.ascii "uXor_param_u8_u64"
	.space 1
	.ascii "uXor_var_u8_u64"
	.space 1
	.ascii "uXor_param_u8_u16"
	.space 1
	.ascii "uXor_var_u8_u16"
	.space 1
	.ascii "uXor_param_C_u32"
	.space 1
	.ascii "uXor_var_C_u32"
	.space 1
	.ascii "uXor_param_C_L"
	.space 1
	.ascii "uXor_var_C_L"
	.space 1
	.ascii "uXor_param_C_i8"
	.space 1
	.ascii "uXor_var_C_i8"
	.space 1
	.ascii "uXor_param_C_u8"
	.space 1
	.ascii "uXor_var_C_u8"
	.space 1
	.ascii "uXor_param_C_C"
	.space 1
	.ascii "uXor_var_C_C"
	.space 1
	.ascii "uXor_param_C_i64"
	.space 1
	.ascii "uXor_var_C_i64"
	.space 1
	.ascii "uXor_param_C_I"
	.space 1
	.ascii "uXor_var_C_I"
	.space 1
	.ascii "uXor_param_C_i32"
	.space 1
	.ascii "uXor_var_C_i32"
	.space 1
	.ascii "uXor_param_C_i16"
	.space 1
	.ascii "uXor_var_C_i16"
	.space 1
	.ascii "uXor_param_C_LC"
	.space 1
	.ascii "uXor_var_C_LC"
	.space 1
	.ascii "uXor_param_C_u64"
	.space 1
	.ascii "uXor_var_C_u64"
	.space 1
	.ascii "uXor_param_C_u16"
	.space 1
	.ascii "uXor_var_C_u16"
	.space 1
	.ascii "uXor_param_i64_u32"
	.space 1
	.ascii "uXor_var_i64_u32"
	.space 1
	.ascii "uXor_param_i64_L"
	.space 1
	.ascii "uXor_var_i64_L"
	.space 1
	.ascii "uXor_param_i64_i8"
	.space 1
	.ascii "uXor_var_i64_i8"
	.space 1
	.ascii "uXor_param_i64_u8"
	.space 1
	.ascii "uXor_var_i64_u8"
	.space 1
	.ascii "uXor_param_i64_C"
	.space 1
	.ascii "uXor_var_i64_C"
	.space 1
	.ascii "uXor_param_i64_i64"
	.space 1
	.ascii "uXor_var_i64_i64"
	.space 1
	.ascii "uXor_param_i64_I"
	.space 1
	.ascii "uXor_var_i64_I"
	.space 1
	.ascii "uXor_param_i64_i32"
	.space 1
	.ascii "uXor_var_i64_i32"
	.space 1
	.ascii "uXor_param_i64_i16"
	.space 1
	.ascii "uXor_var_i64_i16"
	.space 1
	.ascii "uXor_param_i64_LC"
	.space 1
	.ascii "uXor_var_i64_LC"
	.space 1
	.ascii "uXor_param_i64_u64"
	.space 1
	.ascii "uXor_var_i64_u64"
	.space 1
	.ascii "uXor_param_i64_u16"
	.space 1
	.ascii "uXor_var_i64_u16"
	.space 1
	.ascii "uXor_param_I_u32"
	.space 1
	.ascii "uXor_var_I_u32"
	.space 1
	.ascii "uXor_param_I_L"
	.space 1
	.ascii "uXor_var_I_L"
	.space 1
	.ascii "uXor_param_I_i8"
	.space 1
	.ascii "uXor_var_I_i8"
	.space 1
	.ascii "uXor_param_I_u8"
	.space 1
	.ascii "uXor_var_I_u8"
	.space 1
	.ascii "uXor_param_I_C"
	.space 1
	.ascii "uXor_var_I_C"
	.space 1
	.ascii "uXor_param_I_i64"
	.space 1
	.ascii "uXor_var_I_i64"
	.space 1
	.ascii "uXor_param_I_I"
	.space 1
	.ascii "uXor_var_I_I"
	.space 1
	.ascii "uXor_param_I_i32"
	.space 1
	.ascii "uXor_var_I_i32"
	.space 1
	.ascii "uXor_param_I_i16"
	.space 1
	.ascii "uXor_var_I_i16"
	.space 1
	.ascii "uXor_param_I_LC"
	.space 1
	.ascii "uXor_var_I_LC"
	.space 1
	.ascii "uXor_param_I_u64"
	.space 1
	.ascii "uXor_var_I_u64"
	.space 1
	.ascii "uXor_param_I_u16"
	.space 1
	.ascii "uXor_var_I_u16"
	.space 1
	.ascii "uXor_param_i32_u32"
	.space 1
	.ascii "uXor_var_i32_u32"
	.space 1
	.ascii "uXor_param_i32_L"
	.space 1
	.ascii "uXor_var_i32_L"
	.space 1
	.ascii "uXor_param_i32_i8"
	.space 1
	.ascii "uXor_var_i32_i8"
	.space 1
	.ascii "uXor_param_i32_u8"
	.space 1
	.ascii "uXor_var_i32_u8"
	.space 1
	.ascii "uXor_param_i32_C"
	.space 1
	.ascii "uXor_var_i32_C"
	.space 1
	.ascii "uXor_param_i32_i64"
	.space 1
	.ascii "uXor_var_i32_i64"
	.space 1
	.ascii "uXor_param_i32_I"
	.space 1
	.ascii "uXor_var_i32_I"
	.space 1
	.ascii "uXor_param_i32_i32"
	.space 1
	.ascii "uXor_var_i32_i32"
	.space 1
	.ascii "uXor_param_i32_i16"
	.space 1
	.ascii "uXor_var_i32_i16"
	.space 1
	.ascii "uXor_param_i32_LC"
	.space 1
	.ascii "uXor_var_i32_LC"
	.space 1
	.ascii "uXor_param_i32_u64"
	.space 1
	.ascii "uXor_var_i32_u64"
	.space 1
	.ascii "uXor_param_i32_u16"
	.space 1
	.ascii "uXor_var_i32_u16"
	.space 1
	.ascii "uXor_param_i16_u32"
	.space 1
	.ascii "uXor_var_i16_u32"
	.space 1
	.ascii "uXor_param_i16_L"
	.space 1
	.ascii "uXor_var_i16_L"
	.space 1
	.ascii "uXor_param_i16_i8"
	.space 1
	.ascii "uXor_var_i16_i8"
	.space 1
	.ascii "uXor_param_i16_u8"
	.space 1
	.ascii "uXor_var_i16_u8"
	.space 1
	.ascii "uXor_param_i16_C"
	.space 1
	.ascii "uXor_var_i16_C"
	.space 1
	.ascii "uXor_param_i16_i64"
	.space 1
	.ascii "uXor_var_i16_i64"
	.space 1
	.ascii "uXor_param_i16_I"
	.space 1
	.ascii "uXor_var_i16_I"
	.space 1
	.ascii "uXor_param_i16_i32"
	.space 1
	.ascii "uXor_var_i16_i32"
	.space 1
	.ascii "uXor_param_i16_i16"
	.space 1
	.ascii "uXor_var_i16_i16"
	.space 1
	.ascii "uXor_param_i16_LC"
	.space 1
	.ascii "uXor_var_i16_LC"
	.space 1
	.ascii "uXor_param_i16_u64"
	.space 1
	.ascii "uXor_var_i16_u64"
	.space 1
	.ascii "uXor_param_i16_u16"
	.space 1
	.ascii "uXor_var_i16_u16"
	.space 1
	.ascii "uXor_param_LC_u32"
	.space 1
	.ascii "uXor_var_LC_u32"
	.space 1
	.ascii "uXor_param_LC_L"
	.space 1
	.ascii "uXor_var_LC_L"
	.space 1
	.ascii "uXor_param_LC_i8"
	.space 1
	.ascii "uXor_var_LC_i8"
	.space 1
	.ascii "uXor_param_LC_u8"
	.space 1
	.ascii "uXor_var_LC_u8"
	.space 1
	.ascii "uXor_param_LC_C"
	.space 1
	.ascii "uXor_var_LC_C"
	.space 1
	.ascii "uXor_param_LC_i64"
	.space 1
	.ascii "uXor_var_LC_i64"
	.space 1
	.ascii "uXor_param_LC_I"
	.space 1
	.ascii "uXor_var_LC_I"
	.space 1
	.ascii "uXor_param_LC_i32"
	.space 1
	.ascii "uXor_var_LC_i32"
	.space 1
	.ascii "uXor_param_LC_i16"
	.space 1
	.ascii "uXor_var_LC_i16"
	.space 1
	.ascii "uXor_param_LC_LC"
	.space 1
	.ascii "uXor_var_LC_LC"
	.space 1
	.ascii "uXor_param_LC_u64"
	.space 1
	.ascii "uXor_var_LC_u64"
	.space 1
	.ascii "uXor_param_LC_u16"
	.space 1
	.ascii "uXor_var_LC_u16"
	.space 1
	.ascii "uXor_param_u64_u32"
	.space 1
	.ascii "uXor_var_u64_u32"
	.space 1
	.ascii "uXor_param_u64_L"
	.space 1
	.ascii "uXor_var_u64_L"
	.space 1
	.ascii "uXor_param_u64_i8"
	.space 1
	.ascii "uXor_var_u64_i8"
	.space 1
	.ascii "uXor_param_u64_u8"
	.space 1
	.ascii "uXor_var_u64_u8"
	.space 1
	.ascii "uXor_param_u64_C"
	.space 1
	.ascii "uXor_var_u64_C"
	.space 1
	.ascii "uXor_param_u64_i64"
	.space 1
	.ascii "uXor_var_u64_i64"
	.space 1
	.ascii "uXor_param_u64_I"
	.space 1
	.ascii "uXor_var_u64_I"
	.space 1
	.ascii "uXor_param_u64_i32"
	.space 1
	.ascii "uXor_var_u64_i32"
	.space 1
	.ascii "uXor_param_u64_i16"
	.space 1
	.ascii "uXor_var_u64_i16"
	.space 1
	.ascii "uXor_param_u64_LC"
	.space 1
	.ascii "uXor_var_u64_LC"
	.space 1
	.ascii "uXor_param_u64_u64"
	.space 1
	.ascii "uXor_var_u64_u64"
	.space 1
	.ascii "uXor_param_u64_u16"
	.space 1
	.ascii "uXor_var_u64_u16"
	.space 1
	.ascii "uXor_param_u16_u32"
	.space 1
	.ascii "uXor_var_u16_u32"
	.space 1
	.ascii "uXor_param_u16_L"
	.space 1
	.ascii "uXor_var_u16_L"
	.space 1
	.ascii "uXor_param_u16_i8"
	.space 1
	.ascii "uXor_var_u16_i8"
	.space 1
	.ascii "uXor_param_u16_u8"
	.space 1
	.ascii "uXor_var_u16_u8"
	.space 1
	.ascii "uXor_param_u16_C"
	.space 1
	.ascii "uXor_var_u16_C"
	.space 1
	.ascii "uXor_param_u16_i64"
	.space 1
	.ascii "uXor_var_u16_i64"
	.space 1
	.ascii "uXor_param_u16_I"
	.space 1
	.ascii "uXor_var_u16_I"
	.space 1
	.ascii "uXor_param_u16_i32"
	.space 1
	.ascii "uXor_var_u16_i32"
	.space 1
	.ascii "uXor_param_u16_i16"
	.space 1
	.ascii "uXor_var_u16_i16"
	.space 1
	.ascii "uXor_param_u16_LC"
	.space 1
	.ascii "uXor_var_u16_LC"
	.space 1
	.ascii "uXor_param_u16_u64"
	.space 1
	.ascii "uXor_var_u16_u64"
	.space 1
	.ascii "uXor_param_u16_u16"
	.space 1
	.ascii "uXor_var_u16_u16"
	.space 2
	.quad	_Xor_M3
	.quad	_L_1
	.quad	_Xor__uXor_param_u32_u32
	.quad	_L_1+7
	.quad	_Xor__uXor_var_u32_u32
	.quad	_L_1+26
	.quad	_Xor__uXor_param_u32_L
	.quad	_L_1+43
	.quad	_Xor__uXor_var_u32_L
	.quad	_L_1+60
	.quad	_Xor__uXor_param_u32_i8
	.quad	_L_1+75
	.quad	_Xor__uXor_var_u32_i8
	.quad	_L_1+93
	.quad	_Xor__uXor_param_u32_u8
	.quad	_L_1+109
	.quad	_Xor__uXor_var_u32_u8
	.quad	_L_1+127
	.quad	_Xor__uXor_param_u32_C
	.quad	_L_1+143
	.quad	_Xor__uXor_var_u32_C
	.quad	_L_1+160
	.quad	_Xor__uXor_param_u32_i64
	.quad	_L_1+175
	.quad	_Xor__uXor_var_u32_i64
	.quad	_L_1+194
	.quad	_Xor__uXor_param_u32_I
	.quad	_L_1+211
	.quad	_Xor__uXor_var_u32_I
	.quad	_L_1+228
	.quad	_Xor__uXor_param_u32_i32
	.quad	_L_1+243
	.quad	_Xor__uXor_var_u32_i32
	.quad	_L_1+262
	.quad	_Xor__uXor_param_u32_i16
	.quad	_L_1+279
	.quad	_Xor__uXor_var_u32_i16
	.quad	_L_1+298
	.quad	_Xor__uXor_param_u32_LC
	.quad	_L_1+315
	.quad	_Xor__uXor_var_u32_LC
	.quad	_L_1+333
	.quad	_Xor__uXor_param_u32_u64
	.quad	_L_1+349
	.quad	_Xor__uXor_var_u32_u64
	.quad	_L_1+368
	.quad	_Xor__uXor_param_u32_u16
	.quad	_L_1+385
	.quad	_Xor__uXor_var_u32_u16
	.quad	_L_1+404
	.quad	_Xor__uXor_param_L_u32
	.quad	_L_1+421
	.quad	_Xor__uXor_var_L_u32
	.quad	_L_1+438
	.quad	_Xor__uXor_param_L_L
	.quad	_L_1+453
	.quad	_Xor__uXor_var_L_L
	.quad	_L_1+468
	.quad	_Xor__uXor_param_L_i8
	.quad	_L_1+481
	.quad	_Xor__uXor_var_L_i8
	.quad	_L_1+497
	.quad	_Xor__uXor_param_L_u8
	.quad	_L_1+511
	.quad	_Xor__uXor_var_L_u8
	.quad	_L_1+527
	.quad	_Xor__uXor_param_L_C
	.quad	_L_1+541
	.quad	_Xor__uXor_var_L_C
	.quad	_L_1+556
	.quad	_Xor__uXor_param_L_i64
	.quad	_L_1+569
	.quad	_Xor__uXor_var_L_i64
	.quad	_L_1+586
	.quad	_Xor__uXor_param_L_I
	.quad	_L_1+601
	.quad	_Xor__uXor_var_L_I
	.quad	_L_1+616
	.quad	_Xor__uXor_param_L_i32
	.quad	_L_1+629
	.quad	_Xor__uXor_var_L_i32
	.quad	_L_1+646
	.quad	_Xor__uXor_param_L_i16
	.quad	_L_1+661
	.quad	_Xor__uXor_var_L_i16
	.quad	_L_1+678
	.quad	_Xor__uXor_param_L_LC
	.quad	_L_1+693
	.quad	_Xor__uXor_var_L_LC
	.quad	_L_1+709
	.quad	_Xor__uXor_param_L_u64
	.quad	_L_1+723
	.quad	_Xor__uXor_var_L_u64
	.quad	_L_1+740
	.quad	_Xor__uXor_param_L_u16
	.quad	_L_1+755
	.quad	_Xor__uXor_var_L_u16
	.quad	_L_1+772
	.quad	_Xor__uXor_param_i8_u32
	.quad	_L_1+787
	.quad	_Xor__uXor_var_i8_u32
	.quad	_L_1+805
	.quad	_Xor__uXor_param_i8_L
	.quad	_L_1+821
	.quad	_Xor__uXor_var_i8_L
	.quad	_L_1+837
	.quad	_Xor__uXor_param_i8_i8
	.quad	_L_1+851
	.quad	_Xor__uXor_var_i8_i8
	.quad	_L_1+868
	.quad	_Xor__uXor_param_i8_u8
	.quad	_L_1+883
	.quad	_Xor__uXor_var_i8_u8
	.quad	_L_1+900
	.quad	_Xor__uXor_param_i8_C
	.quad	_L_1+915
	.quad	_Xor__uXor_var_i8_C
	.quad	_L_1+931
	.quad	_Xor__uXor_param_i8_i64
	.quad	_L_1+945
	.quad	_Xor__uXor_var_i8_i64
	.quad	_L_1+963
	.quad	_Xor__uXor_param_i8_I
	.quad	_L_1+979
	.quad	_Xor__uXor_var_i8_I
	.quad	_L_1+995
	.quad	_Xor__uXor_param_i8_i32
	.quad	_L_1+1009
	.quad	_Xor__uXor_var_i8_i32
	.quad	_L_1+1027
	.quad	_Xor__uXor_param_i8_i16
	.quad	_L_1+1043
	.quad	_Xor__uXor_var_i8_i16
	.quad	_L_1+1061
	.quad	_Xor__uXor_param_i8_LC
	.quad	_L_1+1077
	.quad	_Xor__uXor_var_i8_LC
	.quad	_L_1+1094
	.quad	_Xor__uXor_param_i8_u64
	.quad	_L_1+1109
	.quad	_Xor__uXor_var_i8_u64
	.quad	_L_1+1127
	.quad	_Xor__uXor_param_i8_u16
	.quad	_L_1+1143
	.quad	_Xor__uXor_var_i8_u16
	.quad	_L_1+1161
	.quad	_Xor__uXor_param_u8_u32
	.quad	_L_1+1177
	.quad	_Xor__uXor_var_u8_u32
	.quad	_L_1+1195
	.quad	_Xor__uXor_param_u8_L
	.quad	_L_1+1211
	.quad	_Xor__uXor_var_u8_L
	.quad	_L_1+1227
	.quad	_Xor__uXor_param_u8_i8
	.quad	_L_1+1241
	.quad	_Xor__uXor_var_u8_i8
	.quad	_L_1+1258
	.quad	_Xor__uXor_param_u8_u8
	.quad	_L_1+1273
	.quad	_Xor__uXor_var_u8_u8
	.quad	_L_1+1290
	.quad	_Xor__uXor_param_u8_C
	.quad	_L_1+1305
	.quad	_Xor__uXor_var_u8_C
	.quad	_L_1+1321
	.quad	_Xor__uXor_param_u8_i64
	.quad	_L_1+1335
	.quad	_Xor__uXor_var_u8_i64
	.quad	_L_1+1353
	.quad	_Xor__uXor_param_u8_I
	.quad	_L_1+1369
	.quad	_Xor__uXor_var_u8_I
	.quad	_L_1+1385
	.quad	_Xor__uXor_param_u8_i32
	.quad	_L_1+1399
	.quad	_Xor__uXor_var_u8_i32
	.quad	_L_1+1417
	.quad	_Xor__uXor_param_u8_i16
	.quad	_L_1+1433
	.quad	_Xor__uXor_var_u8_i16
	.quad	_L_1+1451
	.quad	_Xor__uXor_param_u8_LC
	.quad	_L_1+1467
	.quad	_Xor__uXor_var_u8_LC
	.quad	_L_1+1484
	.quad	_Xor__uXor_param_u8_u64
	.quad	_L_1+1499
	.quad	_Xor__uXor_var_u8_u64
	.quad	_L_1+1517
	.quad	_Xor__uXor_param_u8_u16
	.quad	_L_1+1533
	.quad	_Xor__uXor_var_u8_u16
	.quad	_L_1+1551
	.quad	_Xor__uXor_param_C_u32
	.quad	_L_1+1567
	.quad	_Xor__uXor_var_C_u32
	.quad	_L_1+1584
	.quad	_Xor__uXor_param_C_L
	.quad	_L_1+1599
	.quad	_Xor__uXor_var_C_L
	.quad	_L_1+1614
	.quad	_Xor__uXor_param_C_i8
	.quad	_L_1+1627
	.quad	_Xor__uXor_var_C_i8
	.quad	_L_1+1643
	.quad	_Xor__uXor_param_C_u8
	.quad	_L_1+1657
	.quad	_Xor__uXor_var_C_u8
	.quad	_L_1+1673
	.quad	_Xor__uXor_param_C_C
	.quad	_L_1+1687
	.quad	_Xor__uXor_var_C_C
	.quad	_L_1+1702
	.quad	_Xor__uXor_param_C_i64
	.quad	_L_1+1715
	.quad	_Xor__uXor_var_C_i64
	.quad	_L_1+1732
	.quad	_Xor__uXor_param_C_I
	.quad	_L_1+1747
	.quad	_Xor__uXor_var_C_I
	.quad	_L_1+1762
	.quad	_Xor__uXor_param_C_i32
	.quad	_L_1+1775
	.quad	_Xor__uXor_var_C_i32
	.quad	_L_1+1792
	.quad	_Xor__uXor_param_C_i16
	.quad	_L_1+1807
	.quad	_Xor__uXor_var_C_i16
	.quad	_L_1+1824
	.quad	_Xor__uXor_param_C_LC
	.quad	_L_1+1839
	.quad	_Xor__uXor_var_C_LC
	.quad	_L_1+1855
	.quad	_Xor__uXor_param_C_u64
	.quad	_L_1+1869
	.quad	_Xor__uXor_var_C_u64
	.quad	_L_1+1886
	.quad	_Xor__uXor_param_C_u16
	.quad	_L_1+1901
	.quad	_Xor__uXor_var_C_u16
	.quad	_L_1+1918
	.quad	_Xor__uXor_param_i64_u32
	.quad	_L_1+1933
	.quad	_Xor__uXor_var_i64_u32
	.quad	_L_1+1952
	.quad	_Xor__uXor_param_i64_L
	.quad	_L_1+1969
	.quad	_Xor__uXor_var_i64_L
	.quad	_L_1+1986
	.quad	_Xor__uXor_param_i64_i8
	.quad	_L_1+2001
	.quad	_Xor__uXor_var_i64_i8
	.quad	_L_1+2019
	.quad	_Xor__uXor_param_i64_u8
	.quad	_L_1+2035
	.quad	_Xor__uXor_var_i64_u8
	.quad	_L_1+2053
	.quad	_Xor__uXor_param_i64_C
	.quad	_L_1+2069
	.quad	_Xor__uXor_var_i64_C
	.quad	_L_1+2086
	.quad	_Xor__uXor_param_i64_i64
	.quad	_L_1+2101
	.quad	_Xor__uXor_var_i64_i64
	.quad	_L_1+2120
	.quad	_Xor__uXor_param_i64_I
	.quad	_L_1+2137
	.quad	_Xor__uXor_var_i64_I
	.quad	_L_1+2154
	.quad	_Xor__uXor_param_i64_i32
	.quad	_L_1+2169
	.quad	_Xor__uXor_var_i64_i32
	.quad	_L_1+2188
	.quad	_Xor__uXor_param_i64_i16
	.quad	_L_1+2205
	.quad	_Xor__uXor_var_i64_i16
	.quad	_L_1+2224
	.quad	_Xor__uXor_param_i64_LC
	.quad	_L_1+2241
	.quad	_Xor__uXor_var_i64_LC
	.quad	_L_1+2259
	.quad	_Xor__uXor_param_i64_u64
	.quad	_L_1+2275
	.quad	_Xor__uXor_var_i64_u64
	.quad	_L_1+2294
	.quad	_Xor__uXor_param_i64_u16
	.quad	_L_1+2311
	.quad	_Xor__uXor_var_i64_u16
	.quad	_L_1+2330
	.quad	_Xor__uXor_param_I_u32
	.quad	_L_1+2347
	.quad	_Xor__uXor_var_I_u32
	.quad	_L_1+2364
	.quad	_Xor__uXor_param_I_L
	.quad	_L_1+2379
	.quad	_Xor__uXor_var_I_L
	.quad	_L_1+2394
	.quad	_Xor__uXor_param_I_i8
	.quad	_L_1+2407
	.quad	_Xor__uXor_var_I_i8
	.quad	_L_1+2423
	.quad	_Xor__uXor_param_I_u8
	.quad	_L_1+2437
	.quad	_Xor__uXor_var_I_u8
	.quad	_L_1+2453
	.quad	_Xor__uXor_param_I_C
	.quad	_L_1+2467
	.quad	_Xor__uXor_var_I_C
	.quad	_L_1+2482
	.quad	_Xor__uXor_param_I_i64
	.quad	_L_1+2495
	.quad	_Xor__uXor_var_I_i64
	.quad	_L_1+2512
	.quad	_Xor__uXor_param_I_I
	.quad	_L_1+2527
	.quad	_Xor__uXor_var_I_I
	.quad	_L_1+2542
	.quad	_Xor__uXor_param_I_i32
	.quad	_L_1+2555
	.quad	_Xor__uXor_var_I_i32
	.quad	_L_1+2572
	.quad	_Xor__uXor_param_I_i16
	.quad	_L_1+2587
	.quad	_Xor__uXor_var_I_i16
	.quad	_L_1+2604
	.quad	_Xor__uXor_param_I_LC
	.quad	_L_1+2619
	.quad	_Xor__uXor_var_I_LC
	.quad	_L_1+2635
	.quad	_Xor__uXor_param_I_u64
	.quad	_L_1+2649
	.quad	_Xor__uXor_var_I_u64
	.quad	_L_1+2666
	.quad	_Xor__uXor_param_I_u16
	.quad	_L_1+2681
	.quad	_Xor__uXor_var_I_u16
	.quad	_L_1+2698
	.quad	_Xor__uXor_param_i32_u32
	.quad	_L_1+2713
	.quad	_Xor__uXor_var_i32_u32
	.quad	_L_1+2732
	.quad	_Xor__uXor_param_i32_L
	.quad	_L_1+2749
	.quad	_Xor__uXor_var_i32_L
	.quad	_L_1+2766
	.quad	_Xor__uXor_param_i32_i8
	.quad	_L_1+2781
	.quad	_Xor__uXor_var_i32_i8
	.quad	_L_1+2799
	.quad	_Xor__uXor_param_i32_u8
	.quad	_L_1+2815
	.quad	_Xor__uXor_var_i32_u8
	.quad	_L_1+2833
	.quad	_Xor__uXor_param_i32_C
	.quad	_L_1+2849
	.quad	_Xor__uXor_var_i32_C
	.quad	_L_1+2866
	.quad	_Xor__uXor_param_i32_i64
	.quad	_L_1+2881
	.quad	_Xor__uXor_var_i32_i64
	.quad	_L_1+2900
	.quad	_Xor__uXor_param_i32_I
	.quad	_L_1+2917
	.quad	_Xor__uXor_var_i32_I
	.quad	_L_1+2934
	.quad	_Xor__uXor_param_i32_i32
	.quad	_L_1+2949
	.quad	_Xor__uXor_var_i32_i32
	.quad	_L_1+2968
	.quad	_Xor__uXor_param_i32_i16
	.quad	_L_1+2985
	.quad	_Xor__uXor_var_i32_i16
	.quad	_L_1+3004
	.quad	_Xor__uXor_param_i32_LC
	.quad	_L_1+3021
	.quad	_Xor__uXor_var_i32_LC
	.quad	_L_1+3039
	.quad	_Xor__uXor_param_i32_u64
	.quad	_L_1+3055
	.quad	_Xor__uXor_var_i32_u64
	.quad	_L_1+3074
	.quad	_Xor__uXor_param_i32_u16
	.quad	_L_1+3091
	.quad	_Xor__uXor_var_i32_u16
	.quad	_L_1+3110
	.quad	_Xor__uXor_param_i16_u32
	.quad	_L_1+3127
	.quad	_Xor__uXor_var_i16_u32
	.quad	_L_1+3146
	.quad	_Xor__uXor_param_i16_L
	.quad	_L_1+3163
	.quad	_Xor__uXor_var_i16_L
	.quad	_L_1+3180
	.quad	_Xor__uXor_param_i16_i8
	.quad	_L_1+3195
	.quad	_Xor__uXor_var_i16_i8
	.quad	_L_1+3213
	.quad	_Xor__uXor_param_i16_u8
	.quad	_L_1+3229
	.quad	_Xor__uXor_var_i16_u8
	.quad	_L_1+3247
	.quad	_Xor__uXor_param_i16_C
	.quad	_L_1+3263
	.quad	_Xor__uXor_var_i16_C
	.quad	_L_1+3280
	.quad	_Xor__uXor_param_i16_i64
	.quad	_L_1+3295
	.quad	_Xor__uXor_var_i16_i64
	.quad	_L_1+3314
	.quad	_Xor__uXor_param_i16_I
	.quad	_L_1+3331
	.quad	_Xor__uXor_var_i16_I
	.quad	_L_1+3348
	.quad	_Xor__uXor_param_i16_i32
	.quad	_L_1+3363
	.quad	_Xor__uXor_var_i16_i32
	.quad	_L_1+3382
	.quad	_Xor__uXor_param_i16_i16
	.quad	_L_1+3399
	.quad	_Xor__uXor_var_i16_i16
	.quad	_L_1+3418
	.quad	_Xor__uXor_param_i16_LC
	.quad	_L_1+3435
	.quad	_Xor__uXor_var_i16_LC
	.quad	_L_1+3453
	.quad	_Xor__uXor_param_i16_u64
	.quad	_L_1+3469
	.quad	_Xor__uXor_var_i16_u64
	.quad	_L_1+3488
	.quad	_Xor__uXor_param_i16_u16
	.quad	_L_1+3505
	.quad	_Xor__uXor_var_i16_u16
	.quad	_L_1+3524
	.quad	_Xor__uXor_param_LC_u32
	.quad	_L_1+3541
	.quad	_Xor__uXor_var_LC_u32
	.quad	_L_1+3559
	.quad	_Xor__uXor_param_LC_L
	.quad	_L_1+3575
	.quad	_Xor__uXor_var_LC_L
	.quad	_L_1+3591
	.quad	_Xor__uXor_param_LC_i8
	.quad	_L_1+3605
	.quad	_Xor__uXor_var_LC_i8
	.quad	_L_1+3622
	.quad	_Xor__uXor_param_LC_u8
	.quad	_L_1+3637
	.quad	_Xor__uXor_var_LC_u8
	.quad	_L_1+3654
	.quad	_Xor__uXor_param_LC_C
	.quad	_L_1+3669
	.quad	_Xor__uXor_var_LC_C
	.quad	_L_1+3685
	.quad	_Xor__uXor_param_LC_i64
	.quad	_L_1+3699
	.quad	_Xor__uXor_var_LC_i64
	.quad	_L_1+3717
	.quad	_Xor__uXor_param_LC_I
	.quad	_L_1+3733
	.quad	_Xor__uXor_var_LC_I
	.quad	_L_1+3749
	.quad	_Xor__uXor_param_LC_i32
	.quad	_L_1+3763
	.quad	_Xor__uXor_var_LC_i32
	.quad	_L_1+3781
	.quad	_Xor__uXor_param_LC_i16
	.quad	_L_1+3797
	.quad	_Xor__uXor_var_LC_i16
	.quad	_L_1+3815
	.quad	_Xor__uXor_param_LC_LC
	.quad	_L_1+3831
	.quad	_Xor__uXor_var_LC_LC
	.quad	_L_1+3848
	.quad	_Xor__uXor_param_LC_u64
	.quad	_L_1+3863
	.quad	_Xor__uXor_var_LC_u64
	.quad	_L_1+3881
	.quad	_Xor__uXor_param_LC_u16
	.quad	_L_1+3897
	.quad	_Xor__uXor_var_LC_u16
	.quad	_L_1+3915
	.quad	_Xor__uXor_param_u64_u32
	.quad	_L_1+3931
	.quad	_Xor__uXor_var_u64_u32
	.quad	_L_1+3950
	.quad	_Xor__uXor_param_u64_L
	.quad	_L_1+3967
	.quad	_Xor__uXor_var_u64_L
	.quad	_L_1+3984
	.quad	_Xor__uXor_param_u64_i8
	.quad	_L_1+3999
	.quad	_Xor__uXor_var_u64_i8
	.quad	_L_1+4017
	.quad	_Xor__uXor_param_u64_u8
	.quad	_L_1+4033
	.quad	_Xor__uXor_var_u64_u8
	.quad	_L_1+4051
	.quad	_Xor__uXor_param_u64_C
	.quad	_L_1+4067
	.quad	_Xor__uXor_var_u64_C
	.quad	_L_1+4084
	.quad	_Xor__uXor_param_u64_i64
	.quad	_L_1+4099
	.quad	_Xor__uXor_var_u64_i64
	.quad	_L_1+4118
	.quad	_Xor__uXor_param_u64_I
	.quad	_L_1+4135
	.quad	_Xor__uXor_var_u64_I
	.quad	_L_1+4152
	.quad	_Xor__uXor_param_u64_i32
	.quad	_L_1+4167
	.quad	_Xor__uXor_var_u64_i32
	.quad	_L_1+4186
	.quad	_Xor__uXor_param_u64_i16
	.quad	_L_1+4203
	.quad	_Xor__uXor_var_u64_i16
	.quad	_L_1+4222
	.quad	_Xor__uXor_param_u64_LC
	.quad	_L_1+4239
	.quad	_Xor__uXor_var_u64_LC
	.quad	_L_1+4257
	.quad	_Xor__uXor_param_u64_u64
	.quad	_L_1+4273
	.quad	_Xor__uXor_var_u64_u64
	.quad	_L_1+4292
	.quad	_Xor__uXor_param_u64_u16
	.quad	_L_1+4309
	.quad	_Xor__uXor_var_u64_u16
	.quad	_L_1+4328
	.quad	_Xor__uXor_param_u16_u32
	.quad	_L_1+4345
	.quad	_Xor__uXor_var_u16_u32
	.quad	_L_1+4364
	.quad	_Xor__uXor_param_u16_L
	.quad	_L_1+4381
	.quad	_Xor__uXor_var_u16_L
	.quad	_L_1+4398
	.quad	_Xor__uXor_param_u16_i8
	.quad	_L_1+4413
	.quad	_Xor__uXor_var_u16_i8
	.quad	_L_1+4431
	.quad	_Xor__uXor_param_u16_u8
	.quad	_L_1+4447
	.quad	_Xor__uXor_var_u16_u8
	.quad	_L_1+4465
	.quad	_Xor__uXor_param_u16_C
	.quad	_L_1+4481
	.quad	_Xor__uXor_var_u16_C
	.quad	_L_1+4498
	.quad	_Xor__uXor_param_u16_i64
	.quad	_L_1+4513
	.quad	_Xor__uXor_var_u16_i64
	.quad	_L_1+4532
	.quad	_Xor__uXor_param_u16_I
	.quad	_L_1+4549
	.quad	_Xor__uXor_var_u16_I
	.quad	_L_1+4566
	.quad	_Xor__uXor_param_u16_i32
	.quad	_L_1+4581
	.quad	_Xor__uXor_var_u16_i32
	.quad	_L_1+4600
	.quad	_Xor__uXor_param_u16_i16
	.quad	_L_1+4617
	.quad	_Xor__uXor_var_u16_i16
	.quad	_L_1+4636
	.quad	_Xor__uXor_param_u16_LC
	.quad	_L_1+4653
	.quad	_Xor__uXor_var_u16_LC
	.quad	_L_1+4671
	.quad	_Xor__uXor_param_u16_u64
	.quad	_L_1+4687
	.quad	_Xor__uXor_var_u16_u64
	.quad	_L_1+4706
	.quad	_Xor__uXor_param_u16_u16
	.quad	_L_1+4723
	.quad	_Xor__uXor_var_u16_u16
	.quad	_L_1+4742
	.space 8
	.ascii "../AMD64_DARWIN/Xor.m3"
	.space 2
	.data
	.align 5
_MM_Xor:
	.quad	_L_1+9392
	.space 32
	.quad	_L_1+4760
	.space 24
	.quad	_MM_Xor+208
	.space 8
	.quad	_Xor_M3
	.quad	3
	.word	497
	.space 6
	.quad	498
	.quad	499
	.long	412316860
	.long	1082083332
	.word	502
	.space 2
	.long	503
	.quad	504
	.quad	505
	.quad	506
	.long	1140703494
	.byte	-3
	.byte	-2
	.space 2
	.quad	511
	.long	512
	.space 28
	.quad	_Xor_I3
	.quad	_MM_Xor+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Xor+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Xor+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Xor+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
