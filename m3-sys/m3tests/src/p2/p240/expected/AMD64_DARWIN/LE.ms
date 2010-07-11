	.text
.globl _LE__uLE_var_u16_u16
	.private_extern _LE__uLE_var_u16_u16
_LE__uLE_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_u16_u16
	.private_extern _LE__LE_var_u16_u16
_LE__LE_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_u16_u16
	.private_extern _LE__uLE_param_u16_u16
_LE__uLE_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_u16
	.private_extern _LE__LE_param_u16_u16
_LE__LE_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	cmpw	-20(%rbp), %ax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_u64
	.private_extern _LE__uLE_var_u16_u64
_LE__uLE_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_u64
	.private_extern _LE__LE_var_u16_u64
_LE__LE_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_u64
	.private_extern _LE__uLE_param_u16_u64
_LE__uLE_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_u64
	.private_extern _LE__LE_param_u16_u64
_LE__LE_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_LC
	.private_extern _LE__uLE_var_u16_LC
_LE__uLE_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_LC
	.private_extern _LE__LE_var_u16_LC
_LE__LE_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_LC
	.private_extern _LE__uLE_param_u16_LC
_LE__uLE_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_LC
	.private_extern _LE__LE_param_u16_LC
_LE__LE_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_i16
	.private_extern _LE__uLE_var_u16_i16
_LE__uLE_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_i16
	.private_extern _LE__LE_var_u16_i16
_LE__LE_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %edx
	movzwl	136+_MM_LE(%rip), %eax
	cmpw	%ax, %dx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_i16
	.private_extern _LE__uLE_param_u16_i16
_LE__uLE_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_i16
	.private_extern _LE__LE_param_u16_i16
_LE__LE_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	cmpw	-18(%rbp), %ax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_i32
	.private_extern _LE__uLE_var_u16_i32
_LE__uLE_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_i32
	.private_extern _LE__LE_var_u16_i32
_LE__LE_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_i32
	.private_extern _LE__uLE_param_u16_i32
_LE__uLE_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_i32
	.private_extern _LE__LE_param_u16_i32
_LE__LE_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_I
	.private_extern _LE__uLE_var_u16_I
_LE__uLE_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_I
	.private_extern _LE__LE_var_u16_I
_LE__LE_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_I
	.private_extern _LE__uLE_param_u16_I
_LE__uLE_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_I
	.private_extern _LE__LE_param_u16_I
_LE__LE_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_i64
	.private_extern _LE__uLE_var_u16_i64
_LE__uLE_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_i64
	.private_extern _LE__LE_var_u16_i64
_LE__LE_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_i64
	.private_extern _LE__uLE_param_u16_i64
_LE__uLE_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_i64
	.private_extern _LE__LE_param_u16_i64
_LE__LE_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_C
	.private_extern _LE__uLE_var_u16_C
_LE__uLE_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_C
	.private_extern _LE__LE_var_u16_C
_LE__LE_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_C
	.private_extern _LE__uLE_param_u16_C
_LE__uLE_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_C
	.private_extern _LE__LE_param_u16_C
_LE__LE_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_u8
	.private_extern _LE__uLE_var_u16_u8
_LE__uLE_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_u8
	.private_extern _LE__LE_var_u16_u8
_LE__LE_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %edx
	movzbl	172+_MM_LE(%rip), %eax
	cbtw
	cmpw	%ax, %dx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_u8
	.private_extern _LE__uLE_param_u16_u8
_LE__uLE_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_u8
	.private_extern _LE__LE_param_u16_u8
_LE__LE_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movsbw	-19(%rbp),%ax
	cmpw	-18(%rbp), %ax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_i8
	.private_extern _LE__uLE_var_u16_i8
_LE__uLE_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_i8
	.private_extern _LE__LE_var_u16_i8
_LE__LE_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %edx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpw	%ax, %dx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_i8
	.private_extern _LE__uLE_param_u16_i8
_LE__uLE_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_i8
	.private_extern _LE__LE_param_u16_i8
_LE__LE_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzbl	-19(%rbp), %eax
	cmpw	-18(%rbp), %ax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_L
	.private_extern _LE__uLE_var_u16_L
_LE__uLE_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_L
	.private_extern _LE__LE_var_u16_L
_LE__LE_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_L
	.private_extern _LE__uLE_param_u16_L
_LE__uLE_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_L
	.private_extern _LE__LE_param_u16_L
_LE__LE_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u16_u32
	.private_extern _LE__uLE_var_u16_u32
_LE__uLE_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u16_u32
	.private_extern _LE__LE_var_u16_u32
_LE__LE_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u16_u32
	.private_extern _LE__uLE_param_u16_u32
_LE__uLE_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u16_u32
	.private_extern _LE__LE_param_u16_u32
_LE__LE_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_u16
	.private_extern _LE__uLE_var_u64_u16
_LE__uLE_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_u16
	.private_extern _LE__LE_var_u64_u16
_LE__LE_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_u16
	.private_extern _LE__uLE_param_u64_u16
_LE__uLE_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_u16
	.private_extern _LE__LE_param_u64_u16
_LE__LE_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_u64
	.private_extern _LE__uLE_var_u64_u64
_LE__uLE_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_u64_u64
	.private_extern _LE__LE_var_u64_u64
_LE__LE_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_u64_u64
	.private_extern _LE__uLE_param_u64_u64
_LE__uLE_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_u64
	.private_extern _LE__LE_param_u64_u64
_LE__LE_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_LC
	.private_extern _LE__uLE_var_u64_LC
_LE__uLE_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_LC
	.private_extern _LE__LE_var_u64_LC
_LE__LE_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_LC
	.private_extern _LE__uLE_param_u64_LC
_LE__uLE_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_LC
	.private_extern _LE__LE_param_u64_LC
_LE__LE_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_i16
	.private_extern _LE__uLE_var_u64_i16
_LE__uLE_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_i16
	.private_extern _LE__LE_var_u64_i16
_LE__LE_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_i16
	.private_extern _LE__uLE_param_u64_i16
_LE__uLE_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_i16
	.private_extern _LE__LE_param_u64_i16
_LE__LE_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_i32
	.private_extern _LE__uLE_var_u64_i32
_LE__uLE_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_i32
	.private_extern _LE__LE_var_u64_i32
_LE__LE_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_i32
	.private_extern _LE__uLE_param_u64_i32
_LE__uLE_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_i32
	.private_extern _LE__LE_param_u64_i32
_LE__LE_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_I
	.private_extern _LE__uLE_var_u64_I
_LE__uLE_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_I
	.private_extern _LE__LE_var_u64_I
_LE__LE_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_I
	.private_extern _LE__uLE_param_u64_I
_LE__uLE_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_I
	.private_extern _LE__LE_param_u64_I
_LE__LE_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_i64
	.private_extern _LE__uLE_var_u64_i64
_LE__uLE_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_i64
	.private_extern _LE__LE_var_u64_i64
_LE__LE_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_i64
	.private_extern _LE__uLE_param_u64_i64
_LE__uLE_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_i64
	.private_extern _LE__LE_param_u64_i64
_LE__LE_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_C
	.private_extern _LE__uLE_var_u64_C
_LE__uLE_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_C
	.private_extern _LE__LE_var_u64_C
_LE__LE_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_C
	.private_extern _LE__uLE_param_u64_C
_LE__uLE_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_C
	.private_extern _LE__LE_param_u64_C
_LE__LE_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_u8
	.private_extern _LE__uLE_var_u64_u8
_LE__uLE_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_u8
	.private_extern _LE__LE_var_u64_u8
_LE__LE_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_u8
	.private_extern _LE__uLE_param_u64_u8
_LE__uLE_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_u8
	.private_extern _LE__LE_param_u64_u8
_LE__LE_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_i8
	.private_extern _LE__uLE_var_u64_i8
_LE__uLE_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_i8
	.private_extern _LE__LE_var_u64_i8
_LE__LE_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_i8
	.private_extern _LE__uLE_param_u64_i8
_LE__uLE_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_i8
	.private_extern _LE__LE_param_u64_i8
_LE__LE_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_L
	.private_extern _LE__uLE_var_u64_L
_LE__uLE_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_L
	.private_extern _LE__LE_var_u64_L
_LE__LE_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_L
	.private_extern _LE__uLE_param_u64_L
_LE__uLE_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_L
	.private_extern _LE__LE_param_u64_L
_LE__LE_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u64_u32
	.private_extern _LE__uLE_var_u64_u32
_LE__uLE_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u64_u32
	.private_extern _LE__LE_var_u64_u32
_LE__LE_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u64_u32
	.private_extern _LE__uLE_param_u64_u32
_LE__uLE_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u64_u32
	.private_extern _LE__LE_param_u64_u32
_LE__LE_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_u16
	.private_extern _LE__uLE_var_LC_u16
_LE__uLE_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_u16
	.private_extern _LE__LE_var_LC_u16
_LE__LE_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_u16
	.private_extern _LE__uLE_param_LC_u16
_LE__uLE_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_u16
	.private_extern _LE__LE_param_LC_u16
_LE__LE_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_u64
	.private_extern _LE__uLE_var_LC_u64
_LE__uLE_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_u64
	.private_extern _LE__LE_var_LC_u64
_LE__LE_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_u64
	.private_extern _LE__uLE_param_LC_u64
_LE__uLE_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_u64
	.private_extern _LE__LE_param_LC_u64
_LE__LE_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_LC
	.private_extern _LE__uLE_var_LC_LC
_LE__uLE_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_LC_LC
	.private_extern _LE__LE_var_LC_LC
_LE__LE_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_LC_LC
	.private_extern _LE__uLE_param_LC_LC
_LE__uLE_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_LC
	.private_extern _LE__LE_param_LC_LC
_LE__LE_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_i16
	.private_extern _LE__uLE_var_LC_i16
_LE__uLE_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_i16
	.private_extern _LE__LE_var_LC_i16
_LE__LE_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_i16
	.private_extern _LE__uLE_param_LC_i16
_LE__uLE_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_i16
	.private_extern _LE__LE_param_LC_i16
_LE__LE_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_i32
	.private_extern _LE__uLE_var_LC_i32
_LE__uLE_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_i32
	.private_extern _LE__LE_var_LC_i32
_LE__LE_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_i32
	.private_extern _LE__uLE_param_LC_i32
_LE__uLE_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_i32
	.private_extern _LE__LE_param_LC_i32
_LE__LE_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_I
	.private_extern _LE__uLE_var_LC_I
_LE__uLE_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_I
	.private_extern _LE__LE_var_LC_I
_LE__LE_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_I
	.private_extern _LE__uLE_param_LC_I
_LE__uLE_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_I
	.private_extern _LE__LE_param_LC_I
_LE__LE_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_i64
	.private_extern _LE__uLE_var_LC_i64
_LE__uLE_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_i64
	.private_extern _LE__LE_var_LC_i64
_LE__LE_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_i64
	.private_extern _LE__uLE_param_LC_i64
_LE__uLE_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_i64
	.private_extern _LE__LE_param_LC_i64
_LE__LE_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_C
	.private_extern _LE__uLE_var_LC_C
_LE__uLE_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_C
	.private_extern _LE__LE_var_LC_C
_LE__LE_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_C
	.private_extern _LE__uLE_param_LC_C
_LE__uLE_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_C
	.private_extern _LE__LE_param_LC_C
_LE__LE_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_u8
	.private_extern _LE__uLE_var_LC_u8
_LE__uLE_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_u8
	.private_extern _LE__LE_var_LC_u8
_LE__LE_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_u8
	.private_extern _LE__uLE_param_LC_u8
_LE__uLE_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_u8
	.private_extern _LE__LE_param_LC_u8
_LE__LE_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_i8
	.private_extern _LE__uLE_var_LC_i8
_LE__uLE_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_i8
	.private_extern _LE__LE_var_LC_i8
_LE__LE_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_i8
	.private_extern _LE__uLE_param_LC_i8
_LE__uLE_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_i8
	.private_extern _LE__LE_param_LC_i8
_LE__LE_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_L
	.private_extern _LE__uLE_var_LC_L
_LE__uLE_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_L
	.private_extern _LE__LE_var_LC_L
_LE__LE_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_L
	.private_extern _LE__uLE_param_LC_L
_LE__uLE_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_L
	.private_extern _LE__LE_param_LC_L
_LE__LE_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_LC_u32
	.private_extern _LE__uLE_var_LC_u32
_LE__uLE_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_LC_u32
	.private_extern _LE__LE_var_LC_u32
_LE__LE_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_LC_u32
	.private_extern _LE__uLE_param_LC_u32
_LE__uLE_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_LC_u32
	.private_extern _LE__LE_param_LC_u32
_LE__LE_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_f64_f64
	.private_extern _LE__LE_var_f64_f64
_LE__LE_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_LE(%rip), %xmm1
	movsd	128+_MM_LE(%rip), %xmm0
	ucomisd	%xmm0, %xmm1
	sete	%al
	setnp	%dl
	andl	%edx, %eax
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_f64_f64
	.private_extern _LE__LE_param_f64_f64
_LE__LE_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	ucomisd	%xmm1, %xmm0
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_u16
	.private_extern _LE__uLE_var_i16_u16
_LE__uLE_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %edx
	movzwl	104+_MM_LE(%rip), %eax
	cmpw	%ax, %dx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_u16
	.private_extern _LE__LE_var_i16_u16
_LE__LE_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_u16
	.private_extern _LE__uLE_param_i16_u16
_LE__uLE_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	cmpw	-18(%rbp), %ax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_u16
	.private_extern _LE__LE_param_i16_u16
_LE__LE_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_u64
	.private_extern _LE__uLE_var_i16_u64
_LE__uLE_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_u64
	.private_extern _LE__LE_var_i16_u64
_LE__LE_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_u64
	.private_extern _LE__uLE_param_i16_u64
_LE__uLE_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_u64
	.private_extern _LE__LE_param_i16_u64
_LE__LE_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_LC
	.private_extern _LE__uLE_var_i16_LC
_LE__uLE_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_LC
	.private_extern _LE__LE_var_i16_LC
_LE__LE_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_LC
	.private_extern _LE__uLE_param_i16_LC
_LE__uLE_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_LC
	.private_extern _LE__LE_param_i16_LC
_LE__LE_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_i16
	.private_extern _LE__uLE_var_i16_i16
_LE__uLE_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_i16_i16
	.private_extern _LE__LE_var_i16_i16
_LE__LE_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_i16_i16
	.private_extern _LE__uLE_param_i16_i16
_LE__uLE_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	cmpw	-20(%rbp), %ax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_i16
	.private_extern _LE__LE_param_i16_i16
_LE__LE_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_i32
	.private_extern _LE__uLE_var_i16_i32
_LE__uLE_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_i32
	.private_extern _LE__LE_var_i16_i32
_LE__LE_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_i32
	.private_extern _LE__uLE_param_i16_i32
_LE__uLE_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_i32
	.private_extern _LE__LE_param_i16_i32
_LE__LE_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_I
	.private_extern _LE__uLE_var_i16_I
_LE__uLE_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_I
	.private_extern _LE__LE_var_i16_I
_LE__LE_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_I
	.private_extern _LE__uLE_param_i16_I
_LE__uLE_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_I
	.private_extern _LE__LE_param_i16_I
_LE__LE_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_i64
	.private_extern _LE__uLE_var_i16_i64
_LE__uLE_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_i64
	.private_extern _LE__LE_var_i16_i64
_LE__LE_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_i64
	.private_extern _LE__uLE_param_i16_i64
_LE__uLE_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_i64
	.private_extern _LE__LE_param_i16_i64
_LE__LE_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_C
	.private_extern _LE__uLE_var_i16_C
_LE__uLE_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_C
	.private_extern _LE__LE_var_i16_C
_LE__LE_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_C
	.private_extern _LE__uLE_param_i16_C
_LE__uLE_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_C
	.private_extern _LE__LE_param_i16_C
_LE__LE_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_u8
	.private_extern _LE__uLE_var_i16_u8
_LE__uLE_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %edx
	movzbl	172+_MM_LE(%rip), %eax
	cbtw
	cmpw	%ax, %dx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_u8
	.private_extern _LE__LE_var_i16_u8
_LE__LE_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_u8
	.private_extern _LE__uLE_param_i16_u8
_LE__uLE_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movsbw	-19(%rbp),%ax
	cmpw	-18(%rbp), %ax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_u8
	.private_extern _LE__LE_param_i16_u8
_LE__LE_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_i8
	.private_extern _LE__uLE_var_i16_i8
_LE__uLE_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %edx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpw	%ax, %dx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_i8
	.private_extern _LE__LE_var_i16_i8
_LE__LE_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_i8
	.private_extern _LE__uLE_param_i16_i8
_LE__uLE_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzbl	-19(%rbp), %eax
	cmpw	-18(%rbp), %ax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_i8
	.private_extern _LE__LE_param_i16_i8
_LE__LE_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_L
	.private_extern _LE__uLE_var_i16_L
_LE__uLE_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_L
	.private_extern _LE__LE_var_i16_L
_LE__LE_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_L
	.private_extern _LE__uLE_param_i16_L
_LE__uLE_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_L
	.private_extern _LE__LE_param_i16_L
_LE__LE_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i16_u32
	.private_extern _LE__uLE_var_i16_u32
_LE__uLE_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i16_u32
	.private_extern _LE__LE_var_i16_u32
_LE__LE_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i16_u32
	.private_extern _LE__uLE_param_i16_u32
_LE__uLE_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i16_u32
	.private_extern _LE__LE_param_i16_u32
_LE__LE_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_u16
	.private_extern _LE__uLE_var_i32_u16
_LE__uLE_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %edx
	movzwl	104+_MM_LE(%rip), %eax
	cwtl
	cmpl	%eax, %edx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_u16
	.private_extern _LE__LE_var_i32_u16
_LE__LE_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_u16
	.private_extern _LE__uLE_param_i32_u16
_LE__uLE_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movswl	-22(%rbp),%eax
	cmpl	-20(%rbp), %eax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_u16
	.private_extern _LE__LE_param_i32_u16
_LE__LE_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_u64
	.private_extern _LE__uLE_var_i32_u64
_LE__uLE_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_u64
	.private_extern _LE__LE_var_i32_u64
_LE__LE_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_u64
	.private_extern _LE__uLE_param_i32_u64
_LE__uLE_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_u64
	.private_extern _LE__LE_param_i32_u64
_LE__LE_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_LC
	.private_extern _LE__uLE_var_i32_LC
_LE__uLE_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_LC
	.private_extern _LE__LE_var_i32_LC
_LE__LE_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_LC
	.private_extern _LE__uLE_param_i32_LC
_LE__uLE_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_LC
	.private_extern _LE__LE_param_i32_LC
_LE__LE_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_i16
	.private_extern _LE__uLE_var_i32_i16
_LE__uLE_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %edx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpl	%eax, %edx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_i16
	.private_extern _LE__LE_var_i32_i16
_LE__LE_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_i16
	.private_extern _LE__uLE_param_i32_i16
_LE__uLE_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movzwl	-22(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_i16
	.private_extern _LE__LE_param_i32_i16
_LE__LE_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_i32
	.private_extern _LE__uLE_var_i32_i32
_LE__uLE_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_i32_i32
	.private_extern _LE__LE_var_i32_i32
_LE__LE_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_i32_i32
	.private_extern _LE__uLE_param_i32_i32
_LE__uLE_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cmpl	-24(%rbp), %eax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_i32
	.private_extern _LE__LE_param_i32_i32
_LE__LE_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_I
	.private_extern _LE__uLE_var_i32_I
_LE__uLE_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_I
	.private_extern _LE__LE_var_i32_I
_LE__LE_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_I
	.private_extern _LE__uLE_param_i32_I
_LE__uLE_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_I
	.private_extern _LE__LE_param_i32_I
_LE__LE_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_i64
	.private_extern _LE__uLE_var_i32_i64
_LE__uLE_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_i64
	.private_extern _LE__LE_var_i32_i64
_LE__LE_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_i64
	.private_extern _LE__uLE_param_i32_i64
_LE__uLE_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_i64
	.private_extern _LE__LE_param_i32_i64
_LE__LE_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_C
	.private_extern _LE__uLE_var_i32_C
_LE__uLE_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_C
	.private_extern _LE__LE_var_i32_C
_LE__LE_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_C
	.private_extern _LE__uLE_param_i32_C
_LE__uLE_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_C
	.private_extern _LE__LE_param_i32_C
_LE__LE_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_u8
	.private_extern _LE__uLE_var_i32_u8
_LE__uLE_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %edx
	movzbl	172+_MM_LE(%rip), %eax
	movsbl	%al,%eax
	cmpl	%eax, %edx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_u8
	.private_extern _LE__LE_var_i32_u8
_LE__LE_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_u8
	.private_extern _LE__uLE_param_i32_u8
_LE__uLE_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movsbl	-21(%rbp),%eax
	cmpl	-20(%rbp), %eax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_u8
	.private_extern _LE__LE_param_i32_u8
_LE__LE_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_i8
	.private_extern _LE__uLE_var_i32_i8
_LE__uLE_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %edx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpl	%eax, %edx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_i8
	.private_extern _LE__LE_var_i32_i8
_LE__LE_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_i8
	.private_extern _LE__uLE_param_i32_i8
_LE__uLE_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movzbl	-21(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_i8
	.private_extern _LE__LE_param_i32_i8
_LE__LE_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_L
	.private_extern _LE__uLE_var_i32_L
_LE__uLE_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_L
	.private_extern _LE__LE_var_i32_L
_LE__LE_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_L
	.private_extern _LE__uLE_param_i32_L
_LE__uLE_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_L
	.private_extern _LE__LE_param_i32_L
_LE__LE_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i32_u32
	.private_extern _LE__uLE_var_i32_u32
_LE__uLE_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %edx
	movl	184+_MM_LE(%rip), %eax
	cmpl	%eax, %edx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i32_u32
	.private_extern _LE__LE_var_i32_u32
_LE__LE_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i32_u32
	.private_extern _LE__uLE_param_i32_u32
_LE__uLE_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i32_u32
	.private_extern _LE__LE_param_i32_u32
_LE__LE_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_u16
	.private_extern _LE__uLE_var_I_u16
_LE__uLE_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_u16
	.private_extern _LE__LE_var_I_u16
_LE__LE_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_u16
	.private_extern _LE__uLE_param_I_u16
_LE__uLE_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_u16
	.private_extern _LE__LE_param_I_u16
_LE__LE_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_u64
	.private_extern _LE__uLE_var_I_u64
_LE__uLE_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_u64
	.private_extern _LE__LE_var_I_u64
_LE__LE_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_u64
	.private_extern _LE__uLE_param_I_u64
_LE__uLE_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_u64
	.private_extern _LE__LE_param_I_u64
_LE__LE_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_LC
	.private_extern _LE__uLE_var_I_LC
_LE__uLE_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_LC
	.private_extern _LE__LE_var_I_LC
_LE__LE_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_LC
	.private_extern _LE__uLE_param_I_LC
_LE__uLE_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_LC
	.private_extern _LE__LE_param_I_LC
_LE__LE_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_i16
	.private_extern _LE__uLE_var_I_i16
_LE__uLE_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_i16
	.private_extern _LE__LE_var_I_i16
_LE__LE_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_i16
	.private_extern _LE__uLE_param_I_i16
_LE__uLE_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_i16
	.private_extern _LE__LE_param_I_i16
_LE__LE_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_i32
	.private_extern _LE__uLE_var_I_i32
_LE__uLE_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_i32
	.private_extern _LE__LE_var_I_i32
_LE__LE_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_i32
	.private_extern _LE__uLE_param_I_i32
_LE__uLE_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_i32
	.private_extern _LE__LE_param_I_i32
_LE__LE_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_I
	.private_extern _LE__uLE_var_I_I
_LE__uLE_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_I_I
	.private_extern _LE__LE_var_I_I
_LE__LE_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_I_I
	.private_extern _LE__uLE_param_I_I
_LE__uLE_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_I
	.private_extern _LE__LE_param_I_I
_LE__LE_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_i64
	.private_extern _LE__uLE_var_I_i64
_LE__uLE_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_i64
	.private_extern _LE__LE_var_I_i64
_LE__LE_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_i64
	.private_extern _LE__uLE_param_I_i64
_LE__uLE_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_i64
	.private_extern _LE__LE_param_I_i64
_LE__LE_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_C
	.private_extern _LE__uLE_var_I_C
_LE__uLE_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_C
	.private_extern _LE__LE_var_I_C
_LE__LE_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_C
	.private_extern _LE__uLE_param_I_C
_LE__uLE_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_C
	.private_extern _LE__LE_param_I_C
_LE__LE_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_u8
	.private_extern _LE__uLE_var_I_u8
_LE__uLE_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_u8
	.private_extern _LE__LE_var_I_u8
_LE__LE_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_u8
	.private_extern _LE__uLE_param_I_u8
_LE__uLE_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_u8
	.private_extern _LE__LE_param_I_u8
_LE__LE_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_i8
	.private_extern _LE__uLE_var_I_i8
_LE__uLE_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_i8
	.private_extern _LE__LE_var_I_i8
_LE__LE_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_i8
	.private_extern _LE__uLE_param_I_i8
_LE__uLE_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_i8
	.private_extern _LE__LE_param_I_i8
_LE__LE_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_L
	.private_extern _LE__uLE_var_I_L
_LE__uLE_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_L
	.private_extern _LE__LE_var_I_L
_LE__LE_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_L
	.private_extern _LE__uLE_param_I_L
_LE__uLE_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_L
	.private_extern _LE__LE_param_I_L
_LE__LE_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_I_u32
	.private_extern _LE__uLE_var_I_u32
_LE__uLE_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_I_u32
	.private_extern _LE__LE_var_I_u32
_LE__LE_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_I_u32
	.private_extern _LE__uLE_param_I_u32
_LE__uLE_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_I_u32
	.private_extern _LE__LE_param_I_u32
_LE__LE_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_u16
	.private_extern _LE__uLE_var_i64_u16
_LE__uLE_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_u16
	.private_extern _LE__LE_var_i64_u16
_LE__LE_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_u16
	.private_extern _LE__uLE_param_i64_u16
_LE__uLE_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_u16
	.private_extern _LE__LE_param_i64_u16
_LE__LE_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_u64
	.private_extern _LE__uLE_var_i64_u64
_LE__uLE_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_u64
	.private_extern _LE__LE_var_i64_u64
_LE__LE_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_u64
	.private_extern _LE__uLE_param_i64_u64
_LE__uLE_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_u64
	.private_extern _LE__LE_param_i64_u64
_LE__LE_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_LC
	.private_extern _LE__uLE_var_i64_LC
_LE__uLE_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_LC
	.private_extern _LE__LE_var_i64_LC
_LE__LE_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_LC
	.private_extern _LE__uLE_param_i64_LC
_LE__uLE_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_LC
	.private_extern _LE__LE_param_i64_LC
_LE__LE_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_i16
	.private_extern _LE__uLE_var_i64_i16
_LE__uLE_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_i16
	.private_extern _LE__LE_var_i64_i16
_LE__LE_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_i16
	.private_extern _LE__uLE_param_i64_i16
_LE__uLE_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_i16
	.private_extern _LE__LE_param_i64_i16
_LE__LE_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_i32
	.private_extern _LE__uLE_var_i64_i32
_LE__uLE_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_i32
	.private_extern _LE__LE_var_i64_i32
_LE__LE_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_i32
	.private_extern _LE__uLE_param_i64_i32
_LE__uLE_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_i32
	.private_extern _LE__LE_param_i64_i32
_LE__LE_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_I
	.private_extern _LE__uLE_var_i64_I
_LE__uLE_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_I
	.private_extern _LE__LE_var_i64_I
_LE__LE_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_I
	.private_extern _LE__uLE_param_i64_I
_LE__uLE_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_I
	.private_extern _LE__LE_param_i64_I
_LE__LE_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_i64
	.private_extern _LE__uLE_var_i64_i64
_LE__uLE_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_i64_i64
	.private_extern _LE__LE_var_i64_i64
_LE__LE_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_i64_i64
	.private_extern _LE__uLE_param_i64_i64
_LE__uLE_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_i64
	.private_extern _LE__LE_param_i64_i64
_LE__LE_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_C
	.private_extern _LE__uLE_var_i64_C
_LE__uLE_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_C
	.private_extern _LE__LE_var_i64_C
_LE__LE_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_C
	.private_extern _LE__uLE_param_i64_C
_LE__uLE_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_C
	.private_extern _LE__LE_param_i64_C
_LE__LE_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_u8
	.private_extern _LE__uLE_var_i64_u8
_LE__uLE_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_u8
	.private_extern _LE__LE_var_i64_u8
_LE__LE_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_u8
	.private_extern _LE__uLE_param_i64_u8
_LE__uLE_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_u8
	.private_extern _LE__LE_param_i64_u8
_LE__LE_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_i8
	.private_extern _LE__uLE_var_i64_i8
_LE__uLE_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_i8
	.private_extern _LE__LE_var_i64_i8
_LE__LE_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_i8
	.private_extern _LE__uLE_param_i64_i8
_LE__uLE_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_i8
	.private_extern _LE__LE_param_i64_i8
_LE__LE_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_L
	.private_extern _LE__uLE_var_i64_L
_LE__uLE_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_L
	.private_extern _LE__LE_var_i64_L
_LE__LE_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_L
	.private_extern _LE__uLE_param_i64_L
_LE__uLE_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_L
	.private_extern _LE__LE_param_i64_L
_LE__LE_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i64_u32
	.private_extern _LE__uLE_var_i64_u32
_LE__uLE_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i64_u32
	.private_extern _LE__LE_var_i64_u32
_LE__LE_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i64_u32
	.private_extern _LE__uLE_param_i64_u32
_LE__uLE_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i64_u32
	.private_extern _LE__LE_param_i64_u32
_LE__LE_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_u16
	.private_extern _LE__uLE_var_C_u16
_LE__uLE_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_u16
	.private_extern _LE__LE_var_C_u16
_LE__LE_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_u16
	.private_extern _LE__uLE_param_C_u16
_LE__uLE_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_u16
	.private_extern _LE__LE_param_C_u16
_LE__LE_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_u64
	.private_extern _LE__uLE_var_C_u64
_LE__uLE_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_u64
	.private_extern _LE__LE_var_C_u64
_LE__LE_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_u64
	.private_extern _LE__uLE_param_C_u64
_LE__uLE_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_u64
	.private_extern _LE__LE_param_C_u64
_LE__LE_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_LC
	.private_extern _LE__uLE_var_C_LC
_LE__uLE_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_LC
	.private_extern _LE__LE_var_C_LC
_LE__LE_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_LC
	.private_extern _LE__uLE_param_C_LC
_LE__uLE_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_LC
	.private_extern _LE__LE_param_C_LC
_LE__LE_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_i16
	.private_extern _LE__uLE_var_C_i16
_LE__uLE_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_i16
	.private_extern _LE__LE_var_C_i16
_LE__LE_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_i16
	.private_extern _LE__uLE_param_C_i16
_LE__uLE_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_i16
	.private_extern _LE__LE_param_C_i16
_LE__LE_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_i32
	.private_extern _LE__uLE_var_C_i32
_LE__uLE_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_i32
	.private_extern _LE__LE_var_C_i32
_LE__LE_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_i32
	.private_extern _LE__uLE_param_C_i32
_LE__uLE_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_i32
	.private_extern _LE__LE_param_C_i32
_LE__LE_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_I
	.private_extern _LE__uLE_var_C_I
_LE__uLE_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_I
	.private_extern _LE__LE_var_C_I
_LE__LE_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_I
	.private_extern _LE__uLE_param_C_I
_LE__uLE_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_I
	.private_extern _LE__LE_param_C_I
_LE__LE_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_i64
	.private_extern _LE__uLE_var_C_i64
_LE__uLE_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_i64
	.private_extern _LE__LE_var_C_i64
_LE__LE_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_i64
	.private_extern _LE__uLE_param_C_i64
_LE__uLE_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_i64
	.private_extern _LE__LE_param_C_i64
_LE__LE_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_C
	.private_extern _LE__uLE_var_C_C
_LE__uLE_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_C_C
	.private_extern _LE__LE_var_C_C
_LE__LE_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_C_C
	.private_extern _LE__uLE_param_C_C
_LE__uLE_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_C
	.private_extern _LE__LE_param_C_C
_LE__LE_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_u8
	.private_extern _LE__uLE_var_C_u8
_LE__uLE_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_u8
	.private_extern _LE__LE_var_C_u8
_LE__LE_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_u8
	.private_extern _LE__uLE_param_C_u8
_LE__uLE_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_u8
	.private_extern _LE__LE_param_C_u8
_LE__LE_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_i8
	.private_extern _LE__uLE_var_C_i8
_LE__uLE_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_i8
	.private_extern _LE__LE_var_C_i8
_LE__LE_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_i8
	.private_extern _LE__uLE_param_C_i8
_LE__uLE_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_i8
	.private_extern _LE__LE_param_C_i8
_LE__LE_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_L
	.private_extern _LE__uLE_var_C_L
_LE__uLE_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_L
	.private_extern _LE__LE_var_C_L
_LE__LE_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_L
	.private_extern _LE__uLE_param_C_L
_LE__uLE_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_L
	.private_extern _LE__LE_param_C_L
_LE__LE_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_C_u32
	.private_extern _LE__uLE_var_C_u32
_LE__uLE_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_C_u32
	.private_extern _LE__LE_var_C_u32
_LE__LE_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_C_u32
	.private_extern _LE__uLE_param_C_u32
_LE__uLE_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_C_u32
	.private_extern _LE__LE_param_C_u32
_LE__LE_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_f32_f32
	.private_extern _LE__LE_var_f32_f32
_LE__LE_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_LE(%rip), %xmm1
	movss	168+_MM_LE(%rip), %xmm0
	ucomiss	%xmm0, %xmm1
	sete	%al
	setnp	%dl
	andl	%edx, %eax
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_f32_f32
	.private_extern _LE__LE_param_f32_f32
_LE__LE_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm1
	movss	-24(%rbp), %xmm0
	ucomiss	%xmm1, %xmm0
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_u16
	.private_extern _LE__uLE_var_u8_u16
_LE__uLE_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_u16
	.private_extern _LE__LE_var_u8_u16
_LE__LE_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_u16
	.private_extern _LE__uLE_param_u8_u16
_LE__uLE_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_u16
	.private_extern _LE__LE_param_u8_u16
_LE__LE_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_u64
	.private_extern _LE__uLE_var_u8_u64
_LE__uLE_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_u64
	.private_extern _LE__LE_var_u8_u64
_LE__LE_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_u64
	.private_extern _LE__uLE_param_u8_u64
_LE__uLE_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_u64
	.private_extern _LE__LE_param_u8_u64
_LE__LE_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_LC
	.private_extern _LE__uLE_var_u8_LC
_LE__uLE_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_LC
	.private_extern _LE__LE_var_u8_LC
_LE__LE_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_LC
	.private_extern _LE__uLE_param_u8_LC
_LE__uLE_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_LC
	.private_extern _LE__LE_param_u8_LC
_LE__LE_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_i16
	.private_extern _LE__uLE_var_u8_i16
_LE__uLE_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_i16
	.private_extern _LE__LE_var_u8_i16
_LE__LE_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_i16
	.private_extern _LE__uLE_param_u8_i16
_LE__uLE_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_i16
	.private_extern _LE__LE_param_u8_i16
_LE__LE_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_i32
	.private_extern _LE__uLE_var_u8_i32
_LE__uLE_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_i32
	.private_extern _LE__LE_var_u8_i32
_LE__LE_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_i32
	.private_extern _LE__uLE_param_u8_i32
_LE__uLE_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_i32
	.private_extern _LE__LE_param_u8_i32
_LE__LE_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_I
	.private_extern _LE__uLE_var_u8_I
_LE__uLE_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_I
	.private_extern _LE__LE_var_u8_I
_LE__LE_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_I
	.private_extern _LE__uLE_param_u8_I
_LE__uLE_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_I
	.private_extern _LE__LE_param_u8_I
_LE__LE_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_i64
	.private_extern _LE__uLE_var_u8_i64
_LE__uLE_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_i64
	.private_extern _LE__LE_var_u8_i64
_LE__LE_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_i64
	.private_extern _LE__uLE_param_u8_i64
_LE__uLE_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_i64
	.private_extern _LE__LE_param_u8_i64
_LE__LE_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_C
	.private_extern _LE__uLE_var_u8_C
_LE__uLE_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_C
	.private_extern _LE__LE_var_u8_C
_LE__LE_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_C
	.private_extern _LE__uLE_param_u8_C
_LE__uLE_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_C
	.private_extern _LE__LE_param_u8_C
_LE__LE_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_u8
	.private_extern _LE__uLE_var_u8_u8
_LE__uLE_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_u8_u8
	.private_extern _LE__LE_var_u8_u8
_LE__LE_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_u8_u8
	.private_extern _LE__uLE_param_u8_u8
_LE__uLE_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_u8
	.private_extern _LE__LE_param_u8_u8
_LE__LE_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	cmpb	-18(%rbp), %al
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_i8
	.private_extern _LE__uLE_var_u8_i8
_LE__uLE_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_i8
	.private_extern _LE__LE_var_u8_i8
_LE__LE_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %edx
	movzbl	173+_MM_LE(%rip), %eax
	cmpb	%al, %dl
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_i8
	.private_extern _LE__uLE_param_u8_i8
_LE__uLE_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_i8
	.private_extern _LE__LE_param_u8_i8
_LE__LE_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	cmpb	-17(%rbp), %al
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_L
	.private_extern _LE__uLE_var_u8_L
_LE__uLE_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_L
	.private_extern _LE__LE_var_u8_L
_LE__LE_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_L
	.private_extern _LE__uLE_param_u8_L
_LE__uLE_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_L
	.private_extern _LE__LE_param_u8_L
_LE__LE_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u8_u32
	.private_extern _LE__uLE_var_u8_u32
_LE__uLE_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u8_u32
	.private_extern _LE__LE_var_u8_u32
_LE__LE_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u8_u32
	.private_extern _LE__uLE_param_u8_u32
_LE__uLE_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u8_u32
	.private_extern _LE__LE_param_u8_u32
_LE__LE_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_u16
	.private_extern _LE__uLE_var_i8_u16
_LE__uLE_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_u16
	.private_extern _LE__LE_var_i8_u16
_LE__LE_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_u16
	.private_extern _LE__uLE_param_i8_u16
_LE__uLE_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_u16
	.private_extern _LE__LE_param_i8_u16
_LE__LE_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_u64
	.private_extern _LE__uLE_var_i8_u64
_LE__uLE_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_u64
	.private_extern _LE__LE_var_i8_u64
_LE__LE_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_u64
	.private_extern _LE__uLE_param_i8_u64
_LE__uLE_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_u64
	.private_extern _LE__LE_param_i8_u64
_LE__LE_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_LC
	.private_extern _LE__uLE_var_i8_LC
_LE__uLE_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_LC
	.private_extern _LE__LE_var_i8_LC
_LE__LE_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_LC
	.private_extern _LE__uLE_param_i8_LC
_LE__uLE_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_LC
	.private_extern _LE__LE_param_i8_LC
_LE__LE_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_i16
	.private_extern _LE__uLE_var_i8_i16
_LE__uLE_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_i16
	.private_extern _LE__LE_var_i8_i16
_LE__LE_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_i16
	.private_extern _LE__uLE_param_i8_i16
_LE__uLE_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_i16
	.private_extern _LE__LE_param_i8_i16
_LE__LE_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_i32
	.private_extern _LE__uLE_var_i8_i32
_LE__uLE_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_i32
	.private_extern _LE__LE_var_i8_i32
_LE__LE_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_i32
	.private_extern _LE__uLE_param_i8_i32
_LE__uLE_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_i32
	.private_extern _LE__LE_param_i8_i32
_LE__LE_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_I
	.private_extern _LE__uLE_var_i8_I
_LE__uLE_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_I
	.private_extern _LE__LE_var_i8_I
_LE__LE_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_I
	.private_extern _LE__uLE_param_i8_I
_LE__uLE_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_I
	.private_extern _LE__LE_param_i8_I
_LE__LE_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_i64
	.private_extern _LE__uLE_var_i8_i64
_LE__uLE_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_i64
	.private_extern _LE__LE_var_i8_i64
_LE__LE_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_i64
	.private_extern _LE__uLE_param_i8_i64
_LE__uLE_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_i64
	.private_extern _LE__LE_param_i8_i64
_LE__LE_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_C
	.private_extern _LE__uLE_var_i8_C
_LE__uLE_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_C
	.private_extern _LE__LE_var_i8_C
_LE__LE_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_C
	.private_extern _LE__uLE_param_i8_C
_LE__uLE_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_C
	.private_extern _LE__LE_param_i8_C
_LE__LE_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_u8
	.private_extern _LE__uLE_var_i8_u8
_LE__uLE_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %edx
	movzbl	172+_MM_LE(%rip), %eax
	cmpb	%al, %dl
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_u8
	.private_extern _LE__LE_var_i8_u8
_LE__LE_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_u8
	.private_extern _LE__uLE_param_i8_u8
_LE__uLE_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	cmpb	-17(%rbp), %al
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_u8
	.private_extern _LE__LE_param_i8_u8
_LE__LE_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_i8
	.private_extern _LE__uLE_var_i8_i8
_LE__uLE_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_i8_i8
	.private_extern _LE__LE_var_i8_i8
_LE__LE_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_i8_i8
	.private_extern _LE__uLE_param_i8_i8
_LE__uLE_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	cmpb	-18(%rbp), %al
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_i8
	.private_extern _LE__LE_param_i8_i8
_LE__LE_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_L
	.private_extern _LE__uLE_var_i8_L
_LE__uLE_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_L
	.private_extern _LE__LE_var_i8_L
_LE__LE_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_L
	.private_extern _LE__uLE_param_i8_L
_LE__uLE_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_L
	.private_extern _LE__LE_param_i8_L
_LE__LE_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_i8_u32
	.private_extern _LE__uLE_var_i8_u32
_LE__uLE_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_i8_u32
	.private_extern _LE__LE_var_i8_u32
_LE__LE_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_LE(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_i8_u32
	.private_extern _LE__uLE_param_i8_u32
_LE__uLE_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_i8_u32
	.private_extern _LE__LE_param_i8_u32
_LE__LE_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_u16
	.private_extern _LE__uLE_var_L_u16
_LE__uLE_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_u16
	.private_extern _LE__LE_var_L_u16
_LE__LE_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_u16
	.private_extern _LE__uLE_param_L_u16
_LE__uLE_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_u16
	.private_extern _LE__LE_param_L_u16
_LE__LE_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_u64
	.private_extern _LE__uLE_var_L_u64
_LE__uLE_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_u64
	.private_extern _LE__LE_var_L_u64
_LE__LE_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LE(%rip), %rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_u64
	.private_extern _LE__uLE_param_L_u64
_LE__uLE_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_u64
	.private_extern _LE__LE_param_L_u64
_LE__LE_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_LC
	.private_extern _LE__uLE_var_L_LC
_LE__uLE_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_LC
	.private_extern _LE__LE_var_L_LC
_LE__LE_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LE(%rip), %rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_LC
	.private_extern _LE__uLE_param_L_LC
_LE__uLE_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_LC
	.private_extern _LE__LE_param_L_LC
_LE__LE_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_i16
	.private_extern _LE__uLE_var_L_i16
_LE__uLE_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_i16
	.private_extern _LE__LE_var_L_i16
_LE__LE_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_i16
	.private_extern _LE__uLE_param_L_i16
_LE__uLE_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_i16
	.private_extern _LE__LE_param_L_i16
_LE__LE_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_i32
	.private_extern _LE__uLE_var_L_i32
_LE__uLE_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_i32
	.private_extern _LE__LE_var_L_i32
_LE__LE_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_i32
	.private_extern _LE__uLE_param_L_i32
_LE__uLE_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_i32
	.private_extern _LE__LE_param_L_i32
_LE__LE_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_I
	.private_extern _LE__uLE_var_L_I
_LE__uLE_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_I
	.private_extern _LE__LE_var_L_I
_LE__LE_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_I
	.private_extern _LE__uLE_param_L_I
_LE__uLE_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_I
	.private_extern _LE__LE_param_L_I
_LE__LE_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_i64
	.private_extern _LE__uLE_var_L_i64
_LE__uLE_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_i64
	.private_extern _LE__LE_var_L_i64
_LE__LE_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LE(%rip), %rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_i64
	.private_extern _LE__uLE_param_L_i64
_LE__uLE_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_i64
	.private_extern _LE__LE_param_L_i64
_LE__LE_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_C
	.private_extern _LE__uLE_var_L_C
_LE__uLE_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_C
	.private_extern _LE__LE_var_L_C
_LE__LE_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LE(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_C
	.private_extern _LE__uLE_param_L_C
_LE__uLE_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_C
	.private_extern _LE__LE_param_L_C
_LE__LE_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_u8
	.private_extern _LE__uLE_var_L_u8
_LE__uLE_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_u8
	.private_extern _LE__LE_var_L_u8
_LE__LE_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_u8
	.private_extern _LE__uLE_param_L_u8
_LE__uLE_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_u8
	.private_extern _LE__LE_param_L_u8
_LE__LE_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_i8
	.private_extern _LE__uLE_var_L_i8
_LE__uLE_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_i8
	.private_extern _LE__LE_var_L_i8
_LE__LE_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_i8
	.private_extern _LE__uLE_param_L_i8
_LE__uLE_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_i8
	.private_extern _LE__LE_param_L_i8
_LE__LE_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_L
	.private_extern _LE__uLE_var_L_L
_LE__uLE_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_L_L
	.private_extern _LE__LE_var_L_L
_LE__LE_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_L_L
	.private_extern _LE__uLE_param_L_L
_LE__uLE_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_L
	.private_extern _LE__LE_param_L_L
_LE__LE_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_L_u32
	.private_extern _LE__uLE_var_L_u32
_LE__uLE_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_L_u32
	.private_extern _LE__LE_var_L_u32
_LE__LE_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_L_u32
	.private_extern _LE__uLE_param_L_u32
_LE__uLE_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setae	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_L_u32
	.private_extern _LE__LE_param_L_u32
_LE__LE_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_u16
	.private_extern _LE__uLE_var_u32_u16
_LE__uLE_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_LE(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_u16
	.private_extern _LE__LE_var_u32_u16
_LE__LE_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %edx
	movzwl	104+_MM_LE(%rip), %eax
	cwtl
	cmpl	%eax, %edx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_u16
	.private_extern _LE__uLE_param_u32_u16
_LE__uLE_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_u16
	.private_extern _LE__LE_param_u32_u16
_LE__LE_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movswl	-22(%rbp),%eax
	cmpl	-20(%rbp), %eax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_u64
	.private_extern _LE__uLE_var_u32_u64
_LE__uLE_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_u64
	.private_extern _LE__LE_var_u32_u64
_LE__LE_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_u64
	.private_extern _LE__uLE_param_u32_u64
_LE__uLE_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_u64
	.private_extern _LE__LE_param_u32_u64
_LE__LE_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_LC
	.private_extern _LE__uLE_var_u32_LC
_LE__uLE_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_LC
	.private_extern _LE__LE_var_u32_LC
_LE__LE_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_LC
	.private_extern _LE__uLE_param_u32_LC
_LE__uLE_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_LC
	.private_extern _LE__LE_param_u32_LC
_LE__LE_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_i16
	.private_extern _LE__uLE_var_u32_i16
_LE__uLE_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_i16
	.private_extern _LE__LE_var_u32_i16
_LE__LE_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %edx
	movzwl	136+_MM_LE(%rip), %eax
	movzwl	%ax, %eax
	cmpl	%eax, %edx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_i16
	.private_extern _LE__uLE_param_u32_i16
_LE__uLE_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_i16
	.private_extern _LE__LE_param_u32_i16
_LE__LE_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movzwl	-22(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_i32
	.private_extern _LE__uLE_var_u32_i32
_LE__uLE_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_LE(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_i32
	.private_extern _LE__LE_var_u32_i32
_LE__LE_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %edx
	movl	140+_MM_LE(%rip), %eax
	cmpl	%eax, %edx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_i32
	.private_extern _LE__uLE_param_u32_i32
_LE__uLE_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_i32
	.private_extern _LE__LE_param_u32_i32
_LE__LE_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_I
	.private_extern _LE__uLE_var_u32_I
_LE__uLE_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_I
	.private_extern _LE__LE_var_u32_I
_LE__LE_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_I
	.private_extern _LE__uLE_param_u32_I
_LE__uLE_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_I
	.private_extern _LE__LE_param_u32_I
_LE__LE_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_i64
	.private_extern _LE__uLE_var_u32_i64
_LE__uLE_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_i64
	.private_extern _LE__LE_var_u32_i64
_LE__LE_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_i64
	.private_extern _LE__uLE_param_u32_i64
_LE__uLE_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_i64
	.private_extern _LE__LE_param_u32_i64
_LE__LE_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_C
	.private_extern _LE__uLE_var_u32_C
_LE__uLE_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_C
	.private_extern _LE__LE_var_u32_C
_LE__LE_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_C
	.private_extern _LE__uLE_param_u32_C
_LE__uLE_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_C
	.private_extern _LE__LE_param_u32_C
_LE__LE_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_u8
	.private_extern _LE__uLE_var_u32_u8
_LE__uLE_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_LE(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_u8
	.private_extern _LE__LE_var_u32_u8
_LE__LE_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %edx
	movzbl	172+_MM_LE(%rip), %eax
	movsbl	%al,%eax
	cmpl	%eax, %edx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_u8
	.private_extern _LE__uLE_param_u32_u8
_LE__uLE_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_u8
	.private_extern _LE__LE_param_u32_u8
_LE__LE_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movsbl	-21(%rbp),%eax
	cmpl	-20(%rbp), %eax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_i8
	.private_extern _LE__uLE_var_u32_i8
_LE__uLE_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_i8
	.private_extern _LE__LE_var_u32_i8
_LE__LE_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %edx
	movzbl	173+_MM_LE(%rip), %eax
	movzbl	%al, %eax
	cmpl	%eax, %edx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_i8
	.private_extern _LE__uLE_param_u32_i8
_LE__uLE_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_i8
	.private_extern _LE__LE_param_u32_i8
_LE__LE_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movzbl	-21(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setge	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_L
	.private_extern _LE__uLE_var_u32_L
_LE__uLE_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_var_u32_L
	.private_extern _LE__LE_var_u32_L
_LE__LE_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_LE(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LE(%rip), %rax
	cmpq	%rax, %rdx
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_param_u32_L
	.private_extern _LE__uLE_param_u32_L
_LE__uLE_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_L
	.private_extern _LE__LE_param_u32_L
_LE__LE_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__uLE_var_u32_u32
	.private_extern _LE__uLE_var_u32_u32
_LE__uLE_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__LE_var_u32_u32
	.private_extern _LE__LE_var_u32_u32
_LE__LE_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _LE__uLE_param_u32_u32
	.private_extern _LE__uLE_param_u32_u32
_LE__uLE_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setbe	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE__LE_param_u32_u32
	.private_extern _LE__LE_param_u32_u32
_LE__LE_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cmpl	-24(%rbp), %eax
	setle	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LE_M3
_LE_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_LE(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "LE_M3"
	.space 1
	.ascii "LE_param_u32_u32"
	.space 1
	.ascii "uLE_param_u32_u32"
	.space 1
	.ascii "LE_var_u32_u32"
	.space 1
	.ascii "uLE_var_u32_u32"
	.space 1
	.ascii "LE_param_u32_L"
	.space 1
	.ascii "uLE_param_u32_L"
	.space 1
	.ascii "LE_var_u32_L"
	.space 1
	.ascii "uLE_var_u32_L"
	.space 1
	.ascii "LE_param_u32_i8"
	.space 1
	.ascii "uLE_param_u32_i8"
	.space 1
	.ascii "LE_var_u32_i8"
	.space 1
	.ascii "uLE_var_u32_i8"
	.space 1
	.ascii "LE_param_u32_u8"
	.space 1
	.ascii "uLE_param_u32_u8"
	.space 1
	.ascii "LE_var_u32_u8"
	.space 1
	.ascii "uLE_var_u32_u8"
	.space 1
	.ascii "LE_param_u32_C"
	.space 1
	.ascii "uLE_param_u32_C"
	.space 1
	.ascii "LE_var_u32_C"
	.space 1
	.ascii "uLE_var_u32_C"
	.space 1
	.ascii "LE_param_u32_i64"
	.space 1
	.ascii "uLE_param_u32_i64"
	.space 1
	.ascii "LE_var_u32_i64"
	.space 1
	.ascii "uLE_var_u32_i64"
	.space 1
	.ascii "LE_param_u32_I"
	.space 1
	.ascii "uLE_param_u32_I"
	.space 1
	.ascii "LE_var_u32_I"
	.space 1
	.ascii "uLE_var_u32_I"
	.space 1
	.ascii "LE_param_u32_i32"
	.space 1
	.ascii "uLE_param_u32_i32"
	.space 1
	.ascii "LE_var_u32_i32"
	.space 1
	.ascii "uLE_var_u32_i32"
	.space 1
	.ascii "LE_param_u32_i16"
	.space 1
	.ascii "uLE_param_u32_i16"
	.space 1
	.ascii "LE_var_u32_i16"
	.space 1
	.ascii "uLE_var_u32_i16"
	.space 1
	.ascii "LE_param_u32_LC"
	.space 1
	.ascii "uLE_param_u32_LC"
	.space 1
	.ascii "LE_var_u32_LC"
	.space 1
	.ascii "uLE_var_u32_LC"
	.space 1
	.ascii "LE_param_u32_u64"
	.space 1
	.ascii "uLE_param_u32_u64"
	.space 1
	.ascii "LE_var_u32_u64"
	.space 1
	.ascii "uLE_var_u32_u64"
	.space 1
	.ascii "LE_param_u32_u16"
	.space 1
	.ascii "uLE_param_u32_u16"
	.space 1
	.ascii "LE_var_u32_u16"
	.space 1
	.ascii "uLE_var_u32_u16"
	.space 1
	.ascii "LE_param_L_u32"
	.space 1
	.ascii "uLE_param_L_u32"
	.space 1
	.ascii "LE_var_L_u32"
	.space 1
	.ascii "uLE_var_L_u32"
	.space 1
	.ascii "LE_param_L_L"
	.space 1
	.ascii "uLE_param_L_L"
	.space 1
	.ascii "LE_var_L_L"
	.space 1
	.ascii "uLE_var_L_L"
	.space 1
	.ascii "LE_param_L_i8"
	.space 1
	.ascii "uLE_param_L_i8"
	.space 1
	.ascii "LE_var_L_i8"
	.space 1
	.ascii "uLE_var_L_i8"
	.space 1
	.ascii "LE_param_L_u8"
	.space 1
	.ascii "uLE_param_L_u8"
	.space 1
	.ascii "LE_var_L_u8"
	.space 1
	.ascii "uLE_var_L_u8"
	.space 1
	.ascii "LE_param_L_C"
	.space 1
	.ascii "uLE_param_L_C"
	.space 1
	.ascii "LE_var_L_C"
	.space 1
	.ascii "uLE_var_L_C"
	.space 1
	.ascii "LE_param_L_i64"
	.space 1
	.ascii "uLE_param_L_i64"
	.space 1
	.ascii "LE_var_L_i64"
	.space 1
	.ascii "uLE_var_L_i64"
	.space 1
	.ascii "LE_param_L_I"
	.space 1
	.ascii "uLE_param_L_I"
	.space 1
	.ascii "LE_var_L_I"
	.space 1
	.ascii "uLE_var_L_I"
	.space 1
	.ascii "LE_param_L_i32"
	.space 1
	.ascii "uLE_param_L_i32"
	.space 1
	.ascii "LE_var_L_i32"
	.space 1
	.ascii "uLE_var_L_i32"
	.space 1
	.ascii "LE_param_L_i16"
	.space 1
	.ascii "uLE_param_L_i16"
	.space 1
	.ascii "LE_var_L_i16"
	.space 1
	.ascii "uLE_var_L_i16"
	.space 1
	.ascii "LE_param_L_LC"
	.space 1
	.ascii "uLE_param_L_LC"
	.space 1
	.ascii "LE_var_L_LC"
	.space 1
	.ascii "uLE_var_L_LC"
	.space 1
	.ascii "LE_param_L_u64"
	.space 1
	.ascii "uLE_param_L_u64"
	.space 1
	.ascii "LE_var_L_u64"
	.space 1
	.ascii "uLE_var_L_u64"
	.space 1
	.ascii "LE_param_L_u16"
	.space 1
	.ascii "uLE_param_L_u16"
	.space 1
	.ascii "LE_var_L_u16"
	.space 1
	.ascii "uLE_var_L_u16"
	.space 1
	.ascii "LE_param_i8_u32"
	.space 1
	.ascii "uLE_param_i8_u32"
	.space 1
	.ascii "LE_var_i8_u32"
	.space 1
	.ascii "uLE_var_i8_u32"
	.space 1
	.ascii "LE_param_i8_L"
	.space 1
	.ascii "uLE_param_i8_L"
	.space 1
	.ascii "LE_var_i8_L"
	.space 1
	.ascii "uLE_var_i8_L"
	.space 1
	.ascii "LE_param_i8_i8"
	.space 1
	.ascii "uLE_param_i8_i8"
	.space 1
	.ascii "LE_var_i8_i8"
	.space 1
	.ascii "uLE_var_i8_i8"
	.space 1
	.ascii "LE_param_i8_u8"
	.space 1
	.ascii "uLE_param_i8_u8"
	.space 1
	.ascii "LE_var_i8_u8"
	.space 1
	.ascii "uLE_var_i8_u8"
	.space 1
	.ascii "LE_param_i8_C"
	.space 1
	.ascii "uLE_param_i8_C"
	.space 1
	.ascii "LE_var_i8_C"
	.space 1
	.ascii "uLE_var_i8_C"
	.space 1
	.ascii "LE_param_i8_i64"
	.space 1
	.ascii "uLE_param_i8_i64"
	.space 1
	.ascii "LE_var_i8_i64"
	.space 1
	.ascii "uLE_var_i8_i64"
	.space 1
	.ascii "LE_param_i8_I"
	.space 1
	.ascii "uLE_param_i8_I"
	.space 1
	.ascii "LE_var_i8_I"
	.space 1
	.ascii "uLE_var_i8_I"
	.space 1
	.ascii "LE_param_i8_i32"
	.space 1
	.ascii "uLE_param_i8_i32"
	.space 1
	.ascii "LE_var_i8_i32"
	.space 1
	.ascii "uLE_var_i8_i32"
	.space 1
	.ascii "LE_param_i8_i16"
	.space 1
	.ascii "uLE_param_i8_i16"
	.space 1
	.ascii "LE_var_i8_i16"
	.space 1
	.ascii "uLE_var_i8_i16"
	.space 1
	.ascii "LE_param_i8_LC"
	.space 1
	.ascii "uLE_param_i8_LC"
	.space 1
	.ascii "LE_var_i8_LC"
	.space 1
	.ascii "uLE_var_i8_LC"
	.space 1
	.ascii "LE_param_i8_u64"
	.space 1
	.ascii "uLE_param_i8_u64"
	.space 1
	.ascii "LE_var_i8_u64"
	.space 1
	.ascii "uLE_var_i8_u64"
	.space 1
	.ascii "LE_param_i8_u16"
	.space 1
	.ascii "uLE_param_i8_u16"
	.space 1
	.ascii "LE_var_i8_u16"
	.space 1
	.ascii "uLE_var_i8_u16"
	.space 1
	.ascii "LE_param_u8_u32"
	.space 1
	.ascii "uLE_param_u8_u32"
	.space 1
	.ascii "LE_var_u8_u32"
	.space 1
	.ascii "uLE_var_u8_u32"
	.space 1
	.ascii "LE_param_u8_L"
	.space 1
	.ascii "uLE_param_u8_L"
	.space 1
	.ascii "LE_var_u8_L"
	.space 1
	.ascii "uLE_var_u8_L"
	.space 1
	.ascii "LE_param_u8_i8"
	.space 1
	.ascii "uLE_param_u8_i8"
	.space 1
	.ascii "LE_var_u8_i8"
	.space 1
	.ascii "uLE_var_u8_i8"
	.space 1
	.ascii "LE_param_u8_u8"
	.space 1
	.ascii "uLE_param_u8_u8"
	.space 1
	.ascii "LE_var_u8_u8"
	.space 1
	.ascii "uLE_var_u8_u8"
	.space 1
	.ascii "LE_param_u8_C"
	.space 1
	.ascii "uLE_param_u8_C"
	.space 1
	.ascii "LE_var_u8_C"
	.space 1
	.ascii "uLE_var_u8_C"
	.space 1
	.ascii "LE_param_u8_i64"
	.space 1
	.ascii "uLE_param_u8_i64"
	.space 1
	.ascii "LE_var_u8_i64"
	.space 1
	.ascii "uLE_var_u8_i64"
	.space 1
	.ascii "LE_param_u8_I"
	.space 1
	.ascii "uLE_param_u8_I"
	.space 1
	.ascii "LE_var_u8_I"
	.space 1
	.ascii "uLE_var_u8_I"
	.space 1
	.ascii "LE_param_u8_i32"
	.space 1
	.ascii "uLE_param_u8_i32"
	.space 1
	.ascii "LE_var_u8_i32"
	.space 1
	.ascii "uLE_var_u8_i32"
	.space 1
	.ascii "LE_param_u8_i16"
	.space 1
	.ascii "uLE_param_u8_i16"
	.space 1
	.ascii "LE_var_u8_i16"
	.space 1
	.ascii "uLE_var_u8_i16"
	.space 1
	.ascii "LE_param_u8_LC"
	.space 1
	.ascii "uLE_param_u8_LC"
	.space 1
	.ascii "LE_var_u8_LC"
	.space 1
	.ascii "uLE_var_u8_LC"
	.space 1
	.ascii "LE_param_u8_u64"
	.space 1
	.ascii "uLE_param_u8_u64"
	.space 1
	.ascii "LE_var_u8_u64"
	.space 1
	.ascii "uLE_var_u8_u64"
	.space 1
	.ascii "LE_param_u8_u16"
	.space 1
	.ascii "uLE_param_u8_u16"
	.space 1
	.ascii "LE_var_u8_u16"
	.space 1
	.ascii "uLE_var_u8_u16"
	.space 1
	.ascii "LE_param_f32_f32"
	.space 1
	.ascii "LE_var_f32_f32"
	.space 1
	.ascii "LE_param_C_u32"
	.space 1
	.ascii "uLE_param_C_u32"
	.space 1
	.ascii "LE_var_C_u32"
	.space 1
	.ascii "uLE_var_C_u32"
	.space 1
	.ascii "LE_param_C_L"
	.space 1
	.ascii "uLE_param_C_L"
	.space 1
	.ascii "LE_var_C_L"
	.space 1
	.ascii "uLE_var_C_L"
	.space 1
	.ascii "LE_param_C_i8"
	.space 1
	.ascii "uLE_param_C_i8"
	.space 1
	.ascii "LE_var_C_i8"
	.space 1
	.ascii "uLE_var_C_i8"
	.space 1
	.ascii "LE_param_C_u8"
	.space 1
	.ascii "uLE_param_C_u8"
	.space 1
	.ascii "LE_var_C_u8"
	.space 1
	.ascii "uLE_var_C_u8"
	.space 1
	.ascii "LE_param_C_C"
	.space 1
	.ascii "uLE_param_C_C"
	.space 1
	.ascii "LE_var_C_C"
	.space 1
	.ascii "uLE_var_C_C"
	.space 1
	.ascii "LE_param_C_i64"
	.space 1
	.ascii "uLE_param_C_i64"
	.space 1
	.ascii "LE_var_C_i64"
	.space 1
	.ascii "uLE_var_C_i64"
	.space 1
	.ascii "LE_param_C_I"
	.space 1
	.ascii "uLE_param_C_I"
	.space 1
	.ascii "LE_var_C_I"
	.space 1
	.ascii "uLE_var_C_I"
	.space 1
	.ascii "LE_param_C_i32"
	.space 1
	.ascii "uLE_param_C_i32"
	.space 1
	.ascii "LE_var_C_i32"
	.space 1
	.ascii "uLE_var_C_i32"
	.space 1
	.ascii "LE_param_C_i16"
	.space 1
	.ascii "uLE_param_C_i16"
	.space 1
	.ascii "LE_var_C_i16"
	.space 1
	.ascii "uLE_var_C_i16"
	.space 1
	.ascii "LE_param_C_LC"
	.space 1
	.ascii "uLE_param_C_LC"
	.space 1
	.ascii "LE_var_C_LC"
	.space 1
	.ascii "uLE_var_C_LC"
	.space 1
	.ascii "LE_param_C_u64"
	.space 1
	.ascii "uLE_param_C_u64"
	.space 1
	.ascii "LE_var_C_u64"
	.space 1
	.ascii "uLE_var_C_u64"
	.space 1
	.ascii "LE_param_C_u16"
	.space 1
	.ascii "uLE_param_C_u16"
	.space 1
	.ascii "LE_var_C_u16"
	.space 1
	.ascii "uLE_var_C_u16"
	.space 1
	.ascii "LE_param_i64_u32"
	.space 1
	.ascii "uLE_param_i64_u32"
	.space 1
	.ascii "LE_var_i64_u32"
	.space 1
	.ascii "uLE_var_i64_u32"
	.space 1
	.ascii "LE_param_i64_L"
	.space 1
	.ascii "uLE_param_i64_L"
	.space 1
	.ascii "LE_var_i64_L"
	.space 1
	.ascii "uLE_var_i64_L"
	.space 1
	.ascii "LE_param_i64_i8"
	.space 1
	.ascii "uLE_param_i64_i8"
	.space 1
	.ascii "LE_var_i64_i8"
	.space 1
	.ascii "uLE_var_i64_i8"
	.space 1
	.ascii "LE_param_i64_u8"
	.space 1
	.ascii "uLE_param_i64_u8"
	.space 1
	.ascii "LE_var_i64_u8"
	.space 1
	.ascii "uLE_var_i64_u8"
	.space 1
	.ascii "LE_param_i64_C"
	.space 1
	.ascii "uLE_param_i64_C"
	.space 1
	.ascii "LE_var_i64_C"
	.space 1
	.ascii "uLE_var_i64_C"
	.space 1
	.ascii "LE_param_i64_i64"
	.space 1
	.ascii "uLE_param_i64_i64"
	.space 1
	.ascii "LE_var_i64_i64"
	.space 1
	.ascii "uLE_var_i64_i64"
	.space 1
	.ascii "LE_param_i64_I"
	.space 1
	.ascii "uLE_param_i64_I"
	.space 1
	.ascii "LE_var_i64_I"
	.space 1
	.ascii "uLE_var_i64_I"
	.space 1
	.ascii "LE_param_i64_i32"
	.space 1
	.ascii "uLE_param_i64_i32"
	.space 1
	.ascii "LE_var_i64_i32"
	.space 1
	.ascii "uLE_var_i64_i32"
	.space 1
	.ascii "LE_param_i64_i16"
	.space 1
	.ascii "uLE_param_i64_i16"
	.space 1
	.ascii "LE_var_i64_i16"
	.space 1
	.ascii "uLE_var_i64_i16"
	.space 1
	.ascii "LE_param_i64_LC"
	.space 1
	.ascii "uLE_param_i64_LC"
	.space 1
	.ascii "LE_var_i64_LC"
	.space 1
	.ascii "uLE_var_i64_LC"
	.space 1
	.ascii "LE_param_i64_u64"
	.space 1
	.ascii "uLE_param_i64_u64"
	.space 1
	.ascii "LE_var_i64_u64"
	.space 1
	.ascii "uLE_var_i64_u64"
	.space 1
	.ascii "LE_param_i64_u16"
	.space 1
	.ascii "uLE_param_i64_u16"
	.space 1
	.ascii "LE_var_i64_u16"
	.space 1
	.ascii "uLE_var_i64_u16"
	.space 1
	.ascii "LE_param_I_u32"
	.space 1
	.ascii "uLE_param_I_u32"
	.space 1
	.ascii "LE_var_I_u32"
	.space 1
	.ascii "uLE_var_I_u32"
	.space 1
	.ascii "LE_param_I_L"
	.space 1
	.ascii "uLE_param_I_L"
	.space 1
	.ascii "LE_var_I_L"
	.space 1
	.ascii "uLE_var_I_L"
	.space 1
	.ascii "LE_param_I_i8"
	.space 1
	.ascii "uLE_param_I_i8"
	.space 1
	.ascii "LE_var_I_i8"
	.space 1
	.ascii "uLE_var_I_i8"
	.space 1
	.ascii "LE_param_I_u8"
	.space 1
	.ascii "uLE_param_I_u8"
	.space 1
	.ascii "LE_var_I_u8"
	.space 1
	.ascii "uLE_var_I_u8"
	.space 1
	.ascii "LE_param_I_C"
	.space 1
	.ascii "uLE_param_I_C"
	.space 1
	.ascii "LE_var_I_C"
	.space 1
	.ascii "uLE_var_I_C"
	.space 1
	.ascii "LE_param_I_i64"
	.space 1
	.ascii "uLE_param_I_i64"
	.space 1
	.ascii "LE_var_I_i64"
	.space 1
	.ascii "uLE_var_I_i64"
	.space 1
	.ascii "LE_param_I_I"
	.space 1
	.ascii "uLE_param_I_I"
	.space 1
	.ascii "LE_var_I_I"
	.space 1
	.ascii "uLE_var_I_I"
	.space 1
	.ascii "LE_param_I_i32"
	.space 1
	.ascii "uLE_param_I_i32"
	.space 1
	.ascii "LE_var_I_i32"
	.space 1
	.ascii "uLE_var_I_i32"
	.space 1
	.ascii "LE_param_I_i16"
	.space 1
	.ascii "uLE_param_I_i16"
	.space 1
	.ascii "LE_var_I_i16"
	.space 1
	.ascii "uLE_var_I_i16"
	.space 1
	.ascii "LE_param_I_LC"
	.space 1
	.ascii "uLE_param_I_LC"
	.space 1
	.ascii "LE_var_I_LC"
	.space 1
	.ascii "uLE_var_I_LC"
	.space 1
	.ascii "LE_param_I_u64"
	.space 1
	.ascii "uLE_param_I_u64"
	.space 1
	.ascii "LE_var_I_u64"
	.space 1
	.ascii "uLE_var_I_u64"
	.space 1
	.ascii "LE_param_I_u16"
	.space 1
	.ascii "uLE_param_I_u16"
	.space 1
	.ascii "LE_var_I_u16"
	.space 1
	.ascii "uLE_var_I_u16"
	.space 1
	.ascii "LE_param_i32_u32"
	.space 1
	.ascii "uLE_param_i32_u32"
	.space 1
	.ascii "LE_var_i32_u32"
	.space 1
	.ascii "uLE_var_i32_u32"
	.space 1
	.ascii "LE_param_i32_L"
	.space 1
	.ascii "uLE_param_i32_L"
	.space 1
	.ascii "LE_var_i32_L"
	.space 1
	.ascii "uLE_var_i32_L"
	.space 1
	.ascii "LE_param_i32_i8"
	.space 1
	.ascii "uLE_param_i32_i8"
	.space 1
	.ascii "LE_var_i32_i8"
	.space 1
	.ascii "uLE_var_i32_i8"
	.space 1
	.ascii "LE_param_i32_u8"
	.space 1
	.ascii "uLE_param_i32_u8"
	.space 1
	.ascii "LE_var_i32_u8"
	.space 1
	.ascii "uLE_var_i32_u8"
	.space 1
	.ascii "LE_param_i32_C"
	.space 1
	.ascii "uLE_param_i32_C"
	.space 1
	.ascii "LE_var_i32_C"
	.space 1
	.ascii "uLE_var_i32_C"
	.space 1
	.ascii "LE_param_i32_i64"
	.space 1
	.ascii "uLE_param_i32_i64"
	.space 1
	.ascii "LE_var_i32_i64"
	.space 1
	.ascii "uLE_var_i32_i64"
	.space 1
	.ascii "LE_param_i32_I"
	.space 1
	.ascii "uLE_param_i32_I"
	.space 1
	.ascii "LE_var_i32_I"
	.space 1
	.ascii "uLE_var_i32_I"
	.space 1
	.ascii "LE_param_i32_i32"
	.space 1
	.ascii "uLE_param_i32_i32"
	.space 1
	.ascii "LE_var_i32_i32"
	.space 1
	.ascii "uLE_var_i32_i32"
	.space 1
	.ascii "LE_param_i32_i16"
	.space 1
	.ascii "uLE_param_i32_i16"
	.space 1
	.ascii "LE_var_i32_i16"
	.space 1
	.ascii "uLE_var_i32_i16"
	.space 1
	.ascii "LE_param_i32_LC"
	.space 1
	.ascii "uLE_param_i32_LC"
	.space 1
	.ascii "LE_var_i32_LC"
	.space 1
	.ascii "uLE_var_i32_LC"
	.space 1
	.ascii "LE_param_i32_u64"
	.space 1
	.ascii "uLE_param_i32_u64"
	.space 1
	.ascii "LE_var_i32_u64"
	.space 1
	.ascii "uLE_var_i32_u64"
	.space 1
	.ascii "LE_param_i32_u16"
	.space 1
	.ascii "uLE_param_i32_u16"
	.space 1
	.ascii "LE_var_i32_u16"
	.space 1
	.ascii "uLE_var_i32_u16"
	.space 1
	.ascii "LE_param_i16_u32"
	.space 1
	.ascii "uLE_param_i16_u32"
	.space 1
	.ascii "LE_var_i16_u32"
	.space 1
	.ascii "uLE_var_i16_u32"
	.space 1
	.ascii "LE_param_i16_L"
	.space 1
	.ascii "uLE_param_i16_L"
	.space 1
	.ascii "LE_var_i16_L"
	.space 1
	.ascii "uLE_var_i16_L"
	.space 1
	.ascii "LE_param_i16_i8"
	.space 1
	.ascii "uLE_param_i16_i8"
	.space 1
	.ascii "LE_var_i16_i8"
	.space 1
	.ascii "uLE_var_i16_i8"
	.space 1
	.ascii "LE_param_i16_u8"
	.space 1
	.ascii "uLE_param_i16_u8"
	.space 1
	.ascii "LE_var_i16_u8"
	.space 1
	.ascii "uLE_var_i16_u8"
	.space 1
	.ascii "LE_param_i16_C"
	.space 1
	.ascii "uLE_param_i16_C"
	.space 1
	.ascii "LE_var_i16_C"
	.space 1
	.ascii "uLE_var_i16_C"
	.space 1
	.ascii "LE_param_i16_i64"
	.space 1
	.ascii "uLE_param_i16_i64"
	.space 1
	.ascii "LE_var_i16_i64"
	.space 1
	.ascii "uLE_var_i16_i64"
	.space 1
	.ascii "LE_param_i16_I"
	.space 1
	.ascii "uLE_param_i16_I"
	.space 1
	.ascii "LE_var_i16_I"
	.space 1
	.ascii "uLE_var_i16_I"
	.space 1
	.ascii "LE_param_i16_i32"
	.space 1
	.ascii "uLE_param_i16_i32"
	.space 1
	.ascii "LE_var_i16_i32"
	.space 1
	.ascii "uLE_var_i16_i32"
	.space 1
	.ascii "LE_param_i16_i16"
	.space 1
	.ascii "uLE_param_i16_i16"
	.space 1
	.ascii "LE_var_i16_i16"
	.space 1
	.ascii "uLE_var_i16_i16"
	.space 1
	.ascii "LE_param_i16_LC"
	.space 1
	.ascii "uLE_param_i16_LC"
	.space 1
	.ascii "LE_var_i16_LC"
	.space 1
	.ascii "uLE_var_i16_LC"
	.space 1
	.ascii "LE_param_i16_u64"
	.space 1
	.ascii "uLE_param_i16_u64"
	.space 1
	.ascii "LE_var_i16_u64"
	.space 1
	.ascii "uLE_var_i16_u64"
	.space 1
	.ascii "LE_param_i16_u16"
	.space 1
	.ascii "uLE_param_i16_u16"
	.space 1
	.ascii "LE_var_i16_u16"
	.space 1
	.ascii "uLE_var_i16_u16"
	.space 1
	.ascii "LE_param_f64_f64"
	.space 1
	.ascii "LE_var_f64_f64"
	.space 1
	.ascii "LE_param_LC_u32"
	.space 1
	.ascii "uLE_param_LC_u32"
	.space 1
	.ascii "LE_var_LC_u32"
	.space 1
	.ascii "uLE_var_LC_u32"
	.space 1
	.ascii "LE_param_LC_L"
	.space 1
	.ascii "uLE_param_LC_L"
	.space 1
	.ascii "LE_var_LC_L"
	.space 1
	.ascii "uLE_var_LC_L"
	.space 1
	.ascii "LE_param_LC_i8"
	.space 1
	.ascii "uLE_param_LC_i8"
	.space 1
	.ascii "LE_var_LC_i8"
	.space 1
	.ascii "uLE_var_LC_i8"
	.space 1
	.ascii "LE_param_LC_u8"
	.space 1
	.ascii "uLE_param_LC_u8"
	.space 1
	.ascii "LE_var_LC_u8"
	.space 1
	.ascii "uLE_var_LC_u8"
	.space 1
	.ascii "LE_param_LC_C"
	.space 1
	.ascii "uLE_param_LC_C"
	.space 1
	.ascii "LE_var_LC_C"
	.space 1
	.ascii "uLE_var_LC_C"
	.space 1
	.ascii "LE_param_LC_i64"
	.space 1
	.ascii "uLE_param_LC_i64"
	.space 1
	.ascii "LE_var_LC_i64"
	.space 1
	.ascii "uLE_var_LC_i64"
	.space 1
	.ascii "LE_param_LC_I"
	.space 1
	.ascii "uLE_param_LC_I"
	.space 1
	.ascii "LE_var_LC_I"
	.space 1
	.ascii "uLE_var_LC_I"
	.space 1
	.ascii "LE_param_LC_i32"
	.space 1
	.ascii "uLE_param_LC_i32"
	.space 1
	.ascii "LE_var_LC_i32"
	.space 1
	.ascii "uLE_var_LC_i32"
	.space 1
	.ascii "LE_param_LC_i16"
	.space 1
	.ascii "uLE_param_LC_i16"
	.space 1
	.ascii "LE_var_LC_i16"
	.space 1
	.ascii "uLE_var_LC_i16"
	.space 1
	.ascii "LE_param_LC_LC"
	.space 1
	.ascii "uLE_param_LC_LC"
	.space 1
	.ascii "LE_var_LC_LC"
	.space 1
	.ascii "uLE_var_LC_LC"
	.space 1
	.ascii "LE_param_LC_u64"
	.space 1
	.ascii "uLE_param_LC_u64"
	.space 1
	.ascii "LE_var_LC_u64"
	.space 1
	.ascii "uLE_var_LC_u64"
	.space 1
	.ascii "LE_param_LC_u16"
	.space 1
	.ascii "uLE_param_LC_u16"
	.space 1
	.ascii "LE_var_LC_u16"
	.space 1
	.ascii "uLE_var_LC_u16"
	.space 1
	.ascii "LE_param_u64_u32"
	.space 1
	.ascii "uLE_param_u64_u32"
	.space 1
	.ascii "LE_var_u64_u32"
	.space 1
	.ascii "uLE_var_u64_u32"
	.space 1
	.ascii "LE_param_u64_L"
	.space 1
	.ascii "uLE_param_u64_L"
	.space 1
	.ascii "LE_var_u64_L"
	.space 1
	.ascii "uLE_var_u64_L"
	.space 1
	.ascii "LE_param_u64_i8"
	.space 1
	.ascii "uLE_param_u64_i8"
	.space 1
	.ascii "LE_var_u64_i8"
	.space 1
	.ascii "uLE_var_u64_i8"
	.space 1
	.ascii "LE_param_u64_u8"
	.space 1
	.ascii "uLE_param_u64_u8"
	.space 1
	.ascii "LE_var_u64_u8"
	.space 1
	.ascii "uLE_var_u64_u8"
	.space 1
	.ascii "LE_param_u64_C"
	.space 1
	.ascii "uLE_param_u64_C"
	.space 1
	.ascii "LE_var_u64_C"
	.space 1
	.ascii "uLE_var_u64_C"
	.space 1
	.ascii "LE_param_u64_i64"
	.space 1
	.ascii "uLE_param_u64_i64"
	.space 1
	.ascii "LE_var_u64_i64"
	.space 1
	.ascii "uLE_var_u64_i64"
	.space 1
	.ascii "LE_param_u64_I"
	.space 1
	.ascii "uLE_param_u64_I"
	.space 1
	.ascii "LE_var_u64_I"
	.space 1
	.ascii "uLE_var_u64_I"
	.space 1
	.ascii "LE_param_u64_i32"
	.space 1
	.ascii "uLE_param_u64_i32"
	.space 1
	.ascii "LE_var_u64_i32"
	.space 1
	.ascii "uLE_var_u64_i32"
	.space 1
	.ascii "LE_param_u64_i16"
	.space 1
	.ascii "uLE_param_u64_i16"
	.space 1
	.ascii "LE_var_u64_i16"
	.space 1
	.ascii "uLE_var_u64_i16"
	.space 1
	.ascii "LE_param_u64_LC"
	.space 1
	.ascii "uLE_param_u64_LC"
	.space 1
	.ascii "LE_var_u64_LC"
	.space 1
	.ascii "uLE_var_u64_LC"
	.space 1
	.ascii "LE_param_u64_u64"
	.space 1
	.ascii "uLE_param_u64_u64"
	.space 1
	.ascii "LE_var_u64_u64"
	.space 1
	.ascii "uLE_var_u64_u64"
	.space 1
	.ascii "LE_param_u64_u16"
	.space 1
	.ascii "uLE_param_u64_u16"
	.space 1
	.ascii "LE_var_u64_u16"
	.space 1
	.ascii "uLE_var_u64_u16"
	.space 1
	.ascii "LE_param_u16_u32"
	.space 1
	.ascii "uLE_param_u16_u32"
	.space 1
	.ascii "LE_var_u16_u32"
	.space 1
	.ascii "uLE_var_u16_u32"
	.space 1
	.ascii "LE_param_u16_L"
	.space 1
	.ascii "uLE_param_u16_L"
	.space 1
	.ascii "LE_var_u16_L"
	.space 1
	.ascii "uLE_var_u16_L"
	.space 1
	.ascii "LE_param_u16_i8"
	.space 1
	.ascii "uLE_param_u16_i8"
	.space 1
	.ascii "LE_var_u16_i8"
	.space 1
	.ascii "uLE_var_u16_i8"
	.space 1
	.ascii "LE_param_u16_u8"
	.space 1
	.ascii "uLE_param_u16_u8"
	.space 1
	.ascii "LE_var_u16_u8"
	.space 1
	.ascii "uLE_var_u16_u8"
	.space 1
	.ascii "LE_param_u16_C"
	.space 1
	.ascii "uLE_param_u16_C"
	.space 1
	.ascii "LE_var_u16_C"
	.space 1
	.ascii "uLE_var_u16_C"
	.space 1
	.ascii "LE_param_u16_i64"
	.space 1
	.ascii "uLE_param_u16_i64"
	.space 1
	.ascii "LE_var_u16_i64"
	.space 1
	.ascii "uLE_var_u16_i64"
	.space 1
	.ascii "LE_param_u16_I"
	.space 1
	.ascii "uLE_param_u16_I"
	.space 1
	.ascii "LE_var_u16_I"
	.space 1
	.ascii "uLE_var_u16_I"
	.space 1
	.ascii "LE_param_u16_i32"
	.space 1
	.ascii "uLE_param_u16_i32"
	.space 1
	.ascii "LE_var_u16_i32"
	.space 1
	.ascii "uLE_var_u16_i32"
	.space 1
	.ascii "LE_param_u16_i16"
	.space 1
	.ascii "uLE_param_u16_i16"
	.space 1
	.ascii "LE_var_u16_i16"
	.space 1
	.ascii "uLE_var_u16_i16"
	.space 1
	.ascii "LE_param_u16_LC"
	.space 1
	.ascii "uLE_param_u16_LC"
	.space 1
	.ascii "LE_var_u16_LC"
	.space 1
	.ascii "uLE_var_u16_LC"
	.space 1
	.ascii "LE_param_u16_u64"
	.space 1
	.ascii "uLE_param_u16_u64"
	.space 1
	.ascii "LE_var_u16_u64"
	.space 1
	.ascii "uLE_var_u16_u64"
	.space 1
	.ascii "LE_param_u16_u16"
	.space 1
	.ascii "uLE_param_u16_u16"
	.space 1
	.ascii "LE_var_u16_u16"
	.space 1
	.ascii "uLE_var_u16_u16"
	.space 3
	.quad	_LE_M3
	.quad	_L_1
	.quad	_LE__LE_param_u32_u32
	.quad	_L_1+6
	.quad	_LE__uLE_param_u32_u32
	.quad	_L_1+23
	.quad	_LE__LE_var_u32_u32
	.quad	_L_1+41
	.quad	_LE__uLE_var_u32_u32
	.quad	_L_1+56
	.quad	_LE__LE_param_u32_L
	.quad	_L_1+72
	.quad	_LE__uLE_param_u32_L
	.quad	_L_1+87
	.quad	_LE__LE_var_u32_L
	.quad	_L_1+103
	.quad	_LE__uLE_var_u32_L
	.quad	_L_1+116
	.quad	_LE__LE_param_u32_i8
	.quad	_L_1+130
	.quad	_LE__uLE_param_u32_i8
	.quad	_L_1+146
	.quad	_LE__LE_var_u32_i8
	.quad	_L_1+163
	.quad	_LE__uLE_var_u32_i8
	.quad	_L_1+177
	.quad	_LE__LE_param_u32_u8
	.quad	_L_1+192
	.quad	_LE__uLE_param_u32_u8
	.quad	_L_1+208
	.quad	_LE__LE_var_u32_u8
	.quad	_L_1+225
	.quad	_LE__uLE_var_u32_u8
	.quad	_L_1+239
	.quad	_LE__LE_param_u32_C
	.quad	_L_1+254
	.quad	_LE__uLE_param_u32_C
	.quad	_L_1+269
	.quad	_LE__LE_var_u32_C
	.quad	_L_1+285
	.quad	_LE__uLE_var_u32_C
	.quad	_L_1+298
	.quad	_LE__LE_param_u32_i64
	.quad	_L_1+312
	.quad	_LE__uLE_param_u32_i64
	.quad	_L_1+329
	.quad	_LE__LE_var_u32_i64
	.quad	_L_1+347
	.quad	_LE__uLE_var_u32_i64
	.quad	_L_1+362
	.quad	_LE__LE_param_u32_I
	.quad	_L_1+378
	.quad	_LE__uLE_param_u32_I
	.quad	_L_1+393
	.quad	_LE__LE_var_u32_I
	.quad	_L_1+409
	.quad	_LE__uLE_var_u32_I
	.quad	_L_1+422
	.quad	_LE__LE_param_u32_i32
	.quad	_L_1+436
	.quad	_LE__uLE_param_u32_i32
	.quad	_L_1+453
	.quad	_LE__LE_var_u32_i32
	.quad	_L_1+471
	.quad	_LE__uLE_var_u32_i32
	.quad	_L_1+486
	.quad	_LE__LE_param_u32_i16
	.quad	_L_1+502
	.quad	_LE__uLE_param_u32_i16
	.quad	_L_1+519
	.quad	_LE__LE_var_u32_i16
	.quad	_L_1+537
	.quad	_LE__uLE_var_u32_i16
	.quad	_L_1+552
	.quad	_LE__LE_param_u32_LC
	.quad	_L_1+568
	.quad	_LE__uLE_param_u32_LC
	.quad	_L_1+584
	.quad	_LE__LE_var_u32_LC
	.quad	_L_1+601
	.quad	_LE__uLE_var_u32_LC
	.quad	_L_1+615
	.quad	_LE__LE_param_u32_u64
	.quad	_L_1+630
	.quad	_LE__uLE_param_u32_u64
	.quad	_L_1+647
	.quad	_LE__LE_var_u32_u64
	.quad	_L_1+665
	.quad	_LE__uLE_var_u32_u64
	.quad	_L_1+680
	.quad	_LE__LE_param_u32_u16
	.quad	_L_1+696
	.quad	_LE__uLE_param_u32_u16
	.quad	_L_1+713
	.quad	_LE__LE_var_u32_u16
	.quad	_L_1+731
	.quad	_LE__uLE_var_u32_u16
	.quad	_L_1+746
	.quad	_LE__LE_param_L_u32
	.quad	_L_1+762
	.quad	_LE__uLE_param_L_u32
	.quad	_L_1+777
	.quad	_LE__LE_var_L_u32
	.quad	_L_1+793
	.quad	_LE__uLE_var_L_u32
	.quad	_L_1+806
	.quad	_LE__LE_param_L_L
	.quad	_L_1+820
	.quad	_LE__uLE_param_L_L
	.quad	_L_1+833
	.quad	_LE__LE_var_L_L
	.quad	_L_1+847
	.quad	_LE__uLE_var_L_L
	.quad	_L_1+858
	.quad	_LE__LE_param_L_i8
	.quad	_L_1+870
	.quad	_LE__uLE_param_L_i8
	.quad	_L_1+884
	.quad	_LE__LE_var_L_i8
	.quad	_L_1+899
	.quad	_LE__uLE_var_L_i8
	.quad	_L_1+911
	.quad	_LE__LE_param_L_u8
	.quad	_L_1+924
	.quad	_LE__uLE_param_L_u8
	.quad	_L_1+938
	.quad	_LE__LE_var_L_u8
	.quad	_L_1+953
	.quad	_LE__uLE_var_L_u8
	.quad	_L_1+965
	.quad	_LE__LE_param_L_C
	.quad	_L_1+978
	.quad	_LE__uLE_param_L_C
	.quad	_L_1+991
	.quad	_LE__LE_var_L_C
	.quad	_L_1+1005
	.quad	_LE__uLE_var_L_C
	.quad	_L_1+1016
	.quad	_LE__LE_param_L_i64
	.quad	_L_1+1028
	.quad	_LE__uLE_param_L_i64
	.quad	_L_1+1043
	.quad	_LE__LE_var_L_i64
	.quad	_L_1+1059
	.quad	_LE__uLE_var_L_i64
	.quad	_L_1+1072
	.quad	_LE__LE_param_L_I
	.quad	_L_1+1086
	.quad	_LE__uLE_param_L_I
	.quad	_L_1+1099
	.quad	_LE__LE_var_L_I
	.quad	_L_1+1113
	.quad	_LE__uLE_var_L_I
	.quad	_L_1+1124
	.quad	_LE__LE_param_L_i32
	.quad	_L_1+1136
	.quad	_LE__uLE_param_L_i32
	.quad	_L_1+1151
	.quad	_LE__LE_var_L_i32
	.quad	_L_1+1167
	.quad	_LE__uLE_var_L_i32
	.quad	_L_1+1180
	.quad	_LE__LE_param_L_i16
	.quad	_L_1+1194
	.quad	_LE__uLE_param_L_i16
	.quad	_L_1+1209
	.quad	_LE__LE_var_L_i16
	.quad	_L_1+1225
	.quad	_LE__uLE_var_L_i16
	.quad	_L_1+1238
	.quad	_LE__LE_param_L_LC
	.quad	_L_1+1252
	.quad	_LE__uLE_param_L_LC
	.quad	_L_1+1266
	.quad	_LE__LE_var_L_LC
	.quad	_L_1+1281
	.quad	_LE__uLE_var_L_LC
	.quad	_L_1+1293
	.quad	_LE__LE_param_L_u64
	.quad	_L_1+1306
	.quad	_LE__uLE_param_L_u64
	.quad	_L_1+1321
	.quad	_LE__LE_var_L_u64
	.quad	_L_1+1337
	.quad	_LE__uLE_var_L_u64
	.quad	_L_1+1350
	.quad	_LE__LE_param_L_u16
	.quad	_L_1+1364
	.quad	_LE__uLE_param_L_u16
	.quad	_L_1+1379
	.quad	_LE__LE_var_L_u16
	.quad	_L_1+1395
	.quad	_LE__uLE_var_L_u16
	.quad	_L_1+1408
	.quad	_LE__LE_param_i8_u32
	.quad	_L_1+1422
	.quad	_LE__uLE_param_i8_u32
	.quad	_L_1+1438
	.quad	_LE__LE_var_i8_u32
	.quad	_L_1+1455
	.quad	_LE__uLE_var_i8_u32
	.quad	_L_1+1469
	.quad	_LE__LE_param_i8_L
	.quad	_L_1+1484
	.quad	_LE__uLE_param_i8_L
	.quad	_L_1+1498
	.quad	_LE__LE_var_i8_L
	.quad	_L_1+1513
	.quad	_LE__uLE_var_i8_L
	.quad	_L_1+1525
	.quad	_LE__LE_param_i8_i8
	.quad	_L_1+1538
	.quad	_LE__uLE_param_i8_i8
	.quad	_L_1+1553
	.quad	_LE__LE_var_i8_i8
	.quad	_L_1+1569
	.quad	_LE__uLE_var_i8_i8
	.quad	_L_1+1582
	.quad	_LE__LE_param_i8_u8
	.quad	_L_1+1596
	.quad	_LE__uLE_param_i8_u8
	.quad	_L_1+1611
	.quad	_LE__LE_var_i8_u8
	.quad	_L_1+1627
	.quad	_LE__uLE_var_i8_u8
	.quad	_L_1+1640
	.quad	_LE__LE_param_i8_C
	.quad	_L_1+1654
	.quad	_LE__uLE_param_i8_C
	.quad	_L_1+1668
	.quad	_LE__LE_var_i8_C
	.quad	_L_1+1683
	.quad	_LE__uLE_var_i8_C
	.quad	_L_1+1695
	.quad	_LE__LE_param_i8_i64
	.quad	_L_1+1708
	.quad	_LE__uLE_param_i8_i64
	.quad	_L_1+1724
	.quad	_LE__LE_var_i8_i64
	.quad	_L_1+1741
	.quad	_LE__uLE_var_i8_i64
	.quad	_L_1+1755
	.quad	_LE__LE_param_i8_I
	.quad	_L_1+1770
	.quad	_LE__uLE_param_i8_I
	.quad	_L_1+1784
	.quad	_LE__LE_var_i8_I
	.quad	_L_1+1799
	.quad	_LE__uLE_var_i8_I
	.quad	_L_1+1811
	.quad	_LE__LE_param_i8_i32
	.quad	_L_1+1824
	.quad	_LE__uLE_param_i8_i32
	.quad	_L_1+1840
	.quad	_LE__LE_var_i8_i32
	.quad	_L_1+1857
	.quad	_LE__uLE_var_i8_i32
	.quad	_L_1+1871
	.quad	_LE__LE_param_i8_i16
	.quad	_L_1+1886
	.quad	_LE__uLE_param_i8_i16
	.quad	_L_1+1902
	.quad	_LE__LE_var_i8_i16
	.quad	_L_1+1919
	.quad	_LE__uLE_var_i8_i16
	.quad	_L_1+1933
	.quad	_LE__LE_param_i8_LC
	.quad	_L_1+1948
	.quad	_LE__uLE_param_i8_LC
	.quad	_L_1+1963
	.quad	_LE__LE_var_i8_LC
	.quad	_L_1+1979
	.quad	_LE__uLE_var_i8_LC
	.quad	_L_1+1992
	.quad	_LE__LE_param_i8_u64
	.quad	_L_1+2006
	.quad	_LE__uLE_param_i8_u64
	.quad	_L_1+2022
	.quad	_LE__LE_var_i8_u64
	.quad	_L_1+2039
	.quad	_LE__uLE_var_i8_u64
	.quad	_L_1+2053
	.quad	_LE__LE_param_i8_u16
	.quad	_L_1+2068
	.quad	_LE__uLE_param_i8_u16
	.quad	_L_1+2084
	.quad	_LE__LE_var_i8_u16
	.quad	_L_1+2101
	.quad	_LE__uLE_var_i8_u16
	.quad	_L_1+2115
	.quad	_LE__LE_param_u8_u32
	.quad	_L_1+2130
	.quad	_LE__uLE_param_u8_u32
	.quad	_L_1+2146
	.quad	_LE__LE_var_u8_u32
	.quad	_L_1+2163
	.quad	_LE__uLE_var_u8_u32
	.quad	_L_1+2177
	.quad	_LE__LE_param_u8_L
	.quad	_L_1+2192
	.quad	_LE__uLE_param_u8_L
	.quad	_L_1+2206
	.quad	_LE__LE_var_u8_L
	.quad	_L_1+2221
	.quad	_LE__uLE_var_u8_L
	.quad	_L_1+2233
	.quad	_LE__LE_param_u8_i8
	.quad	_L_1+2246
	.quad	_LE__uLE_param_u8_i8
	.quad	_L_1+2261
	.quad	_LE__LE_var_u8_i8
	.quad	_L_1+2277
	.quad	_LE__uLE_var_u8_i8
	.quad	_L_1+2290
	.quad	_LE__LE_param_u8_u8
	.quad	_L_1+2304
	.quad	_LE__uLE_param_u8_u8
	.quad	_L_1+2319
	.quad	_LE__LE_var_u8_u8
	.quad	_L_1+2335
	.quad	_LE__uLE_var_u8_u8
	.quad	_L_1+2348
	.quad	_LE__LE_param_u8_C
	.quad	_L_1+2362
	.quad	_LE__uLE_param_u8_C
	.quad	_L_1+2376
	.quad	_LE__LE_var_u8_C
	.quad	_L_1+2391
	.quad	_LE__uLE_var_u8_C
	.quad	_L_1+2403
	.quad	_LE__LE_param_u8_i64
	.quad	_L_1+2416
	.quad	_LE__uLE_param_u8_i64
	.quad	_L_1+2432
	.quad	_LE__LE_var_u8_i64
	.quad	_L_1+2449
	.quad	_LE__uLE_var_u8_i64
	.quad	_L_1+2463
	.quad	_LE__LE_param_u8_I
	.quad	_L_1+2478
	.quad	_LE__uLE_param_u8_I
	.quad	_L_1+2492
	.quad	_LE__LE_var_u8_I
	.quad	_L_1+2507
	.quad	_LE__uLE_var_u8_I
	.quad	_L_1+2519
	.quad	_LE__LE_param_u8_i32
	.quad	_L_1+2532
	.quad	_LE__uLE_param_u8_i32
	.quad	_L_1+2548
	.quad	_LE__LE_var_u8_i32
	.quad	_L_1+2565
	.quad	_LE__uLE_var_u8_i32
	.quad	_L_1+2579
	.quad	_LE__LE_param_u8_i16
	.quad	_L_1+2594
	.quad	_LE__uLE_param_u8_i16
	.quad	_L_1+2610
	.quad	_LE__LE_var_u8_i16
	.quad	_L_1+2627
	.quad	_LE__uLE_var_u8_i16
	.quad	_L_1+2641
	.quad	_LE__LE_param_u8_LC
	.quad	_L_1+2656
	.quad	_LE__uLE_param_u8_LC
	.quad	_L_1+2671
	.quad	_LE__LE_var_u8_LC
	.quad	_L_1+2687
	.quad	_LE__uLE_var_u8_LC
	.quad	_L_1+2700
	.quad	_LE__LE_param_u8_u64
	.quad	_L_1+2714
	.quad	_LE__uLE_param_u8_u64
	.quad	_L_1+2730
	.quad	_LE__LE_var_u8_u64
	.quad	_L_1+2747
	.quad	_LE__uLE_var_u8_u64
	.quad	_L_1+2761
	.quad	_LE__LE_param_u8_u16
	.quad	_L_1+2776
	.quad	_LE__uLE_param_u8_u16
	.quad	_L_1+2792
	.quad	_LE__LE_var_u8_u16
	.quad	_L_1+2809
	.quad	_LE__uLE_var_u8_u16
	.quad	_L_1+2823
	.quad	_LE__LE_param_f32_f32
	.quad	_L_1+2838
	.quad	_LE__LE_var_f32_f32
	.quad	_L_1+2855
	.quad	_LE__LE_param_C_u32
	.quad	_L_1+2870
	.quad	_LE__uLE_param_C_u32
	.quad	_L_1+2885
	.quad	_LE__LE_var_C_u32
	.quad	_L_1+2901
	.quad	_LE__uLE_var_C_u32
	.quad	_L_1+2914
	.quad	_LE__LE_param_C_L
	.quad	_L_1+2928
	.quad	_LE__uLE_param_C_L
	.quad	_L_1+2941
	.quad	_LE__LE_var_C_L
	.quad	_L_1+2955
	.quad	_LE__uLE_var_C_L
	.quad	_L_1+2966
	.quad	_LE__LE_param_C_i8
	.quad	_L_1+2978
	.quad	_LE__uLE_param_C_i8
	.quad	_L_1+2992
	.quad	_LE__LE_var_C_i8
	.quad	_L_1+3007
	.quad	_LE__uLE_var_C_i8
	.quad	_L_1+3019
	.quad	_LE__LE_param_C_u8
	.quad	_L_1+3032
	.quad	_LE__uLE_param_C_u8
	.quad	_L_1+3046
	.quad	_LE__LE_var_C_u8
	.quad	_L_1+3061
	.quad	_LE__uLE_var_C_u8
	.quad	_L_1+3073
	.quad	_LE__LE_param_C_C
	.quad	_L_1+3086
	.quad	_LE__uLE_param_C_C
	.quad	_L_1+3099
	.quad	_LE__LE_var_C_C
	.quad	_L_1+3113
	.quad	_LE__uLE_var_C_C
	.quad	_L_1+3124
	.quad	_LE__LE_param_C_i64
	.quad	_L_1+3136
	.quad	_LE__uLE_param_C_i64
	.quad	_L_1+3151
	.quad	_LE__LE_var_C_i64
	.quad	_L_1+3167
	.quad	_LE__uLE_var_C_i64
	.quad	_L_1+3180
	.quad	_LE__LE_param_C_I
	.quad	_L_1+3194
	.quad	_LE__uLE_param_C_I
	.quad	_L_1+3207
	.quad	_LE__LE_var_C_I
	.quad	_L_1+3221
	.quad	_LE__uLE_var_C_I
	.quad	_L_1+3232
	.quad	_LE__LE_param_C_i32
	.quad	_L_1+3244
	.quad	_LE__uLE_param_C_i32
	.quad	_L_1+3259
	.quad	_LE__LE_var_C_i32
	.quad	_L_1+3275
	.quad	_LE__uLE_var_C_i32
	.quad	_L_1+3288
	.quad	_LE__LE_param_C_i16
	.quad	_L_1+3302
	.quad	_LE__uLE_param_C_i16
	.quad	_L_1+3317
	.quad	_LE__LE_var_C_i16
	.quad	_L_1+3333
	.quad	_LE__uLE_var_C_i16
	.quad	_L_1+3346
	.quad	_LE__LE_param_C_LC
	.quad	_L_1+3360
	.quad	_LE__uLE_param_C_LC
	.quad	_L_1+3374
	.quad	_LE__LE_var_C_LC
	.quad	_L_1+3389
	.quad	_LE__uLE_var_C_LC
	.quad	_L_1+3401
	.quad	_LE__LE_param_C_u64
	.quad	_L_1+3414
	.quad	_LE__uLE_param_C_u64
	.quad	_L_1+3429
	.quad	_LE__LE_var_C_u64
	.quad	_L_1+3445
	.quad	_LE__uLE_var_C_u64
	.quad	_L_1+3458
	.quad	_LE__LE_param_C_u16
	.quad	_L_1+3472
	.quad	_LE__uLE_param_C_u16
	.quad	_L_1+3487
	.quad	_LE__LE_var_C_u16
	.quad	_L_1+3503
	.quad	_LE__uLE_var_C_u16
	.quad	_L_1+3516
	.quad	_LE__LE_param_i64_u32
	.quad	_L_1+3530
	.quad	_LE__uLE_param_i64_u32
	.quad	_L_1+3547
	.quad	_LE__LE_var_i64_u32
	.quad	_L_1+3565
	.quad	_LE__uLE_var_i64_u32
	.quad	_L_1+3580
	.quad	_LE__LE_param_i64_L
	.quad	_L_1+3596
	.quad	_LE__uLE_param_i64_L
	.quad	_L_1+3611
	.quad	_LE__LE_var_i64_L
	.quad	_L_1+3627
	.quad	_LE__uLE_var_i64_L
	.quad	_L_1+3640
	.quad	_LE__LE_param_i64_i8
	.quad	_L_1+3654
	.quad	_LE__uLE_param_i64_i8
	.quad	_L_1+3670
	.quad	_LE__LE_var_i64_i8
	.quad	_L_1+3687
	.quad	_LE__uLE_var_i64_i8
	.quad	_L_1+3701
	.quad	_LE__LE_param_i64_u8
	.quad	_L_1+3716
	.quad	_LE__uLE_param_i64_u8
	.quad	_L_1+3732
	.quad	_LE__LE_var_i64_u8
	.quad	_L_1+3749
	.quad	_LE__uLE_var_i64_u8
	.quad	_L_1+3763
	.quad	_LE__LE_param_i64_C
	.quad	_L_1+3778
	.quad	_LE__uLE_param_i64_C
	.quad	_L_1+3793
	.quad	_LE__LE_var_i64_C
	.quad	_L_1+3809
	.quad	_LE__uLE_var_i64_C
	.quad	_L_1+3822
	.quad	_LE__LE_param_i64_i64
	.quad	_L_1+3836
	.quad	_LE__uLE_param_i64_i64
	.quad	_L_1+3853
	.quad	_LE__LE_var_i64_i64
	.quad	_L_1+3871
	.quad	_LE__uLE_var_i64_i64
	.quad	_L_1+3886
	.quad	_LE__LE_param_i64_I
	.quad	_L_1+3902
	.quad	_LE__uLE_param_i64_I
	.quad	_L_1+3917
	.quad	_LE__LE_var_i64_I
	.quad	_L_1+3933
	.quad	_LE__uLE_var_i64_I
	.quad	_L_1+3946
	.quad	_LE__LE_param_i64_i32
	.quad	_L_1+3960
	.quad	_LE__uLE_param_i64_i32
	.quad	_L_1+3977
	.quad	_LE__LE_var_i64_i32
	.quad	_L_1+3995
	.quad	_LE__uLE_var_i64_i32
	.quad	_L_1+4010
	.quad	_LE__LE_param_i64_i16
	.quad	_L_1+4026
	.quad	_LE__uLE_param_i64_i16
	.quad	_L_1+4043
	.quad	_LE__LE_var_i64_i16
	.quad	_L_1+4061
	.quad	_LE__uLE_var_i64_i16
	.quad	_L_1+4076
	.quad	_LE__LE_param_i64_LC
	.quad	_L_1+4092
	.quad	_LE__uLE_param_i64_LC
	.quad	_L_1+4108
	.quad	_LE__LE_var_i64_LC
	.quad	_L_1+4125
	.quad	_LE__uLE_var_i64_LC
	.quad	_L_1+4139
	.quad	_LE__LE_param_i64_u64
	.quad	_L_1+4154
	.quad	_LE__uLE_param_i64_u64
	.quad	_L_1+4171
	.quad	_LE__LE_var_i64_u64
	.quad	_L_1+4189
	.quad	_LE__uLE_var_i64_u64
	.quad	_L_1+4204
	.quad	_LE__LE_param_i64_u16
	.quad	_L_1+4220
	.quad	_LE__uLE_param_i64_u16
	.quad	_L_1+4237
	.quad	_LE__LE_var_i64_u16
	.quad	_L_1+4255
	.quad	_LE__uLE_var_i64_u16
	.quad	_L_1+4270
	.quad	_LE__LE_param_I_u32
	.quad	_L_1+4286
	.quad	_LE__uLE_param_I_u32
	.quad	_L_1+4301
	.quad	_LE__LE_var_I_u32
	.quad	_L_1+4317
	.quad	_LE__uLE_var_I_u32
	.quad	_L_1+4330
	.quad	_LE__LE_param_I_L
	.quad	_L_1+4344
	.quad	_LE__uLE_param_I_L
	.quad	_L_1+4357
	.quad	_LE__LE_var_I_L
	.quad	_L_1+4371
	.quad	_LE__uLE_var_I_L
	.quad	_L_1+4382
	.quad	_LE__LE_param_I_i8
	.quad	_L_1+4394
	.quad	_LE__uLE_param_I_i8
	.quad	_L_1+4408
	.quad	_LE__LE_var_I_i8
	.quad	_L_1+4423
	.quad	_LE__uLE_var_I_i8
	.quad	_L_1+4435
	.quad	_LE__LE_param_I_u8
	.quad	_L_1+4448
	.quad	_LE__uLE_param_I_u8
	.quad	_L_1+4462
	.quad	_LE__LE_var_I_u8
	.quad	_L_1+4477
	.quad	_LE__uLE_var_I_u8
	.quad	_L_1+4489
	.quad	_LE__LE_param_I_C
	.quad	_L_1+4502
	.quad	_LE__uLE_param_I_C
	.quad	_L_1+4515
	.quad	_LE__LE_var_I_C
	.quad	_L_1+4529
	.quad	_LE__uLE_var_I_C
	.quad	_L_1+4540
	.quad	_LE__LE_param_I_i64
	.quad	_L_1+4552
	.quad	_LE__uLE_param_I_i64
	.quad	_L_1+4567
	.quad	_LE__LE_var_I_i64
	.quad	_L_1+4583
	.quad	_LE__uLE_var_I_i64
	.quad	_L_1+4596
	.quad	_LE__LE_param_I_I
	.quad	_L_1+4610
	.quad	_LE__uLE_param_I_I
	.quad	_L_1+4623
	.quad	_LE__LE_var_I_I
	.quad	_L_1+4637
	.quad	_LE__uLE_var_I_I
	.quad	_L_1+4648
	.quad	_LE__LE_param_I_i32
	.quad	_L_1+4660
	.quad	_LE__uLE_param_I_i32
	.quad	_L_1+4675
	.quad	_LE__LE_var_I_i32
	.quad	_L_1+4691
	.quad	_LE__uLE_var_I_i32
	.quad	_L_1+4704
	.quad	_LE__LE_param_I_i16
	.quad	_L_1+4718
	.quad	_LE__uLE_param_I_i16
	.quad	_L_1+4733
	.quad	_LE__LE_var_I_i16
	.quad	_L_1+4749
	.quad	_LE__uLE_var_I_i16
	.quad	_L_1+4762
	.quad	_LE__LE_param_I_LC
	.quad	_L_1+4776
	.quad	_LE__uLE_param_I_LC
	.quad	_L_1+4790
	.quad	_LE__LE_var_I_LC
	.quad	_L_1+4805
	.quad	_LE__uLE_var_I_LC
	.quad	_L_1+4817
	.quad	_LE__LE_param_I_u64
	.quad	_L_1+4830
	.quad	_LE__uLE_param_I_u64
	.quad	_L_1+4845
	.quad	_LE__LE_var_I_u64
	.quad	_L_1+4861
	.quad	_LE__uLE_var_I_u64
	.quad	_L_1+4874
	.quad	_LE__LE_param_I_u16
	.quad	_L_1+4888
	.quad	_LE__uLE_param_I_u16
	.quad	_L_1+4903
	.quad	_LE__LE_var_I_u16
	.quad	_L_1+4919
	.quad	_LE__uLE_var_I_u16
	.quad	_L_1+4932
	.quad	_LE__LE_param_i32_u32
	.quad	_L_1+4946
	.quad	_LE__uLE_param_i32_u32
	.quad	_L_1+4963
	.quad	_LE__LE_var_i32_u32
	.quad	_L_1+4981
	.quad	_LE__uLE_var_i32_u32
	.quad	_L_1+4996
	.quad	_LE__LE_param_i32_L
	.quad	_L_1+5012
	.quad	_LE__uLE_param_i32_L
	.quad	_L_1+5027
	.quad	_LE__LE_var_i32_L
	.quad	_L_1+5043
	.quad	_LE__uLE_var_i32_L
	.quad	_L_1+5056
	.quad	_LE__LE_param_i32_i8
	.quad	_L_1+5070
	.quad	_LE__uLE_param_i32_i8
	.quad	_L_1+5086
	.quad	_LE__LE_var_i32_i8
	.quad	_L_1+5103
	.quad	_LE__uLE_var_i32_i8
	.quad	_L_1+5117
	.quad	_LE__LE_param_i32_u8
	.quad	_L_1+5132
	.quad	_LE__uLE_param_i32_u8
	.quad	_L_1+5148
	.quad	_LE__LE_var_i32_u8
	.quad	_L_1+5165
	.quad	_LE__uLE_var_i32_u8
	.quad	_L_1+5179
	.quad	_LE__LE_param_i32_C
	.quad	_L_1+5194
	.quad	_LE__uLE_param_i32_C
	.quad	_L_1+5209
	.quad	_LE__LE_var_i32_C
	.quad	_L_1+5225
	.quad	_LE__uLE_var_i32_C
	.quad	_L_1+5238
	.quad	_LE__LE_param_i32_i64
	.quad	_L_1+5252
	.quad	_LE__uLE_param_i32_i64
	.quad	_L_1+5269
	.quad	_LE__LE_var_i32_i64
	.quad	_L_1+5287
	.quad	_LE__uLE_var_i32_i64
	.quad	_L_1+5302
	.quad	_LE__LE_param_i32_I
	.quad	_L_1+5318
	.quad	_LE__uLE_param_i32_I
	.quad	_L_1+5333
	.quad	_LE__LE_var_i32_I
	.quad	_L_1+5349
	.quad	_LE__uLE_var_i32_I
	.quad	_L_1+5362
	.quad	_LE__LE_param_i32_i32
	.quad	_L_1+5376
	.quad	_LE__uLE_param_i32_i32
	.quad	_L_1+5393
	.quad	_LE__LE_var_i32_i32
	.quad	_L_1+5411
	.quad	_LE__uLE_var_i32_i32
	.quad	_L_1+5426
	.quad	_LE__LE_param_i32_i16
	.quad	_L_1+5442
	.quad	_LE__uLE_param_i32_i16
	.quad	_L_1+5459
	.quad	_LE__LE_var_i32_i16
	.quad	_L_1+5477
	.quad	_LE__uLE_var_i32_i16
	.quad	_L_1+5492
	.quad	_LE__LE_param_i32_LC
	.quad	_L_1+5508
	.quad	_LE__uLE_param_i32_LC
	.quad	_L_1+5524
	.quad	_LE__LE_var_i32_LC
	.quad	_L_1+5541
	.quad	_LE__uLE_var_i32_LC
	.quad	_L_1+5555
	.quad	_LE__LE_param_i32_u64
	.quad	_L_1+5570
	.quad	_LE__uLE_param_i32_u64
	.quad	_L_1+5587
	.quad	_LE__LE_var_i32_u64
	.quad	_L_1+5605
	.quad	_LE__uLE_var_i32_u64
	.quad	_L_1+5620
	.quad	_LE__LE_param_i32_u16
	.quad	_L_1+5636
	.quad	_LE__uLE_param_i32_u16
	.quad	_L_1+5653
	.quad	_LE__LE_var_i32_u16
	.quad	_L_1+5671
	.quad	_LE__uLE_var_i32_u16
	.quad	_L_1+5686
	.quad	_LE__LE_param_i16_u32
	.quad	_L_1+5702
	.quad	_LE__uLE_param_i16_u32
	.quad	_L_1+5719
	.quad	_LE__LE_var_i16_u32
	.quad	_L_1+5737
	.quad	_LE__uLE_var_i16_u32
	.quad	_L_1+5752
	.quad	_LE__LE_param_i16_L
	.quad	_L_1+5768
	.quad	_LE__uLE_param_i16_L
	.quad	_L_1+5783
	.quad	_LE__LE_var_i16_L
	.quad	_L_1+5799
	.quad	_LE__uLE_var_i16_L
	.quad	_L_1+5812
	.quad	_LE__LE_param_i16_i8
	.quad	_L_1+5826
	.quad	_LE__uLE_param_i16_i8
	.quad	_L_1+5842
	.quad	_LE__LE_var_i16_i8
	.quad	_L_1+5859
	.quad	_LE__uLE_var_i16_i8
	.quad	_L_1+5873
	.quad	_LE__LE_param_i16_u8
	.quad	_L_1+5888
	.quad	_LE__uLE_param_i16_u8
	.quad	_L_1+5904
	.quad	_LE__LE_var_i16_u8
	.quad	_L_1+5921
	.quad	_LE__uLE_var_i16_u8
	.quad	_L_1+5935
	.quad	_LE__LE_param_i16_C
	.quad	_L_1+5950
	.quad	_LE__uLE_param_i16_C
	.quad	_L_1+5965
	.quad	_LE__LE_var_i16_C
	.quad	_L_1+5981
	.quad	_LE__uLE_var_i16_C
	.quad	_L_1+5994
	.quad	_LE__LE_param_i16_i64
	.quad	_L_1+6008
	.quad	_LE__uLE_param_i16_i64
	.quad	_L_1+6025
	.quad	_LE__LE_var_i16_i64
	.quad	_L_1+6043
	.quad	_LE__uLE_var_i16_i64
	.quad	_L_1+6058
	.quad	_LE__LE_param_i16_I
	.quad	_L_1+6074
	.quad	_LE__uLE_param_i16_I
	.quad	_L_1+6089
	.quad	_LE__LE_var_i16_I
	.quad	_L_1+6105
	.quad	_LE__uLE_var_i16_I
	.quad	_L_1+6118
	.quad	_LE__LE_param_i16_i32
	.quad	_L_1+6132
	.quad	_LE__uLE_param_i16_i32
	.quad	_L_1+6149
	.quad	_LE__LE_var_i16_i32
	.quad	_L_1+6167
	.quad	_LE__uLE_var_i16_i32
	.quad	_L_1+6182
	.quad	_LE__LE_param_i16_i16
	.quad	_L_1+6198
	.quad	_LE__uLE_param_i16_i16
	.quad	_L_1+6215
	.quad	_LE__LE_var_i16_i16
	.quad	_L_1+6233
	.quad	_LE__uLE_var_i16_i16
	.quad	_L_1+6248
	.quad	_LE__LE_param_i16_LC
	.quad	_L_1+6264
	.quad	_LE__uLE_param_i16_LC
	.quad	_L_1+6280
	.quad	_LE__LE_var_i16_LC
	.quad	_L_1+6297
	.quad	_LE__uLE_var_i16_LC
	.quad	_L_1+6311
	.quad	_LE__LE_param_i16_u64
	.quad	_L_1+6326
	.quad	_LE__uLE_param_i16_u64
	.quad	_L_1+6343
	.quad	_LE__LE_var_i16_u64
	.quad	_L_1+6361
	.quad	_LE__uLE_var_i16_u64
	.quad	_L_1+6376
	.quad	_LE__LE_param_i16_u16
	.quad	_L_1+6392
	.quad	_LE__uLE_param_i16_u16
	.quad	_L_1+6409
	.quad	_LE__LE_var_i16_u16
	.quad	_L_1+6427
	.quad	_LE__uLE_var_i16_u16
	.quad	_L_1+6442
	.quad	_LE__LE_param_f64_f64
	.quad	_L_1+6458
	.quad	_LE__LE_var_f64_f64
	.quad	_L_1+6475
	.quad	_LE__LE_param_LC_u32
	.quad	_L_1+6490
	.quad	_LE__uLE_param_LC_u32
	.quad	_L_1+6506
	.quad	_LE__LE_var_LC_u32
	.quad	_L_1+6523
	.quad	_LE__uLE_var_LC_u32
	.quad	_L_1+6537
	.quad	_LE__LE_param_LC_L
	.quad	_L_1+6552
	.quad	_LE__uLE_param_LC_L
	.quad	_L_1+6566
	.quad	_LE__LE_var_LC_L
	.quad	_L_1+6581
	.quad	_LE__uLE_var_LC_L
	.quad	_L_1+6593
	.quad	_LE__LE_param_LC_i8
	.quad	_L_1+6606
	.quad	_LE__uLE_param_LC_i8
	.quad	_L_1+6621
	.quad	_LE__LE_var_LC_i8
	.quad	_L_1+6637
	.quad	_LE__uLE_var_LC_i8
	.quad	_L_1+6650
	.quad	_LE__LE_param_LC_u8
	.quad	_L_1+6664
	.quad	_LE__uLE_param_LC_u8
	.quad	_L_1+6679
	.quad	_LE__LE_var_LC_u8
	.quad	_L_1+6695
	.quad	_LE__uLE_var_LC_u8
	.quad	_L_1+6708
	.quad	_LE__LE_param_LC_C
	.quad	_L_1+6722
	.quad	_LE__uLE_param_LC_C
	.quad	_L_1+6736
	.quad	_LE__LE_var_LC_C
	.quad	_L_1+6751
	.quad	_LE__uLE_var_LC_C
	.quad	_L_1+6763
	.quad	_LE__LE_param_LC_i64
	.quad	_L_1+6776
	.quad	_LE__uLE_param_LC_i64
	.quad	_L_1+6792
	.quad	_LE__LE_var_LC_i64
	.quad	_L_1+6809
	.quad	_LE__uLE_var_LC_i64
	.quad	_L_1+6823
	.quad	_LE__LE_param_LC_I
	.quad	_L_1+6838
	.quad	_LE__uLE_param_LC_I
	.quad	_L_1+6852
	.quad	_LE__LE_var_LC_I
	.quad	_L_1+6867
	.quad	_LE__uLE_var_LC_I
	.quad	_L_1+6879
	.quad	_LE__LE_param_LC_i32
	.quad	_L_1+6892
	.quad	_LE__uLE_param_LC_i32
	.quad	_L_1+6908
	.quad	_LE__LE_var_LC_i32
	.quad	_L_1+6925
	.quad	_LE__uLE_var_LC_i32
	.quad	_L_1+6939
	.quad	_LE__LE_param_LC_i16
	.quad	_L_1+6954
	.quad	_LE__uLE_param_LC_i16
	.quad	_L_1+6970
	.quad	_LE__LE_var_LC_i16
	.quad	_L_1+6987
	.quad	_LE__uLE_var_LC_i16
	.quad	_L_1+7001
	.quad	_LE__LE_param_LC_LC
	.quad	_L_1+7016
	.quad	_LE__uLE_param_LC_LC
	.quad	_L_1+7031
	.quad	_LE__LE_var_LC_LC
	.quad	_L_1+7047
	.quad	_LE__uLE_var_LC_LC
	.quad	_L_1+7060
	.quad	_LE__LE_param_LC_u64
	.quad	_L_1+7074
	.quad	_LE__uLE_param_LC_u64
	.quad	_L_1+7090
	.quad	_LE__LE_var_LC_u64
	.quad	_L_1+7107
	.quad	_LE__uLE_var_LC_u64
	.quad	_L_1+7121
	.quad	_LE__LE_param_LC_u16
	.quad	_L_1+7136
	.quad	_LE__uLE_param_LC_u16
	.quad	_L_1+7152
	.quad	_LE__LE_var_LC_u16
	.quad	_L_1+7169
	.quad	_LE__uLE_var_LC_u16
	.quad	_L_1+7183
	.quad	_LE__LE_param_u64_u32
	.quad	_L_1+7198
	.quad	_LE__uLE_param_u64_u32
	.quad	_L_1+7215
	.quad	_LE__LE_var_u64_u32
	.quad	_L_1+7233
	.quad	_LE__uLE_var_u64_u32
	.quad	_L_1+7248
	.quad	_LE__LE_param_u64_L
	.quad	_L_1+7264
	.quad	_LE__uLE_param_u64_L
	.quad	_L_1+7279
	.quad	_LE__LE_var_u64_L
	.quad	_L_1+7295
	.quad	_LE__uLE_var_u64_L
	.quad	_L_1+7308
	.quad	_LE__LE_param_u64_i8
	.quad	_L_1+7322
	.quad	_LE__uLE_param_u64_i8
	.quad	_L_1+7338
	.quad	_LE__LE_var_u64_i8
	.quad	_L_1+7355
	.quad	_LE__uLE_var_u64_i8
	.quad	_L_1+7369
	.quad	_LE__LE_param_u64_u8
	.quad	_L_1+7384
	.quad	_LE__uLE_param_u64_u8
	.quad	_L_1+7400
	.quad	_LE__LE_var_u64_u8
	.quad	_L_1+7417
	.quad	_LE__uLE_var_u64_u8
	.quad	_L_1+7431
	.quad	_LE__LE_param_u64_C
	.quad	_L_1+7446
	.quad	_LE__uLE_param_u64_C
	.quad	_L_1+7461
	.quad	_LE__LE_var_u64_C
	.quad	_L_1+7477
	.quad	_LE__uLE_var_u64_C
	.quad	_L_1+7490
	.quad	_LE__LE_param_u64_i64
	.quad	_L_1+7504
	.quad	_LE__uLE_param_u64_i64
	.quad	_L_1+7521
	.quad	_LE__LE_var_u64_i64
	.quad	_L_1+7539
	.quad	_LE__uLE_var_u64_i64
	.quad	_L_1+7554
	.quad	_LE__LE_param_u64_I
	.quad	_L_1+7570
	.quad	_LE__uLE_param_u64_I
	.quad	_L_1+7585
	.quad	_LE__LE_var_u64_I
	.quad	_L_1+7601
	.quad	_LE__uLE_var_u64_I
	.quad	_L_1+7614
	.quad	_LE__LE_param_u64_i32
	.quad	_L_1+7628
	.quad	_LE__uLE_param_u64_i32
	.quad	_L_1+7645
	.quad	_LE__LE_var_u64_i32
	.quad	_L_1+7663
	.quad	_LE__uLE_var_u64_i32
	.quad	_L_1+7678
	.quad	_LE__LE_param_u64_i16
	.quad	_L_1+7694
	.quad	_LE__uLE_param_u64_i16
	.quad	_L_1+7711
	.quad	_LE__LE_var_u64_i16
	.quad	_L_1+7729
	.quad	_LE__uLE_var_u64_i16
	.quad	_L_1+7744
	.quad	_LE__LE_param_u64_LC
	.quad	_L_1+7760
	.quad	_LE__uLE_param_u64_LC
	.quad	_L_1+7776
	.quad	_LE__LE_var_u64_LC
	.quad	_L_1+7793
	.quad	_LE__uLE_var_u64_LC
	.quad	_L_1+7807
	.quad	_LE__LE_param_u64_u64
	.quad	_L_1+7822
	.quad	_LE__uLE_param_u64_u64
	.quad	_L_1+7839
	.quad	_LE__LE_var_u64_u64
	.quad	_L_1+7857
	.quad	_LE__uLE_var_u64_u64
	.quad	_L_1+7872
	.quad	_LE__LE_param_u64_u16
	.quad	_L_1+7888
	.quad	_LE__uLE_param_u64_u16
	.quad	_L_1+7905
	.quad	_LE__LE_var_u64_u16
	.quad	_L_1+7923
	.quad	_LE__uLE_var_u64_u16
	.quad	_L_1+7938
	.quad	_LE__LE_param_u16_u32
	.quad	_L_1+7954
	.quad	_LE__uLE_param_u16_u32
	.quad	_L_1+7971
	.quad	_LE__LE_var_u16_u32
	.quad	_L_1+7989
	.quad	_LE__uLE_var_u16_u32
	.quad	_L_1+8004
	.quad	_LE__LE_param_u16_L
	.quad	_L_1+8020
	.quad	_LE__uLE_param_u16_L
	.quad	_L_1+8035
	.quad	_LE__LE_var_u16_L
	.quad	_L_1+8051
	.quad	_LE__uLE_var_u16_L
	.quad	_L_1+8064
	.quad	_LE__LE_param_u16_i8
	.quad	_L_1+8078
	.quad	_LE__uLE_param_u16_i8
	.quad	_L_1+8094
	.quad	_LE__LE_var_u16_i8
	.quad	_L_1+8111
	.quad	_LE__uLE_var_u16_i8
	.quad	_L_1+8125
	.quad	_LE__LE_param_u16_u8
	.quad	_L_1+8140
	.quad	_LE__uLE_param_u16_u8
	.quad	_L_1+8156
	.quad	_LE__LE_var_u16_u8
	.quad	_L_1+8173
	.quad	_LE__uLE_var_u16_u8
	.quad	_L_1+8187
	.quad	_LE__LE_param_u16_C
	.quad	_L_1+8202
	.quad	_LE__uLE_param_u16_C
	.quad	_L_1+8217
	.quad	_LE__LE_var_u16_C
	.quad	_L_1+8233
	.quad	_LE__uLE_var_u16_C
	.quad	_L_1+8246
	.quad	_LE__LE_param_u16_i64
	.quad	_L_1+8260
	.quad	_LE__uLE_param_u16_i64
	.quad	_L_1+8277
	.quad	_LE__LE_var_u16_i64
	.quad	_L_1+8295
	.quad	_LE__uLE_var_u16_i64
	.quad	_L_1+8310
	.quad	_LE__LE_param_u16_I
	.quad	_L_1+8326
	.quad	_LE__uLE_param_u16_I
	.quad	_L_1+8341
	.quad	_LE__LE_var_u16_I
	.quad	_L_1+8357
	.quad	_LE__uLE_var_u16_I
	.quad	_L_1+8370
	.quad	_LE__LE_param_u16_i32
	.quad	_L_1+8384
	.quad	_LE__uLE_param_u16_i32
	.quad	_L_1+8401
	.quad	_LE__LE_var_u16_i32
	.quad	_L_1+8419
	.quad	_LE__uLE_var_u16_i32
	.quad	_L_1+8434
	.quad	_LE__LE_param_u16_i16
	.quad	_L_1+8450
	.quad	_LE__uLE_param_u16_i16
	.quad	_L_1+8467
	.quad	_LE__LE_var_u16_i16
	.quad	_L_1+8485
	.quad	_LE__uLE_var_u16_i16
	.quad	_L_1+8500
	.quad	_LE__LE_param_u16_LC
	.quad	_L_1+8516
	.quad	_LE__uLE_param_u16_LC
	.quad	_L_1+8532
	.quad	_LE__LE_var_u16_LC
	.quad	_L_1+8549
	.quad	_LE__uLE_var_u16_LC
	.quad	_L_1+8563
	.quad	_LE__LE_param_u16_u64
	.quad	_L_1+8578
	.quad	_LE__uLE_param_u16_u64
	.quad	_L_1+8595
	.quad	_LE__LE_var_u16_u64
	.quad	_L_1+8613
	.quad	_LE__uLE_var_u16_u64
	.quad	_L_1+8628
	.quad	_LE__LE_param_u16_u16
	.quad	_L_1+8644
	.quad	_LE__uLE_param_u16_u16
	.quad	_L_1+8661
	.quad	_LE__LE_var_u16_u16
	.quad	_L_1+8679
	.quad	_LE__uLE_var_u16_u16
	.quad	_L_1+8694
	.space 8
	.ascii "../AMD64_DARWIN/LE.m3"
	.space 3
	.data
	.align 5
_MM_LE:
	.quad	_L_1+18016
	.space 32
	.quad	_L_1+8712
	.space 24
	.quad	_MM_LE+208
	.space 8
	.quad	_LE_M3
	.quad	3
	.word	337
	.space 6
	.quad	338
	.quad	339
	.long	3161095930
	.long	1081427316
	.word	342
	.space 2
	.long	343
	.quad	344
	.quad	345
	.quad	346
	.long	1135455371
	.byte	93
	.byte	94
	.space 2
	.quad	351
	.long	352
	.space 28
	.quad	_LE_I3
	.quad	_MM_LE+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_LE+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_LE+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_LE+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
