	.text
.globl _LT__uLT_var_i64_i64
	.private_extern _LT__uLT_var_i64_i64
_LT__uLT_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_i64_i64
	.private_extern _LT__LT_var_i64_i64
_LT__LT_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_i64_i64
	.private_extern _LT__uLT_param_i64_i64
_LT__uLT_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_i64
	.private_extern _LT__LT_param_i64_i64
_LT__LT_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_LC
	.private_extern _LT__uLT_var_i64_LC
_LT__uLT_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_LC
	.private_extern _LT__LT_var_i64_LC
_LT__LT_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_LC
	.private_extern _LT__uLT_param_i64_LC
_LT__uLT_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_LC
	.private_extern _LT__LT_param_i64_LC
_LT__LT_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_u8
	.private_extern _LT__uLT_var_i64_u8
_LT__uLT_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_u8
	.private_extern _LT__LT_var_i64_u8
_LT__LT_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_u8
	.private_extern _LT__uLT_param_i64_u8
_LT__uLT_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_u8
	.private_extern _LT__LT_param_i64_u8
_LT__LT_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_I
	.private_extern _LT__uLT_var_i64_I
_LT__uLT_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_I
	.private_extern _LT__LT_var_i64_I
_LT__LT_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_I
	.private_extern _LT__uLT_param_i64_I
_LT__uLT_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_I
	.private_extern _LT__LT_param_i64_I
_LT__LT_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_i8
	.private_extern _LT__uLT_var_i64_i8
_LT__uLT_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_i8
	.private_extern _LT__LT_var_i64_i8
_LT__LT_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_i8
	.private_extern _LT__uLT_param_i64_i8
_LT__uLT_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_i8
	.private_extern _LT__LT_param_i64_i8
_LT__LT_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_u32
	.private_extern _LT__uLT_var_i64_u32
_LT__uLT_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_u32
	.private_extern _LT__LT_var_i64_u32
_LT__LT_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_u32
	.private_extern _LT__uLT_param_i64_u32
_LT__uLT_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_u32
	.private_extern _LT__LT_param_i64_u32
_LT__LT_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_C
	.private_extern _LT__uLT_var_i64_C
_LT__uLT_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_C
	.private_extern _LT__LT_var_i64_C
_LT__LT_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_C
	.private_extern _LT__uLT_param_i64_C
_LT__uLT_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_C
	.private_extern _LT__LT_param_i64_C
_LT__LT_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_u16
	.private_extern _LT__uLT_var_i64_u16
_LT__uLT_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_u16
	.private_extern _LT__LT_var_i64_u16
_LT__LT_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_u16
	.private_extern _LT__uLT_param_i64_u16
_LT__uLT_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_u16
	.private_extern _LT__LT_param_i64_u16
_LT__LT_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_u64
	.private_extern _LT__uLT_var_i64_u64
_LT__uLT_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_u64
	.private_extern _LT__LT_var_i64_u64
_LT__LT_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_u64
	.private_extern _LT__uLT_param_i64_u64
_LT__uLT_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_u64
	.private_extern _LT__LT_param_i64_u64
_LT__LT_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_L
	.private_extern _LT__uLT_var_i64_L
_LT__uLT_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_L
	.private_extern _LT__LT_var_i64_L
_LT__LT_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_L
	.private_extern _LT__uLT_param_i64_L
_LT__uLT_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_L
	.private_extern _LT__LT_param_i64_L
_LT__LT_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_i16
	.private_extern _LT__uLT_var_i64_i16
_LT__uLT_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_i16
	.private_extern _LT__LT_var_i64_i16
_LT__LT_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_i16
	.private_extern _LT__uLT_param_i64_i16
_LT__uLT_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_i16
	.private_extern _LT__LT_param_i64_i16
_LT__LT_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i64_i32
	.private_extern _LT__uLT_var_i64_i32
_LT__uLT_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i64_i32
	.private_extern _LT__LT_var_i64_i32
_LT__LT_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i64_i32
	.private_extern _LT__uLT_param_i64_i32
_LT__uLT_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i64_i32
	.private_extern _LT__LT_param_i64_i32
_LT__LT_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_i64
	.private_extern _LT__uLT_var_LC_i64
_LT__uLT_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_i64
	.private_extern _LT__LT_var_LC_i64
_LT__LT_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_i64
	.private_extern _LT__uLT_param_LC_i64
_LT__uLT_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_i64
	.private_extern _LT__LT_param_LC_i64
_LT__LT_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_LC
	.private_extern _LT__uLT_var_LC_LC
_LT__uLT_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_LC_LC
	.private_extern _LT__LT_var_LC_LC
_LT__LT_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_LC_LC
	.private_extern _LT__uLT_param_LC_LC
_LT__uLT_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_LC
	.private_extern _LT__LT_param_LC_LC
_LT__LT_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_u8
	.private_extern _LT__uLT_var_LC_u8
_LT__uLT_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_u8
	.private_extern _LT__LT_var_LC_u8
_LT__LT_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_u8
	.private_extern _LT__uLT_param_LC_u8
_LT__uLT_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_u8
	.private_extern _LT__LT_param_LC_u8
_LT__LT_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_I
	.private_extern _LT__uLT_var_LC_I
_LT__uLT_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_I
	.private_extern _LT__LT_var_LC_I
_LT__LT_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_I
	.private_extern _LT__uLT_param_LC_I
_LT__uLT_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_I
	.private_extern _LT__LT_param_LC_I
_LT__LT_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_i8
	.private_extern _LT__uLT_var_LC_i8
_LT__uLT_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_i8
	.private_extern _LT__LT_var_LC_i8
_LT__LT_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_i8
	.private_extern _LT__uLT_param_LC_i8
_LT__uLT_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_i8
	.private_extern _LT__LT_param_LC_i8
_LT__LT_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_u32
	.private_extern _LT__uLT_var_LC_u32
_LT__uLT_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_u32
	.private_extern _LT__LT_var_LC_u32
_LT__LT_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_u32
	.private_extern _LT__uLT_param_LC_u32
_LT__uLT_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_u32
	.private_extern _LT__LT_param_LC_u32
_LT__LT_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_C
	.private_extern _LT__uLT_var_LC_C
_LT__uLT_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_C
	.private_extern _LT__LT_var_LC_C
_LT__LT_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_C
	.private_extern _LT__uLT_param_LC_C
_LT__uLT_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_C
	.private_extern _LT__LT_param_LC_C
_LT__LT_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_u16
	.private_extern _LT__uLT_var_LC_u16
_LT__uLT_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_u16
	.private_extern _LT__LT_var_LC_u16
_LT__LT_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_u16
	.private_extern _LT__uLT_param_LC_u16
_LT__uLT_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_u16
	.private_extern _LT__LT_param_LC_u16
_LT__LT_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_u64
	.private_extern _LT__uLT_var_LC_u64
_LT__uLT_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_u64
	.private_extern _LT__LT_var_LC_u64
_LT__LT_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_u64
	.private_extern _LT__uLT_param_LC_u64
_LT__uLT_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_u64
	.private_extern _LT__LT_param_LC_u64
_LT__LT_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_L
	.private_extern _LT__uLT_var_LC_L
_LT__uLT_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_L
	.private_extern _LT__LT_var_LC_L
_LT__LT_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_L
	.private_extern _LT__uLT_param_LC_L
_LT__uLT_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_L
	.private_extern _LT__LT_param_LC_L
_LT__LT_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_i16
	.private_extern _LT__uLT_var_LC_i16
_LT__uLT_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_i16
	.private_extern _LT__LT_var_LC_i16
_LT__LT_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_i16
	.private_extern _LT__uLT_param_LC_i16
_LT__uLT_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_i16
	.private_extern _LT__LT_param_LC_i16
_LT__LT_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_LC_i32
	.private_extern _LT__uLT_var_LC_i32
_LT__uLT_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_LC_i32
	.private_extern _LT__LT_var_LC_i32
_LT__LT_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_LC_i32
	.private_extern _LT__uLT_param_LC_i32
_LT__uLT_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_LC_i32
	.private_extern _LT__LT_param_LC_i32
_LT__LT_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_f32_f32
	.private_extern _LT__LT_var_f32_f32
_LT__LT_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_param_f32_f32
	.private_extern _LT__LT_param_f32_f32
_LT__LT_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm1
	movss	-24(%rbp), %xmm0
	ucomiss	%xmm1, %xmm0
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_i64
	.private_extern _LT__uLT_var_u8_i64
_LT__uLT_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_i64
	.private_extern _LT__LT_var_u8_i64
_LT__LT_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_i64
	.private_extern _LT__uLT_param_u8_i64
_LT__uLT_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_i64
	.private_extern _LT__LT_param_u8_i64
_LT__LT_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_LC
	.private_extern _LT__uLT_var_u8_LC
_LT__uLT_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_LC
	.private_extern _LT__LT_var_u8_LC
_LT__LT_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_LC
	.private_extern _LT__uLT_param_u8_LC
_LT__uLT_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_LC
	.private_extern _LT__LT_param_u8_LC
_LT__LT_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_u8
	.private_extern _LT__uLT_var_u8_u8
_LT__uLT_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_u8_u8
	.private_extern _LT__LT_var_u8_u8
_LT__LT_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_u8_u8
	.private_extern _LT__uLT_param_u8_u8
_LT__uLT_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_u8
	.private_extern _LT__LT_param_u8_u8
_LT__LT_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	cmpb	-18(%rbp), %al
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_I
	.private_extern _LT__uLT_var_u8_I
_LT__uLT_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_I
	.private_extern _LT__LT_var_u8_I
_LT__LT_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_I
	.private_extern _LT__uLT_param_u8_I
_LT__uLT_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_I
	.private_extern _LT__LT_param_u8_I
_LT__LT_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_i8
	.private_extern _LT__uLT_var_u8_i8
_LT__uLT_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_i8
	.private_extern _LT__LT_var_u8_i8
_LT__LT_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %edx
	movzbl	136+_MM_LT(%rip), %eax
	cmpb	%al, %dl
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_i8
	.private_extern _LT__uLT_param_u8_i8
_LT__uLT_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_i8
	.private_extern _LT__LT_param_u8_i8
_LT__LT_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	cmpb	-17(%rbp), %al
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_u32
	.private_extern _LT__uLT_var_u8_u32
_LT__uLT_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_u32
	.private_extern _LT__LT_var_u8_u32
_LT__LT_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_u32
	.private_extern _LT__uLT_param_u8_u32
_LT__uLT_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_u32
	.private_extern _LT__LT_param_u8_u32
_LT__LT_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_C
	.private_extern _LT__uLT_var_u8_C
_LT__uLT_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_C
	.private_extern _LT__LT_var_u8_C
_LT__LT_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_C
	.private_extern _LT__uLT_param_u8_C
_LT__uLT_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_C
	.private_extern _LT__LT_param_u8_C
_LT__LT_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_u16
	.private_extern _LT__uLT_var_u8_u16
_LT__uLT_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_u16
	.private_extern _LT__LT_var_u8_u16
_LT__LT_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_u16
	.private_extern _LT__uLT_param_u8_u16
_LT__uLT_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_u16
	.private_extern _LT__LT_param_u8_u16
_LT__LT_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_u64
	.private_extern _LT__uLT_var_u8_u64
_LT__uLT_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_u64
	.private_extern _LT__LT_var_u8_u64
_LT__LT_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_u64
	.private_extern _LT__uLT_param_u8_u64
_LT__uLT_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_u64
	.private_extern _LT__LT_param_u8_u64
_LT__LT_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_L
	.private_extern _LT__uLT_var_u8_L
_LT__uLT_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_L
	.private_extern _LT__LT_var_u8_L
_LT__LT_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_L
	.private_extern _LT__uLT_param_u8_L
_LT__uLT_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_L
	.private_extern _LT__LT_param_u8_L
_LT__LT_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_i16
	.private_extern _LT__uLT_var_u8_i16
_LT__uLT_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_i16
	.private_extern _LT__LT_var_u8_i16
_LT__LT_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_i16
	.private_extern _LT__uLT_param_u8_i16
_LT__uLT_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_i16
	.private_extern _LT__LT_param_u8_i16
_LT__LT_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u8_i32
	.private_extern _LT__uLT_var_u8_i32
_LT__uLT_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u8_i32
	.private_extern _LT__LT_var_u8_i32
_LT__LT_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u8_i32
	.private_extern _LT__uLT_param_u8_i32
_LT__uLT_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u8_i32
	.private_extern _LT__LT_param_u8_i32
_LT__LT_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_i64
	.private_extern _LT__uLT_var_I_i64
_LT__uLT_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_i64
	.private_extern _LT__LT_var_I_i64
_LT__LT_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_i64
	.private_extern _LT__uLT_param_I_i64
_LT__uLT_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_i64
	.private_extern _LT__LT_param_I_i64
_LT__LT_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_LC
	.private_extern _LT__uLT_var_I_LC
_LT__uLT_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_LC
	.private_extern _LT__LT_var_I_LC
_LT__LT_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_LC
	.private_extern _LT__uLT_param_I_LC
_LT__uLT_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_LC
	.private_extern _LT__LT_param_I_LC
_LT__LT_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_u8
	.private_extern _LT__uLT_var_I_u8
_LT__uLT_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_u8
	.private_extern _LT__LT_var_I_u8
_LT__LT_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_u8
	.private_extern _LT__uLT_param_I_u8
_LT__uLT_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_u8
	.private_extern _LT__LT_param_I_u8
_LT__LT_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_I
	.private_extern _LT__uLT_var_I_I
_LT__uLT_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_I_I
	.private_extern _LT__LT_var_I_I
_LT__LT_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_I_I
	.private_extern _LT__uLT_param_I_I
_LT__uLT_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_I
	.private_extern _LT__LT_param_I_I
_LT__LT_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_i8
	.private_extern _LT__uLT_var_I_i8
_LT__uLT_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_i8
	.private_extern _LT__LT_var_I_i8
_LT__LT_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_i8
	.private_extern _LT__uLT_param_I_i8
_LT__uLT_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_i8
	.private_extern _LT__LT_param_I_i8
_LT__LT_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_u32
	.private_extern _LT__uLT_var_I_u32
_LT__uLT_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_u32
	.private_extern _LT__LT_var_I_u32
_LT__LT_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_u32
	.private_extern _LT__uLT_param_I_u32
_LT__uLT_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_u32
	.private_extern _LT__LT_param_I_u32
_LT__LT_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_C
	.private_extern _LT__uLT_var_I_C
_LT__uLT_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_C
	.private_extern _LT__LT_var_I_C
_LT__LT_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_C
	.private_extern _LT__uLT_param_I_C
_LT__uLT_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_C
	.private_extern _LT__LT_param_I_C
_LT__LT_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_u16
	.private_extern _LT__uLT_var_I_u16
_LT__uLT_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_u16
	.private_extern _LT__LT_var_I_u16
_LT__LT_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_u16
	.private_extern _LT__uLT_param_I_u16
_LT__uLT_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_u16
	.private_extern _LT__LT_param_I_u16
_LT__LT_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_u64
	.private_extern _LT__uLT_var_I_u64
_LT__uLT_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_u64
	.private_extern _LT__LT_var_I_u64
_LT__LT_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_u64
	.private_extern _LT__uLT_param_I_u64
_LT__uLT_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_u64
	.private_extern _LT__LT_param_I_u64
_LT__LT_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_L
	.private_extern _LT__uLT_var_I_L
_LT__uLT_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_L
	.private_extern _LT__LT_var_I_L
_LT__LT_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_L
	.private_extern _LT__uLT_param_I_L
_LT__uLT_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_L
	.private_extern _LT__LT_param_I_L
_LT__LT_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_i16
	.private_extern _LT__uLT_var_I_i16
_LT__uLT_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_i16
	.private_extern _LT__LT_var_I_i16
_LT__LT_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_i16
	.private_extern _LT__uLT_param_I_i16
_LT__uLT_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_i16
	.private_extern _LT__LT_param_I_i16
_LT__LT_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_I_i32
	.private_extern _LT__uLT_var_I_i32
_LT__uLT_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_I_i32
	.private_extern _LT__LT_var_I_i32
_LT__LT_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_I_i32
	.private_extern _LT__uLT_param_I_i32
_LT__uLT_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_I_i32
	.private_extern _LT__LT_param_I_i32
_LT__LT_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_i64
	.private_extern _LT__uLT_var_i8_i64
_LT__uLT_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_i64
	.private_extern _LT__LT_var_i8_i64
_LT__LT_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_i64
	.private_extern _LT__uLT_param_i8_i64
_LT__uLT_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_i64
	.private_extern _LT__LT_param_i8_i64
_LT__LT_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_LC
	.private_extern _LT__uLT_var_i8_LC
_LT__uLT_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_LC
	.private_extern _LT__LT_var_i8_LC
_LT__LT_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_LC
	.private_extern _LT__uLT_param_i8_LC
_LT__uLT_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_LC
	.private_extern _LT__LT_param_i8_LC
_LT__LT_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_u8
	.private_extern _LT__uLT_var_i8_u8
_LT__uLT_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %edx
	movzbl	124+_MM_LT(%rip), %eax
	cmpb	%al, %dl
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_u8
	.private_extern _LT__LT_var_i8_u8
_LT__LT_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_u8
	.private_extern _LT__uLT_param_i8_u8
_LT__uLT_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	cmpb	-17(%rbp), %al
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_u8
	.private_extern _LT__LT_param_i8_u8
_LT__LT_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_I
	.private_extern _LT__uLT_var_i8_I
_LT__uLT_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_I
	.private_extern _LT__LT_var_i8_I
_LT__LT_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_I
	.private_extern _LT__uLT_param_i8_I
_LT__uLT_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_I
	.private_extern _LT__LT_param_i8_I
_LT__LT_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_i8
	.private_extern _LT__uLT_var_i8_i8
_LT__uLT_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_i8_i8
	.private_extern _LT__LT_var_i8_i8
_LT__LT_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_i8_i8
	.private_extern _LT__uLT_param_i8_i8
_LT__uLT_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	cmpb	-18(%rbp), %al
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_i8
	.private_extern _LT__LT_param_i8_i8
_LT__LT_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_u32
	.private_extern _LT__uLT_var_i8_u32
_LT__uLT_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_u32
	.private_extern _LT__LT_var_i8_u32
_LT__LT_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_u32
	.private_extern _LT__uLT_param_i8_u32
_LT__uLT_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_u32
	.private_extern _LT__LT_param_i8_u32
_LT__LT_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_C
	.private_extern _LT__uLT_var_i8_C
_LT__uLT_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_C
	.private_extern _LT__LT_var_i8_C
_LT__LT_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_C
	.private_extern _LT__uLT_param_i8_C
_LT__uLT_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_C
	.private_extern _LT__LT_param_i8_C
_LT__LT_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_u16
	.private_extern _LT__uLT_var_i8_u16
_LT__uLT_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_u16
	.private_extern _LT__LT_var_i8_u16
_LT__LT_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_u16
	.private_extern _LT__uLT_param_i8_u16
_LT__uLT_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_u16
	.private_extern _LT__LT_param_i8_u16
_LT__LT_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_u64
	.private_extern _LT__uLT_var_i8_u64
_LT__uLT_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_u64
	.private_extern _LT__LT_var_i8_u64
_LT__LT_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_u64
	.private_extern _LT__uLT_param_i8_u64
_LT__uLT_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_u64
	.private_extern _LT__LT_param_i8_u64
_LT__LT_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_L
	.private_extern _LT__uLT_var_i8_L
_LT__uLT_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_L
	.private_extern _LT__LT_var_i8_L
_LT__LT_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_L
	.private_extern _LT__uLT_param_i8_L
_LT__uLT_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_L
	.private_extern _LT__LT_param_i8_L
_LT__LT_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_i16
	.private_extern _LT__uLT_var_i8_i16
_LT__uLT_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_i16
	.private_extern _LT__LT_var_i8_i16
_LT__LT_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_i16
	.private_extern _LT__uLT_param_i8_i16
_LT__uLT_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_i16
	.private_extern _LT__LT_param_i8_i16
_LT__LT_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i8_i32
	.private_extern _LT__uLT_var_i8_i32
_LT__uLT_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i8_i32
	.private_extern _LT__LT_var_i8_i32
_LT__LT_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i8_i32
	.private_extern _LT__uLT_param_i8_i32
_LT__uLT_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i8_i32
	.private_extern _LT__LT_param_i8_i32
_LT__LT_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_i64
	.private_extern _LT__uLT_var_u32_i64
_LT__uLT_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_i64
	.private_extern _LT__LT_var_u32_i64
_LT__LT_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_i64
	.private_extern _LT__uLT_param_u32_i64
_LT__uLT_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_i64
	.private_extern _LT__LT_param_u32_i64
_LT__LT_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_LC
	.private_extern _LT__uLT_var_u32_LC
_LT__uLT_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_LC
	.private_extern _LT__LT_var_u32_LC
_LT__LT_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_LC
	.private_extern _LT__uLT_param_u32_LC
_LT__uLT_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_LC
	.private_extern _LT__LT_param_u32_LC
_LT__LT_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_u8
	.private_extern _LT__uLT_var_u32_u8
_LT__uLT_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_u8
	.private_extern _LT__LT_var_u32_u8
_LT__LT_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %edx
	movzbl	124+_MM_LT(%rip), %eax
	movsbl	%al,%eax
	cmpl	%eax, %edx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_u8
	.private_extern _LT__uLT_param_u32_u8
_LT__uLT_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_u8
	.private_extern _LT__LT_param_u32_u8
_LT__LT_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movsbl	-21(%rbp),%eax
	cmpl	-20(%rbp), %eax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_I
	.private_extern _LT__uLT_var_u32_I
_LT__uLT_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_I
	.private_extern _LT__LT_var_u32_I
_LT__LT_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_I
	.private_extern _LT__uLT_param_u32_I
_LT__uLT_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_I
	.private_extern _LT__LT_param_u32_I
_LT__LT_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_i8
	.private_extern _LT__uLT_var_u32_i8
_LT__uLT_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_i8
	.private_extern _LT__LT_var_u32_i8
_LT__LT_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %edx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpl	%eax, %edx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_i8
	.private_extern _LT__uLT_param_u32_i8
_LT__uLT_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_i8
	.private_extern _LT__LT_param_u32_i8
_LT__LT_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movzbl	-21(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_u32
	.private_extern _LT__uLT_var_u32_u32
_LT__uLT_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_u32_u32
	.private_extern _LT__LT_var_u32_u32
_LT__LT_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_u32_u32
	.private_extern _LT__uLT_param_u32_u32
_LT__uLT_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_u32
	.private_extern _LT__LT_param_u32_u32
_LT__LT_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cmpl	-24(%rbp), %eax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_C
	.private_extern _LT__uLT_var_u32_C
_LT__uLT_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_C
	.private_extern _LT__LT_var_u32_C
_LT__LT_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_C
	.private_extern _LT__uLT_param_u32_C
_LT__uLT_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_C
	.private_extern _LT__LT_param_u32_C
_LT__LT_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_u16
	.private_extern _LT__uLT_var_u32_u16
_LT__uLT_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_u16
	.private_extern _LT__LT_var_u32_u16
_LT__LT_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %edx
	movzwl	152+_MM_LT(%rip), %eax
	cwtl
	cmpl	%eax, %edx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_u16
	.private_extern _LT__uLT_param_u32_u16
_LT__uLT_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_u16
	.private_extern _LT__LT_param_u32_u16
_LT__LT_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movswl	-22(%rbp),%eax
	cmpl	-20(%rbp), %eax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_u64
	.private_extern _LT__uLT_var_u32_u64
_LT__uLT_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_u64
	.private_extern _LT__LT_var_u32_u64
_LT__LT_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_u64
	.private_extern _LT__uLT_param_u32_u64
_LT__uLT_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_u64
	.private_extern _LT__LT_param_u32_u64
_LT__LT_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_L
	.private_extern _LT__uLT_var_u32_L
_LT__uLT_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_L
	.private_extern _LT__LT_var_u32_L
_LT__LT_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_L
	.private_extern _LT__uLT_param_u32_L
_LT__uLT_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_L
	.private_extern _LT__LT_param_u32_L
_LT__LT_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_i16
	.private_extern _LT__uLT_var_u32_i16
_LT__uLT_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_i16
	.private_extern _LT__LT_var_u32_i16
_LT__LT_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %edx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpl	%eax, %edx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_i16
	.private_extern _LT__uLT_param_u32_i16
_LT__uLT_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_i16
	.private_extern _LT__LT_param_u32_i16
_LT__LT_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movzwl	-22(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u32_i32
	.private_extern _LT__uLT_var_u32_i32
_LT__uLT_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u32_i32
	.private_extern _LT__LT_var_u32_i32
_LT__LT_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %edx
	movl	188+_MM_LT(%rip), %eax
	cmpl	%eax, %edx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u32_i32
	.private_extern _LT__uLT_param_u32_i32
_LT__uLT_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u32_i32
	.private_extern _LT__LT_param_u32_i32
_LT__LT_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cmpl	-20(%rbp), %eax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_i64
	.private_extern _LT__uLT_var_C_i64
_LT__uLT_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_i64
	.private_extern _LT__LT_var_C_i64
_LT__LT_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_i64
	.private_extern _LT__uLT_param_C_i64
_LT__uLT_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_i64
	.private_extern _LT__LT_param_C_i64
_LT__LT_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_LC
	.private_extern _LT__uLT_var_C_LC
_LT__uLT_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_LC
	.private_extern _LT__LT_var_C_LC
_LT__LT_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_LC
	.private_extern _LT__uLT_param_C_LC
_LT__uLT_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_LC
	.private_extern _LT__LT_param_C_LC
_LT__LT_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_u8
	.private_extern _LT__uLT_var_C_u8
_LT__uLT_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_u8
	.private_extern _LT__LT_var_C_u8
_LT__LT_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_u8
	.private_extern _LT__uLT_param_C_u8
_LT__uLT_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_u8
	.private_extern _LT__LT_param_C_u8
_LT__LT_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_I
	.private_extern _LT__uLT_var_C_I
_LT__uLT_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_I
	.private_extern _LT__LT_var_C_I
_LT__LT_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_I
	.private_extern _LT__uLT_param_C_I
_LT__uLT_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_I
	.private_extern _LT__LT_param_C_I
_LT__LT_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_i8
	.private_extern _LT__uLT_var_C_i8
_LT__uLT_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_i8
	.private_extern _LT__LT_var_C_i8
_LT__LT_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_i8
	.private_extern _LT__uLT_param_C_i8
_LT__uLT_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_i8
	.private_extern _LT__LT_param_C_i8
_LT__LT_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_u32
	.private_extern _LT__uLT_var_C_u32
_LT__uLT_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_u32
	.private_extern _LT__LT_var_C_u32
_LT__LT_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_u32
	.private_extern _LT__uLT_param_C_u32
_LT__uLT_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_u32
	.private_extern _LT__LT_param_C_u32
_LT__LT_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_C
	.private_extern _LT__uLT_var_C_C
_LT__uLT_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_C_C
	.private_extern _LT__LT_var_C_C
_LT__LT_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_C_C
	.private_extern _LT__uLT_param_C_C
_LT__uLT_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_C
	.private_extern _LT__LT_param_C_C
_LT__LT_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_u16
	.private_extern _LT__uLT_var_C_u16
_LT__uLT_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_u16
	.private_extern _LT__LT_var_C_u16
_LT__LT_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_u16
	.private_extern _LT__uLT_param_C_u16
_LT__uLT_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_u16
	.private_extern _LT__LT_param_C_u16
_LT__LT_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_u64
	.private_extern _LT__uLT_var_C_u64
_LT__uLT_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_u64
	.private_extern _LT__LT_var_C_u64
_LT__LT_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_u64
	.private_extern _LT__uLT_param_C_u64
_LT__uLT_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_u64
	.private_extern _LT__LT_param_C_u64
_LT__LT_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_L
	.private_extern _LT__uLT_var_C_L
_LT__uLT_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_L
	.private_extern _LT__LT_var_C_L
_LT__LT_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_L
	.private_extern _LT__uLT_param_C_L
_LT__uLT_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_L
	.private_extern _LT__LT_param_C_L
_LT__LT_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_i16
	.private_extern _LT__uLT_var_C_i16
_LT__uLT_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_i16
	.private_extern _LT__LT_var_C_i16
_LT__LT_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_i16
	.private_extern _LT__uLT_param_C_i16
_LT__uLT_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_i16
	.private_extern _LT__LT_param_C_i16
_LT__LT_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_C_i32
	.private_extern _LT__uLT_var_C_i32
_LT__uLT_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_C_i32
	.private_extern _LT__LT_var_C_i32
_LT__LT_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_C_i32
	.private_extern _LT__uLT_param_C_i32
_LT__uLT_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_C_i32
	.private_extern _LT__LT_param_C_i32
_LT__LT_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_i64
	.private_extern _LT__uLT_var_u16_i64
_LT__uLT_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_i64
	.private_extern _LT__LT_var_u16_i64
_LT__LT_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_i64
	.private_extern _LT__uLT_param_u16_i64
_LT__uLT_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_i64
	.private_extern _LT__LT_param_u16_i64
_LT__LT_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_LC
	.private_extern _LT__uLT_var_u16_LC
_LT__uLT_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_LC
	.private_extern _LT__LT_var_u16_LC
_LT__LT_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_LC
	.private_extern _LT__uLT_param_u16_LC
_LT__uLT_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_LC
	.private_extern _LT__LT_param_u16_LC
_LT__LT_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_u8
	.private_extern _LT__uLT_var_u16_u8
_LT__uLT_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_u8
	.private_extern _LT__LT_var_u16_u8
_LT__LT_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %edx
	movzbl	124+_MM_LT(%rip), %eax
	cbtw
	cmpw	%ax, %dx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_u8
	.private_extern _LT__uLT_param_u16_u8
_LT__uLT_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_u8
	.private_extern _LT__LT_param_u16_u8
_LT__LT_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movsbw	-19(%rbp),%ax
	cmpw	-18(%rbp), %ax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_I
	.private_extern _LT__uLT_var_u16_I
_LT__uLT_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_I
	.private_extern _LT__LT_var_u16_I
_LT__LT_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_I
	.private_extern _LT__uLT_param_u16_I
_LT__uLT_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_I
	.private_extern _LT__LT_param_u16_I
_LT__LT_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_i8
	.private_extern _LT__uLT_var_u16_i8
_LT__uLT_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_i8
	.private_extern _LT__LT_var_u16_i8
_LT__LT_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %edx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpw	%ax, %dx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_i8
	.private_extern _LT__uLT_param_u16_i8
_LT__uLT_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_i8
	.private_extern _LT__LT_param_u16_i8
_LT__LT_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzbl	-19(%rbp), %eax
	cmpw	-18(%rbp), %ax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_u32
	.private_extern _LT__uLT_var_u16_u32
_LT__uLT_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_u32
	.private_extern _LT__LT_var_u16_u32
_LT__LT_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_u32
	.private_extern _LT__uLT_param_u16_u32
_LT__uLT_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_u32
	.private_extern _LT__LT_param_u16_u32
_LT__LT_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_C
	.private_extern _LT__uLT_var_u16_C
_LT__uLT_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_C
	.private_extern _LT__LT_var_u16_C
_LT__LT_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_C
	.private_extern _LT__uLT_param_u16_C
_LT__uLT_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_C
	.private_extern _LT__LT_param_u16_C
_LT__LT_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_u16
	.private_extern _LT__uLT_var_u16_u16
_LT__uLT_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_u16_u16
	.private_extern _LT__LT_var_u16_u16
_LT__LT_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_u16_u16
	.private_extern _LT__uLT_param_u16_u16
_LT__uLT_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_u16
	.private_extern _LT__LT_param_u16_u16
_LT__LT_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	cmpw	-20(%rbp), %ax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_u64
	.private_extern _LT__uLT_var_u16_u64
_LT__uLT_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_u64
	.private_extern _LT__LT_var_u16_u64
_LT__LT_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_u64
	.private_extern _LT__uLT_param_u16_u64
_LT__uLT_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_u64
	.private_extern _LT__LT_param_u16_u64
_LT__LT_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_L
	.private_extern _LT__uLT_var_u16_L
_LT__uLT_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_L
	.private_extern _LT__LT_var_u16_L
_LT__LT_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_L
	.private_extern _LT__uLT_param_u16_L
_LT__uLT_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_L
	.private_extern _LT__LT_param_u16_L
_LT__LT_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_i16
	.private_extern _LT__uLT_var_u16_i16
_LT__uLT_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_i16
	.private_extern _LT__LT_var_u16_i16
_LT__LT_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %edx
	movzwl	184+_MM_LT(%rip), %eax
	cmpw	%ax, %dx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_i16
	.private_extern _LT__uLT_param_u16_i16
_LT__uLT_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_i16
	.private_extern _LT__LT_param_u16_i16
_LT__LT_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	cmpw	-18(%rbp), %ax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u16_i32
	.private_extern _LT__uLT_var_u16_i32
_LT__uLT_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u16_i32
	.private_extern _LT__LT_var_u16_i32
_LT__LT_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u16_i32
	.private_extern _LT__uLT_param_u16_i32
_LT__uLT_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u16_i32
	.private_extern _LT__LT_param_u16_i32
_LT__LT_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_i64
	.private_extern _LT__uLT_var_u64_i64
_LT__uLT_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_i64
	.private_extern _LT__LT_var_u64_i64
_LT__LT_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_i64
	.private_extern _LT__uLT_param_u64_i64
_LT__uLT_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_i64
	.private_extern _LT__LT_param_u64_i64
_LT__LT_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_LC
	.private_extern _LT__uLT_var_u64_LC
_LT__uLT_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_LC
	.private_extern _LT__LT_var_u64_LC
_LT__LT_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_LC
	.private_extern _LT__uLT_param_u64_LC
_LT__uLT_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_LC
	.private_extern _LT__LT_param_u64_LC
_LT__LT_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_u8
	.private_extern _LT__uLT_var_u64_u8
_LT__uLT_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_u8
	.private_extern _LT__LT_var_u64_u8
_LT__LT_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_u8
	.private_extern _LT__uLT_param_u64_u8
_LT__uLT_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_u8
	.private_extern _LT__LT_param_u64_u8
_LT__LT_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_I
	.private_extern _LT__uLT_var_u64_I
_LT__uLT_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_I
	.private_extern _LT__LT_var_u64_I
_LT__LT_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_I
	.private_extern _LT__uLT_param_u64_I
_LT__uLT_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_I
	.private_extern _LT__LT_param_u64_I
_LT__LT_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_i8
	.private_extern _LT__uLT_var_u64_i8
_LT__uLT_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_i8
	.private_extern _LT__LT_var_u64_i8
_LT__LT_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_i8
	.private_extern _LT__uLT_param_u64_i8
_LT__uLT_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_i8
	.private_extern _LT__LT_param_u64_i8
_LT__LT_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_u32
	.private_extern _LT__uLT_var_u64_u32
_LT__uLT_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_u32
	.private_extern _LT__LT_var_u64_u32
_LT__LT_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_u32
	.private_extern _LT__uLT_param_u64_u32
_LT__uLT_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_u32
	.private_extern _LT__LT_param_u64_u32
_LT__LT_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_C
	.private_extern _LT__uLT_var_u64_C
_LT__uLT_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_C
	.private_extern _LT__LT_var_u64_C
_LT__LT_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_C
	.private_extern _LT__uLT_param_u64_C
_LT__uLT_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_C
	.private_extern _LT__LT_param_u64_C
_LT__LT_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_u16
	.private_extern _LT__uLT_var_u64_u16
_LT__uLT_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_u16
	.private_extern _LT__LT_var_u64_u16
_LT__LT_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_u16
	.private_extern _LT__uLT_param_u64_u16
_LT__uLT_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_u16
	.private_extern _LT__LT_param_u64_u16
_LT__LT_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_u64
	.private_extern _LT__uLT_var_u64_u64
_LT__uLT_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_u64_u64
	.private_extern _LT__LT_var_u64_u64
_LT__LT_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_u64_u64
	.private_extern _LT__uLT_param_u64_u64
_LT__uLT_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_u64
	.private_extern _LT__LT_param_u64_u64
_LT__LT_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_L
	.private_extern _LT__uLT_var_u64_L
_LT__uLT_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_L
	.private_extern _LT__LT_var_u64_L
_LT__LT_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_L
	.private_extern _LT__uLT_param_u64_L
_LT__uLT_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_L
	.private_extern _LT__LT_param_u64_L
_LT__LT_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_i16
	.private_extern _LT__uLT_var_u64_i16
_LT__uLT_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_i16
	.private_extern _LT__LT_var_u64_i16
_LT__LT_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_i16
	.private_extern _LT__uLT_param_u64_i16
_LT__uLT_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_i16
	.private_extern _LT__LT_param_u64_i16
_LT__LT_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_u64_i32
	.private_extern _LT__uLT_var_u64_i32
_LT__uLT_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_u64_i32
	.private_extern _LT__LT_var_u64_i32
_LT__LT_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_u64_i32
	.private_extern _LT__uLT_param_u64_i32
_LT__uLT_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_u64_i32
	.private_extern _LT__LT_param_u64_i32
_LT__LT_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_f64_f64
	.private_extern _LT__LT_var_f64_f64
_LT__LT_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_param_f64_f64
	.private_extern _LT__LT_param_f64_f64
_LT__LT_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	ucomisd	%xmm1, %xmm0
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_i64
	.private_extern _LT__uLT_var_L_i64
_LT__uLT_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_i64
	.private_extern _LT__LT_var_L_i64
_LT__LT_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LT(%rip), %rdx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_i64
	.private_extern _LT__uLT_param_L_i64
_LT__uLT_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_i64
	.private_extern _LT__LT_param_L_i64
_LT__LT_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_LC
	.private_extern _LT__uLT_var_L_LC
_LT__uLT_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_LC
	.private_extern _LT__LT_var_L_LC
_LT__LT_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LT(%rip), %rdx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_LC
	.private_extern _LT__uLT_param_L_LC
_LT__uLT_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_LC
	.private_extern _LT__LT_param_L_LC
_LT__LT_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_u8
	.private_extern _LT__uLT_var_L_u8
_LT__uLT_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_u8
	.private_extern _LT__LT_var_L_u8
_LT__LT_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_u8
	.private_extern _LT__uLT_param_L_u8
_LT__uLT_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_u8
	.private_extern _LT__LT_param_L_u8
_LT__LT_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_I
	.private_extern _LT__uLT_var_L_I
_LT__uLT_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_I
	.private_extern _LT__LT_var_L_I
_LT__LT_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_I
	.private_extern _LT__uLT_param_L_I
_LT__uLT_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_I
	.private_extern _LT__LT_param_L_I
_LT__LT_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_i8
	.private_extern _LT__uLT_var_L_i8
_LT__uLT_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_i8
	.private_extern _LT__LT_var_L_i8
_LT__LT_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_i8
	.private_extern _LT__uLT_param_L_i8
_LT__uLT_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_i8
	.private_extern _LT__LT_param_L_i8
_LT__LT_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_u32
	.private_extern _LT__uLT_var_L_u32
_LT__uLT_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_u32
	.private_extern _LT__LT_var_L_u32
_LT__LT_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_LT(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_u32
	.private_extern _LT__uLT_param_L_u32
_LT__uLT_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_u32
	.private_extern _LT__LT_param_L_u32
_LT__LT_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_C
	.private_extern _LT__uLT_var_L_C
_LT__uLT_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_C
	.private_extern _LT__LT_var_L_C
_LT__LT_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_C
	.private_extern _LT__uLT_param_L_C
_LT__uLT_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_C
	.private_extern _LT__LT_param_L_C
_LT__LT_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_u16
	.private_extern _LT__uLT_var_L_u16
_LT__uLT_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_u16
	.private_extern _LT__LT_var_L_u16
_LT__LT_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_u16
	.private_extern _LT__uLT_param_L_u16
_LT__uLT_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_u16
	.private_extern _LT__LT_param_L_u16
_LT__LT_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_u64
	.private_extern _LT__uLT_var_L_u64
_LT__uLT_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LT(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_u64
	.private_extern _LT__LT_var_L_u64
_LT__LT_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_LT(%rip), %rdx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_u64
	.private_extern _LT__uLT_param_L_u64
_LT__uLT_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_u64
	.private_extern _LT__LT_param_L_u64
_LT__LT_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_L
	.private_extern _LT__uLT_var_L_L
_LT__uLT_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_L_L
	.private_extern _LT__LT_var_L_L
_LT__LT_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_L_L
	.private_extern _LT__uLT_param_L_L
_LT__uLT_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_L
	.private_extern _LT__LT_param_L_L
_LT__LT_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_i16
	.private_extern _LT__uLT_var_L_i16
_LT__uLT_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_i16
	.private_extern _LT__LT_var_L_i16
_LT__LT_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_i16
	.private_extern _LT__uLT_param_L_i16
_LT__uLT_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_i16
	.private_extern _LT__LT_param_L_i16
_LT__LT_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_L_i32
	.private_extern _LT__uLT_var_L_i32
_LT__uLT_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_L_i32
	.private_extern _LT__LT_var_L_i32
_LT__LT_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_L_i32
	.private_extern _LT__uLT_param_L_i32
_LT__uLT_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	cmpq	%rax, %rdx
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_L_i32
	.private_extern _LT__LT_param_L_i32
_LT__LT_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	cmpq	-24(%rbp), %rax
	setg	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_i64
	.private_extern _LT__uLT_var_i16_i64
_LT__uLT_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_i64
	.private_extern _LT__LT_var_i16_i64
_LT__LT_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_i64
	.private_extern _LT__uLT_param_i16_i64
_LT__uLT_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_i64
	.private_extern _LT__LT_param_i16_i64
_LT__LT_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_LC
	.private_extern _LT__uLT_var_i16_LC
_LT__uLT_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_LC
	.private_extern _LT__LT_var_i16_LC
_LT__LT_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_LC
	.private_extern _LT__uLT_param_i16_LC
_LT__uLT_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_LC
	.private_extern _LT__LT_param_i16_LC
_LT__LT_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_u8
	.private_extern _LT__uLT_var_i16_u8
_LT__uLT_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %edx
	movzbl	124+_MM_LT(%rip), %eax
	cbtw
	cmpw	%ax, %dx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_u8
	.private_extern _LT__LT_var_i16_u8
_LT__LT_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_u8
	.private_extern _LT__uLT_param_i16_u8
_LT__uLT_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movsbw	-19(%rbp),%ax
	cmpw	-18(%rbp), %ax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_u8
	.private_extern _LT__LT_param_i16_u8
_LT__LT_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_I
	.private_extern _LT__uLT_var_i16_I
_LT__uLT_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_I
	.private_extern _LT__LT_var_i16_I
_LT__LT_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_I
	.private_extern _LT__uLT_param_i16_I
_LT__uLT_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_I
	.private_extern _LT__LT_param_i16_I
_LT__LT_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_i8
	.private_extern _LT__uLT_var_i16_i8
_LT__uLT_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %edx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpw	%ax, %dx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_i8
	.private_extern _LT__LT_var_i16_i8
_LT__LT_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_i8
	.private_extern _LT__uLT_param_i16_i8
_LT__uLT_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzbl	-19(%rbp), %eax
	cmpw	-18(%rbp), %ax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_i8
	.private_extern _LT__LT_param_i16_i8
_LT__LT_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_u32
	.private_extern _LT__uLT_var_i16_u32
_LT__uLT_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_u32
	.private_extern _LT__LT_var_i16_u32
_LT__LT_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_u32
	.private_extern _LT__uLT_param_i16_u32
_LT__uLT_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_u32
	.private_extern _LT__LT_param_i16_u32
_LT__LT_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_C
	.private_extern _LT__uLT_var_i16_C
_LT__uLT_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_C
	.private_extern _LT__LT_var_i16_C
_LT__LT_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_C
	.private_extern _LT__uLT_param_i16_C
_LT__uLT_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_C
	.private_extern _LT__LT_param_i16_C
_LT__LT_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_u16
	.private_extern _LT__uLT_var_i16_u16
_LT__uLT_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %edx
	movzwl	152+_MM_LT(%rip), %eax
	cmpw	%ax, %dx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_u16
	.private_extern _LT__LT_var_i16_u16
_LT__LT_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_u16
	.private_extern _LT__uLT_param_i16_u16
_LT__uLT_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	cmpw	-18(%rbp), %ax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_u16
	.private_extern _LT__LT_param_i16_u16
_LT__LT_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_u64
	.private_extern _LT__uLT_var_i16_u64
_LT__uLT_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_u64
	.private_extern _LT__LT_var_i16_u64
_LT__LT_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_u64
	.private_extern _LT__uLT_param_i16_u64
_LT__uLT_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_u64
	.private_extern _LT__LT_param_i16_u64
_LT__LT_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_L
	.private_extern _LT__uLT_var_i16_L
_LT__uLT_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_L
	.private_extern _LT__LT_var_i16_L
_LT__LT_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_L
	.private_extern _LT__uLT_param_i16_L
_LT__uLT_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_L
	.private_extern _LT__LT_param_i16_L
_LT__LT_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_i16
	.private_extern _LT__uLT_var_i16_i16
_LT__uLT_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_i16_i16
	.private_extern _LT__LT_var_i16_i16
_LT__LT_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_i16_i16
	.private_extern _LT__uLT_param_i16_i16
_LT__uLT_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	cmpw	-20(%rbp), %ax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_i16
	.private_extern _LT__LT_param_i16_i16
_LT__LT_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i16_i32
	.private_extern _LT__uLT_var_i16_i32
_LT__uLT_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i16_i32
	.private_extern _LT__LT_var_i16_i32
_LT__LT_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i16_i32
	.private_extern _LT__uLT_param_i16_i32
_LT__uLT_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i16_i32
	.private_extern _LT__LT_param_i16_i32
_LT__LT_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_i64
	.private_extern _LT__uLT_var_i32_i64
_LT__uLT_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_i64
	.private_extern _LT__LT_var_i32_i64
_LT__LT_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_i64
	.private_extern _LT__uLT_param_i32_i64
_LT__uLT_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_i64
	.private_extern _LT__LT_param_i32_i64
_LT__LT_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_LC
	.private_extern _LT__uLT_var_i32_LC
_LT__uLT_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_LC
	.private_extern _LT__LT_var_i32_LC
_LT__LT_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_LC
	.private_extern _LT__uLT_param_i32_LC
_LT__uLT_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_LC
	.private_extern _LT__LT_param_i32_LC
_LT__LT_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_u8
	.private_extern _LT__uLT_var_i32_u8
_LT__uLT_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %edx
	movzbl	124+_MM_LT(%rip), %eax
	movsbl	%al,%eax
	cmpl	%eax, %edx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_u8
	.private_extern _LT__LT_var_i32_u8
_LT__LT_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_LT(%rip), %eax
	movsbq	%al,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_u8
	.private_extern _LT__uLT_param_i32_u8
_LT__uLT_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movsbl	-21(%rbp),%eax
	cmpl	-20(%rbp), %eax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_u8
	.private_extern _LT__LT_param_i32_u8
_LT__LT_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_I
	.private_extern _LT__uLT_var_i32_I
_LT__uLT_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_I
	.private_extern _LT__LT_var_i32_I
_LT__LT_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_I
	.private_extern _LT__uLT_param_i32_I
_LT__uLT_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_I
	.private_extern _LT__LT_param_i32_I
_LT__LT_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_i8
	.private_extern _LT__uLT_var_i32_i8
_LT__uLT_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %edx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpl	%eax, %edx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_i8
	.private_extern _LT__LT_var_i32_i8
_LT__LT_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_LT(%rip), %eax
	movzbl	%al, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_i8
	.private_extern _LT__uLT_param_i32_i8
_LT__uLT_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movzbl	-21(%rbp), %eax
	cmpl	-20(%rbp), %eax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_i8
	.private_extern _LT__LT_param_i32_i8
_LT__LT_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_u32
	.private_extern _LT__uLT_var_i32_u32
_LT__uLT_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %edx
	movl	140+_MM_LT(%rip), %eax
	cmpl	%eax, %edx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_u32
	.private_extern _LT__LT_var_i32_u32
_LT__LT_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_LT(%rip), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_u32
	.private_extern _LT__uLT_param_i32_u32
_LT__uLT_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cmpl	-20(%rbp), %eax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_u32
	.private_extern _LT__LT_param_i32_u32
_LT__LT_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_C
	.private_extern _LT__uLT_var_i32_C
_LT__uLT_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_C
	.private_extern _LT__LT_var_i32_C
_LT__LT_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_C
	.private_extern _LT__uLT_param_i32_C
_LT__uLT_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_C
	.private_extern _LT__LT_param_i32_C
_LT__LT_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_u16
	.private_extern _LT__uLT_var_i32_u16
_LT__uLT_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %edx
	movzwl	152+_MM_LT(%rip), %eax
	cwtl
	cmpl	%eax, %edx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_u16
	.private_extern _LT__LT_var_i32_u16
_LT__LT_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_LT(%rip), %eax
	movswq	%ax,%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_u16
	.private_extern _LT__uLT_param_i32_u16
_LT__uLT_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movswl	-22(%rbp),%eax
	cmpl	-20(%rbp), %eax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_u16
	.private_extern _LT__LT_param_i32_u16
_LT__LT_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_u64
	.private_extern _LT__uLT_var_i32_u64
_LT__uLT_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_u64
	.private_extern _LT__LT_var_i32_u64
_LT__LT_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_u64
	.private_extern _LT__uLT_param_i32_u64
_LT__uLT_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_u64
	.private_extern _LT__LT_param_i32_u64
_LT__LT_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_L
	.private_extern _LT__uLT_var_i32_L
_LT__uLT_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_L
	.private_extern _LT__LT_var_i32_L
_LT__LT_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_LT(%rip), %rax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_L
	.private_extern _LT__uLT_param_i32_L
_LT__uLT_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	cmpq	%rax, %rdx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_L
	.private_extern _LT__LT_param_i32_L
_LT__LT_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	cmpq	-32(%rbp), %rax
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_i16
	.private_extern _LT__uLT_var_i32_i16
_LT__uLT_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %edx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpl	%eax, %edx
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_var_i32_i16
	.private_extern _LT__LT_var_i32_i16
_LT__LT_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_LT(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_LT(%rip), %eax
	movzwl	%ax, %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_param_i32_i16
	.private_extern _LT__uLT_param_i32_i16
_LT__uLT_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movzwl	-22(%rbp), %eax
	cmpl	-20(%rbp), %eax
	seta	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_i16
	.private_extern _LT__LT_param_i32_i16
_LT__LT_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__uLT_var_i32_i32
	.private_extern _LT__uLT_var_i32_i32
_LT__uLT_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__LT_var_i32_i32
	.private_extern _LT__LT_var_i32_i32
_LT__LT_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _LT__uLT_param_i32_i32
	.private_extern _LT__uLT_param_i32_i32
_LT__uLT_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cmpl	-24(%rbp), %eax
	setb	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT__LT_param_i32_i32
	.private_extern _LT__LT_param_i32_i32
_LT__LT_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	cmpq	%rax, %rdx
	setl	%al
	movzbl	%al, %eax
	leave
	ret
.globl _LT_M3
_LT_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_LT(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "LT_M3"
	.space 1
	.ascii "LT_param_i32_i32"
	.space 1
	.ascii "uLT_param_i32_i32"
	.space 1
	.ascii "LT_var_i32_i32"
	.space 1
	.ascii "uLT_var_i32_i32"
	.space 1
	.ascii "LT_param_i32_i16"
	.space 1
	.ascii "uLT_param_i32_i16"
	.space 1
	.ascii "LT_var_i32_i16"
	.space 1
	.ascii "uLT_var_i32_i16"
	.space 1
	.ascii "LT_param_i32_L"
	.space 1
	.ascii "uLT_param_i32_L"
	.space 1
	.ascii "LT_var_i32_L"
	.space 1
	.ascii "uLT_var_i32_L"
	.space 1
	.ascii "LT_param_i32_u64"
	.space 1
	.ascii "uLT_param_i32_u64"
	.space 1
	.ascii "LT_var_i32_u64"
	.space 1
	.ascii "uLT_var_i32_u64"
	.space 1
	.ascii "LT_param_i32_u16"
	.space 1
	.ascii "uLT_param_i32_u16"
	.space 1
	.ascii "LT_var_i32_u16"
	.space 1
	.ascii "uLT_var_i32_u16"
	.space 1
	.ascii "LT_param_i32_C"
	.space 1
	.ascii "uLT_param_i32_C"
	.space 1
	.ascii "LT_var_i32_C"
	.space 1
	.ascii "uLT_var_i32_C"
	.space 1
	.ascii "LT_param_i32_u32"
	.space 1
	.ascii "uLT_param_i32_u32"
	.space 1
	.ascii "LT_var_i32_u32"
	.space 1
	.ascii "uLT_var_i32_u32"
	.space 1
	.ascii "LT_param_i32_i8"
	.space 1
	.ascii "uLT_param_i32_i8"
	.space 1
	.ascii "LT_var_i32_i8"
	.space 1
	.ascii "uLT_var_i32_i8"
	.space 1
	.ascii "LT_param_i32_I"
	.space 1
	.ascii "uLT_param_i32_I"
	.space 1
	.ascii "LT_var_i32_I"
	.space 1
	.ascii "uLT_var_i32_I"
	.space 1
	.ascii "LT_param_i32_u8"
	.space 1
	.ascii "uLT_param_i32_u8"
	.space 1
	.ascii "LT_var_i32_u8"
	.space 1
	.ascii "uLT_var_i32_u8"
	.space 1
	.ascii "LT_param_i32_LC"
	.space 1
	.ascii "uLT_param_i32_LC"
	.space 1
	.ascii "LT_var_i32_LC"
	.space 1
	.ascii "uLT_var_i32_LC"
	.space 1
	.ascii "LT_param_i32_i64"
	.space 1
	.ascii "uLT_param_i32_i64"
	.space 1
	.ascii "LT_var_i32_i64"
	.space 1
	.ascii "uLT_var_i32_i64"
	.space 1
	.ascii "LT_param_i16_i32"
	.space 1
	.ascii "uLT_param_i16_i32"
	.space 1
	.ascii "LT_var_i16_i32"
	.space 1
	.ascii "uLT_var_i16_i32"
	.space 1
	.ascii "LT_param_i16_i16"
	.space 1
	.ascii "uLT_param_i16_i16"
	.space 1
	.ascii "LT_var_i16_i16"
	.space 1
	.ascii "uLT_var_i16_i16"
	.space 1
	.ascii "LT_param_i16_L"
	.space 1
	.ascii "uLT_param_i16_L"
	.space 1
	.ascii "LT_var_i16_L"
	.space 1
	.ascii "uLT_var_i16_L"
	.space 1
	.ascii "LT_param_i16_u64"
	.space 1
	.ascii "uLT_param_i16_u64"
	.space 1
	.ascii "LT_var_i16_u64"
	.space 1
	.ascii "uLT_var_i16_u64"
	.space 1
	.ascii "LT_param_i16_u16"
	.space 1
	.ascii "uLT_param_i16_u16"
	.space 1
	.ascii "LT_var_i16_u16"
	.space 1
	.ascii "uLT_var_i16_u16"
	.space 1
	.ascii "LT_param_i16_C"
	.space 1
	.ascii "uLT_param_i16_C"
	.space 1
	.ascii "LT_var_i16_C"
	.space 1
	.ascii "uLT_var_i16_C"
	.space 1
	.ascii "LT_param_i16_u32"
	.space 1
	.ascii "uLT_param_i16_u32"
	.space 1
	.ascii "LT_var_i16_u32"
	.space 1
	.ascii "uLT_var_i16_u32"
	.space 1
	.ascii "LT_param_i16_i8"
	.space 1
	.ascii "uLT_param_i16_i8"
	.space 1
	.ascii "LT_var_i16_i8"
	.space 1
	.ascii "uLT_var_i16_i8"
	.space 1
	.ascii "LT_param_i16_I"
	.space 1
	.ascii "uLT_param_i16_I"
	.space 1
	.ascii "LT_var_i16_I"
	.space 1
	.ascii "uLT_var_i16_I"
	.space 1
	.ascii "LT_param_i16_u8"
	.space 1
	.ascii "uLT_param_i16_u8"
	.space 1
	.ascii "LT_var_i16_u8"
	.space 1
	.ascii "uLT_var_i16_u8"
	.space 1
	.ascii "LT_param_i16_LC"
	.space 1
	.ascii "uLT_param_i16_LC"
	.space 1
	.ascii "LT_var_i16_LC"
	.space 1
	.ascii "uLT_var_i16_LC"
	.space 1
	.ascii "LT_param_i16_i64"
	.space 1
	.ascii "uLT_param_i16_i64"
	.space 1
	.ascii "LT_var_i16_i64"
	.space 1
	.ascii "uLT_var_i16_i64"
	.space 1
	.ascii "LT_param_L_i32"
	.space 1
	.ascii "uLT_param_L_i32"
	.space 1
	.ascii "LT_var_L_i32"
	.space 1
	.ascii "uLT_var_L_i32"
	.space 1
	.ascii "LT_param_L_i16"
	.space 1
	.ascii "uLT_param_L_i16"
	.space 1
	.ascii "LT_var_L_i16"
	.space 1
	.ascii "uLT_var_L_i16"
	.space 1
	.ascii "LT_param_L_L"
	.space 1
	.ascii "uLT_param_L_L"
	.space 1
	.ascii "LT_var_L_L"
	.space 1
	.ascii "uLT_var_L_L"
	.space 1
	.ascii "LT_param_L_u64"
	.space 1
	.ascii "uLT_param_L_u64"
	.space 1
	.ascii "LT_var_L_u64"
	.space 1
	.ascii "uLT_var_L_u64"
	.space 1
	.ascii "LT_param_L_u16"
	.space 1
	.ascii "uLT_param_L_u16"
	.space 1
	.ascii "LT_var_L_u16"
	.space 1
	.ascii "uLT_var_L_u16"
	.space 1
	.ascii "LT_param_L_C"
	.space 1
	.ascii "uLT_param_L_C"
	.space 1
	.ascii "LT_var_L_C"
	.space 1
	.ascii "uLT_var_L_C"
	.space 1
	.ascii "LT_param_L_u32"
	.space 1
	.ascii "uLT_param_L_u32"
	.space 1
	.ascii "LT_var_L_u32"
	.space 1
	.ascii "uLT_var_L_u32"
	.space 1
	.ascii "LT_param_L_i8"
	.space 1
	.ascii "uLT_param_L_i8"
	.space 1
	.ascii "LT_var_L_i8"
	.space 1
	.ascii "uLT_var_L_i8"
	.space 1
	.ascii "LT_param_L_I"
	.space 1
	.ascii "uLT_param_L_I"
	.space 1
	.ascii "LT_var_L_I"
	.space 1
	.ascii "uLT_var_L_I"
	.space 1
	.ascii "LT_param_L_u8"
	.space 1
	.ascii "uLT_param_L_u8"
	.space 1
	.ascii "LT_var_L_u8"
	.space 1
	.ascii "uLT_var_L_u8"
	.space 1
	.ascii "LT_param_L_LC"
	.space 1
	.ascii "uLT_param_L_LC"
	.space 1
	.ascii "LT_var_L_LC"
	.space 1
	.ascii "uLT_var_L_LC"
	.space 1
	.ascii "LT_param_L_i64"
	.space 1
	.ascii "uLT_param_L_i64"
	.space 1
	.ascii "LT_var_L_i64"
	.space 1
	.ascii "uLT_var_L_i64"
	.space 1
	.ascii "LT_param_f64_f64"
	.space 1
	.ascii "LT_var_f64_f64"
	.space 1
	.ascii "LT_param_u64_i32"
	.space 1
	.ascii "uLT_param_u64_i32"
	.space 1
	.ascii "LT_var_u64_i32"
	.space 1
	.ascii "uLT_var_u64_i32"
	.space 1
	.ascii "LT_param_u64_i16"
	.space 1
	.ascii "uLT_param_u64_i16"
	.space 1
	.ascii "LT_var_u64_i16"
	.space 1
	.ascii "uLT_var_u64_i16"
	.space 1
	.ascii "LT_param_u64_L"
	.space 1
	.ascii "uLT_param_u64_L"
	.space 1
	.ascii "LT_var_u64_L"
	.space 1
	.ascii "uLT_var_u64_L"
	.space 1
	.ascii "LT_param_u64_u64"
	.space 1
	.ascii "uLT_param_u64_u64"
	.space 1
	.ascii "LT_var_u64_u64"
	.space 1
	.ascii "uLT_var_u64_u64"
	.space 1
	.ascii "LT_param_u64_u16"
	.space 1
	.ascii "uLT_param_u64_u16"
	.space 1
	.ascii "LT_var_u64_u16"
	.space 1
	.ascii "uLT_var_u64_u16"
	.space 1
	.ascii "LT_param_u64_C"
	.space 1
	.ascii "uLT_param_u64_C"
	.space 1
	.ascii "LT_var_u64_C"
	.space 1
	.ascii "uLT_var_u64_C"
	.space 1
	.ascii "LT_param_u64_u32"
	.space 1
	.ascii "uLT_param_u64_u32"
	.space 1
	.ascii "LT_var_u64_u32"
	.space 1
	.ascii "uLT_var_u64_u32"
	.space 1
	.ascii "LT_param_u64_i8"
	.space 1
	.ascii "uLT_param_u64_i8"
	.space 1
	.ascii "LT_var_u64_i8"
	.space 1
	.ascii "uLT_var_u64_i8"
	.space 1
	.ascii "LT_param_u64_I"
	.space 1
	.ascii "uLT_param_u64_I"
	.space 1
	.ascii "LT_var_u64_I"
	.space 1
	.ascii "uLT_var_u64_I"
	.space 1
	.ascii "LT_param_u64_u8"
	.space 1
	.ascii "uLT_param_u64_u8"
	.space 1
	.ascii "LT_var_u64_u8"
	.space 1
	.ascii "uLT_var_u64_u8"
	.space 1
	.ascii "LT_param_u64_LC"
	.space 1
	.ascii "uLT_param_u64_LC"
	.space 1
	.ascii "LT_var_u64_LC"
	.space 1
	.ascii "uLT_var_u64_LC"
	.space 1
	.ascii "LT_param_u64_i64"
	.space 1
	.ascii "uLT_param_u64_i64"
	.space 1
	.ascii "LT_var_u64_i64"
	.space 1
	.ascii "uLT_var_u64_i64"
	.space 1
	.ascii "LT_param_u16_i32"
	.space 1
	.ascii "uLT_param_u16_i32"
	.space 1
	.ascii "LT_var_u16_i32"
	.space 1
	.ascii "uLT_var_u16_i32"
	.space 1
	.ascii "LT_param_u16_i16"
	.space 1
	.ascii "uLT_param_u16_i16"
	.space 1
	.ascii "LT_var_u16_i16"
	.space 1
	.ascii "uLT_var_u16_i16"
	.space 1
	.ascii "LT_param_u16_L"
	.space 1
	.ascii "uLT_param_u16_L"
	.space 1
	.ascii "LT_var_u16_L"
	.space 1
	.ascii "uLT_var_u16_L"
	.space 1
	.ascii "LT_param_u16_u64"
	.space 1
	.ascii "uLT_param_u16_u64"
	.space 1
	.ascii "LT_var_u16_u64"
	.space 1
	.ascii "uLT_var_u16_u64"
	.space 1
	.ascii "LT_param_u16_u16"
	.space 1
	.ascii "uLT_param_u16_u16"
	.space 1
	.ascii "LT_var_u16_u16"
	.space 1
	.ascii "uLT_var_u16_u16"
	.space 1
	.ascii "LT_param_u16_C"
	.space 1
	.ascii "uLT_param_u16_C"
	.space 1
	.ascii "LT_var_u16_C"
	.space 1
	.ascii "uLT_var_u16_C"
	.space 1
	.ascii "LT_param_u16_u32"
	.space 1
	.ascii "uLT_param_u16_u32"
	.space 1
	.ascii "LT_var_u16_u32"
	.space 1
	.ascii "uLT_var_u16_u32"
	.space 1
	.ascii "LT_param_u16_i8"
	.space 1
	.ascii "uLT_param_u16_i8"
	.space 1
	.ascii "LT_var_u16_i8"
	.space 1
	.ascii "uLT_var_u16_i8"
	.space 1
	.ascii "LT_param_u16_I"
	.space 1
	.ascii "uLT_param_u16_I"
	.space 1
	.ascii "LT_var_u16_I"
	.space 1
	.ascii "uLT_var_u16_I"
	.space 1
	.ascii "LT_param_u16_u8"
	.space 1
	.ascii "uLT_param_u16_u8"
	.space 1
	.ascii "LT_var_u16_u8"
	.space 1
	.ascii "uLT_var_u16_u8"
	.space 1
	.ascii "LT_param_u16_LC"
	.space 1
	.ascii "uLT_param_u16_LC"
	.space 1
	.ascii "LT_var_u16_LC"
	.space 1
	.ascii "uLT_var_u16_LC"
	.space 1
	.ascii "LT_param_u16_i64"
	.space 1
	.ascii "uLT_param_u16_i64"
	.space 1
	.ascii "LT_var_u16_i64"
	.space 1
	.ascii "uLT_var_u16_i64"
	.space 1
	.ascii "LT_param_C_i32"
	.space 1
	.ascii "uLT_param_C_i32"
	.space 1
	.ascii "LT_var_C_i32"
	.space 1
	.ascii "uLT_var_C_i32"
	.space 1
	.ascii "LT_param_C_i16"
	.space 1
	.ascii "uLT_param_C_i16"
	.space 1
	.ascii "LT_var_C_i16"
	.space 1
	.ascii "uLT_var_C_i16"
	.space 1
	.ascii "LT_param_C_L"
	.space 1
	.ascii "uLT_param_C_L"
	.space 1
	.ascii "LT_var_C_L"
	.space 1
	.ascii "uLT_var_C_L"
	.space 1
	.ascii "LT_param_C_u64"
	.space 1
	.ascii "uLT_param_C_u64"
	.space 1
	.ascii "LT_var_C_u64"
	.space 1
	.ascii "uLT_var_C_u64"
	.space 1
	.ascii "LT_param_C_u16"
	.space 1
	.ascii "uLT_param_C_u16"
	.space 1
	.ascii "LT_var_C_u16"
	.space 1
	.ascii "uLT_var_C_u16"
	.space 1
	.ascii "LT_param_C_C"
	.space 1
	.ascii "uLT_param_C_C"
	.space 1
	.ascii "LT_var_C_C"
	.space 1
	.ascii "uLT_var_C_C"
	.space 1
	.ascii "LT_param_C_u32"
	.space 1
	.ascii "uLT_param_C_u32"
	.space 1
	.ascii "LT_var_C_u32"
	.space 1
	.ascii "uLT_var_C_u32"
	.space 1
	.ascii "LT_param_C_i8"
	.space 1
	.ascii "uLT_param_C_i8"
	.space 1
	.ascii "LT_var_C_i8"
	.space 1
	.ascii "uLT_var_C_i8"
	.space 1
	.ascii "LT_param_C_I"
	.space 1
	.ascii "uLT_param_C_I"
	.space 1
	.ascii "LT_var_C_I"
	.space 1
	.ascii "uLT_var_C_I"
	.space 1
	.ascii "LT_param_C_u8"
	.space 1
	.ascii "uLT_param_C_u8"
	.space 1
	.ascii "LT_var_C_u8"
	.space 1
	.ascii "uLT_var_C_u8"
	.space 1
	.ascii "LT_param_C_LC"
	.space 1
	.ascii "uLT_param_C_LC"
	.space 1
	.ascii "LT_var_C_LC"
	.space 1
	.ascii "uLT_var_C_LC"
	.space 1
	.ascii "LT_param_C_i64"
	.space 1
	.ascii "uLT_param_C_i64"
	.space 1
	.ascii "LT_var_C_i64"
	.space 1
	.ascii "uLT_var_C_i64"
	.space 1
	.ascii "LT_param_u32_i32"
	.space 1
	.ascii "uLT_param_u32_i32"
	.space 1
	.ascii "LT_var_u32_i32"
	.space 1
	.ascii "uLT_var_u32_i32"
	.space 1
	.ascii "LT_param_u32_i16"
	.space 1
	.ascii "uLT_param_u32_i16"
	.space 1
	.ascii "LT_var_u32_i16"
	.space 1
	.ascii "uLT_var_u32_i16"
	.space 1
	.ascii "LT_param_u32_L"
	.space 1
	.ascii "uLT_param_u32_L"
	.space 1
	.ascii "LT_var_u32_L"
	.space 1
	.ascii "uLT_var_u32_L"
	.space 1
	.ascii "LT_param_u32_u64"
	.space 1
	.ascii "uLT_param_u32_u64"
	.space 1
	.ascii "LT_var_u32_u64"
	.space 1
	.ascii "uLT_var_u32_u64"
	.space 1
	.ascii "LT_param_u32_u16"
	.space 1
	.ascii "uLT_param_u32_u16"
	.space 1
	.ascii "LT_var_u32_u16"
	.space 1
	.ascii "uLT_var_u32_u16"
	.space 1
	.ascii "LT_param_u32_C"
	.space 1
	.ascii "uLT_param_u32_C"
	.space 1
	.ascii "LT_var_u32_C"
	.space 1
	.ascii "uLT_var_u32_C"
	.space 1
	.ascii "LT_param_u32_u32"
	.space 1
	.ascii "uLT_param_u32_u32"
	.space 1
	.ascii "LT_var_u32_u32"
	.space 1
	.ascii "uLT_var_u32_u32"
	.space 1
	.ascii "LT_param_u32_i8"
	.space 1
	.ascii "uLT_param_u32_i8"
	.space 1
	.ascii "LT_var_u32_i8"
	.space 1
	.ascii "uLT_var_u32_i8"
	.space 1
	.ascii "LT_param_u32_I"
	.space 1
	.ascii "uLT_param_u32_I"
	.space 1
	.ascii "LT_var_u32_I"
	.space 1
	.ascii "uLT_var_u32_I"
	.space 1
	.ascii "LT_param_u32_u8"
	.space 1
	.ascii "uLT_param_u32_u8"
	.space 1
	.ascii "LT_var_u32_u8"
	.space 1
	.ascii "uLT_var_u32_u8"
	.space 1
	.ascii "LT_param_u32_LC"
	.space 1
	.ascii "uLT_param_u32_LC"
	.space 1
	.ascii "LT_var_u32_LC"
	.space 1
	.ascii "uLT_var_u32_LC"
	.space 1
	.ascii "LT_param_u32_i64"
	.space 1
	.ascii "uLT_param_u32_i64"
	.space 1
	.ascii "LT_var_u32_i64"
	.space 1
	.ascii "uLT_var_u32_i64"
	.space 1
	.ascii "LT_param_i8_i32"
	.space 1
	.ascii "uLT_param_i8_i32"
	.space 1
	.ascii "LT_var_i8_i32"
	.space 1
	.ascii "uLT_var_i8_i32"
	.space 1
	.ascii "LT_param_i8_i16"
	.space 1
	.ascii "uLT_param_i8_i16"
	.space 1
	.ascii "LT_var_i8_i16"
	.space 1
	.ascii "uLT_var_i8_i16"
	.space 1
	.ascii "LT_param_i8_L"
	.space 1
	.ascii "uLT_param_i8_L"
	.space 1
	.ascii "LT_var_i8_L"
	.space 1
	.ascii "uLT_var_i8_L"
	.space 1
	.ascii "LT_param_i8_u64"
	.space 1
	.ascii "uLT_param_i8_u64"
	.space 1
	.ascii "LT_var_i8_u64"
	.space 1
	.ascii "uLT_var_i8_u64"
	.space 1
	.ascii "LT_param_i8_u16"
	.space 1
	.ascii "uLT_param_i8_u16"
	.space 1
	.ascii "LT_var_i8_u16"
	.space 1
	.ascii "uLT_var_i8_u16"
	.space 1
	.ascii "LT_param_i8_C"
	.space 1
	.ascii "uLT_param_i8_C"
	.space 1
	.ascii "LT_var_i8_C"
	.space 1
	.ascii "uLT_var_i8_C"
	.space 1
	.ascii "LT_param_i8_u32"
	.space 1
	.ascii "uLT_param_i8_u32"
	.space 1
	.ascii "LT_var_i8_u32"
	.space 1
	.ascii "uLT_var_i8_u32"
	.space 1
	.ascii "LT_param_i8_i8"
	.space 1
	.ascii "uLT_param_i8_i8"
	.space 1
	.ascii "LT_var_i8_i8"
	.space 1
	.ascii "uLT_var_i8_i8"
	.space 1
	.ascii "LT_param_i8_I"
	.space 1
	.ascii "uLT_param_i8_I"
	.space 1
	.ascii "LT_var_i8_I"
	.space 1
	.ascii "uLT_var_i8_I"
	.space 1
	.ascii "LT_param_i8_u8"
	.space 1
	.ascii "uLT_param_i8_u8"
	.space 1
	.ascii "LT_var_i8_u8"
	.space 1
	.ascii "uLT_var_i8_u8"
	.space 1
	.ascii "LT_param_i8_LC"
	.space 1
	.ascii "uLT_param_i8_LC"
	.space 1
	.ascii "LT_var_i8_LC"
	.space 1
	.ascii "uLT_var_i8_LC"
	.space 1
	.ascii "LT_param_i8_i64"
	.space 1
	.ascii "uLT_param_i8_i64"
	.space 1
	.ascii "LT_var_i8_i64"
	.space 1
	.ascii "uLT_var_i8_i64"
	.space 1
	.ascii "LT_param_I_i32"
	.space 1
	.ascii "uLT_param_I_i32"
	.space 1
	.ascii "LT_var_I_i32"
	.space 1
	.ascii "uLT_var_I_i32"
	.space 1
	.ascii "LT_param_I_i16"
	.space 1
	.ascii "uLT_param_I_i16"
	.space 1
	.ascii "LT_var_I_i16"
	.space 1
	.ascii "uLT_var_I_i16"
	.space 1
	.ascii "LT_param_I_L"
	.space 1
	.ascii "uLT_param_I_L"
	.space 1
	.ascii "LT_var_I_L"
	.space 1
	.ascii "uLT_var_I_L"
	.space 1
	.ascii "LT_param_I_u64"
	.space 1
	.ascii "uLT_param_I_u64"
	.space 1
	.ascii "LT_var_I_u64"
	.space 1
	.ascii "uLT_var_I_u64"
	.space 1
	.ascii "LT_param_I_u16"
	.space 1
	.ascii "uLT_param_I_u16"
	.space 1
	.ascii "LT_var_I_u16"
	.space 1
	.ascii "uLT_var_I_u16"
	.space 1
	.ascii "LT_param_I_C"
	.space 1
	.ascii "uLT_param_I_C"
	.space 1
	.ascii "LT_var_I_C"
	.space 1
	.ascii "uLT_var_I_C"
	.space 1
	.ascii "LT_param_I_u32"
	.space 1
	.ascii "uLT_param_I_u32"
	.space 1
	.ascii "LT_var_I_u32"
	.space 1
	.ascii "uLT_var_I_u32"
	.space 1
	.ascii "LT_param_I_i8"
	.space 1
	.ascii "uLT_param_I_i8"
	.space 1
	.ascii "LT_var_I_i8"
	.space 1
	.ascii "uLT_var_I_i8"
	.space 1
	.ascii "LT_param_I_I"
	.space 1
	.ascii "uLT_param_I_I"
	.space 1
	.ascii "LT_var_I_I"
	.space 1
	.ascii "uLT_var_I_I"
	.space 1
	.ascii "LT_param_I_u8"
	.space 1
	.ascii "uLT_param_I_u8"
	.space 1
	.ascii "LT_var_I_u8"
	.space 1
	.ascii "uLT_var_I_u8"
	.space 1
	.ascii "LT_param_I_LC"
	.space 1
	.ascii "uLT_param_I_LC"
	.space 1
	.ascii "LT_var_I_LC"
	.space 1
	.ascii "uLT_var_I_LC"
	.space 1
	.ascii "LT_param_I_i64"
	.space 1
	.ascii "uLT_param_I_i64"
	.space 1
	.ascii "LT_var_I_i64"
	.space 1
	.ascii "uLT_var_I_i64"
	.space 1
	.ascii "LT_param_u8_i32"
	.space 1
	.ascii "uLT_param_u8_i32"
	.space 1
	.ascii "LT_var_u8_i32"
	.space 1
	.ascii "uLT_var_u8_i32"
	.space 1
	.ascii "LT_param_u8_i16"
	.space 1
	.ascii "uLT_param_u8_i16"
	.space 1
	.ascii "LT_var_u8_i16"
	.space 1
	.ascii "uLT_var_u8_i16"
	.space 1
	.ascii "LT_param_u8_L"
	.space 1
	.ascii "uLT_param_u8_L"
	.space 1
	.ascii "LT_var_u8_L"
	.space 1
	.ascii "uLT_var_u8_L"
	.space 1
	.ascii "LT_param_u8_u64"
	.space 1
	.ascii "uLT_param_u8_u64"
	.space 1
	.ascii "LT_var_u8_u64"
	.space 1
	.ascii "uLT_var_u8_u64"
	.space 1
	.ascii "LT_param_u8_u16"
	.space 1
	.ascii "uLT_param_u8_u16"
	.space 1
	.ascii "LT_var_u8_u16"
	.space 1
	.ascii "uLT_var_u8_u16"
	.space 1
	.ascii "LT_param_u8_C"
	.space 1
	.ascii "uLT_param_u8_C"
	.space 1
	.ascii "LT_var_u8_C"
	.space 1
	.ascii "uLT_var_u8_C"
	.space 1
	.ascii "LT_param_u8_u32"
	.space 1
	.ascii "uLT_param_u8_u32"
	.space 1
	.ascii "LT_var_u8_u32"
	.space 1
	.ascii "uLT_var_u8_u32"
	.space 1
	.ascii "LT_param_u8_i8"
	.space 1
	.ascii "uLT_param_u8_i8"
	.space 1
	.ascii "LT_var_u8_i8"
	.space 1
	.ascii "uLT_var_u8_i8"
	.space 1
	.ascii "LT_param_u8_I"
	.space 1
	.ascii "uLT_param_u8_I"
	.space 1
	.ascii "LT_var_u8_I"
	.space 1
	.ascii "uLT_var_u8_I"
	.space 1
	.ascii "LT_param_u8_u8"
	.space 1
	.ascii "uLT_param_u8_u8"
	.space 1
	.ascii "LT_var_u8_u8"
	.space 1
	.ascii "uLT_var_u8_u8"
	.space 1
	.ascii "LT_param_u8_LC"
	.space 1
	.ascii "uLT_param_u8_LC"
	.space 1
	.ascii "LT_var_u8_LC"
	.space 1
	.ascii "uLT_var_u8_LC"
	.space 1
	.ascii "LT_param_u8_i64"
	.space 1
	.ascii "uLT_param_u8_i64"
	.space 1
	.ascii "LT_var_u8_i64"
	.space 1
	.ascii "uLT_var_u8_i64"
	.space 1
	.ascii "LT_param_f32_f32"
	.space 1
	.ascii "LT_var_f32_f32"
	.space 1
	.ascii "LT_param_LC_i32"
	.space 1
	.ascii "uLT_param_LC_i32"
	.space 1
	.ascii "LT_var_LC_i32"
	.space 1
	.ascii "uLT_var_LC_i32"
	.space 1
	.ascii "LT_param_LC_i16"
	.space 1
	.ascii "uLT_param_LC_i16"
	.space 1
	.ascii "LT_var_LC_i16"
	.space 1
	.ascii "uLT_var_LC_i16"
	.space 1
	.ascii "LT_param_LC_L"
	.space 1
	.ascii "uLT_param_LC_L"
	.space 1
	.ascii "LT_var_LC_L"
	.space 1
	.ascii "uLT_var_LC_L"
	.space 1
	.ascii "LT_param_LC_u64"
	.space 1
	.ascii "uLT_param_LC_u64"
	.space 1
	.ascii "LT_var_LC_u64"
	.space 1
	.ascii "uLT_var_LC_u64"
	.space 1
	.ascii "LT_param_LC_u16"
	.space 1
	.ascii "uLT_param_LC_u16"
	.space 1
	.ascii "LT_var_LC_u16"
	.space 1
	.ascii "uLT_var_LC_u16"
	.space 1
	.ascii "LT_param_LC_C"
	.space 1
	.ascii "uLT_param_LC_C"
	.space 1
	.ascii "LT_var_LC_C"
	.space 1
	.ascii "uLT_var_LC_C"
	.space 1
	.ascii "LT_param_LC_u32"
	.space 1
	.ascii "uLT_param_LC_u32"
	.space 1
	.ascii "LT_var_LC_u32"
	.space 1
	.ascii "uLT_var_LC_u32"
	.space 1
	.ascii "LT_param_LC_i8"
	.space 1
	.ascii "uLT_param_LC_i8"
	.space 1
	.ascii "LT_var_LC_i8"
	.space 1
	.ascii "uLT_var_LC_i8"
	.space 1
	.ascii "LT_param_LC_I"
	.space 1
	.ascii "uLT_param_LC_I"
	.space 1
	.ascii "LT_var_LC_I"
	.space 1
	.ascii "uLT_var_LC_I"
	.space 1
	.ascii "LT_param_LC_u8"
	.space 1
	.ascii "uLT_param_LC_u8"
	.space 1
	.ascii "LT_var_LC_u8"
	.space 1
	.ascii "uLT_var_LC_u8"
	.space 1
	.ascii "LT_param_LC_LC"
	.space 1
	.ascii "uLT_param_LC_LC"
	.space 1
	.ascii "LT_var_LC_LC"
	.space 1
	.ascii "uLT_var_LC_LC"
	.space 1
	.ascii "LT_param_LC_i64"
	.space 1
	.ascii "uLT_param_LC_i64"
	.space 1
	.ascii "LT_var_LC_i64"
	.space 1
	.ascii "uLT_var_LC_i64"
	.space 1
	.ascii "LT_param_i64_i32"
	.space 1
	.ascii "uLT_param_i64_i32"
	.space 1
	.ascii "LT_var_i64_i32"
	.space 1
	.ascii "uLT_var_i64_i32"
	.space 1
	.ascii "LT_param_i64_i16"
	.space 1
	.ascii "uLT_param_i64_i16"
	.space 1
	.ascii "LT_var_i64_i16"
	.space 1
	.ascii "uLT_var_i64_i16"
	.space 1
	.ascii "LT_param_i64_L"
	.space 1
	.ascii "uLT_param_i64_L"
	.space 1
	.ascii "LT_var_i64_L"
	.space 1
	.ascii "uLT_var_i64_L"
	.space 1
	.ascii "LT_param_i64_u64"
	.space 1
	.ascii "uLT_param_i64_u64"
	.space 1
	.ascii "LT_var_i64_u64"
	.space 1
	.ascii "uLT_var_i64_u64"
	.space 1
	.ascii "LT_param_i64_u16"
	.space 1
	.ascii "uLT_param_i64_u16"
	.space 1
	.ascii "LT_var_i64_u16"
	.space 1
	.ascii "uLT_var_i64_u16"
	.space 1
	.ascii "LT_param_i64_C"
	.space 1
	.ascii "uLT_param_i64_C"
	.space 1
	.ascii "LT_var_i64_C"
	.space 1
	.ascii "uLT_var_i64_C"
	.space 1
	.ascii "LT_param_i64_u32"
	.space 1
	.ascii "uLT_param_i64_u32"
	.space 1
	.ascii "LT_var_i64_u32"
	.space 1
	.ascii "uLT_var_i64_u32"
	.space 1
	.ascii "LT_param_i64_i8"
	.space 1
	.ascii "uLT_param_i64_i8"
	.space 1
	.ascii "LT_var_i64_i8"
	.space 1
	.ascii "uLT_var_i64_i8"
	.space 1
	.ascii "LT_param_i64_I"
	.space 1
	.ascii "uLT_param_i64_I"
	.space 1
	.ascii "LT_var_i64_I"
	.space 1
	.ascii "uLT_var_i64_I"
	.space 1
	.ascii "LT_param_i64_u8"
	.space 1
	.ascii "uLT_param_i64_u8"
	.space 1
	.ascii "LT_var_i64_u8"
	.space 1
	.ascii "uLT_var_i64_u8"
	.space 1
	.ascii "LT_param_i64_LC"
	.space 1
	.ascii "uLT_param_i64_LC"
	.space 1
	.ascii "LT_var_i64_LC"
	.space 1
	.ascii "uLT_var_i64_LC"
	.space 1
	.ascii "LT_param_i64_i64"
	.space 1
	.ascii "uLT_param_i64_i64"
	.space 1
	.ascii "LT_var_i64_i64"
	.space 1
	.ascii "uLT_var_i64_i64"
	.space 3
	.quad	_LT_M3
	.quad	_L_1
	.quad	_LT__LT_param_i32_i32
	.quad	_L_1+6
	.quad	_LT__uLT_param_i32_i32
	.quad	_L_1+23
	.quad	_LT__LT_var_i32_i32
	.quad	_L_1+41
	.quad	_LT__uLT_var_i32_i32
	.quad	_L_1+56
	.quad	_LT__LT_param_i32_i16
	.quad	_L_1+72
	.quad	_LT__uLT_param_i32_i16
	.quad	_L_1+89
	.quad	_LT__LT_var_i32_i16
	.quad	_L_1+107
	.quad	_LT__uLT_var_i32_i16
	.quad	_L_1+122
	.quad	_LT__LT_param_i32_L
	.quad	_L_1+138
	.quad	_LT__uLT_param_i32_L
	.quad	_L_1+153
	.quad	_LT__LT_var_i32_L
	.quad	_L_1+169
	.quad	_LT__uLT_var_i32_L
	.quad	_L_1+182
	.quad	_LT__LT_param_i32_u64
	.quad	_L_1+196
	.quad	_LT__uLT_param_i32_u64
	.quad	_L_1+213
	.quad	_LT__LT_var_i32_u64
	.quad	_L_1+231
	.quad	_LT__uLT_var_i32_u64
	.quad	_L_1+246
	.quad	_LT__LT_param_i32_u16
	.quad	_L_1+262
	.quad	_LT__uLT_param_i32_u16
	.quad	_L_1+279
	.quad	_LT__LT_var_i32_u16
	.quad	_L_1+297
	.quad	_LT__uLT_var_i32_u16
	.quad	_L_1+312
	.quad	_LT__LT_param_i32_C
	.quad	_L_1+328
	.quad	_LT__uLT_param_i32_C
	.quad	_L_1+343
	.quad	_LT__LT_var_i32_C
	.quad	_L_1+359
	.quad	_LT__uLT_var_i32_C
	.quad	_L_1+372
	.quad	_LT__LT_param_i32_u32
	.quad	_L_1+386
	.quad	_LT__uLT_param_i32_u32
	.quad	_L_1+403
	.quad	_LT__LT_var_i32_u32
	.quad	_L_1+421
	.quad	_LT__uLT_var_i32_u32
	.quad	_L_1+436
	.quad	_LT__LT_param_i32_i8
	.quad	_L_1+452
	.quad	_LT__uLT_param_i32_i8
	.quad	_L_1+468
	.quad	_LT__LT_var_i32_i8
	.quad	_L_1+485
	.quad	_LT__uLT_var_i32_i8
	.quad	_L_1+499
	.quad	_LT__LT_param_i32_I
	.quad	_L_1+514
	.quad	_LT__uLT_param_i32_I
	.quad	_L_1+529
	.quad	_LT__LT_var_i32_I
	.quad	_L_1+545
	.quad	_LT__uLT_var_i32_I
	.quad	_L_1+558
	.quad	_LT__LT_param_i32_u8
	.quad	_L_1+572
	.quad	_LT__uLT_param_i32_u8
	.quad	_L_1+588
	.quad	_LT__LT_var_i32_u8
	.quad	_L_1+605
	.quad	_LT__uLT_var_i32_u8
	.quad	_L_1+619
	.quad	_LT__LT_param_i32_LC
	.quad	_L_1+634
	.quad	_LT__uLT_param_i32_LC
	.quad	_L_1+650
	.quad	_LT__LT_var_i32_LC
	.quad	_L_1+667
	.quad	_LT__uLT_var_i32_LC
	.quad	_L_1+681
	.quad	_LT__LT_param_i32_i64
	.quad	_L_1+696
	.quad	_LT__uLT_param_i32_i64
	.quad	_L_1+713
	.quad	_LT__LT_var_i32_i64
	.quad	_L_1+731
	.quad	_LT__uLT_var_i32_i64
	.quad	_L_1+746
	.quad	_LT__LT_param_i16_i32
	.quad	_L_1+762
	.quad	_LT__uLT_param_i16_i32
	.quad	_L_1+779
	.quad	_LT__LT_var_i16_i32
	.quad	_L_1+797
	.quad	_LT__uLT_var_i16_i32
	.quad	_L_1+812
	.quad	_LT__LT_param_i16_i16
	.quad	_L_1+828
	.quad	_LT__uLT_param_i16_i16
	.quad	_L_1+845
	.quad	_LT__LT_var_i16_i16
	.quad	_L_1+863
	.quad	_LT__uLT_var_i16_i16
	.quad	_L_1+878
	.quad	_LT__LT_param_i16_L
	.quad	_L_1+894
	.quad	_LT__uLT_param_i16_L
	.quad	_L_1+909
	.quad	_LT__LT_var_i16_L
	.quad	_L_1+925
	.quad	_LT__uLT_var_i16_L
	.quad	_L_1+938
	.quad	_LT__LT_param_i16_u64
	.quad	_L_1+952
	.quad	_LT__uLT_param_i16_u64
	.quad	_L_1+969
	.quad	_LT__LT_var_i16_u64
	.quad	_L_1+987
	.quad	_LT__uLT_var_i16_u64
	.quad	_L_1+1002
	.quad	_LT__LT_param_i16_u16
	.quad	_L_1+1018
	.quad	_LT__uLT_param_i16_u16
	.quad	_L_1+1035
	.quad	_LT__LT_var_i16_u16
	.quad	_L_1+1053
	.quad	_LT__uLT_var_i16_u16
	.quad	_L_1+1068
	.quad	_LT__LT_param_i16_C
	.quad	_L_1+1084
	.quad	_LT__uLT_param_i16_C
	.quad	_L_1+1099
	.quad	_LT__LT_var_i16_C
	.quad	_L_1+1115
	.quad	_LT__uLT_var_i16_C
	.quad	_L_1+1128
	.quad	_LT__LT_param_i16_u32
	.quad	_L_1+1142
	.quad	_LT__uLT_param_i16_u32
	.quad	_L_1+1159
	.quad	_LT__LT_var_i16_u32
	.quad	_L_1+1177
	.quad	_LT__uLT_var_i16_u32
	.quad	_L_1+1192
	.quad	_LT__LT_param_i16_i8
	.quad	_L_1+1208
	.quad	_LT__uLT_param_i16_i8
	.quad	_L_1+1224
	.quad	_LT__LT_var_i16_i8
	.quad	_L_1+1241
	.quad	_LT__uLT_var_i16_i8
	.quad	_L_1+1255
	.quad	_LT__LT_param_i16_I
	.quad	_L_1+1270
	.quad	_LT__uLT_param_i16_I
	.quad	_L_1+1285
	.quad	_LT__LT_var_i16_I
	.quad	_L_1+1301
	.quad	_LT__uLT_var_i16_I
	.quad	_L_1+1314
	.quad	_LT__LT_param_i16_u8
	.quad	_L_1+1328
	.quad	_LT__uLT_param_i16_u8
	.quad	_L_1+1344
	.quad	_LT__LT_var_i16_u8
	.quad	_L_1+1361
	.quad	_LT__uLT_var_i16_u8
	.quad	_L_1+1375
	.quad	_LT__LT_param_i16_LC
	.quad	_L_1+1390
	.quad	_LT__uLT_param_i16_LC
	.quad	_L_1+1406
	.quad	_LT__LT_var_i16_LC
	.quad	_L_1+1423
	.quad	_LT__uLT_var_i16_LC
	.quad	_L_1+1437
	.quad	_LT__LT_param_i16_i64
	.quad	_L_1+1452
	.quad	_LT__uLT_param_i16_i64
	.quad	_L_1+1469
	.quad	_LT__LT_var_i16_i64
	.quad	_L_1+1487
	.quad	_LT__uLT_var_i16_i64
	.quad	_L_1+1502
	.quad	_LT__LT_param_L_i32
	.quad	_L_1+1518
	.quad	_LT__uLT_param_L_i32
	.quad	_L_1+1533
	.quad	_LT__LT_var_L_i32
	.quad	_L_1+1549
	.quad	_LT__uLT_var_L_i32
	.quad	_L_1+1562
	.quad	_LT__LT_param_L_i16
	.quad	_L_1+1576
	.quad	_LT__uLT_param_L_i16
	.quad	_L_1+1591
	.quad	_LT__LT_var_L_i16
	.quad	_L_1+1607
	.quad	_LT__uLT_var_L_i16
	.quad	_L_1+1620
	.quad	_LT__LT_param_L_L
	.quad	_L_1+1634
	.quad	_LT__uLT_param_L_L
	.quad	_L_1+1647
	.quad	_LT__LT_var_L_L
	.quad	_L_1+1661
	.quad	_LT__uLT_var_L_L
	.quad	_L_1+1672
	.quad	_LT__LT_param_L_u64
	.quad	_L_1+1684
	.quad	_LT__uLT_param_L_u64
	.quad	_L_1+1699
	.quad	_LT__LT_var_L_u64
	.quad	_L_1+1715
	.quad	_LT__uLT_var_L_u64
	.quad	_L_1+1728
	.quad	_LT__LT_param_L_u16
	.quad	_L_1+1742
	.quad	_LT__uLT_param_L_u16
	.quad	_L_1+1757
	.quad	_LT__LT_var_L_u16
	.quad	_L_1+1773
	.quad	_LT__uLT_var_L_u16
	.quad	_L_1+1786
	.quad	_LT__LT_param_L_C
	.quad	_L_1+1800
	.quad	_LT__uLT_param_L_C
	.quad	_L_1+1813
	.quad	_LT__LT_var_L_C
	.quad	_L_1+1827
	.quad	_LT__uLT_var_L_C
	.quad	_L_1+1838
	.quad	_LT__LT_param_L_u32
	.quad	_L_1+1850
	.quad	_LT__uLT_param_L_u32
	.quad	_L_1+1865
	.quad	_LT__LT_var_L_u32
	.quad	_L_1+1881
	.quad	_LT__uLT_var_L_u32
	.quad	_L_1+1894
	.quad	_LT__LT_param_L_i8
	.quad	_L_1+1908
	.quad	_LT__uLT_param_L_i8
	.quad	_L_1+1922
	.quad	_LT__LT_var_L_i8
	.quad	_L_1+1937
	.quad	_LT__uLT_var_L_i8
	.quad	_L_1+1949
	.quad	_LT__LT_param_L_I
	.quad	_L_1+1962
	.quad	_LT__uLT_param_L_I
	.quad	_L_1+1975
	.quad	_LT__LT_var_L_I
	.quad	_L_1+1989
	.quad	_LT__uLT_var_L_I
	.quad	_L_1+2000
	.quad	_LT__LT_param_L_u8
	.quad	_L_1+2012
	.quad	_LT__uLT_param_L_u8
	.quad	_L_1+2026
	.quad	_LT__LT_var_L_u8
	.quad	_L_1+2041
	.quad	_LT__uLT_var_L_u8
	.quad	_L_1+2053
	.quad	_LT__LT_param_L_LC
	.quad	_L_1+2066
	.quad	_LT__uLT_param_L_LC
	.quad	_L_1+2080
	.quad	_LT__LT_var_L_LC
	.quad	_L_1+2095
	.quad	_LT__uLT_var_L_LC
	.quad	_L_1+2107
	.quad	_LT__LT_param_L_i64
	.quad	_L_1+2120
	.quad	_LT__uLT_param_L_i64
	.quad	_L_1+2135
	.quad	_LT__LT_var_L_i64
	.quad	_L_1+2151
	.quad	_LT__uLT_var_L_i64
	.quad	_L_1+2164
	.quad	_LT__LT_param_f64_f64
	.quad	_L_1+2178
	.quad	_LT__LT_var_f64_f64
	.quad	_L_1+2195
	.quad	_LT__LT_param_u64_i32
	.quad	_L_1+2210
	.quad	_LT__uLT_param_u64_i32
	.quad	_L_1+2227
	.quad	_LT__LT_var_u64_i32
	.quad	_L_1+2245
	.quad	_LT__uLT_var_u64_i32
	.quad	_L_1+2260
	.quad	_LT__LT_param_u64_i16
	.quad	_L_1+2276
	.quad	_LT__uLT_param_u64_i16
	.quad	_L_1+2293
	.quad	_LT__LT_var_u64_i16
	.quad	_L_1+2311
	.quad	_LT__uLT_var_u64_i16
	.quad	_L_1+2326
	.quad	_LT__LT_param_u64_L
	.quad	_L_1+2342
	.quad	_LT__uLT_param_u64_L
	.quad	_L_1+2357
	.quad	_LT__LT_var_u64_L
	.quad	_L_1+2373
	.quad	_LT__uLT_var_u64_L
	.quad	_L_1+2386
	.quad	_LT__LT_param_u64_u64
	.quad	_L_1+2400
	.quad	_LT__uLT_param_u64_u64
	.quad	_L_1+2417
	.quad	_LT__LT_var_u64_u64
	.quad	_L_1+2435
	.quad	_LT__uLT_var_u64_u64
	.quad	_L_1+2450
	.quad	_LT__LT_param_u64_u16
	.quad	_L_1+2466
	.quad	_LT__uLT_param_u64_u16
	.quad	_L_1+2483
	.quad	_LT__LT_var_u64_u16
	.quad	_L_1+2501
	.quad	_LT__uLT_var_u64_u16
	.quad	_L_1+2516
	.quad	_LT__LT_param_u64_C
	.quad	_L_1+2532
	.quad	_LT__uLT_param_u64_C
	.quad	_L_1+2547
	.quad	_LT__LT_var_u64_C
	.quad	_L_1+2563
	.quad	_LT__uLT_var_u64_C
	.quad	_L_1+2576
	.quad	_LT__LT_param_u64_u32
	.quad	_L_1+2590
	.quad	_LT__uLT_param_u64_u32
	.quad	_L_1+2607
	.quad	_LT__LT_var_u64_u32
	.quad	_L_1+2625
	.quad	_LT__uLT_var_u64_u32
	.quad	_L_1+2640
	.quad	_LT__LT_param_u64_i8
	.quad	_L_1+2656
	.quad	_LT__uLT_param_u64_i8
	.quad	_L_1+2672
	.quad	_LT__LT_var_u64_i8
	.quad	_L_1+2689
	.quad	_LT__uLT_var_u64_i8
	.quad	_L_1+2703
	.quad	_LT__LT_param_u64_I
	.quad	_L_1+2718
	.quad	_LT__uLT_param_u64_I
	.quad	_L_1+2733
	.quad	_LT__LT_var_u64_I
	.quad	_L_1+2749
	.quad	_LT__uLT_var_u64_I
	.quad	_L_1+2762
	.quad	_LT__LT_param_u64_u8
	.quad	_L_1+2776
	.quad	_LT__uLT_param_u64_u8
	.quad	_L_1+2792
	.quad	_LT__LT_var_u64_u8
	.quad	_L_1+2809
	.quad	_LT__uLT_var_u64_u8
	.quad	_L_1+2823
	.quad	_LT__LT_param_u64_LC
	.quad	_L_1+2838
	.quad	_LT__uLT_param_u64_LC
	.quad	_L_1+2854
	.quad	_LT__LT_var_u64_LC
	.quad	_L_1+2871
	.quad	_LT__uLT_var_u64_LC
	.quad	_L_1+2885
	.quad	_LT__LT_param_u64_i64
	.quad	_L_1+2900
	.quad	_LT__uLT_param_u64_i64
	.quad	_L_1+2917
	.quad	_LT__LT_var_u64_i64
	.quad	_L_1+2935
	.quad	_LT__uLT_var_u64_i64
	.quad	_L_1+2950
	.quad	_LT__LT_param_u16_i32
	.quad	_L_1+2966
	.quad	_LT__uLT_param_u16_i32
	.quad	_L_1+2983
	.quad	_LT__LT_var_u16_i32
	.quad	_L_1+3001
	.quad	_LT__uLT_var_u16_i32
	.quad	_L_1+3016
	.quad	_LT__LT_param_u16_i16
	.quad	_L_1+3032
	.quad	_LT__uLT_param_u16_i16
	.quad	_L_1+3049
	.quad	_LT__LT_var_u16_i16
	.quad	_L_1+3067
	.quad	_LT__uLT_var_u16_i16
	.quad	_L_1+3082
	.quad	_LT__LT_param_u16_L
	.quad	_L_1+3098
	.quad	_LT__uLT_param_u16_L
	.quad	_L_1+3113
	.quad	_LT__LT_var_u16_L
	.quad	_L_1+3129
	.quad	_LT__uLT_var_u16_L
	.quad	_L_1+3142
	.quad	_LT__LT_param_u16_u64
	.quad	_L_1+3156
	.quad	_LT__uLT_param_u16_u64
	.quad	_L_1+3173
	.quad	_LT__LT_var_u16_u64
	.quad	_L_1+3191
	.quad	_LT__uLT_var_u16_u64
	.quad	_L_1+3206
	.quad	_LT__LT_param_u16_u16
	.quad	_L_1+3222
	.quad	_LT__uLT_param_u16_u16
	.quad	_L_1+3239
	.quad	_LT__LT_var_u16_u16
	.quad	_L_1+3257
	.quad	_LT__uLT_var_u16_u16
	.quad	_L_1+3272
	.quad	_LT__LT_param_u16_C
	.quad	_L_1+3288
	.quad	_LT__uLT_param_u16_C
	.quad	_L_1+3303
	.quad	_LT__LT_var_u16_C
	.quad	_L_1+3319
	.quad	_LT__uLT_var_u16_C
	.quad	_L_1+3332
	.quad	_LT__LT_param_u16_u32
	.quad	_L_1+3346
	.quad	_LT__uLT_param_u16_u32
	.quad	_L_1+3363
	.quad	_LT__LT_var_u16_u32
	.quad	_L_1+3381
	.quad	_LT__uLT_var_u16_u32
	.quad	_L_1+3396
	.quad	_LT__LT_param_u16_i8
	.quad	_L_1+3412
	.quad	_LT__uLT_param_u16_i8
	.quad	_L_1+3428
	.quad	_LT__LT_var_u16_i8
	.quad	_L_1+3445
	.quad	_LT__uLT_var_u16_i8
	.quad	_L_1+3459
	.quad	_LT__LT_param_u16_I
	.quad	_L_1+3474
	.quad	_LT__uLT_param_u16_I
	.quad	_L_1+3489
	.quad	_LT__LT_var_u16_I
	.quad	_L_1+3505
	.quad	_LT__uLT_var_u16_I
	.quad	_L_1+3518
	.quad	_LT__LT_param_u16_u8
	.quad	_L_1+3532
	.quad	_LT__uLT_param_u16_u8
	.quad	_L_1+3548
	.quad	_LT__LT_var_u16_u8
	.quad	_L_1+3565
	.quad	_LT__uLT_var_u16_u8
	.quad	_L_1+3579
	.quad	_LT__LT_param_u16_LC
	.quad	_L_1+3594
	.quad	_LT__uLT_param_u16_LC
	.quad	_L_1+3610
	.quad	_LT__LT_var_u16_LC
	.quad	_L_1+3627
	.quad	_LT__uLT_var_u16_LC
	.quad	_L_1+3641
	.quad	_LT__LT_param_u16_i64
	.quad	_L_1+3656
	.quad	_LT__uLT_param_u16_i64
	.quad	_L_1+3673
	.quad	_LT__LT_var_u16_i64
	.quad	_L_1+3691
	.quad	_LT__uLT_var_u16_i64
	.quad	_L_1+3706
	.quad	_LT__LT_param_C_i32
	.quad	_L_1+3722
	.quad	_LT__uLT_param_C_i32
	.quad	_L_1+3737
	.quad	_LT__LT_var_C_i32
	.quad	_L_1+3753
	.quad	_LT__uLT_var_C_i32
	.quad	_L_1+3766
	.quad	_LT__LT_param_C_i16
	.quad	_L_1+3780
	.quad	_LT__uLT_param_C_i16
	.quad	_L_1+3795
	.quad	_LT__LT_var_C_i16
	.quad	_L_1+3811
	.quad	_LT__uLT_var_C_i16
	.quad	_L_1+3824
	.quad	_LT__LT_param_C_L
	.quad	_L_1+3838
	.quad	_LT__uLT_param_C_L
	.quad	_L_1+3851
	.quad	_LT__LT_var_C_L
	.quad	_L_1+3865
	.quad	_LT__uLT_var_C_L
	.quad	_L_1+3876
	.quad	_LT__LT_param_C_u64
	.quad	_L_1+3888
	.quad	_LT__uLT_param_C_u64
	.quad	_L_1+3903
	.quad	_LT__LT_var_C_u64
	.quad	_L_1+3919
	.quad	_LT__uLT_var_C_u64
	.quad	_L_1+3932
	.quad	_LT__LT_param_C_u16
	.quad	_L_1+3946
	.quad	_LT__uLT_param_C_u16
	.quad	_L_1+3961
	.quad	_LT__LT_var_C_u16
	.quad	_L_1+3977
	.quad	_LT__uLT_var_C_u16
	.quad	_L_1+3990
	.quad	_LT__LT_param_C_C
	.quad	_L_1+4004
	.quad	_LT__uLT_param_C_C
	.quad	_L_1+4017
	.quad	_LT__LT_var_C_C
	.quad	_L_1+4031
	.quad	_LT__uLT_var_C_C
	.quad	_L_1+4042
	.quad	_LT__LT_param_C_u32
	.quad	_L_1+4054
	.quad	_LT__uLT_param_C_u32
	.quad	_L_1+4069
	.quad	_LT__LT_var_C_u32
	.quad	_L_1+4085
	.quad	_LT__uLT_var_C_u32
	.quad	_L_1+4098
	.quad	_LT__LT_param_C_i8
	.quad	_L_1+4112
	.quad	_LT__uLT_param_C_i8
	.quad	_L_1+4126
	.quad	_LT__LT_var_C_i8
	.quad	_L_1+4141
	.quad	_LT__uLT_var_C_i8
	.quad	_L_1+4153
	.quad	_LT__LT_param_C_I
	.quad	_L_1+4166
	.quad	_LT__uLT_param_C_I
	.quad	_L_1+4179
	.quad	_LT__LT_var_C_I
	.quad	_L_1+4193
	.quad	_LT__uLT_var_C_I
	.quad	_L_1+4204
	.quad	_LT__LT_param_C_u8
	.quad	_L_1+4216
	.quad	_LT__uLT_param_C_u8
	.quad	_L_1+4230
	.quad	_LT__LT_var_C_u8
	.quad	_L_1+4245
	.quad	_LT__uLT_var_C_u8
	.quad	_L_1+4257
	.quad	_LT__LT_param_C_LC
	.quad	_L_1+4270
	.quad	_LT__uLT_param_C_LC
	.quad	_L_1+4284
	.quad	_LT__LT_var_C_LC
	.quad	_L_1+4299
	.quad	_LT__uLT_var_C_LC
	.quad	_L_1+4311
	.quad	_LT__LT_param_C_i64
	.quad	_L_1+4324
	.quad	_LT__uLT_param_C_i64
	.quad	_L_1+4339
	.quad	_LT__LT_var_C_i64
	.quad	_L_1+4355
	.quad	_LT__uLT_var_C_i64
	.quad	_L_1+4368
	.quad	_LT__LT_param_u32_i32
	.quad	_L_1+4382
	.quad	_LT__uLT_param_u32_i32
	.quad	_L_1+4399
	.quad	_LT__LT_var_u32_i32
	.quad	_L_1+4417
	.quad	_LT__uLT_var_u32_i32
	.quad	_L_1+4432
	.quad	_LT__LT_param_u32_i16
	.quad	_L_1+4448
	.quad	_LT__uLT_param_u32_i16
	.quad	_L_1+4465
	.quad	_LT__LT_var_u32_i16
	.quad	_L_1+4483
	.quad	_LT__uLT_var_u32_i16
	.quad	_L_1+4498
	.quad	_LT__LT_param_u32_L
	.quad	_L_1+4514
	.quad	_LT__uLT_param_u32_L
	.quad	_L_1+4529
	.quad	_LT__LT_var_u32_L
	.quad	_L_1+4545
	.quad	_LT__uLT_var_u32_L
	.quad	_L_1+4558
	.quad	_LT__LT_param_u32_u64
	.quad	_L_1+4572
	.quad	_LT__uLT_param_u32_u64
	.quad	_L_1+4589
	.quad	_LT__LT_var_u32_u64
	.quad	_L_1+4607
	.quad	_LT__uLT_var_u32_u64
	.quad	_L_1+4622
	.quad	_LT__LT_param_u32_u16
	.quad	_L_1+4638
	.quad	_LT__uLT_param_u32_u16
	.quad	_L_1+4655
	.quad	_LT__LT_var_u32_u16
	.quad	_L_1+4673
	.quad	_LT__uLT_var_u32_u16
	.quad	_L_1+4688
	.quad	_LT__LT_param_u32_C
	.quad	_L_1+4704
	.quad	_LT__uLT_param_u32_C
	.quad	_L_1+4719
	.quad	_LT__LT_var_u32_C
	.quad	_L_1+4735
	.quad	_LT__uLT_var_u32_C
	.quad	_L_1+4748
	.quad	_LT__LT_param_u32_u32
	.quad	_L_1+4762
	.quad	_LT__uLT_param_u32_u32
	.quad	_L_1+4779
	.quad	_LT__LT_var_u32_u32
	.quad	_L_1+4797
	.quad	_LT__uLT_var_u32_u32
	.quad	_L_1+4812
	.quad	_LT__LT_param_u32_i8
	.quad	_L_1+4828
	.quad	_LT__uLT_param_u32_i8
	.quad	_L_1+4844
	.quad	_LT__LT_var_u32_i8
	.quad	_L_1+4861
	.quad	_LT__uLT_var_u32_i8
	.quad	_L_1+4875
	.quad	_LT__LT_param_u32_I
	.quad	_L_1+4890
	.quad	_LT__uLT_param_u32_I
	.quad	_L_1+4905
	.quad	_LT__LT_var_u32_I
	.quad	_L_1+4921
	.quad	_LT__uLT_var_u32_I
	.quad	_L_1+4934
	.quad	_LT__LT_param_u32_u8
	.quad	_L_1+4948
	.quad	_LT__uLT_param_u32_u8
	.quad	_L_1+4964
	.quad	_LT__LT_var_u32_u8
	.quad	_L_1+4981
	.quad	_LT__uLT_var_u32_u8
	.quad	_L_1+4995
	.quad	_LT__LT_param_u32_LC
	.quad	_L_1+5010
	.quad	_LT__uLT_param_u32_LC
	.quad	_L_1+5026
	.quad	_LT__LT_var_u32_LC
	.quad	_L_1+5043
	.quad	_LT__uLT_var_u32_LC
	.quad	_L_1+5057
	.quad	_LT__LT_param_u32_i64
	.quad	_L_1+5072
	.quad	_LT__uLT_param_u32_i64
	.quad	_L_1+5089
	.quad	_LT__LT_var_u32_i64
	.quad	_L_1+5107
	.quad	_LT__uLT_var_u32_i64
	.quad	_L_1+5122
	.quad	_LT__LT_param_i8_i32
	.quad	_L_1+5138
	.quad	_LT__uLT_param_i8_i32
	.quad	_L_1+5154
	.quad	_LT__LT_var_i8_i32
	.quad	_L_1+5171
	.quad	_LT__uLT_var_i8_i32
	.quad	_L_1+5185
	.quad	_LT__LT_param_i8_i16
	.quad	_L_1+5200
	.quad	_LT__uLT_param_i8_i16
	.quad	_L_1+5216
	.quad	_LT__LT_var_i8_i16
	.quad	_L_1+5233
	.quad	_LT__uLT_var_i8_i16
	.quad	_L_1+5247
	.quad	_LT__LT_param_i8_L
	.quad	_L_1+5262
	.quad	_LT__uLT_param_i8_L
	.quad	_L_1+5276
	.quad	_LT__LT_var_i8_L
	.quad	_L_1+5291
	.quad	_LT__uLT_var_i8_L
	.quad	_L_1+5303
	.quad	_LT__LT_param_i8_u64
	.quad	_L_1+5316
	.quad	_LT__uLT_param_i8_u64
	.quad	_L_1+5332
	.quad	_LT__LT_var_i8_u64
	.quad	_L_1+5349
	.quad	_LT__uLT_var_i8_u64
	.quad	_L_1+5363
	.quad	_LT__LT_param_i8_u16
	.quad	_L_1+5378
	.quad	_LT__uLT_param_i8_u16
	.quad	_L_1+5394
	.quad	_LT__LT_var_i8_u16
	.quad	_L_1+5411
	.quad	_LT__uLT_var_i8_u16
	.quad	_L_1+5425
	.quad	_LT__LT_param_i8_C
	.quad	_L_1+5440
	.quad	_LT__uLT_param_i8_C
	.quad	_L_1+5454
	.quad	_LT__LT_var_i8_C
	.quad	_L_1+5469
	.quad	_LT__uLT_var_i8_C
	.quad	_L_1+5481
	.quad	_LT__LT_param_i8_u32
	.quad	_L_1+5494
	.quad	_LT__uLT_param_i8_u32
	.quad	_L_1+5510
	.quad	_LT__LT_var_i8_u32
	.quad	_L_1+5527
	.quad	_LT__uLT_var_i8_u32
	.quad	_L_1+5541
	.quad	_LT__LT_param_i8_i8
	.quad	_L_1+5556
	.quad	_LT__uLT_param_i8_i8
	.quad	_L_1+5571
	.quad	_LT__LT_var_i8_i8
	.quad	_L_1+5587
	.quad	_LT__uLT_var_i8_i8
	.quad	_L_1+5600
	.quad	_LT__LT_param_i8_I
	.quad	_L_1+5614
	.quad	_LT__uLT_param_i8_I
	.quad	_L_1+5628
	.quad	_LT__LT_var_i8_I
	.quad	_L_1+5643
	.quad	_LT__uLT_var_i8_I
	.quad	_L_1+5655
	.quad	_LT__LT_param_i8_u8
	.quad	_L_1+5668
	.quad	_LT__uLT_param_i8_u8
	.quad	_L_1+5683
	.quad	_LT__LT_var_i8_u8
	.quad	_L_1+5699
	.quad	_LT__uLT_var_i8_u8
	.quad	_L_1+5712
	.quad	_LT__LT_param_i8_LC
	.quad	_L_1+5726
	.quad	_LT__uLT_param_i8_LC
	.quad	_L_1+5741
	.quad	_LT__LT_var_i8_LC
	.quad	_L_1+5757
	.quad	_LT__uLT_var_i8_LC
	.quad	_L_1+5770
	.quad	_LT__LT_param_i8_i64
	.quad	_L_1+5784
	.quad	_LT__uLT_param_i8_i64
	.quad	_L_1+5800
	.quad	_LT__LT_var_i8_i64
	.quad	_L_1+5817
	.quad	_LT__uLT_var_i8_i64
	.quad	_L_1+5831
	.quad	_LT__LT_param_I_i32
	.quad	_L_1+5846
	.quad	_LT__uLT_param_I_i32
	.quad	_L_1+5861
	.quad	_LT__LT_var_I_i32
	.quad	_L_1+5877
	.quad	_LT__uLT_var_I_i32
	.quad	_L_1+5890
	.quad	_LT__LT_param_I_i16
	.quad	_L_1+5904
	.quad	_LT__uLT_param_I_i16
	.quad	_L_1+5919
	.quad	_LT__LT_var_I_i16
	.quad	_L_1+5935
	.quad	_LT__uLT_var_I_i16
	.quad	_L_1+5948
	.quad	_LT__LT_param_I_L
	.quad	_L_1+5962
	.quad	_LT__uLT_param_I_L
	.quad	_L_1+5975
	.quad	_LT__LT_var_I_L
	.quad	_L_1+5989
	.quad	_LT__uLT_var_I_L
	.quad	_L_1+6000
	.quad	_LT__LT_param_I_u64
	.quad	_L_1+6012
	.quad	_LT__uLT_param_I_u64
	.quad	_L_1+6027
	.quad	_LT__LT_var_I_u64
	.quad	_L_1+6043
	.quad	_LT__uLT_var_I_u64
	.quad	_L_1+6056
	.quad	_LT__LT_param_I_u16
	.quad	_L_1+6070
	.quad	_LT__uLT_param_I_u16
	.quad	_L_1+6085
	.quad	_LT__LT_var_I_u16
	.quad	_L_1+6101
	.quad	_LT__uLT_var_I_u16
	.quad	_L_1+6114
	.quad	_LT__LT_param_I_C
	.quad	_L_1+6128
	.quad	_LT__uLT_param_I_C
	.quad	_L_1+6141
	.quad	_LT__LT_var_I_C
	.quad	_L_1+6155
	.quad	_LT__uLT_var_I_C
	.quad	_L_1+6166
	.quad	_LT__LT_param_I_u32
	.quad	_L_1+6178
	.quad	_LT__uLT_param_I_u32
	.quad	_L_1+6193
	.quad	_LT__LT_var_I_u32
	.quad	_L_1+6209
	.quad	_LT__uLT_var_I_u32
	.quad	_L_1+6222
	.quad	_LT__LT_param_I_i8
	.quad	_L_1+6236
	.quad	_LT__uLT_param_I_i8
	.quad	_L_1+6250
	.quad	_LT__LT_var_I_i8
	.quad	_L_1+6265
	.quad	_LT__uLT_var_I_i8
	.quad	_L_1+6277
	.quad	_LT__LT_param_I_I
	.quad	_L_1+6290
	.quad	_LT__uLT_param_I_I
	.quad	_L_1+6303
	.quad	_LT__LT_var_I_I
	.quad	_L_1+6317
	.quad	_LT__uLT_var_I_I
	.quad	_L_1+6328
	.quad	_LT__LT_param_I_u8
	.quad	_L_1+6340
	.quad	_LT__uLT_param_I_u8
	.quad	_L_1+6354
	.quad	_LT__LT_var_I_u8
	.quad	_L_1+6369
	.quad	_LT__uLT_var_I_u8
	.quad	_L_1+6381
	.quad	_LT__LT_param_I_LC
	.quad	_L_1+6394
	.quad	_LT__uLT_param_I_LC
	.quad	_L_1+6408
	.quad	_LT__LT_var_I_LC
	.quad	_L_1+6423
	.quad	_LT__uLT_var_I_LC
	.quad	_L_1+6435
	.quad	_LT__LT_param_I_i64
	.quad	_L_1+6448
	.quad	_LT__uLT_param_I_i64
	.quad	_L_1+6463
	.quad	_LT__LT_var_I_i64
	.quad	_L_1+6479
	.quad	_LT__uLT_var_I_i64
	.quad	_L_1+6492
	.quad	_LT__LT_param_u8_i32
	.quad	_L_1+6506
	.quad	_LT__uLT_param_u8_i32
	.quad	_L_1+6522
	.quad	_LT__LT_var_u8_i32
	.quad	_L_1+6539
	.quad	_LT__uLT_var_u8_i32
	.quad	_L_1+6553
	.quad	_LT__LT_param_u8_i16
	.quad	_L_1+6568
	.quad	_LT__uLT_param_u8_i16
	.quad	_L_1+6584
	.quad	_LT__LT_var_u8_i16
	.quad	_L_1+6601
	.quad	_LT__uLT_var_u8_i16
	.quad	_L_1+6615
	.quad	_LT__LT_param_u8_L
	.quad	_L_1+6630
	.quad	_LT__uLT_param_u8_L
	.quad	_L_1+6644
	.quad	_LT__LT_var_u8_L
	.quad	_L_1+6659
	.quad	_LT__uLT_var_u8_L
	.quad	_L_1+6671
	.quad	_LT__LT_param_u8_u64
	.quad	_L_1+6684
	.quad	_LT__uLT_param_u8_u64
	.quad	_L_1+6700
	.quad	_LT__LT_var_u8_u64
	.quad	_L_1+6717
	.quad	_LT__uLT_var_u8_u64
	.quad	_L_1+6731
	.quad	_LT__LT_param_u8_u16
	.quad	_L_1+6746
	.quad	_LT__uLT_param_u8_u16
	.quad	_L_1+6762
	.quad	_LT__LT_var_u8_u16
	.quad	_L_1+6779
	.quad	_LT__uLT_var_u8_u16
	.quad	_L_1+6793
	.quad	_LT__LT_param_u8_C
	.quad	_L_1+6808
	.quad	_LT__uLT_param_u8_C
	.quad	_L_1+6822
	.quad	_LT__LT_var_u8_C
	.quad	_L_1+6837
	.quad	_LT__uLT_var_u8_C
	.quad	_L_1+6849
	.quad	_LT__LT_param_u8_u32
	.quad	_L_1+6862
	.quad	_LT__uLT_param_u8_u32
	.quad	_L_1+6878
	.quad	_LT__LT_var_u8_u32
	.quad	_L_1+6895
	.quad	_LT__uLT_var_u8_u32
	.quad	_L_1+6909
	.quad	_LT__LT_param_u8_i8
	.quad	_L_1+6924
	.quad	_LT__uLT_param_u8_i8
	.quad	_L_1+6939
	.quad	_LT__LT_var_u8_i8
	.quad	_L_1+6955
	.quad	_LT__uLT_var_u8_i8
	.quad	_L_1+6968
	.quad	_LT__LT_param_u8_I
	.quad	_L_1+6982
	.quad	_LT__uLT_param_u8_I
	.quad	_L_1+6996
	.quad	_LT__LT_var_u8_I
	.quad	_L_1+7011
	.quad	_LT__uLT_var_u8_I
	.quad	_L_1+7023
	.quad	_LT__LT_param_u8_u8
	.quad	_L_1+7036
	.quad	_LT__uLT_param_u8_u8
	.quad	_L_1+7051
	.quad	_LT__LT_var_u8_u8
	.quad	_L_1+7067
	.quad	_LT__uLT_var_u8_u8
	.quad	_L_1+7080
	.quad	_LT__LT_param_u8_LC
	.quad	_L_1+7094
	.quad	_LT__uLT_param_u8_LC
	.quad	_L_1+7109
	.quad	_LT__LT_var_u8_LC
	.quad	_L_1+7125
	.quad	_LT__uLT_var_u8_LC
	.quad	_L_1+7138
	.quad	_LT__LT_param_u8_i64
	.quad	_L_1+7152
	.quad	_LT__uLT_param_u8_i64
	.quad	_L_1+7168
	.quad	_LT__LT_var_u8_i64
	.quad	_L_1+7185
	.quad	_LT__uLT_var_u8_i64
	.quad	_L_1+7199
	.quad	_LT__LT_param_f32_f32
	.quad	_L_1+7214
	.quad	_LT__LT_var_f32_f32
	.quad	_L_1+7231
	.quad	_LT__LT_param_LC_i32
	.quad	_L_1+7246
	.quad	_LT__uLT_param_LC_i32
	.quad	_L_1+7262
	.quad	_LT__LT_var_LC_i32
	.quad	_L_1+7279
	.quad	_LT__uLT_var_LC_i32
	.quad	_L_1+7293
	.quad	_LT__LT_param_LC_i16
	.quad	_L_1+7308
	.quad	_LT__uLT_param_LC_i16
	.quad	_L_1+7324
	.quad	_LT__LT_var_LC_i16
	.quad	_L_1+7341
	.quad	_LT__uLT_var_LC_i16
	.quad	_L_1+7355
	.quad	_LT__LT_param_LC_L
	.quad	_L_1+7370
	.quad	_LT__uLT_param_LC_L
	.quad	_L_1+7384
	.quad	_LT__LT_var_LC_L
	.quad	_L_1+7399
	.quad	_LT__uLT_var_LC_L
	.quad	_L_1+7411
	.quad	_LT__LT_param_LC_u64
	.quad	_L_1+7424
	.quad	_LT__uLT_param_LC_u64
	.quad	_L_1+7440
	.quad	_LT__LT_var_LC_u64
	.quad	_L_1+7457
	.quad	_LT__uLT_var_LC_u64
	.quad	_L_1+7471
	.quad	_LT__LT_param_LC_u16
	.quad	_L_1+7486
	.quad	_LT__uLT_param_LC_u16
	.quad	_L_1+7502
	.quad	_LT__LT_var_LC_u16
	.quad	_L_1+7519
	.quad	_LT__uLT_var_LC_u16
	.quad	_L_1+7533
	.quad	_LT__LT_param_LC_C
	.quad	_L_1+7548
	.quad	_LT__uLT_param_LC_C
	.quad	_L_1+7562
	.quad	_LT__LT_var_LC_C
	.quad	_L_1+7577
	.quad	_LT__uLT_var_LC_C
	.quad	_L_1+7589
	.quad	_LT__LT_param_LC_u32
	.quad	_L_1+7602
	.quad	_LT__uLT_param_LC_u32
	.quad	_L_1+7618
	.quad	_LT__LT_var_LC_u32
	.quad	_L_1+7635
	.quad	_LT__uLT_var_LC_u32
	.quad	_L_1+7649
	.quad	_LT__LT_param_LC_i8
	.quad	_L_1+7664
	.quad	_LT__uLT_param_LC_i8
	.quad	_L_1+7679
	.quad	_LT__LT_var_LC_i8
	.quad	_L_1+7695
	.quad	_LT__uLT_var_LC_i8
	.quad	_L_1+7708
	.quad	_LT__LT_param_LC_I
	.quad	_L_1+7722
	.quad	_LT__uLT_param_LC_I
	.quad	_L_1+7736
	.quad	_LT__LT_var_LC_I
	.quad	_L_1+7751
	.quad	_LT__uLT_var_LC_I
	.quad	_L_1+7763
	.quad	_LT__LT_param_LC_u8
	.quad	_L_1+7776
	.quad	_LT__uLT_param_LC_u8
	.quad	_L_1+7791
	.quad	_LT__LT_var_LC_u8
	.quad	_L_1+7807
	.quad	_LT__uLT_var_LC_u8
	.quad	_L_1+7820
	.quad	_LT__LT_param_LC_LC
	.quad	_L_1+7834
	.quad	_LT__uLT_param_LC_LC
	.quad	_L_1+7849
	.quad	_LT__LT_var_LC_LC
	.quad	_L_1+7865
	.quad	_LT__uLT_var_LC_LC
	.quad	_L_1+7878
	.quad	_LT__LT_param_LC_i64
	.quad	_L_1+7892
	.quad	_LT__uLT_param_LC_i64
	.quad	_L_1+7908
	.quad	_LT__LT_var_LC_i64
	.quad	_L_1+7925
	.quad	_LT__uLT_var_LC_i64
	.quad	_L_1+7939
	.quad	_LT__LT_param_i64_i32
	.quad	_L_1+7954
	.quad	_LT__uLT_param_i64_i32
	.quad	_L_1+7971
	.quad	_LT__LT_var_i64_i32
	.quad	_L_1+7989
	.quad	_LT__uLT_var_i64_i32
	.quad	_L_1+8004
	.quad	_LT__LT_param_i64_i16
	.quad	_L_1+8020
	.quad	_LT__uLT_param_i64_i16
	.quad	_L_1+8037
	.quad	_LT__LT_var_i64_i16
	.quad	_L_1+8055
	.quad	_LT__uLT_var_i64_i16
	.quad	_L_1+8070
	.quad	_LT__LT_param_i64_L
	.quad	_L_1+8086
	.quad	_LT__uLT_param_i64_L
	.quad	_L_1+8101
	.quad	_LT__LT_var_i64_L
	.quad	_L_1+8117
	.quad	_LT__uLT_var_i64_L
	.quad	_L_1+8130
	.quad	_LT__LT_param_i64_u64
	.quad	_L_1+8144
	.quad	_LT__uLT_param_i64_u64
	.quad	_L_1+8161
	.quad	_LT__LT_var_i64_u64
	.quad	_L_1+8179
	.quad	_LT__uLT_var_i64_u64
	.quad	_L_1+8194
	.quad	_LT__LT_param_i64_u16
	.quad	_L_1+8210
	.quad	_LT__uLT_param_i64_u16
	.quad	_L_1+8227
	.quad	_LT__LT_var_i64_u16
	.quad	_L_1+8245
	.quad	_LT__uLT_var_i64_u16
	.quad	_L_1+8260
	.quad	_LT__LT_param_i64_C
	.quad	_L_1+8276
	.quad	_LT__uLT_param_i64_C
	.quad	_L_1+8291
	.quad	_LT__LT_var_i64_C
	.quad	_L_1+8307
	.quad	_LT__uLT_var_i64_C
	.quad	_L_1+8320
	.quad	_LT__LT_param_i64_u32
	.quad	_L_1+8334
	.quad	_LT__uLT_param_i64_u32
	.quad	_L_1+8351
	.quad	_LT__LT_var_i64_u32
	.quad	_L_1+8369
	.quad	_LT__uLT_var_i64_u32
	.quad	_L_1+8384
	.quad	_LT__LT_param_i64_i8
	.quad	_L_1+8400
	.quad	_LT__uLT_param_i64_i8
	.quad	_L_1+8416
	.quad	_LT__LT_var_i64_i8
	.quad	_L_1+8433
	.quad	_LT__uLT_var_i64_i8
	.quad	_L_1+8447
	.quad	_LT__LT_param_i64_I
	.quad	_L_1+8462
	.quad	_LT__uLT_param_i64_I
	.quad	_L_1+8477
	.quad	_LT__LT_var_i64_I
	.quad	_L_1+8493
	.quad	_LT__uLT_var_i64_I
	.quad	_L_1+8506
	.quad	_LT__LT_param_i64_u8
	.quad	_L_1+8520
	.quad	_LT__uLT_param_i64_u8
	.quad	_L_1+8536
	.quad	_LT__LT_var_i64_u8
	.quad	_L_1+8553
	.quad	_LT__uLT_var_i64_u8
	.quad	_L_1+8567
	.quad	_LT__LT_param_i64_LC
	.quad	_L_1+8582
	.quad	_LT__uLT_param_i64_LC
	.quad	_L_1+8598
	.quad	_LT__LT_var_i64_LC
	.quad	_L_1+8615
	.quad	_LT__uLT_var_i64_LC
	.quad	_L_1+8629
	.quad	_LT__LT_param_i64_i64
	.quad	_L_1+8644
	.quad	_LT__uLT_param_i64_i64
	.quad	_L_1+8661
	.quad	_LT__LT_var_i64_i64
	.quad	_L_1+8679
	.quad	_LT__uLT_var_i64_i64
	.quad	_L_1+8694
	.space 8
	.ascii "../AMD64_DARWIN/LT.m3"
	.space 3
	.data
	.align 5
_MM_LT:
	.quad	_L_1+18016
	.space 32
	.quad	_L_1+8712
	.space 24
	.quad	_MM_LT+208
	.space 8
	.quad	_LT_M3
	.quad	3
	.quad	305
	.quad	306
	.long	1134143341
	.byte	53
	.space 3
	.quad	310
	.byte	55
	.space 3
	.long	312
	.quad	313
	.word	314
	.space 6
	.quad	315
	.long	1855425872
	.long	1081328914
	.quad	318
	.word	319
	.space 2
	.long	320
	.space 24
	.quad	_LT_I3
	.quad	_MM_LT+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_LT+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_LT+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_LT+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
