	.text
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
.globl _EQ__EQ_var_i64_LC
	.private_extern _EQ__EQ_var_i64_LC
_EQ__EQ_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_u8
	.private_extern _EQ__EQ_var_i64_u8
_EQ__EQ_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_I
	.private_extern _EQ__EQ_var_i64_I
_EQ__EQ_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_i8
	.private_extern _EQ__EQ_var_i64_i8
_EQ__EQ_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_u32
	.private_extern _EQ__EQ_var_i64_u32
_EQ__EQ_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_C
	.private_extern _EQ__EQ_var_i64_C
_EQ__EQ_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_u16
	.private_extern _EQ__EQ_var_i64_u16
_EQ__EQ_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_EQ(%rip), %rax
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
	movq	104+_MM_EQ(%rip), %rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i64_L
	.private_extern _EQ__EQ_var_i64_L
_EQ__EQ_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_EQ(%rip), %rdx
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
.globl _EQ__EQ_var_i64_i16
	.private_extern _EQ__EQ_var_i64_i16
_EQ__EQ_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_EQ(%rip), %rax
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
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_i64
	.private_extern _EQ__EQ_var_LC_i64
_EQ__EQ_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_EQ(%rip), %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_u8
	.private_extern _EQ__EQ_var_LC_u8
_EQ__EQ_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_I
	.private_extern _EQ__EQ_var_LC_I
_EQ__EQ_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_i8
	.private_extern _EQ__EQ_var_LC_i8
_EQ__EQ_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_u32
	.private_extern _EQ__EQ_var_LC_u32
_EQ__EQ_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_C
	.private_extern _EQ__EQ_var_LC_C
_EQ__EQ_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_u16
	.private_extern _EQ__EQ_var_LC_u16
_EQ__EQ_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_EQ(%rip), %rax
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
	movq	112+_MM_EQ(%rip), %rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_LC_L
	.private_extern _EQ__EQ_var_LC_L
_EQ__EQ_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_EQ(%rip), %rdx
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
.globl _EQ__EQ_var_LC_i16
	.private_extern _EQ__EQ_var_LC_i16
_EQ__EQ_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_EQ(%rip), %rax
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
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_f32_f32
	.private_extern _EQ__EQ_var_f32_f32
_EQ__EQ_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	120+_MM_EQ(%rip), %xmm1
	movss	120+_MM_EQ(%rip), %xmm0
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
.globl _EQ__EQ_var_u8_i64
	.private_extern _EQ__EQ_var_u8_i64
_EQ__EQ_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u8_LC
	.private_extern _EQ__EQ_var_u8_LC
_EQ__EQ_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u8_I
	.private_extern _EQ__EQ_var_u8_I
_EQ__EQ_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u8_i8
	.private_extern _EQ__EQ_var_u8_i8
_EQ__EQ_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %edx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u8_u32
	.private_extern _EQ__EQ_var_u8_u32
_EQ__EQ_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u8_C
	.private_extern _EQ__EQ_var_u8_C
_EQ__EQ_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u8_u16
	.private_extern _EQ__EQ_var_u8_u16
_EQ__EQ_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u8_L
	.private_extern _EQ__EQ_var_u8_L
_EQ__EQ_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u8_i16
	.private_extern _EQ__EQ_var_u8_i16
_EQ__EQ_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_EQ(%rip), %eax
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
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_I_i64
	.private_extern _EQ__EQ_var_I_i64
_EQ__EQ_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_I_LC
	.private_extern _EQ__EQ_var_I_LC
_EQ__EQ_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_I_u8
	.private_extern _EQ__EQ_var_I_u8
_EQ__EQ_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_I_i8
	.private_extern _EQ__EQ_var_I_i8
_EQ__EQ_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_I_u32
	.private_extern _EQ__EQ_var_I_u32
_EQ__EQ_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_I_C
	.private_extern _EQ__EQ_var_I_C
_EQ__EQ_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_I_u16
	.private_extern _EQ__EQ_var_I_u16
_EQ__EQ_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movq	128+_MM_EQ(%rip), %rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_I_L
	.private_extern _EQ__EQ_var_I_L
_EQ__EQ_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
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
.globl _EQ__EQ_var_I_i16
	.private_extern _EQ__EQ_var_I_i16
_EQ__EQ_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movzwl	184+_MM_EQ(%rip), %eax
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
	movq	128+_MM_EQ(%rip), %rdx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i8_i64
	.private_extern _EQ__EQ_var_i8_i64
_EQ__EQ_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i8_LC
	.private_extern _EQ__EQ_var_i8_LC
_EQ__EQ_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i8_u8
	.private_extern _EQ__EQ_var_i8_u8
_EQ__EQ_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %edx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i8_I
	.private_extern _EQ__EQ_var_i8_I
_EQ__EQ_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i8_u32
	.private_extern _EQ__EQ_var_i8_u32
_EQ__EQ_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i8_C
	.private_extern _EQ__EQ_var_i8_C
_EQ__EQ_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i8_u16
	.private_extern _EQ__EQ_var_i8_u16
_EQ__EQ_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i8_L
	.private_extern _EQ__EQ_var_i8_L
_EQ__EQ_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i8_i16
	.private_extern _EQ__EQ_var_i8_i16
_EQ__EQ_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_EQ(%rip), %eax
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
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u32_i64
	.private_extern _EQ__EQ_var_u32_i64
_EQ__EQ_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u32_LC
	.private_extern _EQ__EQ_var_u32_LC
_EQ__EQ_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u32_u8
	.private_extern _EQ__EQ_var_u32_u8
_EQ__EQ_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u32_I
	.private_extern _EQ__EQ_var_u32_I
_EQ__EQ_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u32_i8
	.private_extern _EQ__EQ_var_u32_i8
_EQ__EQ_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u32_C
	.private_extern _EQ__EQ_var_u32_C
_EQ__EQ_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u32_u16
	.private_extern _EQ__EQ_var_u32_u16
_EQ__EQ_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u32_L
	.private_extern _EQ__EQ_var_u32_L
_EQ__EQ_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u32_i16
	.private_extern _EQ__EQ_var_u32_i16
_EQ__EQ_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %edx
	movzwl	184+_MM_EQ(%rip), %eax
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
	movl	140+_MM_EQ(%rip), %edx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_C_i64
	.private_extern _EQ__EQ_var_C_i64
_EQ__EQ_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_C_LC
	.private_extern _EQ__EQ_var_C_LC
_EQ__EQ_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_C_u8
	.private_extern _EQ__EQ_var_C_u8
_EQ__EQ_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_C_I
	.private_extern _EQ__EQ_var_C_I
_EQ__EQ_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_C_i8
	.private_extern _EQ__EQ_var_C_i8
_EQ__EQ_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_C_u32
	.private_extern _EQ__EQ_var_C_u32
_EQ__EQ_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_C_u16
	.private_extern _EQ__EQ_var_C_u16
_EQ__EQ_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movq	144+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_C_L
	.private_extern _EQ__EQ_var_C_L
_EQ__EQ_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_C_i16
	.private_extern _EQ__EQ_var_C_i16
_EQ__EQ_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rdx
	movzwl	184+_MM_EQ(%rip), %eax
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
	movq	144+_MM_EQ(%rip), %rdx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u16_i64
	.private_extern _EQ__EQ_var_u16_i64
_EQ__EQ_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u16_LC
	.private_extern _EQ__EQ_var_u16_LC
_EQ__EQ_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u16_u8
	.private_extern _EQ__EQ_var_u16_u8
_EQ__EQ_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %edx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u16_I
	.private_extern _EQ__EQ_var_u16_I
_EQ__EQ_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u16_i8
	.private_extern _EQ__EQ_var_u16_i8
_EQ__EQ_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %edx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u16_u32
	.private_extern _EQ__EQ_var_u16_u32
_EQ__EQ_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u16_C
	.private_extern _EQ__EQ_var_u16_C
_EQ__EQ_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_EQ(%rip), %rax
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
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u16_L
	.private_extern _EQ__EQ_var_u16_L
_EQ__EQ_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u16_i16
	.private_extern _EQ__EQ_var_u16_i16
_EQ__EQ_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %edx
	movzwl	184+_MM_EQ(%rip), %eax
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
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_u64_i64
	.private_extern _EQ__EQ_var_u64_i64
_EQ__EQ_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_LC
	.private_extern _EQ__EQ_var_u64_LC
_EQ__EQ_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_u8
	.private_extern _EQ__EQ_var_u64_u8
_EQ__EQ_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_I
	.private_extern _EQ__EQ_var_u64_I
_EQ__EQ_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_i8
	.private_extern _EQ__EQ_var_u64_i8
_EQ__EQ_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_u32
	.private_extern _EQ__EQ_var_u64_u32
_EQ__EQ_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_C
	.private_extern _EQ__EQ_var_u64_C
_EQ__EQ_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_u16
	.private_extern _EQ__EQ_var_u64_u16
_EQ__EQ_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_u64_L
	.private_extern _EQ__EQ_var_u64_L
_EQ__EQ_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_EQ(%rip), %rdx
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
.globl _EQ__EQ_var_u64_i16
	.private_extern _EQ__EQ_var_u64_i16
_EQ__EQ_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_EQ(%rip), %rax
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
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_f64_f64
	.private_extern _EQ__EQ_var_f64_f64
_EQ__EQ_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	168+_MM_EQ(%rip), %xmm1
	movsd	168+_MM_EQ(%rip), %xmm0
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
.globl _EQ__EQ_var_L_i64
	.private_extern _EQ__EQ_var_L_i64
_EQ__EQ_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_EQ(%rip), %rdx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_L_LC
	.private_extern _EQ__EQ_var_L_LC
_EQ__EQ_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_EQ(%rip), %rdx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_L_u8
	.private_extern _EQ__EQ_var_L_u8
_EQ__EQ_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_L_I
	.private_extern _EQ__EQ_var_L_I
_EQ__EQ_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_EQ(%rip), %rdx
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
.globl _EQ__EQ_var_L_i8
	.private_extern _EQ__EQ_var_L_i8
_EQ__EQ_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_L_u32
	.private_extern _EQ__EQ_var_L_u32
_EQ__EQ_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_L_C
	.private_extern _EQ__EQ_var_L_C
_EQ__EQ_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_L_u16
	.private_extern _EQ__EQ_var_L_u16
_EQ__EQ_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_EQ(%rip), %eax
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
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_L_i16
	.private_extern _EQ__EQ_var_L_i16
_EQ__EQ_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
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
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i16_i64
	.private_extern _EQ__EQ_var_i16_i64
_EQ__EQ_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i16_LC
	.private_extern _EQ__EQ_var_i16_LC
_EQ__EQ_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i16_u8
	.private_extern _EQ__EQ_var_i16_u8
_EQ__EQ_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %edx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i16_I
	.private_extern _EQ__EQ_var_i16_I
_EQ__EQ_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i16_i8
	.private_extern _EQ__EQ_var_i16_i8
_EQ__EQ_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %edx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i16_u32
	.private_extern _EQ__EQ_var_i16_u32
_EQ__EQ_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i16_C
	.private_extern _EQ__EQ_var_i16_C
_EQ__EQ_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i16_u16
	.private_extern _EQ__EQ_var_i16_u16
_EQ__EQ_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %edx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i16_L
	.private_extern _EQ__EQ_var_i16_L
_EQ__EQ_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_EQ(%rip), %eax
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
	movzwl	184+_MM_EQ(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i32_i64
	.private_extern _EQ__EQ_var_i32_i64
_EQ__EQ_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i32_LC
	.private_extern _EQ__EQ_var_i32_LC
_EQ__EQ_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i32_u8
	.private_extern _EQ__EQ_var_i32_u8
_EQ__EQ_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %edx
	movzbl	124+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i32_I
	.private_extern _EQ__EQ_var_i32_I
_EQ__EQ_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i32_i8
	.private_extern _EQ__EQ_var_i32_i8
_EQ__EQ_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %edx
	movzbl	136+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i32_u32
	.private_extern _EQ__EQ_var_i32_u32
_EQ__EQ_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %edx
	movl	140+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i32_C
	.private_extern _EQ__EQ_var_i32_C
_EQ__EQ_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i32_u16
	.private_extern _EQ__EQ_var_i32_u16
_EQ__EQ_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %edx
	movzwl	152+_MM_EQ(%rip), %eax
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
	movl	188+_MM_EQ(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_EQ(%rip), %rax
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
.globl _EQ__EQ_var_i32_L
	.private_extern _EQ__EQ_var_i32_L
_EQ__EQ_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %eax
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
.globl _EQ__EQ_var_i32_i16
	.private_extern _EQ__EQ_var_i32_i16
_EQ__EQ_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_EQ(%rip), %edx
	movzwl	184+_MM_EQ(%rip), %eax
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
	.ascii "EQ_param_i32_i32"
	.space 1
	.ascii "EQ_var_i32_i32"
	.space 1
	.ascii "EQ_param_i32_i16"
	.space 1
	.ascii "EQ_var_i32_i16"
	.space 1
	.ascii "EQ_param_i32_L"
	.space 1
	.ascii "EQ_var_i32_L"
	.space 1
	.ascii "EQ_param_i32_u64"
	.space 1
	.ascii "EQ_var_i32_u64"
	.space 1
	.ascii "EQ_param_i32_u16"
	.space 1
	.ascii "EQ_var_i32_u16"
	.space 1
	.ascii "EQ_param_i32_C"
	.space 1
	.ascii "EQ_var_i32_C"
	.space 1
	.ascii "EQ_param_i32_u32"
	.space 1
	.ascii "EQ_var_i32_u32"
	.space 1
	.ascii "EQ_param_i32_i8"
	.space 1
	.ascii "EQ_var_i32_i8"
	.space 1
	.ascii "EQ_param_i32_I"
	.space 1
	.ascii "EQ_var_i32_I"
	.space 1
	.ascii "EQ_param_i32_u8"
	.space 1
	.ascii "EQ_var_i32_u8"
	.space 1
	.ascii "EQ_param_i32_LC"
	.space 1
	.ascii "EQ_var_i32_LC"
	.space 1
	.ascii "EQ_param_i32_i64"
	.space 1
	.ascii "EQ_var_i32_i64"
	.space 1
	.ascii "EQ_param_i16_i32"
	.space 1
	.ascii "EQ_var_i16_i32"
	.space 1
	.ascii "EQ_param_i16_i16"
	.space 1
	.ascii "EQ_var_i16_i16"
	.space 1
	.ascii "EQ_param_i16_L"
	.space 1
	.ascii "EQ_var_i16_L"
	.space 1
	.ascii "EQ_param_i16_u64"
	.space 1
	.ascii "EQ_var_i16_u64"
	.space 1
	.ascii "EQ_param_i16_u16"
	.space 1
	.ascii "EQ_var_i16_u16"
	.space 1
	.ascii "EQ_param_i16_C"
	.space 1
	.ascii "EQ_var_i16_C"
	.space 1
	.ascii "EQ_param_i16_u32"
	.space 1
	.ascii "EQ_var_i16_u32"
	.space 1
	.ascii "EQ_param_i16_i8"
	.space 1
	.ascii "EQ_var_i16_i8"
	.space 1
	.ascii "EQ_param_i16_I"
	.space 1
	.ascii "EQ_var_i16_I"
	.space 1
	.ascii "EQ_param_i16_u8"
	.space 1
	.ascii "EQ_var_i16_u8"
	.space 1
	.ascii "EQ_param_i16_LC"
	.space 1
	.ascii "EQ_var_i16_LC"
	.space 1
	.ascii "EQ_param_i16_i64"
	.space 1
	.ascii "EQ_var_i16_i64"
	.space 1
	.ascii "EQ_param_L_i32"
	.space 1
	.ascii "EQ_var_L_i32"
	.space 1
	.ascii "EQ_param_L_i16"
	.space 1
	.ascii "EQ_var_L_i16"
	.space 1
	.ascii "EQ_param_L_L"
	.space 1
	.ascii "EQ_var_L_L"
	.space 1
	.ascii "EQ_param_L_u64"
	.space 1
	.ascii "EQ_var_L_u64"
	.space 1
	.ascii "EQ_param_L_u16"
	.space 1
	.ascii "EQ_var_L_u16"
	.space 1
	.ascii "EQ_param_L_C"
	.space 1
	.ascii "EQ_var_L_C"
	.space 1
	.ascii "EQ_param_L_u32"
	.space 1
	.ascii "EQ_var_L_u32"
	.space 1
	.ascii "EQ_param_L_i8"
	.space 1
	.ascii "EQ_var_L_i8"
	.space 1
	.ascii "EQ_param_L_I"
	.space 1
	.ascii "EQ_var_L_I"
	.space 1
	.ascii "EQ_param_L_u8"
	.space 1
	.ascii "EQ_var_L_u8"
	.space 1
	.ascii "EQ_param_L_LC"
	.space 1
	.ascii "EQ_var_L_LC"
	.space 1
	.ascii "EQ_param_L_i64"
	.space 1
	.ascii "EQ_var_L_i64"
	.space 1
	.ascii "EQ_param_f64_f64"
	.space 1
	.ascii "EQ_var_f64_f64"
	.space 1
	.ascii "EQ_param_u64_i32"
	.space 1
	.ascii "EQ_var_u64_i32"
	.space 1
	.ascii "EQ_param_u64_i16"
	.space 1
	.ascii "EQ_var_u64_i16"
	.space 1
	.ascii "EQ_param_u64_L"
	.space 1
	.ascii "EQ_var_u64_L"
	.space 1
	.ascii "EQ_param_u64_u64"
	.space 1
	.ascii "EQ_var_u64_u64"
	.space 1
	.ascii "EQ_param_u64_u16"
	.space 1
	.ascii "EQ_var_u64_u16"
	.space 1
	.ascii "EQ_param_u64_C"
	.space 1
	.ascii "EQ_var_u64_C"
	.space 1
	.ascii "EQ_param_u64_u32"
	.space 1
	.ascii "EQ_var_u64_u32"
	.space 1
	.ascii "EQ_param_u64_i8"
	.space 1
	.ascii "EQ_var_u64_i8"
	.space 1
	.ascii "EQ_param_u64_I"
	.space 1
	.ascii "EQ_var_u64_I"
	.space 1
	.ascii "EQ_param_u64_u8"
	.space 1
	.ascii "EQ_var_u64_u8"
	.space 1
	.ascii "EQ_param_u64_LC"
	.space 1
	.ascii "EQ_var_u64_LC"
	.space 1
	.ascii "EQ_param_u64_i64"
	.space 1
	.ascii "EQ_var_u64_i64"
	.space 1
	.ascii "EQ_param_u16_i32"
	.space 1
	.ascii "EQ_var_u16_i32"
	.space 1
	.ascii "EQ_param_u16_i16"
	.space 1
	.ascii "EQ_var_u16_i16"
	.space 1
	.ascii "EQ_param_u16_L"
	.space 1
	.ascii "EQ_var_u16_L"
	.space 1
	.ascii "EQ_param_u16_u64"
	.space 1
	.ascii "EQ_var_u16_u64"
	.space 1
	.ascii "EQ_param_u16_u16"
	.space 1
	.ascii "EQ_var_u16_u16"
	.space 1
	.ascii "EQ_param_u16_C"
	.space 1
	.ascii "EQ_var_u16_C"
	.space 1
	.ascii "EQ_param_u16_u32"
	.space 1
	.ascii "EQ_var_u16_u32"
	.space 1
	.ascii "EQ_param_u16_i8"
	.space 1
	.ascii "EQ_var_u16_i8"
	.space 1
	.ascii "EQ_param_u16_I"
	.space 1
	.ascii "EQ_var_u16_I"
	.space 1
	.ascii "EQ_param_u16_u8"
	.space 1
	.ascii "EQ_var_u16_u8"
	.space 1
	.ascii "EQ_param_u16_LC"
	.space 1
	.ascii "EQ_var_u16_LC"
	.space 1
	.ascii "EQ_param_u16_i64"
	.space 1
	.ascii "EQ_var_u16_i64"
	.space 1
	.ascii "EQ_param_C_i32"
	.space 1
	.ascii "EQ_var_C_i32"
	.space 1
	.ascii "EQ_param_C_i16"
	.space 1
	.ascii "EQ_var_C_i16"
	.space 1
	.ascii "EQ_param_C_L"
	.space 1
	.ascii "EQ_var_C_L"
	.space 1
	.ascii "EQ_param_C_u64"
	.space 1
	.ascii "EQ_var_C_u64"
	.space 1
	.ascii "EQ_param_C_u16"
	.space 1
	.ascii "EQ_var_C_u16"
	.space 1
	.ascii "EQ_param_C_C"
	.space 1
	.ascii "EQ_var_C_C"
	.space 1
	.ascii "EQ_param_C_u32"
	.space 1
	.ascii "EQ_var_C_u32"
	.space 1
	.ascii "EQ_param_C_i8"
	.space 1
	.ascii "EQ_var_C_i8"
	.space 1
	.ascii "EQ_param_C_I"
	.space 1
	.ascii "EQ_var_C_I"
	.space 1
	.ascii "EQ_param_C_u8"
	.space 1
	.ascii "EQ_var_C_u8"
	.space 1
	.ascii "EQ_param_C_LC"
	.space 1
	.ascii "EQ_var_C_LC"
	.space 1
	.ascii "EQ_param_C_i64"
	.space 1
	.ascii "EQ_var_C_i64"
	.space 1
	.ascii "EQ_param_u32_i32"
	.space 1
	.ascii "EQ_var_u32_i32"
	.space 1
	.ascii "EQ_param_u32_i16"
	.space 1
	.ascii "EQ_var_u32_i16"
	.space 1
	.ascii "EQ_param_u32_L"
	.space 1
	.ascii "EQ_var_u32_L"
	.space 1
	.ascii "EQ_param_u32_u64"
	.space 1
	.ascii "EQ_var_u32_u64"
	.space 1
	.ascii "EQ_param_u32_u16"
	.space 1
	.ascii "EQ_var_u32_u16"
	.space 1
	.ascii "EQ_param_u32_C"
	.space 1
	.ascii "EQ_var_u32_C"
	.space 1
	.ascii "EQ_param_u32_u32"
	.space 1
	.ascii "EQ_var_u32_u32"
	.space 1
	.ascii "EQ_param_u32_i8"
	.space 1
	.ascii "EQ_var_u32_i8"
	.space 1
	.ascii "EQ_param_u32_I"
	.space 1
	.ascii "EQ_var_u32_I"
	.space 1
	.ascii "EQ_param_u32_u8"
	.space 1
	.ascii "EQ_var_u32_u8"
	.space 1
	.ascii "EQ_param_u32_LC"
	.space 1
	.ascii "EQ_var_u32_LC"
	.space 1
	.ascii "EQ_param_u32_i64"
	.space 1
	.ascii "EQ_var_u32_i64"
	.space 1
	.ascii "EQ_param_i8_i32"
	.space 1
	.ascii "EQ_var_i8_i32"
	.space 1
	.ascii "EQ_param_i8_i16"
	.space 1
	.ascii "EQ_var_i8_i16"
	.space 1
	.ascii "EQ_param_i8_L"
	.space 1
	.ascii "EQ_var_i8_L"
	.space 1
	.ascii "EQ_param_i8_u64"
	.space 1
	.ascii "EQ_var_i8_u64"
	.space 1
	.ascii "EQ_param_i8_u16"
	.space 1
	.ascii "EQ_var_i8_u16"
	.space 1
	.ascii "EQ_param_i8_C"
	.space 1
	.ascii "EQ_var_i8_C"
	.space 1
	.ascii "EQ_param_i8_u32"
	.space 1
	.ascii "EQ_var_i8_u32"
	.space 1
	.ascii "EQ_param_i8_i8"
	.space 1
	.ascii "EQ_var_i8_i8"
	.space 1
	.ascii "EQ_param_i8_I"
	.space 1
	.ascii "EQ_var_i8_I"
	.space 1
	.ascii "EQ_param_i8_u8"
	.space 1
	.ascii "EQ_var_i8_u8"
	.space 1
	.ascii "EQ_param_i8_LC"
	.space 1
	.ascii "EQ_var_i8_LC"
	.space 1
	.ascii "EQ_param_i8_i64"
	.space 1
	.ascii "EQ_var_i8_i64"
	.space 1
	.ascii "EQ_param_I_i32"
	.space 1
	.ascii "EQ_var_I_i32"
	.space 1
	.ascii "EQ_param_I_i16"
	.space 1
	.ascii "EQ_var_I_i16"
	.space 1
	.ascii "EQ_param_I_L"
	.space 1
	.ascii "EQ_var_I_L"
	.space 1
	.ascii "EQ_param_I_u64"
	.space 1
	.ascii "EQ_var_I_u64"
	.space 1
	.ascii "EQ_param_I_u16"
	.space 1
	.ascii "EQ_var_I_u16"
	.space 1
	.ascii "EQ_param_I_C"
	.space 1
	.ascii "EQ_var_I_C"
	.space 1
	.ascii "EQ_param_I_u32"
	.space 1
	.ascii "EQ_var_I_u32"
	.space 1
	.ascii "EQ_param_I_i8"
	.space 1
	.ascii "EQ_var_I_i8"
	.space 1
	.ascii "EQ_param_I_I"
	.space 1
	.ascii "EQ_var_I_I"
	.space 1
	.ascii "EQ_param_I_u8"
	.space 1
	.ascii "EQ_var_I_u8"
	.space 1
	.ascii "EQ_param_I_LC"
	.space 1
	.ascii "EQ_var_I_LC"
	.space 1
	.ascii "EQ_param_I_i64"
	.space 1
	.ascii "EQ_var_I_i64"
	.space 1
	.ascii "EQ_param_u8_i32"
	.space 1
	.ascii "EQ_var_u8_i32"
	.space 1
	.ascii "EQ_param_u8_i16"
	.space 1
	.ascii "EQ_var_u8_i16"
	.space 1
	.ascii "EQ_param_u8_L"
	.space 1
	.ascii "EQ_var_u8_L"
	.space 1
	.ascii "EQ_param_u8_u64"
	.space 1
	.ascii "EQ_var_u8_u64"
	.space 1
	.ascii "EQ_param_u8_u16"
	.space 1
	.ascii "EQ_var_u8_u16"
	.space 1
	.ascii "EQ_param_u8_C"
	.space 1
	.ascii "EQ_var_u8_C"
	.space 1
	.ascii "EQ_param_u8_u32"
	.space 1
	.ascii "EQ_var_u8_u32"
	.space 1
	.ascii "EQ_param_u8_i8"
	.space 1
	.ascii "EQ_var_u8_i8"
	.space 1
	.ascii "EQ_param_u8_I"
	.space 1
	.ascii "EQ_var_u8_I"
	.space 1
	.ascii "EQ_param_u8_u8"
	.space 1
	.ascii "EQ_var_u8_u8"
	.space 1
	.ascii "EQ_param_u8_LC"
	.space 1
	.ascii "EQ_var_u8_LC"
	.space 1
	.ascii "EQ_param_u8_i64"
	.space 1
	.ascii "EQ_var_u8_i64"
	.space 1
	.ascii "EQ_param_f32_f32"
	.space 1
	.ascii "EQ_var_f32_f32"
	.space 1
	.ascii "EQ_param_LC_i32"
	.space 1
	.ascii "EQ_var_LC_i32"
	.space 1
	.ascii "EQ_param_LC_i16"
	.space 1
	.ascii "EQ_var_LC_i16"
	.space 1
	.ascii "EQ_param_LC_L"
	.space 1
	.ascii "EQ_var_LC_L"
	.space 1
	.ascii "EQ_param_LC_u64"
	.space 1
	.ascii "EQ_var_LC_u64"
	.space 1
	.ascii "EQ_param_LC_u16"
	.space 1
	.ascii "EQ_var_LC_u16"
	.space 1
	.ascii "EQ_param_LC_C"
	.space 1
	.ascii "EQ_var_LC_C"
	.space 1
	.ascii "EQ_param_LC_u32"
	.space 1
	.ascii "EQ_var_LC_u32"
	.space 1
	.ascii "EQ_param_LC_i8"
	.space 1
	.ascii "EQ_var_LC_i8"
	.space 1
	.ascii "EQ_param_LC_I"
	.space 1
	.ascii "EQ_var_LC_I"
	.space 1
	.ascii "EQ_param_LC_u8"
	.space 1
	.ascii "EQ_var_LC_u8"
	.space 1
	.ascii "EQ_param_LC_LC"
	.space 1
	.ascii "EQ_var_LC_LC"
	.space 1
	.ascii "EQ_param_LC_i64"
	.space 1
	.ascii "EQ_var_LC_i64"
	.space 1
	.ascii "EQ_param_i64_i32"
	.space 1
	.ascii "EQ_var_i64_i32"
	.space 1
	.ascii "EQ_param_i64_i16"
	.space 1
	.ascii "EQ_var_i64_i16"
	.space 1
	.ascii "EQ_param_i64_L"
	.space 1
	.ascii "EQ_var_i64_L"
	.space 1
	.ascii "EQ_param_i64_u64"
	.space 1
	.ascii "EQ_var_i64_u64"
	.space 1
	.ascii "EQ_param_i64_u16"
	.space 1
	.ascii "EQ_var_i64_u16"
	.space 1
	.ascii "EQ_param_i64_C"
	.space 1
	.ascii "EQ_var_i64_C"
	.space 1
	.ascii "EQ_param_i64_u32"
	.space 1
	.ascii "EQ_var_i64_u32"
	.space 1
	.ascii "EQ_param_i64_i8"
	.space 1
	.ascii "EQ_var_i64_i8"
	.space 1
	.ascii "EQ_param_i64_I"
	.space 1
	.ascii "EQ_var_i64_I"
	.space 1
	.ascii "EQ_param_i64_u8"
	.space 1
	.ascii "EQ_var_i64_u8"
	.space 1
	.ascii "EQ_param_i64_LC"
	.space 1
	.ascii "EQ_var_i64_LC"
	.space 1
	.ascii "EQ_param_i64_i64"
	.space 1
	.ascii "EQ_var_i64_i64"
	.space 3
	.quad	_EQ_M3
	.quad	_L_1
	.quad	_EQ__EQ_param_i32_i32
	.quad	_L_1+6
	.quad	_EQ__EQ_var_i32_i32
	.quad	_L_1+23
	.quad	_EQ__EQ_param_i32_i16
	.quad	_L_1+38
	.quad	_EQ__EQ_var_i32_i16
	.quad	_L_1+55
	.quad	_EQ__EQ_param_i32_L
	.quad	_L_1+70
	.quad	_EQ__EQ_var_i32_L
	.quad	_L_1+85
	.quad	_EQ__EQ_param_i32_u64
	.quad	_L_1+98
	.quad	_EQ__EQ_var_i32_u64
	.quad	_L_1+115
	.quad	_EQ__EQ_param_i32_u16
	.quad	_L_1+130
	.quad	_EQ__EQ_var_i32_u16
	.quad	_L_1+147
	.quad	_EQ__EQ_param_i32_C
	.quad	_L_1+162
	.quad	_EQ__EQ_var_i32_C
	.quad	_L_1+177
	.quad	_EQ__EQ_param_i32_u32
	.quad	_L_1+190
	.quad	_EQ__EQ_var_i32_u32
	.quad	_L_1+207
	.quad	_EQ__EQ_param_i32_i8
	.quad	_L_1+222
	.quad	_EQ__EQ_var_i32_i8
	.quad	_L_1+238
	.quad	_EQ__EQ_param_i32_I
	.quad	_L_1+252
	.quad	_EQ__EQ_var_i32_I
	.quad	_L_1+267
	.quad	_EQ__EQ_param_i32_u8
	.quad	_L_1+280
	.quad	_EQ__EQ_var_i32_u8
	.quad	_L_1+296
	.quad	_EQ__EQ_param_i32_LC
	.quad	_L_1+310
	.quad	_EQ__EQ_var_i32_LC
	.quad	_L_1+326
	.quad	_EQ__EQ_param_i32_i64
	.quad	_L_1+340
	.quad	_EQ__EQ_var_i32_i64
	.quad	_L_1+357
	.quad	_EQ__EQ_param_i16_i32
	.quad	_L_1+372
	.quad	_EQ__EQ_var_i16_i32
	.quad	_L_1+389
	.quad	_EQ__EQ_param_i16_i16
	.quad	_L_1+404
	.quad	_EQ__EQ_var_i16_i16
	.quad	_L_1+421
	.quad	_EQ__EQ_param_i16_L
	.quad	_L_1+436
	.quad	_EQ__EQ_var_i16_L
	.quad	_L_1+451
	.quad	_EQ__EQ_param_i16_u64
	.quad	_L_1+464
	.quad	_EQ__EQ_var_i16_u64
	.quad	_L_1+481
	.quad	_EQ__EQ_param_i16_u16
	.quad	_L_1+496
	.quad	_EQ__EQ_var_i16_u16
	.quad	_L_1+513
	.quad	_EQ__EQ_param_i16_C
	.quad	_L_1+528
	.quad	_EQ__EQ_var_i16_C
	.quad	_L_1+543
	.quad	_EQ__EQ_param_i16_u32
	.quad	_L_1+556
	.quad	_EQ__EQ_var_i16_u32
	.quad	_L_1+573
	.quad	_EQ__EQ_param_i16_i8
	.quad	_L_1+588
	.quad	_EQ__EQ_var_i16_i8
	.quad	_L_1+604
	.quad	_EQ__EQ_param_i16_I
	.quad	_L_1+618
	.quad	_EQ__EQ_var_i16_I
	.quad	_L_1+633
	.quad	_EQ__EQ_param_i16_u8
	.quad	_L_1+646
	.quad	_EQ__EQ_var_i16_u8
	.quad	_L_1+662
	.quad	_EQ__EQ_param_i16_LC
	.quad	_L_1+676
	.quad	_EQ__EQ_var_i16_LC
	.quad	_L_1+692
	.quad	_EQ__EQ_param_i16_i64
	.quad	_L_1+706
	.quad	_EQ__EQ_var_i16_i64
	.quad	_L_1+723
	.quad	_EQ__EQ_param_L_i32
	.quad	_L_1+738
	.quad	_EQ__EQ_var_L_i32
	.quad	_L_1+753
	.quad	_EQ__EQ_param_L_i16
	.quad	_L_1+766
	.quad	_EQ__EQ_var_L_i16
	.quad	_L_1+781
	.quad	_EQ__EQ_param_L_L
	.quad	_L_1+794
	.quad	_EQ__EQ_var_L_L
	.quad	_L_1+807
	.quad	_EQ__EQ_param_L_u64
	.quad	_L_1+818
	.quad	_EQ__EQ_var_L_u64
	.quad	_L_1+833
	.quad	_EQ__EQ_param_L_u16
	.quad	_L_1+846
	.quad	_EQ__EQ_var_L_u16
	.quad	_L_1+861
	.quad	_EQ__EQ_param_L_C
	.quad	_L_1+874
	.quad	_EQ__EQ_var_L_C
	.quad	_L_1+887
	.quad	_EQ__EQ_param_L_u32
	.quad	_L_1+898
	.quad	_EQ__EQ_var_L_u32
	.quad	_L_1+913
	.quad	_EQ__EQ_param_L_i8
	.quad	_L_1+926
	.quad	_EQ__EQ_var_L_i8
	.quad	_L_1+940
	.quad	_EQ__EQ_param_L_I
	.quad	_L_1+952
	.quad	_EQ__EQ_var_L_I
	.quad	_L_1+965
	.quad	_EQ__EQ_param_L_u8
	.quad	_L_1+976
	.quad	_EQ__EQ_var_L_u8
	.quad	_L_1+990
	.quad	_EQ__EQ_param_L_LC
	.quad	_L_1+1002
	.quad	_EQ__EQ_var_L_LC
	.quad	_L_1+1016
	.quad	_EQ__EQ_param_L_i64
	.quad	_L_1+1028
	.quad	_EQ__EQ_var_L_i64
	.quad	_L_1+1043
	.quad	_EQ__EQ_param_f64_f64
	.quad	_L_1+1056
	.quad	_EQ__EQ_var_f64_f64
	.quad	_L_1+1073
	.quad	_EQ__EQ_param_u64_i32
	.quad	_L_1+1088
	.quad	_EQ__EQ_var_u64_i32
	.quad	_L_1+1105
	.quad	_EQ__EQ_param_u64_i16
	.quad	_L_1+1120
	.quad	_EQ__EQ_var_u64_i16
	.quad	_L_1+1137
	.quad	_EQ__EQ_param_u64_L
	.quad	_L_1+1152
	.quad	_EQ__EQ_var_u64_L
	.quad	_L_1+1167
	.quad	_EQ__EQ_param_u64_u64
	.quad	_L_1+1180
	.quad	_EQ__EQ_var_u64_u64
	.quad	_L_1+1197
	.quad	_EQ__EQ_param_u64_u16
	.quad	_L_1+1212
	.quad	_EQ__EQ_var_u64_u16
	.quad	_L_1+1229
	.quad	_EQ__EQ_param_u64_C
	.quad	_L_1+1244
	.quad	_EQ__EQ_var_u64_C
	.quad	_L_1+1259
	.quad	_EQ__EQ_param_u64_u32
	.quad	_L_1+1272
	.quad	_EQ__EQ_var_u64_u32
	.quad	_L_1+1289
	.quad	_EQ__EQ_param_u64_i8
	.quad	_L_1+1304
	.quad	_EQ__EQ_var_u64_i8
	.quad	_L_1+1320
	.quad	_EQ__EQ_param_u64_I
	.quad	_L_1+1334
	.quad	_EQ__EQ_var_u64_I
	.quad	_L_1+1349
	.quad	_EQ__EQ_param_u64_u8
	.quad	_L_1+1362
	.quad	_EQ__EQ_var_u64_u8
	.quad	_L_1+1378
	.quad	_EQ__EQ_param_u64_LC
	.quad	_L_1+1392
	.quad	_EQ__EQ_var_u64_LC
	.quad	_L_1+1408
	.quad	_EQ__EQ_param_u64_i64
	.quad	_L_1+1422
	.quad	_EQ__EQ_var_u64_i64
	.quad	_L_1+1439
	.quad	_EQ__EQ_param_u16_i32
	.quad	_L_1+1454
	.quad	_EQ__EQ_var_u16_i32
	.quad	_L_1+1471
	.quad	_EQ__EQ_param_u16_i16
	.quad	_L_1+1486
	.quad	_EQ__EQ_var_u16_i16
	.quad	_L_1+1503
	.quad	_EQ__EQ_param_u16_L
	.quad	_L_1+1518
	.quad	_EQ__EQ_var_u16_L
	.quad	_L_1+1533
	.quad	_EQ__EQ_param_u16_u64
	.quad	_L_1+1546
	.quad	_EQ__EQ_var_u16_u64
	.quad	_L_1+1563
	.quad	_EQ__EQ_param_u16_u16
	.quad	_L_1+1578
	.quad	_EQ__EQ_var_u16_u16
	.quad	_L_1+1595
	.quad	_EQ__EQ_param_u16_C
	.quad	_L_1+1610
	.quad	_EQ__EQ_var_u16_C
	.quad	_L_1+1625
	.quad	_EQ__EQ_param_u16_u32
	.quad	_L_1+1638
	.quad	_EQ__EQ_var_u16_u32
	.quad	_L_1+1655
	.quad	_EQ__EQ_param_u16_i8
	.quad	_L_1+1670
	.quad	_EQ__EQ_var_u16_i8
	.quad	_L_1+1686
	.quad	_EQ__EQ_param_u16_I
	.quad	_L_1+1700
	.quad	_EQ__EQ_var_u16_I
	.quad	_L_1+1715
	.quad	_EQ__EQ_param_u16_u8
	.quad	_L_1+1728
	.quad	_EQ__EQ_var_u16_u8
	.quad	_L_1+1744
	.quad	_EQ__EQ_param_u16_LC
	.quad	_L_1+1758
	.quad	_EQ__EQ_var_u16_LC
	.quad	_L_1+1774
	.quad	_EQ__EQ_param_u16_i64
	.quad	_L_1+1788
	.quad	_EQ__EQ_var_u16_i64
	.quad	_L_1+1805
	.quad	_EQ__EQ_param_C_i32
	.quad	_L_1+1820
	.quad	_EQ__EQ_var_C_i32
	.quad	_L_1+1835
	.quad	_EQ__EQ_param_C_i16
	.quad	_L_1+1848
	.quad	_EQ__EQ_var_C_i16
	.quad	_L_1+1863
	.quad	_EQ__EQ_param_C_L
	.quad	_L_1+1876
	.quad	_EQ__EQ_var_C_L
	.quad	_L_1+1889
	.quad	_EQ__EQ_param_C_u64
	.quad	_L_1+1900
	.quad	_EQ__EQ_var_C_u64
	.quad	_L_1+1915
	.quad	_EQ__EQ_param_C_u16
	.quad	_L_1+1928
	.quad	_EQ__EQ_var_C_u16
	.quad	_L_1+1943
	.quad	_EQ__EQ_param_C_C
	.quad	_L_1+1956
	.quad	_EQ__EQ_var_C_C
	.quad	_L_1+1969
	.quad	_EQ__EQ_param_C_u32
	.quad	_L_1+1980
	.quad	_EQ__EQ_var_C_u32
	.quad	_L_1+1995
	.quad	_EQ__EQ_param_C_i8
	.quad	_L_1+2008
	.quad	_EQ__EQ_var_C_i8
	.quad	_L_1+2022
	.quad	_EQ__EQ_param_C_I
	.quad	_L_1+2034
	.quad	_EQ__EQ_var_C_I
	.quad	_L_1+2047
	.quad	_EQ__EQ_param_C_u8
	.quad	_L_1+2058
	.quad	_EQ__EQ_var_C_u8
	.quad	_L_1+2072
	.quad	_EQ__EQ_param_C_LC
	.quad	_L_1+2084
	.quad	_EQ__EQ_var_C_LC
	.quad	_L_1+2098
	.quad	_EQ__EQ_param_C_i64
	.quad	_L_1+2110
	.quad	_EQ__EQ_var_C_i64
	.quad	_L_1+2125
	.quad	_EQ__EQ_param_u32_i32
	.quad	_L_1+2138
	.quad	_EQ__EQ_var_u32_i32
	.quad	_L_1+2155
	.quad	_EQ__EQ_param_u32_i16
	.quad	_L_1+2170
	.quad	_EQ__EQ_var_u32_i16
	.quad	_L_1+2187
	.quad	_EQ__EQ_param_u32_L
	.quad	_L_1+2202
	.quad	_EQ__EQ_var_u32_L
	.quad	_L_1+2217
	.quad	_EQ__EQ_param_u32_u64
	.quad	_L_1+2230
	.quad	_EQ__EQ_var_u32_u64
	.quad	_L_1+2247
	.quad	_EQ__EQ_param_u32_u16
	.quad	_L_1+2262
	.quad	_EQ__EQ_var_u32_u16
	.quad	_L_1+2279
	.quad	_EQ__EQ_param_u32_C
	.quad	_L_1+2294
	.quad	_EQ__EQ_var_u32_C
	.quad	_L_1+2309
	.quad	_EQ__EQ_param_u32_u32
	.quad	_L_1+2322
	.quad	_EQ__EQ_var_u32_u32
	.quad	_L_1+2339
	.quad	_EQ__EQ_param_u32_i8
	.quad	_L_1+2354
	.quad	_EQ__EQ_var_u32_i8
	.quad	_L_1+2370
	.quad	_EQ__EQ_param_u32_I
	.quad	_L_1+2384
	.quad	_EQ__EQ_var_u32_I
	.quad	_L_1+2399
	.quad	_EQ__EQ_param_u32_u8
	.quad	_L_1+2412
	.quad	_EQ__EQ_var_u32_u8
	.quad	_L_1+2428
	.quad	_EQ__EQ_param_u32_LC
	.quad	_L_1+2442
	.quad	_EQ__EQ_var_u32_LC
	.quad	_L_1+2458
	.quad	_EQ__EQ_param_u32_i64
	.quad	_L_1+2472
	.quad	_EQ__EQ_var_u32_i64
	.quad	_L_1+2489
	.quad	_EQ__EQ_param_i8_i32
	.quad	_L_1+2504
	.quad	_EQ__EQ_var_i8_i32
	.quad	_L_1+2520
	.quad	_EQ__EQ_param_i8_i16
	.quad	_L_1+2534
	.quad	_EQ__EQ_var_i8_i16
	.quad	_L_1+2550
	.quad	_EQ__EQ_param_i8_L
	.quad	_L_1+2564
	.quad	_EQ__EQ_var_i8_L
	.quad	_L_1+2578
	.quad	_EQ__EQ_param_i8_u64
	.quad	_L_1+2590
	.quad	_EQ__EQ_var_i8_u64
	.quad	_L_1+2606
	.quad	_EQ__EQ_param_i8_u16
	.quad	_L_1+2620
	.quad	_EQ__EQ_var_i8_u16
	.quad	_L_1+2636
	.quad	_EQ__EQ_param_i8_C
	.quad	_L_1+2650
	.quad	_EQ__EQ_var_i8_C
	.quad	_L_1+2664
	.quad	_EQ__EQ_param_i8_u32
	.quad	_L_1+2676
	.quad	_EQ__EQ_var_i8_u32
	.quad	_L_1+2692
	.quad	_EQ__EQ_param_i8_i8
	.quad	_L_1+2706
	.quad	_EQ__EQ_var_i8_i8
	.quad	_L_1+2721
	.quad	_EQ__EQ_param_i8_I
	.quad	_L_1+2734
	.quad	_EQ__EQ_var_i8_I
	.quad	_L_1+2748
	.quad	_EQ__EQ_param_i8_u8
	.quad	_L_1+2760
	.quad	_EQ__EQ_var_i8_u8
	.quad	_L_1+2775
	.quad	_EQ__EQ_param_i8_LC
	.quad	_L_1+2788
	.quad	_EQ__EQ_var_i8_LC
	.quad	_L_1+2803
	.quad	_EQ__EQ_param_i8_i64
	.quad	_L_1+2816
	.quad	_EQ__EQ_var_i8_i64
	.quad	_L_1+2832
	.quad	_EQ__EQ_param_I_i32
	.quad	_L_1+2846
	.quad	_EQ__EQ_var_I_i32
	.quad	_L_1+2861
	.quad	_EQ__EQ_param_I_i16
	.quad	_L_1+2874
	.quad	_EQ__EQ_var_I_i16
	.quad	_L_1+2889
	.quad	_EQ__EQ_param_I_L
	.quad	_L_1+2902
	.quad	_EQ__EQ_var_I_L
	.quad	_L_1+2915
	.quad	_EQ__EQ_param_I_u64
	.quad	_L_1+2926
	.quad	_EQ__EQ_var_I_u64
	.quad	_L_1+2941
	.quad	_EQ__EQ_param_I_u16
	.quad	_L_1+2954
	.quad	_EQ__EQ_var_I_u16
	.quad	_L_1+2969
	.quad	_EQ__EQ_param_I_C
	.quad	_L_1+2982
	.quad	_EQ__EQ_var_I_C
	.quad	_L_1+2995
	.quad	_EQ__EQ_param_I_u32
	.quad	_L_1+3006
	.quad	_EQ__EQ_var_I_u32
	.quad	_L_1+3021
	.quad	_EQ__EQ_param_I_i8
	.quad	_L_1+3034
	.quad	_EQ__EQ_var_I_i8
	.quad	_L_1+3048
	.quad	_EQ__EQ_param_I_I
	.quad	_L_1+3060
	.quad	_EQ__EQ_var_I_I
	.quad	_L_1+3073
	.quad	_EQ__EQ_param_I_u8
	.quad	_L_1+3084
	.quad	_EQ__EQ_var_I_u8
	.quad	_L_1+3098
	.quad	_EQ__EQ_param_I_LC
	.quad	_L_1+3110
	.quad	_EQ__EQ_var_I_LC
	.quad	_L_1+3124
	.quad	_EQ__EQ_param_I_i64
	.quad	_L_1+3136
	.quad	_EQ__EQ_var_I_i64
	.quad	_L_1+3151
	.quad	_EQ__EQ_param_u8_i32
	.quad	_L_1+3164
	.quad	_EQ__EQ_var_u8_i32
	.quad	_L_1+3180
	.quad	_EQ__EQ_param_u8_i16
	.quad	_L_1+3194
	.quad	_EQ__EQ_var_u8_i16
	.quad	_L_1+3210
	.quad	_EQ__EQ_param_u8_L
	.quad	_L_1+3224
	.quad	_EQ__EQ_var_u8_L
	.quad	_L_1+3238
	.quad	_EQ__EQ_param_u8_u64
	.quad	_L_1+3250
	.quad	_EQ__EQ_var_u8_u64
	.quad	_L_1+3266
	.quad	_EQ__EQ_param_u8_u16
	.quad	_L_1+3280
	.quad	_EQ__EQ_var_u8_u16
	.quad	_L_1+3296
	.quad	_EQ__EQ_param_u8_C
	.quad	_L_1+3310
	.quad	_EQ__EQ_var_u8_C
	.quad	_L_1+3324
	.quad	_EQ__EQ_param_u8_u32
	.quad	_L_1+3336
	.quad	_EQ__EQ_var_u8_u32
	.quad	_L_1+3352
	.quad	_EQ__EQ_param_u8_i8
	.quad	_L_1+3366
	.quad	_EQ__EQ_var_u8_i8
	.quad	_L_1+3381
	.quad	_EQ__EQ_param_u8_I
	.quad	_L_1+3394
	.quad	_EQ__EQ_var_u8_I
	.quad	_L_1+3408
	.quad	_EQ__EQ_param_u8_u8
	.quad	_L_1+3420
	.quad	_EQ__EQ_var_u8_u8
	.quad	_L_1+3435
	.quad	_EQ__EQ_param_u8_LC
	.quad	_L_1+3448
	.quad	_EQ__EQ_var_u8_LC
	.quad	_L_1+3463
	.quad	_EQ__EQ_param_u8_i64
	.quad	_L_1+3476
	.quad	_EQ__EQ_var_u8_i64
	.quad	_L_1+3492
	.quad	_EQ__EQ_param_f32_f32
	.quad	_L_1+3506
	.quad	_EQ__EQ_var_f32_f32
	.quad	_L_1+3523
	.quad	_EQ__EQ_param_LC_i32
	.quad	_L_1+3538
	.quad	_EQ__EQ_var_LC_i32
	.quad	_L_1+3554
	.quad	_EQ__EQ_param_LC_i16
	.quad	_L_1+3568
	.quad	_EQ__EQ_var_LC_i16
	.quad	_L_1+3584
	.quad	_EQ__EQ_param_LC_L
	.quad	_L_1+3598
	.quad	_EQ__EQ_var_LC_L
	.quad	_L_1+3612
	.quad	_EQ__EQ_param_LC_u64
	.quad	_L_1+3624
	.quad	_EQ__EQ_var_LC_u64
	.quad	_L_1+3640
	.quad	_EQ__EQ_param_LC_u16
	.quad	_L_1+3654
	.quad	_EQ__EQ_var_LC_u16
	.quad	_L_1+3670
	.quad	_EQ__EQ_param_LC_C
	.quad	_L_1+3684
	.quad	_EQ__EQ_var_LC_C
	.quad	_L_1+3698
	.quad	_EQ__EQ_param_LC_u32
	.quad	_L_1+3710
	.quad	_EQ__EQ_var_LC_u32
	.quad	_L_1+3726
	.quad	_EQ__EQ_param_LC_i8
	.quad	_L_1+3740
	.quad	_EQ__EQ_var_LC_i8
	.quad	_L_1+3755
	.quad	_EQ__EQ_param_LC_I
	.quad	_L_1+3768
	.quad	_EQ__EQ_var_LC_I
	.quad	_L_1+3782
	.quad	_EQ__EQ_param_LC_u8
	.quad	_L_1+3794
	.quad	_EQ__EQ_var_LC_u8
	.quad	_L_1+3809
	.quad	_EQ__EQ_param_LC_LC
	.quad	_L_1+3822
	.quad	_EQ__EQ_var_LC_LC
	.quad	_L_1+3837
	.quad	_EQ__EQ_param_LC_i64
	.quad	_L_1+3850
	.quad	_EQ__EQ_var_LC_i64
	.quad	_L_1+3866
	.quad	_EQ__EQ_param_i64_i32
	.quad	_L_1+3880
	.quad	_EQ__EQ_var_i64_i32
	.quad	_L_1+3897
	.quad	_EQ__EQ_param_i64_i16
	.quad	_L_1+3912
	.quad	_EQ__EQ_var_i64_i16
	.quad	_L_1+3929
	.quad	_EQ__EQ_param_i64_L
	.quad	_L_1+3944
	.quad	_EQ__EQ_var_i64_L
	.quad	_L_1+3959
	.quad	_EQ__EQ_param_i64_u64
	.quad	_L_1+3972
	.quad	_EQ__EQ_var_i64_u64
	.quad	_L_1+3989
	.quad	_EQ__EQ_param_i64_u16
	.quad	_L_1+4004
	.quad	_EQ__EQ_var_i64_u16
	.quad	_L_1+4021
	.quad	_EQ__EQ_param_i64_C
	.quad	_L_1+4036
	.quad	_EQ__EQ_var_i64_C
	.quad	_L_1+4051
	.quad	_EQ__EQ_param_i64_u32
	.quad	_L_1+4064
	.quad	_EQ__EQ_var_i64_u32
	.quad	_L_1+4081
	.quad	_EQ__EQ_param_i64_i8
	.quad	_L_1+4096
	.quad	_EQ__EQ_var_i64_i8
	.quad	_L_1+4112
	.quad	_EQ__EQ_param_i64_I
	.quad	_L_1+4126
	.quad	_EQ__EQ_var_i64_I
	.quad	_L_1+4141
	.quad	_EQ__EQ_param_i64_u8
	.quad	_L_1+4154
	.quad	_EQ__EQ_var_i64_u8
	.quad	_L_1+4170
	.quad	_EQ__EQ_param_i64_LC
	.quad	_L_1+4184
	.quad	_EQ__EQ_var_i64_LC
	.quad	_L_1+4200
	.quad	_EQ__EQ_param_i64_i64
	.quad	_L_1+4214
	.quad	_EQ__EQ_var_i64_i64
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
	.quad	529
	.quad	530
	.long	1141170700
	.byte	21
	.space 3
	.quad	534
	.byte	23
	.space 3
	.long	536
	.quad	537
	.word	538
	.space 6
	.quad	539
	.long	4157528343
	.long	1082188883
	.quad	542
	.word	543
	.space 2
	.long	544
	.space 24
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
