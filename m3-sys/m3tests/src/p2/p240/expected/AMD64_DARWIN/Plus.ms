	.text
.globl _Plus__uPlus_var_u16_u16
	.private_extern _Plus__uPlus_var_u16_u16
_Plus__uPlus_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L4
	cmpq	$32767, -16(%rbp)
	jle	L5
L4:
	movl	$1761, %edi
	call	__m3_fault
L5:
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
	jl	L10
	cmpq	$32767, -16(%rbp)
	jle	L11
L10:
	movl	$1825, %edi
	call	__m3_fault
L11:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_u64
	.private_extern _Plus__uPlus_var_u16_u64
_Plus__uPlus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_u64
	.private_extern _Plus__Plus_var_u16_u64
_Plus__Plus_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u16_LC
	.private_extern _Plus__uPlus_var_u16_LC
_Plus__uPlus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_LC
	.private_extern _Plus__Plus_var_u16_LC
_Plus__Plus_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u16_i16
	.private_extern _Plus__uPlus_var_u16_i16
_Plus__uPlus_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L32
	movl	$2145, %edi
	call	__m3_fault
L32:
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
	jle	L37
	movl	$2209, %edi
	call	__m3_fault
L37:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i32
	.private_extern _Plus__uPlus_var_u16_i32
_Plus__uPlus_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L42
	movl	$2273, %edi
	call	__m3_fault
L42:
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
	jle	L47
	movl	$2337, %edi
	call	__m3_fault
L47:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_I
	.private_extern _Plus__uPlus_var_u16_I
_Plus__uPlus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_I
	.private_extern _Plus__Plus_var_u16_I
_Plus__Plus_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L52
	cmpq	$32767, -16(%rbp)
	jle	L53
L52:
	movl	$2401, %edi
	call	__m3_fault
L53:
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
	jl	L58
	cmpq	$32767, -16(%rbp)
	jle	L59
L58:
	movl	$2465, %edi
	call	__m3_fault
L59:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i64
	.private_extern _Plus__uPlus_var_u16_i64
_Plus__uPlus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_i64
	.private_extern _Plus__Plus_var_u16_i64
_Plus__Plus_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u16_C
	.private_extern _Plus__uPlus_var_u16_C
_Plus__uPlus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u16_C
	.private_extern _Plus__Plus_var_u16_C
_Plus__Plus_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L72
	cmpq	$32767, -16(%rbp)
	jle	L73
L72:
	movl	$2657, %edi
	call	__m3_fault
L73:
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
	jl	L78
	cmpq	$32767, -16(%rbp)
	jle	L79
L78:
	movl	$2721, %edi
	call	__m3_fault
L79:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_u8
	.private_extern _Plus__uPlus_var_u16_u8
_Plus__uPlus_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L84
	cmpq	$32767, -16(%rbp)
	jle	L85
L84:
	movl	$2785, %edi
	call	__m3_fault
L85:
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
	jl	L90
	cmpq	$32767, -16(%rbp)
	jle	L91
L90:
	movl	$2849, %edi
	call	__m3_fault
L91:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_i8
	.private_extern _Plus__uPlus_var_u16_i8
_Plus__uPlus_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L96
	movl	$2913, %edi
	call	__m3_fault
L96:
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
	jle	L101
	movl	$2977, %edi
	call	__m3_fault
L101:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u16_L
	.private_extern _Plus__uPlus_var_u16_L
_Plus__uPlus_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_u16_u32
	.private_extern _Plus__uPlus_var_u16_u32
_Plus__uPlus_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L114
	cmpq	$32767, -16(%rbp)
	jle	L115
L114:
	movl	$3169, %edi
	call	__m3_fault
L115:
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
	jl	L120
	cmpq	$32767, -16(%rbp)
	jle	L121
L120:
	movl	$3233, %edi
	call	__m3_fault
L121:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u64_u16
	.private_extern _Plus__uPlus_var_u64_u16
_Plus__uPlus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u16
	.private_extern _Plus__Plus_var_u64_u16
_Plus__Plus_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Plus(%rip), %rax
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
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u64
	.private_extern _Plus__Plus_var_u64_u64
_Plus__Plus_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_LC
	.private_extern _Plus__uPlus_var_u64_LC
_Plus__uPlus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_LC
	.private_extern _Plus__Plus_var_u64_LC
_Plus__Plus_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_i16
	.private_extern _Plus__uPlus_var_u64_i16
_Plus__uPlus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i16
	.private_extern _Plus__Plus_var_u64_i16
_Plus__Plus_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i32
	.private_extern _Plus__Plus_var_u64_i32
_Plus__Plus_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_I
	.private_extern _Plus__uPlus_var_u64_I
_Plus__uPlus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_I
	.private_extern _Plus__Plus_var_u64_I
_Plus__Plus_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_i64
	.private_extern _Plus__uPlus_var_u64_i64
_Plus__uPlus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i64
	.private_extern _Plus__Plus_var_u64_i64
_Plus__Plus_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_C
	.private_extern _Plus__uPlus_var_u64_C
_Plus__uPlus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_C
	.private_extern _Plus__Plus_var_u64_C
_Plus__Plus_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_u8
	.private_extern _Plus__uPlus_var_u64_u8
_Plus__uPlus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u8
	.private_extern _Plus__Plus_var_u64_u8
_Plus__Plus_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_i8
	.private_extern _Plus__uPlus_var_u64_i8
_Plus__uPlus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_i8
	.private_extern _Plus__Plus_var_u64_i8
_Plus__Plus_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u64_L
	.private_extern _Plus__uPlus_var_u64_L
_Plus__uPlus_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Plus(%rip), %rax
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
	movq	112+_MM_Plus(%rip), %rdx
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
.globl _Plus__uPlus_var_u64_u32
	.private_extern _Plus__uPlus_var_u64_u32
_Plus__uPlus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u64_u32
	.private_extern _Plus__Plus_var_u64_u32
_Plus__Plus_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_u16
	.private_extern _Plus__uPlus_var_LC_u16
_Plus__uPlus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u16
	.private_extern _Plus__Plus_var_LC_u16
_Plus__Plus_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Plus(%rip), %rax
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
	movq	120+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u64
	.private_extern _Plus__Plus_var_LC_u64
_Plus__Plus_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_LC
	.private_extern _Plus__uPlus_var_LC_LC
_Plus__uPlus_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_LC
	.private_extern _Plus__Plus_var_LC_LC
_Plus__Plus_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_i16
	.private_extern _Plus__uPlus_var_LC_i16
_Plus__uPlus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i16
	.private_extern _Plus__Plus_var_LC_i16
_Plus__Plus_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Plus(%rip), %rax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i32
	.private_extern _Plus__Plus_var_LC_i32
_Plus__Plus_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_I
	.private_extern _Plus__uPlus_var_LC_I
_Plus__uPlus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_I
	.private_extern _Plus__Plus_var_LC_I
_Plus__Plus_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_i64
	.private_extern _Plus__uPlus_var_LC_i64
_Plus__uPlus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i64
	.private_extern _Plus__Plus_var_LC_i64
_Plus__Plus_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_C
	.private_extern _Plus__uPlus_var_LC_C
_Plus__uPlus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_C
	.private_extern _Plus__Plus_var_LC_C
_Plus__Plus_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_u8
	.private_extern _Plus__uPlus_var_LC_u8
_Plus__uPlus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u8
	.private_extern _Plus__Plus_var_LC_u8
_Plus__Plus_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_i8
	.private_extern _Plus__uPlus_var_LC_i8
_Plus__uPlus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_i8
	.private_extern _Plus__Plus_var_LC_i8
_Plus__Plus_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_L
	.private_extern _Plus__uPlus_var_LC_L
_Plus__uPlus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_L
	.private_extern _Plus__Plus_var_LC_L
_Plus__Plus_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_LC_u32
	.private_extern _Plus__uPlus_var_LC_u32
_Plus__uPlus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_LC_u32
	.private_extern _Plus__Plus_var_LC_u32
_Plus__Plus_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__Plus_var_f64_f64
	.private_extern _Plus__Plus_var_f64_f64
_Plus__Plus_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_Plus(%rip), %xmm0
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
.globl _Plus__uPlus_var_i16_u16
	.private_extern _Plus__uPlus_var_i16_u16
_Plus__uPlus_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L322
	cmpq	$65535, -16(%rbp)
	jle	L323
L322:
	movl	$6433, %edi
	call	__m3_fault
L323:
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
	js	L328
	cmpq	$65535, -16(%rbp)
	jle	L329
L328:
	movl	$6497, %edi
	call	__m3_fault
L329:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_u64
	.private_extern _Plus__uPlus_var_i16_u64
_Plus__uPlus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_u64
	.private_extern _Plus__Plus_var_i16_u64
_Plus__Plus_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i16_LC
	.private_extern _Plus__uPlus_var_i16_LC
_Plus__uPlus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_LC
	.private_extern _Plus__Plus_var_i16_LC
_Plus__Plus_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i16_i16
	.private_extern _Plus__uPlus_var_i16_i16
_Plus__uPlus_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L350
	movl	$6817, %edi
	call	__m3_fault
L350:
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
	jle	L355
	movl	$6881, %edi
	call	__m3_fault
L355:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i32
	.private_extern _Plus__uPlus_var_i16_i32
_Plus__uPlus_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L360
	movl	$6945, %edi
	call	__m3_fault
L360:
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
	jle	L365
	movl	$7009, %edi
	call	__m3_fault
L365:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_I
	.private_extern _Plus__uPlus_var_i16_I
_Plus__uPlus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_I
	.private_extern _Plus__Plus_var_i16_I
_Plus__Plus_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L370
	cmpq	$65535, -16(%rbp)
	jle	L371
L370:
	movl	$7073, %edi
	call	__m3_fault
L371:
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
	js	L376
	cmpq	$65535, -16(%rbp)
	jle	L377
L376:
	movl	$7137, %edi
	call	__m3_fault
L377:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i64
	.private_extern _Plus__uPlus_var_i16_i64
_Plus__uPlus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_i64
	.private_extern _Plus__Plus_var_i16_i64
_Plus__Plus_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i16_C
	.private_extern _Plus__uPlus_var_i16_C
_Plus__uPlus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i16_C
	.private_extern _Plus__Plus_var_i16_C
_Plus__Plus_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L390
	cmpq	$65535, -16(%rbp)
	jle	L391
L390:
	movl	$7329, %edi
	call	__m3_fault
L391:
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
	js	L396
	cmpq	$65535, -16(%rbp)
	jle	L397
L396:
	movl	$7393, %edi
	call	__m3_fault
L397:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_u8
	.private_extern _Plus__uPlus_var_i16_u8
_Plus__uPlus_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L402
	cmpq	$65535, -16(%rbp)
	jle	L403
L402:
	movl	$7457, %edi
	call	__m3_fault
L403:
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
	js	L408
	cmpq	$65535, -16(%rbp)
	jle	L409
L408:
	movl	$7521, %edi
	call	__m3_fault
L409:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_i8
	.private_extern _Plus__uPlus_var_i16_i8
_Plus__uPlus_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L414
	movl	$7585, %edi
	call	__m3_fault
L414:
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
	jle	L419
	movl	$7649, %edi
	call	__m3_fault
L419:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i16_L
	.private_extern _Plus__uPlus_var_i16_L
_Plus__uPlus_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_i16_u32
	.private_extern _Plus__uPlus_var_i16_u32
_Plus__uPlus_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L432
	cmpq	$65535, -16(%rbp)
	jle	L433
L432:
	movl	$7841, %edi
	call	__m3_fault
L433:
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
	js	L438
	cmpq	$65535, -16(%rbp)
	jle	L439
L438:
	movl	$7905, %edi
	call	__m3_fault
L439:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u16
	.private_extern _Plus__uPlus_var_i32_u16
_Plus__uPlus_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L444
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L445
L444:
	movl	$7969, %edi
	call	__m3_fault
L445:
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
	js	L450
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L451
L450:
	movl	$8033, %edi
	call	__m3_fault
L451:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u64
	.private_extern _Plus__uPlus_var_i32_u64
_Plus__uPlus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_u64
	.private_extern _Plus__Plus_var_i32_u64
_Plus__Plus_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i32_LC
	.private_extern _Plus__uPlus_var_i32_LC
_Plus__uPlus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_LC
	.private_extern _Plus__Plus_var_i32_LC
_Plus__Plus_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i32_i16
	.private_extern _Plus__uPlus_var_i32_i16
_Plus__uPlus_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L472
	movl	$8353, %edi
	call	__m3_fault
L472:
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
	jle	L477
	movl	$8417, %edi
	call	__m3_fault
L477:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i32
	.private_extern _Plus__uPlus_var_i32_i32
_Plus__uPlus_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L482
	movl	$8481, %edi
	call	__m3_fault
L482:
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
	jle	L487
	movl	$8545, %edi
	call	__m3_fault
L487:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_I
	.private_extern _Plus__uPlus_var_i32_I
_Plus__uPlus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_I
	.private_extern _Plus__Plus_var_i32_I
_Plus__Plus_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L492
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L493
L492:
	movl	$8609, %edi
	call	__m3_fault
L493:
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
	js	L498
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L499
L498:
	movl	$8673, %edi
	call	__m3_fault
L499:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i64
	.private_extern _Plus__uPlus_var_i32_i64
_Plus__uPlus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_i64
	.private_extern _Plus__Plus_var_i32_i64
_Plus__Plus_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i32_C
	.private_extern _Plus__uPlus_var_i32_C
_Plus__uPlus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i32_C
	.private_extern _Plus__Plus_var_i32_C
_Plus__Plus_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L512
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L513
L512:
	movl	$8865, %edi
	call	__m3_fault
L513:
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
	js	L518
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L519
L518:
	movl	$8929, %edi
	call	__m3_fault
L519:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_u8
	.private_extern _Plus__uPlus_var_i32_u8
_Plus__uPlus_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L524
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L525
L524:
	movl	$8993, %edi
	call	__m3_fault
L525:
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
	js	L530
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L531
L530:
	movl	$9057, %edi
	call	__m3_fault
L531:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_i8
	.private_extern _Plus__uPlus_var_i32_i8
_Plus__uPlus_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L536
	movl	$9121, %edi
	call	__m3_fault
L536:
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
	jle	L541
	movl	$9185, %edi
	call	__m3_fault
L541:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i32_L
	.private_extern _Plus__uPlus_var_i32_L
_Plus__uPlus_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_i32_u32
	.private_extern _Plus__uPlus_var_i32_u32
_Plus__uPlus_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L554
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L555
L554:
	movl	$9377, %edi
	call	__m3_fault
L555:
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
	js	L560
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L561
L560:
	movl	$9441, %edi
	call	__m3_fault
L561:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_I_u16
	.private_extern _Plus__uPlus_var_I_u16
_Plus__uPlus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u16
	.private_extern _Plus__Plus_var_I_u16
_Plus__Plus_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u64
	.private_extern _Plus__Plus_var_I_u64
_Plus__Plus_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_I_LC
	.private_extern _Plus__uPlus_var_I_LC
_Plus__uPlus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_LC
	.private_extern _Plus__Plus_var_I_LC
_Plus__Plus_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_I_i16
	.private_extern _Plus__uPlus_var_I_i16
_Plus__uPlus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i16
	.private_extern _Plus__Plus_var_I_i16
_Plus__Plus_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i32
	.private_extern _Plus__Plus_var_I_i32
_Plus__Plus_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movl	140+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_I_I
	.private_extern _Plus__uPlus_var_I_I
_Plus__uPlus_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_I
	.private_extern _Plus__Plus_var_I_I
_Plus__Plus_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	144+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_I_i64
	.private_extern _Plus__uPlus_var_I_i64
_Plus__uPlus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i64
	.private_extern _Plus__Plus_var_I_i64
_Plus__Plus_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_I_C
	.private_extern _Plus__uPlus_var_I_C
_Plus__uPlus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_C
	.private_extern _Plus__Plus_var_I_C
_Plus__Plus_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_I_u8
	.private_extern _Plus__uPlus_var_I_u8
_Plus__uPlus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u8
	.private_extern _Plus__Plus_var_I_u8
_Plus__Plus_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzbl	172+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_I_i8
	.private_extern _Plus__uPlus_var_I_i8
_Plus__uPlus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_i8
	.private_extern _Plus__Plus_var_I_i8
_Plus__Plus_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movzbl	173+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_I_L
	.private_extern _Plus__uPlus_var_I_L
_Plus__uPlus_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
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
	movq	144+_MM_Plus(%rip), %rdx
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
.globl _Plus__uPlus_var_I_u32
	.private_extern _Plus__uPlus_var_I_u32
_Plus__uPlus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_I_u32
	.private_extern _Plus__Plus_var_I_u32
_Plus__Plus_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movl	184+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_i64_u16
	.private_extern _Plus__uPlus_var_i64_u16
_Plus__uPlus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u16
	.private_extern _Plus__Plus_var_i64_u16
_Plus__Plus_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Plus(%rip), %rax
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
	movq	152+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u64
	.private_extern _Plus__Plus_var_i64_u64
_Plus__Plus_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_LC
	.private_extern _Plus__uPlus_var_i64_LC
_Plus__uPlus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_LC
	.private_extern _Plus__Plus_var_i64_LC
_Plus__Plus_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_i16
	.private_extern _Plus__uPlus_var_i64_i16
_Plus__uPlus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i16
	.private_extern _Plus__Plus_var_i64_i16
_Plus__Plus_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Plus(%rip), %rax
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
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i32
	.private_extern _Plus__Plus_var_i64_i32
_Plus__Plus_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_I
	.private_extern _Plus__uPlus_var_i64_I
_Plus__uPlus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_I
	.private_extern _Plus__Plus_var_i64_I
_Plus__Plus_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_i64
	.private_extern _Plus__uPlus_var_i64_i64
_Plus__uPlus_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i64
	.private_extern _Plus__Plus_var_i64_i64
_Plus__Plus_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_C
	.private_extern _Plus__uPlus_var_i64_C
_Plus__uPlus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_C
	.private_extern _Plus__Plus_var_i64_C
_Plus__Plus_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_u8
	.private_extern _Plus__uPlus_var_i64_u8
_Plus__uPlus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u8
	.private_extern _Plus__Plus_var_i64_u8
_Plus__Plus_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_i8
	.private_extern _Plus__uPlus_var_i64_i8
_Plus__uPlus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_i8
	.private_extern _Plus__Plus_var_i64_i8
_Plus__Plus_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i64_L
	.private_extern _Plus__uPlus_var_i64_L
_Plus__uPlus_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Plus(%rip), %rax
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
	movq	152+_MM_Plus(%rip), %rdx
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
.globl _Plus__uPlus_var_i64_u32
	.private_extern _Plus__uPlus_var_i64_u32
_Plus__uPlus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i64_u32
	.private_extern _Plus__Plus_var_i64_u32
_Plus__Plus_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_C_u16
	.private_extern _Plus__uPlus_var_C_u16
_Plus__uPlus_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L758
	movl	$12577, %edi
	call	__m3_fault
L758:
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
	jns	L763
	movl	$12641, %edi
	call	__m3_fault
L763:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_u64
	.private_extern _Plus__uPlus_var_C_u64
_Plus__uPlus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_u64
	.private_extern _Plus__Plus_var_C_u64
_Plus__Plus_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_C_LC
	.private_extern _Plus__uPlus_var_C_LC
_Plus__uPlus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_LC
	.private_extern _Plus__Plus_var_C_LC
_Plus__Plus_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_C_i16
	.private_extern _Plus__uPlus_var_C_i16
_Plus__uPlus_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L784
	movl	$12961, %edi
	call	__m3_fault
L784:
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
	jns	L789
	movl	$13025, %edi
	call	__m3_fault
L789:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i32
	.private_extern _Plus__uPlus_var_C_i32
_Plus__uPlus_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movl	140+_MM_Plus(%rip), %eax
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
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L794
	movl	$13089, %edi
	call	__m3_fault
L794:
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
	jns	L799
	movl	$13153, %edi
	call	__m3_fault
L799:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_I
	.private_extern _Plus__uPlus_var_C_I
_Plus__uPlus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_I
	.private_extern _Plus__Plus_var_C_I
_Plus__Plus_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L804
	movl	$13217, %edi
	call	__m3_fault
L804:
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
	jns	L809
	movl	$13281, %edi
	call	__m3_fault
L809:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i64
	.private_extern _Plus__uPlus_var_C_i64
_Plus__uPlus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_i64
	.private_extern _Plus__Plus_var_C_i64
_Plus__Plus_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_C_C
	.private_extern _Plus__uPlus_var_C_C
_Plus__uPlus_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_C
	.private_extern _Plus__Plus_var_C_C
_Plus__Plus_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L822
	movl	$13473, %edi
	call	__m3_fault
L822:
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
	jns	L827
	movl	$13537, %edi
	call	__m3_fault
L827:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_u8
	.private_extern _Plus__uPlus_var_C_u8
_Plus__uPlus_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L832
	movl	$13601, %edi
	call	__m3_fault
L832:
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
	jns	L837
	movl	$13665, %edi
	call	__m3_fault
L837:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_i8
	.private_extern _Plus__uPlus_var_C_i8
_Plus__uPlus_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L842
	movl	$13729, %edi
	call	__m3_fault
L842:
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
	jns	L847
	movl	$13793, %edi
	call	__m3_fault
L847:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_C_L
	.private_extern _Plus__uPlus_var_C_L
_Plus__uPlus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_C_L
	.private_extern _Plus__Plus_var_C_L
_Plus__Plus_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_C_u32
	.private_extern _Plus__uPlus_var_C_u32
_Plus__uPlus_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movl	184+_MM_Plus(%rip), %eax
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
	movq	160+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L860
	movl	$13985, %edi
	call	__m3_fault
L860:
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
	jns	L865
	movl	$14049, %edi
	call	__m3_fault
L865:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__Plus_var_f32_f32
	.private_extern _Plus__Plus_var_f32_f32
_Plus__Plus_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_Plus(%rip), %xmm0
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
.globl _Plus__uPlus_var_u8_u16
	.private_extern _Plus__uPlus_var_u8_u16
_Plus__uPlus_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L874
	cmpq	$127, -16(%rbp)
	jle	L875
L874:
	movl	$14177, %edi
	call	__m3_fault
L875:
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
	jl	L880
	cmpq	$127, -16(%rbp)
	jle	L881
L880:
	movl	$14241, %edi
	call	__m3_fault
L881:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_u64
	.private_extern _Plus__uPlus_var_u8_u64
_Plus__uPlus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_u64
	.private_extern _Plus__Plus_var_u8_u64
_Plus__Plus_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u8_LC
	.private_extern _Plus__uPlus_var_u8_LC
_Plus__uPlus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_LC
	.private_extern _Plus__Plus_var_u8_LC
_Plus__Plus_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u8_i16
	.private_extern _Plus__uPlus_var_u8_i16
_Plus__uPlus_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L902
	movl	$14561, %edi
	call	__m3_fault
L902:
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
	jle	L907
	movl	$14625, %edi
	call	__m3_fault
L907:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_i32
	.private_extern _Plus__uPlus_var_u8_i32
_Plus__uPlus_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L912
	movl	$14689, %edi
	call	__m3_fault
L912:
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
	jle	L917
	movl	$14753, %edi
	call	__m3_fault
L917:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_I
	.private_extern _Plus__uPlus_var_u8_I
_Plus__uPlus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_I
	.private_extern _Plus__Plus_var_u8_I
_Plus__Plus_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L922
	cmpq	$127, -16(%rbp)
	jle	L923
L922:
	movl	$14817, %edi
	call	__m3_fault
L923:
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
	jl	L928
	cmpq	$127, -16(%rbp)
	jle	L929
L928:
	movl	$14881, %edi
	call	__m3_fault
L929:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_i64
	.private_extern _Plus__uPlus_var_u8_i64
_Plus__uPlus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_i64
	.private_extern _Plus__Plus_var_u8_i64
_Plus__Plus_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u8_C
	.private_extern _Plus__uPlus_var_u8_C
_Plus__uPlus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u8_C
	.private_extern _Plus__Plus_var_u8_C
_Plus__Plus_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L942
	cmpq	$127, -16(%rbp)
	jle	L943
L942:
	movl	$15073, %edi
	call	__m3_fault
L943:
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
	jl	L948
	cmpq	$127, -16(%rbp)
	jle	L949
L948:
	movl	$15137, %edi
	call	__m3_fault
L949:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_u8
	.private_extern _Plus__uPlus_var_u8_u8
_Plus__uPlus_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L954
	cmpq	$127, -16(%rbp)
	jle	L955
L954:
	movl	$15201, %edi
	call	__m3_fault
L955:
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
	jl	L960
	cmpq	$127, -16(%rbp)
	jle	L961
L960:
	movl	$15265, %edi
	call	__m3_fault
L961:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_i8
	.private_extern _Plus__uPlus_var_u8_i8
_Plus__uPlus_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L966
	movl	$15329, %edi
	call	__m3_fault
L966:
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
	jle	L971
	movl	$15393, %edi
	call	__m3_fault
L971:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u8_L
	.private_extern _Plus__uPlus_var_u8_L
_Plus__uPlus_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_u8_u32
	.private_extern _Plus__uPlus_var_u8_u32
_Plus__uPlus_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L984
	cmpq	$127, -16(%rbp)
	jle	L985
L984:
	movl	$15585, %edi
	call	__m3_fault
L985:
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
	jl	L990
	cmpq	$127, -16(%rbp)
	jle	L991
L990:
	movl	$15649, %edi
	call	__m3_fault
L991:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u16
	.private_extern _Plus__uPlus_var_i8_u16
_Plus__uPlus_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L996
	cmpq	$255, -16(%rbp)
	jle	L997
L996:
	movl	$15713, %edi
	call	__m3_fault
L997:
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
	js	L1002
	cmpq	$255, -16(%rbp)
	jle	L1003
L1002:
	movl	$15777, %edi
	call	__m3_fault
L1003:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u64
	.private_extern _Plus__uPlus_var_i8_u64
_Plus__uPlus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_u64
	.private_extern _Plus__Plus_var_i8_u64
_Plus__Plus_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i8_LC
	.private_extern _Plus__uPlus_var_i8_LC
_Plus__uPlus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_LC
	.private_extern _Plus__Plus_var_i8_LC
_Plus__Plus_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i8_i16
	.private_extern _Plus__uPlus_var_i8_i16
_Plus__uPlus_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1024
	movl	$16097, %edi
	call	__m3_fault
L1024:
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
	jle	L1029
	movl	$16161, %edi
	call	__m3_fault
L1029:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_i32
	.private_extern _Plus__uPlus_var_i8_i32
_Plus__uPlus_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1034
	movl	$16225, %edi
	call	__m3_fault
L1034:
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
	jle	L1039
	movl	$16289, %edi
	call	__m3_fault
L1039:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_I
	.private_extern _Plus__uPlus_var_i8_I
_Plus__uPlus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_I
	.private_extern _Plus__Plus_var_i8_I
_Plus__Plus_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1044
	cmpq	$255, -16(%rbp)
	jle	L1045
L1044:
	movl	$16353, %edi
	call	__m3_fault
L1045:
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
	js	L1050
	cmpq	$255, -16(%rbp)
	jle	L1051
L1050:
	movl	$16417, %edi
	call	__m3_fault
L1051:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_i64
	.private_extern _Plus__uPlus_var_i8_i64
_Plus__uPlus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_i64
	.private_extern _Plus__Plus_var_i8_i64
_Plus__Plus_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_i8_C
	.private_extern _Plus__uPlus_var_i8_C
_Plus__uPlus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_i8_C
	.private_extern _Plus__Plus_var_i8_C
_Plus__Plus_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1064
	cmpq	$255, -16(%rbp)
	jle	L1065
L1064:
	movl	$16609, %edi
	call	__m3_fault
L1065:
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
	js	L1070
	cmpq	$255, -16(%rbp)
	jle	L1071
L1070:
	movl	$16673, %edi
	call	__m3_fault
L1071:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_u8
	.private_extern _Plus__uPlus_var_i8_u8
_Plus__uPlus_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1076
	cmpq	$255, -16(%rbp)
	jle	L1077
L1076:
	movl	$16737, %edi
	call	__m3_fault
L1077:
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
	js	L1082
	cmpq	$255, -16(%rbp)
	jle	L1083
L1082:
	movl	$16801, %edi
	call	__m3_fault
L1083:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_i8
	.private_extern _Plus__uPlus_var_i8_i8
_Plus__uPlus_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1088
	movl	$16865, %edi
	call	__m3_fault
L1088:
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
	jle	L1093
	movl	$16929, %edi
	call	__m3_fault
L1093:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_i8_L
	.private_extern _Plus__uPlus_var_i8_L
_Plus__uPlus_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_i8_u32
	.private_extern _Plus__uPlus_var_i8_u32
_Plus__uPlus_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1106
	cmpq	$255, -16(%rbp)
	jle	L1107
L1106:
	movl	$17121, %edi
	call	__m3_fault
L1107:
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
	js	L1112
	cmpq	$255, -16(%rbp)
	jle	L1113
L1112:
	movl	$17185, %edi
	call	__m3_fault
L1113:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_L_u16
	.private_extern _Plus__uPlus_var_L_u16
_Plus__uPlus_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Plus(%rip), %eax
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
	movzwl	104+_MM_Plus(%rip), %eax
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
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_u64
	.private_extern _Plus__Plus_var_L_u64
_Plus__Plus_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_L_LC
	.private_extern _Plus__uPlus_var_L_LC
_Plus__uPlus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_LC
	.private_extern _Plus__Plus_var_L_LC
_Plus__Plus_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_L_i16
	.private_extern _Plus__uPlus_var_L_i16
_Plus__uPlus_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Plus(%rip), %eax
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
	movzwl	136+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
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
	movl	140+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_L_I
	.private_extern _Plus__uPlus_var_L_I
_Plus__uPlus_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Plus(%rip), %rax
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
	movq	144+_MM_Plus(%rip), %rdx
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
.globl _Plus__uPlus_var_L_i64
	.private_extern _Plus__uPlus_var_L_i64
_Plus__uPlus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_i64
	.private_extern _Plus__Plus_var_L_i64
_Plus__Plus_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Plus(%rip), %rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_L_C
	.private_extern _Plus__uPlus_var_L_C
_Plus__uPlus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rdx
	movq	176+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_L_C
	.private_extern _Plus__Plus_var_L_C
_Plus__Plus_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_L_u8
	.private_extern _Plus__uPlus_var_L_u8
_Plus__uPlus_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Plus(%rip), %eax
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
	movzbl	172+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_L_i8
	.private_extern _Plus__uPlus_var_L_i8
_Plus__uPlus_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Plus(%rip), %eax
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
	movzbl	173+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_L_u32
	.private_extern _Plus__uPlus_var_L_u32
_Plus__uPlus_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_u32_u16
	.private_extern _Plus__uPlus_var_u32_u16
_Plus__uPlus_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Plus(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1214
	cmpq	$2147483647, -16(%rbp)
	jle	L1215
L1214:
	movl	$18785, %edi
	call	__m3_fault
L1215:
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
	jl	L1220
	cmpq	$2147483647, -16(%rbp)
	jle	L1221
L1220:
	movl	$18849, %edi
	call	__m3_fault
L1221:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_u64
	.private_extern _Plus__uPlus_var_u32_u64
_Plus__uPlus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_u64
	.private_extern _Plus__Plus_var_u32_u64
_Plus__Plus_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u32_LC
	.private_extern _Plus__uPlus_var_u32_LC
_Plus__uPlus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_LC
	.private_extern _Plus__Plus_var_u32_LC
_Plus__Plus_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u32_i16
	.private_extern _Plus__uPlus_var_u32_i16
_Plus__uPlus_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Plus(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1242
	movl	$19169, %edi
	call	__m3_fault
L1242:
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
	jle	L1247
	movl	$19233, %edi
	call	__m3_fault
L1247:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i32
	.private_extern _Plus__uPlus_var_u32_i32
_Plus__uPlus_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Plus(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1252
	movl	$19297, %edi
	call	__m3_fault
L1252:
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
	jle	L1257
	movl	$19361, %edi
	call	__m3_fault
L1257:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_I
	.private_extern _Plus__uPlus_var_u32_I
_Plus__uPlus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_I
	.private_extern _Plus__Plus_var_u32_I
_Plus__Plus_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1262
	cmpq	$2147483647, -16(%rbp)
	jle	L1263
L1262:
	movl	$19425, %edi
	call	__m3_fault
L1263:
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
	jl	L1268
	cmpq	$2147483647, -16(%rbp)
	jle	L1269
L1268:
	movl	$19489, %edi
	call	__m3_fault
L1269:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i64
	.private_extern _Plus__uPlus_var_u32_i64
_Plus__uPlus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_i64
	.private_extern _Plus__Plus_var_u32_i64
_Plus__Plus_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Plus(%rip), %rax
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
.globl _Plus__uPlus_var_u32_C
	.private_extern _Plus__uPlus_var_u32_C
_Plus__uPlus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Plus__Plus_var_u32_C
	.private_extern _Plus__Plus_var_u32_C
_Plus__Plus_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Plus(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1282
	cmpq	$2147483647, -16(%rbp)
	jle	L1283
L1282:
	movl	$19681, %edi
	call	__m3_fault
L1283:
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
	jl	L1288
	cmpq	$2147483647, -16(%rbp)
	jle	L1289
L1288:
	movl	$19745, %edi
	call	__m3_fault
L1289:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_u8
	.private_extern _Plus__uPlus_var_u32_u8
_Plus__uPlus_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Plus(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1294
	cmpq	$2147483647, -16(%rbp)
	jle	L1295
L1294:
	movl	$19809, %edi
	call	__m3_fault
L1295:
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
	jl	L1300
	cmpq	$2147483647, -16(%rbp)
	jle	L1301
L1300:
	movl	$19873, %edi
	call	__m3_fault
L1301:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_i8
	.private_extern _Plus__uPlus_var_u32_i8
_Plus__uPlus_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Plus(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1306
	movl	$19937, %edi
	call	__m3_fault
L1306:
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
	jle	L1311
	movl	$20001, %edi
	call	__m3_fault
L1311:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Plus__uPlus_var_u32_L
	.private_extern _Plus__uPlus_var_u32_L
_Plus__uPlus_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
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
.globl _Plus__uPlus_var_u32_u32
	.private_extern _Plus__uPlus_var_u32_u32
_Plus__uPlus_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	184+_MM_Plus(%rip), %eax
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
	movl	184+_MM_Plus(%rip), %eax
	movslq	%eax,%rdx
	movl	184+_MM_Plus(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1324
	cmpq	$2147483647, -16(%rbp)
	jle	L1325
L1324:
	movl	$20193, %edi
	call	__m3_fault
L1325:
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
	jl	L1330
	cmpq	$2147483647, -16(%rbp)
	jle	L1331
L1330:
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
	.ascii "Plus_param_u32_u32"
	.space 1
	.ascii "uPlus_param_u32_u32"
	.space 1
	.ascii "Plus_var_u32_u32"
	.space 1
	.ascii "uPlus_var_u32_u32"
	.space 1
	.ascii "Plus_param_u32_L"
	.space 1
	.ascii "uPlus_param_u32_L"
	.space 1
	.ascii "Plus_var_u32_L"
	.space 1
	.ascii "uPlus_var_u32_L"
	.space 1
	.ascii "Plus_param_u32_i8"
	.space 1
	.ascii "uPlus_param_u32_i8"
	.space 1
	.ascii "Plus_var_u32_i8"
	.space 1
	.ascii "uPlus_var_u32_i8"
	.space 1
	.ascii "Plus_param_u32_u8"
	.space 1
	.ascii "uPlus_param_u32_u8"
	.space 1
	.ascii "Plus_var_u32_u8"
	.space 1
	.ascii "uPlus_var_u32_u8"
	.space 1
	.ascii "Plus_param_u32_C"
	.space 1
	.ascii "uPlus_param_u32_C"
	.space 1
	.ascii "Plus_var_u32_C"
	.space 1
	.ascii "uPlus_var_u32_C"
	.space 1
	.ascii "Plus_param_u32_i64"
	.space 1
	.ascii "uPlus_param_u32_i64"
	.space 1
	.ascii "Plus_var_u32_i64"
	.space 1
	.ascii "uPlus_var_u32_i64"
	.space 1
	.ascii "Plus_param_u32_I"
	.space 1
	.ascii "uPlus_param_u32_I"
	.space 1
	.ascii "Plus_var_u32_I"
	.space 1
	.ascii "uPlus_var_u32_I"
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
	.ascii "Plus_param_u32_LC"
	.space 1
	.ascii "uPlus_param_u32_LC"
	.space 1
	.ascii "Plus_var_u32_LC"
	.space 1
	.ascii "uPlus_var_u32_LC"
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
	.ascii "Plus_param_L_u32"
	.space 1
	.ascii "uPlus_param_L_u32"
	.space 1
	.ascii "Plus_var_L_u32"
	.space 1
	.ascii "uPlus_var_L_u32"
	.space 1
	.ascii "Plus_param_L_L"
	.space 1
	.ascii "uPlus_param_L_L"
	.space 1
	.ascii "Plus_var_L_L"
	.space 1
	.ascii "uPlus_var_L_L"
	.space 1
	.ascii "Plus_param_L_i8"
	.space 1
	.ascii "uPlus_param_L_i8"
	.space 1
	.ascii "Plus_var_L_i8"
	.space 1
	.ascii "uPlus_var_L_i8"
	.space 1
	.ascii "Plus_param_L_u8"
	.space 1
	.ascii "uPlus_param_L_u8"
	.space 1
	.ascii "Plus_var_L_u8"
	.space 1
	.ascii "uPlus_var_L_u8"
	.space 1
	.ascii "Plus_param_L_C"
	.space 1
	.ascii "uPlus_param_L_C"
	.space 1
	.ascii "Plus_var_L_C"
	.space 1
	.ascii "uPlus_var_L_C"
	.space 1
	.ascii "Plus_param_L_i64"
	.space 1
	.ascii "uPlus_param_L_i64"
	.space 1
	.ascii "Plus_var_L_i64"
	.space 1
	.ascii "uPlus_var_L_i64"
	.space 1
	.ascii "Plus_param_L_I"
	.space 1
	.ascii "uPlus_param_L_I"
	.space 1
	.ascii "Plus_var_L_I"
	.space 1
	.ascii "uPlus_var_L_I"
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
	.ascii "Plus_param_L_LC"
	.space 1
	.ascii "uPlus_param_L_LC"
	.space 1
	.ascii "Plus_var_L_LC"
	.space 1
	.ascii "uPlus_var_L_LC"
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
	.ascii "Plus_param_i8_u32"
	.space 1
	.ascii "uPlus_param_i8_u32"
	.space 1
	.ascii "Plus_var_i8_u32"
	.space 1
	.ascii "uPlus_var_i8_u32"
	.space 1
	.ascii "Plus_param_i8_L"
	.space 1
	.ascii "uPlus_param_i8_L"
	.space 1
	.ascii "Plus_var_i8_L"
	.space 1
	.ascii "uPlus_var_i8_L"
	.space 1
	.ascii "Plus_param_i8_i8"
	.space 1
	.ascii "uPlus_param_i8_i8"
	.space 1
	.ascii "Plus_var_i8_i8"
	.space 1
	.ascii "uPlus_var_i8_i8"
	.space 1
	.ascii "Plus_param_i8_u8"
	.space 1
	.ascii "uPlus_param_i8_u8"
	.space 1
	.ascii "Plus_var_i8_u8"
	.space 1
	.ascii "uPlus_var_i8_u8"
	.space 1
	.ascii "Plus_param_i8_C"
	.space 1
	.ascii "uPlus_param_i8_C"
	.space 1
	.ascii "Plus_var_i8_C"
	.space 1
	.ascii "uPlus_var_i8_C"
	.space 1
	.ascii "Plus_param_i8_i64"
	.space 1
	.ascii "uPlus_param_i8_i64"
	.space 1
	.ascii "Plus_var_i8_i64"
	.space 1
	.ascii "uPlus_var_i8_i64"
	.space 1
	.ascii "Plus_param_i8_I"
	.space 1
	.ascii "uPlus_param_i8_I"
	.space 1
	.ascii "Plus_var_i8_I"
	.space 1
	.ascii "uPlus_var_i8_I"
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
	.ascii "Plus_param_i8_LC"
	.space 1
	.ascii "uPlus_param_i8_LC"
	.space 1
	.ascii "Plus_var_i8_LC"
	.space 1
	.ascii "uPlus_var_i8_LC"
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
	.ascii "Plus_param_u8_u32"
	.space 1
	.ascii "uPlus_param_u8_u32"
	.space 1
	.ascii "Plus_var_u8_u32"
	.space 1
	.ascii "uPlus_var_u8_u32"
	.space 1
	.ascii "Plus_param_u8_L"
	.space 1
	.ascii "uPlus_param_u8_L"
	.space 1
	.ascii "Plus_var_u8_L"
	.space 1
	.ascii "uPlus_var_u8_L"
	.space 1
	.ascii "Plus_param_u8_i8"
	.space 1
	.ascii "uPlus_param_u8_i8"
	.space 1
	.ascii "Plus_var_u8_i8"
	.space 1
	.ascii "uPlus_var_u8_i8"
	.space 1
	.ascii "Plus_param_u8_u8"
	.space 1
	.ascii "uPlus_param_u8_u8"
	.space 1
	.ascii "Plus_var_u8_u8"
	.space 1
	.ascii "uPlus_var_u8_u8"
	.space 1
	.ascii "Plus_param_u8_C"
	.space 1
	.ascii "uPlus_param_u8_C"
	.space 1
	.ascii "Plus_var_u8_C"
	.space 1
	.ascii "uPlus_var_u8_C"
	.space 1
	.ascii "Plus_param_u8_i64"
	.space 1
	.ascii "uPlus_param_u8_i64"
	.space 1
	.ascii "Plus_var_u8_i64"
	.space 1
	.ascii "uPlus_var_u8_i64"
	.space 1
	.ascii "Plus_param_u8_I"
	.space 1
	.ascii "uPlus_param_u8_I"
	.space 1
	.ascii "Plus_var_u8_I"
	.space 1
	.ascii "uPlus_var_u8_I"
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
	.ascii "Plus_param_u8_LC"
	.space 1
	.ascii "uPlus_param_u8_LC"
	.space 1
	.ascii "Plus_var_u8_LC"
	.space 1
	.ascii "uPlus_var_u8_LC"
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
	.ascii "Plus_param_f32_f32"
	.space 1
	.ascii "Plus_var_f32_f32"
	.space 1
	.ascii "Plus_param_C_u32"
	.space 1
	.ascii "uPlus_param_C_u32"
	.space 1
	.ascii "Plus_var_C_u32"
	.space 1
	.ascii "uPlus_var_C_u32"
	.space 1
	.ascii "Plus_param_C_L"
	.space 1
	.ascii "uPlus_param_C_L"
	.space 1
	.ascii "Plus_var_C_L"
	.space 1
	.ascii "uPlus_var_C_L"
	.space 1
	.ascii "Plus_param_C_i8"
	.space 1
	.ascii "uPlus_param_C_i8"
	.space 1
	.ascii "Plus_var_C_i8"
	.space 1
	.ascii "uPlus_var_C_i8"
	.space 1
	.ascii "Plus_param_C_u8"
	.space 1
	.ascii "uPlus_param_C_u8"
	.space 1
	.ascii "Plus_var_C_u8"
	.space 1
	.ascii "uPlus_var_C_u8"
	.space 1
	.ascii "Plus_param_C_C"
	.space 1
	.ascii "uPlus_param_C_C"
	.space 1
	.ascii "Plus_var_C_C"
	.space 1
	.ascii "uPlus_var_C_C"
	.space 1
	.ascii "Plus_param_C_i64"
	.space 1
	.ascii "uPlus_param_C_i64"
	.space 1
	.ascii "Plus_var_C_i64"
	.space 1
	.ascii "uPlus_var_C_i64"
	.space 1
	.ascii "Plus_param_C_I"
	.space 1
	.ascii "uPlus_param_C_I"
	.space 1
	.ascii "Plus_var_C_I"
	.space 1
	.ascii "uPlus_var_C_I"
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
	.ascii "Plus_param_C_LC"
	.space 1
	.ascii "uPlus_param_C_LC"
	.space 1
	.ascii "Plus_var_C_LC"
	.space 1
	.ascii "uPlus_var_C_LC"
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
	.ascii "Plus_param_i64_u32"
	.space 1
	.ascii "uPlus_param_i64_u32"
	.space 1
	.ascii "Plus_var_i64_u32"
	.space 1
	.ascii "uPlus_var_i64_u32"
	.space 1
	.ascii "Plus_param_i64_L"
	.space 1
	.ascii "uPlus_param_i64_L"
	.space 1
	.ascii "Plus_var_i64_L"
	.space 1
	.ascii "uPlus_var_i64_L"
	.space 1
	.ascii "Plus_param_i64_i8"
	.space 1
	.ascii "uPlus_param_i64_i8"
	.space 1
	.ascii "Plus_var_i64_i8"
	.space 1
	.ascii "uPlus_var_i64_i8"
	.space 1
	.ascii "Plus_param_i64_u8"
	.space 1
	.ascii "uPlus_param_i64_u8"
	.space 1
	.ascii "Plus_var_i64_u8"
	.space 1
	.ascii "uPlus_var_i64_u8"
	.space 1
	.ascii "Plus_param_i64_C"
	.space 1
	.ascii "uPlus_param_i64_C"
	.space 1
	.ascii "Plus_var_i64_C"
	.space 1
	.ascii "uPlus_var_i64_C"
	.space 1
	.ascii "Plus_param_i64_i64"
	.space 1
	.ascii "uPlus_param_i64_i64"
	.space 1
	.ascii "Plus_var_i64_i64"
	.space 1
	.ascii "uPlus_var_i64_i64"
	.space 1
	.ascii "Plus_param_i64_I"
	.space 1
	.ascii "uPlus_param_i64_I"
	.space 1
	.ascii "Plus_var_i64_I"
	.space 1
	.ascii "uPlus_var_i64_I"
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
	.ascii "Plus_param_i64_LC"
	.space 1
	.ascii "uPlus_param_i64_LC"
	.space 1
	.ascii "Plus_var_i64_LC"
	.space 1
	.ascii "uPlus_var_i64_LC"
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
	.ascii "Plus_param_I_u32"
	.space 1
	.ascii "uPlus_param_I_u32"
	.space 1
	.ascii "Plus_var_I_u32"
	.space 1
	.ascii "uPlus_var_I_u32"
	.space 1
	.ascii "Plus_param_I_L"
	.space 1
	.ascii "uPlus_param_I_L"
	.space 1
	.ascii "Plus_var_I_L"
	.space 1
	.ascii "uPlus_var_I_L"
	.space 1
	.ascii "Plus_param_I_i8"
	.space 1
	.ascii "uPlus_param_I_i8"
	.space 1
	.ascii "Plus_var_I_i8"
	.space 1
	.ascii "uPlus_var_I_i8"
	.space 1
	.ascii "Plus_param_I_u8"
	.space 1
	.ascii "uPlus_param_I_u8"
	.space 1
	.ascii "Plus_var_I_u8"
	.space 1
	.ascii "uPlus_var_I_u8"
	.space 1
	.ascii "Plus_param_I_C"
	.space 1
	.ascii "uPlus_param_I_C"
	.space 1
	.ascii "Plus_var_I_C"
	.space 1
	.ascii "uPlus_var_I_C"
	.space 1
	.ascii "Plus_param_I_i64"
	.space 1
	.ascii "uPlus_param_I_i64"
	.space 1
	.ascii "Plus_var_I_i64"
	.space 1
	.ascii "uPlus_var_I_i64"
	.space 1
	.ascii "Plus_param_I_I"
	.space 1
	.ascii "uPlus_param_I_I"
	.space 1
	.ascii "Plus_var_I_I"
	.space 1
	.ascii "uPlus_var_I_I"
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
	.ascii "Plus_param_I_LC"
	.space 1
	.ascii "uPlus_param_I_LC"
	.space 1
	.ascii "Plus_var_I_LC"
	.space 1
	.ascii "uPlus_var_I_LC"
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
	.ascii "Plus_param_i32_u32"
	.space 1
	.ascii "uPlus_param_i32_u32"
	.space 1
	.ascii "Plus_var_i32_u32"
	.space 1
	.ascii "uPlus_var_i32_u32"
	.space 1
	.ascii "Plus_param_i32_L"
	.space 1
	.ascii "uPlus_param_i32_L"
	.space 1
	.ascii "Plus_var_i32_L"
	.space 1
	.ascii "uPlus_var_i32_L"
	.space 1
	.ascii "Plus_param_i32_i8"
	.space 1
	.ascii "uPlus_param_i32_i8"
	.space 1
	.ascii "Plus_var_i32_i8"
	.space 1
	.ascii "uPlus_var_i32_i8"
	.space 1
	.ascii "Plus_param_i32_u8"
	.space 1
	.ascii "uPlus_param_i32_u8"
	.space 1
	.ascii "Plus_var_i32_u8"
	.space 1
	.ascii "uPlus_var_i32_u8"
	.space 1
	.ascii "Plus_param_i32_C"
	.space 1
	.ascii "uPlus_param_i32_C"
	.space 1
	.ascii "Plus_var_i32_C"
	.space 1
	.ascii "uPlus_var_i32_C"
	.space 1
	.ascii "Plus_param_i32_i64"
	.space 1
	.ascii "uPlus_param_i32_i64"
	.space 1
	.ascii "Plus_var_i32_i64"
	.space 1
	.ascii "uPlus_var_i32_i64"
	.space 1
	.ascii "Plus_param_i32_I"
	.space 1
	.ascii "uPlus_param_i32_I"
	.space 1
	.ascii "Plus_var_i32_I"
	.space 1
	.ascii "uPlus_var_i32_I"
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
	.ascii "Plus_param_i32_LC"
	.space 1
	.ascii "uPlus_param_i32_LC"
	.space 1
	.ascii "Plus_var_i32_LC"
	.space 1
	.ascii "uPlus_var_i32_LC"
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
	.ascii "Plus_param_i16_u32"
	.space 1
	.ascii "uPlus_param_i16_u32"
	.space 1
	.ascii "Plus_var_i16_u32"
	.space 1
	.ascii "uPlus_var_i16_u32"
	.space 1
	.ascii "Plus_param_i16_L"
	.space 1
	.ascii "uPlus_param_i16_L"
	.space 1
	.ascii "Plus_var_i16_L"
	.space 1
	.ascii "uPlus_var_i16_L"
	.space 1
	.ascii "Plus_param_i16_i8"
	.space 1
	.ascii "uPlus_param_i16_i8"
	.space 1
	.ascii "Plus_var_i16_i8"
	.space 1
	.ascii "uPlus_var_i16_i8"
	.space 1
	.ascii "Plus_param_i16_u8"
	.space 1
	.ascii "uPlus_param_i16_u8"
	.space 1
	.ascii "Plus_var_i16_u8"
	.space 1
	.ascii "uPlus_var_i16_u8"
	.space 1
	.ascii "Plus_param_i16_C"
	.space 1
	.ascii "uPlus_param_i16_C"
	.space 1
	.ascii "Plus_var_i16_C"
	.space 1
	.ascii "uPlus_var_i16_C"
	.space 1
	.ascii "Plus_param_i16_i64"
	.space 1
	.ascii "uPlus_param_i16_i64"
	.space 1
	.ascii "Plus_var_i16_i64"
	.space 1
	.ascii "uPlus_var_i16_i64"
	.space 1
	.ascii "Plus_param_i16_I"
	.space 1
	.ascii "uPlus_param_i16_I"
	.space 1
	.ascii "Plus_var_i16_I"
	.space 1
	.ascii "uPlus_var_i16_I"
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
	.ascii "Plus_param_i16_LC"
	.space 1
	.ascii "uPlus_param_i16_LC"
	.space 1
	.ascii "Plus_var_i16_LC"
	.space 1
	.ascii "uPlus_var_i16_LC"
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
	.ascii "Plus_param_f64_f64"
	.space 1
	.ascii "Plus_var_f64_f64"
	.space 1
	.ascii "Plus_param_LC_u32"
	.space 1
	.ascii "uPlus_param_LC_u32"
	.space 1
	.ascii "Plus_var_LC_u32"
	.space 1
	.ascii "uPlus_var_LC_u32"
	.space 1
	.ascii "Plus_param_LC_L"
	.space 1
	.ascii "uPlus_param_LC_L"
	.space 1
	.ascii "Plus_var_LC_L"
	.space 1
	.ascii "uPlus_var_LC_L"
	.space 1
	.ascii "Plus_param_LC_i8"
	.space 1
	.ascii "uPlus_param_LC_i8"
	.space 1
	.ascii "Plus_var_LC_i8"
	.space 1
	.ascii "uPlus_var_LC_i8"
	.space 1
	.ascii "Plus_param_LC_u8"
	.space 1
	.ascii "uPlus_param_LC_u8"
	.space 1
	.ascii "Plus_var_LC_u8"
	.space 1
	.ascii "uPlus_var_LC_u8"
	.space 1
	.ascii "Plus_param_LC_C"
	.space 1
	.ascii "uPlus_param_LC_C"
	.space 1
	.ascii "Plus_var_LC_C"
	.space 1
	.ascii "uPlus_var_LC_C"
	.space 1
	.ascii "Plus_param_LC_i64"
	.space 1
	.ascii "uPlus_param_LC_i64"
	.space 1
	.ascii "Plus_var_LC_i64"
	.space 1
	.ascii "uPlus_var_LC_i64"
	.space 1
	.ascii "Plus_param_LC_I"
	.space 1
	.ascii "uPlus_param_LC_I"
	.space 1
	.ascii "Plus_var_LC_I"
	.space 1
	.ascii "uPlus_var_LC_I"
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
	.ascii "Plus_param_LC_LC"
	.space 1
	.ascii "uPlus_param_LC_LC"
	.space 1
	.ascii "Plus_var_LC_LC"
	.space 1
	.ascii "uPlus_var_LC_LC"
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
	.ascii "Plus_param_u64_u32"
	.space 1
	.ascii "uPlus_param_u64_u32"
	.space 1
	.ascii "Plus_var_u64_u32"
	.space 1
	.ascii "uPlus_var_u64_u32"
	.space 1
	.ascii "Plus_param_u64_L"
	.space 1
	.ascii "uPlus_param_u64_L"
	.space 1
	.ascii "Plus_var_u64_L"
	.space 1
	.ascii "uPlus_var_u64_L"
	.space 1
	.ascii "Plus_param_u64_i8"
	.space 1
	.ascii "uPlus_param_u64_i8"
	.space 1
	.ascii "Plus_var_u64_i8"
	.space 1
	.ascii "uPlus_var_u64_i8"
	.space 1
	.ascii "Plus_param_u64_u8"
	.space 1
	.ascii "uPlus_param_u64_u8"
	.space 1
	.ascii "Plus_var_u64_u8"
	.space 1
	.ascii "uPlus_var_u64_u8"
	.space 1
	.ascii "Plus_param_u64_C"
	.space 1
	.ascii "uPlus_param_u64_C"
	.space 1
	.ascii "Plus_var_u64_C"
	.space 1
	.ascii "uPlus_var_u64_C"
	.space 1
	.ascii "Plus_param_u64_i64"
	.space 1
	.ascii "uPlus_param_u64_i64"
	.space 1
	.ascii "Plus_var_u64_i64"
	.space 1
	.ascii "uPlus_var_u64_i64"
	.space 1
	.ascii "Plus_param_u64_I"
	.space 1
	.ascii "uPlus_param_u64_I"
	.space 1
	.ascii "Plus_var_u64_I"
	.space 1
	.ascii "uPlus_var_u64_I"
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
	.ascii "Plus_param_u64_LC"
	.space 1
	.ascii "uPlus_param_u64_LC"
	.space 1
	.ascii "Plus_var_u64_LC"
	.space 1
	.ascii "uPlus_var_u64_LC"
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
	.ascii "Plus_param_u16_u32"
	.space 1
	.ascii "uPlus_param_u16_u32"
	.space 1
	.ascii "Plus_var_u16_u32"
	.space 1
	.ascii "uPlus_var_u16_u32"
	.space 1
	.ascii "Plus_param_u16_L"
	.space 1
	.ascii "uPlus_param_u16_L"
	.space 1
	.ascii "Plus_var_u16_L"
	.space 1
	.ascii "uPlus_var_u16_L"
	.space 1
	.ascii "Plus_param_u16_i8"
	.space 1
	.ascii "uPlus_param_u16_i8"
	.space 1
	.ascii "Plus_var_u16_i8"
	.space 1
	.ascii "uPlus_var_u16_i8"
	.space 1
	.ascii "Plus_param_u16_u8"
	.space 1
	.ascii "uPlus_param_u16_u8"
	.space 1
	.ascii "Plus_var_u16_u8"
	.space 1
	.ascii "uPlus_var_u16_u8"
	.space 1
	.ascii "Plus_param_u16_C"
	.space 1
	.ascii "uPlus_param_u16_C"
	.space 1
	.ascii "Plus_var_u16_C"
	.space 1
	.ascii "uPlus_var_u16_C"
	.space 1
	.ascii "Plus_param_u16_i64"
	.space 1
	.ascii "uPlus_param_u16_i64"
	.space 1
	.ascii "Plus_var_u16_i64"
	.space 1
	.ascii "uPlus_var_u16_i64"
	.space 1
	.ascii "Plus_param_u16_I"
	.space 1
	.ascii "uPlus_param_u16_I"
	.space 1
	.ascii "Plus_var_u16_I"
	.space 1
	.ascii "uPlus_var_u16_I"
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
	.ascii "Plus_param_u16_LC"
	.space 1
	.ascii "uPlus_param_u16_LC"
	.space 1
	.ascii "Plus_var_u16_LC"
	.space 1
	.ascii "uPlus_var_u16_LC"
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
	.quad	_Plus_M3
	.quad	_L_1
	.quad	_Plus__Plus_param_u32_u32
	.quad	_L_1+8
	.quad	_Plus__uPlus_param_u32_u32
	.quad	_L_1+27
	.quad	_Plus__Plus_var_u32_u32
	.quad	_L_1+47
	.quad	_Plus__uPlus_var_u32_u32
	.quad	_L_1+64
	.quad	_Plus__Plus_param_u32_L
	.quad	_L_1+82
	.quad	_Plus__uPlus_param_u32_L
	.quad	_L_1+99
	.quad	_Plus__Plus_var_u32_L
	.quad	_L_1+117
	.quad	_Plus__uPlus_var_u32_L
	.quad	_L_1+132
	.quad	_Plus__Plus_param_u32_i8
	.quad	_L_1+148
	.quad	_Plus__uPlus_param_u32_i8
	.quad	_L_1+166
	.quad	_Plus__Plus_var_u32_i8
	.quad	_L_1+185
	.quad	_Plus__uPlus_var_u32_i8
	.quad	_L_1+201
	.quad	_Plus__Plus_param_u32_u8
	.quad	_L_1+218
	.quad	_Plus__uPlus_param_u32_u8
	.quad	_L_1+236
	.quad	_Plus__Plus_var_u32_u8
	.quad	_L_1+255
	.quad	_Plus__uPlus_var_u32_u8
	.quad	_L_1+271
	.quad	_Plus__Plus_param_u32_C
	.quad	_L_1+288
	.quad	_Plus__uPlus_param_u32_C
	.quad	_L_1+305
	.quad	_Plus__Plus_var_u32_C
	.quad	_L_1+323
	.quad	_Plus__uPlus_var_u32_C
	.quad	_L_1+338
	.quad	_Plus__Plus_param_u32_i64
	.quad	_L_1+354
	.quad	_Plus__uPlus_param_u32_i64
	.quad	_L_1+373
	.quad	_Plus__Plus_var_u32_i64
	.quad	_L_1+393
	.quad	_Plus__uPlus_var_u32_i64
	.quad	_L_1+410
	.quad	_Plus__Plus_param_u32_I
	.quad	_L_1+428
	.quad	_Plus__uPlus_param_u32_I
	.quad	_L_1+445
	.quad	_Plus__Plus_var_u32_I
	.quad	_L_1+463
	.quad	_Plus__uPlus_var_u32_I
	.quad	_L_1+478
	.quad	_Plus__Plus_param_u32_i32
	.quad	_L_1+494
	.quad	_Plus__uPlus_param_u32_i32
	.quad	_L_1+513
	.quad	_Plus__Plus_var_u32_i32
	.quad	_L_1+533
	.quad	_Plus__uPlus_var_u32_i32
	.quad	_L_1+550
	.quad	_Plus__Plus_param_u32_i16
	.quad	_L_1+568
	.quad	_Plus__uPlus_param_u32_i16
	.quad	_L_1+587
	.quad	_Plus__Plus_var_u32_i16
	.quad	_L_1+607
	.quad	_Plus__uPlus_var_u32_i16
	.quad	_L_1+624
	.quad	_Plus__Plus_param_u32_LC
	.quad	_L_1+642
	.quad	_Plus__uPlus_param_u32_LC
	.quad	_L_1+660
	.quad	_Plus__Plus_var_u32_LC
	.quad	_L_1+679
	.quad	_Plus__uPlus_var_u32_LC
	.quad	_L_1+695
	.quad	_Plus__Plus_param_u32_u64
	.quad	_L_1+712
	.quad	_Plus__uPlus_param_u32_u64
	.quad	_L_1+731
	.quad	_Plus__Plus_var_u32_u64
	.quad	_L_1+751
	.quad	_Plus__uPlus_var_u32_u64
	.quad	_L_1+768
	.quad	_Plus__Plus_param_u32_u16
	.quad	_L_1+786
	.quad	_Plus__uPlus_param_u32_u16
	.quad	_L_1+805
	.quad	_Plus__Plus_var_u32_u16
	.quad	_L_1+825
	.quad	_Plus__uPlus_var_u32_u16
	.quad	_L_1+842
	.quad	_Plus__Plus_param_L_u32
	.quad	_L_1+860
	.quad	_Plus__uPlus_param_L_u32
	.quad	_L_1+877
	.quad	_Plus__Plus_var_L_u32
	.quad	_L_1+895
	.quad	_Plus__uPlus_var_L_u32
	.quad	_L_1+910
	.quad	_Plus__Plus_param_L_L
	.quad	_L_1+926
	.quad	_Plus__uPlus_param_L_L
	.quad	_L_1+941
	.quad	_Plus__Plus_var_L_L
	.quad	_L_1+957
	.quad	_Plus__uPlus_var_L_L
	.quad	_L_1+970
	.quad	_Plus__Plus_param_L_i8
	.quad	_L_1+984
	.quad	_Plus__uPlus_param_L_i8
	.quad	_L_1+1000
	.quad	_Plus__Plus_var_L_i8
	.quad	_L_1+1017
	.quad	_Plus__uPlus_var_L_i8
	.quad	_L_1+1031
	.quad	_Plus__Plus_param_L_u8
	.quad	_L_1+1046
	.quad	_Plus__uPlus_param_L_u8
	.quad	_L_1+1062
	.quad	_Plus__Plus_var_L_u8
	.quad	_L_1+1079
	.quad	_Plus__uPlus_var_L_u8
	.quad	_L_1+1093
	.quad	_Plus__Plus_param_L_C
	.quad	_L_1+1108
	.quad	_Plus__uPlus_param_L_C
	.quad	_L_1+1123
	.quad	_Plus__Plus_var_L_C
	.quad	_L_1+1139
	.quad	_Plus__uPlus_var_L_C
	.quad	_L_1+1152
	.quad	_Plus__Plus_param_L_i64
	.quad	_L_1+1166
	.quad	_Plus__uPlus_param_L_i64
	.quad	_L_1+1183
	.quad	_Plus__Plus_var_L_i64
	.quad	_L_1+1201
	.quad	_Plus__uPlus_var_L_i64
	.quad	_L_1+1216
	.quad	_Plus__Plus_param_L_I
	.quad	_L_1+1232
	.quad	_Plus__uPlus_param_L_I
	.quad	_L_1+1247
	.quad	_Plus__Plus_var_L_I
	.quad	_L_1+1263
	.quad	_Plus__uPlus_var_L_I
	.quad	_L_1+1276
	.quad	_Plus__Plus_param_L_i32
	.quad	_L_1+1290
	.quad	_Plus__uPlus_param_L_i32
	.quad	_L_1+1307
	.quad	_Plus__Plus_var_L_i32
	.quad	_L_1+1325
	.quad	_Plus__uPlus_var_L_i32
	.quad	_L_1+1340
	.quad	_Plus__Plus_param_L_i16
	.quad	_L_1+1356
	.quad	_Plus__uPlus_param_L_i16
	.quad	_L_1+1373
	.quad	_Plus__Plus_var_L_i16
	.quad	_L_1+1391
	.quad	_Plus__uPlus_var_L_i16
	.quad	_L_1+1406
	.quad	_Plus__Plus_param_L_LC
	.quad	_L_1+1422
	.quad	_Plus__uPlus_param_L_LC
	.quad	_L_1+1438
	.quad	_Plus__Plus_var_L_LC
	.quad	_L_1+1455
	.quad	_Plus__uPlus_var_L_LC
	.quad	_L_1+1469
	.quad	_Plus__Plus_param_L_u64
	.quad	_L_1+1484
	.quad	_Plus__uPlus_param_L_u64
	.quad	_L_1+1501
	.quad	_Plus__Plus_var_L_u64
	.quad	_L_1+1519
	.quad	_Plus__uPlus_var_L_u64
	.quad	_L_1+1534
	.quad	_Plus__Plus_param_L_u16
	.quad	_L_1+1550
	.quad	_Plus__uPlus_param_L_u16
	.quad	_L_1+1567
	.quad	_Plus__Plus_var_L_u16
	.quad	_L_1+1585
	.quad	_Plus__uPlus_var_L_u16
	.quad	_L_1+1600
	.quad	_Plus__Plus_param_i8_u32
	.quad	_L_1+1616
	.quad	_Plus__uPlus_param_i8_u32
	.quad	_L_1+1634
	.quad	_Plus__Plus_var_i8_u32
	.quad	_L_1+1653
	.quad	_Plus__uPlus_var_i8_u32
	.quad	_L_1+1669
	.quad	_Plus__Plus_param_i8_L
	.quad	_L_1+1686
	.quad	_Plus__uPlus_param_i8_L
	.quad	_L_1+1702
	.quad	_Plus__Plus_var_i8_L
	.quad	_L_1+1719
	.quad	_Plus__uPlus_var_i8_L
	.quad	_L_1+1733
	.quad	_Plus__Plus_param_i8_i8
	.quad	_L_1+1748
	.quad	_Plus__uPlus_param_i8_i8
	.quad	_L_1+1765
	.quad	_Plus__Plus_var_i8_i8
	.quad	_L_1+1783
	.quad	_Plus__uPlus_var_i8_i8
	.quad	_L_1+1798
	.quad	_Plus__Plus_param_i8_u8
	.quad	_L_1+1814
	.quad	_Plus__uPlus_param_i8_u8
	.quad	_L_1+1831
	.quad	_Plus__Plus_var_i8_u8
	.quad	_L_1+1849
	.quad	_Plus__uPlus_var_i8_u8
	.quad	_L_1+1864
	.quad	_Plus__Plus_param_i8_C
	.quad	_L_1+1880
	.quad	_Plus__uPlus_param_i8_C
	.quad	_L_1+1896
	.quad	_Plus__Plus_var_i8_C
	.quad	_L_1+1913
	.quad	_Plus__uPlus_var_i8_C
	.quad	_L_1+1927
	.quad	_Plus__Plus_param_i8_i64
	.quad	_L_1+1942
	.quad	_Plus__uPlus_param_i8_i64
	.quad	_L_1+1960
	.quad	_Plus__Plus_var_i8_i64
	.quad	_L_1+1979
	.quad	_Plus__uPlus_var_i8_i64
	.quad	_L_1+1995
	.quad	_Plus__Plus_param_i8_I
	.quad	_L_1+2012
	.quad	_Plus__uPlus_param_i8_I
	.quad	_L_1+2028
	.quad	_Plus__Plus_var_i8_I
	.quad	_L_1+2045
	.quad	_Plus__uPlus_var_i8_I
	.quad	_L_1+2059
	.quad	_Plus__Plus_param_i8_i32
	.quad	_L_1+2074
	.quad	_Plus__uPlus_param_i8_i32
	.quad	_L_1+2092
	.quad	_Plus__Plus_var_i8_i32
	.quad	_L_1+2111
	.quad	_Plus__uPlus_var_i8_i32
	.quad	_L_1+2127
	.quad	_Plus__Plus_param_i8_i16
	.quad	_L_1+2144
	.quad	_Plus__uPlus_param_i8_i16
	.quad	_L_1+2162
	.quad	_Plus__Plus_var_i8_i16
	.quad	_L_1+2181
	.quad	_Plus__uPlus_var_i8_i16
	.quad	_L_1+2197
	.quad	_Plus__Plus_param_i8_LC
	.quad	_L_1+2214
	.quad	_Plus__uPlus_param_i8_LC
	.quad	_L_1+2231
	.quad	_Plus__Plus_var_i8_LC
	.quad	_L_1+2249
	.quad	_Plus__uPlus_var_i8_LC
	.quad	_L_1+2264
	.quad	_Plus__Plus_param_i8_u64
	.quad	_L_1+2280
	.quad	_Plus__uPlus_param_i8_u64
	.quad	_L_1+2298
	.quad	_Plus__Plus_var_i8_u64
	.quad	_L_1+2317
	.quad	_Plus__uPlus_var_i8_u64
	.quad	_L_1+2333
	.quad	_Plus__Plus_param_i8_u16
	.quad	_L_1+2350
	.quad	_Plus__uPlus_param_i8_u16
	.quad	_L_1+2368
	.quad	_Plus__Plus_var_i8_u16
	.quad	_L_1+2387
	.quad	_Plus__uPlus_var_i8_u16
	.quad	_L_1+2403
	.quad	_Plus__Plus_param_u8_u32
	.quad	_L_1+2420
	.quad	_Plus__uPlus_param_u8_u32
	.quad	_L_1+2438
	.quad	_Plus__Plus_var_u8_u32
	.quad	_L_1+2457
	.quad	_Plus__uPlus_var_u8_u32
	.quad	_L_1+2473
	.quad	_Plus__Plus_param_u8_L
	.quad	_L_1+2490
	.quad	_Plus__uPlus_param_u8_L
	.quad	_L_1+2506
	.quad	_Plus__Plus_var_u8_L
	.quad	_L_1+2523
	.quad	_Plus__uPlus_var_u8_L
	.quad	_L_1+2537
	.quad	_Plus__Plus_param_u8_i8
	.quad	_L_1+2552
	.quad	_Plus__uPlus_param_u8_i8
	.quad	_L_1+2569
	.quad	_Plus__Plus_var_u8_i8
	.quad	_L_1+2587
	.quad	_Plus__uPlus_var_u8_i8
	.quad	_L_1+2602
	.quad	_Plus__Plus_param_u8_u8
	.quad	_L_1+2618
	.quad	_Plus__uPlus_param_u8_u8
	.quad	_L_1+2635
	.quad	_Plus__Plus_var_u8_u8
	.quad	_L_1+2653
	.quad	_Plus__uPlus_var_u8_u8
	.quad	_L_1+2668
	.quad	_Plus__Plus_param_u8_C
	.quad	_L_1+2684
	.quad	_Plus__uPlus_param_u8_C
	.quad	_L_1+2700
	.quad	_Plus__Plus_var_u8_C
	.quad	_L_1+2717
	.quad	_Plus__uPlus_var_u8_C
	.quad	_L_1+2731
	.quad	_Plus__Plus_param_u8_i64
	.quad	_L_1+2746
	.quad	_Plus__uPlus_param_u8_i64
	.quad	_L_1+2764
	.quad	_Plus__Plus_var_u8_i64
	.quad	_L_1+2783
	.quad	_Plus__uPlus_var_u8_i64
	.quad	_L_1+2799
	.quad	_Plus__Plus_param_u8_I
	.quad	_L_1+2816
	.quad	_Plus__uPlus_param_u8_I
	.quad	_L_1+2832
	.quad	_Plus__Plus_var_u8_I
	.quad	_L_1+2849
	.quad	_Plus__uPlus_var_u8_I
	.quad	_L_1+2863
	.quad	_Plus__Plus_param_u8_i32
	.quad	_L_1+2878
	.quad	_Plus__uPlus_param_u8_i32
	.quad	_L_1+2896
	.quad	_Plus__Plus_var_u8_i32
	.quad	_L_1+2915
	.quad	_Plus__uPlus_var_u8_i32
	.quad	_L_1+2931
	.quad	_Plus__Plus_param_u8_i16
	.quad	_L_1+2948
	.quad	_Plus__uPlus_param_u8_i16
	.quad	_L_1+2966
	.quad	_Plus__Plus_var_u8_i16
	.quad	_L_1+2985
	.quad	_Plus__uPlus_var_u8_i16
	.quad	_L_1+3001
	.quad	_Plus__Plus_param_u8_LC
	.quad	_L_1+3018
	.quad	_Plus__uPlus_param_u8_LC
	.quad	_L_1+3035
	.quad	_Plus__Plus_var_u8_LC
	.quad	_L_1+3053
	.quad	_Plus__uPlus_var_u8_LC
	.quad	_L_1+3068
	.quad	_Plus__Plus_param_u8_u64
	.quad	_L_1+3084
	.quad	_Plus__uPlus_param_u8_u64
	.quad	_L_1+3102
	.quad	_Plus__Plus_var_u8_u64
	.quad	_L_1+3121
	.quad	_Plus__uPlus_var_u8_u64
	.quad	_L_1+3137
	.quad	_Plus__Plus_param_u8_u16
	.quad	_L_1+3154
	.quad	_Plus__uPlus_param_u8_u16
	.quad	_L_1+3172
	.quad	_Plus__Plus_var_u8_u16
	.quad	_L_1+3191
	.quad	_Plus__uPlus_var_u8_u16
	.quad	_L_1+3207
	.quad	_Plus__Plus_param_f32_f32
	.quad	_L_1+3224
	.quad	_Plus__Plus_var_f32_f32
	.quad	_L_1+3243
	.quad	_Plus__Plus_param_C_u32
	.quad	_L_1+3260
	.quad	_Plus__uPlus_param_C_u32
	.quad	_L_1+3277
	.quad	_Plus__Plus_var_C_u32
	.quad	_L_1+3295
	.quad	_Plus__uPlus_var_C_u32
	.quad	_L_1+3310
	.quad	_Plus__Plus_param_C_L
	.quad	_L_1+3326
	.quad	_Plus__uPlus_param_C_L
	.quad	_L_1+3341
	.quad	_Plus__Plus_var_C_L
	.quad	_L_1+3357
	.quad	_Plus__uPlus_var_C_L
	.quad	_L_1+3370
	.quad	_Plus__Plus_param_C_i8
	.quad	_L_1+3384
	.quad	_Plus__uPlus_param_C_i8
	.quad	_L_1+3400
	.quad	_Plus__Plus_var_C_i8
	.quad	_L_1+3417
	.quad	_Plus__uPlus_var_C_i8
	.quad	_L_1+3431
	.quad	_Plus__Plus_param_C_u8
	.quad	_L_1+3446
	.quad	_Plus__uPlus_param_C_u8
	.quad	_L_1+3462
	.quad	_Plus__Plus_var_C_u8
	.quad	_L_1+3479
	.quad	_Plus__uPlus_var_C_u8
	.quad	_L_1+3493
	.quad	_Plus__Plus_param_C_C
	.quad	_L_1+3508
	.quad	_Plus__uPlus_param_C_C
	.quad	_L_1+3523
	.quad	_Plus__Plus_var_C_C
	.quad	_L_1+3539
	.quad	_Plus__uPlus_var_C_C
	.quad	_L_1+3552
	.quad	_Plus__Plus_param_C_i64
	.quad	_L_1+3566
	.quad	_Plus__uPlus_param_C_i64
	.quad	_L_1+3583
	.quad	_Plus__Plus_var_C_i64
	.quad	_L_1+3601
	.quad	_Plus__uPlus_var_C_i64
	.quad	_L_1+3616
	.quad	_Plus__Plus_param_C_I
	.quad	_L_1+3632
	.quad	_Plus__uPlus_param_C_I
	.quad	_L_1+3647
	.quad	_Plus__Plus_var_C_I
	.quad	_L_1+3663
	.quad	_Plus__uPlus_var_C_I
	.quad	_L_1+3676
	.quad	_Plus__Plus_param_C_i32
	.quad	_L_1+3690
	.quad	_Plus__uPlus_param_C_i32
	.quad	_L_1+3707
	.quad	_Plus__Plus_var_C_i32
	.quad	_L_1+3725
	.quad	_Plus__uPlus_var_C_i32
	.quad	_L_1+3740
	.quad	_Plus__Plus_param_C_i16
	.quad	_L_1+3756
	.quad	_Plus__uPlus_param_C_i16
	.quad	_L_1+3773
	.quad	_Plus__Plus_var_C_i16
	.quad	_L_1+3791
	.quad	_Plus__uPlus_var_C_i16
	.quad	_L_1+3806
	.quad	_Plus__Plus_param_C_LC
	.quad	_L_1+3822
	.quad	_Plus__uPlus_param_C_LC
	.quad	_L_1+3838
	.quad	_Plus__Plus_var_C_LC
	.quad	_L_1+3855
	.quad	_Plus__uPlus_var_C_LC
	.quad	_L_1+3869
	.quad	_Plus__Plus_param_C_u64
	.quad	_L_1+3884
	.quad	_Plus__uPlus_param_C_u64
	.quad	_L_1+3901
	.quad	_Plus__Plus_var_C_u64
	.quad	_L_1+3919
	.quad	_Plus__uPlus_var_C_u64
	.quad	_L_1+3934
	.quad	_Plus__Plus_param_C_u16
	.quad	_L_1+3950
	.quad	_Plus__uPlus_param_C_u16
	.quad	_L_1+3967
	.quad	_Plus__Plus_var_C_u16
	.quad	_L_1+3985
	.quad	_Plus__uPlus_var_C_u16
	.quad	_L_1+4000
	.quad	_Plus__Plus_param_i64_u32
	.quad	_L_1+4016
	.quad	_Plus__uPlus_param_i64_u32
	.quad	_L_1+4035
	.quad	_Plus__Plus_var_i64_u32
	.quad	_L_1+4055
	.quad	_Plus__uPlus_var_i64_u32
	.quad	_L_1+4072
	.quad	_Plus__Plus_param_i64_L
	.quad	_L_1+4090
	.quad	_Plus__uPlus_param_i64_L
	.quad	_L_1+4107
	.quad	_Plus__Plus_var_i64_L
	.quad	_L_1+4125
	.quad	_Plus__uPlus_var_i64_L
	.quad	_L_1+4140
	.quad	_Plus__Plus_param_i64_i8
	.quad	_L_1+4156
	.quad	_Plus__uPlus_param_i64_i8
	.quad	_L_1+4174
	.quad	_Plus__Plus_var_i64_i8
	.quad	_L_1+4193
	.quad	_Plus__uPlus_var_i64_i8
	.quad	_L_1+4209
	.quad	_Plus__Plus_param_i64_u8
	.quad	_L_1+4226
	.quad	_Plus__uPlus_param_i64_u8
	.quad	_L_1+4244
	.quad	_Plus__Plus_var_i64_u8
	.quad	_L_1+4263
	.quad	_Plus__uPlus_var_i64_u8
	.quad	_L_1+4279
	.quad	_Plus__Plus_param_i64_C
	.quad	_L_1+4296
	.quad	_Plus__uPlus_param_i64_C
	.quad	_L_1+4313
	.quad	_Plus__Plus_var_i64_C
	.quad	_L_1+4331
	.quad	_Plus__uPlus_var_i64_C
	.quad	_L_1+4346
	.quad	_Plus__Plus_param_i64_i64
	.quad	_L_1+4362
	.quad	_Plus__uPlus_param_i64_i64
	.quad	_L_1+4381
	.quad	_Plus__Plus_var_i64_i64
	.quad	_L_1+4401
	.quad	_Plus__uPlus_var_i64_i64
	.quad	_L_1+4418
	.quad	_Plus__Plus_param_i64_I
	.quad	_L_1+4436
	.quad	_Plus__uPlus_param_i64_I
	.quad	_L_1+4453
	.quad	_Plus__Plus_var_i64_I
	.quad	_L_1+4471
	.quad	_Plus__uPlus_var_i64_I
	.quad	_L_1+4486
	.quad	_Plus__Plus_param_i64_i32
	.quad	_L_1+4502
	.quad	_Plus__uPlus_param_i64_i32
	.quad	_L_1+4521
	.quad	_Plus__Plus_var_i64_i32
	.quad	_L_1+4541
	.quad	_Plus__uPlus_var_i64_i32
	.quad	_L_1+4558
	.quad	_Plus__Plus_param_i64_i16
	.quad	_L_1+4576
	.quad	_Plus__uPlus_param_i64_i16
	.quad	_L_1+4595
	.quad	_Plus__Plus_var_i64_i16
	.quad	_L_1+4615
	.quad	_Plus__uPlus_var_i64_i16
	.quad	_L_1+4632
	.quad	_Plus__Plus_param_i64_LC
	.quad	_L_1+4650
	.quad	_Plus__uPlus_param_i64_LC
	.quad	_L_1+4668
	.quad	_Plus__Plus_var_i64_LC
	.quad	_L_1+4687
	.quad	_Plus__uPlus_var_i64_LC
	.quad	_L_1+4703
	.quad	_Plus__Plus_param_i64_u64
	.quad	_L_1+4720
	.quad	_Plus__uPlus_param_i64_u64
	.quad	_L_1+4739
	.quad	_Plus__Plus_var_i64_u64
	.quad	_L_1+4759
	.quad	_Plus__uPlus_var_i64_u64
	.quad	_L_1+4776
	.quad	_Plus__Plus_param_i64_u16
	.quad	_L_1+4794
	.quad	_Plus__uPlus_param_i64_u16
	.quad	_L_1+4813
	.quad	_Plus__Plus_var_i64_u16
	.quad	_L_1+4833
	.quad	_Plus__uPlus_var_i64_u16
	.quad	_L_1+4850
	.quad	_Plus__Plus_param_I_u32
	.quad	_L_1+4868
	.quad	_Plus__uPlus_param_I_u32
	.quad	_L_1+4885
	.quad	_Plus__Plus_var_I_u32
	.quad	_L_1+4903
	.quad	_Plus__uPlus_var_I_u32
	.quad	_L_1+4918
	.quad	_Plus__Plus_param_I_L
	.quad	_L_1+4934
	.quad	_Plus__uPlus_param_I_L
	.quad	_L_1+4949
	.quad	_Plus__Plus_var_I_L
	.quad	_L_1+4965
	.quad	_Plus__uPlus_var_I_L
	.quad	_L_1+4978
	.quad	_Plus__Plus_param_I_i8
	.quad	_L_1+4992
	.quad	_Plus__uPlus_param_I_i8
	.quad	_L_1+5008
	.quad	_Plus__Plus_var_I_i8
	.quad	_L_1+5025
	.quad	_Plus__uPlus_var_I_i8
	.quad	_L_1+5039
	.quad	_Plus__Plus_param_I_u8
	.quad	_L_1+5054
	.quad	_Plus__uPlus_param_I_u8
	.quad	_L_1+5070
	.quad	_Plus__Plus_var_I_u8
	.quad	_L_1+5087
	.quad	_Plus__uPlus_var_I_u8
	.quad	_L_1+5101
	.quad	_Plus__Plus_param_I_C
	.quad	_L_1+5116
	.quad	_Plus__uPlus_param_I_C
	.quad	_L_1+5131
	.quad	_Plus__Plus_var_I_C
	.quad	_L_1+5147
	.quad	_Plus__uPlus_var_I_C
	.quad	_L_1+5160
	.quad	_Plus__Plus_param_I_i64
	.quad	_L_1+5174
	.quad	_Plus__uPlus_param_I_i64
	.quad	_L_1+5191
	.quad	_Plus__Plus_var_I_i64
	.quad	_L_1+5209
	.quad	_Plus__uPlus_var_I_i64
	.quad	_L_1+5224
	.quad	_Plus__Plus_param_I_I
	.quad	_L_1+5240
	.quad	_Plus__uPlus_param_I_I
	.quad	_L_1+5255
	.quad	_Plus__Plus_var_I_I
	.quad	_L_1+5271
	.quad	_Plus__uPlus_var_I_I
	.quad	_L_1+5284
	.quad	_Plus__Plus_param_I_i32
	.quad	_L_1+5298
	.quad	_Plus__uPlus_param_I_i32
	.quad	_L_1+5315
	.quad	_Plus__Plus_var_I_i32
	.quad	_L_1+5333
	.quad	_Plus__uPlus_var_I_i32
	.quad	_L_1+5348
	.quad	_Plus__Plus_param_I_i16
	.quad	_L_1+5364
	.quad	_Plus__uPlus_param_I_i16
	.quad	_L_1+5381
	.quad	_Plus__Plus_var_I_i16
	.quad	_L_1+5399
	.quad	_Plus__uPlus_var_I_i16
	.quad	_L_1+5414
	.quad	_Plus__Plus_param_I_LC
	.quad	_L_1+5430
	.quad	_Plus__uPlus_param_I_LC
	.quad	_L_1+5446
	.quad	_Plus__Plus_var_I_LC
	.quad	_L_1+5463
	.quad	_Plus__uPlus_var_I_LC
	.quad	_L_1+5477
	.quad	_Plus__Plus_param_I_u64
	.quad	_L_1+5492
	.quad	_Plus__uPlus_param_I_u64
	.quad	_L_1+5509
	.quad	_Plus__Plus_var_I_u64
	.quad	_L_1+5527
	.quad	_Plus__uPlus_var_I_u64
	.quad	_L_1+5542
	.quad	_Plus__Plus_param_I_u16
	.quad	_L_1+5558
	.quad	_Plus__uPlus_param_I_u16
	.quad	_L_1+5575
	.quad	_Plus__Plus_var_I_u16
	.quad	_L_1+5593
	.quad	_Plus__uPlus_var_I_u16
	.quad	_L_1+5608
	.quad	_Plus__Plus_param_i32_u32
	.quad	_L_1+5624
	.quad	_Plus__uPlus_param_i32_u32
	.quad	_L_1+5643
	.quad	_Plus__Plus_var_i32_u32
	.quad	_L_1+5663
	.quad	_Plus__uPlus_var_i32_u32
	.quad	_L_1+5680
	.quad	_Plus__Plus_param_i32_L
	.quad	_L_1+5698
	.quad	_Plus__uPlus_param_i32_L
	.quad	_L_1+5715
	.quad	_Plus__Plus_var_i32_L
	.quad	_L_1+5733
	.quad	_Plus__uPlus_var_i32_L
	.quad	_L_1+5748
	.quad	_Plus__Plus_param_i32_i8
	.quad	_L_1+5764
	.quad	_Plus__uPlus_param_i32_i8
	.quad	_L_1+5782
	.quad	_Plus__Plus_var_i32_i8
	.quad	_L_1+5801
	.quad	_Plus__uPlus_var_i32_i8
	.quad	_L_1+5817
	.quad	_Plus__Plus_param_i32_u8
	.quad	_L_1+5834
	.quad	_Plus__uPlus_param_i32_u8
	.quad	_L_1+5852
	.quad	_Plus__Plus_var_i32_u8
	.quad	_L_1+5871
	.quad	_Plus__uPlus_var_i32_u8
	.quad	_L_1+5887
	.quad	_Plus__Plus_param_i32_C
	.quad	_L_1+5904
	.quad	_Plus__uPlus_param_i32_C
	.quad	_L_1+5921
	.quad	_Plus__Plus_var_i32_C
	.quad	_L_1+5939
	.quad	_Plus__uPlus_var_i32_C
	.quad	_L_1+5954
	.quad	_Plus__Plus_param_i32_i64
	.quad	_L_1+5970
	.quad	_Plus__uPlus_param_i32_i64
	.quad	_L_1+5989
	.quad	_Plus__Plus_var_i32_i64
	.quad	_L_1+6009
	.quad	_Plus__uPlus_var_i32_i64
	.quad	_L_1+6026
	.quad	_Plus__Plus_param_i32_I
	.quad	_L_1+6044
	.quad	_Plus__uPlus_param_i32_I
	.quad	_L_1+6061
	.quad	_Plus__Plus_var_i32_I
	.quad	_L_1+6079
	.quad	_Plus__uPlus_var_i32_I
	.quad	_L_1+6094
	.quad	_Plus__Plus_param_i32_i32
	.quad	_L_1+6110
	.quad	_Plus__uPlus_param_i32_i32
	.quad	_L_1+6129
	.quad	_Plus__Plus_var_i32_i32
	.quad	_L_1+6149
	.quad	_Plus__uPlus_var_i32_i32
	.quad	_L_1+6166
	.quad	_Plus__Plus_param_i32_i16
	.quad	_L_1+6184
	.quad	_Plus__uPlus_param_i32_i16
	.quad	_L_1+6203
	.quad	_Plus__Plus_var_i32_i16
	.quad	_L_1+6223
	.quad	_Plus__uPlus_var_i32_i16
	.quad	_L_1+6240
	.quad	_Plus__Plus_param_i32_LC
	.quad	_L_1+6258
	.quad	_Plus__uPlus_param_i32_LC
	.quad	_L_1+6276
	.quad	_Plus__Plus_var_i32_LC
	.quad	_L_1+6295
	.quad	_Plus__uPlus_var_i32_LC
	.quad	_L_1+6311
	.quad	_Plus__Plus_param_i32_u64
	.quad	_L_1+6328
	.quad	_Plus__uPlus_param_i32_u64
	.quad	_L_1+6347
	.quad	_Plus__Plus_var_i32_u64
	.quad	_L_1+6367
	.quad	_Plus__uPlus_var_i32_u64
	.quad	_L_1+6384
	.quad	_Plus__Plus_param_i32_u16
	.quad	_L_1+6402
	.quad	_Plus__uPlus_param_i32_u16
	.quad	_L_1+6421
	.quad	_Plus__Plus_var_i32_u16
	.quad	_L_1+6441
	.quad	_Plus__uPlus_var_i32_u16
	.quad	_L_1+6458
	.quad	_Plus__Plus_param_i16_u32
	.quad	_L_1+6476
	.quad	_Plus__uPlus_param_i16_u32
	.quad	_L_1+6495
	.quad	_Plus__Plus_var_i16_u32
	.quad	_L_1+6515
	.quad	_Plus__uPlus_var_i16_u32
	.quad	_L_1+6532
	.quad	_Plus__Plus_param_i16_L
	.quad	_L_1+6550
	.quad	_Plus__uPlus_param_i16_L
	.quad	_L_1+6567
	.quad	_Plus__Plus_var_i16_L
	.quad	_L_1+6585
	.quad	_Plus__uPlus_var_i16_L
	.quad	_L_1+6600
	.quad	_Plus__Plus_param_i16_i8
	.quad	_L_1+6616
	.quad	_Plus__uPlus_param_i16_i8
	.quad	_L_1+6634
	.quad	_Plus__Plus_var_i16_i8
	.quad	_L_1+6653
	.quad	_Plus__uPlus_var_i16_i8
	.quad	_L_1+6669
	.quad	_Plus__Plus_param_i16_u8
	.quad	_L_1+6686
	.quad	_Plus__uPlus_param_i16_u8
	.quad	_L_1+6704
	.quad	_Plus__Plus_var_i16_u8
	.quad	_L_1+6723
	.quad	_Plus__uPlus_var_i16_u8
	.quad	_L_1+6739
	.quad	_Plus__Plus_param_i16_C
	.quad	_L_1+6756
	.quad	_Plus__uPlus_param_i16_C
	.quad	_L_1+6773
	.quad	_Plus__Plus_var_i16_C
	.quad	_L_1+6791
	.quad	_Plus__uPlus_var_i16_C
	.quad	_L_1+6806
	.quad	_Plus__Plus_param_i16_i64
	.quad	_L_1+6822
	.quad	_Plus__uPlus_param_i16_i64
	.quad	_L_1+6841
	.quad	_Plus__Plus_var_i16_i64
	.quad	_L_1+6861
	.quad	_Plus__uPlus_var_i16_i64
	.quad	_L_1+6878
	.quad	_Plus__Plus_param_i16_I
	.quad	_L_1+6896
	.quad	_Plus__uPlus_param_i16_I
	.quad	_L_1+6913
	.quad	_Plus__Plus_var_i16_I
	.quad	_L_1+6931
	.quad	_Plus__uPlus_var_i16_I
	.quad	_L_1+6946
	.quad	_Plus__Plus_param_i16_i32
	.quad	_L_1+6962
	.quad	_Plus__uPlus_param_i16_i32
	.quad	_L_1+6981
	.quad	_Plus__Plus_var_i16_i32
	.quad	_L_1+7001
	.quad	_Plus__uPlus_var_i16_i32
	.quad	_L_1+7018
	.quad	_Plus__Plus_param_i16_i16
	.quad	_L_1+7036
	.quad	_Plus__uPlus_param_i16_i16
	.quad	_L_1+7055
	.quad	_Plus__Plus_var_i16_i16
	.quad	_L_1+7075
	.quad	_Plus__uPlus_var_i16_i16
	.quad	_L_1+7092
	.quad	_Plus__Plus_param_i16_LC
	.quad	_L_1+7110
	.quad	_Plus__uPlus_param_i16_LC
	.quad	_L_1+7128
	.quad	_Plus__Plus_var_i16_LC
	.quad	_L_1+7147
	.quad	_Plus__uPlus_var_i16_LC
	.quad	_L_1+7163
	.quad	_Plus__Plus_param_i16_u64
	.quad	_L_1+7180
	.quad	_Plus__uPlus_param_i16_u64
	.quad	_L_1+7199
	.quad	_Plus__Plus_var_i16_u64
	.quad	_L_1+7219
	.quad	_Plus__uPlus_var_i16_u64
	.quad	_L_1+7236
	.quad	_Plus__Plus_param_i16_u16
	.quad	_L_1+7254
	.quad	_Plus__uPlus_param_i16_u16
	.quad	_L_1+7273
	.quad	_Plus__Plus_var_i16_u16
	.quad	_L_1+7293
	.quad	_Plus__uPlus_var_i16_u16
	.quad	_L_1+7310
	.quad	_Plus__Plus_param_f64_f64
	.quad	_L_1+7328
	.quad	_Plus__Plus_var_f64_f64
	.quad	_L_1+7347
	.quad	_Plus__Plus_param_LC_u32
	.quad	_L_1+7364
	.quad	_Plus__uPlus_param_LC_u32
	.quad	_L_1+7382
	.quad	_Plus__Plus_var_LC_u32
	.quad	_L_1+7401
	.quad	_Plus__uPlus_var_LC_u32
	.quad	_L_1+7417
	.quad	_Plus__Plus_param_LC_L
	.quad	_L_1+7434
	.quad	_Plus__uPlus_param_LC_L
	.quad	_L_1+7450
	.quad	_Plus__Plus_var_LC_L
	.quad	_L_1+7467
	.quad	_Plus__uPlus_var_LC_L
	.quad	_L_1+7481
	.quad	_Plus__Plus_param_LC_i8
	.quad	_L_1+7496
	.quad	_Plus__uPlus_param_LC_i8
	.quad	_L_1+7513
	.quad	_Plus__Plus_var_LC_i8
	.quad	_L_1+7531
	.quad	_Plus__uPlus_var_LC_i8
	.quad	_L_1+7546
	.quad	_Plus__Plus_param_LC_u8
	.quad	_L_1+7562
	.quad	_Plus__uPlus_param_LC_u8
	.quad	_L_1+7579
	.quad	_Plus__Plus_var_LC_u8
	.quad	_L_1+7597
	.quad	_Plus__uPlus_var_LC_u8
	.quad	_L_1+7612
	.quad	_Plus__Plus_param_LC_C
	.quad	_L_1+7628
	.quad	_Plus__uPlus_param_LC_C
	.quad	_L_1+7644
	.quad	_Plus__Plus_var_LC_C
	.quad	_L_1+7661
	.quad	_Plus__uPlus_var_LC_C
	.quad	_L_1+7675
	.quad	_Plus__Plus_param_LC_i64
	.quad	_L_1+7690
	.quad	_Plus__uPlus_param_LC_i64
	.quad	_L_1+7708
	.quad	_Plus__Plus_var_LC_i64
	.quad	_L_1+7727
	.quad	_Plus__uPlus_var_LC_i64
	.quad	_L_1+7743
	.quad	_Plus__Plus_param_LC_I
	.quad	_L_1+7760
	.quad	_Plus__uPlus_param_LC_I
	.quad	_L_1+7776
	.quad	_Plus__Plus_var_LC_I
	.quad	_L_1+7793
	.quad	_Plus__uPlus_var_LC_I
	.quad	_L_1+7807
	.quad	_Plus__Plus_param_LC_i32
	.quad	_L_1+7822
	.quad	_Plus__uPlus_param_LC_i32
	.quad	_L_1+7840
	.quad	_Plus__Plus_var_LC_i32
	.quad	_L_1+7859
	.quad	_Plus__uPlus_var_LC_i32
	.quad	_L_1+7875
	.quad	_Plus__Plus_param_LC_i16
	.quad	_L_1+7892
	.quad	_Plus__uPlus_param_LC_i16
	.quad	_L_1+7910
	.quad	_Plus__Plus_var_LC_i16
	.quad	_L_1+7929
	.quad	_Plus__uPlus_var_LC_i16
	.quad	_L_1+7945
	.quad	_Plus__Plus_param_LC_LC
	.quad	_L_1+7962
	.quad	_Plus__uPlus_param_LC_LC
	.quad	_L_1+7979
	.quad	_Plus__Plus_var_LC_LC
	.quad	_L_1+7997
	.quad	_Plus__uPlus_var_LC_LC
	.quad	_L_1+8012
	.quad	_Plus__Plus_param_LC_u64
	.quad	_L_1+8028
	.quad	_Plus__uPlus_param_LC_u64
	.quad	_L_1+8046
	.quad	_Plus__Plus_var_LC_u64
	.quad	_L_1+8065
	.quad	_Plus__uPlus_var_LC_u64
	.quad	_L_1+8081
	.quad	_Plus__Plus_param_LC_u16
	.quad	_L_1+8098
	.quad	_Plus__uPlus_param_LC_u16
	.quad	_L_1+8116
	.quad	_Plus__Plus_var_LC_u16
	.quad	_L_1+8135
	.quad	_Plus__uPlus_var_LC_u16
	.quad	_L_1+8151
	.quad	_Plus__Plus_param_u64_u32
	.quad	_L_1+8168
	.quad	_Plus__uPlus_param_u64_u32
	.quad	_L_1+8187
	.quad	_Plus__Plus_var_u64_u32
	.quad	_L_1+8207
	.quad	_Plus__uPlus_var_u64_u32
	.quad	_L_1+8224
	.quad	_Plus__Plus_param_u64_L
	.quad	_L_1+8242
	.quad	_Plus__uPlus_param_u64_L
	.quad	_L_1+8259
	.quad	_Plus__Plus_var_u64_L
	.quad	_L_1+8277
	.quad	_Plus__uPlus_var_u64_L
	.quad	_L_1+8292
	.quad	_Plus__Plus_param_u64_i8
	.quad	_L_1+8308
	.quad	_Plus__uPlus_param_u64_i8
	.quad	_L_1+8326
	.quad	_Plus__Plus_var_u64_i8
	.quad	_L_1+8345
	.quad	_Plus__uPlus_var_u64_i8
	.quad	_L_1+8361
	.quad	_Plus__Plus_param_u64_u8
	.quad	_L_1+8378
	.quad	_Plus__uPlus_param_u64_u8
	.quad	_L_1+8396
	.quad	_Plus__Plus_var_u64_u8
	.quad	_L_1+8415
	.quad	_Plus__uPlus_var_u64_u8
	.quad	_L_1+8431
	.quad	_Plus__Plus_param_u64_C
	.quad	_L_1+8448
	.quad	_Plus__uPlus_param_u64_C
	.quad	_L_1+8465
	.quad	_Plus__Plus_var_u64_C
	.quad	_L_1+8483
	.quad	_Plus__uPlus_var_u64_C
	.quad	_L_1+8498
	.quad	_Plus__Plus_param_u64_i64
	.quad	_L_1+8514
	.quad	_Plus__uPlus_param_u64_i64
	.quad	_L_1+8533
	.quad	_Plus__Plus_var_u64_i64
	.quad	_L_1+8553
	.quad	_Plus__uPlus_var_u64_i64
	.quad	_L_1+8570
	.quad	_Plus__Plus_param_u64_I
	.quad	_L_1+8588
	.quad	_Plus__uPlus_param_u64_I
	.quad	_L_1+8605
	.quad	_Plus__Plus_var_u64_I
	.quad	_L_1+8623
	.quad	_Plus__uPlus_var_u64_I
	.quad	_L_1+8638
	.quad	_Plus__Plus_param_u64_i32
	.quad	_L_1+8654
	.quad	_Plus__uPlus_param_u64_i32
	.quad	_L_1+8673
	.quad	_Plus__Plus_var_u64_i32
	.quad	_L_1+8693
	.quad	_Plus__uPlus_var_u64_i32
	.quad	_L_1+8710
	.quad	_Plus__Plus_param_u64_i16
	.quad	_L_1+8728
	.quad	_Plus__uPlus_param_u64_i16
	.quad	_L_1+8747
	.quad	_Plus__Plus_var_u64_i16
	.quad	_L_1+8767
	.quad	_Plus__uPlus_var_u64_i16
	.quad	_L_1+8784
	.quad	_Plus__Plus_param_u64_LC
	.quad	_L_1+8802
	.quad	_Plus__uPlus_param_u64_LC
	.quad	_L_1+8820
	.quad	_Plus__Plus_var_u64_LC
	.quad	_L_1+8839
	.quad	_Plus__uPlus_var_u64_LC
	.quad	_L_1+8855
	.quad	_Plus__Plus_param_u64_u64
	.quad	_L_1+8872
	.quad	_Plus__uPlus_param_u64_u64
	.quad	_L_1+8891
	.quad	_Plus__Plus_var_u64_u64
	.quad	_L_1+8911
	.quad	_Plus__uPlus_var_u64_u64
	.quad	_L_1+8928
	.quad	_Plus__Plus_param_u64_u16
	.quad	_L_1+8946
	.quad	_Plus__uPlus_param_u64_u16
	.quad	_L_1+8965
	.quad	_Plus__Plus_var_u64_u16
	.quad	_L_1+8985
	.quad	_Plus__uPlus_var_u64_u16
	.quad	_L_1+9002
	.quad	_Plus__Plus_param_u16_u32
	.quad	_L_1+9020
	.quad	_Plus__uPlus_param_u16_u32
	.quad	_L_1+9039
	.quad	_Plus__Plus_var_u16_u32
	.quad	_L_1+9059
	.quad	_Plus__uPlus_var_u16_u32
	.quad	_L_1+9076
	.quad	_Plus__Plus_param_u16_L
	.quad	_L_1+9094
	.quad	_Plus__uPlus_param_u16_L
	.quad	_L_1+9111
	.quad	_Plus__Plus_var_u16_L
	.quad	_L_1+9129
	.quad	_Plus__uPlus_var_u16_L
	.quad	_L_1+9144
	.quad	_Plus__Plus_param_u16_i8
	.quad	_L_1+9160
	.quad	_Plus__uPlus_param_u16_i8
	.quad	_L_1+9178
	.quad	_Plus__Plus_var_u16_i8
	.quad	_L_1+9197
	.quad	_Plus__uPlus_var_u16_i8
	.quad	_L_1+9213
	.quad	_Plus__Plus_param_u16_u8
	.quad	_L_1+9230
	.quad	_Plus__uPlus_param_u16_u8
	.quad	_L_1+9248
	.quad	_Plus__Plus_var_u16_u8
	.quad	_L_1+9267
	.quad	_Plus__uPlus_var_u16_u8
	.quad	_L_1+9283
	.quad	_Plus__Plus_param_u16_C
	.quad	_L_1+9300
	.quad	_Plus__uPlus_param_u16_C
	.quad	_L_1+9317
	.quad	_Plus__Plus_var_u16_C
	.quad	_L_1+9335
	.quad	_Plus__uPlus_var_u16_C
	.quad	_L_1+9350
	.quad	_Plus__Plus_param_u16_i64
	.quad	_L_1+9366
	.quad	_Plus__uPlus_param_u16_i64
	.quad	_L_1+9385
	.quad	_Plus__Plus_var_u16_i64
	.quad	_L_1+9405
	.quad	_Plus__uPlus_var_u16_i64
	.quad	_L_1+9422
	.quad	_Plus__Plus_param_u16_I
	.quad	_L_1+9440
	.quad	_Plus__uPlus_param_u16_I
	.quad	_L_1+9457
	.quad	_Plus__Plus_var_u16_I
	.quad	_L_1+9475
	.quad	_Plus__uPlus_var_u16_I
	.quad	_L_1+9490
	.quad	_Plus__Plus_param_u16_i32
	.quad	_L_1+9506
	.quad	_Plus__uPlus_param_u16_i32
	.quad	_L_1+9525
	.quad	_Plus__Plus_var_u16_i32
	.quad	_L_1+9545
	.quad	_Plus__uPlus_var_u16_i32
	.quad	_L_1+9562
	.quad	_Plus__Plus_param_u16_i16
	.quad	_L_1+9580
	.quad	_Plus__uPlus_param_u16_i16
	.quad	_L_1+9599
	.quad	_Plus__Plus_var_u16_i16
	.quad	_L_1+9619
	.quad	_Plus__uPlus_var_u16_i16
	.quad	_L_1+9636
	.quad	_Plus__Plus_param_u16_LC
	.quad	_L_1+9654
	.quad	_Plus__uPlus_param_u16_LC
	.quad	_L_1+9672
	.quad	_Plus__Plus_var_u16_LC
	.quad	_L_1+9691
	.quad	_Plus__uPlus_var_u16_LC
	.quad	_L_1+9707
	.quad	_Plus__Plus_param_u16_u64
	.quad	_L_1+9724
	.quad	_Plus__uPlus_param_u16_u64
	.quad	_L_1+9743
	.quad	_Plus__Plus_var_u16_u64
	.quad	_L_1+9763
	.quad	_Plus__uPlus_var_u16_u64
	.quad	_L_1+9780
	.quad	_Plus__Plus_param_u16_u16
	.quad	_L_1+9798
	.quad	_Plus__uPlus_param_u16_u16
	.quad	_L_1+9817
	.quad	_Plus__Plus_var_u16_u16
	.quad	_L_1+9837
	.quad	_Plus__uPlus_var_u16_u16
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
	.word	145
	.space 6
	.quad	146
	.quad	147
	.long	2611340116
	.long	1080198340
	.word	150
	.space 2
	.long	151
	.quad	152
	.quad	153
	.quad	154
	.long	1125853168
	.byte	-99
	.byte	-98
	.space 2
	.quad	159
	.long	160
	.space 28
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
