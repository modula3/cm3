	.text
.globl _Times__uTimes_var_u16_u16
	.private_extern _Times__uTimes_var_u16_u16
_Times__uTimes_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u16
	.private_extern _Times__Times_var_u16_u16
_Times__Times_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
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
.globl _Times__uTimes_param_u16_u16
	.private_extern _Times__uTimes_param_u16_u16
_Times__uTimes_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u16
	.private_extern _Times__Times_param_u16_u16
_Times__Times_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
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
.globl _Times__uTimes_var_u16_u64
	.private_extern _Times__uTimes_var_u16_u64
_Times__uTimes_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u64
	.private_extern _Times__Times_var_u16_u64
_Times__Times_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u64
	.private_extern _Times__uTimes_param_u16_u64
_Times__uTimes_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u64
	.private_extern _Times__Times_param_u16_u64
_Times__Times_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_LC
	.private_extern _Times__uTimes_var_u16_LC
_Times__uTimes_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_LC
	.private_extern _Times__Times_var_u16_LC
_Times__Times_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_LC
	.private_extern _Times__uTimes_param_u16_LC
_Times__uTimes_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u16_LC
	.private_extern _Times__Times_param_u16_LC
_Times__Times_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i16
	.private_extern _Times__uTimes_var_u16_i16
_Times__uTimes_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i16
	.private_extern _Times__Times_var_u16_i16
_Times__Times_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L32
	cmpq	$32767, -16(%rbp)
	jle	L33
L32:
	movl	$2145, %edi
	call	__m3_fault
L33:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i16
	.private_extern _Times__uTimes_param_u16_i16
_Times__uTimes_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i16
	.private_extern _Times__Times_param_u16_i16
_Times__Times_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L38
	cmpq	$32767, -16(%rbp)
	jle	L39
L38:
	movl	$2209, %edi
	call	__m3_fault
L39:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i32
	.private_extern _Times__uTimes_var_u16_i32
_Times__uTimes_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i32
	.private_extern _Times__Times_var_u16_i32
_Times__Times_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L44
	cmpq	$32767, -16(%rbp)
	jle	L45
L44:
	movl	$2273, %edi
	call	__m3_fault
L45:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i32
	.private_extern _Times__uTimes_param_u16_i32
_Times__uTimes_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i32
	.private_extern _Times__Times_param_u16_i32
_Times__Times_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L50
	cmpq	$32767, -16(%rbp)
	jle	L51
L50:
	movl	$2337, %edi
	call	__m3_fault
L51:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_I
	.private_extern _Times__uTimes_var_u16_I
_Times__uTimes_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_I
	.private_extern _Times__Times_var_u16_I
_Times__Times_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L56
	cmpq	$32767, -16(%rbp)
	jle	L57
L56:
	movl	$2401, %edi
	call	__m3_fault
L57:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_I
	.private_extern _Times__uTimes_param_u16_I
_Times__uTimes_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_I
	.private_extern _Times__Times_param_u16_I
_Times__Times_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L62
	cmpq	$32767, -16(%rbp)
	jle	L63
L62:
	movl	$2465, %edi
	call	__m3_fault
L63:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i64
	.private_extern _Times__uTimes_var_u16_i64
_Times__uTimes_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i64
	.private_extern _Times__Times_var_u16_i64
_Times__Times_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i64
	.private_extern _Times__uTimes_param_u16_i64
_Times__uTimes_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i64
	.private_extern _Times__Times_param_u16_i64
_Times__Times_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_C
	.private_extern _Times__uTimes_var_u16_C
_Times__uTimes_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_C
	.private_extern _Times__Times_var_u16_C
_Times__Times_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L76
	cmpq	$32767, -16(%rbp)
	jle	L77
L76:
	movl	$2657, %edi
	call	__m3_fault
L77:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_C
	.private_extern _Times__uTimes_param_u16_C
_Times__uTimes_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u16_C
	.private_extern _Times__Times_param_u16_C
_Times__Times_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L82
	cmpq	$32767, -16(%rbp)
	jle	L83
L82:
	movl	$2721, %edi
	call	__m3_fault
L83:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_u8
	.private_extern _Times__uTimes_var_u16_u8
_Times__uTimes_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u8
	.private_extern _Times__Times_var_u16_u8
_Times__Times_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L88
	cmpq	$32767, -16(%rbp)
	jle	L89
L88:
	movl	$2785, %edi
	call	__m3_fault
L89:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u8
	.private_extern _Times__uTimes_param_u16_u8
_Times__uTimes_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_u8
	.private_extern _Times__Times_param_u16_u8
_Times__Times_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movsbq	-19(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L94
	cmpq	$32767, -16(%rbp)
	jle	L95
L94:
	movl	$2849, %edi
	call	__m3_fault
L95:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_i8
	.private_extern _Times__uTimes_var_u16_i8
_Times__uTimes_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_i8
	.private_extern _Times__Times_var_u16_i8
_Times__Times_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L100
	cmpq	$32767, -16(%rbp)
	jle	L101
L100:
	movl	$2913, %edi
	call	__m3_fault
L101:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_i8
	.private_extern _Times__uTimes_param_u16_i8
_Times__uTimes_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_i8
	.private_extern _Times__Times_param_u16_i8
_Times__Times_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movzbl	-19(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L106
	cmpq	$32767, -16(%rbp)
	jle	L107
L106:
	movl	$2977, %edi
	call	__m3_fault
L107:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_L
	.private_extern _Times__uTimes_var_u16_L
_Times__uTimes_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_L
	.private_extern _Times__Times_var_u16_L
_Times__Times_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u16_L
	.private_extern _Times__uTimes_param_u16_L
_Times__uTimes_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u16_L
	.private_extern _Times__Times_param_u16_L
_Times__Times_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u16_u32
	.private_extern _Times__uTimes_var_u16_u32
_Times__uTimes_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u16_u32
	.private_extern _Times__Times_var_u16_u32
_Times__Times_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L120
	cmpq	$32767, -16(%rbp)
	jle	L121
L120:
	movl	$3169, %edi
	call	__m3_fault
L121:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u16_u32
	.private_extern _Times__uTimes_param_u16_u32
_Times__uTimes_param_u16_u32:
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
.globl _Times__Times_param_u16_u32
	.private_extern _Times__Times_param_u16_u32
_Times__Times_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L126
	cmpq	$32767, -16(%rbp)
	jle	L127
L126:
	movl	$3233, %edi
	call	__m3_fault
L127:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u16
	.private_extern _Times__uTimes_var_u64_u16
_Times__uTimes_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u16
	.private_extern _Times__Times_var_u64_u16
_Times__Times_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u16
	.private_extern _Times__uTimes_param_u64_u16
_Times__uTimes_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u16
	.private_extern _Times__Times_param_u64_u16
_Times__Times_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u64
	.private_extern _Times__uTimes_var_u64_u64
_Times__uTimes_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u64
	.private_extern _Times__Times_var_u64_u64
_Times__Times_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u64
	.private_extern _Times__uTimes_param_u64_u64
_Times__uTimes_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u64
	.private_extern _Times__Times_param_u64_u64
_Times__Times_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_LC
	.private_extern _Times__uTimes_var_u64_LC
_Times__uTimes_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_LC
	.private_extern _Times__Times_var_u64_LC
_Times__Times_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_LC
	.private_extern _Times__uTimes_param_u64_LC
_Times__uTimes_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u64_LC
	.private_extern _Times__Times_param_u64_LC
_Times__Times_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i16
	.private_extern _Times__uTimes_var_u64_i16
_Times__uTimes_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i16
	.private_extern _Times__Times_var_u64_i16
_Times__Times_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i16
	.private_extern _Times__uTimes_param_u64_i16
_Times__uTimes_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i16
	.private_extern _Times__Times_param_u64_i16
_Times__Times_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i32
	.private_extern _Times__uTimes_var_u64_i32
_Times__uTimes_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i32
	.private_extern _Times__Times_var_u64_i32
_Times__Times_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i32
	.private_extern _Times__uTimes_param_u64_i32
_Times__uTimes_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i32
	.private_extern _Times__Times_param_u64_i32
_Times__Times_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_I
	.private_extern _Times__uTimes_var_u64_I
_Times__uTimes_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_I
	.private_extern _Times__Times_var_u64_I
_Times__Times_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_I
	.private_extern _Times__uTimes_param_u64_I
_Times__uTimes_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_I
	.private_extern _Times__Times_param_u64_I
_Times__Times_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i64
	.private_extern _Times__uTimes_var_u64_i64
_Times__uTimes_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i64
	.private_extern _Times__Times_var_u64_i64
_Times__Times_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i64
	.private_extern _Times__uTimes_param_u64_i64
_Times__uTimes_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i64
	.private_extern _Times__Times_param_u64_i64
_Times__Times_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_C
	.private_extern _Times__uTimes_var_u64_C
_Times__uTimes_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_C
	.private_extern _Times__Times_var_u64_C
_Times__Times_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_C
	.private_extern _Times__uTimes_param_u64_C
_Times__uTimes_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u64_C
	.private_extern _Times__Times_param_u64_C
_Times__Times_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u8
	.private_extern _Times__uTimes_var_u64_u8
_Times__uTimes_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u8
	.private_extern _Times__Times_var_u64_u8
_Times__Times_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u8
	.private_extern _Times__uTimes_param_u64_u8
_Times__uTimes_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u8
	.private_extern _Times__Times_param_u64_u8
_Times__Times_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_i8
	.private_extern _Times__uTimes_var_u64_i8
_Times__uTimes_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_i8
	.private_extern _Times__Times_var_u64_i8
_Times__Times_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_i8
	.private_extern _Times__uTimes_param_u64_i8
_Times__uTimes_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_i8
	.private_extern _Times__Times_param_u64_i8
_Times__Times_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_L
	.private_extern _Times__uTimes_var_u64_L
_Times__uTimes_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_L
	.private_extern _Times__Times_var_u64_L
_Times__Times_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_L
	.private_extern _Times__uTimes_param_u64_L
_Times__uTimes_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_L
	.private_extern _Times__Times_param_u64_L
_Times__Times_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u64_u32
	.private_extern _Times__uTimes_var_u64_u32
_Times__uTimes_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u64_u32
	.private_extern _Times__Times_var_u64_u32
_Times__Times_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u64_u32
	.private_extern _Times__uTimes_param_u64_u32
_Times__uTimes_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u64_u32
	.private_extern _Times__Times_param_u64_u32
_Times__Times_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u16
	.private_extern _Times__uTimes_var_LC_u16
_Times__uTimes_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u16
	.private_extern _Times__Times_var_LC_u16
_Times__Times_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u16
	.private_extern _Times__uTimes_param_LC_u16
_Times__uTimes_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u16
	.private_extern _Times__Times_param_LC_u16
_Times__Times_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u64
	.private_extern _Times__uTimes_var_LC_u64
_Times__uTimes_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u64
	.private_extern _Times__Times_var_LC_u64
_Times__Times_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u64
	.private_extern _Times__uTimes_param_LC_u64
_Times__uTimes_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u64
	.private_extern _Times__Times_param_LC_u64
_Times__Times_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_LC
	.private_extern _Times__uTimes_var_LC_LC
_Times__uTimes_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_LC
	.private_extern _Times__Times_var_LC_LC
_Times__Times_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_LC
	.private_extern _Times__uTimes_param_LC_LC
_Times__uTimes_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_LC
	.private_extern _Times__Times_param_LC_LC
_Times__Times_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i16
	.private_extern _Times__uTimes_var_LC_i16
_Times__uTimes_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i16
	.private_extern _Times__Times_var_LC_i16
_Times__Times_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i16
	.private_extern _Times__uTimes_param_LC_i16
_Times__uTimes_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i16
	.private_extern _Times__Times_param_LC_i16
_Times__Times_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i32
	.private_extern _Times__uTimes_var_LC_i32
_Times__uTimes_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i32
	.private_extern _Times__Times_var_LC_i32
_Times__Times_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i32
	.private_extern _Times__uTimes_param_LC_i32
_Times__uTimes_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i32
	.private_extern _Times__Times_param_LC_i32
_Times__Times_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_I
	.private_extern _Times__uTimes_var_LC_I
_Times__uTimes_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_I
	.private_extern _Times__Times_var_LC_I
_Times__Times_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_I
	.private_extern _Times__uTimes_param_LC_I
_Times__uTimes_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_I
	.private_extern _Times__Times_param_LC_I
_Times__Times_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i64
	.private_extern _Times__uTimes_var_LC_i64
_Times__uTimes_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i64
	.private_extern _Times__Times_var_LC_i64
_Times__Times_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i64
	.private_extern _Times__uTimes_param_LC_i64
_Times__uTimes_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i64
	.private_extern _Times__Times_param_LC_i64
_Times__Times_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_C
	.private_extern _Times__uTimes_var_LC_C
_Times__uTimes_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_C
	.private_extern _Times__Times_var_LC_C
_Times__Times_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_C
	.private_extern _Times__uTimes_param_LC_C
_Times__uTimes_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_C
	.private_extern _Times__Times_param_LC_C
_Times__Times_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u8
	.private_extern _Times__uTimes_var_LC_u8
_Times__uTimes_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u8
	.private_extern _Times__Times_var_LC_u8
_Times__Times_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u8
	.private_extern _Times__uTimes_param_LC_u8
_Times__uTimes_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u8
	.private_extern _Times__Times_param_LC_u8
_Times__Times_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_i8
	.private_extern _Times__uTimes_var_LC_i8
_Times__uTimes_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_i8
	.private_extern _Times__Times_var_LC_i8
_Times__Times_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_i8
	.private_extern _Times__uTimes_param_LC_i8
_Times__uTimes_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_i8
	.private_extern _Times__Times_param_LC_i8
_Times__Times_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_LC_L
	.private_extern _Times__uTimes_var_LC_L
_Times__uTimes_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_L
	.private_extern _Times__Times_var_LC_L
_Times__Times_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_L
	.private_extern _Times__uTimes_param_LC_L
_Times__uTimes_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_L
	.private_extern _Times__Times_param_LC_L
_Times__Times_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_LC_u32
	.private_extern _Times__uTimes_var_LC_u32
_Times__uTimes_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_LC_u32
	.private_extern _Times__Times_var_LC_u32
_Times__Times_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_LC_u32
	.private_extern _Times__uTimes_param_LC_u32
_Times__uTimes_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_LC_u32
	.private_extern _Times__Times_param_LC_u32
_Times__Times_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_f64_f64
	.private_extern _Times__Times_var_f64_f64
_Times__Times_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_Times(%rip), %xmm1
	movsd	128+_MM_Times(%rip), %xmm0
	mulsd	%xmm1, %xmm0
	leave
	ret
.globl _Times__Times_param_f64_f64
	.private_extern _Times__Times_param_f64_f64
_Times__Times_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm0
	mulsd	-32(%rbp), %xmm0
	leave
	ret
.globl _Times__uTimes_var_i16_u16
	.private_extern _Times__uTimes_var_i16_u16
_Times__uTimes_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u16
	.private_extern _Times__Times_var_i16_u16
_Times__Times_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L328
	cmpq	$65535, -16(%rbp)
	jle	L329
L328:
	movl	$6433, %edi
	call	__m3_fault
L329:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u16
	.private_extern _Times__uTimes_param_i16_u16
_Times__uTimes_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u16
	.private_extern _Times__Times_param_i16_u16
_Times__Times_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L334
	cmpq	$65535, -16(%rbp)
	jle	L335
L334:
	movl	$6497, %edi
	call	__m3_fault
L335:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u64
	.private_extern _Times__uTimes_var_i16_u64
_Times__uTimes_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u64
	.private_extern _Times__Times_var_i16_u64
_Times__Times_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u64
	.private_extern _Times__uTimes_param_i16_u64
_Times__uTimes_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u64
	.private_extern _Times__Times_param_i16_u64
_Times__Times_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_LC
	.private_extern _Times__uTimes_var_i16_LC
_Times__uTimes_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_LC
	.private_extern _Times__Times_var_i16_LC
_Times__Times_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_LC
	.private_extern _Times__uTimes_param_i16_LC
_Times__uTimes_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i16_LC
	.private_extern _Times__Times_param_i16_LC
_Times__Times_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i16
	.private_extern _Times__uTimes_var_i16_i16
_Times__uTimes_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i16
	.private_extern _Times__Times_var_i16_i16
_Times__Times_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L356
	cmpq	$65535, -16(%rbp)
	jle	L357
L356:
	movl	$6817, %edi
	call	__m3_fault
L357:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i16
	.private_extern _Times__uTimes_param_i16_i16
_Times__uTimes_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i16
	.private_extern _Times__Times_param_i16_i16
_Times__Times_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L362
	cmpq	$65535, -16(%rbp)
	jle	L363
L362:
	movl	$6881, %edi
	call	__m3_fault
L363:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i32
	.private_extern _Times__uTimes_var_i16_i32
_Times__uTimes_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i32
	.private_extern _Times__Times_var_i16_i32
_Times__Times_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L368
	cmpq	$65535, -16(%rbp)
	jle	L369
L368:
	movl	$6945, %edi
	call	__m3_fault
L369:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i32
	.private_extern _Times__uTimes_param_i16_i32
_Times__uTimes_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i32
	.private_extern _Times__Times_param_i16_i32
_Times__Times_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L374
	cmpq	$65535, -16(%rbp)
	jle	L375
L374:
	movl	$7009, %edi
	call	__m3_fault
L375:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_I
	.private_extern _Times__uTimes_var_i16_I
_Times__uTimes_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_I
	.private_extern _Times__Times_var_i16_I
_Times__Times_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L380
	cmpq	$65535, -16(%rbp)
	jle	L381
L380:
	movl	$7073, %edi
	call	__m3_fault
L381:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_I
	.private_extern _Times__uTimes_param_i16_I
_Times__uTimes_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_I
	.private_extern _Times__Times_param_i16_I
_Times__Times_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L386
	cmpq	$65535, -16(%rbp)
	jle	L387
L386:
	movl	$7137, %edi
	call	__m3_fault
L387:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i64
	.private_extern _Times__uTimes_var_i16_i64
_Times__uTimes_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i64
	.private_extern _Times__Times_var_i16_i64
_Times__Times_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i64
	.private_extern _Times__uTimes_param_i16_i64
_Times__uTimes_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i64
	.private_extern _Times__Times_param_i16_i64
_Times__Times_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_C
	.private_extern _Times__uTimes_var_i16_C
_Times__uTimes_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_C
	.private_extern _Times__Times_var_i16_C
_Times__Times_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L400
	cmpq	$65535, -16(%rbp)
	jle	L401
L400:
	movl	$7329, %edi
	call	__m3_fault
L401:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_C
	.private_extern _Times__uTimes_param_i16_C
_Times__uTimes_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i16_C
	.private_extern _Times__Times_param_i16_C
_Times__Times_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L406
	cmpq	$65535, -16(%rbp)
	jle	L407
L406:
	movl	$7393, %edi
	call	__m3_fault
L407:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u8
	.private_extern _Times__uTimes_var_i16_u8
_Times__uTimes_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u8
	.private_extern _Times__Times_var_i16_u8
_Times__Times_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L412
	cmpq	$65535, -16(%rbp)
	jle	L413
L412:
	movl	$7457, %edi
	call	__m3_fault
L413:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u8
	.private_extern _Times__uTimes_param_i16_u8
_Times__uTimes_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_u8
	.private_extern _Times__Times_param_i16_u8
_Times__Times_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movsbq	-19(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L418
	cmpq	$65535, -16(%rbp)
	jle	L419
L418:
	movl	$7521, %edi
	call	__m3_fault
L419:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_i8
	.private_extern _Times__uTimes_var_i16_i8
_Times__uTimes_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_i8
	.private_extern _Times__Times_var_i16_i8
_Times__Times_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L424
	cmpq	$65535, -16(%rbp)
	jle	L425
L424:
	movl	$7585, %edi
	call	__m3_fault
L425:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_i8
	.private_extern _Times__uTimes_param_i16_i8
_Times__uTimes_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_i8
	.private_extern _Times__Times_param_i16_i8
_Times__Times_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movzbl	-19(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L430
	cmpq	$65535, -16(%rbp)
	jle	L431
L430:
	movl	$7649, %edi
	call	__m3_fault
L431:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_L
	.private_extern _Times__uTimes_var_i16_L
_Times__uTimes_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_L
	.private_extern _Times__Times_var_i16_L
_Times__Times_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i16_L
	.private_extern _Times__uTimes_param_i16_L
_Times__uTimes_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i16_L
	.private_extern _Times__Times_param_i16_L
_Times__Times_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i16_u32
	.private_extern _Times__uTimes_var_i16_u32
_Times__uTimes_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i16_u32
	.private_extern _Times__Times_var_i16_u32
_Times__Times_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L444
	cmpq	$65535, -16(%rbp)
	jle	L445
L444:
	movl	$7841, %edi
	call	__m3_fault
L445:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i16_u32
	.private_extern _Times__uTimes_param_i16_u32
_Times__uTimes_param_i16_u32:
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
.globl _Times__Times_param_i16_u32
	.private_extern _Times__Times_param_i16_u32
_Times__Times_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L450
	cmpq	$65535, -16(%rbp)
	jle	L451
L450:
	movl	$7905, %edi
	call	__m3_fault
L451:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u16
	.private_extern _Times__uTimes_var_i32_u16
_Times__uTimes_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u16
	.private_extern _Times__Times_var_i32_u16
_Times__Times_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L456
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L457
L456:
	movl	$7969, %edi
	call	__m3_fault
L457:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u16
	.private_extern _Times__uTimes_param_i32_u16
_Times__uTimes_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u16
	.private_extern _Times__Times_param_i32_u16
_Times__Times_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movswq	-22(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L462
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L463
L462:
	movl	$8033, %edi
	call	__m3_fault
L463:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u64
	.private_extern _Times__uTimes_var_i32_u64
_Times__uTimes_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u64
	.private_extern _Times__Times_var_i32_u64
_Times__Times_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u64
	.private_extern _Times__uTimes_param_i32_u64
_Times__uTimes_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u64
	.private_extern _Times__Times_param_i32_u64
_Times__Times_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_LC
	.private_extern _Times__uTimes_var_i32_LC
_Times__uTimes_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_LC
	.private_extern _Times__Times_var_i32_LC
_Times__Times_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_LC
	.private_extern _Times__uTimes_param_i32_LC
_Times__uTimes_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i32_LC
	.private_extern _Times__Times_param_i32_LC
_Times__Times_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i16
	.private_extern _Times__uTimes_var_i32_i16
_Times__uTimes_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i16
	.private_extern _Times__Times_var_i32_i16
_Times__Times_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L484
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L485
L484:
	movl	$8353, %edi
	call	__m3_fault
L485:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i16
	.private_extern _Times__uTimes_param_i32_i16
_Times__uTimes_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i16
	.private_extern _Times__Times_param_i32_i16
_Times__Times_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movzwl	-22(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L490
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L491
L490:
	movl	$8417, %edi
	call	__m3_fault
L491:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i32
	.private_extern _Times__uTimes_var_i32_i32
_Times__uTimes_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i32
	.private_extern _Times__Times_var_i32_i32
_Times__Times_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L496
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L497
L496:
	movl	$8481, %edi
	call	__m3_fault
L497:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i32
	.private_extern _Times__uTimes_param_i32_i32
_Times__uTimes_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i32
	.private_extern _Times__Times_param_i32_i32
_Times__Times_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L502
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L503
L502:
	movl	$8545, %edi
	call	__m3_fault
L503:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_I
	.private_extern _Times__uTimes_var_i32_I
_Times__uTimes_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_I
	.private_extern _Times__Times_var_i32_I
_Times__Times_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L508
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L509
L508:
	movl	$8609, %edi
	call	__m3_fault
L509:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_I
	.private_extern _Times__uTimes_param_i32_I
_Times__uTimes_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_I
	.private_extern _Times__Times_param_i32_I
_Times__Times_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L514
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L515
L514:
	movl	$8673, %edi
	call	__m3_fault
L515:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i64
	.private_extern _Times__uTimes_var_i32_i64
_Times__uTimes_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i64
	.private_extern _Times__Times_var_i32_i64
_Times__Times_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i64
	.private_extern _Times__uTimes_param_i32_i64
_Times__uTimes_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i64
	.private_extern _Times__Times_param_i32_i64
_Times__Times_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_C
	.private_extern _Times__uTimes_var_i32_C
_Times__uTimes_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_C
	.private_extern _Times__Times_var_i32_C
_Times__Times_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L528
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L529
L528:
	movl	$8865, %edi
	call	__m3_fault
L529:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_C
	.private_extern _Times__uTimes_param_i32_C
_Times__uTimes_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i32_C
	.private_extern _Times__Times_param_i32_C
_Times__Times_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L534
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L535
L534:
	movl	$8929, %edi
	call	__m3_fault
L535:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u8
	.private_extern _Times__uTimes_var_i32_u8
_Times__uTimes_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u8
	.private_extern _Times__Times_var_i32_u8
_Times__Times_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L540
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L541
L540:
	movl	$8993, %edi
	call	__m3_fault
L541:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u8
	.private_extern _Times__uTimes_param_i32_u8
_Times__uTimes_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_u8
	.private_extern _Times__Times_param_i32_u8
_Times__Times_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movsbq	-21(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L546
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L547
L546:
	movl	$9057, %edi
	call	__m3_fault
L547:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_i8
	.private_extern _Times__uTimes_var_i32_i8
_Times__uTimes_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_i8
	.private_extern _Times__Times_var_i32_i8
_Times__Times_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L552
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L553
L552:
	movl	$9121, %edi
	call	__m3_fault
L553:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_i8
	.private_extern _Times__uTimes_param_i32_i8
_Times__uTimes_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_i8
	.private_extern _Times__Times_param_i32_i8
_Times__Times_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movzbl	-21(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L558
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L559
L558:
	movl	$9185, %edi
	call	__m3_fault
L559:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_L
	.private_extern _Times__uTimes_var_i32_L
_Times__uTimes_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_L
	.private_extern _Times__Times_var_i32_L
_Times__Times_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i32_L
	.private_extern _Times__uTimes_param_i32_L
_Times__uTimes_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i32_L
	.private_extern _Times__Times_param_i32_L
_Times__Times_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i32_u32
	.private_extern _Times__uTimes_var_i32_u32
_Times__uTimes_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i32_u32
	.private_extern _Times__Times_var_i32_u32
_Times__Times_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L572
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L573
L572:
	movl	$9377, %edi
	call	__m3_fault
L573:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i32_u32
	.private_extern _Times__uTimes_param_i32_u32
_Times__uTimes_param_i32_u32:
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
.globl _Times__Times_param_i32_u32
	.private_extern _Times__Times_param_i32_u32
_Times__Times_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L578
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L579
L578:
	movl	$9441, %edi
	call	__m3_fault
L579:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u16
	.private_extern _Times__uTimes_var_I_u16
_Times__uTimes_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u16
	.private_extern _Times__Times_var_I_u16
_Times__Times_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u16
	.private_extern _Times__uTimes_param_I_u16
_Times__uTimes_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u16
	.private_extern _Times__Times_param_I_u16
_Times__Times_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u64
	.private_extern _Times__uTimes_var_I_u64
_Times__uTimes_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u64
	.private_extern _Times__Times_var_I_u64
_Times__Times_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u64
	.private_extern _Times__uTimes_param_I_u64
_Times__uTimes_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u64
	.private_extern _Times__Times_param_I_u64
_Times__Times_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_LC
	.private_extern _Times__uTimes_var_I_LC
_Times__uTimes_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_LC
	.private_extern _Times__Times_var_I_LC
_Times__Times_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_LC
	.private_extern _Times__uTimes_param_I_LC
_Times__uTimes_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_I_LC
	.private_extern _Times__Times_param_I_LC
_Times__Times_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i16
	.private_extern _Times__uTimes_var_I_i16
_Times__uTimes_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i16
	.private_extern _Times__Times_var_I_i16
_Times__Times_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i16
	.private_extern _Times__uTimes_param_I_i16
_Times__uTimes_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i16
	.private_extern _Times__Times_param_I_i16
_Times__Times_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i32
	.private_extern _Times__uTimes_var_I_i32
_Times__uTimes_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i32
	.private_extern _Times__Times_var_I_i32
_Times__Times_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i32
	.private_extern _Times__uTimes_param_I_i32
_Times__uTimes_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i32
	.private_extern _Times__Times_param_I_i32
_Times__Times_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_I
	.private_extern _Times__uTimes_var_I_I
_Times__uTimes_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_I
	.private_extern _Times__Times_var_I_I
_Times__Times_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_I
	.private_extern _Times__uTimes_param_I_I
_Times__uTimes_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_I
	.private_extern _Times__Times_param_I_I
_Times__Times_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i64
	.private_extern _Times__uTimes_var_I_i64
_Times__uTimes_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i64
	.private_extern _Times__Times_var_I_i64
_Times__Times_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i64
	.private_extern _Times__uTimes_param_I_i64
_Times__uTimes_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i64
	.private_extern _Times__Times_param_I_i64
_Times__Times_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_C
	.private_extern _Times__uTimes_var_I_C
_Times__uTimes_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_C
	.private_extern _Times__Times_var_I_C
_Times__Times_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_C
	.private_extern _Times__uTimes_param_I_C
_Times__uTimes_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_I_C
	.private_extern _Times__Times_param_I_C
_Times__Times_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u8
	.private_extern _Times__uTimes_var_I_u8
_Times__uTimes_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u8
	.private_extern _Times__Times_var_I_u8
_Times__Times_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u8
	.private_extern _Times__uTimes_param_I_u8
_Times__uTimes_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u8
	.private_extern _Times__Times_param_I_u8
_Times__Times_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_i8
	.private_extern _Times__uTimes_var_I_i8
_Times__uTimes_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_i8
	.private_extern _Times__Times_var_I_i8
_Times__Times_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_i8
	.private_extern _Times__uTimes_param_I_i8
_Times__uTimes_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_i8
	.private_extern _Times__Times_param_I_i8
_Times__Times_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_L
	.private_extern _Times__uTimes_var_I_L
_Times__uTimes_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_L
	.private_extern _Times__Times_var_I_L
_Times__Times_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_L
	.private_extern _Times__uTimes_param_I_L
_Times__uTimes_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_L
	.private_extern _Times__Times_param_I_L
_Times__Times_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_I_u32
	.private_extern _Times__uTimes_var_I_u32
_Times__uTimes_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_I_u32
	.private_extern _Times__Times_var_I_u32
_Times__Times_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_I_u32
	.private_extern _Times__uTimes_param_I_u32
_Times__uTimes_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_I_u32
	.private_extern _Times__Times_param_I_u32
_Times__Times_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u16
	.private_extern _Times__uTimes_var_i64_u16
_Times__uTimes_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u16
	.private_extern _Times__Times_var_i64_u16
_Times__Times_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u16
	.private_extern _Times__uTimes_param_i64_u16
_Times__uTimes_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u16
	.private_extern _Times__Times_param_i64_u16
_Times__Times_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u64
	.private_extern _Times__uTimes_var_i64_u64
_Times__uTimes_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u64
	.private_extern _Times__Times_var_i64_u64
_Times__Times_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u64
	.private_extern _Times__uTimes_param_i64_u64
_Times__uTimes_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u64
	.private_extern _Times__Times_param_i64_u64
_Times__Times_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_LC
	.private_extern _Times__uTimes_var_i64_LC
_Times__uTimes_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_LC
	.private_extern _Times__Times_var_i64_LC
_Times__Times_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_LC
	.private_extern _Times__uTimes_param_i64_LC
_Times__uTimes_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i64_LC
	.private_extern _Times__Times_param_i64_LC
_Times__Times_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i16
	.private_extern _Times__uTimes_var_i64_i16
_Times__uTimes_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i16
	.private_extern _Times__Times_var_i64_i16
_Times__Times_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i16
	.private_extern _Times__uTimes_param_i64_i16
_Times__uTimes_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i16
	.private_extern _Times__Times_param_i64_i16
_Times__Times_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i32
	.private_extern _Times__uTimes_var_i64_i32
_Times__uTimes_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i32
	.private_extern _Times__Times_var_i64_i32
_Times__Times_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i32
	.private_extern _Times__uTimes_param_i64_i32
_Times__uTimes_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i32
	.private_extern _Times__Times_param_i64_i32
_Times__Times_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_I
	.private_extern _Times__uTimes_var_i64_I
_Times__uTimes_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_I
	.private_extern _Times__Times_var_i64_I
_Times__Times_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_I
	.private_extern _Times__uTimes_param_i64_I
_Times__uTimes_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_I
	.private_extern _Times__Times_param_i64_I
_Times__Times_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i64
	.private_extern _Times__uTimes_var_i64_i64
_Times__uTimes_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i64
	.private_extern _Times__Times_var_i64_i64
_Times__Times_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i64
	.private_extern _Times__uTimes_param_i64_i64
_Times__uTimes_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i64
	.private_extern _Times__Times_param_i64_i64
_Times__Times_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_C
	.private_extern _Times__uTimes_var_i64_C
_Times__uTimes_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_C
	.private_extern _Times__Times_var_i64_C
_Times__Times_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_C
	.private_extern _Times__uTimes_param_i64_C
_Times__uTimes_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i64_C
	.private_extern _Times__Times_param_i64_C
_Times__Times_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u8
	.private_extern _Times__uTimes_var_i64_u8
_Times__uTimes_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u8
	.private_extern _Times__Times_var_i64_u8
_Times__Times_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u8
	.private_extern _Times__uTimes_param_i64_u8
_Times__uTimes_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u8
	.private_extern _Times__Times_param_i64_u8
_Times__Times_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_i8
	.private_extern _Times__uTimes_var_i64_i8
_Times__uTimes_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_i8
	.private_extern _Times__Times_var_i64_i8
_Times__Times_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_i8
	.private_extern _Times__uTimes_param_i64_i8
_Times__uTimes_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_i8
	.private_extern _Times__Times_param_i64_i8
_Times__Times_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_L
	.private_extern _Times__uTimes_var_i64_L
_Times__uTimes_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_L
	.private_extern _Times__Times_var_i64_L
_Times__Times_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_L
	.private_extern _Times__uTimes_param_i64_L
_Times__uTimes_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_L
	.private_extern _Times__Times_param_i64_L
_Times__Times_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i64_u32
	.private_extern _Times__uTimes_var_i64_u32
_Times__uTimes_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i64_u32
	.private_extern _Times__Times_var_i64_u32
_Times__Times_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i64_u32
	.private_extern _Times__uTimes_param_i64_u32
_Times__uTimes_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i64_u32
	.private_extern _Times__Times_param_i64_u32
_Times__Times_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u16
	.private_extern _Times__uTimes_var_C_u16
_Times__uTimes_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u16
	.private_extern _Times__Times_var_C_u16
_Times__Times_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L776
	movl	$12577, %edi
	call	__m3_fault
L776:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_u16
	.private_extern _Times__uTimes_param_C_u16
_Times__uTimes_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u16
	.private_extern _Times__Times_param_C_u16
_Times__Times_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L781
	movl	$12641, %edi
	call	__m3_fault
L781:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u64
	.private_extern _Times__uTimes_var_C_u64
_Times__uTimes_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u64
	.private_extern _Times__Times_var_C_u64
_Times__Times_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_u64
	.private_extern _Times__uTimes_param_C_u64
_Times__uTimes_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u64
	.private_extern _Times__Times_param_C_u64
_Times__Times_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_LC
	.private_extern _Times__uTimes_var_C_LC
_Times__uTimes_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_LC
	.private_extern _Times__Times_var_C_LC
_Times__Times_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_LC
	.private_extern _Times__uTimes_param_C_LC
_Times__uTimes_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_LC
	.private_extern _Times__Times_param_C_LC
_Times__Times_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_C_i16
	.private_extern _Times__uTimes_var_C_i16
_Times__uTimes_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i16
	.private_extern _Times__Times_var_C_i16
_Times__Times_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L802
	movl	$12961, %edi
	call	__m3_fault
L802:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_i16
	.private_extern _Times__uTimes_param_C_i16
_Times__uTimes_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i16
	.private_extern _Times__Times_param_C_i16
_Times__Times_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L807
	movl	$13025, %edi
	call	__m3_fault
L807:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i32
	.private_extern _Times__uTimes_var_C_i32
_Times__uTimes_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i32
	.private_extern _Times__Times_var_C_i32
_Times__Times_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L812
	movl	$13089, %edi
	call	__m3_fault
L812:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_i32
	.private_extern _Times__uTimes_param_C_i32
_Times__uTimes_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i32
	.private_extern _Times__Times_param_C_i32
_Times__Times_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L817
	movl	$13153, %edi
	call	__m3_fault
L817:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_I
	.private_extern _Times__uTimes_var_C_I
_Times__uTimes_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_I
	.private_extern _Times__Times_var_C_I
_Times__Times_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L822
	movl	$13217, %edi
	call	__m3_fault
L822:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_I
	.private_extern _Times__uTimes_param_C_I
_Times__uTimes_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_I
	.private_extern _Times__Times_param_C_I
_Times__Times_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L827
	movl	$13281, %edi
	call	__m3_fault
L827:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i64
	.private_extern _Times__uTimes_var_C_i64
_Times__uTimes_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i64
	.private_extern _Times__Times_var_C_i64
_Times__Times_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_i64
	.private_extern _Times__uTimes_param_C_i64
_Times__uTimes_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i64
	.private_extern _Times__Times_param_C_i64
_Times__Times_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_C
	.private_extern _Times__uTimes_var_C_C
_Times__uTimes_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_C
	.private_extern _Times__Times_var_C_C
_Times__Times_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L840
	movl	$13473, %edi
	call	__m3_fault
L840:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_C
	.private_extern _Times__uTimes_param_C_C
_Times__uTimes_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_C
	.private_extern _Times__Times_param_C_C
_Times__Times_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L845
	movl	$13537, %edi
	call	__m3_fault
L845:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u8
	.private_extern _Times__uTimes_var_C_u8
_Times__uTimes_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u8
	.private_extern _Times__Times_var_C_u8
_Times__Times_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L850
	movl	$13601, %edi
	call	__m3_fault
L850:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_u8
	.private_extern _Times__uTimes_param_C_u8
_Times__uTimes_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u8
	.private_extern _Times__Times_param_C_u8
_Times__Times_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L855
	movl	$13665, %edi
	call	__m3_fault
L855:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_i8
	.private_extern _Times__uTimes_var_C_i8
_Times__uTimes_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_i8
	.private_extern _Times__Times_var_C_i8
_Times__Times_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L860
	movl	$13729, %edi
	call	__m3_fault
L860:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_i8
	.private_extern _Times__uTimes_param_C_i8
_Times__uTimes_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_i8
	.private_extern _Times__Times_param_C_i8
_Times__Times_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	-24(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L865
	movl	$13793, %edi
	call	__m3_fault
L865:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_L
	.private_extern _Times__uTimes_var_C_L
_Times__uTimes_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_L
	.private_extern _Times__Times_var_C_L
_Times__Times_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_C_L
	.private_extern _Times__uTimes_param_C_L
_Times__uTimes_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_L
	.private_extern _Times__Times_param_C_L
_Times__Times_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_C_u32
	.private_extern _Times__uTimes_var_C_u32
_Times__uTimes_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_C_u32
	.private_extern _Times__Times_var_C_u32
_Times__Times_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L878
	movl	$13985, %edi
	call	__m3_fault
L878:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_C_u32
	.private_extern _Times__uTimes_param_C_u32
_Times__uTimes_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_C_u32
	.private_extern _Times__Times_param_C_u32
_Times__Times_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L883
	movl	$14049, %edi
	call	__m3_fault
L883:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__Times_var_f32_f32
	.private_extern _Times__Times_var_f32_f32
_Times__Times_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_Times(%rip), %xmm1
	movss	168+_MM_Times(%rip), %xmm0
	mulss	%xmm1, %xmm0
	leave
	ret
.globl _Times__Times_param_f32_f32
	.private_extern _Times__Times_param_f32_f32
_Times__Times_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm0
	mulss	-24(%rbp), %xmm0
	leave
	ret
.globl _Times__uTimes_var_u8_u16
	.private_extern _Times__uTimes_var_u8_u16
_Times__uTimes_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u16
	.private_extern _Times__Times_var_u8_u16
_Times__Times_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L892
	cmpq	$127, -16(%rbp)
	jle	L893
L892:
	movl	$14177, %edi
	call	__m3_fault
L893:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u16
	.private_extern _Times__uTimes_param_u8_u16
_Times__uTimes_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u16
	.private_extern _Times__Times_param_u8_u16
_Times__Times_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L898
	cmpq	$127, -16(%rbp)
	jle	L899
L898:
	movl	$14241, %edi
	call	__m3_fault
L899:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u64
	.private_extern _Times__uTimes_var_u8_u64
_Times__uTimes_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u64
	.private_extern _Times__Times_var_u8_u64
_Times__Times_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u64
	.private_extern _Times__uTimes_param_u8_u64
_Times__uTimes_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u64
	.private_extern _Times__Times_param_u8_u64
_Times__Times_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_LC
	.private_extern _Times__uTimes_var_u8_LC
_Times__uTimes_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_LC
	.private_extern _Times__Times_var_u8_LC
_Times__Times_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_LC
	.private_extern _Times__uTimes_param_u8_LC
_Times__uTimes_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u8_LC
	.private_extern _Times__Times_param_u8_LC
_Times__Times_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_u8_i16
	.private_extern _Times__uTimes_var_u8_i16
_Times__uTimes_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i16
	.private_extern _Times__Times_var_u8_i16
_Times__Times_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L920
	cmpq	$127, -16(%rbp)
	jle	L921
L920:
	movl	$14561, %edi
	call	__m3_fault
L921:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i16
	.private_extern _Times__uTimes_param_u8_i16
_Times__uTimes_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i16
	.private_extern _Times__Times_param_u8_i16
_Times__Times_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L926
	cmpq	$127, -16(%rbp)
	jle	L927
L926:
	movl	$14625, %edi
	call	__m3_fault
L927:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_i32
	.private_extern _Times__uTimes_var_u8_i32
_Times__uTimes_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i32
	.private_extern _Times__Times_var_u8_i32
_Times__Times_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L932
	cmpq	$127, -16(%rbp)
	jle	L933
L932:
	movl	$14689, %edi
	call	__m3_fault
L933:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i32
	.private_extern _Times__uTimes_param_u8_i32
_Times__uTimes_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i32
	.private_extern _Times__Times_param_u8_i32
_Times__Times_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L938
	cmpq	$127, -16(%rbp)
	jle	L939
L938:
	movl	$14753, %edi
	call	__m3_fault
L939:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_I
	.private_extern _Times__uTimes_var_u8_I
_Times__uTimes_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_I
	.private_extern _Times__Times_var_u8_I
_Times__Times_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L944
	cmpq	$127, -16(%rbp)
	jle	L945
L944:
	movl	$14817, %edi
	call	__m3_fault
L945:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_I
	.private_extern _Times__uTimes_param_u8_I
_Times__uTimes_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_I
	.private_extern _Times__Times_param_u8_I
_Times__Times_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L950
	cmpq	$127, -16(%rbp)
	jle	L951
L950:
	movl	$14881, %edi
	call	__m3_fault
L951:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_i64
	.private_extern _Times__uTimes_var_u8_i64
_Times__uTimes_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i64
	.private_extern _Times__Times_var_u8_i64
_Times__Times_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i64
	.private_extern _Times__uTimes_param_u8_i64
_Times__uTimes_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i64
	.private_extern _Times__Times_param_u8_i64
_Times__Times_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_C
	.private_extern _Times__uTimes_var_u8_C
_Times__uTimes_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_C
	.private_extern _Times__Times_var_u8_C
_Times__Times_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L964
	cmpq	$127, -16(%rbp)
	jle	L965
L964:
	movl	$15073, %edi
	call	__m3_fault
L965:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_C
	.private_extern _Times__uTimes_param_u8_C
_Times__uTimes_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u8_C
	.private_extern _Times__Times_param_u8_C
_Times__Times_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L970
	cmpq	$127, -16(%rbp)
	jle	L971
L970:
	movl	$15137, %edi
	call	__m3_fault
L971:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u8
	.private_extern _Times__uTimes_var_u8_u8
_Times__uTimes_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u8
	.private_extern _Times__Times_var_u8_u8
_Times__Times_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L976
	cmpq	$127, -16(%rbp)
	jle	L977
L976:
	movl	$15201, %edi
	call	__m3_fault
L977:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u8
	.private_extern _Times__uTimes_param_u8_u8
_Times__uTimes_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_u8
	.private_extern _Times__Times_param_u8_u8
_Times__Times_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movsbq	-18(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L982
	cmpq	$127, -16(%rbp)
	jle	L983
L982:
	movl	$15265, %edi
	call	__m3_fault
L983:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_i8
	.private_extern _Times__uTimes_var_u8_i8
_Times__uTimes_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_i8
	.private_extern _Times__Times_var_u8_i8
_Times__Times_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L988
	cmpq	$127, -16(%rbp)
	jle	L989
L988:
	movl	$15329, %edi
	call	__m3_fault
L989:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_i8
	.private_extern _Times__uTimes_param_u8_i8
_Times__uTimes_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_i8
	.private_extern _Times__Times_param_u8_i8
_Times__Times_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movzbl	-18(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L994
	cmpq	$127, -16(%rbp)
	jle	L995
L994:
	movl	$15393, %edi
	call	__m3_fault
L995:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_L
	.private_extern _Times__uTimes_var_u8_L
_Times__uTimes_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_L
	.private_extern _Times__Times_var_u8_L
_Times__Times_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u8_L
	.private_extern _Times__uTimes_param_u8_L
_Times__uTimes_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u8_L
	.private_extern _Times__Times_param_u8_L
_Times__Times_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u8_u32
	.private_extern _Times__uTimes_var_u8_u32
_Times__uTimes_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u8_u32
	.private_extern _Times__Times_var_u8_u32
_Times__Times_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1008
	cmpq	$127, -16(%rbp)
	jle	L1009
L1008:
	movl	$15585, %edi
	call	__m3_fault
L1009:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u8_u32
	.private_extern _Times__uTimes_param_u8_u32
_Times__uTimes_param_u8_u32:
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
.globl _Times__Times_param_u8_u32
	.private_extern _Times__Times_param_u8_u32
_Times__Times_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1014
	cmpq	$127, -16(%rbp)
	jle	L1015
L1014:
	movl	$15649, %edi
	call	__m3_fault
L1015:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u16
	.private_extern _Times__uTimes_var_i8_u16
_Times__uTimes_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u16
	.private_extern _Times__Times_var_i8_u16
_Times__Times_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1020
	cmpq	$255, -16(%rbp)
	jle	L1021
L1020:
	movl	$15713, %edi
	call	__m3_fault
L1021:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u16
	.private_extern _Times__uTimes_param_i8_u16
_Times__uTimes_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u16
	.private_extern _Times__Times_param_i8_u16
_Times__Times_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movswq	-20(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1026
	cmpq	$255, -16(%rbp)
	jle	L1027
L1026:
	movl	$15777, %edi
	call	__m3_fault
L1027:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u64
	.private_extern _Times__uTimes_var_i8_u64
_Times__uTimes_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u64
	.private_extern _Times__Times_var_i8_u64
_Times__Times_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u64
	.private_extern _Times__uTimes_param_i8_u64
_Times__uTimes_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u64
	.private_extern _Times__Times_param_i8_u64
_Times__Times_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_LC
	.private_extern _Times__uTimes_var_i8_LC
_Times__uTimes_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_LC
	.private_extern _Times__Times_var_i8_LC
_Times__Times_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_LC
	.private_extern _Times__uTimes_param_i8_LC
_Times__uTimes_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i8_LC
	.private_extern _Times__Times_param_i8_LC
_Times__Times_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i16
	.private_extern _Times__uTimes_var_i8_i16
_Times__uTimes_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i16
	.private_extern _Times__Times_var_i8_i16
_Times__Times_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1048
	cmpq	$255, -16(%rbp)
	jle	L1049
L1048:
	movl	$16097, %edi
	call	__m3_fault
L1049:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i16
	.private_extern _Times__uTimes_param_i8_i16
_Times__uTimes_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i16
	.private_extern _Times__Times_param_i8_i16
_Times__Times_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movzwl	-20(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1054
	cmpq	$255, -16(%rbp)
	jle	L1055
L1054:
	movl	$16161, %edi
	call	__m3_fault
L1055:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i32
	.private_extern _Times__uTimes_var_i8_i32
_Times__uTimes_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i32
	.private_extern _Times__Times_var_i8_i32
_Times__Times_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1060
	cmpq	$255, -16(%rbp)
	jle	L1061
L1060:
	movl	$16225, %edi
	call	__m3_fault
L1061:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i32
	.private_extern _Times__uTimes_param_i8_i32
_Times__uTimes_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i32
	.private_extern _Times__Times_param_i8_i32
_Times__Times_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	mov	-24(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1066
	cmpq	$255, -16(%rbp)
	jle	L1067
L1066:
	movl	$16289, %edi
	call	__m3_fault
L1067:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_I
	.private_extern _Times__uTimes_var_i8_I
_Times__uTimes_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_I
	.private_extern _Times__Times_var_i8_I
_Times__Times_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1072
	cmpq	$255, -16(%rbp)
	jle	L1073
L1072:
	movl	$16353, %edi
	call	__m3_fault
L1073:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_I
	.private_extern _Times__uTimes_param_i8_I
_Times__uTimes_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_I
	.private_extern _Times__Times_param_i8_I
_Times__Times_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1078
	cmpq	$255, -16(%rbp)
	jle	L1079
L1078:
	movl	$16417, %edi
	call	__m3_fault
L1079:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i64
	.private_extern _Times__uTimes_var_i8_i64
_Times__uTimes_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i64
	.private_extern _Times__Times_var_i8_i64
_Times__Times_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i64
	.private_extern _Times__uTimes_param_i8_i64
_Times__uTimes_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i64
	.private_extern _Times__Times_param_i8_i64
_Times__Times_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_C
	.private_extern _Times__uTimes_var_i8_C
_Times__uTimes_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_C
	.private_extern _Times__Times_var_i8_C
_Times__Times_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1092
	cmpq	$255, -16(%rbp)
	jle	L1093
L1092:
	movl	$16609, %edi
	call	__m3_fault
L1093:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_C
	.private_extern _Times__uTimes_param_i8_C
_Times__uTimes_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_i8_C
	.private_extern _Times__Times_param_i8_C
_Times__Times_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	-32(%rbp), %rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1098
	cmpq	$255, -16(%rbp)
	jle	L1099
L1098:
	movl	$16673, %edi
	call	__m3_fault
L1099:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u8
	.private_extern _Times__uTimes_var_i8_u8
_Times__uTimes_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u8
	.private_extern _Times__Times_var_i8_u8
_Times__Times_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1104
	cmpq	$255, -16(%rbp)
	jle	L1105
L1104:
	movl	$16737, %edi
	call	__m3_fault
L1105:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u8
	.private_extern _Times__uTimes_param_i8_u8
_Times__uTimes_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_u8
	.private_extern _Times__Times_param_i8_u8
_Times__Times_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	-18(%rbp),%rdx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1110
	cmpq	$255, -16(%rbp)
	jle	L1111
L1110:
	movl	$16801, %edi
	call	__m3_fault
L1111:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_i8
	.private_extern _Times__uTimes_var_i8_i8
_Times__uTimes_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_i8
	.private_extern _Times__Times_var_i8_i8
_Times__Times_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1116
	cmpq	$255, -16(%rbp)
	jle	L1117
L1116:
	movl	$16865, %edi
	call	__m3_fault
L1117:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_i8
	.private_extern _Times__uTimes_param_i8_i8
_Times__uTimes_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_i8
	.private_extern _Times__Times_param_i8_i8
_Times__Times_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movzbl	-18(%rbp), %edx
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1122
	cmpq	$255, -16(%rbp)
	jle	L1123
L1122:
	movl	$16929, %edi
	call	__m3_fault
L1123:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_L
	.private_extern _Times__uTimes_var_i8_L
_Times__uTimes_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_L
	.private_extern _Times__Times_var_i8_L
_Times__Times_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_i8_L
	.private_extern _Times__uTimes_param_i8_L
_Times__uTimes_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_i8_L
	.private_extern _Times__Times_param_i8_L
_Times__Times_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_i8_u32
	.private_extern _Times__uTimes_var_i8_u32
_Times__uTimes_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_i8_u32
	.private_extern _Times__Times_var_i8_u32
_Times__Times_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1136
	cmpq	$255, -16(%rbp)
	jle	L1137
L1136:
	movl	$17121, %edi
	call	__m3_fault
L1137:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_i8_u32
	.private_extern _Times__uTimes_param_i8_u32
_Times__uTimes_param_i8_u32:
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
.globl _Times__Times_param_i8_u32
	.private_extern _Times__Times_param_i8_u32
_Times__Times_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1142
	cmpq	$255, -16(%rbp)
	jle	L1143
L1142:
	movl	$17185, %edi
	call	__m3_fault
L1143:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u16
	.private_extern _Times__uTimes_var_L_u16
_Times__uTimes_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u16
	.private_extern _Times__Times_var_L_u16
_Times__Times_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u16
	.private_extern _Times__uTimes_param_L_u16
_Times__uTimes_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u16
	.private_extern _Times__Times_param_L_u16
_Times__Times_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u64
	.private_extern _Times__uTimes_var_L_u64
_Times__uTimes_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u64
	.private_extern _Times__Times_var_L_u64
_Times__Times_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u64
	.private_extern _Times__uTimes_param_L_u64
_Times__uTimes_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u64
	.private_extern _Times__Times_param_L_u64
_Times__Times_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_LC
	.private_extern _Times__uTimes_var_L_LC
_Times__uTimes_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_LC
	.private_extern _Times__Times_var_L_LC
_Times__Times_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_LC
	.private_extern _Times__uTimes_param_L_LC
_Times__uTimes_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_L_LC
	.private_extern _Times__Times_param_L_LC
_Times__Times_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i16
	.private_extern _Times__uTimes_var_L_i16
_Times__uTimes_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i16
	.private_extern _Times__Times_var_L_i16
_Times__Times_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i16
	.private_extern _Times__uTimes_param_L_i16
_Times__uTimes_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i16
	.private_extern _Times__Times_param_L_i16
_Times__Times_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i32
	.private_extern _Times__uTimes_var_L_i32
_Times__uTimes_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i32
	.private_extern _Times__Times_var_L_i32
_Times__Times_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i32
	.private_extern _Times__uTimes_param_L_i32
_Times__uTimes_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i32
	.private_extern _Times__Times_param_L_i32
_Times__Times_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_I
	.private_extern _Times__uTimes_var_L_I
_Times__uTimes_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_I
	.private_extern _Times__Times_var_L_I
_Times__Times_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_I
	.private_extern _Times__uTimes_param_L_I
_Times__uTimes_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_I
	.private_extern _Times__Times_param_L_I
_Times__Times_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i64
	.private_extern _Times__uTimes_var_L_i64
_Times__uTimes_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i64
	.private_extern _Times__Times_var_L_i64
_Times__Times_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i64
	.private_extern _Times__uTimes_param_L_i64
_Times__uTimes_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i64
	.private_extern _Times__Times_param_L_i64
_Times__Times_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_C
	.private_extern _Times__uTimes_var_L_C
_Times__uTimes_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_C
	.private_extern _Times__Times_var_L_C
_Times__Times_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_C
	.private_extern _Times__uTimes_param_L_C
_Times__uTimes_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_L_C
	.private_extern _Times__Times_param_L_C
_Times__Times_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u8
	.private_extern _Times__uTimes_var_L_u8
_Times__uTimes_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u8
	.private_extern _Times__Times_var_L_u8
_Times__Times_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u8
	.private_extern _Times__uTimes_param_L_u8
_Times__uTimes_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u8
	.private_extern _Times__Times_param_L_u8
_Times__Times_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_i8
	.private_extern _Times__uTimes_var_L_i8
_Times__uTimes_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_i8
	.private_extern _Times__Times_var_L_i8
_Times__Times_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_i8
	.private_extern _Times__uTimes_param_L_i8
_Times__uTimes_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_i8
	.private_extern _Times__Times_param_L_i8
_Times__Times_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_L
	.private_extern _Times__uTimes_var_L_L
_Times__uTimes_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_L
	.private_extern _Times__Times_var_L_L
_Times__Times_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Times(%rip), %rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_L
	.private_extern _Times__uTimes_param_L_L
_Times__uTimes_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_L
	.private_extern _Times__Times_param_L_L
_Times__Times_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_L_u32
	.private_extern _Times__uTimes_var_L_u32
_Times__uTimes_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_L_u32
	.private_extern _Times__Times_var_L_u32
_Times__Times_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_L_u32
	.private_extern _Times__uTimes_param_L_u32
_Times__uTimes_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_L_u32
	.private_extern _Times__Times_param_L_u32
_Times__Times_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u16
	.private_extern _Times__uTimes_var_u32_u16
_Times__uTimes_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u16
	.private_extern _Times__Times_var_u32_u16
_Times__Times_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Times(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1244
	cmpq	$2147483647, -16(%rbp)
	jle	L1245
L1244:
	movl	$18785, %edi
	call	__m3_fault
L1245:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u16
	.private_extern _Times__uTimes_param_u32_u16
_Times__uTimes_param_u32_u16:
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
.globl _Times__Times_param_u32_u16
	.private_extern _Times__Times_param_u32_u16
_Times__Times_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1250
	cmpq	$2147483647, -16(%rbp)
	jle	L1251
L1250:
	movl	$18849, %edi
	call	__m3_fault
L1251:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u64
	.private_extern _Times__uTimes_var_u32_u64
_Times__uTimes_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u64
	.private_extern _Times__Times_var_u32_u64
_Times__Times_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u64
	.private_extern _Times__uTimes_param_u32_u64
_Times__uTimes_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_u64
	.private_extern _Times__Times_param_u32_u64
_Times__Times_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_LC
	.private_extern _Times__uTimes_var_u32_LC
_Times__uTimes_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_LC
	.private_extern _Times__Times_var_u32_LC
_Times__Times_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_LC
	.private_extern _Times__uTimes_param_u32_LC
_Times__uTimes_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u32_LC
	.private_extern _Times__Times_param_u32_LC
_Times__Times_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i16
	.private_extern _Times__uTimes_var_u32_i16
_Times__uTimes_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i16
	.private_extern _Times__Times_var_u32_i16
_Times__Times_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Times(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1272
	cmpq	$2147483647, -16(%rbp)
	jle	L1273
L1272:
	movl	$19169, %edi
	call	__m3_fault
L1273:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i16
	.private_extern _Times__uTimes_param_u32_i16
_Times__uTimes_param_u32_i16:
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
.globl _Times__Times_param_u32_i16
	.private_extern _Times__Times_param_u32_i16
_Times__Times_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1278
	cmpq	$2147483647, -16(%rbp)
	jle	L1279
L1278:
	movl	$19233, %edi
	call	__m3_fault
L1279:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i32
	.private_extern _Times__uTimes_var_u32_i32
_Times__uTimes_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i32
	.private_extern _Times__Times_var_u32_i32
_Times__Times_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Times(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1284
	cmpq	$2147483647, -16(%rbp)
	jle	L1285
L1284:
	movl	$19297, %edi
	call	__m3_fault
L1285:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i32
	.private_extern _Times__uTimes_param_u32_i32
_Times__uTimes_param_u32_i32:
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
.globl _Times__Times_param_u32_i32
	.private_extern _Times__Times_param_u32_i32
_Times__Times_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1290
	cmpq	$2147483647, -16(%rbp)
	jle	L1291
L1290:
	movl	$19361, %edi
	call	__m3_fault
L1291:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_I
	.private_extern _Times__uTimes_var_u32_I
_Times__uTimes_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_I
	.private_extern _Times__Times_var_u32_I
_Times__Times_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1296
	cmpq	$2147483647, -16(%rbp)
	jle	L1297
L1296:
	movl	$19425, %edi
	call	__m3_fault
L1297:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_I
	.private_extern _Times__uTimes_param_u32_I
_Times__uTimes_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_I
	.private_extern _Times__Times_param_u32_I
_Times__Times_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1302
	cmpq	$2147483647, -16(%rbp)
	jle	L1303
L1302:
	movl	$19489, %edi
	call	__m3_fault
L1303:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i64
	.private_extern _Times__uTimes_var_u32_i64
_Times__uTimes_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i64
	.private_extern _Times__Times_var_u32_i64
_Times__Times_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i64
	.private_extern _Times__uTimes_param_u32_i64
_Times__uTimes_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_i64
	.private_extern _Times__Times_param_u32_i64
_Times__Times_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_C
	.private_extern _Times__uTimes_var_u32_C
_Times__uTimes_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_C
	.private_extern _Times__Times_var_u32_C
_Times__Times_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1316
	cmpq	$2147483647, -16(%rbp)
	jle	L1317
L1316:
	movl	$19681, %edi
	call	__m3_fault
L1317:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_C
	.private_extern _Times__uTimes_param_u32_C
_Times__uTimes_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__Times_param_u32_C
	.private_extern _Times__Times_param_u32_C
_Times__Times_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1322
	cmpq	$2147483647, -16(%rbp)
	jle	L1323
L1322:
	movl	$19745, %edi
	call	__m3_fault
L1323:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u8
	.private_extern _Times__uTimes_var_u32_u8
_Times__uTimes_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u8
	.private_extern _Times__Times_var_u32_u8
_Times__Times_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Times(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1328
	cmpq	$2147483647, -16(%rbp)
	jle	L1329
L1328:
	movl	$19809, %edi
	call	__m3_fault
L1329:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u8
	.private_extern _Times__uTimes_param_u32_u8
_Times__uTimes_param_u32_u8:
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
.globl _Times__Times_param_u32_u8
	.private_extern _Times__Times_param_u32_u8
_Times__Times_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1334
	cmpq	$2147483647, -16(%rbp)
	jle	L1335
L1334:
	movl	$19873, %edi
	call	__m3_fault
L1335:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_i8
	.private_extern _Times__uTimes_var_u32_i8
_Times__uTimes_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_i8
	.private_extern _Times__Times_var_u32_i8
_Times__Times_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Times(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1340
	cmpq	$2147483647, -16(%rbp)
	jle	L1341
L1340:
	movl	$19937, %edi
	call	__m3_fault
L1341:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_i8
	.private_extern _Times__uTimes_param_u32_i8
_Times__uTimes_param_u32_i8:
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
.globl _Times__Times_param_u32_i8
	.private_extern _Times__Times_param_u32_i8
_Times__Times_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1346
	cmpq	$2147483647, -16(%rbp)
	jle	L1347
L1346:
	movl	$20001, %edi
	call	__m3_fault
L1347:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_L
	.private_extern _Times__uTimes_var_u32_L
_Times__uTimes_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_L
	.private_extern _Times__Times_var_u32_L
_Times__Times_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Times(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__uTimes_param_u32_L
	.private_extern _Times__uTimes_param_u32_L
_Times__uTimes_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_param_u32_L
	.private_extern _Times__Times_param_u32_L
_Times__Times_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_var_u32_u32
	.private_extern _Times__uTimes_var_u32_u32
_Times__uTimes_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _Times__Times_var_u32_u32
	.private_extern _Times__Times_var_u32_u32
_Times__Times_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	184+_MM_Times(%rip), %eax
	movslq	%eax,%rdx
	movl	184+_MM_Times(%rip), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1360
	cmpq	$2147483647, -16(%rbp)
	jle	L1361
L1360:
	movl	$20193, %edi
	call	__m3_fault
L1361:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times__uTimes_param_u32_u32
	.private_extern _Times__uTimes_param_u32_u32
_Times__uTimes_param_u32_u32:
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
.globl _Times__Times_param_u32_u32
	.private_extern _Times__Times_param_u32_u32
_Times__Times_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1366
	cmpq	$2147483647, -16(%rbp)
	jle	L1367
L1366:
	movl	$20257, %edi
	call	__m3_fault
L1367:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Times_M3
_Times_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Times(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Times(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Times_M3"
	.space 1
	.ascii "Times_param_u32_u32"
	.space 1
	.ascii "uTimes_param_u32_u32"
	.space 1
	.ascii "Times_var_u32_u32"
	.space 1
	.ascii "uTimes_var_u32_u32"
	.space 1
	.ascii "Times_param_u32_L"
	.space 1
	.ascii "uTimes_param_u32_L"
	.space 1
	.ascii "Times_var_u32_L"
	.space 1
	.ascii "uTimes_var_u32_L"
	.space 1
	.ascii "Times_param_u32_i8"
	.space 1
	.ascii "uTimes_param_u32_i8"
	.space 1
	.ascii "Times_var_u32_i8"
	.space 1
	.ascii "uTimes_var_u32_i8"
	.space 1
	.ascii "Times_param_u32_u8"
	.space 1
	.ascii "uTimes_param_u32_u8"
	.space 1
	.ascii "Times_var_u32_u8"
	.space 1
	.ascii "uTimes_var_u32_u8"
	.space 1
	.ascii "Times_param_u32_C"
	.space 1
	.ascii "uTimes_param_u32_C"
	.space 1
	.ascii "Times_var_u32_C"
	.space 1
	.ascii "uTimes_var_u32_C"
	.space 1
	.ascii "Times_param_u32_i64"
	.space 1
	.ascii "uTimes_param_u32_i64"
	.space 1
	.ascii "Times_var_u32_i64"
	.space 1
	.ascii "uTimes_var_u32_i64"
	.space 1
	.ascii "Times_param_u32_I"
	.space 1
	.ascii "uTimes_param_u32_I"
	.space 1
	.ascii "Times_var_u32_I"
	.space 1
	.ascii "uTimes_var_u32_I"
	.space 1
	.ascii "Times_param_u32_i32"
	.space 1
	.ascii "uTimes_param_u32_i32"
	.space 1
	.ascii "Times_var_u32_i32"
	.space 1
	.ascii "uTimes_var_u32_i32"
	.space 1
	.ascii "Times_param_u32_i16"
	.space 1
	.ascii "uTimes_param_u32_i16"
	.space 1
	.ascii "Times_var_u32_i16"
	.space 1
	.ascii "uTimes_var_u32_i16"
	.space 1
	.ascii "Times_param_u32_LC"
	.space 1
	.ascii "uTimes_param_u32_LC"
	.space 1
	.ascii "Times_var_u32_LC"
	.space 1
	.ascii "uTimes_var_u32_LC"
	.space 1
	.ascii "Times_param_u32_u64"
	.space 1
	.ascii "uTimes_param_u32_u64"
	.space 1
	.ascii "Times_var_u32_u64"
	.space 1
	.ascii "uTimes_var_u32_u64"
	.space 1
	.ascii "Times_param_u32_u16"
	.space 1
	.ascii "uTimes_param_u32_u16"
	.space 1
	.ascii "Times_var_u32_u16"
	.space 1
	.ascii "uTimes_var_u32_u16"
	.space 1
	.ascii "Times_param_L_u32"
	.space 1
	.ascii "uTimes_param_L_u32"
	.space 1
	.ascii "Times_var_L_u32"
	.space 1
	.ascii "uTimes_var_L_u32"
	.space 1
	.ascii "Times_param_L_L"
	.space 1
	.ascii "uTimes_param_L_L"
	.space 1
	.ascii "Times_var_L_L"
	.space 1
	.ascii "uTimes_var_L_L"
	.space 1
	.ascii "Times_param_L_i8"
	.space 1
	.ascii "uTimes_param_L_i8"
	.space 1
	.ascii "Times_var_L_i8"
	.space 1
	.ascii "uTimes_var_L_i8"
	.space 1
	.ascii "Times_param_L_u8"
	.space 1
	.ascii "uTimes_param_L_u8"
	.space 1
	.ascii "Times_var_L_u8"
	.space 1
	.ascii "uTimes_var_L_u8"
	.space 1
	.ascii "Times_param_L_C"
	.space 1
	.ascii "uTimes_param_L_C"
	.space 1
	.ascii "Times_var_L_C"
	.space 1
	.ascii "uTimes_var_L_C"
	.space 1
	.ascii "Times_param_L_i64"
	.space 1
	.ascii "uTimes_param_L_i64"
	.space 1
	.ascii "Times_var_L_i64"
	.space 1
	.ascii "uTimes_var_L_i64"
	.space 1
	.ascii "Times_param_L_I"
	.space 1
	.ascii "uTimes_param_L_I"
	.space 1
	.ascii "Times_var_L_I"
	.space 1
	.ascii "uTimes_var_L_I"
	.space 1
	.ascii "Times_param_L_i32"
	.space 1
	.ascii "uTimes_param_L_i32"
	.space 1
	.ascii "Times_var_L_i32"
	.space 1
	.ascii "uTimes_var_L_i32"
	.space 1
	.ascii "Times_param_L_i16"
	.space 1
	.ascii "uTimes_param_L_i16"
	.space 1
	.ascii "Times_var_L_i16"
	.space 1
	.ascii "uTimes_var_L_i16"
	.space 1
	.ascii "Times_param_L_LC"
	.space 1
	.ascii "uTimes_param_L_LC"
	.space 1
	.ascii "Times_var_L_LC"
	.space 1
	.ascii "uTimes_var_L_LC"
	.space 1
	.ascii "Times_param_L_u64"
	.space 1
	.ascii "uTimes_param_L_u64"
	.space 1
	.ascii "Times_var_L_u64"
	.space 1
	.ascii "uTimes_var_L_u64"
	.space 1
	.ascii "Times_param_L_u16"
	.space 1
	.ascii "uTimes_param_L_u16"
	.space 1
	.ascii "Times_var_L_u16"
	.space 1
	.ascii "uTimes_var_L_u16"
	.space 1
	.ascii "Times_param_i8_u32"
	.space 1
	.ascii "uTimes_param_i8_u32"
	.space 1
	.ascii "Times_var_i8_u32"
	.space 1
	.ascii "uTimes_var_i8_u32"
	.space 1
	.ascii "Times_param_i8_L"
	.space 1
	.ascii "uTimes_param_i8_L"
	.space 1
	.ascii "Times_var_i8_L"
	.space 1
	.ascii "uTimes_var_i8_L"
	.space 1
	.ascii "Times_param_i8_i8"
	.space 1
	.ascii "uTimes_param_i8_i8"
	.space 1
	.ascii "Times_var_i8_i8"
	.space 1
	.ascii "uTimes_var_i8_i8"
	.space 1
	.ascii "Times_param_i8_u8"
	.space 1
	.ascii "uTimes_param_i8_u8"
	.space 1
	.ascii "Times_var_i8_u8"
	.space 1
	.ascii "uTimes_var_i8_u8"
	.space 1
	.ascii "Times_param_i8_C"
	.space 1
	.ascii "uTimes_param_i8_C"
	.space 1
	.ascii "Times_var_i8_C"
	.space 1
	.ascii "uTimes_var_i8_C"
	.space 1
	.ascii "Times_param_i8_i64"
	.space 1
	.ascii "uTimes_param_i8_i64"
	.space 1
	.ascii "Times_var_i8_i64"
	.space 1
	.ascii "uTimes_var_i8_i64"
	.space 1
	.ascii "Times_param_i8_I"
	.space 1
	.ascii "uTimes_param_i8_I"
	.space 1
	.ascii "Times_var_i8_I"
	.space 1
	.ascii "uTimes_var_i8_I"
	.space 1
	.ascii "Times_param_i8_i32"
	.space 1
	.ascii "uTimes_param_i8_i32"
	.space 1
	.ascii "Times_var_i8_i32"
	.space 1
	.ascii "uTimes_var_i8_i32"
	.space 1
	.ascii "Times_param_i8_i16"
	.space 1
	.ascii "uTimes_param_i8_i16"
	.space 1
	.ascii "Times_var_i8_i16"
	.space 1
	.ascii "uTimes_var_i8_i16"
	.space 1
	.ascii "Times_param_i8_LC"
	.space 1
	.ascii "uTimes_param_i8_LC"
	.space 1
	.ascii "Times_var_i8_LC"
	.space 1
	.ascii "uTimes_var_i8_LC"
	.space 1
	.ascii "Times_param_i8_u64"
	.space 1
	.ascii "uTimes_param_i8_u64"
	.space 1
	.ascii "Times_var_i8_u64"
	.space 1
	.ascii "uTimes_var_i8_u64"
	.space 1
	.ascii "Times_param_i8_u16"
	.space 1
	.ascii "uTimes_param_i8_u16"
	.space 1
	.ascii "Times_var_i8_u16"
	.space 1
	.ascii "uTimes_var_i8_u16"
	.space 1
	.ascii "Times_param_u8_u32"
	.space 1
	.ascii "uTimes_param_u8_u32"
	.space 1
	.ascii "Times_var_u8_u32"
	.space 1
	.ascii "uTimes_var_u8_u32"
	.space 1
	.ascii "Times_param_u8_L"
	.space 1
	.ascii "uTimes_param_u8_L"
	.space 1
	.ascii "Times_var_u8_L"
	.space 1
	.ascii "uTimes_var_u8_L"
	.space 1
	.ascii "Times_param_u8_i8"
	.space 1
	.ascii "uTimes_param_u8_i8"
	.space 1
	.ascii "Times_var_u8_i8"
	.space 1
	.ascii "uTimes_var_u8_i8"
	.space 1
	.ascii "Times_param_u8_u8"
	.space 1
	.ascii "uTimes_param_u8_u8"
	.space 1
	.ascii "Times_var_u8_u8"
	.space 1
	.ascii "uTimes_var_u8_u8"
	.space 1
	.ascii "Times_param_u8_C"
	.space 1
	.ascii "uTimes_param_u8_C"
	.space 1
	.ascii "Times_var_u8_C"
	.space 1
	.ascii "uTimes_var_u8_C"
	.space 1
	.ascii "Times_param_u8_i64"
	.space 1
	.ascii "uTimes_param_u8_i64"
	.space 1
	.ascii "Times_var_u8_i64"
	.space 1
	.ascii "uTimes_var_u8_i64"
	.space 1
	.ascii "Times_param_u8_I"
	.space 1
	.ascii "uTimes_param_u8_I"
	.space 1
	.ascii "Times_var_u8_I"
	.space 1
	.ascii "uTimes_var_u8_I"
	.space 1
	.ascii "Times_param_u8_i32"
	.space 1
	.ascii "uTimes_param_u8_i32"
	.space 1
	.ascii "Times_var_u8_i32"
	.space 1
	.ascii "uTimes_var_u8_i32"
	.space 1
	.ascii "Times_param_u8_i16"
	.space 1
	.ascii "uTimes_param_u8_i16"
	.space 1
	.ascii "Times_var_u8_i16"
	.space 1
	.ascii "uTimes_var_u8_i16"
	.space 1
	.ascii "Times_param_u8_LC"
	.space 1
	.ascii "uTimes_param_u8_LC"
	.space 1
	.ascii "Times_var_u8_LC"
	.space 1
	.ascii "uTimes_var_u8_LC"
	.space 1
	.ascii "Times_param_u8_u64"
	.space 1
	.ascii "uTimes_param_u8_u64"
	.space 1
	.ascii "Times_var_u8_u64"
	.space 1
	.ascii "uTimes_var_u8_u64"
	.space 1
	.ascii "Times_param_u8_u16"
	.space 1
	.ascii "uTimes_param_u8_u16"
	.space 1
	.ascii "Times_var_u8_u16"
	.space 1
	.ascii "uTimes_var_u8_u16"
	.space 1
	.ascii "Times_param_f32_f32"
	.space 1
	.ascii "Times_var_f32_f32"
	.space 1
	.ascii "Times_param_C_u32"
	.space 1
	.ascii "uTimes_param_C_u32"
	.space 1
	.ascii "Times_var_C_u32"
	.space 1
	.ascii "uTimes_var_C_u32"
	.space 1
	.ascii "Times_param_C_L"
	.space 1
	.ascii "uTimes_param_C_L"
	.space 1
	.ascii "Times_var_C_L"
	.space 1
	.ascii "uTimes_var_C_L"
	.space 1
	.ascii "Times_param_C_i8"
	.space 1
	.ascii "uTimes_param_C_i8"
	.space 1
	.ascii "Times_var_C_i8"
	.space 1
	.ascii "uTimes_var_C_i8"
	.space 1
	.ascii "Times_param_C_u8"
	.space 1
	.ascii "uTimes_param_C_u8"
	.space 1
	.ascii "Times_var_C_u8"
	.space 1
	.ascii "uTimes_var_C_u8"
	.space 1
	.ascii "Times_param_C_C"
	.space 1
	.ascii "uTimes_param_C_C"
	.space 1
	.ascii "Times_var_C_C"
	.space 1
	.ascii "uTimes_var_C_C"
	.space 1
	.ascii "Times_param_C_i64"
	.space 1
	.ascii "uTimes_param_C_i64"
	.space 1
	.ascii "Times_var_C_i64"
	.space 1
	.ascii "uTimes_var_C_i64"
	.space 1
	.ascii "Times_param_C_I"
	.space 1
	.ascii "uTimes_param_C_I"
	.space 1
	.ascii "Times_var_C_I"
	.space 1
	.ascii "uTimes_var_C_I"
	.space 1
	.ascii "Times_param_C_i32"
	.space 1
	.ascii "uTimes_param_C_i32"
	.space 1
	.ascii "Times_var_C_i32"
	.space 1
	.ascii "uTimes_var_C_i32"
	.space 1
	.ascii "Times_param_C_i16"
	.space 1
	.ascii "uTimes_param_C_i16"
	.space 1
	.ascii "Times_var_C_i16"
	.space 1
	.ascii "uTimes_var_C_i16"
	.space 1
	.ascii "Times_param_C_LC"
	.space 1
	.ascii "uTimes_param_C_LC"
	.space 1
	.ascii "Times_var_C_LC"
	.space 1
	.ascii "uTimes_var_C_LC"
	.space 1
	.ascii "Times_param_C_u64"
	.space 1
	.ascii "uTimes_param_C_u64"
	.space 1
	.ascii "Times_var_C_u64"
	.space 1
	.ascii "uTimes_var_C_u64"
	.space 1
	.ascii "Times_param_C_u16"
	.space 1
	.ascii "uTimes_param_C_u16"
	.space 1
	.ascii "Times_var_C_u16"
	.space 1
	.ascii "uTimes_var_C_u16"
	.space 1
	.ascii "Times_param_i64_u32"
	.space 1
	.ascii "uTimes_param_i64_u32"
	.space 1
	.ascii "Times_var_i64_u32"
	.space 1
	.ascii "uTimes_var_i64_u32"
	.space 1
	.ascii "Times_param_i64_L"
	.space 1
	.ascii "uTimes_param_i64_L"
	.space 1
	.ascii "Times_var_i64_L"
	.space 1
	.ascii "uTimes_var_i64_L"
	.space 1
	.ascii "Times_param_i64_i8"
	.space 1
	.ascii "uTimes_param_i64_i8"
	.space 1
	.ascii "Times_var_i64_i8"
	.space 1
	.ascii "uTimes_var_i64_i8"
	.space 1
	.ascii "Times_param_i64_u8"
	.space 1
	.ascii "uTimes_param_i64_u8"
	.space 1
	.ascii "Times_var_i64_u8"
	.space 1
	.ascii "uTimes_var_i64_u8"
	.space 1
	.ascii "Times_param_i64_C"
	.space 1
	.ascii "uTimes_param_i64_C"
	.space 1
	.ascii "Times_var_i64_C"
	.space 1
	.ascii "uTimes_var_i64_C"
	.space 1
	.ascii "Times_param_i64_i64"
	.space 1
	.ascii "uTimes_param_i64_i64"
	.space 1
	.ascii "Times_var_i64_i64"
	.space 1
	.ascii "uTimes_var_i64_i64"
	.space 1
	.ascii "Times_param_i64_I"
	.space 1
	.ascii "uTimes_param_i64_I"
	.space 1
	.ascii "Times_var_i64_I"
	.space 1
	.ascii "uTimes_var_i64_I"
	.space 1
	.ascii "Times_param_i64_i32"
	.space 1
	.ascii "uTimes_param_i64_i32"
	.space 1
	.ascii "Times_var_i64_i32"
	.space 1
	.ascii "uTimes_var_i64_i32"
	.space 1
	.ascii "Times_param_i64_i16"
	.space 1
	.ascii "uTimes_param_i64_i16"
	.space 1
	.ascii "Times_var_i64_i16"
	.space 1
	.ascii "uTimes_var_i64_i16"
	.space 1
	.ascii "Times_param_i64_LC"
	.space 1
	.ascii "uTimes_param_i64_LC"
	.space 1
	.ascii "Times_var_i64_LC"
	.space 1
	.ascii "uTimes_var_i64_LC"
	.space 1
	.ascii "Times_param_i64_u64"
	.space 1
	.ascii "uTimes_param_i64_u64"
	.space 1
	.ascii "Times_var_i64_u64"
	.space 1
	.ascii "uTimes_var_i64_u64"
	.space 1
	.ascii "Times_param_i64_u16"
	.space 1
	.ascii "uTimes_param_i64_u16"
	.space 1
	.ascii "Times_var_i64_u16"
	.space 1
	.ascii "uTimes_var_i64_u16"
	.space 1
	.ascii "Times_param_I_u32"
	.space 1
	.ascii "uTimes_param_I_u32"
	.space 1
	.ascii "Times_var_I_u32"
	.space 1
	.ascii "uTimes_var_I_u32"
	.space 1
	.ascii "Times_param_I_L"
	.space 1
	.ascii "uTimes_param_I_L"
	.space 1
	.ascii "Times_var_I_L"
	.space 1
	.ascii "uTimes_var_I_L"
	.space 1
	.ascii "Times_param_I_i8"
	.space 1
	.ascii "uTimes_param_I_i8"
	.space 1
	.ascii "Times_var_I_i8"
	.space 1
	.ascii "uTimes_var_I_i8"
	.space 1
	.ascii "Times_param_I_u8"
	.space 1
	.ascii "uTimes_param_I_u8"
	.space 1
	.ascii "Times_var_I_u8"
	.space 1
	.ascii "uTimes_var_I_u8"
	.space 1
	.ascii "Times_param_I_C"
	.space 1
	.ascii "uTimes_param_I_C"
	.space 1
	.ascii "Times_var_I_C"
	.space 1
	.ascii "uTimes_var_I_C"
	.space 1
	.ascii "Times_param_I_i64"
	.space 1
	.ascii "uTimes_param_I_i64"
	.space 1
	.ascii "Times_var_I_i64"
	.space 1
	.ascii "uTimes_var_I_i64"
	.space 1
	.ascii "Times_param_I_I"
	.space 1
	.ascii "uTimes_param_I_I"
	.space 1
	.ascii "Times_var_I_I"
	.space 1
	.ascii "uTimes_var_I_I"
	.space 1
	.ascii "Times_param_I_i32"
	.space 1
	.ascii "uTimes_param_I_i32"
	.space 1
	.ascii "Times_var_I_i32"
	.space 1
	.ascii "uTimes_var_I_i32"
	.space 1
	.ascii "Times_param_I_i16"
	.space 1
	.ascii "uTimes_param_I_i16"
	.space 1
	.ascii "Times_var_I_i16"
	.space 1
	.ascii "uTimes_var_I_i16"
	.space 1
	.ascii "Times_param_I_LC"
	.space 1
	.ascii "uTimes_param_I_LC"
	.space 1
	.ascii "Times_var_I_LC"
	.space 1
	.ascii "uTimes_var_I_LC"
	.space 1
	.ascii "Times_param_I_u64"
	.space 1
	.ascii "uTimes_param_I_u64"
	.space 1
	.ascii "Times_var_I_u64"
	.space 1
	.ascii "uTimes_var_I_u64"
	.space 1
	.ascii "Times_param_I_u16"
	.space 1
	.ascii "uTimes_param_I_u16"
	.space 1
	.ascii "Times_var_I_u16"
	.space 1
	.ascii "uTimes_var_I_u16"
	.space 1
	.ascii "Times_param_i32_u32"
	.space 1
	.ascii "uTimes_param_i32_u32"
	.space 1
	.ascii "Times_var_i32_u32"
	.space 1
	.ascii "uTimes_var_i32_u32"
	.space 1
	.ascii "Times_param_i32_L"
	.space 1
	.ascii "uTimes_param_i32_L"
	.space 1
	.ascii "Times_var_i32_L"
	.space 1
	.ascii "uTimes_var_i32_L"
	.space 1
	.ascii "Times_param_i32_i8"
	.space 1
	.ascii "uTimes_param_i32_i8"
	.space 1
	.ascii "Times_var_i32_i8"
	.space 1
	.ascii "uTimes_var_i32_i8"
	.space 1
	.ascii "Times_param_i32_u8"
	.space 1
	.ascii "uTimes_param_i32_u8"
	.space 1
	.ascii "Times_var_i32_u8"
	.space 1
	.ascii "uTimes_var_i32_u8"
	.space 1
	.ascii "Times_param_i32_C"
	.space 1
	.ascii "uTimes_param_i32_C"
	.space 1
	.ascii "Times_var_i32_C"
	.space 1
	.ascii "uTimes_var_i32_C"
	.space 1
	.ascii "Times_param_i32_i64"
	.space 1
	.ascii "uTimes_param_i32_i64"
	.space 1
	.ascii "Times_var_i32_i64"
	.space 1
	.ascii "uTimes_var_i32_i64"
	.space 1
	.ascii "Times_param_i32_I"
	.space 1
	.ascii "uTimes_param_i32_I"
	.space 1
	.ascii "Times_var_i32_I"
	.space 1
	.ascii "uTimes_var_i32_I"
	.space 1
	.ascii "Times_param_i32_i32"
	.space 1
	.ascii "uTimes_param_i32_i32"
	.space 1
	.ascii "Times_var_i32_i32"
	.space 1
	.ascii "uTimes_var_i32_i32"
	.space 1
	.ascii "Times_param_i32_i16"
	.space 1
	.ascii "uTimes_param_i32_i16"
	.space 1
	.ascii "Times_var_i32_i16"
	.space 1
	.ascii "uTimes_var_i32_i16"
	.space 1
	.ascii "Times_param_i32_LC"
	.space 1
	.ascii "uTimes_param_i32_LC"
	.space 1
	.ascii "Times_var_i32_LC"
	.space 1
	.ascii "uTimes_var_i32_LC"
	.space 1
	.ascii "Times_param_i32_u64"
	.space 1
	.ascii "uTimes_param_i32_u64"
	.space 1
	.ascii "Times_var_i32_u64"
	.space 1
	.ascii "uTimes_var_i32_u64"
	.space 1
	.ascii "Times_param_i32_u16"
	.space 1
	.ascii "uTimes_param_i32_u16"
	.space 1
	.ascii "Times_var_i32_u16"
	.space 1
	.ascii "uTimes_var_i32_u16"
	.space 1
	.ascii "Times_param_i16_u32"
	.space 1
	.ascii "uTimes_param_i16_u32"
	.space 1
	.ascii "Times_var_i16_u32"
	.space 1
	.ascii "uTimes_var_i16_u32"
	.space 1
	.ascii "Times_param_i16_L"
	.space 1
	.ascii "uTimes_param_i16_L"
	.space 1
	.ascii "Times_var_i16_L"
	.space 1
	.ascii "uTimes_var_i16_L"
	.space 1
	.ascii "Times_param_i16_i8"
	.space 1
	.ascii "uTimes_param_i16_i8"
	.space 1
	.ascii "Times_var_i16_i8"
	.space 1
	.ascii "uTimes_var_i16_i8"
	.space 1
	.ascii "Times_param_i16_u8"
	.space 1
	.ascii "uTimes_param_i16_u8"
	.space 1
	.ascii "Times_var_i16_u8"
	.space 1
	.ascii "uTimes_var_i16_u8"
	.space 1
	.ascii "Times_param_i16_C"
	.space 1
	.ascii "uTimes_param_i16_C"
	.space 1
	.ascii "Times_var_i16_C"
	.space 1
	.ascii "uTimes_var_i16_C"
	.space 1
	.ascii "Times_param_i16_i64"
	.space 1
	.ascii "uTimes_param_i16_i64"
	.space 1
	.ascii "Times_var_i16_i64"
	.space 1
	.ascii "uTimes_var_i16_i64"
	.space 1
	.ascii "Times_param_i16_I"
	.space 1
	.ascii "uTimes_param_i16_I"
	.space 1
	.ascii "Times_var_i16_I"
	.space 1
	.ascii "uTimes_var_i16_I"
	.space 1
	.ascii "Times_param_i16_i32"
	.space 1
	.ascii "uTimes_param_i16_i32"
	.space 1
	.ascii "Times_var_i16_i32"
	.space 1
	.ascii "uTimes_var_i16_i32"
	.space 1
	.ascii "Times_param_i16_i16"
	.space 1
	.ascii "uTimes_param_i16_i16"
	.space 1
	.ascii "Times_var_i16_i16"
	.space 1
	.ascii "uTimes_var_i16_i16"
	.space 1
	.ascii "Times_param_i16_LC"
	.space 1
	.ascii "uTimes_param_i16_LC"
	.space 1
	.ascii "Times_var_i16_LC"
	.space 1
	.ascii "uTimes_var_i16_LC"
	.space 1
	.ascii "Times_param_i16_u64"
	.space 1
	.ascii "uTimes_param_i16_u64"
	.space 1
	.ascii "Times_var_i16_u64"
	.space 1
	.ascii "uTimes_var_i16_u64"
	.space 1
	.ascii "Times_param_i16_u16"
	.space 1
	.ascii "uTimes_param_i16_u16"
	.space 1
	.ascii "Times_var_i16_u16"
	.space 1
	.ascii "uTimes_var_i16_u16"
	.space 1
	.ascii "Times_param_f64_f64"
	.space 1
	.ascii "Times_var_f64_f64"
	.space 1
	.ascii "Times_param_LC_u32"
	.space 1
	.ascii "uTimes_param_LC_u32"
	.space 1
	.ascii "Times_var_LC_u32"
	.space 1
	.ascii "uTimes_var_LC_u32"
	.space 1
	.ascii "Times_param_LC_L"
	.space 1
	.ascii "uTimes_param_LC_L"
	.space 1
	.ascii "Times_var_LC_L"
	.space 1
	.ascii "uTimes_var_LC_L"
	.space 1
	.ascii "Times_param_LC_i8"
	.space 1
	.ascii "uTimes_param_LC_i8"
	.space 1
	.ascii "Times_var_LC_i8"
	.space 1
	.ascii "uTimes_var_LC_i8"
	.space 1
	.ascii "Times_param_LC_u8"
	.space 1
	.ascii "uTimes_param_LC_u8"
	.space 1
	.ascii "Times_var_LC_u8"
	.space 1
	.ascii "uTimes_var_LC_u8"
	.space 1
	.ascii "Times_param_LC_C"
	.space 1
	.ascii "uTimes_param_LC_C"
	.space 1
	.ascii "Times_var_LC_C"
	.space 1
	.ascii "uTimes_var_LC_C"
	.space 1
	.ascii "Times_param_LC_i64"
	.space 1
	.ascii "uTimes_param_LC_i64"
	.space 1
	.ascii "Times_var_LC_i64"
	.space 1
	.ascii "uTimes_var_LC_i64"
	.space 1
	.ascii "Times_param_LC_I"
	.space 1
	.ascii "uTimes_param_LC_I"
	.space 1
	.ascii "Times_var_LC_I"
	.space 1
	.ascii "uTimes_var_LC_I"
	.space 1
	.ascii "Times_param_LC_i32"
	.space 1
	.ascii "uTimes_param_LC_i32"
	.space 1
	.ascii "Times_var_LC_i32"
	.space 1
	.ascii "uTimes_var_LC_i32"
	.space 1
	.ascii "Times_param_LC_i16"
	.space 1
	.ascii "uTimes_param_LC_i16"
	.space 1
	.ascii "Times_var_LC_i16"
	.space 1
	.ascii "uTimes_var_LC_i16"
	.space 1
	.ascii "Times_param_LC_LC"
	.space 1
	.ascii "uTimes_param_LC_LC"
	.space 1
	.ascii "Times_var_LC_LC"
	.space 1
	.ascii "uTimes_var_LC_LC"
	.space 1
	.ascii "Times_param_LC_u64"
	.space 1
	.ascii "uTimes_param_LC_u64"
	.space 1
	.ascii "Times_var_LC_u64"
	.space 1
	.ascii "uTimes_var_LC_u64"
	.space 1
	.ascii "Times_param_LC_u16"
	.space 1
	.ascii "uTimes_param_LC_u16"
	.space 1
	.ascii "Times_var_LC_u16"
	.space 1
	.ascii "uTimes_var_LC_u16"
	.space 1
	.ascii "Times_param_u64_u32"
	.space 1
	.ascii "uTimes_param_u64_u32"
	.space 1
	.ascii "Times_var_u64_u32"
	.space 1
	.ascii "uTimes_var_u64_u32"
	.space 1
	.ascii "Times_param_u64_L"
	.space 1
	.ascii "uTimes_param_u64_L"
	.space 1
	.ascii "Times_var_u64_L"
	.space 1
	.ascii "uTimes_var_u64_L"
	.space 1
	.ascii "Times_param_u64_i8"
	.space 1
	.ascii "uTimes_param_u64_i8"
	.space 1
	.ascii "Times_var_u64_i8"
	.space 1
	.ascii "uTimes_var_u64_i8"
	.space 1
	.ascii "Times_param_u64_u8"
	.space 1
	.ascii "uTimes_param_u64_u8"
	.space 1
	.ascii "Times_var_u64_u8"
	.space 1
	.ascii "uTimes_var_u64_u8"
	.space 1
	.ascii "Times_param_u64_C"
	.space 1
	.ascii "uTimes_param_u64_C"
	.space 1
	.ascii "Times_var_u64_C"
	.space 1
	.ascii "uTimes_var_u64_C"
	.space 1
	.ascii "Times_param_u64_i64"
	.space 1
	.ascii "uTimes_param_u64_i64"
	.space 1
	.ascii "Times_var_u64_i64"
	.space 1
	.ascii "uTimes_var_u64_i64"
	.space 1
	.ascii "Times_param_u64_I"
	.space 1
	.ascii "uTimes_param_u64_I"
	.space 1
	.ascii "Times_var_u64_I"
	.space 1
	.ascii "uTimes_var_u64_I"
	.space 1
	.ascii "Times_param_u64_i32"
	.space 1
	.ascii "uTimes_param_u64_i32"
	.space 1
	.ascii "Times_var_u64_i32"
	.space 1
	.ascii "uTimes_var_u64_i32"
	.space 1
	.ascii "Times_param_u64_i16"
	.space 1
	.ascii "uTimes_param_u64_i16"
	.space 1
	.ascii "Times_var_u64_i16"
	.space 1
	.ascii "uTimes_var_u64_i16"
	.space 1
	.ascii "Times_param_u64_LC"
	.space 1
	.ascii "uTimes_param_u64_LC"
	.space 1
	.ascii "Times_var_u64_LC"
	.space 1
	.ascii "uTimes_var_u64_LC"
	.space 1
	.ascii "Times_param_u64_u64"
	.space 1
	.ascii "uTimes_param_u64_u64"
	.space 1
	.ascii "Times_var_u64_u64"
	.space 1
	.ascii "uTimes_var_u64_u64"
	.space 1
	.ascii "Times_param_u64_u16"
	.space 1
	.ascii "uTimes_param_u64_u16"
	.space 1
	.ascii "Times_var_u64_u16"
	.space 1
	.ascii "uTimes_var_u64_u16"
	.space 1
	.ascii "Times_param_u16_u32"
	.space 1
	.ascii "uTimes_param_u16_u32"
	.space 1
	.ascii "Times_var_u16_u32"
	.space 1
	.ascii "uTimes_var_u16_u32"
	.space 1
	.ascii "Times_param_u16_L"
	.space 1
	.ascii "uTimes_param_u16_L"
	.space 1
	.ascii "Times_var_u16_L"
	.space 1
	.ascii "uTimes_var_u16_L"
	.space 1
	.ascii "Times_param_u16_i8"
	.space 1
	.ascii "uTimes_param_u16_i8"
	.space 1
	.ascii "Times_var_u16_i8"
	.space 1
	.ascii "uTimes_var_u16_i8"
	.space 1
	.ascii "Times_param_u16_u8"
	.space 1
	.ascii "uTimes_param_u16_u8"
	.space 1
	.ascii "Times_var_u16_u8"
	.space 1
	.ascii "uTimes_var_u16_u8"
	.space 1
	.ascii "Times_param_u16_C"
	.space 1
	.ascii "uTimes_param_u16_C"
	.space 1
	.ascii "Times_var_u16_C"
	.space 1
	.ascii "uTimes_var_u16_C"
	.space 1
	.ascii "Times_param_u16_i64"
	.space 1
	.ascii "uTimes_param_u16_i64"
	.space 1
	.ascii "Times_var_u16_i64"
	.space 1
	.ascii "uTimes_var_u16_i64"
	.space 1
	.ascii "Times_param_u16_I"
	.space 1
	.ascii "uTimes_param_u16_I"
	.space 1
	.ascii "Times_var_u16_I"
	.space 1
	.ascii "uTimes_var_u16_I"
	.space 1
	.ascii "Times_param_u16_i32"
	.space 1
	.ascii "uTimes_param_u16_i32"
	.space 1
	.ascii "Times_var_u16_i32"
	.space 1
	.ascii "uTimes_var_u16_i32"
	.space 1
	.ascii "Times_param_u16_i16"
	.space 1
	.ascii "uTimes_param_u16_i16"
	.space 1
	.ascii "Times_var_u16_i16"
	.space 1
	.ascii "uTimes_var_u16_i16"
	.space 1
	.ascii "Times_param_u16_LC"
	.space 1
	.ascii "uTimes_param_u16_LC"
	.space 1
	.ascii "Times_var_u16_LC"
	.space 1
	.ascii "uTimes_var_u16_LC"
	.space 1
	.ascii "Times_param_u16_u64"
	.space 1
	.ascii "uTimes_param_u16_u64"
	.space 1
	.ascii "Times_var_u16_u64"
	.space 1
	.ascii "uTimes_var_u16_u64"
	.space 1
	.ascii "Times_param_u16_u16"
	.space 1
	.ascii "uTimes_param_u16_u16"
	.space 1
	.ascii "Times_var_u16_u16"
	.space 1
	.ascii "uTimes_var_u16_u16"
	.space 4
	.quad	_Times_M3
	.quad	_L_1
	.quad	_Times__Times_param_u32_u32
	.quad	_L_1+9
	.quad	_Times__uTimes_param_u32_u32
	.quad	_L_1+29
	.quad	_Times__Times_var_u32_u32
	.quad	_L_1+50
	.quad	_Times__uTimes_var_u32_u32
	.quad	_L_1+68
	.quad	_Times__Times_param_u32_L
	.quad	_L_1+87
	.quad	_Times__uTimes_param_u32_L
	.quad	_L_1+105
	.quad	_Times__Times_var_u32_L
	.quad	_L_1+124
	.quad	_Times__uTimes_var_u32_L
	.quad	_L_1+140
	.quad	_Times__Times_param_u32_i8
	.quad	_L_1+157
	.quad	_Times__uTimes_param_u32_i8
	.quad	_L_1+176
	.quad	_Times__Times_var_u32_i8
	.quad	_L_1+196
	.quad	_Times__uTimes_var_u32_i8
	.quad	_L_1+213
	.quad	_Times__Times_param_u32_u8
	.quad	_L_1+231
	.quad	_Times__uTimes_param_u32_u8
	.quad	_L_1+250
	.quad	_Times__Times_var_u32_u8
	.quad	_L_1+270
	.quad	_Times__uTimes_var_u32_u8
	.quad	_L_1+287
	.quad	_Times__Times_param_u32_C
	.quad	_L_1+305
	.quad	_Times__uTimes_param_u32_C
	.quad	_L_1+323
	.quad	_Times__Times_var_u32_C
	.quad	_L_1+342
	.quad	_Times__uTimes_var_u32_C
	.quad	_L_1+358
	.quad	_Times__Times_param_u32_i64
	.quad	_L_1+375
	.quad	_Times__uTimes_param_u32_i64
	.quad	_L_1+395
	.quad	_Times__Times_var_u32_i64
	.quad	_L_1+416
	.quad	_Times__uTimes_var_u32_i64
	.quad	_L_1+434
	.quad	_Times__Times_param_u32_I
	.quad	_L_1+453
	.quad	_Times__uTimes_param_u32_I
	.quad	_L_1+471
	.quad	_Times__Times_var_u32_I
	.quad	_L_1+490
	.quad	_Times__uTimes_var_u32_I
	.quad	_L_1+506
	.quad	_Times__Times_param_u32_i32
	.quad	_L_1+523
	.quad	_Times__uTimes_param_u32_i32
	.quad	_L_1+543
	.quad	_Times__Times_var_u32_i32
	.quad	_L_1+564
	.quad	_Times__uTimes_var_u32_i32
	.quad	_L_1+582
	.quad	_Times__Times_param_u32_i16
	.quad	_L_1+601
	.quad	_Times__uTimes_param_u32_i16
	.quad	_L_1+621
	.quad	_Times__Times_var_u32_i16
	.quad	_L_1+642
	.quad	_Times__uTimes_var_u32_i16
	.quad	_L_1+660
	.quad	_Times__Times_param_u32_LC
	.quad	_L_1+679
	.quad	_Times__uTimes_param_u32_LC
	.quad	_L_1+698
	.quad	_Times__Times_var_u32_LC
	.quad	_L_1+718
	.quad	_Times__uTimes_var_u32_LC
	.quad	_L_1+735
	.quad	_Times__Times_param_u32_u64
	.quad	_L_1+753
	.quad	_Times__uTimes_param_u32_u64
	.quad	_L_1+773
	.quad	_Times__Times_var_u32_u64
	.quad	_L_1+794
	.quad	_Times__uTimes_var_u32_u64
	.quad	_L_1+812
	.quad	_Times__Times_param_u32_u16
	.quad	_L_1+831
	.quad	_Times__uTimes_param_u32_u16
	.quad	_L_1+851
	.quad	_Times__Times_var_u32_u16
	.quad	_L_1+872
	.quad	_Times__uTimes_var_u32_u16
	.quad	_L_1+890
	.quad	_Times__Times_param_L_u32
	.quad	_L_1+909
	.quad	_Times__uTimes_param_L_u32
	.quad	_L_1+927
	.quad	_Times__Times_var_L_u32
	.quad	_L_1+946
	.quad	_Times__uTimes_var_L_u32
	.quad	_L_1+962
	.quad	_Times__Times_param_L_L
	.quad	_L_1+979
	.quad	_Times__uTimes_param_L_L
	.quad	_L_1+995
	.quad	_Times__Times_var_L_L
	.quad	_L_1+1012
	.quad	_Times__uTimes_var_L_L
	.quad	_L_1+1026
	.quad	_Times__Times_param_L_i8
	.quad	_L_1+1041
	.quad	_Times__uTimes_param_L_i8
	.quad	_L_1+1058
	.quad	_Times__Times_var_L_i8
	.quad	_L_1+1076
	.quad	_Times__uTimes_var_L_i8
	.quad	_L_1+1091
	.quad	_Times__Times_param_L_u8
	.quad	_L_1+1107
	.quad	_Times__uTimes_param_L_u8
	.quad	_L_1+1124
	.quad	_Times__Times_var_L_u8
	.quad	_L_1+1142
	.quad	_Times__uTimes_var_L_u8
	.quad	_L_1+1157
	.quad	_Times__Times_param_L_C
	.quad	_L_1+1173
	.quad	_Times__uTimes_param_L_C
	.quad	_L_1+1189
	.quad	_Times__Times_var_L_C
	.quad	_L_1+1206
	.quad	_Times__uTimes_var_L_C
	.quad	_L_1+1220
	.quad	_Times__Times_param_L_i64
	.quad	_L_1+1235
	.quad	_Times__uTimes_param_L_i64
	.quad	_L_1+1253
	.quad	_Times__Times_var_L_i64
	.quad	_L_1+1272
	.quad	_Times__uTimes_var_L_i64
	.quad	_L_1+1288
	.quad	_Times__Times_param_L_I
	.quad	_L_1+1305
	.quad	_Times__uTimes_param_L_I
	.quad	_L_1+1321
	.quad	_Times__Times_var_L_I
	.quad	_L_1+1338
	.quad	_Times__uTimes_var_L_I
	.quad	_L_1+1352
	.quad	_Times__Times_param_L_i32
	.quad	_L_1+1367
	.quad	_Times__uTimes_param_L_i32
	.quad	_L_1+1385
	.quad	_Times__Times_var_L_i32
	.quad	_L_1+1404
	.quad	_Times__uTimes_var_L_i32
	.quad	_L_1+1420
	.quad	_Times__Times_param_L_i16
	.quad	_L_1+1437
	.quad	_Times__uTimes_param_L_i16
	.quad	_L_1+1455
	.quad	_Times__Times_var_L_i16
	.quad	_L_1+1474
	.quad	_Times__uTimes_var_L_i16
	.quad	_L_1+1490
	.quad	_Times__Times_param_L_LC
	.quad	_L_1+1507
	.quad	_Times__uTimes_param_L_LC
	.quad	_L_1+1524
	.quad	_Times__Times_var_L_LC
	.quad	_L_1+1542
	.quad	_Times__uTimes_var_L_LC
	.quad	_L_1+1557
	.quad	_Times__Times_param_L_u64
	.quad	_L_1+1573
	.quad	_Times__uTimes_param_L_u64
	.quad	_L_1+1591
	.quad	_Times__Times_var_L_u64
	.quad	_L_1+1610
	.quad	_Times__uTimes_var_L_u64
	.quad	_L_1+1626
	.quad	_Times__Times_param_L_u16
	.quad	_L_1+1643
	.quad	_Times__uTimes_param_L_u16
	.quad	_L_1+1661
	.quad	_Times__Times_var_L_u16
	.quad	_L_1+1680
	.quad	_Times__uTimes_var_L_u16
	.quad	_L_1+1696
	.quad	_Times__Times_param_i8_u32
	.quad	_L_1+1713
	.quad	_Times__uTimes_param_i8_u32
	.quad	_L_1+1732
	.quad	_Times__Times_var_i8_u32
	.quad	_L_1+1752
	.quad	_Times__uTimes_var_i8_u32
	.quad	_L_1+1769
	.quad	_Times__Times_param_i8_L
	.quad	_L_1+1787
	.quad	_Times__uTimes_param_i8_L
	.quad	_L_1+1804
	.quad	_Times__Times_var_i8_L
	.quad	_L_1+1822
	.quad	_Times__uTimes_var_i8_L
	.quad	_L_1+1837
	.quad	_Times__Times_param_i8_i8
	.quad	_L_1+1853
	.quad	_Times__uTimes_param_i8_i8
	.quad	_L_1+1871
	.quad	_Times__Times_var_i8_i8
	.quad	_L_1+1890
	.quad	_Times__uTimes_var_i8_i8
	.quad	_L_1+1906
	.quad	_Times__Times_param_i8_u8
	.quad	_L_1+1923
	.quad	_Times__uTimes_param_i8_u8
	.quad	_L_1+1941
	.quad	_Times__Times_var_i8_u8
	.quad	_L_1+1960
	.quad	_Times__uTimes_var_i8_u8
	.quad	_L_1+1976
	.quad	_Times__Times_param_i8_C
	.quad	_L_1+1993
	.quad	_Times__uTimes_param_i8_C
	.quad	_L_1+2010
	.quad	_Times__Times_var_i8_C
	.quad	_L_1+2028
	.quad	_Times__uTimes_var_i8_C
	.quad	_L_1+2043
	.quad	_Times__Times_param_i8_i64
	.quad	_L_1+2059
	.quad	_Times__uTimes_param_i8_i64
	.quad	_L_1+2078
	.quad	_Times__Times_var_i8_i64
	.quad	_L_1+2098
	.quad	_Times__uTimes_var_i8_i64
	.quad	_L_1+2115
	.quad	_Times__Times_param_i8_I
	.quad	_L_1+2133
	.quad	_Times__uTimes_param_i8_I
	.quad	_L_1+2150
	.quad	_Times__Times_var_i8_I
	.quad	_L_1+2168
	.quad	_Times__uTimes_var_i8_I
	.quad	_L_1+2183
	.quad	_Times__Times_param_i8_i32
	.quad	_L_1+2199
	.quad	_Times__uTimes_param_i8_i32
	.quad	_L_1+2218
	.quad	_Times__Times_var_i8_i32
	.quad	_L_1+2238
	.quad	_Times__uTimes_var_i8_i32
	.quad	_L_1+2255
	.quad	_Times__Times_param_i8_i16
	.quad	_L_1+2273
	.quad	_Times__uTimes_param_i8_i16
	.quad	_L_1+2292
	.quad	_Times__Times_var_i8_i16
	.quad	_L_1+2312
	.quad	_Times__uTimes_var_i8_i16
	.quad	_L_1+2329
	.quad	_Times__Times_param_i8_LC
	.quad	_L_1+2347
	.quad	_Times__uTimes_param_i8_LC
	.quad	_L_1+2365
	.quad	_Times__Times_var_i8_LC
	.quad	_L_1+2384
	.quad	_Times__uTimes_var_i8_LC
	.quad	_L_1+2400
	.quad	_Times__Times_param_i8_u64
	.quad	_L_1+2417
	.quad	_Times__uTimes_param_i8_u64
	.quad	_L_1+2436
	.quad	_Times__Times_var_i8_u64
	.quad	_L_1+2456
	.quad	_Times__uTimes_var_i8_u64
	.quad	_L_1+2473
	.quad	_Times__Times_param_i8_u16
	.quad	_L_1+2491
	.quad	_Times__uTimes_param_i8_u16
	.quad	_L_1+2510
	.quad	_Times__Times_var_i8_u16
	.quad	_L_1+2530
	.quad	_Times__uTimes_var_i8_u16
	.quad	_L_1+2547
	.quad	_Times__Times_param_u8_u32
	.quad	_L_1+2565
	.quad	_Times__uTimes_param_u8_u32
	.quad	_L_1+2584
	.quad	_Times__Times_var_u8_u32
	.quad	_L_1+2604
	.quad	_Times__uTimes_var_u8_u32
	.quad	_L_1+2621
	.quad	_Times__Times_param_u8_L
	.quad	_L_1+2639
	.quad	_Times__uTimes_param_u8_L
	.quad	_L_1+2656
	.quad	_Times__Times_var_u8_L
	.quad	_L_1+2674
	.quad	_Times__uTimes_var_u8_L
	.quad	_L_1+2689
	.quad	_Times__Times_param_u8_i8
	.quad	_L_1+2705
	.quad	_Times__uTimes_param_u8_i8
	.quad	_L_1+2723
	.quad	_Times__Times_var_u8_i8
	.quad	_L_1+2742
	.quad	_Times__uTimes_var_u8_i8
	.quad	_L_1+2758
	.quad	_Times__Times_param_u8_u8
	.quad	_L_1+2775
	.quad	_Times__uTimes_param_u8_u8
	.quad	_L_1+2793
	.quad	_Times__Times_var_u8_u8
	.quad	_L_1+2812
	.quad	_Times__uTimes_var_u8_u8
	.quad	_L_1+2828
	.quad	_Times__Times_param_u8_C
	.quad	_L_1+2845
	.quad	_Times__uTimes_param_u8_C
	.quad	_L_1+2862
	.quad	_Times__Times_var_u8_C
	.quad	_L_1+2880
	.quad	_Times__uTimes_var_u8_C
	.quad	_L_1+2895
	.quad	_Times__Times_param_u8_i64
	.quad	_L_1+2911
	.quad	_Times__uTimes_param_u8_i64
	.quad	_L_1+2930
	.quad	_Times__Times_var_u8_i64
	.quad	_L_1+2950
	.quad	_Times__uTimes_var_u8_i64
	.quad	_L_1+2967
	.quad	_Times__Times_param_u8_I
	.quad	_L_1+2985
	.quad	_Times__uTimes_param_u8_I
	.quad	_L_1+3002
	.quad	_Times__Times_var_u8_I
	.quad	_L_1+3020
	.quad	_Times__uTimes_var_u8_I
	.quad	_L_1+3035
	.quad	_Times__Times_param_u8_i32
	.quad	_L_1+3051
	.quad	_Times__uTimes_param_u8_i32
	.quad	_L_1+3070
	.quad	_Times__Times_var_u8_i32
	.quad	_L_1+3090
	.quad	_Times__uTimes_var_u8_i32
	.quad	_L_1+3107
	.quad	_Times__Times_param_u8_i16
	.quad	_L_1+3125
	.quad	_Times__uTimes_param_u8_i16
	.quad	_L_1+3144
	.quad	_Times__Times_var_u8_i16
	.quad	_L_1+3164
	.quad	_Times__uTimes_var_u8_i16
	.quad	_L_1+3181
	.quad	_Times__Times_param_u8_LC
	.quad	_L_1+3199
	.quad	_Times__uTimes_param_u8_LC
	.quad	_L_1+3217
	.quad	_Times__Times_var_u8_LC
	.quad	_L_1+3236
	.quad	_Times__uTimes_var_u8_LC
	.quad	_L_1+3252
	.quad	_Times__Times_param_u8_u64
	.quad	_L_1+3269
	.quad	_Times__uTimes_param_u8_u64
	.quad	_L_1+3288
	.quad	_Times__Times_var_u8_u64
	.quad	_L_1+3308
	.quad	_Times__uTimes_var_u8_u64
	.quad	_L_1+3325
	.quad	_Times__Times_param_u8_u16
	.quad	_L_1+3343
	.quad	_Times__uTimes_param_u8_u16
	.quad	_L_1+3362
	.quad	_Times__Times_var_u8_u16
	.quad	_L_1+3382
	.quad	_Times__uTimes_var_u8_u16
	.quad	_L_1+3399
	.quad	_Times__Times_param_f32_f32
	.quad	_L_1+3417
	.quad	_Times__Times_var_f32_f32
	.quad	_L_1+3437
	.quad	_Times__Times_param_C_u32
	.quad	_L_1+3455
	.quad	_Times__uTimes_param_C_u32
	.quad	_L_1+3473
	.quad	_Times__Times_var_C_u32
	.quad	_L_1+3492
	.quad	_Times__uTimes_var_C_u32
	.quad	_L_1+3508
	.quad	_Times__Times_param_C_L
	.quad	_L_1+3525
	.quad	_Times__uTimes_param_C_L
	.quad	_L_1+3541
	.quad	_Times__Times_var_C_L
	.quad	_L_1+3558
	.quad	_Times__uTimes_var_C_L
	.quad	_L_1+3572
	.quad	_Times__Times_param_C_i8
	.quad	_L_1+3587
	.quad	_Times__uTimes_param_C_i8
	.quad	_L_1+3604
	.quad	_Times__Times_var_C_i8
	.quad	_L_1+3622
	.quad	_Times__uTimes_var_C_i8
	.quad	_L_1+3637
	.quad	_Times__Times_param_C_u8
	.quad	_L_1+3653
	.quad	_Times__uTimes_param_C_u8
	.quad	_L_1+3670
	.quad	_Times__Times_var_C_u8
	.quad	_L_1+3688
	.quad	_Times__uTimes_var_C_u8
	.quad	_L_1+3703
	.quad	_Times__Times_param_C_C
	.quad	_L_1+3719
	.quad	_Times__uTimes_param_C_C
	.quad	_L_1+3735
	.quad	_Times__Times_var_C_C
	.quad	_L_1+3752
	.quad	_Times__uTimes_var_C_C
	.quad	_L_1+3766
	.quad	_Times__Times_param_C_i64
	.quad	_L_1+3781
	.quad	_Times__uTimes_param_C_i64
	.quad	_L_1+3799
	.quad	_Times__Times_var_C_i64
	.quad	_L_1+3818
	.quad	_Times__uTimes_var_C_i64
	.quad	_L_1+3834
	.quad	_Times__Times_param_C_I
	.quad	_L_1+3851
	.quad	_Times__uTimes_param_C_I
	.quad	_L_1+3867
	.quad	_Times__Times_var_C_I
	.quad	_L_1+3884
	.quad	_Times__uTimes_var_C_I
	.quad	_L_1+3898
	.quad	_Times__Times_param_C_i32
	.quad	_L_1+3913
	.quad	_Times__uTimes_param_C_i32
	.quad	_L_1+3931
	.quad	_Times__Times_var_C_i32
	.quad	_L_1+3950
	.quad	_Times__uTimes_var_C_i32
	.quad	_L_1+3966
	.quad	_Times__Times_param_C_i16
	.quad	_L_1+3983
	.quad	_Times__uTimes_param_C_i16
	.quad	_L_1+4001
	.quad	_Times__Times_var_C_i16
	.quad	_L_1+4020
	.quad	_Times__uTimes_var_C_i16
	.quad	_L_1+4036
	.quad	_Times__Times_param_C_LC
	.quad	_L_1+4053
	.quad	_Times__uTimes_param_C_LC
	.quad	_L_1+4070
	.quad	_Times__Times_var_C_LC
	.quad	_L_1+4088
	.quad	_Times__uTimes_var_C_LC
	.quad	_L_1+4103
	.quad	_Times__Times_param_C_u64
	.quad	_L_1+4119
	.quad	_Times__uTimes_param_C_u64
	.quad	_L_1+4137
	.quad	_Times__Times_var_C_u64
	.quad	_L_1+4156
	.quad	_Times__uTimes_var_C_u64
	.quad	_L_1+4172
	.quad	_Times__Times_param_C_u16
	.quad	_L_1+4189
	.quad	_Times__uTimes_param_C_u16
	.quad	_L_1+4207
	.quad	_Times__Times_var_C_u16
	.quad	_L_1+4226
	.quad	_Times__uTimes_var_C_u16
	.quad	_L_1+4242
	.quad	_Times__Times_param_i64_u32
	.quad	_L_1+4259
	.quad	_Times__uTimes_param_i64_u32
	.quad	_L_1+4279
	.quad	_Times__Times_var_i64_u32
	.quad	_L_1+4300
	.quad	_Times__uTimes_var_i64_u32
	.quad	_L_1+4318
	.quad	_Times__Times_param_i64_L
	.quad	_L_1+4337
	.quad	_Times__uTimes_param_i64_L
	.quad	_L_1+4355
	.quad	_Times__Times_var_i64_L
	.quad	_L_1+4374
	.quad	_Times__uTimes_var_i64_L
	.quad	_L_1+4390
	.quad	_Times__Times_param_i64_i8
	.quad	_L_1+4407
	.quad	_Times__uTimes_param_i64_i8
	.quad	_L_1+4426
	.quad	_Times__Times_var_i64_i8
	.quad	_L_1+4446
	.quad	_Times__uTimes_var_i64_i8
	.quad	_L_1+4463
	.quad	_Times__Times_param_i64_u8
	.quad	_L_1+4481
	.quad	_Times__uTimes_param_i64_u8
	.quad	_L_1+4500
	.quad	_Times__Times_var_i64_u8
	.quad	_L_1+4520
	.quad	_Times__uTimes_var_i64_u8
	.quad	_L_1+4537
	.quad	_Times__Times_param_i64_C
	.quad	_L_1+4555
	.quad	_Times__uTimes_param_i64_C
	.quad	_L_1+4573
	.quad	_Times__Times_var_i64_C
	.quad	_L_1+4592
	.quad	_Times__uTimes_var_i64_C
	.quad	_L_1+4608
	.quad	_Times__Times_param_i64_i64
	.quad	_L_1+4625
	.quad	_Times__uTimes_param_i64_i64
	.quad	_L_1+4645
	.quad	_Times__Times_var_i64_i64
	.quad	_L_1+4666
	.quad	_Times__uTimes_var_i64_i64
	.quad	_L_1+4684
	.quad	_Times__Times_param_i64_I
	.quad	_L_1+4703
	.quad	_Times__uTimes_param_i64_I
	.quad	_L_1+4721
	.quad	_Times__Times_var_i64_I
	.quad	_L_1+4740
	.quad	_Times__uTimes_var_i64_I
	.quad	_L_1+4756
	.quad	_Times__Times_param_i64_i32
	.quad	_L_1+4773
	.quad	_Times__uTimes_param_i64_i32
	.quad	_L_1+4793
	.quad	_Times__Times_var_i64_i32
	.quad	_L_1+4814
	.quad	_Times__uTimes_var_i64_i32
	.quad	_L_1+4832
	.quad	_Times__Times_param_i64_i16
	.quad	_L_1+4851
	.quad	_Times__uTimes_param_i64_i16
	.quad	_L_1+4871
	.quad	_Times__Times_var_i64_i16
	.quad	_L_1+4892
	.quad	_Times__uTimes_var_i64_i16
	.quad	_L_1+4910
	.quad	_Times__Times_param_i64_LC
	.quad	_L_1+4929
	.quad	_Times__uTimes_param_i64_LC
	.quad	_L_1+4948
	.quad	_Times__Times_var_i64_LC
	.quad	_L_1+4968
	.quad	_Times__uTimes_var_i64_LC
	.quad	_L_1+4985
	.quad	_Times__Times_param_i64_u64
	.quad	_L_1+5003
	.quad	_Times__uTimes_param_i64_u64
	.quad	_L_1+5023
	.quad	_Times__Times_var_i64_u64
	.quad	_L_1+5044
	.quad	_Times__uTimes_var_i64_u64
	.quad	_L_1+5062
	.quad	_Times__Times_param_i64_u16
	.quad	_L_1+5081
	.quad	_Times__uTimes_param_i64_u16
	.quad	_L_1+5101
	.quad	_Times__Times_var_i64_u16
	.quad	_L_1+5122
	.quad	_Times__uTimes_var_i64_u16
	.quad	_L_1+5140
	.quad	_Times__Times_param_I_u32
	.quad	_L_1+5159
	.quad	_Times__uTimes_param_I_u32
	.quad	_L_1+5177
	.quad	_Times__Times_var_I_u32
	.quad	_L_1+5196
	.quad	_Times__uTimes_var_I_u32
	.quad	_L_1+5212
	.quad	_Times__Times_param_I_L
	.quad	_L_1+5229
	.quad	_Times__uTimes_param_I_L
	.quad	_L_1+5245
	.quad	_Times__Times_var_I_L
	.quad	_L_1+5262
	.quad	_Times__uTimes_var_I_L
	.quad	_L_1+5276
	.quad	_Times__Times_param_I_i8
	.quad	_L_1+5291
	.quad	_Times__uTimes_param_I_i8
	.quad	_L_1+5308
	.quad	_Times__Times_var_I_i8
	.quad	_L_1+5326
	.quad	_Times__uTimes_var_I_i8
	.quad	_L_1+5341
	.quad	_Times__Times_param_I_u8
	.quad	_L_1+5357
	.quad	_Times__uTimes_param_I_u8
	.quad	_L_1+5374
	.quad	_Times__Times_var_I_u8
	.quad	_L_1+5392
	.quad	_Times__uTimes_var_I_u8
	.quad	_L_1+5407
	.quad	_Times__Times_param_I_C
	.quad	_L_1+5423
	.quad	_Times__uTimes_param_I_C
	.quad	_L_1+5439
	.quad	_Times__Times_var_I_C
	.quad	_L_1+5456
	.quad	_Times__uTimes_var_I_C
	.quad	_L_1+5470
	.quad	_Times__Times_param_I_i64
	.quad	_L_1+5485
	.quad	_Times__uTimes_param_I_i64
	.quad	_L_1+5503
	.quad	_Times__Times_var_I_i64
	.quad	_L_1+5522
	.quad	_Times__uTimes_var_I_i64
	.quad	_L_1+5538
	.quad	_Times__Times_param_I_I
	.quad	_L_1+5555
	.quad	_Times__uTimes_param_I_I
	.quad	_L_1+5571
	.quad	_Times__Times_var_I_I
	.quad	_L_1+5588
	.quad	_Times__uTimes_var_I_I
	.quad	_L_1+5602
	.quad	_Times__Times_param_I_i32
	.quad	_L_1+5617
	.quad	_Times__uTimes_param_I_i32
	.quad	_L_1+5635
	.quad	_Times__Times_var_I_i32
	.quad	_L_1+5654
	.quad	_Times__uTimes_var_I_i32
	.quad	_L_1+5670
	.quad	_Times__Times_param_I_i16
	.quad	_L_1+5687
	.quad	_Times__uTimes_param_I_i16
	.quad	_L_1+5705
	.quad	_Times__Times_var_I_i16
	.quad	_L_1+5724
	.quad	_Times__uTimes_var_I_i16
	.quad	_L_1+5740
	.quad	_Times__Times_param_I_LC
	.quad	_L_1+5757
	.quad	_Times__uTimes_param_I_LC
	.quad	_L_1+5774
	.quad	_Times__Times_var_I_LC
	.quad	_L_1+5792
	.quad	_Times__uTimes_var_I_LC
	.quad	_L_1+5807
	.quad	_Times__Times_param_I_u64
	.quad	_L_1+5823
	.quad	_Times__uTimes_param_I_u64
	.quad	_L_1+5841
	.quad	_Times__Times_var_I_u64
	.quad	_L_1+5860
	.quad	_Times__uTimes_var_I_u64
	.quad	_L_1+5876
	.quad	_Times__Times_param_I_u16
	.quad	_L_1+5893
	.quad	_Times__uTimes_param_I_u16
	.quad	_L_1+5911
	.quad	_Times__Times_var_I_u16
	.quad	_L_1+5930
	.quad	_Times__uTimes_var_I_u16
	.quad	_L_1+5946
	.quad	_Times__Times_param_i32_u32
	.quad	_L_1+5963
	.quad	_Times__uTimes_param_i32_u32
	.quad	_L_1+5983
	.quad	_Times__Times_var_i32_u32
	.quad	_L_1+6004
	.quad	_Times__uTimes_var_i32_u32
	.quad	_L_1+6022
	.quad	_Times__Times_param_i32_L
	.quad	_L_1+6041
	.quad	_Times__uTimes_param_i32_L
	.quad	_L_1+6059
	.quad	_Times__Times_var_i32_L
	.quad	_L_1+6078
	.quad	_Times__uTimes_var_i32_L
	.quad	_L_1+6094
	.quad	_Times__Times_param_i32_i8
	.quad	_L_1+6111
	.quad	_Times__uTimes_param_i32_i8
	.quad	_L_1+6130
	.quad	_Times__Times_var_i32_i8
	.quad	_L_1+6150
	.quad	_Times__uTimes_var_i32_i8
	.quad	_L_1+6167
	.quad	_Times__Times_param_i32_u8
	.quad	_L_1+6185
	.quad	_Times__uTimes_param_i32_u8
	.quad	_L_1+6204
	.quad	_Times__Times_var_i32_u8
	.quad	_L_1+6224
	.quad	_Times__uTimes_var_i32_u8
	.quad	_L_1+6241
	.quad	_Times__Times_param_i32_C
	.quad	_L_1+6259
	.quad	_Times__uTimes_param_i32_C
	.quad	_L_1+6277
	.quad	_Times__Times_var_i32_C
	.quad	_L_1+6296
	.quad	_Times__uTimes_var_i32_C
	.quad	_L_1+6312
	.quad	_Times__Times_param_i32_i64
	.quad	_L_1+6329
	.quad	_Times__uTimes_param_i32_i64
	.quad	_L_1+6349
	.quad	_Times__Times_var_i32_i64
	.quad	_L_1+6370
	.quad	_Times__uTimes_var_i32_i64
	.quad	_L_1+6388
	.quad	_Times__Times_param_i32_I
	.quad	_L_1+6407
	.quad	_Times__uTimes_param_i32_I
	.quad	_L_1+6425
	.quad	_Times__Times_var_i32_I
	.quad	_L_1+6444
	.quad	_Times__uTimes_var_i32_I
	.quad	_L_1+6460
	.quad	_Times__Times_param_i32_i32
	.quad	_L_1+6477
	.quad	_Times__uTimes_param_i32_i32
	.quad	_L_1+6497
	.quad	_Times__Times_var_i32_i32
	.quad	_L_1+6518
	.quad	_Times__uTimes_var_i32_i32
	.quad	_L_1+6536
	.quad	_Times__Times_param_i32_i16
	.quad	_L_1+6555
	.quad	_Times__uTimes_param_i32_i16
	.quad	_L_1+6575
	.quad	_Times__Times_var_i32_i16
	.quad	_L_1+6596
	.quad	_Times__uTimes_var_i32_i16
	.quad	_L_1+6614
	.quad	_Times__Times_param_i32_LC
	.quad	_L_1+6633
	.quad	_Times__uTimes_param_i32_LC
	.quad	_L_1+6652
	.quad	_Times__Times_var_i32_LC
	.quad	_L_1+6672
	.quad	_Times__uTimes_var_i32_LC
	.quad	_L_1+6689
	.quad	_Times__Times_param_i32_u64
	.quad	_L_1+6707
	.quad	_Times__uTimes_param_i32_u64
	.quad	_L_1+6727
	.quad	_Times__Times_var_i32_u64
	.quad	_L_1+6748
	.quad	_Times__uTimes_var_i32_u64
	.quad	_L_1+6766
	.quad	_Times__Times_param_i32_u16
	.quad	_L_1+6785
	.quad	_Times__uTimes_param_i32_u16
	.quad	_L_1+6805
	.quad	_Times__Times_var_i32_u16
	.quad	_L_1+6826
	.quad	_Times__uTimes_var_i32_u16
	.quad	_L_1+6844
	.quad	_Times__Times_param_i16_u32
	.quad	_L_1+6863
	.quad	_Times__uTimes_param_i16_u32
	.quad	_L_1+6883
	.quad	_Times__Times_var_i16_u32
	.quad	_L_1+6904
	.quad	_Times__uTimes_var_i16_u32
	.quad	_L_1+6922
	.quad	_Times__Times_param_i16_L
	.quad	_L_1+6941
	.quad	_Times__uTimes_param_i16_L
	.quad	_L_1+6959
	.quad	_Times__Times_var_i16_L
	.quad	_L_1+6978
	.quad	_Times__uTimes_var_i16_L
	.quad	_L_1+6994
	.quad	_Times__Times_param_i16_i8
	.quad	_L_1+7011
	.quad	_Times__uTimes_param_i16_i8
	.quad	_L_1+7030
	.quad	_Times__Times_var_i16_i8
	.quad	_L_1+7050
	.quad	_Times__uTimes_var_i16_i8
	.quad	_L_1+7067
	.quad	_Times__Times_param_i16_u8
	.quad	_L_1+7085
	.quad	_Times__uTimes_param_i16_u8
	.quad	_L_1+7104
	.quad	_Times__Times_var_i16_u8
	.quad	_L_1+7124
	.quad	_Times__uTimes_var_i16_u8
	.quad	_L_1+7141
	.quad	_Times__Times_param_i16_C
	.quad	_L_1+7159
	.quad	_Times__uTimes_param_i16_C
	.quad	_L_1+7177
	.quad	_Times__Times_var_i16_C
	.quad	_L_1+7196
	.quad	_Times__uTimes_var_i16_C
	.quad	_L_1+7212
	.quad	_Times__Times_param_i16_i64
	.quad	_L_1+7229
	.quad	_Times__uTimes_param_i16_i64
	.quad	_L_1+7249
	.quad	_Times__Times_var_i16_i64
	.quad	_L_1+7270
	.quad	_Times__uTimes_var_i16_i64
	.quad	_L_1+7288
	.quad	_Times__Times_param_i16_I
	.quad	_L_1+7307
	.quad	_Times__uTimes_param_i16_I
	.quad	_L_1+7325
	.quad	_Times__Times_var_i16_I
	.quad	_L_1+7344
	.quad	_Times__uTimes_var_i16_I
	.quad	_L_1+7360
	.quad	_Times__Times_param_i16_i32
	.quad	_L_1+7377
	.quad	_Times__uTimes_param_i16_i32
	.quad	_L_1+7397
	.quad	_Times__Times_var_i16_i32
	.quad	_L_1+7418
	.quad	_Times__uTimes_var_i16_i32
	.quad	_L_1+7436
	.quad	_Times__Times_param_i16_i16
	.quad	_L_1+7455
	.quad	_Times__uTimes_param_i16_i16
	.quad	_L_1+7475
	.quad	_Times__Times_var_i16_i16
	.quad	_L_1+7496
	.quad	_Times__uTimes_var_i16_i16
	.quad	_L_1+7514
	.quad	_Times__Times_param_i16_LC
	.quad	_L_1+7533
	.quad	_Times__uTimes_param_i16_LC
	.quad	_L_1+7552
	.quad	_Times__Times_var_i16_LC
	.quad	_L_1+7572
	.quad	_Times__uTimes_var_i16_LC
	.quad	_L_1+7589
	.quad	_Times__Times_param_i16_u64
	.quad	_L_1+7607
	.quad	_Times__uTimes_param_i16_u64
	.quad	_L_1+7627
	.quad	_Times__Times_var_i16_u64
	.quad	_L_1+7648
	.quad	_Times__uTimes_var_i16_u64
	.quad	_L_1+7666
	.quad	_Times__Times_param_i16_u16
	.quad	_L_1+7685
	.quad	_Times__uTimes_param_i16_u16
	.quad	_L_1+7705
	.quad	_Times__Times_var_i16_u16
	.quad	_L_1+7726
	.quad	_Times__uTimes_var_i16_u16
	.quad	_L_1+7744
	.quad	_Times__Times_param_f64_f64
	.quad	_L_1+7763
	.quad	_Times__Times_var_f64_f64
	.quad	_L_1+7783
	.quad	_Times__Times_param_LC_u32
	.quad	_L_1+7801
	.quad	_Times__uTimes_param_LC_u32
	.quad	_L_1+7820
	.quad	_Times__Times_var_LC_u32
	.quad	_L_1+7840
	.quad	_Times__uTimes_var_LC_u32
	.quad	_L_1+7857
	.quad	_Times__Times_param_LC_L
	.quad	_L_1+7875
	.quad	_Times__uTimes_param_LC_L
	.quad	_L_1+7892
	.quad	_Times__Times_var_LC_L
	.quad	_L_1+7910
	.quad	_Times__uTimes_var_LC_L
	.quad	_L_1+7925
	.quad	_Times__Times_param_LC_i8
	.quad	_L_1+7941
	.quad	_Times__uTimes_param_LC_i8
	.quad	_L_1+7959
	.quad	_Times__Times_var_LC_i8
	.quad	_L_1+7978
	.quad	_Times__uTimes_var_LC_i8
	.quad	_L_1+7994
	.quad	_Times__Times_param_LC_u8
	.quad	_L_1+8011
	.quad	_Times__uTimes_param_LC_u8
	.quad	_L_1+8029
	.quad	_Times__Times_var_LC_u8
	.quad	_L_1+8048
	.quad	_Times__uTimes_var_LC_u8
	.quad	_L_1+8064
	.quad	_Times__Times_param_LC_C
	.quad	_L_1+8081
	.quad	_Times__uTimes_param_LC_C
	.quad	_L_1+8098
	.quad	_Times__Times_var_LC_C
	.quad	_L_1+8116
	.quad	_Times__uTimes_var_LC_C
	.quad	_L_1+8131
	.quad	_Times__Times_param_LC_i64
	.quad	_L_1+8147
	.quad	_Times__uTimes_param_LC_i64
	.quad	_L_1+8166
	.quad	_Times__Times_var_LC_i64
	.quad	_L_1+8186
	.quad	_Times__uTimes_var_LC_i64
	.quad	_L_1+8203
	.quad	_Times__Times_param_LC_I
	.quad	_L_1+8221
	.quad	_Times__uTimes_param_LC_I
	.quad	_L_1+8238
	.quad	_Times__Times_var_LC_I
	.quad	_L_1+8256
	.quad	_Times__uTimes_var_LC_I
	.quad	_L_1+8271
	.quad	_Times__Times_param_LC_i32
	.quad	_L_1+8287
	.quad	_Times__uTimes_param_LC_i32
	.quad	_L_1+8306
	.quad	_Times__Times_var_LC_i32
	.quad	_L_1+8326
	.quad	_Times__uTimes_var_LC_i32
	.quad	_L_1+8343
	.quad	_Times__Times_param_LC_i16
	.quad	_L_1+8361
	.quad	_Times__uTimes_param_LC_i16
	.quad	_L_1+8380
	.quad	_Times__Times_var_LC_i16
	.quad	_L_1+8400
	.quad	_Times__uTimes_var_LC_i16
	.quad	_L_1+8417
	.quad	_Times__Times_param_LC_LC
	.quad	_L_1+8435
	.quad	_Times__uTimes_param_LC_LC
	.quad	_L_1+8453
	.quad	_Times__Times_var_LC_LC
	.quad	_L_1+8472
	.quad	_Times__uTimes_var_LC_LC
	.quad	_L_1+8488
	.quad	_Times__Times_param_LC_u64
	.quad	_L_1+8505
	.quad	_Times__uTimes_param_LC_u64
	.quad	_L_1+8524
	.quad	_Times__Times_var_LC_u64
	.quad	_L_1+8544
	.quad	_Times__uTimes_var_LC_u64
	.quad	_L_1+8561
	.quad	_Times__Times_param_LC_u16
	.quad	_L_1+8579
	.quad	_Times__uTimes_param_LC_u16
	.quad	_L_1+8598
	.quad	_Times__Times_var_LC_u16
	.quad	_L_1+8618
	.quad	_Times__uTimes_var_LC_u16
	.quad	_L_1+8635
	.quad	_Times__Times_param_u64_u32
	.quad	_L_1+8653
	.quad	_Times__uTimes_param_u64_u32
	.quad	_L_1+8673
	.quad	_Times__Times_var_u64_u32
	.quad	_L_1+8694
	.quad	_Times__uTimes_var_u64_u32
	.quad	_L_1+8712
	.quad	_Times__Times_param_u64_L
	.quad	_L_1+8731
	.quad	_Times__uTimes_param_u64_L
	.quad	_L_1+8749
	.quad	_Times__Times_var_u64_L
	.quad	_L_1+8768
	.quad	_Times__uTimes_var_u64_L
	.quad	_L_1+8784
	.quad	_Times__Times_param_u64_i8
	.quad	_L_1+8801
	.quad	_Times__uTimes_param_u64_i8
	.quad	_L_1+8820
	.quad	_Times__Times_var_u64_i8
	.quad	_L_1+8840
	.quad	_Times__uTimes_var_u64_i8
	.quad	_L_1+8857
	.quad	_Times__Times_param_u64_u8
	.quad	_L_1+8875
	.quad	_Times__uTimes_param_u64_u8
	.quad	_L_1+8894
	.quad	_Times__Times_var_u64_u8
	.quad	_L_1+8914
	.quad	_Times__uTimes_var_u64_u8
	.quad	_L_1+8931
	.quad	_Times__Times_param_u64_C
	.quad	_L_1+8949
	.quad	_Times__uTimes_param_u64_C
	.quad	_L_1+8967
	.quad	_Times__Times_var_u64_C
	.quad	_L_1+8986
	.quad	_Times__uTimes_var_u64_C
	.quad	_L_1+9002
	.quad	_Times__Times_param_u64_i64
	.quad	_L_1+9019
	.quad	_Times__uTimes_param_u64_i64
	.quad	_L_1+9039
	.quad	_Times__Times_var_u64_i64
	.quad	_L_1+9060
	.quad	_Times__uTimes_var_u64_i64
	.quad	_L_1+9078
	.quad	_Times__Times_param_u64_I
	.quad	_L_1+9097
	.quad	_Times__uTimes_param_u64_I
	.quad	_L_1+9115
	.quad	_Times__Times_var_u64_I
	.quad	_L_1+9134
	.quad	_Times__uTimes_var_u64_I
	.quad	_L_1+9150
	.quad	_Times__Times_param_u64_i32
	.quad	_L_1+9167
	.quad	_Times__uTimes_param_u64_i32
	.quad	_L_1+9187
	.quad	_Times__Times_var_u64_i32
	.quad	_L_1+9208
	.quad	_Times__uTimes_var_u64_i32
	.quad	_L_1+9226
	.quad	_Times__Times_param_u64_i16
	.quad	_L_1+9245
	.quad	_Times__uTimes_param_u64_i16
	.quad	_L_1+9265
	.quad	_Times__Times_var_u64_i16
	.quad	_L_1+9286
	.quad	_Times__uTimes_var_u64_i16
	.quad	_L_1+9304
	.quad	_Times__Times_param_u64_LC
	.quad	_L_1+9323
	.quad	_Times__uTimes_param_u64_LC
	.quad	_L_1+9342
	.quad	_Times__Times_var_u64_LC
	.quad	_L_1+9362
	.quad	_Times__uTimes_var_u64_LC
	.quad	_L_1+9379
	.quad	_Times__Times_param_u64_u64
	.quad	_L_1+9397
	.quad	_Times__uTimes_param_u64_u64
	.quad	_L_1+9417
	.quad	_Times__Times_var_u64_u64
	.quad	_L_1+9438
	.quad	_Times__uTimes_var_u64_u64
	.quad	_L_1+9456
	.quad	_Times__Times_param_u64_u16
	.quad	_L_1+9475
	.quad	_Times__uTimes_param_u64_u16
	.quad	_L_1+9495
	.quad	_Times__Times_var_u64_u16
	.quad	_L_1+9516
	.quad	_Times__uTimes_var_u64_u16
	.quad	_L_1+9534
	.quad	_Times__Times_param_u16_u32
	.quad	_L_1+9553
	.quad	_Times__uTimes_param_u16_u32
	.quad	_L_1+9573
	.quad	_Times__Times_var_u16_u32
	.quad	_L_1+9594
	.quad	_Times__uTimes_var_u16_u32
	.quad	_L_1+9612
	.quad	_Times__Times_param_u16_L
	.quad	_L_1+9631
	.quad	_Times__uTimes_param_u16_L
	.quad	_L_1+9649
	.quad	_Times__Times_var_u16_L
	.quad	_L_1+9668
	.quad	_Times__uTimes_var_u16_L
	.quad	_L_1+9684
	.quad	_Times__Times_param_u16_i8
	.quad	_L_1+9701
	.quad	_Times__uTimes_param_u16_i8
	.quad	_L_1+9720
	.quad	_Times__Times_var_u16_i8
	.quad	_L_1+9740
	.quad	_Times__uTimes_var_u16_i8
	.quad	_L_1+9757
	.quad	_Times__Times_param_u16_u8
	.quad	_L_1+9775
	.quad	_Times__uTimes_param_u16_u8
	.quad	_L_1+9794
	.quad	_Times__Times_var_u16_u8
	.quad	_L_1+9814
	.quad	_Times__uTimes_var_u16_u8
	.quad	_L_1+9831
	.quad	_Times__Times_param_u16_C
	.quad	_L_1+9849
	.quad	_Times__uTimes_param_u16_C
	.quad	_L_1+9867
	.quad	_Times__Times_var_u16_C
	.quad	_L_1+9886
	.quad	_Times__uTimes_var_u16_C
	.quad	_L_1+9902
	.quad	_Times__Times_param_u16_i64
	.quad	_L_1+9919
	.quad	_Times__uTimes_param_u16_i64
	.quad	_L_1+9939
	.quad	_Times__Times_var_u16_i64
	.quad	_L_1+9960
	.quad	_Times__uTimes_var_u16_i64
	.quad	_L_1+9978
	.quad	_Times__Times_param_u16_I
	.quad	_L_1+9997
	.quad	_Times__uTimes_param_u16_I
	.quad	_L_1+10015
	.quad	_Times__Times_var_u16_I
	.quad	_L_1+10034
	.quad	_Times__uTimes_var_u16_I
	.quad	_L_1+10050
	.quad	_Times__Times_param_u16_i32
	.quad	_L_1+10067
	.quad	_Times__uTimes_param_u16_i32
	.quad	_L_1+10087
	.quad	_Times__Times_var_u16_i32
	.quad	_L_1+10108
	.quad	_Times__uTimes_var_u16_i32
	.quad	_L_1+10126
	.quad	_Times__Times_param_u16_i16
	.quad	_L_1+10145
	.quad	_Times__uTimes_param_u16_i16
	.quad	_L_1+10165
	.quad	_Times__Times_var_u16_i16
	.quad	_L_1+10186
	.quad	_Times__uTimes_var_u16_i16
	.quad	_L_1+10204
	.quad	_Times__Times_param_u16_LC
	.quad	_L_1+10223
	.quad	_Times__uTimes_param_u16_LC
	.quad	_L_1+10242
	.quad	_Times__Times_var_u16_LC
	.quad	_L_1+10262
	.quad	_Times__uTimes_var_u16_LC
	.quad	_L_1+10279
	.quad	_Times__Times_param_u16_u64
	.quad	_L_1+10297
	.quad	_Times__uTimes_param_u16_u64
	.quad	_L_1+10317
	.quad	_Times__Times_var_u16_u64
	.quad	_L_1+10338
	.quad	_Times__uTimes_var_u16_u64
	.quad	_L_1+10356
	.quad	_Times__Times_param_u16_u16
	.quad	_L_1+10375
	.quad	_Times__uTimes_param_u16_u16
	.quad	_L_1+10395
	.quad	_Times__Times_var_u16_u16
	.quad	_L_1+10416
	.quad	_Times__uTimes_var_u16_u16
	.quad	_L_1+10434
	.space 8
	.ascii "../AMD64_DARWIN/Times.m3"
	.space 8
	.data
	.align 5
_MM_Times:
	.quad	_L_1+19760
	.space 32
	.quad	_L_1+10456
	.space 24
	.quad	_MM_Times+208
	.space 8
	.quad	_Times_M3
	.quad	3
	.word	177
	.space 6
	.quad	178
	.quad	179
	.long	3229815407
	.long	1080460746
	.word	182
	.space 2
	.long	183
	.quad	184
	.quad	185
	.quad	186
	.long	1127952417
	.byte	-67
	.byte	-66
	.space 2
	.quad	191
	.long	192
	.space 28
	.quad	_Times_I3
	.quad	_MM_Times+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Times+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Times+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Times+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
