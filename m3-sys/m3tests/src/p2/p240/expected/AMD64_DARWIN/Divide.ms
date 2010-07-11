	.text
.globl _Divide__uDivide_var_u16_u16
	.private_extern _Divide__uDivide_var_u16_u16
_Divide__uDivide_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_u16
	.private_extern _Divide__Divide_var_u16_u16
_Divide__Divide_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
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
.globl _Divide__uDivide_param_u16_u16
	.private_extern _Divide__uDivide_param_u16_u16
_Divide__uDivide_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_u16
	.private_extern _Divide__Divide_param_u16_u16
_Divide__Divide_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L10
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L10
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L10:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L11
	cmpq	$32767, -16(%rbp)
	jle	L12
L11:
	movl	$1825, %edi
	call	__m3_fault
L12:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u64
	.private_extern _Divide__uDivide_var_u16_u64
_Divide__uDivide_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_u64
	.private_extern _Divide__Divide_var_u16_u64
_Divide__Divide_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L17
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L17
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L17:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_u64
	.private_extern _Divide__uDivide_param_u16_u64
_Divide__uDivide_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_u64
	.private_extern _Divide__Divide_param_u16_u64
_Divide__Divide_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L22
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L22
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L22:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_LC
	.private_extern _Divide__uDivide_var_u16_LC
_Divide__uDivide_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_LC
	.private_extern _Divide__Divide_var_u16_LC
_Divide__Divide_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L27
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L27
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L27:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_LC
	.private_extern _Divide__uDivide_param_u16_LC
_Divide__uDivide_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u16_LC
	.private_extern _Divide__Divide_param_u16_LC
_Divide__Divide_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L32
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L32
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L32:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i16
	.private_extern _Divide__uDivide_var_u16_i16
_Divide__uDivide_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_i16
	.private_extern _Divide__Divide_var_u16_i16
_Divide__Divide_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L37
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L37
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L37:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L38
	cmpq	$32767, -16(%rbp)
	jle	L39
L38:
	movl	$2145, %edi
	call	__m3_fault
L39:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_i16
	.private_extern _Divide__uDivide_param_u16_i16
_Divide__uDivide_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_i16
	.private_extern _Divide__Divide_param_u16_i16
_Divide__Divide_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
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
	je	L44
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L44
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L44:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L45
	cmpq	$32767, -16(%rbp)
	jle	L46
L45:
	movl	$2209, %edi
	call	__m3_fault
L46:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i32
	.private_extern _Divide__uDivide_var_u16_i32
_Divide__uDivide_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_i32
	.private_extern _Divide__Divide_var_u16_i32
_Divide__Divide_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L51
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L51
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L51:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L52
	cmpq	$32767, -16(%rbp)
	jle	L53
L52:
	movl	$2273, %edi
	call	__m3_fault
L53:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_i32
	.private_extern _Divide__uDivide_param_u16_i32
_Divide__uDivide_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_i32
	.private_extern _Divide__Divide_param_u16_i32
_Divide__Divide_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
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
	je	L58
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L58
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L58:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L59
	cmpq	$32767, -16(%rbp)
	jle	L60
L59:
	movl	$2337, %edi
	call	__m3_fault
L60:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_I
	.private_extern _Divide__uDivide_var_u16_I
_Divide__uDivide_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_I
	.private_extern _Divide__Divide_var_u16_I
_Divide__Divide_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L65
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L65
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L65:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L66
	cmpq	$32767, -16(%rbp)
	jle	L67
L66:
	movl	$2401, %edi
	call	__m3_fault
L67:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_I
	.private_extern _Divide__uDivide_param_u16_I
_Divide__uDivide_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_I
	.private_extern _Divide__Divide_param_u16_I
_Divide__Divide_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L72
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L72
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L72:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L73
	cmpq	$32767, -16(%rbp)
	jle	L74
L73:
	movl	$2465, %edi
	call	__m3_fault
L74:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i64
	.private_extern _Divide__uDivide_var_u16_i64
_Divide__uDivide_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_i64
	.private_extern _Divide__Divide_var_u16_i64
_Divide__Divide_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L79
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L79
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L79:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_i64
	.private_extern _Divide__uDivide_param_u16_i64
_Divide__uDivide_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_i64
	.private_extern _Divide__Divide_param_u16_i64
_Divide__Divide_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L84
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L84
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L84:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_C
	.private_extern _Divide__uDivide_var_u16_C
_Divide__uDivide_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_C
	.private_extern _Divide__Divide_var_u16_C
_Divide__Divide_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L89
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L89
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L89:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L90
	cmpq	$32767, -16(%rbp)
	jle	L91
L90:
	movl	$2657, %edi
	call	__m3_fault
L91:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_C
	.private_extern _Divide__uDivide_param_u16_C
_Divide__uDivide_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u16_C
	.private_extern _Divide__Divide_param_u16_C
_Divide__Divide_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L96
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L96
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L96:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L97
	cmpq	$32767, -16(%rbp)
	jle	L98
L97:
	movl	$2721, %edi
	call	__m3_fault
L98:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u8
	.private_extern _Divide__uDivide_var_u16_u8
_Divide__uDivide_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_u8
	.private_extern _Divide__Divide_var_u16_u8
_Divide__Divide_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L103
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L103
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L103:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L104
	cmpq	$32767, -16(%rbp)
	jle	L105
L104:
	movl	$2785, %edi
	call	__m3_fault
L105:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_u8
	.private_extern _Divide__uDivide_param_u16_u8
_Divide__uDivide_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movsbq	-19(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_u8
	.private_extern _Divide__Divide_param_u16_u8
_Divide__Divide_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movsbq	-19(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L110
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L110
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L110:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L111
	cmpq	$32767, -16(%rbp)
	jle	L112
L111:
	movl	$2849, %edi
	call	__m3_fault
L112:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i8
	.private_extern _Divide__uDivide_var_u16_i8
_Divide__uDivide_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_i8
	.private_extern _Divide__Divide_var_u16_i8
_Divide__Divide_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L117
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L117
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L117:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L118
	cmpq	$32767, -16(%rbp)
	jle	L119
L118:
	movl	$2913, %edi
	call	__m3_fault
L119:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_i8
	.private_extern _Divide__uDivide_param_u16_i8
_Divide__uDivide_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_i8
	.private_extern _Divide__Divide_param_u16_i8
_Divide__Divide_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rax
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
	je	L124
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L124
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L124:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L125
	cmpq	$32767, -16(%rbp)
	jle	L126
L125:
	movl	$2977, %edi
	call	__m3_fault
L126:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_L
	.private_extern _Divide__uDivide_var_u16_L
_Divide__uDivide_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_L
	.private_extern _Divide__Divide_var_u16_L
_Divide__Divide_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L131
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L131
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L131:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_L
	.private_extern _Divide__uDivide_param_u16_L
_Divide__uDivide_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_L
	.private_extern _Divide__Divide_param_u16_L
_Divide__Divide_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L136
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L136
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L136:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u32
	.private_extern _Divide__uDivide_var_u16_u32
_Divide__uDivide_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u16_u32
	.private_extern _Divide__Divide_var_u16_u32
_Divide__Divide_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L141
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L141
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L141:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L142
	cmpq	$32767, -16(%rbp)
	jle	L143
L142:
	movl	$3169, %edi
	call	__m3_fault
L143:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u16_u32
	.private_extern _Divide__uDivide_param_u16_u32
_Divide__uDivide_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u16_u32
	.private_extern _Divide__Divide_param_u16_u32
_Divide__Divide_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L148
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L148
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L148:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L149
	cmpq	$32767, -16(%rbp)
	jle	L150
L149:
	movl	$3233, %edi
	call	__m3_fault
L150:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u16
	.private_extern _Divide__uDivide_var_u64_u16
_Divide__uDivide_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_u16
	.private_extern _Divide__Divide_var_u64_u16
_Divide__Divide_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L155
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L155
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L155:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_u16
	.private_extern _Divide__uDivide_param_u64_u16
_Divide__uDivide_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movswq	-26(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_u16
	.private_extern _Divide__Divide_param_u64_u16
_Divide__Divide_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L160
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L160
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L160:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u64
	.private_extern _Divide__uDivide_var_u64_u64
_Divide__uDivide_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_u64
	.private_extern _Divide__Divide_var_u64_u64
_Divide__Divide_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _Divide__uDivide_param_u64_u64
	.private_extern _Divide__uDivide_param_u64_u64
_Divide__uDivide_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_u64
	.private_extern _Divide__Divide_param_u64_u64
_Divide__Divide_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L169
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L169
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L169:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_LC
	.private_extern _Divide__uDivide_var_u64_LC
_Divide__uDivide_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_LC
	.private_extern _Divide__Divide_var_u64_LC
_Divide__Divide_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L174
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L174
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L174:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_LC
	.private_extern _Divide__uDivide_param_u64_LC
_Divide__uDivide_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u64_LC
	.private_extern _Divide__Divide_param_u64_LC
_Divide__Divide_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L179
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L179
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L179:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i16
	.private_extern _Divide__uDivide_var_u64_i16
_Divide__uDivide_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_i16
	.private_extern _Divide__Divide_var_u64_i16
_Divide__Divide_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L184
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L184
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L184:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_i16
	.private_extern _Divide__uDivide_param_u64_i16
_Divide__uDivide_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_i16
	.private_extern _Divide__Divide_param_u64_i16
_Divide__Divide_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L189
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L189
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L189:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i32
	.private_extern _Divide__uDivide_var_u64_i32
_Divide__uDivide_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_i32
	.private_extern _Divide__Divide_var_u64_i32
_Divide__Divide_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L194
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L194
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L194:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_i32
	.private_extern _Divide__uDivide_param_u64_i32
_Divide__uDivide_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_i32
	.private_extern _Divide__Divide_param_u64_i32
_Divide__Divide_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L199
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L199
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L199:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_I
	.private_extern _Divide__uDivide_var_u64_I
_Divide__uDivide_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_I
	.private_extern _Divide__Divide_var_u64_I
_Divide__Divide_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L204
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L204
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L204:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_I
	.private_extern _Divide__uDivide_param_u64_I
_Divide__uDivide_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_I
	.private_extern _Divide__Divide_param_u64_I
_Divide__Divide_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L209
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L209
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L209:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i64
	.private_extern _Divide__uDivide_var_u64_i64
_Divide__uDivide_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_i64
	.private_extern _Divide__Divide_var_u64_i64
_Divide__Divide_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L214
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L214
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L214:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_i64
	.private_extern _Divide__uDivide_param_u64_i64
_Divide__uDivide_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_i64
	.private_extern _Divide__Divide_param_u64_i64
_Divide__Divide_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L219
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L219
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L219:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_C
	.private_extern _Divide__uDivide_var_u64_C
_Divide__uDivide_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_C
	.private_extern _Divide__Divide_var_u64_C
_Divide__Divide_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L224
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L224
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L224:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_C
	.private_extern _Divide__uDivide_param_u64_C
_Divide__uDivide_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u64_C
	.private_extern _Divide__Divide_param_u64_C
_Divide__Divide_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L229
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L229
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L229:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u8
	.private_extern _Divide__uDivide_var_u64_u8
_Divide__uDivide_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_u8
	.private_extern _Divide__Divide_var_u64_u8
_Divide__Divide_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L234
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L234
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L234:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_u8
	.private_extern _Divide__uDivide_param_u64_u8
_Divide__uDivide_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_u8
	.private_extern _Divide__Divide_param_u64_u8
_Divide__Divide_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L239
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L239
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L239:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i8
	.private_extern _Divide__uDivide_var_u64_i8
_Divide__uDivide_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_i8
	.private_extern _Divide__Divide_var_u64_i8
_Divide__Divide_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L244
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L244
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L244:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_i8
	.private_extern _Divide__uDivide_param_u64_i8
_Divide__uDivide_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_i8
	.private_extern _Divide__Divide_param_u64_i8
_Divide__Divide_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L249
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L249
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L249:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_L
	.private_extern _Divide__uDivide_var_u64_L
_Divide__uDivide_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_L
	.private_extern _Divide__Divide_var_u64_L
_Divide__Divide_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L254
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L254
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L254:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_L
	.private_extern _Divide__uDivide_param_u64_L
_Divide__uDivide_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_L
	.private_extern _Divide__Divide_param_u64_L
_Divide__Divide_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L259
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L259
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L259:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u32
	.private_extern _Divide__uDivide_var_u64_u32
_Divide__uDivide_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u64_u32
	.private_extern _Divide__Divide_var_u64_u32
_Divide__Divide_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L264
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L264
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L264:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u64_u32
	.private_extern _Divide__uDivide_param_u64_u32
_Divide__uDivide_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u64_u32
	.private_extern _Divide__Divide_param_u64_u32
_Divide__Divide_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L269
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L269
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L269:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u16
	.private_extern _Divide__uDivide_var_LC_u16
_Divide__uDivide_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_u16
	.private_extern _Divide__Divide_var_LC_u16
_Divide__Divide_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L274
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L274
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L274:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_u16
	.private_extern _Divide__uDivide_param_LC_u16
_Divide__uDivide_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_u16
	.private_extern _Divide__Divide_param_LC_u16
_Divide__Divide_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movswq	-26(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L279
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L279
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L279:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u64
	.private_extern _Divide__uDivide_var_LC_u64
_Divide__uDivide_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_u64
	.private_extern _Divide__Divide_var_LC_u64
_Divide__Divide_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L284
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L284
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L284:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_u64
	.private_extern _Divide__uDivide_param_LC_u64
_Divide__uDivide_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_u64
	.private_extern _Divide__Divide_param_LC_u64
_Divide__Divide_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L289
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L289
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L289:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_LC
	.private_extern _Divide__uDivide_var_LC_LC
_Divide__uDivide_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_LC
	.private_extern _Divide__Divide_var_LC_LC
_Divide__Divide_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _Divide__uDivide_param_LC_LC
	.private_extern _Divide__uDivide_param_LC_LC
_Divide__uDivide_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_LC
	.private_extern _Divide__Divide_param_LC_LC
_Divide__Divide_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L298
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L298
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L298:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i16
	.private_extern _Divide__uDivide_var_LC_i16
_Divide__uDivide_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_i16
	.private_extern _Divide__Divide_var_LC_i16
_Divide__Divide_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L303
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L303
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L303:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_i16
	.private_extern _Divide__uDivide_param_LC_i16
_Divide__uDivide_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_i16
	.private_extern _Divide__Divide_param_LC_i16
_Divide__Divide_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L308
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L308
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L308:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i32
	.private_extern _Divide__uDivide_var_LC_i32
_Divide__uDivide_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_i32
	.private_extern _Divide__Divide_var_LC_i32
_Divide__Divide_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L313
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L313
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L313:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_i32
	.private_extern _Divide__uDivide_param_LC_i32
_Divide__uDivide_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_i32
	.private_extern _Divide__Divide_param_LC_i32
_Divide__Divide_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L318
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L318
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L318:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_I
	.private_extern _Divide__uDivide_var_LC_I
_Divide__uDivide_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_I
	.private_extern _Divide__Divide_var_LC_I
_Divide__Divide_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L323
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L323
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L323:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_I
	.private_extern _Divide__uDivide_param_LC_I
_Divide__uDivide_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_I
	.private_extern _Divide__Divide_param_LC_I
_Divide__Divide_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L328
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L328
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L328:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i64
	.private_extern _Divide__uDivide_var_LC_i64
_Divide__uDivide_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_i64
	.private_extern _Divide__Divide_var_LC_i64
_Divide__Divide_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L333
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L333
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L333:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_i64
	.private_extern _Divide__uDivide_param_LC_i64
_Divide__uDivide_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_i64
	.private_extern _Divide__Divide_param_LC_i64
_Divide__Divide_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L338
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L338
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L338:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_C
	.private_extern _Divide__uDivide_var_LC_C
_Divide__uDivide_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_C
	.private_extern _Divide__Divide_var_LC_C
_Divide__Divide_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L343
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L343
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L343:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_C
	.private_extern _Divide__uDivide_param_LC_C
_Divide__uDivide_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_C
	.private_extern _Divide__Divide_param_LC_C
_Divide__Divide_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L348
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L348
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L348:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u8
	.private_extern _Divide__uDivide_var_LC_u8
_Divide__uDivide_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_u8
	.private_extern _Divide__Divide_var_LC_u8
_Divide__Divide_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L353
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L353
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L353:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_u8
	.private_extern _Divide__uDivide_param_LC_u8
_Divide__uDivide_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_u8
	.private_extern _Divide__Divide_param_LC_u8
_Divide__Divide_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L358
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L358
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L358:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i8
	.private_extern _Divide__uDivide_var_LC_i8
_Divide__uDivide_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_i8
	.private_extern _Divide__Divide_var_LC_i8
_Divide__Divide_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L363
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L363
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L363:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_i8
	.private_extern _Divide__uDivide_param_LC_i8
_Divide__uDivide_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_i8
	.private_extern _Divide__Divide_param_LC_i8
_Divide__Divide_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L368
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L368
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L368:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_L
	.private_extern _Divide__uDivide_var_LC_L
_Divide__uDivide_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_L
	.private_extern _Divide__Divide_var_LC_L
_Divide__Divide_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L373
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L373
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L373:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_L
	.private_extern _Divide__uDivide_param_LC_L
_Divide__uDivide_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_L
	.private_extern _Divide__Divide_param_LC_L
_Divide__Divide_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L378
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L378
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L378:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u32
	.private_extern _Divide__uDivide_var_LC_u32
_Divide__uDivide_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_LC_u32
	.private_extern _Divide__Divide_var_LC_u32
_Divide__Divide_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L383
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L383
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L383:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_LC_u32
	.private_extern _Divide__uDivide_param_LC_u32
_Divide__uDivide_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_LC_u32
	.private_extern _Divide__Divide_param_LC_u32
_Divide__Divide_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L388
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L388
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L388:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_f64_f64
	.private_extern _Divide__Divide_var_f64_f64
_Divide__Divide_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_Divide(%rip), %xmm1
	movsd	128+_MM_Divide(%rip), %xmm0
	movapd	%xmm1, %xmm2
	divsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
	ret
.globl _Divide__Divide_param_f64_f64
	.private_extern _Divide__Divide_param_f64_f64
_Divide__Divide_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-24(%rbp), %xmm0
	divsd	-32(%rbp), %xmm0
	leave
	ret
.globl _Divide__uDivide_var_i16_u16
	.private_extern _Divide__uDivide_var_i16_u16
_Divide__uDivide_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_u16
	.private_extern _Divide__Divide_var_i16_u16
_Divide__Divide_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L397
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L397
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L397:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L398
	cmpq	$65535, -16(%rbp)
	jle	L399
L398:
	movl	$6433, %edi
	call	__m3_fault
L399:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_u16
	.private_extern _Divide__uDivide_param_i16_u16
_Divide__uDivide_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_u16
	.private_extern _Divide__Divide_param_i16_u16
_Divide__Divide_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L404
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L404
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L404:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L405
	cmpq	$65535, -16(%rbp)
	jle	L406
L405:
	movl	$6497, %edi
	call	__m3_fault
L406:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u64
	.private_extern _Divide__uDivide_var_i16_u64
_Divide__uDivide_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_u64
	.private_extern _Divide__Divide_var_i16_u64
_Divide__Divide_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L411
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L411
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L411:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_u64
	.private_extern _Divide__uDivide_param_i16_u64
_Divide__uDivide_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_u64
	.private_extern _Divide__Divide_param_i16_u64
_Divide__Divide_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L416
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L416
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L416:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_LC
	.private_extern _Divide__uDivide_var_i16_LC
_Divide__uDivide_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_LC
	.private_extern _Divide__Divide_var_i16_LC
_Divide__Divide_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L421
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L421
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L421:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_LC
	.private_extern _Divide__uDivide_param_i16_LC
_Divide__uDivide_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i16_LC
	.private_extern _Divide__Divide_param_i16_LC
_Divide__Divide_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L426
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L426
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L426:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i16
	.private_extern _Divide__uDivide_var_i16_i16
_Divide__uDivide_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_i16
	.private_extern _Divide__Divide_var_i16_i16
_Divide__Divide_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L431
	cmpq	$65535, -16(%rbp)
	jle	L432
L431:
	movl	$6817, %edi
	call	__m3_fault
L432:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_i16
	.private_extern _Divide__uDivide_param_i16_i16
_Divide__uDivide_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_i16
	.private_extern _Divide__Divide_param_i16_i16
_Divide__Divide_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L437
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L437
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L437:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L438
	cmpq	$65535, -16(%rbp)
	jle	L439
L438:
	movl	$6881, %edi
	call	__m3_fault
L439:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i32
	.private_extern _Divide__uDivide_var_i16_i32
_Divide__uDivide_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_i32
	.private_extern _Divide__Divide_var_i16_i32
_Divide__Divide_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L444
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L444
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L444:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L445
	cmpq	$65535, -16(%rbp)
	jle	L446
L445:
	movl	$6945, %edi
	call	__m3_fault
L446:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_i32
	.private_extern _Divide__uDivide_param_i16_i32
_Divide__uDivide_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_i32
	.private_extern _Divide__Divide_param_i16_i32
_Divide__Divide_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L451
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L451
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L451:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L452
	cmpq	$65535, -16(%rbp)
	jle	L453
L452:
	movl	$7009, %edi
	call	__m3_fault
L453:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_I
	.private_extern _Divide__uDivide_var_i16_I
_Divide__uDivide_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_I
	.private_extern _Divide__Divide_var_i16_I
_Divide__Divide_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L458
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L458
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L458:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L459
	cmpq	$65535, -16(%rbp)
	jle	L460
L459:
	movl	$7073, %edi
	call	__m3_fault
L460:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_I
	.private_extern _Divide__uDivide_param_i16_I
_Divide__uDivide_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_I
	.private_extern _Divide__Divide_param_i16_I
_Divide__Divide_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L465
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L465
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L465:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L466
	cmpq	$65535, -16(%rbp)
	jle	L467
L466:
	movl	$7137, %edi
	call	__m3_fault
L467:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i64
	.private_extern _Divide__uDivide_var_i16_i64
_Divide__uDivide_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_i64
	.private_extern _Divide__Divide_var_i16_i64
_Divide__Divide_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L472
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L472
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L472:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_i64
	.private_extern _Divide__uDivide_param_i16_i64
_Divide__uDivide_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_i64
	.private_extern _Divide__Divide_param_i16_i64
_Divide__Divide_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L477
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L477
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L477:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_C
	.private_extern _Divide__uDivide_var_i16_C
_Divide__uDivide_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_C
	.private_extern _Divide__Divide_var_i16_C
_Divide__Divide_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L482
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L482
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L482:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L483
	cmpq	$65535, -16(%rbp)
	jle	L484
L483:
	movl	$7329, %edi
	call	__m3_fault
L484:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_C
	.private_extern _Divide__uDivide_param_i16_C
_Divide__uDivide_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i16_C
	.private_extern _Divide__Divide_param_i16_C
_Divide__Divide_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L489
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L489
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L489:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L490
	cmpq	$65535, -16(%rbp)
	jle	L491
L490:
	movl	$7393, %edi
	call	__m3_fault
L491:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u8
	.private_extern _Divide__uDivide_var_i16_u8
_Divide__uDivide_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_u8
	.private_extern _Divide__Divide_var_i16_u8
_Divide__Divide_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L496
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L496
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L496:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L497
	cmpq	$65535, -16(%rbp)
	jle	L498
L497:
	movl	$7457, %edi
	call	__m3_fault
L498:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_u8
	.private_extern _Divide__uDivide_param_i16_u8
_Divide__uDivide_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movsbq	-19(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_u8
	.private_extern _Divide__Divide_param_i16_u8
_Divide__Divide_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movsbq	-19(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L503
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L503
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L503:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L504
	cmpq	$65535, -16(%rbp)
	jle	L505
L504:
	movl	$7521, %edi
	call	__m3_fault
L505:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i8
	.private_extern _Divide__uDivide_var_i16_i8
_Divide__uDivide_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_i8
	.private_extern _Divide__Divide_var_i16_i8
_Divide__Divide_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L510
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L510
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L510:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L511
	cmpq	$65535, -16(%rbp)
	jle	L512
L511:
	movl	$7585, %edi
	call	__m3_fault
L512:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_i8
	.private_extern _Divide__uDivide_param_i16_i8
_Divide__uDivide_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_i8
	.private_extern _Divide__Divide_param_i16_i8
_Divide__Divide_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L517
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L517
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L517:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L518
	cmpq	$65535, -16(%rbp)
	jle	L519
L518:
	movl	$7649, %edi
	call	__m3_fault
L519:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_L
	.private_extern _Divide__uDivide_var_i16_L
_Divide__uDivide_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_L
	.private_extern _Divide__Divide_var_i16_L
_Divide__Divide_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L524
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L524
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L524:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_L
	.private_extern _Divide__uDivide_param_i16_L
_Divide__uDivide_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_L
	.private_extern _Divide__Divide_param_i16_L
_Divide__Divide_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L529
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L529
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L529:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u32
	.private_extern _Divide__uDivide_var_i16_u32
_Divide__uDivide_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i16_u32
	.private_extern _Divide__Divide_var_i16_u32
_Divide__Divide_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L534
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L534
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L534:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L535
	cmpq	$65535, -16(%rbp)
	jle	L536
L535:
	movl	$7841, %edi
	call	__m3_fault
L536:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i16_u32
	.private_extern _Divide__uDivide_param_i16_u32
_Divide__uDivide_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i16_u32
	.private_extern _Divide__Divide_param_i16_u32
_Divide__Divide_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L541
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L541
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L541:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L542
	cmpq	$65535, -16(%rbp)
	jle	L543
L542:
	movl	$7905, %edi
	call	__m3_fault
L543:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u16
	.private_extern _Divide__uDivide_var_i32_u16
_Divide__uDivide_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_u16
	.private_extern _Divide__Divide_var_i32_u16
_Divide__Divide_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L548
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L548
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L548:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L549
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L550
L549:
	movl	$7969, %edi
	call	__m3_fault
L550:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_u16
	.private_extern _Divide__uDivide_param_i32_u16
_Divide__uDivide_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movswq	-22(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_u16
	.private_extern _Divide__Divide_param_i32_u16
_Divide__Divide_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movswq	-22(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L555
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L555
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L555:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L556
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L557
L556:
	movl	$8033, %edi
	call	__m3_fault
L557:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u64
	.private_extern _Divide__uDivide_var_i32_u64
_Divide__uDivide_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_u64
	.private_extern _Divide__Divide_var_i32_u64
_Divide__Divide_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L562
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L562
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L562:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_u64
	.private_extern _Divide__uDivide_param_i32_u64
_Divide__uDivide_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_u64
	.private_extern _Divide__Divide_param_i32_u64
_Divide__Divide_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L567
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L567
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L567:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_LC
	.private_extern _Divide__uDivide_var_i32_LC
_Divide__uDivide_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_LC
	.private_extern _Divide__Divide_var_i32_LC
_Divide__Divide_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L572
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L572
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L572:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_LC
	.private_extern _Divide__uDivide_param_i32_LC
_Divide__uDivide_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i32_LC
	.private_extern _Divide__Divide_param_i32_LC
_Divide__Divide_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L577
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L577
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L577:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i16
	.private_extern _Divide__uDivide_var_i32_i16
_Divide__uDivide_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_i16
	.private_extern _Divide__Divide_var_i32_i16
_Divide__Divide_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L582
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L582
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L582:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L583
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L584
L583:
	movl	$8353, %edi
	call	__m3_fault
L584:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_i16
	.private_extern _Divide__uDivide_param_i32_i16
_Divide__uDivide_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzwl	-22(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_i16
	.private_extern _Divide__Divide_param_i32_i16
_Divide__Divide_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L589
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L589
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L589:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L590
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L591
L590:
	movl	$8417, %edi
	call	__m3_fault
L591:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i32
	.private_extern _Divide__uDivide_var_i32_i32
_Divide__uDivide_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_i32
	.private_extern _Divide__Divide_var_i32_i32
_Divide__Divide_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L596
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L597
L596:
	movl	$8481, %edi
	call	__m3_fault
L597:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_i32
	.private_extern _Divide__uDivide_param_i32_i32
_Divide__uDivide_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_i32
	.private_extern _Divide__Divide_param_i32_i32
_Divide__Divide_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L602
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L602
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L602:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L603
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L604
L603:
	movl	$8545, %edi
	call	__m3_fault
L604:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_I
	.private_extern _Divide__uDivide_var_i32_I
_Divide__uDivide_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_I
	.private_extern _Divide__Divide_var_i32_I
_Divide__Divide_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L609
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L609
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L609:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L610
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L611
L610:
	movl	$8609, %edi
	call	__m3_fault
L611:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_I
	.private_extern _Divide__uDivide_param_i32_I
_Divide__uDivide_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_I
	.private_extern _Divide__Divide_param_i32_I
_Divide__Divide_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L616
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L616
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L616:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L617
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L618
L617:
	movl	$8673, %edi
	call	__m3_fault
L618:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i64
	.private_extern _Divide__uDivide_var_i32_i64
_Divide__uDivide_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_i64
	.private_extern _Divide__Divide_var_i32_i64
_Divide__Divide_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L623
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L623
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L623:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_i64
	.private_extern _Divide__uDivide_param_i32_i64
_Divide__uDivide_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_i64
	.private_extern _Divide__Divide_param_i32_i64
_Divide__Divide_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L628
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L628
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L628:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_C
	.private_extern _Divide__uDivide_var_i32_C
_Divide__uDivide_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_C
	.private_extern _Divide__Divide_var_i32_C
_Divide__Divide_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L633
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L633
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L633:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L634
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L635
L634:
	movl	$8865, %edi
	call	__m3_fault
L635:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_C
	.private_extern _Divide__uDivide_param_i32_C
_Divide__uDivide_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i32_C
	.private_extern _Divide__Divide_param_i32_C
_Divide__Divide_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L640
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L640
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L640:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L641
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L642
L641:
	movl	$8929, %edi
	call	__m3_fault
L642:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u8
	.private_extern _Divide__uDivide_var_i32_u8
_Divide__uDivide_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_u8
	.private_extern _Divide__Divide_var_i32_u8
_Divide__Divide_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L647
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L647
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L647:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L648
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L649
L648:
	movl	$8993, %edi
	call	__m3_fault
L649:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_u8
	.private_extern _Divide__uDivide_param_i32_u8
_Divide__uDivide_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movsbq	-21(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_u8
	.private_extern _Divide__Divide_param_i32_u8
_Divide__Divide_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movsbq	-21(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L654
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L654
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L654:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L655
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L656
L655:
	movl	$9057, %edi
	call	__m3_fault
L656:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i8
	.private_extern _Divide__uDivide_var_i32_i8
_Divide__uDivide_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_i8
	.private_extern _Divide__Divide_var_i32_i8
_Divide__Divide_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L661
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L661
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L661:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L662
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L663
L662:
	movl	$9121, %edi
	call	__m3_fault
L663:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_i8
	.private_extern _Divide__uDivide_param_i32_i8
_Divide__uDivide_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzbl	-21(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_i8
	.private_extern _Divide__Divide_param_i32_i8
_Divide__Divide_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L668
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L668
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L668:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L669
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L670
L669:
	movl	$9185, %edi
	call	__m3_fault
L670:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_L
	.private_extern _Divide__uDivide_var_i32_L
_Divide__uDivide_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_L
	.private_extern _Divide__Divide_var_i32_L
_Divide__Divide_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L675
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L675
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L675:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_L
	.private_extern _Divide__uDivide_param_i32_L
_Divide__uDivide_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_L
	.private_extern _Divide__Divide_param_i32_L
_Divide__Divide_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L680
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L680
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L680:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u32
	.private_extern _Divide__uDivide_var_i32_u32
_Divide__uDivide_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i32_u32
	.private_extern _Divide__Divide_var_i32_u32
_Divide__Divide_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L685
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L685
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L685:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L686
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L687
L686:
	movl	$9377, %edi
	call	__m3_fault
L687:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i32_u32
	.private_extern _Divide__uDivide_param_i32_u32
_Divide__uDivide_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i32_u32
	.private_extern _Divide__Divide_param_i32_u32
_Divide__Divide_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L692
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L692
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L692:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L693
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L694
L693:
	movl	$9441, %edi
	call	__m3_fault
L694:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u16
	.private_extern _Divide__uDivide_var_I_u16
_Divide__uDivide_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_u16
	.private_extern _Divide__Divide_var_I_u16
_Divide__Divide_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L699
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L699
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L699:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_u16
	.private_extern _Divide__uDivide_param_I_u16
_Divide__uDivide_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movswq	-26(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_u16
	.private_extern _Divide__Divide_param_I_u16
_Divide__Divide_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L704
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L704
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L704:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u64
	.private_extern _Divide__uDivide_var_I_u64
_Divide__uDivide_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_u64
	.private_extern _Divide__Divide_var_I_u64
_Divide__Divide_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L709
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L709
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L709:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_u64
	.private_extern _Divide__uDivide_param_I_u64
_Divide__uDivide_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_u64
	.private_extern _Divide__Divide_param_I_u64
_Divide__Divide_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L714
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L714
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L714:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_LC
	.private_extern _Divide__uDivide_var_I_LC
_Divide__uDivide_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_LC
	.private_extern _Divide__Divide_var_I_LC
_Divide__Divide_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L719
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L719
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L719:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_LC
	.private_extern _Divide__uDivide_param_I_LC
_Divide__uDivide_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_I_LC
	.private_extern _Divide__Divide_param_I_LC
_Divide__Divide_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L724
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L724
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L724:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i16
	.private_extern _Divide__uDivide_var_I_i16
_Divide__uDivide_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_i16
	.private_extern _Divide__Divide_var_I_i16
_Divide__Divide_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L729
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L729
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L729:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_i16
	.private_extern _Divide__uDivide_param_I_i16
_Divide__uDivide_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_i16
	.private_extern _Divide__Divide_param_I_i16
_Divide__Divide_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L734
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L734
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L734:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i32
	.private_extern _Divide__uDivide_var_I_i32
_Divide__uDivide_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_i32
	.private_extern _Divide__Divide_var_I_i32
_Divide__Divide_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L739
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L739
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L739:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_i32
	.private_extern _Divide__uDivide_param_I_i32
_Divide__uDivide_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_i32
	.private_extern _Divide__Divide_param_I_i32
_Divide__Divide_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L744
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L744
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L744:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_I
	.private_extern _Divide__uDivide_var_I_I
_Divide__uDivide_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_I
	.private_extern _Divide__Divide_var_I_I
_Divide__Divide_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _Divide__uDivide_param_I_I
	.private_extern _Divide__uDivide_param_I_I
_Divide__uDivide_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_I
	.private_extern _Divide__Divide_param_I_I
_Divide__Divide_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L753
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L753
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L753:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i64
	.private_extern _Divide__uDivide_var_I_i64
_Divide__uDivide_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_i64
	.private_extern _Divide__Divide_var_I_i64
_Divide__Divide_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L758
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L758
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L758:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_i64
	.private_extern _Divide__uDivide_param_I_i64
_Divide__uDivide_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_i64
	.private_extern _Divide__Divide_param_I_i64
_Divide__Divide_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L763
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L763
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L763:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_C
	.private_extern _Divide__uDivide_var_I_C
_Divide__uDivide_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_C
	.private_extern _Divide__Divide_var_I_C
_Divide__Divide_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L768
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L768
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L768:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_C
	.private_extern _Divide__uDivide_param_I_C
_Divide__uDivide_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_I_C
	.private_extern _Divide__Divide_param_I_C
_Divide__Divide_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L773
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L773
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L773:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u8
	.private_extern _Divide__uDivide_var_I_u8
_Divide__uDivide_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_u8
	.private_extern _Divide__Divide_var_I_u8
_Divide__Divide_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L778
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L778
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L778:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_u8
	.private_extern _Divide__uDivide_param_I_u8
_Divide__uDivide_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_u8
	.private_extern _Divide__Divide_param_I_u8
_Divide__Divide_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L783
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L783
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L783:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i8
	.private_extern _Divide__uDivide_var_I_i8
_Divide__uDivide_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_i8
	.private_extern _Divide__Divide_var_I_i8
_Divide__Divide_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L788
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L788
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L788:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_i8
	.private_extern _Divide__uDivide_param_I_i8
_Divide__uDivide_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_i8
	.private_extern _Divide__Divide_param_I_i8
_Divide__Divide_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L793
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L793
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L793:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_L
	.private_extern _Divide__uDivide_var_I_L
_Divide__uDivide_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_L
	.private_extern _Divide__Divide_var_I_L
_Divide__Divide_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L798
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L798
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L798:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_L
	.private_extern _Divide__uDivide_param_I_L
_Divide__uDivide_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_L
	.private_extern _Divide__Divide_param_I_L
_Divide__Divide_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L803
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L803
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L803:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u32
	.private_extern _Divide__uDivide_var_I_u32
_Divide__uDivide_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_I_u32
	.private_extern _Divide__Divide_var_I_u32
_Divide__Divide_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L808
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L808
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L808:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_I_u32
	.private_extern _Divide__uDivide_param_I_u32
_Divide__uDivide_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_I_u32
	.private_extern _Divide__Divide_param_I_u32
_Divide__Divide_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L813
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L813
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L813:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u16
	.private_extern _Divide__uDivide_var_i64_u16
_Divide__uDivide_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_u16
	.private_extern _Divide__Divide_var_i64_u16
_Divide__Divide_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L818
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L818
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L818:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_u16
	.private_extern _Divide__uDivide_param_i64_u16
_Divide__uDivide_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movswq	-26(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_u16
	.private_extern _Divide__Divide_param_i64_u16
_Divide__Divide_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L823
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L823
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L823:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u64
	.private_extern _Divide__uDivide_var_i64_u64
_Divide__uDivide_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_u64
	.private_extern _Divide__Divide_var_i64_u64
_Divide__Divide_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L828
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L828
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L828:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_u64
	.private_extern _Divide__uDivide_param_i64_u64
_Divide__uDivide_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_u64
	.private_extern _Divide__Divide_param_i64_u64
_Divide__Divide_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L833
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L833
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L833:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_LC
	.private_extern _Divide__uDivide_var_i64_LC
_Divide__uDivide_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_LC
	.private_extern _Divide__Divide_var_i64_LC
_Divide__Divide_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L838
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L838
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L838:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_LC
	.private_extern _Divide__uDivide_param_i64_LC
_Divide__uDivide_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i64_LC
	.private_extern _Divide__Divide_param_i64_LC
_Divide__Divide_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L843
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L843
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L843:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i16
	.private_extern _Divide__uDivide_var_i64_i16
_Divide__uDivide_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_i16
	.private_extern _Divide__Divide_var_i64_i16
_Divide__Divide_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L848
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L848
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L848:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_i16
	.private_extern _Divide__uDivide_param_i64_i16
_Divide__uDivide_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_i16
	.private_extern _Divide__Divide_param_i64_i16
_Divide__Divide_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L853
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L853
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L853:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i32
	.private_extern _Divide__uDivide_var_i64_i32
_Divide__uDivide_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_i32
	.private_extern _Divide__Divide_var_i64_i32
_Divide__Divide_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L858
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L858
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L858:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_i32
	.private_extern _Divide__uDivide_param_i64_i32
_Divide__uDivide_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_i32
	.private_extern _Divide__Divide_param_i64_i32
_Divide__Divide_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L863
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L863
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L863:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_I
	.private_extern _Divide__uDivide_var_i64_I
_Divide__uDivide_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_I
	.private_extern _Divide__Divide_var_i64_I
_Divide__Divide_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L868
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L868
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L868:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_I
	.private_extern _Divide__uDivide_param_i64_I
_Divide__uDivide_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_I
	.private_extern _Divide__Divide_param_i64_I
_Divide__Divide_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L873
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L873
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L873:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i64
	.private_extern _Divide__uDivide_var_i64_i64
_Divide__uDivide_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_i64
	.private_extern _Divide__Divide_var_i64_i64
_Divide__Divide_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _Divide__uDivide_param_i64_i64
	.private_extern _Divide__uDivide_param_i64_i64
_Divide__uDivide_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_i64
	.private_extern _Divide__Divide_param_i64_i64
_Divide__Divide_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L882
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L882
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L882:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_C
	.private_extern _Divide__uDivide_var_i64_C
_Divide__uDivide_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_C
	.private_extern _Divide__Divide_var_i64_C
_Divide__Divide_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L887
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L887
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L887:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_C
	.private_extern _Divide__uDivide_param_i64_C
_Divide__uDivide_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i64_C
	.private_extern _Divide__Divide_param_i64_C
_Divide__Divide_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L892
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L892
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L892:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u8
	.private_extern _Divide__uDivide_var_i64_u8
_Divide__uDivide_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_u8
	.private_extern _Divide__Divide_var_i64_u8
_Divide__Divide_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L897
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L897
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L897:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_u8
	.private_extern _Divide__uDivide_param_i64_u8
_Divide__uDivide_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_u8
	.private_extern _Divide__Divide_param_i64_u8
_Divide__Divide_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L902
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L902
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L902:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i8
	.private_extern _Divide__uDivide_var_i64_i8
_Divide__uDivide_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_i8
	.private_extern _Divide__Divide_var_i64_i8
_Divide__Divide_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L907
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L907
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L907:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_i8
	.private_extern _Divide__uDivide_param_i64_i8
_Divide__uDivide_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_i8
	.private_extern _Divide__Divide_param_i64_i8
_Divide__Divide_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L912
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L912
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L912:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_L
	.private_extern _Divide__uDivide_var_i64_L
_Divide__uDivide_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_L
	.private_extern _Divide__Divide_var_i64_L
_Divide__Divide_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L917
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L917
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L917:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_L
	.private_extern _Divide__uDivide_param_i64_L
_Divide__uDivide_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_L
	.private_extern _Divide__Divide_param_i64_L
_Divide__Divide_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L922
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L922
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L922:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u32
	.private_extern _Divide__uDivide_var_i64_u32
_Divide__uDivide_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i64_u32
	.private_extern _Divide__Divide_var_i64_u32
_Divide__Divide_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L927
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L927
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L927:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i64_u32
	.private_extern _Divide__uDivide_param_i64_u32
_Divide__uDivide_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i64_u32
	.private_extern _Divide__Divide_param_i64_u32
_Divide__Divide_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L932
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L932
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L932:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u16
	.private_extern _Divide__uDivide_var_C_u16
_Divide__uDivide_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_u16
	.private_extern _Divide__Divide_var_C_u16
_Divide__Divide_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L937
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L937
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L937:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L938
	movl	$12577, %edi
	call	__m3_fault
L938:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_u16
	.private_extern _Divide__uDivide_param_C_u16
_Divide__uDivide_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_u16
	.private_extern _Divide__Divide_param_C_u16
_Divide__Divide_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movswq	-26(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L943
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L943
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L943:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L944
	movl	$12641, %edi
	call	__m3_fault
L944:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u64
	.private_extern _Divide__uDivide_var_C_u64
_Divide__uDivide_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_u64
	.private_extern _Divide__Divide_var_C_u64
_Divide__Divide_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L949
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L949
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L949:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_u64
	.private_extern _Divide__uDivide_param_C_u64
_Divide__uDivide_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_u64
	.private_extern _Divide__Divide_param_C_u64
_Divide__Divide_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L954
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L954
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L954:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_LC
	.private_extern _Divide__uDivide_var_C_LC
_Divide__uDivide_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_LC
	.private_extern _Divide__Divide_var_C_LC
_Divide__Divide_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L959
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L959
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L959:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_LC
	.private_extern _Divide__uDivide_param_C_LC
_Divide__uDivide_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_LC
	.private_extern _Divide__Divide_param_C_LC
_Divide__Divide_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L964
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L964
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L964:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i16
	.private_extern _Divide__uDivide_var_C_i16
_Divide__uDivide_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_i16
	.private_extern _Divide__Divide_var_C_i16
_Divide__Divide_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L969
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L969
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L969:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L970
	movl	$12961, %edi
	call	__m3_fault
L970:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_i16
	.private_extern _Divide__uDivide_param_C_i16
_Divide__uDivide_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_i16
	.private_extern _Divide__Divide_param_C_i16
_Divide__Divide_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L975
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L975
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L975:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L976
	movl	$13025, %edi
	call	__m3_fault
L976:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i32
	.private_extern _Divide__uDivide_var_C_i32
_Divide__uDivide_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_i32
	.private_extern _Divide__Divide_var_C_i32
_Divide__Divide_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L981
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L981
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L981:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L982
	movl	$13089, %edi
	call	__m3_fault
L982:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_i32
	.private_extern _Divide__uDivide_param_C_i32
_Divide__uDivide_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_i32
	.private_extern _Divide__Divide_param_C_i32
_Divide__Divide_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L987
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L987
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L987:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L988
	movl	$13153, %edi
	call	__m3_fault
L988:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_I
	.private_extern _Divide__uDivide_var_C_I
_Divide__uDivide_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_I
	.private_extern _Divide__Divide_var_C_I
_Divide__Divide_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L993
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L993
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L993:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L994
	movl	$13217, %edi
	call	__m3_fault
L994:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_I
	.private_extern _Divide__uDivide_param_C_I
_Divide__uDivide_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_I
	.private_extern _Divide__Divide_param_C_I
_Divide__Divide_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L999
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L999
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L999:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1000
	movl	$13281, %edi
	call	__m3_fault
L1000:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i64
	.private_extern _Divide__uDivide_var_C_i64
_Divide__uDivide_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_i64
	.private_extern _Divide__Divide_var_C_i64
_Divide__Divide_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1005
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1005
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1005:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_i64
	.private_extern _Divide__uDivide_param_C_i64
_Divide__uDivide_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_i64
	.private_extern _Divide__Divide_param_C_i64
_Divide__Divide_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1010
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1010
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1010:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_C
	.private_extern _Divide__uDivide_var_C_C
_Divide__uDivide_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_C
	.private_extern _Divide__Divide_var_C_C
_Divide__Divide_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1015
	movl	$13473, %edi
	call	__m3_fault
L1015:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_C
	.private_extern _Divide__uDivide_param_C_C
_Divide__uDivide_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_C
	.private_extern _Divide__Divide_param_C_C
_Divide__Divide_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1020
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1020
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1020:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1021
	movl	$13537, %edi
	call	__m3_fault
L1021:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u8
	.private_extern _Divide__uDivide_var_C_u8
_Divide__uDivide_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_u8
	.private_extern _Divide__Divide_var_C_u8
_Divide__Divide_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1026
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1026
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1026:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1027
	movl	$13601, %edi
	call	__m3_fault
L1027:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_u8
	.private_extern _Divide__uDivide_param_C_u8
_Divide__uDivide_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_u8
	.private_extern _Divide__Divide_param_C_u8
_Divide__Divide_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1032
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1032
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1032:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1033
	movl	$13665, %edi
	call	__m3_fault
L1033:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i8
	.private_extern _Divide__uDivide_var_C_i8
_Divide__uDivide_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_i8
	.private_extern _Divide__Divide_var_C_i8
_Divide__Divide_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1038
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1038
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1038:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1039
	movl	$13729, %edi
	call	__m3_fault
L1039:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_i8
	.private_extern _Divide__uDivide_param_C_i8
_Divide__uDivide_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_i8
	.private_extern _Divide__Divide_param_C_i8
_Divide__Divide_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1044
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1044
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1044:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1045
	movl	$13793, %edi
	call	__m3_fault
L1045:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_L
	.private_extern _Divide__uDivide_var_C_L
_Divide__uDivide_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_L
	.private_extern _Divide__Divide_var_C_L
_Divide__Divide_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1050
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1050
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1050:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_L
	.private_extern _Divide__uDivide_param_C_L
_Divide__uDivide_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_L
	.private_extern _Divide__Divide_param_C_L
_Divide__Divide_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1055
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1055
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1055:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u32
	.private_extern _Divide__uDivide_var_C_u32
_Divide__uDivide_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_C_u32
	.private_extern _Divide__Divide_var_C_u32
_Divide__Divide_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1060
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1060
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1060:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1061
	movl	$13985, %edi
	call	__m3_fault
L1061:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_C_u32
	.private_extern _Divide__uDivide_param_C_u32
_Divide__uDivide_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	leave
	ret
.globl _Divide__Divide_param_C_u32
	.private_extern _Divide__Divide_param_C_u32
_Divide__Divide_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1066
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1066
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1066:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1067
	movl	$14049, %edi
	call	__m3_fault
L1067:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_f32_f32
	.private_extern _Divide__Divide_var_f32_f32
_Divide__Divide_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_Divide(%rip), %xmm1
	movss	168+_MM_Divide(%rip), %xmm0
	movaps	%xmm1, %xmm2
	divss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	leave
	ret
.globl _Divide__Divide_param_f32_f32
	.private_extern _Divide__Divide_param_f32_f32
_Divide__Divide_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-20(%rbp), %xmm0
	divss	-24(%rbp), %xmm0
	leave
	ret
.globl _Divide__uDivide_var_u8_u16
	.private_extern _Divide__uDivide_var_u8_u16
_Divide__uDivide_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_u16
	.private_extern _Divide__Divide_var_u8_u16
_Divide__Divide_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1076
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1076
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1076:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1077
	cmpq	$127, -16(%rbp)
	jle	L1078
L1077:
	movl	$14177, %edi
	call	__m3_fault
L1078:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_u16
	.private_extern _Divide__uDivide_param_u8_u16
_Divide__uDivide_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_u16
	.private_extern _Divide__Divide_param_u8_u16
_Divide__Divide_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1083
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1083
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1083:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1084
	cmpq	$127, -16(%rbp)
	jle	L1085
L1084:
	movl	$14241, %edi
	call	__m3_fault
L1085:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u64
	.private_extern _Divide__uDivide_var_u8_u64
_Divide__uDivide_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_u64
	.private_extern _Divide__Divide_var_u8_u64
_Divide__Divide_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1090
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1090
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1090:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_u64
	.private_extern _Divide__uDivide_param_u8_u64
_Divide__uDivide_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_u64
	.private_extern _Divide__Divide_param_u8_u64
_Divide__Divide_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1095
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1095
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1095:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_LC
	.private_extern _Divide__uDivide_var_u8_LC
_Divide__uDivide_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_LC
	.private_extern _Divide__Divide_var_u8_LC
_Divide__Divide_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1100
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1100
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1100:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_LC
	.private_extern _Divide__uDivide_param_u8_LC
_Divide__uDivide_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u8_LC
	.private_extern _Divide__Divide_param_u8_LC
_Divide__Divide_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1105
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1105
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1105:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i16
	.private_extern _Divide__uDivide_var_u8_i16
_Divide__uDivide_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_i16
	.private_extern _Divide__Divide_var_u8_i16
_Divide__Divide_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1110
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1110
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1110:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1111
	cmpq	$127, -16(%rbp)
	jle	L1112
L1111:
	movl	$14561, %edi
	call	__m3_fault
L1112:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_i16
	.private_extern _Divide__uDivide_param_u8_i16
_Divide__uDivide_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_i16
	.private_extern _Divide__Divide_param_u8_i16
_Divide__Divide_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rax
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
	je	L1117
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1117
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1117:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1118
	cmpq	$127, -16(%rbp)
	jle	L1119
L1118:
	movl	$14625, %edi
	call	__m3_fault
L1119:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i32
	.private_extern _Divide__uDivide_var_u8_i32
_Divide__uDivide_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_i32
	.private_extern _Divide__Divide_var_u8_i32
_Divide__Divide_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1124
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1124
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1124:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1125
	cmpq	$127, -16(%rbp)
	jle	L1126
L1125:
	movl	$14689, %edi
	call	__m3_fault
L1126:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_i32
	.private_extern _Divide__uDivide_param_u8_i32
_Divide__uDivide_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_i32
	.private_extern _Divide__Divide_param_u8_i32
_Divide__Divide_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
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
	je	L1131
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1131
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1131:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1132
	cmpq	$127, -16(%rbp)
	jle	L1133
L1132:
	movl	$14753, %edi
	call	__m3_fault
L1133:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_I
	.private_extern _Divide__uDivide_var_u8_I
_Divide__uDivide_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_I
	.private_extern _Divide__Divide_var_u8_I
_Divide__Divide_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1138
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1138
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1138:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1139
	cmpq	$127, -16(%rbp)
	jle	L1140
L1139:
	movl	$14817, %edi
	call	__m3_fault
L1140:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_I
	.private_extern _Divide__uDivide_param_u8_I
_Divide__uDivide_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_I
	.private_extern _Divide__Divide_param_u8_I
_Divide__Divide_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1145
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1145
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1145:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1146
	cmpq	$127, -16(%rbp)
	jle	L1147
L1146:
	movl	$14881, %edi
	call	__m3_fault
L1147:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i64
	.private_extern _Divide__uDivide_var_u8_i64
_Divide__uDivide_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_i64
	.private_extern _Divide__Divide_var_u8_i64
_Divide__Divide_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1152
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1152
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1152:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_i64
	.private_extern _Divide__uDivide_param_u8_i64
_Divide__uDivide_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_i64
	.private_extern _Divide__Divide_param_u8_i64
_Divide__Divide_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1157
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1157
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1157:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_C
	.private_extern _Divide__uDivide_var_u8_C
_Divide__uDivide_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_C
	.private_extern _Divide__Divide_var_u8_C
_Divide__Divide_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1162
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1162
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1162:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1163
	cmpq	$127, -16(%rbp)
	jle	L1164
L1163:
	movl	$15073, %edi
	call	__m3_fault
L1164:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_C
	.private_extern _Divide__uDivide_param_u8_C
_Divide__uDivide_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u8_C
	.private_extern _Divide__Divide_param_u8_C
_Divide__Divide_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1169
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1169
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1169:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1170
	cmpq	$127, -16(%rbp)
	jle	L1171
L1170:
	movl	$15137, %edi
	call	__m3_fault
L1171:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u8
	.private_extern _Divide__uDivide_var_u8_u8
_Divide__uDivide_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_u8
	.private_extern _Divide__Divide_var_u8_u8
_Divide__Divide_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1176
	cmpq	$127, -16(%rbp)
	jle	L1177
L1176:
	movl	$15201, %edi
	call	__m3_fault
L1177:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_u8
	.private_extern _Divide__uDivide_param_u8_u8
_Divide__uDivide_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movsbq	-18(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_u8
	.private_extern _Divide__Divide_param_u8_u8
_Divide__Divide_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movsbq	-18(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1182
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1182
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1182:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1183
	cmpq	$127, -16(%rbp)
	jle	L1184
L1183:
	movl	$15265, %edi
	call	__m3_fault
L1184:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i8
	.private_extern _Divide__uDivide_var_u8_i8
_Divide__uDivide_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_i8
	.private_extern _Divide__Divide_var_u8_i8
_Divide__Divide_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1189
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1189
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1189:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1190
	cmpq	$127, -16(%rbp)
	jle	L1191
L1190:
	movl	$15329, %edi
	call	__m3_fault
L1191:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_i8
	.private_extern _Divide__uDivide_param_u8_i8
_Divide__uDivide_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_i8
	.private_extern _Divide__Divide_param_u8_i8
_Divide__Divide_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
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
	je	L1196
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1196
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1196:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1197
	cmpq	$127, -16(%rbp)
	jle	L1198
L1197:
	movl	$15393, %edi
	call	__m3_fault
L1198:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_L
	.private_extern _Divide__uDivide_var_u8_L
_Divide__uDivide_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_L
	.private_extern _Divide__Divide_var_u8_L
_Divide__Divide_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1203
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1203
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1203:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_L
	.private_extern _Divide__uDivide_param_u8_L
_Divide__uDivide_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_L
	.private_extern _Divide__Divide_param_u8_L
_Divide__Divide_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1208
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1208
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1208:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u32
	.private_extern _Divide__uDivide_var_u8_u32
_Divide__uDivide_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u8_u32
	.private_extern _Divide__Divide_var_u8_u32
_Divide__Divide_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1213
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1213
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1213:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1214
	cmpq	$127, -16(%rbp)
	jle	L1215
L1214:
	movl	$15585, %edi
	call	__m3_fault
L1215:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u8_u32
	.private_extern _Divide__uDivide_param_u8_u32
_Divide__uDivide_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -48(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u8_u32
	.private_extern _Divide__Divide_param_u8_u32
_Divide__Divide_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -40(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1220
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1220
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1220:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1221
	cmpq	$127, -16(%rbp)
	jle	L1222
L1221:
	movl	$15649, %edi
	call	__m3_fault
L1222:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u16
	.private_extern _Divide__uDivide_var_i8_u16
_Divide__uDivide_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_u16
	.private_extern _Divide__Divide_var_i8_u16
_Divide__Divide_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1227
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1227
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1227:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1228
	cmpq	$255, -16(%rbp)
	jle	L1229
L1228:
	movl	$15713, %edi
	call	__m3_fault
L1229:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_u16
	.private_extern _Divide__uDivide_param_i8_u16
_Divide__uDivide_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_u16
	.private_extern _Divide__Divide_param_i8_u16
_Divide__Divide_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movswq	-20(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1234
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1234
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1234:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1235
	cmpq	$255, -16(%rbp)
	jle	L1236
L1235:
	movl	$15777, %edi
	call	__m3_fault
L1236:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u64
	.private_extern _Divide__uDivide_var_i8_u64
_Divide__uDivide_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_u64
	.private_extern _Divide__Divide_var_i8_u64
_Divide__Divide_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1241
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1241
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1241:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_u64
	.private_extern _Divide__uDivide_param_i8_u64
_Divide__uDivide_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_u64
	.private_extern _Divide__Divide_param_i8_u64
_Divide__Divide_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1246
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1246
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1246:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_LC
	.private_extern _Divide__uDivide_var_i8_LC
_Divide__uDivide_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_LC
	.private_extern _Divide__Divide_var_i8_LC
_Divide__Divide_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1251
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1251
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1251:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_LC
	.private_extern _Divide__uDivide_param_i8_LC
_Divide__uDivide_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i8_LC
	.private_extern _Divide__Divide_param_i8_LC
_Divide__Divide_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1256
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1256
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1256:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i16
	.private_extern _Divide__uDivide_var_i8_i16
_Divide__uDivide_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_i16
	.private_extern _Divide__Divide_var_i8_i16
_Divide__Divide_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1261
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1261
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1261:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1262
	cmpq	$255, -16(%rbp)
	jle	L1263
L1262:
	movl	$16097, %edi
	call	__m3_fault
L1263:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_i16
	.private_extern _Divide__uDivide_param_i8_i16
_Divide__uDivide_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_i16
	.private_extern _Divide__Divide_param_i8_i16
_Divide__Divide_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L1268
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1268
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1268:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1269
	cmpq	$255, -16(%rbp)
	jle	L1270
L1269:
	movl	$16161, %edi
	call	__m3_fault
L1270:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i32
	.private_extern _Divide__uDivide_var_i8_i32
_Divide__uDivide_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_i32
	.private_extern _Divide__Divide_var_i8_i32
_Divide__Divide_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1275
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1275
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1275:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1276
	cmpq	$255, -16(%rbp)
	jle	L1277
L1276:
	movl	$16225, %edi
	call	__m3_fault
L1277:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_i32
	.private_extern _Divide__uDivide_param_i8_i32
_Divide__uDivide_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_i32
	.private_extern _Divide__Divide_param_i8_i32
_Divide__Divide_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L1282
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1282
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1282:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1283
	cmpq	$255, -16(%rbp)
	jle	L1284
L1283:
	movl	$16289, %edi
	call	__m3_fault
L1284:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_I
	.private_extern _Divide__uDivide_var_i8_I
_Divide__uDivide_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_I
	.private_extern _Divide__Divide_var_i8_I
_Divide__Divide_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1289
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1289
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1289:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1290
	cmpq	$255, -16(%rbp)
	jle	L1291
L1290:
	movl	$16353, %edi
	call	__m3_fault
L1291:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_I
	.private_extern _Divide__uDivide_param_i8_I
_Divide__uDivide_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_I
	.private_extern _Divide__Divide_param_i8_I
_Divide__Divide_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1296
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1296
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1296:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1297
	cmpq	$255, -16(%rbp)
	jle	L1298
L1297:
	movl	$16417, %edi
	call	__m3_fault
L1298:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i64
	.private_extern _Divide__uDivide_var_i8_i64
_Divide__uDivide_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_i64
	.private_extern _Divide__Divide_var_i8_i64
_Divide__Divide_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1303
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1303
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1303:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_i64
	.private_extern _Divide__uDivide_param_i8_i64
_Divide__uDivide_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_i64
	.private_extern _Divide__Divide_param_i8_i64
_Divide__Divide_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1308
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1308
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1308:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_C
	.private_extern _Divide__uDivide_var_i8_C
_Divide__uDivide_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_C
	.private_extern _Divide__Divide_var_i8_C
_Divide__Divide_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1313
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1313
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1313:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1314
	cmpq	$255, -16(%rbp)
	jle	L1315
L1314:
	movl	$16609, %edi
	call	__m3_fault
L1315:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_C
	.private_extern _Divide__uDivide_param_i8_C
_Divide__uDivide_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_i8_C
	.private_extern _Divide__Divide_param_i8_C
_Divide__Divide_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1320
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1320
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1320:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1321
	cmpq	$255, -16(%rbp)
	jle	L1322
L1321:
	movl	$16673, %edi
	call	__m3_fault
L1322:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u8
	.private_extern _Divide__uDivide_var_i8_u8
_Divide__uDivide_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_u8
	.private_extern _Divide__Divide_var_i8_u8
_Divide__Divide_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1327
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1327
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1327:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1328
	cmpq	$255, -16(%rbp)
	jle	L1329
L1328:
	movl	$16737, %edi
	call	__m3_fault
L1329:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_u8
	.private_extern _Divide__uDivide_param_i8_u8
_Divide__uDivide_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movsbq	-18(%rbp),%rdx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_u8
	.private_extern _Divide__Divide_param_i8_u8
_Divide__Divide_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movsbq	-18(%rbp),%rdx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1334
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1334
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1334:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1335
	cmpq	$255, -16(%rbp)
	jle	L1336
L1335:
	movl	$16801, %edi
	call	__m3_fault
L1336:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i8
	.private_extern _Divide__uDivide_var_i8_i8
_Divide__uDivide_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_i8
	.private_extern _Divide__Divide_var_i8_i8
_Divide__Divide_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1341
	cmpq	$255, -16(%rbp)
	jle	L1342
L1341:
	movl	$16865, %edi
	call	__m3_fault
L1342:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_i8
	.private_extern _Divide__uDivide_param_i8_i8
_Divide__uDivide_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_i8
	.private_extern _Divide__Divide_param_i8_i8
_Divide__Divide_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
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
	je	L1347
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1347
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1347:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1348
	cmpq	$255, -16(%rbp)
	jle	L1349
L1348:
	movl	$16929, %edi
	call	__m3_fault
L1349:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_L
	.private_extern _Divide__uDivide_var_i8_L
_Divide__uDivide_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_L
	.private_extern _Divide__Divide_var_i8_L
_Divide__Divide_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1354
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1354
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1354:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_L
	.private_extern _Divide__uDivide_param_i8_L
_Divide__uDivide_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_L
	.private_extern _Divide__Divide_param_i8_L
_Divide__Divide_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1359
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1359
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1359:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u32
	.private_extern _Divide__uDivide_var_i8_u32
_Divide__uDivide_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_i8_u32
	.private_extern _Divide__Divide_var_i8_u32
_Divide__Divide_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1364
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1364
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1364:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1365
	cmpq	$255, -16(%rbp)
	jle	L1366
L1365:
	movl	$17121, %edi
	call	__m3_fault
L1366:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_i8_u32
	.private_extern _Divide__uDivide_param_i8_u32
_Divide__uDivide_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_i8_u32
	.private_extern _Divide__Divide_param_i8_u32
_Divide__Divide_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1371
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1371
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1371:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1372
	cmpq	$255, -16(%rbp)
	jle	L1373
L1372:
	movl	$17185, %edi
	call	__m3_fault
L1373:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_u16
	.private_extern _Divide__uDivide_var_L_u16
_Divide__uDivide_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_u16
	.private_extern _Divide__Divide_var_L_u16
_Divide__Divide_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1378
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1378
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1378:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_u16
	.private_extern _Divide__uDivide_param_L_u16
_Divide__uDivide_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movswq	-26(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_u16
	.private_extern _Divide__Divide_param_L_u16
_Divide__Divide_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1383
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1383
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1383:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_u64
	.private_extern _Divide__uDivide_var_L_u64
_Divide__uDivide_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_u64
	.private_extern _Divide__Divide_var_L_u64
_Divide__Divide_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1388
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1388
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1388:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_u64
	.private_extern _Divide__uDivide_param_L_u64
_Divide__uDivide_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_u64
	.private_extern _Divide__Divide_param_L_u64
_Divide__Divide_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1393
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1393
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1393:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_LC
	.private_extern _Divide__uDivide_var_L_LC
_Divide__uDivide_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_LC
	.private_extern _Divide__Divide_var_L_LC
_Divide__Divide_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1398
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1398
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1398:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_LC
	.private_extern _Divide__uDivide_param_L_LC
_Divide__uDivide_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_L_LC
	.private_extern _Divide__Divide_param_L_LC
_Divide__Divide_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1403
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1403
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1403:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_i16
	.private_extern _Divide__uDivide_var_L_i16
_Divide__uDivide_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_i16
	.private_extern _Divide__Divide_var_L_i16
_Divide__Divide_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1408
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1408
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1408:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_i16
	.private_extern _Divide__uDivide_param_L_i16
_Divide__uDivide_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_i16
	.private_extern _Divide__Divide_param_L_i16
_Divide__Divide_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1413
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1413
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1413:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_i32
	.private_extern _Divide__uDivide_var_L_i32
_Divide__uDivide_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_i32
	.private_extern _Divide__Divide_var_L_i32
_Divide__Divide_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1418
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1418
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1418:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_i32
	.private_extern _Divide__uDivide_param_L_i32
_Divide__uDivide_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_i32
	.private_extern _Divide__Divide_param_L_i32
_Divide__Divide_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1423
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1423
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1423:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_I
	.private_extern _Divide__uDivide_var_L_I
_Divide__uDivide_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_I
	.private_extern _Divide__Divide_var_L_I
_Divide__Divide_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1428
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1428
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1428:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_I
	.private_extern _Divide__uDivide_param_L_I
_Divide__uDivide_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_I
	.private_extern _Divide__Divide_param_L_I
_Divide__Divide_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1433
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1433
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1433:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_i64
	.private_extern _Divide__uDivide_var_L_i64
_Divide__uDivide_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_i64
	.private_extern _Divide__Divide_var_L_i64
_Divide__Divide_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1438
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1438
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1438:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_i64
	.private_extern _Divide__uDivide_param_L_i64
_Divide__uDivide_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_i64
	.private_extern _Divide__Divide_param_L_i64
_Divide__Divide_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1443
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1443
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1443:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_C
	.private_extern _Divide__uDivide_var_L_C
_Divide__uDivide_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_C
	.private_extern _Divide__Divide_var_L_C
_Divide__Divide_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1448
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1448
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1448:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_C
	.private_extern _Divide__uDivide_param_L_C
_Divide__uDivide_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_L_C
	.private_extern _Divide__Divide_param_L_C
_Divide__Divide_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1453
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1453
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1453:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_u8
	.private_extern _Divide__uDivide_var_L_u8
_Divide__uDivide_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_u8
	.private_extern _Divide__Divide_var_L_u8
_Divide__Divide_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1458
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1458
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1458:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_u8
	.private_extern _Divide__uDivide_param_L_u8
_Divide__uDivide_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_u8
	.private_extern _Divide__Divide_param_L_u8
_Divide__Divide_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1463
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1463
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1463:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_i8
	.private_extern _Divide__uDivide_var_L_i8
_Divide__uDivide_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_i8
	.private_extern _Divide__Divide_var_L_i8
_Divide__Divide_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1468
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1468
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1468:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_i8
	.private_extern _Divide__uDivide_param_L_i8
_Divide__uDivide_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_i8
	.private_extern _Divide__Divide_param_L_i8
_Divide__Divide_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1473
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1473
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1473:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_L
	.private_extern _Divide__uDivide_var_L_L
_Divide__uDivide_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_L
	.private_extern _Divide__Divide_var_L_L
_Divide__Divide_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _Divide__uDivide_param_L_L
	.private_extern _Divide__uDivide_param_L_L
_Divide__uDivide_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_L
	.private_extern _Divide__Divide_param_L_L
_Divide__Divide_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1482
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1482
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1482:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_u32
	.private_extern _Divide__uDivide_var_L_u32
_Divide__uDivide_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_L_u32
	.private_extern _Divide__Divide_var_L_u32
_Divide__Divide_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1487
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1487
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1487:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_L_u32
	.private_extern _Divide__uDivide_param_L_u32
_Divide__uDivide_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_L_u32
	.private_extern _Divide__Divide_param_L_u32
_Divide__Divide_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1492
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1492
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1492:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u16
	.private_extern _Divide__uDivide_var_u32_u16
_Divide__uDivide_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_u16
	.private_extern _Divide__Divide_var_u32_u16
_Divide__Divide_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1497
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1497
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1497:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1498
	cmpq	$2147483647, -16(%rbp)
	jle	L1499
L1498:
	movl	$18785, %edi
	call	__m3_fault
L1499:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_u16
	.private_extern _Divide__uDivide_param_u32_u16
_Divide__uDivide_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movswq	-22(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_u16
	.private_extern _Divide__Divide_param_u32_u16
_Divide__Divide_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movswq	-22(%rbp),%rax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1504
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1504
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1504:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1505
	cmpq	$2147483647, -16(%rbp)
	jle	L1506
L1505:
	movl	$18849, %edi
	call	__m3_fault
L1506:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u64
	.private_extern _Divide__uDivide_var_u32_u64
_Divide__uDivide_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_u64
	.private_extern _Divide__Divide_var_u32_u64
_Divide__Divide_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1511
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1511
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1511:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_u64
	.private_extern _Divide__uDivide_param_u32_u64
_Divide__uDivide_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_u64
	.private_extern _Divide__Divide_param_u32_u64
_Divide__Divide_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1516
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1516
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1516:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_LC
	.private_extern _Divide__uDivide_var_u32_LC
_Divide__uDivide_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_LC
	.private_extern _Divide__Divide_var_u32_LC
_Divide__Divide_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	120+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1521
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1521
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1521:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_LC
	.private_extern _Divide__uDivide_param_u32_LC
_Divide__uDivide_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u32_LC
	.private_extern _Divide__Divide_param_u32_LC
_Divide__Divide_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1526
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1526
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1526:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i16
	.private_extern _Divide__uDivide_var_u32_i16
_Divide__uDivide_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_i16
	.private_extern _Divide__Divide_var_u32_i16
_Divide__Divide_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1531
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1531
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1531:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1532
	cmpq	$2147483647, -16(%rbp)
	jle	L1533
L1532:
	movl	$19169, %edi
	call	__m3_fault
L1533:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_i16
	.private_extern _Divide__uDivide_param_u32_i16
_Divide__uDivide_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzwl	-22(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_i16
	.private_extern _Divide__Divide_param_u32_i16
_Divide__Divide_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movzwl	-22(%rbp), %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1538
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1538
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1538:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1539
	cmpq	$2147483647, -16(%rbp)
	jle	L1540
L1539:
	movl	$19233, %edi
	call	__m3_fault
L1540:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i32
	.private_extern _Divide__uDivide_var_u32_i32
_Divide__uDivide_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_i32
	.private_extern _Divide__Divide_var_u32_i32
_Divide__Divide_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1545
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1545
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1545:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1546
	cmpq	$2147483647, -16(%rbp)
	jle	L1547
L1546:
	movl	$19297, %edi
	call	__m3_fault
L1547:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_i32
	.private_extern _Divide__uDivide_param_u32_i32
_Divide__uDivide_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_i32
	.private_extern _Divide__Divide_param_u32_i32
_Divide__Divide_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1552
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1552
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1552:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1553
	cmpq	$2147483647, -16(%rbp)
	jle	L1554
L1553:
	movl	$19361, %edi
	call	__m3_fault
L1554:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_I
	.private_extern _Divide__uDivide_var_u32_I
_Divide__uDivide_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_I
	.private_extern _Divide__Divide_var_u32_I
_Divide__Divide_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1559
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1559
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1559:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1560
	cmpq	$2147483647, -16(%rbp)
	jle	L1561
L1560:
	movl	$19425, %edi
	call	__m3_fault
L1561:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_I
	.private_extern _Divide__uDivide_param_u32_I
_Divide__uDivide_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_I
	.private_extern _Divide__Divide_param_u32_I
_Divide__Divide_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1566
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1566
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1566:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1567
	cmpq	$2147483647, -16(%rbp)
	jle	L1568
L1567:
	movl	$19489, %edi
	call	__m3_fault
L1568:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i64
	.private_extern _Divide__uDivide_var_u32_i64
_Divide__uDivide_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_i64
	.private_extern _Divide__Divide_var_u32_i64
_Divide__Divide_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	152+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1573
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1573
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1573:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_i64
	.private_extern _Divide__uDivide_param_u32_i64
_Divide__uDivide_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_i64
	.private_extern _Divide__Divide_param_u32_i64
_Divide__Divide_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1578
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1578
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1578:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_C
	.private_extern _Divide__uDivide_var_u32_C
_Divide__uDivide_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_C
	.private_extern _Divide__Divide_var_u32_C
_Divide__Divide_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1583
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1583
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1583:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1584
	cmpq	$2147483647, -16(%rbp)
	jle	L1585
L1584:
	movl	$19681, %edi
	call	__m3_fault
L1585:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_C
	.private_extern _Divide__uDivide_param_u32_C
_Divide__uDivide_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movl	$0, %edx
	divq	-32(%rbp)
	leave
	ret
.globl _Divide__Divide_param_u32_C
	.private_extern _Divide__Divide_param_u32_C
_Divide__Divide_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, -56(%rbp)
	cmpq	$0, -64(%rbp)
	je	L1590
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1590
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1590:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1591
	cmpq	$2147483647, -16(%rbp)
	jle	L1592
L1591:
	movl	$19745, %edi
	call	__m3_fault
L1592:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u8
	.private_extern _Divide__uDivide_var_u32_u8
_Divide__uDivide_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_u8
	.private_extern _Divide__Divide_var_u32_u8
_Divide__Divide_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1597
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1597
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1597:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1598
	cmpq	$2147483647, -16(%rbp)
	jle	L1599
L1598:
	movl	$19809, %edi
	call	__m3_fault
L1599:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_u8
	.private_extern _Divide__uDivide_param_u32_u8
_Divide__uDivide_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movsbq	-21(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_u8
	.private_extern _Divide__Divide_param_u32_u8
_Divide__Divide_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movsbq	-21(%rbp),%rax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1604
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1604
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1604:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1605
	cmpq	$2147483647, -16(%rbp)
	jle	L1606
L1605:
	movl	$19873, %edi
	call	__m3_fault
L1606:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i8
	.private_extern _Divide__uDivide_var_u32_i8
_Divide__uDivide_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_i8
	.private_extern _Divide__Divide_var_u32_i8
_Divide__Divide_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1611
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1611
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1611:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1612
	cmpq	$2147483647, -16(%rbp)
	jle	L1613
L1612:
	movl	$19937, %edi
	call	__m3_fault
L1613:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_i8
	.private_extern _Divide__uDivide_param_u32_i8
_Divide__uDivide_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzbl	-21(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_i8
	.private_extern _Divide__Divide_param_u32_i8
_Divide__Divide_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movzbl	-21(%rbp), %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1618
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1618
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1618:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1619
	cmpq	$2147483647, -16(%rbp)
	jle	L1620
L1619:
	movl	$20001, %edi
	call	__m3_fault
L1620:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_L
	.private_extern _Divide__uDivide_var_u32_L
_Divide__uDivide_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_L
	.private_extern _Divide__Divide_var_u32_L
_Divide__Divide_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1625
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1625
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1625:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_L
	.private_extern _Divide__uDivide_param_u32_L
_Divide__uDivide_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -64(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_L
	.private_extern _Divide__Divide_param_u32_L
_Divide__Divide_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1630
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1630
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1630:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u32
	.private_extern _Divide__uDivide_var_u32_u32
_Divide__uDivide_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_u32_u32
	.private_extern _Divide__Divide_var_u32_u32
_Divide__Divide_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movl	184+_MM_Divide(%rip), %eax
	cltq
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1635
	cmpq	$2147483647, -16(%rbp)
	jle	L1636
L1635:
	movl	$20193, %edi
	call	__m3_fault
L1636:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_param_u32_u32
	.private_extern _Divide__uDivide_param_u32_u32
_Divide__uDivide_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-48(%rbp), %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_param_u32_u32
	.private_extern _Divide__Divide_param_u32_u32
_Divide__Divide_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1641
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1641
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1641:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1642
	cmpq	$2147483647, -16(%rbp)
	jle	L1643
L1642:
	movl	$20257, %edi
	call	__m3_fault
L1643:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide_M3
_Divide_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Divide(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Divide(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Divide_M3"
	.space 1
	.ascii "Divide_param_u32_u32"
	.space 1
	.ascii "uDivide_param_u32_u32"
	.space 1
	.ascii "Divide_var_u32_u32"
	.space 1
	.ascii "uDivide_var_u32_u32"
	.space 1
	.ascii "Divide_param_u32_L"
	.space 1
	.ascii "uDivide_param_u32_L"
	.space 1
	.ascii "Divide_var_u32_L"
	.space 1
	.ascii "uDivide_var_u32_L"
	.space 1
	.ascii "Divide_param_u32_i8"
	.space 1
	.ascii "uDivide_param_u32_i8"
	.space 1
	.ascii "Divide_var_u32_i8"
	.space 1
	.ascii "uDivide_var_u32_i8"
	.space 1
	.ascii "Divide_param_u32_u8"
	.space 1
	.ascii "uDivide_param_u32_u8"
	.space 1
	.ascii "Divide_var_u32_u8"
	.space 1
	.ascii "uDivide_var_u32_u8"
	.space 1
	.ascii "Divide_param_u32_C"
	.space 1
	.ascii "uDivide_param_u32_C"
	.space 1
	.ascii "Divide_var_u32_C"
	.space 1
	.ascii "uDivide_var_u32_C"
	.space 1
	.ascii "Divide_param_u32_i64"
	.space 1
	.ascii "uDivide_param_u32_i64"
	.space 1
	.ascii "Divide_var_u32_i64"
	.space 1
	.ascii "uDivide_var_u32_i64"
	.space 1
	.ascii "Divide_param_u32_I"
	.space 1
	.ascii "uDivide_param_u32_I"
	.space 1
	.ascii "Divide_var_u32_I"
	.space 1
	.ascii "uDivide_var_u32_I"
	.space 1
	.ascii "Divide_param_u32_i32"
	.space 1
	.ascii "uDivide_param_u32_i32"
	.space 1
	.ascii "Divide_var_u32_i32"
	.space 1
	.ascii "uDivide_var_u32_i32"
	.space 1
	.ascii "Divide_param_u32_i16"
	.space 1
	.ascii "uDivide_param_u32_i16"
	.space 1
	.ascii "Divide_var_u32_i16"
	.space 1
	.ascii "uDivide_var_u32_i16"
	.space 1
	.ascii "Divide_param_u32_LC"
	.space 1
	.ascii "uDivide_param_u32_LC"
	.space 1
	.ascii "Divide_var_u32_LC"
	.space 1
	.ascii "uDivide_var_u32_LC"
	.space 1
	.ascii "Divide_param_u32_u64"
	.space 1
	.ascii "uDivide_param_u32_u64"
	.space 1
	.ascii "Divide_var_u32_u64"
	.space 1
	.ascii "uDivide_var_u32_u64"
	.space 1
	.ascii "Divide_param_u32_u16"
	.space 1
	.ascii "uDivide_param_u32_u16"
	.space 1
	.ascii "Divide_var_u32_u16"
	.space 1
	.ascii "uDivide_var_u32_u16"
	.space 1
	.ascii "Divide_param_L_u32"
	.space 1
	.ascii "uDivide_param_L_u32"
	.space 1
	.ascii "Divide_var_L_u32"
	.space 1
	.ascii "uDivide_var_L_u32"
	.space 1
	.ascii "Divide_param_L_L"
	.space 1
	.ascii "uDivide_param_L_L"
	.space 1
	.ascii "Divide_var_L_L"
	.space 1
	.ascii "uDivide_var_L_L"
	.space 1
	.ascii "Divide_param_L_i8"
	.space 1
	.ascii "uDivide_param_L_i8"
	.space 1
	.ascii "Divide_var_L_i8"
	.space 1
	.ascii "uDivide_var_L_i8"
	.space 1
	.ascii "Divide_param_L_u8"
	.space 1
	.ascii "uDivide_param_L_u8"
	.space 1
	.ascii "Divide_var_L_u8"
	.space 1
	.ascii "uDivide_var_L_u8"
	.space 1
	.ascii "Divide_param_L_C"
	.space 1
	.ascii "uDivide_param_L_C"
	.space 1
	.ascii "Divide_var_L_C"
	.space 1
	.ascii "uDivide_var_L_C"
	.space 1
	.ascii "Divide_param_L_i64"
	.space 1
	.ascii "uDivide_param_L_i64"
	.space 1
	.ascii "Divide_var_L_i64"
	.space 1
	.ascii "uDivide_var_L_i64"
	.space 1
	.ascii "Divide_param_L_I"
	.space 1
	.ascii "uDivide_param_L_I"
	.space 1
	.ascii "Divide_var_L_I"
	.space 1
	.ascii "uDivide_var_L_I"
	.space 1
	.ascii "Divide_param_L_i32"
	.space 1
	.ascii "uDivide_param_L_i32"
	.space 1
	.ascii "Divide_var_L_i32"
	.space 1
	.ascii "uDivide_var_L_i32"
	.space 1
	.ascii "Divide_param_L_i16"
	.space 1
	.ascii "uDivide_param_L_i16"
	.space 1
	.ascii "Divide_var_L_i16"
	.space 1
	.ascii "uDivide_var_L_i16"
	.space 1
	.ascii "Divide_param_L_LC"
	.space 1
	.ascii "uDivide_param_L_LC"
	.space 1
	.ascii "Divide_var_L_LC"
	.space 1
	.ascii "uDivide_var_L_LC"
	.space 1
	.ascii "Divide_param_L_u64"
	.space 1
	.ascii "uDivide_param_L_u64"
	.space 1
	.ascii "Divide_var_L_u64"
	.space 1
	.ascii "uDivide_var_L_u64"
	.space 1
	.ascii "Divide_param_L_u16"
	.space 1
	.ascii "uDivide_param_L_u16"
	.space 1
	.ascii "Divide_var_L_u16"
	.space 1
	.ascii "uDivide_var_L_u16"
	.space 1
	.ascii "Divide_param_i8_u32"
	.space 1
	.ascii "uDivide_param_i8_u32"
	.space 1
	.ascii "Divide_var_i8_u32"
	.space 1
	.ascii "uDivide_var_i8_u32"
	.space 1
	.ascii "Divide_param_i8_L"
	.space 1
	.ascii "uDivide_param_i8_L"
	.space 1
	.ascii "Divide_var_i8_L"
	.space 1
	.ascii "uDivide_var_i8_L"
	.space 1
	.ascii "Divide_param_i8_i8"
	.space 1
	.ascii "uDivide_param_i8_i8"
	.space 1
	.ascii "Divide_var_i8_i8"
	.space 1
	.ascii "uDivide_var_i8_i8"
	.space 1
	.ascii "Divide_param_i8_u8"
	.space 1
	.ascii "uDivide_param_i8_u8"
	.space 1
	.ascii "Divide_var_i8_u8"
	.space 1
	.ascii "uDivide_var_i8_u8"
	.space 1
	.ascii "Divide_param_i8_C"
	.space 1
	.ascii "uDivide_param_i8_C"
	.space 1
	.ascii "Divide_var_i8_C"
	.space 1
	.ascii "uDivide_var_i8_C"
	.space 1
	.ascii "Divide_param_i8_i64"
	.space 1
	.ascii "uDivide_param_i8_i64"
	.space 1
	.ascii "Divide_var_i8_i64"
	.space 1
	.ascii "uDivide_var_i8_i64"
	.space 1
	.ascii "Divide_param_i8_I"
	.space 1
	.ascii "uDivide_param_i8_I"
	.space 1
	.ascii "Divide_var_i8_I"
	.space 1
	.ascii "uDivide_var_i8_I"
	.space 1
	.ascii "Divide_param_i8_i32"
	.space 1
	.ascii "uDivide_param_i8_i32"
	.space 1
	.ascii "Divide_var_i8_i32"
	.space 1
	.ascii "uDivide_var_i8_i32"
	.space 1
	.ascii "Divide_param_i8_i16"
	.space 1
	.ascii "uDivide_param_i8_i16"
	.space 1
	.ascii "Divide_var_i8_i16"
	.space 1
	.ascii "uDivide_var_i8_i16"
	.space 1
	.ascii "Divide_param_i8_LC"
	.space 1
	.ascii "uDivide_param_i8_LC"
	.space 1
	.ascii "Divide_var_i8_LC"
	.space 1
	.ascii "uDivide_var_i8_LC"
	.space 1
	.ascii "Divide_param_i8_u64"
	.space 1
	.ascii "uDivide_param_i8_u64"
	.space 1
	.ascii "Divide_var_i8_u64"
	.space 1
	.ascii "uDivide_var_i8_u64"
	.space 1
	.ascii "Divide_param_i8_u16"
	.space 1
	.ascii "uDivide_param_i8_u16"
	.space 1
	.ascii "Divide_var_i8_u16"
	.space 1
	.ascii "uDivide_var_i8_u16"
	.space 1
	.ascii "Divide_param_u8_u32"
	.space 1
	.ascii "uDivide_param_u8_u32"
	.space 1
	.ascii "Divide_var_u8_u32"
	.space 1
	.ascii "uDivide_var_u8_u32"
	.space 1
	.ascii "Divide_param_u8_L"
	.space 1
	.ascii "uDivide_param_u8_L"
	.space 1
	.ascii "Divide_var_u8_L"
	.space 1
	.ascii "uDivide_var_u8_L"
	.space 1
	.ascii "Divide_param_u8_i8"
	.space 1
	.ascii "uDivide_param_u8_i8"
	.space 1
	.ascii "Divide_var_u8_i8"
	.space 1
	.ascii "uDivide_var_u8_i8"
	.space 1
	.ascii "Divide_param_u8_u8"
	.space 1
	.ascii "uDivide_param_u8_u8"
	.space 1
	.ascii "Divide_var_u8_u8"
	.space 1
	.ascii "uDivide_var_u8_u8"
	.space 1
	.ascii "Divide_param_u8_C"
	.space 1
	.ascii "uDivide_param_u8_C"
	.space 1
	.ascii "Divide_var_u8_C"
	.space 1
	.ascii "uDivide_var_u8_C"
	.space 1
	.ascii "Divide_param_u8_i64"
	.space 1
	.ascii "uDivide_param_u8_i64"
	.space 1
	.ascii "Divide_var_u8_i64"
	.space 1
	.ascii "uDivide_var_u8_i64"
	.space 1
	.ascii "Divide_param_u8_I"
	.space 1
	.ascii "uDivide_param_u8_I"
	.space 1
	.ascii "Divide_var_u8_I"
	.space 1
	.ascii "uDivide_var_u8_I"
	.space 1
	.ascii "Divide_param_u8_i32"
	.space 1
	.ascii "uDivide_param_u8_i32"
	.space 1
	.ascii "Divide_var_u8_i32"
	.space 1
	.ascii "uDivide_var_u8_i32"
	.space 1
	.ascii "Divide_param_u8_i16"
	.space 1
	.ascii "uDivide_param_u8_i16"
	.space 1
	.ascii "Divide_var_u8_i16"
	.space 1
	.ascii "uDivide_var_u8_i16"
	.space 1
	.ascii "Divide_param_u8_LC"
	.space 1
	.ascii "uDivide_param_u8_LC"
	.space 1
	.ascii "Divide_var_u8_LC"
	.space 1
	.ascii "uDivide_var_u8_LC"
	.space 1
	.ascii "Divide_param_u8_u64"
	.space 1
	.ascii "uDivide_param_u8_u64"
	.space 1
	.ascii "Divide_var_u8_u64"
	.space 1
	.ascii "uDivide_var_u8_u64"
	.space 1
	.ascii "Divide_param_u8_u16"
	.space 1
	.ascii "uDivide_param_u8_u16"
	.space 1
	.ascii "Divide_var_u8_u16"
	.space 1
	.ascii "uDivide_var_u8_u16"
	.space 1
	.ascii "Divide_param_f32_f32"
	.space 1
	.ascii "Divide_var_f32_f32"
	.space 1
	.ascii "Divide_param_C_u32"
	.space 1
	.ascii "uDivide_param_C_u32"
	.space 1
	.ascii "Divide_var_C_u32"
	.space 1
	.ascii "uDivide_var_C_u32"
	.space 1
	.ascii "Divide_param_C_L"
	.space 1
	.ascii "uDivide_param_C_L"
	.space 1
	.ascii "Divide_var_C_L"
	.space 1
	.ascii "uDivide_var_C_L"
	.space 1
	.ascii "Divide_param_C_i8"
	.space 1
	.ascii "uDivide_param_C_i8"
	.space 1
	.ascii "Divide_var_C_i8"
	.space 1
	.ascii "uDivide_var_C_i8"
	.space 1
	.ascii "Divide_param_C_u8"
	.space 1
	.ascii "uDivide_param_C_u8"
	.space 1
	.ascii "Divide_var_C_u8"
	.space 1
	.ascii "uDivide_var_C_u8"
	.space 1
	.ascii "Divide_param_C_C"
	.space 1
	.ascii "uDivide_param_C_C"
	.space 1
	.ascii "Divide_var_C_C"
	.space 1
	.ascii "uDivide_var_C_C"
	.space 1
	.ascii "Divide_param_C_i64"
	.space 1
	.ascii "uDivide_param_C_i64"
	.space 1
	.ascii "Divide_var_C_i64"
	.space 1
	.ascii "uDivide_var_C_i64"
	.space 1
	.ascii "Divide_param_C_I"
	.space 1
	.ascii "uDivide_param_C_I"
	.space 1
	.ascii "Divide_var_C_I"
	.space 1
	.ascii "uDivide_var_C_I"
	.space 1
	.ascii "Divide_param_C_i32"
	.space 1
	.ascii "uDivide_param_C_i32"
	.space 1
	.ascii "Divide_var_C_i32"
	.space 1
	.ascii "uDivide_var_C_i32"
	.space 1
	.ascii "Divide_param_C_i16"
	.space 1
	.ascii "uDivide_param_C_i16"
	.space 1
	.ascii "Divide_var_C_i16"
	.space 1
	.ascii "uDivide_var_C_i16"
	.space 1
	.ascii "Divide_param_C_LC"
	.space 1
	.ascii "uDivide_param_C_LC"
	.space 1
	.ascii "Divide_var_C_LC"
	.space 1
	.ascii "uDivide_var_C_LC"
	.space 1
	.ascii "Divide_param_C_u64"
	.space 1
	.ascii "uDivide_param_C_u64"
	.space 1
	.ascii "Divide_var_C_u64"
	.space 1
	.ascii "uDivide_var_C_u64"
	.space 1
	.ascii "Divide_param_C_u16"
	.space 1
	.ascii "uDivide_param_C_u16"
	.space 1
	.ascii "Divide_var_C_u16"
	.space 1
	.ascii "uDivide_var_C_u16"
	.space 1
	.ascii "Divide_param_i64_u32"
	.space 1
	.ascii "uDivide_param_i64_u32"
	.space 1
	.ascii "Divide_var_i64_u32"
	.space 1
	.ascii "uDivide_var_i64_u32"
	.space 1
	.ascii "Divide_param_i64_L"
	.space 1
	.ascii "uDivide_param_i64_L"
	.space 1
	.ascii "Divide_var_i64_L"
	.space 1
	.ascii "uDivide_var_i64_L"
	.space 1
	.ascii "Divide_param_i64_i8"
	.space 1
	.ascii "uDivide_param_i64_i8"
	.space 1
	.ascii "Divide_var_i64_i8"
	.space 1
	.ascii "uDivide_var_i64_i8"
	.space 1
	.ascii "Divide_param_i64_u8"
	.space 1
	.ascii "uDivide_param_i64_u8"
	.space 1
	.ascii "Divide_var_i64_u8"
	.space 1
	.ascii "uDivide_var_i64_u8"
	.space 1
	.ascii "Divide_param_i64_C"
	.space 1
	.ascii "uDivide_param_i64_C"
	.space 1
	.ascii "Divide_var_i64_C"
	.space 1
	.ascii "uDivide_var_i64_C"
	.space 1
	.ascii "Divide_param_i64_i64"
	.space 1
	.ascii "uDivide_param_i64_i64"
	.space 1
	.ascii "Divide_var_i64_i64"
	.space 1
	.ascii "uDivide_var_i64_i64"
	.space 1
	.ascii "Divide_param_i64_I"
	.space 1
	.ascii "uDivide_param_i64_I"
	.space 1
	.ascii "Divide_var_i64_I"
	.space 1
	.ascii "uDivide_var_i64_I"
	.space 1
	.ascii "Divide_param_i64_i32"
	.space 1
	.ascii "uDivide_param_i64_i32"
	.space 1
	.ascii "Divide_var_i64_i32"
	.space 1
	.ascii "uDivide_var_i64_i32"
	.space 1
	.ascii "Divide_param_i64_i16"
	.space 1
	.ascii "uDivide_param_i64_i16"
	.space 1
	.ascii "Divide_var_i64_i16"
	.space 1
	.ascii "uDivide_var_i64_i16"
	.space 1
	.ascii "Divide_param_i64_LC"
	.space 1
	.ascii "uDivide_param_i64_LC"
	.space 1
	.ascii "Divide_var_i64_LC"
	.space 1
	.ascii "uDivide_var_i64_LC"
	.space 1
	.ascii "Divide_param_i64_u64"
	.space 1
	.ascii "uDivide_param_i64_u64"
	.space 1
	.ascii "Divide_var_i64_u64"
	.space 1
	.ascii "uDivide_var_i64_u64"
	.space 1
	.ascii "Divide_param_i64_u16"
	.space 1
	.ascii "uDivide_param_i64_u16"
	.space 1
	.ascii "Divide_var_i64_u16"
	.space 1
	.ascii "uDivide_var_i64_u16"
	.space 1
	.ascii "Divide_param_I_u32"
	.space 1
	.ascii "uDivide_param_I_u32"
	.space 1
	.ascii "Divide_var_I_u32"
	.space 1
	.ascii "uDivide_var_I_u32"
	.space 1
	.ascii "Divide_param_I_L"
	.space 1
	.ascii "uDivide_param_I_L"
	.space 1
	.ascii "Divide_var_I_L"
	.space 1
	.ascii "uDivide_var_I_L"
	.space 1
	.ascii "Divide_param_I_i8"
	.space 1
	.ascii "uDivide_param_I_i8"
	.space 1
	.ascii "Divide_var_I_i8"
	.space 1
	.ascii "uDivide_var_I_i8"
	.space 1
	.ascii "Divide_param_I_u8"
	.space 1
	.ascii "uDivide_param_I_u8"
	.space 1
	.ascii "Divide_var_I_u8"
	.space 1
	.ascii "uDivide_var_I_u8"
	.space 1
	.ascii "Divide_param_I_C"
	.space 1
	.ascii "uDivide_param_I_C"
	.space 1
	.ascii "Divide_var_I_C"
	.space 1
	.ascii "uDivide_var_I_C"
	.space 1
	.ascii "Divide_param_I_i64"
	.space 1
	.ascii "uDivide_param_I_i64"
	.space 1
	.ascii "Divide_var_I_i64"
	.space 1
	.ascii "uDivide_var_I_i64"
	.space 1
	.ascii "Divide_param_I_I"
	.space 1
	.ascii "uDivide_param_I_I"
	.space 1
	.ascii "Divide_var_I_I"
	.space 1
	.ascii "uDivide_var_I_I"
	.space 1
	.ascii "Divide_param_I_i32"
	.space 1
	.ascii "uDivide_param_I_i32"
	.space 1
	.ascii "Divide_var_I_i32"
	.space 1
	.ascii "uDivide_var_I_i32"
	.space 1
	.ascii "Divide_param_I_i16"
	.space 1
	.ascii "uDivide_param_I_i16"
	.space 1
	.ascii "Divide_var_I_i16"
	.space 1
	.ascii "uDivide_var_I_i16"
	.space 1
	.ascii "Divide_param_I_LC"
	.space 1
	.ascii "uDivide_param_I_LC"
	.space 1
	.ascii "Divide_var_I_LC"
	.space 1
	.ascii "uDivide_var_I_LC"
	.space 1
	.ascii "Divide_param_I_u64"
	.space 1
	.ascii "uDivide_param_I_u64"
	.space 1
	.ascii "Divide_var_I_u64"
	.space 1
	.ascii "uDivide_var_I_u64"
	.space 1
	.ascii "Divide_param_I_u16"
	.space 1
	.ascii "uDivide_param_I_u16"
	.space 1
	.ascii "Divide_var_I_u16"
	.space 1
	.ascii "uDivide_var_I_u16"
	.space 1
	.ascii "Divide_param_i32_u32"
	.space 1
	.ascii "uDivide_param_i32_u32"
	.space 1
	.ascii "Divide_var_i32_u32"
	.space 1
	.ascii "uDivide_var_i32_u32"
	.space 1
	.ascii "Divide_param_i32_L"
	.space 1
	.ascii "uDivide_param_i32_L"
	.space 1
	.ascii "Divide_var_i32_L"
	.space 1
	.ascii "uDivide_var_i32_L"
	.space 1
	.ascii "Divide_param_i32_i8"
	.space 1
	.ascii "uDivide_param_i32_i8"
	.space 1
	.ascii "Divide_var_i32_i8"
	.space 1
	.ascii "uDivide_var_i32_i8"
	.space 1
	.ascii "Divide_param_i32_u8"
	.space 1
	.ascii "uDivide_param_i32_u8"
	.space 1
	.ascii "Divide_var_i32_u8"
	.space 1
	.ascii "uDivide_var_i32_u8"
	.space 1
	.ascii "Divide_param_i32_C"
	.space 1
	.ascii "uDivide_param_i32_C"
	.space 1
	.ascii "Divide_var_i32_C"
	.space 1
	.ascii "uDivide_var_i32_C"
	.space 1
	.ascii "Divide_param_i32_i64"
	.space 1
	.ascii "uDivide_param_i32_i64"
	.space 1
	.ascii "Divide_var_i32_i64"
	.space 1
	.ascii "uDivide_var_i32_i64"
	.space 1
	.ascii "Divide_param_i32_I"
	.space 1
	.ascii "uDivide_param_i32_I"
	.space 1
	.ascii "Divide_var_i32_I"
	.space 1
	.ascii "uDivide_var_i32_I"
	.space 1
	.ascii "Divide_param_i32_i32"
	.space 1
	.ascii "uDivide_param_i32_i32"
	.space 1
	.ascii "Divide_var_i32_i32"
	.space 1
	.ascii "uDivide_var_i32_i32"
	.space 1
	.ascii "Divide_param_i32_i16"
	.space 1
	.ascii "uDivide_param_i32_i16"
	.space 1
	.ascii "Divide_var_i32_i16"
	.space 1
	.ascii "uDivide_var_i32_i16"
	.space 1
	.ascii "Divide_param_i32_LC"
	.space 1
	.ascii "uDivide_param_i32_LC"
	.space 1
	.ascii "Divide_var_i32_LC"
	.space 1
	.ascii "uDivide_var_i32_LC"
	.space 1
	.ascii "Divide_param_i32_u64"
	.space 1
	.ascii "uDivide_param_i32_u64"
	.space 1
	.ascii "Divide_var_i32_u64"
	.space 1
	.ascii "uDivide_var_i32_u64"
	.space 1
	.ascii "Divide_param_i32_u16"
	.space 1
	.ascii "uDivide_param_i32_u16"
	.space 1
	.ascii "Divide_var_i32_u16"
	.space 1
	.ascii "uDivide_var_i32_u16"
	.space 1
	.ascii "Divide_param_i16_u32"
	.space 1
	.ascii "uDivide_param_i16_u32"
	.space 1
	.ascii "Divide_var_i16_u32"
	.space 1
	.ascii "uDivide_var_i16_u32"
	.space 1
	.ascii "Divide_param_i16_L"
	.space 1
	.ascii "uDivide_param_i16_L"
	.space 1
	.ascii "Divide_var_i16_L"
	.space 1
	.ascii "uDivide_var_i16_L"
	.space 1
	.ascii "Divide_param_i16_i8"
	.space 1
	.ascii "uDivide_param_i16_i8"
	.space 1
	.ascii "Divide_var_i16_i8"
	.space 1
	.ascii "uDivide_var_i16_i8"
	.space 1
	.ascii "Divide_param_i16_u8"
	.space 1
	.ascii "uDivide_param_i16_u8"
	.space 1
	.ascii "Divide_var_i16_u8"
	.space 1
	.ascii "uDivide_var_i16_u8"
	.space 1
	.ascii "Divide_param_i16_C"
	.space 1
	.ascii "uDivide_param_i16_C"
	.space 1
	.ascii "Divide_var_i16_C"
	.space 1
	.ascii "uDivide_var_i16_C"
	.space 1
	.ascii "Divide_param_i16_i64"
	.space 1
	.ascii "uDivide_param_i16_i64"
	.space 1
	.ascii "Divide_var_i16_i64"
	.space 1
	.ascii "uDivide_var_i16_i64"
	.space 1
	.ascii "Divide_param_i16_I"
	.space 1
	.ascii "uDivide_param_i16_I"
	.space 1
	.ascii "Divide_var_i16_I"
	.space 1
	.ascii "uDivide_var_i16_I"
	.space 1
	.ascii "Divide_param_i16_i32"
	.space 1
	.ascii "uDivide_param_i16_i32"
	.space 1
	.ascii "Divide_var_i16_i32"
	.space 1
	.ascii "uDivide_var_i16_i32"
	.space 1
	.ascii "Divide_param_i16_i16"
	.space 1
	.ascii "uDivide_param_i16_i16"
	.space 1
	.ascii "Divide_var_i16_i16"
	.space 1
	.ascii "uDivide_var_i16_i16"
	.space 1
	.ascii "Divide_param_i16_LC"
	.space 1
	.ascii "uDivide_param_i16_LC"
	.space 1
	.ascii "Divide_var_i16_LC"
	.space 1
	.ascii "uDivide_var_i16_LC"
	.space 1
	.ascii "Divide_param_i16_u64"
	.space 1
	.ascii "uDivide_param_i16_u64"
	.space 1
	.ascii "Divide_var_i16_u64"
	.space 1
	.ascii "uDivide_var_i16_u64"
	.space 1
	.ascii "Divide_param_i16_u16"
	.space 1
	.ascii "uDivide_param_i16_u16"
	.space 1
	.ascii "Divide_var_i16_u16"
	.space 1
	.ascii "uDivide_var_i16_u16"
	.space 1
	.ascii "Divide_param_f64_f64"
	.space 1
	.ascii "Divide_var_f64_f64"
	.space 1
	.ascii "Divide_param_LC_u32"
	.space 1
	.ascii "uDivide_param_LC_u32"
	.space 1
	.ascii "Divide_var_LC_u32"
	.space 1
	.ascii "uDivide_var_LC_u32"
	.space 1
	.ascii "Divide_param_LC_L"
	.space 1
	.ascii "uDivide_param_LC_L"
	.space 1
	.ascii "Divide_var_LC_L"
	.space 1
	.ascii "uDivide_var_LC_L"
	.space 1
	.ascii "Divide_param_LC_i8"
	.space 1
	.ascii "uDivide_param_LC_i8"
	.space 1
	.ascii "Divide_var_LC_i8"
	.space 1
	.ascii "uDivide_var_LC_i8"
	.space 1
	.ascii "Divide_param_LC_u8"
	.space 1
	.ascii "uDivide_param_LC_u8"
	.space 1
	.ascii "Divide_var_LC_u8"
	.space 1
	.ascii "uDivide_var_LC_u8"
	.space 1
	.ascii "Divide_param_LC_C"
	.space 1
	.ascii "uDivide_param_LC_C"
	.space 1
	.ascii "Divide_var_LC_C"
	.space 1
	.ascii "uDivide_var_LC_C"
	.space 1
	.ascii "Divide_param_LC_i64"
	.space 1
	.ascii "uDivide_param_LC_i64"
	.space 1
	.ascii "Divide_var_LC_i64"
	.space 1
	.ascii "uDivide_var_LC_i64"
	.space 1
	.ascii "Divide_param_LC_I"
	.space 1
	.ascii "uDivide_param_LC_I"
	.space 1
	.ascii "Divide_var_LC_I"
	.space 1
	.ascii "uDivide_var_LC_I"
	.space 1
	.ascii "Divide_param_LC_i32"
	.space 1
	.ascii "uDivide_param_LC_i32"
	.space 1
	.ascii "Divide_var_LC_i32"
	.space 1
	.ascii "uDivide_var_LC_i32"
	.space 1
	.ascii "Divide_param_LC_i16"
	.space 1
	.ascii "uDivide_param_LC_i16"
	.space 1
	.ascii "Divide_var_LC_i16"
	.space 1
	.ascii "uDivide_var_LC_i16"
	.space 1
	.ascii "Divide_param_LC_LC"
	.space 1
	.ascii "uDivide_param_LC_LC"
	.space 1
	.ascii "Divide_var_LC_LC"
	.space 1
	.ascii "uDivide_var_LC_LC"
	.space 1
	.ascii "Divide_param_LC_u64"
	.space 1
	.ascii "uDivide_param_LC_u64"
	.space 1
	.ascii "Divide_var_LC_u64"
	.space 1
	.ascii "uDivide_var_LC_u64"
	.space 1
	.ascii "Divide_param_LC_u16"
	.space 1
	.ascii "uDivide_param_LC_u16"
	.space 1
	.ascii "Divide_var_LC_u16"
	.space 1
	.ascii "uDivide_var_LC_u16"
	.space 1
	.ascii "Divide_param_u64_u32"
	.space 1
	.ascii "uDivide_param_u64_u32"
	.space 1
	.ascii "Divide_var_u64_u32"
	.space 1
	.ascii "uDivide_var_u64_u32"
	.space 1
	.ascii "Divide_param_u64_L"
	.space 1
	.ascii "uDivide_param_u64_L"
	.space 1
	.ascii "Divide_var_u64_L"
	.space 1
	.ascii "uDivide_var_u64_L"
	.space 1
	.ascii "Divide_param_u64_i8"
	.space 1
	.ascii "uDivide_param_u64_i8"
	.space 1
	.ascii "Divide_var_u64_i8"
	.space 1
	.ascii "uDivide_var_u64_i8"
	.space 1
	.ascii "Divide_param_u64_u8"
	.space 1
	.ascii "uDivide_param_u64_u8"
	.space 1
	.ascii "Divide_var_u64_u8"
	.space 1
	.ascii "uDivide_var_u64_u8"
	.space 1
	.ascii "Divide_param_u64_C"
	.space 1
	.ascii "uDivide_param_u64_C"
	.space 1
	.ascii "Divide_var_u64_C"
	.space 1
	.ascii "uDivide_var_u64_C"
	.space 1
	.ascii "Divide_param_u64_i64"
	.space 1
	.ascii "uDivide_param_u64_i64"
	.space 1
	.ascii "Divide_var_u64_i64"
	.space 1
	.ascii "uDivide_var_u64_i64"
	.space 1
	.ascii "Divide_param_u64_I"
	.space 1
	.ascii "uDivide_param_u64_I"
	.space 1
	.ascii "Divide_var_u64_I"
	.space 1
	.ascii "uDivide_var_u64_I"
	.space 1
	.ascii "Divide_param_u64_i32"
	.space 1
	.ascii "uDivide_param_u64_i32"
	.space 1
	.ascii "Divide_var_u64_i32"
	.space 1
	.ascii "uDivide_var_u64_i32"
	.space 1
	.ascii "Divide_param_u64_i16"
	.space 1
	.ascii "uDivide_param_u64_i16"
	.space 1
	.ascii "Divide_var_u64_i16"
	.space 1
	.ascii "uDivide_var_u64_i16"
	.space 1
	.ascii "Divide_param_u64_LC"
	.space 1
	.ascii "uDivide_param_u64_LC"
	.space 1
	.ascii "Divide_var_u64_LC"
	.space 1
	.ascii "uDivide_var_u64_LC"
	.space 1
	.ascii "Divide_param_u64_u64"
	.space 1
	.ascii "uDivide_param_u64_u64"
	.space 1
	.ascii "Divide_var_u64_u64"
	.space 1
	.ascii "uDivide_var_u64_u64"
	.space 1
	.ascii "Divide_param_u64_u16"
	.space 1
	.ascii "uDivide_param_u64_u16"
	.space 1
	.ascii "Divide_var_u64_u16"
	.space 1
	.ascii "uDivide_var_u64_u16"
	.space 1
	.ascii "Divide_param_u16_u32"
	.space 1
	.ascii "uDivide_param_u16_u32"
	.space 1
	.ascii "Divide_var_u16_u32"
	.space 1
	.ascii "uDivide_var_u16_u32"
	.space 1
	.ascii "Divide_param_u16_L"
	.space 1
	.ascii "uDivide_param_u16_L"
	.space 1
	.ascii "Divide_var_u16_L"
	.space 1
	.ascii "uDivide_var_u16_L"
	.space 1
	.ascii "Divide_param_u16_i8"
	.space 1
	.ascii "uDivide_param_u16_i8"
	.space 1
	.ascii "Divide_var_u16_i8"
	.space 1
	.ascii "uDivide_var_u16_i8"
	.space 1
	.ascii "Divide_param_u16_u8"
	.space 1
	.ascii "uDivide_param_u16_u8"
	.space 1
	.ascii "Divide_var_u16_u8"
	.space 1
	.ascii "uDivide_var_u16_u8"
	.space 1
	.ascii "Divide_param_u16_C"
	.space 1
	.ascii "uDivide_param_u16_C"
	.space 1
	.ascii "Divide_var_u16_C"
	.space 1
	.ascii "uDivide_var_u16_C"
	.space 1
	.ascii "Divide_param_u16_i64"
	.space 1
	.ascii "uDivide_param_u16_i64"
	.space 1
	.ascii "Divide_var_u16_i64"
	.space 1
	.ascii "uDivide_var_u16_i64"
	.space 1
	.ascii "Divide_param_u16_I"
	.space 1
	.ascii "uDivide_param_u16_I"
	.space 1
	.ascii "Divide_var_u16_I"
	.space 1
	.ascii "uDivide_var_u16_I"
	.space 1
	.ascii "Divide_param_u16_i32"
	.space 1
	.ascii "uDivide_param_u16_i32"
	.space 1
	.ascii "Divide_var_u16_i32"
	.space 1
	.ascii "uDivide_var_u16_i32"
	.space 1
	.ascii "Divide_param_u16_i16"
	.space 1
	.ascii "uDivide_param_u16_i16"
	.space 1
	.ascii "Divide_var_u16_i16"
	.space 1
	.ascii "uDivide_var_u16_i16"
	.space 1
	.ascii "Divide_param_u16_LC"
	.space 1
	.ascii "uDivide_param_u16_LC"
	.space 1
	.ascii "Divide_var_u16_LC"
	.space 1
	.ascii "uDivide_var_u16_LC"
	.space 1
	.ascii "Divide_param_u16_u64"
	.space 1
	.ascii "uDivide_param_u16_u64"
	.space 1
	.ascii "Divide_var_u16_u64"
	.space 1
	.ascii "uDivide_var_u16_u64"
	.space 1
	.ascii "Divide_param_u16_u16"
	.space 1
	.ascii "uDivide_param_u16_u16"
	.space 1
	.ascii "Divide_var_u16_u16"
	.space 1
	.ascii "uDivide_var_u16_u16"
	.space 7
	.quad	_Divide_M3
	.quad	_L_1
	.quad	_Divide__Divide_param_u32_u32
	.quad	_L_1+10
	.quad	_Divide__uDivide_param_u32_u32
	.quad	_L_1+31
	.quad	_Divide__Divide_var_u32_u32
	.quad	_L_1+53
	.quad	_Divide__uDivide_var_u32_u32
	.quad	_L_1+72
	.quad	_Divide__Divide_param_u32_L
	.quad	_L_1+92
	.quad	_Divide__uDivide_param_u32_L
	.quad	_L_1+111
	.quad	_Divide__Divide_var_u32_L
	.quad	_L_1+131
	.quad	_Divide__uDivide_var_u32_L
	.quad	_L_1+148
	.quad	_Divide__Divide_param_u32_i8
	.quad	_L_1+166
	.quad	_Divide__uDivide_param_u32_i8
	.quad	_L_1+186
	.quad	_Divide__Divide_var_u32_i8
	.quad	_L_1+207
	.quad	_Divide__uDivide_var_u32_i8
	.quad	_L_1+225
	.quad	_Divide__Divide_param_u32_u8
	.quad	_L_1+244
	.quad	_Divide__uDivide_param_u32_u8
	.quad	_L_1+264
	.quad	_Divide__Divide_var_u32_u8
	.quad	_L_1+285
	.quad	_Divide__uDivide_var_u32_u8
	.quad	_L_1+303
	.quad	_Divide__Divide_param_u32_C
	.quad	_L_1+322
	.quad	_Divide__uDivide_param_u32_C
	.quad	_L_1+341
	.quad	_Divide__Divide_var_u32_C
	.quad	_L_1+361
	.quad	_Divide__uDivide_var_u32_C
	.quad	_L_1+378
	.quad	_Divide__Divide_param_u32_i64
	.quad	_L_1+396
	.quad	_Divide__uDivide_param_u32_i64
	.quad	_L_1+417
	.quad	_Divide__Divide_var_u32_i64
	.quad	_L_1+439
	.quad	_Divide__uDivide_var_u32_i64
	.quad	_L_1+458
	.quad	_Divide__Divide_param_u32_I
	.quad	_L_1+478
	.quad	_Divide__uDivide_param_u32_I
	.quad	_L_1+497
	.quad	_Divide__Divide_var_u32_I
	.quad	_L_1+517
	.quad	_Divide__uDivide_var_u32_I
	.quad	_L_1+534
	.quad	_Divide__Divide_param_u32_i32
	.quad	_L_1+552
	.quad	_Divide__uDivide_param_u32_i32
	.quad	_L_1+573
	.quad	_Divide__Divide_var_u32_i32
	.quad	_L_1+595
	.quad	_Divide__uDivide_var_u32_i32
	.quad	_L_1+614
	.quad	_Divide__Divide_param_u32_i16
	.quad	_L_1+634
	.quad	_Divide__uDivide_param_u32_i16
	.quad	_L_1+655
	.quad	_Divide__Divide_var_u32_i16
	.quad	_L_1+677
	.quad	_Divide__uDivide_var_u32_i16
	.quad	_L_1+696
	.quad	_Divide__Divide_param_u32_LC
	.quad	_L_1+716
	.quad	_Divide__uDivide_param_u32_LC
	.quad	_L_1+736
	.quad	_Divide__Divide_var_u32_LC
	.quad	_L_1+757
	.quad	_Divide__uDivide_var_u32_LC
	.quad	_L_1+775
	.quad	_Divide__Divide_param_u32_u64
	.quad	_L_1+794
	.quad	_Divide__uDivide_param_u32_u64
	.quad	_L_1+815
	.quad	_Divide__Divide_var_u32_u64
	.quad	_L_1+837
	.quad	_Divide__uDivide_var_u32_u64
	.quad	_L_1+856
	.quad	_Divide__Divide_param_u32_u16
	.quad	_L_1+876
	.quad	_Divide__uDivide_param_u32_u16
	.quad	_L_1+897
	.quad	_Divide__Divide_var_u32_u16
	.quad	_L_1+919
	.quad	_Divide__uDivide_var_u32_u16
	.quad	_L_1+938
	.quad	_Divide__Divide_param_L_u32
	.quad	_L_1+958
	.quad	_Divide__uDivide_param_L_u32
	.quad	_L_1+977
	.quad	_Divide__Divide_var_L_u32
	.quad	_L_1+997
	.quad	_Divide__uDivide_var_L_u32
	.quad	_L_1+1014
	.quad	_Divide__Divide_param_L_L
	.quad	_L_1+1032
	.quad	_Divide__uDivide_param_L_L
	.quad	_L_1+1049
	.quad	_Divide__Divide_var_L_L
	.quad	_L_1+1067
	.quad	_Divide__uDivide_var_L_L
	.quad	_L_1+1082
	.quad	_Divide__Divide_param_L_i8
	.quad	_L_1+1098
	.quad	_Divide__uDivide_param_L_i8
	.quad	_L_1+1116
	.quad	_Divide__Divide_var_L_i8
	.quad	_L_1+1135
	.quad	_Divide__uDivide_var_L_i8
	.quad	_L_1+1151
	.quad	_Divide__Divide_param_L_u8
	.quad	_L_1+1168
	.quad	_Divide__uDivide_param_L_u8
	.quad	_L_1+1186
	.quad	_Divide__Divide_var_L_u8
	.quad	_L_1+1205
	.quad	_Divide__uDivide_var_L_u8
	.quad	_L_1+1221
	.quad	_Divide__Divide_param_L_C
	.quad	_L_1+1238
	.quad	_Divide__uDivide_param_L_C
	.quad	_L_1+1255
	.quad	_Divide__Divide_var_L_C
	.quad	_L_1+1273
	.quad	_Divide__uDivide_var_L_C
	.quad	_L_1+1288
	.quad	_Divide__Divide_param_L_i64
	.quad	_L_1+1304
	.quad	_Divide__uDivide_param_L_i64
	.quad	_L_1+1323
	.quad	_Divide__Divide_var_L_i64
	.quad	_L_1+1343
	.quad	_Divide__uDivide_var_L_i64
	.quad	_L_1+1360
	.quad	_Divide__Divide_param_L_I
	.quad	_L_1+1378
	.quad	_Divide__uDivide_param_L_I
	.quad	_L_1+1395
	.quad	_Divide__Divide_var_L_I
	.quad	_L_1+1413
	.quad	_Divide__uDivide_var_L_I
	.quad	_L_1+1428
	.quad	_Divide__Divide_param_L_i32
	.quad	_L_1+1444
	.quad	_Divide__uDivide_param_L_i32
	.quad	_L_1+1463
	.quad	_Divide__Divide_var_L_i32
	.quad	_L_1+1483
	.quad	_Divide__uDivide_var_L_i32
	.quad	_L_1+1500
	.quad	_Divide__Divide_param_L_i16
	.quad	_L_1+1518
	.quad	_Divide__uDivide_param_L_i16
	.quad	_L_1+1537
	.quad	_Divide__Divide_var_L_i16
	.quad	_L_1+1557
	.quad	_Divide__uDivide_var_L_i16
	.quad	_L_1+1574
	.quad	_Divide__Divide_param_L_LC
	.quad	_L_1+1592
	.quad	_Divide__uDivide_param_L_LC
	.quad	_L_1+1610
	.quad	_Divide__Divide_var_L_LC
	.quad	_L_1+1629
	.quad	_Divide__uDivide_var_L_LC
	.quad	_L_1+1645
	.quad	_Divide__Divide_param_L_u64
	.quad	_L_1+1662
	.quad	_Divide__uDivide_param_L_u64
	.quad	_L_1+1681
	.quad	_Divide__Divide_var_L_u64
	.quad	_L_1+1701
	.quad	_Divide__uDivide_var_L_u64
	.quad	_L_1+1718
	.quad	_Divide__Divide_param_L_u16
	.quad	_L_1+1736
	.quad	_Divide__uDivide_param_L_u16
	.quad	_L_1+1755
	.quad	_Divide__Divide_var_L_u16
	.quad	_L_1+1775
	.quad	_Divide__uDivide_var_L_u16
	.quad	_L_1+1792
	.quad	_Divide__Divide_param_i8_u32
	.quad	_L_1+1810
	.quad	_Divide__uDivide_param_i8_u32
	.quad	_L_1+1830
	.quad	_Divide__Divide_var_i8_u32
	.quad	_L_1+1851
	.quad	_Divide__uDivide_var_i8_u32
	.quad	_L_1+1869
	.quad	_Divide__Divide_param_i8_L
	.quad	_L_1+1888
	.quad	_Divide__uDivide_param_i8_L
	.quad	_L_1+1906
	.quad	_Divide__Divide_var_i8_L
	.quad	_L_1+1925
	.quad	_Divide__uDivide_var_i8_L
	.quad	_L_1+1941
	.quad	_Divide__Divide_param_i8_i8
	.quad	_L_1+1958
	.quad	_Divide__uDivide_param_i8_i8
	.quad	_L_1+1977
	.quad	_Divide__Divide_var_i8_i8
	.quad	_L_1+1997
	.quad	_Divide__uDivide_var_i8_i8
	.quad	_L_1+2014
	.quad	_Divide__Divide_param_i8_u8
	.quad	_L_1+2032
	.quad	_Divide__uDivide_param_i8_u8
	.quad	_L_1+2051
	.quad	_Divide__Divide_var_i8_u8
	.quad	_L_1+2071
	.quad	_Divide__uDivide_var_i8_u8
	.quad	_L_1+2088
	.quad	_Divide__Divide_param_i8_C
	.quad	_L_1+2106
	.quad	_Divide__uDivide_param_i8_C
	.quad	_L_1+2124
	.quad	_Divide__Divide_var_i8_C
	.quad	_L_1+2143
	.quad	_Divide__uDivide_var_i8_C
	.quad	_L_1+2159
	.quad	_Divide__Divide_param_i8_i64
	.quad	_L_1+2176
	.quad	_Divide__uDivide_param_i8_i64
	.quad	_L_1+2196
	.quad	_Divide__Divide_var_i8_i64
	.quad	_L_1+2217
	.quad	_Divide__uDivide_var_i8_i64
	.quad	_L_1+2235
	.quad	_Divide__Divide_param_i8_I
	.quad	_L_1+2254
	.quad	_Divide__uDivide_param_i8_I
	.quad	_L_1+2272
	.quad	_Divide__Divide_var_i8_I
	.quad	_L_1+2291
	.quad	_Divide__uDivide_var_i8_I
	.quad	_L_1+2307
	.quad	_Divide__Divide_param_i8_i32
	.quad	_L_1+2324
	.quad	_Divide__uDivide_param_i8_i32
	.quad	_L_1+2344
	.quad	_Divide__Divide_var_i8_i32
	.quad	_L_1+2365
	.quad	_Divide__uDivide_var_i8_i32
	.quad	_L_1+2383
	.quad	_Divide__Divide_param_i8_i16
	.quad	_L_1+2402
	.quad	_Divide__uDivide_param_i8_i16
	.quad	_L_1+2422
	.quad	_Divide__Divide_var_i8_i16
	.quad	_L_1+2443
	.quad	_Divide__uDivide_var_i8_i16
	.quad	_L_1+2461
	.quad	_Divide__Divide_param_i8_LC
	.quad	_L_1+2480
	.quad	_Divide__uDivide_param_i8_LC
	.quad	_L_1+2499
	.quad	_Divide__Divide_var_i8_LC
	.quad	_L_1+2519
	.quad	_Divide__uDivide_var_i8_LC
	.quad	_L_1+2536
	.quad	_Divide__Divide_param_i8_u64
	.quad	_L_1+2554
	.quad	_Divide__uDivide_param_i8_u64
	.quad	_L_1+2574
	.quad	_Divide__Divide_var_i8_u64
	.quad	_L_1+2595
	.quad	_Divide__uDivide_var_i8_u64
	.quad	_L_1+2613
	.quad	_Divide__Divide_param_i8_u16
	.quad	_L_1+2632
	.quad	_Divide__uDivide_param_i8_u16
	.quad	_L_1+2652
	.quad	_Divide__Divide_var_i8_u16
	.quad	_L_1+2673
	.quad	_Divide__uDivide_var_i8_u16
	.quad	_L_1+2691
	.quad	_Divide__Divide_param_u8_u32
	.quad	_L_1+2710
	.quad	_Divide__uDivide_param_u8_u32
	.quad	_L_1+2730
	.quad	_Divide__Divide_var_u8_u32
	.quad	_L_1+2751
	.quad	_Divide__uDivide_var_u8_u32
	.quad	_L_1+2769
	.quad	_Divide__Divide_param_u8_L
	.quad	_L_1+2788
	.quad	_Divide__uDivide_param_u8_L
	.quad	_L_1+2806
	.quad	_Divide__Divide_var_u8_L
	.quad	_L_1+2825
	.quad	_Divide__uDivide_var_u8_L
	.quad	_L_1+2841
	.quad	_Divide__Divide_param_u8_i8
	.quad	_L_1+2858
	.quad	_Divide__uDivide_param_u8_i8
	.quad	_L_1+2877
	.quad	_Divide__Divide_var_u8_i8
	.quad	_L_1+2897
	.quad	_Divide__uDivide_var_u8_i8
	.quad	_L_1+2914
	.quad	_Divide__Divide_param_u8_u8
	.quad	_L_1+2932
	.quad	_Divide__uDivide_param_u8_u8
	.quad	_L_1+2951
	.quad	_Divide__Divide_var_u8_u8
	.quad	_L_1+2971
	.quad	_Divide__uDivide_var_u8_u8
	.quad	_L_1+2988
	.quad	_Divide__Divide_param_u8_C
	.quad	_L_1+3006
	.quad	_Divide__uDivide_param_u8_C
	.quad	_L_1+3024
	.quad	_Divide__Divide_var_u8_C
	.quad	_L_1+3043
	.quad	_Divide__uDivide_var_u8_C
	.quad	_L_1+3059
	.quad	_Divide__Divide_param_u8_i64
	.quad	_L_1+3076
	.quad	_Divide__uDivide_param_u8_i64
	.quad	_L_1+3096
	.quad	_Divide__Divide_var_u8_i64
	.quad	_L_1+3117
	.quad	_Divide__uDivide_var_u8_i64
	.quad	_L_1+3135
	.quad	_Divide__Divide_param_u8_I
	.quad	_L_1+3154
	.quad	_Divide__uDivide_param_u8_I
	.quad	_L_1+3172
	.quad	_Divide__Divide_var_u8_I
	.quad	_L_1+3191
	.quad	_Divide__uDivide_var_u8_I
	.quad	_L_1+3207
	.quad	_Divide__Divide_param_u8_i32
	.quad	_L_1+3224
	.quad	_Divide__uDivide_param_u8_i32
	.quad	_L_1+3244
	.quad	_Divide__Divide_var_u8_i32
	.quad	_L_1+3265
	.quad	_Divide__uDivide_var_u8_i32
	.quad	_L_1+3283
	.quad	_Divide__Divide_param_u8_i16
	.quad	_L_1+3302
	.quad	_Divide__uDivide_param_u8_i16
	.quad	_L_1+3322
	.quad	_Divide__Divide_var_u8_i16
	.quad	_L_1+3343
	.quad	_Divide__uDivide_var_u8_i16
	.quad	_L_1+3361
	.quad	_Divide__Divide_param_u8_LC
	.quad	_L_1+3380
	.quad	_Divide__uDivide_param_u8_LC
	.quad	_L_1+3399
	.quad	_Divide__Divide_var_u8_LC
	.quad	_L_1+3419
	.quad	_Divide__uDivide_var_u8_LC
	.quad	_L_1+3436
	.quad	_Divide__Divide_param_u8_u64
	.quad	_L_1+3454
	.quad	_Divide__uDivide_param_u8_u64
	.quad	_L_1+3474
	.quad	_Divide__Divide_var_u8_u64
	.quad	_L_1+3495
	.quad	_Divide__uDivide_var_u8_u64
	.quad	_L_1+3513
	.quad	_Divide__Divide_param_u8_u16
	.quad	_L_1+3532
	.quad	_Divide__uDivide_param_u8_u16
	.quad	_L_1+3552
	.quad	_Divide__Divide_var_u8_u16
	.quad	_L_1+3573
	.quad	_Divide__uDivide_var_u8_u16
	.quad	_L_1+3591
	.quad	_Divide__Divide_param_f32_f32
	.quad	_L_1+3610
	.quad	_Divide__Divide_var_f32_f32
	.quad	_L_1+3631
	.quad	_Divide__Divide_param_C_u32
	.quad	_L_1+3650
	.quad	_Divide__uDivide_param_C_u32
	.quad	_L_1+3669
	.quad	_Divide__Divide_var_C_u32
	.quad	_L_1+3689
	.quad	_Divide__uDivide_var_C_u32
	.quad	_L_1+3706
	.quad	_Divide__Divide_param_C_L
	.quad	_L_1+3724
	.quad	_Divide__uDivide_param_C_L
	.quad	_L_1+3741
	.quad	_Divide__Divide_var_C_L
	.quad	_L_1+3759
	.quad	_Divide__uDivide_var_C_L
	.quad	_L_1+3774
	.quad	_Divide__Divide_param_C_i8
	.quad	_L_1+3790
	.quad	_Divide__uDivide_param_C_i8
	.quad	_L_1+3808
	.quad	_Divide__Divide_var_C_i8
	.quad	_L_1+3827
	.quad	_Divide__uDivide_var_C_i8
	.quad	_L_1+3843
	.quad	_Divide__Divide_param_C_u8
	.quad	_L_1+3860
	.quad	_Divide__uDivide_param_C_u8
	.quad	_L_1+3878
	.quad	_Divide__Divide_var_C_u8
	.quad	_L_1+3897
	.quad	_Divide__uDivide_var_C_u8
	.quad	_L_1+3913
	.quad	_Divide__Divide_param_C_C
	.quad	_L_1+3930
	.quad	_Divide__uDivide_param_C_C
	.quad	_L_1+3947
	.quad	_Divide__Divide_var_C_C
	.quad	_L_1+3965
	.quad	_Divide__uDivide_var_C_C
	.quad	_L_1+3980
	.quad	_Divide__Divide_param_C_i64
	.quad	_L_1+3996
	.quad	_Divide__uDivide_param_C_i64
	.quad	_L_1+4015
	.quad	_Divide__Divide_var_C_i64
	.quad	_L_1+4035
	.quad	_Divide__uDivide_var_C_i64
	.quad	_L_1+4052
	.quad	_Divide__Divide_param_C_I
	.quad	_L_1+4070
	.quad	_Divide__uDivide_param_C_I
	.quad	_L_1+4087
	.quad	_Divide__Divide_var_C_I
	.quad	_L_1+4105
	.quad	_Divide__uDivide_var_C_I
	.quad	_L_1+4120
	.quad	_Divide__Divide_param_C_i32
	.quad	_L_1+4136
	.quad	_Divide__uDivide_param_C_i32
	.quad	_L_1+4155
	.quad	_Divide__Divide_var_C_i32
	.quad	_L_1+4175
	.quad	_Divide__uDivide_var_C_i32
	.quad	_L_1+4192
	.quad	_Divide__Divide_param_C_i16
	.quad	_L_1+4210
	.quad	_Divide__uDivide_param_C_i16
	.quad	_L_1+4229
	.quad	_Divide__Divide_var_C_i16
	.quad	_L_1+4249
	.quad	_Divide__uDivide_var_C_i16
	.quad	_L_1+4266
	.quad	_Divide__Divide_param_C_LC
	.quad	_L_1+4284
	.quad	_Divide__uDivide_param_C_LC
	.quad	_L_1+4302
	.quad	_Divide__Divide_var_C_LC
	.quad	_L_1+4321
	.quad	_Divide__uDivide_var_C_LC
	.quad	_L_1+4337
	.quad	_Divide__Divide_param_C_u64
	.quad	_L_1+4354
	.quad	_Divide__uDivide_param_C_u64
	.quad	_L_1+4373
	.quad	_Divide__Divide_var_C_u64
	.quad	_L_1+4393
	.quad	_Divide__uDivide_var_C_u64
	.quad	_L_1+4410
	.quad	_Divide__Divide_param_C_u16
	.quad	_L_1+4428
	.quad	_Divide__uDivide_param_C_u16
	.quad	_L_1+4447
	.quad	_Divide__Divide_var_C_u16
	.quad	_L_1+4467
	.quad	_Divide__uDivide_var_C_u16
	.quad	_L_1+4484
	.quad	_Divide__Divide_param_i64_u32
	.quad	_L_1+4502
	.quad	_Divide__uDivide_param_i64_u32
	.quad	_L_1+4523
	.quad	_Divide__Divide_var_i64_u32
	.quad	_L_1+4545
	.quad	_Divide__uDivide_var_i64_u32
	.quad	_L_1+4564
	.quad	_Divide__Divide_param_i64_L
	.quad	_L_1+4584
	.quad	_Divide__uDivide_param_i64_L
	.quad	_L_1+4603
	.quad	_Divide__Divide_var_i64_L
	.quad	_L_1+4623
	.quad	_Divide__uDivide_var_i64_L
	.quad	_L_1+4640
	.quad	_Divide__Divide_param_i64_i8
	.quad	_L_1+4658
	.quad	_Divide__uDivide_param_i64_i8
	.quad	_L_1+4678
	.quad	_Divide__Divide_var_i64_i8
	.quad	_L_1+4699
	.quad	_Divide__uDivide_var_i64_i8
	.quad	_L_1+4717
	.quad	_Divide__Divide_param_i64_u8
	.quad	_L_1+4736
	.quad	_Divide__uDivide_param_i64_u8
	.quad	_L_1+4756
	.quad	_Divide__Divide_var_i64_u8
	.quad	_L_1+4777
	.quad	_Divide__uDivide_var_i64_u8
	.quad	_L_1+4795
	.quad	_Divide__Divide_param_i64_C
	.quad	_L_1+4814
	.quad	_Divide__uDivide_param_i64_C
	.quad	_L_1+4833
	.quad	_Divide__Divide_var_i64_C
	.quad	_L_1+4853
	.quad	_Divide__uDivide_var_i64_C
	.quad	_L_1+4870
	.quad	_Divide__Divide_param_i64_i64
	.quad	_L_1+4888
	.quad	_Divide__uDivide_param_i64_i64
	.quad	_L_1+4909
	.quad	_Divide__Divide_var_i64_i64
	.quad	_L_1+4931
	.quad	_Divide__uDivide_var_i64_i64
	.quad	_L_1+4950
	.quad	_Divide__Divide_param_i64_I
	.quad	_L_1+4970
	.quad	_Divide__uDivide_param_i64_I
	.quad	_L_1+4989
	.quad	_Divide__Divide_var_i64_I
	.quad	_L_1+5009
	.quad	_Divide__uDivide_var_i64_I
	.quad	_L_1+5026
	.quad	_Divide__Divide_param_i64_i32
	.quad	_L_1+5044
	.quad	_Divide__uDivide_param_i64_i32
	.quad	_L_1+5065
	.quad	_Divide__Divide_var_i64_i32
	.quad	_L_1+5087
	.quad	_Divide__uDivide_var_i64_i32
	.quad	_L_1+5106
	.quad	_Divide__Divide_param_i64_i16
	.quad	_L_1+5126
	.quad	_Divide__uDivide_param_i64_i16
	.quad	_L_1+5147
	.quad	_Divide__Divide_var_i64_i16
	.quad	_L_1+5169
	.quad	_Divide__uDivide_var_i64_i16
	.quad	_L_1+5188
	.quad	_Divide__Divide_param_i64_LC
	.quad	_L_1+5208
	.quad	_Divide__uDivide_param_i64_LC
	.quad	_L_1+5228
	.quad	_Divide__Divide_var_i64_LC
	.quad	_L_1+5249
	.quad	_Divide__uDivide_var_i64_LC
	.quad	_L_1+5267
	.quad	_Divide__Divide_param_i64_u64
	.quad	_L_1+5286
	.quad	_Divide__uDivide_param_i64_u64
	.quad	_L_1+5307
	.quad	_Divide__Divide_var_i64_u64
	.quad	_L_1+5329
	.quad	_Divide__uDivide_var_i64_u64
	.quad	_L_1+5348
	.quad	_Divide__Divide_param_i64_u16
	.quad	_L_1+5368
	.quad	_Divide__uDivide_param_i64_u16
	.quad	_L_1+5389
	.quad	_Divide__Divide_var_i64_u16
	.quad	_L_1+5411
	.quad	_Divide__uDivide_var_i64_u16
	.quad	_L_1+5430
	.quad	_Divide__Divide_param_I_u32
	.quad	_L_1+5450
	.quad	_Divide__uDivide_param_I_u32
	.quad	_L_1+5469
	.quad	_Divide__Divide_var_I_u32
	.quad	_L_1+5489
	.quad	_Divide__uDivide_var_I_u32
	.quad	_L_1+5506
	.quad	_Divide__Divide_param_I_L
	.quad	_L_1+5524
	.quad	_Divide__uDivide_param_I_L
	.quad	_L_1+5541
	.quad	_Divide__Divide_var_I_L
	.quad	_L_1+5559
	.quad	_Divide__uDivide_var_I_L
	.quad	_L_1+5574
	.quad	_Divide__Divide_param_I_i8
	.quad	_L_1+5590
	.quad	_Divide__uDivide_param_I_i8
	.quad	_L_1+5608
	.quad	_Divide__Divide_var_I_i8
	.quad	_L_1+5627
	.quad	_Divide__uDivide_var_I_i8
	.quad	_L_1+5643
	.quad	_Divide__Divide_param_I_u8
	.quad	_L_1+5660
	.quad	_Divide__uDivide_param_I_u8
	.quad	_L_1+5678
	.quad	_Divide__Divide_var_I_u8
	.quad	_L_1+5697
	.quad	_Divide__uDivide_var_I_u8
	.quad	_L_1+5713
	.quad	_Divide__Divide_param_I_C
	.quad	_L_1+5730
	.quad	_Divide__uDivide_param_I_C
	.quad	_L_1+5747
	.quad	_Divide__Divide_var_I_C
	.quad	_L_1+5765
	.quad	_Divide__uDivide_var_I_C
	.quad	_L_1+5780
	.quad	_Divide__Divide_param_I_i64
	.quad	_L_1+5796
	.quad	_Divide__uDivide_param_I_i64
	.quad	_L_1+5815
	.quad	_Divide__Divide_var_I_i64
	.quad	_L_1+5835
	.quad	_Divide__uDivide_var_I_i64
	.quad	_L_1+5852
	.quad	_Divide__Divide_param_I_I
	.quad	_L_1+5870
	.quad	_Divide__uDivide_param_I_I
	.quad	_L_1+5887
	.quad	_Divide__Divide_var_I_I
	.quad	_L_1+5905
	.quad	_Divide__uDivide_var_I_I
	.quad	_L_1+5920
	.quad	_Divide__Divide_param_I_i32
	.quad	_L_1+5936
	.quad	_Divide__uDivide_param_I_i32
	.quad	_L_1+5955
	.quad	_Divide__Divide_var_I_i32
	.quad	_L_1+5975
	.quad	_Divide__uDivide_var_I_i32
	.quad	_L_1+5992
	.quad	_Divide__Divide_param_I_i16
	.quad	_L_1+6010
	.quad	_Divide__uDivide_param_I_i16
	.quad	_L_1+6029
	.quad	_Divide__Divide_var_I_i16
	.quad	_L_1+6049
	.quad	_Divide__uDivide_var_I_i16
	.quad	_L_1+6066
	.quad	_Divide__Divide_param_I_LC
	.quad	_L_1+6084
	.quad	_Divide__uDivide_param_I_LC
	.quad	_L_1+6102
	.quad	_Divide__Divide_var_I_LC
	.quad	_L_1+6121
	.quad	_Divide__uDivide_var_I_LC
	.quad	_L_1+6137
	.quad	_Divide__Divide_param_I_u64
	.quad	_L_1+6154
	.quad	_Divide__uDivide_param_I_u64
	.quad	_L_1+6173
	.quad	_Divide__Divide_var_I_u64
	.quad	_L_1+6193
	.quad	_Divide__uDivide_var_I_u64
	.quad	_L_1+6210
	.quad	_Divide__Divide_param_I_u16
	.quad	_L_1+6228
	.quad	_Divide__uDivide_param_I_u16
	.quad	_L_1+6247
	.quad	_Divide__Divide_var_I_u16
	.quad	_L_1+6267
	.quad	_Divide__uDivide_var_I_u16
	.quad	_L_1+6284
	.quad	_Divide__Divide_param_i32_u32
	.quad	_L_1+6302
	.quad	_Divide__uDivide_param_i32_u32
	.quad	_L_1+6323
	.quad	_Divide__Divide_var_i32_u32
	.quad	_L_1+6345
	.quad	_Divide__uDivide_var_i32_u32
	.quad	_L_1+6364
	.quad	_Divide__Divide_param_i32_L
	.quad	_L_1+6384
	.quad	_Divide__uDivide_param_i32_L
	.quad	_L_1+6403
	.quad	_Divide__Divide_var_i32_L
	.quad	_L_1+6423
	.quad	_Divide__uDivide_var_i32_L
	.quad	_L_1+6440
	.quad	_Divide__Divide_param_i32_i8
	.quad	_L_1+6458
	.quad	_Divide__uDivide_param_i32_i8
	.quad	_L_1+6478
	.quad	_Divide__Divide_var_i32_i8
	.quad	_L_1+6499
	.quad	_Divide__uDivide_var_i32_i8
	.quad	_L_1+6517
	.quad	_Divide__Divide_param_i32_u8
	.quad	_L_1+6536
	.quad	_Divide__uDivide_param_i32_u8
	.quad	_L_1+6556
	.quad	_Divide__Divide_var_i32_u8
	.quad	_L_1+6577
	.quad	_Divide__uDivide_var_i32_u8
	.quad	_L_1+6595
	.quad	_Divide__Divide_param_i32_C
	.quad	_L_1+6614
	.quad	_Divide__uDivide_param_i32_C
	.quad	_L_1+6633
	.quad	_Divide__Divide_var_i32_C
	.quad	_L_1+6653
	.quad	_Divide__uDivide_var_i32_C
	.quad	_L_1+6670
	.quad	_Divide__Divide_param_i32_i64
	.quad	_L_1+6688
	.quad	_Divide__uDivide_param_i32_i64
	.quad	_L_1+6709
	.quad	_Divide__Divide_var_i32_i64
	.quad	_L_1+6731
	.quad	_Divide__uDivide_var_i32_i64
	.quad	_L_1+6750
	.quad	_Divide__Divide_param_i32_I
	.quad	_L_1+6770
	.quad	_Divide__uDivide_param_i32_I
	.quad	_L_1+6789
	.quad	_Divide__Divide_var_i32_I
	.quad	_L_1+6809
	.quad	_Divide__uDivide_var_i32_I
	.quad	_L_1+6826
	.quad	_Divide__Divide_param_i32_i32
	.quad	_L_1+6844
	.quad	_Divide__uDivide_param_i32_i32
	.quad	_L_1+6865
	.quad	_Divide__Divide_var_i32_i32
	.quad	_L_1+6887
	.quad	_Divide__uDivide_var_i32_i32
	.quad	_L_1+6906
	.quad	_Divide__Divide_param_i32_i16
	.quad	_L_1+6926
	.quad	_Divide__uDivide_param_i32_i16
	.quad	_L_1+6947
	.quad	_Divide__Divide_var_i32_i16
	.quad	_L_1+6969
	.quad	_Divide__uDivide_var_i32_i16
	.quad	_L_1+6988
	.quad	_Divide__Divide_param_i32_LC
	.quad	_L_1+7008
	.quad	_Divide__uDivide_param_i32_LC
	.quad	_L_1+7028
	.quad	_Divide__Divide_var_i32_LC
	.quad	_L_1+7049
	.quad	_Divide__uDivide_var_i32_LC
	.quad	_L_1+7067
	.quad	_Divide__Divide_param_i32_u64
	.quad	_L_1+7086
	.quad	_Divide__uDivide_param_i32_u64
	.quad	_L_1+7107
	.quad	_Divide__Divide_var_i32_u64
	.quad	_L_1+7129
	.quad	_Divide__uDivide_var_i32_u64
	.quad	_L_1+7148
	.quad	_Divide__Divide_param_i32_u16
	.quad	_L_1+7168
	.quad	_Divide__uDivide_param_i32_u16
	.quad	_L_1+7189
	.quad	_Divide__Divide_var_i32_u16
	.quad	_L_1+7211
	.quad	_Divide__uDivide_var_i32_u16
	.quad	_L_1+7230
	.quad	_Divide__Divide_param_i16_u32
	.quad	_L_1+7250
	.quad	_Divide__uDivide_param_i16_u32
	.quad	_L_1+7271
	.quad	_Divide__Divide_var_i16_u32
	.quad	_L_1+7293
	.quad	_Divide__uDivide_var_i16_u32
	.quad	_L_1+7312
	.quad	_Divide__Divide_param_i16_L
	.quad	_L_1+7332
	.quad	_Divide__uDivide_param_i16_L
	.quad	_L_1+7351
	.quad	_Divide__Divide_var_i16_L
	.quad	_L_1+7371
	.quad	_Divide__uDivide_var_i16_L
	.quad	_L_1+7388
	.quad	_Divide__Divide_param_i16_i8
	.quad	_L_1+7406
	.quad	_Divide__uDivide_param_i16_i8
	.quad	_L_1+7426
	.quad	_Divide__Divide_var_i16_i8
	.quad	_L_1+7447
	.quad	_Divide__uDivide_var_i16_i8
	.quad	_L_1+7465
	.quad	_Divide__Divide_param_i16_u8
	.quad	_L_1+7484
	.quad	_Divide__uDivide_param_i16_u8
	.quad	_L_1+7504
	.quad	_Divide__Divide_var_i16_u8
	.quad	_L_1+7525
	.quad	_Divide__uDivide_var_i16_u8
	.quad	_L_1+7543
	.quad	_Divide__Divide_param_i16_C
	.quad	_L_1+7562
	.quad	_Divide__uDivide_param_i16_C
	.quad	_L_1+7581
	.quad	_Divide__Divide_var_i16_C
	.quad	_L_1+7601
	.quad	_Divide__uDivide_var_i16_C
	.quad	_L_1+7618
	.quad	_Divide__Divide_param_i16_i64
	.quad	_L_1+7636
	.quad	_Divide__uDivide_param_i16_i64
	.quad	_L_1+7657
	.quad	_Divide__Divide_var_i16_i64
	.quad	_L_1+7679
	.quad	_Divide__uDivide_var_i16_i64
	.quad	_L_1+7698
	.quad	_Divide__Divide_param_i16_I
	.quad	_L_1+7718
	.quad	_Divide__uDivide_param_i16_I
	.quad	_L_1+7737
	.quad	_Divide__Divide_var_i16_I
	.quad	_L_1+7757
	.quad	_Divide__uDivide_var_i16_I
	.quad	_L_1+7774
	.quad	_Divide__Divide_param_i16_i32
	.quad	_L_1+7792
	.quad	_Divide__uDivide_param_i16_i32
	.quad	_L_1+7813
	.quad	_Divide__Divide_var_i16_i32
	.quad	_L_1+7835
	.quad	_Divide__uDivide_var_i16_i32
	.quad	_L_1+7854
	.quad	_Divide__Divide_param_i16_i16
	.quad	_L_1+7874
	.quad	_Divide__uDivide_param_i16_i16
	.quad	_L_1+7895
	.quad	_Divide__Divide_var_i16_i16
	.quad	_L_1+7917
	.quad	_Divide__uDivide_var_i16_i16
	.quad	_L_1+7936
	.quad	_Divide__Divide_param_i16_LC
	.quad	_L_1+7956
	.quad	_Divide__uDivide_param_i16_LC
	.quad	_L_1+7976
	.quad	_Divide__Divide_var_i16_LC
	.quad	_L_1+7997
	.quad	_Divide__uDivide_var_i16_LC
	.quad	_L_1+8015
	.quad	_Divide__Divide_param_i16_u64
	.quad	_L_1+8034
	.quad	_Divide__uDivide_param_i16_u64
	.quad	_L_1+8055
	.quad	_Divide__Divide_var_i16_u64
	.quad	_L_1+8077
	.quad	_Divide__uDivide_var_i16_u64
	.quad	_L_1+8096
	.quad	_Divide__Divide_param_i16_u16
	.quad	_L_1+8116
	.quad	_Divide__uDivide_param_i16_u16
	.quad	_L_1+8137
	.quad	_Divide__Divide_var_i16_u16
	.quad	_L_1+8159
	.quad	_Divide__uDivide_var_i16_u16
	.quad	_L_1+8178
	.quad	_Divide__Divide_param_f64_f64
	.quad	_L_1+8198
	.quad	_Divide__Divide_var_f64_f64
	.quad	_L_1+8219
	.quad	_Divide__Divide_param_LC_u32
	.quad	_L_1+8238
	.quad	_Divide__uDivide_param_LC_u32
	.quad	_L_1+8258
	.quad	_Divide__Divide_var_LC_u32
	.quad	_L_1+8279
	.quad	_Divide__uDivide_var_LC_u32
	.quad	_L_1+8297
	.quad	_Divide__Divide_param_LC_L
	.quad	_L_1+8316
	.quad	_Divide__uDivide_param_LC_L
	.quad	_L_1+8334
	.quad	_Divide__Divide_var_LC_L
	.quad	_L_1+8353
	.quad	_Divide__uDivide_var_LC_L
	.quad	_L_1+8369
	.quad	_Divide__Divide_param_LC_i8
	.quad	_L_1+8386
	.quad	_Divide__uDivide_param_LC_i8
	.quad	_L_1+8405
	.quad	_Divide__Divide_var_LC_i8
	.quad	_L_1+8425
	.quad	_Divide__uDivide_var_LC_i8
	.quad	_L_1+8442
	.quad	_Divide__Divide_param_LC_u8
	.quad	_L_1+8460
	.quad	_Divide__uDivide_param_LC_u8
	.quad	_L_1+8479
	.quad	_Divide__Divide_var_LC_u8
	.quad	_L_1+8499
	.quad	_Divide__uDivide_var_LC_u8
	.quad	_L_1+8516
	.quad	_Divide__Divide_param_LC_C
	.quad	_L_1+8534
	.quad	_Divide__uDivide_param_LC_C
	.quad	_L_1+8552
	.quad	_Divide__Divide_var_LC_C
	.quad	_L_1+8571
	.quad	_Divide__uDivide_var_LC_C
	.quad	_L_1+8587
	.quad	_Divide__Divide_param_LC_i64
	.quad	_L_1+8604
	.quad	_Divide__uDivide_param_LC_i64
	.quad	_L_1+8624
	.quad	_Divide__Divide_var_LC_i64
	.quad	_L_1+8645
	.quad	_Divide__uDivide_var_LC_i64
	.quad	_L_1+8663
	.quad	_Divide__Divide_param_LC_I
	.quad	_L_1+8682
	.quad	_Divide__uDivide_param_LC_I
	.quad	_L_1+8700
	.quad	_Divide__Divide_var_LC_I
	.quad	_L_1+8719
	.quad	_Divide__uDivide_var_LC_I
	.quad	_L_1+8735
	.quad	_Divide__Divide_param_LC_i32
	.quad	_L_1+8752
	.quad	_Divide__uDivide_param_LC_i32
	.quad	_L_1+8772
	.quad	_Divide__Divide_var_LC_i32
	.quad	_L_1+8793
	.quad	_Divide__uDivide_var_LC_i32
	.quad	_L_1+8811
	.quad	_Divide__Divide_param_LC_i16
	.quad	_L_1+8830
	.quad	_Divide__uDivide_param_LC_i16
	.quad	_L_1+8850
	.quad	_Divide__Divide_var_LC_i16
	.quad	_L_1+8871
	.quad	_Divide__uDivide_var_LC_i16
	.quad	_L_1+8889
	.quad	_Divide__Divide_param_LC_LC
	.quad	_L_1+8908
	.quad	_Divide__uDivide_param_LC_LC
	.quad	_L_1+8927
	.quad	_Divide__Divide_var_LC_LC
	.quad	_L_1+8947
	.quad	_Divide__uDivide_var_LC_LC
	.quad	_L_1+8964
	.quad	_Divide__Divide_param_LC_u64
	.quad	_L_1+8982
	.quad	_Divide__uDivide_param_LC_u64
	.quad	_L_1+9002
	.quad	_Divide__Divide_var_LC_u64
	.quad	_L_1+9023
	.quad	_Divide__uDivide_var_LC_u64
	.quad	_L_1+9041
	.quad	_Divide__Divide_param_LC_u16
	.quad	_L_1+9060
	.quad	_Divide__uDivide_param_LC_u16
	.quad	_L_1+9080
	.quad	_Divide__Divide_var_LC_u16
	.quad	_L_1+9101
	.quad	_Divide__uDivide_var_LC_u16
	.quad	_L_1+9119
	.quad	_Divide__Divide_param_u64_u32
	.quad	_L_1+9138
	.quad	_Divide__uDivide_param_u64_u32
	.quad	_L_1+9159
	.quad	_Divide__Divide_var_u64_u32
	.quad	_L_1+9181
	.quad	_Divide__uDivide_var_u64_u32
	.quad	_L_1+9200
	.quad	_Divide__Divide_param_u64_L
	.quad	_L_1+9220
	.quad	_Divide__uDivide_param_u64_L
	.quad	_L_1+9239
	.quad	_Divide__Divide_var_u64_L
	.quad	_L_1+9259
	.quad	_Divide__uDivide_var_u64_L
	.quad	_L_1+9276
	.quad	_Divide__Divide_param_u64_i8
	.quad	_L_1+9294
	.quad	_Divide__uDivide_param_u64_i8
	.quad	_L_1+9314
	.quad	_Divide__Divide_var_u64_i8
	.quad	_L_1+9335
	.quad	_Divide__uDivide_var_u64_i8
	.quad	_L_1+9353
	.quad	_Divide__Divide_param_u64_u8
	.quad	_L_1+9372
	.quad	_Divide__uDivide_param_u64_u8
	.quad	_L_1+9392
	.quad	_Divide__Divide_var_u64_u8
	.quad	_L_1+9413
	.quad	_Divide__uDivide_var_u64_u8
	.quad	_L_1+9431
	.quad	_Divide__Divide_param_u64_C
	.quad	_L_1+9450
	.quad	_Divide__uDivide_param_u64_C
	.quad	_L_1+9469
	.quad	_Divide__Divide_var_u64_C
	.quad	_L_1+9489
	.quad	_Divide__uDivide_var_u64_C
	.quad	_L_1+9506
	.quad	_Divide__Divide_param_u64_i64
	.quad	_L_1+9524
	.quad	_Divide__uDivide_param_u64_i64
	.quad	_L_1+9545
	.quad	_Divide__Divide_var_u64_i64
	.quad	_L_1+9567
	.quad	_Divide__uDivide_var_u64_i64
	.quad	_L_1+9586
	.quad	_Divide__Divide_param_u64_I
	.quad	_L_1+9606
	.quad	_Divide__uDivide_param_u64_I
	.quad	_L_1+9625
	.quad	_Divide__Divide_var_u64_I
	.quad	_L_1+9645
	.quad	_Divide__uDivide_var_u64_I
	.quad	_L_1+9662
	.quad	_Divide__Divide_param_u64_i32
	.quad	_L_1+9680
	.quad	_Divide__uDivide_param_u64_i32
	.quad	_L_1+9701
	.quad	_Divide__Divide_var_u64_i32
	.quad	_L_1+9723
	.quad	_Divide__uDivide_var_u64_i32
	.quad	_L_1+9742
	.quad	_Divide__Divide_param_u64_i16
	.quad	_L_1+9762
	.quad	_Divide__uDivide_param_u64_i16
	.quad	_L_1+9783
	.quad	_Divide__Divide_var_u64_i16
	.quad	_L_1+9805
	.quad	_Divide__uDivide_var_u64_i16
	.quad	_L_1+9824
	.quad	_Divide__Divide_param_u64_LC
	.quad	_L_1+9844
	.quad	_Divide__uDivide_param_u64_LC
	.quad	_L_1+9864
	.quad	_Divide__Divide_var_u64_LC
	.quad	_L_1+9885
	.quad	_Divide__uDivide_var_u64_LC
	.quad	_L_1+9903
	.quad	_Divide__Divide_param_u64_u64
	.quad	_L_1+9922
	.quad	_Divide__uDivide_param_u64_u64
	.quad	_L_1+9943
	.quad	_Divide__Divide_var_u64_u64
	.quad	_L_1+9965
	.quad	_Divide__uDivide_var_u64_u64
	.quad	_L_1+9984
	.quad	_Divide__Divide_param_u64_u16
	.quad	_L_1+10004
	.quad	_Divide__uDivide_param_u64_u16
	.quad	_L_1+10025
	.quad	_Divide__Divide_var_u64_u16
	.quad	_L_1+10047
	.quad	_Divide__uDivide_var_u64_u16
	.quad	_L_1+10066
	.quad	_Divide__Divide_param_u16_u32
	.quad	_L_1+10086
	.quad	_Divide__uDivide_param_u16_u32
	.quad	_L_1+10107
	.quad	_Divide__Divide_var_u16_u32
	.quad	_L_1+10129
	.quad	_Divide__uDivide_var_u16_u32
	.quad	_L_1+10148
	.quad	_Divide__Divide_param_u16_L
	.quad	_L_1+10168
	.quad	_Divide__uDivide_param_u16_L
	.quad	_L_1+10187
	.quad	_Divide__Divide_var_u16_L
	.quad	_L_1+10207
	.quad	_Divide__uDivide_var_u16_L
	.quad	_L_1+10224
	.quad	_Divide__Divide_param_u16_i8
	.quad	_L_1+10242
	.quad	_Divide__uDivide_param_u16_i8
	.quad	_L_1+10262
	.quad	_Divide__Divide_var_u16_i8
	.quad	_L_1+10283
	.quad	_Divide__uDivide_var_u16_i8
	.quad	_L_1+10301
	.quad	_Divide__Divide_param_u16_u8
	.quad	_L_1+10320
	.quad	_Divide__uDivide_param_u16_u8
	.quad	_L_1+10340
	.quad	_Divide__Divide_var_u16_u8
	.quad	_L_1+10361
	.quad	_Divide__uDivide_var_u16_u8
	.quad	_L_1+10379
	.quad	_Divide__Divide_param_u16_C
	.quad	_L_1+10398
	.quad	_Divide__uDivide_param_u16_C
	.quad	_L_1+10417
	.quad	_Divide__Divide_var_u16_C
	.quad	_L_1+10437
	.quad	_Divide__uDivide_var_u16_C
	.quad	_L_1+10454
	.quad	_Divide__Divide_param_u16_i64
	.quad	_L_1+10472
	.quad	_Divide__uDivide_param_u16_i64
	.quad	_L_1+10493
	.quad	_Divide__Divide_var_u16_i64
	.quad	_L_1+10515
	.quad	_Divide__uDivide_var_u16_i64
	.quad	_L_1+10534
	.quad	_Divide__Divide_param_u16_I
	.quad	_L_1+10554
	.quad	_Divide__uDivide_param_u16_I
	.quad	_L_1+10573
	.quad	_Divide__Divide_var_u16_I
	.quad	_L_1+10593
	.quad	_Divide__uDivide_var_u16_I
	.quad	_L_1+10610
	.quad	_Divide__Divide_param_u16_i32
	.quad	_L_1+10628
	.quad	_Divide__uDivide_param_u16_i32
	.quad	_L_1+10649
	.quad	_Divide__Divide_var_u16_i32
	.quad	_L_1+10671
	.quad	_Divide__uDivide_var_u16_i32
	.quad	_L_1+10690
	.quad	_Divide__Divide_param_u16_i16
	.quad	_L_1+10710
	.quad	_Divide__uDivide_param_u16_i16
	.quad	_L_1+10731
	.quad	_Divide__Divide_var_u16_i16
	.quad	_L_1+10753
	.quad	_Divide__uDivide_var_u16_i16
	.quad	_L_1+10772
	.quad	_Divide__Divide_param_u16_LC
	.quad	_L_1+10792
	.quad	_Divide__uDivide_param_u16_LC
	.quad	_L_1+10812
	.quad	_Divide__Divide_var_u16_LC
	.quad	_L_1+10833
	.quad	_Divide__uDivide_var_u16_LC
	.quad	_L_1+10851
	.quad	_Divide__Divide_param_u16_u64
	.quad	_L_1+10870
	.quad	_Divide__uDivide_param_u16_u64
	.quad	_L_1+10891
	.quad	_Divide__Divide_var_u16_u64
	.quad	_L_1+10913
	.quad	_Divide__uDivide_var_u16_u64
	.quad	_L_1+10932
	.quad	_Divide__Divide_param_u16_u16
	.quad	_L_1+10952
	.quad	_Divide__uDivide_param_u16_u16
	.quad	_L_1+10973
	.quad	_Divide__Divide_var_u16_u16
	.quad	_L_1+10995
	.quad	_Divide__uDivide_var_u16_u16
	.quad	_L_1+11014
	.space 8
	.ascii "../AMD64_DARWIN/Divide.m3"
	.space 7
	.data
	.align 5
_MM_Divide:
	.quad	_L_1+20344
	.space 32
	.quad	_L_1+11040
	.space 24
	.quad	_MM_Divide+208
	.space 8
	.quad	_Divide_M3
	.quad	3
	.word	241
	.space 6
	.quad	242
	.quad	243
	.long	171798692
	.long	1080985559
	.word	246
	.space 2
	.long	247
	.quad	248
	.quad	249
	.quad	250
	.long	1132150915
	.byte	-3
	.byte	-2
	.space 2
	.quad	255
	.long	256
	.space 28
	.quad	_Divide_I3
	.quad	_MM_Divide+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Divide+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Divide+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Divide+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
