	.text
.globl _Divide__uDivide_var_i64_i64
	.private_extern _Divide__uDivide_var_i64_i64
_Divide__uDivide_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	je	L8
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L8
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L8:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_LC
	.private_extern _Divide__uDivide_var_i64_LC
_Divide__uDivide_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_i64_LC
	.private_extern _Divide__Divide_var_i64_LC
_Divide__Divide_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
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
	je	L13
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L13
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L13:
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
	je	L18
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L18
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L18:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u8
	.private_extern _Divide__uDivide_var_i64_u8
_Divide__uDivide_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L23
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L23
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L23:
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
	je	L28
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L28
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L28:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_I
	.private_extern _Divide__uDivide_var_i64_I
_Divide__uDivide_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L33
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L33
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L33:
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
	je	L38
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L38
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L38:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i8
	.private_extern _Divide__uDivide_var_i64_i8
_Divide__uDivide_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L43
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L43
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L43:
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
	je	L48
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L48
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L48:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u32
	.private_extern _Divide__uDivide_var_i64_u32
_Divide__uDivide_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L53
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L53
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L53:
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
	je	L58
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L58
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L58:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_C
	.private_extern _Divide__uDivide_var_i64_C
_Divide__uDivide_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_i64_C
	.private_extern _Divide__Divide_var_i64_C
_Divide__Divide_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
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
	je	L63
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L63
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L63:
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
	je	L68
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L68
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L68:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u16
	.private_extern _Divide__uDivide_var_i64_u16
_Divide__uDivide_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L73
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L73
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L73:
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
	je	L78
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L78
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L78:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_u64
	.private_extern _Divide__uDivide_var_i64_u64
_Divide__uDivide_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_i64_u64
	.private_extern _Divide__Divide_var_i64_u64
_Divide__Divide_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L83
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L83
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L83:
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
	je	L88
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L88
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L88:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_L
	.private_extern _Divide__uDivide_var_i64_L
_Divide__uDivide_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
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
	movq	104+_MM_Divide(%rip), %rax
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
	je	L93
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L93
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L93:
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
	je	L98
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L98
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L98:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i16
	.private_extern _Divide__uDivide_var_i64_i16
_Divide__uDivide_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
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
	je	L108
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L108
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L108:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i64_i32
	.private_extern _Divide__uDivide_var_i64_i32
_Divide__uDivide_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L113
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L113
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L113:
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
	je	L118
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L118
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L118:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i64
	.private_extern _Divide__uDivide_var_LC_i64
_Divide__uDivide_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L123
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L123
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L123:
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
	je	L128
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L128
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L128:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_LC
	.private_extern _Divide__uDivide_var_LC_LC
_Divide__uDivide_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rdx
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
	je	L137
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L137
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L137:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u8
	.private_extern _Divide__uDivide_var_LC_u8
_Divide__uDivide_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L142
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L142
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L142:
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
	je	L147
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L147
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L147:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_I
	.private_extern _Divide__uDivide_var_LC_I
_Divide__uDivide_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L152
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L152
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L152:
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
	je	L157
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L157
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L157:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i8
	.private_extern _Divide__uDivide_var_LC_i8
_Divide__uDivide_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L162
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L162
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L162:
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
	je	L167
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L167
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L167:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u32
	.private_extern _Divide__uDivide_var_LC_u32
_Divide__uDivide_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L172
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L172
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L172:
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
	je	L177
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L177
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L177:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_C
	.private_extern _Divide__uDivide_var_LC_C
_Divide__uDivide_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rdx
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
	movq	112+_MM_Divide(%rip), %rax
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
	je	L182
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L182
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L182:
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
	je	L187
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L187
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L187:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u16
	.private_extern _Divide__uDivide_var_LC_u16
_Divide__uDivide_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L192
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L192
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L192:
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
	je	L197
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L197
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L197:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_u64
	.private_extern _Divide__uDivide_var_LC_u64
_Divide__uDivide_var_LC_u64:
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
.globl _Divide__Divide_var_LC_u64
	.private_extern _Divide__Divide_var_LC_u64
_Divide__Divide_var_LC_u64:
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
	je	L202
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L202
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L202:
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
	je	L207
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L207
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L207:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_L
	.private_extern _Divide__uDivide_var_LC_L
_Divide__uDivide_var_LC_L:
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
.globl _Divide__Divide_var_LC_L
	.private_extern _Divide__Divide_var_LC_L
_Divide__Divide_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
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
	je	L212
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L212
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L212:
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
	je	L217
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L217
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L217:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i16
	.private_extern _Divide__uDivide_var_LC_i16
_Divide__uDivide_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L222
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L222
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L222:
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
	je	L227
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L227
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L227:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_LC_i32
	.private_extern _Divide__uDivide_var_LC_i32
_Divide__uDivide_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L232
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L232
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L232:
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
	je	L237
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L237
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L237:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_f32_f32
	.private_extern _Divide__Divide_var_f32_f32
_Divide__Divide_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	120+_MM_Divide(%rip), %xmm1
	movss	120+_MM_Divide(%rip), %xmm0
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
.globl _Divide__uDivide_var_u8_i64
	.private_extern _Divide__uDivide_var_u8_i64
_Divide__uDivide_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L246
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L246
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L246:
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
	je	L251
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L251
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L251:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_LC
	.private_extern _Divide__uDivide_var_u8_LC
_Divide__uDivide_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u8_LC
	.private_extern _Divide__Divide_var_u8_LC
_Divide__Divide_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
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
	je	L256
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L256
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L256:
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
	je	L261
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L261
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L261:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u8
	.private_extern _Divide__uDivide_var_u8_u8
_Divide__uDivide_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L266
	cmpq	$127, -16(%rbp)
	jle	L267
L266:
	movl	$5153, %edi
	call	__m3_fault
L267:
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
	je	L272
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L272
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L272:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L273
	cmpq	$127, -16(%rbp)
	jle	L274
L273:
	movl	$5217, %edi
	call	__m3_fault
L274:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_I
	.private_extern _Divide__uDivide_var_u8_I
_Divide__uDivide_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L279
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L279
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L279:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L280
	cmpq	$127, -16(%rbp)
	jle	L281
L280:
	movl	$5281, %edi
	call	__m3_fault
L281:
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
	je	L286
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L286
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L286:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L287
	cmpq	$127, -16(%rbp)
	jle	L288
L287:
	movl	$5345, %edi
	call	__m3_fault
L288:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i8
	.private_extern _Divide__uDivide_var_u8_i8
_Divide__uDivide_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L293
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L293
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L293:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L294
	cmpq	$127, -16(%rbp)
	jle	L295
L294:
	movl	$5409, %edi
	call	__m3_fault
L295:
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
	je	L300
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L300
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L300:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L301
	cmpq	$127, -16(%rbp)
	jle	L302
L301:
	movl	$5473, %edi
	call	__m3_fault
L302:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u32
	.private_extern _Divide__uDivide_var_u8_u32
_Divide__uDivide_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L307
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L307
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L307:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L308
	cmpq	$127, -16(%rbp)
	jle	L309
L308:
	movl	$5537, %edi
	call	__m3_fault
L309:
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
	je	L314
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L314
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L314:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L315
	cmpq	$127, -16(%rbp)
	jle	L316
L315:
	movl	$5601, %edi
	call	__m3_fault
L316:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_C
	.private_extern _Divide__uDivide_var_u8_C
_Divide__uDivide_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u8_C
	.private_extern _Divide__Divide_var_u8_C
_Divide__Divide_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	124+_MM_Divide(%rip), %eax
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
	je	L321
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L321
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L321:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L322
	cmpq	$127, -16(%rbp)
	jle	L323
L322:
	movl	$5665, %edi
	call	__m3_fault
L323:
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
	je	L328
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L328
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L328:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L329
	cmpq	$127, -16(%rbp)
	jle	L330
L329:
	movl	$5729, %edi
	call	__m3_fault
L330:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u16
	.private_extern _Divide__uDivide_var_u8_u16
_Divide__uDivide_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L335
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L335
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L335:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L336
	cmpq	$127, -16(%rbp)
	jle	L337
L336:
	movl	$5793, %edi
	call	__m3_fault
L337:
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
	je	L342
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L342
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L342:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L343
	cmpq	$127, -16(%rbp)
	jle	L344
L343:
	movl	$5857, %edi
	call	__m3_fault
L344:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_u64
	.private_extern _Divide__uDivide_var_u8_u64
_Divide__uDivide_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u8_u64
	.private_extern _Divide__Divide_var_u8_u64
_Divide__Divide_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
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
	je	L349
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L349
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L349:
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
	je	L354
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L354
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L354:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_L
	.private_extern _Divide__uDivide_var_u8_L
_Divide__uDivide_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
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
	je	L359
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L359
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L359:
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
	je	L364
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L364
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L364:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i16
	.private_extern _Divide__uDivide_var_u8_i16
_Divide__uDivide_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L369
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L369
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L369:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L370
	cmpq	$127, -16(%rbp)
	jle	L371
L370:
	movl	$6177, %edi
	call	__m3_fault
L371:
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
	je	L376
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L376
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L376:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L377
	cmpq	$127, -16(%rbp)
	jle	L378
L377:
	movl	$6241, %edi
	call	__m3_fault
L378:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u8_i32
	.private_extern _Divide__uDivide_var_u8_i32
_Divide__uDivide_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L384
	cmpq	$127, -16(%rbp)
	jle	L385
L384:
	movl	$6305, %edi
	call	__m3_fault
L385:
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
	je	L390
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L390
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L390:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L391
	cmpq	$127, -16(%rbp)
	jle	L392
L391:
	movl	$6369, %edi
	call	__m3_fault
L392:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i64
	.private_extern _Divide__uDivide_var_I_i64
_Divide__uDivide_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
	je	L402
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L402
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L402:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_LC
	.private_extern _Divide__uDivide_var_I_LC
_Divide__uDivide_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_I_LC
	.private_extern _Divide__Divide_var_I_LC
_Divide__Divide_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
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
	je	L407
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L407
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L407:
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
	je	L412
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L412
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L412:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u8
	.private_extern _Divide__uDivide_var_I_u8
_Divide__uDivide_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L417
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L417
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L417:
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
	je	L422
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L422
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L422:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_I
	.private_extern _Divide__uDivide_var_I_I
_Divide__uDivide_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	je	L431
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L431
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L431:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i8
	.private_extern _Divide__uDivide_var_I_i8
_Divide__uDivide_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L436
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L436
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L436:
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
	je	L441
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L441
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L441:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u32
	.private_extern _Divide__uDivide_var_I_u32
_Divide__uDivide_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L446
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L446
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L446:
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
	je	L451
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L451
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L451:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_C
	.private_extern _Divide__uDivide_var_I_C
_Divide__uDivide_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_I_C
	.private_extern _Divide__Divide_var_I_C
_Divide__Divide_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
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
	je	L456
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L456
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L456:
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
	je	L461
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L461
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L461:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u16
	.private_extern _Divide__uDivide_var_I_u16
_Divide__uDivide_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L466
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L466
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L466:
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
	je	L471
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L471
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L471:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_u64
	.private_extern _Divide__uDivide_var_I_u64
_Divide__uDivide_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_I_u64
	.private_extern _Divide__Divide_var_I_u64
_Divide__Divide_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L476
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L476
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L476:
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
	je	L481
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L481
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L481:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_L
	.private_extern _Divide__uDivide_var_I_L
_Divide__uDivide_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
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
	movq	128+_MM_Divide(%rip), %rax
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
	je	L486
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L486
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L486:
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
	je	L491
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L491
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L491:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i16
	.private_extern _Divide__uDivide_var_I_i16
_Divide__uDivide_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
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
	je	L501
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L501
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L501:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_I_i32
	.private_extern _Divide__uDivide_var_I_i32
_Divide__uDivide_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L506
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L506
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L506:
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
	je	L511
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L511
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L511:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i64
	.private_extern _Divide__uDivide_var_i8_i64
_Divide__uDivide_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L516
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L516
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L516:
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
	je	L521
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L521
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L521:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_LC
	.private_extern _Divide__uDivide_var_i8_LC
_Divide__uDivide_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i8_LC
	.private_extern _Divide__Divide_var_i8_LC
_Divide__Divide_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
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
	je	L526
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L526
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L526:
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
	je	L531
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L531
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L531:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u8
	.private_extern _Divide__uDivide_var_i8_u8
_Divide__uDivide_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L536
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L536
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L536:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L537
	cmpq	$255, -16(%rbp)
	jle	L538
L537:
	movl	$8225, %edi
	call	__m3_fault
L538:
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
	je	L543
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L543
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L543:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L544
	cmpq	$255, -16(%rbp)
	jle	L545
L544:
	movl	$8289, %edi
	call	__m3_fault
L545:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_I
	.private_extern _Divide__uDivide_var_i8_I
_Divide__uDivide_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L550
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L550
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L550:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L551
	cmpq	$255, -16(%rbp)
	jle	L552
L551:
	movl	$8353, %edi
	call	__m3_fault
L552:
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
	je	L557
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L557
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L557:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L558
	cmpq	$255, -16(%rbp)
	jle	L559
L558:
	movl	$8417, %edi
	call	__m3_fault
L559:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i8
	.private_extern _Divide__uDivide_var_i8_i8
_Divide__uDivide_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L564
	cmpq	$255, -16(%rbp)
	jle	L565
L564:
	movl	$8481, %edi
	call	__m3_fault
L565:
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
	je	L570
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L570
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L570:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L571
	cmpq	$255, -16(%rbp)
	jle	L572
L571:
	movl	$8545, %edi
	call	__m3_fault
L572:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u32
	.private_extern _Divide__uDivide_var_i8_u32
_Divide__uDivide_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L577
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L577
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L577:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L578
	cmpq	$255, -16(%rbp)
	jle	L579
L578:
	movl	$8609, %edi
	call	__m3_fault
L579:
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
	je	L584
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L584
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L584:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L585
	cmpq	$255, -16(%rbp)
	jle	L586
L585:
	movl	$8673, %edi
	call	__m3_fault
L586:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_C
	.private_extern _Divide__uDivide_var_i8_C
_Divide__uDivide_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i8_C
	.private_extern _Divide__Divide_var_i8_C
_Divide__Divide_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	136+_MM_Divide(%rip), %eax
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
	je	L591
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L591
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L591:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L592
	cmpq	$255, -16(%rbp)
	jle	L593
L592:
	movl	$8737, %edi
	call	__m3_fault
L593:
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
	je	L598
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L598
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L598:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L599
	cmpq	$255, -16(%rbp)
	jle	L600
L599:
	movl	$8801, %edi
	call	__m3_fault
L600:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u16
	.private_extern _Divide__uDivide_var_i8_u16
_Divide__uDivide_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L605
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L605
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L605:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L606
	cmpq	$255, -16(%rbp)
	jle	L607
L606:
	movl	$8865, %edi
	call	__m3_fault
L607:
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
	je	L612
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L612
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L612:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L613
	cmpq	$255, -16(%rbp)
	jle	L614
L613:
	movl	$8929, %edi
	call	__m3_fault
L614:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_u64
	.private_extern _Divide__uDivide_var_i8_u64
_Divide__uDivide_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i8_u64
	.private_extern _Divide__Divide_var_i8_u64
_Divide__Divide_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
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
	je	L619
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L619
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L619:
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
	je	L624
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L624
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L624:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_L
	.private_extern _Divide__uDivide_var_i8_L
_Divide__uDivide_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
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
	je	L629
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L629
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L629:
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
	je	L634
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L634
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L634:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i16
	.private_extern _Divide__uDivide_var_i8_i16
_Divide__uDivide_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L639
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L639
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L639:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L640
	cmpq	$255, -16(%rbp)
	jle	L641
L640:
	movl	$9249, %edi
	call	__m3_fault
L641:
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
	je	L646
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L646
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L646:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L647
	cmpq	$255, -16(%rbp)
	jle	L648
L647:
	movl	$9313, %edi
	call	__m3_fault
L648:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i8_i32
	.private_extern _Divide__uDivide_var_i8_i32
_Divide__uDivide_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L653
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L653
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L653:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L654
	cmpq	$255, -16(%rbp)
	jle	L655
L654:
	movl	$9377, %edi
	call	__m3_fault
L655:
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
	je	L660
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L660
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L660:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L661
	cmpq	$255, -16(%rbp)
	jle	L662
L661:
	movl	$9441, %edi
	call	__m3_fault
L662:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i64
	.private_extern _Divide__uDivide_var_u32_i64
_Divide__uDivide_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L667
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L667
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L667:
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
	je	L672
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L672
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L672:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_LC
	.private_extern _Divide__uDivide_var_u32_LC
_Divide__uDivide_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u32_LC
	.private_extern _Divide__Divide_var_u32_LC
_Divide__Divide_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
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
	je	L677
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L677
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L677:
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
	je	L682
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L682
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L682:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u8
	.private_extern _Divide__uDivide_var_u32_u8
_Divide__uDivide_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L687
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L687
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L687:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L688
	cmpq	$2147483647, -16(%rbp)
	jle	L689
L688:
	movl	$9761, %edi
	call	__m3_fault
L689:
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
	je	L694
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L694
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L694:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L695
	cmpq	$2147483647, -16(%rbp)
	jle	L696
L695:
	movl	$9825, %edi
	call	__m3_fault
L696:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_I
	.private_extern _Divide__uDivide_var_u32_I
_Divide__uDivide_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L701
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L701
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L701:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L702
	cmpq	$2147483647, -16(%rbp)
	jle	L703
L702:
	movl	$9889, %edi
	call	__m3_fault
L703:
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
	je	L708
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L708
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L708:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L709
	cmpq	$2147483647, -16(%rbp)
	jle	L710
L709:
	movl	$9953, %edi
	call	__m3_fault
L710:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i8
	.private_extern _Divide__uDivide_var_u32_i8
_Divide__uDivide_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L715
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L715
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L715:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L716
	cmpq	$2147483647, -16(%rbp)
	jle	L717
L716:
	movl	$10017, %edi
	call	__m3_fault
L717:
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
	je	L722
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L722
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L722:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L723
	cmpq	$2147483647, -16(%rbp)
	jle	L724
L723:
	movl	$10081, %edi
	call	__m3_fault
L724:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u32
	.private_extern _Divide__uDivide_var_u32_u32
_Divide__uDivide_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L729
	cmpq	$2147483647, -16(%rbp)
	jle	L730
L729:
	movl	$10145, %edi
	call	__m3_fault
L730:
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
	je	L735
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L735
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L735:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L736
	cmpq	$2147483647, -16(%rbp)
	jle	L737
L736:
	movl	$10209, %edi
	call	__m3_fault
L737:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_C
	.private_extern _Divide__uDivide_var_u32_C
_Divide__uDivide_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u32_C
	.private_extern _Divide__Divide_var_u32_C
_Divide__Divide_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Divide(%rip), %eax
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
	je	L742
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L742
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L742:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L743
	cmpq	$2147483647, -16(%rbp)
	jle	L744
L743:
	movl	$10273, %edi
	call	__m3_fault
L744:
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
	je	L749
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L749
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L749:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L750
	cmpq	$2147483647, -16(%rbp)
	jle	L751
L750:
	movl	$10337, %edi
	call	__m3_fault
L751:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u16
	.private_extern _Divide__uDivide_var_u32_u16
_Divide__uDivide_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L756
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L756
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L756:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L757
	cmpq	$2147483647, -16(%rbp)
	jle	L758
L757:
	movl	$10401, %edi
	call	__m3_fault
L758:
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
	je	L763
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L763
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L763:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L764
	cmpq	$2147483647, -16(%rbp)
	jle	L765
L764:
	movl	$10465, %edi
	call	__m3_fault
L765:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_u64
	.private_extern _Divide__uDivide_var_u32_u64
_Divide__uDivide_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u32_u64
	.private_extern _Divide__Divide_var_u32_u64
_Divide__Divide_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
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
	je	L770
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L770
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L770:
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
	je	L775
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L775
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L775:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_L
	.private_extern _Divide__uDivide_var_u32_L
_Divide__uDivide_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
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
	je	L780
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L780
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L780:
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
	je	L785
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L785
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L785:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i16
	.private_extern _Divide__uDivide_var_u32_i16
_Divide__uDivide_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L790
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L790
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L790:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L791
	cmpq	$2147483647, -16(%rbp)
	jle	L792
L791:
	movl	$10785, %edi
	call	__m3_fault
L792:
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
	je	L797
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L797
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L797:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L798
	cmpq	$2147483647, -16(%rbp)
	jle	L799
L798:
	movl	$10849, %edi
	call	__m3_fault
L799:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u32_i32
	.private_extern _Divide__uDivide_var_u32_i32
_Divide__uDivide_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L804
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L804
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L804:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L805
	cmpq	$2147483647, -16(%rbp)
	jle	L806
L805:
	movl	$10913, %edi
	call	__m3_fault
L806:
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
	je	L811
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L811
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L811:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L812
	cmpq	$2147483647, -16(%rbp)
	jle	L813
L812:
	movl	$10977, %edi
	call	__m3_fault
L813:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i64
	.private_extern _Divide__uDivide_var_C_i64
_Divide__uDivide_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	je	L823
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L823
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L823:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_LC
	.private_extern _Divide__uDivide_var_C_LC
_Divide__uDivide_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	112+_MM_Divide(%rip), %rdx
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
	movq	144+_MM_Divide(%rip), %rax
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
	je	L833
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L833
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L833:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u8
	.private_extern _Divide__uDivide_var_C_u8
_Divide__uDivide_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L839
	movl	$11297, %edi
	call	__m3_fault
L839:
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
	je	L844
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L844
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L844:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L845
	movl	$11361, %edi
	call	__m3_fault
L845:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_I
	.private_extern _Divide__uDivide_var_C_I
_Divide__uDivide_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L850
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L850
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L850:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L851
	movl	$11425, %edi
	call	__m3_fault
L851:
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
	je	L856
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L856
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L856:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L857
	movl	$11489, %edi
	call	__m3_fault
L857:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i8
	.private_extern _Divide__uDivide_var_C_i8
_Divide__uDivide_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L862
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L862
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L862:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L863
	movl	$11553, %edi
	call	__m3_fault
L863:
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
	je	L868
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L868
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L868:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L869
	movl	$11617, %edi
	call	__m3_fault
L869:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u32
	.private_extern _Divide__uDivide_var_C_u32
_Divide__uDivide_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L874
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L874
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L874:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L875
	movl	$11681, %edi
	call	__m3_fault
L875:
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
	je	L880
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L880
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L880:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L881
	movl	$11745, %edi
	call	__m3_fault
L881:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_C
	.private_extern _Divide__uDivide_var_C_C
_Divide__uDivide_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	144+_MM_Divide(%rip), %rdx
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	144+_MM_Divide(%rip), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L886
	movl	$11809, %edi
	call	__m3_fault
L886:
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
	je	L891
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L891
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L891:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L892
	movl	$11873, %edi
	call	__m3_fault
L892:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u16
	.private_extern _Divide__uDivide_var_C_u16
_Divide__uDivide_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L898
	movl	$11937, %edi
	call	__m3_fault
L898:
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
	je	L903
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L903
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L903:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L904
	movl	$12001, %edi
	call	__m3_fault
L904:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_u64
	.private_extern _Divide__uDivide_var_C_u64
_Divide__uDivide_var_C_u64:
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
.globl _Divide__Divide_var_C_u64
	.private_extern _Divide__Divide_var_C_u64
_Divide__Divide_var_C_u64:
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
	je	L909
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L909
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L909:
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
	je	L914
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L914
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L914:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_L
	.private_extern _Divide__uDivide_var_C_L
_Divide__uDivide_var_C_L:
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
.globl _Divide__Divide_var_C_L
	.private_extern _Divide__Divide_var_C_L
_Divide__Divide_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
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
	je	L919
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L919
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L919:
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
	je	L924
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L924
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L924:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i16
	.private_extern _Divide__uDivide_var_C_i16
_Divide__uDivide_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L929
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L929
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L929:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L930
	movl	$12321, %edi
	call	__m3_fault
L930:
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
	je	L935
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L935
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L935:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L936
	movl	$12385, %edi
	call	__m3_fault
L936:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_C_i32
	.private_extern _Divide__uDivide_var_C_i32
_Divide__uDivide_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movq	144+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L941
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L941
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L941:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L942
	movl	$12449, %edi
	call	__m3_fault
L942:
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
	je	L947
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L947
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L947:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L948
	movl	$12513, %edi
	call	__m3_fault
L948:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i64
	.private_extern _Divide__uDivide_var_u16_i64
_Divide__uDivide_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L953
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L953
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L953:
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
	je	L958
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L958
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L958:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_LC
	.private_extern _Divide__uDivide_var_u16_LC
_Divide__uDivide_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u16_LC
	.private_extern _Divide__Divide_var_u16_LC
_Divide__Divide_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
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
	je	L963
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L963
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L963:
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
	je	L968
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L968
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L968:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u8
	.private_extern _Divide__uDivide_var_u16_u8
_Divide__uDivide_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L973
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L973
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L973:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L974
	cmpq	$32767, -16(%rbp)
	jle	L975
L974:
	movl	$12833, %edi
	call	__m3_fault
L975:
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
	je	L980
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L980
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L980:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L981
	cmpq	$32767, -16(%rbp)
	jle	L982
L981:
	movl	$12897, %edi
	call	__m3_fault
L982:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_I
	.private_extern _Divide__uDivide_var_u16_I
_Divide__uDivide_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L987
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L987
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L987:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L988
	cmpq	$32767, -16(%rbp)
	jle	L989
L988:
	movl	$12961, %edi
	call	__m3_fault
L989:
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
	je	L994
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L994
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L994:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L995
	cmpq	$32767, -16(%rbp)
	jle	L996
L995:
	movl	$13025, %edi
	call	__m3_fault
L996:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i8
	.private_extern _Divide__uDivide_var_u16_i8
_Divide__uDivide_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1001
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1001
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1001:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1002
	cmpq	$32767, -16(%rbp)
	jle	L1003
L1002:
	movl	$13089, %edi
	call	__m3_fault
L1003:
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
	je	L1008
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1008
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1008:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1009
	cmpq	$32767, -16(%rbp)
	jle	L1010
L1009:
	movl	$13153, %edi
	call	__m3_fault
L1010:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u32
	.private_extern _Divide__uDivide_var_u16_u32
_Divide__uDivide_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1015
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1015
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1015:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1016
	cmpq	$32767, -16(%rbp)
	jle	L1017
L1016:
	movl	$13217, %edi
	call	__m3_fault
L1017:
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
	je	L1022
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1022
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1022:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1023
	cmpq	$32767, -16(%rbp)
	jle	L1024
L1023:
	movl	$13281, %edi
	call	__m3_fault
L1024:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_C
	.private_extern _Divide__uDivide_var_u16_C
_Divide__uDivide_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u16_C
	.private_extern _Divide__Divide_var_u16_C
_Divide__Divide_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	152+_MM_Divide(%rip), %eax
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
	je	L1029
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1029
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1029:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1030
	cmpq	$32767, -16(%rbp)
	jle	L1031
L1030:
	movl	$13345, %edi
	call	__m3_fault
L1031:
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
	je	L1036
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1036
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1036:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1037
	cmpq	$32767, -16(%rbp)
	jle	L1038
L1037:
	movl	$13409, %edi
	call	__m3_fault
L1038:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u16
	.private_extern _Divide__uDivide_var_u16_u16
_Divide__uDivide_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1043
	cmpq	$32767, -16(%rbp)
	jle	L1044
L1043:
	movl	$13473, %edi
	call	__m3_fault
L1044:
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
	je	L1049
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1049
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1049:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1050
	cmpq	$32767, -16(%rbp)
	jle	L1051
L1050:
	movl	$13537, %edi
	call	__m3_fault
L1051:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_u64
	.private_extern _Divide__uDivide_var_u16_u64
_Divide__uDivide_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_u16_u64
	.private_extern _Divide__Divide_var_u16_u64
_Divide__Divide_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
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
	je	L1056
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1056
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1056:
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
	je	L1061
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1061
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1061:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_L
	.private_extern _Divide__uDivide_var_u16_L
_Divide__uDivide_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
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
	je	L1066
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1066
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1066:
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
	je	L1071
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1071
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1071:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i16
	.private_extern _Divide__uDivide_var_u16_i16
_Divide__uDivide_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
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
	cmpq	$-32768, -16(%rbp)
	jl	L1077
	cmpq	$32767, -16(%rbp)
	jle	L1078
L1077:
	movl	$13857, %edi
	call	__m3_fault
L1078:
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
	cmpq	$-32768, -16(%rbp)
	jl	L1084
	cmpq	$32767, -16(%rbp)
	jle	L1085
L1084:
	movl	$13921, %edi
	call	__m3_fault
L1085:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u16_i32
	.private_extern _Divide__uDivide_var_u16_i32
_Divide__uDivide_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1091
	cmpq	$32767, -16(%rbp)
	jle	L1092
L1091:
	movl	$13985, %edi
	call	__m3_fault
L1092:
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
	je	L1097
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1097
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1097:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1098
	cmpq	$32767, -16(%rbp)
	jle	L1099
L1098:
	movl	$14049, %edi
	call	__m3_fault
L1099:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i64
	.private_extern _Divide__uDivide_var_u64_i64
_Divide__uDivide_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1104
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1104
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1104:
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
	je	L1109
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1109
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1109:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_LC
	.private_extern _Divide__uDivide_var_u64_LC
_Divide__uDivide_var_u64_LC:
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
.globl _Divide__Divide_var_u64_LC
	.private_extern _Divide__Divide_var_u64_LC
_Divide__Divide_var_u64_LC:
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
	je	L1114
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1114
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1114:
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
	je	L1119
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1119
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1119:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u8
	.private_extern _Divide__uDivide_var_u64_u8
_Divide__uDivide_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
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
	je	L1129
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1129
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1129:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_I
	.private_extern _Divide__uDivide_var_u64_I
_Divide__uDivide_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1134
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1134
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1134:
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
	je	L1139
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1139
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1139:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i8
	.private_extern _Divide__uDivide_var_u64_i8
_Divide__uDivide_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1144
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1144
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1144:
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
	je	L1149
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1149
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1149:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u32
	.private_extern _Divide__uDivide_var_u64_u32
_Divide__uDivide_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1154
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1154
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1154:
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
	je	L1159
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1159
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1159:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_C
	.private_extern _Divide__uDivide_var_u64_C
_Divide__uDivide_var_u64_C:
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
.globl _Divide__Divide_var_u64_C
	.private_extern _Divide__Divide_var_u64_C
_Divide__Divide_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
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
	je	L1164
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1164
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1164:
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
	je	L1169
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1169
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1169:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u16
	.private_extern _Divide__uDivide_var_u64_u16
_Divide__uDivide_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1174
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1174
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1174:
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
	je	L1179
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1179
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1179:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_u64
	.private_extern _Divide__uDivide_var_u64_u64
_Divide__uDivide_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
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
.globl _Divide__Divide_var_u64_u64
	.private_extern _Divide__Divide_var_u64_u64
_Divide__Divide_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	160+_MM_Divide(%rip), %rax
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
	je	L1188
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1188
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1188:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_L
	.private_extern _Divide__uDivide_var_u64_L
_Divide__uDivide_var_u64_L:
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
.globl _Divide__Divide_var_u64_L
	.private_extern _Divide__Divide_var_u64_L
_Divide__Divide_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
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
	je	L1193
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1193
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1193:
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
	je	L1198
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1198
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1198:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i16
	.private_extern _Divide__uDivide_var_u64_i16
_Divide__uDivide_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
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
	je	L1208
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1208
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1208:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_u64_i32
	.private_extern _Divide__uDivide_var_u64_i32
_Divide__uDivide_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
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
	je	L1218
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1218
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1218:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__Divide_var_f64_f64
	.private_extern _Divide__Divide_var_f64_f64
_Divide__Divide_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	168+_MM_Divide(%rip), %xmm1
	movsd	168+_MM_Divide(%rip), %xmm0
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
.globl _Divide__uDivide_var_L_i64
	.private_extern _Divide__uDivide_var_L_i64
_Divide__uDivide_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movq	104+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
	je	L1232
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1232
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1232:
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
	movq	112+_MM_Divide(%rip), %rax
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
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1237
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1237
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1237:
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
	je	L1242
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1242
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1242:
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
	movzbl	124+_MM_Divide(%rip), %eax
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
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1247
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1247
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1247:
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
	je	L1252
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1252
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1252:
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
	movq	128+_MM_Divide(%rip), %rax
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
	movq	128+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1257
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1257
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1257:
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
	je	L1262
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1262
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1262:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_i8
	.private_extern _Divide__uDivide_var_L_i8
_Divide__uDivide_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1267
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1267
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1267:
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
	je	L1272
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1272
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1272:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_u32
	.private_extern _Divide__uDivide_var_L_u32
_Divide__uDivide_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movl	140+_MM_Divide(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1277
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1277
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1277:
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
	je	L1282
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1282
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1282:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_C
	.private_extern _Divide__uDivide_var_L_C
_Divide__uDivide_var_L_C:
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
.globl _Divide__Divide_var_L_C
	.private_extern _Divide__Divide_var_L_C
_Divide__Divide_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
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
	je	L1287
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1287
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1287:
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
	je	L1292
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1292
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1292:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_u16
	.private_extern _Divide__uDivide_var_L_u16
_Divide__uDivide_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1297
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1297
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1297:
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
	je	L1302
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1302
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1302:
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
	movq	160+_MM_Divide(%rip), %rax
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
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1307
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1307
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1307:
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
	je	L1312
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1312
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1312:
	movq	-40(%rbp), %rax
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
	je	L1321
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	xorq	%rdx, %rax
	testq	%rax, %rax
	jns	L1321
	subq	$1, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1321:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_L_i16
	.private_extern _Divide__uDivide_var_L_i16
_Divide__uDivide_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Divide(%rip), %rax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1326
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1326
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1326:
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
	je	L1331
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1331
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1331:
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
	movl	188+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1336
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1336
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1336:
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
	je	L1341
	movq	-40(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1341
	subq	$1, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1341:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i64
	.private_extern _Divide__uDivide_var_i16_i64
_Divide__uDivide_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1346
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1346
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1346:
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
	je	L1351
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1351
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1351:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_LC
	.private_extern _Divide__uDivide_var_i16_LC
_Divide__uDivide_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i16_LC
	.private_extern _Divide__Divide_var_i16_LC
_Divide__Divide_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
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
	je	L1356
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1356
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1356:
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
	je	L1361
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1361
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1361:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u8
	.private_extern _Divide__uDivide_var_i16_u8
_Divide__uDivide_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1366
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1366
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1366:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1367
	cmpq	$65535, -16(%rbp)
	jle	L1368
L1367:
	movl	$17505, %edi
	call	__m3_fault
L1368:
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
	je	L1373
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1373
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1373:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1374
	cmpq	$65535, -16(%rbp)
	jle	L1375
L1374:
	movl	$17569, %edi
	call	__m3_fault
L1375:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_I
	.private_extern _Divide__uDivide_var_i16_I
_Divide__uDivide_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1380
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1380
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1380:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1381
	cmpq	$65535, -16(%rbp)
	jle	L1382
L1381:
	movl	$17633, %edi
	call	__m3_fault
L1382:
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
	je	L1387
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1387
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1387:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1388
	cmpq	$65535, -16(%rbp)
	jle	L1389
L1388:
	movl	$17697, %edi
	call	__m3_fault
L1389:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i8
	.private_extern _Divide__uDivide_var_i16_i8
_Divide__uDivide_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1394
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1394
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1394:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1395
	cmpq	$65535, -16(%rbp)
	jle	L1396
L1395:
	movl	$17761, %edi
	call	__m3_fault
L1396:
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
	je	L1401
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1401
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1401:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1402
	cmpq	$65535, -16(%rbp)
	jle	L1403
L1402:
	movl	$17825, %edi
	call	__m3_fault
L1403:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u32
	.private_extern _Divide__uDivide_var_i16_u32
_Divide__uDivide_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1409
	cmpq	$65535, -16(%rbp)
	jle	L1410
L1409:
	movl	$17889, %edi
	call	__m3_fault
L1410:
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
	je	L1415
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1415
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1415:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1416
	cmpq	$65535, -16(%rbp)
	jle	L1417
L1416:
	movl	$17953, %edi
	call	__m3_fault
L1417:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_C
	.private_extern _Divide__uDivide_var_i16_C
_Divide__uDivide_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i16_C
	.private_extern _Divide__Divide_var_i16_C
_Divide__Divide_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	184+_MM_Divide(%rip), %eax
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
	je	L1422
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1422
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1422:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1423
	cmpq	$65535, -16(%rbp)
	jle	L1424
L1423:
	movl	$18017, %edi
	call	__m3_fault
L1424:
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
	je	L1429
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1429
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1429:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1430
	cmpq	$65535, -16(%rbp)
	jle	L1431
L1430:
	movl	$18081, %edi
	call	__m3_fault
L1431:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u16
	.private_extern _Divide__uDivide_var_i16_u16
_Divide__uDivide_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1436
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1436
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1436:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1437
	cmpq	$65535, -16(%rbp)
	jle	L1438
L1437:
	movl	$18145, %edi
	call	__m3_fault
L1438:
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
	je	L1443
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1443
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1443:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1444
	cmpq	$65535, -16(%rbp)
	jle	L1445
L1444:
	movl	$18209, %edi
	call	__m3_fault
L1445:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_u64
	.private_extern _Divide__uDivide_var_i16_u64
_Divide__uDivide_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i16_u64
	.private_extern _Divide__Divide_var_i16_u64
_Divide__Divide_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
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
	je	L1450
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1450
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1450:
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
	je	L1455
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1455
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1455:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_L
	.private_extern _Divide__uDivide_var_i16_L
_Divide__uDivide_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
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
	je	L1460
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1460
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1460:
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
	je	L1465
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1465
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1465:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i16
	.private_extern _Divide__uDivide_var_i16_i16
_Divide__uDivide_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1470
	cmpq	$65535, -16(%rbp)
	jle	L1471
L1470:
	movl	$18529, %edi
	call	__m3_fault
L1471:
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
	je	L1476
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1476
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1476:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1477
	cmpq	$65535, -16(%rbp)
	jle	L1478
L1477:
	movl	$18593, %edi
	call	__m3_fault
L1478:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i16_i32
	.private_extern _Divide__uDivide_var_i16_i32
_Divide__uDivide_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1483
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1483
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1483:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1484
	cmpq	$65535, -16(%rbp)
	jle	L1485
L1484:
	movl	$18657, %edi
	call	__m3_fault
L1485:
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
	je	L1490
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1490
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1490:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1491
	cmpq	$65535, -16(%rbp)
	jle	L1492
L1491:
	movl	$18721, %edi
	call	__m3_fault
L1492:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i64
	.private_extern _Divide__uDivide_var_i32_i64
_Divide__uDivide_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Divide(%rip), %rax
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
	je	L1502
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1502
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1502:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_LC
	.private_extern _Divide__uDivide_var_i32_LC
_Divide__uDivide_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i32_LC
	.private_extern _Divide__Divide_var_i32_LC
_Divide__Divide_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
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
	je	L1507
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1507
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1507:
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
	je	L1512
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1512
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1512:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u8
	.private_extern _Divide__uDivide_var_i32_u8
_Divide__uDivide_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Divide(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1517
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1517
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1517:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1518
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1519
L1518:
	movl	$19041, %edi
	call	__m3_fault
L1519:
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
	je	L1524
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1524
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1524:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1525
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1526
L1525:
	movl	$19105, %edi
	call	__m3_fault
L1526:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_I
	.private_extern _Divide__uDivide_var_i32_I
_Divide__uDivide_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Divide(%rip), %rax
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
	cmpq	$0, -16(%rbp)
	js	L1532
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1533
L1532:
	movl	$19169, %edi
	call	__m3_fault
L1533:
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
	cmpq	$0, -16(%rbp)
	js	L1539
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1540
L1539:
	movl	$19233, %edi
	call	__m3_fault
L1540:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i8
	.private_extern _Divide__uDivide_var_i32_i8
_Divide__uDivide_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
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
	cmpq	$0, -16(%rbp)
	js	L1546
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1547
L1546:
	movl	$19297, %edi
	call	__m3_fault
L1547:
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
	cmpq	$0, -16(%rbp)
	js	L1553
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1554
L1553:
	movl	$19361, %edi
	call	__m3_fault
L1554:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u32
	.private_extern _Divide__uDivide_var_i32_u32
_Divide__uDivide_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movl	140+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Divide(%rip), %eax
	cltq
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
	cmpq	$0, -16(%rbp)
	js	L1560
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1561
L1560:
	movl	$19425, %edi
	call	__m3_fault
L1561:
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
	cmpq	$0, -16(%rbp)
	js	L1567
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1568
L1567:
	movl	$19489, %edi
	call	__m3_fault
L1568:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_C
	.private_extern _Divide__uDivide_var_i32_C
_Divide__uDivide_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i32_C
	.private_extern _Divide__Divide_var_i32_C
_Divide__Divide_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	188+_MM_Divide(%rip), %eax
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
	je	L1573
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1573
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1573:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1574
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1575
L1574:
	movl	$19553, %edi
	call	__m3_fault
L1575:
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
	je	L1580
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1580
	subq	$1, -56(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -64(%rbp)
L1580:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1581
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1582
L1581:
	movl	$19617, %edi
	call	__m3_fault
L1582:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u16
	.private_extern _Divide__uDivide_var_i32_u16
_Divide__uDivide_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Divide(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1587
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1587
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1587:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1588
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1589
L1588:
	movl	$19681, %edi
	call	__m3_fault
L1589:
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
	je	L1594
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1594
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1594:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1595
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1596
L1595:
	movl	$19745, %edi
	call	__m3_fault
L1596:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_u64
	.private_extern _Divide__uDivide_var_i32_u64
_Divide__uDivide_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
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
.globl _Divide__Divide_var_i32_u64
	.private_extern _Divide__Divide_var_i32_u64
_Divide__Divide_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
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
	je	L1601
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1601
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1601:
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
	je	L1606
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1606
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1606:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_L
	.private_extern _Divide__uDivide_var_i32_L
_Divide__uDivide_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
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
	je	L1611
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1611
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1611:
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
	je	L1616
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1616
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1616:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i16
	.private_extern _Divide__uDivide_var_i32_i16
_Divide__uDivide_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1621
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1621
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1621:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1622
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1623
L1622:
	movl	$20065, %edi
	call	__m3_fault
L1623:
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
	je	L1628
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1628
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1628:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1629
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1630
L1629:
	movl	$20129, %edi
	call	__m3_fault
L1630:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Divide__uDivide_var_i32_i32
	.private_extern _Divide__uDivide_var_i32_i32
_Divide__uDivide_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -48(%rbp)
	movl	188+_MM_Divide(%rip), %eax
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
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movl	188+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1635
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1636
L1635:
	movl	$20193, %edi
	call	__m3_fault
L1636:
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
	cmpq	$0, -16(%rbp)
	js	L1642
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
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
	.ascii "Divide_param_i32_L"
	.space 1
	.ascii "uDivide_param_i32_L"
	.space 1
	.ascii "Divide_var_i32_L"
	.space 1
	.ascii "uDivide_var_i32_L"
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
	.ascii "Divide_param_i32_C"
	.space 1
	.ascii "uDivide_param_i32_C"
	.space 1
	.ascii "Divide_var_i32_C"
	.space 1
	.ascii "uDivide_var_i32_C"
	.space 1
	.ascii "Divide_param_i32_u32"
	.space 1
	.ascii "uDivide_param_i32_u32"
	.space 1
	.ascii "Divide_var_i32_u32"
	.space 1
	.ascii "uDivide_var_i32_u32"
	.space 1
	.ascii "Divide_param_i32_i8"
	.space 1
	.ascii "uDivide_param_i32_i8"
	.space 1
	.ascii "Divide_var_i32_i8"
	.space 1
	.ascii "uDivide_var_i32_i8"
	.space 1
	.ascii "Divide_param_i32_I"
	.space 1
	.ascii "uDivide_param_i32_I"
	.space 1
	.ascii "Divide_var_i32_I"
	.space 1
	.ascii "uDivide_var_i32_I"
	.space 1
	.ascii "Divide_param_i32_u8"
	.space 1
	.ascii "uDivide_param_i32_u8"
	.space 1
	.ascii "Divide_var_i32_u8"
	.space 1
	.ascii "uDivide_var_i32_u8"
	.space 1
	.ascii "Divide_param_i32_LC"
	.space 1
	.ascii "uDivide_param_i32_LC"
	.space 1
	.ascii "Divide_var_i32_LC"
	.space 1
	.ascii "uDivide_var_i32_LC"
	.space 1
	.ascii "Divide_param_i32_i64"
	.space 1
	.ascii "uDivide_param_i32_i64"
	.space 1
	.ascii "Divide_var_i32_i64"
	.space 1
	.ascii "uDivide_var_i32_i64"
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
	.ascii "Divide_param_i16_L"
	.space 1
	.ascii "uDivide_param_i16_L"
	.space 1
	.ascii "Divide_var_i16_L"
	.space 1
	.ascii "uDivide_var_i16_L"
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
	.ascii "Divide_param_i16_C"
	.space 1
	.ascii "uDivide_param_i16_C"
	.space 1
	.ascii "Divide_var_i16_C"
	.space 1
	.ascii "uDivide_var_i16_C"
	.space 1
	.ascii "Divide_param_i16_u32"
	.space 1
	.ascii "uDivide_param_i16_u32"
	.space 1
	.ascii "Divide_var_i16_u32"
	.space 1
	.ascii "uDivide_var_i16_u32"
	.space 1
	.ascii "Divide_param_i16_i8"
	.space 1
	.ascii "uDivide_param_i16_i8"
	.space 1
	.ascii "Divide_var_i16_i8"
	.space 1
	.ascii "uDivide_var_i16_i8"
	.space 1
	.ascii "Divide_param_i16_I"
	.space 1
	.ascii "uDivide_param_i16_I"
	.space 1
	.ascii "Divide_var_i16_I"
	.space 1
	.ascii "uDivide_var_i16_I"
	.space 1
	.ascii "Divide_param_i16_u8"
	.space 1
	.ascii "uDivide_param_i16_u8"
	.space 1
	.ascii "Divide_var_i16_u8"
	.space 1
	.ascii "uDivide_var_i16_u8"
	.space 1
	.ascii "Divide_param_i16_LC"
	.space 1
	.ascii "uDivide_param_i16_LC"
	.space 1
	.ascii "Divide_var_i16_LC"
	.space 1
	.ascii "uDivide_var_i16_LC"
	.space 1
	.ascii "Divide_param_i16_i64"
	.space 1
	.ascii "uDivide_param_i16_i64"
	.space 1
	.ascii "Divide_var_i16_i64"
	.space 1
	.ascii "uDivide_var_i16_i64"
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
	.ascii "Divide_param_L_L"
	.space 1
	.ascii "uDivide_param_L_L"
	.space 1
	.ascii "Divide_var_L_L"
	.space 1
	.ascii "uDivide_var_L_L"
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
	.ascii "Divide_param_L_C"
	.space 1
	.ascii "uDivide_param_L_C"
	.space 1
	.ascii "Divide_var_L_C"
	.space 1
	.ascii "uDivide_var_L_C"
	.space 1
	.ascii "Divide_param_L_u32"
	.space 1
	.ascii "uDivide_param_L_u32"
	.space 1
	.ascii "Divide_var_L_u32"
	.space 1
	.ascii "uDivide_var_L_u32"
	.space 1
	.ascii "Divide_param_L_i8"
	.space 1
	.ascii "uDivide_param_L_i8"
	.space 1
	.ascii "Divide_var_L_i8"
	.space 1
	.ascii "uDivide_var_L_i8"
	.space 1
	.ascii "Divide_param_L_I"
	.space 1
	.ascii "uDivide_param_L_I"
	.space 1
	.ascii "Divide_var_L_I"
	.space 1
	.ascii "uDivide_var_L_I"
	.space 1
	.ascii "Divide_param_L_u8"
	.space 1
	.ascii "uDivide_param_L_u8"
	.space 1
	.ascii "Divide_var_L_u8"
	.space 1
	.ascii "uDivide_var_L_u8"
	.space 1
	.ascii "Divide_param_L_LC"
	.space 1
	.ascii "uDivide_param_L_LC"
	.space 1
	.ascii "Divide_var_L_LC"
	.space 1
	.ascii "uDivide_var_L_LC"
	.space 1
	.ascii "Divide_param_L_i64"
	.space 1
	.ascii "uDivide_param_L_i64"
	.space 1
	.ascii "Divide_var_L_i64"
	.space 1
	.ascii "uDivide_var_L_i64"
	.space 1
	.ascii "Divide_param_f64_f64"
	.space 1
	.ascii "Divide_var_f64_f64"
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
	.ascii "Divide_param_u64_L"
	.space 1
	.ascii "uDivide_param_u64_L"
	.space 1
	.ascii "Divide_var_u64_L"
	.space 1
	.ascii "uDivide_var_u64_L"
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
	.ascii "Divide_param_u64_C"
	.space 1
	.ascii "uDivide_param_u64_C"
	.space 1
	.ascii "Divide_var_u64_C"
	.space 1
	.ascii "uDivide_var_u64_C"
	.space 1
	.ascii "Divide_param_u64_u32"
	.space 1
	.ascii "uDivide_param_u64_u32"
	.space 1
	.ascii "Divide_var_u64_u32"
	.space 1
	.ascii "uDivide_var_u64_u32"
	.space 1
	.ascii "Divide_param_u64_i8"
	.space 1
	.ascii "uDivide_param_u64_i8"
	.space 1
	.ascii "Divide_var_u64_i8"
	.space 1
	.ascii "uDivide_var_u64_i8"
	.space 1
	.ascii "Divide_param_u64_I"
	.space 1
	.ascii "uDivide_param_u64_I"
	.space 1
	.ascii "Divide_var_u64_I"
	.space 1
	.ascii "uDivide_var_u64_I"
	.space 1
	.ascii "Divide_param_u64_u8"
	.space 1
	.ascii "uDivide_param_u64_u8"
	.space 1
	.ascii "Divide_var_u64_u8"
	.space 1
	.ascii "uDivide_var_u64_u8"
	.space 1
	.ascii "Divide_param_u64_LC"
	.space 1
	.ascii "uDivide_param_u64_LC"
	.space 1
	.ascii "Divide_var_u64_LC"
	.space 1
	.ascii "uDivide_var_u64_LC"
	.space 1
	.ascii "Divide_param_u64_i64"
	.space 1
	.ascii "uDivide_param_u64_i64"
	.space 1
	.ascii "Divide_var_u64_i64"
	.space 1
	.ascii "uDivide_var_u64_i64"
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
	.ascii "Divide_param_u16_L"
	.space 1
	.ascii "uDivide_param_u16_L"
	.space 1
	.ascii "Divide_var_u16_L"
	.space 1
	.ascii "uDivide_var_u16_L"
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
	.space 1
	.ascii "Divide_param_u16_C"
	.space 1
	.ascii "uDivide_param_u16_C"
	.space 1
	.ascii "Divide_var_u16_C"
	.space 1
	.ascii "uDivide_var_u16_C"
	.space 1
	.ascii "Divide_param_u16_u32"
	.space 1
	.ascii "uDivide_param_u16_u32"
	.space 1
	.ascii "Divide_var_u16_u32"
	.space 1
	.ascii "uDivide_var_u16_u32"
	.space 1
	.ascii "Divide_param_u16_i8"
	.space 1
	.ascii "uDivide_param_u16_i8"
	.space 1
	.ascii "Divide_var_u16_i8"
	.space 1
	.ascii "uDivide_var_u16_i8"
	.space 1
	.ascii "Divide_param_u16_I"
	.space 1
	.ascii "uDivide_param_u16_I"
	.space 1
	.ascii "Divide_var_u16_I"
	.space 1
	.ascii "uDivide_var_u16_I"
	.space 1
	.ascii "Divide_param_u16_u8"
	.space 1
	.ascii "uDivide_param_u16_u8"
	.space 1
	.ascii "Divide_var_u16_u8"
	.space 1
	.ascii "uDivide_var_u16_u8"
	.space 1
	.ascii "Divide_param_u16_LC"
	.space 1
	.ascii "uDivide_param_u16_LC"
	.space 1
	.ascii "Divide_var_u16_LC"
	.space 1
	.ascii "uDivide_var_u16_LC"
	.space 1
	.ascii "Divide_param_u16_i64"
	.space 1
	.ascii "uDivide_param_u16_i64"
	.space 1
	.ascii "Divide_var_u16_i64"
	.space 1
	.ascii "uDivide_var_u16_i64"
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
	.ascii "Divide_param_C_L"
	.space 1
	.ascii "uDivide_param_C_L"
	.space 1
	.ascii "Divide_var_C_L"
	.space 1
	.ascii "uDivide_var_C_L"
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
	.ascii "Divide_param_C_C"
	.space 1
	.ascii "uDivide_param_C_C"
	.space 1
	.ascii "Divide_var_C_C"
	.space 1
	.ascii "uDivide_var_C_C"
	.space 1
	.ascii "Divide_param_C_u32"
	.space 1
	.ascii "uDivide_param_C_u32"
	.space 1
	.ascii "Divide_var_C_u32"
	.space 1
	.ascii "uDivide_var_C_u32"
	.space 1
	.ascii "Divide_param_C_i8"
	.space 1
	.ascii "uDivide_param_C_i8"
	.space 1
	.ascii "Divide_var_C_i8"
	.space 1
	.ascii "uDivide_var_C_i8"
	.space 1
	.ascii "Divide_param_C_I"
	.space 1
	.ascii "uDivide_param_C_I"
	.space 1
	.ascii "Divide_var_C_I"
	.space 1
	.ascii "uDivide_var_C_I"
	.space 1
	.ascii "Divide_param_C_u8"
	.space 1
	.ascii "uDivide_param_C_u8"
	.space 1
	.ascii "Divide_var_C_u8"
	.space 1
	.ascii "uDivide_var_C_u8"
	.space 1
	.ascii "Divide_param_C_LC"
	.space 1
	.ascii "uDivide_param_C_LC"
	.space 1
	.ascii "Divide_var_C_LC"
	.space 1
	.ascii "uDivide_var_C_LC"
	.space 1
	.ascii "Divide_param_C_i64"
	.space 1
	.ascii "uDivide_param_C_i64"
	.space 1
	.ascii "Divide_var_C_i64"
	.space 1
	.ascii "uDivide_var_C_i64"
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
	.ascii "Divide_param_u32_L"
	.space 1
	.ascii "uDivide_param_u32_L"
	.space 1
	.ascii "Divide_var_u32_L"
	.space 1
	.ascii "uDivide_var_u32_L"
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
	.ascii "Divide_param_u32_C"
	.space 1
	.ascii "uDivide_param_u32_C"
	.space 1
	.ascii "Divide_var_u32_C"
	.space 1
	.ascii "uDivide_var_u32_C"
	.space 1
	.ascii "Divide_param_u32_u32"
	.space 1
	.ascii "uDivide_param_u32_u32"
	.space 1
	.ascii "Divide_var_u32_u32"
	.space 1
	.ascii "uDivide_var_u32_u32"
	.space 1
	.ascii "Divide_param_u32_i8"
	.space 1
	.ascii "uDivide_param_u32_i8"
	.space 1
	.ascii "Divide_var_u32_i8"
	.space 1
	.ascii "uDivide_var_u32_i8"
	.space 1
	.ascii "Divide_param_u32_I"
	.space 1
	.ascii "uDivide_param_u32_I"
	.space 1
	.ascii "Divide_var_u32_I"
	.space 1
	.ascii "uDivide_var_u32_I"
	.space 1
	.ascii "Divide_param_u32_u8"
	.space 1
	.ascii "uDivide_param_u32_u8"
	.space 1
	.ascii "Divide_var_u32_u8"
	.space 1
	.ascii "uDivide_var_u32_u8"
	.space 1
	.ascii "Divide_param_u32_LC"
	.space 1
	.ascii "uDivide_param_u32_LC"
	.space 1
	.ascii "Divide_var_u32_LC"
	.space 1
	.ascii "uDivide_var_u32_LC"
	.space 1
	.ascii "Divide_param_u32_i64"
	.space 1
	.ascii "uDivide_param_u32_i64"
	.space 1
	.ascii "Divide_var_u32_i64"
	.space 1
	.ascii "uDivide_var_u32_i64"
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
	.ascii "Divide_param_i8_L"
	.space 1
	.ascii "uDivide_param_i8_L"
	.space 1
	.ascii "Divide_var_i8_L"
	.space 1
	.ascii "uDivide_var_i8_L"
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
	.ascii "Divide_param_i8_C"
	.space 1
	.ascii "uDivide_param_i8_C"
	.space 1
	.ascii "Divide_var_i8_C"
	.space 1
	.ascii "uDivide_var_i8_C"
	.space 1
	.ascii "Divide_param_i8_u32"
	.space 1
	.ascii "uDivide_param_i8_u32"
	.space 1
	.ascii "Divide_var_i8_u32"
	.space 1
	.ascii "uDivide_var_i8_u32"
	.space 1
	.ascii "Divide_param_i8_i8"
	.space 1
	.ascii "uDivide_param_i8_i8"
	.space 1
	.ascii "Divide_var_i8_i8"
	.space 1
	.ascii "uDivide_var_i8_i8"
	.space 1
	.ascii "Divide_param_i8_I"
	.space 1
	.ascii "uDivide_param_i8_I"
	.space 1
	.ascii "Divide_var_i8_I"
	.space 1
	.ascii "uDivide_var_i8_I"
	.space 1
	.ascii "Divide_param_i8_u8"
	.space 1
	.ascii "uDivide_param_i8_u8"
	.space 1
	.ascii "Divide_var_i8_u8"
	.space 1
	.ascii "uDivide_var_i8_u8"
	.space 1
	.ascii "Divide_param_i8_LC"
	.space 1
	.ascii "uDivide_param_i8_LC"
	.space 1
	.ascii "Divide_var_i8_LC"
	.space 1
	.ascii "uDivide_var_i8_LC"
	.space 1
	.ascii "Divide_param_i8_i64"
	.space 1
	.ascii "uDivide_param_i8_i64"
	.space 1
	.ascii "Divide_var_i8_i64"
	.space 1
	.ascii "uDivide_var_i8_i64"
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
	.ascii "Divide_param_I_L"
	.space 1
	.ascii "uDivide_param_I_L"
	.space 1
	.ascii "Divide_var_I_L"
	.space 1
	.ascii "uDivide_var_I_L"
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
	.ascii "Divide_param_I_C"
	.space 1
	.ascii "uDivide_param_I_C"
	.space 1
	.ascii "Divide_var_I_C"
	.space 1
	.ascii "uDivide_var_I_C"
	.space 1
	.ascii "Divide_param_I_u32"
	.space 1
	.ascii "uDivide_param_I_u32"
	.space 1
	.ascii "Divide_var_I_u32"
	.space 1
	.ascii "uDivide_var_I_u32"
	.space 1
	.ascii "Divide_param_I_i8"
	.space 1
	.ascii "uDivide_param_I_i8"
	.space 1
	.ascii "Divide_var_I_i8"
	.space 1
	.ascii "uDivide_var_I_i8"
	.space 1
	.ascii "Divide_param_I_I"
	.space 1
	.ascii "uDivide_param_I_I"
	.space 1
	.ascii "Divide_var_I_I"
	.space 1
	.ascii "uDivide_var_I_I"
	.space 1
	.ascii "Divide_param_I_u8"
	.space 1
	.ascii "uDivide_param_I_u8"
	.space 1
	.ascii "Divide_var_I_u8"
	.space 1
	.ascii "uDivide_var_I_u8"
	.space 1
	.ascii "Divide_param_I_LC"
	.space 1
	.ascii "uDivide_param_I_LC"
	.space 1
	.ascii "Divide_var_I_LC"
	.space 1
	.ascii "uDivide_var_I_LC"
	.space 1
	.ascii "Divide_param_I_i64"
	.space 1
	.ascii "uDivide_param_I_i64"
	.space 1
	.ascii "Divide_var_I_i64"
	.space 1
	.ascii "uDivide_var_I_i64"
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
	.ascii "Divide_param_u8_L"
	.space 1
	.ascii "uDivide_param_u8_L"
	.space 1
	.ascii "Divide_var_u8_L"
	.space 1
	.ascii "uDivide_var_u8_L"
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
	.ascii "Divide_param_u8_C"
	.space 1
	.ascii "uDivide_param_u8_C"
	.space 1
	.ascii "Divide_var_u8_C"
	.space 1
	.ascii "uDivide_var_u8_C"
	.space 1
	.ascii "Divide_param_u8_u32"
	.space 1
	.ascii "uDivide_param_u8_u32"
	.space 1
	.ascii "Divide_var_u8_u32"
	.space 1
	.ascii "uDivide_var_u8_u32"
	.space 1
	.ascii "Divide_param_u8_i8"
	.space 1
	.ascii "uDivide_param_u8_i8"
	.space 1
	.ascii "Divide_var_u8_i8"
	.space 1
	.ascii "uDivide_var_u8_i8"
	.space 1
	.ascii "Divide_param_u8_I"
	.space 1
	.ascii "uDivide_param_u8_I"
	.space 1
	.ascii "Divide_var_u8_I"
	.space 1
	.ascii "uDivide_var_u8_I"
	.space 1
	.ascii "Divide_param_u8_u8"
	.space 1
	.ascii "uDivide_param_u8_u8"
	.space 1
	.ascii "Divide_var_u8_u8"
	.space 1
	.ascii "uDivide_var_u8_u8"
	.space 1
	.ascii "Divide_param_u8_LC"
	.space 1
	.ascii "uDivide_param_u8_LC"
	.space 1
	.ascii "Divide_var_u8_LC"
	.space 1
	.ascii "uDivide_var_u8_LC"
	.space 1
	.ascii "Divide_param_u8_i64"
	.space 1
	.ascii "uDivide_param_u8_i64"
	.space 1
	.ascii "Divide_var_u8_i64"
	.space 1
	.ascii "uDivide_var_u8_i64"
	.space 1
	.ascii "Divide_param_f32_f32"
	.space 1
	.ascii "Divide_var_f32_f32"
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
	.ascii "Divide_param_LC_L"
	.space 1
	.ascii "uDivide_param_LC_L"
	.space 1
	.ascii "Divide_var_LC_L"
	.space 1
	.ascii "uDivide_var_LC_L"
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
	.ascii "Divide_param_LC_C"
	.space 1
	.ascii "uDivide_param_LC_C"
	.space 1
	.ascii "Divide_var_LC_C"
	.space 1
	.ascii "uDivide_var_LC_C"
	.space 1
	.ascii "Divide_param_LC_u32"
	.space 1
	.ascii "uDivide_param_LC_u32"
	.space 1
	.ascii "Divide_var_LC_u32"
	.space 1
	.ascii "uDivide_var_LC_u32"
	.space 1
	.ascii "Divide_param_LC_i8"
	.space 1
	.ascii "uDivide_param_LC_i8"
	.space 1
	.ascii "Divide_var_LC_i8"
	.space 1
	.ascii "uDivide_var_LC_i8"
	.space 1
	.ascii "Divide_param_LC_I"
	.space 1
	.ascii "uDivide_param_LC_I"
	.space 1
	.ascii "Divide_var_LC_I"
	.space 1
	.ascii "uDivide_var_LC_I"
	.space 1
	.ascii "Divide_param_LC_u8"
	.space 1
	.ascii "uDivide_param_LC_u8"
	.space 1
	.ascii "Divide_var_LC_u8"
	.space 1
	.ascii "uDivide_var_LC_u8"
	.space 1
	.ascii "Divide_param_LC_LC"
	.space 1
	.ascii "uDivide_param_LC_LC"
	.space 1
	.ascii "Divide_var_LC_LC"
	.space 1
	.ascii "uDivide_var_LC_LC"
	.space 1
	.ascii "Divide_param_LC_i64"
	.space 1
	.ascii "uDivide_param_LC_i64"
	.space 1
	.ascii "Divide_var_LC_i64"
	.space 1
	.ascii "uDivide_var_LC_i64"
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
	.ascii "Divide_param_i64_L"
	.space 1
	.ascii "uDivide_param_i64_L"
	.space 1
	.ascii "Divide_var_i64_L"
	.space 1
	.ascii "uDivide_var_i64_L"
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
	.ascii "Divide_param_i64_C"
	.space 1
	.ascii "uDivide_param_i64_C"
	.space 1
	.ascii "Divide_var_i64_C"
	.space 1
	.ascii "uDivide_var_i64_C"
	.space 1
	.ascii "Divide_param_i64_u32"
	.space 1
	.ascii "uDivide_param_i64_u32"
	.space 1
	.ascii "Divide_var_i64_u32"
	.space 1
	.ascii "uDivide_var_i64_u32"
	.space 1
	.ascii "Divide_param_i64_i8"
	.space 1
	.ascii "uDivide_param_i64_i8"
	.space 1
	.ascii "Divide_var_i64_i8"
	.space 1
	.ascii "uDivide_var_i64_i8"
	.space 1
	.ascii "Divide_param_i64_I"
	.space 1
	.ascii "uDivide_param_i64_I"
	.space 1
	.ascii "Divide_var_i64_I"
	.space 1
	.ascii "uDivide_var_i64_I"
	.space 1
	.ascii "Divide_param_i64_u8"
	.space 1
	.ascii "uDivide_param_i64_u8"
	.space 1
	.ascii "Divide_var_i64_u8"
	.space 1
	.ascii "uDivide_var_i64_u8"
	.space 1
	.ascii "Divide_param_i64_LC"
	.space 1
	.ascii "uDivide_param_i64_LC"
	.space 1
	.ascii "Divide_var_i64_LC"
	.space 1
	.ascii "uDivide_var_i64_LC"
	.space 1
	.ascii "Divide_param_i64_i64"
	.space 1
	.ascii "uDivide_param_i64_i64"
	.space 1
	.ascii "Divide_var_i64_i64"
	.space 1
	.ascii "uDivide_var_i64_i64"
	.space 7
	.quad	_Divide_M3
	.quad	_L_1
	.quad	_Divide__Divide_param_i32_i32
	.quad	_L_1+10
	.quad	_Divide__uDivide_param_i32_i32
	.quad	_L_1+31
	.quad	_Divide__Divide_var_i32_i32
	.quad	_L_1+53
	.quad	_Divide__uDivide_var_i32_i32
	.quad	_L_1+72
	.quad	_Divide__Divide_param_i32_i16
	.quad	_L_1+92
	.quad	_Divide__uDivide_param_i32_i16
	.quad	_L_1+113
	.quad	_Divide__Divide_var_i32_i16
	.quad	_L_1+135
	.quad	_Divide__uDivide_var_i32_i16
	.quad	_L_1+154
	.quad	_Divide__Divide_param_i32_L
	.quad	_L_1+174
	.quad	_Divide__uDivide_param_i32_L
	.quad	_L_1+193
	.quad	_Divide__Divide_var_i32_L
	.quad	_L_1+213
	.quad	_Divide__uDivide_var_i32_L
	.quad	_L_1+230
	.quad	_Divide__Divide_param_i32_u64
	.quad	_L_1+248
	.quad	_Divide__uDivide_param_i32_u64
	.quad	_L_1+269
	.quad	_Divide__Divide_var_i32_u64
	.quad	_L_1+291
	.quad	_Divide__uDivide_var_i32_u64
	.quad	_L_1+310
	.quad	_Divide__Divide_param_i32_u16
	.quad	_L_1+330
	.quad	_Divide__uDivide_param_i32_u16
	.quad	_L_1+351
	.quad	_Divide__Divide_var_i32_u16
	.quad	_L_1+373
	.quad	_Divide__uDivide_var_i32_u16
	.quad	_L_1+392
	.quad	_Divide__Divide_param_i32_C
	.quad	_L_1+412
	.quad	_Divide__uDivide_param_i32_C
	.quad	_L_1+431
	.quad	_Divide__Divide_var_i32_C
	.quad	_L_1+451
	.quad	_Divide__uDivide_var_i32_C
	.quad	_L_1+468
	.quad	_Divide__Divide_param_i32_u32
	.quad	_L_1+486
	.quad	_Divide__uDivide_param_i32_u32
	.quad	_L_1+507
	.quad	_Divide__Divide_var_i32_u32
	.quad	_L_1+529
	.quad	_Divide__uDivide_var_i32_u32
	.quad	_L_1+548
	.quad	_Divide__Divide_param_i32_i8
	.quad	_L_1+568
	.quad	_Divide__uDivide_param_i32_i8
	.quad	_L_1+588
	.quad	_Divide__Divide_var_i32_i8
	.quad	_L_1+609
	.quad	_Divide__uDivide_var_i32_i8
	.quad	_L_1+627
	.quad	_Divide__Divide_param_i32_I
	.quad	_L_1+646
	.quad	_Divide__uDivide_param_i32_I
	.quad	_L_1+665
	.quad	_Divide__Divide_var_i32_I
	.quad	_L_1+685
	.quad	_Divide__uDivide_var_i32_I
	.quad	_L_1+702
	.quad	_Divide__Divide_param_i32_u8
	.quad	_L_1+720
	.quad	_Divide__uDivide_param_i32_u8
	.quad	_L_1+740
	.quad	_Divide__Divide_var_i32_u8
	.quad	_L_1+761
	.quad	_Divide__uDivide_var_i32_u8
	.quad	_L_1+779
	.quad	_Divide__Divide_param_i32_LC
	.quad	_L_1+798
	.quad	_Divide__uDivide_param_i32_LC
	.quad	_L_1+818
	.quad	_Divide__Divide_var_i32_LC
	.quad	_L_1+839
	.quad	_Divide__uDivide_var_i32_LC
	.quad	_L_1+857
	.quad	_Divide__Divide_param_i32_i64
	.quad	_L_1+876
	.quad	_Divide__uDivide_param_i32_i64
	.quad	_L_1+897
	.quad	_Divide__Divide_var_i32_i64
	.quad	_L_1+919
	.quad	_Divide__uDivide_var_i32_i64
	.quad	_L_1+938
	.quad	_Divide__Divide_param_i16_i32
	.quad	_L_1+958
	.quad	_Divide__uDivide_param_i16_i32
	.quad	_L_1+979
	.quad	_Divide__Divide_var_i16_i32
	.quad	_L_1+1001
	.quad	_Divide__uDivide_var_i16_i32
	.quad	_L_1+1020
	.quad	_Divide__Divide_param_i16_i16
	.quad	_L_1+1040
	.quad	_Divide__uDivide_param_i16_i16
	.quad	_L_1+1061
	.quad	_Divide__Divide_var_i16_i16
	.quad	_L_1+1083
	.quad	_Divide__uDivide_var_i16_i16
	.quad	_L_1+1102
	.quad	_Divide__Divide_param_i16_L
	.quad	_L_1+1122
	.quad	_Divide__uDivide_param_i16_L
	.quad	_L_1+1141
	.quad	_Divide__Divide_var_i16_L
	.quad	_L_1+1161
	.quad	_Divide__uDivide_var_i16_L
	.quad	_L_1+1178
	.quad	_Divide__Divide_param_i16_u64
	.quad	_L_1+1196
	.quad	_Divide__uDivide_param_i16_u64
	.quad	_L_1+1217
	.quad	_Divide__Divide_var_i16_u64
	.quad	_L_1+1239
	.quad	_Divide__uDivide_var_i16_u64
	.quad	_L_1+1258
	.quad	_Divide__Divide_param_i16_u16
	.quad	_L_1+1278
	.quad	_Divide__uDivide_param_i16_u16
	.quad	_L_1+1299
	.quad	_Divide__Divide_var_i16_u16
	.quad	_L_1+1321
	.quad	_Divide__uDivide_var_i16_u16
	.quad	_L_1+1340
	.quad	_Divide__Divide_param_i16_C
	.quad	_L_1+1360
	.quad	_Divide__uDivide_param_i16_C
	.quad	_L_1+1379
	.quad	_Divide__Divide_var_i16_C
	.quad	_L_1+1399
	.quad	_Divide__uDivide_var_i16_C
	.quad	_L_1+1416
	.quad	_Divide__Divide_param_i16_u32
	.quad	_L_1+1434
	.quad	_Divide__uDivide_param_i16_u32
	.quad	_L_1+1455
	.quad	_Divide__Divide_var_i16_u32
	.quad	_L_1+1477
	.quad	_Divide__uDivide_var_i16_u32
	.quad	_L_1+1496
	.quad	_Divide__Divide_param_i16_i8
	.quad	_L_1+1516
	.quad	_Divide__uDivide_param_i16_i8
	.quad	_L_1+1536
	.quad	_Divide__Divide_var_i16_i8
	.quad	_L_1+1557
	.quad	_Divide__uDivide_var_i16_i8
	.quad	_L_1+1575
	.quad	_Divide__Divide_param_i16_I
	.quad	_L_1+1594
	.quad	_Divide__uDivide_param_i16_I
	.quad	_L_1+1613
	.quad	_Divide__Divide_var_i16_I
	.quad	_L_1+1633
	.quad	_Divide__uDivide_var_i16_I
	.quad	_L_1+1650
	.quad	_Divide__Divide_param_i16_u8
	.quad	_L_1+1668
	.quad	_Divide__uDivide_param_i16_u8
	.quad	_L_1+1688
	.quad	_Divide__Divide_var_i16_u8
	.quad	_L_1+1709
	.quad	_Divide__uDivide_var_i16_u8
	.quad	_L_1+1727
	.quad	_Divide__Divide_param_i16_LC
	.quad	_L_1+1746
	.quad	_Divide__uDivide_param_i16_LC
	.quad	_L_1+1766
	.quad	_Divide__Divide_var_i16_LC
	.quad	_L_1+1787
	.quad	_Divide__uDivide_var_i16_LC
	.quad	_L_1+1805
	.quad	_Divide__Divide_param_i16_i64
	.quad	_L_1+1824
	.quad	_Divide__uDivide_param_i16_i64
	.quad	_L_1+1845
	.quad	_Divide__Divide_var_i16_i64
	.quad	_L_1+1867
	.quad	_Divide__uDivide_var_i16_i64
	.quad	_L_1+1886
	.quad	_Divide__Divide_param_L_i32
	.quad	_L_1+1906
	.quad	_Divide__uDivide_param_L_i32
	.quad	_L_1+1925
	.quad	_Divide__Divide_var_L_i32
	.quad	_L_1+1945
	.quad	_Divide__uDivide_var_L_i32
	.quad	_L_1+1962
	.quad	_Divide__Divide_param_L_i16
	.quad	_L_1+1980
	.quad	_Divide__uDivide_param_L_i16
	.quad	_L_1+1999
	.quad	_Divide__Divide_var_L_i16
	.quad	_L_1+2019
	.quad	_Divide__uDivide_var_L_i16
	.quad	_L_1+2036
	.quad	_Divide__Divide_param_L_L
	.quad	_L_1+2054
	.quad	_Divide__uDivide_param_L_L
	.quad	_L_1+2071
	.quad	_Divide__Divide_var_L_L
	.quad	_L_1+2089
	.quad	_Divide__uDivide_var_L_L
	.quad	_L_1+2104
	.quad	_Divide__Divide_param_L_u64
	.quad	_L_1+2120
	.quad	_Divide__uDivide_param_L_u64
	.quad	_L_1+2139
	.quad	_Divide__Divide_var_L_u64
	.quad	_L_1+2159
	.quad	_Divide__uDivide_var_L_u64
	.quad	_L_1+2176
	.quad	_Divide__Divide_param_L_u16
	.quad	_L_1+2194
	.quad	_Divide__uDivide_param_L_u16
	.quad	_L_1+2213
	.quad	_Divide__Divide_var_L_u16
	.quad	_L_1+2233
	.quad	_Divide__uDivide_var_L_u16
	.quad	_L_1+2250
	.quad	_Divide__Divide_param_L_C
	.quad	_L_1+2268
	.quad	_Divide__uDivide_param_L_C
	.quad	_L_1+2285
	.quad	_Divide__Divide_var_L_C
	.quad	_L_1+2303
	.quad	_Divide__uDivide_var_L_C
	.quad	_L_1+2318
	.quad	_Divide__Divide_param_L_u32
	.quad	_L_1+2334
	.quad	_Divide__uDivide_param_L_u32
	.quad	_L_1+2353
	.quad	_Divide__Divide_var_L_u32
	.quad	_L_1+2373
	.quad	_Divide__uDivide_var_L_u32
	.quad	_L_1+2390
	.quad	_Divide__Divide_param_L_i8
	.quad	_L_1+2408
	.quad	_Divide__uDivide_param_L_i8
	.quad	_L_1+2426
	.quad	_Divide__Divide_var_L_i8
	.quad	_L_1+2445
	.quad	_Divide__uDivide_var_L_i8
	.quad	_L_1+2461
	.quad	_Divide__Divide_param_L_I
	.quad	_L_1+2478
	.quad	_Divide__uDivide_param_L_I
	.quad	_L_1+2495
	.quad	_Divide__Divide_var_L_I
	.quad	_L_1+2513
	.quad	_Divide__uDivide_var_L_I
	.quad	_L_1+2528
	.quad	_Divide__Divide_param_L_u8
	.quad	_L_1+2544
	.quad	_Divide__uDivide_param_L_u8
	.quad	_L_1+2562
	.quad	_Divide__Divide_var_L_u8
	.quad	_L_1+2581
	.quad	_Divide__uDivide_var_L_u8
	.quad	_L_1+2597
	.quad	_Divide__Divide_param_L_LC
	.quad	_L_1+2614
	.quad	_Divide__uDivide_param_L_LC
	.quad	_L_1+2632
	.quad	_Divide__Divide_var_L_LC
	.quad	_L_1+2651
	.quad	_Divide__uDivide_var_L_LC
	.quad	_L_1+2667
	.quad	_Divide__Divide_param_L_i64
	.quad	_L_1+2684
	.quad	_Divide__uDivide_param_L_i64
	.quad	_L_1+2703
	.quad	_Divide__Divide_var_L_i64
	.quad	_L_1+2723
	.quad	_Divide__uDivide_var_L_i64
	.quad	_L_1+2740
	.quad	_Divide__Divide_param_f64_f64
	.quad	_L_1+2758
	.quad	_Divide__Divide_var_f64_f64
	.quad	_L_1+2779
	.quad	_Divide__Divide_param_u64_i32
	.quad	_L_1+2798
	.quad	_Divide__uDivide_param_u64_i32
	.quad	_L_1+2819
	.quad	_Divide__Divide_var_u64_i32
	.quad	_L_1+2841
	.quad	_Divide__uDivide_var_u64_i32
	.quad	_L_1+2860
	.quad	_Divide__Divide_param_u64_i16
	.quad	_L_1+2880
	.quad	_Divide__uDivide_param_u64_i16
	.quad	_L_1+2901
	.quad	_Divide__Divide_var_u64_i16
	.quad	_L_1+2923
	.quad	_Divide__uDivide_var_u64_i16
	.quad	_L_1+2942
	.quad	_Divide__Divide_param_u64_L
	.quad	_L_1+2962
	.quad	_Divide__uDivide_param_u64_L
	.quad	_L_1+2981
	.quad	_Divide__Divide_var_u64_L
	.quad	_L_1+3001
	.quad	_Divide__uDivide_var_u64_L
	.quad	_L_1+3018
	.quad	_Divide__Divide_param_u64_u64
	.quad	_L_1+3036
	.quad	_Divide__uDivide_param_u64_u64
	.quad	_L_1+3057
	.quad	_Divide__Divide_var_u64_u64
	.quad	_L_1+3079
	.quad	_Divide__uDivide_var_u64_u64
	.quad	_L_1+3098
	.quad	_Divide__Divide_param_u64_u16
	.quad	_L_1+3118
	.quad	_Divide__uDivide_param_u64_u16
	.quad	_L_1+3139
	.quad	_Divide__Divide_var_u64_u16
	.quad	_L_1+3161
	.quad	_Divide__uDivide_var_u64_u16
	.quad	_L_1+3180
	.quad	_Divide__Divide_param_u64_C
	.quad	_L_1+3200
	.quad	_Divide__uDivide_param_u64_C
	.quad	_L_1+3219
	.quad	_Divide__Divide_var_u64_C
	.quad	_L_1+3239
	.quad	_Divide__uDivide_var_u64_C
	.quad	_L_1+3256
	.quad	_Divide__Divide_param_u64_u32
	.quad	_L_1+3274
	.quad	_Divide__uDivide_param_u64_u32
	.quad	_L_1+3295
	.quad	_Divide__Divide_var_u64_u32
	.quad	_L_1+3317
	.quad	_Divide__uDivide_var_u64_u32
	.quad	_L_1+3336
	.quad	_Divide__Divide_param_u64_i8
	.quad	_L_1+3356
	.quad	_Divide__uDivide_param_u64_i8
	.quad	_L_1+3376
	.quad	_Divide__Divide_var_u64_i8
	.quad	_L_1+3397
	.quad	_Divide__uDivide_var_u64_i8
	.quad	_L_1+3415
	.quad	_Divide__Divide_param_u64_I
	.quad	_L_1+3434
	.quad	_Divide__uDivide_param_u64_I
	.quad	_L_1+3453
	.quad	_Divide__Divide_var_u64_I
	.quad	_L_1+3473
	.quad	_Divide__uDivide_var_u64_I
	.quad	_L_1+3490
	.quad	_Divide__Divide_param_u64_u8
	.quad	_L_1+3508
	.quad	_Divide__uDivide_param_u64_u8
	.quad	_L_1+3528
	.quad	_Divide__Divide_var_u64_u8
	.quad	_L_1+3549
	.quad	_Divide__uDivide_var_u64_u8
	.quad	_L_1+3567
	.quad	_Divide__Divide_param_u64_LC
	.quad	_L_1+3586
	.quad	_Divide__uDivide_param_u64_LC
	.quad	_L_1+3606
	.quad	_Divide__Divide_var_u64_LC
	.quad	_L_1+3627
	.quad	_Divide__uDivide_var_u64_LC
	.quad	_L_1+3645
	.quad	_Divide__Divide_param_u64_i64
	.quad	_L_1+3664
	.quad	_Divide__uDivide_param_u64_i64
	.quad	_L_1+3685
	.quad	_Divide__Divide_var_u64_i64
	.quad	_L_1+3707
	.quad	_Divide__uDivide_var_u64_i64
	.quad	_L_1+3726
	.quad	_Divide__Divide_param_u16_i32
	.quad	_L_1+3746
	.quad	_Divide__uDivide_param_u16_i32
	.quad	_L_1+3767
	.quad	_Divide__Divide_var_u16_i32
	.quad	_L_1+3789
	.quad	_Divide__uDivide_var_u16_i32
	.quad	_L_1+3808
	.quad	_Divide__Divide_param_u16_i16
	.quad	_L_1+3828
	.quad	_Divide__uDivide_param_u16_i16
	.quad	_L_1+3849
	.quad	_Divide__Divide_var_u16_i16
	.quad	_L_1+3871
	.quad	_Divide__uDivide_var_u16_i16
	.quad	_L_1+3890
	.quad	_Divide__Divide_param_u16_L
	.quad	_L_1+3910
	.quad	_Divide__uDivide_param_u16_L
	.quad	_L_1+3929
	.quad	_Divide__Divide_var_u16_L
	.quad	_L_1+3949
	.quad	_Divide__uDivide_var_u16_L
	.quad	_L_1+3966
	.quad	_Divide__Divide_param_u16_u64
	.quad	_L_1+3984
	.quad	_Divide__uDivide_param_u16_u64
	.quad	_L_1+4005
	.quad	_Divide__Divide_var_u16_u64
	.quad	_L_1+4027
	.quad	_Divide__uDivide_var_u16_u64
	.quad	_L_1+4046
	.quad	_Divide__Divide_param_u16_u16
	.quad	_L_1+4066
	.quad	_Divide__uDivide_param_u16_u16
	.quad	_L_1+4087
	.quad	_Divide__Divide_var_u16_u16
	.quad	_L_1+4109
	.quad	_Divide__uDivide_var_u16_u16
	.quad	_L_1+4128
	.quad	_Divide__Divide_param_u16_C
	.quad	_L_1+4148
	.quad	_Divide__uDivide_param_u16_C
	.quad	_L_1+4167
	.quad	_Divide__Divide_var_u16_C
	.quad	_L_1+4187
	.quad	_Divide__uDivide_var_u16_C
	.quad	_L_1+4204
	.quad	_Divide__Divide_param_u16_u32
	.quad	_L_1+4222
	.quad	_Divide__uDivide_param_u16_u32
	.quad	_L_1+4243
	.quad	_Divide__Divide_var_u16_u32
	.quad	_L_1+4265
	.quad	_Divide__uDivide_var_u16_u32
	.quad	_L_1+4284
	.quad	_Divide__Divide_param_u16_i8
	.quad	_L_1+4304
	.quad	_Divide__uDivide_param_u16_i8
	.quad	_L_1+4324
	.quad	_Divide__Divide_var_u16_i8
	.quad	_L_1+4345
	.quad	_Divide__uDivide_var_u16_i8
	.quad	_L_1+4363
	.quad	_Divide__Divide_param_u16_I
	.quad	_L_1+4382
	.quad	_Divide__uDivide_param_u16_I
	.quad	_L_1+4401
	.quad	_Divide__Divide_var_u16_I
	.quad	_L_1+4421
	.quad	_Divide__uDivide_var_u16_I
	.quad	_L_1+4438
	.quad	_Divide__Divide_param_u16_u8
	.quad	_L_1+4456
	.quad	_Divide__uDivide_param_u16_u8
	.quad	_L_1+4476
	.quad	_Divide__Divide_var_u16_u8
	.quad	_L_1+4497
	.quad	_Divide__uDivide_var_u16_u8
	.quad	_L_1+4515
	.quad	_Divide__Divide_param_u16_LC
	.quad	_L_1+4534
	.quad	_Divide__uDivide_param_u16_LC
	.quad	_L_1+4554
	.quad	_Divide__Divide_var_u16_LC
	.quad	_L_1+4575
	.quad	_Divide__uDivide_var_u16_LC
	.quad	_L_1+4593
	.quad	_Divide__Divide_param_u16_i64
	.quad	_L_1+4612
	.quad	_Divide__uDivide_param_u16_i64
	.quad	_L_1+4633
	.quad	_Divide__Divide_var_u16_i64
	.quad	_L_1+4655
	.quad	_Divide__uDivide_var_u16_i64
	.quad	_L_1+4674
	.quad	_Divide__Divide_param_C_i32
	.quad	_L_1+4694
	.quad	_Divide__uDivide_param_C_i32
	.quad	_L_1+4713
	.quad	_Divide__Divide_var_C_i32
	.quad	_L_1+4733
	.quad	_Divide__uDivide_var_C_i32
	.quad	_L_1+4750
	.quad	_Divide__Divide_param_C_i16
	.quad	_L_1+4768
	.quad	_Divide__uDivide_param_C_i16
	.quad	_L_1+4787
	.quad	_Divide__Divide_var_C_i16
	.quad	_L_1+4807
	.quad	_Divide__uDivide_var_C_i16
	.quad	_L_1+4824
	.quad	_Divide__Divide_param_C_L
	.quad	_L_1+4842
	.quad	_Divide__uDivide_param_C_L
	.quad	_L_1+4859
	.quad	_Divide__Divide_var_C_L
	.quad	_L_1+4877
	.quad	_Divide__uDivide_var_C_L
	.quad	_L_1+4892
	.quad	_Divide__Divide_param_C_u64
	.quad	_L_1+4908
	.quad	_Divide__uDivide_param_C_u64
	.quad	_L_1+4927
	.quad	_Divide__Divide_var_C_u64
	.quad	_L_1+4947
	.quad	_Divide__uDivide_var_C_u64
	.quad	_L_1+4964
	.quad	_Divide__Divide_param_C_u16
	.quad	_L_1+4982
	.quad	_Divide__uDivide_param_C_u16
	.quad	_L_1+5001
	.quad	_Divide__Divide_var_C_u16
	.quad	_L_1+5021
	.quad	_Divide__uDivide_var_C_u16
	.quad	_L_1+5038
	.quad	_Divide__Divide_param_C_C
	.quad	_L_1+5056
	.quad	_Divide__uDivide_param_C_C
	.quad	_L_1+5073
	.quad	_Divide__Divide_var_C_C
	.quad	_L_1+5091
	.quad	_Divide__uDivide_var_C_C
	.quad	_L_1+5106
	.quad	_Divide__Divide_param_C_u32
	.quad	_L_1+5122
	.quad	_Divide__uDivide_param_C_u32
	.quad	_L_1+5141
	.quad	_Divide__Divide_var_C_u32
	.quad	_L_1+5161
	.quad	_Divide__uDivide_var_C_u32
	.quad	_L_1+5178
	.quad	_Divide__Divide_param_C_i8
	.quad	_L_1+5196
	.quad	_Divide__uDivide_param_C_i8
	.quad	_L_1+5214
	.quad	_Divide__Divide_var_C_i8
	.quad	_L_1+5233
	.quad	_Divide__uDivide_var_C_i8
	.quad	_L_1+5249
	.quad	_Divide__Divide_param_C_I
	.quad	_L_1+5266
	.quad	_Divide__uDivide_param_C_I
	.quad	_L_1+5283
	.quad	_Divide__Divide_var_C_I
	.quad	_L_1+5301
	.quad	_Divide__uDivide_var_C_I
	.quad	_L_1+5316
	.quad	_Divide__Divide_param_C_u8
	.quad	_L_1+5332
	.quad	_Divide__uDivide_param_C_u8
	.quad	_L_1+5350
	.quad	_Divide__Divide_var_C_u8
	.quad	_L_1+5369
	.quad	_Divide__uDivide_var_C_u8
	.quad	_L_1+5385
	.quad	_Divide__Divide_param_C_LC
	.quad	_L_1+5402
	.quad	_Divide__uDivide_param_C_LC
	.quad	_L_1+5420
	.quad	_Divide__Divide_var_C_LC
	.quad	_L_1+5439
	.quad	_Divide__uDivide_var_C_LC
	.quad	_L_1+5455
	.quad	_Divide__Divide_param_C_i64
	.quad	_L_1+5472
	.quad	_Divide__uDivide_param_C_i64
	.quad	_L_1+5491
	.quad	_Divide__Divide_var_C_i64
	.quad	_L_1+5511
	.quad	_Divide__uDivide_var_C_i64
	.quad	_L_1+5528
	.quad	_Divide__Divide_param_u32_i32
	.quad	_L_1+5546
	.quad	_Divide__uDivide_param_u32_i32
	.quad	_L_1+5567
	.quad	_Divide__Divide_var_u32_i32
	.quad	_L_1+5589
	.quad	_Divide__uDivide_var_u32_i32
	.quad	_L_1+5608
	.quad	_Divide__Divide_param_u32_i16
	.quad	_L_1+5628
	.quad	_Divide__uDivide_param_u32_i16
	.quad	_L_1+5649
	.quad	_Divide__Divide_var_u32_i16
	.quad	_L_1+5671
	.quad	_Divide__uDivide_var_u32_i16
	.quad	_L_1+5690
	.quad	_Divide__Divide_param_u32_L
	.quad	_L_1+5710
	.quad	_Divide__uDivide_param_u32_L
	.quad	_L_1+5729
	.quad	_Divide__Divide_var_u32_L
	.quad	_L_1+5749
	.quad	_Divide__uDivide_var_u32_L
	.quad	_L_1+5766
	.quad	_Divide__Divide_param_u32_u64
	.quad	_L_1+5784
	.quad	_Divide__uDivide_param_u32_u64
	.quad	_L_1+5805
	.quad	_Divide__Divide_var_u32_u64
	.quad	_L_1+5827
	.quad	_Divide__uDivide_var_u32_u64
	.quad	_L_1+5846
	.quad	_Divide__Divide_param_u32_u16
	.quad	_L_1+5866
	.quad	_Divide__uDivide_param_u32_u16
	.quad	_L_1+5887
	.quad	_Divide__Divide_var_u32_u16
	.quad	_L_1+5909
	.quad	_Divide__uDivide_var_u32_u16
	.quad	_L_1+5928
	.quad	_Divide__Divide_param_u32_C
	.quad	_L_1+5948
	.quad	_Divide__uDivide_param_u32_C
	.quad	_L_1+5967
	.quad	_Divide__Divide_var_u32_C
	.quad	_L_1+5987
	.quad	_Divide__uDivide_var_u32_C
	.quad	_L_1+6004
	.quad	_Divide__Divide_param_u32_u32
	.quad	_L_1+6022
	.quad	_Divide__uDivide_param_u32_u32
	.quad	_L_1+6043
	.quad	_Divide__Divide_var_u32_u32
	.quad	_L_1+6065
	.quad	_Divide__uDivide_var_u32_u32
	.quad	_L_1+6084
	.quad	_Divide__Divide_param_u32_i8
	.quad	_L_1+6104
	.quad	_Divide__uDivide_param_u32_i8
	.quad	_L_1+6124
	.quad	_Divide__Divide_var_u32_i8
	.quad	_L_1+6145
	.quad	_Divide__uDivide_var_u32_i8
	.quad	_L_1+6163
	.quad	_Divide__Divide_param_u32_I
	.quad	_L_1+6182
	.quad	_Divide__uDivide_param_u32_I
	.quad	_L_1+6201
	.quad	_Divide__Divide_var_u32_I
	.quad	_L_1+6221
	.quad	_Divide__uDivide_var_u32_I
	.quad	_L_1+6238
	.quad	_Divide__Divide_param_u32_u8
	.quad	_L_1+6256
	.quad	_Divide__uDivide_param_u32_u8
	.quad	_L_1+6276
	.quad	_Divide__Divide_var_u32_u8
	.quad	_L_1+6297
	.quad	_Divide__uDivide_var_u32_u8
	.quad	_L_1+6315
	.quad	_Divide__Divide_param_u32_LC
	.quad	_L_1+6334
	.quad	_Divide__uDivide_param_u32_LC
	.quad	_L_1+6354
	.quad	_Divide__Divide_var_u32_LC
	.quad	_L_1+6375
	.quad	_Divide__uDivide_var_u32_LC
	.quad	_L_1+6393
	.quad	_Divide__Divide_param_u32_i64
	.quad	_L_1+6412
	.quad	_Divide__uDivide_param_u32_i64
	.quad	_L_1+6433
	.quad	_Divide__Divide_var_u32_i64
	.quad	_L_1+6455
	.quad	_Divide__uDivide_var_u32_i64
	.quad	_L_1+6474
	.quad	_Divide__Divide_param_i8_i32
	.quad	_L_1+6494
	.quad	_Divide__uDivide_param_i8_i32
	.quad	_L_1+6514
	.quad	_Divide__Divide_var_i8_i32
	.quad	_L_1+6535
	.quad	_Divide__uDivide_var_i8_i32
	.quad	_L_1+6553
	.quad	_Divide__Divide_param_i8_i16
	.quad	_L_1+6572
	.quad	_Divide__uDivide_param_i8_i16
	.quad	_L_1+6592
	.quad	_Divide__Divide_var_i8_i16
	.quad	_L_1+6613
	.quad	_Divide__uDivide_var_i8_i16
	.quad	_L_1+6631
	.quad	_Divide__Divide_param_i8_L
	.quad	_L_1+6650
	.quad	_Divide__uDivide_param_i8_L
	.quad	_L_1+6668
	.quad	_Divide__Divide_var_i8_L
	.quad	_L_1+6687
	.quad	_Divide__uDivide_var_i8_L
	.quad	_L_1+6703
	.quad	_Divide__Divide_param_i8_u64
	.quad	_L_1+6720
	.quad	_Divide__uDivide_param_i8_u64
	.quad	_L_1+6740
	.quad	_Divide__Divide_var_i8_u64
	.quad	_L_1+6761
	.quad	_Divide__uDivide_var_i8_u64
	.quad	_L_1+6779
	.quad	_Divide__Divide_param_i8_u16
	.quad	_L_1+6798
	.quad	_Divide__uDivide_param_i8_u16
	.quad	_L_1+6818
	.quad	_Divide__Divide_var_i8_u16
	.quad	_L_1+6839
	.quad	_Divide__uDivide_var_i8_u16
	.quad	_L_1+6857
	.quad	_Divide__Divide_param_i8_C
	.quad	_L_1+6876
	.quad	_Divide__uDivide_param_i8_C
	.quad	_L_1+6894
	.quad	_Divide__Divide_var_i8_C
	.quad	_L_1+6913
	.quad	_Divide__uDivide_var_i8_C
	.quad	_L_1+6929
	.quad	_Divide__Divide_param_i8_u32
	.quad	_L_1+6946
	.quad	_Divide__uDivide_param_i8_u32
	.quad	_L_1+6966
	.quad	_Divide__Divide_var_i8_u32
	.quad	_L_1+6987
	.quad	_Divide__uDivide_var_i8_u32
	.quad	_L_1+7005
	.quad	_Divide__Divide_param_i8_i8
	.quad	_L_1+7024
	.quad	_Divide__uDivide_param_i8_i8
	.quad	_L_1+7043
	.quad	_Divide__Divide_var_i8_i8
	.quad	_L_1+7063
	.quad	_Divide__uDivide_var_i8_i8
	.quad	_L_1+7080
	.quad	_Divide__Divide_param_i8_I
	.quad	_L_1+7098
	.quad	_Divide__uDivide_param_i8_I
	.quad	_L_1+7116
	.quad	_Divide__Divide_var_i8_I
	.quad	_L_1+7135
	.quad	_Divide__uDivide_var_i8_I
	.quad	_L_1+7151
	.quad	_Divide__Divide_param_i8_u8
	.quad	_L_1+7168
	.quad	_Divide__uDivide_param_i8_u8
	.quad	_L_1+7187
	.quad	_Divide__Divide_var_i8_u8
	.quad	_L_1+7207
	.quad	_Divide__uDivide_var_i8_u8
	.quad	_L_1+7224
	.quad	_Divide__Divide_param_i8_LC
	.quad	_L_1+7242
	.quad	_Divide__uDivide_param_i8_LC
	.quad	_L_1+7261
	.quad	_Divide__Divide_var_i8_LC
	.quad	_L_1+7281
	.quad	_Divide__uDivide_var_i8_LC
	.quad	_L_1+7298
	.quad	_Divide__Divide_param_i8_i64
	.quad	_L_1+7316
	.quad	_Divide__uDivide_param_i8_i64
	.quad	_L_1+7336
	.quad	_Divide__Divide_var_i8_i64
	.quad	_L_1+7357
	.quad	_Divide__uDivide_var_i8_i64
	.quad	_L_1+7375
	.quad	_Divide__Divide_param_I_i32
	.quad	_L_1+7394
	.quad	_Divide__uDivide_param_I_i32
	.quad	_L_1+7413
	.quad	_Divide__Divide_var_I_i32
	.quad	_L_1+7433
	.quad	_Divide__uDivide_var_I_i32
	.quad	_L_1+7450
	.quad	_Divide__Divide_param_I_i16
	.quad	_L_1+7468
	.quad	_Divide__uDivide_param_I_i16
	.quad	_L_1+7487
	.quad	_Divide__Divide_var_I_i16
	.quad	_L_1+7507
	.quad	_Divide__uDivide_var_I_i16
	.quad	_L_1+7524
	.quad	_Divide__Divide_param_I_L
	.quad	_L_1+7542
	.quad	_Divide__uDivide_param_I_L
	.quad	_L_1+7559
	.quad	_Divide__Divide_var_I_L
	.quad	_L_1+7577
	.quad	_Divide__uDivide_var_I_L
	.quad	_L_1+7592
	.quad	_Divide__Divide_param_I_u64
	.quad	_L_1+7608
	.quad	_Divide__uDivide_param_I_u64
	.quad	_L_1+7627
	.quad	_Divide__Divide_var_I_u64
	.quad	_L_1+7647
	.quad	_Divide__uDivide_var_I_u64
	.quad	_L_1+7664
	.quad	_Divide__Divide_param_I_u16
	.quad	_L_1+7682
	.quad	_Divide__uDivide_param_I_u16
	.quad	_L_1+7701
	.quad	_Divide__Divide_var_I_u16
	.quad	_L_1+7721
	.quad	_Divide__uDivide_var_I_u16
	.quad	_L_1+7738
	.quad	_Divide__Divide_param_I_C
	.quad	_L_1+7756
	.quad	_Divide__uDivide_param_I_C
	.quad	_L_1+7773
	.quad	_Divide__Divide_var_I_C
	.quad	_L_1+7791
	.quad	_Divide__uDivide_var_I_C
	.quad	_L_1+7806
	.quad	_Divide__Divide_param_I_u32
	.quad	_L_1+7822
	.quad	_Divide__uDivide_param_I_u32
	.quad	_L_1+7841
	.quad	_Divide__Divide_var_I_u32
	.quad	_L_1+7861
	.quad	_Divide__uDivide_var_I_u32
	.quad	_L_1+7878
	.quad	_Divide__Divide_param_I_i8
	.quad	_L_1+7896
	.quad	_Divide__uDivide_param_I_i8
	.quad	_L_1+7914
	.quad	_Divide__Divide_var_I_i8
	.quad	_L_1+7933
	.quad	_Divide__uDivide_var_I_i8
	.quad	_L_1+7949
	.quad	_Divide__Divide_param_I_I
	.quad	_L_1+7966
	.quad	_Divide__uDivide_param_I_I
	.quad	_L_1+7983
	.quad	_Divide__Divide_var_I_I
	.quad	_L_1+8001
	.quad	_Divide__uDivide_var_I_I
	.quad	_L_1+8016
	.quad	_Divide__Divide_param_I_u8
	.quad	_L_1+8032
	.quad	_Divide__uDivide_param_I_u8
	.quad	_L_1+8050
	.quad	_Divide__Divide_var_I_u8
	.quad	_L_1+8069
	.quad	_Divide__uDivide_var_I_u8
	.quad	_L_1+8085
	.quad	_Divide__Divide_param_I_LC
	.quad	_L_1+8102
	.quad	_Divide__uDivide_param_I_LC
	.quad	_L_1+8120
	.quad	_Divide__Divide_var_I_LC
	.quad	_L_1+8139
	.quad	_Divide__uDivide_var_I_LC
	.quad	_L_1+8155
	.quad	_Divide__Divide_param_I_i64
	.quad	_L_1+8172
	.quad	_Divide__uDivide_param_I_i64
	.quad	_L_1+8191
	.quad	_Divide__Divide_var_I_i64
	.quad	_L_1+8211
	.quad	_Divide__uDivide_var_I_i64
	.quad	_L_1+8228
	.quad	_Divide__Divide_param_u8_i32
	.quad	_L_1+8246
	.quad	_Divide__uDivide_param_u8_i32
	.quad	_L_1+8266
	.quad	_Divide__Divide_var_u8_i32
	.quad	_L_1+8287
	.quad	_Divide__uDivide_var_u8_i32
	.quad	_L_1+8305
	.quad	_Divide__Divide_param_u8_i16
	.quad	_L_1+8324
	.quad	_Divide__uDivide_param_u8_i16
	.quad	_L_1+8344
	.quad	_Divide__Divide_var_u8_i16
	.quad	_L_1+8365
	.quad	_Divide__uDivide_var_u8_i16
	.quad	_L_1+8383
	.quad	_Divide__Divide_param_u8_L
	.quad	_L_1+8402
	.quad	_Divide__uDivide_param_u8_L
	.quad	_L_1+8420
	.quad	_Divide__Divide_var_u8_L
	.quad	_L_1+8439
	.quad	_Divide__uDivide_var_u8_L
	.quad	_L_1+8455
	.quad	_Divide__Divide_param_u8_u64
	.quad	_L_1+8472
	.quad	_Divide__uDivide_param_u8_u64
	.quad	_L_1+8492
	.quad	_Divide__Divide_var_u8_u64
	.quad	_L_1+8513
	.quad	_Divide__uDivide_var_u8_u64
	.quad	_L_1+8531
	.quad	_Divide__Divide_param_u8_u16
	.quad	_L_1+8550
	.quad	_Divide__uDivide_param_u8_u16
	.quad	_L_1+8570
	.quad	_Divide__Divide_var_u8_u16
	.quad	_L_1+8591
	.quad	_Divide__uDivide_var_u8_u16
	.quad	_L_1+8609
	.quad	_Divide__Divide_param_u8_C
	.quad	_L_1+8628
	.quad	_Divide__uDivide_param_u8_C
	.quad	_L_1+8646
	.quad	_Divide__Divide_var_u8_C
	.quad	_L_1+8665
	.quad	_Divide__uDivide_var_u8_C
	.quad	_L_1+8681
	.quad	_Divide__Divide_param_u8_u32
	.quad	_L_1+8698
	.quad	_Divide__uDivide_param_u8_u32
	.quad	_L_1+8718
	.quad	_Divide__Divide_var_u8_u32
	.quad	_L_1+8739
	.quad	_Divide__uDivide_var_u8_u32
	.quad	_L_1+8757
	.quad	_Divide__Divide_param_u8_i8
	.quad	_L_1+8776
	.quad	_Divide__uDivide_param_u8_i8
	.quad	_L_1+8795
	.quad	_Divide__Divide_var_u8_i8
	.quad	_L_1+8815
	.quad	_Divide__uDivide_var_u8_i8
	.quad	_L_1+8832
	.quad	_Divide__Divide_param_u8_I
	.quad	_L_1+8850
	.quad	_Divide__uDivide_param_u8_I
	.quad	_L_1+8868
	.quad	_Divide__Divide_var_u8_I
	.quad	_L_1+8887
	.quad	_Divide__uDivide_var_u8_I
	.quad	_L_1+8903
	.quad	_Divide__Divide_param_u8_u8
	.quad	_L_1+8920
	.quad	_Divide__uDivide_param_u8_u8
	.quad	_L_1+8939
	.quad	_Divide__Divide_var_u8_u8
	.quad	_L_1+8959
	.quad	_Divide__uDivide_var_u8_u8
	.quad	_L_1+8976
	.quad	_Divide__Divide_param_u8_LC
	.quad	_L_1+8994
	.quad	_Divide__uDivide_param_u8_LC
	.quad	_L_1+9013
	.quad	_Divide__Divide_var_u8_LC
	.quad	_L_1+9033
	.quad	_Divide__uDivide_var_u8_LC
	.quad	_L_1+9050
	.quad	_Divide__Divide_param_u8_i64
	.quad	_L_1+9068
	.quad	_Divide__uDivide_param_u8_i64
	.quad	_L_1+9088
	.quad	_Divide__Divide_var_u8_i64
	.quad	_L_1+9109
	.quad	_Divide__uDivide_var_u8_i64
	.quad	_L_1+9127
	.quad	_Divide__Divide_param_f32_f32
	.quad	_L_1+9146
	.quad	_Divide__Divide_var_f32_f32
	.quad	_L_1+9167
	.quad	_Divide__Divide_param_LC_i32
	.quad	_L_1+9186
	.quad	_Divide__uDivide_param_LC_i32
	.quad	_L_1+9206
	.quad	_Divide__Divide_var_LC_i32
	.quad	_L_1+9227
	.quad	_Divide__uDivide_var_LC_i32
	.quad	_L_1+9245
	.quad	_Divide__Divide_param_LC_i16
	.quad	_L_1+9264
	.quad	_Divide__uDivide_param_LC_i16
	.quad	_L_1+9284
	.quad	_Divide__Divide_var_LC_i16
	.quad	_L_1+9305
	.quad	_Divide__uDivide_var_LC_i16
	.quad	_L_1+9323
	.quad	_Divide__Divide_param_LC_L
	.quad	_L_1+9342
	.quad	_Divide__uDivide_param_LC_L
	.quad	_L_1+9360
	.quad	_Divide__Divide_var_LC_L
	.quad	_L_1+9379
	.quad	_Divide__uDivide_var_LC_L
	.quad	_L_1+9395
	.quad	_Divide__Divide_param_LC_u64
	.quad	_L_1+9412
	.quad	_Divide__uDivide_param_LC_u64
	.quad	_L_1+9432
	.quad	_Divide__Divide_var_LC_u64
	.quad	_L_1+9453
	.quad	_Divide__uDivide_var_LC_u64
	.quad	_L_1+9471
	.quad	_Divide__Divide_param_LC_u16
	.quad	_L_1+9490
	.quad	_Divide__uDivide_param_LC_u16
	.quad	_L_1+9510
	.quad	_Divide__Divide_var_LC_u16
	.quad	_L_1+9531
	.quad	_Divide__uDivide_var_LC_u16
	.quad	_L_1+9549
	.quad	_Divide__Divide_param_LC_C
	.quad	_L_1+9568
	.quad	_Divide__uDivide_param_LC_C
	.quad	_L_1+9586
	.quad	_Divide__Divide_var_LC_C
	.quad	_L_1+9605
	.quad	_Divide__uDivide_var_LC_C
	.quad	_L_1+9621
	.quad	_Divide__Divide_param_LC_u32
	.quad	_L_1+9638
	.quad	_Divide__uDivide_param_LC_u32
	.quad	_L_1+9658
	.quad	_Divide__Divide_var_LC_u32
	.quad	_L_1+9679
	.quad	_Divide__uDivide_var_LC_u32
	.quad	_L_1+9697
	.quad	_Divide__Divide_param_LC_i8
	.quad	_L_1+9716
	.quad	_Divide__uDivide_param_LC_i8
	.quad	_L_1+9735
	.quad	_Divide__Divide_var_LC_i8
	.quad	_L_1+9755
	.quad	_Divide__uDivide_var_LC_i8
	.quad	_L_1+9772
	.quad	_Divide__Divide_param_LC_I
	.quad	_L_1+9790
	.quad	_Divide__uDivide_param_LC_I
	.quad	_L_1+9808
	.quad	_Divide__Divide_var_LC_I
	.quad	_L_1+9827
	.quad	_Divide__uDivide_var_LC_I
	.quad	_L_1+9843
	.quad	_Divide__Divide_param_LC_u8
	.quad	_L_1+9860
	.quad	_Divide__uDivide_param_LC_u8
	.quad	_L_1+9879
	.quad	_Divide__Divide_var_LC_u8
	.quad	_L_1+9899
	.quad	_Divide__uDivide_var_LC_u8
	.quad	_L_1+9916
	.quad	_Divide__Divide_param_LC_LC
	.quad	_L_1+9934
	.quad	_Divide__uDivide_param_LC_LC
	.quad	_L_1+9953
	.quad	_Divide__Divide_var_LC_LC
	.quad	_L_1+9973
	.quad	_Divide__uDivide_var_LC_LC
	.quad	_L_1+9990
	.quad	_Divide__Divide_param_LC_i64
	.quad	_L_1+10008
	.quad	_Divide__uDivide_param_LC_i64
	.quad	_L_1+10028
	.quad	_Divide__Divide_var_LC_i64
	.quad	_L_1+10049
	.quad	_Divide__uDivide_var_LC_i64
	.quad	_L_1+10067
	.quad	_Divide__Divide_param_i64_i32
	.quad	_L_1+10086
	.quad	_Divide__uDivide_param_i64_i32
	.quad	_L_1+10107
	.quad	_Divide__Divide_var_i64_i32
	.quad	_L_1+10129
	.quad	_Divide__uDivide_var_i64_i32
	.quad	_L_1+10148
	.quad	_Divide__Divide_param_i64_i16
	.quad	_L_1+10168
	.quad	_Divide__uDivide_param_i64_i16
	.quad	_L_1+10189
	.quad	_Divide__Divide_var_i64_i16
	.quad	_L_1+10211
	.quad	_Divide__uDivide_var_i64_i16
	.quad	_L_1+10230
	.quad	_Divide__Divide_param_i64_L
	.quad	_L_1+10250
	.quad	_Divide__uDivide_param_i64_L
	.quad	_L_1+10269
	.quad	_Divide__Divide_var_i64_L
	.quad	_L_1+10289
	.quad	_Divide__uDivide_var_i64_L
	.quad	_L_1+10306
	.quad	_Divide__Divide_param_i64_u64
	.quad	_L_1+10324
	.quad	_Divide__uDivide_param_i64_u64
	.quad	_L_1+10345
	.quad	_Divide__Divide_var_i64_u64
	.quad	_L_1+10367
	.quad	_Divide__uDivide_var_i64_u64
	.quad	_L_1+10386
	.quad	_Divide__Divide_param_i64_u16
	.quad	_L_1+10406
	.quad	_Divide__uDivide_param_i64_u16
	.quad	_L_1+10427
	.quad	_Divide__Divide_var_i64_u16
	.quad	_L_1+10449
	.quad	_Divide__uDivide_var_i64_u16
	.quad	_L_1+10468
	.quad	_Divide__Divide_param_i64_C
	.quad	_L_1+10488
	.quad	_Divide__uDivide_param_i64_C
	.quad	_L_1+10507
	.quad	_Divide__Divide_var_i64_C
	.quad	_L_1+10527
	.quad	_Divide__uDivide_var_i64_C
	.quad	_L_1+10544
	.quad	_Divide__Divide_param_i64_u32
	.quad	_L_1+10562
	.quad	_Divide__uDivide_param_i64_u32
	.quad	_L_1+10583
	.quad	_Divide__Divide_var_i64_u32
	.quad	_L_1+10605
	.quad	_Divide__uDivide_var_i64_u32
	.quad	_L_1+10624
	.quad	_Divide__Divide_param_i64_i8
	.quad	_L_1+10644
	.quad	_Divide__uDivide_param_i64_i8
	.quad	_L_1+10664
	.quad	_Divide__Divide_var_i64_i8
	.quad	_L_1+10685
	.quad	_Divide__uDivide_var_i64_i8
	.quad	_L_1+10703
	.quad	_Divide__Divide_param_i64_I
	.quad	_L_1+10722
	.quad	_Divide__uDivide_param_i64_I
	.quad	_L_1+10741
	.quad	_Divide__Divide_var_i64_I
	.quad	_L_1+10761
	.quad	_Divide__uDivide_var_i64_I
	.quad	_L_1+10778
	.quad	_Divide__Divide_param_i64_u8
	.quad	_L_1+10796
	.quad	_Divide__uDivide_param_i64_u8
	.quad	_L_1+10816
	.quad	_Divide__Divide_var_i64_u8
	.quad	_L_1+10837
	.quad	_Divide__uDivide_var_i64_u8
	.quad	_L_1+10855
	.quad	_Divide__Divide_param_i64_LC
	.quad	_L_1+10874
	.quad	_Divide__uDivide_param_i64_LC
	.quad	_L_1+10894
	.quad	_Divide__Divide_var_i64_LC
	.quad	_L_1+10915
	.quad	_Divide__uDivide_var_i64_LC
	.quad	_L_1+10933
	.quad	_Divide__Divide_param_i64_i64
	.quad	_L_1+10952
	.quad	_Divide__uDivide_param_i64_i64
	.quad	_L_1+10973
	.quad	_Divide__Divide_var_i64_i64
	.quad	_L_1+10995
	.quad	_Divide__uDivide_var_i64_i64
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
	.quad	241
	.quad	242
	.long	1131626103
	.byte	-11
	.space 3
	.quad	246
	.byte	-9
	.space 3
	.long	248
	.quad	249
	.word	250
	.space 6
	.quad	251
	.long	2473901162
	.long	1081051160
	.quad	254
	.word	255
	.space 2
	.long	256
	.space 24
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
