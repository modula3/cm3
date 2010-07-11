	.text
.globl _Mod__uMod_var_u16_u16
	.private_extern _Mod__uMod_var_u16_u16
_Mod__uMod_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_u16
	.private_extern _Mod__Mod_var_u16_u16
_Mod__Mod_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L4
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L4
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L4:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L5
	cmpq	$32767, -16(%rbp)
	jle	L6
L5:
	movl	$1761, %edi
	call	__m3_fault
L6:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_u16
	.private_extern _Mod__uMod_param_u16_u16
_Mod__uMod_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_u16
	.private_extern _Mod__Mod_param_u16_u16
_Mod__Mod_param_u16_u16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L11
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L11
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L11:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L12
	cmpq	$32767, -16(%rbp)
	jle	L13
L12:
	movl	$1825, %edi
	call	__m3_fault
L13:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u64
	.private_extern _Mod__uMod_var_u16_u64
_Mod__uMod_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_u64
	.private_extern _Mod__Mod_var_u16_u64
_Mod__Mod_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L18
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L18
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L18:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_u64
	.private_extern _Mod__uMod_param_u16_u64
_Mod__uMod_param_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_u64
	.private_extern _Mod__Mod_param_u16_u64
_Mod__Mod_param_u16_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L23
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L23
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L23:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_LC
	.private_extern _Mod__uMod_var_u16_LC
_Mod__uMod_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_LC
	.private_extern _Mod__Mod_var_u16_LC
_Mod__Mod_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L28
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L28
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L28:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_LC
	.private_extern _Mod__uMod_param_u16_LC
_Mod__uMod_param_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_LC
	.private_extern _Mod__Mod_param_u16_LC
_Mod__Mod_param_u16_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L33
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L33
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L33:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i16
	.private_extern _Mod__uMod_var_u16_i16
_Mod__uMod_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_i16
	.private_extern _Mod__Mod_var_u16_i16
_Mod__Mod_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L38
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L38
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L38:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L39
	movl	$2145, %edi
	call	__m3_fault
L39:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_i16
	.private_extern _Mod__uMod_param_u16_i16
_Mod__uMod_param_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_i16
	.private_extern _Mod__Mod_param_u16_i16
_Mod__Mod_param_u16_i16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L44
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L44
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L44:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L45
	movl	$2209, %edi
	call	__m3_fault
L45:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i32
	.private_extern _Mod__uMod_var_u16_i32
_Mod__uMod_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_i32
	.private_extern _Mod__Mod_var_u16_i32
_Mod__Mod_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L50
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L50
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L50:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L51
	movl	$2273, %edi
	call	__m3_fault
L51:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_i32
	.private_extern _Mod__uMod_param_u16_i32
_Mod__uMod_param_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_i32
	.private_extern _Mod__Mod_param_u16_i32
_Mod__Mod_param_u16_i32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L56
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L56
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L56:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L57
	movl	$2337, %edi
	call	__m3_fault
L57:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_I
	.private_extern _Mod__uMod_var_u16_I
_Mod__uMod_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_I
	.private_extern _Mod__Mod_var_u16_I
_Mod__Mod_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L62
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L62
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L62:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L63
	cmpq	$32767, -16(%rbp)
	jle	L64
L63:
	movl	$2401, %edi
	call	__m3_fault
L64:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_I
	.private_extern _Mod__uMod_param_u16_I
_Mod__uMod_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_I
	.private_extern _Mod__Mod_param_u16_I
_Mod__Mod_param_u16_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L69
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L69
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L69:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L70
	cmpq	$32767, -16(%rbp)
	jle	L71
L70:
	movl	$2465, %edi
	call	__m3_fault
L71:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i64
	.private_extern _Mod__uMod_var_u16_i64
_Mod__uMod_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_i64
	.private_extern _Mod__Mod_var_u16_i64
_Mod__Mod_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L76
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L76
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L76:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_i64
	.private_extern _Mod__uMod_param_u16_i64
_Mod__uMod_param_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_i64
	.private_extern _Mod__Mod_param_u16_i64
_Mod__Mod_param_u16_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L81
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L81
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L81:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_C
	.private_extern _Mod__uMod_var_u16_C
_Mod__uMod_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_C
	.private_extern _Mod__Mod_var_u16_C
_Mod__Mod_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L86
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L86
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L86:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L87
	movl	$2657, %edi
	call	__m3_fault
L87:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_C
	.private_extern _Mod__uMod_param_u16_C
_Mod__uMod_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_C
	.private_extern _Mod__Mod_param_u16_C
_Mod__Mod_param_u16_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L92
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L92
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L92:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L93
	movl	$2721, %edi
	call	__m3_fault
L93:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u8
	.private_extern _Mod__uMod_var_u16_u8
_Mod__uMod_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_u8
	.private_extern _Mod__Mod_var_u16_u8
_Mod__Mod_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L98
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L98
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L98:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L99
	cmpq	$32767, -16(%rbp)
	jle	L100
L99:
	movl	$2785, %edi
	call	__m3_fault
L100:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_u8
	.private_extern _Mod__uMod_param_u16_u8
_Mod__uMod_param_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_u8
	.private_extern _Mod__Mod_param_u16_u8
_Mod__Mod_param_u16_u8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L105
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L105
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L105:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L106
	cmpq	$32767, -16(%rbp)
	jle	L107
L106:
	movl	$2849, %edi
	call	__m3_fault
L107:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i8
	.private_extern _Mod__uMod_var_u16_i8
_Mod__uMod_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_i8
	.private_extern _Mod__Mod_var_u16_i8
_Mod__Mod_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L112
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L112
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L112:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_i8
	.private_extern _Mod__uMod_param_u16_i8
_Mod__uMod_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_i8
	.private_extern _Mod__Mod_param_u16_i8
_Mod__Mod_param_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L117
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L117
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L117:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_L
	.private_extern _Mod__uMod_var_u16_L
_Mod__uMod_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_L
	.private_extern _Mod__Mod_var_u16_L
_Mod__Mod_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L122
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L122
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L122:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_L
	.private_extern _Mod__uMod_param_u16_L
_Mod__uMod_param_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_L
	.private_extern _Mod__Mod_param_u16_L
_Mod__Mod_param_u16_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L127
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L127
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L127:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u32
	.private_extern _Mod__uMod_var_u16_u32
_Mod__uMod_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u16_u32
	.private_extern _Mod__Mod_var_u16_u32
_Mod__Mod_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L132
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L132
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L132:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L133
	cmpq	$32767, -16(%rbp)
	jle	L134
L133:
	movl	$3169, %edi
	call	__m3_fault
L134:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u16_u32
	.private_extern _Mod__uMod_param_u16_u32
_Mod__uMod_param_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u16_u32
	.private_extern _Mod__Mod_param_u16_u32
_Mod__Mod_param_u16_u32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L139
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L139
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L139:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L140
	cmpq	$32767, -16(%rbp)
	jle	L141
L140:
	movl	$3233, %edi
	call	__m3_fault
L141:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u16
	.private_extern _Mod__uMod_var_u64_u16
_Mod__uMod_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_u16
	.private_extern _Mod__Mod_var_u64_u16
_Mod__Mod_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L146
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L146
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L146:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_u16
	.private_extern _Mod__uMod_param_u64_u16
_Mod__uMod_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_u16
	.private_extern _Mod__Mod_param_u64_u16
_Mod__Mod_param_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L151
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L151
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L151:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u64
	.private_extern _Mod__uMod_var_u64_u64
_Mod__uMod_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_u64
	.private_extern _Mod__Mod_var_u64_u64
_Mod__Mod_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L156
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L156
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L156:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_u64
	.private_extern _Mod__uMod_param_u64_u64
_Mod__uMod_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_u64
	.private_extern _Mod__Mod_param_u64_u64
_Mod__Mod_param_u64_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L161
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L161
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L161:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_LC
	.private_extern _Mod__uMod_var_u64_LC
_Mod__uMod_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_LC
	.private_extern _Mod__Mod_var_u64_LC
_Mod__Mod_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L166
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L166
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L166:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_LC
	.private_extern _Mod__uMod_param_u64_LC
_Mod__uMod_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_LC
	.private_extern _Mod__Mod_param_u64_LC
_Mod__Mod_param_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L171
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L171
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L171:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i16
	.private_extern _Mod__uMod_var_u64_i16
_Mod__uMod_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_i16
	.private_extern _Mod__Mod_var_u64_i16
_Mod__Mod_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L176
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L176
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L176:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_i16
	.private_extern _Mod__uMod_param_u64_i16
_Mod__uMod_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_i16
	.private_extern _Mod__Mod_param_u64_i16
_Mod__Mod_param_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L181
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L181
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L181:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i32
	.private_extern _Mod__uMod_var_u64_i32
_Mod__uMod_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_i32
	.private_extern _Mod__Mod_var_u64_i32
_Mod__Mod_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L186
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L186
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L186:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_i32
	.private_extern _Mod__uMod_param_u64_i32
_Mod__uMod_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_i32
	.private_extern _Mod__Mod_param_u64_i32
_Mod__Mod_param_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L191
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L191
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L191:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_I
	.private_extern _Mod__uMod_var_u64_I
_Mod__uMod_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_I
	.private_extern _Mod__Mod_var_u64_I
_Mod__Mod_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L196
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L196
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L196:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_I
	.private_extern _Mod__uMod_param_u64_I
_Mod__uMod_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_I
	.private_extern _Mod__Mod_param_u64_I
_Mod__Mod_param_u64_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L201
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L201
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L201:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i64
	.private_extern _Mod__uMod_var_u64_i64
_Mod__uMod_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_i64
	.private_extern _Mod__Mod_var_u64_i64
_Mod__Mod_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L206
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L206
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L206:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_i64
	.private_extern _Mod__uMod_param_u64_i64
_Mod__uMod_param_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_i64
	.private_extern _Mod__Mod_param_u64_i64
_Mod__Mod_param_u64_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L211
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L211
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L211:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_C
	.private_extern _Mod__uMod_var_u64_C
_Mod__uMod_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_C
	.private_extern _Mod__Mod_var_u64_C
_Mod__Mod_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L216
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L216
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L216:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_C
	.private_extern _Mod__uMod_param_u64_C
_Mod__uMod_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_C
	.private_extern _Mod__Mod_param_u64_C
_Mod__Mod_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L221
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L221
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L221:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u8
	.private_extern _Mod__uMod_var_u64_u8
_Mod__uMod_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_u8
	.private_extern _Mod__Mod_var_u64_u8
_Mod__Mod_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L226
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L226
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L226:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_u8
	.private_extern _Mod__uMod_param_u64_u8
_Mod__uMod_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_u8
	.private_extern _Mod__Mod_param_u64_u8
_Mod__Mod_param_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L231
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L231
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L231:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i8
	.private_extern _Mod__uMod_var_u64_i8
_Mod__uMod_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_i8
	.private_extern _Mod__Mod_var_u64_i8
_Mod__Mod_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L236
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L236
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L236:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_i8
	.private_extern _Mod__uMod_param_u64_i8
_Mod__uMod_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_i8
	.private_extern _Mod__Mod_param_u64_i8
_Mod__Mod_param_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L241
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L241
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L241:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_L
	.private_extern _Mod__uMod_var_u64_L
_Mod__uMod_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_L
	.private_extern _Mod__Mod_var_u64_L
_Mod__Mod_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L246
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L246
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L246:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_L
	.private_extern _Mod__uMod_param_u64_L
_Mod__uMod_param_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_L
	.private_extern _Mod__Mod_param_u64_L
_Mod__Mod_param_u64_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L251
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L251
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L251:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u32
	.private_extern _Mod__uMod_var_u64_u32
_Mod__uMod_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u64_u32
	.private_extern _Mod__Mod_var_u64_u32
_Mod__Mod_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L256
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L256
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L256:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u64_u32
	.private_extern _Mod__uMod_param_u64_u32
_Mod__uMod_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u64_u32
	.private_extern _Mod__Mod_param_u64_u32
_Mod__Mod_param_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L261
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L261
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L261:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u16
	.private_extern _Mod__uMod_var_LC_u16
_Mod__uMod_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_u16
	.private_extern _Mod__Mod_var_LC_u16
_Mod__Mod_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L266
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L266
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L266:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_u16
	.private_extern _Mod__uMod_param_LC_u16
_Mod__uMod_param_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_u16
	.private_extern _Mod__Mod_param_LC_u16
_Mod__Mod_param_LC_u16:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L271
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L271
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L271:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u64
	.private_extern _Mod__uMod_var_LC_u64
_Mod__uMod_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_u64
	.private_extern _Mod__Mod_var_LC_u64
_Mod__Mod_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L276
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L276
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L276:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_u64
	.private_extern _Mod__uMod_param_LC_u64
_Mod__uMod_param_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_u64
	.private_extern _Mod__Mod_param_LC_u64
_Mod__Mod_param_LC_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L281
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L281
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L281:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_LC
	.private_extern _Mod__uMod_var_LC_LC
_Mod__uMod_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_LC
	.private_extern _Mod__Mod_var_LC_LC
_Mod__Mod_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L286
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L286
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L286:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_LC
	.private_extern _Mod__uMod_param_LC_LC
_Mod__uMod_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_LC
	.private_extern _Mod__Mod_param_LC_LC
_Mod__Mod_param_LC_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L291
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L291
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L291:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i16
	.private_extern _Mod__uMod_var_LC_i16
_Mod__uMod_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_i16
	.private_extern _Mod__Mod_var_LC_i16
_Mod__Mod_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L296
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L296
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L296:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_i16
	.private_extern _Mod__uMod_param_LC_i16
_Mod__uMod_param_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_i16
	.private_extern _Mod__Mod_param_LC_i16
_Mod__Mod_param_LC_i16:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L301
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L301
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L301:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i32
	.private_extern _Mod__uMod_var_LC_i32
_Mod__uMod_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_i32
	.private_extern _Mod__Mod_var_LC_i32
_Mod__Mod_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L306
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L306
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L306:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_i32
	.private_extern _Mod__uMod_param_LC_i32
_Mod__uMod_param_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_i32
	.private_extern _Mod__Mod_param_LC_i32
_Mod__Mod_param_LC_i32:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L311
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L311
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L311:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_I
	.private_extern _Mod__uMod_var_LC_I
_Mod__uMod_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_I
	.private_extern _Mod__Mod_var_LC_I
_Mod__Mod_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L316
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L316
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L316:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_I
	.private_extern _Mod__uMod_param_LC_I
_Mod__uMod_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_I
	.private_extern _Mod__Mod_param_LC_I
_Mod__Mod_param_LC_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L321
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L321
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L321:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i64
	.private_extern _Mod__uMod_var_LC_i64
_Mod__uMod_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_i64
	.private_extern _Mod__Mod_var_LC_i64
_Mod__Mod_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L326
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L326
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L326:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_i64
	.private_extern _Mod__uMod_param_LC_i64
_Mod__uMod_param_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_i64
	.private_extern _Mod__Mod_param_LC_i64
_Mod__Mod_param_LC_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L331
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L331
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L331:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_C
	.private_extern _Mod__uMod_var_LC_C
_Mod__uMod_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_C
	.private_extern _Mod__Mod_var_LC_C
_Mod__Mod_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L336
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L336
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L336:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_C
	.private_extern _Mod__uMod_param_LC_C
_Mod__uMod_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_C
	.private_extern _Mod__Mod_param_LC_C
_Mod__Mod_param_LC_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L341
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L341
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L341:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u8
	.private_extern _Mod__uMod_var_LC_u8
_Mod__uMod_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_u8
	.private_extern _Mod__Mod_var_LC_u8
_Mod__Mod_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L346
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L346
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L346:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_u8
	.private_extern _Mod__uMod_param_LC_u8
_Mod__uMod_param_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_u8
	.private_extern _Mod__Mod_param_LC_u8
_Mod__Mod_param_LC_u8:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L351
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L351
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L351:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i8
	.private_extern _Mod__uMod_var_LC_i8
_Mod__uMod_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_i8
	.private_extern _Mod__Mod_var_LC_i8
_Mod__Mod_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L356
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L356
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L356:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_i8
	.private_extern _Mod__uMod_param_LC_i8
_Mod__uMod_param_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_i8
	.private_extern _Mod__Mod_param_LC_i8
_Mod__Mod_param_LC_i8:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L361
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L361
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L361:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_L
	.private_extern _Mod__uMod_var_LC_L
_Mod__uMod_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_L
	.private_extern _Mod__Mod_var_LC_L
_Mod__Mod_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L366
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L366
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L366:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_L
	.private_extern _Mod__uMod_param_LC_L
_Mod__uMod_param_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_L
	.private_extern _Mod__Mod_param_LC_L
_Mod__Mod_param_LC_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L371
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L371
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L371:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u32
	.private_extern _Mod__uMod_var_LC_u32
_Mod__uMod_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_LC_u32
	.private_extern _Mod__Mod_var_LC_u32
_Mod__Mod_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L376
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L376
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L376:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_LC_u32
	.private_extern _Mod__uMod_param_LC_u32
_Mod__uMod_param_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_LC_u32
	.private_extern _Mod__Mod_param_LC_u32
_Mod__Mod_param_LC_u32:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L381
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L381
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L381:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__Mod_var_f64_f64
	.private_extern _Mod__Mod_var_f64_f64
_Mod__Mod_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movsd	-16(%rbp), %xmm0
	divsd	-24(%rbp), %xmm0
	movsd	%xmm0, -32(%rbp)
	movsd	-32(%rbp), %xmm0
	cvttsd2siq	%xmm0, %rax
	movq	%rax, -40(%rbp)
	cvtsi2sdq	-40(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	ucomisd	%xmm1, %xmm0
	jb	L389
L388:
	cvtsi2sdq	-40(%rbp), %xmm0
	movsd	%xmm0, -56(%rbp)
	jmp	L386
L389:
	movq	-40(%rbp), %rax
	subq	$1, %rax
	cvtsi2sdq	%rax, %xmm0
	movsd	%xmm0, -56(%rbp)
L386:
	movsd	-56(%rbp), %xmm1
	mulsd	-24(%rbp), %xmm1
	movsd	-16(%rbp), %xmm0
	subsd	%xmm1, %xmm0
	leave
	ret
.globl _Mod__Mod_param_f64_f64
	.private_extern _Mod__Mod_param_f64_f64
_Mod__Mod_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -56(%rbp)
	movsd	%xmm1, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-64(%rbp), %rax
	movq	%rax, -24(%rbp)
	movsd	-16(%rbp), %xmm0
	divsd	-24(%rbp), %xmm0
	movsd	%xmm0, -32(%rbp)
	movsd	-32(%rbp), %xmm0
	cvttsd2siq	%xmm0, %rax
	movq	%rax, -40(%rbp)
	cvtsi2sdq	-40(%rbp), %xmm1
	movsd	-32(%rbp), %xmm0
	ucomisd	%xmm1, %xmm0
	jb	L396
L395:
	cvtsi2sdq	-40(%rbp), %xmm0
	movsd	%xmm0, -72(%rbp)
	jmp	L393
L396:
	movq	-40(%rbp), %rax
	subq	$1, %rax
	cvtsi2sdq	%rax, %xmm0
	movsd	%xmm0, -72(%rbp)
L393:
	movsd	-72(%rbp), %xmm1
	mulsd	-24(%rbp), %xmm1
	movsd	-16(%rbp), %xmm0
	subsd	%xmm1, %xmm0
	leave
	ret
.globl _Mod__uMod_var_i16_u16
	.private_extern _Mod__uMod_var_i16_u16
_Mod__uMod_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_u16
	.private_extern _Mod__Mod_var_i16_u16
_Mod__Mod_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L400
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L400
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L400:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L401
	cmpq	$65535, -16(%rbp)
	jle	L402
L401:
	movl	$6433, %edi
	call	__m3_fault
L402:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_u16
	.private_extern _Mod__uMod_param_i16_u16
_Mod__uMod_param_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_u16
	.private_extern _Mod__Mod_param_i16_u16
_Mod__Mod_param_i16_u16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L407
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L407
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L407:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L408
	cmpq	$65535, -16(%rbp)
	jle	L409
L408:
	movl	$6497, %edi
	call	__m3_fault
L409:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u64
	.private_extern _Mod__uMod_var_i16_u64
_Mod__uMod_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_u64
	.private_extern _Mod__Mod_var_i16_u64
_Mod__Mod_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L414
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L414
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L414:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_u64
	.private_extern _Mod__uMod_param_i16_u64
_Mod__uMod_param_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_u64
	.private_extern _Mod__Mod_param_i16_u64
_Mod__Mod_param_i16_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L419
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L419
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L419:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_LC
	.private_extern _Mod__uMod_var_i16_LC
_Mod__uMod_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_LC
	.private_extern _Mod__Mod_var_i16_LC
_Mod__Mod_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L424
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L424
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L424:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_LC
	.private_extern _Mod__uMod_param_i16_LC
_Mod__uMod_param_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_LC
	.private_extern _Mod__Mod_param_i16_LC
_Mod__Mod_param_i16_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L429
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L429
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L429:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i16
	.private_extern _Mod__uMod_var_i16_i16
_Mod__uMod_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_i16
	.private_extern _Mod__Mod_var_i16_i16
_Mod__Mod_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L434
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L434
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L434:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_i16
	.private_extern _Mod__uMod_param_i16_i16
_Mod__uMod_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_i16
	.private_extern _Mod__Mod_param_i16_i16
_Mod__Mod_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	L439
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L439
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L439:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i32
	.private_extern _Mod__uMod_var_i16_i32
_Mod__uMod_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_i32
	.private_extern _Mod__Mod_var_i16_i32
_Mod__Mod_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L444
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L444
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L444:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L445
	movl	$6945, %edi
	call	__m3_fault
L445:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_i32
	.private_extern _Mod__uMod_param_i16_i32
_Mod__uMod_param_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_i32
	.private_extern _Mod__Mod_param_i16_i32
_Mod__Mod_param_i16_i32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L450
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L450
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L450:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L451
	movl	$7009, %edi
	call	__m3_fault
L451:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_I
	.private_extern _Mod__uMod_var_i16_I
_Mod__uMod_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_I
	.private_extern _Mod__Mod_var_i16_I
_Mod__Mod_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L456
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L456
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L456:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L457
	cmpq	$65535, -16(%rbp)
	jle	L458
L457:
	movl	$7073, %edi
	call	__m3_fault
L458:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_I
	.private_extern _Mod__uMod_param_i16_I
_Mod__uMod_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_I
	.private_extern _Mod__Mod_param_i16_I
_Mod__Mod_param_i16_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L463
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L463
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L463:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L464
	cmpq	$65535, -16(%rbp)
	jle	L465
L464:
	movl	$7137, %edi
	call	__m3_fault
L465:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i64
	.private_extern _Mod__uMod_var_i16_i64
_Mod__uMod_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_i64
	.private_extern _Mod__Mod_var_i16_i64
_Mod__Mod_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L470
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L470
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L470:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_i64
	.private_extern _Mod__uMod_param_i16_i64
_Mod__uMod_param_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_i64
	.private_extern _Mod__Mod_param_i16_i64
_Mod__Mod_param_i16_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L475
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L475
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L475:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_C
	.private_extern _Mod__uMod_var_i16_C
_Mod__uMod_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_C
	.private_extern _Mod__Mod_var_i16_C
_Mod__Mod_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L480
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L480
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L480:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L481
	movl	$7329, %edi
	call	__m3_fault
L481:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_C
	.private_extern _Mod__uMod_param_i16_C
_Mod__uMod_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_C
	.private_extern _Mod__Mod_param_i16_C
_Mod__Mod_param_i16_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L486
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L486
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L486:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L487
	movl	$7393, %edi
	call	__m3_fault
L487:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u8
	.private_extern _Mod__uMod_var_i16_u8
_Mod__uMod_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_u8
	.private_extern _Mod__Mod_var_i16_u8
_Mod__Mod_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L492
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L492
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L492:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L493
	cmpq	$65535, -16(%rbp)
	jle	L494
L493:
	movl	$7457, %edi
	call	__m3_fault
L494:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_u8
	.private_extern _Mod__uMod_param_i16_u8
_Mod__uMod_param_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_u8
	.private_extern _Mod__Mod_param_i16_u8
_Mod__Mod_param_i16_u8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L499
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L499
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L499:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L500
	cmpq	$65535, -16(%rbp)
	jle	L501
L500:
	movl	$7521, %edi
	call	__m3_fault
L501:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i8
	.private_extern _Mod__uMod_var_i16_i8
_Mod__uMod_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_i8
	.private_extern _Mod__Mod_var_i16_i8
_Mod__Mod_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L506
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L506
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L506:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_i8
	.private_extern _Mod__uMod_param_i16_i8
_Mod__uMod_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_i8
	.private_extern _Mod__Mod_param_i16_i8
_Mod__Mod_param_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	L511
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L511
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L511:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_L
	.private_extern _Mod__uMod_var_i16_L
_Mod__uMod_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_L
	.private_extern _Mod__Mod_var_i16_L
_Mod__Mod_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L516
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L516
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L516:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_L
	.private_extern _Mod__uMod_param_i16_L
_Mod__uMod_param_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_L
	.private_extern _Mod__Mod_param_i16_L
_Mod__Mod_param_i16_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L521
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L521
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L521:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u32
	.private_extern _Mod__uMod_var_i16_u32
_Mod__uMod_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i16_u32
	.private_extern _Mod__Mod_var_i16_u32
_Mod__Mod_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L526
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L526
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L526:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L527
	cmpq	$65535, -16(%rbp)
	jle	L528
L527:
	movl	$7841, %edi
	call	__m3_fault
L528:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i16_u32
	.private_extern _Mod__uMod_param_i16_u32
_Mod__uMod_param_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i16_u32
	.private_extern _Mod__Mod_param_i16_u32
_Mod__Mod_param_i16_u32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L533
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L533
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L533:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L534
	cmpq	$65535, -16(%rbp)
	jle	L535
L534:
	movl	$7905, %edi
	call	__m3_fault
L535:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u16
	.private_extern _Mod__uMod_var_i32_u16
_Mod__uMod_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_u16
	.private_extern _Mod__Mod_var_i32_u16
_Mod__Mod_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L540
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L540
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L540:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L541
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L542
L541:
	movl	$7969, %edi
	call	__m3_fault
L542:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_u16
	.private_extern _Mod__uMod_param_i32_u16
_Mod__uMod_param_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_u16
	.private_extern _Mod__Mod_param_i32_u16
_Mod__Mod_param_i32_u16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L547
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L547
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L547:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L548
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L549
L548:
	movl	$8033, %edi
	call	__m3_fault
L549:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u64
	.private_extern _Mod__uMod_var_i32_u64
_Mod__uMod_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_u64
	.private_extern _Mod__Mod_var_i32_u64
_Mod__Mod_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L554
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L554
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L554:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_u64
	.private_extern _Mod__uMod_param_i32_u64
_Mod__uMod_param_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_u64
	.private_extern _Mod__Mod_param_i32_u64
_Mod__Mod_param_i32_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L559
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L559
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L559:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_LC
	.private_extern _Mod__uMod_var_i32_LC
_Mod__uMod_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_LC
	.private_extern _Mod__Mod_var_i32_LC
_Mod__Mod_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L564
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L564
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L564:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_LC
	.private_extern _Mod__uMod_param_i32_LC
_Mod__uMod_param_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_LC
	.private_extern _Mod__Mod_param_i32_LC
_Mod__Mod_param_i32_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L569
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L569
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L569:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i16
	.private_extern _Mod__uMod_var_i32_i16
_Mod__uMod_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_i16
	.private_extern _Mod__Mod_var_i32_i16
_Mod__Mod_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L574
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L574
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L574:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_i16
	.private_extern _Mod__uMod_param_i32_i16
_Mod__uMod_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_i16
	.private_extern _Mod__Mod_param_i32_i16
_Mod__Mod_param_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzwl	-22(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	L579
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L579
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L579:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i32
	.private_extern _Mod__uMod_var_i32_i32
_Mod__uMod_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_i32
	.private_extern _Mod__Mod_var_i32_i32
_Mod__Mod_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L584
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L584
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L584:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_i32
	.private_extern _Mod__uMod_param_i32_i32
_Mod__uMod_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_i32
	.private_extern _Mod__Mod_param_i32_i32
_Mod__Mod_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	L589
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L589
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L589:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_I
	.private_extern _Mod__uMod_var_i32_I
_Mod__uMod_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_I
	.private_extern _Mod__Mod_var_i32_I
_Mod__Mod_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L594
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L594
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L594:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L595
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L596
L595:
	movl	$8609, %edi
	call	__m3_fault
L596:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_I
	.private_extern _Mod__uMod_param_i32_I
_Mod__uMod_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_I
	.private_extern _Mod__Mod_param_i32_I
_Mod__Mod_param_i32_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L601
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L601
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L601:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L602
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L603
L602:
	movl	$8673, %edi
	call	__m3_fault
L603:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i64
	.private_extern _Mod__uMod_var_i32_i64
_Mod__uMod_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_i64
	.private_extern _Mod__Mod_var_i32_i64
_Mod__Mod_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L608
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L608
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L608:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_i64
	.private_extern _Mod__uMod_param_i32_i64
_Mod__uMod_param_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_i64
	.private_extern _Mod__Mod_param_i32_i64
_Mod__Mod_param_i32_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L613
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L613
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L613:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_C
	.private_extern _Mod__uMod_var_i32_C
_Mod__uMod_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_C
	.private_extern _Mod__Mod_var_i32_C
_Mod__Mod_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L618
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L618
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L618:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L619
	movl	$8865, %edi
	call	__m3_fault
L619:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_C
	.private_extern _Mod__uMod_param_i32_C
_Mod__uMod_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_C
	.private_extern _Mod__Mod_param_i32_C
_Mod__Mod_param_i32_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L624
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L624
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L624:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L625
	movl	$8929, %edi
	call	__m3_fault
L625:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u8
	.private_extern _Mod__uMod_var_i32_u8
_Mod__uMod_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_u8
	.private_extern _Mod__Mod_var_i32_u8
_Mod__Mod_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L630
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L630
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L630:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L631
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L632
L631:
	movl	$8993, %edi
	call	__m3_fault
L632:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_u8
	.private_extern _Mod__uMod_param_i32_u8
_Mod__uMod_param_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_u8
	.private_extern _Mod__Mod_param_i32_u8
_Mod__Mod_param_i32_u8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L637
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L637
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L637:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L638
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L639
L638:
	movl	$9057, %edi
	call	__m3_fault
L639:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i8
	.private_extern _Mod__uMod_var_i32_i8
_Mod__uMod_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_i8
	.private_extern _Mod__Mod_var_i32_i8
_Mod__Mod_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L644
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L644
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L644:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_i8
	.private_extern _Mod__uMod_param_i32_i8
_Mod__uMod_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_i8
	.private_extern _Mod__Mod_param_i32_i8
_Mod__Mod_param_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzbl	-21(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	L649
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L649
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L649:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_L
	.private_extern _Mod__uMod_var_i32_L
_Mod__uMod_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_L
	.private_extern _Mod__Mod_var_i32_L
_Mod__Mod_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L654
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L654
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L654:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_L
	.private_extern _Mod__uMod_param_i32_L
_Mod__uMod_param_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_L
	.private_extern _Mod__Mod_param_i32_L
_Mod__Mod_param_i32_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L659
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L659
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L659:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u32
	.private_extern _Mod__uMod_var_i32_u32
_Mod__uMod_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i32_u32
	.private_extern _Mod__Mod_var_i32_u32
_Mod__Mod_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L664
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L664
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L664:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L665
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L666
L665:
	movl	$9377, %edi
	call	__m3_fault
L666:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i32_u32
	.private_extern _Mod__uMod_param_i32_u32
_Mod__uMod_param_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i32_u32
	.private_extern _Mod__Mod_param_i32_u32
_Mod__Mod_param_i32_u32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L671
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L671
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L671:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L672
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L673
L672:
	movl	$9441, %edi
	call	__m3_fault
L673:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u16
	.private_extern _Mod__uMod_var_I_u16
_Mod__uMod_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_u16
	.private_extern _Mod__Mod_var_I_u16
_Mod__Mod_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L678
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L678
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L678:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_u16
	.private_extern _Mod__uMod_param_I_u16
_Mod__uMod_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_u16
	.private_extern _Mod__Mod_param_I_u16
_Mod__Mod_param_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L683
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L683
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L683:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u64
	.private_extern _Mod__uMod_var_I_u64
_Mod__uMod_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_u64
	.private_extern _Mod__Mod_var_I_u64
_Mod__Mod_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L688
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L688
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L688:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_u64
	.private_extern _Mod__uMod_param_I_u64
_Mod__uMod_param_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_u64
	.private_extern _Mod__Mod_param_I_u64
_Mod__Mod_param_I_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L693
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L693
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L693:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_LC
	.private_extern _Mod__uMod_var_I_LC
_Mod__uMod_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_LC
	.private_extern _Mod__Mod_var_I_LC
_Mod__Mod_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L698
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L698
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L698:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_LC
	.private_extern _Mod__uMod_param_I_LC
_Mod__uMod_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_LC
	.private_extern _Mod__Mod_param_I_LC
_Mod__Mod_param_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L703
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L703
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L703:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i16
	.private_extern _Mod__uMod_var_I_i16
_Mod__uMod_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_i16
	.private_extern _Mod__Mod_var_I_i16
_Mod__Mod_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L708
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L708
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L708:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_i16
	.private_extern _Mod__uMod_param_I_i16
_Mod__uMod_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_i16
	.private_extern _Mod__Mod_param_I_i16
_Mod__Mod_param_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L713
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L713
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L713:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i32
	.private_extern _Mod__uMod_var_I_i32
_Mod__uMod_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_i32
	.private_extern _Mod__Mod_var_I_i32
_Mod__Mod_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L718
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L718
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L718:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_i32
	.private_extern _Mod__uMod_param_I_i32
_Mod__uMod_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_i32
	.private_extern _Mod__Mod_param_I_i32
_Mod__Mod_param_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L723
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L723
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L723:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_I
	.private_extern _Mod__uMod_var_I_I
_Mod__uMod_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_I
	.private_extern _Mod__Mod_var_I_I
_Mod__Mod_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L728
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L728
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L728:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_I
	.private_extern _Mod__uMod_param_I_I
_Mod__uMod_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_I
	.private_extern _Mod__Mod_param_I_I
_Mod__Mod_param_I_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L733
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L733
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L733:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i64
	.private_extern _Mod__uMod_var_I_i64
_Mod__uMod_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_i64
	.private_extern _Mod__Mod_var_I_i64
_Mod__Mod_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L738
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L738
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L738:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_i64
	.private_extern _Mod__uMod_param_I_i64
_Mod__uMod_param_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_i64
	.private_extern _Mod__Mod_param_I_i64
_Mod__Mod_param_I_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L743
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L743
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L743:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_C
	.private_extern _Mod__uMod_var_I_C
_Mod__uMod_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_C
	.private_extern _Mod__Mod_var_I_C
_Mod__Mod_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L748
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L748
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L748:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_C
	.private_extern _Mod__uMod_param_I_C
_Mod__uMod_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_C
	.private_extern _Mod__Mod_param_I_C
_Mod__Mod_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L753
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L753
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L753:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u8
	.private_extern _Mod__uMod_var_I_u8
_Mod__uMod_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_u8
	.private_extern _Mod__Mod_var_I_u8
_Mod__Mod_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L758
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L758
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L758:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_u8
	.private_extern _Mod__uMod_param_I_u8
_Mod__uMod_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_u8
	.private_extern _Mod__Mod_param_I_u8
_Mod__Mod_param_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L763
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L763
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L763:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i8
	.private_extern _Mod__uMod_var_I_i8
_Mod__uMod_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_i8
	.private_extern _Mod__Mod_var_I_i8
_Mod__Mod_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L768
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L768
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L768:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_i8
	.private_extern _Mod__uMod_param_I_i8
_Mod__uMod_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_i8
	.private_extern _Mod__Mod_param_I_i8
_Mod__Mod_param_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L773
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L773
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L773:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_L
	.private_extern _Mod__uMod_var_I_L
_Mod__uMod_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_L
	.private_extern _Mod__Mod_var_I_L
_Mod__Mod_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L778
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L778
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L778:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_L
	.private_extern _Mod__uMod_param_I_L
_Mod__uMod_param_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_L
	.private_extern _Mod__Mod_param_I_L
_Mod__Mod_param_I_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L783
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L783
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L783:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u32
	.private_extern _Mod__uMod_var_I_u32
_Mod__uMod_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_I_u32
	.private_extern _Mod__Mod_var_I_u32
_Mod__Mod_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L788
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L788
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L788:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_I_u32
	.private_extern _Mod__uMod_param_I_u32
_Mod__uMod_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_I_u32
	.private_extern _Mod__Mod_param_I_u32
_Mod__Mod_param_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L793
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L793
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L793:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u16
	.private_extern _Mod__uMod_var_i64_u16
_Mod__uMod_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_u16
	.private_extern _Mod__Mod_var_i64_u16
_Mod__Mod_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L798
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L798
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L798:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_u16
	.private_extern _Mod__uMod_param_i64_u16
_Mod__uMod_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_u16
	.private_extern _Mod__Mod_param_i64_u16
_Mod__Mod_param_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L803
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L803
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L803:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u64
	.private_extern _Mod__uMod_var_i64_u64
_Mod__uMod_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_u64
	.private_extern _Mod__Mod_var_i64_u64
_Mod__Mod_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L808
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L808
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L808:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_u64
	.private_extern _Mod__uMod_param_i64_u64
_Mod__uMod_param_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_u64
	.private_extern _Mod__Mod_param_i64_u64
_Mod__Mod_param_i64_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L813
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L813
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L813:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_LC
	.private_extern _Mod__uMod_var_i64_LC
_Mod__uMod_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_LC
	.private_extern _Mod__Mod_var_i64_LC
_Mod__Mod_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L818
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L818
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L818:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_LC
	.private_extern _Mod__uMod_param_i64_LC
_Mod__uMod_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_LC
	.private_extern _Mod__Mod_param_i64_LC
_Mod__Mod_param_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L823
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L823
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L823:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i16
	.private_extern _Mod__uMod_var_i64_i16
_Mod__uMod_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_i16
	.private_extern _Mod__Mod_var_i64_i16
_Mod__Mod_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L828
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L828
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L828:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_i16
	.private_extern _Mod__uMod_param_i64_i16
_Mod__uMod_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_i16
	.private_extern _Mod__Mod_param_i64_i16
_Mod__Mod_param_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L833
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L833
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L833:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i32
	.private_extern _Mod__uMod_var_i64_i32
_Mod__uMod_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_i32
	.private_extern _Mod__Mod_var_i64_i32
_Mod__Mod_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L838
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L838
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L838:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_i32
	.private_extern _Mod__uMod_param_i64_i32
_Mod__uMod_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_i32
	.private_extern _Mod__Mod_param_i64_i32
_Mod__Mod_param_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L843
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L843
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L843:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_I
	.private_extern _Mod__uMod_var_i64_I
_Mod__uMod_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_I
	.private_extern _Mod__Mod_var_i64_I
_Mod__Mod_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L848
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L848
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L848:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_I
	.private_extern _Mod__uMod_param_i64_I
_Mod__uMod_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_I
	.private_extern _Mod__Mod_param_i64_I
_Mod__Mod_param_i64_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L853
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L853
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L853:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i64
	.private_extern _Mod__uMod_var_i64_i64
_Mod__uMod_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_i64
	.private_extern _Mod__Mod_var_i64_i64
_Mod__Mod_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L858
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L858
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L858:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_i64
	.private_extern _Mod__uMod_param_i64_i64
_Mod__uMod_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_i64
	.private_extern _Mod__Mod_param_i64_i64
_Mod__Mod_param_i64_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L863
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L863
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L863:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_C
	.private_extern _Mod__uMod_var_i64_C
_Mod__uMod_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_C
	.private_extern _Mod__Mod_var_i64_C
_Mod__Mod_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L868
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L868
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L868:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_C
	.private_extern _Mod__uMod_param_i64_C
_Mod__uMod_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_C
	.private_extern _Mod__Mod_param_i64_C
_Mod__Mod_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L873
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L873
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L873:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u8
	.private_extern _Mod__uMod_var_i64_u8
_Mod__uMod_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_u8
	.private_extern _Mod__Mod_var_i64_u8
_Mod__Mod_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L878
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L878
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L878:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_u8
	.private_extern _Mod__uMod_param_i64_u8
_Mod__uMod_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_u8
	.private_extern _Mod__Mod_param_i64_u8
_Mod__Mod_param_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L883
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L883
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L883:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i8
	.private_extern _Mod__uMod_var_i64_i8
_Mod__uMod_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_i8
	.private_extern _Mod__Mod_var_i64_i8
_Mod__Mod_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L888
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L888
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L888:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_i8
	.private_extern _Mod__uMod_param_i64_i8
_Mod__uMod_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_i8
	.private_extern _Mod__Mod_param_i64_i8
_Mod__Mod_param_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L893
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L893
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L893:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_L
	.private_extern _Mod__uMod_var_i64_L
_Mod__uMod_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_L
	.private_extern _Mod__Mod_var_i64_L
_Mod__Mod_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L898
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L898
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L898:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_L
	.private_extern _Mod__uMod_param_i64_L
_Mod__uMod_param_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_L
	.private_extern _Mod__Mod_param_i64_L
_Mod__Mod_param_i64_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L903
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L903
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L903:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u32
	.private_extern _Mod__uMod_var_i64_u32
_Mod__uMod_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i64_u32
	.private_extern _Mod__Mod_var_i64_u32
_Mod__Mod_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L908
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L908
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L908:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i64_u32
	.private_extern _Mod__uMod_param_i64_u32
_Mod__uMod_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i64_u32
	.private_extern _Mod__Mod_param_i64_u32
_Mod__Mod_param_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L913
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L913
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L913:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u16
	.private_extern _Mod__uMod_var_C_u16
_Mod__uMod_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_u16
	.private_extern _Mod__Mod_var_C_u16
_Mod__Mod_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L918
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L918
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L918:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L919
	movl	$12577, %edi
	call	__m3_fault
L919:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_u16
	.private_extern _Mod__uMod_param_C_u16
_Mod__uMod_param_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_u16
	.private_extern _Mod__Mod_param_C_u16
_Mod__Mod_param_C_u16:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L924
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L924
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L924:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L925
	movl	$12641, %edi
	call	__m3_fault
L925:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u64
	.private_extern _Mod__uMod_var_C_u64
_Mod__uMod_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_u64
	.private_extern _Mod__Mod_var_C_u64
_Mod__Mod_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L930
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L930
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L930:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_u64
	.private_extern _Mod__uMod_param_C_u64
_Mod__uMod_param_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_u64
	.private_extern _Mod__Mod_param_C_u64
_Mod__Mod_param_C_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L935
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L935
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L935:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_LC
	.private_extern _Mod__uMod_var_C_LC
_Mod__uMod_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_LC
	.private_extern _Mod__Mod_var_C_LC
_Mod__Mod_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L940
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L940
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L940:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_LC
	.private_extern _Mod__uMod_param_C_LC
_Mod__uMod_param_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_LC
	.private_extern _Mod__Mod_param_C_LC
_Mod__Mod_param_C_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L945
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L945
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L945:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i16
	.private_extern _Mod__uMod_var_C_i16
_Mod__uMod_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_i16
	.private_extern _Mod__Mod_var_C_i16
_Mod__Mod_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L950
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L950
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L950:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_i16
	.private_extern _Mod__uMod_param_C_i16
_Mod__uMod_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_i16
	.private_extern _Mod__Mod_param_C_i16
_Mod__Mod_param_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movzwl	-26(%rbp), %edx
	movq	%rdx, -48(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-48(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L955
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L955
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L955:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i32
	.private_extern _Mod__uMod_var_C_i32
_Mod__uMod_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_i32
	.private_extern _Mod__Mod_var_C_i32
_Mod__Mod_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L960
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L960
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L960:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_i32
	.private_extern _Mod__uMod_param_C_i32
_Mod__uMod_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_i32
	.private_extern _Mod__Mod_param_C_i32
_Mod__Mod_param_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	mov	-28(%rbp), %edx
	movq	%rdx, -48(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-48(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L965
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L965
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L965:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_I
	.private_extern _Mod__uMod_var_C_I
_Mod__uMod_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_I
	.private_extern _Mod__Mod_var_C_I
_Mod__Mod_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L970
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L970
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L970:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L971
	movl	$13217, %edi
	call	__m3_fault
L971:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_I
	.private_extern _Mod__uMod_param_C_I
_Mod__uMod_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_I
	.private_extern _Mod__Mod_param_C_I
_Mod__Mod_param_C_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L976
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L976
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L976:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L977
	movl	$13281, %edi
	call	__m3_fault
L977:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i64
	.private_extern _Mod__uMod_var_C_i64
_Mod__uMod_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_i64
	.private_extern _Mod__Mod_var_C_i64
_Mod__Mod_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L982
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L982
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L982:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_i64
	.private_extern _Mod__uMod_param_C_i64
_Mod__uMod_param_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_i64
	.private_extern _Mod__Mod_param_C_i64
_Mod__Mod_param_C_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L987
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L987
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L987:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_C
	.private_extern _Mod__uMod_var_C_C
_Mod__uMod_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_C
	.private_extern _Mod__Mod_var_C_C
_Mod__Mod_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L992
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L992
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L992:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_C
	.private_extern _Mod__uMod_param_C_C
_Mod__uMod_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_C
	.private_extern _Mod__Mod_param_C_C
_Mod__Mod_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-48(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L997
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L997
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L997:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u8
	.private_extern _Mod__uMod_var_C_u8
_Mod__uMod_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_u8
	.private_extern _Mod__Mod_var_C_u8
_Mod__Mod_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1002
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1002
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1002:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1003
	movl	$13601, %edi
	call	__m3_fault
L1003:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_u8
	.private_extern _Mod__uMod_param_C_u8
_Mod__uMod_param_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_u8
	.private_extern _Mod__Mod_param_C_u8
_Mod__Mod_param_C_u8:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1008
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1008
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1008:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1009
	movl	$13665, %edi
	call	__m3_fault
L1009:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i8
	.private_extern _Mod__uMod_var_C_i8
_Mod__uMod_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_i8
	.private_extern _Mod__Mod_var_C_i8
_Mod__Mod_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1014
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1014
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1014:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_i8
	.private_extern _Mod__uMod_param_C_i8
_Mod__uMod_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_i8
	.private_extern _Mod__Mod_param_C_i8
_Mod__Mod_param_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movzbl	-25(%rbp), %edx
	movq	%rdx, -48(%rbp)
	movq	-56(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-48(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1019
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L1019
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L1019:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_L
	.private_extern _Mod__uMod_var_C_L
_Mod__uMod_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_L
	.private_extern _Mod__Mod_var_C_L
_Mod__Mod_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1024
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1024
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1024:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_L
	.private_extern _Mod__uMod_param_C_L
_Mod__uMod_param_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_L
	.private_extern _Mod__Mod_param_C_L
_Mod__Mod_param_C_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1029
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1029
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1029:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u32
	.private_extern _Mod__uMod_var_C_u32
_Mod__uMod_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_C_u32
	.private_extern _Mod__Mod_var_C_u32
_Mod__Mod_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1034
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1034
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1034:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1035
	movl	$13985, %edi
	call	__m3_fault
L1035:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_C_u32
	.private_extern _Mod__uMod_param_C_u32
_Mod__uMod_param_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_C_u32
	.private_extern _Mod__Mod_param_C_u32
_Mod__Mod_param_C_u32:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1040
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1040
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1040:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1041
	movl	$14049, %edi
	call	__m3_fault
L1041:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__Mod_var_f32_f32
	.private_extern _Mod__Mod_var_f32_f32
_Mod__Mod_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	168+_MM_Mod(%rip), %eax
	movl	%eax, -8(%rbp)
	movl	168+_MM_Mod(%rip), %eax
	movl	%eax, -12(%rbp)
	movss	-8(%rbp), %xmm0
	divss	-12(%rbp), %xmm0
	movss	%xmm0, -16(%rbp)
	movss	-16(%rbp), %xmm0
	cvttss2siq	%xmm0, %rax
	movq	%rax, -24(%rbp)
	cvtsi2ssq	-24(%rbp), %xmm1
	movss	-16(%rbp), %xmm0
	ucomiss	%xmm1, %xmm0
	jb	L1049
L1048:
	cvtsi2ssq	-24(%rbp), %xmm0
	movss	%xmm0, -36(%rbp)
	jmp	L1046
L1049:
	movq	-24(%rbp), %rax
	subq	$1, %rax
	cvtsi2ssq	%rax, %xmm0
	movss	%xmm0, -36(%rbp)
L1046:
	movss	-36(%rbp), %xmm1
	mulss	-12(%rbp), %xmm1
	movss	-8(%rbp), %xmm0
	subss	%xmm1, %xmm0
	leave
	ret
.globl _Mod__Mod_param_f32_f32
	.private_extern _Mod__Mod_param_f32_f32
_Mod__Mod_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -36(%rbp)
	movss	%xmm1, -40(%rbp)
	movl	-36(%rbp), %eax
	movl	%eax, -8(%rbp)
	movl	-40(%rbp), %eax
	movl	%eax, -12(%rbp)
	movss	-8(%rbp), %xmm0
	divss	-12(%rbp), %xmm0
	movss	%xmm0, -16(%rbp)
	movss	-16(%rbp), %xmm0
	cvttss2siq	%xmm0, %rax
	movq	%rax, -24(%rbp)
	cvtsi2ssq	-24(%rbp), %xmm1
	movss	-16(%rbp), %xmm0
	ucomiss	%xmm1, %xmm0
	jb	L1056
L1055:
	cvtsi2ssq	-24(%rbp), %xmm0
	movss	%xmm0, -44(%rbp)
	jmp	L1053
L1056:
	movq	-24(%rbp), %rax
	subq	$1, %rax
	cvtsi2ssq	%rax, %xmm0
	movss	%xmm0, -44(%rbp)
L1053:
	movss	-44(%rbp), %xmm1
	mulss	-12(%rbp), %xmm1
	movss	-8(%rbp), %xmm0
	subss	%xmm1, %xmm0
	leave
	ret
.globl _Mod__uMod_var_u8_u16
	.private_extern _Mod__uMod_var_u8_u16
_Mod__uMod_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_u16
	.private_extern _Mod__Mod_var_u8_u16
_Mod__Mod_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1060
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1060
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1060:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1061
	cmpq	$127, -16(%rbp)
	jle	L1062
L1061:
	movl	$14177, %edi
	call	__m3_fault
L1062:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_u16
	.private_extern _Mod__uMod_param_u8_u16
_Mod__uMod_param_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_u16
	.private_extern _Mod__Mod_param_u8_u16
_Mod__Mod_param_u8_u16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1067
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1067
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1067:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1068
	cmpq	$127, -16(%rbp)
	jle	L1069
L1068:
	movl	$14241, %edi
	call	__m3_fault
L1069:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u64
	.private_extern _Mod__uMod_var_u8_u64
_Mod__uMod_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_u64
	.private_extern _Mod__Mod_var_u8_u64
_Mod__Mod_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1074
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1074
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1074:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_u64
	.private_extern _Mod__uMod_param_u8_u64
_Mod__uMod_param_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_u64
	.private_extern _Mod__Mod_param_u8_u64
_Mod__Mod_param_u8_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1079
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1079
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1079:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_LC
	.private_extern _Mod__uMod_var_u8_LC
_Mod__uMod_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_LC
	.private_extern _Mod__Mod_var_u8_LC
_Mod__Mod_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1084
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1084
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1084:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_LC
	.private_extern _Mod__uMod_param_u8_LC
_Mod__uMod_param_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_LC
	.private_extern _Mod__Mod_param_u8_LC
_Mod__Mod_param_u8_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1089
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1089
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1089:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i16
	.private_extern _Mod__uMod_var_u8_i16
_Mod__uMod_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_i16
	.private_extern _Mod__Mod_var_u8_i16
_Mod__Mod_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1094
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1094
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1094:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1095
	movl	$14561, %edi
	call	__m3_fault
L1095:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_i16
	.private_extern _Mod__uMod_param_u8_i16
_Mod__uMod_param_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_i16
	.private_extern _Mod__Mod_param_u8_i16
_Mod__Mod_param_u8_i16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1100
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1100
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1100:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1101
	movl	$14625, %edi
	call	__m3_fault
L1101:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i32
	.private_extern _Mod__uMod_var_u8_i32
_Mod__uMod_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_i32
	.private_extern _Mod__Mod_var_u8_i32
_Mod__Mod_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1106
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1106
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1106:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1107
	movl	$14689, %edi
	call	__m3_fault
L1107:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_i32
	.private_extern _Mod__uMod_param_u8_i32
_Mod__uMod_param_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_i32
	.private_extern _Mod__Mod_param_u8_i32
_Mod__Mod_param_u8_i32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1112
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1112
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1112:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1113
	movl	$14753, %edi
	call	__m3_fault
L1113:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_I
	.private_extern _Mod__uMod_var_u8_I
_Mod__uMod_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_I
	.private_extern _Mod__Mod_var_u8_I
_Mod__Mod_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1118
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1118
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1118:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1119
	cmpq	$127, -16(%rbp)
	jle	L1120
L1119:
	movl	$14817, %edi
	call	__m3_fault
L1120:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_I
	.private_extern _Mod__uMod_param_u8_I
_Mod__uMod_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_I
	.private_extern _Mod__Mod_param_u8_I
_Mod__Mod_param_u8_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1125
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1125
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1125:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1126
	cmpq	$127, -16(%rbp)
	jle	L1127
L1126:
	movl	$14881, %edi
	call	__m3_fault
L1127:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i64
	.private_extern _Mod__uMod_var_u8_i64
_Mod__uMod_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_i64
	.private_extern _Mod__Mod_var_u8_i64
_Mod__Mod_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1132
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1132
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1132:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_i64
	.private_extern _Mod__uMod_param_u8_i64
_Mod__uMod_param_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_i64
	.private_extern _Mod__Mod_param_u8_i64
_Mod__Mod_param_u8_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1137
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1137
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1137:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_C
	.private_extern _Mod__uMod_var_u8_C
_Mod__uMod_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_C
	.private_extern _Mod__Mod_var_u8_C
_Mod__Mod_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1142
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1142
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1142:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1143
	movl	$15073, %edi
	call	__m3_fault
L1143:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_C
	.private_extern _Mod__uMod_param_u8_C
_Mod__uMod_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_C
	.private_extern _Mod__Mod_param_u8_C
_Mod__Mod_param_u8_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1148
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1148
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1148:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1149
	movl	$15137, %edi
	call	__m3_fault
L1149:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u8
	.private_extern _Mod__uMod_var_u8_u8
_Mod__uMod_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_u8
	.private_extern _Mod__Mod_var_u8_u8
_Mod__Mod_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1154
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1154
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1154:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1155
	cmpq	$127, -16(%rbp)
	jle	L1156
L1155:
	movl	$15201, %edi
	call	__m3_fault
L1156:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_u8
	.private_extern _Mod__uMod_param_u8_u8
_Mod__uMod_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_u8
	.private_extern _Mod__Mod_param_u8_u8
_Mod__Mod_param_u8_u8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1161
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1161
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1161:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1162
	cmpq	$127, -16(%rbp)
	jle	L1163
L1162:
	movl	$15265, %edi
	call	__m3_fault
L1163:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i8
	.private_extern _Mod__uMod_var_u8_i8
_Mod__uMod_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_i8
	.private_extern _Mod__Mod_var_u8_i8
_Mod__Mod_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1168
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1168
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1168:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1169
	movl	$15329, %edi
	call	__m3_fault
L1169:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_i8
	.private_extern _Mod__uMod_param_u8_i8
_Mod__uMod_param_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_i8
	.private_extern _Mod__Mod_param_u8_i8
_Mod__Mod_param_u8_i8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1174
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1174
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1174:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L1175
	movl	$15393, %edi
	call	__m3_fault
L1175:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_L
	.private_extern _Mod__uMod_var_u8_L
_Mod__uMod_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_L
	.private_extern _Mod__Mod_var_u8_L
_Mod__Mod_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1180
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1180
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1180:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_L
	.private_extern _Mod__uMod_param_u8_L
_Mod__uMod_param_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_L
	.private_extern _Mod__Mod_param_u8_L
_Mod__Mod_param_u8_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1185
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1185
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1185:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u32
	.private_extern _Mod__uMod_var_u8_u32
_Mod__uMod_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u8_u32
	.private_extern _Mod__Mod_var_u8_u32
_Mod__Mod_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1190
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1190
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1190:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1191
	cmpq	$127, -16(%rbp)
	jle	L1192
L1191:
	movl	$15585, %edi
	call	__m3_fault
L1192:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u8_u32
	.private_extern _Mod__uMod_param_u8_u32
_Mod__uMod_param_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u8_u32
	.private_extern _Mod__Mod_param_u8_u32
_Mod__Mod_param_u8_u32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1197
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1197
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1197:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L1198
	cmpq	$127, -16(%rbp)
	jle	L1199
L1198:
	movl	$15649, %edi
	call	__m3_fault
L1199:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u16
	.private_extern _Mod__uMod_var_i8_u16
_Mod__uMod_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_u16
	.private_extern _Mod__Mod_var_i8_u16
_Mod__Mod_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1204
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1204
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1204:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1205
	cmpq	$255, -16(%rbp)
	jle	L1206
L1205:
	movl	$15713, %edi
	call	__m3_fault
L1206:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_u16
	.private_extern _Mod__uMod_param_i8_u16
_Mod__uMod_param_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_u16
	.private_extern _Mod__Mod_param_i8_u16
_Mod__Mod_param_i8_u16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1211
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1211
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1211:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1212
	cmpq	$255, -16(%rbp)
	jle	L1213
L1212:
	movl	$15777, %edi
	call	__m3_fault
L1213:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u64
	.private_extern _Mod__uMod_var_i8_u64
_Mod__uMod_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_u64
	.private_extern _Mod__Mod_var_i8_u64
_Mod__Mod_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1218
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1218
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1218:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_u64
	.private_extern _Mod__uMod_param_i8_u64
_Mod__uMod_param_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_u64
	.private_extern _Mod__Mod_param_i8_u64
_Mod__Mod_param_i8_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1223
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1223
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1223:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_LC
	.private_extern _Mod__uMod_var_i8_LC
_Mod__uMod_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_LC
	.private_extern _Mod__Mod_var_i8_LC
_Mod__Mod_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1228
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1228
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1228:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_LC
	.private_extern _Mod__uMod_param_i8_LC
_Mod__uMod_param_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_LC
	.private_extern _Mod__Mod_param_i8_LC
_Mod__Mod_param_i8_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1233
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1233
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1233:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i16
	.private_extern _Mod__uMod_var_i8_i16
_Mod__uMod_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_i16
	.private_extern _Mod__Mod_var_i8_i16
_Mod__Mod_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1238
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1238
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1238:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1239
	movl	$16097, %edi
	call	__m3_fault
L1239:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_i16
	.private_extern _Mod__uMod_param_i8_i16
_Mod__uMod_param_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_i16
	.private_extern _Mod__Mod_param_i8_i16
_Mod__Mod_param_i8_i16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1244
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1244
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1244:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1245
	movl	$16161, %edi
	call	__m3_fault
L1245:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i32
	.private_extern _Mod__uMod_var_i8_i32
_Mod__uMod_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_i32
	.private_extern _Mod__Mod_var_i8_i32
_Mod__Mod_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1250
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1250
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1250:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1251
	movl	$16225, %edi
	call	__m3_fault
L1251:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_i32
	.private_extern _Mod__uMod_param_i8_i32
_Mod__uMod_param_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_i32
	.private_extern _Mod__Mod_param_i8_i32
_Mod__Mod_param_i8_i32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1256
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1256
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1256:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1257
	movl	$16289, %edi
	call	__m3_fault
L1257:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_I
	.private_extern _Mod__uMod_var_i8_I
_Mod__uMod_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_I
	.private_extern _Mod__Mod_var_i8_I
_Mod__Mod_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1262
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1262
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1262:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1263
	cmpq	$255, -16(%rbp)
	jle	L1264
L1263:
	movl	$16353, %edi
	call	__m3_fault
L1264:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_I
	.private_extern _Mod__uMod_param_i8_I
_Mod__uMod_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_I
	.private_extern _Mod__Mod_param_i8_I
_Mod__Mod_param_i8_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1269
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1269
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1269:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1270
	cmpq	$255, -16(%rbp)
	jle	L1271
L1270:
	movl	$16417, %edi
	call	__m3_fault
L1271:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i64
	.private_extern _Mod__uMod_var_i8_i64
_Mod__uMod_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_i64
	.private_extern _Mod__Mod_var_i8_i64
_Mod__Mod_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1276
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1276
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1276:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_i64
	.private_extern _Mod__uMod_param_i8_i64
_Mod__uMod_param_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_i64
	.private_extern _Mod__Mod_param_i8_i64
_Mod__Mod_param_i8_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1281
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1281
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1281:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_C
	.private_extern _Mod__uMod_var_i8_C
_Mod__uMod_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_C
	.private_extern _Mod__Mod_var_i8_C
_Mod__Mod_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1286
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1286
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1286:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1287
	movl	$16609, %edi
	call	__m3_fault
L1287:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_C
	.private_extern _Mod__uMod_param_i8_C
_Mod__uMod_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_C
	.private_extern _Mod__Mod_param_i8_C
_Mod__Mod_param_i8_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1292
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1292
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1292:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L1293
	movl	$16673, %edi
	call	__m3_fault
L1293:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u8
	.private_extern _Mod__uMod_var_i8_u8
_Mod__uMod_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_u8
	.private_extern _Mod__Mod_var_i8_u8
_Mod__Mod_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1298
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1298
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1298:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1299
	cmpq	$255, -16(%rbp)
	jle	L1300
L1299:
	movl	$16737, %edi
	call	__m3_fault
L1300:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_u8
	.private_extern _Mod__uMod_param_i8_u8
_Mod__uMod_param_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_u8
	.private_extern _Mod__Mod_param_i8_u8
_Mod__Mod_param_i8_u8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1305
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1305
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1305:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1306
	cmpq	$255, -16(%rbp)
	jle	L1307
L1306:
	movl	$16801, %edi
	call	__m3_fault
L1307:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i8
	.private_extern _Mod__uMod_var_i8_i8
_Mod__uMod_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_i8
	.private_extern _Mod__Mod_var_i8_i8
_Mod__Mod_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1312
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1312
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1312:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_i8
	.private_extern _Mod__uMod_param_i8_i8
_Mod__uMod_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_i8
	.private_extern _Mod__Mod_param_i8_i8
_Mod__Mod_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -48(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -40(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	L1317
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1317
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L1317:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_L
	.private_extern _Mod__uMod_var_i8_L
_Mod__uMod_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_L
	.private_extern _Mod__Mod_var_i8_L
_Mod__Mod_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1322
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1322
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1322:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_L
	.private_extern _Mod__uMod_param_i8_L
_Mod__uMod_param_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_L
	.private_extern _Mod__Mod_param_i8_L
_Mod__Mod_param_i8_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1327
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1327
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1327:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u32
	.private_extern _Mod__uMod_var_i8_u32
_Mod__uMod_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_i8_u32
	.private_extern _Mod__Mod_var_i8_u32
_Mod__Mod_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1332
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1332
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1332:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1333
	cmpq	$255, -16(%rbp)
	jle	L1334
L1333:
	movl	$17121, %edi
	call	__m3_fault
L1334:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_i8_u32
	.private_extern _Mod__uMod_param_i8_u32
_Mod__uMod_param_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_i8_u32
	.private_extern _Mod__Mod_param_i8_u32
_Mod__Mod_param_i8_u32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1339
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1339
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1339:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1340
	cmpq	$255, -16(%rbp)
	jle	L1341
L1340:
	movl	$17185, %edi
	call	__m3_fault
L1341:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_u16
	.private_extern _Mod__uMod_var_L_u16
_Mod__uMod_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_u16
	.private_extern _Mod__Mod_var_L_u16
_Mod__Mod_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1346
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1346
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1346:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_u16
	.private_extern _Mod__uMod_param_L_u16
_Mod__uMod_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movswq	-26(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_u16
	.private_extern _Mod__Mod_param_L_u16
_Mod__Mod_param_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1351
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1351
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1351:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_u64
	.private_extern _Mod__uMod_var_L_u64
_Mod__uMod_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_u64
	.private_extern _Mod__Mod_var_L_u64
_Mod__Mod_var_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1356
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1356
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1356:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_u64
	.private_extern _Mod__uMod_param_L_u64
_Mod__uMod_param_L_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_u64
	.private_extern _Mod__Mod_param_L_u64
_Mod__Mod_param_L_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1361
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1361
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1361:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_LC
	.private_extern _Mod__uMod_var_L_LC
_Mod__uMod_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_LC
	.private_extern _Mod__Mod_var_L_LC
_Mod__Mod_var_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1366
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1366
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1366:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_LC
	.private_extern _Mod__uMod_param_L_LC
_Mod__uMod_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_LC
	.private_extern _Mod__Mod_param_L_LC
_Mod__Mod_param_L_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1371
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1371
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1371:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_i16
	.private_extern _Mod__uMod_var_L_i16
_Mod__uMod_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_i16
	.private_extern _Mod__Mod_var_L_i16
_Mod__Mod_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1376
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1376
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1376:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_i16
	.private_extern _Mod__uMod_param_L_i16
_Mod__uMod_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rdx
	movzwl	-26(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_i16
	.private_extern _Mod__Mod_param_L_i16
_Mod__Mod_param_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1381
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1381
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1381:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_i32
	.private_extern _Mod__uMod_var_L_i32
_Mod__uMod_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_i32
	.private_extern _Mod__Mod_var_L_i32
_Mod__Mod_var_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1386
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1386
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1386:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_i32
	.private_extern _Mod__uMod_param_L_i32
_Mod__uMod_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	mov	-28(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_i32
	.private_extern _Mod__Mod_param_L_i32
_Mod__Mod_param_L_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1391
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1391
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1391:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_I
	.private_extern _Mod__uMod_var_L_I
_Mod__uMod_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_I
	.private_extern _Mod__Mod_var_L_I
_Mod__Mod_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1396
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1396
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1396:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_I
	.private_extern _Mod__uMod_param_L_I
_Mod__uMod_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_I
	.private_extern _Mod__Mod_param_L_I
_Mod__Mod_param_L_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1401
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1401
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1401:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_i64
	.private_extern _Mod__uMod_var_L_i64
_Mod__uMod_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_i64
	.private_extern _Mod__Mod_var_L_i64
_Mod__Mod_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1406
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1406
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1406:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_i64
	.private_extern _Mod__uMod_param_L_i64
_Mod__uMod_param_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_i64
	.private_extern _Mod__Mod_param_L_i64
_Mod__Mod_param_L_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1411
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1411
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1411:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_C
	.private_extern _Mod__uMod_var_L_C
_Mod__uMod_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_C
	.private_extern _Mod__Mod_var_L_C
_Mod__Mod_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1416
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1416
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1416:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_C
	.private_extern _Mod__uMod_param_L_C
_Mod__uMod_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_C
	.private_extern _Mod__Mod_param_L_C
_Mod__Mod_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1421
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1421
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1421:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_u8
	.private_extern _Mod__uMod_var_L_u8
_Mod__uMod_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_u8
	.private_extern _Mod__Mod_var_L_u8
_Mod__Mod_var_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1426
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1426
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1426:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_u8
	.private_extern _Mod__uMod_param_L_u8
_Mod__uMod_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movsbq	-25(%rbp),%rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_u8
	.private_extern _Mod__Mod_param_L_u8
_Mod__Mod_param_L_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1431
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1431
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1431:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_i8
	.private_extern _Mod__uMod_var_L_i8
_Mod__uMod_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_i8
	.private_extern _Mod__Mod_var_L_i8
_Mod__Mod_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1436
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1436
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1436:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_i8
	.private_extern _Mod__uMod_param_L_i8
_Mod__uMod_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rdx
	movzbl	-25(%rbp), %eax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_i8
	.private_extern _Mod__Mod_param_L_i8
_Mod__Mod_param_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1441
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1441
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1441:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_L
	.private_extern _Mod__uMod_var_L_L
_Mod__uMod_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_L
	.private_extern _Mod__Mod_var_L_L
_Mod__Mod_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1446
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1446
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1446:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_L
	.private_extern _Mod__uMod_param_L_L
_Mod__uMod_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_L
	.private_extern _Mod__Mod_param_L_L
_Mod__Mod_param_L_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1451
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1451
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1451:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_u32
	.private_extern _Mod__uMod_var_L_u32
_Mod__uMod_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_L_u32
	.private_extern _Mod__Mod_var_L_u32
_Mod__Mod_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1456
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1456
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1456:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_L_u32
	.private_extern _Mod__uMod_param_L_u32
_Mod__uMod_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_L_u32
	.private_extern _Mod__Mod_param_L_u32
_Mod__Mod_param_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-40(%rbp)
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1461
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1461
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1461:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u16
	.private_extern _Mod__uMod_var_u32_u16
_Mod__uMod_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_u16
	.private_extern _Mod__Mod_var_u32_u16
_Mod__Mod_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	104+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1466
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1466
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1466:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1467
	cmpq	$2147483647, -16(%rbp)
	jle	L1468
L1467:
	movl	$18785, %edi
	call	__m3_fault
L1468:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_u16
	.private_extern _Mod__uMod_param_u32_u16
_Mod__uMod_param_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_u16
	.private_extern _Mod__Mod_param_u32_u16
_Mod__Mod_param_u32_u16:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1473
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1473
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1473:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1474
	cmpq	$2147483647, -16(%rbp)
	jle	L1475
L1474:
	movl	$18849, %edi
	call	__m3_fault
L1475:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u64
	.private_extern _Mod__uMod_var_u32_u64
_Mod__uMod_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_u64
	.private_extern _Mod__Mod_var_u32_u64
_Mod__Mod_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1480
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1480
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1480:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_u64
	.private_extern _Mod__uMod_param_u32_u64
_Mod__uMod_param_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_u64
	.private_extern _Mod__Mod_param_u32_u64
_Mod__Mod_param_u32_u64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1485
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1485
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1485:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_LC
	.private_extern _Mod__uMod_var_u32_LC
_Mod__uMod_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_LC
	.private_extern _Mod__Mod_var_u32_LC
_Mod__Mod_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	120+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1490
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1490
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1490:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_LC
	.private_extern _Mod__uMod_param_u32_LC
_Mod__uMod_param_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_LC
	.private_extern _Mod__Mod_param_u32_LC
_Mod__Mod_param_u32_LC:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1495
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1495
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1495:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i16
	.private_extern _Mod__uMod_var_u32_i16
_Mod__uMod_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_i16
	.private_extern _Mod__Mod_var_u32_i16
_Mod__Mod_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	136+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1500
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1500
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1500:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_i16
	.private_extern _Mod__uMod_param_u32_i16
_Mod__uMod_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_i16
	.private_extern _Mod__Mod_param_u32_i16
_Mod__Mod_param_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1505
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1505
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1505:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i32
	.private_extern _Mod__uMod_var_u32_i32
_Mod__uMod_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_i32
	.private_extern _Mod__Mod_var_u32_i32
_Mod__Mod_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1510
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1510
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1510:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1511
	movl	$19297, %edi
	call	__m3_fault
L1511:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_i32
	.private_extern _Mod__uMod_param_u32_i32
_Mod__uMod_param_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_i32
	.private_extern _Mod__Mod_param_u32_i32
_Mod__Mod_param_u32_i32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1516
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1516
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1516:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1517
	movl	$19361, %edi
	call	__m3_fault
L1517:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_I
	.private_extern _Mod__uMod_var_u32_I
_Mod__uMod_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_I
	.private_extern _Mod__Mod_var_u32_I
_Mod__Mod_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1522
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1522
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1522:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1523
	cmpq	$2147483647, -16(%rbp)
	jle	L1524
L1523:
	movl	$19425, %edi
	call	__m3_fault
L1524:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_I
	.private_extern _Mod__uMod_param_u32_I
_Mod__uMod_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_I
	.private_extern _Mod__Mod_param_u32_I
_Mod__Mod_param_u32_I:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1529
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1529
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1529:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1530
	cmpq	$2147483647, -16(%rbp)
	jle	L1531
L1530:
	movl	$19489, %edi
	call	__m3_fault
L1531:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i64
	.private_extern _Mod__uMod_var_u32_i64
_Mod__uMod_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_i64
	.private_extern _Mod__Mod_var_u32_i64
_Mod__Mod_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	152+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1536
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1536
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1536:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_i64
	.private_extern _Mod__uMod_param_u32_i64
_Mod__uMod_param_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_i64
	.private_extern _Mod__Mod_param_u32_i64
_Mod__Mod_param_u32_i64:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1541
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1541
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1541:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_C
	.private_extern _Mod__uMod_var_u32_C
_Mod__uMod_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_C
	.private_extern _Mod__Mod_var_u32_C
_Mod__Mod_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1546
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1546
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1546:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1547
	movl	$19681, %edi
	call	__m3_fault
L1547:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_C
	.private_extern _Mod__uMod_param_u32_C
_Mod__uMod_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movl	$0, %edx
	divq	-32(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_C
	.private_extern _Mod__Mod_param_u32_C
_Mod__Mod_param_u32_C:
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
	movq	%rdx, -56(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1552
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1552
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1552:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L1553
	movl	$19745, %edi
	call	__m3_fault
L1553:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u8
	.private_extern _Mod__uMod_var_u32_u8
_Mod__uMod_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_u8
	.private_extern _Mod__Mod_var_u32_u8
_Mod__Mod_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	172+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1558
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1558
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1558:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1559
	cmpq	$2147483647, -16(%rbp)
	jle	L1560
L1559:
	movl	$19809, %edi
	call	__m3_fault
L1560:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_u8
	.private_extern _Mod__uMod_param_u32_u8
_Mod__uMod_param_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_u8
	.private_extern _Mod__Mod_param_u32_u8
_Mod__Mod_param_u32_u8:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1565
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1565
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1565:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1566
	cmpq	$2147483647, -16(%rbp)
	jle	L1567
L1566:
	movl	$19873, %edi
	call	__m3_fault
L1567:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i8
	.private_extern _Mod__uMod_var_u32_i8
_Mod__uMod_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_i8
	.private_extern _Mod__Mod_var_u32_i8
_Mod__Mod_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	173+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1572
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1572
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1572:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_i8
	.private_extern _Mod__uMod_param_u32_i8
_Mod__uMod_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_i8
	.private_extern _Mod__Mod_param_u32_i8
_Mod__Mod_param_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1577
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1577
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1577:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_L
	.private_extern _Mod__uMod_var_u32_L
_Mod__uMod_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_L
	.private_extern _Mod__Mod_var_u32_L
_Mod__Mod_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1582
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1582
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1582:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_L
	.private_extern _Mod__uMod_param_u32_L
_Mod__uMod_param_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-56(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_L
	.private_extern _Mod__Mod_param_u32_L
_Mod__Mod_param_u32_L:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1587
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1587
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1587:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u32
	.private_extern _Mod__uMod_var_u32_u32
_Mod__uMod_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_var_u32_u32
	.private_extern _Mod__Mod_var_u32_u32
_Mod__Mod_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movl	184+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1592
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1592
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1592:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1593
	cmpq	$2147483647, -16(%rbp)
	jle	L1594
L1593:
	movl	$20193, %edi
	call	__m3_fault
L1594:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_param_u32_u32
	.private_extern _Mod__uMod_param_u32_u32
_Mod__uMod_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -40(%rbp)
	movq	%rdx, %rax
	movl	$0, %edx
	divq	-40(%rbp)
	movq	%rdx, %rax
	leave
	ret
.globl _Mod__Mod_param_u32_u32
	.private_extern _Mod__Mod_param_u32_u32
_Mod__Mod_param_u32_u32:
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
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1599
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1599
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1599:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L1600
	cmpq	$2147483647, -16(%rbp)
	jle	L1601
L1600:
	movl	$20257, %edi
	call	__m3_fault
L1601:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod_M3
_Mod_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Mod(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Mod(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Mod_M3"
	.space 1
	.ascii "Mod_param_u32_u32"
	.space 1
	.ascii "uMod_param_u32_u32"
	.space 1
	.ascii "Mod_var_u32_u32"
	.space 1
	.ascii "uMod_var_u32_u32"
	.space 1
	.ascii "Mod_param_u32_L"
	.space 1
	.ascii "uMod_param_u32_L"
	.space 1
	.ascii "Mod_var_u32_L"
	.space 1
	.ascii "uMod_var_u32_L"
	.space 1
	.ascii "Mod_param_u32_i8"
	.space 1
	.ascii "uMod_param_u32_i8"
	.space 1
	.ascii "Mod_var_u32_i8"
	.space 1
	.ascii "uMod_var_u32_i8"
	.space 1
	.ascii "Mod_param_u32_u8"
	.space 1
	.ascii "uMod_param_u32_u8"
	.space 1
	.ascii "Mod_var_u32_u8"
	.space 1
	.ascii "uMod_var_u32_u8"
	.space 1
	.ascii "Mod_param_u32_C"
	.space 1
	.ascii "uMod_param_u32_C"
	.space 1
	.ascii "Mod_var_u32_C"
	.space 1
	.ascii "uMod_var_u32_C"
	.space 1
	.ascii "Mod_param_u32_i64"
	.space 1
	.ascii "uMod_param_u32_i64"
	.space 1
	.ascii "Mod_var_u32_i64"
	.space 1
	.ascii "uMod_var_u32_i64"
	.space 1
	.ascii "Mod_param_u32_I"
	.space 1
	.ascii "uMod_param_u32_I"
	.space 1
	.ascii "Mod_var_u32_I"
	.space 1
	.ascii "uMod_var_u32_I"
	.space 1
	.ascii "Mod_param_u32_i32"
	.space 1
	.ascii "uMod_param_u32_i32"
	.space 1
	.ascii "Mod_var_u32_i32"
	.space 1
	.ascii "uMod_var_u32_i32"
	.space 1
	.ascii "Mod_param_u32_i16"
	.space 1
	.ascii "uMod_param_u32_i16"
	.space 1
	.ascii "Mod_var_u32_i16"
	.space 1
	.ascii "uMod_var_u32_i16"
	.space 1
	.ascii "Mod_param_u32_LC"
	.space 1
	.ascii "uMod_param_u32_LC"
	.space 1
	.ascii "Mod_var_u32_LC"
	.space 1
	.ascii "uMod_var_u32_LC"
	.space 1
	.ascii "Mod_param_u32_u64"
	.space 1
	.ascii "uMod_param_u32_u64"
	.space 1
	.ascii "Mod_var_u32_u64"
	.space 1
	.ascii "uMod_var_u32_u64"
	.space 1
	.ascii "Mod_param_u32_u16"
	.space 1
	.ascii "uMod_param_u32_u16"
	.space 1
	.ascii "Mod_var_u32_u16"
	.space 1
	.ascii "uMod_var_u32_u16"
	.space 1
	.ascii "Mod_param_L_u32"
	.space 1
	.ascii "uMod_param_L_u32"
	.space 1
	.ascii "Mod_var_L_u32"
	.space 1
	.ascii "uMod_var_L_u32"
	.space 1
	.ascii "Mod_param_L_L"
	.space 1
	.ascii "uMod_param_L_L"
	.space 1
	.ascii "Mod_var_L_L"
	.space 1
	.ascii "uMod_var_L_L"
	.space 1
	.ascii "Mod_param_L_i8"
	.space 1
	.ascii "uMod_param_L_i8"
	.space 1
	.ascii "Mod_var_L_i8"
	.space 1
	.ascii "uMod_var_L_i8"
	.space 1
	.ascii "Mod_param_L_u8"
	.space 1
	.ascii "uMod_param_L_u8"
	.space 1
	.ascii "Mod_var_L_u8"
	.space 1
	.ascii "uMod_var_L_u8"
	.space 1
	.ascii "Mod_param_L_C"
	.space 1
	.ascii "uMod_param_L_C"
	.space 1
	.ascii "Mod_var_L_C"
	.space 1
	.ascii "uMod_var_L_C"
	.space 1
	.ascii "Mod_param_L_i64"
	.space 1
	.ascii "uMod_param_L_i64"
	.space 1
	.ascii "Mod_var_L_i64"
	.space 1
	.ascii "uMod_var_L_i64"
	.space 1
	.ascii "Mod_param_L_I"
	.space 1
	.ascii "uMod_param_L_I"
	.space 1
	.ascii "Mod_var_L_I"
	.space 1
	.ascii "uMod_var_L_I"
	.space 1
	.ascii "Mod_param_L_i32"
	.space 1
	.ascii "uMod_param_L_i32"
	.space 1
	.ascii "Mod_var_L_i32"
	.space 1
	.ascii "uMod_var_L_i32"
	.space 1
	.ascii "Mod_param_L_i16"
	.space 1
	.ascii "uMod_param_L_i16"
	.space 1
	.ascii "Mod_var_L_i16"
	.space 1
	.ascii "uMod_var_L_i16"
	.space 1
	.ascii "Mod_param_L_LC"
	.space 1
	.ascii "uMod_param_L_LC"
	.space 1
	.ascii "Mod_var_L_LC"
	.space 1
	.ascii "uMod_var_L_LC"
	.space 1
	.ascii "Mod_param_L_u64"
	.space 1
	.ascii "uMod_param_L_u64"
	.space 1
	.ascii "Mod_var_L_u64"
	.space 1
	.ascii "uMod_var_L_u64"
	.space 1
	.ascii "Mod_param_L_u16"
	.space 1
	.ascii "uMod_param_L_u16"
	.space 1
	.ascii "Mod_var_L_u16"
	.space 1
	.ascii "uMod_var_L_u16"
	.space 1
	.ascii "Mod_param_i8_u32"
	.space 1
	.ascii "uMod_param_i8_u32"
	.space 1
	.ascii "Mod_var_i8_u32"
	.space 1
	.ascii "uMod_var_i8_u32"
	.space 1
	.ascii "Mod_param_i8_L"
	.space 1
	.ascii "uMod_param_i8_L"
	.space 1
	.ascii "Mod_var_i8_L"
	.space 1
	.ascii "uMod_var_i8_L"
	.space 1
	.ascii "Mod_param_i8_i8"
	.space 1
	.ascii "uMod_param_i8_i8"
	.space 1
	.ascii "Mod_var_i8_i8"
	.space 1
	.ascii "uMod_var_i8_i8"
	.space 1
	.ascii "Mod_param_i8_u8"
	.space 1
	.ascii "uMod_param_i8_u8"
	.space 1
	.ascii "Mod_var_i8_u8"
	.space 1
	.ascii "uMod_var_i8_u8"
	.space 1
	.ascii "Mod_param_i8_C"
	.space 1
	.ascii "uMod_param_i8_C"
	.space 1
	.ascii "Mod_var_i8_C"
	.space 1
	.ascii "uMod_var_i8_C"
	.space 1
	.ascii "Mod_param_i8_i64"
	.space 1
	.ascii "uMod_param_i8_i64"
	.space 1
	.ascii "Mod_var_i8_i64"
	.space 1
	.ascii "uMod_var_i8_i64"
	.space 1
	.ascii "Mod_param_i8_I"
	.space 1
	.ascii "uMod_param_i8_I"
	.space 1
	.ascii "Mod_var_i8_I"
	.space 1
	.ascii "uMod_var_i8_I"
	.space 1
	.ascii "Mod_param_i8_i32"
	.space 1
	.ascii "uMod_param_i8_i32"
	.space 1
	.ascii "Mod_var_i8_i32"
	.space 1
	.ascii "uMod_var_i8_i32"
	.space 1
	.ascii "Mod_param_i8_i16"
	.space 1
	.ascii "uMod_param_i8_i16"
	.space 1
	.ascii "Mod_var_i8_i16"
	.space 1
	.ascii "uMod_var_i8_i16"
	.space 1
	.ascii "Mod_param_i8_LC"
	.space 1
	.ascii "uMod_param_i8_LC"
	.space 1
	.ascii "Mod_var_i8_LC"
	.space 1
	.ascii "uMod_var_i8_LC"
	.space 1
	.ascii "Mod_param_i8_u64"
	.space 1
	.ascii "uMod_param_i8_u64"
	.space 1
	.ascii "Mod_var_i8_u64"
	.space 1
	.ascii "uMod_var_i8_u64"
	.space 1
	.ascii "Mod_param_i8_u16"
	.space 1
	.ascii "uMod_param_i8_u16"
	.space 1
	.ascii "Mod_var_i8_u16"
	.space 1
	.ascii "uMod_var_i8_u16"
	.space 1
	.ascii "Mod_param_u8_u32"
	.space 1
	.ascii "uMod_param_u8_u32"
	.space 1
	.ascii "Mod_var_u8_u32"
	.space 1
	.ascii "uMod_var_u8_u32"
	.space 1
	.ascii "Mod_param_u8_L"
	.space 1
	.ascii "uMod_param_u8_L"
	.space 1
	.ascii "Mod_var_u8_L"
	.space 1
	.ascii "uMod_var_u8_L"
	.space 1
	.ascii "Mod_param_u8_i8"
	.space 1
	.ascii "uMod_param_u8_i8"
	.space 1
	.ascii "Mod_var_u8_i8"
	.space 1
	.ascii "uMod_var_u8_i8"
	.space 1
	.ascii "Mod_param_u8_u8"
	.space 1
	.ascii "uMod_param_u8_u8"
	.space 1
	.ascii "Mod_var_u8_u8"
	.space 1
	.ascii "uMod_var_u8_u8"
	.space 1
	.ascii "Mod_param_u8_C"
	.space 1
	.ascii "uMod_param_u8_C"
	.space 1
	.ascii "Mod_var_u8_C"
	.space 1
	.ascii "uMod_var_u8_C"
	.space 1
	.ascii "Mod_param_u8_i64"
	.space 1
	.ascii "uMod_param_u8_i64"
	.space 1
	.ascii "Mod_var_u8_i64"
	.space 1
	.ascii "uMod_var_u8_i64"
	.space 1
	.ascii "Mod_param_u8_I"
	.space 1
	.ascii "uMod_param_u8_I"
	.space 1
	.ascii "Mod_var_u8_I"
	.space 1
	.ascii "uMod_var_u8_I"
	.space 1
	.ascii "Mod_param_u8_i32"
	.space 1
	.ascii "uMod_param_u8_i32"
	.space 1
	.ascii "Mod_var_u8_i32"
	.space 1
	.ascii "uMod_var_u8_i32"
	.space 1
	.ascii "Mod_param_u8_i16"
	.space 1
	.ascii "uMod_param_u8_i16"
	.space 1
	.ascii "Mod_var_u8_i16"
	.space 1
	.ascii "uMod_var_u8_i16"
	.space 1
	.ascii "Mod_param_u8_LC"
	.space 1
	.ascii "uMod_param_u8_LC"
	.space 1
	.ascii "Mod_var_u8_LC"
	.space 1
	.ascii "uMod_var_u8_LC"
	.space 1
	.ascii "Mod_param_u8_u64"
	.space 1
	.ascii "uMod_param_u8_u64"
	.space 1
	.ascii "Mod_var_u8_u64"
	.space 1
	.ascii "uMod_var_u8_u64"
	.space 1
	.ascii "Mod_param_u8_u16"
	.space 1
	.ascii "uMod_param_u8_u16"
	.space 1
	.ascii "Mod_var_u8_u16"
	.space 1
	.ascii "uMod_var_u8_u16"
	.space 1
	.ascii "Mod_param_f32_f32"
	.space 1
	.ascii "Mod_var_f32_f32"
	.space 1
	.ascii "Mod_param_C_u32"
	.space 1
	.ascii "uMod_param_C_u32"
	.space 1
	.ascii "Mod_var_C_u32"
	.space 1
	.ascii "uMod_var_C_u32"
	.space 1
	.ascii "Mod_param_C_L"
	.space 1
	.ascii "uMod_param_C_L"
	.space 1
	.ascii "Mod_var_C_L"
	.space 1
	.ascii "uMod_var_C_L"
	.space 1
	.ascii "Mod_param_C_i8"
	.space 1
	.ascii "uMod_param_C_i8"
	.space 1
	.ascii "Mod_var_C_i8"
	.space 1
	.ascii "uMod_var_C_i8"
	.space 1
	.ascii "Mod_param_C_u8"
	.space 1
	.ascii "uMod_param_C_u8"
	.space 1
	.ascii "Mod_var_C_u8"
	.space 1
	.ascii "uMod_var_C_u8"
	.space 1
	.ascii "Mod_param_C_C"
	.space 1
	.ascii "uMod_param_C_C"
	.space 1
	.ascii "Mod_var_C_C"
	.space 1
	.ascii "uMod_var_C_C"
	.space 1
	.ascii "Mod_param_C_i64"
	.space 1
	.ascii "uMod_param_C_i64"
	.space 1
	.ascii "Mod_var_C_i64"
	.space 1
	.ascii "uMod_var_C_i64"
	.space 1
	.ascii "Mod_param_C_I"
	.space 1
	.ascii "uMod_param_C_I"
	.space 1
	.ascii "Mod_var_C_I"
	.space 1
	.ascii "uMod_var_C_I"
	.space 1
	.ascii "Mod_param_C_i32"
	.space 1
	.ascii "uMod_param_C_i32"
	.space 1
	.ascii "Mod_var_C_i32"
	.space 1
	.ascii "uMod_var_C_i32"
	.space 1
	.ascii "Mod_param_C_i16"
	.space 1
	.ascii "uMod_param_C_i16"
	.space 1
	.ascii "Mod_var_C_i16"
	.space 1
	.ascii "uMod_var_C_i16"
	.space 1
	.ascii "Mod_param_C_LC"
	.space 1
	.ascii "uMod_param_C_LC"
	.space 1
	.ascii "Mod_var_C_LC"
	.space 1
	.ascii "uMod_var_C_LC"
	.space 1
	.ascii "Mod_param_C_u64"
	.space 1
	.ascii "uMod_param_C_u64"
	.space 1
	.ascii "Mod_var_C_u64"
	.space 1
	.ascii "uMod_var_C_u64"
	.space 1
	.ascii "Mod_param_C_u16"
	.space 1
	.ascii "uMod_param_C_u16"
	.space 1
	.ascii "Mod_var_C_u16"
	.space 1
	.ascii "uMod_var_C_u16"
	.space 1
	.ascii "Mod_param_i64_u32"
	.space 1
	.ascii "uMod_param_i64_u32"
	.space 1
	.ascii "Mod_var_i64_u32"
	.space 1
	.ascii "uMod_var_i64_u32"
	.space 1
	.ascii "Mod_param_i64_L"
	.space 1
	.ascii "uMod_param_i64_L"
	.space 1
	.ascii "Mod_var_i64_L"
	.space 1
	.ascii "uMod_var_i64_L"
	.space 1
	.ascii "Mod_param_i64_i8"
	.space 1
	.ascii "uMod_param_i64_i8"
	.space 1
	.ascii "Mod_var_i64_i8"
	.space 1
	.ascii "uMod_var_i64_i8"
	.space 1
	.ascii "Mod_param_i64_u8"
	.space 1
	.ascii "uMod_param_i64_u8"
	.space 1
	.ascii "Mod_var_i64_u8"
	.space 1
	.ascii "uMod_var_i64_u8"
	.space 1
	.ascii "Mod_param_i64_C"
	.space 1
	.ascii "uMod_param_i64_C"
	.space 1
	.ascii "Mod_var_i64_C"
	.space 1
	.ascii "uMod_var_i64_C"
	.space 1
	.ascii "Mod_param_i64_i64"
	.space 1
	.ascii "uMod_param_i64_i64"
	.space 1
	.ascii "Mod_var_i64_i64"
	.space 1
	.ascii "uMod_var_i64_i64"
	.space 1
	.ascii "Mod_param_i64_I"
	.space 1
	.ascii "uMod_param_i64_I"
	.space 1
	.ascii "Mod_var_i64_I"
	.space 1
	.ascii "uMod_var_i64_I"
	.space 1
	.ascii "Mod_param_i64_i32"
	.space 1
	.ascii "uMod_param_i64_i32"
	.space 1
	.ascii "Mod_var_i64_i32"
	.space 1
	.ascii "uMod_var_i64_i32"
	.space 1
	.ascii "Mod_param_i64_i16"
	.space 1
	.ascii "uMod_param_i64_i16"
	.space 1
	.ascii "Mod_var_i64_i16"
	.space 1
	.ascii "uMod_var_i64_i16"
	.space 1
	.ascii "Mod_param_i64_LC"
	.space 1
	.ascii "uMod_param_i64_LC"
	.space 1
	.ascii "Mod_var_i64_LC"
	.space 1
	.ascii "uMod_var_i64_LC"
	.space 1
	.ascii "Mod_param_i64_u64"
	.space 1
	.ascii "uMod_param_i64_u64"
	.space 1
	.ascii "Mod_var_i64_u64"
	.space 1
	.ascii "uMod_var_i64_u64"
	.space 1
	.ascii "Mod_param_i64_u16"
	.space 1
	.ascii "uMod_param_i64_u16"
	.space 1
	.ascii "Mod_var_i64_u16"
	.space 1
	.ascii "uMod_var_i64_u16"
	.space 1
	.ascii "Mod_param_I_u32"
	.space 1
	.ascii "uMod_param_I_u32"
	.space 1
	.ascii "Mod_var_I_u32"
	.space 1
	.ascii "uMod_var_I_u32"
	.space 1
	.ascii "Mod_param_I_L"
	.space 1
	.ascii "uMod_param_I_L"
	.space 1
	.ascii "Mod_var_I_L"
	.space 1
	.ascii "uMod_var_I_L"
	.space 1
	.ascii "Mod_param_I_i8"
	.space 1
	.ascii "uMod_param_I_i8"
	.space 1
	.ascii "Mod_var_I_i8"
	.space 1
	.ascii "uMod_var_I_i8"
	.space 1
	.ascii "Mod_param_I_u8"
	.space 1
	.ascii "uMod_param_I_u8"
	.space 1
	.ascii "Mod_var_I_u8"
	.space 1
	.ascii "uMod_var_I_u8"
	.space 1
	.ascii "Mod_param_I_C"
	.space 1
	.ascii "uMod_param_I_C"
	.space 1
	.ascii "Mod_var_I_C"
	.space 1
	.ascii "uMod_var_I_C"
	.space 1
	.ascii "Mod_param_I_i64"
	.space 1
	.ascii "uMod_param_I_i64"
	.space 1
	.ascii "Mod_var_I_i64"
	.space 1
	.ascii "uMod_var_I_i64"
	.space 1
	.ascii "Mod_param_I_I"
	.space 1
	.ascii "uMod_param_I_I"
	.space 1
	.ascii "Mod_var_I_I"
	.space 1
	.ascii "uMod_var_I_I"
	.space 1
	.ascii "Mod_param_I_i32"
	.space 1
	.ascii "uMod_param_I_i32"
	.space 1
	.ascii "Mod_var_I_i32"
	.space 1
	.ascii "uMod_var_I_i32"
	.space 1
	.ascii "Mod_param_I_i16"
	.space 1
	.ascii "uMod_param_I_i16"
	.space 1
	.ascii "Mod_var_I_i16"
	.space 1
	.ascii "uMod_var_I_i16"
	.space 1
	.ascii "Mod_param_I_LC"
	.space 1
	.ascii "uMod_param_I_LC"
	.space 1
	.ascii "Mod_var_I_LC"
	.space 1
	.ascii "uMod_var_I_LC"
	.space 1
	.ascii "Mod_param_I_u64"
	.space 1
	.ascii "uMod_param_I_u64"
	.space 1
	.ascii "Mod_var_I_u64"
	.space 1
	.ascii "uMod_var_I_u64"
	.space 1
	.ascii "Mod_param_I_u16"
	.space 1
	.ascii "uMod_param_I_u16"
	.space 1
	.ascii "Mod_var_I_u16"
	.space 1
	.ascii "uMod_var_I_u16"
	.space 1
	.ascii "Mod_param_i32_u32"
	.space 1
	.ascii "uMod_param_i32_u32"
	.space 1
	.ascii "Mod_var_i32_u32"
	.space 1
	.ascii "uMod_var_i32_u32"
	.space 1
	.ascii "Mod_param_i32_L"
	.space 1
	.ascii "uMod_param_i32_L"
	.space 1
	.ascii "Mod_var_i32_L"
	.space 1
	.ascii "uMod_var_i32_L"
	.space 1
	.ascii "Mod_param_i32_i8"
	.space 1
	.ascii "uMod_param_i32_i8"
	.space 1
	.ascii "Mod_var_i32_i8"
	.space 1
	.ascii "uMod_var_i32_i8"
	.space 1
	.ascii "Mod_param_i32_u8"
	.space 1
	.ascii "uMod_param_i32_u8"
	.space 1
	.ascii "Mod_var_i32_u8"
	.space 1
	.ascii "uMod_var_i32_u8"
	.space 1
	.ascii "Mod_param_i32_C"
	.space 1
	.ascii "uMod_param_i32_C"
	.space 1
	.ascii "Mod_var_i32_C"
	.space 1
	.ascii "uMod_var_i32_C"
	.space 1
	.ascii "Mod_param_i32_i64"
	.space 1
	.ascii "uMod_param_i32_i64"
	.space 1
	.ascii "Mod_var_i32_i64"
	.space 1
	.ascii "uMod_var_i32_i64"
	.space 1
	.ascii "Mod_param_i32_I"
	.space 1
	.ascii "uMod_param_i32_I"
	.space 1
	.ascii "Mod_var_i32_I"
	.space 1
	.ascii "uMod_var_i32_I"
	.space 1
	.ascii "Mod_param_i32_i32"
	.space 1
	.ascii "uMod_param_i32_i32"
	.space 1
	.ascii "Mod_var_i32_i32"
	.space 1
	.ascii "uMod_var_i32_i32"
	.space 1
	.ascii "Mod_param_i32_i16"
	.space 1
	.ascii "uMod_param_i32_i16"
	.space 1
	.ascii "Mod_var_i32_i16"
	.space 1
	.ascii "uMod_var_i32_i16"
	.space 1
	.ascii "Mod_param_i32_LC"
	.space 1
	.ascii "uMod_param_i32_LC"
	.space 1
	.ascii "Mod_var_i32_LC"
	.space 1
	.ascii "uMod_var_i32_LC"
	.space 1
	.ascii "Mod_param_i32_u64"
	.space 1
	.ascii "uMod_param_i32_u64"
	.space 1
	.ascii "Mod_var_i32_u64"
	.space 1
	.ascii "uMod_var_i32_u64"
	.space 1
	.ascii "Mod_param_i32_u16"
	.space 1
	.ascii "uMod_param_i32_u16"
	.space 1
	.ascii "Mod_var_i32_u16"
	.space 1
	.ascii "uMod_var_i32_u16"
	.space 1
	.ascii "Mod_param_i16_u32"
	.space 1
	.ascii "uMod_param_i16_u32"
	.space 1
	.ascii "Mod_var_i16_u32"
	.space 1
	.ascii "uMod_var_i16_u32"
	.space 1
	.ascii "Mod_param_i16_L"
	.space 1
	.ascii "uMod_param_i16_L"
	.space 1
	.ascii "Mod_var_i16_L"
	.space 1
	.ascii "uMod_var_i16_L"
	.space 1
	.ascii "Mod_param_i16_i8"
	.space 1
	.ascii "uMod_param_i16_i8"
	.space 1
	.ascii "Mod_var_i16_i8"
	.space 1
	.ascii "uMod_var_i16_i8"
	.space 1
	.ascii "Mod_param_i16_u8"
	.space 1
	.ascii "uMod_param_i16_u8"
	.space 1
	.ascii "Mod_var_i16_u8"
	.space 1
	.ascii "uMod_var_i16_u8"
	.space 1
	.ascii "Mod_param_i16_C"
	.space 1
	.ascii "uMod_param_i16_C"
	.space 1
	.ascii "Mod_var_i16_C"
	.space 1
	.ascii "uMod_var_i16_C"
	.space 1
	.ascii "Mod_param_i16_i64"
	.space 1
	.ascii "uMod_param_i16_i64"
	.space 1
	.ascii "Mod_var_i16_i64"
	.space 1
	.ascii "uMod_var_i16_i64"
	.space 1
	.ascii "Mod_param_i16_I"
	.space 1
	.ascii "uMod_param_i16_I"
	.space 1
	.ascii "Mod_var_i16_I"
	.space 1
	.ascii "uMod_var_i16_I"
	.space 1
	.ascii "Mod_param_i16_i32"
	.space 1
	.ascii "uMod_param_i16_i32"
	.space 1
	.ascii "Mod_var_i16_i32"
	.space 1
	.ascii "uMod_var_i16_i32"
	.space 1
	.ascii "Mod_param_i16_i16"
	.space 1
	.ascii "uMod_param_i16_i16"
	.space 1
	.ascii "Mod_var_i16_i16"
	.space 1
	.ascii "uMod_var_i16_i16"
	.space 1
	.ascii "Mod_param_i16_LC"
	.space 1
	.ascii "uMod_param_i16_LC"
	.space 1
	.ascii "Mod_var_i16_LC"
	.space 1
	.ascii "uMod_var_i16_LC"
	.space 1
	.ascii "Mod_param_i16_u64"
	.space 1
	.ascii "uMod_param_i16_u64"
	.space 1
	.ascii "Mod_var_i16_u64"
	.space 1
	.ascii "uMod_var_i16_u64"
	.space 1
	.ascii "Mod_param_i16_u16"
	.space 1
	.ascii "uMod_param_i16_u16"
	.space 1
	.ascii "Mod_var_i16_u16"
	.space 1
	.ascii "uMod_var_i16_u16"
	.space 1
	.ascii "Mod_param_f64_f64"
	.space 1
	.ascii "Mod_var_f64_f64"
	.space 1
	.ascii "Mod_param_LC_u32"
	.space 1
	.ascii "uMod_param_LC_u32"
	.space 1
	.ascii "Mod_var_LC_u32"
	.space 1
	.ascii "uMod_var_LC_u32"
	.space 1
	.ascii "Mod_param_LC_L"
	.space 1
	.ascii "uMod_param_LC_L"
	.space 1
	.ascii "Mod_var_LC_L"
	.space 1
	.ascii "uMod_var_LC_L"
	.space 1
	.ascii "Mod_param_LC_i8"
	.space 1
	.ascii "uMod_param_LC_i8"
	.space 1
	.ascii "Mod_var_LC_i8"
	.space 1
	.ascii "uMod_var_LC_i8"
	.space 1
	.ascii "Mod_param_LC_u8"
	.space 1
	.ascii "uMod_param_LC_u8"
	.space 1
	.ascii "Mod_var_LC_u8"
	.space 1
	.ascii "uMod_var_LC_u8"
	.space 1
	.ascii "Mod_param_LC_C"
	.space 1
	.ascii "uMod_param_LC_C"
	.space 1
	.ascii "Mod_var_LC_C"
	.space 1
	.ascii "uMod_var_LC_C"
	.space 1
	.ascii "Mod_param_LC_i64"
	.space 1
	.ascii "uMod_param_LC_i64"
	.space 1
	.ascii "Mod_var_LC_i64"
	.space 1
	.ascii "uMod_var_LC_i64"
	.space 1
	.ascii "Mod_param_LC_I"
	.space 1
	.ascii "uMod_param_LC_I"
	.space 1
	.ascii "Mod_var_LC_I"
	.space 1
	.ascii "uMod_var_LC_I"
	.space 1
	.ascii "Mod_param_LC_i32"
	.space 1
	.ascii "uMod_param_LC_i32"
	.space 1
	.ascii "Mod_var_LC_i32"
	.space 1
	.ascii "uMod_var_LC_i32"
	.space 1
	.ascii "Mod_param_LC_i16"
	.space 1
	.ascii "uMod_param_LC_i16"
	.space 1
	.ascii "Mod_var_LC_i16"
	.space 1
	.ascii "uMod_var_LC_i16"
	.space 1
	.ascii "Mod_param_LC_LC"
	.space 1
	.ascii "uMod_param_LC_LC"
	.space 1
	.ascii "Mod_var_LC_LC"
	.space 1
	.ascii "uMod_var_LC_LC"
	.space 1
	.ascii "Mod_param_LC_u64"
	.space 1
	.ascii "uMod_param_LC_u64"
	.space 1
	.ascii "Mod_var_LC_u64"
	.space 1
	.ascii "uMod_var_LC_u64"
	.space 1
	.ascii "Mod_param_LC_u16"
	.space 1
	.ascii "uMod_param_LC_u16"
	.space 1
	.ascii "Mod_var_LC_u16"
	.space 1
	.ascii "uMod_var_LC_u16"
	.space 1
	.ascii "Mod_param_u64_u32"
	.space 1
	.ascii "uMod_param_u64_u32"
	.space 1
	.ascii "Mod_var_u64_u32"
	.space 1
	.ascii "uMod_var_u64_u32"
	.space 1
	.ascii "Mod_param_u64_L"
	.space 1
	.ascii "uMod_param_u64_L"
	.space 1
	.ascii "Mod_var_u64_L"
	.space 1
	.ascii "uMod_var_u64_L"
	.space 1
	.ascii "Mod_param_u64_i8"
	.space 1
	.ascii "uMod_param_u64_i8"
	.space 1
	.ascii "Mod_var_u64_i8"
	.space 1
	.ascii "uMod_var_u64_i8"
	.space 1
	.ascii "Mod_param_u64_u8"
	.space 1
	.ascii "uMod_param_u64_u8"
	.space 1
	.ascii "Mod_var_u64_u8"
	.space 1
	.ascii "uMod_var_u64_u8"
	.space 1
	.ascii "Mod_param_u64_C"
	.space 1
	.ascii "uMod_param_u64_C"
	.space 1
	.ascii "Mod_var_u64_C"
	.space 1
	.ascii "uMod_var_u64_C"
	.space 1
	.ascii "Mod_param_u64_i64"
	.space 1
	.ascii "uMod_param_u64_i64"
	.space 1
	.ascii "Mod_var_u64_i64"
	.space 1
	.ascii "uMod_var_u64_i64"
	.space 1
	.ascii "Mod_param_u64_I"
	.space 1
	.ascii "uMod_param_u64_I"
	.space 1
	.ascii "Mod_var_u64_I"
	.space 1
	.ascii "uMod_var_u64_I"
	.space 1
	.ascii "Mod_param_u64_i32"
	.space 1
	.ascii "uMod_param_u64_i32"
	.space 1
	.ascii "Mod_var_u64_i32"
	.space 1
	.ascii "uMod_var_u64_i32"
	.space 1
	.ascii "Mod_param_u64_i16"
	.space 1
	.ascii "uMod_param_u64_i16"
	.space 1
	.ascii "Mod_var_u64_i16"
	.space 1
	.ascii "uMod_var_u64_i16"
	.space 1
	.ascii "Mod_param_u64_LC"
	.space 1
	.ascii "uMod_param_u64_LC"
	.space 1
	.ascii "Mod_var_u64_LC"
	.space 1
	.ascii "uMod_var_u64_LC"
	.space 1
	.ascii "Mod_param_u64_u64"
	.space 1
	.ascii "uMod_param_u64_u64"
	.space 1
	.ascii "Mod_var_u64_u64"
	.space 1
	.ascii "uMod_var_u64_u64"
	.space 1
	.ascii "Mod_param_u64_u16"
	.space 1
	.ascii "uMod_param_u64_u16"
	.space 1
	.ascii "Mod_var_u64_u16"
	.space 1
	.ascii "uMod_var_u64_u16"
	.space 1
	.ascii "Mod_param_u16_u32"
	.space 1
	.ascii "uMod_param_u16_u32"
	.space 1
	.ascii "Mod_var_u16_u32"
	.space 1
	.ascii "uMod_var_u16_u32"
	.space 1
	.ascii "Mod_param_u16_L"
	.space 1
	.ascii "uMod_param_u16_L"
	.space 1
	.ascii "Mod_var_u16_L"
	.space 1
	.ascii "uMod_var_u16_L"
	.space 1
	.ascii "Mod_param_u16_i8"
	.space 1
	.ascii "uMod_param_u16_i8"
	.space 1
	.ascii "Mod_var_u16_i8"
	.space 1
	.ascii "uMod_var_u16_i8"
	.space 1
	.ascii "Mod_param_u16_u8"
	.space 1
	.ascii "uMod_param_u16_u8"
	.space 1
	.ascii "Mod_var_u16_u8"
	.space 1
	.ascii "uMod_var_u16_u8"
	.space 1
	.ascii "Mod_param_u16_C"
	.space 1
	.ascii "uMod_param_u16_C"
	.space 1
	.ascii "Mod_var_u16_C"
	.space 1
	.ascii "uMod_var_u16_C"
	.space 1
	.ascii "Mod_param_u16_i64"
	.space 1
	.ascii "uMod_param_u16_i64"
	.space 1
	.ascii "Mod_var_u16_i64"
	.space 1
	.ascii "uMod_var_u16_i64"
	.space 1
	.ascii "Mod_param_u16_I"
	.space 1
	.ascii "uMod_param_u16_I"
	.space 1
	.ascii "Mod_var_u16_I"
	.space 1
	.ascii "uMod_var_u16_I"
	.space 1
	.ascii "Mod_param_u16_i32"
	.space 1
	.ascii "uMod_param_u16_i32"
	.space 1
	.ascii "Mod_var_u16_i32"
	.space 1
	.ascii "uMod_var_u16_i32"
	.space 1
	.ascii "Mod_param_u16_i16"
	.space 1
	.ascii "uMod_param_u16_i16"
	.space 1
	.ascii "Mod_var_u16_i16"
	.space 1
	.ascii "uMod_var_u16_i16"
	.space 1
	.ascii "Mod_param_u16_LC"
	.space 1
	.ascii "uMod_param_u16_LC"
	.space 1
	.ascii "Mod_var_u16_LC"
	.space 1
	.ascii "uMod_var_u16_LC"
	.space 1
	.ascii "Mod_param_u16_u64"
	.space 1
	.ascii "uMod_param_u16_u64"
	.space 1
	.ascii "Mod_var_u16_u64"
	.space 1
	.ascii "uMod_var_u16_u64"
	.space 1
	.ascii "Mod_param_u16_u16"
	.space 1
	.ascii "uMod_param_u16_u16"
	.space 1
	.ascii "Mod_var_u16_u16"
	.space 1
	.ascii "uMod_var_u16_u16"
	.space 6
	.quad	_Mod_M3
	.quad	_L_1
	.quad	_Mod__Mod_param_u32_u32
	.quad	_L_1+7
	.quad	_Mod__uMod_param_u32_u32
	.quad	_L_1+25
	.quad	_Mod__Mod_var_u32_u32
	.quad	_L_1+44
	.quad	_Mod__uMod_var_u32_u32
	.quad	_L_1+60
	.quad	_Mod__Mod_param_u32_L
	.quad	_L_1+77
	.quad	_Mod__uMod_param_u32_L
	.quad	_L_1+93
	.quad	_Mod__Mod_var_u32_L
	.quad	_L_1+110
	.quad	_Mod__uMod_var_u32_L
	.quad	_L_1+124
	.quad	_Mod__Mod_param_u32_i8
	.quad	_L_1+139
	.quad	_Mod__uMod_param_u32_i8
	.quad	_L_1+156
	.quad	_Mod__Mod_var_u32_i8
	.quad	_L_1+174
	.quad	_Mod__uMod_var_u32_i8
	.quad	_L_1+189
	.quad	_Mod__Mod_param_u32_u8
	.quad	_L_1+205
	.quad	_Mod__uMod_param_u32_u8
	.quad	_L_1+222
	.quad	_Mod__Mod_var_u32_u8
	.quad	_L_1+240
	.quad	_Mod__uMod_var_u32_u8
	.quad	_L_1+255
	.quad	_Mod__Mod_param_u32_C
	.quad	_L_1+271
	.quad	_Mod__uMod_param_u32_C
	.quad	_L_1+287
	.quad	_Mod__Mod_var_u32_C
	.quad	_L_1+304
	.quad	_Mod__uMod_var_u32_C
	.quad	_L_1+318
	.quad	_Mod__Mod_param_u32_i64
	.quad	_L_1+333
	.quad	_Mod__uMod_param_u32_i64
	.quad	_L_1+351
	.quad	_Mod__Mod_var_u32_i64
	.quad	_L_1+370
	.quad	_Mod__uMod_var_u32_i64
	.quad	_L_1+386
	.quad	_Mod__Mod_param_u32_I
	.quad	_L_1+403
	.quad	_Mod__uMod_param_u32_I
	.quad	_L_1+419
	.quad	_Mod__Mod_var_u32_I
	.quad	_L_1+436
	.quad	_Mod__uMod_var_u32_I
	.quad	_L_1+450
	.quad	_Mod__Mod_param_u32_i32
	.quad	_L_1+465
	.quad	_Mod__uMod_param_u32_i32
	.quad	_L_1+483
	.quad	_Mod__Mod_var_u32_i32
	.quad	_L_1+502
	.quad	_Mod__uMod_var_u32_i32
	.quad	_L_1+518
	.quad	_Mod__Mod_param_u32_i16
	.quad	_L_1+535
	.quad	_Mod__uMod_param_u32_i16
	.quad	_L_1+553
	.quad	_Mod__Mod_var_u32_i16
	.quad	_L_1+572
	.quad	_Mod__uMod_var_u32_i16
	.quad	_L_1+588
	.quad	_Mod__Mod_param_u32_LC
	.quad	_L_1+605
	.quad	_Mod__uMod_param_u32_LC
	.quad	_L_1+622
	.quad	_Mod__Mod_var_u32_LC
	.quad	_L_1+640
	.quad	_Mod__uMod_var_u32_LC
	.quad	_L_1+655
	.quad	_Mod__Mod_param_u32_u64
	.quad	_L_1+671
	.quad	_Mod__uMod_param_u32_u64
	.quad	_L_1+689
	.quad	_Mod__Mod_var_u32_u64
	.quad	_L_1+708
	.quad	_Mod__uMod_var_u32_u64
	.quad	_L_1+724
	.quad	_Mod__Mod_param_u32_u16
	.quad	_L_1+741
	.quad	_Mod__uMod_param_u32_u16
	.quad	_L_1+759
	.quad	_Mod__Mod_var_u32_u16
	.quad	_L_1+778
	.quad	_Mod__uMod_var_u32_u16
	.quad	_L_1+794
	.quad	_Mod__Mod_param_L_u32
	.quad	_L_1+811
	.quad	_Mod__uMod_param_L_u32
	.quad	_L_1+827
	.quad	_Mod__Mod_var_L_u32
	.quad	_L_1+844
	.quad	_Mod__uMod_var_L_u32
	.quad	_L_1+858
	.quad	_Mod__Mod_param_L_L
	.quad	_L_1+873
	.quad	_Mod__uMod_param_L_L
	.quad	_L_1+887
	.quad	_Mod__Mod_var_L_L
	.quad	_L_1+902
	.quad	_Mod__uMod_var_L_L
	.quad	_L_1+914
	.quad	_Mod__Mod_param_L_i8
	.quad	_L_1+927
	.quad	_Mod__uMod_param_L_i8
	.quad	_L_1+942
	.quad	_Mod__Mod_var_L_i8
	.quad	_L_1+958
	.quad	_Mod__uMod_var_L_i8
	.quad	_L_1+971
	.quad	_Mod__Mod_param_L_u8
	.quad	_L_1+985
	.quad	_Mod__uMod_param_L_u8
	.quad	_L_1+1000
	.quad	_Mod__Mod_var_L_u8
	.quad	_L_1+1016
	.quad	_Mod__uMod_var_L_u8
	.quad	_L_1+1029
	.quad	_Mod__Mod_param_L_C
	.quad	_L_1+1043
	.quad	_Mod__uMod_param_L_C
	.quad	_L_1+1057
	.quad	_Mod__Mod_var_L_C
	.quad	_L_1+1072
	.quad	_Mod__uMod_var_L_C
	.quad	_L_1+1084
	.quad	_Mod__Mod_param_L_i64
	.quad	_L_1+1097
	.quad	_Mod__uMod_param_L_i64
	.quad	_L_1+1113
	.quad	_Mod__Mod_var_L_i64
	.quad	_L_1+1130
	.quad	_Mod__uMod_var_L_i64
	.quad	_L_1+1144
	.quad	_Mod__Mod_param_L_I
	.quad	_L_1+1159
	.quad	_Mod__uMod_param_L_I
	.quad	_L_1+1173
	.quad	_Mod__Mod_var_L_I
	.quad	_L_1+1188
	.quad	_Mod__uMod_var_L_I
	.quad	_L_1+1200
	.quad	_Mod__Mod_param_L_i32
	.quad	_L_1+1213
	.quad	_Mod__uMod_param_L_i32
	.quad	_L_1+1229
	.quad	_Mod__Mod_var_L_i32
	.quad	_L_1+1246
	.quad	_Mod__uMod_var_L_i32
	.quad	_L_1+1260
	.quad	_Mod__Mod_param_L_i16
	.quad	_L_1+1275
	.quad	_Mod__uMod_param_L_i16
	.quad	_L_1+1291
	.quad	_Mod__Mod_var_L_i16
	.quad	_L_1+1308
	.quad	_Mod__uMod_var_L_i16
	.quad	_L_1+1322
	.quad	_Mod__Mod_param_L_LC
	.quad	_L_1+1337
	.quad	_Mod__uMod_param_L_LC
	.quad	_L_1+1352
	.quad	_Mod__Mod_var_L_LC
	.quad	_L_1+1368
	.quad	_Mod__uMod_var_L_LC
	.quad	_L_1+1381
	.quad	_Mod__Mod_param_L_u64
	.quad	_L_1+1395
	.quad	_Mod__uMod_param_L_u64
	.quad	_L_1+1411
	.quad	_Mod__Mod_var_L_u64
	.quad	_L_1+1428
	.quad	_Mod__uMod_var_L_u64
	.quad	_L_1+1442
	.quad	_Mod__Mod_param_L_u16
	.quad	_L_1+1457
	.quad	_Mod__uMod_param_L_u16
	.quad	_L_1+1473
	.quad	_Mod__Mod_var_L_u16
	.quad	_L_1+1490
	.quad	_Mod__uMod_var_L_u16
	.quad	_L_1+1504
	.quad	_Mod__Mod_param_i8_u32
	.quad	_L_1+1519
	.quad	_Mod__uMod_param_i8_u32
	.quad	_L_1+1536
	.quad	_Mod__Mod_var_i8_u32
	.quad	_L_1+1554
	.quad	_Mod__uMod_var_i8_u32
	.quad	_L_1+1569
	.quad	_Mod__Mod_param_i8_L
	.quad	_L_1+1585
	.quad	_Mod__uMod_param_i8_L
	.quad	_L_1+1600
	.quad	_Mod__Mod_var_i8_L
	.quad	_L_1+1616
	.quad	_Mod__uMod_var_i8_L
	.quad	_L_1+1629
	.quad	_Mod__Mod_param_i8_i8
	.quad	_L_1+1643
	.quad	_Mod__uMod_param_i8_i8
	.quad	_L_1+1659
	.quad	_Mod__Mod_var_i8_i8
	.quad	_L_1+1676
	.quad	_Mod__uMod_var_i8_i8
	.quad	_L_1+1690
	.quad	_Mod__Mod_param_i8_u8
	.quad	_L_1+1705
	.quad	_Mod__uMod_param_i8_u8
	.quad	_L_1+1721
	.quad	_Mod__Mod_var_i8_u8
	.quad	_L_1+1738
	.quad	_Mod__uMod_var_i8_u8
	.quad	_L_1+1752
	.quad	_Mod__Mod_param_i8_C
	.quad	_L_1+1767
	.quad	_Mod__uMod_param_i8_C
	.quad	_L_1+1782
	.quad	_Mod__Mod_var_i8_C
	.quad	_L_1+1798
	.quad	_Mod__uMod_var_i8_C
	.quad	_L_1+1811
	.quad	_Mod__Mod_param_i8_i64
	.quad	_L_1+1825
	.quad	_Mod__uMod_param_i8_i64
	.quad	_L_1+1842
	.quad	_Mod__Mod_var_i8_i64
	.quad	_L_1+1860
	.quad	_Mod__uMod_var_i8_i64
	.quad	_L_1+1875
	.quad	_Mod__Mod_param_i8_I
	.quad	_L_1+1891
	.quad	_Mod__uMod_param_i8_I
	.quad	_L_1+1906
	.quad	_Mod__Mod_var_i8_I
	.quad	_L_1+1922
	.quad	_Mod__uMod_var_i8_I
	.quad	_L_1+1935
	.quad	_Mod__Mod_param_i8_i32
	.quad	_L_1+1949
	.quad	_Mod__uMod_param_i8_i32
	.quad	_L_1+1966
	.quad	_Mod__Mod_var_i8_i32
	.quad	_L_1+1984
	.quad	_Mod__uMod_var_i8_i32
	.quad	_L_1+1999
	.quad	_Mod__Mod_param_i8_i16
	.quad	_L_1+2015
	.quad	_Mod__uMod_param_i8_i16
	.quad	_L_1+2032
	.quad	_Mod__Mod_var_i8_i16
	.quad	_L_1+2050
	.quad	_Mod__uMod_var_i8_i16
	.quad	_L_1+2065
	.quad	_Mod__Mod_param_i8_LC
	.quad	_L_1+2081
	.quad	_Mod__uMod_param_i8_LC
	.quad	_L_1+2097
	.quad	_Mod__Mod_var_i8_LC
	.quad	_L_1+2114
	.quad	_Mod__uMod_var_i8_LC
	.quad	_L_1+2128
	.quad	_Mod__Mod_param_i8_u64
	.quad	_L_1+2143
	.quad	_Mod__uMod_param_i8_u64
	.quad	_L_1+2160
	.quad	_Mod__Mod_var_i8_u64
	.quad	_L_1+2178
	.quad	_Mod__uMod_var_i8_u64
	.quad	_L_1+2193
	.quad	_Mod__Mod_param_i8_u16
	.quad	_L_1+2209
	.quad	_Mod__uMod_param_i8_u16
	.quad	_L_1+2226
	.quad	_Mod__Mod_var_i8_u16
	.quad	_L_1+2244
	.quad	_Mod__uMod_var_i8_u16
	.quad	_L_1+2259
	.quad	_Mod__Mod_param_u8_u32
	.quad	_L_1+2275
	.quad	_Mod__uMod_param_u8_u32
	.quad	_L_1+2292
	.quad	_Mod__Mod_var_u8_u32
	.quad	_L_1+2310
	.quad	_Mod__uMod_var_u8_u32
	.quad	_L_1+2325
	.quad	_Mod__Mod_param_u8_L
	.quad	_L_1+2341
	.quad	_Mod__uMod_param_u8_L
	.quad	_L_1+2356
	.quad	_Mod__Mod_var_u8_L
	.quad	_L_1+2372
	.quad	_Mod__uMod_var_u8_L
	.quad	_L_1+2385
	.quad	_Mod__Mod_param_u8_i8
	.quad	_L_1+2399
	.quad	_Mod__uMod_param_u8_i8
	.quad	_L_1+2415
	.quad	_Mod__Mod_var_u8_i8
	.quad	_L_1+2432
	.quad	_Mod__uMod_var_u8_i8
	.quad	_L_1+2446
	.quad	_Mod__Mod_param_u8_u8
	.quad	_L_1+2461
	.quad	_Mod__uMod_param_u8_u8
	.quad	_L_1+2477
	.quad	_Mod__Mod_var_u8_u8
	.quad	_L_1+2494
	.quad	_Mod__uMod_var_u8_u8
	.quad	_L_1+2508
	.quad	_Mod__Mod_param_u8_C
	.quad	_L_1+2523
	.quad	_Mod__uMod_param_u8_C
	.quad	_L_1+2538
	.quad	_Mod__Mod_var_u8_C
	.quad	_L_1+2554
	.quad	_Mod__uMod_var_u8_C
	.quad	_L_1+2567
	.quad	_Mod__Mod_param_u8_i64
	.quad	_L_1+2581
	.quad	_Mod__uMod_param_u8_i64
	.quad	_L_1+2598
	.quad	_Mod__Mod_var_u8_i64
	.quad	_L_1+2616
	.quad	_Mod__uMod_var_u8_i64
	.quad	_L_1+2631
	.quad	_Mod__Mod_param_u8_I
	.quad	_L_1+2647
	.quad	_Mod__uMod_param_u8_I
	.quad	_L_1+2662
	.quad	_Mod__Mod_var_u8_I
	.quad	_L_1+2678
	.quad	_Mod__uMod_var_u8_I
	.quad	_L_1+2691
	.quad	_Mod__Mod_param_u8_i32
	.quad	_L_1+2705
	.quad	_Mod__uMod_param_u8_i32
	.quad	_L_1+2722
	.quad	_Mod__Mod_var_u8_i32
	.quad	_L_1+2740
	.quad	_Mod__uMod_var_u8_i32
	.quad	_L_1+2755
	.quad	_Mod__Mod_param_u8_i16
	.quad	_L_1+2771
	.quad	_Mod__uMod_param_u8_i16
	.quad	_L_1+2788
	.quad	_Mod__Mod_var_u8_i16
	.quad	_L_1+2806
	.quad	_Mod__uMod_var_u8_i16
	.quad	_L_1+2821
	.quad	_Mod__Mod_param_u8_LC
	.quad	_L_1+2837
	.quad	_Mod__uMod_param_u8_LC
	.quad	_L_1+2853
	.quad	_Mod__Mod_var_u8_LC
	.quad	_L_1+2870
	.quad	_Mod__uMod_var_u8_LC
	.quad	_L_1+2884
	.quad	_Mod__Mod_param_u8_u64
	.quad	_L_1+2899
	.quad	_Mod__uMod_param_u8_u64
	.quad	_L_1+2916
	.quad	_Mod__Mod_var_u8_u64
	.quad	_L_1+2934
	.quad	_Mod__uMod_var_u8_u64
	.quad	_L_1+2949
	.quad	_Mod__Mod_param_u8_u16
	.quad	_L_1+2965
	.quad	_Mod__uMod_param_u8_u16
	.quad	_L_1+2982
	.quad	_Mod__Mod_var_u8_u16
	.quad	_L_1+3000
	.quad	_Mod__uMod_var_u8_u16
	.quad	_L_1+3015
	.quad	_Mod__Mod_param_f32_f32
	.quad	_L_1+3031
	.quad	_Mod__Mod_var_f32_f32
	.quad	_L_1+3049
	.quad	_Mod__Mod_param_C_u32
	.quad	_L_1+3065
	.quad	_Mod__uMod_param_C_u32
	.quad	_L_1+3081
	.quad	_Mod__Mod_var_C_u32
	.quad	_L_1+3098
	.quad	_Mod__uMod_var_C_u32
	.quad	_L_1+3112
	.quad	_Mod__Mod_param_C_L
	.quad	_L_1+3127
	.quad	_Mod__uMod_param_C_L
	.quad	_L_1+3141
	.quad	_Mod__Mod_var_C_L
	.quad	_L_1+3156
	.quad	_Mod__uMod_var_C_L
	.quad	_L_1+3168
	.quad	_Mod__Mod_param_C_i8
	.quad	_L_1+3181
	.quad	_Mod__uMod_param_C_i8
	.quad	_L_1+3196
	.quad	_Mod__Mod_var_C_i8
	.quad	_L_1+3212
	.quad	_Mod__uMod_var_C_i8
	.quad	_L_1+3225
	.quad	_Mod__Mod_param_C_u8
	.quad	_L_1+3239
	.quad	_Mod__uMod_param_C_u8
	.quad	_L_1+3254
	.quad	_Mod__Mod_var_C_u8
	.quad	_L_1+3270
	.quad	_Mod__uMod_var_C_u8
	.quad	_L_1+3283
	.quad	_Mod__Mod_param_C_C
	.quad	_L_1+3297
	.quad	_Mod__uMod_param_C_C
	.quad	_L_1+3311
	.quad	_Mod__Mod_var_C_C
	.quad	_L_1+3326
	.quad	_Mod__uMod_var_C_C
	.quad	_L_1+3338
	.quad	_Mod__Mod_param_C_i64
	.quad	_L_1+3351
	.quad	_Mod__uMod_param_C_i64
	.quad	_L_1+3367
	.quad	_Mod__Mod_var_C_i64
	.quad	_L_1+3384
	.quad	_Mod__uMod_var_C_i64
	.quad	_L_1+3398
	.quad	_Mod__Mod_param_C_I
	.quad	_L_1+3413
	.quad	_Mod__uMod_param_C_I
	.quad	_L_1+3427
	.quad	_Mod__Mod_var_C_I
	.quad	_L_1+3442
	.quad	_Mod__uMod_var_C_I
	.quad	_L_1+3454
	.quad	_Mod__Mod_param_C_i32
	.quad	_L_1+3467
	.quad	_Mod__uMod_param_C_i32
	.quad	_L_1+3483
	.quad	_Mod__Mod_var_C_i32
	.quad	_L_1+3500
	.quad	_Mod__uMod_var_C_i32
	.quad	_L_1+3514
	.quad	_Mod__Mod_param_C_i16
	.quad	_L_1+3529
	.quad	_Mod__uMod_param_C_i16
	.quad	_L_1+3545
	.quad	_Mod__Mod_var_C_i16
	.quad	_L_1+3562
	.quad	_Mod__uMod_var_C_i16
	.quad	_L_1+3576
	.quad	_Mod__Mod_param_C_LC
	.quad	_L_1+3591
	.quad	_Mod__uMod_param_C_LC
	.quad	_L_1+3606
	.quad	_Mod__Mod_var_C_LC
	.quad	_L_1+3622
	.quad	_Mod__uMod_var_C_LC
	.quad	_L_1+3635
	.quad	_Mod__Mod_param_C_u64
	.quad	_L_1+3649
	.quad	_Mod__uMod_param_C_u64
	.quad	_L_1+3665
	.quad	_Mod__Mod_var_C_u64
	.quad	_L_1+3682
	.quad	_Mod__uMod_var_C_u64
	.quad	_L_1+3696
	.quad	_Mod__Mod_param_C_u16
	.quad	_L_1+3711
	.quad	_Mod__uMod_param_C_u16
	.quad	_L_1+3727
	.quad	_Mod__Mod_var_C_u16
	.quad	_L_1+3744
	.quad	_Mod__uMod_var_C_u16
	.quad	_L_1+3758
	.quad	_Mod__Mod_param_i64_u32
	.quad	_L_1+3773
	.quad	_Mod__uMod_param_i64_u32
	.quad	_L_1+3791
	.quad	_Mod__Mod_var_i64_u32
	.quad	_L_1+3810
	.quad	_Mod__uMod_var_i64_u32
	.quad	_L_1+3826
	.quad	_Mod__Mod_param_i64_L
	.quad	_L_1+3843
	.quad	_Mod__uMod_param_i64_L
	.quad	_L_1+3859
	.quad	_Mod__Mod_var_i64_L
	.quad	_L_1+3876
	.quad	_Mod__uMod_var_i64_L
	.quad	_L_1+3890
	.quad	_Mod__Mod_param_i64_i8
	.quad	_L_1+3905
	.quad	_Mod__uMod_param_i64_i8
	.quad	_L_1+3922
	.quad	_Mod__Mod_var_i64_i8
	.quad	_L_1+3940
	.quad	_Mod__uMod_var_i64_i8
	.quad	_L_1+3955
	.quad	_Mod__Mod_param_i64_u8
	.quad	_L_1+3971
	.quad	_Mod__uMod_param_i64_u8
	.quad	_L_1+3988
	.quad	_Mod__Mod_var_i64_u8
	.quad	_L_1+4006
	.quad	_Mod__uMod_var_i64_u8
	.quad	_L_1+4021
	.quad	_Mod__Mod_param_i64_C
	.quad	_L_1+4037
	.quad	_Mod__uMod_param_i64_C
	.quad	_L_1+4053
	.quad	_Mod__Mod_var_i64_C
	.quad	_L_1+4070
	.quad	_Mod__uMod_var_i64_C
	.quad	_L_1+4084
	.quad	_Mod__Mod_param_i64_i64
	.quad	_L_1+4099
	.quad	_Mod__uMod_param_i64_i64
	.quad	_L_1+4117
	.quad	_Mod__Mod_var_i64_i64
	.quad	_L_1+4136
	.quad	_Mod__uMod_var_i64_i64
	.quad	_L_1+4152
	.quad	_Mod__Mod_param_i64_I
	.quad	_L_1+4169
	.quad	_Mod__uMod_param_i64_I
	.quad	_L_1+4185
	.quad	_Mod__Mod_var_i64_I
	.quad	_L_1+4202
	.quad	_Mod__uMod_var_i64_I
	.quad	_L_1+4216
	.quad	_Mod__Mod_param_i64_i32
	.quad	_L_1+4231
	.quad	_Mod__uMod_param_i64_i32
	.quad	_L_1+4249
	.quad	_Mod__Mod_var_i64_i32
	.quad	_L_1+4268
	.quad	_Mod__uMod_var_i64_i32
	.quad	_L_1+4284
	.quad	_Mod__Mod_param_i64_i16
	.quad	_L_1+4301
	.quad	_Mod__uMod_param_i64_i16
	.quad	_L_1+4319
	.quad	_Mod__Mod_var_i64_i16
	.quad	_L_1+4338
	.quad	_Mod__uMod_var_i64_i16
	.quad	_L_1+4354
	.quad	_Mod__Mod_param_i64_LC
	.quad	_L_1+4371
	.quad	_Mod__uMod_param_i64_LC
	.quad	_L_1+4388
	.quad	_Mod__Mod_var_i64_LC
	.quad	_L_1+4406
	.quad	_Mod__uMod_var_i64_LC
	.quad	_L_1+4421
	.quad	_Mod__Mod_param_i64_u64
	.quad	_L_1+4437
	.quad	_Mod__uMod_param_i64_u64
	.quad	_L_1+4455
	.quad	_Mod__Mod_var_i64_u64
	.quad	_L_1+4474
	.quad	_Mod__uMod_var_i64_u64
	.quad	_L_1+4490
	.quad	_Mod__Mod_param_i64_u16
	.quad	_L_1+4507
	.quad	_Mod__uMod_param_i64_u16
	.quad	_L_1+4525
	.quad	_Mod__Mod_var_i64_u16
	.quad	_L_1+4544
	.quad	_Mod__uMod_var_i64_u16
	.quad	_L_1+4560
	.quad	_Mod__Mod_param_I_u32
	.quad	_L_1+4577
	.quad	_Mod__uMod_param_I_u32
	.quad	_L_1+4593
	.quad	_Mod__Mod_var_I_u32
	.quad	_L_1+4610
	.quad	_Mod__uMod_var_I_u32
	.quad	_L_1+4624
	.quad	_Mod__Mod_param_I_L
	.quad	_L_1+4639
	.quad	_Mod__uMod_param_I_L
	.quad	_L_1+4653
	.quad	_Mod__Mod_var_I_L
	.quad	_L_1+4668
	.quad	_Mod__uMod_var_I_L
	.quad	_L_1+4680
	.quad	_Mod__Mod_param_I_i8
	.quad	_L_1+4693
	.quad	_Mod__uMod_param_I_i8
	.quad	_L_1+4708
	.quad	_Mod__Mod_var_I_i8
	.quad	_L_1+4724
	.quad	_Mod__uMod_var_I_i8
	.quad	_L_1+4737
	.quad	_Mod__Mod_param_I_u8
	.quad	_L_1+4751
	.quad	_Mod__uMod_param_I_u8
	.quad	_L_1+4766
	.quad	_Mod__Mod_var_I_u8
	.quad	_L_1+4782
	.quad	_Mod__uMod_var_I_u8
	.quad	_L_1+4795
	.quad	_Mod__Mod_param_I_C
	.quad	_L_1+4809
	.quad	_Mod__uMod_param_I_C
	.quad	_L_1+4823
	.quad	_Mod__Mod_var_I_C
	.quad	_L_1+4838
	.quad	_Mod__uMod_var_I_C
	.quad	_L_1+4850
	.quad	_Mod__Mod_param_I_i64
	.quad	_L_1+4863
	.quad	_Mod__uMod_param_I_i64
	.quad	_L_1+4879
	.quad	_Mod__Mod_var_I_i64
	.quad	_L_1+4896
	.quad	_Mod__uMod_var_I_i64
	.quad	_L_1+4910
	.quad	_Mod__Mod_param_I_I
	.quad	_L_1+4925
	.quad	_Mod__uMod_param_I_I
	.quad	_L_1+4939
	.quad	_Mod__Mod_var_I_I
	.quad	_L_1+4954
	.quad	_Mod__uMod_var_I_I
	.quad	_L_1+4966
	.quad	_Mod__Mod_param_I_i32
	.quad	_L_1+4979
	.quad	_Mod__uMod_param_I_i32
	.quad	_L_1+4995
	.quad	_Mod__Mod_var_I_i32
	.quad	_L_1+5012
	.quad	_Mod__uMod_var_I_i32
	.quad	_L_1+5026
	.quad	_Mod__Mod_param_I_i16
	.quad	_L_1+5041
	.quad	_Mod__uMod_param_I_i16
	.quad	_L_1+5057
	.quad	_Mod__Mod_var_I_i16
	.quad	_L_1+5074
	.quad	_Mod__uMod_var_I_i16
	.quad	_L_1+5088
	.quad	_Mod__Mod_param_I_LC
	.quad	_L_1+5103
	.quad	_Mod__uMod_param_I_LC
	.quad	_L_1+5118
	.quad	_Mod__Mod_var_I_LC
	.quad	_L_1+5134
	.quad	_Mod__uMod_var_I_LC
	.quad	_L_1+5147
	.quad	_Mod__Mod_param_I_u64
	.quad	_L_1+5161
	.quad	_Mod__uMod_param_I_u64
	.quad	_L_1+5177
	.quad	_Mod__Mod_var_I_u64
	.quad	_L_1+5194
	.quad	_Mod__uMod_var_I_u64
	.quad	_L_1+5208
	.quad	_Mod__Mod_param_I_u16
	.quad	_L_1+5223
	.quad	_Mod__uMod_param_I_u16
	.quad	_L_1+5239
	.quad	_Mod__Mod_var_I_u16
	.quad	_L_1+5256
	.quad	_Mod__uMod_var_I_u16
	.quad	_L_1+5270
	.quad	_Mod__Mod_param_i32_u32
	.quad	_L_1+5285
	.quad	_Mod__uMod_param_i32_u32
	.quad	_L_1+5303
	.quad	_Mod__Mod_var_i32_u32
	.quad	_L_1+5322
	.quad	_Mod__uMod_var_i32_u32
	.quad	_L_1+5338
	.quad	_Mod__Mod_param_i32_L
	.quad	_L_1+5355
	.quad	_Mod__uMod_param_i32_L
	.quad	_L_1+5371
	.quad	_Mod__Mod_var_i32_L
	.quad	_L_1+5388
	.quad	_Mod__uMod_var_i32_L
	.quad	_L_1+5402
	.quad	_Mod__Mod_param_i32_i8
	.quad	_L_1+5417
	.quad	_Mod__uMod_param_i32_i8
	.quad	_L_1+5434
	.quad	_Mod__Mod_var_i32_i8
	.quad	_L_1+5452
	.quad	_Mod__uMod_var_i32_i8
	.quad	_L_1+5467
	.quad	_Mod__Mod_param_i32_u8
	.quad	_L_1+5483
	.quad	_Mod__uMod_param_i32_u8
	.quad	_L_1+5500
	.quad	_Mod__Mod_var_i32_u8
	.quad	_L_1+5518
	.quad	_Mod__uMod_var_i32_u8
	.quad	_L_1+5533
	.quad	_Mod__Mod_param_i32_C
	.quad	_L_1+5549
	.quad	_Mod__uMod_param_i32_C
	.quad	_L_1+5565
	.quad	_Mod__Mod_var_i32_C
	.quad	_L_1+5582
	.quad	_Mod__uMod_var_i32_C
	.quad	_L_1+5596
	.quad	_Mod__Mod_param_i32_i64
	.quad	_L_1+5611
	.quad	_Mod__uMod_param_i32_i64
	.quad	_L_1+5629
	.quad	_Mod__Mod_var_i32_i64
	.quad	_L_1+5648
	.quad	_Mod__uMod_var_i32_i64
	.quad	_L_1+5664
	.quad	_Mod__Mod_param_i32_I
	.quad	_L_1+5681
	.quad	_Mod__uMod_param_i32_I
	.quad	_L_1+5697
	.quad	_Mod__Mod_var_i32_I
	.quad	_L_1+5714
	.quad	_Mod__uMod_var_i32_I
	.quad	_L_1+5728
	.quad	_Mod__Mod_param_i32_i32
	.quad	_L_1+5743
	.quad	_Mod__uMod_param_i32_i32
	.quad	_L_1+5761
	.quad	_Mod__Mod_var_i32_i32
	.quad	_L_1+5780
	.quad	_Mod__uMod_var_i32_i32
	.quad	_L_1+5796
	.quad	_Mod__Mod_param_i32_i16
	.quad	_L_1+5813
	.quad	_Mod__uMod_param_i32_i16
	.quad	_L_1+5831
	.quad	_Mod__Mod_var_i32_i16
	.quad	_L_1+5850
	.quad	_Mod__uMod_var_i32_i16
	.quad	_L_1+5866
	.quad	_Mod__Mod_param_i32_LC
	.quad	_L_1+5883
	.quad	_Mod__uMod_param_i32_LC
	.quad	_L_1+5900
	.quad	_Mod__Mod_var_i32_LC
	.quad	_L_1+5918
	.quad	_Mod__uMod_var_i32_LC
	.quad	_L_1+5933
	.quad	_Mod__Mod_param_i32_u64
	.quad	_L_1+5949
	.quad	_Mod__uMod_param_i32_u64
	.quad	_L_1+5967
	.quad	_Mod__Mod_var_i32_u64
	.quad	_L_1+5986
	.quad	_Mod__uMod_var_i32_u64
	.quad	_L_1+6002
	.quad	_Mod__Mod_param_i32_u16
	.quad	_L_1+6019
	.quad	_Mod__uMod_param_i32_u16
	.quad	_L_1+6037
	.quad	_Mod__Mod_var_i32_u16
	.quad	_L_1+6056
	.quad	_Mod__uMod_var_i32_u16
	.quad	_L_1+6072
	.quad	_Mod__Mod_param_i16_u32
	.quad	_L_1+6089
	.quad	_Mod__uMod_param_i16_u32
	.quad	_L_1+6107
	.quad	_Mod__Mod_var_i16_u32
	.quad	_L_1+6126
	.quad	_Mod__uMod_var_i16_u32
	.quad	_L_1+6142
	.quad	_Mod__Mod_param_i16_L
	.quad	_L_1+6159
	.quad	_Mod__uMod_param_i16_L
	.quad	_L_1+6175
	.quad	_Mod__Mod_var_i16_L
	.quad	_L_1+6192
	.quad	_Mod__uMod_var_i16_L
	.quad	_L_1+6206
	.quad	_Mod__Mod_param_i16_i8
	.quad	_L_1+6221
	.quad	_Mod__uMod_param_i16_i8
	.quad	_L_1+6238
	.quad	_Mod__Mod_var_i16_i8
	.quad	_L_1+6256
	.quad	_Mod__uMod_var_i16_i8
	.quad	_L_1+6271
	.quad	_Mod__Mod_param_i16_u8
	.quad	_L_1+6287
	.quad	_Mod__uMod_param_i16_u8
	.quad	_L_1+6304
	.quad	_Mod__Mod_var_i16_u8
	.quad	_L_1+6322
	.quad	_Mod__uMod_var_i16_u8
	.quad	_L_1+6337
	.quad	_Mod__Mod_param_i16_C
	.quad	_L_1+6353
	.quad	_Mod__uMod_param_i16_C
	.quad	_L_1+6369
	.quad	_Mod__Mod_var_i16_C
	.quad	_L_1+6386
	.quad	_Mod__uMod_var_i16_C
	.quad	_L_1+6400
	.quad	_Mod__Mod_param_i16_i64
	.quad	_L_1+6415
	.quad	_Mod__uMod_param_i16_i64
	.quad	_L_1+6433
	.quad	_Mod__Mod_var_i16_i64
	.quad	_L_1+6452
	.quad	_Mod__uMod_var_i16_i64
	.quad	_L_1+6468
	.quad	_Mod__Mod_param_i16_I
	.quad	_L_1+6485
	.quad	_Mod__uMod_param_i16_I
	.quad	_L_1+6501
	.quad	_Mod__Mod_var_i16_I
	.quad	_L_1+6518
	.quad	_Mod__uMod_var_i16_I
	.quad	_L_1+6532
	.quad	_Mod__Mod_param_i16_i32
	.quad	_L_1+6547
	.quad	_Mod__uMod_param_i16_i32
	.quad	_L_1+6565
	.quad	_Mod__Mod_var_i16_i32
	.quad	_L_1+6584
	.quad	_Mod__uMod_var_i16_i32
	.quad	_L_1+6600
	.quad	_Mod__Mod_param_i16_i16
	.quad	_L_1+6617
	.quad	_Mod__uMod_param_i16_i16
	.quad	_L_1+6635
	.quad	_Mod__Mod_var_i16_i16
	.quad	_L_1+6654
	.quad	_Mod__uMod_var_i16_i16
	.quad	_L_1+6670
	.quad	_Mod__Mod_param_i16_LC
	.quad	_L_1+6687
	.quad	_Mod__uMod_param_i16_LC
	.quad	_L_1+6704
	.quad	_Mod__Mod_var_i16_LC
	.quad	_L_1+6722
	.quad	_Mod__uMod_var_i16_LC
	.quad	_L_1+6737
	.quad	_Mod__Mod_param_i16_u64
	.quad	_L_1+6753
	.quad	_Mod__uMod_param_i16_u64
	.quad	_L_1+6771
	.quad	_Mod__Mod_var_i16_u64
	.quad	_L_1+6790
	.quad	_Mod__uMod_var_i16_u64
	.quad	_L_1+6806
	.quad	_Mod__Mod_param_i16_u16
	.quad	_L_1+6823
	.quad	_Mod__uMod_param_i16_u16
	.quad	_L_1+6841
	.quad	_Mod__Mod_var_i16_u16
	.quad	_L_1+6860
	.quad	_Mod__uMod_var_i16_u16
	.quad	_L_1+6876
	.quad	_Mod__Mod_param_f64_f64
	.quad	_L_1+6893
	.quad	_Mod__Mod_var_f64_f64
	.quad	_L_1+6911
	.quad	_Mod__Mod_param_LC_u32
	.quad	_L_1+6927
	.quad	_Mod__uMod_param_LC_u32
	.quad	_L_1+6944
	.quad	_Mod__Mod_var_LC_u32
	.quad	_L_1+6962
	.quad	_Mod__uMod_var_LC_u32
	.quad	_L_1+6977
	.quad	_Mod__Mod_param_LC_L
	.quad	_L_1+6993
	.quad	_Mod__uMod_param_LC_L
	.quad	_L_1+7008
	.quad	_Mod__Mod_var_LC_L
	.quad	_L_1+7024
	.quad	_Mod__uMod_var_LC_L
	.quad	_L_1+7037
	.quad	_Mod__Mod_param_LC_i8
	.quad	_L_1+7051
	.quad	_Mod__uMod_param_LC_i8
	.quad	_L_1+7067
	.quad	_Mod__Mod_var_LC_i8
	.quad	_L_1+7084
	.quad	_Mod__uMod_var_LC_i8
	.quad	_L_1+7098
	.quad	_Mod__Mod_param_LC_u8
	.quad	_L_1+7113
	.quad	_Mod__uMod_param_LC_u8
	.quad	_L_1+7129
	.quad	_Mod__Mod_var_LC_u8
	.quad	_L_1+7146
	.quad	_Mod__uMod_var_LC_u8
	.quad	_L_1+7160
	.quad	_Mod__Mod_param_LC_C
	.quad	_L_1+7175
	.quad	_Mod__uMod_param_LC_C
	.quad	_L_1+7190
	.quad	_Mod__Mod_var_LC_C
	.quad	_L_1+7206
	.quad	_Mod__uMod_var_LC_C
	.quad	_L_1+7219
	.quad	_Mod__Mod_param_LC_i64
	.quad	_L_1+7233
	.quad	_Mod__uMod_param_LC_i64
	.quad	_L_1+7250
	.quad	_Mod__Mod_var_LC_i64
	.quad	_L_1+7268
	.quad	_Mod__uMod_var_LC_i64
	.quad	_L_1+7283
	.quad	_Mod__Mod_param_LC_I
	.quad	_L_1+7299
	.quad	_Mod__uMod_param_LC_I
	.quad	_L_1+7314
	.quad	_Mod__Mod_var_LC_I
	.quad	_L_1+7330
	.quad	_Mod__uMod_var_LC_I
	.quad	_L_1+7343
	.quad	_Mod__Mod_param_LC_i32
	.quad	_L_1+7357
	.quad	_Mod__uMod_param_LC_i32
	.quad	_L_1+7374
	.quad	_Mod__Mod_var_LC_i32
	.quad	_L_1+7392
	.quad	_Mod__uMod_var_LC_i32
	.quad	_L_1+7407
	.quad	_Mod__Mod_param_LC_i16
	.quad	_L_1+7423
	.quad	_Mod__uMod_param_LC_i16
	.quad	_L_1+7440
	.quad	_Mod__Mod_var_LC_i16
	.quad	_L_1+7458
	.quad	_Mod__uMod_var_LC_i16
	.quad	_L_1+7473
	.quad	_Mod__Mod_param_LC_LC
	.quad	_L_1+7489
	.quad	_Mod__uMod_param_LC_LC
	.quad	_L_1+7505
	.quad	_Mod__Mod_var_LC_LC
	.quad	_L_1+7522
	.quad	_Mod__uMod_var_LC_LC
	.quad	_L_1+7536
	.quad	_Mod__Mod_param_LC_u64
	.quad	_L_1+7551
	.quad	_Mod__uMod_param_LC_u64
	.quad	_L_1+7568
	.quad	_Mod__Mod_var_LC_u64
	.quad	_L_1+7586
	.quad	_Mod__uMod_var_LC_u64
	.quad	_L_1+7601
	.quad	_Mod__Mod_param_LC_u16
	.quad	_L_1+7617
	.quad	_Mod__uMod_param_LC_u16
	.quad	_L_1+7634
	.quad	_Mod__Mod_var_LC_u16
	.quad	_L_1+7652
	.quad	_Mod__uMod_var_LC_u16
	.quad	_L_1+7667
	.quad	_Mod__Mod_param_u64_u32
	.quad	_L_1+7683
	.quad	_Mod__uMod_param_u64_u32
	.quad	_L_1+7701
	.quad	_Mod__Mod_var_u64_u32
	.quad	_L_1+7720
	.quad	_Mod__uMod_var_u64_u32
	.quad	_L_1+7736
	.quad	_Mod__Mod_param_u64_L
	.quad	_L_1+7753
	.quad	_Mod__uMod_param_u64_L
	.quad	_L_1+7769
	.quad	_Mod__Mod_var_u64_L
	.quad	_L_1+7786
	.quad	_Mod__uMod_var_u64_L
	.quad	_L_1+7800
	.quad	_Mod__Mod_param_u64_i8
	.quad	_L_1+7815
	.quad	_Mod__uMod_param_u64_i8
	.quad	_L_1+7832
	.quad	_Mod__Mod_var_u64_i8
	.quad	_L_1+7850
	.quad	_Mod__uMod_var_u64_i8
	.quad	_L_1+7865
	.quad	_Mod__Mod_param_u64_u8
	.quad	_L_1+7881
	.quad	_Mod__uMod_param_u64_u8
	.quad	_L_1+7898
	.quad	_Mod__Mod_var_u64_u8
	.quad	_L_1+7916
	.quad	_Mod__uMod_var_u64_u8
	.quad	_L_1+7931
	.quad	_Mod__Mod_param_u64_C
	.quad	_L_1+7947
	.quad	_Mod__uMod_param_u64_C
	.quad	_L_1+7963
	.quad	_Mod__Mod_var_u64_C
	.quad	_L_1+7980
	.quad	_Mod__uMod_var_u64_C
	.quad	_L_1+7994
	.quad	_Mod__Mod_param_u64_i64
	.quad	_L_1+8009
	.quad	_Mod__uMod_param_u64_i64
	.quad	_L_1+8027
	.quad	_Mod__Mod_var_u64_i64
	.quad	_L_1+8046
	.quad	_Mod__uMod_var_u64_i64
	.quad	_L_1+8062
	.quad	_Mod__Mod_param_u64_I
	.quad	_L_1+8079
	.quad	_Mod__uMod_param_u64_I
	.quad	_L_1+8095
	.quad	_Mod__Mod_var_u64_I
	.quad	_L_1+8112
	.quad	_Mod__uMod_var_u64_I
	.quad	_L_1+8126
	.quad	_Mod__Mod_param_u64_i32
	.quad	_L_1+8141
	.quad	_Mod__uMod_param_u64_i32
	.quad	_L_1+8159
	.quad	_Mod__Mod_var_u64_i32
	.quad	_L_1+8178
	.quad	_Mod__uMod_var_u64_i32
	.quad	_L_1+8194
	.quad	_Mod__Mod_param_u64_i16
	.quad	_L_1+8211
	.quad	_Mod__uMod_param_u64_i16
	.quad	_L_1+8229
	.quad	_Mod__Mod_var_u64_i16
	.quad	_L_1+8248
	.quad	_Mod__uMod_var_u64_i16
	.quad	_L_1+8264
	.quad	_Mod__Mod_param_u64_LC
	.quad	_L_1+8281
	.quad	_Mod__uMod_param_u64_LC
	.quad	_L_1+8298
	.quad	_Mod__Mod_var_u64_LC
	.quad	_L_1+8316
	.quad	_Mod__uMod_var_u64_LC
	.quad	_L_1+8331
	.quad	_Mod__Mod_param_u64_u64
	.quad	_L_1+8347
	.quad	_Mod__uMod_param_u64_u64
	.quad	_L_1+8365
	.quad	_Mod__Mod_var_u64_u64
	.quad	_L_1+8384
	.quad	_Mod__uMod_var_u64_u64
	.quad	_L_1+8400
	.quad	_Mod__Mod_param_u64_u16
	.quad	_L_1+8417
	.quad	_Mod__uMod_param_u64_u16
	.quad	_L_1+8435
	.quad	_Mod__Mod_var_u64_u16
	.quad	_L_1+8454
	.quad	_Mod__uMod_var_u64_u16
	.quad	_L_1+8470
	.quad	_Mod__Mod_param_u16_u32
	.quad	_L_1+8487
	.quad	_Mod__uMod_param_u16_u32
	.quad	_L_1+8505
	.quad	_Mod__Mod_var_u16_u32
	.quad	_L_1+8524
	.quad	_Mod__uMod_var_u16_u32
	.quad	_L_1+8540
	.quad	_Mod__Mod_param_u16_L
	.quad	_L_1+8557
	.quad	_Mod__uMod_param_u16_L
	.quad	_L_1+8573
	.quad	_Mod__Mod_var_u16_L
	.quad	_L_1+8590
	.quad	_Mod__uMod_var_u16_L
	.quad	_L_1+8604
	.quad	_Mod__Mod_param_u16_i8
	.quad	_L_1+8619
	.quad	_Mod__uMod_param_u16_i8
	.quad	_L_1+8636
	.quad	_Mod__Mod_var_u16_i8
	.quad	_L_1+8654
	.quad	_Mod__uMod_var_u16_i8
	.quad	_L_1+8669
	.quad	_Mod__Mod_param_u16_u8
	.quad	_L_1+8685
	.quad	_Mod__uMod_param_u16_u8
	.quad	_L_1+8702
	.quad	_Mod__Mod_var_u16_u8
	.quad	_L_1+8720
	.quad	_Mod__uMod_var_u16_u8
	.quad	_L_1+8735
	.quad	_Mod__Mod_param_u16_C
	.quad	_L_1+8751
	.quad	_Mod__uMod_param_u16_C
	.quad	_L_1+8767
	.quad	_Mod__Mod_var_u16_C
	.quad	_L_1+8784
	.quad	_Mod__uMod_var_u16_C
	.quad	_L_1+8798
	.quad	_Mod__Mod_param_u16_i64
	.quad	_L_1+8813
	.quad	_Mod__uMod_param_u16_i64
	.quad	_L_1+8831
	.quad	_Mod__Mod_var_u16_i64
	.quad	_L_1+8850
	.quad	_Mod__uMod_var_u16_i64
	.quad	_L_1+8866
	.quad	_Mod__Mod_param_u16_I
	.quad	_L_1+8883
	.quad	_Mod__uMod_param_u16_I
	.quad	_L_1+8899
	.quad	_Mod__Mod_var_u16_I
	.quad	_L_1+8916
	.quad	_Mod__uMod_var_u16_I
	.quad	_L_1+8930
	.quad	_Mod__Mod_param_u16_i32
	.quad	_L_1+8945
	.quad	_Mod__uMod_param_u16_i32
	.quad	_L_1+8963
	.quad	_Mod__Mod_var_u16_i32
	.quad	_L_1+8982
	.quad	_Mod__uMod_var_u16_i32
	.quad	_L_1+8998
	.quad	_Mod__Mod_param_u16_i16
	.quad	_L_1+9015
	.quad	_Mod__uMod_param_u16_i16
	.quad	_L_1+9033
	.quad	_Mod__Mod_var_u16_i16
	.quad	_L_1+9052
	.quad	_Mod__uMod_var_u16_i16
	.quad	_L_1+9068
	.quad	_Mod__Mod_param_u16_LC
	.quad	_L_1+9085
	.quad	_Mod__uMod_param_u16_LC
	.quad	_L_1+9102
	.quad	_Mod__Mod_var_u16_LC
	.quad	_L_1+9120
	.quad	_Mod__uMod_var_u16_LC
	.quad	_L_1+9135
	.quad	_Mod__Mod_param_u16_u64
	.quad	_L_1+9151
	.quad	_Mod__uMod_param_u16_u64
	.quad	_L_1+9169
	.quad	_Mod__Mod_var_u16_u64
	.quad	_L_1+9188
	.quad	_Mod__uMod_var_u16_u64
	.quad	_L_1+9204
	.quad	_Mod__Mod_param_u16_u16
	.quad	_L_1+9221
	.quad	_Mod__uMod_param_u16_u16
	.quad	_L_1+9239
	.quad	_Mod__Mod_var_u16_u16
	.quad	_L_1+9258
	.quad	_Mod__uMod_var_u16_u16
	.quad	_L_1+9274
	.space 8
	.ascii "../AMD64_DARWIN/Mod.m3"
	.space 2
	.data
	.align 5
_MM_Mod:
	.quad	_L_1+18600
	.space 32
	.quad	_L_1+9296
	.space 24
	.quad	_MM_Mod+208
	.space 8
	.quad	_Mod_M3
	.quad	3
	.word	273
	.space 6
	.quad	274
	.quad	275
	.long	2542620639
	.long	1081164910
	.word	278
	.space 2
	.long	279
	.quad	280
	.quad	281
	.quad	282
	.long	1133356122
	.byte	29
	.byte	30
	.space 2
	.quad	287
	.long	288
	.space 28
	.quad	_Mod_I3
	.quad	_MM_Mod+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Mod+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Mod+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Mod+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
