	.text
.globl _Mod__uMod_var_i64_i64
	.private_extern _Mod__uMod_var_i64_i64
_Mod__uMod_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
	je	L9
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L9
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L9:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_LC
	.private_extern _Mod__uMod_var_i64_LC
_Mod__uMod_var_i64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L14
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L14
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L14:
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
	je	L19
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L19
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L19:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u8
	.private_extern _Mod__uMod_var_i64_u8
_Mod__uMod_var_i64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L24
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L24
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L24:
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
	je	L29
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L29
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L29:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_I
	.private_extern _Mod__uMod_var_i64_I
_Mod__uMod_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L34
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L34
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L34:
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
	je	L39
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L39
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L39:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i8
	.private_extern _Mod__uMod_var_i64_i8
_Mod__uMod_var_i64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L44
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L44
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L44:
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
	je	L49
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L49
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L49:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u32
	.private_extern _Mod__uMod_var_i64_u32
_Mod__uMod_var_i64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L54
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L54
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L54:
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
	je	L59
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L59
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L59:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_C
	.private_extern _Mod__uMod_var_i64_C
_Mod__uMod_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L64
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L64
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L64:
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
	je	L69
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L69
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L69:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u16
	.private_extern _Mod__uMod_var_i64_u16
_Mod__uMod_var_i64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L74
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L74
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L74:
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
	je	L79
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L79
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L79:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_u64
	.private_extern _Mod__uMod_var_i64_u64
_Mod__uMod_var_i64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L84
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L84
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L84:
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
	je	L89
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L89
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L89:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_L
	.private_extern _Mod__uMod_var_i64_L
_Mod__uMod_var_i64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L94
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L94
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L94:
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
	je	L99
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L99
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L99:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i16
	.private_extern _Mod__uMod_var_i64_i16
_Mod__uMod_var_i64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L104
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L104
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L104:
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
	je	L109
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L109
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L109:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i64_i32
	.private_extern _Mod__uMod_var_i64_i32
_Mod__uMod_var_i64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L114
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L114
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L114:
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
	je	L119
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L119
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L119:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i64
	.private_extern _Mod__uMod_var_LC_i64
_Mod__uMod_var_LC_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L124
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L124
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L124:
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
	je	L129
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L129
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L129:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_LC
	.private_extern _Mod__uMod_var_LC_LC
_Mod__uMod_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L134
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L134
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L134:
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
	je	L139
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L139
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L139:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u8
	.private_extern _Mod__uMod_var_LC_u8
_Mod__uMod_var_LC_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L144
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L144
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L144:
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
	je	L149
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L149
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L149:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_I
	.private_extern _Mod__uMod_var_LC_I
_Mod__uMod_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L154
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L154
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L154:
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
	je	L159
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L159
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L159:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i8
	.private_extern _Mod__uMod_var_LC_i8
_Mod__uMod_var_LC_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L164
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L164
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L164:
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
	je	L169
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L169
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L169:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u32
	.private_extern _Mod__uMod_var_LC_u32
_Mod__uMod_var_LC_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L174
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L174
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L174:
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
	je	L179
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L179
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L179:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_C
	.private_extern _Mod__uMod_var_LC_C
_Mod__uMod_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L184
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L184
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L184:
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
	je	L189
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L189
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L189:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u16
	.private_extern _Mod__uMod_var_LC_u16
_Mod__uMod_var_LC_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L194
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L194
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L194:
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
	je	L199
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L199
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L199:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_u64
	.private_extern _Mod__uMod_var_LC_u64
_Mod__uMod_var_LC_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movq	160+_MM_Mod(%rip), %rax
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
	je	L204
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L204
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L204:
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
	je	L209
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L209
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L209:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_L
	.private_extern _Mod__uMod_var_LC_L
_Mod__uMod_var_LC_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L214
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L214
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L214:
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
	je	L219
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L219
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L219:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i16
	.private_extern _Mod__uMod_var_LC_i16
_Mod__uMod_var_LC_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L224
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L224
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L224:
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
	je	L229
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L229
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L229:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_LC_i32
	.private_extern _Mod__uMod_var_LC_i32
_Mod__uMod_var_LC_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Mod(%rip), %rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L234
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L234
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L234:
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
	je	L239
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L239
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L239:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__Mod_var_f32_f32
	.private_extern _Mod__Mod_var_f32_f32
_Mod__Mod_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	120+_MM_Mod(%rip), %eax
	movl	%eax, -8(%rbp)
	movl	120+_MM_Mod(%rip), %eax
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
	jb	L247
L246:
	cvtsi2ssq	-24(%rbp), %xmm0
	movss	%xmm0, -36(%rbp)
	jmp	L244
L247:
	movq	-24(%rbp), %rax
	subq	$1, %rax
	cvtsi2ssq	%rax, %xmm0
	movss	%xmm0, -36(%rbp)
L244:
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
	jb	L254
L253:
	cvtsi2ssq	-24(%rbp), %xmm0
	movss	%xmm0, -44(%rbp)
	jmp	L251
L254:
	movq	-24(%rbp), %rax
	subq	$1, %rax
	cvtsi2ssq	%rax, %xmm0
	movss	%xmm0, -44(%rbp)
L251:
	movss	-44(%rbp), %xmm1
	mulss	-12(%rbp), %xmm1
	movss	-8(%rbp), %xmm0
	subss	%xmm1, %xmm0
	leave
	ret
.globl _Mod__uMod_var_u8_i64
	.private_extern _Mod__uMod_var_u8_i64
_Mod__uMod_var_u8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L258
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L258
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L258:
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
	je	L263
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L263
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L263:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_LC
	.private_extern _Mod__uMod_var_u8_LC
_Mod__uMod_var_u8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movzbl	124+_MM_Mod(%rip), %eax
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
	je	L268
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L268
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L268:
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
	je	L273
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L273
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L273:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u8
	.private_extern _Mod__uMod_var_u8_u8
_Mod__uMod_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L278
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L278
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L278:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L279
	cmpq	$127, -16(%rbp)
	jle	L280
L279:
	movl	$5153, %edi
	call	__m3_fault
L280:
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
	je	L285
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L285
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L285:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L286
	cmpq	$127, -16(%rbp)
	jle	L287
L286:
	movl	$5217, %edi
	call	__m3_fault
L287:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_I
	.private_extern _Mod__uMod_var_u8_I
_Mod__uMod_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L292
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L292
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L292:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L293
	cmpq	$127, -16(%rbp)
	jle	L294
L293:
	movl	$5281, %edi
	call	__m3_fault
L294:
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
	je	L299
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L299
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L299:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L300
	cmpq	$127, -16(%rbp)
	jle	L301
L300:
	movl	$5345, %edi
	call	__m3_fault
L301:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i8
	.private_extern _Mod__uMod_var_u8_i8
_Mod__uMod_var_u8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L307
	movl	$5409, %edi
	call	__m3_fault
L307:
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
	je	L312
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L312
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L312:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L313
	movl	$5473, %edi
	call	__m3_fault
L313:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u32
	.private_extern _Mod__uMod_var_u8_u32
_Mod__uMod_var_u8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L318
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L318
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L318:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L319
	cmpq	$127, -16(%rbp)
	jle	L320
L319:
	movl	$5537, %edi
	call	__m3_fault
L320:
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
	je	L325
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L325
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L325:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L326
	cmpq	$127, -16(%rbp)
	jle	L327
L326:
	movl	$5601, %edi
	call	__m3_fault
L327:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_C
	.private_extern _Mod__uMod_var_u8_C
_Mod__uMod_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movzbl	124+_MM_Mod(%rip), %eax
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
	je	L332
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L332
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L332:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L333
	movl	$5665, %edi
	call	__m3_fault
L333:
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
	je	L338
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L338
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L338:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L339
	movl	$5729, %edi
	call	__m3_fault
L339:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u16
	.private_extern _Mod__uMod_var_u8_u16
_Mod__uMod_var_u8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L344
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L344
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L344:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L345
	cmpq	$127, -16(%rbp)
	jle	L346
L345:
	movl	$5793, %edi
	call	__m3_fault
L346:
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
	je	L351
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L351
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L351:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L352
	cmpq	$127, -16(%rbp)
	jle	L353
L352:
	movl	$5857, %edi
	call	__m3_fault
L353:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_u64
	.private_extern _Mod__uMod_var_u8_u64
_Mod__uMod_var_u8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movq	160+_MM_Mod(%rip), %rax
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
	movzbl	124+_MM_Mod(%rip), %eax
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
	je	L358
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L358
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L358:
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
	je	L363
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L363
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L363:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_L
	.private_extern _Mod__uMod_var_u8_L
_Mod__uMod_var_u8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
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
	je	L368
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L368
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L368:
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
	je	L373
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L373
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L373:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i16
	.private_extern _Mod__uMod_var_u8_i16
_Mod__uMod_var_u8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L378
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L378
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L378:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L379
	movl	$6177, %edi
	call	__m3_fault
L379:
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
	je	L384
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L384
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L384:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L385
	movl	$6241, %edi
	call	__m3_fault
L385:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u8_i32
	.private_extern _Mod__uMod_var_u8_i32
_Mod__uMod_var_u8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L390
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L390
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L390:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L391
	movl	$6305, %edi
	call	__m3_fault
L391:
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
	je	L396
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L396
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L396:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$127, -16(%rbp)
	jle	L397
	movl	$6369, %edi
	call	__m3_fault
L397:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i64
	.private_extern _Mod__uMod_var_I_i64
_Mod__uMod_var_I_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L402
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L402
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L402:
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
	je	L407
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L407
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L407:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_LC
	.private_extern _Mod__uMod_var_I_LC
_Mod__uMod_var_I_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L412
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L412
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L412:
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
	je	L417
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L417
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L417:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u8
	.private_extern _Mod__uMod_var_I_u8
_Mod__uMod_var_I_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L422
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L422
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L422:
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
	je	L427
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L427
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L427:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_I
	.private_extern _Mod__uMod_var_I_I
_Mod__uMod_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L432
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L432
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L432:
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
	je	L437
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L437
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L437:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i8
	.private_extern _Mod__uMod_var_I_i8
_Mod__uMod_var_I_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L442
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L442
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L442:
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
	je	L447
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L447
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L447:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u32
	.private_extern _Mod__uMod_var_I_u32
_Mod__uMod_var_I_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L452
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L452
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L452:
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
	je	L457
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L457
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L457:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_C
	.private_extern _Mod__uMod_var_I_C
_Mod__uMod_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L462
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L462
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L462:
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
	je	L467
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L467
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L467:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u16
	.private_extern _Mod__uMod_var_I_u16
_Mod__uMod_var_I_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L472
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L472
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L472:
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
	je	L477
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L477
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L477:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_u64
	.private_extern _Mod__uMod_var_I_u64
_Mod__uMod_var_I_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L482
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L482
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L482:
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
	je	L487
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L487
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L487:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_L
	.private_extern _Mod__uMod_var_I_L
_Mod__uMod_var_I_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
	je	L497
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L497
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L497:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i16
	.private_extern _Mod__uMod_var_I_i16
_Mod__uMod_var_I_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L502
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L502
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L502:
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
	je	L507
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L507
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L507:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_I_i32
	.private_extern _Mod__uMod_var_I_i32
_Mod__uMod_var_I_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L512
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L512
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L512:
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
	je	L517
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L517
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L517:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i64
	.private_extern _Mod__uMod_var_i8_i64
_Mod__uMod_var_i8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	104+_MM_Mod(%rip), %rax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L522
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L522
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L522:
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
	je	L527
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L527
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L527:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_LC
	.private_extern _Mod__uMod_var_i8_LC
_Mod__uMod_var_i8_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Mod(%rip), %rax
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
	movzbl	136+_MM_Mod(%rip), %eax
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
	je	L532
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L532
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L532:
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
	je	L537
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L537
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L537:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u8
	.private_extern _Mod__uMod_var_i8_u8
_Mod__uMod_var_i8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L542
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L542
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L542:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L543
	cmpq	$255, -16(%rbp)
	jle	L544
L543:
	movl	$8225, %edi
	call	__m3_fault
L544:
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
	je	L549
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L549
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L549:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L550
	cmpq	$255, -16(%rbp)
	jle	L551
L550:
	movl	$8289, %edi
	call	__m3_fault
L551:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_I
	.private_extern _Mod__uMod_var_i8_I
_Mod__uMod_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_Mod(%rip), %rax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L556
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L556
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L556:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L557
	cmpq	$255, -16(%rbp)
	jle	L558
L557:
	movl	$8353, %edi
	call	__m3_fault
L558:
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
	je	L563
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L563
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L563:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L564
	cmpq	$255, -16(%rbp)
	jle	L565
L564:
	movl	$8417, %edi
	call	__m3_fault
L565:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i8
	.private_extern _Mod__uMod_var_i8_i8
_Mod__uMod_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L570
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L570
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L570:
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
	je	L575
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L575
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L575:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u32
	.private_extern _Mod__uMod_var_i8_u32
_Mod__uMod_var_i8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movl	140+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L580
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L580
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L580:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L581
	cmpq	$255, -16(%rbp)
	jle	L582
L581:
	movl	$8609, %edi
	call	__m3_fault
L582:
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
	je	L587
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L587
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L587:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L588
	cmpq	$255, -16(%rbp)
	jle	L589
L588:
	movl	$8673, %edi
	call	__m3_fault
L589:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_C
	.private_extern _Mod__uMod_var_i8_C
_Mod__uMod_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	144+_MM_Mod(%rip), %rax
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
	movzbl	136+_MM_Mod(%rip), %eax
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
	cmpq	$255, -16(%rbp)
	jle	L595
	movl	$8737, %edi
	call	__m3_fault
L595:
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
	je	L600
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L600
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L600:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L601
	movl	$8801, %edi
	call	__m3_fault
L601:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u16
	.private_extern _Mod__uMod_var_i8_u16
_Mod__uMod_var_i8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L606
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L606
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L606:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L607
	cmpq	$255, -16(%rbp)
	jle	L608
L607:
	movl	$8865, %edi
	call	__m3_fault
L608:
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
	je	L613
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L613
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L613:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L614
	cmpq	$255, -16(%rbp)
	jle	L615
L614:
	movl	$8929, %edi
	call	__m3_fault
L615:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_u64
	.private_extern _Mod__uMod_var_i8_u64
_Mod__uMod_var_i8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Mod(%rip), %rax
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
	movzbl	136+_MM_Mod(%rip), %eax
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
	je	L620
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L620
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L620:
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
	je	L625
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L625
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L625:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_L
	.private_extern _Mod__uMod_var_i8_L
_Mod__uMod_var_i8_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
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
	je	L630
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L630
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L630:
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
	je	L635
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L635
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L635:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i16
	.private_extern _Mod__uMod_var_i8_i16
_Mod__uMod_var_i8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L640
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L640
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L640:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L641
	movl	$9249, %edi
	call	__m3_fault
L641:
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
	je	L646
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L646
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L646:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L647
	movl	$9313, %edi
	call	__m3_fault
L647:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i8_i32
	.private_extern _Mod__uMod_var_i8_i32
_Mod__uMod_var_i8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %edx
	movl	188+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L652
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L652
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L652:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L653
	movl	$9377, %edi
	call	__m3_fault
L653:
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
	je	L658
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L658
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L658:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$255, -16(%rbp)
	jle	L659
	movl	$9441, %edi
	call	__m3_fault
L659:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i64
	.private_extern _Mod__uMod_var_u32_i64
_Mod__uMod_var_u32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
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
	je	L669
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L669
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L669:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_LC
	.private_extern _Mod__uMod_var_u32_LC
_Mod__uMod_var_u32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movl	140+_MM_Mod(%rip), %eax
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
	je	L674
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L674
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L674:
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
	je	L679
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L679
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L679:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u8
	.private_extern _Mod__uMod_var_u32_u8
_Mod__uMod_var_u32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L684
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L684
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L684:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L685
	cmpq	$2147483647, -16(%rbp)
	jle	L686
L685:
	movl	$9761, %edi
	call	__m3_fault
L686:
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
	je	L691
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L691
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L691:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L692
	cmpq	$2147483647, -16(%rbp)
	jle	L693
L692:
	movl	$9825, %edi
	call	__m3_fault
L693:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_I
	.private_extern _Mod__uMod_var_u32_I
_Mod__uMod_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L699
	cmpq	$2147483647, -16(%rbp)
	jle	L700
L699:
	movl	$9889, %edi
	call	__m3_fault
L700:
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
	je	L705
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L705
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L705:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L706
	cmpq	$2147483647, -16(%rbp)
	jle	L707
L706:
	movl	$9953, %edi
	call	__m3_fault
L707:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i8
	.private_extern _Mod__uMod_var_u32_i8
_Mod__uMod_var_u32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L712
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L712
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L712:
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
	je	L717
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L717
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L717:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u32
	.private_extern _Mod__uMod_var_u32_u32
_Mod__uMod_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L722
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L722
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L722:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L723
	cmpq	$2147483647, -16(%rbp)
	jle	L724
L723:
	movl	$10145, %edi
	call	__m3_fault
L724:
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
	je	L729
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L729
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L729:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L730
	cmpq	$2147483647, -16(%rbp)
	jle	L731
L730:
	movl	$10209, %edi
	call	__m3_fault
L731:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_C
	.private_extern _Mod__uMod_var_u32_C
_Mod__uMod_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movl	140+_MM_Mod(%rip), %eax
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
	je	L736
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L736
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L736:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L737
	movl	$10273, %edi
	call	__m3_fault
L737:
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
	je	L742
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L742
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L742:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L743
	movl	$10337, %edi
	call	__m3_fault
L743:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u16
	.private_extern _Mod__uMod_var_u32_u16
_Mod__uMod_var_u32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L749
	cmpq	$2147483647, -16(%rbp)
	jle	L750
L749:
	movl	$10401, %edi
	call	__m3_fault
L750:
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
	je	L755
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L755
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L755:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L756
	cmpq	$2147483647, -16(%rbp)
	jle	L757
L756:
	movl	$10465, %edi
	call	__m3_fault
L757:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_u64
	.private_extern _Mod__uMod_var_u32_u64
_Mod__uMod_var_u32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movq	160+_MM_Mod(%rip), %rax
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
	movl	140+_MM_Mod(%rip), %eax
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
	je	L762
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L762
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L762:
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
	je	L767
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L767
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L767:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_L
	.private_extern _Mod__uMod_var_u32_L
_Mod__uMod_var_u32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
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
	je	L772
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L772
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L772:
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
	je	L777
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L777
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L777:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i16
	.private_extern _Mod__uMod_var_u32_i16
_Mod__uMod_var_u32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L782
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L782
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L782:
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
	je	L787
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L787
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L787:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u32_i32
	.private_extern _Mod__uMod_var_u32_i32
_Mod__uMod_var_u32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Mod(%rip), %eax
	movslq	%eax,%rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L792
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L792
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L792:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L793
	movl	$10913, %edi
	call	__m3_fault
L793:
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
	je	L798
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L798
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L798:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$2147483647, -16(%rbp)
	jle	L799
	movl	$10977, %edi
	call	__m3_fault
L799:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i64
	.private_extern _Mod__uMod_var_C_i64
_Mod__uMod_var_C_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L804
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L804
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L804:
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
	je	L809
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L809
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L809:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_LC
	.private_extern _Mod__uMod_var_C_LC
_Mod__uMod_var_C_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L814
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L814
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L814:
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
	je	L819
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L819
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L819:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u8
	.private_extern _Mod__uMod_var_C_u8
_Mod__uMod_var_C_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L824
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L824
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L824:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L825
	movl	$11297, %edi
	call	__m3_fault
L825:
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
	je	L830
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L830
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L830:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L831
	movl	$11361, %edi
	call	__m3_fault
L831:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_I
	.private_extern _Mod__uMod_var_C_I
_Mod__uMod_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L836
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L836
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L836:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L837
	movl	$11425, %edi
	call	__m3_fault
L837:
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
	je	L842
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L842
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L842:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L843
	movl	$11489, %edi
	call	__m3_fault
L843:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i8
	.private_extern _Mod__uMod_var_C_i8
_Mod__uMod_var_C_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L848
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L848
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L848:
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
	je	L853
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L853
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L853:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u32
	.private_extern _Mod__uMod_var_C_u32
_Mod__uMod_var_C_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L859
	movl	$11681, %edi
	call	__m3_fault
L859:
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
	je	L864
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L864
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L864:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L865
	movl	$11745, %edi
	call	__m3_fault
L865:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_C
	.private_extern _Mod__uMod_var_C_C
_Mod__uMod_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L870
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L870
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L870:
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
	je	L875
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L875
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L875:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u16
	.private_extern _Mod__uMod_var_C_u16
_Mod__uMod_var_C_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L880
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L880
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L880:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L881
	movl	$11937, %edi
	call	__m3_fault
L881:
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
	je	L886
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L886
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L886:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L887
	movl	$12001, %edi
	call	__m3_fault
L887:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_u64
	.private_extern _Mod__uMod_var_C_u64
_Mod__uMod_var_C_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movq	160+_MM_Mod(%rip), %rax
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
	je	L892
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L892
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L892:
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
	je	L897
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L897
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L897:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_L
	.private_extern _Mod__uMod_var_C_L
_Mod__uMod_var_C_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L902
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L902
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L902:
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
	je	L907
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L907
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L907:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i16
	.private_extern _Mod__uMod_var_C_i16
_Mod__uMod_var_C_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L912
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L912
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L912:
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
	je	L917
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L917
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L917:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_C_i32
	.private_extern _Mod__uMod_var_C_i32
_Mod__uMod_var_C_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Mod(%rip), %rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -40(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L922
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L922
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L922:
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
	je	L927
	movq	-56(%rbp), %rax
	xorq	-48(%rbp), %rax
	testq	%rax, %rax
	jns	L927
	movq	-48(%rbp), %rax
	addq	%rax, -40(%rbp)
L927:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i64
	.private_extern _Mod__uMod_var_u16_i64
_Mod__uMod_var_u16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L932
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L932
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L932:
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
	je	L937
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L937
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L937:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_LC
	.private_extern _Mod__uMod_var_u16_LC
_Mod__uMod_var_u16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Mod(%rip), %rax
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
	movzwl	152+_MM_Mod(%rip), %eax
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
	je	L942
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L942
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L942:
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
	je	L947
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L947
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L947:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u8
	.private_extern _Mod__uMod_var_u16_u8
_Mod__uMod_var_u16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L952
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L952
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L952:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L953
	cmpq	$32767, -16(%rbp)
	jle	L954
L953:
	movl	$12833, %edi
	call	__m3_fault
L954:
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
	je	L959
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L959
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L959:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L960
	cmpq	$32767, -16(%rbp)
	jle	L961
L960:
	movl	$12897, %edi
	call	__m3_fault
L961:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_I
	.private_extern _Mod__uMod_var_u16_I
_Mod__uMod_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L966
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L966
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L966:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L967
	cmpq	$32767, -16(%rbp)
	jle	L968
L967:
	movl	$12961, %edi
	call	__m3_fault
L968:
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
	je	L973
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L973
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L973:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L974
	cmpq	$32767, -16(%rbp)
	jle	L975
L974:
	movl	$13025, %edi
	call	__m3_fault
L975:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i8
	.private_extern _Mod__uMod_var_u16_i8
_Mod__uMod_var_u16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L980
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L980
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L980:
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
	je	L985
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L985
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L985:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u32
	.private_extern _Mod__uMod_var_u16_u32
_Mod__uMod_var_u16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L990
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L990
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L990:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L991
	cmpq	$32767, -16(%rbp)
	jle	L992
L991:
	movl	$13217, %edi
	call	__m3_fault
L992:
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
	je	L997
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L997
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L997:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L998
	cmpq	$32767, -16(%rbp)
	jle	L999
L998:
	movl	$13281, %edi
	call	__m3_fault
L999:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_C
	.private_extern _Mod__uMod_var_u16_C
_Mod__uMod_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	144+_MM_Mod(%rip), %rax
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
	movzwl	152+_MM_Mod(%rip), %eax
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
	je	L1004
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1004
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1004:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L1005
	movl	$13345, %edi
	call	__m3_fault
L1005:
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
	je	L1010
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1010
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1010:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L1011
	movl	$13409, %edi
	call	__m3_fault
L1011:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u16
	.private_extern _Mod__uMod_var_u16_u16
_Mod__uMod_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1016
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1016
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1016:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1017
	cmpq	$32767, -16(%rbp)
	jle	L1018
L1017:
	movl	$13473, %edi
	call	__m3_fault
L1018:
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
	je	L1023
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1023
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1023:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L1024
	cmpq	$32767, -16(%rbp)
	jle	L1025
L1024:
	movl	$13537, %edi
	call	__m3_fault
L1025:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_u64
	.private_extern _Mod__uMod_var_u16_u64
_Mod__uMod_var_u16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movq	160+_MM_Mod(%rip), %rax
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
	movzwl	152+_MM_Mod(%rip), %eax
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
	je	L1030
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1030
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1030:
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
	je	L1035
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1035
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1035:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_L
	.private_extern _Mod__uMod_var_u16_L
_Mod__uMod_var_u16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
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
	je	L1040
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1040
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1040:
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
	je	L1045
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1045
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1045:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i16
	.private_extern _Mod__uMod_var_u16_i16
_Mod__uMod_var_u16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1050
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1050
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1050:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L1051
	movl	$13857, %edi
	call	__m3_fault
L1051:
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
	je	L1056
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1056
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1056:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L1057
	movl	$13921, %edi
	call	__m3_fault
L1057:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u16_i32
	.private_extern _Mod__uMod_var_u16_i32
_Mod__uMod_var_u16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1062
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1062
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1062:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L1063
	movl	$13985, %edi
	call	__m3_fault
L1063:
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
	je	L1068
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1068
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1068:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$32767, -16(%rbp)
	jle	L1069
	movl	$14049, %edi
	call	__m3_fault
L1069:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i64
	.private_extern _Mod__uMod_var_u64_i64
_Mod__uMod_var_u64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
.globl _Mod__uMod_var_u64_LC
	.private_extern _Mod__uMod_var_u64_LC
_Mod__uMod_var_u64_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Mod(%rip), %rax
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
.globl _Mod__uMod_var_u64_u8
	.private_extern _Mod__uMod_var_u64_u8
_Mod__uMod_var_u64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
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
	je	L1099
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1099
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1099:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_I
	.private_extern _Mod__uMod_var_u64_I
_Mod__uMod_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1104
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1104
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1104:
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
	je	L1109
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1109
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1109:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i8
	.private_extern _Mod__uMod_var_u64_i8
_Mod__uMod_var_u64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1114
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1114
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1114:
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
	je	L1119
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1119
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1119:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u32
	.private_extern _Mod__uMod_var_u64_u32
_Mod__uMod_var_u64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1124
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1124
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1124:
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
	je	L1129
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1129
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1129:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_C
	.private_extern _Mod__uMod_var_u64_C
_Mod__uMod_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	144+_MM_Mod(%rip), %rax
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
	je	L1134
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1134
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1134:
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
	je	L1139
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1139
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1139:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u16
	.private_extern _Mod__uMod_var_u64_u16
_Mod__uMod_var_u64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1144
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1144
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1144:
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
	je	L1149
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1149
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1149:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_u64
	.private_extern _Mod__uMod_var_u64_u64
_Mod__uMod_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Mod(%rip), %rax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	160+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
	je	L1159
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1159
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1159:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_L
	.private_extern _Mod__uMod_var_u64_L
_Mod__uMod_var_u64_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1164
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1164
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1164:
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
	je	L1169
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1169
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1169:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i16
	.private_extern _Mod__uMod_var_u64_i16
_Mod__uMod_var_u64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1174
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1174
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1174:
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
	je	L1179
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1179
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1179:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_u64_i32
	.private_extern _Mod__uMod_var_u64_i32
_Mod__uMod_var_u64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	188+_MM_Mod(%rip), %eax
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
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1184
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1184
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1184:
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
	je	L1189
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1189
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1189:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__Mod_var_f64_f64
	.private_extern _Mod__Mod_var_f64_f64
_Mod__Mod_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	168+_MM_Mod(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	168+_MM_Mod(%rip), %rax
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
	jb	L1197
L1196:
	cvtsi2sdq	-40(%rbp), %xmm0
	movsd	%xmm0, -56(%rbp)
	jmp	L1194
L1197:
	movq	-40(%rbp), %rax
	subq	$1, %rax
	cvtsi2sdq	%rax, %xmm0
	movsd	%xmm0, -56(%rbp)
L1194:
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
	jb	L1204
L1203:
	cvtsi2sdq	-40(%rbp), %xmm0
	movsd	%xmm0, -72(%rbp)
	jmp	L1201
L1204:
	movq	-40(%rbp), %rax
	subq	$1, %rax
	cvtsi2sdq	%rax, %xmm0
	movsd	%xmm0, -72(%rbp)
L1201:
	movsd	-72(%rbp), %xmm1
	mulsd	-24(%rbp), %xmm1
	movsd	-16(%rbp), %xmm0
	subsd	%xmm1, %xmm0
	leave
	ret
.globl _Mod__uMod_var_L_i64
	.private_extern _Mod__uMod_var_L_i64
_Mod__uMod_var_L_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	104+_MM_Mod(%rip), %rax
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
	movq	104+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1208
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1208
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1208:
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
	je	L1213
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1213
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1213:
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
	movq	112+_MM_Mod(%rip), %rax
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
	je	L1223
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1223
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1223:
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
	movzbl	124+_MM_Mod(%rip), %eax
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
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
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
.globl _Mod__uMod_var_L_I
	.private_extern _Mod__uMod_var_L_I
_Mod__uMod_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_Mod(%rip), %rax
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
	movq	128+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
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
	je	L1243
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1243
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1243:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_i8
	.private_extern _Mod__uMod_var_L_i8
_Mod__uMod_var_L_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1248
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1248
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1248:
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
	je	L1253
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1253
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1253:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_u32
	.private_extern _Mod__uMod_var_L_u32
_Mod__uMod_var_L_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movl	140+_MM_Mod(%rip), %eax
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
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1258
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1258
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1258:
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
	je	L1263
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1263
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1263:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_C
	.private_extern _Mod__uMod_var_L_C
_Mod__uMod_var_L_C:
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
.globl _Mod__Mod_var_L_C
	.private_extern _Mod__Mod_var_L_C
_Mod__Mod_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, -32(%rbp)
	movq	144+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1268
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1268
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1268:
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
	je	L1273
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1273
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1273:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_u16
	.private_extern _Mod__uMod_var_L_u16
_Mod__uMod_var_L_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1278
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1278
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1278:
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
	je	L1283
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1283
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1283:
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
	movq	160+_MM_Mod(%rip), %rax
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
	movq	160+_MM_Mod(%rip), %rdx
	movq	%rdx, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1288
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1288
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1288:
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
	je	L1293
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1293
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1293:
	movq	-48(%rbp), %rax
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
	je	L1298
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1298
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1298:
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
	je	L1303
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1303
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1303:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_L_i16
	.private_extern _Mod__uMod_var_L_i16
_Mod__uMod_var_L_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Mod(%rip), %rax
	movq	%rax, %rdx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1308
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1308
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1308:
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
	je	L1313
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1313
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1313:
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
	movl	188+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1318
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1318
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1318:
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
	je	L1323
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1323
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1323:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i64
	.private_extern _Mod__uMod_var_i16_i64
_Mod__uMod_var_i16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	104+_MM_Mod(%rip), %rax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1328
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1328
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1328:
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
	je	L1333
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1333
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1333:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_LC
	.private_extern _Mod__uMod_var_i16_LC
_Mod__uMod_var_i16_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Mod(%rip), %rax
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
	movzwl	184+_MM_Mod(%rip), %eax
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
	je	L1338
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1338
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1338:
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
	je	L1343
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1343
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1343:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u8
	.private_extern _Mod__uMod_var_i16_u8
_Mod__uMod_var_i16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1348
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1348
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1348:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1349
	cmpq	$65535, -16(%rbp)
	jle	L1350
L1349:
	movl	$17505, %edi
	call	__m3_fault
L1350:
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
	je	L1355
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1355
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1355:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1356
	cmpq	$65535, -16(%rbp)
	jle	L1357
L1356:
	movl	$17569, %edi
	call	__m3_fault
L1357:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_I
	.private_extern _Mod__uMod_var_i16_I
_Mod__uMod_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_Mod(%rip), %rax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1362
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1362
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1362:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1363
	cmpq	$65535, -16(%rbp)
	jle	L1364
L1363:
	movl	$17633, %edi
	call	__m3_fault
L1364:
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
	je	L1369
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1369
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1369:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1370
	cmpq	$65535, -16(%rbp)
	jle	L1371
L1370:
	movl	$17697, %edi
	call	__m3_fault
L1371:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i8
	.private_extern _Mod__uMod_var_i16_i8
_Mod__uMod_var_i16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1376
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1376
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1376:
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
	je	L1381
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1381
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L1381:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u32
	.private_extern _Mod__uMod_var_i16_u32
_Mod__uMod_var_i16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movl	140+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1387
	cmpq	$65535, -16(%rbp)
	jle	L1388
L1387:
	movl	$17889, %edi
	call	__m3_fault
L1388:
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
	je	L1393
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1393
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1393:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1394
	cmpq	$65535, -16(%rbp)
	jle	L1395
L1394:
	movl	$17953, %edi
	call	__m3_fault
L1395:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_C
	.private_extern _Mod__uMod_var_i16_C
_Mod__uMod_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	144+_MM_Mod(%rip), %rax
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
	movzwl	184+_MM_Mod(%rip), %eax
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
	je	L1400
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1400
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1400:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1401
	movl	$18017, %edi
	call	__m3_fault
L1401:
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
	je	L1406
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1406
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1406:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1407
	movl	$18081, %edi
	call	__m3_fault
L1407:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u16
	.private_extern _Mod__uMod_var_i16_u16
_Mod__uMod_var_i16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1412
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1412
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1412:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1413
	cmpq	$65535, -16(%rbp)
	jle	L1414
L1413:
	movl	$18145, %edi
	call	__m3_fault
L1414:
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
	je	L1419
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1419
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1419:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1420
	cmpq	$65535, -16(%rbp)
	jle	L1421
L1420:
	movl	$18209, %edi
	call	__m3_fault
L1421:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_u64
	.private_extern _Mod__uMod_var_i16_u64
_Mod__uMod_var_i16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Mod(%rip), %rax
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
	movzwl	184+_MM_Mod(%rip), %eax
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
	je	L1431
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1431
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1431:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_L
	.private_extern _Mod__uMod_var_i16_L
_Mod__uMod_var_i16_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
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
	je	L1441
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1441
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1441:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i16
	.private_extern _Mod__uMod_var_i16_i16
_Mod__uMod_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1446
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1446
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1446:
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
	je	L1451
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1451
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L1451:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i16_i32
	.private_extern _Mod__uMod_var_i16_i32
_Mod__uMod_var_i16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %edx
	movl	188+_MM_Mod(%rip), %eax
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
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
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
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1457
	movl	$18657, %edi
	call	__m3_fault
L1457:
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
	je	L1462
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1462
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1462:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$65535, -16(%rbp)
	jle	L1463
	movl	$18721, %edi
	call	__m3_fault
L1463:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i64
	.private_extern _Mod__uMod_var_i32_i64
_Mod__uMod_var_i32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	104+_MM_Mod(%rip), %rax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	104+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1468
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1468
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1468:
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
	je	L1473
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1473
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1473:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_LC
	.private_extern _Mod__uMod_var_i32_LC
_Mod__uMod_var_i32_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Mod(%rip), %rax
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
	movl	188+_MM_Mod(%rip), %eax
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
	je	L1478
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1478
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1478:
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
	je	L1483
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1483
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1483:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u8
	.private_extern _Mod__uMod_var_i32_u8
_Mod__uMod_var_i32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzbl	124+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	124+_MM_Mod(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1488
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1488
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1488:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1489
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1490
L1489:
	movl	$19041, %edi
	call	__m3_fault
L1490:
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
	je	L1495
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1495
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1495:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1496
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1497
L1496:
	movl	$19105, %edi
	call	__m3_fault
L1497:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_I
	.private_extern _Mod__uMod_var_i32_I
_Mod__uMod_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_Mod(%rip), %rax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	128+_MM_Mod(%rip), %rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1502
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1502
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1502:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1503
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1504
L1503:
	movl	$19169, %edi
	call	__m3_fault
L1504:
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
	je	L1509
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1509
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1509:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1510
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1511
L1510:
	movl	$19233, %edi
	call	__m3_fault
L1511:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i8
	.private_extern _Mod__uMod_var_i32_i8
_Mod__uMod_var_i32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzbl	136+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movzbl	136+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1516
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1516
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1516:
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
	je	L1521
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1521
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L1521:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u32
	.private_extern _Mod__uMod_var_i32_u32
_Mod__uMod_var_i32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movl	140+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movl	140+_MM_Mod(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1526
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1526
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1526:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1527
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1528
L1527:
	movl	$19425, %edi
	call	__m3_fault
L1528:
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
	je	L1533
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1533
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1533:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1534
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1535
L1534:
	movl	$19489, %edi
	call	__m3_fault
L1535:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_C
	.private_extern _Mod__uMod_var_i32_C
_Mod__uMod_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	144+_MM_Mod(%rip), %rax
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
	movl	188+_MM_Mod(%rip), %eax
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
	je	L1540
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1540
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1540:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1541
	movl	$19553, %edi
	call	__m3_fault
L1541:
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
	je	L1546
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1546
	movq	-40(%rbp), %rax
	addq	%rax, -56(%rbp)
L1546:
	movq	-56(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1547
	movl	$19617, %edi
	call	__m3_fault
L1547:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u16
	.private_extern _Mod__uMod_var_i32_u16
_Mod__uMod_var_i32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzwl	152+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	152+_MM_Mod(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1552
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1552
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1552:
	movq	-40(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1553
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1554
L1553:
	movl	$19681, %edi
	call	__m3_fault
L1554:
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
	je	L1559
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1559
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1559:
	movq	-48(%rbp), %rdx
	movq	%rdx, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L1560
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L1561
L1560:
	movl	$19745, %edi
	call	__m3_fault
L1561:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_u64
	.private_extern _Mod__uMod_var_i32_u64
_Mod__uMod_var_i32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Mod(%rip), %rax
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
	movl	188+_MM_Mod(%rip), %eax
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
	je	L1566
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1566
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1566:
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
	je	L1571
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1571
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1571:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_L
	.private_extern _Mod__uMod_var_i32_L
_Mod__uMod_var_i32_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
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
	je	L1576
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1576
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1576:
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
	je	L1581
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1581
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1581:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i16
	.private_extern _Mod__uMod_var_i32_i16
_Mod__uMod_var_i32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movzwl	184+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movzwl	184+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1586
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1586
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1586:
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
	je	L1591
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1591
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L1591:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _Mod__uMod_var_i32_i32
	.private_extern _Mod__uMod_var_i32_i32
_Mod__uMod_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %edx
	movl	188+_MM_Mod(%rip), %eax
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
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movl	188+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -24(%rbp)
	cmpq	$0, -24(%rbp)
	je	L1596
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1596
	movq	-32(%rbp), %rax
	addq	%rax, -24(%rbp)
L1596:
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
	je	L1601
	movq	-48(%rbp), %rax
	xorq	-40(%rbp), %rax
	testq	%rax, %rax
	jns	L1601
	movq	-40(%rbp), %rax
	addq	%rax, -32(%rbp)
L1601:
	movq	-32(%rbp), %rax
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
	.ascii "Mod_param_i32_L"
	.space 1
	.ascii "uMod_param_i32_L"
	.space 1
	.ascii "Mod_var_i32_L"
	.space 1
	.ascii "uMod_var_i32_L"
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
	.ascii "Mod_param_i32_C"
	.space 1
	.ascii "uMod_param_i32_C"
	.space 1
	.ascii "Mod_var_i32_C"
	.space 1
	.ascii "uMod_var_i32_C"
	.space 1
	.ascii "Mod_param_i32_u32"
	.space 1
	.ascii "uMod_param_i32_u32"
	.space 1
	.ascii "Mod_var_i32_u32"
	.space 1
	.ascii "uMod_var_i32_u32"
	.space 1
	.ascii "Mod_param_i32_i8"
	.space 1
	.ascii "uMod_param_i32_i8"
	.space 1
	.ascii "Mod_var_i32_i8"
	.space 1
	.ascii "uMod_var_i32_i8"
	.space 1
	.ascii "Mod_param_i32_I"
	.space 1
	.ascii "uMod_param_i32_I"
	.space 1
	.ascii "Mod_var_i32_I"
	.space 1
	.ascii "uMod_var_i32_I"
	.space 1
	.ascii "Mod_param_i32_u8"
	.space 1
	.ascii "uMod_param_i32_u8"
	.space 1
	.ascii "Mod_var_i32_u8"
	.space 1
	.ascii "uMod_var_i32_u8"
	.space 1
	.ascii "Mod_param_i32_LC"
	.space 1
	.ascii "uMod_param_i32_LC"
	.space 1
	.ascii "Mod_var_i32_LC"
	.space 1
	.ascii "uMod_var_i32_LC"
	.space 1
	.ascii "Mod_param_i32_i64"
	.space 1
	.ascii "uMod_param_i32_i64"
	.space 1
	.ascii "Mod_var_i32_i64"
	.space 1
	.ascii "uMod_var_i32_i64"
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
	.ascii "Mod_param_i16_L"
	.space 1
	.ascii "uMod_param_i16_L"
	.space 1
	.ascii "Mod_var_i16_L"
	.space 1
	.ascii "uMod_var_i16_L"
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
	.ascii "Mod_param_i16_C"
	.space 1
	.ascii "uMod_param_i16_C"
	.space 1
	.ascii "Mod_var_i16_C"
	.space 1
	.ascii "uMod_var_i16_C"
	.space 1
	.ascii "Mod_param_i16_u32"
	.space 1
	.ascii "uMod_param_i16_u32"
	.space 1
	.ascii "Mod_var_i16_u32"
	.space 1
	.ascii "uMod_var_i16_u32"
	.space 1
	.ascii "Mod_param_i16_i8"
	.space 1
	.ascii "uMod_param_i16_i8"
	.space 1
	.ascii "Mod_var_i16_i8"
	.space 1
	.ascii "uMod_var_i16_i8"
	.space 1
	.ascii "Mod_param_i16_I"
	.space 1
	.ascii "uMod_param_i16_I"
	.space 1
	.ascii "Mod_var_i16_I"
	.space 1
	.ascii "uMod_var_i16_I"
	.space 1
	.ascii "Mod_param_i16_u8"
	.space 1
	.ascii "uMod_param_i16_u8"
	.space 1
	.ascii "Mod_var_i16_u8"
	.space 1
	.ascii "uMod_var_i16_u8"
	.space 1
	.ascii "Mod_param_i16_LC"
	.space 1
	.ascii "uMod_param_i16_LC"
	.space 1
	.ascii "Mod_var_i16_LC"
	.space 1
	.ascii "uMod_var_i16_LC"
	.space 1
	.ascii "Mod_param_i16_i64"
	.space 1
	.ascii "uMod_param_i16_i64"
	.space 1
	.ascii "Mod_var_i16_i64"
	.space 1
	.ascii "uMod_var_i16_i64"
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
	.ascii "Mod_param_L_L"
	.space 1
	.ascii "uMod_param_L_L"
	.space 1
	.ascii "Mod_var_L_L"
	.space 1
	.ascii "uMod_var_L_L"
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
	.ascii "Mod_param_L_C"
	.space 1
	.ascii "uMod_param_L_C"
	.space 1
	.ascii "Mod_var_L_C"
	.space 1
	.ascii "uMod_var_L_C"
	.space 1
	.ascii "Mod_param_L_u32"
	.space 1
	.ascii "uMod_param_L_u32"
	.space 1
	.ascii "Mod_var_L_u32"
	.space 1
	.ascii "uMod_var_L_u32"
	.space 1
	.ascii "Mod_param_L_i8"
	.space 1
	.ascii "uMod_param_L_i8"
	.space 1
	.ascii "Mod_var_L_i8"
	.space 1
	.ascii "uMod_var_L_i8"
	.space 1
	.ascii "Mod_param_L_I"
	.space 1
	.ascii "uMod_param_L_I"
	.space 1
	.ascii "Mod_var_L_I"
	.space 1
	.ascii "uMod_var_L_I"
	.space 1
	.ascii "Mod_param_L_u8"
	.space 1
	.ascii "uMod_param_L_u8"
	.space 1
	.ascii "Mod_var_L_u8"
	.space 1
	.ascii "uMod_var_L_u8"
	.space 1
	.ascii "Mod_param_L_LC"
	.space 1
	.ascii "uMod_param_L_LC"
	.space 1
	.ascii "Mod_var_L_LC"
	.space 1
	.ascii "uMod_var_L_LC"
	.space 1
	.ascii "Mod_param_L_i64"
	.space 1
	.ascii "uMod_param_L_i64"
	.space 1
	.ascii "Mod_var_L_i64"
	.space 1
	.ascii "uMod_var_L_i64"
	.space 1
	.ascii "Mod_param_f64_f64"
	.space 1
	.ascii "Mod_var_f64_f64"
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
	.ascii "Mod_param_u64_L"
	.space 1
	.ascii "uMod_param_u64_L"
	.space 1
	.ascii "Mod_var_u64_L"
	.space 1
	.ascii "uMod_var_u64_L"
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
	.ascii "Mod_param_u64_C"
	.space 1
	.ascii "uMod_param_u64_C"
	.space 1
	.ascii "Mod_var_u64_C"
	.space 1
	.ascii "uMod_var_u64_C"
	.space 1
	.ascii "Mod_param_u64_u32"
	.space 1
	.ascii "uMod_param_u64_u32"
	.space 1
	.ascii "Mod_var_u64_u32"
	.space 1
	.ascii "uMod_var_u64_u32"
	.space 1
	.ascii "Mod_param_u64_i8"
	.space 1
	.ascii "uMod_param_u64_i8"
	.space 1
	.ascii "Mod_var_u64_i8"
	.space 1
	.ascii "uMod_var_u64_i8"
	.space 1
	.ascii "Mod_param_u64_I"
	.space 1
	.ascii "uMod_param_u64_I"
	.space 1
	.ascii "Mod_var_u64_I"
	.space 1
	.ascii "uMod_var_u64_I"
	.space 1
	.ascii "Mod_param_u64_u8"
	.space 1
	.ascii "uMod_param_u64_u8"
	.space 1
	.ascii "Mod_var_u64_u8"
	.space 1
	.ascii "uMod_var_u64_u8"
	.space 1
	.ascii "Mod_param_u64_LC"
	.space 1
	.ascii "uMod_param_u64_LC"
	.space 1
	.ascii "Mod_var_u64_LC"
	.space 1
	.ascii "uMod_var_u64_LC"
	.space 1
	.ascii "Mod_param_u64_i64"
	.space 1
	.ascii "uMod_param_u64_i64"
	.space 1
	.ascii "Mod_var_u64_i64"
	.space 1
	.ascii "uMod_var_u64_i64"
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
	.ascii "Mod_param_u16_L"
	.space 1
	.ascii "uMod_param_u16_L"
	.space 1
	.ascii "Mod_var_u16_L"
	.space 1
	.ascii "uMod_var_u16_L"
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
	.space 1
	.ascii "Mod_param_u16_C"
	.space 1
	.ascii "uMod_param_u16_C"
	.space 1
	.ascii "Mod_var_u16_C"
	.space 1
	.ascii "uMod_var_u16_C"
	.space 1
	.ascii "Mod_param_u16_u32"
	.space 1
	.ascii "uMod_param_u16_u32"
	.space 1
	.ascii "Mod_var_u16_u32"
	.space 1
	.ascii "uMod_var_u16_u32"
	.space 1
	.ascii "Mod_param_u16_i8"
	.space 1
	.ascii "uMod_param_u16_i8"
	.space 1
	.ascii "Mod_var_u16_i8"
	.space 1
	.ascii "uMod_var_u16_i8"
	.space 1
	.ascii "Mod_param_u16_I"
	.space 1
	.ascii "uMod_param_u16_I"
	.space 1
	.ascii "Mod_var_u16_I"
	.space 1
	.ascii "uMod_var_u16_I"
	.space 1
	.ascii "Mod_param_u16_u8"
	.space 1
	.ascii "uMod_param_u16_u8"
	.space 1
	.ascii "Mod_var_u16_u8"
	.space 1
	.ascii "uMod_var_u16_u8"
	.space 1
	.ascii "Mod_param_u16_LC"
	.space 1
	.ascii "uMod_param_u16_LC"
	.space 1
	.ascii "Mod_var_u16_LC"
	.space 1
	.ascii "uMod_var_u16_LC"
	.space 1
	.ascii "Mod_param_u16_i64"
	.space 1
	.ascii "uMod_param_u16_i64"
	.space 1
	.ascii "Mod_var_u16_i64"
	.space 1
	.ascii "uMod_var_u16_i64"
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
	.ascii "Mod_param_C_L"
	.space 1
	.ascii "uMod_param_C_L"
	.space 1
	.ascii "Mod_var_C_L"
	.space 1
	.ascii "uMod_var_C_L"
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
	.ascii "Mod_param_C_C"
	.space 1
	.ascii "uMod_param_C_C"
	.space 1
	.ascii "Mod_var_C_C"
	.space 1
	.ascii "uMod_var_C_C"
	.space 1
	.ascii "Mod_param_C_u32"
	.space 1
	.ascii "uMod_param_C_u32"
	.space 1
	.ascii "Mod_var_C_u32"
	.space 1
	.ascii "uMod_var_C_u32"
	.space 1
	.ascii "Mod_param_C_i8"
	.space 1
	.ascii "uMod_param_C_i8"
	.space 1
	.ascii "Mod_var_C_i8"
	.space 1
	.ascii "uMod_var_C_i8"
	.space 1
	.ascii "Mod_param_C_I"
	.space 1
	.ascii "uMod_param_C_I"
	.space 1
	.ascii "Mod_var_C_I"
	.space 1
	.ascii "uMod_var_C_I"
	.space 1
	.ascii "Mod_param_C_u8"
	.space 1
	.ascii "uMod_param_C_u8"
	.space 1
	.ascii "Mod_var_C_u8"
	.space 1
	.ascii "uMod_var_C_u8"
	.space 1
	.ascii "Mod_param_C_LC"
	.space 1
	.ascii "uMod_param_C_LC"
	.space 1
	.ascii "Mod_var_C_LC"
	.space 1
	.ascii "uMod_var_C_LC"
	.space 1
	.ascii "Mod_param_C_i64"
	.space 1
	.ascii "uMod_param_C_i64"
	.space 1
	.ascii "Mod_var_C_i64"
	.space 1
	.ascii "uMod_var_C_i64"
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
	.ascii "Mod_param_u32_L"
	.space 1
	.ascii "uMod_param_u32_L"
	.space 1
	.ascii "Mod_var_u32_L"
	.space 1
	.ascii "uMod_var_u32_L"
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
	.ascii "Mod_param_u32_C"
	.space 1
	.ascii "uMod_param_u32_C"
	.space 1
	.ascii "Mod_var_u32_C"
	.space 1
	.ascii "uMod_var_u32_C"
	.space 1
	.ascii "Mod_param_u32_u32"
	.space 1
	.ascii "uMod_param_u32_u32"
	.space 1
	.ascii "Mod_var_u32_u32"
	.space 1
	.ascii "uMod_var_u32_u32"
	.space 1
	.ascii "Mod_param_u32_i8"
	.space 1
	.ascii "uMod_param_u32_i8"
	.space 1
	.ascii "Mod_var_u32_i8"
	.space 1
	.ascii "uMod_var_u32_i8"
	.space 1
	.ascii "Mod_param_u32_I"
	.space 1
	.ascii "uMod_param_u32_I"
	.space 1
	.ascii "Mod_var_u32_I"
	.space 1
	.ascii "uMod_var_u32_I"
	.space 1
	.ascii "Mod_param_u32_u8"
	.space 1
	.ascii "uMod_param_u32_u8"
	.space 1
	.ascii "Mod_var_u32_u8"
	.space 1
	.ascii "uMod_var_u32_u8"
	.space 1
	.ascii "Mod_param_u32_LC"
	.space 1
	.ascii "uMod_param_u32_LC"
	.space 1
	.ascii "Mod_var_u32_LC"
	.space 1
	.ascii "uMod_var_u32_LC"
	.space 1
	.ascii "Mod_param_u32_i64"
	.space 1
	.ascii "uMod_param_u32_i64"
	.space 1
	.ascii "Mod_var_u32_i64"
	.space 1
	.ascii "uMod_var_u32_i64"
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
	.ascii "Mod_param_i8_L"
	.space 1
	.ascii "uMod_param_i8_L"
	.space 1
	.ascii "Mod_var_i8_L"
	.space 1
	.ascii "uMod_var_i8_L"
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
	.ascii "Mod_param_i8_C"
	.space 1
	.ascii "uMod_param_i8_C"
	.space 1
	.ascii "Mod_var_i8_C"
	.space 1
	.ascii "uMod_var_i8_C"
	.space 1
	.ascii "Mod_param_i8_u32"
	.space 1
	.ascii "uMod_param_i8_u32"
	.space 1
	.ascii "Mod_var_i8_u32"
	.space 1
	.ascii "uMod_var_i8_u32"
	.space 1
	.ascii "Mod_param_i8_i8"
	.space 1
	.ascii "uMod_param_i8_i8"
	.space 1
	.ascii "Mod_var_i8_i8"
	.space 1
	.ascii "uMod_var_i8_i8"
	.space 1
	.ascii "Mod_param_i8_I"
	.space 1
	.ascii "uMod_param_i8_I"
	.space 1
	.ascii "Mod_var_i8_I"
	.space 1
	.ascii "uMod_var_i8_I"
	.space 1
	.ascii "Mod_param_i8_u8"
	.space 1
	.ascii "uMod_param_i8_u8"
	.space 1
	.ascii "Mod_var_i8_u8"
	.space 1
	.ascii "uMod_var_i8_u8"
	.space 1
	.ascii "Mod_param_i8_LC"
	.space 1
	.ascii "uMod_param_i8_LC"
	.space 1
	.ascii "Mod_var_i8_LC"
	.space 1
	.ascii "uMod_var_i8_LC"
	.space 1
	.ascii "Mod_param_i8_i64"
	.space 1
	.ascii "uMod_param_i8_i64"
	.space 1
	.ascii "Mod_var_i8_i64"
	.space 1
	.ascii "uMod_var_i8_i64"
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
	.ascii "Mod_param_I_L"
	.space 1
	.ascii "uMod_param_I_L"
	.space 1
	.ascii "Mod_var_I_L"
	.space 1
	.ascii "uMod_var_I_L"
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
	.ascii "Mod_param_I_C"
	.space 1
	.ascii "uMod_param_I_C"
	.space 1
	.ascii "Mod_var_I_C"
	.space 1
	.ascii "uMod_var_I_C"
	.space 1
	.ascii "Mod_param_I_u32"
	.space 1
	.ascii "uMod_param_I_u32"
	.space 1
	.ascii "Mod_var_I_u32"
	.space 1
	.ascii "uMod_var_I_u32"
	.space 1
	.ascii "Mod_param_I_i8"
	.space 1
	.ascii "uMod_param_I_i8"
	.space 1
	.ascii "Mod_var_I_i8"
	.space 1
	.ascii "uMod_var_I_i8"
	.space 1
	.ascii "Mod_param_I_I"
	.space 1
	.ascii "uMod_param_I_I"
	.space 1
	.ascii "Mod_var_I_I"
	.space 1
	.ascii "uMod_var_I_I"
	.space 1
	.ascii "Mod_param_I_u8"
	.space 1
	.ascii "uMod_param_I_u8"
	.space 1
	.ascii "Mod_var_I_u8"
	.space 1
	.ascii "uMod_var_I_u8"
	.space 1
	.ascii "Mod_param_I_LC"
	.space 1
	.ascii "uMod_param_I_LC"
	.space 1
	.ascii "Mod_var_I_LC"
	.space 1
	.ascii "uMod_var_I_LC"
	.space 1
	.ascii "Mod_param_I_i64"
	.space 1
	.ascii "uMod_param_I_i64"
	.space 1
	.ascii "Mod_var_I_i64"
	.space 1
	.ascii "uMod_var_I_i64"
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
	.ascii "Mod_param_u8_L"
	.space 1
	.ascii "uMod_param_u8_L"
	.space 1
	.ascii "Mod_var_u8_L"
	.space 1
	.ascii "uMod_var_u8_L"
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
	.ascii "Mod_param_u8_C"
	.space 1
	.ascii "uMod_param_u8_C"
	.space 1
	.ascii "Mod_var_u8_C"
	.space 1
	.ascii "uMod_var_u8_C"
	.space 1
	.ascii "Mod_param_u8_u32"
	.space 1
	.ascii "uMod_param_u8_u32"
	.space 1
	.ascii "Mod_var_u8_u32"
	.space 1
	.ascii "uMod_var_u8_u32"
	.space 1
	.ascii "Mod_param_u8_i8"
	.space 1
	.ascii "uMod_param_u8_i8"
	.space 1
	.ascii "Mod_var_u8_i8"
	.space 1
	.ascii "uMod_var_u8_i8"
	.space 1
	.ascii "Mod_param_u8_I"
	.space 1
	.ascii "uMod_param_u8_I"
	.space 1
	.ascii "Mod_var_u8_I"
	.space 1
	.ascii "uMod_var_u8_I"
	.space 1
	.ascii "Mod_param_u8_u8"
	.space 1
	.ascii "uMod_param_u8_u8"
	.space 1
	.ascii "Mod_var_u8_u8"
	.space 1
	.ascii "uMod_var_u8_u8"
	.space 1
	.ascii "Mod_param_u8_LC"
	.space 1
	.ascii "uMod_param_u8_LC"
	.space 1
	.ascii "Mod_var_u8_LC"
	.space 1
	.ascii "uMod_var_u8_LC"
	.space 1
	.ascii "Mod_param_u8_i64"
	.space 1
	.ascii "uMod_param_u8_i64"
	.space 1
	.ascii "Mod_var_u8_i64"
	.space 1
	.ascii "uMod_var_u8_i64"
	.space 1
	.ascii "Mod_param_f32_f32"
	.space 1
	.ascii "Mod_var_f32_f32"
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
	.ascii "Mod_param_LC_L"
	.space 1
	.ascii "uMod_param_LC_L"
	.space 1
	.ascii "Mod_var_LC_L"
	.space 1
	.ascii "uMod_var_LC_L"
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
	.ascii "Mod_param_LC_C"
	.space 1
	.ascii "uMod_param_LC_C"
	.space 1
	.ascii "Mod_var_LC_C"
	.space 1
	.ascii "uMod_var_LC_C"
	.space 1
	.ascii "Mod_param_LC_u32"
	.space 1
	.ascii "uMod_param_LC_u32"
	.space 1
	.ascii "Mod_var_LC_u32"
	.space 1
	.ascii "uMod_var_LC_u32"
	.space 1
	.ascii "Mod_param_LC_i8"
	.space 1
	.ascii "uMod_param_LC_i8"
	.space 1
	.ascii "Mod_var_LC_i8"
	.space 1
	.ascii "uMod_var_LC_i8"
	.space 1
	.ascii "Mod_param_LC_I"
	.space 1
	.ascii "uMod_param_LC_I"
	.space 1
	.ascii "Mod_var_LC_I"
	.space 1
	.ascii "uMod_var_LC_I"
	.space 1
	.ascii "Mod_param_LC_u8"
	.space 1
	.ascii "uMod_param_LC_u8"
	.space 1
	.ascii "Mod_var_LC_u8"
	.space 1
	.ascii "uMod_var_LC_u8"
	.space 1
	.ascii "Mod_param_LC_LC"
	.space 1
	.ascii "uMod_param_LC_LC"
	.space 1
	.ascii "Mod_var_LC_LC"
	.space 1
	.ascii "uMod_var_LC_LC"
	.space 1
	.ascii "Mod_param_LC_i64"
	.space 1
	.ascii "uMod_param_LC_i64"
	.space 1
	.ascii "Mod_var_LC_i64"
	.space 1
	.ascii "uMod_var_LC_i64"
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
	.ascii "Mod_param_i64_L"
	.space 1
	.ascii "uMod_param_i64_L"
	.space 1
	.ascii "Mod_var_i64_L"
	.space 1
	.ascii "uMod_var_i64_L"
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
	.ascii "Mod_param_i64_C"
	.space 1
	.ascii "uMod_param_i64_C"
	.space 1
	.ascii "Mod_var_i64_C"
	.space 1
	.ascii "uMod_var_i64_C"
	.space 1
	.ascii "Mod_param_i64_u32"
	.space 1
	.ascii "uMod_param_i64_u32"
	.space 1
	.ascii "Mod_var_i64_u32"
	.space 1
	.ascii "uMod_var_i64_u32"
	.space 1
	.ascii "Mod_param_i64_i8"
	.space 1
	.ascii "uMod_param_i64_i8"
	.space 1
	.ascii "Mod_var_i64_i8"
	.space 1
	.ascii "uMod_var_i64_i8"
	.space 1
	.ascii "Mod_param_i64_I"
	.space 1
	.ascii "uMod_param_i64_I"
	.space 1
	.ascii "Mod_var_i64_I"
	.space 1
	.ascii "uMod_var_i64_I"
	.space 1
	.ascii "Mod_param_i64_u8"
	.space 1
	.ascii "uMod_param_i64_u8"
	.space 1
	.ascii "Mod_var_i64_u8"
	.space 1
	.ascii "uMod_var_i64_u8"
	.space 1
	.ascii "Mod_param_i64_LC"
	.space 1
	.ascii "uMod_param_i64_LC"
	.space 1
	.ascii "Mod_var_i64_LC"
	.space 1
	.ascii "uMod_var_i64_LC"
	.space 1
	.ascii "Mod_param_i64_i64"
	.space 1
	.ascii "uMod_param_i64_i64"
	.space 1
	.ascii "Mod_var_i64_i64"
	.space 1
	.ascii "uMod_var_i64_i64"
	.space 6
	.quad	_Mod_M3
	.quad	_L_1
	.quad	_Mod__Mod_param_i32_i32
	.quad	_L_1+7
	.quad	_Mod__uMod_param_i32_i32
	.quad	_L_1+25
	.quad	_Mod__Mod_var_i32_i32
	.quad	_L_1+44
	.quad	_Mod__uMod_var_i32_i32
	.quad	_L_1+60
	.quad	_Mod__Mod_param_i32_i16
	.quad	_L_1+77
	.quad	_Mod__uMod_param_i32_i16
	.quad	_L_1+95
	.quad	_Mod__Mod_var_i32_i16
	.quad	_L_1+114
	.quad	_Mod__uMod_var_i32_i16
	.quad	_L_1+130
	.quad	_Mod__Mod_param_i32_L
	.quad	_L_1+147
	.quad	_Mod__uMod_param_i32_L
	.quad	_L_1+163
	.quad	_Mod__Mod_var_i32_L
	.quad	_L_1+180
	.quad	_Mod__uMod_var_i32_L
	.quad	_L_1+194
	.quad	_Mod__Mod_param_i32_u64
	.quad	_L_1+209
	.quad	_Mod__uMod_param_i32_u64
	.quad	_L_1+227
	.quad	_Mod__Mod_var_i32_u64
	.quad	_L_1+246
	.quad	_Mod__uMod_var_i32_u64
	.quad	_L_1+262
	.quad	_Mod__Mod_param_i32_u16
	.quad	_L_1+279
	.quad	_Mod__uMod_param_i32_u16
	.quad	_L_1+297
	.quad	_Mod__Mod_var_i32_u16
	.quad	_L_1+316
	.quad	_Mod__uMod_var_i32_u16
	.quad	_L_1+332
	.quad	_Mod__Mod_param_i32_C
	.quad	_L_1+349
	.quad	_Mod__uMod_param_i32_C
	.quad	_L_1+365
	.quad	_Mod__Mod_var_i32_C
	.quad	_L_1+382
	.quad	_Mod__uMod_var_i32_C
	.quad	_L_1+396
	.quad	_Mod__Mod_param_i32_u32
	.quad	_L_1+411
	.quad	_Mod__uMod_param_i32_u32
	.quad	_L_1+429
	.quad	_Mod__Mod_var_i32_u32
	.quad	_L_1+448
	.quad	_Mod__uMod_var_i32_u32
	.quad	_L_1+464
	.quad	_Mod__Mod_param_i32_i8
	.quad	_L_1+481
	.quad	_Mod__uMod_param_i32_i8
	.quad	_L_1+498
	.quad	_Mod__Mod_var_i32_i8
	.quad	_L_1+516
	.quad	_Mod__uMod_var_i32_i8
	.quad	_L_1+531
	.quad	_Mod__Mod_param_i32_I
	.quad	_L_1+547
	.quad	_Mod__uMod_param_i32_I
	.quad	_L_1+563
	.quad	_Mod__Mod_var_i32_I
	.quad	_L_1+580
	.quad	_Mod__uMod_var_i32_I
	.quad	_L_1+594
	.quad	_Mod__Mod_param_i32_u8
	.quad	_L_1+609
	.quad	_Mod__uMod_param_i32_u8
	.quad	_L_1+626
	.quad	_Mod__Mod_var_i32_u8
	.quad	_L_1+644
	.quad	_Mod__uMod_var_i32_u8
	.quad	_L_1+659
	.quad	_Mod__Mod_param_i32_LC
	.quad	_L_1+675
	.quad	_Mod__uMod_param_i32_LC
	.quad	_L_1+692
	.quad	_Mod__Mod_var_i32_LC
	.quad	_L_1+710
	.quad	_Mod__uMod_var_i32_LC
	.quad	_L_1+725
	.quad	_Mod__Mod_param_i32_i64
	.quad	_L_1+741
	.quad	_Mod__uMod_param_i32_i64
	.quad	_L_1+759
	.quad	_Mod__Mod_var_i32_i64
	.quad	_L_1+778
	.quad	_Mod__uMod_var_i32_i64
	.quad	_L_1+794
	.quad	_Mod__Mod_param_i16_i32
	.quad	_L_1+811
	.quad	_Mod__uMod_param_i16_i32
	.quad	_L_1+829
	.quad	_Mod__Mod_var_i16_i32
	.quad	_L_1+848
	.quad	_Mod__uMod_var_i16_i32
	.quad	_L_1+864
	.quad	_Mod__Mod_param_i16_i16
	.quad	_L_1+881
	.quad	_Mod__uMod_param_i16_i16
	.quad	_L_1+899
	.quad	_Mod__Mod_var_i16_i16
	.quad	_L_1+918
	.quad	_Mod__uMod_var_i16_i16
	.quad	_L_1+934
	.quad	_Mod__Mod_param_i16_L
	.quad	_L_1+951
	.quad	_Mod__uMod_param_i16_L
	.quad	_L_1+967
	.quad	_Mod__Mod_var_i16_L
	.quad	_L_1+984
	.quad	_Mod__uMod_var_i16_L
	.quad	_L_1+998
	.quad	_Mod__Mod_param_i16_u64
	.quad	_L_1+1013
	.quad	_Mod__uMod_param_i16_u64
	.quad	_L_1+1031
	.quad	_Mod__Mod_var_i16_u64
	.quad	_L_1+1050
	.quad	_Mod__uMod_var_i16_u64
	.quad	_L_1+1066
	.quad	_Mod__Mod_param_i16_u16
	.quad	_L_1+1083
	.quad	_Mod__uMod_param_i16_u16
	.quad	_L_1+1101
	.quad	_Mod__Mod_var_i16_u16
	.quad	_L_1+1120
	.quad	_Mod__uMod_var_i16_u16
	.quad	_L_1+1136
	.quad	_Mod__Mod_param_i16_C
	.quad	_L_1+1153
	.quad	_Mod__uMod_param_i16_C
	.quad	_L_1+1169
	.quad	_Mod__Mod_var_i16_C
	.quad	_L_1+1186
	.quad	_Mod__uMod_var_i16_C
	.quad	_L_1+1200
	.quad	_Mod__Mod_param_i16_u32
	.quad	_L_1+1215
	.quad	_Mod__uMod_param_i16_u32
	.quad	_L_1+1233
	.quad	_Mod__Mod_var_i16_u32
	.quad	_L_1+1252
	.quad	_Mod__uMod_var_i16_u32
	.quad	_L_1+1268
	.quad	_Mod__Mod_param_i16_i8
	.quad	_L_1+1285
	.quad	_Mod__uMod_param_i16_i8
	.quad	_L_1+1302
	.quad	_Mod__Mod_var_i16_i8
	.quad	_L_1+1320
	.quad	_Mod__uMod_var_i16_i8
	.quad	_L_1+1335
	.quad	_Mod__Mod_param_i16_I
	.quad	_L_1+1351
	.quad	_Mod__uMod_param_i16_I
	.quad	_L_1+1367
	.quad	_Mod__Mod_var_i16_I
	.quad	_L_1+1384
	.quad	_Mod__uMod_var_i16_I
	.quad	_L_1+1398
	.quad	_Mod__Mod_param_i16_u8
	.quad	_L_1+1413
	.quad	_Mod__uMod_param_i16_u8
	.quad	_L_1+1430
	.quad	_Mod__Mod_var_i16_u8
	.quad	_L_1+1448
	.quad	_Mod__uMod_var_i16_u8
	.quad	_L_1+1463
	.quad	_Mod__Mod_param_i16_LC
	.quad	_L_1+1479
	.quad	_Mod__uMod_param_i16_LC
	.quad	_L_1+1496
	.quad	_Mod__Mod_var_i16_LC
	.quad	_L_1+1514
	.quad	_Mod__uMod_var_i16_LC
	.quad	_L_1+1529
	.quad	_Mod__Mod_param_i16_i64
	.quad	_L_1+1545
	.quad	_Mod__uMod_param_i16_i64
	.quad	_L_1+1563
	.quad	_Mod__Mod_var_i16_i64
	.quad	_L_1+1582
	.quad	_Mod__uMod_var_i16_i64
	.quad	_L_1+1598
	.quad	_Mod__Mod_param_L_i32
	.quad	_L_1+1615
	.quad	_Mod__uMod_param_L_i32
	.quad	_L_1+1631
	.quad	_Mod__Mod_var_L_i32
	.quad	_L_1+1648
	.quad	_Mod__uMod_var_L_i32
	.quad	_L_1+1662
	.quad	_Mod__Mod_param_L_i16
	.quad	_L_1+1677
	.quad	_Mod__uMod_param_L_i16
	.quad	_L_1+1693
	.quad	_Mod__Mod_var_L_i16
	.quad	_L_1+1710
	.quad	_Mod__uMod_var_L_i16
	.quad	_L_1+1724
	.quad	_Mod__Mod_param_L_L
	.quad	_L_1+1739
	.quad	_Mod__uMod_param_L_L
	.quad	_L_1+1753
	.quad	_Mod__Mod_var_L_L
	.quad	_L_1+1768
	.quad	_Mod__uMod_var_L_L
	.quad	_L_1+1780
	.quad	_Mod__Mod_param_L_u64
	.quad	_L_1+1793
	.quad	_Mod__uMod_param_L_u64
	.quad	_L_1+1809
	.quad	_Mod__Mod_var_L_u64
	.quad	_L_1+1826
	.quad	_Mod__uMod_var_L_u64
	.quad	_L_1+1840
	.quad	_Mod__Mod_param_L_u16
	.quad	_L_1+1855
	.quad	_Mod__uMod_param_L_u16
	.quad	_L_1+1871
	.quad	_Mod__Mod_var_L_u16
	.quad	_L_1+1888
	.quad	_Mod__uMod_var_L_u16
	.quad	_L_1+1902
	.quad	_Mod__Mod_param_L_C
	.quad	_L_1+1917
	.quad	_Mod__uMod_param_L_C
	.quad	_L_1+1931
	.quad	_Mod__Mod_var_L_C
	.quad	_L_1+1946
	.quad	_Mod__uMod_var_L_C
	.quad	_L_1+1958
	.quad	_Mod__Mod_param_L_u32
	.quad	_L_1+1971
	.quad	_Mod__uMod_param_L_u32
	.quad	_L_1+1987
	.quad	_Mod__Mod_var_L_u32
	.quad	_L_1+2004
	.quad	_Mod__uMod_var_L_u32
	.quad	_L_1+2018
	.quad	_Mod__Mod_param_L_i8
	.quad	_L_1+2033
	.quad	_Mod__uMod_param_L_i8
	.quad	_L_1+2048
	.quad	_Mod__Mod_var_L_i8
	.quad	_L_1+2064
	.quad	_Mod__uMod_var_L_i8
	.quad	_L_1+2077
	.quad	_Mod__Mod_param_L_I
	.quad	_L_1+2091
	.quad	_Mod__uMod_param_L_I
	.quad	_L_1+2105
	.quad	_Mod__Mod_var_L_I
	.quad	_L_1+2120
	.quad	_Mod__uMod_var_L_I
	.quad	_L_1+2132
	.quad	_Mod__Mod_param_L_u8
	.quad	_L_1+2145
	.quad	_Mod__uMod_param_L_u8
	.quad	_L_1+2160
	.quad	_Mod__Mod_var_L_u8
	.quad	_L_1+2176
	.quad	_Mod__uMod_var_L_u8
	.quad	_L_1+2189
	.quad	_Mod__Mod_param_L_LC
	.quad	_L_1+2203
	.quad	_Mod__uMod_param_L_LC
	.quad	_L_1+2218
	.quad	_Mod__Mod_var_L_LC
	.quad	_L_1+2234
	.quad	_Mod__uMod_var_L_LC
	.quad	_L_1+2247
	.quad	_Mod__Mod_param_L_i64
	.quad	_L_1+2261
	.quad	_Mod__uMod_param_L_i64
	.quad	_L_1+2277
	.quad	_Mod__Mod_var_L_i64
	.quad	_L_1+2294
	.quad	_Mod__uMod_var_L_i64
	.quad	_L_1+2308
	.quad	_Mod__Mod_param_f64_f64
	.quad	_L_1+2323
	.quad	_Mod__Mod_var_f64_f64
	.quad	_L_1+2341
	.quad	_Mod__Mod_param_u64_i32
	.quad	_L_1+2357
	.quad	_Mod__uMod_param_u64_i32
	.quad	_L_1+2375
	.quad	_Mod__Mod_var_u64_i32
	.quad	_L_1+2394
	.quad	_Mod__uMod_var_u64_i32
	.quad	_L_1+2410
	.quad	_Mod__Mod_param_u64_i16
	.quad	_L_1+2427
	.quad	_Mod__uMod_param_u64_i16
	.quad	_L_1+2445
	.quad	_Mod__Mod_var_u64_i16
	.quad	_L_1+2464
	.quad	_Mod__uMod_var_u64_i16
	.quad	_L_1+2480
	.quad	_Mod__Mod_param_u64_L
	.quad	_L_1+2497
	.quad	_Mod__uMod_param_u64_L
	.quad	_L_1+2513
	.quad	_Mod__Mod_var_u64_L
	.quad	_L_1+2530
	.quad	_Mod__uMod_var_u64_L
	.quad	_L_1+2544
	.quad	_Mod__Mod_param_u64_u64
	.quad	_L_1+2559
	.quad	_Mod__uMod_param_u64_u64
	.quad	_L_1+2577
	.quad	_Mod__Mod_var_u64_u64
	.quad	_L_1+2596
	.quad	_Mod__uMod_var_u64_u64
	.quad	_L_1+2612
	.quad	_Mod__Mod_param_u64_u16
	.quad	_L_1+2629
	.quad	_Mod__uMod_param_u64_u16
	.quad	_L_1+2647
	.quad	_Mod__Mod_var_u64_u16
	.quad	_L_1+2666
	.quad	_Mod__uMod_var_u64_u16
	.quad	_L_1+2682
	.quad	_Mod__Mod_param_u64_C
	.quad	_L_1+2699
	.quad	_Mod__uMod_param_u64_C
	.quad	_L_1+2715
	.quad	_Mod__Mod_var_u64_C
	.quad	_L_1+2732
	.quad	_Mod__uMod_var_u64_C
	.quad	_L_1+2746
	.quad	_Mod__Mod_param_u64_u32
	.quad	_L_1+2761
	.quad	_Mod__uMod_param_u64_u32
	.quad	_L_1+2779
	.quad	_Mod__Mod_var_u64_u32
	.quad	_L_1+2798
	.quad	_Mod__uMod_var_u64_u32
	.quad	_L_1+2814
	.quad	_Mod__Mod_param_u64_i8
	.quad	_L_1+2831
	.quad	_Mod__uMod_param_u64_i8
	.quad	_L_1+2848
	.quad	_Mod__Mod_var_u64_i8
	.quad	_L_1+2866
	.quad	_Mod__uMod_var_u64_i8
	.quad	_L_1+2881
	.quad	_Mod__Mod_param_u64_I
	.quad	_L_1+2897
	.quad	_Mod__uMod_param_u64_I
	.quad	_L_1+2913
	.quad	_Mod__Mod_var_u64_I
	.quad	_L_1+2930
	.quad	_Mod__uMod_var_u64_I
	.quad	_L_1+2944
	.quad	_Mod__Mod_param_u64_u8
	.quad	_L_1+2959
	.quad	_Mod__uMod_param_u64_u8
	.quad	_L_1+2976
	.quad	_Mod__Mod_var_u64_u8
	.quad	_L_1+2994
	.quad	_Mod__uMod_var_u64_u8
	.quad	_L_1+3009
	.quad	_Mod__Mod_param_u64_LC
	.quad	_L_1+3025
	.quad	_Mod__uMod_param_u64_LC
	.quad	_L_1+3042
	.quad	_Mod__Mod_var_u64_LC
	.quad	_L_1+3060
	.quad	_Mod__uMod_var_u64_LC
	.quad	_L_1+3075
	.quad	_Mod__Mod_param_u64_i64
	.quad	_L_1+3091
	.quad	_Mod__uMod_param_u64_i64
	.quad	_L_1+3109
	.quad	_Mod__Mod_var_u64_i64
	.quad	_L_1+3128
	.quad	_Mod__uMod_var_u64_i64
	.quad	_L_1+3144
	.quad	_Mod__Mod_param_u16_i32
	.quad	_L_1+3161
	.quad	_Mod__uMod_param_u16_i32
	.quad	_L_1+3179
	.quad	_Mod__Mod_var_u16_i32
	.quad	_L_1+3198
	.quad	_Mod__uMod_var_u16_i32
	.quad	_L_1+3214
	.quad	_Mod__Mod_param_u16_i16
	.quad	_L_1+3231
	.quad	_Mod__uMod_param_u16_i16
	.quad	_L_1+3249
	.quad	_Mod__Mod_var_u16_i16
	.quad	_L_1+3268
	.quad	_Mod__uMod_var_u16_i16
	.quad	_L_1+3284
	.quad	_Mod__Mod_param_u16_L
	.quad	_L_1+3301
	.quad	_Mod__uMod_param_u16_L
	.quad	_L_1+3317
	.quad	_Mod__Mod_var_u16_L
	.quad	_L_1+3334
	.quad	_Mod__uMod_var_u16_L
	.quad	_L_1+3348
	.quad	_Mod__Mod_param_u16_u64
	.quad	_L_1+3363
	.quad	_Mod__uMod_param_u16_u64
	.quad	_L_1+3381
	.quad	_Mod__Mod_var_u16_u64
	.quad	_L_1+3400
	.quad	_Mod__uMod_var_u16_u64
	.quad	_L_1+3416
	.quad	_Mod__Mod_param_u16_u16
	.quad	_L_1+3433
	.quad	_Mod__uMod_param_u16_u16
	.quad	_L_1+3451
	.quad	_Mod__Mod_var_u16_u16
	.quad	_L_1+3470
	.quad	_Mod__uMod_var_u16_u16
	.quad	_L_1+3486
	.quad	_Mod__Mod_param_u16_C
	.quad	_L_1+3503
	.quad	_Mod__uMod_param_u16_C
	.quad	_L_1+3519
	.quad	_Mod__Mod_var_u16_C
	.quad	_L_1+3536
	.quad	_Mod__uMod_var_u16_C
	.quad	_L_1+3550
	.quad	_Mod__Mod_param_u16_u32
	.quad	_L_1+3565
	.quad	_Mod__uMod_param_u16_u32
	.quad	_L_1+3583
	.quad	_Mod__Mod_var_u16_u32
	.quad	_L_1+3602
	.quad	_Mod__uMod_var_u16_u32
	.quad	_L_1+3618
	.quad	_Mod__Mod_param_u16_i8
	.quad	_L_1+3635
	.quad	_Mod__uMod_param_u16_i8
	.quad	_L_1+3652
	.quad	_Mod__Mod_var_u16_i8
	.quad	_L_1+3670
	.quad	_Mod__uMod_var_u16_i8
	.quad	_L_1+3685
	.quad	_Mod__Mod_param_u16_I
	.quad	_L_1+3701
	.quad	_Mod__uMod_param_u16_I
	.quad	_L_1+3717
	.quad	_Mod__Mod_var_u16_I
	.quad	_L_1+3734
	.quad	_Mod__uMod_var_u16_I
	.quad	_L_1+3748
	.quad	_Mod__Mod_param_u16_u8
	.quad	_L_1+3763
	.quad	_Mod__uMod_param_u16_u8
	.quad	_L_1+3780
	.quad	_Mod__Mod_var_u16_u8
	.quad	_L_1+3798
	.quad	_Mod__uMod_var_u16_u8
	.quad	_L_1+3813
	.quad	_Mod__Mod_param_u16_LC
	.quad	_L_1+3829
	.quad	_Mod__uMod_param_u16_LC
	.quad	_L_1+3846
	.quad	_Mod__Mod_var_u16_LC
	.quad	_L_1+3864
	.quad	_Mod__uMod_var_u16_LC
	.quad	_L_1+3879
	.quad	_Mod__Mod_param_u16_i64
	.quad	_L_1+3895
	.quad	_Mod__uMod_param_u16_i64
	.quad	_L_1+3913
	.quad	_Mod__Mod_var_u16_i64
	.quad	_L_1+3932
	.quad	_Mod__uMod_var_u16_i64
	.quad	_L_1+3948
	.quad	_Mod__Mod_param_C_i32
	.quad	_L_1+3965
	.quad	_Mod__uMod_param_C_i32
	.quad	_L_1+3981
	.quad	_Mod__Mod_var_C_i32
	.quad	_L_1+3998
	.quad	_Mod__uMod_var_C_i32
	.quad	_L_1+4012
	.quad	_Mod__Mod_param_C_i16
	.quad	_L_1+4027
	.quad	_Mod__uMod_param_C_i16
	.quad	_L_1+4043
	.quad	_Mod__Mod_var_C_i16
	.quad	_L_1+4060
	.quad	_Mod__uMod_var_C_i16
	.quad	_L_1+4074
	.quad	_Mod__Mod_param_C_L
	.quad	_L_1+4089
	.quad	_Mod__uMod_param_C_L
	.quad	_L_1+4103
	.quad	_Mod__Mod_var_C_L
	.quad	_L_1+4118
	.quad	_Mod__uMod_var_C_L
	.quad	_L_1+4130
	.quad	_Mod__Mod_param_C_u64
	.quad	_L_1+4143
	.quad	_Mod__uMod_param_C_u64
	.quad	_L_1+4159
	.quad	_Mod__Mod_var_C_u64
	.quad	_L_1+4176
	.quad	_Mod__uMod_var_C_u64
	.quad	_L_1+4190
	.quad	_Mod__Mod_param_C_u16
	.quad	_L_1+4205
	.quad	_Mod__uMod_param_C_u16
	.quad	_L_1+4221
	.quad	_Mod__Mod_var_C_u16
	.quad	_L_1+4238
	.quad	_Mod__uMod_var_C_u16
	.quad	_L_1+4252
	.quad	_Mod__Mod_param_C_C
	.quad	_L_1+4267
	.quad	_Mod__uMod_param_C_C
	.quad	_L_1+4281
	.quad	_Mod__Mod_var_C_C
	.quad	_L_1+4296
	.quad	_Mod__uMod_var_C_C
	.quad	_L_1+4308
	.quad	_Mod__Mod_param_C_u32
	.quad	_L_1+4321
	.quad	_Mod__uMod_param_C_u32
	.quad	_L_1+4337
	.quad	_Mod__Mod_var_C_u32
	.quad	_L_1+4354
	.quad	_Mod__uMod_var_C_u32
	.quad	_L_1+4368
	.quad	_Mod__Mod_param_C_i8
	.quad	_L_1+4383
	.quad	_Mod__uMod_param_C_i8
	.quad	_L_1+4398
	.quad	_Mod__Mod_var_C_i8
	.quad	_L_1+4414
	.quad	_Mod__uMod_var_C_i8
	.quad	_L_1+4427
	.quad	_Mod__Mod_param_C_I
	.quad	_L_1+4441
	.quad	_Mod__uMod_param_C_I
	.quad	_L_1+4455
	.quad	_Mod__Mod_var_C_I
	.quad	_L_1+4470
	.quad	_Mod__uMod_var_C_I
	.quad	_L_1+4482
	.quad	_Mod__Mod_param_C_u8
	.quad	_L_1+4495
	.quad	_Mod__uMod_param_C_u8
	.quad	_L_1+4510
	.quad	_Mod__Mod_var_C_u8
	.quad	_L_1+4526
	.quad	_Mod__uMod_var_C_u8
	.quad	_L_1+4539
	.quad	_Mod__Mod_param_C_LC
	.quad	_L_1+4553
	.quad	_Mod__uMod_param_C_LC
	.quad	_L_1+4568
	.quad	_Mod__Mod_var_C_LC
	.quad	_L_1+4584
	.quad	_Mod__uMod_var_C_LC
	.quad	_L_1+4597
	.quad	_Mod__Mod_param_C_i64
	.quad	_L_1+4611
	.quad	_Mod__uMod_param_C_i64
	.quad	_L_1+4627
	.quad	_Mod__Mod_var_C_i64
	.quad	_L_1+4644
	.quad	_Mod__uMod_var_C_i64
	.quad	_L_1+4658
	.quad	_Mod__Mod_param_u32_i32
	.quad	_L_1+4673
	.quad	_Mod__uMod_param_u32_i32
	.quad	_L_1+4691
	.quad	_Mod__Mod_var_u32_i32
	.quad	_L_1+4710
	.quad	_Mod__uMod_var_u32_i32
	.quad	_L_1+4726
	.quad	_Mod__Mod_param_u32_i16
	.quad	_L_1+4743
	.quad	_Mod__uMod_param_u32_i16
	.quad	_L_1+4761
	.quad	_Mod__Mod_var_u32_i16
	.quad	_L_1+4780
	.quad	_Mod__uMod_var_u32_i16
	.quad	_L_1+4796
	.quad	_Mod__Mod_param_u32_L
	.quad	_L_1+4813
	.quad	_Mod__uMod_param_u32_L
	.quad	_L_1+4829
	.quad	_Mod__Mod_var_u32_L
	.quad	_L_1+4846
	.quad	_Mod__uMod_var_u32_L
	.quad	_L_1+4860
	.quad	_Mod__Mod_param_u32_u64
	.quad	_L_1+4875
	.quad	_Mod__uMod_param_u32_u64
	.quad	_L_1+4893
	.quad	_Mod__Mod_var_u32_u64
	.quad	_L_1+4912
	.quad	_Mod__uMod_var_u32_u64
	.quad	_L_1+4928
	.quad	_Mod__Mod_param_u32_u16
	.quad	_L_1+4945
	.quad	_Mod__uMod_param_u32_u16
	.quad	_L_1+4963
	.quad	_Mod__Mod_var_u32_u16
	.quad	_L_1+4982
	.quad	_Mod__uMod_var_u32_u16
	.quad	_L_1+4998
	.quad	_Mod__Mod_param_u32_C
	.quad	_L_1+5015
	.quad	_Mod__uMod_param_u32_C
	.quad	_L_1+5031
	.quad	_Mod__Mod_var_u32_C
	.quad	_L_1+5048
	.quad	_Mod__uMod_var_u32_C
	.quad	_L_1+5062
	.quad	_Mod__Mod_param_u32_u32
	.quad	_L_1+5077
	.quad	_Mod__uMod_param_u32_u32
	.quad	_L_1+5095
	.quad	_Mod__Mod_var_u32_u32
	.quad	_L_1+5114
	.quad	_Mod__uMod_var_u32_u32
	.quad	_L_1+5130
	.quad	_Mod__Mod_param_u32_i8
	.quad	_L_1+5147
	.quad	_Mod__uMod_param_u32_i8
	.quad	_L_1+5164
	.quad	_Mod__Mod_var_u32_i8
	.quad	_L_1+5182
	.quad	_Mod__uMod_var_u32_i8
	.quad	_L_1+5197
	.quad	_Mod__Mod_param_u32_I
	.quad	_L_1+5213
	.quad	_Mod__uMod_param_u32_I
	.quad	_L_1+5229
	.quad	_Mod__Mod_var_u32_I
	.quad	_L_1+5246
	.quad	_Mod__uMod_var_u32_I
	.quad	_L_1+5260
	.quad	_Mod__Mod_param_u32_u8
	.quad	_L_1+5275
	.quad	_Mod__uMod_param_u32_u8
	.quad	_L_1+5292
	.quad	_Mod__Mod_var_u32_u8
	.quad	_L_1+5310
	.quad	_Mod__uMod_var_u32_u8
	.quad	_L_1+5325
	.quad	_Mod__Mod_param_u32_LC
	.quad	_L_1+5341
	.quad	_Mod__uMod_param_u32_LC
	.quad	_L_1+5358
	.quad	_Mod__Mod_var_u32_LC
	.quad	_L_1+5376
	.quad	_Mod__uMod_var_u32_LC
	.quad	_L_1+5391
	.quad	_Mod__Mod_param_u32_i64
	.quad	_L_1+5407
	.quad	_Mod__uMod_param_u32_i64
	.quad	_L_1+5425
	.quad	_Mod__Mod_var_u32_i64
	.quad	_L_1+5444
	.quad	_Mod__uMod_var_u32_i64
	.quad	_L_1+5460
	.quad	_Mod__Mod_param_i8_i32
	.quad	_L_1+5477
	.quad	_Mod__uMod_param_i8_i32
	.quad	_L_1+5494
	.quad	_Mod__Mod_var_i8_i32
	.quad	_L_1+5512
	.quad	_Mod__uMod_var_i8_i32
	.quad	_L_1+5527
	.quad	_Mod__Mod_param_i8_i16
	.quad	_L_1+5543
	.quad	_Mod__uMod_param_i8_i16
	.quad	_L_1+5560
	.quad	_Mod__Mod_var_i8_i16
	.quad	_L_1+5578
	.quad	_Mod__uMod_var_i8_i16
	.quad	_L_1+5593
	.quad	_Mod__Mod_param_i8_L
	.quad	_L_1+5609
	.quad	_Mod__uMod_param_i8_L
	.quad	_L_1+5624
	.quad	_Mod__Mod_var_i8_L
	.quad	_L_1+5640
	.quad	_Mod__uMod_var_i8_L
	.quad	_L_1+5653
	.quad	_Mod__Mod_param_i8_u64
	.quad	_L_1+5667
	.quad	_Mod__uMod_param_i8_u64
	.quad	_L_1+5684
	.quad	_Mod__Mod_var_i8_u64
	.quad	_L_1+5702
	.quad	_Mod__uMod_var_i8_u64
	.quad	_L_1+5717
	.quad	_Mod__Mod_param_i8_u16
	.quad	_L_1+5733
	.quad	_Mod__uMod_param_i8_u16
	.quad	_L_1+5750
	.quad	_Mod__Mod_var_i8_u16
	.quad	_L_1+5768
	.quad	_Mod__uMod_var_i8_u16
	.quad	_L_1+5783
	.quad	_Mod__Mod_param_i8_C
	.quad	_L_1+5799
	.quad	_Mod__uMod_param_i8_C
	.quad	_L_1+5814
	.quad	_Mod__Mod_var_i8_C
	.quad	_L_1+5830
	.quad	_Mod__uMod_var_i8_C
	.quad	_L_1+5843
	.quad	_Mod__Mod_param_i8_u32
	.quad	_L_1+5857
	.quad	_Mod__uMod_param_i8_u32
	.quad	_L_1+5874
	.quad	_Mod__Mod_var_i8_u32
	.quad	_L_1+5892
	.quad	_Mod__uMod_var_i8_u32
	.quad	_L_1+5907
	.quad	_Mod__Mod_param_i8_i8
	.quad	_L_1+5923
	.quad	_Mod__uMod_param_i8_i8
	.quad	_L_1+5939
	.quad	_Mod__Mod_var_i8_i8
	.quad	_L_1+5956
	.quad	_Mod__uMod_var_i8_i8
	.quad	_L_1+5970
	.quad	_Mod__Mod_param_i8_I
	.quad	_L_1+5985
	.quad	_Mod__uMod_param_i8_I
	.quad	_L_1+6000
	.quad	_Mod__Mod_var_i8_I
	.quad	_L_1+6016
	.quad	_Mod__uMod_var_i8_I
	.quad	_L_1+6029
	.quad	_Mod__Mod_param_i8_u8
	.quad	_L_1+6043
	.quad	_Mod__uMod_param_i8_u8
	.quad	_L_1+6059
	.quad	_Mod__Mod_var_i8_u8
	.quad	_L_1+6076
	.quad	_Mod__uMod_var_i8_u8
	.quad	_L_1+6090
	.quad	_Mod__Mod_param_i8_LC
	.quad	_L_1+6105
	.quad	_Mod__uMod_param_i8_LC
	.quad	_L_1+6121
	.quad	_Mod__Mod_var_i8_LC
	.quad	_L_1+6138
	.quad	_Mod__uMod_var_i8_LC
	.quad	_L_1+6152
	.quad	_Mod__Mod_param_i8_i64
	.quad	_L_1+6167
	.quad	_Mod__uMod_param_i8_i64
	.quad	_L_1+6184
	.quad	_Mod__Mod_var_i8_i64
	.quad	_L_1+6202
	.quad	_Mod__uMod_var_i8_i64
	.quad	_L_1+6217
	.quad	_Mod__Mod_param_I_i32
	.quad	_L_1+6233
	.quad	_Mod__uMod_param_I_i32
	.quad	_L_1+6249
	.quad	_Mod__Mod_var_I_i32
	.quad	_L_1+6266
	.quad	_Mod__uMod_var_I_i32
	.quad	_L_1+6280
	.quad	_Mod__Mod_param_I_i16
	.quad	_L_1+6295
	.quad	_Mod__uMod_param_I_i16
	.quad	_L_1+6311
	.quad	_Mod__Mod_var_I_i16
	.quad	_L_1+6328
	.quad	_Mod__uMod_var_I_i16
	.quad	_L_1+6342
	.quad	_Mod__Mod_param_I_L
	.quad	_L_1+6357
	.quad	_Mod__uMod_param_I_L
	.quad	_L_1+6371
	.quad	_Mod__Mod_var_I_L
	.quad	_L_1+6386
	.quad	_Mod__uMod_var_I_L
	.quad	_L_1+6398
	.quad	_Mod__Mod_param_I_u64
	.quad	_L_1+6411
	.quad	_Mod__uMod_param_I_u64
	.quad	_L_1+6427
	.quad	_Mod__Mod_var_I_u64
	.quad	_L_1+6444
	.quad	_Mod__uMod_var_I_u64
	.quad	_L_1+6458
	.quad	_Mod__Mod_param_I_u16
	.quad	_L_1+6473
	.quad	_Mod__uMod_param_I_u16
	.quad	_L_1+6489
	.quad	_Mod__Mod_var_I_u16
	.quad	_L_1+6506
	.quad	_Mod__uMod_var_I_u16
	.quad	_L_1+6520
	.quad	_Mod__Mod_param_I_C
	.quad	_L_1+6535
	.quad	_Mod__uMod_param_I_C
	.quad	_L_1+6549
	.quad	_Mod__Mod_var_I_C
	.quad	_L_1+6564
	.quad	_Mod__uMod_var_I_C
	.quad	_L_1+6576
	.quad	_Mod__Mod_param_I_u32
	.quad	_L_1+6589
	.quad	_Mod__uMod_param_I_u32
	.quad	_L_1+6605
	.quad	_Mod__Mod_var_I_u32
	.quad	_L_1+6622
	.quad	_Mod__uMod_var_I_u32
	.quad	_L_1+6636
	.quad	_Mod__Mod_param_I_i8
	.quad	_L_1+6651
	.quad	_Mod__uMod_param_I_i8
	.quad	_L_1+6666
	.quad	_Mod__Mod_var_I_i8
	.quad	_L_1+6682
	.quad	_Mod__uMod_var_I_i8
	.quad	_L_1+6695
	.quad	_Mod__Mod_param_I_I
	.quad	_L_1+6709
	.quad	_Mod__uMod_param_I_I
	.quad	_L_1+6723
	.quad	_Mod__Mod_var_I_I
	.quad	_L_1+6738
	.quad	_Mod__uMod_var_I_I
	.quad	_L_1+6750
	.quad	_Mod__Mod_param_I_u8
	.quad	_L_1+6763
	.quad	_Mod__uMod_param_I_u8
	.quad	_L_1+6778
	.quad	_Mod__Mod_var_I_u8
	.quad	_L_1+6794
	.quad	_Mod__uMod_var_I_u8
	.quad	_L_1+6807
	.quad	_Mod__Mod_param_I_LC
	.quad	_L_1+6821
	.quad	_Mod__uMod_param_I_LC
	.quad	_L_1+6836
	.quad	_Mod__Mod_var_I_LC
	.quad	_L_1+6852
	.quad	_Mod__uMod_var_I_LC
	.quad	_L_1+6865
	.quad	_Mod__Mod_param_I_i64
	.quad	_L_1+6879
	.quad	_Mod__uMod_param_I_i64
	.quad	_L_1+6895
	.quad	_Mod__Mod_var_I_i64
	.quad	_L_1+6912
	.quad	_Mod__uMod_var_I_i64
	.quad	_L_1+6926
	.quad	_Mod__Mod_param_u8_i32
	.quad	_L_1+6941
	.quad	_Mod__uMod_param_u8_i32
	.quad	_L_1+6958
	.quad	_Mod__Mod_var_u8_i32
	.quad	_L_1+6976
	.quad	_Mod__uMod_var_u8_i32
	.quad	_L_1+6991
	.quad	_Mod__Mod_param_u8_i16
	.quad	_L_1+7007
	.quad	_Mod__uMod_param_u8_i16
	.quad	_L_1+7024
	.quad	_Mod__Mod_var_u8_i16
	.quad	_L_1+7042
	.quad	_Mod__uMod_var_u8_i16
	.quad	_L_1+7057
	.quad	_Mod__Mod_param_u8_L
	.quad	_L_1+7073
	.quad	_Mod__uMod_param_u8_L
	.quad	_L_1+7088
	.quad	_Mod__Mod_var_u8_L
	.quad	_L_1+7104
	.quad	_Mod__uMod_var_u8_L
	.quad	_L_1+7117
	.quad	_Mod__Mod_param_u8_u64
	.quad	_L_1+7131
	.quad	_Mod__uMod_param_u8_u64
	.quad	_L_1+7148
	.quad	_Mod__Mod_var_u8_u64
	.quad	_L_1+7166
	.quad	_Mod__uMod_var_u8_u64
	.quad	_L_1+7181
	.quad	_Mod__Mod_param_u8_u16
	.quad	_L_1+7197
	.quad	_Mod__uMod_param_u8_u16
	.quad	_L_1+7214
	.quad	_Mod__Mod_var_u8_u16
	.quad	_L_1+7232
	.quad	_Mod__uMod_var_u8_u16
	.quad	_L_1+7247
	.quad	_Mod__Mod_param_u8_C
	.quad	_L_1+7263
	.quad	_Mod__uMod_param_u8_C
	.quad	_L_1+7278
	.quad	_Mod__Mod_var_u8_C
	.quad	_L_1+7294
	.quad	_Mod__uMod_var_u8_C
	.quad	_L_1+7307
	.quad	_Mod__Mod_param_u8_u32
	.quad	_L_1+7321
	.quad	_Mod__uMod_param_u8_u32
	.quad	_L_1+7338
	.quad	_Mod__Mod_var_u8_u32
	.quad	_L_1+7356
	.quad	_Mod__uMod_var_u8_u32
	.quad	_L_1+7371
	.quad	_Mod__Mod_param_u8_i8
	.quad	_L_1+7387
	.quad	_Mod__uMod_param_u8_i8
	.quad	_L_1+7403
	.quad	_Mod__Mod_var_u8_i8
	.quad	_L_1+7420
	.quad	_Mod__uMod_var_u8_i8
	.quad	_L_1+7434
	.quad	_Mod__Mod_param_u8_I
	.quad	_L_1+7449
	.quad	_Mod__uMod_param_u8_I
	.quad	_L_1+7464
	.quad	_Mod__Mod_var_u8_I
	.quad	_L_1+7480
	.quad	_Mod__uMod_var_u8_I
	.quad	_L_1+7493
	.quad	_Mod__Mod_param_u8_u8
	.quad	_L_1+7507
	.quad	_Mod__uMod_param_u8_u8
	.quad	_L_1+7523
	.quad	_Mod__Mod_var_u8_u8
	.quad	_L_1+7540
	.quad	_Mod__uMod_var_u8_u8
	.quad	_L_1+7554
	.quad	_Mod__Mod_param_u8_LC
	.quad	_L_1+7569
	.quad	_Mod__uMod_param_u8_LC
	.quad	_L_1+7585
	.quad	_Mod__Mod_var_u8_LC
	.quad	_L_1+7602
	.quad	_Mod__uMod_var_u8_LC
	.quad	_L_1+7616
	.quad	_Mod__Mod_param_u8_i64
	.quad	_L_1+7631
	.quad	_Mod__uMod_param_u8_i64
	.quad	_L_1+7648
	.quad	_Mod__Mod_var_u8_i64
	.quad	_L_1+7666
	.quad	_Mod__uMod_var_u8_i64
	.quad	_L_1+7681
	.quad	_Mod__Mod_param_f32_f32
	.quad	_L_1+7697
	.quad	_Mod__Mod_var_f32_f32
	.quad	_L_1+7715
	.quad	_Mod__Mod_param_LC_i32
	.quad	_L_1+7731
	.quad	_Mod__uMod_param_LC_i32
	.quad	_L_1+7748
	.quad	_Mod__Mod_var_LC_i32
	.quad	_L_1+7766
	.quad	_Mod__uMod_var_LC_i32
	.quad	_L_1+7781
	.quad	_Mod__Mod_param_LC_i16
	.quad	_L_1+7797
	.quad	_Mod__uMod_param_LC_i16
	.quad	_L_1+7814
	.quad	_Mod__Mod_var_LC_i16
	.quad	_L_1+7832
	.quad	_Mod__uMod_var_LC_i16
	.quad	_L_1+7847
	.quad	_Mod__Mod_param_LC_L
	.quad	_L_1+7863
	.quad	_Mod__uMod_param_LC_L
	.quad	_L_1+7878
	.quad	_Mod__Mod_var_LC_L
	.quad	_L_1+7894
	.quad	_Mod__uMod_var_LC_L
	.quad	_L_1+7907
	.quad	_Mod__Mod_param_LC_u64
	.quad	_L_1+7921
	.quad	_Mod__uMod_param_LC_u64
	.quad	_L_1+7938
	.quad	_Mod__Mod_var_LC_u64
	.quad	_L_1+7956
	.quad	_Mod__uMod_var_LC_u64
	.quad	_L_1+7971
	.quad	_Mod__Mod_param_LC_u16
	.quad	_L_1+7987
	.quad	_Mod__uMod_param_LC_u16
	.quad	_L_1+8004
	.quad	_Mod__Mod_var_LC_u16
	.quad	_L_1+8022
	.quad	_Mod__uMod_var_LC_u16
	.quad	_L_1+8037
	.quad	_Mod__Mod_param_LC_C
	.quad	_L_1+8053
	.quad	_Mod__uMod_param_LC_C
	.quad	_L_1+8068
	.quad	_Mod__Mod_var_LC_C
	.quad	_L_1+8084
	.quad	_Mod__uMod_var_LC_C
	.quad	_L_1+8097
	.quad	_Mod__Mod_param_LC_u32
	.quad	_L_1+8111
	.quad	_Mod__uMod_param_LC_u32
	.quad	_L_1+8128
	.quad	_Mod__Mod_var_LC_u32
	.quad	_L_1+8146
	.quad	_Mod__uMod_var_LC_u32
	.quad	_L_1+8161
	.quad	_Mod__Mod_param_LC_i8
	.quad	_L_1+8177
	.quad	_Mod__uMod_param_LC_i8
	.quad	_L_1+8193
	.quad	_Mod__Mod_var_LC_i8
	.quad	_L_1+8210
	.quad	_Mod__uMod_var_LC_i8
	.quad	_L_1+8224
	.quad	_Mod__Mod_param_LC_I
	.quad	_L_1+8239
	.quad	_Mod__uMod_param_LC_I
	.quad	_L_1+8254
	.quad	_Mod__Mod_var_LC_I
	.quad	_L_1+8270
	.quad	_Mod__uMod_var_LC_I
	.quad	_L_1+8283
	.quad	_Mod__Mod_param_LC_u8
	.quad	_L_1+8297
	.quad	_Mod__uMod_param_LC_u8
	.quad	_L_1+8313
	.quad	_Mod__Mod_var_LC_u8
	.quad	_L_1+8330
	.quad	_Mod__uMod_var_LC_u8
	.quad	_L_1+8344
	.quad	_Mod__Mod_param_LC_LC
	.quad	_L_1+8359
	.quad	_Mod__uMod_param_LC_LC
	.quad	_L_1+8375
	.quad	_Mod__Mod_var_LC_LC
	.quad	_L_1+8392
	.quad	_Mod__uMod_var_LC_LC
	.quad	_L_1+8406
	.quad	_Mod__Mod_param_LC_i64
	.quad	_L_1+8421
	.quad	_Mod__uMod_param_LC_i64
	.quad	_L_1+8438
	.quad	_Mod__Mod_var_LC_i64
	.quad	_L_1+8456
	.quad	_Mod__uMod_var_LC_i64
	.quad	_L_1+8471
	.quad	_Mod__Mod_param_i64_i32
	.quad	_L_1+8487
	.quad	_Mod__uMod_param_i64_i32
	.quad	_L_1+8505
	.quad	_Mod__Mod_var_i64_i32
	.quad	_L_1+8524
	.quad	_Mod__uMod_var_i64_i32
	.quad	_L_1+8540
	.quad	_Mod__Mod_param_i64_i16
	.quad	_L_1+8557
	.quad	_Mod__uMod_param_i64_i16
	.quad	_L_1+8575
	.quad	_Mod__Mod_var_i64_i16
	.quad	_L_1+8594
	.quad	_Mod__uMod_var_i64_i16
	.quad	_L_1+8610
	.quad	_Mod__Mod_param_i64_L
	.quad	_L_1+8627
	.quad	_Mod__uMod_param_i64_L
	.quad	_L_1+8643
	.quad	_Mod__Mod_var_i64_L
	.quad	_L_1+8660
	.quad	_Mod__uMod_var_i64_L
	.quad	_L_1+8674
	.quad	_Mod__Mod_param_i64_u64
	.quad	_L_1+8689
	.quad	_Mod__uMod_param_i64_u64
	.quad	_L_1+8707
	.quad	_Mod__Mod_var_i64_u64
	.quad	_L_1+8726
	.quad	_Mod__uMod_var_i64_u64
	.quad	_L_1+8742
	.quad	_Mod__Mod_param_i64_u16
	.quad	_L_1+8759
	.quad	_Mod__uMod_param_i64_u16
	.quad	_L_1+8777
	.quad	_Mod__Mod_var_i64_u16
	.quad	_L_1+8796
	.quad	_Mod__uMod_var_i64_u16
	.quad	_L_1+8812
	.quad	_Mod__Mod_param_i64_C
	.quad	_L_1+8829
	.quad	_Mod__uMod_param_i64_C
	.quad	_L_1+8845
	.quad	_Mod__Mod_var_i64_C
	.quad	_L_1+8862
	.quad	_Mod__uMod_var_i64_C
	.quad	_L_1+8876
	.quad	_Mod__Mod_param_i64_u32
	.quad	_L_1+8891
	.quad	_Mod__uMod_param_i64_u32
	.quad	_L_1+8909
	.quad	_Mod__Mod_var_i64_u32
	.quad	_L_1+8928
	.quad	_Mod__uMod_var_i64_u32
	.quad	_L_1+8944
	.quad	_Mod__Mod_param_i64_i8
	.quad	_L_1+8961
	.quad	_Mod__uMod_param_i64_i8
	.quad	_L_1+8978
	.quad	_Mod__Mod_var_i64_i8
	.quad	_L_1+8996
	.quad	_Mod__uMod_var_i64_i8
	.quad	_L_1+9011
	.quad	_Mod__Mod_param_i64_I
	.quad	_L_1+9027
	.quad	_Mod__uMod_param_i64_I
	.quad	_L_1+9043
	.quad	_Mod__Mod_var_i64_I
	.quad	_L_1+9060
	.quad	_Mod__uMod_var_i64_I
	.quad	_L_1+9074
	.quad	_Mod__Mod_param_i64_u8
	.quad	_L_1+9089
	.quad	_Mod__uMod_param_i64_u8
	.quad	_L_1+9106
	.quad	_Mod__Mod_var_i64_u8
	.quad	_L_1+9124
	.quad	_Mod__uMod_var_i64_u8
	.quad	_L_1+9139
	.quad	_Mod__Mod_param_i64_LC
	.quad	_L_1+9155
	.quad	_Mod__uMod_param_i64_LC
	.quad	_L_1+9172
	.quad	_Mod__Mod_var_i64_LC
	.quad	_L_1+9190
	.quad	_Mod__uMod_var_i64_LC
	.quad	_L_1+9205
	.quad	_Mod__Mod_param_i64_i64
	.quad	_L_1+9221
	.quad	_Mod__uMod_param_i64_i64
	.quad	_L_1+9239
	.quad	_Mod__Mod_var_i64_i64
	.quad	_L_1+9258
	.quad	_Mod__uMod_var_i64_i64
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
	.quad	273
	.quad	274
	.long	1133093716
	.byte	21
	.space 3
	.quad	278
	.byte	23
	.space 3
	.long	280
	.quad	281
	.word	282
	.space 6
	.quad	283
	.long	1546188227
	.long	1081197711
	.quad	286
	.word	287
	.space 2
	.long	288
	.space 24
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
