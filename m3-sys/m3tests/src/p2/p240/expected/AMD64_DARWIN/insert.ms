	.text
.globl _insert__uInsert_var_i64_i64
	.private_extern _insert__uInsert_var_i64_i64
_insert__uInsert_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L2
	movl	$1729, %edi
	call	__m3_fault
L2:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L3
	movq	104+_MM_insert(%rip), %rax
	movq	%rax, -32(%rbp)
	jmp	L4
L3:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L5
	movq	104+_MM_insert(%rip), %rax
	movq	%rax, -24(%rbp)
	jmp	L6
L5:
	movq	104+_MM_insert(%rip), %rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	104+_MM_insert(%rip), %rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L6:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L4:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_i64_i64
	.private_extern _insert__uInsert_param_i64_i64
_insert__uInsert_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L9
	movl	$1761, %edi
	call	__m3_fault
L9:
	cmpq	$0, -48(%rbp)
	je	L10
	cmpq	$64, -48(%rbp)
	je	L11
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	-32(%rbp), %rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	movq	%rax, -56(%rbp)
	jmp	L12
L11:
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
L12:
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
	jmp	L13
L10:
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
L13:
	movq	-64(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_LC_LC
	.private_extern _insert__uInsert_var_LC_LC
_insert__uInsert_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L16
	movl	$1793, %edi
	call	__m3_fault
L16:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L17
	movq	112+_MM_insert(%rip), %rax
	movq	%rax, -32(%rbp)
	jmp	L18
L17:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L19
	movq	112+_MM_insert(%rip), %rax
	movq	%rax, -24(%rbp)
	jmp	L20
L19:
	movq	112+_MM_insert(%rip), %rax
	movq	%rax, %rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	112+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L20:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L18:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_LC_LC
	.private_extern _insert__uInsert_param_LC_LC
_insert__uInsert_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L23
	movl	$1825, %edi
	call	__m3_fault
L23:
	cmpq	$0, -48(%rbp)
	jne	L24
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	jmp	L25
L24:
	cmpq	$64, -48(%rbp)
	jne	L26
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L27
L26:
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -56(%rbp)
L27:
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
L25:
	movq	-64(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_u8_u8
	.private_extern _insert__uInsert_var_u8_u8
_insert__uInsert_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L30
	movl	$1857, %edi
	call	__m3_fault
L30:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L31
	movzbl	124+_MM_insert(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -32(%rbp)
	jmp	L32
L31:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L33
	movzbl	124+_MM_insert(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	jmp	L34
L33:
	movzbl	124+_MM_insert(%rip), %eax
	movsbq	%al,%rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movzbl	124+_MM_insert(%rip), %eax
	movsbq	%al,%rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L34:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L32:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_u8_u8
	.private_extern _insert__uInsert_param_u8_u8
_insert__uInsert_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L37
	movl	$1889, %edi
	call	__m3_fault
L37:
	cmpq	$0, -40(%rbp)
	jne	L38
	movsbq	-17(%rbp),%rax
	movq	%rax, -56(%rbp)
	jmp	L39
L38:
	cmpq	$64, -40(%rbp)
	jne	L40
	movsbq	-18(%rbp),%rax
	movq	%rax, -48(%rbp)
	jmp	L41
L40:
	movsbq	-17(%rbp),%rdx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movsbq	-18(%rbp),%rdx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -48(%rbp)
L41:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
L39:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_I_I
	.private_extern _insert__uInsert_var_I_I
_insert__uInsert_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L44
	movl	$1921, %edi
	call	__m3_fault
L44:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L45
	movq	128+_MM_insert(%rip), %rax
	movq	%rax, -32(%rbp)
	jmp	L46
L45:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L47
	movq	128+_MM_insert(%rip), %rax
	movq	%rax, -24(%rbp)
	jmp	L48
L47:
	movq	128+_MM_insert(%rip), %rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	128+_MM_insert(%rip), %rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L48:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L46:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_I_I
	.private_extern _insert__uInsert_param_I_I
_insert__uInsert_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L51
	movl	$1953, %edi
	call	__m3_fault
L51:
	cmpq	$0, -48(%rbp)
	je	L52
	cmpq	$64, -48(%rbp)
	je	L53
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	-32(%rbp), %rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	movq	%rax, -56(%rbp)
	jmp	L54
L53:
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
L54:
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
	jmp	L55
L52:
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
L55:
	movq	-64(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_i8_i8
	.private_extern _insert__uInsert_var_i8_i8
_insert__uInsert_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L58
	movl	$1985, %edi
	call	__m3_fault
L58:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L59
	movzbl	136+_MM_insert(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	jmp	L60
L59:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L61
	movzbl	136+_MM_insert(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	jmp	L62
L61:
	movzbl	136+_MM_insert(%rip), %eax
	movzbl	%al, %esi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movzbl	136+_MM_insert(%rip), %eax
	movzbl	%al, %edx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L62:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L60:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_i8_i8
	.private_extern _insert__uInsert_param_i8_i8
_insert__uInsert_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L65
	movl	$2017, %edi
	call	__m3_fault
L65:
	cmpq	$0, -40(%rbp)
	jne	L66
	movzbl	-17(%rbp), %eax
	movq	%rax, -56(%rbp)
	jmp	L67
L66:
	cmpq	$64, -40(%rbp)
	jne	L68
	movzbl	-18(%rbp), %eax
	movq	%rax, -48(%rbp)
	jmp	L69
L68:
	movzbl	-17(%rbp), %edx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movzbl	-18(%rbp), %edx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -48(%rbp)
L69:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
L67:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_u32_u32
	.private_extern _insert__uInsert_var_u32_u32
_insert__uInsert_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L72
	movl	$2049, %edi
	call	__m3_fault
L72:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L73
	movl	140+_MM_insert(%rip), %eax
	cltq
	movq	%rax, -32(%rbp)
	jmp	L74
L73:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L75
	movl	140+_MM_insert(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	jmp	L76
L75:
	movl	140+_MM_insert(%rip), %eax
	movslq	%eax,%rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movl	140+_MM_insert(%rip), %eax
	movslq	%eax,%rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L76:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L74:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_u32_u32
	.private_extern _insert__uInsert_param_u32_u32
_insert__uInsert_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L79
	movl	$2081, %edi
	call	__m3_fault
L79:
	cmpq	$0, -40(%rbp)
	jne	L80
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -56(%rbp)
	jmp	L81
L80:
	cmpq	$64, -40(%rbp)
	jne	L82
	movl	-24(%rbp), %eax
	cltq
	movq	%rax, -48(%rbp)
	jmp	L83
L82:
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -48(%rbp)
L83:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
L81:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_C_C
	.private_extern _insert__uInsert_var_C_C
_insert__uInsert_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L86
	movl	$2113, %edi
	call	__m3_fault
L86:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L87
	movq	144+_MM_insert(%rip), %rax
	movq	%rax, -32(%rbp)
	jmp	L88
L87:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L89
	movq	144+_MM_insert(%rip), %rax
	movq	%rax, -24(%rbp)
	jmp	L90
L89:
	movq	144+_MM_insert(%rip), %rax
	movq	%rax, %rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	144+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L90:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L88:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_C_C
	.private_extern _insert__uInsert_param_C_C
_insert__uInsert_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L93
	movl	$2145, %edi
	call	__m3_fault
L93:
	cmpq	$0, -48(%rbp)
	jne	L94
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
	jmp	L95
L94:
	cmpq	$64, -48(%rbp)
	jne	L96
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L97
L96:
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -56(%rbp)
L97:
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
L95:
	movq	-64(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_u16_u16
	.private_extern _insert__uInsert_var_u16_u16
_insert__uInsert_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L100
	movl	$2177, %edi
	call	__m3_fault
L100:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L101
	movzwl	152+_MM_insert(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -32(%rbp)
	jmp	L102
L101:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L103
	movzwl	152+_MM_insert(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	jmp	L104
L103:
	movzwl	152+_MM_insert(%rip), %eax
	movswq	%ax,%rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movzwl	152+_MM_insert(%rip), %eax
	movswq	%ax,%rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L104:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L102:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_u16_u16
	.private_extern _insert__uInsert_param_u16_u16
_insert__uInsert_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L107
	movl	$2209, %edi
	call	__m3_fault
L107:
	cmpq	$0, -40(%rbp)
	jne	L108
	movswq	-18(%rbp),%rax
	movq	%rax, -56(%rbp)
	jmp	L109
L108:
	cmpq	$64, -40(%rbp)
	jne	L110
	movswq	-20(%rbp),%rax
	movq	%rax, -48(%rbp)
	jmp	L111
L110:
	movswq	-18(%rbp),%rdx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movswq	-20(%rbp),%rdx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -48(%rbp)
L111:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
L109:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_u64_u64
	.private_extern _insert__uInsert_var_u64_u64
_insert__uInsert_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L114
	movl	$2241, %edi
	call	__m3_fault
L114:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L115
	movq	160+_MM_insert(%rip), %rax
	movq	%rax, -32(%rbp)
	jmp	L116
L115:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L117
	movq	160+_MM_insert(%rip), %rax
	movq	%rax, -24(%rbp)
	jmp	L118
L117:
	movq	160+_MM_insert(%rip), %rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	160+_MM_insert(%rip), %rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L118:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L116:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_u64_u64
	.private_extern _insert__uInsert_param_u64_u64
_insert__uInsert_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L121
	movl	$2273, %edi
	call	__m3_fault
L121:
	cmpq	$0, -48(%rbp)
	je	L122
	cmpq	$64, -48(%rbp)
	je	L123
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	-32(%rbp), %rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	movq	%rax, -56(%rbp)
	jmp	L124
L123:
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
L124:
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
	jmp	L125
L122:
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
L125:
	movq	-64(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_L_L
	.private_extern _insert__uInsert_var_L_L
_insert__uInsert_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L128
	movl	$2305, %edi
	call	__m3_fault
L128:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L129
	movq	176+_MM_insert(%rip), %rax
	movq	%rax, -32(%rbp)
	jmp	L130
L129:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L131
	movq	176+_MM_insert(%rip), %rax
	movq	%rax, -24(%rbp)
	jmp	L132
L131:
	movq	176+_MM_insert(%rip), %rsi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	176+_MM_insert(%rip), %rdx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L132:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L130:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_L_L
	.private_extern _insert__uInsert_param_L_L
_insert__uInsert_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L135
	movl	$2337, %edi
	call	__m3_fault
L135:
	cmpq	$0, -48(%rbp)
	je	L136
	cmpq	$64, -48(%rbp)
	je	L137
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rax, %rdx
	andq	-24(%rbp), %rdx
	movq	-48(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	-32(%rbp), %rax
	movq	-40(%rbp), %rcx
	salq	%cl, %rax
	orq	%rdx, %rax
	movq	%rax, -56(%rbp)
	jmp	L138
L137:
	movq	-32(%rbp), %rax
	movq	%rax, -56(%rbp)
L138:
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
	jmp	L139
L136:
	movq	-24(%rbp), %rax
	movq	%rax, -64(%rbp)
L139:
	movq	-64(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_i16_i16
	.private_extern _insert__uInsert_var_i16_i16
_insert__uInsert_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L142
	movl	$2369, %edi
	call	__m3_fault
L142:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L143
	movzwl	184+_MM_insert(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	jmp	L144
L143:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L145
	movzwl	184+_MM_insert(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	jmp	L146
L145:
	movzwl	184+_MM_insert(%rip), %eax
	movzwl	%ax, %esi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movzwl	184+_MM_insert(%rip), %eax
	movzwl	%ax, %edx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L146:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L144:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_i16_i16
	.private_extern _insert__uInsert_param_i16_i16
_insert__uInsert_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L149
	movl	$2401, %edi
	call	__m3_fault
L149:
	cmpq	$0, -40(%rbp)
	jne	L150
	movzwl	-18(%rbp), %eax
	movq	%rax, -56(%rbp)
	jmp	L151
L150:
	cmpq	$64, -40(%rbp)
	jne	L152
	movzwl	-20(%rbp), %eax
	movq	%rax, -48(%rbp)
	jmp	L153
L152:
	movzwl	-18(%rbp), %edx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	movzwl	-20(%rbp), %edx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -48(%rbp)
L153:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
L151:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_var_i32_i32
	.private_extern _insert__uInsert_var_i32_i32
_insert__uInsert_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_insert(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L156
	movl	$2433, %edi
	call	__m3_fault
L156:
	movq	200+_MM_insert(%rip), %rax
	testq	%rax, %rax
	jne	L157
	movl	188+_MM_insert(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	jmp	L158
L157:
	movq	200+_MM_insert(%rip), %rax
	cmpq	$64, %rax
	jne	L159
	movl	188+_MM_insert(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	jmp	L160
L159:
	movl	188+_MM_insert(%rip), %eax
	mov	%eax, %esi
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	movq	%rax, %rdx
	notq	%rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movl	188+_MM_insert(%rip), %eax
	mov	%eax, %edx
	movq	200+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	192+_MM_insert(%rip), %rax
	movq	%rax, %rcx
	movq	%rdx, %rax
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -24(%rbp)
L160:
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
L158:
	movq	-32(%rbp), %rax
	leave
	ret
.globl _insert__uInsert_param_i32_i32
	.private_extern _insert__uInsert_param_i32_i32
_insert__uInsert_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L163
	movl	$2465, %edi
	call	__m3_fault
L163:
	cmpq	$0, -40(%rbp)
	jne	L164
	mov	-20(%rbp), %eax
	movq	%rax, -56(%rbp)
	jmp	L165
L164:
	cmpq	$64, -40(%rbp)
	jne	L166
	mov	-24(%rbp), %eax
	movq	%rax, -48(%rbp)
	jmp	L167
L166:
	mov	-20(%rbp), %edx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	notq	%rax
	movq	%rdx, %rsi
	andq	%rax, %rsi
	mov	-24(%rbp), %edx
	movq	-40(%rbp), %rcx
	movq	$-1, %rax
	salq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	-32(%rbp), %rcx
	salq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, -48(%rbp)
L167:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
L165:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _insert_M3
_insert_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_insert(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_insert(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "insert_M3"
	.space 1
	.ascii "uInsert_param_i32_i32"
	.space 1
	.ascii "uInsert_var_i32_i32"
	.space 1
	.ascii "uInsert_param_i16_i16"
	.space 1
	.ascii "uInsert_var_i16_i16"
	.space 1
	.ascii "uInsert_param_L_L"
	.space 1
	.ascii "uInsert_var_L_L"
	.space 1
	.ascii "uInsert_param_u64_u64"
	.space 1
	.ascii "uInsert_var_u64_u64"
	.space 1
	.ascii "uInsert_param_u16_u16"
	.space 1
	.ascii "uInsert_var_u16_u16"
	.space 1
	.ascii "uInsert_param_C_C"
	.space 1
	.ascii "uInsert_var_C_C"
	.space 1
	.ascii "uInsert_param_u32_u32"
	.space 1
	.ascii "uInsert_var_u32_u32"
	.space 1
	.ascii "uInsert_param_i8_i8"
	.space 1
	.ascii "uInsert_var_i8_i8"
	.space 1
	.ascii "uInsert_param_I_I"
	.space 1
	.ascii "uInsert_var_I_I"
	.space 1
	.ascii "uInsert_param_u8_u8"
	.space 1
	.ascii "uInsert_var_u8_u8"
	.space 1
	.ascii "uInsert_param_LC_LC"
	.space 1
	.ascii "uInsert_var_LC_LC"
	.space 1
	.ascii "uInsert_param_i64_i64"
	.space 1
	.ascii "uInsert_var_i64_i64"
	.space 3
	.quad	_insert_M3
	.quad	_L_1
	.quad	_insert__uInsert_param_i32_i32
	.quad	_L_1+10
	.quad	_insert__uInsert_var_i32_i32
	.quad	_L_1+32
	.quad	_insert__uInsert_param_i16_i16
	.quad	_L_1+52
	.quad	_insert__uInsert_var_i16_i16
	.quad	_L_1+74
	.quad	_insert__uInsert_param_L_L
	.quad	_L_1+94
	.quad	_insert__uInsert_var_L_L
	.quad	_L_1+112
	.quad	_insert__uInsert_param_u64_u64
	.quad	_L_1+128
	.quad	_insert__uInsert_var_u64_u64
	.quad	_L_1+150
	.quad	_insert__uInsert_param_u16_u16
	.quad	_L_1+170
	.quad	_insert__uInsert_var_u16_u16
	.quad	_L_1+192
	.quad	_insert__uInsert_param_C_C
	.quad	_L_1+212
	.quad	_insert__uInsert_var_C_C
	.quad	_L_1+230
	.quad	_insert__uInsert_param_u32_u32
	.quad	_L_1+246
	.quad	_insert__uInsert_var_u32_u32
	.quad	_L_1+268
	.quad	_insert__uInsert_param_i8_i8
	.quad	_L_1+288
	.quad	_insert__uInsert_var_i8_i8
	.quad	_L_1+308
	.quad	_insert__uInsert_param_I_I
	.quad	_L_1+326
	.quad	_insert__uInsert_var_I_I
	.quad	_L_1+344
	.quad	_insert__uInsert_param_u8_u8
	.quad	_L_1+360
	.quad	_insert__uInsert_var_u8_u8
	.quad	_L_1+380
	.quad	_insert__uInsert_param_LC_LC
	.quad	_L_1+398
	.quad	_insert__uInsert_var_LC_LC
	.quad	_L_1+418
	.quad	_insert__uInsert_param_i64_i64
	.quad	_L_1+436
	.quad	_insert__uInsert_var_i64_i64
	.quad	_L_1+458
	.space 8
	.ascii "../AMD64_DARWIN/insert.m3"
	.space 7
	.data
	.align 5
_MM_insert:
	.quad	_L_1+888
	.space 32
	.quad	_L_1+480
	.space 24
	.quad	_MM_insert+208
	.space 8
	.quad	_insert_M3
	.quad	3
	.quad	817
	.quad	818
	.long	1145894011
	.byte	53
	.space 3
	.quad	822
	.byte	55
	.space 3
	.long	824
	.quad	825
	.word	826
	.space 6
	.quad	827
	.long	3401614098
	.long	1082779297
	.quad	830
	.word	831
	.space 2
	.long	832
	.space 24
	.quad	_insert_I3
	.quad	_MM_insert+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_insert+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_insert+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_insert+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
