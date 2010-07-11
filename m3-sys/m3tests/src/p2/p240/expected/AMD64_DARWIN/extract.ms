	.text
.globl _extract__uExtract_var_i64_i64
	.private_extern _extract__uExtract_var_i64_i64
_extract__uExtract_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L2
	movl	$1729, %edi
	call	__m3_fault
L2:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L3
	movq	104+_MM_extract(%rip), %rax
	movq	%rax, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L4
L3:
	movq	$0, -24(%rbp)
L4:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_i64_i64
	.private_extern _extract__uExtract_param_i64_i64
_extract__uExtract_param_i64_i64:
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
	jle	L7
	movl	$1761, %edi
	call	__m3_fault
L7:
	cmpq	$0, -48(%rbp)
	je	L8
	movq	-24(%rbp), %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L9
L8:
	movq	$0, -56(%rbp)
L9:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_LC_LC
	.private_extern _extract__uExtract_var_LC_LC
_extract__uExtract_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L12
	movl	$1793, %edi
	call	__m3_fault
L12:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L13
	movq	112+_MM_extract(%rip), %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L14
L13:
	movq	$0, -24(%rbp)
L14:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_LC_LC
	.private_extern _extract__uExtract_param_LC_LC
_extract__uExtract_param_LC_LC:
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
	jle	L17
	movl	$1825, %edi
	call	__m3_fault
L17:
	cmpq	$0, -48(%rbp)
	je	L18
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rsi
	salq	%cl, %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L19
L18:
	movq	$0, -56(%rbp)
L19:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_u8_u8
	.private_extern _extract__uExtract_var_u8_u8
_extract__uExtract_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L22
	movl	$1857, %edi
	call	__m3_fault
L22:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L23
	movzbl	124+_MM_extract(%rip), %eax
	movsbq	%al,%rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L24
L23:
	movq	$0, -24(%rbp)
L24:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_u8_u8
	.private_extern _extract__uExtract_param_u8_u8
_extract__uExtract_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L27
	movl	$1889, %edi
	call	__m3_fault
L27:
	cmpq	$0, -40(%rbp)
	je	L28
	movsbq	-17(%rbp),%rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L29
L28:
	movq	$0, -48(%rbp)
L29:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_I_I
	.private_extern _extract__uExtract_var_I_I
_extract__uExtract_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L32
	movl	$1921, %edi
	call	__m3_fault
L32:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L33
	movq	128+_MM_extract(%rip), %rax
	movq	%rax, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L34
L33:
	movq	$0, -24(%rbp)
L34:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_I_I
	.private_extern _extract__uExtract_param_I_I
_extract__uExtract_param_I_I:
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
	jle	L37
	movl	$1953, %edi
	call	__m3_fault
L37:
	cmpq	$0, -48(%rbp)
	je	L38
	movq	-24(%rbp), %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L39
L38:
	movq	$0, -56(%rbp)
L39:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_i8_i8
	.private_extern _extract__uExtract_var_i8_i8
_extract__uExtract_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L42
	movl	$1985, %edi
	call	__m3_fault
L42:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L43
	movzbl	136+_MM_extract(%rip), %eax
	movzbl	%al, %esi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L44
L43:
	movq	$0, -24(%rbp)
L44:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_i8_i8
	.private_extern _extract__uExtract_param_i8_i8
_extract__uExtract_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L47
	movl	$2017, %edi
	call	__m3_fault
L47:
	cmpq	$0, -40(%rbp)
	je	L48
	movzbl	-17(%rbp), %esi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L49
L48:
	movq	$0, -48(%rbp)
L49:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_u32_u32
	.private_extern _extract__uExtract_var_u32_u32
_extract__uExtract_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L52
	movl	$2049, %edi
	call	__m3_fault
L52:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L53
	movl	140+_MM_extract(%rip), %eax
	movslq	%eax,%rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L54
L53:
	movq	$0, -24(%rbp)
L54:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_u32_u32
	.private_extern _extract__uExtract_param_u32_u32
_extract__uExtract_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L57
	movl	$2081, %edi
	call	__m3_fault
L57:
	cmpq	$0, -40(%rbp)
	je	L58
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L59
L58:
	movq	$0, -48(%rbp)
L59:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_C_C
	.private_extern _extract__uExtract_var_C_C
_extract__uExtract_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L62
	movl	$2113, %edi
	call	__m3_fault
L62:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L63
	movq	144+_MM_extract(%rip), %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L64
L63:
	movq	$0, -24(%rbp)
L64:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_C_C
	.private_extern _extract__uExtract_param_C_C
_extract__uExtract_param_C_C:
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
	jle	L67
	movl	$2145, %edi
	call	__m3_fault
L67:
	cmpq	$0, -48(%rbp)
	je	L68
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rsi
	salq	%cl, %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L69
L68:
	movq	$0, -56(%rbp)
L69:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_u16_u16
	.private_extern _extract__uExtract_var_u16_u16
_extract__uExtract_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L72
	movl	$2177, %edi
	call	__m3_fault
L72:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L73
	movzwl	152+_MM_extract(%rip), %eax
	movswq	%ax,%rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L74
L73:
	movq	$0, -24(%rbp)
L74:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_u16_u16
	.private_extern _extract__uExtract_param_u16_u16
_extract__uExtract_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L77
	movl	$2209, %edi
	call	__m3_fault
L77:
	cmpq	$0, -40(%rbp)
	je	L78
	movswq	-18(%rbp),%rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L79
L78:
	movq	$0, -48(%rbp)
L79:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_u64_u64
	.private_extern _extract__uExtract_var_u64_u64
_extract__uExtract_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L82
	movl	$2241, %edi
	call	__m3_fault
L82:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L83
	movq	160+_MM_extract(%rip), %rax
	movq	%rax, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L84
L83:
	movq	$0, -24(%rbp)
L84:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_u64_u64
	.private_extern _extract__uExtract_param_u64_u64
_extract__uExtract_param_u64_u64:
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
	jle	L87
	movl	$2273, %edi
	call	__m3_fault
L87:
	cmpq	$0, -48(%rbp)
	je	L88
	movq	-24(%rbp), %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L89
L88:
	movq	$0, -56(%rbp)
L89:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_L_L
	.private_extern _extract__uExtract_var_L_L
_extract__uExtract_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L92
	movl	$2305, %edi
	call	__m3_fault
L92:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L93
	movq	176+_MM_extract(%rip), %rax
	movq	%rax, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L94
L93:
	movq	$0, -24(%rbp)
L94:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_L_L
	.private_extern _extract__uExtract_param_L_L
_extract__uExtract_param_L_L:
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
	jle	L97
	movl	$2337, %edi
	call	__m3_fault
L97:
	cmpq	$0, -48(%rbp)
	je	L98
	movq	-24(%rbp), %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-48(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L99
L98:
	movq	$0, -56(%rbp)
L99:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_i16_i16
	.private_extern _extract__uExtract_var_i16_i16
_extract__uExtract_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L102
	movl	$2369, %edi
	call	__m3_fault
L102:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L103
	movzwl	184+_MM_extract(%rip), %eax
	movzwl	%ax, %esi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L104
L103:
	movq	$0, -24(%rbp)
L104:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_i16_i16
	.private_extern _extract__uExtract_param_i16_i16
_extract__uExtract_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
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
	movl	$2401, %edi
	call	__m3_fault
L107:
	cmpq	$0, -40(%rbp)
	je	L108
	movzwl	-18(%rbp), %esi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L109
L108:
	movq	$0, -48(%rbp)
L109:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_var_i32_i32
	.private_extern _extract__uExtract_var_i32_i32
_extract__uExtract_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	200+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L112
	movl	$2433, %edi
	call	__m3_fault
L112:
	movq	200+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L113
	movl	188+_MM_extract(%rip), %eax
	mov	%eax, %esi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	192+_MM_extract(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	200+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L114
L113:
	movq	$0, -24(%rbp)
L114:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _extract__uExtract_param_i32_i32
	.private_extern _extract__uExtract_param_i32_i32
_extract__uExtract_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$64, -16(%rbp)
	jle	L117
	movl	$2465, %edi
	call	__m3_fault
L117:
	cmpq	$0, -40(%rbp)
	je	L118
	mov	-20(%rbp), %esi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	salq	%cl, %rsi
	movq	-40(%rbp), %rdx
	movl	$64, %eax
	movq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L119
L118:
	movq	$0, -48(%rbp)
L119:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _extract_M3
_extract_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_extract(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_extract(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "extract_M3"
	.space 1
	.ascii "uExtract_param_i32_i32"
	.space 1
	.ascii "uExtract_var_i32_i32"
	.space 1
	.ascii "uExtract_param_i16_i16"
	.space 1
	.ascii "uExtract_var_i16_i16"
	.space 1
	.ascii "uExtract_param_L_L"
	.space 1
	.ascii "uExtract_var_L_L"
	.space 1
	.ascii "uExtract_param_u64_u64"
	.space 1
	.ascii "uExtract_var_u64_u64"
	.space 1
	.ascii "uExtract_param_u16_u16"
	.space 1
	.ascii "uExtract_var_u16_u16"
	.space 1
	.ascii "uExtract_param_C_C"
	.space 1
	.ascii "uExtract_var_C_C"
	.space 1
	.ascii "uExtract_param_u32_u32"
	.space 1
	.ascii "uExtract_var_u32_u32"
	.space 1
	.ascii "uExtract_param_i8_i8"
	.space 1
	.ascii "uExtract_var_i8_i8"
	.space 1
	.ascii "uExtract_param_I_I"
	.space 1
	.ascii "uExtract_var_I_I"
	.space 1
	.ascii "uExtract_param_u8_u8"
	.space 1
	.ascii "uExtract_var_u8_u8"
	.space 1
	.ascii "uExtract_param_LC_LC"
	.space 1
	.ascii "uExtract_var_LC_LC"
	.space 1
	.ascii "uExtract_param_i64_i64"
	.space 1
	.ascii "uExtract_var_i64_i64"
	.space 2
	.quad	_extract_M3
	.quad	_L_1
	.quad	_extract__uExtract_param_i32_i32
	.quad	_L_1+11
	.quad	_extract__uExtract_var_i32_i32
	.quad	_L_1+34
	.quad	_extract__uExtract_param_i16_i16
	.quad	_L_1+55
	.quad	_extract__uExtract_var_i16_i16
	.quad	_L_1+78
	.quad	_extract__uExtract_param_L_L
	.quad	_L_1+99
	.quad	_extract__uExtract_var_L_L
	.quad	_L_1+118
	.quad	_extract__uExtract_param_u64_u64
	.quad	_L_1+135
	.quad	_extract__uExtract_var_u64_u64
	.quad	_L_1+158
	.quad	_extract__uExtract_param_u16_u16
	.quad	_L_1+179
	.quad	_extract__uExtract_var_u16_u16
	.quad	_L_1+202
	.quad	_extract__uExtract_param_C_C
	.quad	_L_1+223
	.quad	_extract__uExtract_var_C_C
	.quad	_L_1+242
	.quad	_extract__uExtract_param_u32_u32
	.quad	_L_1+259
	.quad	_extract__uExtract_var_u32_u32
	.quad	_L_1+282
	.quad	_extract__uExtract_param_i8_i8
	.quad	_L_1+303
	.quad	_extract__uExtract_var_i8_i8
	.quad	_L_1+324
	.quad	_extract__uExtract_param_I_I
	.quad	_L_1+343
	.quad	_extract__uExtract_var_I_I
	.quad	_L_1+362
	.quad	_extract__uExtract_param_u8_u8
	.quad	_L_1+379
	.quad	_extract__uExtract_var_u8_u8
	.quad	_L_1+400
	.quad	_extract__uExtract_param_LC_LC
	.quad	_L_1+419
	.quad	_extract__uExtract_var_LC_LC
	.quad	_L_1+440
	.quad	_extract__uExtract_param_i64_i64
	.quad	_L_1+459
	.quad	_extract__uExtract_var_i64_i64
	.quad	_L_1+482
	.space 8
	.ascii "../AMD64_DARWIN/extract.m3"
	.space 6
	.data
	.align 5
_MM_extract:
	.quad	_L_1+912
	.space 32
	.quad	_L_1+504
	.space 24
	.quad	_MM_extract+208
	.space 8
	.quad	_extract_M3
	.quad	3
	.quad	849
	.quad	850
	.long	1146418823
	.byte	85
	.space 3
	.quad	854
	.byte	87
	.space 3
	.long	856
	.quad	857
	.word	858
	.space 6
	.quad	859
	.long	1408749273
	.long	1082844899
	.quad	862
	.word	863
	.space 2
	.long	864
	.space 24
	.quad	_extract_I3
	.quad	_MM_extract+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_extract+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_extract+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_extract+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
