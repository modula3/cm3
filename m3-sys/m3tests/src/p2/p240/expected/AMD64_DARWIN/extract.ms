	.text
.no_dead_strip _extract__uExtract_var_u16_u16
_extract__uExtract_var_u16_u16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	pushq	%rbx
LCFI2:
	subq	$8, %rsp
LCFI3:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L2
	movl	$1729, %edi
	call	__m3_fault
L2:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L3
	movzwl	104+_MM_extract(%rip), %eax
	movzwl	%ax, %edx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L4
L3:
	movl	$0, %eax
L4:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI4:
	ret
LFE0:
.no_dead_strip _extract__uExtract_param_u16_u16
_extract__uExtract_param_u16_u16:
LFB1:
	pushq	%rbp
LCFI5:
	movq	%rsp, %rbp
LCFI6:
	pushq	%rbx
LCFI7:
	subq	$40, %rsp
LCFI8:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L6
	movl	$1761, %edi
	call	__m3_fault
L6:
	cmpq	$0, -40(%rbp)
	je	L7
	movzwl	-18(%rbp), %edx
	movq	-40(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-40(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L8
L7:
	movl	$0, %eax
L8:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI9:
	ret
LFE1:
.no_dead_strip _extract__uExtract_var_u32_u32
_extract__uExtract_var_u32_u32:
LFB2:
	pushq	%rbp
LCFI10:
	movq	%rsp, %rbp
LCFI11:
	pushq	%rbx
LCFI12:
	subq	$8, %rsp
LCFI13:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L10
	movl	$1793, %edi
	call	__m3_fault
L10:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L11
	movl	108+_MM_extract(%rip), %eax
	mov	%eax, %edx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L12
L11:
	movl	$0, %eax
L12:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI14:
	ret
LFE2:
.no_dead_strip _extract__uExtract_param_u32_u32
_extract__uExtract_param_u32_u32:
LFB3:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	pushq	%rbx
LCFI17:
	subq	$40, %rsp
LCFI18:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L14
	movl	$1825, %edi
	call	__m3_fault
L14:
	cmpq	$0, -40(%rbp)
	je	L15
	mov	-20(%rbp), %edx
	movq	-40(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-40(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L16
L15:
	movl	$0, %eax
L16:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI19:
	ret
LFE3:
.no_dead_strip _extract__uExtract_var_LC_LC
_extract__uExtract_var_LC_LC:
LFB4:
	pushq	%rbp
LCFI20:
	movq	%rsp, %rbp
LCFI21:
	pushq	%rbx
LCFI22:
	subq	$8, %rsp
LCFI23:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L18
	movl	$1857, %edi
	call	__m3_fault
L18:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L19
	movq	112+_MM_extract(%rip), %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L20
L19:
	movl	$0, %eax
L20:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI24:
	ret
LFE4:
.no_dead_strip _extract__uExtract_param_LC_LC
_extract__uExtract_param_LC_LC:
LFB5:
	pushq	%rbp
LCFI25:
	movq	%rsp, %rbp
LCFI26:
	pushq	%rbx
LCFI27:
	subq	$40, %rsp
LCFI28:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rdx
	movq	-48(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L22
	movl	$1889, %edi
	call	__m3_fault
L22:
	cmpq	$0, -48(%rbp)
	je	L23
	movq	-48(%rbp), %rax
	movl	$64, %edx
	subq	%rax, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-48(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L24
L23:
	movl	$0, %eax
L24:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI29:
	ret
LFE5:
.no_dead_strip _extract__uExtract_var_I_I
_extract__uExtract_var_I_I:
LFB6:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	pushq	%rbx
LCFI32:
	subq	$8, %rsp
LCFI33:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L26
	movl	$1921, %edi
	call	__m3_fault
L26:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L27
	movq	120+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L28
L27:
	movl	$0, %eax
L28:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI34:
	ret
LFE6:
.no_dead_strip _extract__uExtract_param_I_I
_extract__uExtract_param_I_I:
LFB7:
	pushq	%rbp
LCFI35:
	movq	%rsp, %rbp
LCFI36:
	pushq	%rbx
LCFI37:
	subq	$40, %rsp
LCFI38:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rdx
	movq	-48(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L30
	movl	$1953, %edi
	call	__m3_fault
L30:
	cmpq	$0, -48(%rbp)
	je	L31
	movq	-24(%rbp), %rdx
	movq	-48(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-40(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-48(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L32
L31:
	movl	$0, %eax
L32:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI39:
	ret
LFE7:
.no_dead_strip _extract__uExtract_var_i16_i16
_extract__uExtract_var_i16_i16:
LFB8:
	pushq	%rbp
LCFI40:
	movq	%rsp, %rbp
LCFI41:
	pushq	%rbx
LCFI42:
	subq	$8, %rsp
LCFI43:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L34
	movl	$1985, %edi
	call	__m3_fault
L34:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L35
	movzwl	128+_MM_extract(%rip), %eax
	movswq	%ax, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L36
L35:
	movl	$0, %eax
L36:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI44:
	ret
LFE8:
.no_dead_strip _extract__uExtract_param_i16_i16
_extract__uExtract_param_i16_i16:
LFB9:
	pushq	%rbp
LCFI45:
	movq	%rsp, %rbp
LCFI46:
	pushq	%rbx
LCFI47:
	subq	$40, %rsp
LCFI48:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L38
	movl	$2017, %edi
	call	__m3_fault
L38:
	cmpq	$0, -40(%rbp)
	je	L39
	movswq	-18(%rbp), %rdx
	movq	-40(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-40(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L40
L39:
	movl	$0, %eax
L40:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI49:
	ret
LFE9:
.no_dead_strip _extract__uExtract_var_u64_u64
_extract__uExtract_var_u64_u64:
LFB10:
	pushq	%rbp
LCFI50:
	movq	%rsp, %rbp
LCFI51:
	pushq	%rbx
LCFI52:
	subq	$8, %rsp
LCFI53:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L42
	movl	$2049, %edi
	call	__m3_fault
L42:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L43
	movq	136+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L44
L43:
	movl	$0, %eax
L44:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI54:
	ret
LFE10:
.no_dead_strip _extract__uExtract_param_u64_u64
_extract__uExtract_param_u64_u64:
LFB11:
	pushq	%rbp
LCFI55:
	movq	%rsp, %rbp
LCFI56:
	pushq	%rbx
LCFI57:
	subq	$40, %rsp
LCFI58:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rdx
	movq	-48(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L46
	movl	$2081, %edi
	call	__m3_fault
L46:
	cmpq	$0, -48(%rbp)
	je	L47
	movq	-24(%rbp), %rdx
	movq	-48(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-40(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-48(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L48
L47:
	movl	$0, %eax
L48:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI59:
	ret
LFE11:
.no_dead_strip _extract__uExtract_var_i32_i32
_extract__uExtract_var_i32_i32:
LFB12:
	pushq	%rbp
LCFI60:
	movq	%rsp, %rbp
LCFI61:
	pushq	%rbx
LCFI62:
	subq	$8, %rsp
LCFI63:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L50
	movl	$2113, %edi
	call	__m3_fault
L50:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L51
	movl	152+_MM_extract(%rip), %eax
	movslq	%eax, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L52
L51:
	movl	$0, %eax
L52:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI64:
	ret
LFE12:
.no_dead_strip _extract__uExtract_param_i32_i32
_extract__uExtract_param_i32_i32:
LFB13:
	pushq	%rbp
LCFI65:
	movq	%rsp, %rbp
LCFI66:
	pushq	%rbx
LCFI67:
	subq	$40, %rsp
LCFI68:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L54
	movl	$2145, %edi
	call	__m3_fault
L54:
	cmpq	$0, -40(%rbp)
	je	L55
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-40(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-40(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L56
L55:
	movl	$0, %eax
L56:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI69:
	ret
LFE13:
.no_dead_strip _extract__uExtract_var_C_C
_extract__uExtract_var_C_C:
LFB14:
	pushq	%rbp
LCFI70:
	movq	%rsp, %rbp
LCFI71:
	pushq	%rbx
LCFI72:
	subq	$8, %rsp
LCFI73:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L58
	movl	$2177, %edi
	call	__m3_fault
L58:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L59
	movq	160+_MM_extract(%rip), %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L60
L59:
	movl	$0, %eax
L60:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI74:
	ret
LFE14:
.no_dead_strip _extract__uExtract_param_C_C
_extract__uExtract_param_C_C:
LFB15:
	pushq	%rbp
LCFI75:
	movq	%rsp, %rbp
LCFI76:
	pushq	%rbx
LCFI77:
	subq	$40, %rsp
LCFI78:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rdx
	movq	-48(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L62
	movl	$2209, %edi
	call	__m3_fault
L62:
	cmpq	$0, -48(%rbp)
	je	L63
	movq	-48(%rbp), %rax
	movl	$64, %edx
	subq	%rax, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-48(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L64
L63:
	movl	$0, %eax
L64:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI79:
	ret
LFE15:
.no_dead_strip _extract__uExtract_var_u8_u8
_extract__uExtract_var_u8_u8:
LFB16:
	pushq	%rbp
LCFI80:
	movq	%rsp, %rbp
LCFI81:
	pushq	%rbx
LCFI82:
	subq	$8, %rsp
LCFI83:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L66
	movl	$2241, %edi
	call	__m3_fault
L66:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L67
	movzbl	168+_MM_extract(%rip), %eax
	movzbl	%al, %edx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L68
L67:
	movl	$0, %eax
L68:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI84:
	ret
LFE16:
.no_dead_strip _extract__uExtract_param_u8_u8
_extract__uExtract_param_u8_u8:
LFB17:
	pushq	%rbp
LCFI85:
	movq	%rsp, %rbp
LCFI86:
	pushq	%rbx
LCFI87:
	subq	$40, %rsp
LCFI88:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L70
	movl	$2273, %edi
	call	__m3_fault
L70:
	cmpq	$0, -40(%rbp)
	je	L71
	movzbl	-17(%rbp), %edx
	movq	-40(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-40(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L72
L71:
	movl	$0, %eax
L72:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI89:
	ret
LFE17:
.no_dead_strip _extract__uExtract_var_i64_i64
_extract__uExtract_var_i64_i64:
LFB18:
	pushq	%rbp
LCFI90:
	movq	%rsp, %rbp
LCFI91:
	pushq	%rbx
LCFI92:
	subq	$8, %rsp
LCFI93:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L74
	movl	$2305, %edi
	call	__m3_fault
L74:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L75
	movq	176+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L76
L75:
	movl	$0, %eax
L76:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI94:
	ret
LFE18:
.no_dead_strip _extract__uExtract_param_i64_i64
_extract__uExtract_param_i64_i64:
LFB19:
	pushq	%rbp
LCFI95:
	movq	%rsp, %rbp
LCFI96:
	pushq	%rbx
LCFI97:
	subq	$40, %rsp
LCFI98:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rdx
	movq	-48(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L78
	movl	$2337, %edi
	call	__m3_fault
L78:
	cmpq	$0, -48(%rbp)
	je	L79
	movq	-24(%rbp), %rdx
	movq	-48(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-40(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-48(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L80
L79:
	movl	$0, %eax
L80:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI99:
	ret
LFE19:
.no_dead_strip _extract__uExtract_var_L_L
_extract__uExtract_var_L_L:
LFB20:
	pushq	%rbp
LCFI100:
	movq	%rsp, %rbp
LCFI101:
	pushq	%rbx
LCFI102:
	subq	$8, %rsp
LCFI103:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L82
	movl	$2369, %edi
	call	__m3_fault
L82:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L83
	movq	192+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L84
L83:
	movl	$0, %eax
L84:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI104:
	ret
LFE20:
.no_dead_strip _extract__uExtract_param_L_L
_extract__uExtract_param_L_L:
LFB21:
	pushq	%rbp
LCFI105:
	movq	%rsp, %rbp
LCFI106:
	pushq	%rbx
LCFI107:
	subq	$40, %rsp
LCFI108:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rdx
	movq	-48(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L86
	movl	$2401, %edi
	call	__m3_fault
L86:
	cmpq	$0, -48(%rbp)
	je	L87
	movq	-24(%rbp), %rdx
	movq	-48(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-40(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-48(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L88
L87:
	movl	$0, %eax
L88:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI109:
	ret
LFE21:
.no_dead_strip _extract__uExtract_var_i8_i8
_extract__uExtract_var_i8_i8:
LFB22:
	pushq	%rbp
LCFI110:
	movq	%rsp, %rbp
LCFI111:
	pushq	%rbx
LCFI112:
	subq	$8, %rsp
LCFI113:
	movq	208+_MM_extract(%rip), %rax
	movq	%rax, %rdx
	movq	216+_MM_extract(%rip), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L90
	movl	$2433, %edi
	call	__m3_fault
L90:
	movq	216+_MM_extract(%rip), %rax
	testq	%rax, %rax
	je	L91
	movzbl	200+_MM_extract(%rip), %eax
	movsbq	%al, %rdx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	208+_MM_extract(%rip), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	216+_MM_extract(%rip), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L92
L91:
	movl	$0, %eax
L92:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI114:
	ret
LFE22:
.no_dead_strip _extract__uExtract_param_i8_i8
_extract__uExtract_param_i8_i8:
LFB23:
	pushq	%rbp
LCFI115:
	movq	%rsp, %rbp
LCFI116:
	pushq	%rbx
LCFI117:
	subq	$40, %rsp
LCFI118:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	cmpq	$64, %rax
	jle	L94
	movl	$2465, %edi
	call	__m3_fault
L94:
	cmpq	$0, -40(%rbp)
	je	L95
	movsbq	-17(%rbp), %rdx
	movq	-40(%rbp), %rax
	movl	$64, %ecx
	subq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	-40(%rbp), %rax
	movl	$64, %edx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	movq	%rbx, %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	jmp	L96
L95:
	movl	$0, %eax
L96:
	addq	$40, %rsp
	popq	%rbx
	leave
LCFI119:
	ret
LFE23:
.globl _extract_M3
.no_dead_strip _extract_M3
_extract_M3:
LFB24:
	pushq	%rbp
LCFI120:
	movq	%rsp, %rbp
LCFI121:
	movq	%rdi, -8(%rbp)
	leaq	_MM_extract(%rip), %rax
	leave
LCFI122:
	ret
LFE24:
__m3_fault:
LFB25:
	pushq	%rbp
LCFI123:
	movq	%rsp, %rbp
LCFI124:
	subq	$16, %rsp
LCFI125:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rsi
	leaq	_MM_extract(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
LCFI126:
	ret
LFE25:
	.const_data
	.align 5
_L_1:
	.ascii "extract_M3"
	.space 1
	.ascii "uExtract_param_i8_i8"
	.space 1
	.ascii "uExtract_var_i8_i8"
	.space 1
	.ascii "uExtract_param_L_L"
	.space 1
	.ascii "uExtract_var_L_L"
	.space 1
	.ascii "uExtract_param_i64_i64"
	.space 1
	.ascii "uExtract_var_i64_i64"
	.space 1
	.ascii "uExtract_param_u8_u8"
	.space 1
	.ascii "uExtract_var_u8_u8"
	.space 1
	.ascii "uExtract_param_C_C"
	.space 1
	.ascii "uExtract_var_C_C"
	.space 1
	.ascii "uExtract_param_i32_i32"
	.space 1
	.ascii "uExtract_var_i32_i32"
	.space 1
	.ascii "uExtract_param_u64_u64"
	.space 1
	.ascii "uExtract_var_u64_u64"
	.space 1
	.ascii "uExtract_param_i16_i16"
	.space 1
	.ascii "uExtract_var_i16_i16"
	.space 1
	.ascii "uExtract_param_I_I"
	.space 1
	.ascii "uExtract_var_I_I"
	.space 1
	.ascii "uExtract_param_LC_LC"
	.space 1
	.ascii "uExtract_var_LC_LC"
	.space 1
	.ascii "uExtract_param_u32_u32"
	.space 1
	.ascii "uExtract_var_u32_u32"
	.space 1
	.ascii "uExtract_param_u16_u16"
	.space 1
	.ascii "uExtract_var_u16_u16"
	.space 2
	.quad	_extract_M3
	.quad	_L_1
	.quad	_extract__uExtract_param_i8_i8
	.quad	_L_1+11
	.quad	_extract__uExtract_var_i8_i8
	.quad	_L_1+32
	.quad	_extract__uExtract_param_L_L
	.quad	_L_1+51
	.quad	_extract__uExtract_var_L_L
	.quad	_L_1+70
	.quad	_extract__uExtract_param_i64_i64
	.quad	_L_1+87
	.quad	_extract__uExtract_var_i64_i64
	.quad	_L_1+110
	.quad	_extract__uExtract_param_u8_u8
	.quad	_L_1+131
	.quad	_extract__uExtract_var_u8_u8
	.quad	_L_1+152
	.quad	_extract__uExtract_param_C_C
	.quad	_L_1+171
	.quad	_extract__uExtract_var_C_C
	.quad	_L_1+190
	.quad	_extract__uExtract_param_i32_i32
	.quad	_L_1+207
	.quad	_extract__uExtract_var_i32_i32
	.quad	_L_1+230
	.quad	_extract__uExtract_param_u64_u64
	.quad	_L_1+251
	.quad	_extract__uExtract_var_u64_u64
	.quad	_L_1+274
	.quad	_extract__uExtract_param_i16_i16
	.quad	_L_1+295
	.quad	_extract__uExtract_var_i16_i16
	.quad	_L_1+318
	.quad	_extract__uExtract_param_I_I
	.quad	_L_1+339
	.quad	_extract__uExtract_var_I_I
	.quad	_L_1+358
	.quad	_extract__uExtract_param_LC_LC
	.quad	_L_1+375
	.quad	_extract__uExtract_var_LC_LC
	.quad	_L_1+396
	.quad	_extract__uExtract_param_u32_u32
	.quad	_L_1+415
	.quad	_extract__uExtract_var_u32_u32
	.quad	_L_1+438
	.quad	_extract__uExtract_param_u16_u16
	.quad	_L_1+459
	.quad	_extract__uExtract_var_u16_u16
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
	.quad	_MM_extract+224
	.space 8
	.quad	_extract_M3
	.quad	3
	.word	849
	.space 2
	.long	850
	.quad	851
	.quad	852
	.word	853
	.space 6
	.quad	854
	.long	377957122
	.long	1082834649
	.long	857
	.space 4
	.quad	858
	.byte	91
	.space 7
	.quad	860
	.long	1146582827
	.space 4
	.quad	863
	.byte	96
	.space 31
	.quad	_extract_I3
	.quad	_MM_extract+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_extract+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_extract+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_extract+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
.globl _extract_M3.eh
_extract_M3.eh = 0
.no_dead_strip _extract_M3.eh
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0x0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x78
	.byte	0x10
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.byte	0x90
	.byte	0x1
	.align 3
LECIE1:
_extract__uExtract_var_u16_u16.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB0-.
	.set L$set$2,LFE0-LFB0
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI0-LFB0
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$5,LCFI3-LCFI1
	.long L$set$5
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$6,LCFI4-LCFI3
	.long L$set$6
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE1:
_extract__uExtract_param_u16_u16.eh:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB1-.
	.set L$set$8,LFE1-LFB1
	.quad L$set$8
	.byte	0x0
	.byte	0x4
	.set L$set$9,LCFI5-LFB1
	.long L$set$9
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$10,LCFI6-LCFI5
	.long L$set$10
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$11,LCFI8-LCFI6
	.long L$set$11
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$12,LCFI9-LCFI8
	.long L$set$12
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE3:
_extract__uExtract_var_u32_u32.eh:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB2-.
	.set L$set$14,LFE2-LFB2
	.quad L$set$14
	.byte	0x0
	.byte	0x4
	.set L$set$15,LCFI10-LFB2
	.long L$set$15
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$16,LCFI11-LCFI10
	.long L$set$16
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$17,LCFI13-LCFI11
	.long L$set$17
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$18,LCFI14-LCFI13
	.long L$set$18
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE5:
_extract__uExtract_param_u32_u32.eh:
LSFDE7:
	.set L$set$19,LEFDE7-LASFDE7
	.long L$set$19
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB3-.
	.set L$set$20,LFE3-LFB3
	.quad L$set$20
	.byte	0x0
	.byte	0x4
	.set L$set$21,LCFI15-LFB3
	.long L$set$21
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$22,LCFI16-LCFI15
	.long L$set$22
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$23,LCFI18-LCFI16
	.long L$set$23
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$24,LCFI19-LCFI18
	.long L$set$24
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE7:
_extract__uExtract_var_LC_LC.eh:
LSFDE9:
	.set L$set$25,LEFDE9-LASFDE9
	.long L$set$25
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB4-.
	.set L$set$26,LFE4-LFB4
	.quad L$set$26
	.byte	0x0
	.byte	0x4
	.set L$set$27,LCFI20-LFB4
	.long L$set$27
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$28,LCFI21-LCFI20
	.long L$set$28
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$29,LCFI23-LCFI21
	.long L$set$29
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$30,LCFI24-LCFI23
	.long L$set$30
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE9:
_extract__uExtract_param_LC_LC.eh:
LSFDE11:
	.set L$set$31,LEFDE11-LASFDE11
	.long L$set$31
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB5-.
	.set L$set$32,LFE5-LFB5
	.quad L$set$32
	.byte	0x0
	.byte	0x4
	.set L$set$33,LCFI25-LFB5
	.long L$set$33
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$34,LCFI26-LCFI25
	.long L$set$34
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$35,LCFI28-LCFI26
	.long L$set$35
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$36,LCFI29-LCFI28
	.long L$set$36
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE11:
_extract__uExtract_var_I_I.eh:
LSFDE13:
	.set L$set$37,LEFDE13-LASFDE13
	.long L$set$37
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB6-.
	.set L$set$38,LFE6-LFB6
	.quad L$set$38
	.byte	0x0
	.byte	0x4
	.set L$set$39,LCFI30-LFB6
	.long L$set$39
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$40,LCFI31-LCFI30
	.long L$set$40
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$41,LCFI33-LCFI31
	.long L$set$41
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$42,LCFI34-LCFI33
	.long L$set$42
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE13:
_extract__uExtract_param_I_I.eh:
LSFDE15:
	.set L$set$43,LEFDE15-LASFDE15
	.long L$set$43
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB7-.
	.set L$set$44,LFE7-LFB7
	.quad L$set$44
	.byte	0x0
	.byte	0x4
	.set L$set$45,LCFI35-LFB7
	.long L$set$45
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$46,LCFI36-LCFI35
	.long L$set$46
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$47,LCFI38-LCFI36
	.long L$set$47
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$48,LCFI39-LCFI38
	.long L$set$48
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE15:
_extract__uExtract_var_i16_i16.eh:
LSFDE17:
	.set L$set$49,LEFDE17-LASFDE17
	.long L$set$49
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB8-.
	.set L$set$50,LFE8-LFB8
	.quad L$set$50
	.byte	0x0
	.byte	0x4
	.set L$set$51,LCFI40-LFB8
	.long L$set$51
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$52,LCFI41-LCFI40
	.long L$set$52
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$53,LCFI43-LCFI41
	.long L$set$53
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$54,LCFI44-LCFI43
	.long L$set$54
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE17:
_extract__uExtract_param_i16_i16.eh:
LSFDE19:
	.set L$set$55,LEFDE19-LASFDE19
	.long L$set$55
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB9-.
	.set L$set$56,LFE9-LFB9
	.quad L$set$56
	.byte	0x0
	.byte	0x4
	.set L$set$57,LCFI45-LFB9
	.long L$set$57
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$58,LCFI46-LCFI45
	.long L$set$58
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$59,LCFI48-LCFI46
	.long L$set$59
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$60,LCFI49-LCFI48
	.long L$set$60
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE19:
_extract__uExtract_var_u64_u64.eh:
LSFDE21:
	.set L$set$61,LEFDE21-LASFDE21
	.long L$set$61
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB10-.
	.set L$set$62,LFE10-LFB10
	.quad L$set$62
	.byte	0x0
	.byte	0x4
	.set L$set$63,LCFI50-LFB10
	.long L$set$63
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$64,LCFI51-LCFI50
	.long L$set$64
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$65,LCFI53-LCFI51
	.long L$set$65
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$66,LCFI54-LCFI53
	.long L$set$66
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE21:
_extract__uExtract_param_u64_u64.eh:
LSFDE23:
	.set L$set$67,LEFDE23-LASFDE23
	.long L$set$67
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB11-.
	.set L$set$68,LFE11-LFB11
	.quad L$set$68
	.byte	0x0
	.byte	0x4
	.set L$set$69,LCFI55-LFB11
	.long L$set$69
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$70,LCFI56-LCFI55
	.long L$set$70
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$71,LCFI58-LCFI56
	.long L$set$71
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$72,LCFI59-LCFI58
	.long L$set$72
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE23:
_extract__uExtract_var_i32_i32.eh:
LSFDE25:
	.set L$set$73,LEFDE25-LASFDE25
	.long L$set$73
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB12-.
	.set L$set$74,LFE12-LFB12
	.quad L$set$74
	.byte	0x0
	.byte	0x4
	.set L$set$75,LCFI60-LFB12
	.long L$set$75
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$76,LCFI61-LCFI60
	.long L$set$76
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$77,LCFI63-LCFI61
	.long L$set$77
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$78,LCFI64-LCFI63
	.long L$set$78
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE25:
_extract__uExtract_param_i32_i32.eh:
LSFDE27:
	.set L$set$79,LEFDE27-LASFDE27
	.long L$set$79
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB13-.
	.set L$set$80,LFE13-LFB13
	.quad L$set$80
	.byte	0x0
	.byte	0x4
	.set L$set$81,LCFI65-LFB13
	.long L$set$81
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$82,LCFI66-LCFI65
	.long L$set$82
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$83,LCFI68-LCFI66
	.long L$set$83
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$84,LCFI69-LCFI68
	.long L$set$84
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE27:
_extract__uExtract_var_C_C.eh:
LSFDE29:
	.set L$set$85,LEFDE29-LASFDE29
	.long L$set$85
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB14-.
	.set L$set$86,LFE14-LFB14
	.quad L$set$86
	.byte	0x0
	.byte	0x4
	.set L$set$87,LCFI70-LFB14
	.long L$set$87
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$88,LCFI71-LCFI70
	.long L$set$88
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$89,LCFI73-LCFI71
	.long L$set$89
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$90,LCFI74-LCFI73
	.long L$set$90
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE29:
_extract__uExtract_param_C_C.eh:
LSFDE31:
	.set L$set$91,LEFDE31-LASFDE31
	.long L$set$91
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB15-.
	.set L$set$92,LFE15-LFB15
	.quad L$set$92
	.byte	0x0
	.byte	0x4
	.set L$set$93,LCFI75-LFB15
	.long L$set$93
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$94,LCFI76-LCFI75
	.long L$set$94
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$95,LCFI78-LCFI76
	.long L$set$95
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$96,LCFI79-LCFI78
	.long L$set$96
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE31:
_extract__uExtract_var_u8_u8.eh:
LSFDE33:
	.set L$set$97,LEFDE33-LASFDE33
	.long L$set$97
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB16-.
	.set L$set$98,LFE16-LFB16
	.quad L$set$98
	.byte	0x0
	.byte	0x4
	.set L$set$99,LCFI80-LFB16
	.long L$set$99
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$100,LCFI81-LCFI80
	.long L$set$100
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$101,LCFI83-LCFI81
	.long L$set$101
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$102,LCFI84-LCFI83
	.long L$set$102
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE33:
_extract__uExtract_param_u8_u8.eh:
LSFDE35:
	.set L$set$103,LEFDE35-LASFDE35
	.long L$set$103
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB17-.
	.set L$set$104,LFE17-LFB17
	.quad L$set$104
	.byte	0x0
	.byte	0x4
	.set L$set$105,LCFI85-LFB17
	.long L$set$105
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$106,LCFI86-LCFI85
	.long L$set$106
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$107,LCFI88-LCFI86
	.long L$set$107
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$108,LCFI89-LCFI88
	.long L$set$108
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE35:
_extract__uExtract_var_i64_i64.eh:
LSFDE37:
	.set L$set$109,LEFDE37-LASFDE37
	.long L$set$109
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB18-.
	.set L$set$110,LFE18-LFB18
	.quad L$set$110
	.byte	0x0
	.byte	0x4
	.set L$set$111,LCFI90-LFB18
	.long L$set$111
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$112,LCFI91-LCFI90
	.long L$set$112
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$113,LCFI93-LCFI91
	.long L$set$113
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$114,LCFI94-LCFI93
	.long L$set$114
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE37:
_extract__uExtract_param_i64_i64.eh:
LSFDE39:
	.set L$set$115,LEFDE39-LASFDE39
	.long L$set$115
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB19-.
	.set L$set$116,LFE19-LFB19
	.quad L$set$116
	.byte	0x0
	.byte	0x4
	.set L$set$117,LCFI95-LFB19
	.long L$set$117
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$118,LCFI96-LCFI95
	.long L$set$118
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$119,LCFI98-LCFI96
	.long L$set$119
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$120,LCFI99-LCFI98
	.long L$set$120
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE39:
_extract__uExtract_var_L_L.eh:
LSFDE41:
	.set L$set$121,LEFDE41-LASFDE41
	.long L$set$121
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB20-.
	.set L$set$122,LFE20-LFB20
	.quad L$set$122
	.byte	0x0
	.byte	0x4
	.set L$set$123,LCFI100-LFB20
	.long L$set$123
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$124,LCFI101-LCFI100
	.long L$set$124
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$125,LCFI103-LCFI101
	.long L$set$125
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$126,LCFI104-LCFI103
	.long L$set$126
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE41:
_extract__uExtract_param_L_L.eh:
LSFDE43:
	.set L$set$127,LEFDE43-LASFDE43
	.long L$set$127
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB21-.
	.set L$set$128,LFE21-LFB21
	.quad L$set$128
	.byte	0x0
	.byte	0x4
	.set L$set$129,LCFI105-LFB21
	.long L$set$129
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$130,LCFI106-LCFI105
	.long L$set$130
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$131,LCFI108-LCFI106
	.long L$set$131
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$132,LCFI109-LCFI108
	.long L$set$132
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE43:
_extract__uExtract_var_i8_i8.eh:
LSFDE45:
	.set L$set$133,LEFDE45-LASFDE45
	.long L$set$133
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB22-.
	.set L$set$134,LFE22-LFB22
	.quad L$set$134
	.byte	0x0
	.byte	0x4
	.set L$set$135,LCFI110-LFB22
	.long L$set$135
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$136,LCFI111-LCFI110
	.long L$set$136
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$137,LCFI113-LCFI111
	.long L$set$137
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$138,LCFI114-LCFI113
	.long L$set$138
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE45:
_extract__uExtract_param_i8_i8.eh:
LSFDE47:
	.set L$set$139,LEFDE47-LASFDE47
	.long L$set$139
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB23-.
	.set L$set$140,LFE23-LFB23
	.quad L$set$140
	.byte	0x0
	.byte	0x4
	.set L$set$141,LCFI115-LFB23
	.long L$set$141
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$142,LCFI116-LCFI115
	.long L$set$142
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$143,LCFI118-LCFI116
	.long L$set$143
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$144,LCFI119-LCFI118
	.long L$set$144
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE47:
__m3_fault.eh:
LSFDE49:
	.set L$set$145,LEFDE49-LASFDE49
	.long L$set$145
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB25-.
	.set L$set$146,LFE25-LFB25
	.quad L$set$146
	.byte	0x0
	.byte	0x4
	.set L$set$147,LCFI123-LFB25
	.long L$set$147
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$148,LCFI124-LCFI123
	.long L$set$148
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$149,LCFI126-LCFI124
	.long L$set$149
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE49:
	.subsections_via_symbols
